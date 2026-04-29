mod editor;
mod status_bar;
mod mod_select;

#[cfg(target_family = "unix")]
use std::path::PathBuf;
use std::{
    collections::VecDeque,
    fmt::Debug,
    fs::{File, OpenOptions},
    io::Write,
    ops::Not,
    sync::{Arc, RwLock, mpsc::{self, Sender}}
};

use crate::{
    CodeArgs,
    FileOrProject,
    code::{editor::{Editor, ModuleOrFile},
    mod_select::{ModuleSelectView, ModuleSelectViewArgs},
    status_bar::StatusBar}
};

use anyhow::{Result, bail};
use chrono::Utc;
use crossterm::event::{self, Event, KeyCode, KeyEventKind, KeyModifiers};
use ratatui::{
    DefaultTerminal,
    Frame,
    buffer::Buffer,
    layout::{Constraint, Direction, Layout, Rect},
    style::{Style, Stylize},
    text::{Line, Span},
    widgets::{Block, Borders, FrameExt, ListState, Paragraph, Widget, WidgetRef}
};

trait ChasmWidget: Debug {
    fn handle_event(&mut self, event: &Event, ctx: &mut WidgetContext);
    fn render(&self, area: Rect, buf: &mut Buffer);
}
impl Widget for &dyn ChasmWidget {
    fn render(self, area: Rect, buf: &mut Buffer) {
        self.render(area, buf);
    }
}
impl WidgetRef for &dyn ChasmWidget {
    fn render_ref(&self, area: Rect, buf: &mut Buffer) {
        self.render(area, buf);
    }
}

struct WidgetContext<'a> {
    metadata: &'a Metadata,
    command_queue_tx: CommandQueueTx<'a>,
}
impl<'a> WidgetContext<'a> {
    fn log(&mut self, message: String) {
        self.command_queue_tx.push(AppCommand::Log(message));
    }
}

struct AppContext {
    metadata: Metadata,
    command_queue: CommandQueue,
}
impl AppContext {
    #[cfg(test)]
    fn new_test() -> Self {
        Self {
            metadata: Metadata {
                status_area: StatusArea(Rect::new(100, 100, 100, 100)),
                view_area: ViewArea(Rect::new(100, 100, 100, 100)),
            },
            command_queue: CommandQueue::new(),
        }
    }

    fn widget_context<'a>(&'a mut self) -> WidgetContext<'a> {
        WidgetContext {
            metadata: &self.metadata,
            command_queue_tx: self.command_queue.tx(),
        }
    }
}

struct CommandQueue {
    q: VecDeque<AppCommand>,
}
impl CommandQueue {
    fn new() -> Self {
        Self {
            q: VecDeque::new(),
        }
    }
    
    fn tx<'a>(&'a mut self) -> CommandQueueTx<'a> {
        CommandQueueTx { cq: self }
    }

    fn drain(&mut self) -> impl Iterator<Item = AppCommand> + '_ {
        self.q.drain(..)
    }
}

struct CommandQueueTx<'a> {
    cq: &'a mut CommandQueue,
}
impl CommandQueueTx<'_> {
    fn push(&mut self, cmd: AppCommand) {
        self.cq.q.push_back(cmd);
    }
}

#[derive(Copy, Clone, Debug, Default)]
struct Metadata {
    status_area: StatusArea,
    view_area: ViewArea,
}

pub fn code(args: CodeArgs) -> Result<()> {
    let (log_file_path, log_file) = create_log_file()?;
    eprintln!("Logging to {}", log_file_path.display());
    ratatui::run(|term| App::new(args.file_or_project, log_file).run(term))
}

#[derive(Debug)]
enum AppCommand {
    None,
    PopView,
    PushView(Box<dyn ChasmWidget>),

    Yes,
    No,

    Log(String),
    SearchStatus(String),
    CommandStatus(String),
    Quit,
}

#[derive(Copy, Clone, Debug, Default)]
struct StatusArea(Rect);
#[derive(Copy, Clone, Debug, Default)]
struct ViewArea(Rect);

#[derive(Debug)]
pub struct App {
    status: StatusBar,
    view_stack: Vec<Box<dyn ChasmWidget>>,

    should_exit: bool,
    exit_modal_active: bool,
    log_tx: Sender<String>,
    log_handle: std::thread::JoinHandle<()>,
}
impl App {
    pub(crate) fn new(file_or_project: FileOrProject, log_file: File) -> Self {
        let first_widget = select_first_widget(file_or_project);
        let (log_tx, log_handle) = spawn_logger(log_file);

        Self {
            status: StatusBar::new(),
            view_stack: vec![first_widget],

            should_exit: false,
            exit_modal_active: false,
            log_tx,
            log_handle,
        }
    }

    pub fn run(&mut self, terminal: &mut DefaultTerminal) -> Result<()> {

        let metadata = Metadata::default();
        let cq = CommandQueue::new();
        let mut app_ctx = AppContext { metadata, command_queue: cq };

        while !self.should_exit {
            terminal.draw(|frame| {
                let (status_area, view_area) = Self::top_level_layout(&frame);
                app_ctx.metadata = Metadata { status_area, view_area };
                self.draw(frame, status_area, view_area);
            })?;

            self.handle_events(&mut app_ctx)?;
        }
        Ok(())
    }

    fn top_level_layout(f: &Frame) -> (StatusArea, ViewArea) {
        let layout = Layout::default()
            .direction(Direction::Vertical)
            .constraints([
                Constraint::Length(1),
                Constraint::Fill(1),
            ])
            .split(f.area());

        (StatusArea(layout[0]), ViewArea(layout[1]))
    }

    fn draw(&self, frame: &mut Frame, status_area: StatusArea, view_area: ViewArea) {
        frame.render_widget(&self.status, status_area.0);
        frame.render_widget_ref(self.view_stack.last().unwrap().as_ref(), view_area.0);
    }

    fn handle_events(&mut self, app_ctx: &mut AppContext) -> Result<()> {
        let mut widget_ctx = app_ctx.widget_context();
        match event::read()? {
            e @ Event::Key(key_event) if key_event.kind == KeyEventKind::Press => {
                self.status.handle_event(&e, &mut widget_ctx);

                match key_event.code {
                    KeyCode::Char('q') if key_event.modifiers == KeyModifiers::CONTROL => {
                        if !self.exit_modal_active {
                            self.show_exit_modal();
                        }
                    }
                    _other => {
                        self.view_stack.last_mut().unwrap().handle_event(&e, &mut widget_ctx);
                    }
                }

                for cmd in app_ctx.command_queue.drain() {
                    match cmd {
                        AppCommand::Yes => {
                            if self.exit_modal_active {
                                self.view_stack.pop();
                                self.exit_modal_active = false;
                                self.should_exit = true;
                            }
                        }
                        AppCommand::No => {
                            if self.exit_modal_active {
                                self.view_stack.pop();
                                self.exit_modal_active = false;
                            }
                        }
                        AppCommand::None => {}
                        AppCommand::PushView(widget) => self.view_stack.push(widget),
                        AppCommand::Log(record) => {
                            if self.log_handle.is_finished() {
                                bail!("Logger thread has terminated unexpectedly");
                            }

                            if let Err(e) = self.log_tx.send(record) {
                                bail!("Logger thread dropped rx handle: {e}");
                            }
                        }
                        AppCommand::SearchStatus(input) => {
                            self.status.set_search_input(input);
                        }
                        AppCommand::CommandStatus(input) => {
                            self.status.set_command_input(input);
                        }
                        AppCommand::Quit => {
                            if ! self.exit_modal_active {
                                self.show_exit_modal();
                            }
                        }
                        other => { 
                            panic!("Unexpected command from exit modal: {other:?}");
                        }
                    }
                }
            }
            e => {
                self.status.handle_event(&e, &mut widget_ctx);
            }
        };

        Ok(())
    }

    fn show_exit_modal(&mut self) {
        let modal = YesNoModal::new("Exit", "Are you sure you want to exit?");
        self.view_stack.push(Box::new(modal));
        self.exit_modal_active = true;
    }
}

fn select_first_widget(file_or_project: FileOrProject) -> Box<dyn ChasmWidget> {
    match file_or_project {
        FileOrProject::File(path) => {
            let mod_or_file = ModuleOrFile::File(path);
            let editor = Editor::new(mod_or_file);
            Box::new(editor) as Box<dyn ChasmWidget>
        }
        FileOrProject::Project(project) => {
            let mut select_list_state = ListState::default();
            select_list_state.select(Some(0));
            let module_paths = project.module_names();
            let args = ModuleSelectViewArgs {
                project: Arc::new(RwLock::new(project)),
                select_list_state,
                module_paths,
            };
            Box::new(ModuleSelectView::new(args)) as Box<dyn ChasmWidget>
        }
    }
}

#[derive(Copy, Clone, Debug)]
enum ModalSelection {
    Yes,
    No,
}
impl Not for ModalSelection {
    type Output = Self;

    fn not(self) -> Self::Output {
        match self {
            ModalSelection::Yes => ModalSelection::No,
            ModalSelection::No => ModalSelection::Yes
        }
    }
}
struct YesNoModal {
    title: &'static str,
    prompt: Span<'static>,
    selection: ModalSelection,
}
impl YesNoModal {
    pub fn new(title: &'static str, prompt: &'static str) -> Self {
        let prompt = Span::raw(prompt);
        Self { title, prompt, selection: ModalSelection::No }
    }
}
impl ChasmWidget for YesNoModal {
    fn handle_event(&mut self, event: &Event, ctx: &mut WidgetContext) {
        if let Event::Key(key_event) = event && key_event.kind == KeyEventKind::Press {
            match key_event.code {
                KeyCode::Char('h') | KeyCode::Left => {
                    // left, no wrap
                    self.selection = ModalSelection::Yes;
                }
                KeyCode::Char('l') | KeyCode::Right => {
                    // right, no wrap
                    self.selection = ModalSelection::No;
                }
                KeyCode::Char('Y') => {
                    if matches!(self.selection, ModalSelection::Yes) {
                        ctx.command_queue_tx.push(AppCommand::Yes);
                    } else {
                        self.selection = ModalSelection::Yes;
                    }
                }
                KeyCode::Char('N') => {
                    if matches!(self.selection, ModalSelection::No) {
                        ctx.command_queue_tx.push(AppCommand::No);
                    } else {
                        self.selection = ModalSelection::No;
                    }
                }
                KeyCode::Tab => {
                    // toggle
                    self.selection = !self.selection;
                }
                KeyCode::Enter => {
                    match self.selection {
                        ModalSelection::Yes => ctx.command_queue_tx.push(AppCommand::Yes),
                        ModalSelection::No => ctx.command_queue_tx.push(AppCommand::No),
                    }
                }
                KeyCode::Esc => {
                    ctx.command_queue_tx.push(AppCommand::No);
                }
                _other => {
                }
            }
        } else {
        }
    }

    fn render(&self, area: Rect, buf: &mut Buffer) {
        let modal_block = Block::default().title(self.title).borders(Borders::ALL);
        let modal_area = centered_rect(50, 12, area);
        let inner_area = modal_block.inner(modal_area);
        let inner_layout = Layout::default()
            .direction(Direction::Vertical)
            .constraints([
                Constraint::Min(0),     // top spacer
                Constraint::Length(3),  // prompt
                Constraint::Length(1),  // gap
                Constraint::Length(1),  // buttons
                Constraint::Min(0),     // bottom spacer
            ])
            .split(inner_area);

        let prompt_area = inner_layout[1];
        let button_row_area = inner_layout[3];

        let button_layout = Layout::default()
            .direction(Direction::Horizontal)
            .constraints([
                Constraint::Min(0),
                Constraint::Length(7), // yes
                Constraint::Length(4), // gap between buttons
                Constraint::Length(6), // no
                Constraint::Min(0),
            ])
            .split(button_row_area);

        let yes_area = button_layout[1];
        let no_area = button_layout[3];

        let prompt_block = Paragraph::new(self.prompt.clone()).centered();
        let yes_style = if matches!(self.selection, ModalSelection::Yes) {
            Style::default().reversed().bold()
        } else {
            Style::default()
        };

        let no_style = if matches!(self.selection, ModalSelection::No) {
            Style::default().reversed().bold()
        } else {
            Style::default()
        };

        let yes_button = Paragraph::new(Line::from(vec!["[ ".into(), "Y".underlined(), "es ]".into()])).centered().style(yes_style).block(Block::default());
        let no_button = Paragraph::new(Line::from(vec!["[ ".into(), "N".underlined(), "o ]".into()])).centered().style(no_style).block(Block::default());

        modal_block.render(modal_area, buf);
        prompt_block.render(prompt_area, buf);
        yes_button.render(yes_area, buf);
        no_button.render(no_area, buf)
    }
}
impl std::fmt::Debug for YesNoModal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("YesNoModal")
            .field("title", &self.title)
            .field("prompt", &self.prompt)
            .field("selection", &self.selection)
            .finish()
    }
}

fn centered_rect(width: u16, height: u16, area: Rect) -> Rect {
    let x = area.x + (area.width.saturating_sub(width)) / 2;
    let y = area.y + (area.height.saturating_sub(height)) / 2;

    Rect::new(x, y, width, height)
}

#[cfg(target_family = "unix")]
fn create_log_file() -> Result<(PathBuf, File)> {
    let xdg_chasm_base = xdg::BaseDirectories::with_prefix("chasm");
    let log_file_path = xdg_chasm_base.place_state_file("chasm-code.log")?;
    let log_file = OpenOptions::new()
        .create(true)
        .append(true)
        .open(&log_file_path)?;

    Ok((log_file_path, log_file))
}

#[cfg(target_family = "windows")]
fn create_log_file() -> Result<(PathBuf, File)> {
    let log_file_path = std::env::var("LOCALAPPDATA")
        .map(PathBuf::from)?
        .join("Chasm")
        .join("logs")
        .join("chasm-code.log");

    let log_file = OpenOptions::new()
        .create(true)
        .append(true)
        .open(&log_file_path)?;

    Ok((log_file_path, log_file))
}

fn spawn_logger(log_file: File) -> (Sender<String>, std::thread::JoinHandle<()>) {
    let (log_tx, log_rx) = mpsc::channel();

    let handle = std::thread::spawn(move || {
        while let Ok(log_entry) = log_rx.recv() {
            let now = Utc::now();
            if let Err(e) = writeln!(&log_file, "[{}] {}", now, log_entry) {
                eprintln!("Failed to write to log file: {e}");
            }
        }
    });

    (log_tx, handle)
}