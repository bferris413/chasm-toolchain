use std::{
    fmt::{Debug, Write}, ops::Not, sync::{Arc, RwLock}
};

use crate::{CodeArgs, project::{ChasmProject, ModulePath}};

use anyhow::Result;
use crossterm::event::{self, Event, KeyCode, KeyEventKind};
use ratatui::{
    DefaultTerminal,
    Frame,
    buffer::Buffer,
    layout::{Constraint, Direction, Layout, Rect},
    style::{Color, Style, Stylize},
    text::{Line, Span, Text},
    widgets::{Block, Borders, FrameExt, List, ListState, Paragraph, StatefulWidget, Widget, WidgetRef}
};

trait ChasmWidget: Debug {
    fn handle_event(&mut self, event: &Event) -> AppCommand;
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

pub fn code(args: CodeArgs) -> Result<()> {
    let project = ChasmProject::load(args.project_dir.0)?;
    ratatui::run(|terminal| App::new(project).run(terminal))
}

#[derive(Debug)]
enum AppCommand {
    None,
    Quit,
    PopView,
    PushView(Box<dyn ChasmWidget>),
    Yes,
    No,
}

#[derive(Debug)]
pub struct App {
    project: Arc<RwLock<ChasmProject>>,
    status: StatusBar,
    view_stack: Vec<Box<dyn ChasmWidget>>,

    should_exit: bool,
    exit_modal_active: bool,
}
impl App {
    pub fn new(project: ChasmProject) -> Self {
        let project = Arc::new(RwLock::new(project));
        let mut select_list_state = ListState::default();
        select_list_state.select(Some(0));

        let args = ModuleSelectViewArgs {
            project: project.clone(),
            select_list_state,
            module_paths: project.read().unwrap().module_names(),
        };
        Self {
            project: project.clone(),
            status: StatusBar::new(),
            view_stack: vec![Box::new(ModuleSelectView::new(args))],

            should_exit: false,
            exit_modal_active: false,
        }
    }

    pub fn run(&mut self, terminal: &mut DefaultTerminal) -> Result<()> {
        while !self.should_exit {
            terminal.draw(|frame| self.draw(frame))?;
            self.handle_events()?;
        }
        Ok(())
    }

    fn draw(&self, frame: &mut Frame) {
        let layout = Layout::default()
            .direction(Direction::Vertical)
            .constraints([
                Constraint::Length(1),
                Constraint::Fill(1),
            ])
            .split(frame.area());

        frame.render_widget(&self.status, layout[0]);
        frame.render_widget_ref(self.view_stack.last().unwrap().as_ref(), layout[1]);
    }

    fn handle_events(&mut self) -> Result<()> {
        match event::read()? {
            e @ Event::Key(key_event) if key_event.kind == KeyEventKind::Press => {
                self.status.handle_event(&e); 

                match key_event.code {
                    KeyCode::Char('q') => {
                        if !self.exit_modal_active {
                            let modal = YesNoModal::new("Exit", "Are you sure you want to exit?");
                            self.view_stack.push(Box::new(modal));
                            self.exit_modal_active = true;
                        }
                    }
                    _other => {
                        let cmd = self.view_stack.last_mut().unwrap().handle_event(&e);
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
                            other => { 
                                panic!("Unexpected command from exit modal: {other:?}");
                            }
                        }
                    }
                }
            }
            _ => {}
        };

        Ok(())
    }
}

#[derive(Debug)]
struct StatusBar {
    event_text: String,
}
impl StatusBar {
    pub fn new() -> Self {
        Self {
            event_text: String::new(),
         }
     }
}
impl ChasmWidget for StatusBar {
    fn handle_event(&mut self, event: &Event) -> AppCommand {
        match event {
            Event::Key(key_event) if key_event.kind == KeyEventKind::Press => {
                self.event_text.clear();
                let modifier = if key_event.modifiers.is_empty() {
                    String::new()
                } else {
                    format!("{}+", key_event.modifiers)
                };
                write!(&mut self.event_text, "<pressed {modifier}{}>", key_event.code).unwrap();
            }
            other => {
                self.event_text.clear();
                write!(&mut self.event_text, "<event {:?}>", other).unwrap();
            }
        };

        AppCommand::None
    }

    fn render(&self, area: Rect, buf: &mut Buffer) {
        let title = "Chasm Editor";
        let block = Block::default().style(Style::new().bg(Color::LightGreen));
        let text = Line::from(vec![
            Span::styled(title, Style::new().bold()),
            Span::raw(" - "),
            Span::raw(&self.event_text),
        ]);
        Paragraph::new(text)
            .block(block)
            .render(area, buf);
    }
}
impl WidgetRef for StatusBar {
    fn render_ref(&self, area: Rect, buf: &mut Buffer) {
        ChasmWidget::render(self, area, buf);
    }
}
impl Widget for &StatusBar {
    fn render(self, area: Rect, buf: &mut Buffer) {
        ChasmWidget::render(self, area, buf);
    }
}

#[derive(Debug)]
struct Editor {
    module: ModulePath,
    code: Vec<String>,
    scroll_y: usize,
}
impl Editor {
    pub fn new(module: ModulePath) -> Self {

        let code = std::fs::read_to_string(&module.path).unwrap_or_else(|e| {
            format!("Error loading module {}: {}\n\nReload of something...", module.name, e)
        });

        let code = code.split_inclusive('\n').map(|line| line.to_string()).collect();

        Self {
            module,
            code,
            scroll_y: 0,
        }
    }
}
impl ChasmWidget for Editor {
    
    fn handle_event(&mut self, event: &Event) -> AppCommand {
        if let Event::Key(key_event) = event && key_event.kind == KeyEventKind::Press {
            match key_event.code {
                KeyCode::Char('j') | KeyCode::Down => {
                    // down, no wrap
                    self.scroll_y = self.scroll_y.saturating_add(1);
                    AppCommand::None
                }
                KeyCode::Char('k') | KeyCode::Up => {
                    // up, no wrap
                    self.scroll_y = self.scroll_y.saturating_sub(1);
                    AppCommand::None
                }
                _other => {
                    AppCommand::None
                }
            }
        } else {
            AppCommand::None
        }
    }

    fn render(&self, area: Rect, buf: &mut Buffer) {
        let height = area.height as usize;
        let digits_for_line_numbers = digits(self.code.len() as u64);
        let total_lines = self.code.len();

        let layout = Layout::default()
            .direction(Direction::Horizontal)
            .constraints([
                Constraint::Length(digits_for_line_numbers as u16 + 1),
                Constraint::Fill(1),
            ])
            .split(area);

        let gutter = layout[0];
        let code_area = layout[1];

        let start = self.scroll_y;
        let end = (start + height).min(total_lines);

        // 1️⃣ Render line numbers manually
        for (screen_row, line_idx) in (start..end).enumerate() {
            let line_number = line_idx + 1;

            let text = format!("{:<width$} ", line_number, width = digits_for_line_numbers as usize);

            buf.set_string(
                gutter.x,
                gutter.y + screen_row as u16,
                text,
                Style::default().fg(Color::DarkGray),
            );
        }
        
        let code_slice = &self.code[start..end as usize];
        let code_as_lines = code_slice.iter().map(|line| Line::from(line.as_str())).collect::<Vec<_>>();

        Paragraph::new(code_as_lines).block(Block::default()).render(code_area, buf);  
    }
}

#[derive(Debug)]
struct ModuleSelectViewArgs {
    pub project: Arc<RwLock<ChasmProject>>,
    pub select_list_state: ListState,
    pub module_paths: Vec<ModulePath>,
}

#[derive(Debug)]
struct ModuleSelectView {
    project: Arc<RwLock<ChasmProject>>,
    select_list_state: ListState,
    list: List<'static>,
    module_paths: Vec<ModulePath>,
}
impl ModuleSelectView {
    pub fn new(args:ModuleSelectViewArgs) -> Self {
        let base_item = ["Create new module".to_string()].into_iter();
        let modules = args.module_paths.iter().map(|m| m.name.clone());
        let list = List::new(base_item.chain(modules))
            .highlight_style(Style::new().reversed())
            .highlight_symbol(">> ".bold())
            .repeat_highlight_symbol(true);

        Self { project: args.project, select_list_state: args.select_list_state, list, module_paths: args.module_paths }
    }

    fn update_selection(&mut self, next_fn: fn(isize) -> isize) {
        let len = self.list.len() as isize;
        let cur_selected = self.select_list_state.selected().unwrap_or(0) as isize;

        let mut next = next_fn(cur_selected);

        if next < 0 {
            next = len - 1;
        } else if next >= len {
            next = 0;
        }

        self.select_list_state.select(Some(next as usize));
    }
}

impl ChasmWidget for ModuleSelectView {
    fn handle_event(&mut self, event: &Event) -> AppCommand {
        if let Event::Key(key_event) = event && key_event.kind == KeyEventKind::Press {
            match key_event.code {
                KeyCode::Char('j') | KeyCode::Down => {
                    self.update_selection(|n| n + 1);
                }
                KeyCode::Char('k') | KeyCode::Up => {
                    self.update_selection(|n| n - 1);
                }
                KeyCode::Enter => {
                    // Open selected module or create new module if "Create new module" is selected
                    let selected = self.select_list_state.selected().unwrap();
                    if selected == 0 {
                        // TODO create new module
                    } else {
                        let selected = selected - 1; // offset to account for "Create new"
                        let module_path = self.module_paths[selected].clone();
                        let editor = Editor::new(module_path);
                        return AppCommand::PushView(Box::new(editor));
                    }

                }
                _other => {
                    // No-op for now
                }
            }
        }

        AppCommand::None
    }

    fn render(&self, area: Rect, buf: &mut Buffer) {
        let mut state = self.select_list_state;
        StatefulWidget::render(&self.list, area, buf, &mut state);
    }
}

#[derive(Copy, Clone, Debug)]
enum Selection {
    Yes,
    No,
}
impl Not for Selection {
    type Output = Self;

    fn not(self) -> Self::Output {
        match self {
            Selection::Yes => Selection::No,
            Selection::No => Selection::Yes
        }
    }
}
struct YesNoModal {
    title: &'static str,
    prompt: Span<'static>,
    selection: Selection,
}
impl YesNoModal {
    pub fn new(title: &'static str, prompt: &'static str) -> Self {
        let prompt = Span::raw(prompt);
        Self { title, prompt, selection: Selection::No }
    }
}
impl ChasmWidget for YesNoModal {
    fn handle_event(&mut self, event: &Event) -> AppCommand {
        if let Event::Key(key_event) = event && key_event.kind == KeyEventKind::Press {
            match key_event.code {
                KeyCode::Char('h') | KeyCode::Left => {
                    // left, no wrap
                    self.selection = Selection::Yes;
                    AppCommand::None
                }
                KeyCode::Char('l') | KeyCode::Right => {
                    // right, no wrap
                    self.selection = Selection::No;
                    AppCommand::None
                }
                KeyCode::Tab => {
                    // toggle
                    self.selection = !self.selection;
                    AppCommand::None
                }
                KeyCode::Enter => {
                    match self.selection {
                        Selection::Yes => AppCommand::Yes,
                        Selection::No => AppCommand::No,
                    }
                }
                KeyCode::Esc => {
                    AppCommand::No
                }
                _other => {
                    AppCommand::None
                }
            }
        } else {
            AppCommand::None
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
        let yes_style = if matches!(self.selection, Selection::Yes) {
            Style::default().reversed().bold()
        } else {
            Style::default()
        };

        let no_style = if matches!(self.selection, Selection::No) {
            Style::default().reversed().bold()
        } else {
            Style::default()
        };

        let yes_button = Paragraph::new("[ Yes ]").centered().style(yes_style).block(Block::default());
        let no_button = Paragraph::new("[ No ]").centered().style(no_style).block(Block::default());

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

fn digits(n: u64) -> u32 {
    if n == 0 {
        1
    } else {
        n.ilog10() + 1
    }
}