use std::{
    fmt::{Debug, Write},
    sync::{Arc, RwLock},
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
    text::{Line, Span},
    widgets::{Block, FrameExt, List, ListState, Paragraph, StatefulWidget, Widget, WidgetRef}
};

trait ChasmWidget: Debug {
    fn handle_event(&mut self, event: &Event);
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
pub struct App {
    project: Arc<RwLock<ChasmProject>>,
    status: StatusBar,
    should_exit: bool,
    view_stack: Vec<Box<dyn ChasmWidget>>,
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
            should_exit: false,
            view_stack: vec![Box::new(ModuleSelectView::new(args))],
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
        frame.render_widget_ref(self.view_stack[0].as_ref(), layout[1]);
    }

    fn handle_events(&mut self) -> Result<()> {
        match event::read()? {
            e @ Event::Key(key_event) if key_event.kind == KeyEventKind::Press => {
                match key_event.code {
                    KeyCode::Char('q') => self.exit(),
                    _other => {
                        self.status.handle_event(&e); 
                        self.view_stack[0].handle_event(&e);
                    }
                }
            }
            _ => {}
        };

        Ok(())
    }

    fn exit(&mut self) {
        self.should_exit = true;
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
    fn handle_event(&mut self, event: &Event) {
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
    // TODO
}
impl Widget for &Editor {
    fn render(self, area: Rect, buf: &mut Buffer) {

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
}
impl ModuleSelectView {
    pub fn new(args:ModuleSelectViewArgs) -> Self {
        let base_item = ["Create new module".to_string()].into_iter();
        let modules = args.module_paths.into_iter().map(|m| m.name);
        let list = List::new(base_item.chain(modules))
            .highlight_style(Style::new().reversed())
            .highlight_symbol(">> ".bold())
            .repeat_highlight_symbol(true);

        Self { project: args.project, select_list_state: args.select_list_state, list }
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
    fn handle_event(&mut self, event: &Event) {
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
                }
                _other => {
                    // No-op for now
                }
            }
        }
    }

    fn render(&self, area: Rect, buf: &mut Buffer) {
        let mut state = self.select_list_state;
        StatefulWidget::render(&self.list, area, buf, &mut state);
    }
}