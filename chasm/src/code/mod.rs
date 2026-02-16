use crate::{CodeArgs, project::ChasmProject};

use anyhow::Result;
use crossterm::event::{self, Event, KeyCode, KeyEvent, KeyEventKind};
use ratatui::{
    DefaultTerminal,
    Frame,
    buffer::Buffer,
    layout::{Constraint, Direction, Layout, Rect},
    style::{Color, Style, Stylize},
    text::Text,
    widgets::{Block, Paragraph, Widget}
};

pub fn code(args: CodeArgs) -> Result<()> {
    let project = ChasmProject::load(args.project_dir.0)?;
    dbg!(project);
    ratatui::run(|terminal| App::new().run(terminal))
}

#[derive(Debug)]
pub struct App {
    status: StatusBar,
    editor: Editor,
    should_exit: bool
}
impl App {
    pub fn new() -> Self {
        Self {
            status: StatusBar {},
            editor: Editor {},
            should_exit: false,
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
        frame.render_widget(&self.editor, layout[1]);
    }

    fn handle_events(&mut self) -> Result<()> {
        match event::read()? {
            Event::Key(key_event) if key_event.kind == KeyEventKind::Press => {
                self.handle_key_event(key_event)
            }
            _ => {}
        };

        Ok(())
    }

    fn handle_key_event(&mut self, key_event: KeyEvent) {
        match key_event.code {
            KeyCode::Char('q') => self.exit(),
            _ => {}
        }
    }

    fn exit(&mut self) {
        self.should_exit = true;
    }

}

#[derive(Debug)]
struct StatusBar {
    // TODO
}
impl Widget for &StatusBar {
    fn render(self, area: Rect, buf: &mut Buffer) {
        let title = "Chasm Editor".bold();
        let block = Block::default().style(Style::new().bg(Color::LightGreen));
        Paragraph::new(title)
            .block(block)
            .render(area, buf);
    }
}

#[derive(Debug)]
struct Editor {
    // TODO
}
impl Widget for &Editor {
    fn render(self, area: Rect, buf: &mut Buffer) {
        let counter_text = Text::from(vec![
            "Line1".into(),
            "Line2".into(),
            "Line3".into(),
        ]);

        Paragraph::new(counter_text).block(Block::default()).render(area, buf);
    }
}