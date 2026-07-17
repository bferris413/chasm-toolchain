use std::path::{Path, PathBuf};

use crossterm::event::Event;
use ratatui::{buffer::Buffer, layout::{Constraint, Direction, Layout, Rect}, style::{Color, Style}, widgets::{Paragraph, Widget}};

use crate::code::{ChasmWidget, WidgetContext, input::Input};

#[derive(Debug)]
pub (super) struct LoadFile {
    input: Input,
    dir_files: Vec<String>,
}
impl LoadFile {
    pub fn new(default_dir: PathBuf) -> Self {
        assert!(default_dir.is_dir(), "LoadFile should be initialized with a directory path");

        Self {
            input: Input::new(default_dir.to_string_lossy().to_string()),
            dir_files: get_dir_files(&default_dir),
        }
    }

    pub fn input(&self) -> &str {
        self.input.input()
    }
}
impl ChasmWidget for LoadFile {
    fn handle_event(&mut self, event: &Event, ctx: &mut WidgetContext) {
        self.input.handle_event(event, ctx);
        let cur_input = Path::new(self.input.input());

        if cur_input.is_dir() && cur_input.exists() {
            self.dir_files = get_dir_files(cur_input);
        } else if let Some(parent) = cur_input.parent() && parent.is_dir() && parent.exists() {
            self.dir_files = get_dir_files(parent);
        } else {
            self.dir_files.clear();
        }
    }

    fn render(&self, area: Rect, buf: &mut Buffer) {
        let layout = Layout::default()
            .direction(Direction::Vertical)
            .constraints([Constraint::Length(1), Constraint::Length(2), Constraint::Min(0)])
            .split(area);

        Paragraph::new("Enter file path to load:")
            .style(Style::default().fg(Color::Cyan))
            .render(layout[0], buf);

        self.input.render(layout[1], buf);

        let files_str = self.dir_files.join("\n");
        Paragraph::new(files_str)
            .style(Style::default().fg(Color::Yellow))
            .render(layout[2], buf);
    }
}

fn get_dir_files(dir: &Path) -> Vec<String> {
    let Ok(entries) = dir.read_dir() else {
        return vec![];
    };

    let mut out: Vec<String> = entries
        .filter_map(|entry| {
            let entry = entry.ok()?;
            let path = entry.path();

            let mut name = entry.file_name().to_string_lossy().into_owned();

            // UX signal: mark directories
            if path.is_dir() {
                name.push('/');
            }

            Some(name)
        })
        .collect();

    // stable, predictable ordering for UI/completion
    out.sort_unstable();

    out
}
