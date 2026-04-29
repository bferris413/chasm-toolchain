//! Command input functionality.

use std::fmt;

pub (super) struct CommandBuffer {
    is_active: bool,
    buffer: String,
}
impl CommandBuffer {
    pub fn new() -> Self {
        Self {
            is_active: false,
            buffer: String::new(),
        }
    }
    pub fn is_active(&self) -> bool {
        self.is_active
    }
    pub fn activate(&mut self) {
        self.is_active = true;
    }
    pub fn deactivate(&mut self) {
        self.is_active = false;
        self.buffer.clear();
    }
    pub fn push(&mut self, c: char) {
        self.buffer.push(c);
    }
    pub fn pop(&mut self) -> Option<char> {
        self.buffer.pop()
    }
    pub fn current_input(&self) -> CommandInput<'_> {
        CommandInput {
            prefix: ":",
            input: &self.buffer,
        }
    }

    pub fn get(&self) -> Result<Command, String> {
        match self.buffer.as_str() {
            "w" | "write" => Ok(Command::Save),
            "q" | "quit" => Ok(Command::Quit),
            "" => Err("No command entered".to_string()),
            cmd => Err(format!("Unknown command: '{cmd}'")),
        }
    }
}

pub (super) enum Command {
    Save,
    Quit,
}

pub (super) struct CommandInput<'a> {
    prefix: &'a str,
    input: &'a str,
}
impl fmt::Display for CommandInput<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}{}", self.prefix, self.input)
    }
}