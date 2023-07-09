use crate::value::OpError;
use macros::mux;
use std::error::Error;
use std::fmt::{Debug, Display, Formatter};

#[derive(Debug)]
#[mux]
pub enum RuntimeError {
    OpError(OpError),
}

impl Display for RuntimeError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        todo!()
    }
}

impl Error for RuntimeError {}
