#[derive(Debug)]
pub enum CompileError {
    IdentAlreadyExists,
    CantFindIdent,
    TooManyIdent,
    NotInLoop,
    LabelNotExists,
    LabelDefinedMultiTimes,
}

pub type Result<T> = std::result::Result<T, CompileError>;
