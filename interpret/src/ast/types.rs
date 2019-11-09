#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum ValueType {
    I32,
    I64,
    F32,
    F64,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ResultType {
    pub types: Option<ValueType>,
}

#[derive(Debug, Clone)]
pub struct FunctionType {
    pub parameters: Vec<ValueType>,
    pub results: Vec<ValueType>,
}

#[derive(Debug, Copy, Clone)]
pub struct Limits {
    pub lower: u32,
    pub upper: Option<u32>,
}

#[derive(Debug, Copy, Clone)]
pub struct MemoryType {
    pub limits: Limits,
}

#[derive(Debug, Copy, Clone)]
pub struct TableType {
    pub element: ElementType,
    pub limits: Limits,
}

#[derive(Debug, Copy, Clone)]
pub enum ElementType {
    FunctionReference,
}

#[derive(Debug, Copy, Clone)]
pub struct GlobalType {
    pub ty: ValueType,
    pub mutability: Mutability,
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum Mutability {
    Constant,
    Variable,
}
