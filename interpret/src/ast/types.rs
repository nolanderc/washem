#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum ValueType {
    I32,
    I64,
    F32,
    F64,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum ResultType {
    Unit,
    Value(ValueType),
}

#[derive(Debug)]
pub struct FunctionType {
    pub parameters: Vec<ValueType>,
    pub results: Vec<ValueType>,
}

#[derive(Debug)]
pub struct Limits {
    pub lower: u32,
    pub upper: Option<u32>,
}

#[derive(Debug)]
pub struct MemoryType {
    pub limits: Limits,
}

#[derive(Debug)]
pub struct TableType {
    pub element: ElementType,
    pub limits: Limits,
}

#[derive(Debug)]
pub enum ElementType {
    FunctionReference,
}

#[derive(Debug)]
pub struct GlobalType {
    pub ty: ValueType,
    pub mutability: Mutability,
}

#[derive(Debug)]
pub enum Mutability {
    Constant,
    Variable,
}
