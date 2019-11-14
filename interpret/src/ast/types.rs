
pub const PAGE_SIZE: usize = 1 << 16;

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

#[derive(Debug, Clone, PartialEq)]
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

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum ElementType {
    FunctionReference,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct GlobalType {
    pub ty: ValueType,
    pub mutability: Mutability,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Mutability {
    Constant,
    Variable,
}

impl FunctionType {
    pub fn matches(&self, other: &Self) -> bool {
        self == other
    }
}

impl TableType {
    pub fn matches(&self, other: &Self) -> bool {
        self.limits.matches(&other.limits) && self.element == other.element
    }
}

impl MemoryType {
    pub fn matches(&self, other: &Self) -> bool {
        self.limits.matches(&other.limits)
    }
}

impl GlobalType {
    pub fn matches(self, other: Self) -> bool {
        self == other
    }
}

impl Limits {
    pub fn matches(&self, other: &Self) -> bool {
        self.lower >= other.lower
            && match (self.upper, other.upper) {
                (_, None) => true,
                (Some(m1), Some(m2)) if m1 <= m2 => true,
                _ => false,
            }
    }
}

impl ResultType {
    pub fn arity(&self) -> u32 {
        if self.types.is_some() { 1 } else { 0 }
    }
}

