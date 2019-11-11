#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct FunctionAddress(pub u32);

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct TableAddress(pub u32);

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct MemoryAddress(pub u32);

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct GlobalAddress(pub u32);
