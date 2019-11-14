#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct FunctionAddress(pub(super) u32);

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct TableAddress(pub(super) u32);

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct MemoryAddress(pub(super) u32);

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct GlobalAddress(pub(super) u32);
