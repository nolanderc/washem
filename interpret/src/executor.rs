#![allow(dead_code)]

use crate::ast::types::*;

pub struct Executor {
    store: Store,
}

struct Store {
    functions: Vec<FunctionInstance>,
}

struct FunctionInstance {
    ty: FunctionType,
}
