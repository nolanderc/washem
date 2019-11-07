pub mod expression;
pub mod indices;
pub mod instruction;
pub mod sections;
pub mod types;

use self::sections::*;

#[derive(Debug)]
pub struct Module {
    pub sections: ModuleSections,
}

#[derive(Debug)]
pub struct ModuleSections {
    pub pre_types: Option<CustomSection>,
    pub types: Option<TypeSection>,
    pub pre_import: Option<CustomSection>,
    pub import: Option<ImportSection>,
    pub pre_function: Option<CustomSection>,
    pub function: Option<FunctionSection>,
    pub pre_table: Option<CustomSection>,
    pub table: Option<TableSection>,
    pub pre_memory: Option<CustomSection>,
    pub memory: Option<MemorySection>,
    pub pre_global: Option<CustomSection>,
    pub global: Option<GlobalSection>,
    pub pre_export: Option<CustomSection>,
    pub export: Option<ExportSection>,
    pub pre_start: Option<CustomSection>,
    pub start: Option<StartSection>,
    pub pre_element: Option<CustomSection>,
    pub element: Option<ElementSection>,
    pub pre_code: Option<CustomSection>,
    pub code: Option<CodeSection>,
    pub pre_data: Option<CustomSection>,
    pub data: Option<DataSection>,
}
