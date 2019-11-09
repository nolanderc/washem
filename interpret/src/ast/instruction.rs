use super::indices::*;
use super::types::*;

#[derive(Debug)]
pub enum Instruction {
    // Control
    Unreachable,
    Nop,

    Block {
        result: ResultType,
        instructions: Vec<Instruction>,
    },

    Loop {
        result: ResultType,
        instructions: Vec<Instruction>,
    },

    Conditional {
        result: ResultType,
        success: Vec<Instruction>,
        failure: Vec<Instruction>,
    },

    Branch(LabelIndex),
    BranchConditional(LabelIndex),
    BranchTable {
        targets: Vec<LabelIndex>,
        default: LabelIndex,
    },

    Return,
    Call(FunctionIndex),
    CallIndirect(TypeIndex),

    // Parametric
    Drop,
    Select,

    // Variable
    LocalGet(LocalIndex),
    LocalSet(LocalIndex),
    LocalTee(LocalIndex),
    GlobalGet(GlobalIndex),
    GlobalSet(GlobalIndex),

    // Memory
    I32Load(MemoryArgument),
    I64Load(MemoryArgument),
    F32Load(MemoryArgument),
    F64Load(MemoryArgument),
    I32Load8Signed(MemoryArgument),
    I32Load8Unsigned(MemoryArgument),
    I32Load16Signed(MemoryArgument),
    I32Load16Unsigned(MemoryArgument),
    I64Load8Signed(MemoryArgument),
    I64Load8Unsigned(MemoryArgument),
    I64Load16Signed(MemoryArgument),
    I64Load16Unsigned(MemoryArgument),
    I64Load32Signed(MemoryArgument),
    I64Load32Unsigned(MemoryArgument),
    I32Store(MemoryArgument),
    I64Store(MemoryArgument),
    F32Store(MemoryArgument),
    F64Store(MemoryArgument),
    I32Store8(MemoryArgument),
    I32Store16(MemoryArgument),
    I64Store8(MemoryArgument),
    I64Store16(MemoryArgument),
    I64Store32(MemoryArgument),
    MemorySize,
    MemoryGrow,

    // Numeric
    I32Const(i32),
    I64Const(i64),
    F32Const(f32),
    F64Const(f64),

    I32EqualZero,
    I32Equal,
    I32NotEqual,
    I32LessThanSigned,
    I32LessThanUnsigned,
    I32GreaterThanSigned,
    I32GreaterThanUnsigned,
    I32LessEqualSigned,
    I32LessEqualUnsigned,
    I32GreaterEqualSigned,
    I32GreaterEqualUnsigned,

    I64EqualZero,
    I64Equal,
    I64NotEqual,
    I64LessThanSigned,
    I64LessThanUnsigned,
    I64GreaterThanSigned,
    I64GreaterThanUnsigned,
    I64LessEqualSigned,
    I64LessEqualUnsigned,
    I64GreaterEqualSigned,
    I64GreaterEqualUnsigned,

    F32Equal,
    F32NotEqual,
    F32LessThan,
    F32GreaterThan,
    F32LessEqual,
    F32GreaterEqual,

    F64Equal,
    F64NotEqual,
    F64LessThan,
    F64GreaterThan,
    F64LessEqual,
    F64GreaterEqual,

    I32LeadingZeros,
    I32TrailingZeros,
    I32CountOnes,
    I32Add,
    I32Sub,
    I32Mul,
    I32DivSigned,
    I32DivUnsigned,
    I32RemainderSigned,
    I32RemainderUnsigned,
    I32And,
    I32Or,
    I32Xor,
    I32ShiftLeft,
    I32ShiftRightSigned,
    I32ShiftRightUnsigned,
    I32RotateLeft,
    I32RotateRight,

    I64LeadingZeros,
    I64TrailingZeros,
    I64CountOnes,
    I64Add,
    I64Sub,
    I64Mul,
    I64DivSigned,
    I64DivUnsigned,
    I64RemainderSigned,
    I64RemainderUnsigned,
    I64And,
    I64Or,
    I64Xor,
    I64ShiftLeft,
    I64ShiftRightSigned,
    I64ShiftRightUnsigned,
    I64RotateLeft,
    I64RotateRight,

    F32Abs,
    F32Neg,
    F32Ceil,
    F32Floor,
    F32Trunc,
    F32Nearest,
    F32Sqrt,
    F32Add,
    F32Sub,
    F32Mul,
    F32Div,
    F32Min,
    F32Max,
    F32Copysign,

    F64Abs,
    F64Neg,
    F64Ceil,
    F64Floor,
    F64Trunc,
    F64Nearest,
    F64Sqrt,
    F64Add,
    F64Sub,
    F64Mul,
    F64Div,
    F64Min,
    F64Max,
    F64Copysign,

    I32WrapI64,
    I32TruncF32Signed,
    I32TruncF32Unsigned,
    I32TruncF64Signed,
    I32TruncF64Unsigned,
    I64ExtendI32Signed,
    I64ExtendI32Unsigned,
    I64TruncF32Signed,
    I64TruncF32Unsigned,
    I64TruncF64Signed,
    I64TruncF64Unsigned,
    F32ConvertI32Signed,
    F32ConvertI32Unsigned,
    F32ConvertI64Signed,
    F32ConvertI64Unsigned,
    F32DemoteF64,
    F64ConvertI32Signed,
    F64ConvertI32Unsigned,
    F64ConvertI64Signed,
    F64ConvertI64Unsigned,
    F64PromoteF32,
    I32ReinterpretF32,
    I64ReinterpretF64,
    F32ReinterpretI32,
    F64ReinterpretI64,
}

#[derive(Debug)]
pub enum InstructionClass<'a> {
    // Numeric
    Constant(ValueType),
    Unary(ValueType),
    Binary(ValueType),
    Test(ValueType),
    Comparison(ValueType),
    Conversion {
        source: ValueType,
        target: ValueType,
    },

    // Parametric
    Drop,
    Select,

    // Variable
    LocalGet(LocalIndex),
    LocalSet(LocalIndex),
    LocalTee(LocalIndex),
    GlobalGet(GlobalIndex),
    GlobalSet(GlobalIndex),

    // Memory
    Load {
        value: ValueType,
        memory: MemoryArgument,
        bit_width: u8,
    },
    Store {
        value: ValueType,
        memory: MemoryArgument,
        bit_width: u8,
    },
    MemorySize,
    MemoryGrow,

    // Control
    Nop,
    Unreachable,
    Block {
        result: &'a ResultType,
        instructions: &'a [Instruction],
    },
    Loop {
        result: &'a ResultType,
        instructions: &'a [Instruction],
    },
    Conditional {
        result: &'a ResultType,
        success: &'a [Instruction],
        failure: &'a [Instruction],
    },
    Branch(LabelIndex),
    BranchConditional(LabelIndex),
    BranchTable {
        targets: &'a [LabelIndex],
        default: LabelIndex,
    },
    Return,
    Call(FunctionIndex),
    CallIndirect(TypeIndex),
}

#[derive(Debug, Copy, Clone)]
pub struct MemoryArgument {
    pub align: u32,
    pub offset: u32,
}

impl Instruction {
    pub fn class(&self) -> InstructionClass {
        use Instruction::*;
        use InstructionClass as Class;
        use ValueType::*;

        match self {
            I32Const(_) => Class::Constant(I32),
            I64Const(_) => Class::Constant(I64),
            F32Const(_) => Class::Constant(F32),
            F64Const(_) => Class::Constant(F64),

            I32LeadingZeros | I32TrailingZeros | I32CountOnes => Class::Unary(I32),
            I64LeadingZeros | I64TrailingZeros | I64CountOnes => Class::Unary(I64),
            F32Abs | F32Neg | F32Ceil | F32Floor | F32Trunc | F32Nearest | F32Sqrt => {
                Class::Unary(F32)
            }
            F64Abs | F64Neg | F64Ceil | F64Floor | F64Trunc | F64Nearest | F64Sqrt => {
                Class::Unary(F64)
            }

            I32Add
            | I32Sub
            | I32Mul
            | I32DivSigned
            | I32DivUnsigned
            | I32RemainderSigned
            | I32RemainderUnsigned
            | I32And
            | I32Or
            | I32Xor
            | I32ShiftLeft
            | I32ShiftRightSigned
            | I32ShiftRightUnsigned
            | I32RotateLeft
            | I32RotateRight => Class::Binary(I32),

            I64Add
            | I64Sub
            | I64Mul
            | I64DivSigned
            | I64DivUnsigned
            | I64RemainderSigned
            | I64RemainderUnsigned
            | I64And
            | I64Or
            | I64Xor
            | I64ShiftLeft
            | I64ShiftRightSigned
            | I64ShiftRightUnsigned
            | I64RotateLeft
            | I64RotateRight => Class::Binary(I64),

            F32Add | F32Sub | F32Mul | F32Div | F32Min | F32Max | F32Copysign => Class::Binary(F32),
            F64Add | F64Sub | F64Mul | F64Div | F64Min | F64Max | F64Copysign => Class::Binary(F64),

            I32EqualZero => Class::Test(I32),
            I64EqualZero => Class::Test(I64),

            I32Equal
            | I32NotEqual
            | I32LessThanSigned
            | I32LessThanUnsigned
            | I32GreaterThanSigned
            | I32GreaterThanUnsigned
            | I32LessEqualSigned
            | I32LessEqualUnsigned
            | I32GreaterEqualSigned
            | I32GreaterEqualUnsigned => Class::Comparison(I32),

            I64Equal
            | I64NotEqual
            | I64LessThanSigned
            | I64LessThanUnsigned
            | I64GreaterThanSigned
            | I64GreaterThanUnsigned
            | I64LessEqualSigned
            | I64LessEqualUnsigned
            | I64GreaterEqualSigned
            | I64GreaterEqualUnsigned => Class::Comparison(I64),

            F32Equal | F32NotEqual | F32LessThan | F32GreaterThan | F32LessEqual
            | F32GreaterEqual => Class::Comparison(F32),

            F64Equal | F64NotEqual | F64LessThan | F64GreaterThan | F64LessEqual
            | F64GreaterEqual => Class::Comparison(F64),

            I32WrapI64 => Class::Conversion {
                source: I64,
                target: I32,
            },
            I32TruncF32Signed => Class::Conversion {
                source: F32,
                target: I32,
            },
            I32TruncF32Unsigned => Class::Conversion {
                source: F32,
                target: I32,
            },
            I32TruncF64Signed => Class::Conversion {
                source: F64,
                target: I32,
            },
            I32TruncF64Unsigned => Class::Conversion {
                source: F64,
                target: I32,
            },
            I64ExtendI32Signed => Class::Conversion {
                source: I32,
                target: I64,
            },
            I64ExtendI32Unsigned => Class::Conversion {
                source: I32,
                target: I64,
            },
            I64TruncF32Signed => Class::Conversion {
                source: F32,
                target: I64,
            },
            I64TruncF32Unsigned => Class::Conversion {
                source: F32,
                target: I64,
            },
            I64TruncF64Signed => Class::Conversion {
                source: F64,
                target: I64,
            },
            I64TruncF64Unsigned => Class::Conversion {
                source: F64,
                target: I64,
            },
            F32ConvertI32Signed => Class::Conversion {
                source: I32,
                target: F32,
            },
            F32ConvertI32Unsigned => Class::Conversion {
                source: I32,
                target: F32,
            },
            F32ConvertI64Signed => Class::Conversion {
                source: I64,
                target: F32,
            },
            F32ConvertI64Unsigned => Class::Conversion {
                source: I64,
                target: F32,
            },
            F32DemoteF64 => Class::Conversion {
                source: F64,
                target: F32,
            },
            F64ConvertI32Signed => Class::Conversion {
                source: I32,
                target: F64,
            },
            F64ConvertI32Unsigned => Class::Conversion {
                source: I32,
                target: F64,
            },
            F64ConvertI64Signed => Class::Conversion {
                source: I64,
                target: F64,
            },
            F64ConvertI64Unsigned => Class::Conversion {
                source: I64,
                target: F64,
            },
            F64PromoteF32 => Class::Conversion {
                source: F32,
                target: F64,
            },
            I32ReinterpretF32 => Class::Conversion {
                source: F32,
                target: I32,
            },
            I64ReinterpretF64 => Class::Conversion {
                source: F64,
                target: I64,
            },
            F32ReinterpretI32 => Class::Conversion {
                source: I32,
                target: F32,
            },
            F64ReinterpretI64 => Class::Conversion {
                source: I64,
                target: F64,
            },

            Drop => Class::Drop,
            Select => Class::Select,

            LocalGet(index) => Class::LocalGet(*index),
            LocalSet(index) => Class::LocalSet(*index),
            LocalTee(index) => Class::LocalTee(*index),
            GlobalGet(index) => Class::GlobalGet(*index),
            GlobalSet(index) => Class::GlobalSet(*index),

            I32Load(memarg) => Class::Load {
                value: I32,
                memory: *memarg,
                bit_width: 32,
            },
            I64Load(memarg) => Class::Load {
                value: I64,
                memory: *memarg,
                bit_width: 64,
            },
            F32Load(memarg) => Class::Load {
                value: F32,
                memory: *memarg,
                bit_width: 32,
            },
            F64Load(memarg) => Class::Load {
                value: F64,
                memory: *memarg,
                bit_width: 64,
            },
            I32Load8Signed(memarg) => Class::Load {
                value: I32,
                memory: *memarg,
                bit_width: 8,
            },
            I32Load8Unsigned(memarg) => Class::Load {
                value: I32,
                memory: *memarg,
                bit_width: 8,
            },
            I32Load16Signed(memarg) => Class::Load {
                value: I32,
                memory: *memarg,
                bit_width: 16,
            },
            I32Load16Unsigned(memarg) => Class::Load {
                value: I32,
                memory: *memarg,
                bit_width: 16,
            },
            I64Load8Signed(memarg) => Class::Load {
                value: I64,
                memory: *memarg,
                bit_width: 8,
            },
            I64Load8Unsigned(memarg) => Class::Load {
                value: I64,
                memory: *memarg,
                bit_width: 8,
            },
            I64Load16Signed(memarg) => Class::Load {
                value: I64,
                memory: *memarg,
                bit_width: 16,
            },
            I64Load16Unsigned(memarg) => Class::Load {
                value: I64,
                memory: *memarg,
                bit_width: 16,
            },
            I64Load32Signed(memarg) => Class::Load {
                value: I64,
                memory: *memarg,
                bit_width: 32,
            },
            I64Load32Unsigned(memarg) => Class::Load {
                value: I64,
                memory: *memarg,
                bit_width: 32,
            },
            I32Store(memarg) => Class::Store {
                value: I32,
                memory: *memarg,
                bit_width: 32,
            },
            I64Store(memarg) => Class::Store {
                value: I64,
                memory: *memarg,
                bit_width: 64,
            },
            F32Store(memarg) => Class::Store {
                value: F32,
                memory: *memarg,
                bit_width: 32,
            },
            F64Store(memarg) => Class::Store {
                value: F64,
                memory: *memarg,
                bit_width: 64,
            },
            I32Store8(memarg) => Class::Store {
                value: I32,
                memory: *memarg,
                bit_width: 8,
            },
            I32Store16(memarg) => Class::Store {
                value: I32,
                memory: *memarg,
                bit_width: 16,
            },
            I64Store8(memarg) => Class::Store {
                value: I64,
                memory: *memarg,
                bit_width: 8,
            },
            I64Store16(memarg) => Class::Store {
                value: I64,
                memory: *memarg,
                bit_width: 16,
            },
            I64Store32(memarg) => Class::Store {
                value: I64,
                memory: *memarg,
                bit_width: 32,
            },

            MemorySize => Class::MemorySize,
            MemoryGrow => Class::MemoryGrow,

            Nop => Class::Nop,
            Unreachable => Class::Unreachable,

            Block {
                result,
                instructions,
            } => Class::Block {
                result: &result,
                instructions: &instructions,
            },
            Loop {
                result,
                instructions,
            } => Class::Loop {
                result: &result,
                instructions: &instructions,
            },

            Conditional {
                result,
                success,
                failure,
            } => Class::Conditional {
                result: &result,
                success: &success,
                failure: &failure,
            },

            Branch(label) => Class::Branch(*label),
            BranchConditional(label) => Class::BranchConditional(*label),
            BranchTable { targets, default } => Class::BranchTable {
                targets: &targets,
                default: *default,
            },

            Return => Class::Return,
            Call(func) => Class::Call(*func),
            CallIndirect(ty) => Class::CallIndirect(*ty),
        }
    }
}
