#![allow(dead_code)]

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct VarRef(pub usize);

pub type Field = String;

#[derive(Debug, Clone)]
pub enum Expr {
    Var(VarRef),
    Constant(Field),
    ArrayIndex(VarRef, Box<Expr>),

    Add(Box<Expr>, Box<Expr>),
    Sub(Box<Expr>, Box<Expr>),
    Mul(Box<Expr>, Box<Expr>),
    Power(Box<Expr>, Box<Expr>),
    Div(Box<Expr>, Box<Expr>),
    IntDiv(Box<Expr>, Box<Expr>),
    Rem(Box<Expr>, Box<Expr>),

    LessThan(Box<Expr>, Box<Expr>),
    LessThanEq(Box<Expr>, Box<Expr>),
    GreaterThan(Box<Expr>, Box<Expr>),
    GreaterThanEq(Box<Expr>, Box<Expr>),
    Equal(Box<Expr>, Box<Expr>),

    And(Box<Expr>, Box<Expr>),
    Or(Box<Expr>, Box<Expr>),
    Not(Box<Expr>),

    BitAnd(Box<Expr>, Box<Expr>),
    BitOr(Box<Expr>, Box<Expr>),
    BitNot(Box<Expr>),
    BitXor(Box<Expr>, Box<Expr>),
    BitRShift(Box<Expr>, Box<Expr>),
    BitLShift(Box<Expr>, Box<Expr>),
}

impl Expr {
    pub const EXPR_COUNT: usize = std::mem::variant_count::<Expr>();
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum VariableType {
    Field,
    Array(usize),
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum SignalType {
    Input,
    Output,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum VariableRole {
    Signal(SignalType),
    Local,
}

#[derive(Debug, Clone)]
pub struct Variable {
    pub id: VarRef,
    pub name: String,
    pub var_type: VariableType,
    pub role: VariableRole,
    pub initialized: bool,
}

#[derive(Clone)]
pub enum Instruction {
    VarDecl(Variable),
    Assign(VarRef, Box<Expr>),
    ArrayAssign(VarRef, Box<Expr>, Box<Expr>),
    Constraint(VarRef, Box<Expr>),
    If {
        cond: Expr,
        then_branch: Vec<Instruction>,
        else_branch: Option<Vec<Instruction>>,
    },
    For {
        init: Box<Instruction>,
        cond: Expr,
        step: Box<Instruction>,
        body: Vec<Instruction>,
    },
    While {
        cond: Expr,
        body: Vec<Instruction>,
    },
}

impl Instruction {
    pub const INSTRUCTION_COUNT: usize = std::mem::variant_count::<Instruction>();
}

pub struct Circuit {
    pub variables: Vec<Variable>,
    pub instructions: Vec<Instruction>,
}

impl Circuit {
    pub fn new(variables: Vec<Variable>, instructions: Vec<Instruction>) -> Self {
        Self {
            variables,
            instructions,
        }
    }

    pub fn get_variable(&self, varref: &VarRef) -> &Variable {
        self.variables
            .iter()
            .find(|var| var.id == *varref)
            .expect(&format!("Variable {} not found", varref.0))
    }
}
