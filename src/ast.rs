#[derive(Debug)]
pub struct VarRef(pub usize);

pub type Field = usize;

pub enum Expr {
    Var(VarRef),
    Constant(Field),

    Add(Box<Expr>, Box<Expr>),
    Sub(Box<Expr>, Box<Expr>),
    Mul(Box<Expr>, Box<Expr>),

    Assign(VarRef, Box<Expr>),
    Constrain(VarRef, Box<Expr>),

    LessThan(Box<Expr>, Box<Expr>),
    LessThanEq(Box<Expr>, Box<Expr>),
    GreaterThan(Box<Expr>, Box<Expr>),
    GreaterThanEq(Box<Expr>, Box<Expr>),
    Equal(Box<Expr>, Box<Expr>),

    And(Box<Expr>, Box<Expr>),
    Or(Box<Expr>, Box<Expr>),
    Not(Box<Expr>),
}

#[derive(Debug)]
pub enum VariableType {
    Field,
    Array(usize),
}

#[derive(Debug, PartialEq, Eq)]
pub enum SignalType {
    Input,
    Output,
}

#[derive(Debug, PartialEq, Eq)]
pub enum VariableScope {
    Signal(SignalType),
    Local,
}

#[derive(Debug)]
pub struct Variable {
    pub id: VarRef,
    pub name: String,
    pub _type: VariableType,
    pub scope: VariableScope,
}

pub enum Instruction {
    ExprStmt(Expr),
    If {
        cond: Expr,
        then_branch: Vec<Instruction>,
        else_branch: Option<Vec<Instruction>>,
    },
    For {
        init: Expr,
        cond: Expr,
        step: Expr,
        body: Vec<Instruction>,
    },
    While {
        cond: Expr,
        body: Vec<Instruction>,
    },
}

pub struct Circuit {
    pub variables: Vec<Variable>,
    pub instructions: Vec<Instruction>,
}
