#![allow(dead_code)]

#[derive(Debug, Clone)]
pub struct VarRef {
    // Unique id used by the variable
    pub id: usize,
    // Used to specify a certain index for arrays.
    // An array can never be referenced as a whole without a specific index.
    pub idx: Option<usize>,
}

pub type Field = String;

#[derive(Debug, Clone)]
pub enum Expr {
    Var(VarRef),
    Constant(Field),

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
    //Not(Box<Expr>),
    BitAnd(Box<Expr>, Box<Expr>),
    BitOr(Box<Expr>, Box<Expr>),
    BitNot(Box<Expr>),
    BitXor(Box<Expr>, Box<Expr>),
    BitRShift(Box<Expr>, Box<Expr>),
    BitLShift(Box<Expr>, Box<Expr>),
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
    pub var_ref: VarRef,
    pub name: String,
    pub var_type: VariableType,
    pub role: VariableRole,
    pub initialized: Vec<bool>,
}

impl Variable {
    pub fn all_initialized(&self) -> bool {
        self.initialized.iter().all(|&x| x)
    }

    pub fn any_initialized(&self) -> bool {
        self.initialized.iter().any(|&x| x)
    }

    // Return only initialized indices
    pub fn get_initialized(&self) -> Vec<usize> {
        self.initialized
            .iter()
            .enumerate()
            .filter_map(|(i, &x)| if x { Some(i) } else { None })
            .collect()
    }
}

#[derive(Debug, Clone)]
pub enum Instruction {
    VarDecl(Variable, Option<Expr>),
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

    pub fn get_variable(&self, var_ref: &VarRef) -> &Variable {
        self.variables
            .iter()
            .find(|var| var.var_ref.id == var_ref.id)
            .expect(&format!("Variable {} not found", var_ref.id))
    }
}
