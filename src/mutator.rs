use crate::{
    ast::{Circuit, Expr, Instruction, SignalType, VarRef, Variable, VariableRole, VariableType},
    rng::Rng,
};

pub struct Mutator {
    rng: Rng,
    curr_var_id: Option<usize>,
    n_instr: usize,
    variables: Vec<Variable>,
}

impl Mutator {
    pub fn new(rng: Rng) -> Self {
        Self {
            rng,
            curr_var_id: None,
            n_instr: 0,
            variables: Vec::new(),
        }
    }

    pub fn generate(&mut self) -> Circuit {
        let mut instructions = Vec::new();

        for _ in 1..self.rng.rand(1, 25) {
            instructions.push(self.generate_instr());
        }

        Circuit::new(self.variables.clone(), instructions)
    }

    pub fn generate_instr(&mut self) -> Instruction {
        if self.n_instr >= 100 {
            return Instruction::Nop;
        }

        self.n_instr += 1;

        let instr: Instruction = match self.rng.rand(0, Instruction::INSTRUCTION_COUNT - 1) {
            0 => {
                // ExprStmt
                Instruction::ExprStmt(self.generate_expr())
            }
            1 => {
                // VarDecl
                Instruction::VarDecl(self.generate_var())
            }
            2 => {
                // If
                let cond = self.generate_expr();

                let mut then_branch = Vec::<Instruction>::new();
                for _ in 1..self.rng.rand(1, 5) {
                    then_branch.push(self.generate_instr());
                }

                let mut else_branch: Option<Vec<Instruction>> = None;
                // Add an else branch half of the time
                if self.rng.rand(0, 1) == 0 {
                    let mut instrs = Vec::<Instruction>::new();
                    for _ in 1..self.rng.rand(1, 5) {
                        instrs.push(self.generate_instr());
                    }
                    else_branch = Some(instrs);
                }

                Instruction::If {
                    cond: cond,
                    then_branch: then_branch,
                    else_branch: else_branch,
                }
            }
            3 => {
                // For
                let init = if let Expr::Assign(varr, expr) = self.generate_assign() {
                    Expr::Assign(varr, expr)
                } else {
                    return Instruction::Nop;
                };
                let cond = self.generate_expr();
                let step = if let Expr::Assign(varr, expr) = self.generate_assign() {
                    Expr::Assign(varr, expr)
                } else {
                    return Instruction::Nop;
                };

                let mut body = Vec::<Instruction>::new();
                for _ in 1..self.rng.rand(1, 5) {
                    body.push(self.generate_instr());
                }

                if body.iter().all(|x| matches!(x, Instruction::Nop)) {
                    return Instruction::Nop;
                }

                Instruction::For {
                    init: init,
                    cond: cond,
                    step: step,
                    body: body,
                }
            }
            4 => {
                // While
                let cond = self.generate_expr();

                let mut body = Vec::<Instruction>::new();
                for _ in 1..self.rng.rand(1, 5) {
                    body.push(self.generate_instr());
                }

                if body.iter().all(|x| matches!(x, Instruction::Nop)) {
                    return Instruction::Nop;
                }

                Instruction::While {
                    cond: cond,
                    body: body,
                }
            }
            5 => {
                // Nop
                Instruction::Nop
            }
            _ => unreachable!(),
        };

        instr
    }

    pub fn generate_expr(&mut self) -> Expr {
        if self.rng.rand(0, 1) == 0 {
            return Expr::Nop;
        }

        match self.rng.rand(1, Expr::EXPR_COUNT - 1) {
            0 => {
                // Var
                if let Some(varid) = self.curr_var_id {
                    let varref = self.rng.rand(0, varid);
                    Expr::Var(VarRef(varref))
                } else {
                    Expr::Nop
                }
            }
            1 | 2 => {
                // Constant
                Expr::Constant(self.rng.next().to_string())
            }
            /*2 => {
                // ArrayIndex
                todo!();
            }*/
            3 => {
                // Add
                let left = Box::new(self.generate_expr());
                let right = Box::new(self.generate_expr());
                Expr::Add(left, right)
            }
            4 => {
                // Sub
                let left = Box::new(self.generate_expr());
                let right = Box::new(self.generate_expr());
                Expr::Sub(left, right)
            }
            5 => {
                // Mul
                let left = Box::new(self.generate_expr());
                let right = Box::new(self.generate_expr());
                Expr::Mul(left, right)
            }
            6 => {
                // Power
                let left = Box::new(self.generate_expr());
                let right = Box::new(self.generate_expr());
                Expr::Power(left, right)
            }
            7 => {
                // Div
                let left = Box::new(self.generate_expr());
                let right = Box::new(self.generate_expr());
                Expr::Div(left, right)
            }
            8 => {
                // IntDiv
                let left = Box::new(self.generate_expr());
                let right = Box::new(self.generate_expr());
                Expr::IntDiv(left, right)
            }
            9 => {
                // Rem
                let left = Box::new(self.generate_expr());
                let right = Box::new(self.generate_expr());
                Expr::Rem(left, right)
            }
            10 | 11 => {
                // Assign
                self.generate_assign()
            }
            /*11 => {
                // ArrayAssign
                //todo!();
            }*/
            12 => {
                // Constraint
                let vars: Vec<VarRef> = self
                    .variables
                    .iter()
                    .filter(|var| matches!(var.role, VariableRole::Signal(SignalType::Output)))
                    .map(|var| var.id.clone())
                    .collect();

                if let Some(var_id) = self.rng.rand_vec(&vars) {
                    let val = Box::new(self.generate_expr());
                    Expr::Constraint(var_id.clone(), val)
                } else {
                    Expr::Nop
                }
            }
            13 => {
                // LessThan
                let left = Box::new(self.generate_expr());
                let right = Box::new(self.generate_expr());
                Expr::LessThan(left, right)
            }
            14 => {
                // LessThanEq
                let left = Box::new(self.generate_expr());
                let right = Box::new(self.generate_expr());
                Expr::LessThanEq(left, right)
            }
            15 => {
                // GreaterThan
                let left = Box::new(self.generate_expr());
                let right = Box::new(self.generate_expr());
                Expr::GreaterThan(left, right)
            }
            16 => {
                // GreaterThanEq
                let left = Box::new(self.generate_expr());
                let right = Box::new(self.generate_expr());
                Expr::GreaterThanEq(left, right)
            }
            17 => {
                // Equal
                let left = Box::new(self.generate_expr());
                let right = Box::new(self.generate_expr());
                Expr::Equal(left, right)
            }
            18 => {
                // And
                let left = Box::new(self.generate_expr());
                let right = Box::new(self.generate_expr());
                Expr::And(left, right)
            }
            19 => {
                // Or
                let left = Box::new(self.generate_expr());
                let right = Box::new(self.generate_expr());
                Expr::Or(left, right)
            }
            20 => {
                // Not
                Expr::Not(Box::new(self.generate_expr()))
            }
            21 => {
                // BitAnd
                let left = Box::new(self.generate_expr());
                let right = Box::new(self.generate_expr());
                Expr::BitAnd(left, right)
            }
            22 => {
                // BitOr
                let left = Box::new(self.generate_expr());
                let right = Box::new(self.generate_expr());
                Expr::BitOr(left, right)
            }
            23 => {
                // BitNot
                Expr::BitNot(Box::new(self.generate_expr()))
            }
            24 => {
                // BitXor
                let left = Box::new(self.generate_expr());
                let right = Box::new(self.generate_expr());
                Expr::BitXor(left, right)
            }
            25 => {
                // BitRShift
                let left = Box::new(self.generate_expr());
                let right = Box::new(self.generate_expr());
                Expr::BitRShift(left, right)
            }
            26 => {
                // BitLShift
                let left = Box::new(self.generate_expr());
                let right = Box::new(self.generate_expr());
                Expr::BitLShift(left, right)
            }
            27 => Expr::Nop,
            _ => unreachable!(),
        }
    }

    pub fn generate_var(&mut self) -> Variable {
        let var_type: VariableType;
        if self.rng.rand(0, 1) == 0 {
            var_type = VariableType::Field;
        } else {
            var_type = VariableType::Array(self.rng.rand(1, 10));
        }

        let role: VariableRole;
        if self.rng.rand(0, 1) == 0 {
            if self.rng.rand(0, 1) == 0 {
                role = VariableRole::Signal(SignalType::Input);
            } else {
                role = VariableRole::Signal(SignalType::Output);
            }
        } else {
            role = VariableRole::Local;
        }

        if self.curr_var_id.is_none() {
            self.curr_var_id = Some(0);
        } else {
            *self.curr_var_id.as_mut().unwrap() += 1;
        }

        let var = Variable {
            id: VarRef(self.curr_var_id.unwrap()),
            name: self.rng.rand_string(8),
            var_type,
            role,
        };

        self.variables.push(var.clone());
        println!("{:?}", var);

        var
    }

    fn generate_assign(&mut self) -> Expr {
        let vars: Vec<VarRef> = self
            .variables
            .iter()
            .filter(|var| {
                matches!(var.role, VariableRole::Local)
                    && matches!(var.var_type, VariableType::Field)
            })
            .map(|var| var.id.clone())
            .collect();

        if let Some(var_id) = self.rng.rand_vec(&vars) {
            let val = Box::new(self.generate_expr());
            println!("{:?}", Expr::Assign(var_id.clone(), val.clone()));
            Expr::Assign(var_id.clone(), val)
        } else {
            Expr::Nop
        }
    }
}
