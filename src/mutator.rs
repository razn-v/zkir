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

        for _ in 1..self.rng.rand(15, 30) {
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
                let expr = self.generate_expr(0);
                if matches!(expr, Expr::Nop) {
                    Instruction::Nop
                } else {
                    Instruction::ExprStmt(expr)
                }
            }
            1 => {
                // VarDecl
                Instruction::VarDecl(self.generate_var())
            }
            2 => {
                // If
                let cond = self.generate_expr(0);

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
                let init = if let Expr::Assign(varr, expr) = self.generate_assign(0) {
                    Expr::Assign(varr, expr)
                } else {
                    return Instruction::Nop;
                };
                let cond = self.generate_expr(0);
                let step = if let Expr::Assign(varr, expr) = self.generate_assign(0) {
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
                let cond = self.generate_expr(0);

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

    pub fn generate_expr(&mut self, depth: usize) -> Expr {
        /*if depth >= 10 {
            return Expr::Nop;
        }*/

        if self.rng.rand(1, 3) == 1 {
            return Expr::Constant(self.rng.next().to_string());
        }

        if self.rng.rand(1, 3) == 1 && !self.variables.is_empty() {
            let varref = self.rng.rand(0, self.curr_var_id.unwrap());
            return Expr::Var(VarRef(varref));
        }

        if self.rng.rand(1, 2) == 1 {
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
                let left = self.generate_expr(depth + 1);
                let right = self.generate_expr(depth + 1);

                if matches!(left, Expr::Nop) || matches!(right, Expr::Nop) {
                    return Expr::Nop;
                }

                Expr::Add(Box::new(left), Box::new(right))
            }
            4 => {
                // Sub
                let left = self.generate_expr(depth + 1);
                let right = self.generate_expr(depth + 1);

                if matches!(left, Expr::Nop) || matches!(right, Expr::Nop) {
                    return Expr::Nop;
                }

                Expr::Sub(Box::new(left), Box::new(right))
            }
            5 => {
                // Mul
                let left = self.generate_expr(depth + 1);
                let right = self.generate_expr(depth + 1);

                if matches!(left, Expr::Nop) || matches!(right, Expr::Nop) {
                    return Expr::Nop;
                }

                Expr::Mul(Box::new(left), Box::new(right))
            }
            6 => {
                // Power
                let left = self.generate_expr(depth + 1);
                let right = self.generate_expr(depth + 1);

                if matches!(left, Expr::Nop) || matches!(right, Expr::Nop) {
                    return Expr::Nop;
                }

                Expr::Power(Box::new(left), Box::new(right))
            }
            7 => {
                // Div
                let left = Box::new(self.generate_expr(depth + 1));
                let right = Box::new(self.generate_expr(depth + 1));
                Expr::Div(left, right)
            }
            8 => {
                // IntDiv
                let left = self.generate_expr(depth + 1);
                let right = self.generate_expr(depth + 1);

                if matches!(left, Expr::Nop) || matches!(right, Expr::Nop) {
                    return Expr::Nop;
                }

                Expr::IntDiv(Box::new(left), Box::new(right))
            }
            9 => {
                // Rem
                let left = self.generate_expr(depth + 1);
                let right = self.generate_expr(depth + 1);

                if matches!(left, Expr::Nop) || matches!(right, Expr::Nop) {
                    return Expr::Nop;
                }

                Expr::Rem(Box::new(left), Box::new(right))
            }
            10 | 11 => {
                // Assign
                self.generate_assign(depth + 1)
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
                    let val = Box::new(self.generate_expr(depth + 1));
                    Expr::Constraint(var_id.clone(), val)
                } else {
                    Expr::Nop
                }
            }
            13 => {
                // LessThan
                let left = self.generate_expr(depth + 1);
                let right = self.generate_expr(depth + 1);

                if matches!(left, Expr::Nop) || matches!(right, Expr::Nop) {
                    return Expr::Nop;
                }

                Expr::LessThan(Box::new(left), Box::new(right))
            }
            14 => {
                // LessThanEq
                let left = self.generate_expr(depth + 1);
                let right = self.generate_expr(depth + 1);

                if matches!(left, Expr::Nop) || matches!(right, Expr::Nop) {
                    return Expr::Nop;
                }

                Expr::LessThanEq(Box::new(left), Box::new(right))
            }
            15 => {
                // GreaterThan
                let left = self.generate_expr(depth + 1);
                let right = self.generate_expr(depth + 1);

                if matches!(left, Expr::Nop) || matches!(right, Expr::Nop) {
                    return Expr::Nop;
                }

                Expr::GreaterThan(Box::new(left), Box::new(right))
            }
            16 => {
                // GreaterThanEq
                let left = self.generate_expr(depth + 1);
                let right = self.generate_expr(depth + 1);

                if matches!(left, Expr::Nop) || matches!(right, Expr::Nop) {
                    return Expr::Nop;
                }

                Expr::GreaterThanEq(Box::new(left), Box::new(right))
            }
            17 => {
                // Equal
                let left = self.generate_expr(depth + 1);
                let right = self.generate_expr(depth + 1);

                if matches!(left, Expr::Nop) || matches!(right, Expr::Nop) {
                    return Expr::Nop;
                }

                Expr::Equal(Box::new(left), Box::new(right))
            }
            18 => {
                // And
                let left = self.generate_expr(depth + 1);
                let right = self.generate_expr(depth + 1);

                if matches!(left, Expr::Nop) || matches!(right, Expr::Nop) {
                    return Expr::Nop;
                }

                Expr::And(Box::new(left), Box::new(right))
            }
            19 => {
                // Or
                let left = self.generate_expr(depth + 1);
                let right = self.generate_expr(depth + 1);

                if matches!(left, Expr::Nop) || matches!(right, Expr::Nop) {
                    return Expr::Nop;
                }

                Expr::Or(Box::new(left), Box::new(right))
            }
            20 => {
                // Not
                let expr = self.generate_expr(depth + 1);
                if matches!(expr, Expr::Nop) {
                    return Expr::Nop;
                }
                Expr::Not(Box::new(expr))
            }
            21 => {
                // BitAnd
                let left = self.generate_expr(depth + 1);
                let right = self.generate_expr(depth + 1);

                if matches!(left, Expr::Nop) || matches!(right, Expr::Nop) {
                    return Expr::Nop;
                }

                Expr::BitAnd(Box::new(left), Box::new(right))
            }
            22 => {
                // BitOr
                let left = self.generate_expr(depth + 1);
                let right = self.generate_expr(depth + 1);

                if matches!(left, Expr::Nop) || matches!(right, Expr::Nop) {
                    return Expr::Nop;
                }

                Expr::BitOr(Box::new(left), Box::new(right))
            }
            23 => {
                // BitNot
                let expr = self.generate_expr(depth + 1);
                if matches!(expr, Expr::Nop) {
                    return Expr::Nop;
                }
                Expr::BitNot(Box::new(expr))
            }
            24 => {
                // BitXor
                let left = self.generate_expr(depth + 1);
                let right = self.generate_expr(depth + 1);

                if matches!(left, Expr::Nop) || matches!(right, Expr::Nop) {
                    return Expr::Nop;
                }

                Expr::BitXor(Box::new(left), Box::new(right))
            }
            25 => {
                // BitRShift
                let left = self.generate_expr(depth + 1);
                let right = self.generate_expr(depth + 1);

                if matches!(left, Expr::Nop) || matches!(right, Expr::Nop) {
                    return Expr::Nop;
                }

                Expr::BitRShift(Box::new(left), Box::new(right))
            }
            26 => {
                // BitLShift
                let left = self.generate_expr(depth + 1);
                let right = self.generate_expr(depth + 1);

                if matches!(left, Expr::Nop) || matches!(right, Expr::Nop) {
                    return Expr::Nop;
                }

                Expr::BitLShift(Box::new(left), Box::new(right))
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

    fn generate_assign(&mut self, depth: usize) -> Expr {
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
            let val = self.generate_expr(depth + 1);
            if !matches!(val, Expr::Nop) {
                return Expr::Assign(var_id.clone(), Box::new(val));
            }
        }

        Expr::Nop
    }
}
