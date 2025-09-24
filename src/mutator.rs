use crate::{
    ast::{Circuit, Expr, Instruction, SignalType, VarRef, Variable, VariableRole, VariableType},
    rng::Rng,
};

macro_rules! gen_bin_expr {
    ($self:ident, $depth:ident, $op:path) => {{
        let left = $self.generate_expr($depth + 1);
        let right = $self.generate_expr($depth + 1);

        if left.is_none() || right.is_none() {
            return None;
        }

        Some($op(Box::new(left.unwrap()), Box::new(right.unwrap())))
    }};
}

macro_rules! gen_un_expr {
    ($self:ident, $depth:ident, $op:path) => {{
        let expr = $self.generate_expr($depth + 1);
        if expr.is_none() {
            return None;
        }

        Some($op(Box::new(expr.unwrap())))
    }};
}

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
            let instr = self.generate_instr();
            if let Some(instr) = instr {
                instructions.push(instr);
            }
        }

        Circuit::new(self.variables.clone(), instructions)
    }

    pub fn generate_instr(&mut self) -> Option<Instruction> {
        if self.n_instr >= 100 {
            return None;
        }

        self.n_instr += 1;

        let instr: Option<Instruction> = match self.rng.rand(0, Instruction::INSTRUCTION_COUNT - 1)
        {
            0 => {
                // ExprStmt
                let expr = self.generate_expr(0);
                if let Some(expr) = expr {
                    Some(Instruction::ExprStmt(expr))
                } else {
                    None
                }
            }
            1 => {
                // VarDecl
                Some(Instruction::VarDecl(self.generate_var()))
            }
            2 => {
                // If
                let cond = self.generate_expr(0);
                if cond.is_none() {
                    return None;
                }

                let mut then_branch = Vec::<Instruction>::new();
                for _ in 1..self.rng.rand(1, 5) {
                    let instr = self.generate_instr();
                    if let Some(instr) = instr {
                        then_branch.push(instr);
                    }
                }

                let mut else_branch: Option<Vec<Instruction>> = None;
                // Add an else branch half of the time
                if self.rng.rand(0, 1) == 0 {
                    let mut instrs = Vec::<Instruction>::new();
                    for _ in 1..self.rng.rand(1, 5) {
                        let instr = self.generate_instr();
                        if let Some(instr) = instr {
                            instrs.push(instr);
                        }
                    }
                    else_branch = Some(instrs);
                }

                Some(Instruction::If {
                    cond: cond.unwrap(),
                    then_branch: then_branch,
                    else_branch: else_branch,
                })
            }
            3 => {
                // For
                let init = if let Some(Expr::Assign(varr, expr)) = self.generate_assign(0) {
                    Expr::Assign(varr, expr)
                } else {
                    return None;
                };

                let cond = self.generate_expr(0);
                if cond.is_none() {
                    return None;
                }

                let step = if let Some(Expr::Assign(varr, expr)) = self.generate_assign(0) {
                    Expr::Assign(varr, expr)
                } else {
                    return None;
                };

                let mut body = Vec::<Instruction>::new();
                for _ in 1..self.rng.rand(1, 5) {
                    let instr = self.generate_instr();
                    if let Some(instr) = instr {
                        body.push(instr);
                    }
                }

                if body.is_empty() {
                    return None;
                }

                Some(Instruction::For {
                    init: init,
                    cond: cond.unwrap(),
                    step: step,
                    body: body,
                })
            }
            4 => {
                // While
                let cond = self.generate_expr(0);
                if cond.is_none() {
                    return None;
                }

                let mut body = Vec::<Instruction>::new();
                for _ in 1..self.rng.rand(1, 5) {
                    let instr = self.generate_instr();
                    if let Some(instr) = instr {
                        body.push(instr);
                    }
                }

                if body.is_empty() {
                    return None;
                }

                Some(Instruction::While {
                    cond: cond.unwrap(),
                    body: body,
                })
            }
            _ => unreachable!(),
        };

        instr
    }

    pub fn generate_expr(&mut self, depth: usize) -> Option<Expr> {
        /*if depth >= 10 {
            return None;
        }*/

        if depth != 0 && self.rng.rand(1, 3) == 1 {
            return Some(Expr::Constant(self.rng.next().to_string()));
        }

        if depth != 0 && self.rng.rand(1, 3) == 1 && !self.variables.is_empty() {
            let varref = self.rng.rand(0, self.curr_var_id.unwrap());
            return Some(Expr::Var(VarRef(varref)));
        }

        if self.rng.rand(1, 5) == 1 {
            return None;
        }

        match self.rng.rand(1, Expr::EXPR_COUNT - 1) {
            0 => {
                // Var
                if let Some(varid) = self.curr_var_id {
                    let varref = self.rng.rand(0, varid);
                    Some(Expr::Var(VarRef(varref)))
                } else {
                    None
                }
            }
            1 | 2 => {
                // Constant
                Some(Expr::Constant(self.rng.next().to_string()))
            }
            /*2 => {
                // ArrayIndex
                todo!();
            }*/
            3 => gen_bin_expr!(self, depth, Expr::Add),
            4 => gen_bin_expr!(self, depth, Expr::Sub),
            5 => gen_bin_expr!(self, depth, Expr::Mul),
            6 => gen_bin_expr!(self, depth, Expr::Power),
            7 => gen_bin_expr!(self, depth, Expr::Div),
            8 => gen_bin_expr!(self, depth, Expr::IntDiv),
            9 => gen_bin_expr!(self, depth, Expr::Rem),
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
                    let expr = self.generate_expr(depth + 1);
                    if let Some(expr) = expr {
                        return Some(Expr::Constraint(var_id.clone(), Box::new(expr)));
                    }
                }

                None
            }
            13 => gen_bin_expr!(self, depth, Expr::LessThan),
            14 => gen_bin_expr!(self, depth, Expr::LessThanEq),
            15 => gen_bin_expr!(self, depth, Expr::GreaterThan),
            16 => gen_bin_expr!(self, depth, Expr::GreaterThanEq),
            17 => gen_bin_expr!(self, depth, Expr::Equal),
            18 => gen_bin_expr!(self, depth, Expr::And),
            19 => gen_bin_expr!(self, depth, Expr::Or),
            20 => gen_un_expr!(self, depth, Expr::Not),
            21 => gen_bin_expr!(self, depth, Expr::BitAnd),
            22 => gen_bin_expr!(self, depth, Expr::BitOr),
            23 => gen_un_expr!(self, depth, Expr::BitNot),
            24 => gen_bin_expr!(self, depth, Expr::BitXor),
            25 => gen_bin_expr!(self, depth, Expr::BitRShift),
            26 => gen_bin_expr!(self, depth, Expr::BitLShift),
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

        var
    }

    fn generate_assign(&mut self, depth: usize) -> Option<Expr> {
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
            let expr = self.generate_expr(depth + 1);
            if let Some(expr) = expr {
                return Some(Expr::Assign(var_id.clone(), Box::new(expr)));
            }
        }

        None
    }
}
