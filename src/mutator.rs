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
    n_instr: usize,
    variables: Vec<Variable>,
}

impl Mutator {
    pub fn new(rng: Rng) -> Self {
        Self {
            rng,
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
                // VarDecl
                Some(Instruction::VarDecl(self.generate_var()))
            }
            1 => {
                // Assign
                self.generate_assign()
            }
            2 => {
                // ArrayAssign
                // TODO
                None
            }
            3 => {
                // Constraint
                let vars: Vec<VarRef> = self
                    .variables
                    .iter()
                    .filter(|var| matches!(var.role, VariableRole::Signal(SignalType::Output)))
                    .map(|var| var.id.clone())
                    .collect();

                if let Some(var_id) = self.rng.rand_vec(&vars) {
                    let expr = self.generate_expr(0);
                    if let Some(expr) = expr {
                        return Some(Instruction::Constraint(var_id.clone(), Box::new(expr)));
                    }
                }

                None
            }
            4 => {
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

                if then_branch.is_empty() {
                    return None;
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
                    if !instrs.is_empty() {
                        else_branch = Some(instrs);
                    }
                }

                Some(Instruction::If {
                    cond: cond.unwrap(),
                    then_branch: then_branch,
                    else_branch: else_branch,
                })
            }
            5 => {
                // For
                let init = if let Some(Instruction::Assign(varr, expr)) = self.generate_assign() {
                    Instruction::Assign(varr, expr)
                } else {
                    return None;
                };

                let cond = self.generate_expr(0);
                if cond.is_none() {
                    return None;
                }

                let step = if let Some(Instruction::Assign(varr, expr)) = self.generate_assign() {
                    Instruction::Assign(varr, expr)
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
                    init: Box::new(init),
                    cond: cond.unwrap(),
                    step: Box::new(step),
                    body: body,
                })
            }
            6 => {
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
            _ => None,
        };

        instr
    }

    pub fn generate_expr(&mut self, depth: usize) -> Option<Expr> {
        /*if depth >= 10 {
            return None;
        }*/

        // 1/3 chance of returning a random constant
        if depth != 0 && self.rng.rand(1, 3) == 1 {
            return Some(Expr::Constant(self.rng.next().to_string()));
        }

        // 1/3 chance of picking up a random field variable
        if depth != 0 && self.rng.rand(1, 3) == 1 {
            let vars: Vec<VarRef> = self
                .variables
                .iter()
                .filter(|var| matches!(var.var_type, VariableType::Field))
                .map(|var| var.id.clone())
                .collect();

            let varref = self.rng.rand_vec(&vars);
            if let Some(varref) = varref {
                return Some(Expr::Var(varref.clone()));
            }
        }

        // 1/5 chance of stopping here
        if self.rng.rand(1, 5) == 1 {
            return None;
        }

        match self.rng.rand(1, Expr::EXPR_COUNT - 1) {
            0 => {
                // Var
                let varref = self.rng.rand_vec(&self.variables);
                if let Some(varref) = varref {
                    Some(Expr::Var(varref.id.clone()))
                } else {
                    None
                }
            }
            1 | 2 => {
                // Constant
                Some(Expr::Constant(self.rng.next().to_string()))
            }
            3 => gen_bin_expr!(self, depth, Expr::Add),
            4 => gen_bin_expr!(self, depth, Expr::Sub),
            5 => gen_bin_expr!(self, depth, Expr::Mul),
            6 => gen_bin_expr!(self, depth, Expr::Power),
            7 => gen_bin_expr!(self, depth, Expr::Div),
            8 => gen_bin_expr!(self, depth, Expr::IntDiv),
            9 => gen_bin_expr!(self, depth, Expr::Rem),
            10 => gen_bin_expr!(self, depth, Expr::LessThan),
            11 => gen_bin_expr!(self, depth, Expr::LessThanEq),
            12 => gen_bin_expr!(self, depth, Expr::GreaterThan),
            13 => gen_bin_expr!(self, depth, Expr::GreaterThanEq),
            14 => gen_bin_expr!(self, depth, Expr::Equal),
            15 => gen_bin_expr!(self, depth, Expr::And),
            16 => gen_bin_expr!(self, depth, Expr::Or),
            // Broken on Circom for some reason
            //17 => gen_un_expr!(self, depth, Expr::Not),
            18 => gen_bin_expr!(self, depth, Expr::BitAnd),
            19 => gen_bin_expr!(self, depth, Expr::BitOr),
            20 => gen_un_expr!(self, depth, Expr::BitNot),
            21 => gen_bin_expr!(self, depth, Expr::BitXor),
            22 => gen_bin_expr!(self, depth, Expr::BitRShift),
            23 => gen_bin_expr!(self, depth, Expr::BitLShift),
            _ => None,
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

        let var = Variable {
            id: VarRef(self.variables.len()),
            name: self.rng.rand_string(8),
            var_type,
            role,
        };

        self.variables.push(var.clone());

        var
    }

    fn generate_assign(&mut self) -> Option<Instruction> {
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
            let expr = self.generate_expr(0);
            if let Some(expr) = expr {
                return Some(Instruction::Assign(var_id.clone(), Box::new(expr)));
            }
        }

        None
    }
}
