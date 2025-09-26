use crate::{
    ast::{Circuit, Expr, Instruction, SignalType, VarRef, Variable, VariableRole, VariableType},
    rng::Rng,
};

macro_rules! gen_bin_expr {
    ($op:path) => {{
        |m: &mut Mutator, depth: usize, only_initialized_vars: bool| {
            let left = m.generate_expr(depth + 1, only_initialized_vars);
            let right = m.generate_expr(depth + 1, only_initialized_vars);

            if left.is_none() || right.is_none() {
                return None;
            }

            Some($op(Box::new(left.unwrap()), Box::new(right.unwrap())))
        }
    }};
}

macro_rules! gen_un_expr {
    ($op:path) => {{
        |m: &mut Mutator, depth: usize, only_initialized_vars: bool| {
            let expr = m.generate_expr(depth + 1, only_initialized_vars);
            expr.map(|e| $op(Box::new(e)))
        }
    }};
}

struct Scope {
    vars: Vec<Variable>,
}

impl Scope {
    fn new() -> Self {
        Self { vars: Vec::new() }
    }
}

struct ScopeStack {
    stack: Vec<Scope>,
    all_vars: Vec<Variable>,
    next_id: usize,
}

impl ScopeStack {
    fn new() -> Self {
        Self {
            stack: vec![],
            all_vars: Vec::new(),
            next_id: 0,
        }
    }

    fn enter_scope(&mut self) {
        self.stack.push(Scope::new());
    }

    fn leave_scope(&mut self) {
        self.stack.pop();
    }

    fn add_var(&mut self, name: String, var_type: VariableType, role: VariableRole) -> Variable {
        let var = Variable {
            id: VarRef(self.next_id),
            name: name,
            var_type: var_type,
            role: role,
            initialized: false,
        };

        self.stack.last_mut().unwrap().vars.push(var.clone());
        self.all_vars.push(var.clone());
        self.next_id += 1;

        var
    }

    // Return all variables within reach inside the current scope
    fn get_scope_vars(&self) -> Vec<&Variable> {
        self.stack.iter().flat_map(|s| &s.vars).collect()
    }

    fn mark_initialized(&mut self, var_id: &VarRef) {
        for scope in &mut self.stack {
            for var in &mut scope.vars {
                if var.id == *var_id {
                    var.initialized = true;
                    return;
                }
            }
        }
    }
}

pub struct Mutator {
    rng: Rng,
    n_instr: usize,
    scope_stack: ScopeStack,
}

impl Mutator {
    pub fn new(rng: Rng) -> Self {
        Self {
            rng,
            n_instr: 0,
            scope_stack: ScopeStack::new(),
        }
    }

    pub fn generate(&mut self) -> Circuit {
        let mut instructions = Vec::new();

        self.scope_stack.enter_scope();

        for _ in 1..self.rng.rand(15, 30) {
            let instr = self.generate_instr();
            if let Some(instr) = instr {
                instructions.push(instr);
            }
        }

        self.scope_stack.leave_scope();
        assert!(self.scope_stack.stack.is_empty());

        Circuit::new(self.scope_stack.all_vars.clone(), instructions)
    }

    pub fn generate_instr(&mut self) -> Option<Instruction> {
        if self.n_instr >= 100 {
            return None;
        }

        self.n_instr += 1;

        let funcs: &[fn(&mut Mutator) -> Option<Instruction>; Instruction::INSTRUCTION_COUNT] = &[
            Self::gen_var_decl,
            Self::gen_assign,
            Self::gen_array_assign,
            Self::gen_constraint,
            Self::gen_if,
            Self::gen_for,
            Self::gen_while,
        ];

        let idx: usize = self.rng.rand(0, funcs.len() - 1);
        (funcs[idx])(self)
    }

    pub fn gen_var_decl(&mut self) -> Option<Instruction> {
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

        Some(Instruction::VarDecl(self.scope_stack.add_var(
            self.rng.rand_string(8),
            var_type,
            role,
        )))
    }

    pub fn gen_assign(&mut self) -> Option<Instruction> {
        let vars: Vec<VarRef> = self
            .scope_stack
            .get_scope_vars()
            .iter()
            .filter(|var| {
                matches!(var.role, VariableRole::Local)
                    && matches!(var.var_type, VariableType::Field)
            })
            .map(|var| var.id.clone())
            .collect();

        let var_id = self.rng.rand_vec(&vars)?.clone();

        // We can only initialize variables to initialized variables
        let expr = self.generate_expr(0, true)?;

        // Mark variable as initialized
        self.scope_stack.mark_initialized(&var_id);

        Some(Instruction::Assign(var_id, Box::new(expr)))
    }

    pub fn gen_array_assign(&mut self) -> Option<Instruction> {
        let vars: Vec<(VarRef, usize)> = self
            .scope_stack
            .get_scope_vars()
            .into_iter()
            .filter_map(|var| {
                if matches!(var.role, VariableRole::Local) {
                    if let VariableType::Array(n) = var.var_type {
                        return Some((var.id.clone(), n));
                    }
                }
                None
            })
            .collect();

        if let Some((varref, n)) = self.rng.rand_vec(&vars) {
            // We can only assign an array index to initialized variables
            let expr = self.generate_expr(0, true);

            if let Some(expr) = expr {
                let idx = self.rng.rand(0, n - 1);

                return Some(Instruction::ArrayAssign(
                    varref.clone(),
                    Box::new(Expr::Constant(idx.to_string())),
                    Box::new(expr),
                ));
            }
        }

        None
    }

    pub fn gen_constraint(&mut self) -> Option<Instruction> {
        let vars: Vec<VarRef> = self
            .scope_stack
            .get_scope_vars()
            .iter()
            .filter(|var| matches!(var.role, VariableRole::Signal(SignalType::Output)))
            .map(|var| var.id.clone())
            .collect();

        if let Some(var_id) = self.rng.rand_vec(&vars) {
            let expr = self.generate_expr(0, false);
            if let Some(expr) = expr {
                return Some(Instruction::Constraint(var_id.clone(), Box::new(expr)));
            }
        }

        None
    }

    pub fn gen_if(&mut self) -> Option<Instruction> {
        let cond = self.generate_expr(0, true)?;

        self.scope_stack.enter_scope();

        let mut then_branch = Vec::<Instruction>::new();
        for _ in 1..self.rng.rand(1, 5) {
            let instr = self.generate_instr();
            if let Some(instr) = instr {
                then_branch.push(instr);
            }
        }

        self.scope_stack.leave_scope();

        if then_branch.is_empty() {
            println!("No then_branch found for IF");
            return None;
        }

        self.scope_stack.enter_scope();

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

        self.scope_stack.leave_scope();

        Some(Instruction::If {
            cond: cond,
            then_branch: then_branch,
            else_branch: else_branch,
        })
    }

    pub fn gen_for(&mut self) -> Option<Instruction> {
        let init = if let Some(Instruction::Assign(varr, expr)) = self.gen_assign() {
            Instruction::Assign(varr, expr)
        } else {
            return None;
        };

        let cond = self.generate_expr(0, true)?;

        let step = if let Some(Instruction::Assign(varr, expr)) = self.gen_assign() {
            Instruction::Assign(varr, expr)
        } else {
            return None;
        };

        self.scope_stack.enter_scope();

        let mut body = Vec::<Instruction>::new();
        for _ in 1..self.rng.rand(1, 5) {
            let instr = self.generate_instr();
            if let Some(instr) = instr {
                body.push(instr);
            }
        }

        self.scope_stack.leave_scope();

        if body.is_empty() {
            return None;
        }

        Some(Instruction::For {
            init: Box::new(init),
            cond: cond,
            step: Box::new(step),
            body: body,
        })
    }

    pub fn gen_while(&mut self) -> Option<Instruction> {
        let cond = self.generate_expr(0, true)?;

        self.scope_stack.enter_scope();

        let mut body = Vec::<Instruction>::new();
        for _ in 1..self.rng.rand(1, 5) {
            let instr = self.generate_instr();
            if let Some(instr) = instr {
                body.push(instr);
            }
        }

        self.scope_stack.leave_scope();

        if body.is_empty() {
            return None;
        }

        Some(Instruction::While {
            cond: cond,
            body: body,
        })
    }

    pub fn generate_expr(&mut self, depth: usize, only_initialized_vars: bool) -> Option<Expr> {
        // 1/3 chance of returning a random constant
        if depth != 0 && self.rng.rand(1, 3) == 1 {
            return Some(Expr::Constant(self.rng.next().to_string()));
        }

        // 1/5 chance of stopping here
        if self.rng.rand(1, 5) == 1 {
            return None;
        }

        let funcs: &[fn(&mut Mutator, depth: usize, only_initialized_vars: bool) -> Option<Expr>;
             Expr::EXPR_COUNT] = &[
            Self::gen_var,
            Self::gen_constant,
            Self::gen_array_index,
            gen_bin_expr!(Expr::Add),
            gen_bin_expr!(Expr::Sub),
            gen_bin_expr!(Expr::Mul),
            gen_bin_expr!(Expr::Power),
            gen_bin_expr!(Expr::Div),
            gen_bin_expr!(Expr::IntDiv),
            gen_bin_expr!(Expr::Rem),
            gen_bin_expr!(Expr::LessThan),
            gen_bin_expr!(Expr::LessThanEq),
            gen_bin_expr!(Expr::GreaterThan),
            gen_bin_expr!(Expr::GreaterThanEq),
            gen_bin_expr!(Expr::Equal),
            gen_bin_expr!(Expr::And),
            gen_bin_expr!(Expr::Or),
            // Broken on Circom for some reason
            //gen_un_expr!(Expr::Not),
            gen_bin_expr!(Expr::BitAnd),
            gen_bin_expr!(Expr::BitOr),
            gen_un_expr!(Expr::BitNot),
            gen_bin_expr!(Expr::BitXor),
            gen_bin_expr!(Expr::BitRShift),
            gen_bin_expr!(Expr::BitLShift),
        ];

        let idx: usize = self.rng.rand(0, funcs.len() - 1);
        (funcs[idx])(self, depth, only_initialized_vars)
    }

    pub fn gen_var(&mut self, _depth: usize, only_initialized_vars: bool) -> Option<Expr> {
        // Only pick Field variables because Arrays can only be accessed with ArrayIndex expressions
        let vars = self
            .scope_stack
            .get_scope_vars()
            .iter()
            .filter(|var| {
                if only_initialized_vars && !var.initialized {
                    false
                } else {
                    matches!(var.var_type, VariableType::Field)
                }
            })
            .map(|var| var.id.clone())
            .collect();
        let varref = self.rng.rand_vec(&vars);

        if let Some(varref) = varref {
            Some(Expr::Var(varref.clone()))
        } else {
            None
        }
    }

    pub fn gen_constant(&mut self, _depth: usize, _only_initialized_vars: bool) -> Option<Expr> {
        Some(Expr::Constant(self.rng.next().to_string()))
    }

    pub fn gen_array_index(&mut self, _depth: usize, only_initialized_vars: bool) -> Option<Expr> {
        let vars: Vec<(VarRef, usize)> = self
            .scope_stack
            .get_scope_vars()
            .iter()
            .filter_map(|var| {
                if let VariableType::Array(n) = var.var_type {
                    if only_initialized_vars && !var.initialized {
                        return None;
                    }
                    Some((var.id.clone(), n))
                } else {
                    None
                }
            })
            .collect();

        if let Some((varref, n)) = self.rng.rand_vec(&vars) {
            let idx = self.rng.rand(0, n - 1);

            Some(Expr::ArrayIndex(
                varref.clone(),
                Box::new(Expr::Constant(idx.to_string())),
            ))
        } else {
            None
        }
    }
}
