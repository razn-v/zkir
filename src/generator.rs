use crate::{
    ast::{Circuit, Expr, Instruction, SignalType, VarRef, Variable, VariableRole, VariableType},
    rng::Rng,
};

macro_rules! gen_bin_expr {
    ($op:path) => {{
        |m: &mut Generator, depth: usize| {
            let left = m.generate_expr(depth + 1);
            let right = m.generate_expr(depth + 1);

            if left.is_none() || right.is_none() {
                return None;
            }

            Some($op(Box::new(left.unwrap()), Box::new(right.unwrap())))
        }
    }};
}

macro_rules! gen_un_expr {
    ($op:path) => {{
        |m: &mut Generator, depth: usize| {
            let expr = m.generate_expr(depth + 1);
            expr.map(|e| $op(Box::new(e)))
        }
    }};
}

#[derive(Debug)]
struct Scope {
    vars: Vec<Variable>,
}

impl Scope {
    fn new() -> Self {
        Self { vars: Vec::new() }
    }
}

#[derive(Debug)]
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

    fn add_var(
        &mut self,
        name: String,
        var_type: VariableType,
        role: VariableRole,
        initialized: bool,
    ) -> Variable {
        let var = Variable {
            id: VarRef(self.next_id),
            name: name,
            var_type: var_type,
            role: role,
            initialized,
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

    // Return true if we have variables within reach
    fn has_scope_vars(&self) -> bool {
        return !self.get_scope_vars().is_empty();
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

pub struct Generator {
    rng: Rng,
    n_instr: usize,
    scope_stack: ScopeStack,
}

impl Generator {
    pub fn new(rng: Rng) -> Self {
        Self {
            rng,
            n_instr: 0,
            scope_stack: ScopeStack::new(),
        }
    }

    fn has_vars_of_type<F>(&self, predicate: F) -> bool
    where
        F: Fn(&Variable) -> bool,
    {
        self.scope_stack
            .get_scope_vars()
            .iter()
            .any(|var| predicate(var))
    }

    pub fn generate(&mut self) -> Circuit {
        let mut instructions = Vec::new();

        self.scope_stack.enter_scope();

        let n = self.rng.rand(15, 30);
        println!("Generating {} instructions...", n);

        for _ in 1..n {
            print!("=== NEW INSTRUCTION GENERATION ");
            let instr = self.generate_instr(true);
            if let Some(instr) = instr {
                instructions.push(instr);
                self.n_instr = 0;
                println!("=== SUCCEEDED! ===")
            } else {
                println!("=== FAILED! ===")
            }
        }

        println!("Generated {}/{} instructions", instructions.len(), n);

        self.scope_stack.leave_scope();
        assert!(self.scope_stack.stack.is_empty());

        Circuit::new(self.scope_stack.all_vars.clone(), instructions)
    }

    pub fn generate_instr(&mut self, first: bool) -> Option<Instruction> {
        if self.n_instr >= 10 {
            println!("Reached instruction limit.");
            return None;
        }

        self.n_instr += 1;

        let has_vars = self.has_vars_of_type(|var| {
            // Assignments are only possible on variables
            matches!(var.role, VariableRole::Local)
        });
        let has_arrays = self.has_vars_of_type(|var| {
            // Array assignments are only possible on local arrays
            matches!(var.role, VariableRole::Local)
                && matches!(var.var_type, VariableType::Array(_))
        });
        let has_output_signals = self.has_vars_of_type(|var| {
            // We can only use signal outputs in constraints
            matches!(var.role, VariableRole::Signal(SignalType::Output))
        });

        let mut funcs: Vec<fn(&mut Generator) -> Option<Instruction>> = vec![
            Self::gen_var_decl_default,
            Self::gen_if,
            Self::gen_for,
            Self::gen_while,
        ];

        // Only add `gen_assign`, `gen_array_assign` and `gen_constraint` if we can
        if has_vars {
            funcs.push(Self::gen_assign);
        }
        if has_arrays {
            funcs.push(Self::gen_array_assign);
        }
        if has_output_signals {
            funcs.push(Self::gen_constraint);
        }

        let idx: usize = self.rng.rand(0, funcs.len() - 1);
        if first {
            match idx {
                0 => print!("gen_var_decl_default"),
                1 => print!("gen_if"),
                2 => print!("gen_for"),
                3 => print!("gen_while"),
                4 => print!("gen_assign"),
                5 => print!("gen_array_assign"),
                6 => print!("gen_constraint"),
                _ => unreachable!(),
            }
            println!(" ===");
        }
        (funcs[idx])(self)
    }

    fn gen_var_decl_default(&mut self) -> Option<Instruction> {
        self.gen_var_decl(None, None, None)
    }

    pub fn gen_var_decl(
        &mut self,
        type_kind: Option<VariableType>,
        role_kind: Option<VariableRole>,
        default_value: Option<Expr>,
    ) -> Option<Instruction> {
        let var_type = type_kind.unwrap_or_else(|| {
            if self.rng.rand(0, 1) == 0 {
                VariableType::Field
            } else {
                VariableType::Array(self.rng.rand(1, 10))
            }
        });

        // 9/10 chance of declaring a variable with a default value
        let mut default_value = default_value;
        if default_value.is_none() && self.rng.rand(1, 10) != 1 {
            let val = self.generate_expr(0)?;
            default_value = Some(val);
        }

        let role = role_kind.unwrap_or_else(|| {
            // We can only assign a default value to locals
            if default_value.is_some() {
                return VariableRole::Local;
            }

            if self.rng.rand(0, 1) == 0 {
                if self.rng.rand(0, 1) == 0 {
                    VariableRole::Signal(SignalType::Input)
                } else {
                    VariableRole::Signal(SignalType::Output)
                }
            } else {
                VariableRole::Local
            }
        });

        Some(Instruction::VarDecl(
            self.scope_stack.add_var(
                self.rng.rand_string(8),
                var_type,
                role,
                default_value.is_some(),
            ),
            default_value,
        ))
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
        let expr = self.generate_expr(0)?;

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
            let expr = self.generate_expr(0);

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
            let expr = self.generate_expr(0);
            if let Some(expr) = expr {
                return Some(Instruction::Constraint(var_id.clone(), Box::new(expr)));
            }
        }

        None
    }

    pub fn gen_if(&mut self) -> Option<Instruction> {
        let cond = self.generate_expr(0)?;

        self.scope_stack.enter_scope();

        let mut then_branch = Vec::<Instruction>::new();
        for _ in 1..self.rng.rand(1, 5) {
            let instr = self.generate_instr(false);
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
                let instr = self.generate_instr(false);
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
        let init: Option<Instruction>;

        // If no variables are available, we always declare a new variable,
        // otherwise we have a 4/5 chance
        if !self.scope_stack.has_scope_vars() || self.rng.rand(1, 5) != 1 {
            // Our variable *must* have a default value
            let default_value = self.generate_expr(0)?;
            init = self.gen_var_decl(
                Some(VariableType::Field),
                Some(VariableRole::Local),
                Some(default_value),
            );
        } else {
            init = self.gen_assign();
        }

        if init.is_none() {
            unreachable!();
        }

        let cond = self.generate_expr(0)?;
        let step = if let Some(Instruction::Assign(varr, expr)) = self.gen_assign() {
            Instruction::Assign(varr, expr)
        } else {
            println!("No step found for FOR");
            return None;
        };

        self.scope_stack.enter_scope();

        let mut body = Vec::<Instruction>::new();
        for _ in 1..self.rng.rand(1, 5) {
            let instr = self.generate_instr(false);
            if let Some(instr) = instr {
                body.push(instr);
            }
        }

        self.scope_stack.leave_scope();

        if body.is_empty() {
            println!("No body found for FOR");
            return None;
        }

        Some(Instruction::For {
            init: Box::new(init.unwrap()),
            cond: cond,
            step: Box::new(step),
            body: body,
        })
    }

    pub fn gen_while(&mut self) -> Option<Instruction> {
        let cond = self.generate_expr(0)?;

        self.scope_stack.enter_scope();

        let mut body = Vec::<Instruction>::new();
        for _ in 1..self.rng.rand(1, 5) {
            let instr = self.generate_instr(false);
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

    pub fn generate_expr(&mut self, depth: usize) -> Option<Expr> {
        // Hard limit to prevent stack overflow
        if depth >= 5 {
            return Some(Expr::Constant(self.rng.next().to_string()));
        }

        // 1/3 chance of stopping there by returning a random constant
        if depth >= 1 && self.rng.rand(1, 3) == 1 {
            return Some(Expr::Constant(self.rng.next().to_string()));
        }

        // Check if we have variables available for gen_var_ref
        let has_field_vars = self.has_vars_of_type(|var| {
            // We can only reference initialized variables in expressions
            matches!(var.var_type, VariableType::Field) && var.initialized
        });

        // Check if we have arrays available for gen_array_index
        let has_array_vars = self.has_vars_of_type(|var| {
            // We can only reference initialized arrays in expressions
            matches!(var.var_type, VariableType::Array(_)) && var.initialized
        });

        let mut funcs: Vec<fn(&mut Generator, depth: usize) -> Option<Expr>> = vec![
            Self::gen_constant,
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

        // Only add `gen_var_ref` and `gen_array_index` if we can
        if has_field_vars {
            funcs.push(Self::gen_var_ref);
        }
        if has_array_vars {
            funcs.push(Self::gen_array_index);
        }

        let idx: usize = self.rng.rand(0, funcs.len() - 1);
        (funcs[idx])(self, depth)
    }

    pub fn gen_var_ref(&mut self, _depth: usize) -> Option<Expr> {
        // Only pick Field variables because Arrays can only be accessed with ArrayIndex expressions
        let vars = self
            .scope_stack
            .get_scope_vars()
            .iter()
            .filter(|var| {
                if !var.initialized {
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

    pub fn gen_constant(&mut self, _depth: usize) -> Option<Expr> {
        Some(Expr::Constant(self.rng.next().to_string()))
    }

    pub fn gen_array_index(&mut self, _depth: usize) -> Option<Expr> {
        let vars: Vec<(VarRef, usize)> = self
            .scope_stack
            .get_scope_vars()
            .iter()
            .filter_map(|var| {
                if let VariableType::Array(n) = var.var_type {
                    if !var.initialized {
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
