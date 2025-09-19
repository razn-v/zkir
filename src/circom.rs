use crate::ast::*;

pub struct CircomTranspiler {
    pub circuit: Circuit,
}

impl CircomTranspiler {
    fn transpile_expr(&self, expr: &Expr) -> String {
        match expr {
            Expr::Var(var) => self.circuit.variables.get(*var).unwrap().name.clone(),
            Expr::Constant(field) => field.to_string(),
            Expr::Add(left, right) => {
                format!(
                    "{}+{}",
                    self.transpile_expr(left),
                    self.transpile_expr(right)
                )
            }
            Expr::Sub(left, right) => format!(
                "{}-{}",
                self.transpile_expr(left),
                self.transpile_expr(right)
            ),
            Expr::Mul(left, right) => format!(
                "{}*{}",
                self.transpile_expr(left),
                self.transpile_expr(right)
            ),
            Expr::Assign(var, right) => {
                let var = self.circuit.variables.get(*var).unwrap();
                assert_eq!(
                    var.scope,
                    VariableScope::Local,
                    "Assignment is only possible with variables"
                );
                format!("{}={}", var.name, self.transpile_expr(right))
            }
            Expr::Constrain(var, right) => {
                let var = self.circuit.variables.get(*var).unwrap();
                assert_eq!(
                    var.scope,
                    VariableScope::Signal(SignalType::Output),
                    "Assignment is only possible with variables"
                );
                format!("{}<=={}", var.name, self.transpile_expr(right))
            }
            Expr::LessThan(left, right) => {
                format!(
                    "{}<{}",
                    self.transpile_expr(left),
                    self.transpile_expr(right)
                )
            }
            Expr::LessThanEq(left, right) => {
                format!(
                    "{}<={}",
                    self.transpile_expr(left),
                    self.transpile_expr(right)
                )
            }
            Expr::GreaterThan(left, right) => {
                format!(
                    "{}>{}",
                    self.transpile_expr(left),
                    self.transpile_expr(right)
                )
            }
            Expr::GreaterThanEq(left, right) => {
                format!(
                    "{}>={}",
                    self.transpile_expr(left),
                    self.transpile_expr(right)
                )
            }
            Expr::Equal(left, right) => {
                format!(
                    "{}=={}",
                    self.transpile_expr(left),
                    self.transpile_expr(right)
                )
            }
        }
    }

    fn transpile_instruction(&self, instr: &Instruction) -> String {
        match instr {
            Instruction::ExprStmt(expr) => format!("{};", self.transpile_expr(expr)),
            Instruction::If {
                cond,
                then_branch,
                else_branch,
            } => {
                if let Some(else_branch) = else_branch {
                    format!(
                        r#"if ({}) {{
                            {}
                        }} else {{
                            {}
                        }}"#,
                        self.transpile_expr(cond),
                        then_branch
                            .iter()
                            .map(|ins| self.transpile_instruction(ins))
                            .collect::<Vec<String>>()
                            .join("\n"),
                        else_branch
                            .iter()
                            .map(|ins| self.transpile_instruction(ins))
                            .collect::<Vec<String>>()
                            .join("\n"),
                    )
                } else {
                    format!(
                        r#"if ({}) {{
                            {}
                        }}"#,
                        self.transpile_expr(cond),
                        then_branch
                            .iter()
                            .map(|ins| self.transpile_instruction(ins))
                            .collect::<Vec<String>>()
                            .join("\n")
                    )
                }
            }
            Instruction::For {
                init,
                cond,
                step,
                body,
            } => {
                assert!(
                    matches!(init, Expr::Assign(_, _)),
                    "For-loop inits must be an assignment"
                );
                assert!(
                    matches!(step, Expr::Assign(_, _)),
                    "For-loop steps must be an assignment"
                );
                assert!(!body.is_empty(), "Empty for loop bodies are not allowed");

                format!(
                    r#"
                    for ({init}; {cond}; {step}) {{
                        {body}
                    }}
                    "#,
                    init = self.transpile_expr(init),
                    cond = self.transpile_expr(cond),
                    step = self.transpile_expr(step),
                    body = body
                        .iter()
                        .map(|ins| self.transpile_instruction(ins))
                        .collect::<Vec<String>>()
                        .join("\n")
                )
            }
            Instruction::While { cond, body } => String::new(),
        }
    }

    fn transpile_variable(&self, var: &Variable) -> String {
        let prefix = match &var.scope {
            VariableScope::Signal(_type) => {
                let _type = match _type {
                    SignalType::Input => "input",
                    SignalType::Output => "output",
                };
                format!("signal {_type}")
            }
            VariableScope::Local => String::from("var"),
        };

        match var._type {
            VariableType::Field => {
                format!("{prefix} {name};", prefix = prefix, name = var.name)
            }
            VariableType::Array(size) => {
                format!(
                    "{prefix} {name}[{size}];",
                    prefix = prefix,
                    name = var.name,
                    size = size
                )
            }
        }
    }

    pub fn transpile_circuit(&self) -> String {
        let variables: String = self
            .circuit
            .variables
            .iter()
            .map(|var| self.transpile_variable(var))
            .collect::<Vec<String>>()
            .join("\n    ");
        let instructions: String = self
            .circuit
            .instructions
            .iter()
            .map(|ins| self.transpile_instruction(ins))
            .collect::<Vec<String>>()
            .concat();

        format!(
            r#"
template Program() {{
    {variables}
    {instructions}
}}

component main = Program();
        "#,
            variables = variables,
            instructions = instructions,
        )
    }
}
