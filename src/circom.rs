use crate::ast::*;

pub struct CircomTranspiler<'a> {
    pub circuit: &'a Circuit,
}

impl<'a> CircomTranspiler<'a> {
    pub fn new(circuit: &'a Circuit) -> Self {
        Self { circuit }
    }

    fn transpile_expr(&self, expr: &Expr) -> String {
        match expr {
            Expr::Var(varref) => self.circuit.get_variable(varref).name.clone(),
            Expr::Constant(field) => field.to_string(),
            Expr::ArrayIndex(varref, expr) => {
                let var = self.circuit.get_variable(varref);
                format!("{}[{}]", var.name, self.transpile_expr(expr))
            }
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
            Expr::Power(left, right) => format!(
                "{}**{}",
                self.transpile_expr(left),
                self.transpile_expr(right)
            ),
            Expr::Div(left, right) => format!(
                "{}/{}",
                self.transpile_expr(left),
                self.transpile_expr(right)
            ),
            Expr::IntDiv(left, right) => format!(
                "{}\\{}",
                self.transpile_expr(left),
                self.transpile_expr(right)
            ),
            Expr::Rem(left, right) => format!(
                "{}%{}",
                self.transpile_expr(left),
                self.transpile_expr(right)
            ),
            Expr::Assign(varref, right) => {
                let var = self.circuit.get_variable(varref);
                assert_eq!(
                    var.role,
                    VariableRole::Local,
                    "Assignment is only possible with variables"
                );

                format!("{}={}", var.name, self.transpile_expr(right))
            }
            Expr::ArrayAssign(varref, index, val) => {
                let var = self.circuit.get_variable(varref);
                assert_eq!(
                    var.role,
                    VariableRole::Local,
                    "Assignment is only possible with variables"
                );
                assert!(matches!(var.var_type, VariableType::Array(_)));

                format!(
                    "{}[{}]={}",
                    var.name,
                    self.transpile_expr(index),
                    self.transpile_expr(val)
                )
            }
            Expr::Constrain(varref, right) => {
                let var = self.circuit.get_variable(varref);
                assert_eq!(
                    var.role,
                    VariableRole::Signal(SignalType::Output),
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
            Expr::And(left, right) => {
                format!(
                    "{}&&{}",
                    self.transpile_expr(left),
                    self.transpile_expr(right)
                )
            }
            Expr::Or(left, right) => {
                format!(
                    "{}||{}",
                    self.transpile_expr(left),
                    self.transpile_expr(right)
                )
            }
            Expr::Not(expr) => format!("!{}", self.transpile_expr(expr)),
            Expr::BitAnd(left, right) => {
                format!(
                    "{}&{}",
                    self.transpile_expr(left),
                    self.transpile_expr(right)
                )
            }
            Expr::BitOr(left, right) => {
                format!(
                    "{}|{}",
                    self.transpile_expr(left),
                    self.transpile_expr(right)
                )
            }
            Expr::BitNot(expr) => format!("~{}", self.transpile_expr(expr)),
            Expr::BitXor(left, right) => {
                format!(
                    "{}^{}",
                    self.transpile_expr(left),
                    self.transpile_expr(right)
                )
            }
            Expr::BitRShift(left, right) => {
                format!(
                    "{}>>{}",
                    self.transpile_expr(left),
                    self.transpile_expr(right)
                )
            }
            Expr::BitLShift(left, right) => {
                format!(
                    "{}<<{}",
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
            Instruction::While { cond, body } => {
                assert!(!body.is_empty(), "Empty while loop bodies are not allowed");

                format!(
                    r#"
                    while ({cond}) {{
                        {body}
                    }}
                    "#,
                    cond = self.transpile_expr(cond),
                    body = body
                        .iter()
                        .map(|ins| self.transpile_instruction(ins))
                        .collect::<Vec<String>>()
                        .join("\n")
                )
            }
        }
    }

    fn transpile_variable(&self, var: &Variable) -> String {
        let prefix = match &var.role {
            VariableRole::Signal(_type) => {
                let _type = match _type {
                    SignalType::Input => "input",
                    SignalType::Output => "output",
                };
                format!("signal {_type}")
            }
            VariableRole::Local => String::from("var"),
        };

        match var.var_type {
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
