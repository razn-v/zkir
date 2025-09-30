use crate::ast::*;
use std::fmt::Write;

pub struct CircomTranspiler<'a> {
    pub circuit: &'a Circuit,
    indent_size: usize,
}

impl<'a> CircomTranspiler<'a> {
    pub fn new(circuit: &'a Circuit) -> Self {
        Self {
            circuit,
            indent_size: 1,
        }
    }

    fn get_indent(&self) -> String {
        "   ".repeat(self.indent_size)
    }

    fn transpile_expr(&self, expr: &Expr) -> String {
        match expr {
            Expr::Var(varref) => {
                let var = self.circuit.get_variable(varref);

                if let Some(idx) = varref.idx {
                    // We have a reference to an array index
                    format!("({}[{}])", var.name, idx)
                } else {
                    // We have a refenrece to a field variable
                    var.name.clone()
                }
            }
            Expr::Constant(field) => field.to_string(),
            Expr::Add(left, right) => {
                format!(
                    "({}+{})",
                    self.transpile_expr(left),
                    self.transpile_expr(right)
                )
            }
            Expr::Sub(left, right) => format!(
                "({}-{})",
                self.transpile_expr(left),
                self.transpile_expr(right)
            ),
            Expr::Mul(left, right) => format!(
                "({}*{})",
                self.transpile_expr(left),
                self.transpile_expr(right)
            ),
            Expr::Power(left, right) => format!(
                "({}**{})",
                self.transpile_expr(left),
                self.transpile_expr(right)
            ),
            Expr::Div(left, right) => format!(
                "({}/{})",
                self.transpile_expr(left),
                self.transpile_expr(right)
            ),
            Expr::IntDiv(left, right) => format!(
                "({}\\{})",
                self.transpile_expr(left),
                self.transpile_expr(right)
            ),
            Expr::Rem(left, right) => format!(
                "({}%{})",
                self.transpile_expr(left),
                self.transpile_expr(right)
            ),
            Expr::LessThan(left, right) => {
                format!(
                    "({}<{})",
                    self.transpile_expr(left),
                    self.transpile_expr(right)
                )
            }
            Expr::LessThanEq(left, right) => {
                format!(
                    "({}<={})",
                    self.transpile_expr(left),
                    self.transpile_expr(right)
                )
            }
            Expr::GreaterThan(left, right) => {
                format!(
                    "({}>{})",
                    self.transpile_expr(left),
                    self.transpile_expr(right)
                )
            }
            Expr::GreaterThanEq(left, right) => {
                format!(
                    "({}>={})",
                    self.transpile_expr(left),
                    self.transpile_expr(right)
                )
            }
            Expr::Equal(left, right) => {
                format!(
                    "({}=={})",
                    self.transpile_expr(left),
                    self.transpile_expr(right)
                )
            }
            Expr::And(left, right) => {
                format!(
                    "({}&&{})",
                    self.transpile_expr(left),
                    self.transpile_expr(right)
                )
            }
            Expr::Or(left, right) => {
                format!(
                    "({}||{})",
                    self.transpile_expr(left),
                    self.transpile_expr(right)
                )
            }
            //Expr::Not(expr) => format!("(!{})", self.transpile_expr(expr)),
            Expr::BitAnd(left, right) => {
                format!(
                    "({}&{})",
                    self.transpile_expr(left),
                    self.transpile_expr(right)
                )
            }
            Expr::BitOr(left, right) => {
                format!(
                    "({}|{})",
                    self.transpile_expr(left),
                    self.transpile_expr(right)
                )
            }
            Expr::BitNot(expr) => format!("(~{})", self.transpile_expr(expr)),
            Expr::BitXor(left, right) => {
                format!(
                    "({}^{})",
                    self.transpile_expr(left),
                    self.transpile_expr(right)
                )
            }
            Expr::BitRShift(left, right) => {
                format!(
                    "({}>>{})",
                    self.transpile_expr(left),
                    self.transpile_expr(right)
                )
            }
            Expr::BitLShift(left, right) => {
                format!(
                    "({}<<{})",
                    self.transpile_expr(left),
                    self.transpile_expr(right)
                )
            }
        }
    }

    fn transpile_instruction(&mut self, instr: &Instruction) -> String {
        match instr {
            Instruction::Assign(varref, right) => {
                let var = self.circuit.get_variable(varref);
                assert_eq!(
                    var.role,
                    VariableRole::Local,
                    "Assignment are only possible with variables"
                );
                assert_eq!(
                    var.var_type,
                    VariableType::Field,
                    "Assignment are only possible with fields"
                );

                format!(
                    "{}{}={};",
                    self.get_indent(),
                    var.name,
                    self.transpile_expr(right)
                )
            }
            Instruction::ArrayAssign(varref, index, val) => {
                let var = self.circuit.get_variable(varref);
                assert_eq!(
                    var.role,
                    VariableRole::Local,
                    "Assignment is only possible between variables"
                );
                assert!(matches!(var.var_type, VariableType::Array(_)));

                format!(
                    "{}{}[{}]={};",
                    self.get_indent(),
                    var.name,
                    self.transpile_expr(index),
                    self.transpile_expr(val)
                )
            }
            Instruction::Constraint(varref, right) => {
                let var = self.circuit.get_variable(varref);
                assert_eq!(
                    var.role,
                    VariableRole::Signal(SignalType::Output),
                    "Constraint are only possible when the target is an output signal"
                );
                format!(
                    "{}{}<=={};",
                    self.get_indent(),
                    var.name,
                    self.transpile_expr(right)
                )
            }
            Instruction::VarDecl(var, default_value) => {
                let prefix = match &var.role {
                    VariableRole::Signal(signal_type) => {
                        let signal_type = match signal_type {
                            SignalType::Input => "input",
                            SignalType::Output => "output",
                        };
                        format!("signal {signal_type}")
                    }
                    VariableRole::Local => String::from("var"),
                };

                match var.var_type {
                    VariableType::Field => {
                        if let Some(default_value) = default_value {
                            format!(
                                "{}{prefix} {name} = {default_value};",
                                self.get_indent(),
                                prefix = prefix,
                                name = var.name,
                                default_value = self.transpile_expr(default_value)
                            )
                        } else {
                            format!("{}{prefix} {name};", self.get_indent(), name = var.name)
                        }
                    }
                    VariableType::Array(size) => {
                        if let Some(default_value) = default_value {
                            format!(
                                "{}{prefix} {name}[{size}] = {default_value};",
                                self.get_indent(),
                                prefix = prefix,
                                name = var.name,
                                size = size,
                                default_value = self.transpile_expr(default_value)
                            )
                        } else {
                            format!(
                                "{}{prefix} {name}[{size}];",
                                self.get_indent(),
                                prefix = prefix,
                                name = var.name,
                                size = size
                            )
                        }
                    }
                }
            }
            Instruction::If {
                cond,
                then_branch,
                else_branch,
            } => {
                let mut res = String::new();

                if let Some(else_branch) = else_branch {
                    write!(
                        &mut res,
                        "{}if ({}) {{\n",
                        self.get_indent(),
                        self.transpile_expr(cond)
                    )
                    .unwrap();

                    self.indent_size += 1;
                    write!(
                        &mut res,
                        "{}\n",
                        then_branch
                            .iter()
                            .map(|ins| self.transpile_instruction(ins))
                            .collect::<Vec<String>>()
                            .join("\n")
                    )
                    .unwrap();

                    self.indent_size -= 1;
                    write!(&mut res, "{}}} else {{\n", self.get_indent()).unwrap();

                    self.indent_size += 1;
                    write!(
                        &mut res,
                        "{}\n",
                        else_branch
                            .iter()
                            .map(|ins| self.transpile_instruction(ins))
                            .collect::<Vec<String>>()
                            .join("\n")
                    )
                    .unwrap();

                    self.indent_size -= 1;
                    write!(&mut res, "{}}}", self.get_indent()).unwrap();
                } else {
                    write!(
                        &mut res,
                        "{}if ({}) {{\n",
                        self.get_indent(),
                        self.transpile_expr(cond)
                    )
                    .unwrap();

                    self.indent_size += 1;
                    write!(
                        &mut res,
                        "{}\n",
                        then_branch
                            .iter()
                            .map(|ins| self.transpile_instruction(ins))
                            .collect::<Vec<String>>()
                            .join("\n")
                    )
                    .unwrap();

                    self.indent_size -= 1;
                    write!(&mut res, "{}}}", self.get_indent()).unwrap();
                }

                res
            }
            Instruction::For {
                init,
                cond,
                step,
                body,
            } => {
                assert!(
                    matches!(**init, Instruction::Assign(_, _))
                        || matches!(**init, Instruction::VarDecl(_, _)),
                    "For-loop inits must be an assignment"
                );
                assert!(
                    matches!(**step, Instruction::Assign(_, _)),
                    "For-loop steps must be an assignment"
                );
                assert!(!body.is_empty(), "Empty for loop bodies are not allowed");

                let mut res = String::new();

                write!(
                    &mut res,
                    "{}for ({init} {cond}; {step}) {{\n",
                    self.get_indent(),
                    init = self.transpile_instruction(init).trim(),
                    cond = self.transpile_expr(cond),
                    step = self.transpile_instruction(step).replace(";", "").trim(),
                )
                .unwrap();

                self.indent_size += 1;
                write!(
                    &mut res,
                    "{}\n",
                    body.iter()
                        .map(|ins| self.transpile_instruction(ins))
                        .collect::<Vec<String>>()
                        .join("\n")
                )
                .unwrap();

                self.indent_size -= 1;
                write!(&mut res, "{}}}", self.get_indent()).unwrap();

                res
            }
            Instruction::While { cond, body } => {
                assert!(!body.is_empty(), "Empty while loop bodies are not allowed");

                let mut res = String::new();

                write!(
                    &mut res,
                    "{}while ({cond}) {{\n",
                    self.get_indent(),
                    cond = self.transpile_expr(cond),
                )
                .unwrap();

                self.indent_size += 1;
                write!(
                    &mut res,
                    "{}\n",
                    body.iter()
                        .map(|ins| self.transpile_instruction(ins))
                        .collect::<Vec<String>>()
                        .join("\n")
                )
                .unwrap();

                self.indent_size -= 1;
                write!(&mut res, "{}}}", self.get_indent()).unwrap();

                res
            }
        }
    }

    pub fn transpile_circuit(&mut self) -> String {
        let instructions: String = self
            .circuit
            .instructions
            .iter()
            .map(|ins| self.transpile_instruction(ins) + "\n")
            .collect::<Vec<String>>()
            .concat();

        format!(
            r#"
template Program() {{
{instructions}}}

component main = Program();
        "#
        )
    }
}
