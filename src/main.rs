use std::io::Write;

type VarRef = usize;
type Field = usize;

enum Expr {
    Var(VarRef),
    Constant(Field),
    Add(Box<Expr>, Box<Expr>),
    Sub(Box<Expr>, Box<Expr>),
    Mul(Box<Expr>, Box<Expr>),
}

impl Expr {
    fn transpile(&self, vars: &Vec<Variable>) -> String {
        match self {
            Expr::Var(varref) => vars.get(*varref).unwrap().name.clone(),
            Expr::Constant(field) => field.to_string(),
            Expr::Add(left, right) => format!("{}+{}", left.transpile(vars), right.transpile(vars)),
            Expr::Sub(left, right) => format!("{}-{}", left.transpile(vars), right.transpile(vars)),
            Expr::Mul(left, right) => format!("{}*{}", left.transpile(vars), right.transpile(vars)),
        }
    }
}

struct Constraint {
    left: Expr,
    right: Expr,
}

impl Constraint {
    fn transpile(&self, vars: &Vec<Variable>) -> String {
        format!(
            "{} <== {};",
            self.left.transpile(vars),
            self.right.transpile(vars)
        )
    }
}

enum VariableType {
    Field,
    Array(usize),
}

#[derive(Debug, PartialEq, Eq)]
enum SignalType {
    Input,
    Output,
}

#[derive(Debug, PartialEq, Eq)]
enum VariableScope {
    Signal(SignalType),
    Local,
}

struct Variable {
    id: VarRef,
    name: String,
    _type: VariableType,
    scope: VariableScope,
}

impl Variable {
    fn transpile(&self) -> String {
        let prefix = match &self.scope {
            VariableScope::Signal(_type) => {
                let _type = match _type {
                    SignalType::Input => "input",
                    SignalType::Output => "output",
                };
                format!("signal {_type}")
            }
            VariableScope::Local => String::from("var"),
        };

        match self._type {
            VariableType::Field => {
                format!("{prefix} {name};", prefix = prefix, name = self.name)
            }
            VariableType::Array(size) => {
                format!(
                    "{prefix} {name}[{size}];",
                    prefix = prefix,
                    name = self.name,
                    size = size
                )
            }
        }
    }
}

enum Instruction {
    Constrain {
        target: VarRef,
        value: Expr,
    },
    Assign {
        target: VarRef,
        value: Expr,
    },
    If {
        cond: Expr,
        then_branch: Vec<Instruction>,
        else_branch: Option<Vec<Instruction>>,
    },
    For {
        var: VarRef,
        start: Expr,
        end: Expr,
        expr: Expr,
        body: Vec<Instruction>,
    },
    While {
        cond: Expr,
        body: Vec<Instruction>,
    },
}

impl Instruction {
    fn transpile(&self, vars: &Vec<Variable>) -> String {
        match self {
            Instruction::Constrain { target, value } => {
                let var = vars.get(*target).unwrap();
                assert_eq!(
                    var.scope,
                    VariableScope::Signal(SignalType::Output),
                    "Constraints are only allowed with signal outputs"
                );
                format!("{} <== {};", var.name, value.transpile(vars))
            }
            Instruction::Assign { target, value } => {
                let var = vars.get(*target).unwrap();
                assert_eq!(
                    var.scope,
                    VariableScope::Local,
                    "Assignment is only possible with variables"
                );
                format!("{} = {};", var.name, value.transpile(vars))
            }
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
                        cond.transpile(vars),
                        then_branch
                            .iter()
                            .map(|ins| ins.transpile(vars))
                            .collect::<Vec<String>>()
                            .join("\n"),
                        else_branch
                            .iter()
                            .map(|ins| ins.transpile(vars))
                            .collect::<Vec<String>>()
                            .join("\n"),
                    )
                } else {
                    format!(
                        r#"if ({}) {{
                            {}
                        }}"#,
                        cond.transpile(vars),
                        then_branch
                            .iter()
                            .map(|ins| ins.transpile(vars))
                            .collect::<Vec<String>>()
                            .join("\n")
                    )
                }
            }
            Instruction::For {
                var,
                start,
                end,
                expr,
                body,
            } => {
                format!(
                    r#"
                    for (var {var} = {start}; {var} < {end}; {expr}) {{
                        {body}
                    }}
                    "#,
                    var = vars.get(*var).unwrap().name,
                    start = start.transpile(vars),
                    end = end.transpile(vars),
                    expr = expr.transpile(vars),
                    body = body
                        .iter()
                        .map(|ins| ins.transpile(vars))
                        .collect::<Vec<String>>()
                        .join("\n")
                )
            }
            Instruction::While { cond, body } => String::new(),
        }
    }
}

struct Circuit {
    variables: Vec<Variable>,
    instructions: Vec<Instruction>,
}

impl Circuit {
    fn transpile(&self) -> String {
        let variables: String = self
            .variables
            .iter()
            .map(|var| var.transpile())
            .collect::<Vec<String>>()
            .join("\n    ");
        let instructions: String = self
            .instructions
            .iter()
            .map(|ins| ins.transpile(&self.variables))
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

fn main() {
    let mut circuit = Circuit {
        variables: vec![
            Variable {
                id: 0,
                name: String::from("myvar"),
                _type: VariableType::Field,
                scope: VariableScope::Local,
            },
            Variable {
                id: 1,
                name: String::from("myarray"),
                _type: VariableType::Field,
                scope: VariableScope::Local,
            },
        ],
        instructions: vec![
            Instruction::Constrain {
                target: 0,
                value: Expr::Constant(1),
            },
            Instruction::For {
                var: 0,
                start: Expr::Constant(0),
                end: Expr::Constant(10),
                expr: Expr::Add(Box::new(Expr::Var(0)), Box::new(Expr::Constant(1))),
                body: vec![Instruction::Constrain {
                    target: 0,
                    value: Expr::Mul(Box::new(Expr::Var(0)), Box::new(Expr::Var(0))),
                }],
            },
        ],
    };

    let output = circuit.transpile();

    let mut file = std::fs::File::create("./circuit.circom").unwrap();
    file.write_all(output.as_bytes()).unwrap();

    println!("{}", output);
}
