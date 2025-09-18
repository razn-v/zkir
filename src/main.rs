use std::io::Write;

type VarRef = usize;
type Field = usize;

enum Expr {
    Var(VarRef),
    Constant(Field),

    Add(Box<Expr>, Box<Expr>),
    Sub(Box<Expr>, Box<Expr>),
    Mul(Box<Expr>, Box<Expr>),

    Assign(VarRef, Box<Expr>),
    Constrain(VarRef, Box<Expr>),

    LessThan(Box<Expr>, Box<Expr>),
    LessThanEq(Box<Expr>, Box<Expr>),
    GreaterThan(Box<Expr>, Box<Expr>),
    GreaterThanEq(Box<Expr>, Box<Expr>),
    Equal(Box<Expr>, Box<Expr>),
}

impl Expr {
    fn transpile(&self, vars: &Vec<Variable>) -> String {
        match self {
            Expr::Var(var) => vars.get(*var).unwrap().name.clone(),
            Expr::Constant(field) => field.to_string(),
            Expr::Add(left, right) => format!("{}+{}", left.transpile(vars), right.transpile(vars)),
            Expr::Sub(left, right) => format!("{}-{}", left.transpile(vars), right.transpile(vars)),
            Expr::Mul(left, right) => format!("{}*{}", left.transpile(vars), right.transpile(vars)),
            Expr::Assign(var, right) => {
                let var = vars.get(*var).unwrap();
                assert_eq!(
                    var.scope,
                    VariableScope::Local,
                    "Assignment is only possible with variables"
                );
                format!("{}={}", var.name, right.transpile(vars))
            }
            Expr::Constrain(var, right) => {
                let var = vars.get(*var).unwrap();
                assert_eq!(
                    var.scope,
                    VariableScope::Signal(SignalType::Output),
                    "Assignment is only possible with variables"
                );
                format!("{}<=={}", var.name, right.transpile(vars))
            }
            Expr::LessThan(left, right) => {
                format!("{}<{}", left.transpile(vars), right.transpile(vars))
            }
            Expr::LessThanEq(left, right) => {
                format!("{}<={}", left.transpile(vars), right.transpile(vars))
            }
            Expr::GreaterThan(left, right) => {
                format!("{}>{}", left.transpile(vars), right.transpile(vars))
            }
            Expr::GreaterThanEq(left, right) => {
                format!("{}>={}", left.transpile(vars), right.transpile(vars))
            }
            Expr::Equal(left, right) => {
                format!("{}=={}", left.transpile(vars), right.transpile(vars))
            }
        }
    }
}

#[derive(Debug)]
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

#[derive(Debug)]
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
    ExprStmt(Expr),
    If {
        cond: Expr,
        then_branch: Vec<Instruction>,
        else_branch: Option<Vec<Instruction>>,
    },
    For {
        init: Expr,
        cond: Expr,
        step: Expr,
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
            Instruction::ExprStmt(expr) => format!("{};", expr.transpile(vars)),
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
                init,
                cond,
                step,
                body,
            } => {
                assert!(
                    matches!(init, Expr::Assign(_, _)),
                    "For-loop inits must be an assignment"
                );
                assert!(!body.is_empty(), "Empty for loop bodies are not allowed");

                format!(
                    r#"
                    for ({init}; {cond}; {step}) {{
                        {body}
                    }}
                    "#,
                    init = init.transpile(vars),
                    cond = cond.transpile(vars),
                    step = step.transpile(vars),
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
        variables: vec![Variable {
            id: 0,
            name: String::from("myvar"),
            _type: VariableType::Field,
            scope: VariableScope::Local,
        }],
        instructions: vec![Instruction::For {
            init: Expr::Assign(0, Box::new(Expr::Constant(123))),
            cond: Expr::LessThan(Box::new(Expr::Var(0)), Box::new(Expr::Constant(42))),
            step: Expr::Add(Box::new(Expr::Var(0)), Box::new(Expr::Constant(1))),
            body: vec![Instruction::ExprStmt(Expr::Add(
                Box::new(Expr::Constant(1)),
                Box::new(Expr::Constant(1)),
            ))],
        }],
    };

    let output = circuit.transpile();

    let mut file = std::fs::File::create("./circuit.circom").unwrap();
    file.write_all(output.as_bytes()).unwrap();

    println!("{}", output);
}
