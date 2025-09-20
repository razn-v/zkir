use std::io::Write;

mod ast;
mod circom;

use crate::ast::*;
use crate::circom::*;

fn main() {
    let circuit = Circuit {
        variables: vec![
            Variable {
                id: VarRef(0),
                name: String::from("myvar"),
                _type: VariableType::Field,
                role: VariableRole::Local,
            },
            Variable {
                id: VarRef(1),
                name: String::from("myvar2"),
                _type: VariableType::Field,
                role: VariableRole::Local,
            },
            Variable {
                id: VarRef(2),
                name: String::from("myarray"),
                _type: VariableType::Array(5),
                role: VariableRole::Local,
            },
            Variable {
                id: VarRef(3),
                name: String::from("myoutput"),
                _type: VariableType::Field,
                role: VariableRole::Signal(SignalType::Output),
            },
        ],
        instructions: vec![Instruction::For {
            init: Expr::Assign(VarRef(0), Box::new(Expr::Constant(123))),
            cond: Expr::LessThan(Box::new(Expr::Var(VarRef(0))), Box::new(Expr::Constant(42))),
            step: Expr::Assign(VarRef(0), Box::new(Expr::Constant(1))),
            body: vec![
                Instruction::ExprStmt(Expr::Assign(
                    VarRef(1),
                    Box::new(Expr::Not(Box::new(Expr::Constant(5)))),
                )),
                Instruction::ExprStmt(Expr::ArrayAssign(
                    VarRef(2),
                    Box::new(Expr::Constant(2)),
                    Box::new(Expr::Constant(42)),
                )),
                Instruction::While {
                    cond: Expr::LessThan(
                        Box::new(Expr::Var(VarRef(1))),
                        Box::new(Expr::Constant(12)),
                    ),
                    body: vec![Instruction::ExprStmt(Expr::Assign(
                        VarRef(1),
                        Box::new(Expr::Add(
                            Box::new(Expr::Var(VarRef(1))),
                            Box::new(Expr::Constant(1)),
                        )),
                    ))],
                },
                Instruction::ExprStmt(Expr::Constrain(
                    VarRef(3),
                    Box::new(Expr::ArrayIndex(VarRef(2), Box::new(Expr::Constant(2)))),
                )),
            ],
        }],
    };

    let circom_backend = CircomTranspiler { circuit: circuit };
    let output = circom_backend.transpile_circuit();

    let mut file = std::fs::File::create("./circuit.circom").unwrap();
    file.write_all(output.as_bytes()).unwrap();

    println!("{}", output);
}
