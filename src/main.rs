use std::io::Write;

mod ast;
mod circom;
mod generator;
mod rng;

use crate::ast::*;
use crate::circom::*;
use crate::generator::Generator;
use crate::rng::Rng;

fn main() {
    /*
    let circuit = Circuit::new(vec![
        Instruction::VarDecl(Variable {
            id: VarRef(0),
            name: String::from("myvar"),
            var_type: VariableType::Field,
            role: VariableRole::Local,
        }),
        Instruction::VarDecl(Variable {
            id: VarRef(1),
            name: String::from("myvar2"),
            var_type: VariableType::Field,
            role: VariableRole::Local,
        }),
        Instruction::VarDecl(Variable {
            id: VarRef(2),
            name: String::from("myarray"),
            var_type: VariableType::Array(5),
            role: VariableRole::Local,
        }),
        Instruction::VarDecl(Variable {
            id: VarRef(3),
            name: String::from("myoutput"),
            var_type: VariableType::Field,
            role: VariableRole::Signal(SignalType::Output),
        }),
        Instruction::For {
            init: Expr::Assign(VarRef(0), Box::new(Expr::Constant(String::from("123")))),
            cond: Expr::LessThan(
                Box::new(Expr::Var(VarRef(0))),
                Box::new(Expr::Constant(String::from("42"))),
            ),
            step: Expr::Assign(VarRef(0), Box::new(Expr::Constant(String::from("1")))),
            body: vec![
                Instruction::ExprStmt(Expr::Assign(
                    VarRef(1),
                    Box::new(Expr::Not(Box::new(Expr::Constant(String::from("5"))))),
                )),
                Instruction::ExprStmt(Expr::ArrayAssign(
                    VarRef(2),
                    Box::new(Expr::Constant(String::from("2"))),
                    Box::new(Expr::Constant(String::from("42"))),
                )),
                Instruction::While {
                    cond: Expr::LessThan(
                        Box::new(Expr::Var(VarRef(1))),
                        Box::new(Expr::Constant(String::from("12"))),
                    ),
                    body: vec![Instruction::ExprStmt(Expr::Assign(
                        VarRef(1),
                        Box::new(Expr::Add(
                            Box::new(Expr::Var(VarRef(1))),
                            Box::new(Expr::Constant(String::from("1"))),
                        )),
                    ))],
                },
                Instruction::ExprStmt(Expr::Constraint(
                    VarRef(3),
                    Box::new(Expr::ArrayIndex(
                        VarRef(2),
                        Box::new(Expr::Constant(String::from("2"))),
                    )),
                )),
            ],
        },
    ]); */

    let rng = Rng::new();
    let mut generator = Generator::new(rng);
    let circuit = generator.generate();

    let mut circom_backend = CircomTranspiler::new(&circuit);
    let output = circom_backend.transpile_circuit();

    let mut file = std::fs::File::create("./circuit.circom").unwrap();
    file.write_all(output.as_bytes()).unwrap();

    println!("{}", output);
}
