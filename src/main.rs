use std::io::Write;

mod ast;
mod circom;

use crate::ast::*;
use crate::circom::*;

fn main() {
    let circuit = Circuit {
        variables: vec![Variable {
            id: VarRef(0),
            name: String::from("myvar"),
            _type: VariableType::Field,
            scope: VariableScope::Local,
        }],
        instructions: vec![Instruction::For {
            init: Expr::Assign(VarRef(0), Box::new(Expr::Constant(123))),
            cond: Expr::LessThan(Box::new(Expr::Var(VarRef(0))), Box::new(Expr::Constant(42))),
            step: Expr::Assign(VarRef(0), Box::new(Expr::Constant(1))),
            body: vec![Instruction::ExprStmt(Expr::Add(
                Box::new(Expr::Constant(1)),
                Box::new(Expr::Constant(1)),
            ))],
        }],
    };

    let circom_backend = CircomTranspiler { circuit: circuit };
    let output = circom_backend.transpile_circuit();

    let mut file = std::fs::File::create("./circuit.circom").unwrap();
    file.write_all(output.as_bytes()).unwrap();

    println!("{}", output);
}
