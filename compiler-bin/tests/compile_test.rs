use frameql::compile_from_str;

#[test]
fn compilation_success() {
    let prog_str = r#"
input relation Rin(b: bool)
output relation Rout(b: bool)

Rout(b) :- Rin(b).
"#;
    compile_from_str(&prog_str);
}

#[test]
fn compilation_success2() {
    let prog_str = r#"
import base64

output relation Base64(encoding: string, position: i32)

Base64("TEST", 2).
Base64("!@#!@#", 3).
Base64("Hello !@#!@#", 4).
"#;
    compile_from_str(&prog_str);
}

#[test]
fn compilation_success3() {
    let prog_str = r#"
// Input relations
input relation Links(src: string, dst: string, link_status: bool)

// Output relations
output relation ConnectedNodes(src: string, dst: string)

/*
 * Rules to calculate `ConnectedNodes` relation
 */
ConnectedNodes(src, dst) :- Links(src, dst, true).
ConnectedNodes(src, dst) :- ConnectedNodes(src, intermediate_node), Links(intermediate_node, dst, true), (src != dst).
"#;
    compile_from_str(&prog_str);
}
