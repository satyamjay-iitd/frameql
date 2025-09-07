use pest_derive::Parser;

pub mod parser;

#[derive(Parser)]
#[grammar = "grammar.pest"]
pub struct FrameQLParser;

#[cfg(test)]
mod tests {
    use pest::Parser as _;

    use crate::parser::parse_datalog;

    use super::*;

    #[test]
    fn it_works() {
        let input = r#"
input relation Links(src: string, dst: string, link_status: bool)
output relation ConnectedNodes(src: string, dst: string)

ConnectedNodes(src, dst) :- Links(src, dst, true).
ConnectedNodes(src, dst) :- ConnectedNodes(src, intermediate_node), Links(intermediate_node, dst, true), (src != dst).
"#;
        let x: pest::iterators::Pair<'_, Rule> = FrameQLParser::parse(Rule::datalog, input)
            .unwrap()
            .next()
            .unwrap();
        parse_datalog(x);
    }
}
