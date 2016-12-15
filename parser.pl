parse(ParseTree) --> block(ParseTree).

block --> left_curly, stmts, right_curly.

stmts --> assign, stmts.

assign --> [].
assign --> id, assign_op, expr, semicolon.

expr --> term.
expr --> term, add_op, expr.
expr --> term, sub_op, expr.

term --> factor.
term --> factor, mult_op, term.
term --> factor, div_op.

factor --> int.
factor --> id.
factor --> left_paren, expr, right_paren.

int(int(I)) --> [I], {number(I)}.