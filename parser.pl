parse(ParseTree) --> block(ParseTree).

block(block(LC, Statement, RC)) --> left_curly(LC), stmts(Statement), right_curly(RC).

stmts(statements(Assign, Statement)) --> assign(Assign), stmts(Statement).
stmts(statements) --> [].

assign(assignment(ID, Assign, Expr, SC)) --> id(ID), assign_op(Assign), expr(Expr), semicolon(SC).

expr(expression(Term)) --> term(Term).
expr(expression(Term, Add, Expr)) --> term(Term), add_op(Add), expr(Expr).
expr(expression(Term, Sub, Expr)) --> term(Term), sub_op(Sub), expr(Expr).

term(term(Factor)) --> factor(Factor).
term(term(Factor, Mult, Term)) --> factor(Factor), mult_op(Mult), term(Term).
term(term(Factor, Div, Term)) --> factor(Factor), div_op(Div), term(Term).

factor(factor(ID)) --> id(ID).
factor(factor(I)) --> int(I).
factor(factor(LP, Expr, RP)) --> left_paren(LP), expr(Expr), right_paren(RP).

assign_op(assign_op) --> [=].
mult_op(mult_op) --> [*].
div_op(div_op) --> [/].
sub_op(sub_op) --> [-].
add_op(add_op) --> [+].
semicolon(semicolon) --> [;].
left_paren(left_paren) --> ['('].
right_paren(right_paren) --> [')'].
left_curly(left_curly) --> ['{'].
right_curly(right_curly) --> ['}'].

int(int(I)) --> [I], {number(I)}.
id(ident(ID)) --> [ID], {atom(ID)}.