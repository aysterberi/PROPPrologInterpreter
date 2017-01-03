/***
A skeleton for Assignment 3 on PROP HT2016 at DSV/SU.
Peter Idestam-Almquist, 2016-12-15.
***/

/* If you choose to use the tokenizer, uncomment the following code. */

:- [tokenizer].

run(InputFile,OutputFile):-
	tokenize(InputFile,Program),
	parse(ParseTree,Program,[]),
	/*evaluate(ParseTree,[],VariablesOut), */
	output_result(OutputFile,ParseTree,VariablesOut).

/***
Example call of the top level predicate run/2 if you choose to use the tokenizer:
?- run('program1.txt','myparsetree1.txt').
***/

/* If you choose to NOT use the tokenizer, uncomment the following code. */
/***
run(Program,OutputFile):-
	parse(ParseTree,Program,[]),
	evaluate(ParseTree,[],VariablesOut), 
	output_result(OutputFile,ParseTree,VariablesOut).

***/
/***
Example call of the top level predicate run/2 if you choose to NOT use the tokenizer:
?- run([a,=,1,*,2,+,'(',3,-,4,')',/,5,;],'myparsetree1.txt').
***/

output_result(OutputFile,ParseTree,Variables):- 
	open(OutputFile,write,OutputStream),
	write(OutputStream,'PARSE TREE:'), 
	nl(OutputStream), 
	writeln_term(OutputStream,0,ParseTree),
	nl(OutputStream), 
	write(OutputStream,'EVALUATION:'), 
	nl(OutputStream), 
	write_list(OutputStream,Variables), 
	close(OutputStream).
	
writeln_term(Stream,Tabs,int(X)):-
	write_tabs(Stream,Tabs), 
	writeln(Stream,int(X)).
writeln_term(Stream,Tabs,ident(X)):-
	write_tabs(Stream,Tabs), 
	writeln(Stream,ident(X)).
writeln_term(Stream,Tabs,Term):-
	functor(Term,_Functor,0), !,
	write_tabs(Stream,Tabs),
	writeln(Stream,Term).
writeln_term(Stream,Tabs1,Term):-
	functor(Term,Functor,Arity),
	write_tabs(Stream,Tabs1),
	writeln(Stream,Functor),
	Tabs2 is Tabs1 + 1,
	writeln_args(Stream,Tabs2,Term,1,Arity).
	
writeln_args(Stream,Tabs,Term,N,N):-
	arg(N,Term,Arg),
	writeln_term(Stream,Tabs,Arg).
writeln_args(Stream,Tabs,Term,N1,M):-
	arg(N1,Term,Arg),
	writeln_term(Stream,Tabs,Arg), 
	N2 is N1 + 1,
	writeln_args(Stream,Tabs,Term,N2,M).
	
write_tabs(_,0).
write_tabs(Stream,Num1):-
	write(Stream,'\t'),
	Num2 is Num1 - 1,
	write_tabs(Stream,Num2).

writeln(Stream,Term):-
	write(Stream,Term), 
	nl(Stream).
	
write_list(_Stream,[]). 
write_list(Stream,[Ident = Value|Vars]):-
	write(Stream,Ident),
	write(Stream,' = '),
	format(Stream,'~1f',Value), 
	nl(Stream), 
	write_list(Stream,Vars).
	
/***
parse(-ParseTree)-->
	A grammar defining our programming language,
	and returning a parse tree.
***/

parse(ParseTree) --> 
    block(ParseTree).

block(block(LeftCurly, Statements, RightCurly)) -->
    left_curly(LeftCurly), stmts(Statements), right_curly(RightCurly).     

stmts(statements) -->
    [].
stmts(statements(Assign, Statements)) -->
    assign(Assign), stmts(Statements).

assign(assignment(ID, Assign, Expression, SemiColon)) -->
    id(ID), assign_op(Assign), expr(Expression), semicolon(SemiColon).

expr(expression(Term, AddOp, Expression)) --> 
    term(Term), add_op(AddOp), expr(Expression).
expr(expression(Term, SubOp, Expression)) --> 
    term(Term), sub_op(SubOp), expr(Expression).
expr(expression(Term)) -->
	term(Term).

term(term(Factor, MulOp, Term)) -->
    factor(Factor), mult_op(MulOp), term(Term).
term(term(Factor, DivOp, Term)) -->
    factor(Factor), div_op(DivOp), term(Term).
term(term(Factor)) -->
	factor(Factor).


factor(factor(I)) -->
    int(I).
factor(factor(ID)) --> 
    id(ID).
factor(factor(LeftParen, Expression, RightParen)) -->
    left_paren(LeftParen), expr(Expression), right_paren(RightParen).

int(int(I)) --> 
    [I], {number(I)}.
id(ident(ID)) --> 
    [ID], {atom(ID)}.

left_curly(left_curly) --> ['{'].
right_curly(right_curly) --> ['}'].
left_paren(left_paren) --> ['('].
right_paren(right_paren) --> [')'].
assign_op(assign_op) --> [=].
semicolon(semicolon) --> [;].
add_op(add_op) --> [+].
sub_op(sub_op) --> [-].
mult_op(mult_op) --> [*].
div_op(div_op) --> [/].
	
/***
evaluate(+ParseTree,+VariablesIn,-VariablesOut):-
	Evaluates a parse-tree and returns the state of the program
	after evaluation as a list of variables and their values in 
	the form [var = value, ...].
***/

/* EVALUATOR TO BE WRITTEN FOR ASSIGNMENT3 */
