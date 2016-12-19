evaluate(block(left_curly, Statements, right_curly), VariablesIn, VariablesOut):-
		evaluate(Statements, VariablesIn, VariablesOut).

evaluate(statements, VariablesIn, VariablesOut):-
		VariablesOut = VariablesIn.
evaluate(statements(Assignment, Statements), VariablesIn, VariablesOut):-
		evaluate(Assignment, VariablesIn, AssignOut),
		append(VariablesIn, AssignOut, AppendOut),
		evaluate(Statements, AppendOut, VariablesOut).

evaluate(assignment(ident(ID), assign_op, Expression, semicolon), VariablesIn, VariablesOut):-
		evaluate(Expression, VariablesIn, ExprOut),
		VariablesOut = [ID = ExprOut].

evaluate(expression(Term), VariablesIn, VariablesOut):-
		evaluate(Term, VariablesIn, VariablesOut).
evaluate(expression(Term, Operator, Expression), VariablesIn, Accumulated, PrevOperator, VariablesOut):-
		evaluate(Term, VariablesIn, TermOut),
		((PrevOperator == add_op) ->
		Accumulation is Accumulated + TermOut;
		Accumulation is Accumulated - TermOut),
		evaluate(Expression, VariablesIn, Accumulation, Operator, VariablesOut).
evaluate(expression(Term), VariablesIn, Accumulated, Operator, VariablesOut):-
		evaluate(Term, VariablesIn, TermOut),
		((Operator == add_op) ->
		VariablesOut is Accumulated + TermOut;
		VariablesOut is Accumulated - TermOut).
evaluate(expression(Term, Operator, Expression), VariablesIn, VariablesOut):-
		evaluate(Term, VariablesIn, TermOut),
		((Operator == add_op) ->
		evaluate(Expression, VariablesIn, TermOut, add_op, VariablesOut);
		evaluate(Expression, VariablesIn, TermOut, sub_op, VariablesOut)).

evaluate(term(Factor), VariablesIn, VariablesOut):-
		evaluate(Factor, VariablesIn, VariablesOut).
evaluate(term(Factor, Operator, Term), VariablesIn, Accumulated, PrevOperator, VariablesOut):-
		evaluate(Factor, VariablesIn, FactorOut),
		((PrevOperator == mult_op) ->
		Accumulation is Accumulated * FactorOut;
		Accumulation is Accumulated / FactorOut),
		evaluate(Term, VariablesIn, Accumulation, Operator, VariablesOut).
evaluate(term(Factor), VariablesIn, Accumulated, Operator, VariablesOut):-
		evaluate(Factor, VariablesIn, FactorOut),
		((Operator == mult_op) ->
		VariablesOut is Accumulated * FactorOut;
		VariablesOut is Accumulated / FactorOut).
evaluate(term(Factor, Operator, Term), VariablesIn, VariablesOut):-
		evaluate(Factor, VariablesIn, FactorOut),
		((Operator == mult_op) ->
		evaluate(Term, VariablesIn, FactorOut, mult_op, VariablesOut);
		evaluate(Term, VariablesIn, FactorOut, div_op, VariablesOut)).

evaluate(factor(left_paren, Expression, right_paren), VariablesIn, VariablesOut):-
		evaluate(Expression, VariablesIn, VariablesOut).
evaluate(factor(Ident), VariablesIn, VariablesOut):-
		evaluate(Ident, VariablesIn, VariablesOut).
evaluate(factor(Int), VariablesIn, VariablesOut):-
		evaluate(Int, VariablesIn, VariablesOut).

evaluate(ident(ID), VariablesIn, VariablesOut):-
		member(ID=X,VariablesIn),
		evaluate(int(X), VariablesIn, VariablesOut).
evaluate(ident(ID), _, VariablesOut):- VariablesOut = ID.
evaluate(int(Int), _, VariablesOut):- VariablesOut is Int.