package com.sri.ai.grinder.parser.antlr;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.core.DefaultSymbol;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.grinder.library.FunctorConstants;
import com.sri.ai.grinder.library.boole.And;
import com.sri.ai.grinder.library.boole.ForAll;
import com.sri.ai.grinder.library.boole.Implication;
import com.sri.ai.grinder.library.boole.Not;
import com.sri.ai.grinder.library.boole.Or;
import com.sri.ai.grinder.library.boole.ThereExists;
import com.sri.ai.grinder.library.controlflow.IfThenElse;
import com.sri.ai.grinder.library.lambda.Lambda;
import com.sri.ai.grinder.library.number.Division;
import com.sri.ai.grinder.library.number.Minus;
import com.sri.ai.grinder.library.number.Plus;
import com.sri.ai.grinder.library.number.Times;
import com.sri.ai.grinder.library.set.extensional.ExtensionalSet;
import com.sri.ai.grinder.library.set.intensional.IntensionalSet;
import com.sri.ai.grinder.library.set.tuple.Tuple;

public class ExpressionVisitor extends AntlrGrinderBaseVisitor<Expression> {
	
	@Override
	public Expression visitSymbol(AntlrGrinderParser.SymbolContext ctx) { 	
		Expression result = newSymbol(ctx.getText());
		return result;
	}
	
	// parenthesis, e.g.:(1+2)
	// '(' expr ')' #parenthesesAroundExpression
	@Override 
	public Expression visitParenthesesAroundExpression(
			AntlrGrinderParser.ParenthesesAroundExpressionContext ctx) {
		Expression result = visit(ctx.expr());
		return result;
	}
	
	// function application, e.g.: f(X)
	// functor=expr '(' ( args+=expr (',' args+=expr)* )? ')' # functionApplication
	@Override 
	public Expression visitFunctionApplication(
			AntlrGrinderParser.FunctionApplicationContext ctx) {
		Expression result = Expressions.make(visit(ctx.functor), expressions(ctx.args));
// TODO - make this call?		
//		result = possiblyFlatten(result);
		
		return result;
	}
	
	// tuple, e.g.: (A, B, C)
	// '(' expr ',' expr (',' expr)* ')' #tuple
	@Override 
	public Expression visitTuple(AntlrGrinderParser.TupleContext ctx) {	
		Expression result = Tuple.make(expressions(ctx.expr()));
		return result;
	}
	
	// cardinality, e.g.: | X |
	// '|' expr '|' #cardinality
	@Override
	public Expression visitCardinality(AntlrGrinderParser.CardinalityContext ctx) {
		Expression result = Expressions.make(FunctorConstants.CARDINALITY, visit(ctx.expr()));
		return result;
	}
	
	// extensional multiset, e.g.: {{ A, B, C, C, D }}
	// '{{' ( expr (',' expr)* )? '}}' #extensionalMultiset
	@Override 
	public Expression visitExtensionalMultiset(AntlrGrinderParser.ExtensionalMultisetContext ctx) { 
		Expression result = ExtensionalSet.makeMultiSet(expressionsList(ctx.expr()));
		return result;
	}
	
	// intensional multiset, e.g.: {{ (on X) f(X) | X != a }}
	// '{{' ('(' ON ( scopeargs+=expr (',' scopeargs+=expr)* )? ')')? head=expr ('|' condition=expr)? '}}' #intensionalMultiset
	@Override
	public Expression visitIntensionalMultiset(
			AntlrGrinderParser.IntensionalMultisetContext ctx) {
		Expression scopingExpression = null;
		if (ctx.scopeargs.size() > 0) {
			scopingExpression = IntensionalSet.makeScopingExpression(expressionsList(ctx.scopeargs));
		}
		Expression head      = visit(ctx.head);
		Expression condition = null;
		if (ctx.condition != null) {
			condition = Expressions.make(IntensionalSet.CONDITION_LABEL, visit(ctx.condition));
		}
	
		Expression result = Expressions.make(IntensionalSet.MULTI_SET_LABEL, scopingExpression, head, condition);
		return result;
	}
	
	// extensional uniset, e.g.: { A, B, C, C, D }
	// '{' ( expr (',' expr)* )? '}' #extensionalUniset
	@Override
	public Expression visitExtensionalUniset(
			AntlrGrinderParser.ExtensionalUnisetContext ctx) {
		Expression result = ExtensionalSet.makeUniSet(expressionsList(ctx.expr()));
		return result;
	}
	
	// intensional uniset, e.g.: { (on X) f(X) | X != a }
	// '{' ('{' ON ( scopeargs+=expr (',' scopeargs+=expr)* )? ')')? head=expr ('|' condition=expr)? '}' #intensionalUniset
	@Override
	public Expression visitIntensionalUniset(
			AntlrGrinderParser.IntensionalUnisetContext ctx) {
		Expression scopingExpression = null;
		if (ctx.scopeargs.size() > 0) {
			scopingExpression = IntensionalSet.makeScopingExpression(expressionsList(ctx.scopeargs));
		}
		Expression head      = visit(ctx.head);
		Expression condition = null;
		if (ctx.condition != null) {
			condition = Expressions.make(IntensionalSet.CONDITION_LABEL, visit(ctx.condition));
		}
	
		Expression result = Expressions.make(IntensionalSet.UNI_SET_LABEL, scopingExpression, head, condition);
		return result;
	}
	
	// bracketed expression, for parfactors and random variables, e.g. [if p(X) then 1 else 2]
	// '[' expr ']' #bracketedExpression
	@Override
	public Expression visitBracketedExpression(
			AntlrGrinderParser.BracketedExpressionContext ctx) {
		Expression result = Expressions.make(FunctorConstants.LEFT_DOT_RIGHT, visit(ctx.expr()));
		return result;
	}
	
	// value of, e.g.: 'value of'(1 + 2)
	// VALUE OF expr #valueOf
	@Override
	public Expression visitValueOf(AntlrGrinderParser.ValueOfContext ctx) {
		Expression result = Expressions.make(FunctorConstants.VALUE_OF, visit(ctx.expr()));
		return result;
	}
	
	// underscore set, e.g.: x_{ y + 0.3 : { a, b, c } }
	// head=expr '_{' left=expr ':' right=expr '}' #underscoreCurly
	@Override
	public Expression visitUnderscoreCurly(
			AntlrGrinderParser.UnderscoreCurlyContext ctx) {
		Expression result = Expressions.make(". _{ . : . }", visit(ctx.head), visit(ctx.left), visit(ctx.right));
		return result;
	}
	
	// occurs in, e.g.: x occurs in y
	// element=expr OCCURS IN collection=expr #occursIn
	@Override
	public Expression visitOccursIn(AntlrGrinderParser.OccursInContext ctx) {
		Expression result = Expressions.make("occurs in", visit(ctx.element), visit(ctx.collection));
		return result;
	}
	
	// index of in, e.g.: index of x in y
	// INDEX OF of=expr IN in=expr #indexOfIn
	@Override
	public Expression visitIndexOfIn(AntlrGrinderParser.IndexOfInContext ctx) {
		Expression result = Expressions.make("index of . in .", visit(ctx.of), visit(ctx.in));
		return result;
	}
	
	// case statement, e.g.: case x y : z, a : b
	// CASE arg=expr ( caseconiditions+=expr ':' caseactions+=expr (',' caseconiditions+=expr ':' caseactions+=expr)* )? #caseStatement
	@Override
	public Expression visitCaseStatement(AntlrGrinderParser.CaseStatementContext ctx) {
		List<Expression> cases = new ArrayList<Expression>();
		for (int i = 0; i < ctx.caseconiditions.size(); i++) {
			cases.add(Expressions.make(":", visit(ctx.caseconiditions.get(i)), visit(ctx.caseactions.get(i))));
		}
		Expression casesList = null;
		if (cases.size() == 1) {
			casesList = cases.get(0);
		} 
		else {
			casesList = Expressions.make("kleene list", cases.toArray());
		}
		
		Expression result = Expressions.make("case", visit(ctx.arg), casesList);
		return result;
	}
	
	// not, e.g.: not A and B -> (not(A)) and B
	// NOT expr #not
	@Override
	public Expression visitNot(AntlrGrinderParser.NotContext ctx) {
		Expression result = Expressions.make(Not.FUNCTOR, visit(ctx.expr()));
		return result;
	}
	
	// negative, e.g.: 2 * -1 -> 2 * (-1)
	// '-' expr #negative
	@Override
	public Expression visitNegative(AntlrGrinderParser.NegativeContext ctx) {
		Expression result = Expressions.make(FunctorConstants.MINUS, visit(ctx.expr()));
		return result;
	}
	
	// exponentiation, e.g. 2^3^4 -> 2^(3^4)
	// base=expr '^'<assoc=right> exponent=expr #Exponentiation
	@Override
	public Expression visitExponentiation(AntlrGrinderParser.ExponentiationContext ctx) {
		Expression result = Expressions.make(FunctorConstants.EXPONENTIATION, visit(ctx.base), visit(ctx.exponent));
		return result;
	}
	
	// division, e.g.: 2*3/2 -> 2*(3/2)
	// left=expr '/' right=expr #division
	@Override
	public Expression visitDivision(AntlrGrinderParser.DivisionContext ctx) {
		Expression result = Division.make(visit(ctx.numerator), visit(ctx.denominator));
		return result;
	}
	
	// multiplication, e.g.: 1+2*3 -> 1+(2*3)
	// leftop=expr '*' rightop=expr #multiplication
	@Override
	public Expression visitMultiplication(AntlrGrinderParser.MultiplicationContext ctx) {
		Expression result = Times.make(Arrays.asList(visit(ctx.leftop), visit(ctx.rightop)));
		result = possiblyFlatten(result);
		return result;
	}
	
	// addition, e.g.: 1-2+3 -> 1-(2+3)
	// leftop=expr '+' rightop=expr #addition
	@Override
	public Expression visitAddition(AntlrGrinderParser.AdditionContext ctx) {
		Expression result = Plus.make(Arrays.asList(visit(ctx.leftop), visit(ctx.rightop)));
		result = possiblyFlatten(result);
		return result;
	}
	
	// subtraction, e.g.: 1-2
	// minuend=expr '-' subtrahend=expr #subtraction
	@Override
	public Expression visitSubtraction(AntlrGrinderParser.SubtractionContext ctx) {
		Expression result = Minus.make(visit(ctx.minuend), visit(ctx.subtrahend));
		return result;
	}
	
	// set intersection, e.g.: {a, b, c} intersection {b}
	// leftop=expr INTERSECTION rightop=expr #intersection
	@Override
	public Expression visitIntersection(
			AntlrGrinderParser.IntersectionContext ctx) {
		Expression result = Expressions.make(FunctorConstants.INTERSECTION, visit(ctx.leftop), visit(ctx.rightop));
		result = possiblyFlatten(result);
		return result;
	}
	
	// set union, {a, b, c} union {b, d}
	// leftop=expr UNION rightop=expr #union
	@Override
	public Expression visitUnion(AntlrGrinderParser.UnionContext ctx) {
		Expression result = Expressions.make(FunctorConstants.UNION, visit(ctx.leftop), visit(ctx.rightop));
		result = possiblyFlatten(result);
		return result;
	}	
	
	// set membership, x in {x, y, z}
	// leftop=expr IN rightop=expr #in
	@Override
	public Expression visitIn(AntlrGrinderParser.InContext ctx) {
		Expression result = Expressions.make(FunctorConstants.IN, visit(ctx.leftop), visit(ctx.rightop));
		return result;
	}
	
	// comparison operators, e.g.: X = Y, 2 < 3
	// leftop=expr op=('<' | '<=' | '=' | '!=' | '>=' | '>') rightop=expr #comparison
	@Override
	public Expression visitComparison(AntlrGrinderParser.ComparisonContext ctx) {
		Expression result = Expressions.make(ctx.op.getText(), visit(ctx.leftop), visit(ctx.rightop));
		result = possiblyFlatten(result);
		return result;
	}
	
	// alternative equality token 'is', e.g. x is y
	//  leftop=expr IS rightop=expr #is
	@Override
	public Expression visitIs(AntlrGrinderParser.IsContext ctx) {
		Expression result = Expressions.make(FunctorConstants.IS, visit(ctx.leftop), visit(ctx.rightop));
		return result;
	}
	
	// conjunction, e.g.: A or B and C -> A or (B and C)
	// leftconj=expr AND rightconj=expr #and
	@Override
	public Expression visitAnd(AntlrGrinderParser.AndContext ctx) {
		Expression result = And.make(visit(ctx.leftconj), visit(ctx.rightconj));
		result = possiblyFlatten(result);
		return result;
	}
	
	// disjunction, e.g.: A => B or C -> A => (B or C)
	// leftdisj=expr OR rightdisj=expr #or
	@Override
	public Expression visitOr(AntlrGrinderParser.OrContext ctx) {
		Expression result = Or.make(visit(ctx.leftdisj), visit(ctx.rightdisj));
		result = possiblyFlatten(result);
		return result;
	}
	
	// implication, e.g.: A = B => C = D
	// antecedent=expr IMPLICATION consequent=expr #implication
	@Override
	public Expression visitImplication(AntlrGrinderParser.ImplicationContext ctx) {
		Expression result = Implication.make(visit(ctx.antecedent), visit(ctx.consequent));
		return result;
	}
	
	// biconditional, e.g.: A = B <=> C = D
	// leftop=expr BICONDITIONAL rightop=expr #biconditional
	@Override
	public Expression visitBiconditional(
			AntlrGrinderParser.BiconditionalContext ctx) {
		Expression result = Expressions.make(FunctorConstants.EQUIVALENCE, visit(ctx.leftop), visit(ctx.rightop));
		return result;
	}
	
	// conditional, e.g.: if X = Y then 1 else 2
	// IF condition=expr THEN thenbranch=expr ELSE elsebranch=expr #ifThenElse
	@Override
	public Expression visitIfThenElse(AntlrGrinderParser.IfThenElseContext ctx) {
		Expression result = Expressions.make(IfThenElse.FUNCTOR, visit(ctx.condition), visit(ctx.thenbranch), visit(ctx.elsebranch));
		return result;
	}
	
	// lambda, e.g.: lambda f(X) : 2 + f(X)
	// LAMBDA ( parameters+=expr (',' parameters+=expr)* )? ':' body=expr #lamda
	@Override
	public Expression visitLamda(
			AntlrGrinderParser.LamdaContext ctx) {
		Expression result = Lambda.make(expressionsList(ctx.parameters), visit(ctx.body));
		return result;
	}
	
	// TODO what is this meant to be?
	// leftop=expr SINGLE_ARROW rightop=expr #rightArrow
	@Override
	public Expression visitRightArrow(AntlrGrinderParser.RightArrowContext ctx) {
		Expression result = Expressions.make("->", visit(ctx.leftop), visit(ctx.rightop));
		return result;
	}

	// e.g.: previous message to <<expression>> from <<expression>>
	@Override
	public Expression visitPreviousMessageToFrom(
			AntlrGrinderParser.PreviousMessageToFromContext ctx) {
		Expression result = Expressions.make("previous message to . from .", visit(ctx.to), visit(ctx.from));
		return result;
	}

	// e.g.: message to <<expression>> from <<expression>>
	@Override
	public Expression visitMessageToFrom(
			AntlrGrinderParser.MessageToFromContext ctx) {
		Expression result = Expressions.make("message to . from .", visit(ctx.to), visit(ctx.from));
		return result;
	}
	
	// e.g.: neighbors of variable <<expression>>
	@Override
	public Expression visitNeighborsOfVariable(
			AntlrGrinderParser.NeighborsOfVariableContext ctx) {
		Expression result = Expressions.make("neighbors of variable", visit(ctx.variable));
		return result;
	}
	
	// e.g.: neighbors of factor <<expression>>
	@Override
	public Expression visitNeighborsOfFactor(
			AntlrGrinderParser.NeighborsOfFactorContext ctx) {
		Expression result = Expressions.make("neighbors of factor", visit(ctx.factor));
		return result;
	}

	// e.g.: neighbors of <<expression>> from <<expression>>
	@Override
	public Expression visitNeighborsOfFrom(
			AntlrGrinderParser.NeighborsOfFromContext ctx) {
		Expression result = Expressions.make("neighbors of . from .", visit(ctx.of), visit(ctx.from));
		return result;
	}
	
	// universal quantification, e.g.: for all X : X != a
	// FOR ALL index=expr ':' body=expr #forAll
	@Override
	public Expression visitForAll(AntlrGrinderParser.ForAllContext ctx) {
		Expression result = ForAll.make(visit(ctx.index), visit(ctx.body));
		return result;
	}

	// existential quantification, e.g.: there exists X : X = a
	// THERE EXISTS index=expr ':' body=expr #thereExists
	@Override
	public Expression visitThereExists(AntlrGrinderParser.ThereExistsContext ctx) {
		Expression result = ThereExists.make(visit(ctx.index), visit(ctx.body));
		return result;
	}
	
	// an expression symbol, e.g.:<X + Y>
	// '<' expr '>' #expressionSymbol
	public Expression visitExpressionSymbol(
			AntlrGrinderParser.ExpressionSymbolContext ctx) {
		Expression result = DefaultSymbol.createSymbol(visit(ctx.expr()));
		return result;
	}

	//
	// PROTECTED
	//
	protected Expression newSymbol(String text) {
		if ((text.startsWith("'") && text.endsWith("'"))
				|| (text.startsWith("\"") && text.endsWith("\""))) {
			text = text.substring(1, text.length() - 1);
		}
		
// TODO convert escape sequences		

		text = new String(text);

		Expression result = DefaultSymbol.createSymbol(text);
		return result;
	}
	
	protected Object[] expressions(List<AntlrGrinderParser.ExprContext> exprContexts) {
		List<Expression> result = new ArrayList<Expression>();
		for (AntlrGrinderParser.ExprContext exprContext : exprContexts) {
			result.add(visit(exprContext));
		}
		return result.toArray();
	}
	
	protected List<Expression> expressionsList(List<AntlrGrinderParser.ExprContext> exprContexts) {
		List<Expression> result = new ArrayList<Expression>();
		for (AntlrGrinderParser.ExprContext exprContext : exprContexts) {
			result.add(visit(exprContext));
		}
		return result;
	}
	
	protected Expression possiblyFlatten(Expression expression) {
		Expression result = expression;
		
		Object functor = expression.getFunctor();
		if (functor != null) {
			if (functor.equals(FunctorConstants.TIMES) || functor.equals(FunctorConstants.PLUS) || 
			    functor.equals(FunctorConstants.INTERSECTION) || functor.equals(FunctorConstants.UNION) || 
			    functor.equals(FunctorConstants.EQUAL) ||
			    functor.equals(FunctorConstants.AND) || functor.equals(FunctorConstants.OR)) {
				List<Expression> args = new ArrayList<Expression>();
				for (Expression arg : expression.getArguments()) {
					if (arg.getFunctor() != null && functor.equals(arg.getFunctor())) {
						args.addAll(arg.getArguments());
					}
					else {
						args.add(arg);
					}
				}
				result = Expressions.make(functor, args.toArray());
			}
		}
		
		return result;
	}
}
