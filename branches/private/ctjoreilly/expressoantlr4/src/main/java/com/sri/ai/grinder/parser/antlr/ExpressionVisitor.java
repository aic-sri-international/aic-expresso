package com.sri.ai.grinder.parser.antlr;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.core.DefaultSymbol;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.grinder.library.FunctorConstants;
import com.sri.ai.grinder.library.boole.Not;
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
	
	// e.g.:(1+2)
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
		result = possiblyFlatten(result);
		
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
		Expression head              = visit(ctx.head);
		Expression condition         = Expressions.TRUE;
		if (ctx.condition != null) {
			condition = visit(ctx.condition);
		}
	
		Expression result = IntensionalSet.makeMultiSet(scopingExpression, head, condition);
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
		Expression head              = visit(ctx.head);
		Expression condition         = Expressions.TRUE;
		if (ctx.condition != null) {
			condition = visit(ctx.condition);
		}
	
		Expression result = IntensionalSet.makeUniSet(scopingExpression, head, condition);
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
		Expression result = Not.make(visit(ctx.expr()));
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
	
	@Override
	public Expression visitIn(AntlrGrinderParser.InContext ctx) {
		Expression result = Expressions.make("in", visit(ctx.getChild(0)), visit(ctx.getChild(2)));
		return result;
	}
	
	@Override
	public Expression visitLamda(
			AntlrGrinderParser.LamdaContext ctx) {
// TODO - implement properly		
		Expression result = Lambda.make(visit(ctx.getChild(1)), visit(ctx.getChild(3)));
		return result;
	}
	
//
//	@Override
//	public Expression visitBiconditional(
//			AntlrGrinderParser.BiconditionalContext ctx) {
//		return visitChildren(ctx);
//	}
//
//	@Override
//	public Expression visitThereExists(AntlrGrinderParser.ThereExistsContext ctx) {
//		return visitChildren(ctx);
//	}
//
//	@Override
//	public Expression visitUnion(AntlrGrinderParser.UnionContext ctx) {
//		return visitChildren(ctx);
//	}
//
//	@Override
//	public Expression visitPreviousMessageToFrom(
//			AntlrGrinderParser.PreviousMessageToFromContext ctx) {
//		return visitChildren(ctx);
//	}
//
//	@Override
//	public Expression visitMessageToFrom(
//			AntlrGrinderParser.MessageToFromContext ctx) {
//		return visitChildren(ctx);
//	}
//

//
//	@Override
//	public Expression visitIntersection(
//			AntlrGrinderParser.IntersectionContext ctx) {
//		return visitChildren(ctx);
//	}
//

//
//	@Override
//	public Expression visitIs(AntlrGrinderParser.IsContext ctx) {
//		return visitChildren(ctx);
//	}
//
//	@Override
//	public Expression visitImplication(AntlrGrinderParser.ImplicationContext ctx) {
//		return visitChildren(ctx);
//	}
//
//	@Override
//	public Expression visitRightArrow(AntlrGrinderParser.RightArrowContext ctx) {
//		return visitChildren(ctx);
//	}
//
//	@Override
//	public Expression visitOr(AntlrGrinderParser.OrContext ctx) {
//		return visitChildren(ctx);
//	}
//
//	@Override
//	public Expression visitIfThenElse(AntlrGrinderParser.IfThenElseContext ctx) {
//		return visitChildren(ctx);
//	}
//
//	@Override
//	public Expression visitNeighborsOfFrom(
//			AntlrGrinderParser.NeighborsOfFromContext ctx) {
//		return visitChildren(ctx);
//	}
//
//	@Override
//	public Expression visitComparison(AntlrGrinderParser.ComparisonContext ctx) {
//		return visitChildren(ctx);
//	}
//
//	@Override
//	public Expression visitAnd(AntlrGrinderParser.AndContext ctx) {
//		return visitChildren(ctx);
//	}
//
//	@Override
//	public Expression visitForAll(AntlrGrinderParser.ForAllContext ctx) {
//		return visitChildren(ctx);
//	}

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
	
	/**
    | ^(INTERSECTION ^(INTERSECTION (a+=.)+) b=.)  ->  ^(INTERSECTION ($a)+ $b)
    | ^(UNION        ^(UNION        (a+=.)+) b=.)  ->  ^(UNION        ($a)+ $b)
    | ^(EQUAL        ^(EQUAL        (a+=.)+) b=.)  ->  ^(EQUAL        ($a)+ $b)
    | ^(AND          ^(AND          (a+=.)+) b=.)  ->  ^(AND          ($a)+ $b)
    | ^(OR           ^(OR           (a+=.)+) b=.)  ->  ^(OR           ($a)+ $b)
	 */
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
