package com.sri.ai.grinder.parser.antlr;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.core.DefaultSymbol;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.grinder.library.lambda.Lambda;
import com.sri.ai.grinder.library.number.Plus;
import com.sri.ai.grinder.library.set.tuple.Tuple;

public class ExpressionVisitor extends AntlrGrinderBaseVisitor<Expression> {
	
	@Override
	public Expression visitSymbol(AntlrGrinderParser.SymbolContext ctx) { 
System.out.println("s="+ctx.getText());		
		Expression result = newSymbol(ctx.getText());
		return result;
	}
	
	@Override // '(' <<expression>> ')'
	public Expression visitParenthesesAroundExpression(
			AntlrGrinderParser.ParenthesesAroundExpressionContext ctx) {	
System.out.println("()="+ctx.getText());				
		return visit(ctx.getChild(1));
	}
	
	@Override // function application, e.g.: f(X)
	public Expression visitFunctionApplication(
			AntlrGrinderParser.FunctionApplicationContext ctx) {
System.out.println("f()="+ctx.getText());		
		Expression functor = visit(ctx.getChild(0));
		Expression result = null;
		if (ctx.getChildCount() >= 4) {
			result = Expressions.make(functor, argList(ctx, 2, ctx.getChildCount()-1));
		}
		else {
			result = Expressions.make(functor);
		}
		return result;
	}
	
	@Override // tuple, e.g.: (A, B, C)
	public Expression visitTuple(AntlrGrinderParser.TupleContext ctx) {
System.out.println("tuple()="+ctx.getText());		
		Expression result = Tuple.make(argList(ctx, 1, ctx.getChildCount()-1));
		return result;
	}
	
	
	
	
	@Override
	public Expression visitPlus(AntlrGrinderParser.PlusContext ctx) {
		Expression result = Plus.make(Arrays.asList(visit(ctx.getChild(0)), visit(ctx.getChild(2))));
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
		Expression result = Lambda.make(visit(ctx.getChild(1)), visit(ctx.getChild(3)));
		return result;
	}
	
//
//	@Override
//	public Expression visitBracketedExpression(
//			AntlrGrinderParser.BracketedExpressionContext ctx) {
//		return visitChildren(ctx);
//	}
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
//	public Expression visitDivision(AntlrGrinderParser.DivisionContext ctx) {
//		return visitChildren(ctx);
//	}
//
//	@Override
//	public Expression visitTimes(AntlrGrinderParser.TimesContext ctx) {
//		return visitChildren(ctx);
//	}
//
//	@Override
//	public Expression visitIntensionalUniset(
//			AntlrGrinderParser.IntensionalUnisetContext ctx) {
//		return visitChildren(ctx);
//	}
//
//	@Override
//	public Expression visitUnion(AntlrGrinderParser.UnionContext ctx) {
//		return visitChildren(ctx);
//	}
//
//	@Override
//	public Expression visitPower(AntlrGrinderParser.PowerContext ctx) {
//		return visitChildren(ctx);
//	}
//
//	@Override
//	public Expression visitExtensionalUniset(
//			AntlrGrinderParser.ExtensionalUnisetContext ctx) {
//		return visitChildren(ctx);
//	}
//
//	@Override
//	public Expression visitValueOf(AntlrGrinderParser.ValueOfContext ctx) {
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
//	public Expression visitUnderscoreCurly(
//			AntlrGrinderParser.UnderscoreCurlyContext ctx) {
//		return visitChildren(ctx);
//	}
//
//
//	@Override
//	public Expression visitIndexOfIn(AntlrGrinderParser.IndexOfInContext ctx) {
//		return visitChildren(ctx);
//	}
//
//	@Override
//	public Expression visitComparison(AntlrGrinderParser.ComparisonContext ctx) {
//		return visitChildren(ctx);
//	}
//
//	@Override
//	public Expression visitOccursIn(AntlrGrinderParser.OccursInContext ctx) {
//		return visitChildren(ctx);
//	}
//
//	@Override
//	public Expression visitAnd(AntlrGrinderParser.AndContext ctx) {
//		return visitChildren(ctx);
//	}
//
//	@Override
//	public Expression visitNot(AntlrGrinderParser.NotContext ctx) {
//		return visitChildren(ctx);
//	}
//
//	@Override
//	public Expression visitSubtract(AntlrGrinderParser.SubtractContext ctx) {
//		return visitChildren(ctx);
//	}
//
//	@Override
//	public Expression visitForAll(AntlrGrinderParser.ForAllContext ctx) {
//		return visitChildren(ctx);
//	}
//
//	@Override
//	public Expression visitNegative(AntlrGrinderParser.NegativeContext ctx) {
//		return visitChildren(ctx);
//	}
//
//	@Override
//	public Expression visitCase(AntlrGrinderParser.CaseContext ctx) {
//		return visitChildren(ctx);
//	}
//
//	@Override
//	public Expression visitCardinality(AntlrGrinderParser.CardinalityContext ctx) {
//		return visitChildren(ctx);
//	}
//
//	@Override
//	public Expression visitIntensionalMultiset(
//			AntlrGrinderParser.IntensionalMultisetContext ctx) {
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
	
	protected Object[] argList(AntlrGrinderParser.ExprContext ctx, int from, int to) {
		List<Expression> result = new ArrayList<Expression>();
		for (int i = from; i < to; i +=2) {
			result.add(visit(ctx.getChild(i)));
		}
		return result.toArray();
	}
	
	protected Object[] childExpressions(AntlrGrinderParser.ExprContext ctx) {
		List<Expression> result = new ArrayList<Expression>();
		for (int i = 0; i < ctx.getChildCount(); i++) {
			result.add(visit(ctx.getChild(i)));
		}
		return result.toArray();
	}
}
