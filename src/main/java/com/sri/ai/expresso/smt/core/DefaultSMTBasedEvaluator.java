package com.sri.ai.expresso.smt.core;

import static com.sri.ai.expresso.helper.Expressions.FALSE;
import static com.sri.ai.expresso.helper.Expressions.TRUE;
import static com.sri.ai.util.Util.println;

import java.util.List;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.smt.api.SMTBasedEvaluator;
import com.sri.ai.expresso.smt.api.SMTBasedContext;
import com.sri.ai.expresso.smt.api.SMTModel;
import com.sri.ai.expresso.smt.core.yices.YicesExpression;
import com.sri.ai.grinder.api.Context;
import com.sri.ai.grinder.api.Theory;
import com.sri.ai.grinder.library.controlflow.IfThenElse;
import com.sri.ai.util.base.Pair;

public class DefaultSMTBasedEvaluator implements SMTBasedEvaluator {
	
	@Override
	public Expression eval(Expression expression, Context context) {
		Expression unsimplifiedResult = null;
		Expression literal = findLiteral(expression, context);
		if(literal == null) {
			unsimplifiedResult = unconditionalEval(expression, context);
		}
		else {
			Theory theory = context.getTheory();
			Expression literalNegation = theory.getLiteralNegation(literal, context);	
			
			boolean literalIsSatisfiable = isSatisfiable(context, literal);
			boolean literalNegationIsSatisfiable = isSatisfiable(context, literalNegation);
			
			Expression expressionUnderTrueLiteralValue;
			Expression expressionUnderFalseLiteralValue;
			Expression evaluatedExpressionUnderTrueLiteralValue;
			Expression evaluatedExpressionUnderFalseLiteralValue;
			if(literalIsSatisfiable && literalNegationIsSatisfiable) {
				expressionUnderTrueLiteralValue = makeExpressionUnderLiteralValue(expression, literal, TRUE, context);
				evaluatedExpressionUnderTrueLiteralValue = eval(expressionUnderTrueLiteralValue, context.conjoin(literal));
				
				expressionUnderFalseLiteralValue = makeExpressionUnderLiteralValue(expression, literal, FALSE, context);
				evaluatedExpressionUnderFalseLiteralValue = eval(expressionUnderFalseLiteralValue, context.conjoin(literalNegation));
				
				unsimplifiedResult = IfThenElse.make(literal, evaluatedExpressionUnderTrueLiteralValue, evaluatedExpressionUnderFalseLiteralValue);
			}
			else if(literalIsSatisfiable) {
				expressionUnderTrueLiteralValue = makeExpressionUnderLiteralValue(expression, literal, TRUE, context);
				evaluatedExpressionUnderTrueLiteralValue = eval(expressionUnderTrueLiteralValue, context.conjoin(literal));
				
				unsimplifiedResult = expressionUnderTrueLiteralValue;
			}
			else {
				expressionUnderFalseLiteralValue = makeExpressionUnderLiteralValue(expression, literal, FALSE, context);
				evaluatedExpressionUnderFalseLiteralValue = eval(expressionUnderFalseLiteralValue, context.conjoin(literalNegation));
				
				unsimplifiedResult = expressionUnderFalseLiteralValue;
			}
		}
		Expression simplifiedResult = unconditionalEval(unsimplifiedResult, context);
		return simplifiedResult;
	}

	private Expression unconditionalEval(Expression expression, Context context) {
		// TODO Auto-generated method stub
		return null;
	}
	
	private Expression makeExpressionUnderLiteralValue(Expression expression, Expression literal, Expression literalValue,
			Context context) {
		Expression modifiedExpression;
		
		if(expression.equals(literal)) {
			modifiedExpression = literalValue;
		}
		else {
			modifiedExpression = makeExpressionUnderLiteralValueWithoutCheckingIfRootExpressionIsLiteral(expression, literal, literalValue, context);
		}
		return modifiedExpression;
	}
	
	private Expression makeExpressionUnderLiteralValueWithoutCheckingIfRootExpressionIsLiteral(Expression expression, Expression literal, Expression literalValue,
			Context context) {
		Expression modifiedExpression = expression;
		
		//TODO: check if literal is found specifically in the clause argument of the expression (as opposed to just any argument) so that top-rewriting is guaranteed to simplify the root
		//TODO: also check if a literal constant (such as TRUE or FALSE) is found in the clause argument (since top-rewriting is warranted here as well)
		boolean literalFoundInArgument = false;
		List<Expression> subExpressions = expression.getArguments();
		for(Expression subExpression : subExpressions) {
			if(subExpression.equals(literal)) {
				literalFoundInArgument = true;
				modifiedExpression = modifiedExpression.replaceFirstOccurrence(subExpression, literalValue, context);
			}
		}
		if(literalFoundInArgument) {
			modifiedExpression = context.getTheory().getTopRewriter().apply(modifiedExpression, context);
		}
		
		subExpressions = modifiedExpression.getArguments();
		for(Expression subExpression : subExpressions) {
			Expression modifiedSubExpression = makeExpressionUnderLiteralValueWithoutCheckingIfRootExpressionIsLiteral(
					subExpression, literal, literalValue, context);
			if(modifiedSubExpression != subExpression) {
				modifiedExpression = modifiedExpression.replaceFirstOccurrence(subExpression, modifiedSubExpression, context);
			}
		}
		//TODO: Check again for literal and top-rewrite?
		return modifiedExpression;
	}

	private Expression findLiteral(Expression expression, Context context) {
		//TODO: add to Expressions methods
		//TODO: make iterative instead of recursive
		Expression literal = null;
		if(context.isLiteral(expression)) {
			literal = expression;
		}
		else {
			findLiteralInSubExpressions(expression, context);
		}
		return literal;
	}
	
	private Expression findLiteralInSubExpressions(Expression expression, Context context) {
		//TODO: add to Expressions methods
		//TODO: make iterative instead of recursive
		Expression literal = null;
		List<Expression> subExpressions = expression.getArguments();
		for(Expression subExpression : subExpressions) {
			literal = findLiteral(subExpression, context);
			if(literal != null) {
				break;
			}
		}
		return literal;
	}
	
	private Expression findOccurrenceOfExpressionContainingLiteral(Expression expression, Expression literal, Context context) {
		//TODO: add to Expressions methods
		//TODO: make iterative instead of recursive
		Expression literalParentExpression = null;
		if(expression.equals(literal)) {
			literalParentExpression = expression;
		}
		else {
			literalParentExpression = 
					findOccurrenceOfExpressionContainingLiteralWithoutCheckingIfTheRootExpressionIsTheLiteral(expression, literal, context);
		}
		return literalParentExpression;
	}
	
	private Expression findOccurrenceOfExpressionContainingLiteralWithoutCheckingIfTheRootExpressionIsTheLiteral(
			Expression expression, Expression literal, Context context) {
		//TODO: add to Expressions methods
		//TODO: make iterative instead of recursive
		Expression literalParentExpression = null;
		List<Expression> subExpressions = expression.getArguments();
		for(Expression subExpression : subExpressions) {
			if(subExpression.equals(literal)) {
				literalParentExpression = expression;
			}
			else {
				literalParentExpression = findOccurrenceOfExpressionContainingLiteralWithoutCheckingIfTheRootExpressionIsTheLiteral(
						subExpression, literal, context);
			}
			if(literalParentExpression != null) {
				break;
			}
		}
		return literalParentExpression;
	}

	
	public boolean isSatisfiable(Context context, Expression literal) {
		return !context.isContradiction(literal);
	}
}
