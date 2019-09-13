package com.sri.ai.expresso.smt.core;

import static com.sri.ai.expresso.helper.Expressions.FALSE;
import static com.sri.ai.expresso.helper.Expressions.TRUE;
import static com.sri.ai.util.Util.mapIntoArrayList;
import java.util.List;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.core.DefaultFunctionApplication;
import com.sri.ai.expresso.smt.api.SMTBasedEvaluator;
import com.sri.ai.expresso.smt.api.SMTBasedContext;
import com.sri.ai.grinder.api.Context;
import com.sri.ai.grinder.api.Theory;
import com.sri.ai.grinder.library.controlflow.IfThenElse;

public class DefaultSMTBasedEvaluator implements SMTBasedEvaluator {
	
	@Override
	public Expression eval(Expression expression, SMTBasedContext smtContext) {
		Expression unsimplifiedResult = null;
		if(smtContext.isVariable(expression) || smtContext.isUniquelyNamedConstant(expression)) {
			unsimplifiedResult = expression;
		}
		else {
			Expression literal = findLiteral(expression, smtContext);
			if(literal == null) {
				unsimplifiedResult = expression;
			}
			else {
				Theory theory = smtContext.getTheory();
				Expression literalNegation = theory.getLiteralNegation(literal, smtContext);	
				
				boolean literalIsSatisfiable = isSatisfiable(smtContext, literal);
				boolean literalNegationIsSatisfiable = isSatisfiable(smtContext, literalNegation);
				
				Expression expressionUnderTrueLiteralValue;
				Expression expressionUnderFalseLiteralValue;
				Expression evaluatedExpressionUnderTrueLiteralValue;
				Expression evaluatedExpressionUnderFalseLiteralValue;
				if(literalIsSatisfiable && literalNegationIsSatisfiable) {
					expressionUnderTrueLiteralValue = makeExpressionUnderLiteralValue(expression, literal, TRUE, smtContext);
					evaluatedExpressionUnderTrueLiteralValue = eval(expressionUnderTrueLiteralValue, (SMTBasedContext) smtContext.conjoin(literal));
					smtContext.popStackFrame();

					
					expressionUnderFalseLiteralValue = makeExpressionUnderLiteralValue(expression, literal, FALSE, smtContext);
					evaluatedExpressionUnderFalseLiteralValue = eval(expressionUnderFalseLiteralValue, (SMTBasedContext) smtContext.conjoin(literalNegation));
					smtContext.popStackFrame();

					
					unsimplifiedResult = IfThenElse.make(literal, evaluatedExpressionUnderTrueLiteralValue, evaluatedExpressionUnderFalseLiteralValue);
				}
				else if(literalIsSatisfiable) {
					expressionUnderTrueLiteralValue = makeExpressionUnderLiteralValue(expression, literal, TRUE, smtContext);
					evaluatedExpressionUnderTrueLiteralValue = eval(expressionUnderTrueLiteralValue, (SMTBasedContext) smtContext.conjoin(literal));
					smtContext.popStackFrame();

					
					unsimplifiedResult = evaluatedExpressionUnderTrueLiteralValue;
				}
				else {
					expressionUnderFalseLiteralValue = makeExpressionUnderLiteralValue(expression, literal, FALSE, smtContext);
					evaluatedExpressionUnderFalseLiteralValue = eval(expressionUnderFalseLiteralValue, (SMTBasedContext) smtContext.conjoin(literalNegation));
					smtContext.popStackFrame();

					
					unsimplifiedResult = evaluatedExpressionUnderFalseLiteralValue;
				}
			}
		}
		Expression simplifiedResult = unconditionalEval(unsimplifiedResult, smtContext);
				
		return simplifiedResult;
	}
	


	private Expression unconditionalEval(Expression expression, SMTBasedContext smtContext) {
// NEW
		Expression result = null;
		
		Expression functor = expression.getFunctor();
		if(functor == null) {
			if(smtContext.isUniquelyNamedConstant(expression)) {
				result = expression;
			}
			else if(smtContext.isVariable(expression)) {
				Expression valueOfVariableBasedOnContext = smtContext.getValueOfVariable(expression);
				if(valueOfVariableBasedOnContext == null) {
					result = expression;
				}
				else {
					result = valueOfVariableBasedOnContext;
				}
			}	
		}
		else {
			Theory theory = smtContext.getTheory();
			List<Expression> functionArguments = expression.getArguments();
			List<Expression> simplifiedFunctionArguments = mapIntoArrayList(functionArguments, (a)->unconditionalEval(a,smtContext));
			Expression newFunctionApplication = new DefaultFunctionApplication(functor,simplifiedFunctionArguments);
			Expression simplifiedNewFunctionApplication = theory.getTopRewriter().apply(newFunctionApplication, smtContext);
			while(newFunctionApplication != simplifiedNewFunctionApplication) {
				newFunctionApplication = simplifiedNewFunctionApplication;
				simplifiedNewFunctionApplication = theory.getTopRewriter().apply(newFunctionApplication, smtContext);
			}
			result = simplifiedNewFunctionApplication;
		}
		return result;
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
			literal = findLiteralInSubExpressions(expression, context);
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
	
	public boolean isSatisfiable(Context context, Expression literal) {
		return !context.isContradiction(literal);
	}
}