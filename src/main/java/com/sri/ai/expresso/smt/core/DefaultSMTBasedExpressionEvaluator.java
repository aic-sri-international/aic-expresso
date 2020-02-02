package com.sri.ai.expresso.smt.core;

import static com.sri.ai.expresso.helper.Expressions.FALSE;
import static com.sri.ai.expresso.helper.Expressions.TRUE;
import static com.sri.ai.util.Util.mapIntoArrayList;
import static com.sri.ai.util.Util.myAssert;
import java.util.List;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.core.DefaultFunctionApplication;
import com.sri.ai.expresso.smt.api.SMTBasedContext;
import com.sri.ai.grinder.api.Context;
import com.sri.ai.grinder.api.ExpressionEvaluator;
import com.sri.ai.grinder.api.Theory;
import com.sri.ai.grinder.library.controlflow.IfThenElse;

public class DefaultSMTBasedExpressionEvaluator implements ExpressionEvaluator {
	
	private static long topRewriteTime = 0;
	private static long literalNegationTime = 0;
	private static long expressionEqualsTime = 0;
	private static long expressionIsVariableOrConstantTime = 0;
	private static long isContradictionTime = 0;
	private static long isLiteralTime = 0;
	private static long makeExpressionUnderLiteralValueTime = 0;
	

	@Override
	public Expression eval(Expression expression, Context context) {
		myAssert(SMTBasedContext.class.isAssignableFrom(context.getClass()), ()->"ERROR: " + this.getClass().getSimpleName() + " only works with SMTBasedContext derived classes.  The context passed in was of class "+ context.getClass().getSimpleName() + "!!!");
		SMTBasedContext smtBasedContext = (SMTBasedContext) context;
		Expression unsimplifiedResult = null;
		
		long initialTime = System.currentTimeMillis();
		boolean isVariable = smtBasedContext.isVariable(expression);
		boolean isConstant = smtBasedContext.isUniquelyNamedConstant(expression);
		long finalTime = System.currentTimeMillis();
		expressionIsVariableOrConstantTime += (finalTime - initialTime);
		
		if(isVariable || isConstant) {
			unsimplifiedResult = expression;
		}
		else {
			Expression literal = findLiteral(expression, smtBasedContext);
			if(literal == null) {
				unsimplifiedResult = expression;
			}
			else {
				Theory theory = smtBasedContext.getTheory();
				
				initialTime = System.currentTimeMillis();
				Expression literalNegation = theory.getLiteralNegation(literal, smtBasedContext);	
				finalTime = System.currentTimeMillis();
				literalNegationTime += (finalTime - initialTime);
				
				boolean literalIsSatisfiable = isSatisfiable(smtBasedContext, literal);
				boolean literalNegationIsSatisfiable = isSatisfiable(smtBasedContext, literalNegation);
				
				Expression expressionUnderTrueLiteralValue;
				Expression expressionUnderFalseLiteralValue;
				Expression evaluatedExpressionUnderTrueLiteralValue;
				Expression evaluatedExpressionUnderFalseLiteralValue;
				if(literalIsSatisfiable && literalNegationIsSatisfiable) {
					expressionUnderTrueLiteralValue = makeExpressionUnderLiteralValue(expression, literal, TRUE, smtBasedContext);
					evaluatedExpressionUnderTrueLiteralValue = eval(expressionUnderTrueLiteralValue, (SMTBasedContext) smtBasedContext.conjoin(literal));
					smtBasedContext.popStackFrame();

					
					expressionUnderFalseLiteralValue = makeExpressionUnderLiteralValue(expression, literal, FALSE, smtBasedContext);
					evaluatedExpressionUnderFalseLiteralValue = eval(expressionUnderFalseLiteralValue, (SMTBasedContext) smtBasedContext.conjoin(literalNegation));
					smtBasedContext.popStackFrame();

					
					unsimplifiedResult = IfThenElse.make(literal, evaluatedExpressionUnderTrueLiteralValue, evaluatedExpressionUnderFalseLiteralValue);
				}
				else if(literalIsSatisfiable) {
					expressionUnderTrueLiteralValue = makeExpressionUnderLiteralValue(expression, literal, TRUE, smtBasedContext);
					evaluatedExpressionUnderTrueLiteralValue = eval(expressionUnderTrueLiteralValue, (SMTBasedContext) smtBasedContext.conjoin(literal));
					smtBasedContext.popStackFrame();

					
					unsimplifiedResult = evaluatedExpressionUnderTrueLiteralValue;
				}
				else {
					expressionUnderFalseLiteralValue = makeExpressionUnderLiteralValue(expression, literal, FALSE, smtBasedContext);
					evaluatedExpressionUnderFalseLiteralValue = eval(expressionUnderFalseLiteralValue, (SMTBasedContext) smtBasedContext.conjoin(literalNegation));
					smtBasedContext.popStackFrame();

					
					unsimplifiedResult = evaluatedExpressionUnderFalseLiteralValue;
				}
			}
		}
		Expression result = unconditionalEval(unsimplifiedResult, smtBasedContext);
				
		return result;
	}
	


	private Expression unconditionalEval(Expression expression, SMTBasedContext smtBasedContext) {
		Expression result = null;
		
		Expression functor = expression.getFunctor();
		if(functor == null) {
			if(smtBasedContext.isUniquelyNamedConstant(expression)) {
				result = expression;
			}
			else if(smtBasedContext.isVariable(expression)) {
				Expression valueOfVariableBasedOnContext = smtBasedContext.getValueOfVariable(expression);
				if(valueOfVariableBasedOnContext == null) {
					result = expression;
				}
				else {
					result = valueOfVariableBasedOnContext;
				}
			}	
		}
		else {
			Theory theory = smtBasedContext.getTheory();
			List<Expression> functionArguments = expression.getArguments();
			List<Expression> simplifiedFunctionArguments = mapIntoArrayList(functionArguments, (a)->unconditionalEval(a,smtBasedContext));
			Expression newFunctionApplication = new DefaultFunctionApplication(functor,simplifiedFunctionArguments);
			
			long initialTime = System.currentTimeMillis();
			Expression simplifiedNewFunctionApplication = theory.getTopRewriter().apply(newFunctionApplication, smtBasedContext);
			long finalTime = System.currentTimeMillis();
			topRewriteTime += (finalTime - initialTime);
			
			while(newFunctionApplication != simplifiedNewFunctionApplication) {
				newFunctionApplication = simplifiedNewFunctionApplication;
				initialTime = System.currentTimeMillis();
				simplifiedNewFunctionApplication = theory.getTopRewriter().apply(newFunctionApplication, smtBasedContext);
				finalTime = System.currentTimeMillis();
				topRewriteTime += (finalTime - initialTime);
			}
			result = simplifiedNewFunctionApplication;
		}
		return result;
	}

	private Expression makeExpressionUnderLiteralValue(Expression expression, Expression literal, Expression literalValue,
			Context context) {
		Expression modifiedExpression;
		
		long initialTime = System.currentTimeMillis();
		boolean expressionIsLiteral = expression.equals(literal);
		long finalTime = System.currentTimeMillis();
		expressionEqualsTime += (finalTime - initialTime);
		
		if(expressionIsLiteral) {
			modifiedExpression = literalValue;
		}
		else {
			modifiedExpression = makeExpressionUnderLiteralValueWithoutCheckingIfRootExpressionIsLiteral(expression, literal, literalValue, context);
		}
		finalTime = System.currentTimeMillis();
		makeExpressionUnderLiteralValueTime += (finalTime - initialTime);
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
			long initialTime = System.currentTimeMillis();
			modifiedExpression = context.getTheory().getTopRewriter().apply(modifiedExpression, context);
			long finalTime = System.currentTimeMillis();
			topRewriteTime += (finalTime - initialTime);
		}
		
		subExpressions = modifiedExpression.getArguments();
		for(Expression subExpression : subExpressions) {
			Expression modifiedSubExpression = makeExpressionUnderLiteralValueWithoutCheckingIfRootExpressionIsLiteral(
					subExpression, literal, literalValue, context);
			if(modifiedSubExpression != subExpression) {
				modifiedExpression = modifiedExpression.replaceFirstOccurrence(subExpression, modifiedSubExpression, context);
			}
		}
		return modifiedExpression;
	}

	private Expression findLiteral(Expression expression, Context context) {
		//TODO: add to Expressions methods
		Expression literal;
		
		long initialTime = System.currentTimeMillis();
		boolean isLiteral = context.isLiteral(expression);
		long finalTime = System.currentTimeMillis();
		isLiteralTime += (finalTime - initialTime);
		
		if(isLiteral) {
			literal = expression;
		}
		else {
			literal = findLiteralInSubExpressions(expression, context);
		}
		return literal;
	}
	
	private Expression findLiteralInSubExpressions(Expression expression, Context context) {
		//TODO: add to Expressions methods
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
		long initialTime = System.currentTimeMillis();
		boolean isSAT = !context.isContradiction(literal);
		long finalTime = System.currentTimeMillis();
		isContradictionTime += (finalTime - initialTime);
		return isSAT;
	}
	
	public long getTopRewriteTime() {
		return topRewriteTime;
	}
	public long getLiteralNegationTime() {
		return literalNegationTime;
	}
	public long getExpressionEqualsTime() {
		return expressionEqualsTime;
	}
	public long getIsVariableOrConstantTime() {
		return expressionIsVariableOrConstantTime;
	}
	public long getIsContradictionTime() {
		return isContradictionTime;
	}
	public long getIsLiteralTime() {
		return isLiteralTime;
	}
	public long getMakeExpressionUnderLiteralValueTime() {
		return makeExpressionUnderLiteralValueTime;
	}
	
}
