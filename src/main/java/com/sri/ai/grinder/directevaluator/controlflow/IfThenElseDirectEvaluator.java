package com.sri.ai.grinder.directevaluator.controlflow;

import static com.sri.ai.expresso.helper.Expressions.TRUE;
import static com.sri.ai.expresso.helper.Expressions.FALSE;

import static com.sri.ai.grinder.library.controlflow.IfThenElse.isIfThenElse;
import static com.sri.ai.expresso.helper.NormalizedExpression.normalize;
import static com.sri.ai.expresso.helper.NormalizedExpression.wrapAsNormalized;


import java.io.InvalidClassException;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.helper.NormalizedExpression;
import com.sri.ai.grinder.api.Context;
import com.sri.ai.grinder.directevaluator.api.DirectEvaluator;
import com.sri.ai.grinder.library.controlflow.IfThenElse;

public class IfThenElseDirectEvaluator implements DirectEvaluator {




	public NormalizedExpression evaluate(Expression expression, Context context) throws InvalidClassException {
		NormalizedExpression result;
		if (isIfThenElse(expression)) {
			result = ifThenElseEvaluate(expression, context);
		}
		else {
			InvalidClassException e = new InvalidClassException("IfThenElseDirectEvaluator created to evaluate the expression: " + expression);
			throw e;
		}
		return result;
	}

	private NormalizedExpression ifThenElseEvaluate(Expression expression, Context context) {
		Expression condition = IfThenElse.condition(expression);
		Expression thenBranch = IfThenElse.thenBranch(expression);
		Expression elseBranch = IfThenElse.elseBranch(expression);
		
		NormalizedExpression result;
		
		NormalizedExpression normalizedConditionUnderContext = normalize(condition, context);
		if(normalizedConditionUnderContext.getInnerExpression() == TRUE)	{ //short circuit to "then" branch
			result = normalize(thenBranch, context);
		}
		else if(normalizedConditionUnderContext.getInnerExpression() == FALSE) { //short circuit to "else" branch
			result = normalize(elseBranch, context);
		}
		else {
			result = generateNormalizedIfThenElseSolution(normalizedConditionUnderContext, thenBranch, elseBranch);
		}

		return result;
	}

	private NormalizedExpression generateNormalizedIfThenElseSolution(NormalizedExpression NormalizedIfThenElseCondition,			Expression trueExpression, Expression falseExpression) {
		
		Expression literal = IfThenElse.condition(NormalizedIfThenElseCondition);
		Context context = NormalizedIfThenElseCondition.getContext();
		NormalizedExpression thenBranch = (NormalizedExpression) IfThenElse.thenBranch(NormalizedIfThenElseCondition);
		NormalizedExpression elseBranch = (NormalizedExpression) IfThenElse.elseBranch(NormalizedIfThenElseCondition);
		
		NormalizedExpression result;
		
		if(literal == TRUE)	{ //TRUE leaf reached
			result = normalize(trueExpression, context);
		}
		else if(literal == FALSE) { //FALSE leaf reached
			result = normalize(falseExpression, context);
		}
		else {
			Expression resultCondition = literal;
			Expression resultThenBranch = generateNormalizedIfThenElseSolution(thenBranch, trueExpression, falseExpression);
			Expression resultElseBranch = generateNormalizedIfThenElseSolution(elseBranch, trueExpression, falseExpression);
			
			Expression resultingIfThenElse = IfThenElse.make(resultCondition, resultThenBranch, resultElseBranch);
			
			result = wrapAsNormalized(resultingIfThenElse, context);
		}

		return result;
	}

}












