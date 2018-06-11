package com.sri.ai.expresso.optimization;

import java.util.HashMap;
import java.util.Map;
import java.util.Set;

import org.apache.commons.math3.analysis.MultivariateFunction;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.autodifferentiation.AutomaticDifferentiation;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.grinder.api.Context;
import com.sri.ai.grinder.api.Theory;
import com.sri.ai.grinder.application.CommonTheory;
import com.sri.ai.grinder.core.TrueContext;

/**
 * Class to convert an Expression into a MultivariateFunction (from Apache Commons Math). 
 * Used for optimization.
 * @author Sarah Perrin
 *
 */

public class FunctionToOptimize implements MultivariateFunction {
	
	public Expression expression;
	
	public Theory theory;
	public Context context;
	public AutomaticDifferentiation autoDifferentiator;
	
	public FunctionToOptimize(Expression expression) {
		this.expression = expression;
		
		this.theory = new CommonTheory();
		this.context = new TrueContext(theory);
		autoDifferentiator = new AutomaticDifferentiation(e -> context.evaluate(e));
	}
	
	/**
	 * Implementation of the method from the interface MultivariateFunction
	 *
	 */
    public double value(double[] variables) {
    	
    	Set<Expression> variablesInExpression = Expressions.freeVariables(expression, context);
    	Map<Expression, Double> map = createMap(variables, variablesInExpression);
    	
    	Expression evaluatedExpression = makeEvaluatedAndSimplifiedExpression(map, variablesInExpression);
    	
    	double result = evaluatedExpression.doubleValue();
    	return result;
    	
    }
    
    /**
	 * Create the HashMap which associates every variable of the expression to its value.
	 *
	 */
	private Map<Expression, Double> createMap(double[] variables, Set<Expression> variablesInExpression) {
		Map<Expression, Double> result = new HashMap<Expression, Double>();
    	int i = 0;
    	for (Expression e : variablesInExpression) {
    		result.put(e, variables[i]);
    		i++;
    	}
    	return result;
	}

    /**
	 * Replace the variables of the expression with their values and simplify the result.
	 *
	 */
	private Expression makeEvaluatedAndSimplifiedExpression(Map<Expression, Double> map,
			Set<Expression> variablesInExpression) {
		Expression expressionReplacedPrevious = expression;
    	Expression expressionReplaced = expression;
    	for (Expression e : variablesInExpression) {
    		expressionReplaced = expressionReplacedPrevious.replaceAllOccurrences(e, Expressions.makeSymbol(map.get(e)), context);
    		expressionReplacedPrevious = expressionReplaced;
    	}
    	Expression result = autoDifferentiator.simplify(expressionReplaced);
		return result;
	}

}