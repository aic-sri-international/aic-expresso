package com.sri.ai.expresso.minimization;

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
 * Used for minimization.
 * @author Sarah Perrin
 *
 */

public class FunctionToMinimize implements MultivariateFunction {
	
	public Expression expression;
	
	public Theory theory;
	public Context context;
	public AutomaticDifferentiation autoDifferentiator;
	
	public FunctionToMinimize(Expression expression) {
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
    	
    	Map<Expression, Double> map = new HashMap<Expression, Double>();
    	Set<Expression> variablesInExpression = createMap(variables, map);
    	
    	Expression evaluatedExpression = makeEvaluatedAndSimplifiedExpression(map, variablesInExpression);
    	
    	double result = evaluatedExpression.doubleValue();
    	return result;
    	
    }
    
    /**
	 * Create the HashMap which associates every variable of the expression to its value.
	 *
	 */
	private Set<Expression> createMap(double[] variables, Map<Expression, Double> map) {
		Set<Expression> variablesInExpression = Expressions.freeVariables(expression, context);
    	int i = 0;
    	for (Expression e : variablesInExpression) {
    		map.put(e, variables[i]);
    		i++;
    	}
		return variablesInExpression;
	}

    /**
	 * Replace the variables of the expression with their values and simplify the result.
	 *
	 */
	private Expression makeEvaluatedAndSimplifiedExpression(Map<Expression, Double> map,
			Set<Expression> variablesInExpression) {
		Expression expressionReplacedPrev = expression;
    	Expression expressionReplaced = expression;
    	for (Expression e : variablesInExpression) {
    		expressionReplaced = expressionReplacedPrev.replaceAllOccurrences(e, Expressions.makeSymbol(map.get(e)), context);
    		expressionReplacedPrev = expressionReplaced;
    	}
    	Expression evaluatedExpression = autoDifferentiator.simplify(expressionReplaced);
		return evaluatedExpression;
	}

}