package com.sri.ai.expresso.minimization;

import java.util.HashMap;
import java.util.Map;
import java.util.Set;
import java.util.Vector;

import org.apache.commons.math3.analysis.MultivariateVectorFunction;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.autodifferentiation.AutomaticDifferentiation;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.grinder.api.Context;
import com.sri.ai.grinder.api.Theory;
import com.sri.ai.grinder.application.CommonTheory;
import com.sri.ai.grinder.core.TrueContext;

/**
 * Class to convert an Expression into a MultivariateVectorFunction (from Apache Commons Math), which corresponds to its gradient. 
 * Used for minimization.
 * @author Sarah Perrin
 *
 */

public class FunctionToMinimizeGradient implements MultivariateVectorFunction {
	
	public Expression expression;
	
	public Theory theory;
	public Context context;
	public AutomaticDifferentiation autoDifferentiator;
	
	public FunctionToMinimizeGradient(Expression expression) {
		this.expression = expression;
		
		this.theory = new CommonTheory();
		this.context = new TrueContext(theory);
		autoDifferentiator = new AutomaticDifferentiation(e -> context.evaluate(e));
	}
	
	/**
	 * Build the gradient vector from an Expression by differentiating the expression.
	 *
	 */
	public Vector<Expression> buildGradientVectorFrom(Expression expression) {
		
		Set<Expression> variablesInExpression = Expressions.freeVariables(expression, context);
		Vector<Expression> result = new Vector<Expression>();
		for (Expression arg : variablesInExpression) {
			result.add(autoDifferentiator.differentiateExpression(expression, arg));
		}
		return result;
	}
    
	/**
	 * Implementation of the method from the interface MultivariateVectorFunction
	 *
	 */
    public double[] value(double[] variables) {
		
    	Map<Expression, Double> map = new HashMap<Expression, Double>();
    	Set<Expression> variablesInExpression = createMap(variables, map);
    	
    	Vector<Expression> gradient = buildGradientVectorFrom(expression);
    	
    	double[] result = makeEvaluatedAndSimplifiedExpression(map, variablesInExpression, gradient);
    	
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
	private double[] makeEvaluatedAndSimplifiedExpression(Map<Expression, Double> map, Set<Expression> variablesInExpression,
			Vector<Expression> gradient) {
		
		double[] result = new double[gradient.size()];
		int j = 0;
    	for (Expression jthTermOfGradient : gradient) {
        	Expression expressionReplacedPrev = jthTermOfGradient;
        	Expression expressionReplaced = jthTermOfGradient;
        	for (Expression e : variablesInExpression) {
        		expressionReplaced = expressionReplacedPrev.replaceAllOccurrences(e, Expressions.makeSymbol(map.get(e)), context);
        		expressionReplacedPrev = expressionReplaced;
        	}
        	
        	Expression evaluatedExpression = autoDifferentiator.simplify(expressionReplaced);
			result[j] = evaluatedExpression.doubleValue();
        	j++;
    	}
    	return result;
	}
}
