package com.sri.ai.expresso.optimization;

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
 * The gradient needs to be provided here, contrary to FunctionToOptimizeGradient. 
 * Used for optimization.
 * @author Sarah Perrin
 *
 */

public class GradientToOptimize implements MultivariateVectorFunction {
	
	public Expression expression;
	
	public Vector<Expression> gradient;
	
	public Theory theory;
	public Context context;
	public AutomaticDifferentiation autoDifferentiator;
	
	public GradientToOptimize(Expression expression, Vector<Expression> gradient) {
		this.expression = expression;
		this.gradient = gradient;
		this.theory = new CommonTheory();
		this.context = new TrueContext(theory);
		autoDifferentiator = new AutomaticDifferentiation(e -> context.evaluate(e));
	}
    
	/**
	 * Implementation of the method from the interface MultivariateVectorFunction from Apache Commons Math.
	 *
	 */
    @Override
	public double[] value(double[] variables) {

		Set<Expression> variablesInExpression = Expressions.freeVariables(expression, context);
		Map<Expression, Double> map = createMap(variables, variablesInExpression);
    	
    	double[] result = makeEvaluatedAndSimplifiedExpression(map, variablesInExpression, gradient);
    	
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
	private double[] makeEvaluatedAndSimplifiedExpression(Map<Expression, Double> map, Set<Expression> variablesInExpression,
			Vector<Expression> gradient) {
		
		double[] result = new double[gradient.size()];
		int i = 0;
    	for (Expression ithTermOfGradient : gradient) {
        	Expression expressionReplacedPrevious = ithTermOfGradient;
        	Expression expressionReplaced = ithTermOfGradient;
        	for (Expression e : variablesInExpression) {
        		expressionReplaced = expressionReplacedPrevious.replaceAllOccurrences(e, Expressions.makeSymbol(map.get(e)), context);
        		expressionReplacedPrevious = expressionReplaced;
        	}
        	Expression evaluatedExpression = autoDifferentiator.simplify(expressionReplaced);
			result[i] = evaluatedExpression.doubleValue();
        	i++;
    	}
    	return result;
	}

}
