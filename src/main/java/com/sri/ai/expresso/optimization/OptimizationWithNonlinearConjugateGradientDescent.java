package com.sri.ai.expresso.optimization;


import org.apache.commons.math3.analysis.MultivariateVectorFunction;
import org.apache.commons.math3.optim.InitialGuess;
import org.apache.commons.math3.optim.MaxEval;
import org.apache.commons.math3.optim.PointValuePair;
import org.apache.commons.math3.optim.SimpleValueChecker;
import org.apache.commons.math3.optim.nonlinear.scalar.GoalType;
import org.apache.commons.math3.optim.nonlinear.scalar.ObjectiveFunction;
import org.apache.commons.math3.optim.nonlinear.scalar.ObjectiveFunctionGradient;
import org.apache.commons.math3.optim.nonlinear.scalar.gradient.NonLinearConjugateGradientOptimizer;

import com.sri.ai.expresso.api.Expression;

/**
 * Class to optimize {@link Expression}, more precisely to optimize unconstrained multivariate functions. 
 * Use the Nonlinear Conjugate Gradient Descent algorithm which is much faster than Melder-Nead algorithm.
 * Should be used when the Expression is differentiable.
 * NOTE: can find a local optimum but not necessarily a global optimum. It depends on the initial guess.
 * 
 * @author Sarah Perrin
 *
 */

public class OptimizationWithNonlinearConjugateGradientDescent implements Optimization {
	
	public Expression expressionToOptimize;
	public GoalType goalType;
	public InitialGuess initialGuess;
	public NonLinearConjugateGradientOptimizer gradientOptimizer;
	public MaxEval maxEval;
    
	/**
	 * Default constructor. The user only needs to provide the expression to optimize and his initial guess.
	 *
	 */
	public OptimizationWithNonlinearConjugateGradientDescent(Expression expression, GoalType goalType, double[] initialPoint) {
		
		this(expression, goalType, initialPoint, new NonLinearConjugateGradientOptimizer(NonLinearConjugateGradientOptimizer.Formula.POLAK_RIBIERE, 
				new SimpleValueChecker(1e-6, 1e-6)), new MaxEval(10000));
	}
	
	/**
	 * More advanced constructor to choose parameters such as the gradientOptimizer and the maximum number of evaluations.
	 *
	 */
	public OptimizationWithNonlinearConjugateGradientDescent(Expression expression, GoalType goalType, double[] initialPoint, 
			NonLinearConjugateGradientOptimizer gradientOptimizer, MaxEval maxEval) {
		
		this.expressionToOptimize = expression;
		this.goalType = goalType;
		this.initialGuess = new InitialGuess(initialPoint);
		this.gradientOptimizer = gradientOptimizer;
		this.maxEval = maxEval;
	}
    
    /**
	 * return the optimum value of a function as a double.
	 *
	 */
	@Override
	public double findOptimum() {

		PointValuePair optimum = optimize(gradientOptimizer);
		double result = optimum.getSecond();
		return result;
	}
	
	/**
	 * return the argopt of a function as a double[].
	 *
	 */
	@Override
	public double[] findArgopt() {

		PointValuePair optimum = optimize(gradientOptimizer);
		double[] result = optimum.getPoint();
		return result;
	}

	/**
	 * Main method. It computes the optimum of the function and returns a PointValuePair which contains the argopt and the optimum. 
	 * Both can be computed directly with findOptimum and findArgopt. 
	 *
	 */
	public PointValuePair optimize(NonLinearConjugateGradientOptimizer optimizer) {
		
		final FunctionToOptimize f = new FunctionToOptimize(this.expressionToOptimize);
		final FunctionToOptimizeGradient gradient = new FunctionToOptimizeGradient(expressionToOptimize);
		
		ObjectiveFunction objectiveFunction = new ObjectiveFunction(f);
		MultivariateVectorFunction gradientMultivariateFunction = gradient;
		ObjectiveFunctionGradient objectiveFunctionGradient = new ObjectiveFunctionGradient(gradientMultivariateFunction);
		
		
		final PointValuePair optimum =
				optimizer.optimize(this.maxEval, objectiveFunction, objectiveFunctionGradient, this.goalType, this.initialGuess); 

	    /*System.out.println(Arrays.toString(optimum.getPoint()) + " : "
	            + optimum.getSecond());*/
	        
	    return optimum;
		
	}
}