package com.sri.ai.expresso.minimization;


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
 * Class to minimize {@link Expression}, more precisely to minimize unconstrained multivariate functions. 
 * Use the Nonlinear Conjugate Gradient Descent algorithm which is much faster than Melder-Nead algorithm.
 * Should be used when the Expression is differentiable.
 * NOTE: can find a local minimum but not necessarily a global minimum. It depends on the initial guess.
 * @author Sarah Perrin
 *
 */

public class MinimizationWithNonlinearConjugateGradientDescent implements Minimization {
	
	public Expression expressionToMinimize;
	public InitialGuess initialGuess;
	public NonLinearConjugateGradientOptimizer gradientOptimizer;
	public MaxEval maxEval;
    
	/**
	 * Default constructor. The user only needs to provide the expression to minimize and his initial guess.
	 *
	 */
	public MinimizationWithNonlinearConjugateGradientDescent(Expression expression, double[] initialPoint) {
		
		this(expression, initialPoint, new NonLinearConjugateGradientOptimizer(NonLinearConjugateGradientOptimizer.Formula.POLAK_RIBIERE, 
				new SimpleValueChecker(1e-13, 1e-13)), new MaxEval(1000));
	}
	
	/**
	 * More advanced constructor to choose parameters such as the gradientOptimizer and the maximum number of evaluations.
	 *
	 */
	public MinimizationWithNonlinearConjugateGradientDescent(Expression expression, double[] initialPoint, 
			NonLinearConjugateGradientOptimizer gradientOptimizer, MaxEval maxEval) {
		
		this.expressionToMinimize = expression;
		this.initialGuess = new InitialGuess(initialPoint);
		this.gradientOptimizer = gradientOptimizer;
		this.maxEval = maxEval;
	}
    
    /**
	 * return the minimum value of a function as a double.
	 *
	 */
	public double findMinimum() {

		PointValuePair minimum = minimize(gradientOptimizer);
		double result = minimum.getSecond();
		return result;
	}
	
	/**
	 * return the argmin of a function as a double[].
	 *
	 */
	public double[] findArgmin() {

		PointValuePair minimum = minimize(gradientOptimizer);
		double[] result = minimum.getPoint();
		return result;
	}

	/**
	 * Main method. It computes the minimum of the function and returns a PointValuePair which contains the argmin and the min. 
	 * Both can be computed directly with findMinimum and findArgmin. 
	 *
	 */
	public PointValuePair minimize(NonLinearConjugateGradientOptimizer optimizer) {
		
		final FunctionToMinimize f = new FunctionToMinimize(this.expressionToMinimize);
		final FunctionToMinimizeGradient gradient = new FunctionToMinimizeGradient(expressionToMinimize);
		
		ObjectiveFunction objectiveFunction = new ObjectiveFunction(f);
		MultivariateVectorFunction gradientMultivariateFunction = gradient;
		ObjectiveFunctionGradient objectiveFunctionGradient = new ObjectiveFunctionGradient(gradientMultivariateFunction);
		
		final PointValuePair optimum =
				optimizer.optimize(this.maxEval, objectiveFunction, objectiveFunctionGradient, GoalType.MINIMIZE, this.initialGuess); 

	    /*System.out.println(Arrays.toString(optimum.getPoint()) + " : "
	            + optimum.getSecond());*/
	        
	    return optimum;
		
	}
}