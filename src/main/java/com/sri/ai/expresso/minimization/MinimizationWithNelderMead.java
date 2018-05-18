package com.sri.ai.expresso.minimization;

import java.util.Set;
import org.apache.commons.math3.optim.InitialGuess;
import org.apache.commons.math3.optim.MaxEval;
import org.apache.commons.math3.optim.PointValuePair;
import org.apache.commons.math3.optim.nonlinear.scalar.GoalType;
import org.apache.commons.math3.optim.nonlinear.scalar.ObjectiveFunction;
import org.apache.commons.math3.optim.nonlinear.scalar.noderiv.NelderMeadSimplex;
import org.apache.commons.math3.optim.nonlinear.scalar.noderiv.SimplexOptimizer;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.grinder.api.Context;
import com.sri.ai.grinder.api.Theory;
import com.sri.ai.grinder.application.CommonTheory;
import com.sri.ai.grinder.core.TrueContext;

/**
 * Class to minimize {@link Expression}, more precisely to minimize unconstrained multivariate functions. 
 * Use the Nelder-Mead algorithm which only necessitates the function to be continuous. Thus it can be used if the function is not differentiable.
 * NOTE: can find a local minimum but not necessarily a global minimum. It depends on the initial guess.
 * @author Sarah Perrin
 *
 */

public class MinimizationWithNelderMead implements Minimization {
	
	public Expression expressionToMinimize;
	public InitialGuess initialGuess;
	public SimplexOptimizer simplexOptimizer;
	public MaxEval maxEval;
	
	/**
	 * Default constructor which will compute the minimization of the expression from the initial point chosen.
	 *
	 */
	public MinimizationWithNelderMead(Expression expression, double[] initialPoint) {
		this(expression, initialPoint, new SimplexOptimizer(1e-10, 1e-30), new MaxEval(100000));
	}
	
	/**
	 * More advanced constructor to choose parameters such as the simplexOptimizer and the maximum number of evaluations.
	 *
	 */
	public MinimizationWithNelderMead(Expression expression, double[] initialPoint, SimplexOptimizer simplexOptimizer, MaxEval maxEval) {
		this.expressionToMinimize = expression;
		this.initialGuess = new InitialGuess(initialPoint);
		this.simplexOptimizer = simplexOptimizer;
		this.maxEval = maxEval;
	}

	/**
	 * return the minimum value of a function as a double.
	 *
	 */
	public double findMinimum() {

		PointValuePair minimum = minimize(simplexOptimizer);
		double result = minimum.getSecond();
		return result;
	}
	
	/**
	 * return the argmin of a function as a double[].
	 *
	 */
	public double[] findArgmin() {

		

		PointValuePair minimum = minimize(simplexOptimizer);
		double[] result = minimum.getPoint();
		return result;
	}
	
	public int numberOfVariablesInExpression() {
		
		Theory theory = new CommonTheory();
		Context context = new TrueContext(theory);

		Set<Expression> variablesInExpression = Expressions.freeVariables(expressionToMinimize, context);
		return variablesInExpression.size();
	}

	/**
	 * Main method. It computes the minimum of the function and returns a PointValuePair which contains the argmin and the min. 
	 * Both can be computed directly with findMinimum and findArgmin. 
	 *
	 */
	private PointValuePair minimize(SimplexOptimizer optimizer) {
		
		final FunctionToMinimize f = new FunctionToMinimize(this.expressionToMinimize);
		
		double[] nelderMeadSimplexParameters = new double[numberOfVariablesInExpression()];
		for (int i = 0; i < numberOfVariablesInExpression(); i++) {
			nelderMeadSimplexParameters[i] = 0.2;
		}
		
		final PointValuePair minimum = optimizer.optimize(this.maxEval, new ObjectiveFunction(f),
				GoalType.MINIMIZE, this.initialGuess, new NelderMeadSimplex(nelderMeadSimplexParameters));

		//System.out.println(Arrays.toString(minimum.getPoint()) + " : " + minimum.getSecond());
		
		return minimum;
	}

}