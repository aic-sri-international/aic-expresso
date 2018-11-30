package com.sri.ai.expresso.optimization;

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
 * Class to optimize {@link Expression}, more precisely to optimize unconstrained multivariate functions. 
 * Use the Nelder-Mead algorithm which only necessitates the function to be continuous. Thus it can be used if the function is not differentiable.
 * NOTE: can find a local optimum but not necessarily a global optimum. It depends on the initial guess.
 * @author Sarah Perrin
 *
 */

public class OptimizationWithNelderMead implements Optimization {
	
	public Expression expressionToOptimize;
	public InitialGuess initialGuess;
	public SimplexOptimizer simplexOptimizer;
	public MaxEval maxEval;
	public GoalType goalType;
	
	/**
	 * Default constructor which will compute the optimization of the expression from the initial point chosen.
	 *
	 */
	public OptimizationWithNelderMead(Expression expression, GoalType goalType, double[] initialPoint) {
		this(expression, goalType, initialPoint, new SimplexOptimizer(1e-10, 1e-30), new MaxEval(10000));
	}
	
	/**
	 * More advanced constructor to choose parameters such as the simplexOptimizer and the maximum number of evaluations.
	 *
	 */
	public OptimizationWithNelderMead(Expression expression, GoalType goalType, double[] initialPoint, SimplexOptimizer simplexOptimizer, MaxEval maxEval) {
		this.expressionToOptimize = expression;
		this.goalType = goalType;
		this.initialGuess = new InitialGuess(initialPoint);
		this.simplexOptimizer = simplexOptimizer;
		this.maxEval = maxEval;
	}

	/**
	 * return the optimum value of a function as a double.
	 *
	 */
	public double findOptimum() {

		PointValuePair optimum = optimize(simplexOptimizer);
		double result = optimum.getSecond();
		return result;
	}
	
	/**
	 * return the argopt of a function as a double[].
	 *
	 */
	public double[] findArgopt() {

		PointValuePair optimum = optimize(simplexOptimizer);
		double[] result = optimum.getPoint();
		return result;
	}
	
	public int numberOfVariablesInExpression() {
		
		Theory theory = new CommonTheory();
		Context context = new TrueContext(theory);

		Set<Expression> variablesInExpression = Expressions.freeVariables(expressionToOptimize, context);
		return variablesInExpression.size();
	}

	/**
	 * Main method. It computes the optimum of the function and returns a PointValuePair which contains the argopt and the optimum. 
	 * Both can be computed directly with findOptimum and findArgopt. 
	 *
	 */
	private PointValuePair optimize(SimplexOptimizer optimizer) {
		
		final FunctionToOptimize f = new FunctionToOptimize(this.expressionToOptimize);
		
		double[] nelderMeadSimplexParameters = new double[numberOfVariablesInExpression()];
		for (int i = 0; i < numberOfVariablesInExpression(); i++) {
			nelderMeadSimplexParameters[i] = 0.2;
		}
		
		final PointValuePair optimum = optimizer.optimize(this.maxEval, new ObjectiveFunction(f),
				this.goalType, this.initialGuess, new NelderMeadSimplex(nelderMeadSimplexParameters));

		//System.out.println(Arrays.toString(optimum.getPoint()) + " : " + optimum.getSecond());
		
		return optimum;
	}

}