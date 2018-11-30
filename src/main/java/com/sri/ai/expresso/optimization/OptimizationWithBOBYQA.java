package com.sri.ai.expresso.optimization;

import java.util.Set;

import org.apache.commons.math3.optim.InitialGuess;
import org.apache.commons.math3.optim.MaxEval;
import org.apache.commons.math3.optim.PointValuePair;
import org.apache.commons.math3.optim.SimpleBounds;
import org.apache.commons.math3.optim.nonlinear.scalar.GoalType;
import org.apache.commons.math3.optim.nonlinear.scalar.ObjectiveFunction;
import org.apache.commons.math3.optim.nonlinear.scalar.noderiv.BOBYQAOptimizer;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.grinder.api.Context;
import com.sri.ai.grinder.api.Theory;
import com.sri.ai.grinder.application.CommonTheory;
import com.sri.ai.grinder.core.TrueContext;

/**
 * Class to optimize an {@link Expression}, more precisely to optimize bound constrained multivariate functions. 
 * Typically, the variables will have to be between 0 and 1 as they are probabilities when it is used in PRAiSE.
 * BOBYQA algorithm only necessitates the function to be continuous. Thus it can be used if the function is not differentiable.
 * The number of variables must be greater than 2.
 * NOTE: can find a local minimum but not necessarily a global minimum. It depends on the initial guess.
 * @author Sarah Perrin
 *
 */

public class OptimizationWithBOBYQA implements Optimization {
	public Expression expressionToOptimize;
	public GoalType goalType;
	public InitialGuess initialGuess;
	public BOBYQAOptimizer bobyqaOptimizer;
	public MaxEval maxEval;
	
	/**
	 * Default constructor which will compute the optimization of the expression from the initial point chosen. 
	 * BOBYQAOptimizer must be invoked with new BOBYQAOptimizer(int). int is the number of interpolation conditions. 
	 * For a problem of dimension n, its value must be in the interval [n+2, (n+1)(n+2)/2]. Choices that exceed 2n+1 are not recommended.
	 *
	 */
	public OptimizationWithBOBYQA(Expression expression, GoalType goalType, double[] initialPoint, BOBYQAOptimizer bobyqaOptimizer) {
		this(expression, goalType, initialPoint, bobyqaOptimizer, new MaxEval(100000));
	}
	
	/**
	 * More advanced constructor to choose parameters such as the simplexOptimizer and the maximum number of evaluations.
	 *
	 */
	public OptimizationWithBOBYQA(Expression expression, GoalType goalType, double[] initialPoint, BOBYQAOptimizer bobyqaOptimizer, MaxEval maxEval) {
		this.expressionToOptimize = expression;
		this.goalType = goalType;
		this.initialGuess = new InitialGuess(initialPoint);
		this.bobyqaOptimizer = bobyqaOptimizer;
		this.maxEval = maxEval;
	}

	/**
	 * return the optimum value of a function as a double.
	 *
	 */
	public double findOptimum() {

		PointValuePair optimum = optimize(bobyqaOptimizer);
		double result = optimum.getSecond();
		return result;
	}
	
	/**
	 * return the Argopt of a function as a double[].
	 *
	 */
	public double[] findArgopt() {
		
		PointValuePair optimum = optimize(bobyqaOptimizer);
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
	private PointValuePair optimize(BOBYQAOptimizer optimizer) {
		
		final FunctionToOptimize f = new FunctionToOptimize(this.expressionToOptimize);
		
		int dimension = numberOfVariablesInExpression();
		double[] ones = new double[dimension];
		for (int i = 0; i < dimension; i++) {
			ones[i] = 1;
		}
		
		final PointValuePair optimum = optimizer.optimize(this.maxEval, new ObjectiveFunction(f),
				this.goalType, new SimpleBounds(new double[dimension], ones), this.initialGuess);

		//System.out.println(Arrays.toString(minimum.getPoint()) + " : " + minimum.getSecond());
		
		return optimum;
	}

}
