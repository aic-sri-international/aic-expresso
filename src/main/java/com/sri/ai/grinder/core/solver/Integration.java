package com.sri.ai.grinder.core.solver;

import static com.sri.ai.expresso.helper.Expressions.TRUE;

import com.sri.ai.grinder.api.Context;
import com.sri.ai.grinder.api.SingleQuantifierEliminationProblem;
import com.sri.ai.grinder.api.ExpressionStepSolver.Step;

public class Integration {

	private SingleQuantifierEliminationProblem problem;
	private SingleQuantifierEliminationProblem simplifiedProblem;
	private Context context;
	private Step integrationResult;
	
	public Integration(SingleQuantifierEliminationProblem problem, SingleQuantifierEliminationProblem simplifiedProblem, Context context, Step result) {
		super();
		this.problem = problem;
		this.simplifiedProblem = simplifiedProblem;
		this.context = context;
		this.integrationResult = result;
	}

	public SingleQuantifierEliminationProblem getProblem() {
		return problem;
	}

	public SingleQuantifierEliminationProblem getSimplifiedProblem() {
		return simplifiedProblem;
	}

	public Context getContext() {
		return context;
	}

	public Step getResult() {
		return integrationResult;
	}
	
	@Override
	public String toString() {
		String result = 
				problem.toExpression()
				+ (!context.equals(TRUE)?
						(" under context " + context + " simplified to " + simplifiedProblem.toExpression())
						: "")
				+ " ---> " + integrationResult;
		
		return result;
	}
}