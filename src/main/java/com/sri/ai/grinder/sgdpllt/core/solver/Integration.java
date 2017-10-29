package com.sri.ai.grinder.sgdpllt.core.solver;

import static com.sri.ai.expresso.helper.Expressions.TRUE;

import com.sri.ai.grinder.sgdpllt.api.Context;
import com.sri.ai.grinder.sgdpllt.api.QuantifierEliminationProblem;
import com.sri.ai.grinder.sgdpllt.api.ExpressionStepSolver.Step;

public class Integration {

	private QuantifierEliminationProblem problem;
	private QuantifierEliminationProblem simplifiedProblem;
	private Context context;
	private Step integrationResult;
	
	public Integration(QuantifierEliminationProblem problem, QuantifierEliminationProblem simplifiedProblem, Context context, Step result) {
		super();
		this.problem = problem;
		this.simplifiedProblem = simplifiedProblem;
		this.context = context;
		this.integrationResult = result;
	}

	public QuantifierEliminationProblem getProblem() {
		return problem;
	}

	public QuantifierEliminationProblem getSimplifiedProblem() {
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