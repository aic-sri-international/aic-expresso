package com.sri.ai.grinder.sgdpll2.core.solver;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.sgdpll2.api.Constraint;
import com.sri.ai.grinder.sgdpll2.api.ContextDependentProblemStepSolver;

/**
 * An abstract implementation for step solvers for quantified expressions.
 * 
 * @author braz
 *
 */
@Beta
public abstract class AbstractQuantifierStepSolver implements ContextDependentProblemStepSolver {

	@Override
	public SolutionStep step(Constraint contextualConstraint, RewritingProcess process) {
		SolutionStep result;

		Expression literalInQuantifiedExpression = getUndefinedLiteralFromQuantifiedExpression(contextualConstraint, process);
		if (literalInQuantifiedExpression != null) {
			result = new ItDependsOn(literalInQuantifiedExpression);
		}
		else {
			result = stepGivenLiteralFreeQuantifiedExpression(contextualConstraint, process);
		}

		return result;
	}

	/**
	 * Returns a literal in the quantified expression, which value is not defined by contextual constraint,
	 * or <code>null</code> if there is none.
	 * @param contextualConstraint a contextual constraint
	 * @param process a rewriting process
	 * @return a literal in the quantified expression, which value is not defined by contextual constraint,
	 * or <code>null</code> if there is none.
	 */
	protected abstract Expression getUndefinedLiteralFromQuantifiedExpression(Constraint contextualConstraint, RewritingProcess process);

	/**
	 */
	protected abstract SolutionStep stepGivenLiteralFreeQuantifiedExpression(Constraint contextualConstraint, RewritingProcess process);
}