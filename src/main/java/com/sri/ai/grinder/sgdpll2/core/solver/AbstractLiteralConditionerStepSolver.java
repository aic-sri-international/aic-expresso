package com.sri.ai.grinder.sgdpll2.core.solver;

import static com.sri.ai.util.Util.getFirstNonNullResultOrNull;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.interpreter.AbstractInterpreter;
import com.sri.ai.grinder.interpreter.BruteForceCommonInterpreter;
import com.sri.ai.grinder.sgdpll2.api.Constraint2;
import com.sri.ai.grinder.sgdpll2.api.ContextDependentProblemStepSolver;
import com.sri.ai.grinder.sgdpll2.core.constraint.ConstraintSplitting;

/**
 * An abstract implementation for step solvers based on conditioning on the literals of a given expression.
 * <p>
 * Currently supports only expressions that are composed of function applications or symbols only.
 * 
 * @author braz
 *
 */
@Beta
public abstract class AbstractLiteralConditionerStepSolver implements ContextDependentProblemStepSolver {

	private Expression expression;
	
	public AbstractLiteralConditionerStepSolver(Expression expression) {
		super();
		this.expression = expression;
	}

	/**
	 * Abstract method defining what to do once we obtain a literal-free expression.
	 */
	protected abstract SolutionStep stepGivenLiteralFreeExpression(
			Expression literalFreeExpression,
			Constraint2 contextualConstraint,
			RewritingProcess process);

	public Expression getExpression() {
		return expression;
	}

	@Override
	public SolutionStep step(Constraint2 contextualConstraint, RewritingProcess process) {
		SolutionStep result;

			Expression literalInExpression = getNonDefinedLiteral(expression, contextualConstraint, process);

			if (literalInExpression != null) {
				result = new ItDependsOn(literalInExpression);
			}
			else {
				Expression literalFreeExpression = simplifyGivenContextualConstraint(expression, contextualConstraint, process);
				result = stepGivenLiteralFreeExpression(literalFreeExpression, contextualConstraint, process);
			}

		return result;
	}

	/**
	 * Returns a literal in an expression not implied by contextual constraint,
	 * or <code>null</code> if there is none.
	 * @param expression an expression
	 * @param contextualConstraint a contextual constraint
	 * @param process a rewriting process
	 * @return a literal in the expression, which value is not implied by contextual constraint,
	 * or <code>null</code> if there is none.
	 */
	private Expression getNonDefinedLiteral(Expression expression, Constraint2 contextualConstraint, RewritingProcess process) {
		Expression result;
		if (isNonDefinedLiteral(expression, contextualConstraint, process)) {
			result = expression;
		}
		else {
			result = getFirstNonNullResultOrNull(
					expression.getSubExpressions(),
					s -> getNonDefinedLiteral(s, contextualConstraint, process));
		}
		return result;
	}

	private boolean isNonDefinedLiteral(Expression expression, Constraint2 contextualConstraint, RewritingProcess process) {
		if (contextualConstraint.getConstraintTheory().isLiteral(expression, process)) {
			ConstraintSplitting split = new ConstraintSplitting(contextualConstraint, expression, process);
			boolean undefined = split.getResult() == ConstraintSplitting.Result.LITERAL_IS_UNDEFINED;
			return undefined;
		}
		else {
			return false;
		}
	}

	/**
	 * Simplifies a given expression with a {@link BruteForceCommonInterpreter} using enumeration under given contextual constraint.
	 * @param expression
	 * @param contextualConstraint
	 * @param process
	 * @return
	 */
	public static Expression simplifyGivenContextualConstraint(
			Expression expression, Constraint2 contextualConstraint, RewritingProcess process) {
		// TODO: make the simplifier be a parameter for {@link AbstractLiteralConditionerStepSolver}
		AbstractInterpreter interpreter = new BruteForceCommonInterpreter(true /* simplify given contextual constraint */);
		Expression result = interpreter.simplifyUnderContextualConstraint(expression, contextualConstraint, process);
		return result;
	}
}