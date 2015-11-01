package com.sri.ai.grinder.sgdpll2.core.solver;

import static com.sri.ai.util.Util.getFirstNonNullResultOrNull;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.api.SimplifierUnderContextualConstraint;
import com.sri.ai.grinder.interpreter.SymbolicCommonInterpreterWithLiteralConditioning;
import com.sri.ai.grinder.sgdpll2.api.Constraint2;
import com.sri.ai.grinder.sgdpll2.api.ContextDependentProblemStepSolver;
import com.sri.ai.grinder.sgdpll2.core.constraint.ConstraintSplitting;

/**
 * An implementation for step solvers based on conditioning on the literals of a given expression.
 * <p>
 * This step solver either returns a step solution containing an literal in the expression
 * as of yet not defined by the contextual constraint
 * or, if that all literals are defined by the contextual constraint,
 * it simplifies it (thus generating a literal-free expression equivalent to the original
 * one given the contextual constraint) and returns a step solution containing it.
 * <p> 
 * The treatment of the literal-free expression can be extended by
 * overriding {@link #stepGivenLiteralFreeExpression(Expression, Constraint2, RewritingProcess)},
 * which is invoked on the literal-free expression, contextual constraint (including the literals conditioning)
 * and the rewriting process.
 * Its default (as stated above) is simply returning the given literal-free expression (the leaf).
 * <p>
 * In order to simplify the original expression into one without literals given a contextual constraint,
 * this class requires that a {@link SimplifierUnderContextualConstraint} be provided
 * that knows how to simplify the functions appearing the expression.
 * <p>
 * Currently supports only expressions that are composed of function applications or symbols only.
 * 
 * @author braz
 *
 */
@Beta
public class LiteralConditionerStepSolver implements ContextDependentProblemStepSolver {

	private Expression expression;
	private SimplifierUnderContextualConstraint simplifierUnderContextualConstraint;
	
	public LiteralConditionerStepSolver(Expression expression, SimplifierUnderContextualConstraint simplifierUnderContextualConstraint) {
		super();
		this.expression = expression;
		this.simplifierUnderContextualConstraint = simplifierUnderContextualConstraint;
	}

	/**
	 * Method defining what to do once we obtain a literal-free expression,
	 * with default implementation simply returning a solution containing
	 * the literal-free expression.
	 */
	protected SolutionStep stepGivenLiteralFreeExpression(
			Expression literalFreeExpression,
			Constraint2 contextualConstraint,
			RewritingProcess process) {
		
		return new Solution(literalFreeExpression);
	}

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
	 * Simplifies a given expression with a {@link SymbolicCommonInterpreterWithLiteralConditioning} using enumeration under given contextual constraint.
	 * @param expression
	 * @param contextualConstraint
	 * @param process
	 * @return
	 */
	private Expression simplifyGivenContextualConstraint(
			Expression expression, Constraint2 contextualConstraint, RewritingProcess process) {
		Expression result = simplifierUnderContextualConstraint.simplifyUnderContextualConstraint(expression, contextualConstraint, process);
		return result;
	}
}