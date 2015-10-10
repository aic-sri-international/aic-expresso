package com.sri.ai.grinder.sgdpll2.core.solver;

import static com.sri.ai.expresso.helper.Expressions.isSubExpressionOf;
import static com.sri.ai.grinder.library.EnumerationCommonInterpreter.simplifyGivenContextualConstraint;
import static com.sri.ai.grinder.library.controlflow.IfThenElse.condition;
import static com.sri.ai.grinder.library.controlflow.IfThenElse.elseBranch;
import static com.sri.ai.grinder.library.controlflow.IfThenElse.isIfThenElse;
import static com.sri.ai.grinder.library.controlflow.IfThenElse.thenBranch;
import static com.sri.ai.util.Util.getFirstNonNullResultOrNull;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.library.controlflow.IfThenElse;
import com.sri.ai.grinder.plaindpll.group.AssociativeCommutativeGroup;
import com.sri.ai.grinder.sgdpll2.api.Constraint;
import com.sri.ai.grinder.sgdpll2.api.ContextDependentProblemStepSolver;
import com.sri.ai.grinder.sgdpll2.api.SingleVariableConstraint;
import com.sri.ai.grinder.sgdpll2.core.constraint.ConstraintSplitting;

/**
 * An abstract implementation for step solvers for quantified expressions.
 * 
 * @author braz
 *
 */
@Beta
public abstract class AbstractQuantifierStepSolver implements ContextDependentProblemStepSolver, Cloneable {

	private AssociativeCommutativeGroup group;
	
	private SingleVariableConstraint indexConstraint;
	
	private Expression body;
	
	public AbstractQuantifierStepSolver(AssociativeCommutativeGroup group, SingleVariableConstraint indexConstraint, Expression body) {
		super();
		this.group = group;
		this.indexConstraint = indexConstraint;
		this.body = body;
	}

	/**
	 * Abstract method defining a quantified expression with a given index constraint and literal-free body is to be solved.
	 * @param indexConstraint the index constraint
	 * @param literalFreeBody literal-free body
	 */
	protected abstract SolutionStep stepGivenLiteralFreeBody(
			Constraint contextualConstraint, SingleVariableConstraint indexConstraint, Expression literalFreeBody, RewritingProcess process);

	public AbstractQuantifierStepSolver clone() {
		AbstractQuantifierStepSolver result = null;
		try {
			result = (AbstractQuantifierStepSolver) super.clone();
		} catch (CloneNotSupportedException e) {
			e.printStackTrace();
		}
		return result;
	}
	
	/**
	 * Creates a new version of this object with a new index constraint.
	 * @param newIndexConstraint
	 * @return
	 */
	protected AbstractQuantifierStepSolver makeWithNewIndexConstraint(SingleVariableConstraint newIndexConstraint) {
		AbstractQuantifierStepSolver result = clone();
		result.group = group;
		result.body = body;
		result.indexConstraint = newIndexConstraint;
		return result;
	}

	public Expression getBody() {
		return body;
	}
	
	public SingleVariableConstraint getIndexConstraint() {
		return indexConstraint;
	}
	
	public Expression getIndex() {
		return indexConstraint.getVariable();
	}
	
	@Override
	public SolutionStep step(Constraint contextualConstraint, RewritingProcess process) {
		SolutionStep result;

		Constraint contextualConstraintForBody = contextualConstraint.conjoin(getIndexConstraint(), process);
		if (contextualConstraintForBody == null) {
			result = new Solution(group.additiveIdentityElement());
		}
		else {
			Expression literalInBody = getNonDefinedLiteral(getBody(), contextualConstraintForBody, process);

			if (literalInBody != null) {
				if (isSubExpressionOf(getIndex(), literalInBody)) {
					result = resultIfLiteralContainsIndex(contextualConstraint, literalInBody, process);
				}
				else {
					result = new ItDependsOn(literalInBody);
				}
			}
			else {
				Expression literalFreeBody = simplifyGivenContextualConstraint(body, contextualConstraintForBody, process);
				result = stepGivenLiteralFreeBody(contextualConstraint, indexConstraint, literalFreeBody, process);
			}
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
	private Expression getNonDefinedLiteral(Expression expression, Constraint contextualConstraint, RewritingProcess process) {
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

	private boolean isNonDefinedLiteral(Expression expression, Constraint contextualConstraint, RewritingProcess process) {
		if (contextualConstraint.getConstraintTheory().isLiteral(expression, process)) {
			ConstraintSplitting split = new ConstraintSplitting(contextualConstraint, expression, process);
			boolean undefined = split.getResult() == ConstraintSplitting.Result.LITERAL_IS_UNDEFINED;
			return undefined;
		}
		else {
			return false;
		}
	}

	private SolutionStep resultIfLiteralContainsIndex(Constraint contextualConstraint, Expression literal, RewritingProcess process) {
		
		// if the splitter contains the index, we must split the quantifier:
		// Quant_x:C Body  --->   (Quant_{x:C and L} Body) op (Quant_{x:C and not L} Body)
		
		SolutionStep result;
		ConstraintSplitting split = new ConstraintSplitting(getIndexConstraint(), literal, contextualConstraint, process);
		switch (split.getResult()) {
		case CONSTRAINT_IS_CONTRADICTORY:
			result = new Solution(group.additiveIdentityElement());
			break;
		case LITERAL_IS_UNDEFINED:
			Expression subSolution1 = solveSubProblem(split.getConstraintAndLiteral(),         contextualConstraint, process);
			Expression subSolution2 = solveSubProblem(split.getConstraintAndLiteralNegation(), contextualConstraint, process);
			Expression solutionValue = combine(subSolution1, subSolution2, contextualConstraint, process);
			result = new Solution(solutionValue);
			break;
		case LITERAL_IS_TRUE:
			solutionValue = solveSubProblem(split.getConstraintAndLiteral(), contextualConstraint, process);
			result = new Solution(solutionValue);
			break;
		case LITERAL_IS_FALSE:
			solutionValue = solveSubProblem(split.getConstraintAndLiteralNegation(), contextualConstraint, process);
			result = new Solution(solutionValue);
			break;
		default: throw new Error("Unrecognized result for " + ConstraintSplitting.class + ": " + split.getResult());
		}
		return result;
	}

	private Expression solveSubProblem(Constraint newIndexConstraint, Constraint contextualConstraint, RewritingProcess process) {
		SingleVariableConstraint newIndexConstraintAsSingleVariableConstraint = (SingleVariableConstraint) newIndexConstraint;
		ContextDependentProblemStepSolver subProblem = makeWithNewIndexConstraint(newIndexConstraintAsSingleVariableConstraint);
		Expression result = ContextDependentProblemSolver.solve(subProblem, contextualConstraint, process);
		return result;
	}

	protected Expression combine(Expression solution1, Expression solution2, Constraint contextualConstraint, RewritingProcess process) {
		Expression result;
		if (isIfThenElse(solution1)) {
			// (if C1 then A1 else A2) op solution2 ---> if C1 then (A1 op solution2) else (A2 op solution2)
			ConstraintSplitting split = new ConstraintSplitting(contextualConstraint, condition(solution1), process);
			switch (split.getResult()) {
			case CONSTRAINT_IS_CONTRADICTORY:
				result = null;
				break;
			case LITERAL_IS_UNDEFINED:
				Expression subSolution1 = combine(thenBranch(solution1), solution2, split.getConstraintAndLiteral(), process);
				Expression subSolution2 = combine(elseBranch(solution1), solution2, split.getConstraintAndLiteralNegation(), process);
				result = IfThenElse.make(condition(solution1), subSolution1, subSolution2, true);
				break;
			case LITERAL_IS_TRUE:
				result = combine(thenBranch(solution1), solution2, split.getConstraintAndLiteral(), process);
				break;
			case LITERAL_IS_FALSE:
				result = combine(elseBranch(solution1), solution2, split.getConstraintAndLiteral(), process);
				break;
			default: throw new Error("Unrecognized result for " + ConstraintSplitting.class + ": " + split.getResult());
			}
		}
		else if (isIfThenElse(solution2)) {
			// solution1 op (if C2 then B1 else B2) ---> if C2 then (solution1 op B2) else (solution1 op B2)
			ConstraintSplitting split = new ConstraintSplitting(contextualConstraint, condition(solution2), process);
			switch (split.getResult()) {
			case CONSTRAINT_IS_CONTRADICTORY:
				result = null;
				break;
			case LITERAL_IS_UNDEFINED:
				Expression subSolution1 = combine(solution1, thenBranch(solution2), split.getConstraintAndLiteral(), process);
				Expression subSolution2 = combine(solution1, elseBranch(solution2), split.getConstraintAndLiteralNegation(), process);
				result = IfThenElse.make(condition(solution2), subSolution1, subSolution2, true);
				break;
			case LITERAL_IS_TRUE:
				result = combine(solution1, thenBranch(solution2), split.getConstraintAndLiteral(), process);
				break;
			case LITERAL_IS_FALSE:
				result = combine(solution1, elseBranch(solution2), split.getConstraintAndLiteralNegation(), process);
				break;
			default: throw new Error("Unrecognized result for " + ConstraintSplitting.class + ": " + split.getResult());
			}
		}
		else {
			result = group.add(solution1, solution2, process);
		}
		return result;
	}
}