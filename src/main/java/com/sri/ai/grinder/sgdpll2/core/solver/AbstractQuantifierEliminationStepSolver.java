package com.sri.ai.grinder.sgdpll2.core.solver;

import static com.sri.ai.expresso.helper.Expressions.isSubExpressionOf;
import static com.sri.ai.grinder.library.controlflow.IfThenElse.condition;
import static com.sri.ai.grinder.library.controlflow.IfThenElse.elseBranch;
import static com.sri.ai.grinder.library.controlflow.IfThenElse.isIfThenElse;
import static com.sri.ai.grinder.library.controlflow.IfThenElse.thenBranch;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.api.SimplifierUnderContextualConstraint;
import com.sri.ai.grinder.library.controlflow.IfThenElse;
import com.sri.ai.grinder.plaindpll.group.AssociativeCommutativeGroup;
import com.sri.ai.grinder.sgdpll2.api.Constraint2;
import com.sri.ai.grinder.sgdpll2.api.ConstraintTheory;
import com.sri.ai.grinder.sgdpll2.api.ContextDependentProblemStepSolver;
import com.sri.ai.grinder.sgdpll2.api.SingleVariableConstraint;
import com.sri.ai.grinder.sgdpll2.core.constraint.ConstraintSplitting;

/**
 * An abstract implementation for step solvers for quantified expressions
 * (the quantification being based on an associative commutative group's operation).
 * <p>
 * This is done by applying a {@link LiteralConditionerStepSolver} on the body expression,
 * picking literals in it according to the contextual constraint conjoined with the index constraint,
 * and "intercepting" literals containing the indices and splitting the quantifier
 * based on that, solving the two resulting sub-problems.
 * <p>
 * For example, if we have <code>sum({{ (on X in SomeType) if Y != bob then 2 else 3 | X != john }})</code>
 * under contextual constraint <code>Z = alice</code>,
 * {@link LiteralConditionerStepSolver#step(Constraint2, RewritingProcess)} is
 * invoked with contextual constraint <code>Z = alice and X != john</code>.
 * The solution step will depend on literal <code>Y != bob</code>.
 * <p>
 * If however the quantified expression is
 * <code>sum({{ (on X in SomeType) if X != bob then 2 else 3 | X != john }})</code>,
 * the solution step will not be one depending on a literal, but a definite solution equivalent to
 * <code>sum({{ (on X in SomeType) 2 | X != john and X != bob}}) +
 *       sum({{ (on X in SomeType) 3 | X != john and X = bob}})</code>.
 * <p>
 * Because these two sub-problems have literal-free bodies <code>2</code> and <code>3</code>,
 * they will be solved by the extension's
 * {@link #eliminateQuantifierForLiteralFreeBodyAndSingleVariableConstraint(Constraint2, SingleVariableConstraint, Expression, RewritingProcess)}
 * (which for sums with constant bodies will be equal to the model count of the index constraint
 * under the contextual constraint times the constant).
 * <p>
 * Extending classes must define method
 * {@link #eliminateQuantifierForLiteralFreeBodyAndSingleVariableConstraint(Constraint2, SingleVariableConstraint, Expression, RewritingProcess)
 * to solve the case in which the body is its given literal-free version,
 * for the given contextual constraint and index constraint.
 * <p>
 * At the time of this writing,
 * {@link LiteralConditionerStepSolver} supports only expressions that are composed of
 * function applications or symbols only,
 * so this extension inherits this restriction if that is still in place.
 * 
 * @author braz
 *
 */
@Beta
public abstract class AbstractQuantifierEliminationStepSolver implements ContextDependentProblemStepSolver, Cloneable {

	private AssociativeCommutativeGroup group;
	
	private SingleVariableConstraint indexConstraint;

	private Expression body;
	
	private LiteralConditionerStepSolver bodyStepSolver;
	
	private SimplifierUnderContextualConstraint simplifierUnderContextualConstraint;
	
	public AbstractQuantifierEliminationStepSolver(AssociativeCommutativeGroup group, SimplifierUnderContextualConstraint simplifierUnderContextualConstraint, SingleVariableConstraint indexConstraint, Expression body) {
		this.group = group;
		this.indexConstraint = indexConstraint;
		this.body = body;
		this.simplifierUnderContextualConstraint = simplifierUnderContextualConstraint;
	}

	/**
	 * Abstract method defining a quantified expression with a given index constraint and literal-free body is to be solved.
	 * @param indexConstraint the index constraint
	 * @param literalFreeBody literal-free body
	 */
	protected abstract SolutionStep eliminateQuantifierForLiteralFreeBodyAndSingleVariableConstraint(
			Constraint2 contextualConstraint,
			SingleVariableConstraint indexConstraint,
			Expression literalFreeBody,
			RewritingProcess process);

	public AbstractQuantifierEliminationStepSolver clone() {
		AbstractQuantifierEliminationStepSolver result = null;
		try {
			result = (AbstractQuantifierEliminationStepSolver) super.clone();
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
	protected AbstractQuantifierEliminationStepSolver makeWithNewIndexConstraint(SingleVariableConstraint newIndexConstraint) {
		AbstractQuantifierEliminationStepSolver result = clone();
		result.group = group;
		result.indexConstraint = newIndexConstraint;
		return result;
	}

	public AssociativeCommutativeGroup getGroup() {
		return group;
	}
	
	public SingleVariableConstraint getIndexConstraint() {
		return indexConstraint;
	}
	
	/**
	 * Convenience method for <code>indexConstraint.getConstraintTheory()</code>.
	 * @return
	 */
	public ConstraintTheory getConstraintTheory() {
		return indexConstraint.getConstraintTheory();
	}
	
	public Expression getIndex() {
		return indexConstraint.getVariable();
	}
	
	private LiteralConditionerStepSolver getBodyStepSolver() {
		if (bodyStepSolver == null) {
			return new LiteralConditionerStepSolver(body, simplifierUnderContextualConstraint);
		}
		else {
			return bodyStepSolver;
		}
	}

	@Override
	public SolutionStep step(Constraint2 contextualConstraint, RewritingProcess process) {
		SolutionStep result;

		Constraint2 contextualConstraintForBody = contextualConstraint.conjoin(getIndexConstraint(), process);
		if (contextualConstraintForBody == null) {
			result = new Solution(group.additiveIdentityElement());
		}
		else {
//			System.out.println("Selecting body literal");	
//			System.out.println("Contextual constraint: " + contextualConstraint);	
//			System.out.println("Index constraint: " + indexConstraint);	
//			System.out.println("Body: " + body);	
			LiteralConditionerStepSolver bodyStepSolver = getBodyStepSolver();
			SolutionStep bodyStep = bodyStepSolver.step(contextualConstraintForBody, process);

//			System.out.println("Body step solver: " + bodyStepSolver);	
//			System.out.println("Body step solver initial literal: " + bodyStepSolver.initialLiteralToConsider);	
//			System.out.println("Body step: " + bodyStep);	

			if (bodyStep.itDepends()) {
				// "intercept" literals containing the index and split the quantifier based on it
				if (isSubExpressionOf(getIndex(), bodyStep.getExpression())) {
//					System.out.println("Literal depends on index. Solving two subproblems and combining");	
					Expression literalOnIndex = bodyStep.getExpression();
					result = resultIfLiteralContainsIndex(contextualConstraint, literalOnIndex, process);
//					System.out.println("Combination of two sub-problems is " + result);	
				}
				else { // not on index, just pass the expression on which we depend on, but with appropriate sub-step solvers (this, for now)
//					System.out.println("Literal does not depend on index.");	
					AbstractQuantifierEliminationStepSolver cloneOfThisStepSolver = clone();
//					cloneOfThisStepSolver.bodyStepSolver = (LiteralConditionerStepSolver) bodyStep.getStepSolverForWhenExpressionIsTrue();
//					System.out.println("Made clone of AbstractQuantifierEliminationStepSolver");	
//					System.out.println("Clone's body step solver is " + cloneOfThisStepSolver.bodyStepSolver);	
//					System.out.println("Clone's body step solver initial literal is " + cloneOfThisStepSolver.bodyStepSolver.initialLiteralToConsider);	
//					System.out.println("This should be ahead of this body step solver initial literal " + bodyStepSolver.initialLiteralToConsider);	
					// for now, both true and false sides can use the same solver because they behave the same either way
					// later, we want to add a record of which literals were chosen true or false in the body step solver
					// so that it does not need to recompute that from the contextual constraint anymore;
					// at that point, we will need to create two sub-step solvers here, one for each case.
					result = new ItDependsOn(bodyStep.getExpression(), null, cloneOfThisStepSolver, cloneOfThisStepSolver);
					// we cannot directly re-use bodyStep.getConstraintSplitting() because it was not obtained from the same contextual constraint
				}
			}
			else { // body is already literal free
				result =
						eliminateQuantifierForLiteralFreeBodyAndSingleVariableConstraint(
								contextualConstraint, indexConstraint, bodyStep.getExpression(), process);
			}
		}

		return result;
	}

	private SolutionStep resultIfLiteralContainsIndex(Constraint2 contextualConstraint, Expression literal, RewritingProcess process) {
		
		// if the splitter contains the index, we must split the quantifier:
		// Quant_x:C Body  --->   (Quant_{x:C and L} Body) op (Quant_{x:C and not L} Body)
		
		SolutionStep result;
		ConstraintSplitting split = new ConstraintSplitting(getIndexConstraint(), literal, contextualConstraint, process);
		Expression solutionValue;
		switch (split.getResult()) {
		case CONSTRAINT_IS_CONTRADICTORY:
			solutionValue = null;
			break;
		case LITERAL_IS_UNDEFINED:
			Expression subSolution1 = solveSubProblem(split.getConstraintAndLiteral(),         contextualConstraint, process);
			Expression subSolution2 = solveSubProblem(split.getConstraintAndLiteralNegation(), contextualConstraint, process);
			solutionValue = combine(subSolution1, subSolution2, contextualConstraint, process);
			break;
		case LITERAL_IS_TRUE:
			solutionValue = solveSubProblem(split.getConstraintAndLiteral(), contextualConstraint, process);
			break;
		case LITERAL_IS_FALSE:
			solutionValue = solveSubProblem(split.getConstraintAndLiteralNegation(), contextualConstraint, process);
			break;
		default: throw new Error("Unrecognized result for " + ConstraintSplitting.class + ": " + split.getResult());
		}

		if (solutionValue == null) {
			result = null;
		}
		else {
			result = new Solution(solutionValue);
		}
		
		return result;
	}

	private Expression solveSubProblem(Constraint2 newIndexConstraint, Constraint2 contextualConstraint, RewritingProcess process) {
		SingleVariableConstraint newIndexConstraintAsSingleVariableConstraint = (SingleVariableConstraint) newIndexConstraint;
		ContextDependentProblemStepSolver subProblem = makeWithNewIndexConstraint(newIndexConstraintAsSingleVariableConstraint);
		Expression result = subProblem.solve(contextualConstraint, process);
		return result;
	}

	protected Expression combine(Expression solution1, Expression solution2, Constraint2 contextualConstraint, RewritingProcess process) {
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