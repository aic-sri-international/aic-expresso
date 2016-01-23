package com.sri.ai.grinder.sgdpll2.core.solver;

import static com.sri.ai.expresso.helper.Expressions.isSubExpressionOf;
import static com.sri.ai.grinder.library.controlflow.IfThenElse.condition;
import static com.sri.ai.grinder.library.controlflow.IfThenElse.elseBranch;
import static com.sri.ai.grinder.library.controlflow.IfThenElse.isIfThenElse;
import static com.sri.ai.grinder.library.controlflow.IfThenElse.thenBranch;

import com.google.common.annotations.Beta;
import com.google.common.base.Predicate;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.api.SimplifierUnderContextualConstraint;
import com.sri.ai.grinder.library.controlflow.IfThenElse;
import com.sri.ai.grinder.plaindpll.group.AssociativeCommutativeGroup;
import com.sri.ai.grinder.sgdpll2.api.Constraint2;
import com.sri.ai.grinder.sgdpll2.api.ConstraintTheory;
import com.sri.ai.grinder.sgdpll2.api.ContextDependentExpressionProblemStepSolver;
import com.sri.ai.grinder.sgdpll2.api.SingleVariableConstraint;
import com.sri.ai.grinder.sgdpll2.core.constraint.ConstraintSplitting;

/**
 * An abstract implementation for step solvers for quantified expressions
 * (the quantification being based on an associative commutative group's operation).
 * <p>
 * This is done by applying a {@link EvaluatorStepSolver} on the body expression,
 * picking literals in it according to the contextual constraint conjoined with the index constraint,
 * and "intercepting" literals containing the indices and splitting the quantifier
 * based on that, solving the two resulting sub-problems.
 * <p>
 * For example, if we have <code>sum({{ (on X in SomeType) if Y != bob then 2 else 3 | X != john }})</code>
 * under contextual constraint <code>Z = alice</code>,
 * {@link HorriblyInefficientEvaluatorStepSolver#step(Constraint2, RewritingProcess)} is
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
 * {@link HorriblyInefficientEvaluatorStepSolver} supports only expressions that are composed of
 * function applications or symbols only,
 * so this extension inherits this restriction if that is still in place.
 * 
 * @author braz
 *
 */
@Beta
public abstract class AbstractQuantifierEliminationStepSolver implements ContextDependentExpressionProblemStepSolver, Cloneable {

	/** 
	 * Indicates whether index-free literals are conditioned first.
	 * 
	 * This option has been introduced because conditioning on an index-free literal
	 * avoids the need to combine solutions, as it must be done with the literal does contain the index.
	 * Also, splitting on an index-free literal allows us to re-use the contextual constraint splitting
	 * (see code below for details on how this is done).
	 * Surprisingly, however, benchmarks turned slower when this is true.
	 * So, I am making this off by default,
	 * but leaving it in case there is a mistake somewhere, or future code
	 * makes this the best option.
	 * 
	 * TODO: It's been made static for now because making it a parameter requires
	 * adding a new parameter to a lot of nested methods all the way from the user code,
	 * which is too much for an option that is not seriously used at this point,
	 * but only for testing.
	 * Another option requiring less code change would be to set it as a
	 * global object in the rewriting process, but even that is some work
	 * because the rewriting process is not always readily available at user code level.
	 * If it is ever adopted for general use, it should be made either
	 * a parameter or a rewriting process global object.
	 */
	public static boolean conditionOnIndexFreeLiteralsFirst = false;
	
	public static boolean useEvaluatorStepSolverIfNotConditioningOnIndexFreeLiteralsFirst = true;
	
	private AssociativeCommutativeGroup group;
	
	private SingleVariableConstraint indexConstraint;

	private Expression body;
	
	private HorriblyInefficientEvaluatorStepSolver initialBodyEvaluationOnIndexFreeLiteralsStepSolver;
	
	private ContextDependentExpressionProblemStepSolver initialBodyEvaluationStepSolver;
	
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
	
	private HorriblyInefficientEvaluatorStepSolver getInitialBodyOnIndexFreeLiteralsStepSolver() {
		if (initialBodyEvaluationOnIndexFreeLiteralsStepSolver == null) {
			return new HorriblyInefficientEvaluatorStepSolver(
					body,
					e -> ! isSubExpressionOf(getIndex(), e), // only literals not involving the index
					simplifierUnderContextualConstraint);
		}
		else {
			return initialBodyEvaluationOnIndexFreeLiteralsStepSolver;
		}
	}

	private ContextDependentExpressionProblemStepSolver getInitialBodyStepSolver(ConstraintTheory constraintTheory) {
		if (initialBodyEvaluationStepSolver == null) {
			if (useEvaluatorStepSolver()) {
				initialBodyEvaluationStepSolver
				= new EvaluatorStepSolver(body, constraintTheory.getTopSimplifier());
			}
			else {
				Predicate<Expression> literalSelector = 
						conditionOnIndexFreeLiteralsFirst // if this is true, we only need to check the literals involving the index now
						? e -> isSubExpressionOf(getIndex(), e)
								: e -> true; // else we need to check all literals
				initialBodyEvaluationStepSolver = new HorriblyInefficientEvaluatorStepSolver(
						body,
						literalSelector, 
						simplifierUnderContextualConstraint);
			}
		}
		return initialBodyEvaluationStepSolver;
	}

	@Override
	public SolutionStep step(Constraint2 contextualConstraint, RewritingProcess process) {

		if (indexConstraint == null) {
			return new Solution(group.additiveIdentityElement());
		}
		
		SolutionStep result;

		if (conditionOnIndexFreeLiteralsFirst) {
			HorriblyInefficientEvaluatorStepSolver symbolicEvaluationOnLiteralsOnFreeVariablesOnlyStepSolver = getInitialBodyOnIndexFreeLiteralsStepSolver();
			SolutionStep symbolicEvaluationOnLiteralsOnFreeVariablesOnlyStep = symbolicEvaluationOnLiteralsOnFreeVariablesOnlyStepSolver.step(contextualConstraint, process);

			if (symbolicEvaluationOnLiteralsOnFreeVariablesOnlyStep.itDepends()) {
				AbstractQuantifierEliminationStepSolver subStepSolverForWhenLiteralIsTrue = clone();
				AbstractQuantifierEliminationStepSolver subStepSolverForWhenLiteralIsFalse = clone();
				subStepSolverForWhenLiteralIsTrue.initialBodyEvaluationOnIndexFreeLiteralsStepSolver = (HorriblyInefficientEvaluatorStepSolver) symbolicEvaluationOnLiteralsOnFreeVariablesOnlyStep.getStepSolverForWhenLiteralIsTrue();
				subStepSolverForWhenLiteralIsFalse.initialBodyEvaluationOnIndexFreeLiteralsStepSolver = (HorriblyInefficientEvaluatorStepSolver) symbolicEvaluationOnLiteralsOnFreeVariablesOnlyStep.getStepSolverForWhenLiteralIsFalse();
				result = new ItDependsOn(symbolicEvaluationOnLiteralsOnFreeVariablesOnlyStep.getLiteral(), symbolicEvaluationOnLiteralsOnFreeVariablesOnlyStep.getConstraintSplitting(), subStepSolverForWhenLiteralIsTrue, subStepSolverForWhenLiteralIsFalse);
			}
			else {
				result = null; // no more index-free literals to condition
			}
		}
		else {
			result = null; // not conditioning on index-free literals first 
		}

		if (result == null) { // not conditioning on index-free literals first, or no more index-free literals to condition
			Constraint2 contextualConstraintForBody = contextualConstraint.conjoin(getIndexConstraint(), process);
			if (contextualConstraintForBody == null) {
				result = new Solution(group.additiveIdentityElement()); // any solution is vacuously correct
			}
			else {
				ContextDependentExpressionProblemStepSolver bodyStepSolver = getInitialBodyStepSolver(contextualConstraint.getConstraintTheory());
				SolutionStep bodyStep = bodyStepSolver.step(contextualConstraintForBody, process);

				if (bodyStep.itDepends()) {
					// "intercept" literals containing the index and split the quantifier based on it
					if (isSubExpressionOf(getIndex(), bodyStep.getLiteral())) {
						Expression literalOnIndex = bodyStep.getLiteral();
						result = resultIfLiteralContainsIndex(contextualConstraint, literalOnIndex, process);
					}
					else { // not on index, just pass the expression on which we depend on, but with appropriate sub-step solvers (this, for now)
						AbstractQuantifierEliminationStepSolver subStepSolverForWhenLiteralIsTrue = clone();
						AbstractQuantifierEliminationStepSolver subStepSolverForWhenLiteralIsFalse = clone();
						subStepSolverForWhenLiteralIsTrue.initialBodyEvaluationStepSolver = bodyStep.getStepSolverForWhenLiteralIsTrue();
						subStepSolverForWhenLiteralIsFalse.initialBodyEvaluationStepSolver = bodyStep.getStepSolverForWhenLiteralIsFalse();
						result = new ItDependsOn(bodyStep.getLiteral(), null, subStepSolverForWhenLiteralIsTrue, subStepSolverForWhenLiteralIsFalse);
						// we cannot directly re-use bodyStep.getConstraintSplitting() because it was not obtained from the same contextual constraint,
						// but from the contextual constraint conjoined with the index constraint.
					}
				}
				else { // body is already literal free
					result
					= eliminateQuantifierForLiteralFreeBodyAndSingleVariableConstraint(
							contextualConstraint, indexConstraint, bodyStep.getValue(), process);
				}
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
		ContextDependentExpressionProblemStepSolver subProblem = makeWithNewIndexConstraint(newIndexConstraintAsSingleVariableConstraint);
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

	/**
	 * @param expression
	 * @param simplifierUnderContextualConstraint
	 * @param constraintTheory
	 * @return
	 */
	public static ContextDependentExpressionProblemStepSolver makeEvaluator(Expression expression, SimplifierUnderContextualConstraint simplifierUnderContextualConstraint, ConstraintTheory constraintTheory) {
		ContextDependentExpressionProblemStepSolver evaluator;
		if (useEvaluatorStepSolver()) {
			evaluator = new EvaluatorStepSolver(expression, constraintTheory.getTopSimplifier());
		} else {
			evaluator = new HorriblyInefficientEvaluatorStepSolver(expression, simplifierUnderContextualConstraint);
		}
		return evaluator;
	}

	public static boolean useEvaluatorStepSolver() {
		boolean result = useEvaluatorStepSolverIfNotConditioningOnIndexFreeLiteralsFirst && !conditionOnIndexFreeLiteralsFirst;
		return result;
	}
}