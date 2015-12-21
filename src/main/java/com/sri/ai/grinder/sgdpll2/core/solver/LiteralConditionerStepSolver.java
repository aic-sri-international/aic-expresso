package com.sri.ai.grinder.sgdpll2.core.solver;

import static com.sri.ai.expresso.helper.Expressions.TRUE;
import static com.sri.ai.util.Util.collectToArrayList;
import static com.sri.ai.util.Util.myAssert;

import java.util.ArrayList;
import java.util.Iterator;

import com.google.common.annotations.Beta;
import com.google.common.base.Predicate;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.helper.SubExpressionsDepthFirstIterator;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.api.SimplifierUnderContextualConstraint;
import com.sri.ai.grinder.interpreter.SymbolicCommonInterpreterWithLiteralConditioning;
import com.sri.ai.grinder.sgdpll2.api.Constraint2;
import com.sri.ai.grinder.sgdpll2.api.ConstraintTheory;
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
	private ArrayList<Expression> literalsInExpression;
	public int initialLiteralToConsider;
	private SimplifierUnderContextualConstraint simplifierUnderContextualConstraint;
	
	public LiteralConditionerStepSolver(Expression expression, SimplifierUnderContextualConstraint simplifierUnderContextualConstraint) {
		super();
		this.expression = expression;
		this.simplifierUnderContextualConstraint = simplifierUnderContextualConstraint;
	}

	@Override
	public LiteralConditionerStepSolver clone() {
		LiteralConditionerStepSolver result = null;
		try {
			result = (LiteralConditionerStepSolver) super.clone();
		} catch (CloneNotSupportedException e) {
			e.printStackTrace();
		}
		return result;
	}
	
	public Expression getExpression() {
		return expression;
	}

	/**
	 * Returns a cached list of literals in expression,
	 * computed only the first time this is invoked (unless this is a clone of another step solver for which it has already been computed). 
	 * @param constraintTheory
	 * @param process
	 * @return
	 */
	protected ArrayList<Expression> getLiteralsInExpression(ConstraintTheory constraintTheory, RewritingProcess process) {
		if (literalsInExpression == null) {
			Iterator<Expression> subExpressionsIterator = new SubExpressionsDepthFirstIterator(expression);
			Predicate<Expression> literalsOnly = e -> constraintTheory.isLiteral(e, process);
			literalsInExpression = collectToArrayList(subExpressionsIterator, literalsOnly);
			initialLiteralToConsider = 0;
		}
		return literalsInExpression;
	}
	
	@Override
	public SolutionStep step(Constraint2 contextualConstraint, RewritingProcess process) {

		SolutionStep result;
		
		SolutionStep stepAllLiteralsAreDefined = getSolutionStepForWhetherAllLiteralsAreDefined(contextualConstraint, process);
//		SolutionStep stepAllLiteralsAreDefined = getSolutionStepForWhetherAllLiteralsAreDefined(expression, contextualConstraint, process); // this versions checks all literals

		if (stepAllLiteralsAreDefined == null) {
			result = null;
		}
		else if (stepAllLiteralsAreDefined.itDepends()){
			result = stepAllLiteralsAreDefined;
		}
		else { // must be Solution(TRUE)
			Expression literalFreeExpression = simplifyGivenContextualConstraint(expression, contextualConstraint, process);
			result = new Solution(literalFreeExpression);
		}

		return result;
	}


	/**
	 * Returns a solution step towards defining all literals in expression,
	 * or null if contextual constraint is found inconsistent,
	 * or new Solution(TRUE) if all literals are defined.
	 * @param contextualConstraint a contextual constraint
	 * @param process a rewriting process
	 * @return a solution step towards defining all literals in expression,
	 * or null if contextual constraint is found inconsistent,
	 * or new Solution(TRUE) if all literals are defined
	 */
	private SolutionStep getSolutionStepForWhetherAllLiteralsAreDefined(Constraint2 contextualConstraint, RewritingProcess process) {

		ArrayList<Expression> literalsInExpression = getLiteralsInExpression(contextualConstraint.getConstraintTheory(), process);
		
		for (int i = initialLiteralToConsider; i != literalsInExpression.size(); i++) {
			
			Expression literal = literalsInExpression.get(i);
			
			myAssert(
					() -> literal.getSyntacticFormType().equals("Function application") || literal.getSyntacticFormType().equals("Symbol"),
					() -> this.getClass() + ": applies to function applications or symbols only, but got " + literal );
			
			ConstraintSplitting split = new ConstraintSplitting(contextualConstraint, literal, process);
			if (split.getResult() == ConstraintSplitting.Result.CONSTRAINT_IS_CONTRADICTORY) {
				return null;
			}
			else if (split.getResult() == ConstraintSplitting.Result.LITERAL_IS_UNDEFINED) {
				LiteralConditionerStepSolver stepSolverFromNowOn = clone();
				stepSolverFromNowOn.initialLiteralToConsider++;
				return new ItDependsOn(literal, split, stepSolverFromNowOn, stepSolverFromNowOn);
			}
		}
		
		return new Solution(TRUE);
	}

	/**
	 * Returns a solution step towards defining all literals in expression,
	 * or null if contextual constraint is found inconsistent,
	 * or new Solution(TRUE) if all literals are defined.
	 * @param expression an expression
	 * @param contextualConstraint a contextual constraint
	 * @param process a rewriting process
	 * @return a solution step towards defining all literals in expression,
	 * or null if contextual constraint is found inconsistent,
	 * or new Solution(TRUE) if all literals are defined
	 */
	@SuppressWarnings("unused")
	private SolutionStep getSolutionStepForWhetherAllLiteralsAreDefined(Expression expression, Constraint2 contextualConstraint, RewritingProcess process) {
		if (contextualConstraint.getConstraintTheory().isLiteral(expression, process)) {
			ConstraintSplitting split = new ConstraintSplitting(contextualConstraint, expression, process);
			if (split.getResult() == ConstraintSplitting.Result.CONSTRAINT_IS_CONTRADICTORY) {
				return null;
			}
			else if (split.getResult() == ConstraintSplitting.Result.LITERAL_IS_UNDEFINED) {
				return new ItDependsOn(expression, split, clone(), clone());
			}
		}

		myAssert(
				() -> expression.getSyntacticFormType().equals("Function application") || expression.getSyntacticFormType().equals("Symbol"),
				() -> this.getClass() + ": applies to function applications or symbols only, but got " + expression );

		// the search below used to be done with Util.getFirstNonNullResultOrNull, but once we started to take null solution steps into account, it was impossible to use that and differentiate between null solution steps, or null for not satisfying the predicate
		for (Expression subExpression : expression.getSubExpressions()) {
			SolutionStep subSolutionStep =
					getSolutionStepForWhetherAllLiteralsAreDefined(subExpression, contextualConstraint, process);
			if (subSolutionStep == null || subSolutionStep.itDepends()) {
				return subSolutionStep;
			}
		}
		
		return new Solution(TRUE);
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