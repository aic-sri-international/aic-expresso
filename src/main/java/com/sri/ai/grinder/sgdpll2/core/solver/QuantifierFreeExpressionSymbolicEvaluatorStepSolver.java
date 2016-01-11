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
import com.sri.ai.grinder.sgdpll2.api.ContextDependentExpressionProblemStepSolver;
import com.sri.ai.grinder.sgdpll2.core.constraint.ConstraintSplitting;

/**
 * A step solver for symbolically evaluating a quantifier-free expression.
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
public class QuantifierFreeExpressionSymbolicEvaluatorStepSolver implements ContextDependentExpressionProblemStepSolver {

	private Expression expression;
	private Predicate<Expression> literalSelector;
	private ArrayList<Expression> selectedLiteralsInExpression;
	public int initialLiteralToConsider;
	private SimplifierUnderContextualConstraint simplifierUnderContextualConstraint;
	
	public QuantifierFreeExpressionSymbolicEvaluatorStepSolver(Expression expression, SimplifierUnderContextualConstraint simplifierUnderContextualConstraint) {
		this(expression, e -> true, simplifierUnderContextualConstraint);
	}

	public QuantifierFreeExpressionSymbolicEvaluatorStepSolver(Expression expression, Predicate<Expression> literalSelector, SimplifierUnderContextualConstraint simplifierUnderContextualConstraint) {
		super();
		this.expression = expression;
		this.literalSelector = literalSelector;
		this.simplifierUnderContextualConstraint = simplifierUnderContextualConstraint;
	}

	@Override
	public QuantifierFreeExpressionSymbolicEvaluatorStepSolver clone() {
		QuantifierFreeExpressionSymbolicEvaluatorStepSolver result = null;
		try {
			result = (QuantifierFreeExpressionSymbolicEvaluatorStepSolver) super.clone();
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
		if (selectedLiteralsInExpression == null) {
			Iterator<Expression> subExpressionsIterator = new SubExpressionsDepthFirstIterator(expression);
			Predicate<Expression> literalsOnly = e -> constraintTheory.isLiteral(e, process) && literalSelector.apply(e);
			selectedLiteralsInExpression = collectToArrayList(subExpressionsIterator, literalsOnly);
			initialLiteralToConsider = 0;
		}
		return selectedLiteralsInExpression;
	}
	
	@Override
	public SolutionStep step(Constraint2 contextualConstraint, RewritingProcess process) {

		SolutionStep result;
		
		SolutionStep stepAllLiteralsAreDefined = getSolutionStepForWhetherAllLiteralsAreDefined(contextualConstraint, process);

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
				QuantifierFreeExpressionSymbolicEvaluatorStepSolver stepSolverFromNowOn = clone();
				stepSolverFromNowOn.initialLiteralToConsider++;
				return new ItDependsOn(literal, split, stepSolverFromNowOn, stepSolverFromNowOn);
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