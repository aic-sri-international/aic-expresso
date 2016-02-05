package com.sri.ai.grinder.sgdpll2.core.solver;

import static com.sri.ai.expresso.helper.Expressions.FALSE;
import static com.sri.ai.expresso.helper.Expressions.TRUE;
import static com.sri.ai.grinder.sgdpll2.theory.base.ExpressionConditionedOnLiteralSolutionStep.stepDependingOnLiteral;
import static com.sri.ai.util.Util.map;

import java.util.Map;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.api.TopSimplifier;
import com.sri.ai.grinder.sgdpll2.api.Constraint2;
import com.sri.ai.grinder.sgdpll2.api.ContextDependentExpressionProblemStepSolver;
import com.sri.ai.util.base.IdentityWrapper;
import com.sri.ai.util.collect.StackedHashMap;

/**
 * A step solver for symbolically evaluating an expression.
 * <p>
 * Currently supports only expressions that are composed of function applications or symbols.
 *
 * This step solver implements the following pseudo-code:
 * <pre>
 * An evaluator step solver for an expression E
keeps a map Evaluators from identity wrappers
and the index SEIndex of the next sub-expression to be analyzed.
of the sub-expression of E to their own evaluator step solver.
Step solver assumes sub-expressions before SEIndex
fully evaluated and E top-simplified already with respect to them.

Its step is:

if E is literal
    split on it with sub step solvers true and false
else if SEIndex != E.numberOfArguments
    SE <- E.get(SEIndex)
    SEEvaluator <- Evaluators(SE), or make a new one
    SEStep <- SEEvaluator.step
    if SEStep depends on L with sub step solvers S1 and S2
        return ItDependsOn on literal SE
            with sub-step solvers for true and false literals,
            to evaluate E,
            containing a Evaluators(SE) modified to map
            to SEStep's sub-step solvers for SE being true and false,
            respectively.
    else
        value <- SEStep.getValue()

        if value == SE
            // no change to SE, so no change to E. Just move on to next sub-expression
            Evaluator' <- clone
            Evaluator'.Evaluators <- stacked map on Evaluators with new entry { SE -> SEEvaluator // so we remember the solution if a future simplification surfaces an already fully evaluated sub-expression, including SE, as E'
            Evaluator'.SEIndex++ // go to next sub-expression
            return Evaluator'.step
        else
            E' <- top-simplify(E[SE/value])
		    if E' == SE // E has been reduced to SE itself, which we just evaluated
		        return SEStep
            else
                Evaluator' <- Evaluators(E') // E' may be a previously evaluated sub-expression or not; re-use evaluator or make a new one 
                return Evaluator'.step
else        
    return Solution(E) // Remember: step solver assumes sub-expressions before SEIndex fully evaluated and E top-simplified already wrt them
</pre> 
 * @author braz
 *
 */
@Beta
public class EvaluatorStepSolver implements ContextDependentExpressionProblemStepSolver {

	private Expression expression;
	private int subExpressionIndex;
	private Map<IdentityWrapper, ContextDependentExpressionProblemStepSolver> evaluators;
	private TopSimplifier topSimplifier;
	
	public EvaluatorStepSolver(Expression expression, TopSimplifier topSimplifier) {
		super();
		this.expression = expression;
		this.subExpressionIndex = 0;
		this.evaluators = map();
		this.topSimplifier = topSimplifier;
	}

	@Override
	public EvaluatorStepSolver clone() {
		EvaluatorStepSolver result = null;
		try {
			result = (EvaluatorStepSolver) super.clone();
		} catch (CloneNotSupportedException e) {
			e.printStackTrace();
		}
		return result;
	}
	
	public Expression getExpression() {
		return expression;
	}

	@Override
	public SolutionStep step(Constraint2 contextualConstraint, RewritingProcess process) {
		SolutionStep result = null;
		
		if (expressionIsLiteral(contextualConstraint, process)) {
			result = stepDependingOnLiteral(expression, TRUE, FALSE, contextualConstraint, process);
		}
		else if (subExpressionIndex != expression.numberOfArguments()) {
			Expression subExpression = expression.get(subExpressionIndex);
			ContextDependentExpressionProblemStepSolver subExpressionEvaluator = getEvaluatorFor(subExpression);
			SolutionStep subExpressionStep = subExpressionEvaluator.step(contextualConstraint, process);

			if (subExpressionStep == null) {
				return null;
			}
			if (subExpressionStep.itDepends()) {
				EvaluatorStepSolver ifTrue = clone();
				ifTrue.setEvaluatorFor(subExpression, subExpressionStep.getStepSolverForWhenLiteralIsTrue());
				EvaluatorStepSolver ifFalse = clone();
				ifFalse.setEvaluatorFor(subExpression, subExpressionStep.getStepSolverForWhenLiteralIsFalse());
				result =
						new ItDependsOn(
								subExpressionStep.getLiteral(),
								subExpressionStep.getConstraintSplitting(),
								ifTrue,
								ifFalse);
			}
			else if (subExpressionStep.getValue() == subExpression) {
				// no change, just record the evaluator for this sub-expression and move on to the next one
				ContextDependentExpressionProblemStepSolver nextStepSolver;
				nextStepSolver = clone();
				((EvaluatorStepSolver) nextStepSolver).setEvaluatorFor(subExpression, subExpressionEvaluator);
				((EvaluatorStepSolver) nextStepSolver).subExpressionIndex++;
				result = nextStepSolver.step(contextualConstraint, process);
			}
			else {
				Expression expressionWithSubExpressionReplacedByItsValue
				= expression.set(subExpressionIndex, subExpressionStep.getValue());

				Expression topSimplified
				= topSimplifier.apply(expressionWithSubExpressionReplacedByItsValue, process);

				if (topSimplified == subExpression) {
					// topSimplified turns out to be the sub-expression itself
					// we already know the result for that: the non-dependent subExpressionStep itself.
					return subExpressionStep;
				}
				else {
					// topSimplified is either a former sub-expression, or new.
					// try to reuse evaluator if available, or a make a new one, and use it
					ContextDependentExpressionProblemStepSolver nextStepSolver
					= getEvaluatorFor(topSimplified);
					result = nextStepSolver.step(contextualConstraint, process);
				}
			}
		}
		else {
			// all sub-expressions are evaluated and expression is top-simplified
			result = new Solution(expression);
		}
		
		return result;
	}

	private ContextDependentExpressionProblemStepSolver getEvaluatorFor(Expression expression) {
		ContextDependentExpressionProblemStepSolver result	= evaluators.get(expression);
		if (result == null) {
			result = new EvaluatorStepSolver(expression, topSimplifier);
		}
		return result;
	}

	private void setEvaluatorFor(Expression expression, ContextDependentExpressionProblemStepSolver stepSolver) {
		// make new map based on old one, without altering it
		evaluators = new StackedHashMap<IdentityWrapper, ContextDependentExpressionProblemStepSolver>(evaluators);
		evaluators.put(new IdentityWrapper(expression), stepSolver);
	}

	private boolean expressionIsLiteral(Constraint2 contextualConstraint, RewritingProcess process) {
		return contextualConstraint.getConstraintTheory().isLiteral(expression, process);
	}
	
	@Override
	public String toString() {
		return "Evaluate " + expression + " from " + subExpressionIndex + "-th sub-expression on";
	}
}