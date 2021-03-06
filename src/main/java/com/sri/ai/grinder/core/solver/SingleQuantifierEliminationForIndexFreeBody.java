package com.sri.ai.grinder.core.solver;

import static com.sri.ai.expresso.helper.Expressions.isSubExpressionOf;
import static com.sri.ai.util.Util.requires;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.api.Context;
import com.sri.ai.grinder.api.SingleQuantifierEliminationProblem;
import com.sri.ai.grinder.api.SingleVariableConstraint;
import com.sri.ai.grinder.api.Theory;
import com.sri.ai.grinder.group.AssociativeCommutativeGroup;
import com.sri.ai.grinder.library.controlflow.IfThenElse;

/**
 * Quantifier elimination when the body does not contain the index by doing one of two things:
 * <ul>
 * <li> if the group is idempotent, simplify to <code>if condition for satisfiability of constraint then body else group's identity</code>.
 * <li> if the group is not idempotent, 
 * applies {@link AssociativeCommutativeGroup#addNTimes(Expression, Expression, Context)} to
 * the literal-free body and {@link SingleVariableConstraint#modelCount(Context)},
 * followed by {@link Theory#simplify(Expression, Context)}.
 * </ul>
 * 
 * @author braz
 *
 */
public class SingleQuantifierEliminationForIndexFreeBody extends SingleQuantifierEliminationProblemWrapper {

	private Context context;
	
	public SingleQuantifierEliminationForIndexFreeBody(SingleQuantifierEliminationProblem problem, Context context) {
		super(problem);
		this.context = context;
	}
	
	public SingleQuantifierEliminationForIndexFreeBody copyWithNewProblem(SingleQuantifierEliminationProblem problem) {
		return new SingleQuantifierEliminationForIndexFreeBody(problem, context);
	}

	public Expression eliminateQuantifierForLiteralFreeBody() {
		checkThatIndexDoesNotAppearInBody();
		Expression result = eliminateQuantifier();
		return result;
	}

	private Expression eliminateQuantifier() {
		Expression result;
		if (getGroup().isIdempotent()) {
			result = makeIfIndexConstraintIsSatisfiableThenBodyElseNothing();
		}
		else {
			result = bodyTimesNumberOfIndexValues();
		}
		return result;
	}

	private Expression makeIfIndexConstraintIsSatisfiableThenBodyElseNothing() {
		Expression conditionForSatisfiability = computerConstraintSatisfiability();
		Expression result = makeIfConditionThenBodyElseNothing(conditionForSatisfiability);
		return result;
	}

	private Expression makeIfConditionThenBodyElseNothing(Expression conditionForSatisfiability) {
		Expression identityElement = getGroup().additiveIdentityElement();
		Expression result = IfThenElse.makeWithoutConditionalCondition(conditionForSatisfiability, getBody(), identityElement);
		return result;
	}

	private Expression computerConstraintSatisfiability() {
		SingleVariableConstraint singleVariableConstraint = (SingleVariableConstraint) getConstraint();
		Expression conditionForSatisfiability = singleVariableConstraint.satisfiability(context);
		checkWeCanSolveSatisfiabilityOfConstraint(conditionForSatisfiability);
		return conditionForSatisfiability;
	}

	private Expression bodyTimesNumberOfIndexValues() {
		SingleVariableConstraint singleVariableConstraint = (SingleVariableConstraint) getConstraint();
		Expression modelCount = singleVariableConstraint.modelCount(context);
		Expression result = bodyTimes(modelCount);
		return result;
	}

	private Expression bodyTimes(Expression modelCount) {
		return getGroup().addNTimes(getBody(), modelCount, context);
	}

	private void checkThatIndexDoesNotAppearInBody() {
		requires( !isSubExpressionOf(getIndex(), getBody()), () -> getClass() + ": index occurs in body: " + toExpression());
	}

	private void checkWeCanSolveSatisfiabilityOfConstraint(Expression conditionForSatisfiability) throws Error {
		requires(conditionForSatisfiability != null, () -> "No satisfiability solver present for " + getIndex() + " while solving " + baseProblem);
	}
}