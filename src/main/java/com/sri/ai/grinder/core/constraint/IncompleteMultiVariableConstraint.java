package com.sri.ai.grinder.core.constraint;

import static com.sri.ai.expresso.helper.Expressions.FALSE;
import static com.sri.ai.expresso.helper.Expressions.TRUE;
import static com.sri.ai.util.Util.min;
import static com.sri.ai.util.Util.myAssert;

import java.util.LinkedHashMap;
import java.util.Map;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.api.Constraint;
import com.sri.ai.grinder.api.Context;
import com.sri.ai.grinder.api.SingleVariableConstraint;
import com.sri.ai.grinder.api.Theory;
import com.sri.ai.grinder.library.boole.And;
import com.sri.ai.util.Util;

/**
 * An incomplete implementation of {@link Constraint} on multiple variables.
 * This implementation is based on the idea of keeping a {@link SingleVariableConstraint} for each variable in the literals conjoined so far.
 * 
 * When conjoined, a variable is picked from a literal (for example, the first one in regular search) and that literal is conjoined to the
 * {@link SingleVariableConstraint} corresponding to that variable.
 * 
 * The implementation is incomplete, that is, it does not necessarily detect unsatisfiability after conjoining a literal.
 * For example, the sequence of literals <code>X != Y</code>, <code>Y = X</code> is unsatisfiable, but this will not be detected
 * if the first variable in the literal is picked, because the first literal will be used to form a {@link SingleVariableConstraint} for <code>X</code>,
 * while the second one will be used to form a separate {@link SingleVariableConstraint} for <code>Y</code>.
 * However, this implementation may be useful for some scenarios, so it is kept for the time being.
 * 
 * @author braz
 *
 */
public class IncompleteMultiVariableConstraint extends AbstractConstraint {

	private static final long serialVersionUID = 1L;
	
	private Map<Expression, SingleVariableConstraint> fromVariableToItsConstraint;
	
	public IncompleteMultiVariableConstraint(Theory theory) {
		this(theory, Util.map());
	}
	
	private IncompleteMultiVariableConstraint(Theory theory, Map<Expression, SingleVariableConstraint> fromVariableToItsConstraint) {
		super(theory);
		this.fromVariableToItsConstraint = fromVariableToItsConstraint;
	}
	
	private IncompleteMultiVariableConstraint makeWithNewFromVariableToItsConstraint(Map<Expression, SingleVariableConstraint> newFromVariableToItsConstraint) {
		return new IncompleteMultiVariableConstraint(getTheory(), newFromVariableToItsConstraint);
	}
	
	@Override
	public IncompleteMultiVariableConstraint conjoinWithLiteral(Expression literal, Context context) {
		IncompleteMultiVariableConstraint result;
		
		Expression variable = getSomeVariableFor(literal, context);
		boolean literalHasNoVariables = variable == null;
		if (literalHasNoVariables) {
			result = conjointLiteralWithoutVariables(literal, context);
		}
		else {
			result = conjoinLiteralForVariable(literal, variable, context);
		}
		
		return result;
	}

	private IncompleteMultiVariableConstraint conjointLiteralWithoutVariables(Expression literal, Context context) {
		IncompleteMultiVariableConstraint result;
		Expression literalEvaluation = getTheory().simplify(literal, context);
		if (literalEvaluation.equals(TRUE)) {
			result = this;
		}
		else if (literalEvaluation.equals(FALSE)) {
			result = makeContradiction();
		}
		else {
			throw new Error("Literal " + literal + " has no variables but does not evaluate to either true or false. This may be caused by not including theories definining the meaning of operators and constants in it, preventing its evaluation");
		}
		return result;
	}

	private IncompleteMultiVariableConstraint conjoinLiteralForVariable(Expression literal, Expression variable, Context context) {
		IncompleteMultiVariableConstraint result;
		SingleVariableConstraint singleVariableConstraint = getConstraintFor(variable, context);
		SingleVariableConstraint newSingleVariableConstraint = singleVariableConstraint.conjoin(literal, context);
		if (newSingleVariableConstraint.isContradiction()) {
			result = makeContradiction();	
		}
		else if (newSingleVariableConstraint == singleVariableConstraint) {
			result = this;
		}
		else {
			result = makeCopyWithNewSingleVariableConstraintForVariable(variable, newSingleVariableConstraint);
		}
		return result;
	}

	private IncompleteMultiVariableConstraint makeCopyWithNewSingleVariableConstraintForVariable(Expression variable, SingleVariableConstraint newSingleVariableConstraint) {
		IncompleteMultiVariableConstraint result;
		Map<Expression, SingleVariableConstraint> newFromVariableToItsConstraint = new LinkedHashMap<>(fromVariableToItsConstraint);
		newFromVariableToItsConstraint.put(variable, newSingleVariableConstraint);
		result = makeWithNewFromVariableToItsConstraint(newFromVariableToItsConstraint);
		return result;
	}

	@Override
	public IncompleteMultiVariableConstraint makeContradiction() {
		return (IncompleteMultiVariableConstraint) super.makeContradiction();
	}
	
	private Expression getSomeVariableFor(Expression literal, Context context) {
		Expression result = min(getTheory().getVariablesIn(literal, context), (e1, e2) -> e1.compareTo(e2));
		return result;
	}

	protected SingleVariableConstraint getConstraintFor(Expression variable, Context context) {
		SingleVariableConstraint result = fromVariableToItsConstraint.get(variable);
		if (result == null) {
			result = getTheory().makeSingleVariableConstraint(variable, context);
		}
		return result;
	}
	
	@Override
	protected Expression computeInnerExpressionIfNotContradiction() {
		Expression result = And.make(fromVariableToItsConstraint.values());
		return result;
	}

	@Override
	public IncompleteMultiVariableConstraint clone() {
		return (IncompleteMultiVariableConstraint) super.clone();
	}
	
	@Override
	public Constraint conjoin(Expression formula, Context context) {
		myAssert(
				() -> context.isLiteral(formula) || formula instanceof Constraint,
				() -> this.getClass() + " currently only supports conjoining with literals and constraints, but received " + formula);
		
		Constraint result;

		if (formula instanceof Constraint) {
			result = conjoinWithConjunctiveClause(formula, context);
		}
		else {
			result = conjoinWithLiteral(formula, context);
		}

		return result;
	}

	@Override
	public Expression binding(Expression variable) {
		SingleVariableConstraint singleVariableConstraint = fromVariableToItsConstraint.get(variable);
		return singleVariableConstraint.binding(variable);
	}
}