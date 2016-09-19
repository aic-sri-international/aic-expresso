package com.sri.ai.grinder.sgdpllt.core.constraint;

import static com.sri.ai.expresso.helper.Expressions.TRUE;
import static com.sri.ai.util.Util.min;
import static com.sri.ai.util.Util.myAssert;

import java.util.LinkedHashMap;
import java.util.Map;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.sgdpllt.api.Constraint;
import com.sri.ai.grinder.sgdpllt.api.Context;
import com.sri.ai.grinder.sgdpllt.api.SingleVariableConstraint;
import com.sri.ai.grinder.sgdpllt.api.Theory;
import com.sri.ai.grinder.sgdpllt.library.boole.And;
import com.sri.ai.util.Util;

/**
 * An {@link Constraint} on multiple variables.
 * Unlike {@link MultiVariableContextWithCheckedProperty} and its extension
 * {@link CompleteMultiVariableContext}, it does not check for satisfiability
 * or any other property, but simply keeps an aggregate of single-variable constraints.
 * 
 * @author braz
 *
 */
public class DefaultMultiVariableConstraint extends AbstractConstraint {

	private static final long serialVersionUID = 1L;
	
	private Map<Expression, SingleVariableConstraint> fromVariableToItsConstraint;
	
	public DefaultMultiVariableConstraint(Theory theory) {
		this(theory, Util.map());
	}
	
	private DefaultMultiVariableConstraint(Theory theory, Map<Expression, SingleVariableConstraint> fromVariableToItsConstraint) {
		super(theory);
		this.fromVariableToItsConstraint = fromVariableToItsConstraint;
	}
	
	private DefaultMultiVariableConstraint makeWithNewFromVariableToItsConstraint(Map<Expression, SingleVariableConstraint> newFromVariableToItsConstraint) {
		return new DefaultMultiVariableConstraint(getTheory(), newFromVariableToItsConstraint);
	}
	
	@Override
	public DefaultMultiVariableConstraint conjoinWithLiteral(Expression literal, Context context) {
		DefaultMultiVariableConstraint result;
		
		Expression variable = getSomeVariableFor(literal, context);
		if (variable == null) {
			// the literal has no variables.
			Expression literalEvaluation = getTheory().simplify(literal, context);
			if (literalEvaluation.equals(TRUE)) {
				result = this;
			}
			else {
				result = makeContradiction();
			}
		}
		else {
			SingleVariableConstraint singleVariableConstraint = getConstraintFor(variable, context);
			SingleVariableConstraint newSingleVariableConstraint = singleVariableConstraint.conjoin(literal, context);
			if (newSingleVariableConstraint.isContradiction()) {
				result = makeContradiction();	
			}
			else if (newSingleVariableConstraint == singleVariableConstraint) {
				result = this;
			}
			else {
				Map<Expression, SingleVariableConstraint> newFromVariableToItsConstraint = new LinkedHashMap<>(fromVariableToItsConstraint);
				newFromVariableToItsConstraint.put(variable, newSingleVariableConstraint);
				result = makeWithNewFromVariableToItsConstraint(newFromVariableToItsConstraint);
			}
		}
		
		return result;
	}

	/**
	 * @return
	 */
	@Override
	public DefaultMultiVariableConstraint makeContradiction() {
		return (DefaultMultiVariableConstraint) super.makeContradiction();
	}
	
	private Expression getSomeVariableFor(Expression literal, Context context) {
		Expression result = min(getTheory().getVariablesIn(literal, context), (e1, e2) -> e1.compareTo(e2));
		return result;
	}

	/**
	 * @param variable
	 * @return
	 */
	protected SingleVariableConstraint getConstraintFor(Expression variable, Context context) {
		SingleVariableConstraint result = fromVariableToItsConstraint.get(variable);
		if (result == null) {
			result = getTheory().makeSingleVariableConstraint(variable, getTheory(), context);
		}
		return result;
	}
	
	@Override
	protected Expression computeInnerExpressionIfNotContradiction() {
		Expression result = And.make(fromVariableToItsConstraint.values());
		return result;
	}

	@Override
	public DefaultMultiVariableConstraint clone() {
		return (DefaultMultiVariableConstraint) super.clone();
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