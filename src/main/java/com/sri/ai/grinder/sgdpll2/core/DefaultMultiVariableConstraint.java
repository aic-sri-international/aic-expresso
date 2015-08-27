package com.sri.ai.grinder.sgdpll2.core;

import static com.sri.ai.util.Util.min;

import java.util.LinkedHashMap;
import java.util.Map;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.helper.AbstractExpressionWrapper;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.library.boole.And;
import com.sri.ai.grinder.sgdpll2.api.Constraint;
import com.sri.ai.grinder.sgdpll2.api.ConstraintTheory;
import com.sri.ai.util.Util;

/**
 * An {@link Constraint} on multiple variables.
 * 
 * @author braz
 *
 */
public class DefaultMultiVariableConstraint extends AbstractExpressionWrapper implements MultiVariableConstraint {

	private static final long serialVersionUID = 1L;
	
	private ConstraintTheory constraintTheory;
	private Map<Expression, SingleVariableConstraint> fromVariableToItsConstraint;

	public DefaultMultiVariableConstraint(ConstraintTheory constraintTheory) {
		this(constraintTheory, Util.map());
	}
	
	private DefaultMultiVariableConstraint(ConstraintTheory constraintTheory, Map<Expression, SingleVariableConstraint> fromVariableToItsConstraint) {
		this.constraintTheory = constraintTheory;
		this.fromVariableToItsConstraint = fromVariableToItsConstraint;
	}
	
	private DefaultMultiVariableConstraint makeWithNewFromVariableToItsConstraint(Map<Expression, SingleVariableConstraint> newFromVariableToItsConstraint) {
		return new DefaultMultiVariableConstraint(constraintTheory, newFromVariableToItsConstraint);
	}
	
	@Override
	public DefaultMultiVariableConstraint conjoin(Expression literal, RewritingProcess process) {
		DefaultMultiVariableConstraint result;
		
		Expression variable = getVariableFor(literal, process);
		SingleVariableConstraint singleVariableConstraint = getConstraintFor(variable);
		SingleVariableConstraint newSingleVariableConstraint = singleVariableConstraint.conjoin(literal, process);
		if (newSingleVariableConstraint == null) {
			result = null;	
		}
		else if (newSingleVariableConstraint == singleVariableConstraint) {
			result = this;
		}
		else {
			Map<Expression, SingleVariableConstraint> newFromVariableToItsConstraint = new LinkedHashMap<>(fromVariableToItsConstraint);
			newFromVariableToItsConstraint.put(variable, newSingleVariableConstraint);
			result = makeWithNewFromVariableToItsConstraint(newFromVariableToItsConstraint);
		}
		
		return result;
	}
	
	private Expression getVariableFor(Expression literal, RewritingProcess process) {
		Expression result = min(constraintTheory.getVariablesIn(literal, process), (e1, e2) -> e1.compareTo(e2));
		return result;
	}

	/**
	 * @param variable
	 * @return
	 */
	protected SingleVariableConstraint getConstraintFor(Expression variable) {
		SingleVariableConstraint result = fromVariableToItsConstraint.get(variable);
		if (result == null) {
			result = constraintTheory.makeSingleVariableConstraint(variable);
		}
		return result;
	}
	
	@Override
	protected Expression computeInnerExpression() {
		Expression result = And.make(fromVariableToItsConstraint.values());
		return result;
	}

	@Override
	public Expression clone() {
		DefaultMultiVariableConstraint result = new DefaultMultiVariableConstraint(constraintTheory, new LinkedHashMap<>(fromVariableToItsConstraint));
		return result;
	}
	
	@Override
	public ConstraintTheory getConstraintTheory() {
		return constraintTheory;
	}
}