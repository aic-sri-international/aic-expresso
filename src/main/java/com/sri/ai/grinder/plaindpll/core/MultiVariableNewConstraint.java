package com.sri.ai.grinder.plaindpll.core;

import static com.sri.ai.util.Util.min;

import java.util.LinkedHashMap;
import java.util.Map;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.helper.AbstractExpressionWrapper;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.library.boole.And;
import com.sri.ai.grinder.plaindpll.api.Constraint;
import com.sri.ai.grinder.plaindpll.api.NewConstraint;
import com.sri.ai.grinder.plaindpll.api.NewConstraintTheory;
import com.sri.ai.grinder.plaindpll.api.SingleVariableNewConstraint;
import com.sri.ai.util.Util;

/**
 * An {@link Constraint} on multiple variables.
 * 
 * @author braz
 *
 */
public class MultiVariableNewConstraint extends AbstractExpressionWrapper implements NewConstraint {

	private static final long serialVersionUID = 1L;
	
	private NewConstraintTheory constraintTheory;
	private Map<Expression, SingleVariableNewConstraint> fromVariableToItsConstraint;

	public MultiVariableNewConstraint(NewConstraintTheory constraintTheory) {
		this(constraintTheory, Util.map());
	}
	
	private MultiVariableNewConstraint(NewConstraintTheory constraintTheory, Map<Expression, SingleVariableNewConstraint> fromVariableToItsConstraint) {
		this.constraintTheory = constraintTheory;
		this.fromVariableToItsConstraint = fromVariableToItsConstraint;
	}
	
	private MultiVariableNewConstraint makeWithNewFromVariableToItsConstraint(Map<Expression, SingleVariableNewConstraint> newFromVariableToItsConstraint) {
		return new MultiVariableNewConstraint(constraintTheory, newFromVariableToItsConstraint);
	}
	
	@Override
	public NewConstraint conjoin(Expression literal, RewritingProcess process) {
		MultiVariableNewConstraint result;
		
		Expression variable = getVariableFor(literal, process);
		SingleVariableNewConstraint singleVariableConstraint = getConstraintFor(variable);
		SingleVariableNewConstraint newSingleVariableConstraint = singleVariableConstraint.conjoin(literal, process);
		if (newSingleVariableConstraint == null) {
			result = null;	
		}
		else if (newSingleVariableConstraint == singleVariableConstraint) {
			result = this;
		}
		else {
			Map<Expression, SingleVariableNewConstraint> newFromVariableToItsConstraint = new LinkedHashMap<>(fromVariableToItsConstraint);
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
	protected SingleVariableNewConstraint getConstraintFor(Expression variable) {
		SingleVariableNewConstraint result = fromVariableToItsConstraint.get(variable);
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
		MultiVariableNewConstraint result = new MultiVariableNewConstraint(constraintTheory, new LinkedHashMap<>(fromVariableToItsConstraint));
		return result;
	}

}