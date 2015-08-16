package com.sri.ai.grinder.plaindpll.api;

import static com.sri.ai.util.Util.addAllToList;
import static com.sri.ai.util.Util.getFirstNonNullResultOrNull;
import static com.sri.ai.util.Util.mapIntoArrayList;
import static com.sri.ai.util.Util.mapValuesNonDestructively;
import static com.sri.ai.util.Util.min;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.Map;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.helper.AbstractExpressionWrapper;
import com.sri.ai.expresso.helper.SubExpressionsDepthFirstIterator;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.library.boole.And;
import com.sri.ai.grinder.library.number.Times;
import com.sri.ai.util.collect.PredicateIterator;

/**
 * An {@link Constraint} on multiple variables.
 * 
 * @author braz
 *
 */
public class MultiVariableConstraint extends AbstractExpressionWrapper {

	private static final long serialVersionUID = 1L;
	
	private ConstraintTheory constraintTheory;
	private Map<Expression, SingleVariableConstraint> fromVariableToItsConstraint;
	
	private MultiVariableConstraint(ConstraintTheory constraintTheory, Map<Expression, SingleVariableConstraint> fromVariableToItsConstraint) {
		this.constraintTheory = constraintTheory;
		this.fromVariableToItsConstraint = fromVariableToItsConstraint;
	}
	
	private MultiVariableConstraint makeWithNewFromVariableToItsConstraint(Map<Expression, SingleVariableConstraint> newFromVariableToItsConstraint) {
		return new MultiVariableConstraint(constraintTheory, newFromVariableToItsConstraint);
	}
	
	public MultiVariableConstraint simplifyGiven(Expression literal, RewritingProcess process) {
		Map<Expression, SingleVariableConstraint> updatedFromVariableToItsConstraint =
				mapValuesNonDestructively(fromVariableToItsConstraint, c -> c.simplifyGiven(literal, process));

		MultiVariableConstraint result;
		if (updatedFromVariableToItsConstraint != fromVariableToItsConstraint) {
			result = makeWithNewFromVariableToItsConstraint(updatedFromVariableToItsConstraint);
		}
		else {
			result = this;
		}
		
		return result;
	}
	
	MultiVariableConstraint conjoin(Expression literal, RewritingProcess process) {
		MultiVariableConstraint result;
		
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
	
	public Expression pickSplitter(RewritingProcess process) {
		Expression result = getFirstNonNullResultOrNull(fromVariableToItsConstraint.values(), c -> c.pickSplitter(process));
		return result;
	}

	public Expression modelCount(RewritingProcess process) {
		ArrayList<Expression> counts = mapIntoArrayList(fromVariableToItsConstraint.values(), c -> c.modelCount(process));
		Expression result = Times.make(counts);
		return result;
	}

	@Override
	protected Expression computeInnerExpression() {
		Expression result = And.make(fromVariableToItsConstraint.values());
		return result;
	}

	@Override
	public Expression clone() {
		MultiVariableConstraint result = new MultiVariableConstraint(constraintTheory, new LinkedHashMap<>(fromVariableToItsConstraint));
		return result;
	}

}