package com.sri.ai.grinder.plaindpll.theory;

import com.google.common.base.Function;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.helper.AbstractExpressionWrapper;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.plaindpll.api.Constraint;

/** An class implementing basic {@link Constraint} functionality. */	
@SuppressWarnings("serial")
public abstract class AbstractConstraint extends AbstractExpressionWrapper implements Constraint {
	
	protected Constraint parentConstraint;

	public abstract AbstractConstraint clone();
	
	@Override
	public AbstractConstraint copyWithNewParent(Constraint parentConstraint) {
		AbstractConstraint result = (AbstractConstraint) clone();
		result.parentConstraint = parentConstraint;
		return result;
	}
	
//	
//	/**
//	 * A constraint under which this constraint is embedded,
//	 * and the consequences of which should be utilized as much as possible by this constraint;
//	 * also, the parent constraint should be able to reply to queries from the child constraint.
//	 */
//	protected Constraint getParentConstraint() {
//		return parentConstraint;
//	}

	@Override
	public Constraint incorporatePossiblyDestructively(boolean splitterSign, Expression splitter, RewritingProcess process) {
		throw new Error(this.getClass().getSimpleName() + "." + (new Object(){}).getClass().getEnclosingMethod() + " not implemented yet.");
	}

	@Override
	public boolean directlyImpliesNonTrivialLiteral(Expression literal, RewritingProcess process) {
		throw new Error(this.getClass().getSimpleName() + "." + (new Object(){}).getClass().getEnclosingMethod() + " not implemented yet.");
	}
	
	@Override
	public Constraint updateRepresentativesPossiblyDestructively(Function<Expression, Expression> getRepresentative, RewritingProcess process) {
		throw new Error(this.getClass().getSimpleName() + "." + (new Object(){}).getClass().getEnclosingMethod() + " not implemented yet.");
	}
}