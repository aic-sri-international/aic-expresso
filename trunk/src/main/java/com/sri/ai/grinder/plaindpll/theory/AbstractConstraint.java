package com.sri.ai.grinder.plaindpll.theory;

import java.util.Collection;
import java.util.Collections;

import com.google.common.base.Function;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.helper.AbstractExpressionWrapper;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.plaindpll.api.Constraint;

/** An class implementing basic {@link Constraint} functionality. */	
@SuppressWarnings("serial")
public abstract class AbstractConstraint extends AbstractExpressionWrapper implements Constraint {
	
	public abstract AbstractConstraint clone();
	
	/**
	 * Default implementation returning an immutable empty list.
	 */
	public Collection<Expression> getImpliedNonRepresentableSplitters() {
		return Collections.emptyList();
	}
	
	// TODO: give safeguarded default implementations for these methods
	
	@Override
	public void incorporateNonTrivialNormalizedSplitterDestructively(boolean splitterSign, Expression splitter, RewritingProcess process) {
		throw new Error(this.getClass().getSimpleName() + "." + (new Object(){}).getClass().getEnclosingMethod() + " not implemented yet.");
	}
	
	@Override
	public boolean directlyImpliesNonTrivialLiteral(Expression literal, RewritingProcess process) {
		throw new Error(this.getClass().getSimpleName() + "." + (new Object(){}).getClass().getEnclosingMethod() + " not implemented yet.");
	}
	
	@Override
	public void updateRepresentativesDestructively(Function<Expression, Expression> getRepresentative, RewritingProcess process) {
		throw new Error(this.getClass().getSimpleName() + "." + (new Object(){}).getClass().getEnclosingMethod() + " not implemented yet.");
	}
}