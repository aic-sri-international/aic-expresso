package com.sri.ai.grinder.plaindpll.theory;

import static com.sri.ai.grinder.helper.GrinderUtil.getTypeCardinality;

import java.util.Collection;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.plaindpll.api.Constraint;
import com.sri.ai.grinder.plaindpll.api.TermTheory;
import com.sri.ai.grinder.plaindpll.theory.EqualityTheory.EqualityConstraint;

/** Defined for the benefit of {@link EqualityConstraint} outside of it because the latter is a non-static class. */	
@SuppressWarnings("serial")
public abstract class AbstractNonEqualitiesConstraintForSingleVariable extends AbstractConstraint implements NonEqualitiesConstraintForSingleVariable {
	protected Expression variable;
	protected long cachedIndexDomainSize;

	public AbstractNonEqualitiesConstraintForSingleVariable(Expression variable, Constraint parentEqualityConstraint) {
		this.variable = variable;
		this.cachedIndexDomainSize = -1;
		this.parentConstraint = parentEqualityConstraint;
	}
	
	public AbstractNonEqualitiesConstraintForSingleVariable clone() {
		assert false : "Cloning of " + getClass() + " not implemented yet.";
		return null;
	}
	
	public AbstractNonEqualitiesConstraintForSingleVariable copyWithNewParent(Constraint parentConstraint) {
		return (AbstractNonEqualitiesConstraintForSingleVariable) super.copyWithNewParent(parentConstraint);
	}

	@Override
	public EqualityTheory getTheory() {
		return ((EqualityConstraint)parentConstraint).getTheory();
	}

	@Override
	public Collection<Expression> getSupportedIndices() {
		return parentConstraint.getSupportedIndices();
	}

	protected TermTheory getTermTheory() {
		return ((EqualityConstraint)parentConstraint).getTermTheory();
	}
	
	protected long getIndexDomainSize(RewritingProcess process) {
		if (cachedIndexDomainSize == -1) {
			cachedIndexDomainSize = getTypeCardinality(variable, process);
		}
		return cachedIndexDomainSize;
	}

	@Override
	public Constraint incorporate(boolean splitterSign, Expression splitter, RewritingProcess process) {
		assert false : (new Object(){}).getClass().getEnclosingMethod() + " not implemented yet."; // more robust to method renaming
		return null;
	}

	@Override
	public Expression normalize(Expression expression, RewritingProcess process) {
		assert false : (new Object(){}).getClass().getEnclosingMethod() + " not implemented yet."; // more robust to method renaming
		return null;
	}
}