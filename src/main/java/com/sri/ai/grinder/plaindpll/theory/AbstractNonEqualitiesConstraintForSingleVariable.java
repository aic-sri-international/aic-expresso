package com.sri.ai.grinder.plaindpll.theory;

import static com.sri.ai.grinder.helper.GrinderUtil.getTypeCardinality;
import static com.sri.ai.util.Util.list;

import java.util.Collection;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.plaindpll.api.Constraint;
import com.sri.ai.grinder.plaindpll.api.TermTheory;
import com.sri.ai.grinder.plaindpll.theory.EqualityTheory.EqualityTheoryConstraint;
import com.sri.ai.util.Util;

/** Defined for the benefit of {@link EqualityTheoryConstraint} outside of it because the latter is a non-static class. */	
@SuppressWarnings("serial")
public abstract class AbstractNonEqualitiesConstraintForSingleVariable extends AbstractOwnRepresentationConstraint implements NonEqualitiesConstraintForSingleVariable {
	protected Expression variable;
	protected long cachedIndexDomainSize;

	public AbstractNonEqualitiesConstraintForSingleVariable(Expression variable, Constraint parentEqualityConstraint) {
		super(list(variable));
		this.variable = variable;
		this.cachedIndexDomainSize = -1;
		this.parentConstraint = parentEqualityConstraint;
	}
	
	@Override
	public AbstractNonEqualitiesConstraintForSingleVariable clone() {
		Util.myAssert(() -> false, () -> "Cloning of " + getClass() + " not implemented yet.");
		return null;
	}
	
	public AbstractNonEqualitiesConstraintForSingleVariable copyWithNewParent(Constraint parentConstraint) {
		return (AbstractNonEqualitiesConstraintForSingleVariable) super.copyWithNewParent(parentConstraint);
	}

	@Override
	public EqualityTheory getTheory() {
		return ((EqualityTheoryConstraint)parentConstraint).getTheory();
	}

	@Override
	public Collection<Expression> getSupportedIndices() {
		return parentConstraint.getSupportedIndices();
	}

	protected TermTheory getTermTheory() {
		return ((EqualityTheoryConstraint)parentConstraint).getTermTheory();
	}
	
	protected long getIndexDomainSize(RewritingProcess process) {
		if (cachedIndexDomainSize == -1) {
			cachedIndexDomainSize = getTypeCardinality(variable, process);
		}
		return cachedIndexDomainSize;
	}

	@Override
	public Constraint incorporate(boolean splitterSign, Expression splitter, RewritingProcess process) {
		Util.myAssert(() -> false, () -> (new Object(){}).getClass().getEnclosingMethod() + " not implemented yet."); // more robust to method renaming
		return null;
	}
	
	@Override
	protected void applyNormalizedSplitterDestructively(boolean splitterSign, Expression splitter, RewritingProcess process) {
		Util.myAssert(() -> false, () -> (new Object(){}).getClass().getEnclosingMethod() + " not implemented yet."); // more robust to method renaming
	}


	@Override
	public Expression normalizeExpressionWithoutLiterals(Expression expression, RewritingProcess process) {
		Util.myAssert(() -> false, () -> (new Object(){}).getClass().getEnclosingMethod() + " not implemented yet."); // more robust to method renaming
		return null;
	}
}