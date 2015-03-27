package com.sri.ai.grinder.plaindpll.theory;

import static com.sri.ai.grinder.helper.GrinderUtil.getTypeCardinality;
import static com.sri.ai.util.Util.list;
import static com.sri.ai.util.Util.myAssert;

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
	protected NonEqualitiesConstraint nonEqualitiesConstraint; // the DefaultNonEqualitiesConstraint containing this one

	public AbstractNonEqualitiesConstraintForSingleVariable(Expression variable, NonEqualitiesConstraint nonEqualitiesConstraint) {
		super(list(variable));
		this.variable = variable;
		this.cachedIndexDomainSize = -1;
		this.nonEqualitiesConstraint = nonEqualitiesConstraint;
	}
	
	@Override
	public AbstractNonEqualitiesConstraintForSingleVariable clone() {
		Util.myAssert(() -> false, () -> "Cloning of " + getClass() + " not implemented yet.");
		return null;
	}
	
	@Override
	public NonEqualitiesConstraint getNonEqualitiesConstraint(NonEqualitiesConstraint nonEqualitiesConstraint) {
		myAssert(() -> nonEqualitiesConstraint != null, "nonEqualitiesConstraint must not be null");
		return nonEqualitiesConstraint;
	}

	@Override
	public NonEqualitiesConstraintForSingleVariable cloneWithNewNonEqualitiesConstraint(NonEqualitiesConstraint nonEqualitiesConstraint) {
		AbstractNonEqualitiesConstraintForSingleVariable result = clone();
		result.nonEqualitiesConstraint = nonEqualitiesConstraint;
		return result;
	}

	@Override
	public EqualityTheory getTheory() {
		return nonEqualitiesConstraint.getTheory();
	}

	@Override
	public Collection<Expression> getSupportedIndices() {
		return nonEqualitiesConstraint.getSupportedIndices();
	}

	protected TermTheory getTermTheory() {
		return nonEqualitiesConstraint.getTermTheory();
	}
	
	protected long getVariableDomainSize(RewritingProcess process) {
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