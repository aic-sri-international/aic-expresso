package com.sri.ai.grinder.plaindpll.theory;

import static com.sri.ai.grinder.helper.GrinderUtil.getTypeCardinality;
import static com.sri.ai.util.Util.list;

import java.util.Collection;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.helper.AbstractExpressionWrapper;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.plaindpll.api.ConjunctiveConstraint;
import com.sri.ai.grinder.plaindpll.theory.EqualityTheory.EqualityConstraint;

/** Defined for the benefit of {@link EqualityConstraint} outside of it because the latter is a non-static class. */	
@SuppressWarnings("serial")
public abstract class AbstractNonEqualitiesConstraintForSingleVariable extends AbstractExpressionWrapper implements NonEqualitiesConstraintForSingleVariable {
	protected Expression variable;
	protected EqualityConstraint parentEqualityConstraint;
	protected long cachedIndexDomainSize;

	public AbstractNonEqualitiesConstraintForSingleVariable(Expression variable, EqualityConstraint parentEqualityConstraint) {
		this.variable = variable;
		this.cachedIndexDomainSize = -1;
		this.parentEqualityConstraint = parentEqualityConstraint;
	}
	
	public AbstractNonEqualitiesConstraintForSingleVariable clone() {
		assert false : "Cloning of " + getClass() + " not implemented yet.";
		return null;
	}

	@Override
	public EqualityTheory getTheory() {
		return parentEqualityConstraint.getTheory();
	}

	@Override
	public Collection<Expression> getSupportedIndices() {
		return list(variable);
	}

	protected long getIndexDomainSize(RewritingProcess process) {
		if (cachedIndexDomainSize == -1) {
			cachedIndexDomainSize = getTypeCardinality(variable, process);
		}
		return cachedIndexDomainSize;
	}

	@Override
	public ConjunctiveConstraint incorporate(boolean splitterSign, Expression splitter, RewritingProcess process) {
		assert false : (new Object(){}).getClass().getEnclosingMethod() + " not implemented yet."; // more robust to method renaming
		return null;
	}

	@Override
	public Expression normalize(Expression expression, RewritingProcess process) {
		assert false : (new Object(){}).getClass().getEnclosingMethod() + " not implemented yet."; // more robust to method renaming
		return null;
	}
}