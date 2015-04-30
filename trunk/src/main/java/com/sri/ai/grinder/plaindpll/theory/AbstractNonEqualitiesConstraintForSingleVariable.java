package com.sri.ai.grinder.plaindpll.theory;

import static com.sri.ai.grinder.helper.GrinderUtil.getTypeCardinality;
import static com.sri.ai.grinder.library.FunctorConstants.TYPE;
import static com.sri.ai.util.Util.list;

import java.util.Collection;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.core.DefaultSyntacticFunctionApplication;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.plaindpll.api.Constraint;
import com.sri.ai.grinder.plaindpll.api.TermTheory;
import com.sri.ai.util.Util;

@SuppressWarnings("serial")
public abstract class AbstractNonEqualitiesConstraintForSingleVariable extends AbstractOwnRepresentationConstraint implements NonEqualitiesConstraintForSingleVariable {
	protected Expression variable;
	protected long cachedIndexDomainSize;
	protected EqualityConstraintTheory theory;
	protected Collection<Expression> supportedIndices;

	public AbstractNonEqualitiesConstraintForSingleVariable(Expression variable, EqualityConstraintTheory theory, Collection<Expression> supportedIndices) {
		super(list(variable));
		this.variable = variable;
		this.cachedIndexDomainSize = -1;
		this.theory = theory;
		this.supportedIndices = supportedIndices;
	}
	
	@Override
	public AbstractNonEqualitiesConstraintForSingleVariable clone() {
		Util.myAssert(() -> false, () -> "Cloning of " + getClass() + " not implemented yet.");
		return null;
	}
	
	@Override
	public Expression getVariable() {
		return variable;
	}
	
	protected Expression getVariableDomain(RewritingProcess process) {
		Expression variableType = process.getContextualSymbolType(variable);
		if (variableType == null) {
			variableType = new DefaultSyntacticFunctionApplication(TYPE, variable);
		}
		return variableType;
	}

	@Override
	public EqualityConstraintTheory getTheory() {
		return theory;
	}

	@Override
	public Collection<Expression> getSupportedIndices() {
		return supportedIndices;
	}

	protected TermTheory getTermTheory() {
		return getTheory().getTermTheory();
	}
	
	protected long getVariableDomainSize(RewritingProcess process) {
		if (cachedIndexDomainSize == -1) {
			cachedIndexDomainSize = getTypeCardinality(variable, process);
		}
		return cachedIndexDomainSize;
	}

	@Override
	public Expression pickSplitter(Collection<Expression> indicesSubSet, RewritingProcess process) {
		throw new Error((new Object(){}).getClass().getEnclosingMethod() + " not valid for " + getClass() + ". Use similar method taking predicate for directly implied disequalities.");
	}

	@Override
	public Constraint incorporate(boolean splitterSign, Expression splitter, RewritingProcess process) {
		Util.myAssert(() -> false, () -> (new Object(){}).getClass().getEnclosingMethod() + " disabled for " + getClass().getSimpleName() + "; use version that takes exterior constraint instead"); // more robust to method renaming
		return null;
	}

	@Override
	public void incorporateDestructively(boolean splitterSign, Expression splitter, RewritingProcess process) {
		Util.myAssert(() -> false, () -> (new Object(){}).getClass().getEnclosingMethod() + " disabled for " + getClass().getSimpleName() + "; incorporation must be done with version that takes exterior constraint instead"); // more robust to method renaming
	}

	@Override
	protected void incorporateNonTrivialNormalizedSplitterDestructively(boolean splitterSign, Expression splitter, RewritingProcess process) {
		Util.myAssert(() -> false, () -> (new Object(){}).getClass().getEnclosingMethod() + " disabled for " + getClass().getSimpleName() + "; incorporation must be done with version that takes exterior constraint instead"); // more robust to method renaming
	}

	@Override
	public Expression normalizeExpressionWithoutLiterals(Expression expression, RewritingProcess process) {
		Util.myAssert(() -> false, () -> (new Object(){}).getClass().getEnclosingMethod() + " not implemented yet."); // more robust to method renaming
		return null;
	}
}