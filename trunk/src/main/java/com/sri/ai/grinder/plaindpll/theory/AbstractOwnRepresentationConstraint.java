package com.sri.ai.grinder.plaindpll.theory;

import java.util.Collection;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.plaindpll.api.Constraint;
import com.sri.ai.grinder.plaindpll.core.Contradiction;

/**
 * An abstract class implementing a {@link Constraint} that represents its own constraint,
 * instead of simply translating splitters to base constraint objects,
 * and translating solutions from them.
 * <p>
 * It provides an implementation of {@link #incorporate(boolean, Expression, RewritingProcess)} that
 * normalizes the splitter according to the solution to see if it is simplified to a constant
 * (in which case it is either redundant or a contradiction).
 * In case it is not, creates a clone of itself and invokes abstract method
 * {@link #applyNormalizedSplitterDestructively(boolean, Expression, RewritingProcess)},
 * which does not need to worry about checking redundancy or inconsistency, nor about creating a clone.
 * <p>
 * It also provides a {@link #supportedIndices} field.
 * <p>
 * TODO: this class needs redesigning; the only thing in it that is fundamentally dependent on a
 * class "providing its own representation" is the provision of a supportedIndices field.
 * The {@link #incorporate(boolean, Expression, RewritingProcess)} implementation does not
 * depend on a class providing its own implementation. 
 * However, this method's implementation does assume the constraint knows how to normalize a splitter,
 * so we cannot simply move it up to {@link AbstractConstraint}, which does not provide that guarantee
 * yet as of the time of this writing (April 2015).
 */	
@SuppressWarnings("serial")
public abstract class AbstractOwnRepresentationConstraint extends AbstractConstraint {
	
	protected Collection<Expression> supportedIndices;

	public AbstractOwnRepresentationConstraint(Collection<Expression> supportedIndices) {
		this.supportedIndices = supportedIndices;
	}
	
	public abstract AbstractOwnRepresentationConstraint clone();
	
	@Override
	public Collection<Expression> getSupportedIndices() {
		return supportedIndices;
	}

	@Override
	public Constraint incorporate(boolean splitterSign, Expression splitter, RewritingProcess process) {
		Constraint result;

		Expression normalizedSplitterGivenConstraint = normalizeSplitterGivenConstraint(splitter, process);
		
		if (normalizedSplitterGivenConstraint.equals(splitterSign)) {
			result = this; // splitter is redundant given constraint
		}
		else if (normalizedSplitterGivenConstraint.equals( ! splitterSign)) {
			result = null; // splitter is contradictory given constraint
		}
		else {
			try {
				result = applyNormalizedSplitter(splitterSign, normalizedSplitterGivenConstraint, process);
			}
			catch (Contradiction e) {
				result = null;
			}
		}

		return result;
	}

	private Constraint applyNormalizedSplitter(boolean splitterSign, Expression splitter, RewritingProcess process) {
		AbstractOwnRepresentationConstraint newConstraint = clone();
		newConstraint.applyNormalizedSplitterDestructively(splitterSign, splitter, process);
		return newConstraint;
	}

	/**
	 * Modify this constraint's inner representation to include this splitter,
	 * which has already been checked for redundancy or inconsistency with the constraint.
	 */
	abstract protected void applyNormalizedSplitterDestructively(boolean splitterSign, Expression splitter, RewritingProcess process);
}