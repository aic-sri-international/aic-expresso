package com.sri.ai.grinder.plaindpll.theory;

import java.util.Collection;
import java.util.Collections;

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
 * {@link #incorporateNonTrivialNormalizedSplitterDestructively(boolean, Expression, RewritingProcess)},
 * which does not need to worry about checking redundancy or inconsistency, nor about creating a clone.
 * <p>
 * It also provides a {@link #supportedIndices} field.
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
		return Collections.unmodifiableCollection(supportedIndices);
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
				result = incorporateNonTrivialNormalizedSplitter(splitterSign, normalizedSplitterGivenConstraint, process);
			}
			catch (Contradiction e) {
				result = null;
			}
		}

		return result;
		// Note: while the above method could in principle be used for constraints that do not keep their own representation,
		// but rely on some other constraint as a basis,
		// it is generally not as appropriate for them, since they will typically delegate the task of normalizing a splitter
		// to the base constraint.
		// (Moreover, for it to be raised to constraints in general, incorporateNonTrivialNormalizedSplitter would have to be added
		// to Constraint).
	}

	private Constraint incorporateNonTrivialNormalizedSplitter(boolean splitterSign, Expression splitter, RewritingProcess process) {
		AbstractOwnRepresentationConstraint newConstraint = clone();
		newConstraint.incorporateNonTrivialNormalizedSplitterDestructively(splitterSign, splitter, process);
		return newConstraint;
	}

	/**
	 * Modify this constraint's inner representation to include this splitter,
	 * which has already been checked for redundancy or inconsistency with the constraint.
	 */
	public abstract void incorporateNonTrivialNormalizedSplitterDestructively(boolean splitterSign, Expression splitter, RewritingProcess process);
}