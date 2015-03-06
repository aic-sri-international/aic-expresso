package com.sri.ai.grinder.library.equality.cardinality.plaindpll.api;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.library.equality.cardinality.plaindpll.core.SignedSplitter;

/**
 * An interface for theoryWithEquality-specific representations of conjunctive constraints.
 * 
 * @author braz
 *
 */
@Beta
public interface ConjunctiveConstraint extends Constraint {
	
	public abstract ConjunctiveConstraint applySplitter(boolean splitterSign, Expression splitter, RewritingProcess process);

	/** Same as {@link #applySplitter(boolean, Expression, RewritingProcess)} but using {@link SignedSplitter}. */
	default ConjunctiveConstraint applySplitter(SignedSplitter signedSplitter, RewritingProcess process) {
		return applySplitter(signedSplitter.getSplitterSign(), signedSplitter.getSplitter(), process);
	}

	/**
	 * Simplifies a given splitter to true if implied by constraint, false if its negation is implied by constraint,
	 * or a version of itself with terms replaced by representatives.
	 * Note that {@link #normalize(Expression, RewritingProcess)} cannot be used instead of this method
	 * because, for certain theories, the result of normalizing an splitter by treating it as an expression
	 * may produce a non-boolean-constant expression that is not a splitter either.
	 * For example, in equality theory, {@link #normalize(Expression, RewritingProcess)}
	 * transforms 'Term = true' into 'Term' and 'Term = false' into 'not Term',
	 * and these are not splitters for that theory anymore.
	 */
	Expression normalizeSplitterGivenConstraint(Expression splitter, RewritingProcess process);

	/**
	 * Receives an expression and returns an equivalent one according to some normalization property
	 * For example, an implementation involving equality may choose to always represent all symbols in an equality cluster
	 * by the same symbol. 
	 * This method is not required to perform complete inference (that is, to return some minimal representation
	 * of the expression).
	 * @param expression
	 * @param process
	 * @return
	 */
	Expression normalize(Expression expression, RewritingProcess process);
}