package com.sri.ai.grinder.library.equality.cardinality.plaindpll;

import java.util.Collection;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.api.RewritingProcess;

/**
 * An {@link Expression} with efficient internal representation for operations on being expanded by a splitter (literal in constraint theory) and
 * model counting.
 * This interface is defined for use primarily by {@link SGDPLLT}.
 * 
 * @author braz
 *
 */
public interface Constraint extends Expression {

	/** The theory to which this constraint belongs. */
	Theory getTheory();
	
	/**
	 * The set of variables on subsets of which one can count models of this constraint.
	 */
	public abstract Collection<Expression> getSupportedIndices();

	/**
	 * Generates new constraint representing conjunction of this constraint and given splitter
	 * (or its negation, depending on the sign).
	 * @param splitterSign the splitter's sign (true for splitter itself, false for its negation)
	 * @param splitter the splitter according to this theoryWithEquality's choice
	 * @param process the rewriting process
	 */
	public abstract Constraint applySplitter(boolean splitterSign, Expression splitter, RewritingProcess process);

	/** Same as {@link #applySplitter(boolean, Expression, RewritingProcess)} but using {@link SignedSplitter}. */
	default Constraint applySplitter(SignedSplitter signedSplitter, RewritingProcess process) {
		return applySplitter(signedSplitter.getSplitterSign(), signedSplitter.getSplitter(), process);
	}

	/**
	 * Provides a splitter, not already explicitly represented by the constraint,
	 * toward a state in which the model count for the given subset of indices
	 * can be computed (the model count may be condition on the other variables),
	 * or returns null if it already is in such a state.
	 */
	public abstract Expression pickSplitter(Collection<Expression> indicesSubSet, RewritingProcess process);

	/**
	 * Same as {@link #pickSplitter(Collection, RewritingProcess)} invoked on getIndices().
	 */
	default Expression pickSplitter(RewritingProcess process) {
		return pickSplitter(getSupportedIndices(), process);
	}

	/**
	 * Computes model count for constraint, given a sub-set of indices, in polynomial time.
	 * Assumes that {@link #pickSplitter(Collection, RewritingProcess)} returns <code>null</code>,
	 * that is, the constraint is in such a state and context that allows the determination of a unique model count.
	 * The model count is required to contain no literals implied by the contextual constraint.
	 */
	public abstract Expression modelCount(Collection<Expression> indicesSubSet, RewritingProcess process);

	/**
	 * Same as {@link #modelCount(Collection, RewritingProcess)} invoked on getIndices().
	 */
	default Expression modelCount(RewritingProcess process) {
		return modelCount(getSupportedIndices(), process);
	}
}