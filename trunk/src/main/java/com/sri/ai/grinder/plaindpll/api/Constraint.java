package com.sri.ai.grinder.plaindpll.api;

import java.util.Collection;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.plaindpll.core.ExpressionConstraint;
import com.sri.ai.grinder.plaindpll.core.SGDPLLT;
import com.sri.ai.grinder.plaindpll.core.SignedSplitter;
import com.sri.ai.grinder.plaindpll.problemtype.Satisfiability;

/**
 * An {@link Expression} with efficient internal representation for operations on being expanded by a splitter (literal in constraint theory) and
 * model counting.
 * This interface is defined for use primarily by {@link SGDPLLT}.
 * 
 * @author braz
 *
 */
public interface Constraint extends Expression {

	/** Makes a copy of this constraint, but with a new parent constraint. */
	Constraint copyWithNewParent(Constraint parentConstraint);
	
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
	public abstract Constraint incorporate(boolean splitterSign, Expression splitter, RewritingProcess process);

	/** Same as {@link #incorporate(boolean, Expression, RewritingProcess)} but using {@link SignedSplitter}. */
	default Constraint incorporate(SignedSplitter signedSplitter, RewritingProcess process) {
		return incorporate(signedSplitter.getSplitterSign(), signedSplitter.getSplitter(), process);
	}

	/**
	 * Provides a splitter, not already explicitly represented by the constraint,
	 * toward a state in which the model count for the given subset of indices
	 * can be computed (the model count may be condition on the other variables),
	 * or returns null if it already is in such a state.
	 */
	public abstract Expression pickSplitter(Collection<Expression> indicesSubSet, RewritingProcess process);

	/**
	 * Computes model count for constraint, given a sub-set of indices, in polynomial time.
	 * Assumes that {@link #pickSplitter(Collection, RewritingProcess)} returns <code>null</code>,
	 * that is, the constraint is in such a state and context that allows the determination of a unique model count.
	 * The model count is required to contain no literals implied by the contextual constraint.
	 */
	public abstract Expression modelCount(Collection<Expression> indicesSubSet, RewritingProcess process);

	/**
	 * Given a sub-set of supported indices, projects the constraint onto the remaining ones.
	 * Resulting constraint still supports all original indices.
	 * Default implementation uses symbolic satisfiability through {@link SGDPLLT}.
	 * Specific constraint implementations will typically have more efficient ways to do it.
	 */
	default Constraint project(Collection<Expression> eliminatedIndices, RewritingProcess process) {
		Solver projector = new SGDPLLT(getTheory(), new Satisfiability());
		Expression resultExpression = projector.solve(this, eliminatedIndices, process); // this was the motivation for making Constraint implement Expression
		// note that solvers should be aware that their input or part of their input may be a Constraint, and take advantage of the internal representations already present in them, instead of simply converting them to an Expression and redoing all the work.
		Constraint result = new ExpressionConstraint(getTheory(), getSupportedIndices(), resultExpression);
		return result;
	}
}