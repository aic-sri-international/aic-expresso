package com.sri.ai.grinder.plaindpll.api;

import static com.sri.ai.expresso.helper.Expressions.TRUE;

import java.util.Collection;

import com.google.common.base.Function;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.plaindpll.core.ExpressionConstraint;
import com.sri.ai.grinder.plaindpll.core.SGDPLLT;
import com.sri.ai.grinder.plaindpll.core.SignedSplitter;
import com.sri.ai.grinder.plaindpll.problemtype.Satisfiability;
import com.sri.ai.util.base.BinaryPredicate;

/**
 * An {@link Expression} with efficient internal representation for operations on being expanded by a splitter (literal in constraint constraintTheory) and
 * model counting.
 * This interface is defined for use primarily by {@link SGDPLLT}.
 * 
 * @author braz
 *
 */
public interface Constraint extends Expression {

	Constraint clone();
	
	/** The constraintTheory to which this constraint belongs. */
	ConstraintTheory getTheory();

	/**
	 * The set of variables on subsets of which one can count models of this constraint,
	 * and which have preference in coming first in splitters (even if not being counted on).
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
	 * Same as {@link #incorporate(boolean, Expression, RewritingProcess)}, but 
	 * must destructively alter the instance itself.
	 * This option is available for performance reasons; sometimes during setup stages,
	 * it is much more efficient to make several changes to the same object
	 * instead of creating an instance after each change.
	 * This violates the immutability assumption about {@link Expression} and {@link Constraint}
	 * and should only be used for setup purposes, before an object is released by its creator
	 * to the world at large (because then it may be assumed immutable by other objects holding it,
	 * so it should behave immutable from then on).
	 * @param splitterSign the splitter's sign (true for splitter itself, false for its negation)
	 * @param splitter the splitter according to this theoryWithEquality's choice
	 * @param process the rewriting process
	 */
	void incorporateDestructively(boolean splitterSign, Expression splitter, RewritingProcess process);

	/**
	 * Same as {@link #incorporateDestructively(boolean, Expression, RewritingProcess)} but taking
	 * a binary predicate to evaluate whether terms are directly implied disequal by the external context
	 * (this will probably be merged with the contextual constraint mechanism).
	 * Default implementation uses the former method while ignoring the extra parameter.
	 * @param splitterSign
	 * @param splitter
	 * @param disequalityDirectlyImpliedExternally
	 * @param process
	 */
	default void incorporateDestructively(boolean splitterSign, Expression splitter, BinaryPredicate<Expression, Expression> disequalityDirectlyImpliedExternally, RewritingProcess process) {
		incorporateDestructively(splitterSign, splitter, process);
	}

	/**
	 * Given a function mapping each term either to itself or to another term meant to represent it
	 * (determined, most likely, by a system of equalities somewhere),
	 * apply it to the present constraint, possibly destructively if that means much better performance.
	 * Terms with distinct representatives should not appear in the resulting constraint.
	 * @param getRepresentative
	 * @param process
	 */
	void updateRepresentativesDestructively(Function<Expression, Expression> getRepresentative, RewritingProcess process);
	// TODO: change this method to take explicit list of which terms to update, so that implementations do not need to check each of the terms they use.

	/**
	 * Provides a splitter, not already explicitly represented by the constraint,
	 * toward a state in which the model count for the given subset of indices
	 * can be computed (the model count may be condition on the other variables),
	 * or returns null if it already is in such a state.
	 */
	Expression pickSplitter(Collection<Expression> indicesSubSet, RewritingProcess process);

	/**
	 * Similar to {@link #pickSplitter(Collection, RewritingProcess)}, but taking
	 * an extra binary function argument determining directly implied disequalities by an external source;
	 * default implementation will ignore it; this will probably be merged into the contextual constraint mechanism.
	 * @param indicesSubSet
	 * @param disequalityDirectlyImpliedExternally
	 * @param process
	 * @return
	 */
	default Expression pickSplitter(Collection<Expression> indicesSubSet, BinaryPredicate<Expression, Expression> disequalityDirectlyImpliedExternally, RewritingProcess process) {
		return pickSplitter(indicesSubSet, process);
	}

	/**
	 * Computes model count for constraint, given a sub-set of indices, in polynomial time.
	 * Assumes that {@link #pickSplitter(Collection, RewritingProcess)} returns <code>null</code>,
	 * that is, the constraint is in such a state and context that allows the determination of a unique model count.
	 * The model count is required to contain no literals implied by the contextual constraint.
	 */
	Expression modelCount(Collection<Expression> indicesSubSet, RewritingProcess process);

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
		Constraint result = ExpressionConstraint.wrap(getTheory(), getSupportedIndices(), resultExpression);
		return result;
	}

	/**
	 * Indicates whether a given literal is directly implied by this constraint
	 * (that is, it's value is either trivial or explicitly represented by the constraint, without inference);
	 * implementations are allowed to assume that the literal is already simplified,
	 * and that it is not a trivial TRUE or FALSE. 
	 * @param literal
	 * @param process
	 * @return
	 */
	boolean directlyImpliesNonTrivialLiteral(Expression literal, RewritingProcess process);

	/**
	 * Indicates whether a given literal is directly implied by this constraint
	 * (that is, it's value is either trivial or explicitly represented by the constraint, without inference);
	 * default implementation simplifies it according to constraintTheory and asks {@link #directlyImpliesNonTrivialLiteral(Expression, RewritingProcess)}.
	 * @param literal
	 * @param process
	 * @return
	 */
	default boolean directlyImpliesLiteral(Expression literal, RewritingProcess process) {
		boolean result;
		Expression simplifiedLiteral = getTheory().simplify(literal, process);
		if (simplifiedLiteral.getSyntacticFormType().equals("Symbol")) {
			result = simplifiedLiteral.equals(TRUE);
		}
		else {
			result = directlyImpliesNonTrivialLiteral(simplifiedLiteral, process);
		}
		return result;
	}

	/**
	 * Simplifies a given splitter to true if implied by constraint, false if its negation is implied by constraint,
	 * or a version of itself with terms replaced by representatives.
	 * This is similar to {@link #directlyImpliesLiteral(Expression, RewritingProcess)} but potentially more efficient
	 * because a splitter is a restricted type of literal.
	 */
	Expression normalizeSplitterGivenConstraint(Expression splitter, RewritingProcess process);

	/**
	 * Receives an expression without literals and returns an equivalent one according to some normalization property
	 * For example, an implementation involving equality may choose to always represent all symbols in an equality cluster
	 * by the same symbol. 
	 * @param expression
	 * @param process
	 * @return
	 */
	Expression normalizeExpressionWithoutLiterals(Expression expression, RewritingProcess process);
}