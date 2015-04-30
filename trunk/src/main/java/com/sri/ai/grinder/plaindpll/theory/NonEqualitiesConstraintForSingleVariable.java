package com.sri.ai.grinder.plaindpll.theory;

import java.util.Collection;
import java.util.List;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.plaindpll.api.Constraint;

/** 
 * Represents a conjunction of literals of binary constraint operators between a variable and
 * a set of terms such that distinct terms.
 * This interface is defined for constraints used inside an EqualityTheoryConstraint
 * in addition to equalities themselves.
 * This is a pretty-specific, performance-based interface meant to be used inside {@link NonEqualitiesConstraint} implementations.
 * It does not behave like a stand-alone {@link Constraint} implementation because its picking and incorporation of splitters
 * are done by alternative methods that take the containing (enveloping constraint).
 * The original methods should throw an error if invoked.
 */	
public interface NonEqualitiesConstraintForSingleVariable extends Constraint {

	/** Returns the single variable against which all other terms are constrained. */
	Expression getVariable();

	NonEqualitiesConstraintForSingleVariable clone();
	
	/**
	 * Similar to {@link #pickSplitter(Collection, RewritingProcess)}, but taking
	 * an extra binary function argument determining directly implied disequalities by an external source;
	 * default implementation will ignore it; this will probably be merged into the contextual constraint mechanism.
	 * @param indicesSubSet
	 * @param disequalityDirectlyImpliedExternally
	 * @param process
	 * @return
	 */
	Expression pickSplitterGivenExternalConstraint(Collection<Expression> indicesSubSet, Constraint externalConstraint, RewritingProcess process);

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
	void incorporateDestructively(boolean splitterSign, Expression splitter, Constraint externalConstraint, RewritingProcess process);

	/**
	 * A more efficient replacement for {@link #directlyImpliesLiteral(Expression, RewritingProcess)} for disequality literals.
	 * @param term1
	 * @param term
	 * @param process
	 * @return
	 */
	boolean directlyImpliesDisequalityOfVariableAnd(Expression term, RewritingProcess process);

	/**
	 * Assumes the main variable is a free variable, and returns splitters on it required to hold for this constraint to hold.
	 * @return
	 */
	List<Expression> getSplittersToBeSatisfied();
	
	/**
	 * Assumes the main variable is a free variable,
	 * and returns splitters it the negations of which are required to hold for this constraint to hold.
	 * @return
	 */
	List<Expression> getSplittersToBeNotSatisfied();
}