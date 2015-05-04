package com.sri.ai.grinder.plaindpll.theory;

import java.util.Collection;
import java.util.List;

import com.google.common.base.Function;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.plaindpll.api.Constraint;

/** 
 * Represents a conjunction of literals of binary constraint operators between a variable and
 * a set of terms such that distinct terms.
 * This interface is defined for constraints used inside an EqualityConstraintTheoryConstraint
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
	 * an external constraint setting an additional context
	 * default implementation will ignore it; this will probably be merged into the contextual constraint mechanism.
	 * @param indicesSubSet
	 * @param disequalityDirectlyImpliedExternally
	 * @param process
	 * @return
	 */
	Expression pickSplitterGivenExternalConstraint(Collection<Expression> indicesSubSet, Constraint externalConstraint, RewritingProcess process);

	/**
	 * Same as {@link #incorporateDestructively(boolean, Expression, RewritingProcess)} but taking
	 * an external constraint setting an additional context
	 * (this will probably be merged with the contextual constraint mechanism)
	 * and which may be <i>updated</i> with information that this constraint itself cannot represent
	 * (this will probably be replaced by a constraint propagation scheme later).
	 * Default implementation uses the former method while ignoring the extra parameter.
	 * @param splitterSign
	 * @param splitter
	 * @param externalConstraint
	 * @param process
	 */
	void incorporateDestructively(boolean splitterSign, Expression splitter, Constraint externalConstraint, RewritingProcess process);

	/**
	 * Same as {@link #updateRepresentativesDestructively(boolean, Expression, RewritingProcess)} but taking
	 * an external constraint that is <i>updated</i> with information that this constraint itself cannot represent.
	 * (this will probably be replaced by a constraint propagation scheme later).
	 * Default implementation uses the former method while ignoring the extra parameter.
	 * @param getRepresentative
	 * @param externalConstraint
	 * @param process
	 */
	void updateRepresentativesDestructively(Function<Expression, Expression> getRepresentative, NonEqualitiesConstraint externalConstraint, RewritingProcess process);

	/**
	 * Makes simplifications and adjustments, possibly throwing a {@link Contradiction},
	 * whenever a given literal is being stored in <i>another</i> single-variable constraint
	 * stored in the same {@link NonEqualitiesConstraint}.
	 * @param literal
	 * @param process
	 */
	void informDestructively(Expression literal, RewritingProcess process);
	
	/**
	 * A more efficient replacement for {@link #directlyImpliesLiteral(Expression, RewritingProcess)} for disequality literals.
	 * @param term1
	 * @param term
	 * @param process
	 * @return
	 */
	boolean directlyImpliesDisequalityOfVariableAnd(Expression term, RewritingProcess process);

	/**
	 * Provides the positive splitters implied by this constraint
	 * (these, conjuncted with the negations of the ones provided by {@link #getSplittersToBeNotSatisfied()},
	 * should be equivalent to the constraint).
	 * @return
	 */
	List<Expression> getSplittersToBeSatisfied();
	
	/**
	 * Provides the negative splitters implied by this constraint
	 * (the negation of these, conjuncted with the ones provided by {@link #getSplittersToBeSatisfied()},
	 * should be equivalent to the constraint).
	 * @return
	 */
	List<Expression> getSplittersToBeNotSatisfied();
}