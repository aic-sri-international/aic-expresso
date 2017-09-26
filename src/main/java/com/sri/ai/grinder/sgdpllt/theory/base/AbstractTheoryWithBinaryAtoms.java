package com.sri.ai.grinder.sgdpllt.theory.base;

import java.util.Collection;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.sgdpllt.api.Context;
import com.sri.ai.grinder.sgdpllt.core.constraint.AbstractTheory;

public abstract class AbstractTheoryWithBinaryAtoms extends AbstractTheory {

	/**
	 * Indicates whether the theory can safely assume that all applications of its theory functors are atoms in it,
	 * regardless of their argument types (this spares the theory to do the checking).
	 */
	protected boolean atomFunctorsAreUniqueToThisTheory;

	/**
	 * The strings of the theory functors
	 */
	Collection<String> atomFunctors; // NOTE: package protected so AbstractTheoryWithBinaryAtomsTestingSupport can access

	/**
	 * Constructor taking the theory's functor strings,
	 * a boolean indicating whether any application of this functors, regardless of their arguments types,
	 * are to be considered atoms in this theory (for efficiency purposes),
	 * and a simplifier for these functions.
	 * @param atomFunctors
	 * @param atomFunctorsAreUniqueToThisTheory
	 * @param simplifier
	 */
	public AbstractTheoryWithBinaryAtoms(
			Collection<String> atomFunctors,
			boolean atomFunctorsAreUniqueToThisTheory) {
		super();
		this.atomFunctorsAreUniqueToThisTheory = atomFunctorsAreUniqueToThisTheory;
		this.atomFunctors = atomFunctors;
	}

	protected abstract boolean applicationOfAtomFunctorIsIndeedAtom(Expression applicationOfAtomFunctor, Context context);

	/**
	 * Indicates whether an expression is a function application of one of the theory functors.
	 * @param expression
	 * @return
	 */
	protected boolean isApplicationOfAtomFunctor(Expression expression) {
		boolean result = expression.getFunctor() != null && atomFunctors.contains(expression.getFunctor().toString());
		return result;
	}

	@Override
	public boolean isInterpretedInThisTheoryBesidesBooleanConnectives(Expression expression) {
		boolean result = isApplicationOfAtomFunctor(expression) || atomFunctors.contains(expression.toString()); 
		return result;
	}

	/**
	 * Implements decision of whether an expression is an atom by checking
	 * if it is a function application of one of the atom functors and,
	 * if so, delegating the decision to {@link #applicationOfAtomFunctorIsIndeedAtom(Expression, Context)}.
	 */
	@Override
	public boolean isAtom(Expression expression, Context context) {
		boolean result;
	
		boolean isApplicationOfAtomFunctor = isApplicationOfAtomFunctor(expression);

		if ( ! isApplicationOfAtomFunctor) {
			result = false;
		}
		else {
			result = applicationOfAtomFunctorIsToBeConsideredAtom(expression, context);
		}
		
		return result;
	}

	/**
	 * Decides whether application of atom functor is to be considered an atom.
	 * The decision is true if atom functors are unique to this theory,
	 * and delegated to {@link #applicationOfAtomFunctorIsIndeedAtom(Expression, Context)} otherwise.
	 * @param applicationOfAtomFunctor
	 * @param context
	 * @return
	 */
	protected boolean applicationOfAtomFunctorIsToBeConsideredAtom(Expression applicationOfAtomFunctor, Context context) {
		boolean result;
		if (atomFunctorsAreUniqueToThisTheory) {
			result = true;
		}
		else {
			result = applicationOfAtomFunctorIsIndeedAtom(applicationOfAtomFunctor, context);
		}
		return result;
	}
}