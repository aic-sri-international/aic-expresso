/*
 * Copyright (c) 2013, SRI International
 * All rights reserved.
 * Licensed under the The BSD 3-Clause License;
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at:
 * 
 * http://opensource.org/licenses/BSD-3-Clause
 * 
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 
 * Redistributions of source code must retain the above copyright
 * notice, this list of conditions and the following disclaimer.
 * 
 * Redistributions in binary form must reproduce the above copyright
 * notice, this list of conditions and the following disclaimer in the
 * documentation and/or other materials provided with the distribution.
 * 
 * Neither the name of the aic-expresso nor the names of its
 * contributors may be used to endorse or promote products derived from
 * this software without specific prior written permission.
 * 
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
 * FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
 * COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, 
 * INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES 
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
 * SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) 
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, 
 * STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) 
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED
 * OF THE POSSIBILITY OF SUCH DAMAGE.
 */
package com.sri.ai.grinder.sgdpll.theory.base;

import static com.sri.ai.expresso.helper.Expressions.apply;
import static com.sri.ai.expresso.helper.Expressions.makeSymbol;
import static com.sri.ai.grinder.helper.GrinderUtil.getTypeCardinality;
import static com.sri.ai.grinder.library.FunctorConstants.NOT;
import static com.sri.ai.util.Util.myAssert;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.Symbol;
import com.sri.ai.grinder.api.Context;
import com.sri.ai.grinder.helper.GrinderUtil;
import com.sri.ai.grinder.library.boole.Not;
import com.sri.ai.grinder.sgdpll.api.ConstraintTheory;
import com.sri.ai.grinder.sgdpll.core.constraint.AbstractSingleVariableConstraintWithDependentNormalizedAtoms;
import com.sri.ai.util.base.Pair;

/**
 * An abstract single-variable constraint solver providing
 * functionality for theories involving only
 * binary atoms. It assumes there are interaction among distinct normalized atoms.
 * <p>
 * To maximize the detection of inconsistency,
 * extensions can restrict the stored atoms to be applications of
 * just a subset of the possible <i>normal functors</code>.
 * For example, an inequalities theory may choose to represent all normalized atoms
 * as applications of <code>=</code>, <code><</code>, and <code><=</code>,
 * converting atoms of <code>!=</code>, <code>></code>, and <code>>=</code>
 * as negations of normalized atoms.
 * <p>
 * To achieve this, the extension must implement {@link #getNormalFunctors()}
 * to return a collection of the strings of the normal functors,
 * and {@link #getNegationFunctor(String)} to make a functor string to its negation
 * functor's string.
 * <p>
 * To ensure that only normalized atoms are stored in the constraint,
 * the negation of a non-normal function must always be a normal functor.
 * If the theory is such that it is not always possible to negate a non-normal functor to a normal one,
 * then the extension must include all possible functors in {@link #getNormalFunctors()}.
 * <p>
 * Extensions must also provide a way to <i>flip</i> functors with {@link #getFlipFunctor(String)}.
 * 
 * @author braz
 *
 */
@Beta
public abstract class AbstractSingleVariableConstraintWithBinaryAtoms extends AbstractSingleVariableConstraintWithDependentNormalizedAtoms {

	private static final long serialVersionUID = 1L;

	/**
	 * The collection of functors for normalized atoms in this type of constraint.
	 * @return the collection of functors for normalized atoms in this type of constraint.
	 */
	abstract protected Collection<String> getNormalFunctors();

	/**
	 * Maps the string a functor <code>f</code> to the string of a functor <code>g</code>
	 * such that <code>f(X,Y) = not g(X,Y)</code> (for example, <code><</code> should map to <code>>=</code>);
	 * make sure the negation of non-normal functors are always normal functors.
	 * @param functor
	 * @return
	 */
	abstract protected String getNegationFunctor(String functor);
	
	/**
	 * Maps the string a normal functor <code>f</code> to the string of a normal functor <code>g</code>
	 * such that <code>f(X,Y) = g(Y,X)</code> (for example, <code><</code> should map to <code>></code>);
	 * make sure normal functors always map to normal functors.
	 * @param functor
	 * @return
	 */
	abstract protected String getFlipFunctor(String functor);
	
	/**
	 * Given an atom, returns an equivalent atom in which the constraint's variable appears as either one of the arguments
	 * For example, if <code>X</code> is the constraint's variable and we have atom <code>Y + 1 < X + 2</code>,
	 * a valid return value is <code>Y - 1 < X</code>.
	 * @param atom
	 * @param process
	 * @return
	 */
	abstract protected Expression isolateVariable(Expression atom, Context process);

	public AbstractSingleVariableConstraintWithBinaryAtoms(Expression variable, ConstraintTheory constraintTheory) {
		super(variable, constraintTheory);
	}

	public AbstractSingleVariableConstraintWithBinaryAtoms(
			Expression variable,
			ArrayList<Expression> positiveNormalizedAtoms,
			ArrayList<Expression> negativeNormalizedAtoms,
			List<Expression> externalLiterals,
			ConstraintTheory constraintTheory) {
		
		super(variable, positiveNormalizedAtoms, negativeNormalizedAtoms, externalLiterals, constraintTheory);
	}

	public AbstractSingleVariableConstraintWithBinaryAtoms(AbstractSingleVariableConstraintWithBinaryAtoms other) {
		super(other);
	}

	@Override
	protected Pair<Boolean, Expression> fromLiteralOnVariableToSignAndNormalizedAtom(Expression variable, Expression literal, Context process) {
	
		Pair<Boolean, Expression> result;
	
		Pair<Boolean, Expression> signAndAtom = getEquivalentSignAndAtom(literal, process);
		boolean sign = signAndAtom.first;
		Expression atom = signAndAtom.second;
		
		Expression atomWithIsolatedVariable = isolateVariable(atom, process);
		
		Pair<Boolean, Expression> signAndNormalFunctor = getEquivalentSignAndNormalFunctor(sign, atomWithIsolatedVariable);
		sign = signAndNormalFunctor.first;
		Expression normalFunctor = signAndNormalFunctor.second;
		
		Expression normalizedAtom = makeNormalizedAtomOrUseOriginalIfAlreadyNormalized(normalFunctor, atomWithIsolatedVariable);
		
		result = Pair.make(sign, normalizedAtom);

		return result;
	}

	private Pair<Boolean, Expression> getEquivalentSignAndAtom(Expression literal, Context process) throws Error {
		boolean sign;
		Expression atom = getEquivalentAtomIfPossible(literal, process);
		if (atom == null) {
			// must be a negation (otherwise atom would be the same as literal)
			atom = literal.get(0);
			sign = false;
		}
		else {
			sign = true;
		}
		Pair<Boolean, Expression> signAndAtom = Pair.make(sign, atom);
		return signAndAtom;
	}

	/**
	 * If possible, returns an atom equivalent to literal.
	 * This is only not possible if literal is a negation <i>and</i>
	 * its functor has no negation function.
	 * @param literal
	 * @return an atom equivalent to literal, or null if there is no such equivalent literal.
	 * @throws Error if literal is not a valid literal for this theory
	 */
	private Expression getEquivalentAtomIfPossible(Expression literal, Context process) throws Error {
		Expression result;
		if (literal.hasFunctor(NOT)) {
			String negatedFunctor = getNegationFunctor(literal.get(0).getFunctor().toString());
			if (negatedFunctor == null) {
				result = null; // no equivalent atom
			}
			else {
				result = apply(negatedFunctor, literal.get(0).get(0), literal.get(0).get(1));
			}
		}
		else {
			result = literal; // literal is an atom already
		}
		return result;
	}

	/**
	 * Determine which normal functor to use, possibly negating functor if non-normal
	 * @param sign
	 * @param atom
	 * @return
	 */

	private Pair<Boolean, Expression> getEquivalentSignAndNormalFunctor(boolean sign, Expression atom) {
		Expression normalFunctor;
		Expression functor = atom.getFunctor();
		String functorString = functor.toString();
		if (getNormalFunctors().contains(functorString)) {
			normalFunctor = functor;
		}
		else {
			Symbol negatedFunctor = makeSymbol(getNegationFunctor(functorString));
			myAssert( () -> getNormalFunctors().contains(negatedFunctor), () -> getClass().getSimpleName() + ": requires negation of non-normal functors to be normal functors, but got non-normal '" + functor + "' which has non-normal negation '" + negatedFunctor + "'");
			normalFunctor = negatedFunctor;
			sign = !sign; // flip sign since we negated the functor
		}
		Pair<Boolean, Expression> signAndNormalFunctor = Pair.make(sign, normalFunctor);
		return signAndNormalFunctor;
	}

	/**
	 * Returns a normalized atom equivalent to <code>Expressions.apply(normalFunctor.getFunctor(), originalAtom.get(0), originalAtom.get(1))</code>.
	 * The method checks if the original atom is equivalent to the above in order to avoid creating a new object unnecessarily.
	 * If it is not, then a new atom is constructed.
	 * This will be <code>Expressions.apply(normalFunctor.getFunctor(), originalAtom.get(0), originalAtom.get(1))</code>
	 * if <code>originalAtom.get(0)</code> is the constraint's variable, since
	 * for an atom to be normal, the constraint's variable must be its first argument.
	 * If that is not the case, then we flip the functor (using {@link #getFlipFunctor(String)}
	 * and arguments in order to satisfy that normal property.
	 * @param normalFunctor
	 * @param originalAtom
	 * @return
	 */
	private Expression makeNormalizedAtomOrUseOriginalIfAlreadyNormalized(Expression normalFunctor, Expression originalAtom) {
		Expression normalizedAtom;
		
		boolean needToFlipArguments = ! originalAtom.get(0).equals(getVariable());
		if (!needToFlipArguments) {
			if (originalAtom.getFunctor().equals(normalFunctor)) {
				normalizedAtom = originalAtom; // already normalized
			}
			else {
				// arguments already in order but original functor is not normal, so use normal functor
				normalizedAtom = apply(normalFunctor, originalAtom.get(0), originalAtom.get(1));
			}
		}
		else {
			// need to flip arguments, so find flipNormalFunctor:
			String flipNormalFunctor = getFlipFunctor(normalFunctor.toString());
			normalizedAtom = apply(flipNormalFunctor, originalAtom.get(1), originalAtom.get(0));
		}
		return normalizedAtom;
	}

	@Override
	public Expression fromNormalizedAtomToItsNegationAsLiteral(Expression normalizedAtom) {
		Expression result;
		String functorString = normalizedAtom.getFunctor().toString();
		String negatedFunctorString = getNegationFunctor(functorString);
		if (negatedFunctorString != null) {
			result = apply(negatedFunctorString, normalizedAtom.get(0), normalizedAtom.get(1));
		}
		else {
			result = Not.make(normalizedAtom);
		}
		return result;
	}

	private long cachedIndexTypeSize = -1;

	/**
	 * Returns the size of the variable's type
	 * by using {@link GrinderUtil#getTypeCardinality(Expression, Context)}.
	 * @param process
	 * @return
	 */
	public long getVariableTypeSize(Context process) {
		if (cachedIndexTypeSize == -1) {
			cachedIndexTypeSize = getTypeCardinality(getVariable(), process);
		}
		return cachedIndexTypeSize;
	}

	private Expression cachedVariableType;
	
	/**
	 * Return an expression describing the variable's type.
	 * @param process
	 * @return
	 */
	public Expression getVariableTypeExpression(Context process) {
		if (cachedVariableType == null) {
			cachedVariableType = GrinderUtil.getType(getVariable(), process);
//			cachedVariableType = process.getContextualSymbolType(getVariable());
//			if (cachedVariableType == null) {
//				cachedVariableType = new DefaultSyntacticFunctionApplication(TYPE, getVariable());
//			}
		}
		return cachedVariableType;
	}
}