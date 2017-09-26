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
package com.sri.ai.grinder.sgdpllt.theory.base;

import static com.sri.ai.grinder.sgdpllt.library.FunctorConstants.DISEQUALITY;
import static com.sri.ai.grinder.sgdpllt.library.FunctorConstants.EQUALITY;
import static com.sri.ai.grinder.sgdpllt.library.boole.Not.not;

import java.util.Collection;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.grinder.sgdpllt.api.Context;
import com.sri.ai.grinder.sgdpllt.api.Theory;


/** 
 * A {@link Theory} for constraint theories with binary atoms, including equality.
 * This provides a property for turning off propagation of all literals at single-variable constraint level
 * once the variable is bound.
 * 
 * @author braz
 */
@Beta
abstract public class AbstractTheoryWithBinaryAtomsIncludingEquality extends AbstractTheoryWithBinaryAtoms {

	private boolean propagateAllLiteralsWhenVariableIsBound;

	/**
	 * Constructor
	 * @param theoryFunctorsIncludingEqualityAndInequality must include equality and inequality
	 * @param atomFunctorsAreUniqueToThisTheory indicates whether this is the only theory using these functors (for efficiency)
	 * @param propagateAllLiteralsWhenVariableIsBound indicates automatic propagation of value once variable gets bound.
	 */
	public AbstractTheoryWithBinaryAtomsIncludingEquality(
			Collection<String> theoryFunctorsIncludingEqualityAndInequality,
			boolean atomFunctorsAreUniqueToThisTheory,
			boolean propagateAllLiteralsWhenVariableIsBound) {

		super(theoryFunctorsIncludingEqualityAndInequality, atomFunctorsAreUniqueToThisTheory);
		this.propagateAllLiteralsWhenVariableIsBound = propagateAllLiteralsWhenVariableIsBound;
	}

	public boolean getPropagateAllLiteralsWhenVariableIsBound() {
		return propagateAllLiteralsWhenVariableIsBound;
	}

	/**
	 * Inverts <code>=</code> to <code>!=</code> and vice-versa,
	 * and adds <code>not</code> to other atoms.
	 */
	@Override
	public Expression getAtomNegation(Expression atom, Context context) {
		Expression result;
		if (atom.hasFunctor(EQUALITY)) {
			result = Expressions.apply(DISEQUALITY, atom.get(0), atom.get(1));
		}
		else if (atom.hasFunctor(DISEQUALITY)) {
			result = Expressions.apply(EQUALITY, atom.get(0), atom.get(1));
		}
		else {
			result = not(atom);
		}
		return result;
	}
}