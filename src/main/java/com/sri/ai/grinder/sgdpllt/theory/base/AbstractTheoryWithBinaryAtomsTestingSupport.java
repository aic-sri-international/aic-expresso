/*
 * Copyright (c) 2016, SRI International
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

import static com.sri.ai.expresso.helper.Expressions.apply;
import static com.sri.ai.expresso.helper.Expressions.makeSymbol;
import static com.sri.ai.util.Util.pickUniformly;

import java.util.Collection;
import java.util.Map;
import java.util.Random;
import java.util.Set;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.Type;
import com.sri.ai.grinder.sgdpllt.api.Context;
import com.sri.ai.grinder.sgdpllt.core.constraint.AbstractTheoryTestingSupport;
import com.sri.ai.util.collect.PredicateIterator;

@Beta
abstract public class AbstractTheoryWithBinaryAtomsTestingSupport extends AbstractTheoryTestingSupport {

	public AbstractTheoryWithBinaryAtomsTestingSupport(AbstractTheoryWithBinaryAtoms theory) {
		super(theory);
	}
	
	@Override
	public AbstractTheoryWithBinaryAtoms getTheory() {
		return (AbstractTheoryWithBinaryAtoms) super.getTheory();
	}
	
	/**
	 * Makes a random atom by uniformly picking among the theory functors and testing variables.
	 */
	@Override
	public Expression makeRandomAtomOn(String variable, Random random, Context context) {
		Map<String, Type> variablesAndTypes = getVariableNamesAndTypesForTesting();
		Type type = variablesAndTypes.get(variable);
		Set<String> allVariables = variablesAndTypes.keySet();
		PredicateIterator<String> isNameOfOtherVariableOfSameTypeAsMainVariable =
				PredicateIterator.make(allVariables, s -> {
					Type typeOfOther = variablesAndTypes.get(s);
					return typeOfOther.equals(type) || typeOfOther.toString().equals(type.toString());	
				});
		Expression otherTerm;
		if (random.nextBoolean()) {
			otherTerm = makeSymbol(pickUniformly(isNameOfOtherVariableOfSameTypeAsMainVariable, random));
		}
		else {
			otherTerm = type.sampleUniquelyNamedConstant(random);
		}
		
		String functor = pickUniformly(getTheoryFunctors(), random);
		
		Expression possiblyTrivialAtom =
				random.nextBoolean()?
						apply(functor, variable, otherTerm) : apply(functor, otherTerm, variable);
						
		Expression result = getTheory().simplify(possiblyTrivialAtom, context);
				
		return result;
	}
	
	protected Collection<String> getTheoryFunctors() {
		return getTheory().theoryFunctors;
	}
}
