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
import static com.sri.ai.expresso.helper.Expressions.parse;
import static com.sri.ai.util.Util.pickUniformly;

import java.util.Collection;
import java.util.Random;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.Type;
import com.sri.ai.grinder.sgdpllt.api.Context;
import com.sri.ai.grinder.sgdpllt.core.constraint.AbstractTheoryTestingSupport;

@Beta
abstract public class AbstractTheoryWithBinaryAtomsTestingSupport extends AbstractTheoryTestingSupport {

	public AbstractTheoryWithBinaryAtomsTestingSupport(AbstractTheoryWithBinaryAtoms theory, Random random) {
		super(theory, random);
	}
	
	@Override
	public AbstractTheoryWithBinaryAtoms getTheory() {
		return (AbstractTheoryWithBinaryAtoms) super.getTheory();
	}
	
	/**
	 * Makes a random atom by uniformly picking among the theory functors and testing variables.
	 */
	@Override
	public Expression makeRandomAtomOn(String mainVariable, Context context) {
		String mainVariableName = getVariableName(mainVariable);
		Type mainType = getTestingVariableType(mainVariable);
				
		Expression otherTerm = parse(pickTestingVariableAtRandom(mainType, otherName -> !otherName.equals(mainVariableName)));
		
		String functor = pickUniformly(getTheoryFunctors(), getRandom());

		Expression mainVariableExpression = parse(mainVariable);
		Expression possiblyTrivialAtom =
				getRandom().nextBoolean()?
						apply(functor, mainVariableExpression, otherTerm) : apply(functor, otherTerm, mainVariableExpression);
						
		Expression result = getTheory().simplify(possiblyTrivialAtom, context);
				
		return result;
	}
	
	protected Collection<String> getTheoryFunctors() {
		return getTheory().theoryFunctors;
	}
}
