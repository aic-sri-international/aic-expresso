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
package com.sri.ai.grinder.sgdpllt.theory.linearrealarithmetic;

import static com.sri.ai.util.Util.map;

import java.util.Random;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.type.RealInterval;
import com.sri.ai.grinder.sgdpllt.api.Context;
import com.sri.ai.grinder.sgdpllt.tester.TheoryTestingSupport;
import com.sri.ai.grinder.sgdpllt.theory.base.AbstractTheoryWithBinaryAtomsTestingSupport;

@Beta
public class LinearRealArithmeticTheoryTestingSupport extends AbstractTheoryWithBinaryAtomsTestingSupport {
	public static final RealInterval TESTING_REAL_INTERVAL_TYPE = new RealInterval("[0;4]"); 
	
	public LinearRealArithmeticTheoryTestingSupport(LinearRealArithmeticTheory theory, Random random, boolean generalizedVariableSupportEnabled) {
		super(theory, random, generalizedVariableSupportEnabled);
		setVariableNamesAndTypesForTesting(map("X", TESTING_REAL_INTERVAL_TYPE, "Y", TESTING_REAL_INTERVAL_TYPE, "Z", TESTING_REAL_INTERVAL_TYPE, 
				"unary_lra/1", TESTING_REAL_INTERVAL_TYPE, "binary_lra/2", TESTING_REAL_INTERVAL_TYPE));
	}
	
	/**
	 * Makes a linear real arithmetic random atom on variable.
	 * Currently unimplemented, throwing an Error instead
	 * (generating random atoms is not as useful for this theory,
	 * since it cannot be tested by brute force).
	 */
	@Override
	public Expression makeRandomAtomOn(String variable, Context context, TheoryTestingSupport globalTheoryTestingSupport) {
		// TODO: write this method
		throw new Error("Random generation of linear real arithmetic not yet implemented.");
	}
}
