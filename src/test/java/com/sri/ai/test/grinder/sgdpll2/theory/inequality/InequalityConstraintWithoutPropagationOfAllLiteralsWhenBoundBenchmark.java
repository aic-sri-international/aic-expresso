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
package com.sri.ai.test.grinder.sgdpll2.theory.inequality;

import com.google.common.annotations.Beta;

@Beta
public class InequalityConstraintWithoutPropagationOfAllLiteralsWhenBoundBenchmark extends AbstractInequalityConstraintBenchmark {

	@Override
	protected boolean getPropagateAllLiteralsWhenVariableIsBound() {
		return false;
	}

	// on December 31, 2015:
	// testMaxForSingleVariableConstraints: too long to wait
	// testMultiVariableConstraints: 2.048 s
	// testSumForSingleVariableConstraints: too long to wait
	// testCompleteMultiVariableConstraints: 11.7 s
	// testSingleVariableConstraints: 0.454 s
	// testModelCountingForSingleVariableConstraints: 10.899 s

	// on January 5, 2016:
	// testMaxForSingleVariableConstraints: 19.337 s
	// testMultiVariableConstraints: 2.485 s
	// testSumForSingleVariableConstraints: 15.192 s
	// testCompleteMultiVariableConstraints: 1.4 s
	// testSingleVariableConstraints: 0.8 s
	// testModelCountingForSingleVariableConstraints: 0.864 s
	// as we can see, about 10 times improvement for model counting after better splitting order and state preservation
}
