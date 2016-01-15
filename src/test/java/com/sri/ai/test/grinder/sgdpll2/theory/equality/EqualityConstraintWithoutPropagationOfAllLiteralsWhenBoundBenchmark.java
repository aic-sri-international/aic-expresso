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
package com.sri.ai.test.grinder.sgdpll2.theory.equality;

import com.google.common.annotations.Beta;

@Beta
public class EqualityConstraintWithoutPropagationOfAllLiteralsWhenBoundBenchmark extends AbstractEqualityConstraintBenchmark {

	@Override
	protected boolean getPropagateAllLiteralsWhenVariableIsBound() {
		return false;
	}
	
	// before introducing context-specific evaluator step solver:
	// testMaxForSingleVariableConstraints: 24.358 s
	// testCompleteSatisfiabilitySpecialCases: 0.481 s
	// testSatisfiabilitySpecialCases: 0.002 s
	// testMultiVariableConstraints: 0.517 s
	// testSumForSingleVariableConstraints: 22.283 s
	// testCompleteMultiVariableConstraints: 0.132 s
	// testSingleVariableConstraints: 0.032 s
	// testModelCountingForSingleVariableConstraints: 0.271 s

	// on January 14, 2016: (after introducing context-specific evaluator step solver):
	// testMaxForSingleVariableConstraints: 4.228 s
	// testCompleteSatisfiabilitySpecialCases: 0.655 s
	// testSatisfiabilitySpecialCases: 0.001 s
	// testMultiVariableConstraints: 0.504 s
	// testSumForSingleVariableConstraints: 2.629 s
	// testCompleteMultiVariableConstraints: 0.195 s
	// testSingleVariableConstraints: 0.033 s
	// testModelCountingForSingleVariableConstraints: 0.304 s
}