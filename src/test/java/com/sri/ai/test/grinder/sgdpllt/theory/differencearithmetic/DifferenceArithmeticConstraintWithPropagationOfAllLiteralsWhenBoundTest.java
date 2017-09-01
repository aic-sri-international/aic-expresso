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
package com.sri.ai.test.grinder.sgdpllt.theory.differencearithmetic;

import static com.sri.ai.expresso.helper.Expressions.parse;
import static com.sri.ai.grinder.sgdpllt.core.solver.AbstractQuantifierEliminationStepSolver.BRUTE_FORCE_CHECKING_OF_NON_CONDITIONAL_PROBLEMS;
import static com.sri.ai.util.Util.println;

import org.junit.Test;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.sgdpllt.api.Context;
import com.sri.ai.grinder.sgdpllt.api.Theory;
import com.sri.ai.grinder.sgdpllt.core.TrueContext;
import com.sri.ai.grinder.sgdpllt.interpreter.BruteForceCommonInterpreter;
import com.sri.ai.grinder.sgdpllt.interpreter.BruteForceInterpreter;
import com.sri.ai.grinder.sgdpllt.theory.differencearithmetic.DifferenceArithmeticTheory;

@Beta
public class DifferenceArithmeticConstraintWithPropagationOfAllLiteralsWhenBoundTest extends AbstractDifferenceArithmeticConstraintTest {

	@Override
	protected boolean getPropagateAllLiteralsWhenVariableIsBound() {
		return true;
	}
	
	@Test
	public void debugginTests() {
		// Problems that revealed bugs in the past.
		// Running them in debug mode that compares not only final result, but all non-conditional sub-problem results.
		
		Expression problem;
		
		problem = parse("sum({{ ( on K in 0..4 ) sum({{ ( on J in 0..4 ) sum({{ ( on I in 0..4 ) if K + 4 >= 0 then if J < 1 then if I = K + 4 then 8 else 3 else if K >= J then 3 else 2 else if I <= 4 then if I = 3 then 4 else 1 else if J + 1 > 0 then 8 else 0 : (K > I) and (K > 0) and (K > J + -3) }}) }}) }})");
		debug(problem);
		
		problem = parse("sum({{ ( on K in 0..4 ) sum({{ ( on J in 0..4 ) sum({{ ( on I in 0..4 ) if J <= 0 then if K < J + 4 then if J <= I then 5 else 5 else if I > 0 then 6 else 2 else if K >= J then if J + 3 = 0 then 9 else 4 else if K = 0 then 7 else 7 : I <= K + -1 }}) }}) }})");
		debug(problem);
	}

	/**
	 * @param problem
	 */
	public void debug(Expression problem) {
		Theory theory = new DifferenceArithmeticTheory(true, true);
		
		Context context = new TrueContext(theory);
		
		context = context.putGlobalObject(BRUTE_FORCE_CHECKING_OF_NON_CONDITIONAL_PROBLEMS, "Yes");

		Expression symbolicSolution = theory.evaluate(problem, context);
		
		println(symbolicSolution);

		BruteForceInterpreter bruteForceInterpreter = new BruteForceCommonInterpreter();
		
		Expression bruteForceSolution = bruteForceInterpreter.apply(problem, context);
		
		println(bruteForceSolution);
	}
}