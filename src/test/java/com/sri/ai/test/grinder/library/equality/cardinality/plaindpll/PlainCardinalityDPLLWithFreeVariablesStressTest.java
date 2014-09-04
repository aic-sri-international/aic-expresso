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
package com.sri.ai.test.grinder.library.equality.cardinality.plaindpll;

import java.util.Iterator;
import java.util.List;
import java.util.Random;

import org.junit.Assume;
import org.junit.Before;
import org.junit.Test;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.api.Rewriter;
import com.sri.ai.grinder.helper.GrinderUtil;
import com.sri.ai.grinder.library.equality.RandomCardinalityProblemGenerator;
import com.sri.ai.grinder.library.equality.cardinality.core.CountsDeclaration;
import com.sri.ai.grinder.library.equality.cardinality.plaindpll.PlainCardinalityDPLLWithFreeVariables;
import com.sri.ai.util.Util;
import com.sri.ai.util.collect.FirstNIterator;

@Beta
/**
 * A generic stress test for cardinality.
 * It is mean for creating stress tests that will not change with time and serve as benchmarks.
 * If the need for new tests arise, they can be extended from this one and its parameter methods
 * can be overridden.
 * 
 * @author braz
 */
public class PlainCardinalityDPLLWithFreeVariablesStressTest {

	// These parameters are in method form so variants can be declared as extending classes.
	
	public boolean useFreeVariables()            { return true;  }
	public int     getRandomSeedForProblems()    { return 1;     }
	public int     getSizeOfDataset()            { return 50;    }
	public int     getMinimumSize()              { return 2;     }
	public int     getMaximumSize()              { return 7;     }
	public int     getNumberOfRunsForAveraging() { return 10;    }
	public boolean isConsoleOutput()             { return true;  }

	@Before
	public void ignoreTest() {
		Assume.assumeFalse("Stress Tests Ignored.", Boolean.getBoolean("ignore.stress.tests"));
	}
	
	@Test
	public void test() {
		
		GrinderUtil.setMinimumOutputForProfiling();

		Rewriter rewriter = new PlainCardinalityDPLLWithFreeVariables(new CountsDeclaration(10));
		
		long totalStart = System.currentTimeMillis();

		for (int size = getMinimumSize(); size <= getMaximumSize(); size++) { // <= used to match {@link RandomCardinalityProblemGenerator}
			int minimumNumberOfIndices = useFreeVariables()? size/2 : size;
			Iterator<Expression> cardinalityExpressionsIterator = new RandomCardinalityProblemGenerator(new Random(getRandomSeedForProblems()), size, size, minimumNumberOfIndices, size, 3);
			List<Expression> cardinalityExpressions = Util.listFrom(new FirstNIterator<Expression>(getSizeOfDataset(), cardinalityExpressionsIterator));
			int problemIndex = 1;
			for (Expression cardinalityExpression : cardinalityExpressions) {
				if (isConsoleOutput()) {
					System.out.println(cardinalityExpression);
				}
				Expression cardinality = null;
				long start = System.currentTimeMillis();
				for (int i = 0; i != getNumberOfRunsForAveraging(); i++) {
					cardinality = rewriter.rewrite(cardinalityExpression);
				}
				final long time = System.currentTimeMillis() - start;
				if (isConsoleOutput()) {
					System.out.println("->\n" + cardinality + "\n(Size " + size + ", " + problemIndex + "-th problem, " + ((double)time)/getNumberOfRunsForAveraging() + " ms, " + rewriter.getName() + ")\n");
				}
				problemIndex++;
			}
		}
		
		long total = System.currentTimeMillis() - totalStart;
		
		System.out.println("Total time: " + total + " ms");
	}
}
