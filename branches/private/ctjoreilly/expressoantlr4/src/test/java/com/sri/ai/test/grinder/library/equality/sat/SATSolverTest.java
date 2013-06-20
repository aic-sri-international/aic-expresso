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
package com.sri.ai.test.grinder.library.equality.sat;

import org.junit.Assert;
import org.junit.Test;

import com.google.common.base.Stopwatch;
import com.sri.ai.brewer.api.Grammar;
import com.sri.ai.brewer.core.CommonGrammar;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.library.DirectCardinalityComputationFactory;
import com.sri.ai.grinder.library.equality.sat.EqualityLogicSATSolver;
import com.sri.ai.grinder.library.equality.sat.SAT4JSolver;
import com.sri.ai.grinder.library.equality.sat.SATSolver;
import com.sri.ai.test.grinder.AbstractGrinderTest;

public class SATSolverTest extends AbstractGrinderTest {
	
	@Override
	public Grammar makeGrammar() {
		return new CommonGrammar();
	}
	
	public SATSolver[] newSATSolvers() {
		return new SATSolver[] {
				new SAT4JSolver(), 
				new EqualityLogicSATSolver(),
		};
	}
	
	@Test
	public void testSatisfiableBasic() {
		testSatisfiable("X = Y or X != Y");
		testSatisfiable("X = a and Y = a and X = Y");
		testSatisfiable("not (X != bob and X' != X => X' != bob)");
		testSatisfiable("not (X != bob and X != mary and X' != X => X' != bob)");
		testSatisfiable("not (X != bob and X != mary and X != john and X' != X => X' != bob)");
	}
	
	@Test
	public void testSatisfiableLargeDNF() {	
		testSatisfiable("X = w7 => not(X0 != Y and X0 != Z and Z != Y and (X0 = w7 and X = Y or X0 = w7 and X = Z))");
	}
	
	@Test
	public void testUnsatisfiableBasic() {
		testUnsatisfiable("X = a and Y = a and X = b");
		testUnsatisfiable("X = Y and X != Y");
		testUnsatisfiable("X = a and Y = a and X != Y");
		testUnsatisfiable("and(X = a, X = Y, Y != a)");
		testUnsatisfiable("(X = person1) <=> (X != person1)");
		testUnsatisfiable("(X = person1 or X = person2) <=> (X != person1 and X != person2)");
		testUnsatisfiable("(X = person1 or X = person2 or X = person3) <=> (X != person1 and X != person2 and X != person3)");
		testUnsatisfiable("(X = person1) and (X != person1)");
		testUnsatisfiable("(X = person1 or X = person2) and (X != person1 and X != person2)");
		testUnsatisfiable("(X = person1 or X = person2 or X = person3) and (X != person1 and X != person2 and X != person3)");
		
		// TODO - currently not supported.
		//testUnsatisfiable("for all X: (X = person1) => (X != person1)");
		//testUnsatisfiable("for all X: (X = person1 or X = person2) => (X != person1 and X != person2)");
		//testUnsatisfiable("for all X: (X = person1 or X = person2 or X = person3) => (X != person1 and X != person2 and X != person3)");
	}
	
	@Test
	public void testFoundFailingCase1() {
		testSatisfiable("not (X != w7 and X != X3 and X3 != w7 and X1 != X2 and X1 != X3 and X2 != X3 and (X2 != w7 or X1 != X) and not (X2 = w7) and not (X1 != X and X1 != w7) and not false and not (X2 = w7 and X1 = X) => X1 != w7 and false = 0)");
	}

	//
	// PRIVATE
	//
	private void testSatisfiable(String strFormula) {
		test(strFormula, true);
	}
	
	private void testUnsatisfiable(String strFormula) {
		test(strFormula, false);
	}
	
	private void test(String strFormula, boolean expectedSatisfiable) {  
		Expression       formula = parse(strFormula);
		
		for (SATSolver solver : newSATSolvers()) {
			RewritingProcess process = newProcess();
			System.out.println(solver.getName() + " to solve : "+formula);
			Stopwatch stopwatch = new Stopwatch().start();
			boolean satisfiable = solver.isSatisfiable(formula, process);
			long evaluationTime = stopwatch.elapsedMillis();
			System.out.println("- Satisfiable? ->");
			System.out.println(""+satisfiable+", solver ("+solver.getName()+") time: " + evaluationTime + " ms.");
			Assert.assertEquals(expectedSatisfiable, satisfiable);
		}
	}
	
	private RewritingProcess newProcess() {
		return DirectCardinalityComputationFactory.newCardinalityProcess(Expressions.TRUE);
	}
}
