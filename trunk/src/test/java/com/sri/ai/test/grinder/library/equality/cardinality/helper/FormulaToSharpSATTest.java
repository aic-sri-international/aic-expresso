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
package com.sri.ai.test.grinder.library.equality.cardinality.helper;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import org.junit.Assert;
import org.junit.Test;

import com.sri.ai.brewer.api.Grammar;
import com.sri.ai.brewer.core.CommonGrammar;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.library.DirectCardinalityComputationFactory;
import com.sri.ai.grinder.library.equality.cardinality.helper.FormulaToSharpSAT;
import com.sri.ai.test.grinder.AbstractGrinderTest;

public class FormulaToSharpSATTest  extends AbstractGrinderTest {
	
	@Override
	public Grammar makeGrammar() {
		return new CommonGrammar();
	}
	
	@Test
	public void test_BasicConversion() {
		RewritingProcess process = newProcess();

		SharpSATConversion conversionListener = new SharpSATConversion();
		FormulaToSharpSAT.converToSharpSAT(parse("and(or(X = a, Y = b, Z = c))"), 3, process, conversionListener);
				
		assertBasicDomain(conversionListener, 1);
		//
		Assert.assertEquals(Arrays.asList(1, 5, 9), conversionListener.clauses.get(12));
		
		FormulaToSharpSAT.converToSharpSAT(parse("and(or(X != a, Y != b, Z != c))"), 3, process, conversionListener);
		
		assertBasicDomain(conversionListener, 1);
		//
		Assert.assertEquals(Arrays.asList(-1, -5, -9), conversionListener.clauses.get(12));
	}
	
	@Test
	public void test_numberConstantsSmallerThanDomain() {
// TODO		
	}
	
	//
	// PRIVATE
	//
	private void assertBasicDomain(SharpSATConversion conversionListener, int expectedFormulaClauses) {
		Assert.assertEquals(9, conversionListener.numberVariables);
		Assert.assertEquals(12+expectedFormulaClauses, conversionListener.clauses.size());
		//
		Assert.assertEquals(Arrays.asList(1, 2, 3), conversionListener.clauses.get(0));
		Assert.assertEquals(Arrays.asList(4, 5, 6), conversionListener.clauses.get(1));
		Assert.assertEquals(Arrays.asList(7, 8, 9), conversionListener.clauses.get(2));
		//
		Assert.assertEquals(Arrays.asList(-1, -2), conversionListener.clauses.get(3));
		Assert.assertEquals(Arrays.asList(-1, -3), conversionListener.clauses.get(4));
		Assert.assertEquals(Arrays.asList(-2, -3), conversionListener.clauses.get(5));
		Assert.assertEquals(Arrays.asList(-4, -5), conversionListener.clauses.get(6));
		Assert.assertEquals(Arrays.asList(-4, -6), conversionListener.clauses.get(7));
		Assert.assertEquals(Arrays.asList(-5, -6), conversionListener.clauses.get(8));
		Assert.assertEquals(Arrays.asList(-7, -8), conversionListener.clauses.get(9));
		Assert.assertEquals(Arrays.asList(-7, -9), conversionListener.clauses.get(10));
		Assert.assertEquals(Arrays.asList(-8, -9), conversionListener.clauses.get(11));
	}
	
	private class SharpSATConversion implements FormulaToSharpSAT.ConversionListener {
		public int                 numberVariables = 0;
		public List<List<Integer>> clauses         = new ArrayList<List<Integer>>();
		
		@Override
		public void start(int numberVariables) {
			clauses.clear();
			this.numberVariables = numberVariables;
		}
		
		@Override
		public void clause(int[] clause) {
			List<Integer> lClause = new ArrayList<Integer>();
			
			for (int i = 0; i < clause.length; i++) {
				lClause.add(clause[i]);
			}
			
			clauses.add(lClause);
		}
		
		@Override
		public void end() {
			
		}
	}
	
	private RewritingProcess newProcess() {
		return DirectCardinalityComputationFactory.newCardinalityProcess(Expressions.TRUE);
	}
}
