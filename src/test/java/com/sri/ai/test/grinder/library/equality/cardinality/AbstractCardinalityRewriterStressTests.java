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
package com.sri.ai.test.grinder.library.equality.cardinality;

import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.atomic.AtomicLong;

import org.junit.Before;
import org.junit.Test;

import com.sri.ai.brewer.BrewerConfiguration;
import com.sri.ai.brewer.api.Grammar;
import com.sri.ai.brewer.core.CommonGrammar;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.GrinderConfiguration;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.test.grinder.AbstractGrinderTest;
import com.sri.ai.test.grinder.TestData;
import com.sri.ai.util.AICUtilConfiguration;
import com.sri.ai.util.base.Triple;
import com.sri.ai.util.concurrent.BranchAndMerge;

public abstract class AbstractCardinalityRewriterStressTests extends AbstractGrinderTest {
	
	public interface CardinalityRewriter {
		String getName();
		long call(String cardExpression, String expected);
	}
	
	public interface CardinalityRewriteProcessFactory {
		RewritingProcess newInstance(Expression rootExpression, RewritingProcess parentProcess);
	}
	
	public interface CardinalityStressTest {
		String getTitle();
		// Test Title, expression, average times it took for each rewriter stressed tested
		List<Triple<String, String, List<Long>>> stressTest(List<CardinalityRewriter> cardinalityRewriters);
	}

	//
	protected static final int				 _domainSize = 10;
	protected static final int               _numberRewritesToAverage = 3;
	protected static final CountsDeclaration _sharedCountsDeclaration = new CountsDeclaration(_domainSize);
	
	@Before
	public void setup() {
		// NOTE: All Times should be taken with all trace turned off
		// as this has a huge impact on these tests (due to the amount generated).
		// In addition, turn concurrency off in order to test the algorithms run synchronously be default.
		// i.e.:
		// -Dgrinder.display.tree.util.ui=false
		// -Dtrace.level=off
		// -Djustification.level=off
		// -Dsriutil.branch.and.merge.threading.enabled=false
		// Setting here explicitly so its not forgotten.
		GrinderConfiguration.setProperty(GrinderConfiguration.KEY_DISPLAY_TREE_UTIL_UI, "false");
		GrinderConfiguration.disableTrace();
		GrinderConfiguration.disableJustification();
		AICUtilConfiguration.setProperty(AICUtilConfiguration.KEY_BRANCH_AND_MERGE_THREADING_ENABLED, "false");
		BranchAndMerge.reset();
		// For convenience
		BrewerConfiguration.setProperty(BrewerConfiguration.KEY_OUTPUT_PARSING_TIME_INFO, "false");
	}
	
	@Override
	public Grammar makeGrammar() {
		return new CommonGrammar();
	}
	
	public List<CardinalityRewriter> makeCardinalityRewriters() {
		List<CardinalityRewriter> result = new ArrayList<CardinalityRewriter>();
		
		return result;
	}
	
	public List<CardinalityStressTest> makeCardinalityStressTests() {
		List<CardinalityStressTest> result = new ArrayList<CardinalityStressTest>();
		
		return result;
	}
	
	@Test
	public void stressTestCardinalityRewriters() {
		List<CardinalityRewriter>   cardinalityRewriters   = makeCardinalityRewriters();
		List<CardinalityStressTest> cardinalityStressTests = makeCardinalityStressTests();
		
		// Run all the cardinality stress tests and collect their results
		List<List<Triple<String, String, List<Long>>>> cstResults = new ArrayList<List<Triple<String, String, List<Long>>>>();
		for (CardinalityStressTest cst : cardinalityStressTests) {
			System.out.println("--- Running "+cst.getTitle());
			List<Triple<String, String, List<Long>>> results = cst.stressTest(cardinalityRewriters);
			cstResults.add(results);
			System.out.println("---");
		}
		List<AtomicLong> grandTotalTimes = new ArrayList<AtomicLong>();
		for (int i = 0; i < cardinalityRewriters.size(); i++) {
			grandTotalTimes.add(new AtomicLong(0L));
		}
		// Now output their results.
		for (int c = 0; c < cstResults.size(); c++) {
			CardinalityStressTest cst = cardinalityStressTests.get(c);
			List<Triple<String, String, List<Long>>> results = cstResults.get(c);
			
			System.out.println("===");
			System.out.println(cst.getTitle());
			System.out.println("---");
			List<AtomicLong> totalTimes = new ArrayList<AtomicLong>();
			for (int i = 0; i < cardinalityRewriters.size(); i++) {
				if (i > 0) {
					System.out.print(", ");
				}
				System.out.print(cardinalityRewriters.get(i).getName());
				totalTimes.add(new AtomicLong(0L));
			}
			System.out.println("");
			System.out.println("---");
			for (Triple<String, String, List<Long>> testResult : results) {
				System.out.print(testResult.first);
				System.out.print(" ");
				for (int i = 0; i < testResult.third.size(); i++) {
					if (i > 0) {
						System.out.print(", ");
					}
					long time = testResult.third.get(i);
					System.out.print(time);
					totalTimes.get(i).addAndGet(time);
					grandTotalTimes.get(i).addAndGet(time);
				}
				System.out.println("");
			}
			outputTotals(cardinalityRewriters, "Stress Test Suite", totalTimes);
		}
		outputTotals(cardinalityRewriters, "Grand", grandTotalTimes);
		
		//
		// Now output the Excel Spreadsheet output
		// Now output their results.
		System.out.println("");
		System.out.println("EXCEL SPREADSHEET OUTUT");
		System.out.println("=======================");
		System.out.print("\n\nExpression");
		for (int i = 0; i < cardinalityRewriters.size(); i++) {
			System.out.print(", ");
			System.out.print("\""+cardinalityRewriters.get(i).getName()+"\"");
		}
		System.out.println("");
		for (int c = 0; c < cstResults.size(); c++) {
			List<Triple<String, String, List<Long>>> results = cstResults.get(c);
			for (Triple<String, String, List<Long>> testResult : results) {
				System.out.print("\""+testResult.second+"\"");
				for (int i = 0; i < testResult.third.size(); i++) {
					System.out.print(", ");
					System.out.print(testResult.third.get(i));
				}
				System.out.println("");
			}
		}
	}
	
	//
	// PROTECTED METHODS
	//
	protected class DefaultCardinalityRewriter extends TestData implements CardinalityRewriter {
		private String name;
		private CardinalityRewriteProcessFactory factory;
		private String cardRewriterName;
		private CountsDeclaration countsDeclaration;
		private int numberRewritesToAverage;
		private String E;
		private Expression exprE;
		
		public DefaultCardinalityRewriter(String name, CardinalityRewriteProcessFactory factory, String cardRewriterName,
				CountsDeclaration countsDeclaration, int numberRewritesToAverage) {
			super(false, IGNORE_EXPECTED);
			this.name = name;
			this.factory = factory;
			this.cardRewriterName = cardRewriterName;
			this.countsDeclaration = countsDeclaration;
			if (numberRewritesToAverage < 1) {
				throw new IllegalArgumentException("numberRewritesToAverage must be >= 1");
			}
			this.numberRewritesToAverage = numberRewritesToAverage;
		}
		
		@Override
		public String getName() {
			return name;
		}
		
		@Override
		public long call(String cardExpression, String expected) {
			this.expected = expected;
			E = cardExpression;
			long average = 0L; 
			
			for (int i = 0; i < numberRewritesToAverage; i++) {
				average += perform(new TestData[] {this});
			}
			average = average / numberRewritesToAverage;			
			
			return average;
		}
		
		@Override
		public Expression getTopExpression() {
			this.exprE = parse(E);
			
			return exprE;
		}
		
		@Override
		public Expression callRewrite(RewritingProcess process) {
			
			countsDeclaration.setup(process);
			
			Expression result = factory.newInstance(exprE, process).rewrite(cardRewriterName, exprE);
			
			return result;
		}
	}
	
	protected void outputTotals(List<CardinalityRewriter> cardinalityRewriters, String title, List<AtomicLong> totalTimes) {
		System.out.print("--- "+title+" Totals: ");
		for (int i = 0; i < cardinalityRewriters.size(); i++) {
			if (i > 0) {
				System.out.print(", ");
			}
			System.out.print(cardinalityRewriters.get(i).getName());
		}
		System.out.println(" ---");
		for (int i = 0; i < totalTimes.size(); i++) {
			if (i > 0) {
				System.out.print(", ");
			}
			System.out.print(totalTimes.get(i));
		}
		System.out.println("\n===\n");
	}
}

