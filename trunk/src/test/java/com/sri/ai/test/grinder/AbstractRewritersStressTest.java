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
package com.sri.ai.test.grinder;

import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.atomic.AtomicLong;

import org.junit.Assert;
import org.junit.Assume;
import org.junit.Before;
import org.junit.Test;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.api.Rewriter;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.helper.GrinderUtil;
import com.sri.ai.util.base.Triple;

/**
 * A partial implementation of a type of test taking a list of rewriters and a list of expressions
 * representing problems to be solved by those rewriters.
 * <p>
 * It runs all rewriters on all tests, measuring their time over {@link #numberOfRewritesToAverage},
 * and outputs those times per problem to the standard output.
 * <p>
 * It leaves a few methods to be declared by extensions that implement the generation of
 * the rewriters, the problems, and the rewriting process per rewriter and problem.
 * 
 * @author braz
 */
@Beta
public abstract class AbstractRewritersStressTest extends AbstractGrinderTest {

	protected static int numberOfRewritesToAverage = 3;

	public abstract RewritingProcess makeRewritingProcessFor(Rewriter rewriter, Expression problemExpression);

	public abstract List<? extends Rewriter> makeRewriters();

	public abstract List<StressTestData> makeStressTestDataObjects();

	@Override
	public RewritingProcess makeRewritingProcess(Expression topExpression) {
		throw new Error("AbstractGrinderTest.makeRewritingProcess not used in AbstractStressTest because each rewriter may need a different type of rewriting process, so it uses Rewriter.makeRewritingProcess(Expression) instead");
	}

	public long call(Rewriter rewriter, Expression problemExpression, String expected, int numberRewritesToAverage) {
		
		RewritingProcess process = makeRewritingProcessFor(rewriter, problemExpression);
		
		Expression actual = null;
		long average = 0L; 
		for (int i = 0; i < numberRewritesToAverage; i++) {
			long start = System.currentTimeMillis();
			actual = rewriter.rewrite(problemExpression, process);
			long time = System.currentTimeMillis() - start;
			average += time;
		}
		average = average / numberRewritesToAverage;			
		
		if (expected != IGNORE_EXPECTED) {
			Assert.assertEquals(expected, actual.toString());
		}
		
		return average;
	}

	@Before
	public void ignoreTest() {
		Assume.assumeFalse("Stress Tests Ignored.", Boolean.getBoolean("ignore.stress.tests"));
	}

	@Before
	public void setup() {
		GrinderUtil.setMinimumOutputForProfiling();
	}

	@Test
	public void stressTestAllRewriters() {
		List<? extends Rewriter> rewriters = makeRewriters();
		List<StressTestData> stressTestDataObjects = makeStressTestDataObjects();
		
		// Run all the cardinality stress tests and collect their results
		List<List<Triple<String, String, List<Long>>>> stressTestResults = new ArrayList<List<Triple<String, String, List<Long>>>>();
		for (StressTestData stressTestData : stressTestDataObjects) {
			System.out.println("--- Running " + stressTestData.getTitle());
			List<Triple<String, String, List<Long>>> results = runAllRewritersOnStressTestDataObject(rewriters, stressTestData);
			stressTestResults.add(results);
			System.out.println("---");
		}
		
		outputResults(rewriters, stressTestDataObjects, stressTestResults);
	}

	protected List<Triple<String, String, List<Long>>> runAllRewritersOnStressTestDataObject(List<? extends Rewriter> rewriters, StressTestData testData) {
		List<Triple<String, String, List<Long>>> results = new ArrayList<Triple<String, String, List<Long>>>();
		for (int i = 0; i < testData.getProblemExpressions().size(); i++) {
			String problemExpressionString = testData.getProblemExpressions().get(i);
			String expected          = testData.getExpectedExpressions()[i];
	
			String resultTitle = String.format("Times for '%s' %s: ", testData.getTitle(), problemExpressionString);
			List<Long> times = new ArrayList<Long>();
			for (Rewriter rewriter : rewriters) {
				System.out.println("Calling " + rewriter.getName() + " on " + problemExpressionString);
				Expression problemExpression = parse(problemExpressionString);
				times.add(call(rewriter, problemExpression, expected, numberOfRewritesToAverage));
			}
			results.add(new Triple<String, String, List<Long>>(resultTitle, problemExpressionString, times));
		}
		
		return results;
	}

	public AbstractRewritersStressTest() {
		super();
	}

	protected void outputResults(List<? extends Rewriter> rewriters, List<StressTestData> stressTestDataObjects, List<List<Triple<String, String, List<Long>>>> stressTestResults) {
		List<AtomicLong> grandTotalTimes = new ArrayList<AtomicLong>();
		for (int i = 0; i < rewriters.size(); i++) {
			grandTotalTimes.add(new AtomicLong(0L));
		}
		// Now output their results.
		for (int testIndex = 0; testIndex < stressTestResults.size(); testIndex++) {
			StressTestData stressTestData = stressTestDataObjects.get(testIndex);
			List<Triple<String, String, List<Long>>> results = stressTestResults.get(testIndex);
			
			System.out.println("===");
			System.out.println(stressTestData.getTitle());
			System.out.println("---");
			List<AtomicLong> totalTimes = new ArrayList<AtomicLong>();
			for (int i = 0; i < rewriters.size(); i++) {
				if (i > 0) {
					System.out.print(", ");
				}
				System.out.print(rewriters.get(i).getName());
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
			outputTotals(rewriters, "Stress Test Suite", totalTimes);
		}
		outputTotals(rewriters, "Grand", grandTotalTimes);
		
		//
		// Now output the Excel Spreadsheet output
		System.out.println("");
		System.out.println("EXCEL SPREADSHEET OUTPUT");
		System.out.println("========================");
		System.out.print("\n\nExpression");
		for (int i = 0; i < rewriters.size(); i++) {
			System.out.print(", ");
			System.out.print("\"" + rewriters.get(i).getName() + "\"");
		}
		System.out.println("");
		for (int c = 0; c < stressTestResults.size(); c++) {
			List<Triple<String, String, List<Long>>> results = stressTestResults.get(c);
			for (Triple<String, String, List<Long>> testResult : results) {
				System.out.print("\"" + testResult.second + "\"");
				for (int i = 0; i < testResult.third.size(); i++) {
					System.out.print(", ");
					System.out.print(testResult.third.get(i));
				}
				System.out.println("");
			}
		}
	}

	protected void outputTotals(List<? extends Rewriter> rewriters, String title, List<AtomicLong> totalTimes) {
		System.out.print("--- " + title + " Totals: ");
		for (int i = 0; i < rewriters.size(); i++) {
			if (i > 0) {
				System.out.print(", ");
			}
			System.out.print(rewriters.get(i).getName());
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