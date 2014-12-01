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
package com.sri.ai.test.grinder.library.equality.cardinality.direct;

import java.util.Iterator;
import java.util.List;
import java.util.Random;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.api.Rewriter;
import com.sri.ai.grinder.helper.GrinderUtil;
import com.sri.ai.grinder.library.equality.RandomCardinalityProblemGenerator;
import com.sri.ai.grinder.library.equality.cardinality.core.CountsDeclaration;
import com.sri.ai.grinder.library.equality.cardinality.direct.core.Cardinality;
import com.sri.ai.grinder.library.equality.cardinality.plaindpll.ModelCounting;
import com.sri.ai.grinder.library.equality.cardinality.plaindpll.EqualityOnSymbolsTheory;
import com.sri.ai.grinder.library.equality.cardinality.plaindpll.DPLLGeneralizedAndSymbolic;
import com.sri.ai.util.Util;
import com.sri.ai.util.collect.FirstNIterator;
import com.sri.ai.util.experiment.Experiment;
import com.sri.ai.util.experiment.Experiment.DataSeriesSpec;
import com.sri.ai.util.rangeoperation.api.DAEFunction;
import com.sri.ai.util.rangeoperation.api.DependencyAwareEnvironment;
import com.sri.ai.util.rangeoperation.library.rangeoperations.Dimension;

/**
 * Plots graphs showing performance of given cardinality rewriters on a set of random cardinality problems
 * with respect to some generating parameters such as depth, number of variables, etc.
 */
public class RandomCardinalityExpressionsExperiment {
	

	private static final String X_VARIABLE_NAME = "depth = number of variables = number of constants";

	public static int SEED = 1;
	public static int sizeOfDataset = 50;
	public static int minimumSize = 2;
	public static int maximumSize = 4;
	public static int numberOfRunsForAveraging = 10;
	public static boolean useFreeVariables = false;
	
	@SuppressWarnings("unchecked")
	public static void performanceByNumberOfVariables() {
		
		GrinderUtil.setMinimumOutputForProfiling();
		
		long start = System.currentTimeMillis();
		
		Experiment.experiment(
				new Dimension("algorithm",
						Util.list(
								new DPLLGeneralizedAndSymbolic(new EqualityOnSymbolsTheory(), new ModelCounting(), new CountsDeclaration(10)),
								new Cardinality(new CountsDeclaration(10))
								)),
				
				"ylabel", "Average time per problem (ms)",
				new Dimension(X_VARIABLE_NAME, minimumSize, maximumSize, 1),

				computeTimeForSolvingCardinalityExpressionsBatch,
				
				new DataSeriesSpec("algorithm", Util.list(
						Util.list("title 'Plain no   caching'", "w linespoints"),
						Util.list("title 'Direct'", "w linespoints")
						)));
		
		long total = System.currentTimeMillis() - start;
		
		System.out.println("Total time: " + total + " ms");
	}
	
	private static DAEFunction computeTimeForSolvingCardinalityExpressionsBatch = new DAEFunction() {

		@SuppressWarnings("unchecked")
		@Override
		public Object apply(DependencyAwareEnvironment environment) {
			int size = environment.getInt(X_VARIABLE_NAME);
			Rewriter rewriter = (Rewriter) environment.get("algorithm");
			final List<Expression> cardinalityExpressions = (List<Expression>) environment.getResultOrRecompute(sampleCardinalityExpressions);
			long totalTime = 0;
			int problemIndex = 1;
			for (Expression cardinalityExpression : cardinalityExpressions) {
				System.out.println(cardinalityExpression);
				Expression cardinality = null;
				long start = System.currentTimeMillis();
				for (int i = 0; i != numberOfRunsForAveraging; i++) {
					cardinality = rewriter.rewrite(cardinalityExpression);
				}
				final long time = System.currentTimeMillis() - start;
				totalTime += time;
				System.out.println("->\n" + cardinality + "\n(Size " + size + ", " + problemIndex + "-th problem, " + ((double)time)/numberOfRunsForAveraging + " ms, " + rewriter.getName() + ")\n");
				problemIndex++;
			}
			double average = ((double) totalTime) / numberOfRunsForAveraging / cardinalityExpressions.size();	
			return average;
		}

		@Override
		public boolean isRandom() {
			return true;
		}

	};
	
	private static DAEFunction sampleCardinalityExpressions = new DAEFunction() {

		@Override
		public Object apply(DependencyAwareEnvironment environment) {
			int size = environment.getInt(X_VARIABLE_NAME);
			System.out.println("Size: " + size);	
			int minimumNumberOfIndices = useFreeVariables? size/2 : size;
			Iterator<Expression> cardinalityExpressionsIterator = new RandomCardinalityProblemGenerator(new Random(SEED), size, size, minimumNumberOfIndices, size, 3);
			List<Expression> cardinalityExpressions = Util.listFrom(new FirstNIterator<Expression>(sizeOfDataset, cardinalityExpressionsIterator));
			return cardinalityExpressions;
		}

		@Override
		public boolean isRandom() {
			return true;
		}
	};
	
	public static void main(String[] args) {
		performanceByNumberOfVariables();
	}
}
