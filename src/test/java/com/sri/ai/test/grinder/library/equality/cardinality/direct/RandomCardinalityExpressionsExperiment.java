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
import com.sri.ai.grinder.library.equality.cardinality.direct.core.Cardinality;
import com.sri.ai.test.grinder.library.equality.cardinality.CountsDeclaration;
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
	
	public static int numberOfRunsForAveraging = 2;
	
	@SuppressWarnings("unchecked")
	public static void performanceByNumberOfVariables() {
		
		GrinderUtil.setMinimumOutputForProfiling();
		
		Experiment.experiment(
				new Dimension("algorithm", Util.list(new Cardinality(new CountsDeclaration(10)), new Cardinality(new CountsDeclaration(20)))),
				
				"ylabel", "Time (ms)",
				new Dimension("depth, number of variables, number of constants", 1, 4, 1),

				computeTimeForSolvingCardinalityExpressionsBatch,
				
				new DataSeriesSpec("algorithm", Util.list(
						Util.list("title 'Direct 1'", "w linespoints"),
						Util.list("title 'Direct 2'", "w linespoints"))));
	}
	
	private static DAEFunction computeTimeForSolvingCardinalityExpressionsBatch = new DAEFunction() {

		@SuppressWarnings("unchecked")
		@Override
		public Object apply(DependencyAwareEnvironment environment) {
			Rewriter rewriter = (Rewriter) environment.get("algorithm");
			final List<Expression> cardinalityExpressions = (List<Expression>) environment.getResultOrRecompute(sampleCardinalityExpressions);
			Iterator<Expression> cardinalityExpressionsIterator = cardinalityExpressions.iterator();
			long totalTime = 0;
			for (int i = 0; i != numberOfRunsForAveraging; i++) {
				Expression cardinalityExpression = cardinalityExpressionsIterator.next();
				System.out.println(cardinalityExpression);
				long start = System.currentTimeMillis();
				Expression cardinality = rewriter.rewrite(cardinalityExpression);
				final long time = System.currentTimeMillis() - start;
				totalTime += time;
				System.out.println("->\n" + cardinality + "\n(" + time + " ms)\n");
			}
			long average = totalTime / numberOfRunsForAveraging;	
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
			int size = environment.getInt("depth, number of variables, number of constants");
			System.out.println("Size: " + size);	
			Iterator<Expression> cardinalityExpressionsIterator = new RandomCardinalityProblemGenerator(new Random(), size, size, size, 3);
			List<Expression> cardinalityExpressions = Util.listFrom(new FirstNIterator<Expression>(numberOfRunsForAveraging, cardinalityExpressionsIterator));
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
