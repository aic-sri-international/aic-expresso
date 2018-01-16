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
package com.sri.ai.grinder.interpreter;

import static com.sri.ai.expresso.helper.Expressions.ONE;
import static com.sri.ai.expresso.helper.Expressions.makeSymbol;
import static com.sri.ai.util.Util.myAssert;
import static com.sri.ai.util.collect.FunctionIterator.functionIterator;

import java.util.Random;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.Symbol;
import com.sri.ai.grinder.api.Context;
import com.sri.ai.grinder.api.MultiQuantifierEliminationProblem;
import com.sri.ai.grinder.core.solver.MeasurableMultiQuantifierEliminationProblem;
import com.sri.ai.grinder.core.solver.MeasurableSingleQuantifierEliminationProblem;
import com.sri.ai.grinder.helper.AssignmentsSamplingIterator;
import com.sri.ai.grinder.library.number.Division;
import com.sri.ai.grinder.rewriter.core.Exhaustive;
import com.sri.ai.grinder.rewriter.core.Recursive;
import com.sri.ai.util.collect.FunctionIterator;
import com.sri.ai.util.collect.LazyIterator;
import com.sri.ai.util.math.Rational;

/**
 * A lazy iterator going over a sample of indices assignments.
 * 
 * @author braz
 *
 */
public class SamplingAdderLazyIterator implements LazyIterator<Expression> {

	private FunctionIterator<Expression, Expression> sumEstimateIterator;
	private AdderIterator sumOfSamplesIterator;
	
	private MeasurableSingleQuantifierEliminationProblem problem;
	private Rational numberOfSamplesSoFar;

	public SamplingAdderLazyIterator(
			MultiQuantifierEliminationProblem problem, 
			TopRewriterUsingContextAssignments topRewriterUsingContextAssignments,
			Random random, 
			Context context) {

		myAssert(problem.getIndices().size() == 1, () -> this.getClass() + " requires single-index problems but got " + problem);

		makeSumEstimateIterator(problem, topRewriterUsingContextAssignments, random, context);

		this.problem = getMeasurableProblem(problem).getFirstIndexVersion();
		this.numberOfSamplesSoFar = Rational.ZERO;
	}

	private MeasurableMultiQuantifierEliminationProblem getMeasurableProblem(MultiQuantifierEliminationProblem problem) {
		MeasurableMultiQuantifierEliminationProblem measurableProblem;
		if (problem instanceof MeasurableMultiQuantifierEliminationProblem) {
			measurableProblem = (MeasurableMultiQuantifierEliminationProblem) problem;
		}
		else {
			measurableProblem = new MeasurableMultiQuantifierEliminationProblem(problem);
		}
		return measurableProblem;
	}

	private void makeSumEstimateIterator(MultiQuantifierEliminationProblem problem, TopRewriterUsingContextAssignments topRewriterUsingContextAssignments, Random random, Context context) {
		sumOfSamplesIterator = makeSumOfSamplesIterator(problem, topRewriterUsingContextAssignments, random, context);
		sumEstimateIterator = functionIterator(sumOfSamplesIterator, this::computeSumEstimateFromSumOfSamples);
	}

	private AdderIterator makeSumOfSamplesIterator(MultiQuantifierEliminationProblem problem, TopRewriterUsingContextAssignments topRewriterUsingContextAssignments, Random random, Context context) {
		AssignmentsSamplingIterator sampledAssignmentsIterator = 
				new AssignmentsSamplingIterator(
						problem.getIndices(), 
						problem.getConstraint(),
						new Recursive(new Exhaustive(topRewriterUsingContextAssignments)), 
						random, 
						context);
		
		AdderIterator result = new AdderIterator(
				problem.getGroup(),
				sampledAssignmentsIterator,
				problem.getBody(),
				topRewriterUsingContextAssignments,
				context);
		
		return result;
	}

	private Expression computeSumEstimateFromSumOfSamples(Expression sumOfSamples) {
		Expression average = computeAverage(sumOfSamples);
		Expression result = multiplyByMeasure(average);
		return result;
	}

	private Expression computeAverage(Expression groupSumOfSamples) {
		Expression average = problem.getGroup().addNTimes(groupSumOfSamples, Division.make(ONE, makeSymbol(numberOfSamplesSoFar)), sumOfSamplesIterator.context);
		return average;
	}

	private Expression multiplyByMeasure(Expression average) {
		Symbol measureExpression = makeSymbol(problem.getMeasure(sumOfSamplesIterator.context));
		Expression result = problem.getGroup().addNTimes(average, measureExpression, sumOfSamplesIterator.context);
		return result;
	}

	public void setContext(Context newContext) {
		sumOfSamplesIterator.setContext(newContext);
	}
	
	@Override
	public boolean hasNext() {
		return sumEstimateIterator.hasNext();
	}

	@Override
	public void goToNextWithoutComputingCurrent() {
		numberOfSamplesSoFar = numberOfSamplesSoFar.add(1);
		sumEstimateIterator.goToNextWithoutComputingCurrent();
	}

	@Override
	public Expression computeCurrent() {
		return sumEstimateIterator.computeCurrent();
	}
}