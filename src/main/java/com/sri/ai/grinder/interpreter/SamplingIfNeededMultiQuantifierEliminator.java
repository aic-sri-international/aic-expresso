/*
 * Copyright (c) 2017, SRI International
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

import java.util.Random;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.Type;
import com.sri.ai.grinder.api.Context;
import com.sri.ai.grinder.api.MultiQuantifierEliminationProblem;
import com.sri.ai.grinder.core.solver.AbstractMultiQuantifierEliminator;
import com.sri.ai.grinder.core.solver.MeasurableMultiQuantifierEliminationProblem;
import com.sri.ai.grinder.helper.GrinderUtil;
import com.sri.ai.grinder.rewriter.api.Rewriter;

/**
 * A quantifier eliminator that decides whether to use brute force or sampling
 * depending on sample size, domain size, and a "always-sample" parameter.
 */
public class SamplingIfNeededMultiQuantifierEliminator extends AbstractMultiQuantifierEliminator {

	protected TopRewriterUsingContextAssignments topRewriterUsingContextAssignments;
	private int sampleSize;
	private boolean alwaysSample;
	private Random random;
	
	private BruteForceMultiQuantifierEliminator bruteForce;
	private SamplingWithFixedSampleSizeSingleQuantifierEliminator sampling;
	
	public SamplingIfNeededMultiQuantifierEliminator(
			TopRewriterUsingContextAssignments topRewriterUsingContextAssignments, 
			int sampleSize, 
			boolean alwaysSample, 
			Rewriter indicesConditionEvaluator, 
			Random random) {
		
		this.topRewriterUsingContextAssignments = topRewriterUsingContextAssignments;
		this.sampleSize = sampleSize;
		this.alwaysSample = alwaysSample;
		this.random = random;
	}
	
	private BruteForceMultiQuantifierEliminator getBruteForce() {
		if (bruteForce == null) {
			bruteForce = new BruteForceMultiQuantifierEliminator(topRewriterUsingContextAssignments);
		}
		return bruteForce;
	}
	
	private SamplingWithFixedSampleSizeSingleQuantifierEliminator getSampling() {
		if (sampling == null) {
			sampling = new SamplingWithFixedSampleSizeSingleQuantifierEliminator(topRewriterUsingContextAssignments, sampleSize, random);
		}
		return sampling;
	}
	
	@Override
	public Expression solve(MultiQuantifierEliminationProblem problem, Context context) {
		// we create a measurable problem here so that the measure is computed inside it only once and reused afterwards.
		MeasurableMultiQuantifierEliminationProblem measurableProblem =
				new MeasurableMultiQuantifierEliminationProblem(problem);
		boolean sample = decideWhetherToSample(measurableProblem, context);
		Expression result = solve(sample, measurableProblem, context);
		return result;
	}

	private boolean decideWhetherToSample(MeasurableMultiQuantifierEliminationProblem problem, Context context) {
		boolean sample;
		if (problem.getIndices().size() == 1) {						
			sample = samplingIsCheaper(problem, context);
		}
		else {
			sample = false;
		}
		return sample;
	}

	private Expression solve(boolean sample, MeasurableMultiQuantifierEliminationProblem problem, Context context) {
		Expression result;
		if (sample) {
			result = getSampling().solve(problem, context);
		}
		else {
			result = getBruteForce().solve(problem, context);
		}
		return result;
	}

	private boolean samplingIsCheaper(MeasurableMultiQuantifierEliminationProblem problem, Context context) {
		boolean result = 
				alwaysSample
				||
				domainIsContinuousOrDiscreteAndLargerThanSampleSize(problem, context);
		return result;
	}

	private boolean domainIsContinuousOrDiscreteAndLargerThanSampleSize(MeasurableMultiQuantifierEliminationProblem problem, Context context) {
		Type type = GrinderUtil.getTypeOfExpression(problem.getIndices().get(0), context);
		boolean result = 
				type == null 
				|| !type.isDiscrete() 
				|| problem.getMeasure(context).compareTo(sampleSize) > 0;
		return result;
	}
}