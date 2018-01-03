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
package com.sri.ai.grinder.sgdpllt.interpreter;

import static com.sri.ai.expresso.api.IntensionalSet.intensionalMultiSet;

import java.util.Random;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.IndexExpressionsSet;
import com.sri.ai.expresso.api.Type;
import com.sri.ai.expresso.core.ExtensionalIndexExpressionsSet;
import com.sri.ai.grinder.helper.GrinderUtil;
import com.sri.ai.grinder.sgdpllt.api.Context;
import com.sri.ai.grinder.sgdpllt.api.MultiQuantifierEliminationProblem;
import com.sri.ai.grinder.sgdpllt.api.SingleQuantifierEliminationProblem;
import com.sri.ai.grinder.sgdpllt.core.solver.AbstractMultiQuantifierEliminator;
import com.sri.ai.grinder.sgdpllt.library.indexexpression.IndexExpressions;
import com.sri.ai.grinder.sgdpllt.library.set.Measure;
import com.sri.ai.grinder.sgdpllt.rewriter.api.Rewriter;
import com.sri.ai.util.math.Rational;

/**
 * A quantifier eliminator that decides whether to use brute force or sampling
 * depending on sample size, domain size, and a "always-sample" parameter.
 */
public class SamplingIfNeededMultiQuantifierEliminator extends AbstractMultiQuantifierEliminator {

	protected TopRewriterUsingContextAssignments topRewriterUsingContextAssignments;
	private int sampleSize;
	private boolean alwaysSample;
	private Rewriter indicesConditionEvaluator;
	private Random random;
	
	private BruteForceMultiQuantifierEliminator bruteForce;
	private SamplingSingleQuantifierEliminator sampling;
	
	public SamplingIfNeededMultiQuantifierEliminator(
			TopRewriterUsingContextAssignments topRewriterUsingContextAssignments, 
			int sampleSize, 
			boolean alwaysSample, 
			Rewriter indicesConditionEvaluator, 
			Random random) {
		
		this.topRewriterUsingContextAssignments = topRewriterUsingContextAssignments;
		this.sampleSize = sampleSize;
		this.alwaysSample = alwaysSample;
		this.indicesConditionEvaluator = indicesConditionEvaluator;
		this.random = random;
	}
	
	private BruteForceMultiQuantifierEliminator getBruteForce() {
		if (bruteForce == null) {
			bruteForce = new BruteForceMultiQuantifierEliminator(topRewriterUsingContextAssignments);
		}
		return bruteForce;
	}
	
	private SamplingSingleQuantifierEliminator getSampling() {
		if (sampling == null) {
			sampling = new SamplingSingleQuantifierEliminator(topRewriterUsingContextAssignments, sampleSize, indicesConditionEvaluator, random);
		}
		return sampling;
	}
	
	@Override
	public Expression solve(MultiQuantifierEliminationProblem problem, Context context) {
		boolean sample = decideWhetherToSample(problem, context);
		Expression result = solve(sample, problem, context);
		return result;
	}

	private boolean decideWhetherToSample(MultiQuantifierEliminationProblem problem, Context context) {
		boolean sample;
		if (problem.getIndices().size() == 1) {						
			SingleQuantifierEliminationProblem firstIndexProblem = problem.getFirstIndexVersion();
			Rational measureOfDomainSatisfyingCondition = computeMeasureOfDomainSatisfyingCondition(firstIndexProblem, context);
			sample = samplingIsCheaper(firstIndexProblem, measureOfDomainSatisfyingCondition, context);
		}
		else {
			sample = false;
		}
		return sample;
	}

	private Expression solve(boolean sample, MultiQuantifierEliminationProblem problem, Context context) {
		Expression result;
		if (sample) {
			result = getSampling().solve(problem, context);
		}
		else {
			result = getBruteForce().solve(problem, context);
		}
		return result;
	}

	private boolean samplingIsCheaper(SingleQuantifierEliminationProblem problem, Rational measureOfDomainSatisfyingCondition, Context context) {
		boolean result = 
				alwaysSample
				||
				domainIsContinuousOrDiscreteButLargerThanSampleSize(problem, measureOfDomainSatisfyingCondition, context);
		return result;
	}

	private Rational computeMeasureOfDomainSatisfyingCondition(SingleQuantifierEliminationProblem problem, Context context) {
		Expression intensionalSetOfAllIndexValues = getIntensionalSetOfAllIndexValues(problem);
		Rational result = Measure.get(intensionalSetOfAllIndexValues, context);
		return result;
	}

	private Expression getIntensionalSetOfAllIndexValues(SingleQuantifierEliminationProblem problem) {
		IndexExpressionsSet indexExpressionsSet = new ExtensionalIndexExpressionsSet(IndexExpressions.makeIndexExpression(problem.getIndex(), problem.getIndexType()));
		Expression intensionalSet = intensionalMultiSet(indexExpressionsSet, problem.getIndex(), problem.getConstraint());
		return intensionalSet;
	}

	private boolean domainIsContinuousOrDiscreteButLargerThanSampleSize(SingleQuantifierEliminationProblem problem, Rational measureOfDomainSatisfyingCondition, Context context) {
		Type type = GrinderUtil.getTypeOfExpression(problem.getIndex(), context);
		boolean result = 
				type == null 
				|| !type.isDiscrete() 
				|| measureOfDomainSatisfyingCondition.compareTo(sampleSize) > 0;
		return result;
	}
}