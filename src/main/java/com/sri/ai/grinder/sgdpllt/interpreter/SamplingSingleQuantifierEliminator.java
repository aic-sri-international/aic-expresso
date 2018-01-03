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
import static com.sri.ai.expresso.helper.Expressions.ONE;
import static com.sri.ai.expresso.helper.Expressions.makeSymbol;
import static com.sri.ai.util.Util.myAssert;

import java.util.Iterator;
import java.util.List;
import java.util.Random;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.IndexExpressionsSet;
import com.sri.ai.expresso.core.ExtensionalIndexExpressionsSet;
import com.sri.ai.grinder.helper.AssignmentsSamplingIterator;
import com.sri.ai.grinder.sgdpllt.api.Context;
import com.sri.ai.grinder.sgdpllt.api.MultiQuantifierEliminationProblem;
import com.sri.ai.grinder.sgdpllt.api.SingleQuantifierEliminationProblem;
import com.sri.ai.grinder.sgdpllt.group.AssociativeCommutativeGroup;
import com.sri.ai.grinder.sgdpllt.library.indexexpression.IndexExpressions;
import com.sri.ai.grinder.sgdpllt.library.number.Division;
import com.sri.ai.grinder.sgdpllt.library.set.Measure;
import com.sri.ai.grinder.sgdpllt.rewriter.api.Rewriter;
import com.sri.ai.util.math.Rational;

/**
 * A sampling quantifier elimination for problems with a single index.
 */
public class SamplingSingleQuantifierEliminator extends AbstractIterativeMultiQuantifierEliminator {

	private int sampleSize;
	private Rewriter indicesConditionEvaluator;
	private Random random;
	
	public SamplingSingleQuantifierEliminator(
			TopRewriterUsingContextAssignments topRewriterWithBaseAssignment, 
			int sampleSize, 
			Rewriter indicesConditionEvaluator, 
			Random random) {
		
		super(topRewriterWithBaseAssignment);
		this.sampleSize = sampleSize;
		this.indicesConditionEvaluator = indicesConditionEvaluator;
		this.random = random;
	}
	
	@Override
	public Expression solve(MultiQuantifierEliminationProblem problem, Context context) {
		
		myAssert(problem.getIndices().size() == 1, () -> this.getClass() + " requires single-index problems but got " + problem);
		
		Expression result = solveBySamplingSingleIndexIfCheaper(problem.getFirstIndexVersion(), context);
				
		return result;
	}

	private Expression solveBySamplingSingleIndexIfCheaper(SingleQuantifierEliminationProblem problem, Context context) {
		Rational measureOfDomainSatisfyingCondition = computeMeasureOfDomainSatisfyingCondition(problem, context);
		Expression result = computeResultBasedOnSamples(problem, measureOfDomainSatisfyingCondition, context);								
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

	private Expression computeResultBasedOnSamples(SingleQuantifierEliminationProblem problem, Rational measureOfDomainSatisfyingCondition, Context context) {
		Expression groupSumOfSamples = super.solve(problem, context);			
		AssociativeCommutativeGroup group = problem.getGroup();
		Expression average = group.addNTimes(groupSumOfSamples, Division.make(ONE, makeSymbol(sampleSize)), context);
		Expression result = group.addNTimes(average, makeSymbol(measureOfDomainSatisfyingCondition), context);
		return result;
	}

	@Override
	public
	Iterator<Assignment> 
	makeAssignmentsIterator(List<Expression> indices, Expression indicesCondition, Context context) {
		Iterator<Assignment> result = 
				new AssignmentsSamplingIterator(
						indices, sampleSize, indicesCondition, indicesConditionEvaluator, random, context);
		return result;
	}

	@Override
	public Expression makeSummand(MultiQuantifierEliminationProblem problem, Context context) {
		Expression result = problem.getBody();
		return result;
	}
}