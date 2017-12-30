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
import static com.sri.ai.util.Util.list;

import java.util.Iterator;
import java.util.List;
import java.util.Random;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.IndexExpressionsSet;
import com.sri.ai.expresso.api.Type;
import com.sri.ai.expresso.core.ExtensionalIndexExpressionsSet;
import com.sri.ai.grinder.helper.AssignmentsIterator;
import com.sri.ai.grinder.helper.AssignmentsSamplingIterator;
import com.sri.ai.grinder.helper.GrinderUtil;
import com.sri.ai.grinder.sgdpllt.api.Context;
import com.sri.ai.grinder.sgdpllt.group.AssociativeCommutativeGroup;
import com.sri.ai.grinder.sgdpllt.library.controlflow.IfThenElse;
import com.sri.ai.grinder.sgdpllt.library.indexexpression.IndexExpressions;
import com.sri.ai.grinder.sgdpllt.library.number.Division;
import com.sri.ai.grinder.sgdpllt.library.set.Measure;
import com.sri.ai.grinder.sgdpllt.rewriter.api.Rewriter;
import com.sri.ai.util.math.Rational;

/**
 * 
 * @author oreilly
 *
 */
public class SamplingMultiIndexQuantifierEliminator extends AbstractIterativeMultiIndexQuantifierEliminator {

	private int sampleSize;
	private boolean alwaysSample;
	private Rewriter indicesConditionEvaluator;
	private Random random;
	private boolean sampleSingleIndex;
	
	public SamplingMultiIndexQuantifierEliminator(
			TopRewriterUsingContextAssignments topRewriterWithBaseAssignment, 
			int sampleSizeN, 
			boolean alwaysSample, 
			Rewriter indicesConditionEvaluator, 
			Random random) {
		
		super(topRewriterWithBaseAssignment);
		this.sampleSize = sampleSizeN;
		this.alwaysSample = alwaysSample;
		this.indicesConditionEvaluator = indicesConditionEvaluator;
		this.random = random;
	}
	
	@Override
	public Expression solve(AssociativeCommutativeGroup group, List<Expression> indices, Expression indicesCondition, Expression body, Context context) {
		Expression result = null;
		
		if (indices.size() == 1) {						
			result = solveBySamplingSingleIndexIfCheaper(group, indices.get(0), indicesCondition, body, context);
		}
				
		if (result == null) {
			result = super.solve(group, indices, indicesCondition,  body, context);
		}
		
		return result;
	}

	private Expression solveBySamplingSingleIndexIfCheaper(AssociativeCommutativeGroup group, Expression index, Expression indicesCondition, Expression body, Context context) {
		Expression result = null;
		Rational measureOfDomainSatisfyingCondition = computeMeasureOfDomainSatisfyingCondition(index, indicesCondition, context);
		sampleSingleIndex = decideWhetherToSampleEvenWithSingleIndex(index, measureOfDomainSatisfyingCondition, context);
		if (sampleSingleIndex) {
			result = computeResultBasedOnSamples(group, index, indicesCondition, body, measureOfDomainSatisfyingCondition, context);								
		}
		return result;
	}

	private Rational computeMeasureOfDomainSatisfyingCondition(Expression index, Expression indexCondition, Context context) {
		Expression indexType = GrinderUtil.getTypeExpressionOfExpression(index, context);
		IndexExpressionsSet indexExpressionsSet = new ExtensionalIndexExpressionsSet(IndexExpressions.makeIndexExpression(index, indexType));
		Expression intensionalSet = intensionalMultiSet(indexExpressionsSet, index, indexCondition);
		Rational result = Measure.get(intensionalSet, context);
		return result;
	}

	private boolean decideWhetherToSampleEvenWithSingleIndex(Expression index, Rational measureSetOfI, Context context) {
		boolean result = 
				alwaysSample
				||
				domainIsContinuousOrDiscreteButLargerThanSampleSize(index, measureSetOfI, context);
		return result;
	}

	private boolean domainIsContinuousOrDiscreteButLargerThanSampleSize(Expression index, Rational measureSetOfI, Context context) {
		Type type = GrinderUtil.getTypeOfExpression(index, context);
		boolean result = 
				type == null 
				|| !type.isDiscrete() 
				|| measureSetOfI.compareTo(sampleSize) > 0;
		return result;
	}

	private Expression computeResultBasedOnSamples(AssociativeCommutativeGroup group, Expression index, Expression indicesCondition, Expression body, Rational measureSetOfI, Context context) {
		// Quantifier({{ (on I in Samples) Head }} )							
		Expression groupSumFromSamples = super.solve(group, list(index), indicesCondition, body, context);			

		// Average = Quantifier( {{ (on I in Samples) Head }} ) / n
		Expression average = group.addNTimes(groupSumFromSamples, Division.make(ONE, makeSymbol(sampleSize)), context);
		
		// return Average * | SetOfI |
		Expression result = group.addNTimes(average, makeSymbol(measureSetOfI), context);
		return result;
	}

	@Override
	public
	Iterator<Assignment> 
	makeAssignmentsIterator(List<Expression> indices, Expression indicesCondition, Context context) {
		
		Iterator<Assignment> result;
		if (sampleSingleIndex) {
			result = new AssignmentsSamplingIterator(indices, sampleSize, indicesCondition, indicesConditionEvaluator, random, context);
		}
		else {
			result = new AssignmentsIterator(indices, context);
		}
		return result;
	}

	@Override
	public Expression makeSummand(AssociativeCommutativeGroup group, List<Expression> indices, Expression indicesCondition, Expression body, Context context) {
		Expression result;
		if (sampleSingleIndex) {
			// AssignmentsSamplingIterator takes the indicesCondition into account 
			// so no need to take it into account in this case
			result = body;
		}
		else {
			result = IfThenElse.make(indicesCondition, body, group.additiveIdentityElement());
		}		
		return result;
	}
}