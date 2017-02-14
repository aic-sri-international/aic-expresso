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

import java.util.Arrays;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Random;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.IndexExpressionsSet;
import com.sri.ai.expresso.api.IntensionalSet;
import com.sri.ai.expresso.core.ExtensionalIndexExpressionsSet;
import com.sri.ai.expresso.helper.Expressions;
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
import com.sri.ai.grinder.sgdpllt.rewriter.api.TopRewriter;
import com.sri.ai.grinder.sgdpllt.rewriter.core.Exhaustive;
import com.sri.ai.grinder.sgdpllt.rewriter.core.Recursive;
import com.sri.ai.util.math.Rational;

/**
 * 
 * @author oreilly
 *
 */
public class SampleMultiIndexQuantifierEliminator extends AbstractIterativeMultiIndexQuantifierElimination {
	private int sampleSizeN;
	private Rewriter indicesConditionRewriter;
	private Random random;
	
	public SampleMultiIndexQuantifierEliminator(TopRewriter topRewriter, int sampleSizeN, Rewriter indicesConditionRewriter, Random random) {
		super(topRewriter);
		this.sampleSizeN = sampleSizeN;
		this.indicesConditionRewriter = new Recursive(new Exhaustive(indicesConditionRewriter));
		this.random = random;
	}
	
	public SampleMultiIndexQuantifierEliminator(TopRewriterUsingContextAssignments topRewriterWithBaseAssignment, int sampleSizeN, Rewriter indicesConditionRewriter, Random random) {
		super(topRewriterWithBaseAssignment);
		this.sampleSizeN = sampleSizeN;
		this.indicesConditionRewriter = new Recursive(new Exhaustive(indicesConditionRewriter));
		this.random = random;
	}
	
	@Override
	public Iterator<Map<Expression, Expression>> makeAssignmentsIterator(List<Expression> indices, Expression indicesCondition, Context context) {
		Iterator<Map<Expression, Expression>> result;
		if (indices.size() == 2 && indices.get(1).equals(Expressions.TRUE)) {
			result = new AssignmentsSamplingIterator(indices.subList(0, 1), sampleSizeN, indicesCondition, indicesConditionRewriter, random, context);
		}
		else {
			result = new AssignmentsIterator(indices, context);
		}
		return result;
	}
	
	@Override
	public Expression makeSummand(AssociativeCommutativeGroup group, List<Expression> indices, Expression indicesCondition, Expression body, Context context) {
		Expression result;
		if (indices.size() == 2 && indices.get(1).equals(Expressions.TRUE)) {
			// NOTE: the AssignmentsSamplingIterator takes the indicesCondition into account 
			// so no need to take it into account here (which is the case in BruteForceMultiIndexQuantifierEliminator).
			result = body;
		}
		else {
			result = IfThenElse.make(indicesCondition, body, group.additiveIdentityElement());
		}		
		return result;
	}
	
	@Override
	public Expression solve(AssociativeCommutativeGroup group, List<Expression> indices, Expression indicesCondition, Expression body, Context context) {
		Expression result = null;
		
		// Check if we want to sample		
		if (indices.size() == 1) {						
			
			// SetOfI = {{ (on I in Domain) I : Condition }}
			Rational measureSetOfI = computeMeasure(indices.get(0), indicesCondition, group.additiveIdentityElement(), context);
			
			if (measureSetOfI.compareTo(sampleSizeN) > 0) {
				// Quantifier({{ (on I in Samples) Head }} )
				
				// NOTE: we are using the indices[2] with 2nd arg=TRUE so that the sampling logic can determine when it should activate
				// in makeAssignmentsInterator() and makeSummand().
				Expression sampleSum = super.solve(group, Arrays.asList(indices.get(0), Expressions.TRUE), Expressions.TRUE,  body, context);			
				
				// Average = Quantifier( {{ (on I in Samples) Head }}) / n
				Expression average = group.addNTimes(sampleSum, Division.make(Expressions.ONE, Expressions.makeSymbol(sampleSizeN)), context);
				
				// return Average * | SetOfI |
				result = group.addNTimes(average, Expressions.makeSymbol(measureSetOfI), context);
			}
		}
				
		if (result == null) {
			result = super.solve(group, indices, indicesCondition,  body, context);
		}
		
		return result;
	}
	
	private Rational computeMeasure(Expression index, Expression indexCondition, Expression additiveIdentityElement, Context context) {
		Rational result;
		
		Expression indexType = GrinderUtil.getTypeExpression(index, context);
		IndexExpressionsSet indexExpressionsSet = new ExtensionalIndexExpressionsSet(IndexExpressions.makeIndexExpression(index, indexType));
		
		Expression intensionalSet = IntensionalSet.intensionalMultiSet(indexExpressionsSet, Expressions.ONE, indexCondition);
		
		result = Measure.get(intensionalSet, context);
		
		return result;
	}
}