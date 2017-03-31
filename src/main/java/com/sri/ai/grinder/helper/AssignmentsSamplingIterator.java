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
package com.sri.ai.grinder.helper;

import static com.sri.ai.util.Util.map;

import java.util.List;
import java.util.Map;
import java.util.Random;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.Type;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.expresso.type.FunctionType;
import com.sri.ai.expresso.type.IntegerExpressoType;
import com.sri.ai.expresso.type.IntegerInterval;
import com.sri.ai.expresso.type.RealExpressoType;
import com.sri.ai.expresso.type.RealInterval;
import com.sri.ai.grinder.sgdpllt.api.Context;
import com.sri.ai.grinder.sgdpllt.interpreter.AbstractIterativeMultiIndexQuantifierElimination;
import com.sri.ai.grinder.sgdpllt.library.set.Sets;
import com.sri.ai.grinder.sgdpllt.library.set.extensional.ExtensionalSet;
import com.sri.ai.grinder.sgdpllt.rewriter.api.Rewriter;
import com.sri.ai.grinder.sgdpllt.theory.differencearithmetic.RangeAndExceptionsSet;
import com.sri.ai.grinder.sgdpllt.theory.differencearithmetic.SingleVariableDifferenceArithmeticConstraint;
import com.sri.ai.grinder.sgdpllt.theory.differencearithmetic.ValuesOfSingleVariableDifferenceArithmeticConstraintStepSolver;
import com.sri.ai.grinder.sgdpllt.theory.linearrealarithmetic.IntervalWithMeasureEquivalentToSingleVariableLinearRealArithmeticConstraintStepSolver;
import com.sri.ai.grinder.sgdpllt.theory.linearrealarithmetic.SingleVariableLinearRealArithmeticConstraint;
import com.sri.ai.util.collect.EZIterator;

/**
 * An assignments iterator that samples over a single variables domain.
 * 
 * @author oreilly
 *
 */
public class AssignmentsSamplingIterator extends EZIterator<Map<Expression, Expression>> {
	private Expression index;
	private int  sampleSizeN;
	private int  currentN;
	private Type typeToSampleFrom; 
	private Expression condition;
	private Rewriter conditionRewriter;
	private Random random;
	private Context context;
	
	public AssignmentsSamplingIterator(List<Expression> indices, int sampleSizeN, Expression condition, Rewriter conditionRewriter, Random random, Context context) {
		if (indices.size() != 1) {
			throw new UnsupportedOperationException("Assignment sampling iterator only supports a single index currently, received: "+indices);
		}
		this.index             = indices.get(0);
		this.sampleSizeN       = sampleSizeN;
		this.currentN          = 0;
		this.typeToSampleFrom  = getTypeToSampleFrom(index, condition, context);
		if (this.typeToSampleFrom == null) {
			// Means we have an empty set
			currentN = sampleSizeN;
		}
		this.condition         = condition;
		this.conditionRewriter = conditionRewriter;
		this.random            = random;
		this.context           = context;
	}
	
	@Override
	protected Map<Expression, Expression> calculateNext() {
		Map<Expression, Expression> result = null;
		
		if (currentN < sampleSizeN) {
			currentN++;
			do {
				Expression assignment         = typeToSampleFrom.sampleUniquelyNamedConstant(random);
				Context contextWithAssignment = AbstractIterativeMultiIndexQuantifierElimination.extendAssignments(map(index, assignment), context);
				Expression conditionValue     = conditionRewriter.apply(condition, contextWithAssignment);
				if (conditionValue.equals(Expressions.TRUE)) {
					result = map(index, assignment);
				}				
			} while (result == null);
		}
		
		return result;
	}
	
	public static Type getTypeToSampleFrom(Expression variable, Expression condition, Context context) {
		Type result = GrinderUtil.getType(variable, context);
		
		if (result instanceof FunctionType) {
			FunctionType functionType = (FunctionType) result;
			result = new LazySampledFunctionType(functionType.getCodomain(), functionType.getArgumentTypes().toArray(new Type[functionType.getArity()]));
		}
		else {
			if (condition.equals(false)) {
				result = null;
			}
			else if (condition.equals(true)) {
				// we leave as is.
			}
			else if (result instanceof RealExpressoType || result instanceof RealInterval) {		
				IntervalWithMeasureEquivalentToSingleVariableLinearRealArithmeticConstraintStepSolver solver = 
						new IntervalWithMeasureEquivalentToSingleVariableLinearRealArithmeticConstraintStepSolver((SingleVariableLinearRealArithmeticConstraint) condition);
				
				Expression realInterval = solver.solve(context);
				if (Sets.isEmptySet(realInterval)) {
					result = null; // used to indicate an empty set.
				}
				else if (ExtensionalSet.isExtensionalSet(realInterval) && ExtensionalSet.isSingleton(realInterval)) {
					String singletonValue = realInterval.get(0).toString();
					result = new RealInterval("["+singletonValue+";"+singletonValue+"]");				
				}
				else {
					result = new RealInterval(realInterval.toString());
				}
			}
			else if (result instanceof IntegerExpressoType || result instanceof IntegerInterval) {
				ValuesOfSingleVariableDifferenceArithmeticConstraintStepSolver solver = new ValuesOfSingleVariableDifferenceArithmeticConstraintStepSolver((SingleVariableDifferenceArithmeticConstraint)condition);
				
				// NOTE: the exceptions set returned here is implicit in the condition so no need to use it here.
				RangeAndExceptionsSet rangeAndExceptionsSet = (RangeAndExceptionsSet) solver.solve(context);
				
				if (rangeAndExceptionsSet.isEmpty()) {
					result = null; // used to indicate an empty set.
				}
				else if (rangeAndExceptionsSet.isSingleton()) {
					result = new IntegerInterval(rangeAndExceptionsSet.getSingleValue().intValueExact(), rangeAndExceptionsSet.getSingleValue().intValueExact());
				}
				else {
					result = new IntegerInterval(rangeAndExceptionsSet.getStrictLowerBound().intValueExact()+1, rangeAndExceptionsSet.getNonStrictUpperBound().intValueExact());
				}
			}
		}
		
		if (result != null && !result.isSampleUniquelyNamedConstantSupported()) {
			throw new IllegalArgumentException("Unable to sample "+variable+" from "+result);
		}
		
		return result;
	}
}