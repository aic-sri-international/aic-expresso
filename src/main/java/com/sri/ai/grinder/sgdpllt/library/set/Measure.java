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
package com.sri.ai.grinder.sgdpllt.library.set;

import static com.sri.ai.expresso.helper.Expressions.ZERO;

import java.util.Arrays;
import java.util.List;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.IndexExpressionsSet;
import com.sri.ai.expresso.api.IntensionalSet;
import com.sri.ai.expresso.api.Type;
import com.sri.ai.expresso.core.DefaultCountingFormula;
import com.sri.ai.expresso.core.ExtensionalIndexExpressionsSet;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.expresso.type.FunctionType;
import com.sri.ai.expresso.type.RealExpressoType;
import com.sri.ai.expresso.type.RealInterval;
import com.sri.ai.expresso.type.TupleType;
import com.sri.ai.grinder.helper.GrinderUtil;
import com.sri.ai.grinder.sgdpllt.api.Context;
import com.sri.ai.grinder.sgdpllt.library.controlflow.IfThenElse;
import com.sri.ai.grinder.sgdpllt.library.indexexpression.IndexExpressions;
import com.sri.ai.grinder.sgdpllt.theory.linearrealarithmetic.MeasureOfSingleVariableLinearRealArithmeticConstraintStepSolver;
import com.sri.ai.grinder.sgdpllt.theory.linearrealarithmetic.SingleVariableLinearRealArithmeticConstraint;
import com.sri.ai.util.math.Rational;

/**
 * Measure of an intensional set:<br>
 * <pre>
 * measure(SetOfI) is
 * 
 * - measure(co-domain)^measure(domain) if SetOfI is a function type
 * 
 * - prod_i measure(component i) if SetOfI is a tuple type
 * 
 * - 0 if SetOfI is an intensional set with Condition equal to "false"
 * 
 * - measure(Domain) if SetOfI is an intensional set {{ (on I Domain) I }}
 * 
 * - | SetOfI | if SetOfI is a discrete type, or an intensional set indexed by a discrete 
 *   type with Condition instance of SingleVariableConstraint
 * 
 * - b -  a, if SetOfI is a type [a;b] (Real is to be considered [-infinity;infinity]), 
 *   or if SetOfI is an intensional set indexed by a real-valued index and an interval [a;b] is returned by 
 *   IntervalWithMeasureEquivalentToSingleVariableLinearRealArithmeticConstraintStepSolver
 *   (a and b could be infinite values, in which case the measure will be infinite) 
 * 
 * </pre>
 *  
 * @author oreilly
 *
 */
public class Measure {
	
	public static Rational get(Expression intensionalSetExpression, Context context) {
		Rational result;
		if (Sets.isIntensionalSet(intensionalSetExpression)) {
			IntensionalSet intensionalSet           = (IntensionalSet) intensionalSetExpression;
			IndexExpressionsSet indexExpressionsSet = intensionalSet.getIndexExpressions();
			List<Expression> indices                = IndexExpressions.getIndices(indexExpressionsSet);
			if (indices.size() == 1) {
				Expression evaluatedResult;
				Expression intensionalSetIndex     = indices.get(0);
				Expression intensionalSetHead      = intensionalSet.getHead();
				if (!intensionalSetHead.equals(intensionalSetIndex)) {
					throw new UnsupportedOperationException("Index and Head must be the same to calculate the meaure of an Intensional : "+intensionalSet);
				}
				Expression intensionalSetCondition = intensionalSet.getCondition();
				Context intensionalSetContext      = context.extendWith(indexExpressionsSet);
				Type indexType                     = GrinderUtil.getTypeOfExpression(intensionalSetIndex, intensionalSetContext);
				
				if (intensionalSetCondition.equals(false)) {
					evaluatedResult = Expressions.ZERO; // short circuit known empty sets up front.
				}
				else if (indexType instanceof RealExpressoType || indexType instanceof RealInterval) {
					// NOTE : For Reals can always assume the condition is of this type.				
					SingleVariableLinearRealArithmeticConstraint svConstraint = (SingleVariableLinearRealArithmeticConstraint) intensionalSetCondition;
					MeasureOfSingleVariableLinearRealArithmeticConstraintStepSolver realSolver = new MeasureOfSingleVariableLinearRealArithmeticConstraintStepSolver(svConstraint);
					evaluatedResult = realSolver.solve(intensionalSetContext);
				}
				else if (indexType instanceof FunctionType) {
					
					if (!intensionalSetCondition.equals(true)) {
						throw new UnsupportedOperationException("Measure of intensional set with a function type domain currently do not support conditions: "+intensionalSet);
					}
					
					// measure(co-domain)^measure(domain)
					FunctionType indexFunctionType = (FunctionType) indexType;
					
					Expression condomainIntensionalSet = constructComponentIntensionalSet(indexFunctionType.getCodomain(), intensionalSet, ZERO, intensionalSetContext);
					Rational codomainMeasure = get(condomainIntensionalSet, intensionalSetContext);
					Rational domainMeasure = Rational.ONE;
					for (Type argDomainType : indexFunctionType.getArgumentTypes()) {
						Expression argDomainIntensionalSet = constructComponentIntensionalSet(argDomainType, intensionalSet, ZERO, intensionalSetContext);
						Rational argMeasure = get(argDomainIntensionalSet, intensionalSetContext);
						domainMeasure = domainMeasure.multiply(argMeasure);
					}
					
					evaluatedResult = Expressions.makeSymbol(codomainMeasure.pow(domainMeasure.intValueExact()));
				} 
				else if (indexType instanceof TupleType) {
					
					if (!intensionalSetCondition.equals(true)) {
						throw new UnsupportedOperationException("Measure of intensional set with a tuple type domain currently do not support conditions: "+intensionalSet);
					}
					
					// (element_1, ..., element_n) = measure(element_1) * ... * measure(element_n)
					TupleType indexTupleType = (TupleType) indexType;
					Rational elementMeasuresProduct = Rational.ONE;
					for (Type elementType : indexTupleType.getElementTypes()) {
						Expression elementDomainIntensionalSet = constructComponentIntensionalSet(elementType, intensionalSet, ZERO, intensionalSetContext);
						Rational elementMeasure = get(elementDomainIntensionalSet, intensionalSetContext);
						elementMeasuresProduct = elementMeasuresProduct.multiply(elementMeasure);
					}
					
					evaluatedResult = Expressions.makeSymbol(elementMeasuresProduct);
				} else {
					Expression countingFormula = new DefaultCountingFormula(indexExpressionsSet, intensionalSet.getCondition());
					evaluatedResult = context.getTheory().evaluate(countingFormula, context);
				}
				
				if (Expressions.isNumber(evaluatedResult)) {
					result = evaluatedResult.rationalValue();
				}
				else {
					throw new UnsupportedOperationException("Unable to compute a finite measure for: "+intensionalSet+", got : "+evaluatedResult);					
				}				
			}
			else {
				throw new UnsupportedOperationException("Currently only support the measure of single indexed intensional sets: "+intensionalSet);
			}
		}		
		else {
			throw new IllegalArgumentException("Not an intensional set: "+intensionalSetExpression);
		}
		return result;
	}
	
	private static Expression constructComponentIntensionalSet(Type indexType, IntensionalSet intensionalSet, Expression additiveIdentityElement, Context intensionalSetContext) {
		Expression conditionedBody = IfThenElse.make(intensionalSet.getCondition(), intensionalSet.getHead(), additiveIdentityElement);
		Expression componentIndex  = Expressions.makeUniqueVariable("C", conditionedBody, intensionalSetContext);
		Expression indexExpression = IndexExpressions.makeIndexExpression(componentIndex, Expressions.parse(indexType.getName()));
		
		Expression intensionalCondition = Expressions.TRUE;
		// NOTE: handle the REAL cases where an SingleVariableLinearRealArithmeticConstraint is expected.
		if (indexType instanceof RealExpressoType || indexType instanceof RealInterval) {
			SingleVariableLinearRealArithmeticConstraint svlraConstraint = new SingleVariableLinearRealArithmeticConstraint(componentIndex, true, intensionalSetContext.getTheory());
			intensionalCondition = svlraConstraint;
		}
		
		Expression result = IntensionalSet.make(Sets.isMultiSet(intensionalSet) ? IntensionalSet.MULTI_SET_LABEL : IntensionalSet.UNI_SET_LABEL,
								new ExtensionalIndexExpressionsSet(Arrays.asList(indexExpression)), 								
								componentIndex, 
								intensionalCondition);
		return result;
	}
}
