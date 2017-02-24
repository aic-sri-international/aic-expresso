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
package com.sri.ai.grinder.sgdpllt.library.number;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.core.DefaultSymbol;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.grinder.sgdpllt.api.Context;
import com.sri.ai.grinder.sgdpllt.core.TrueContext;
import com.sri.ai.grinder.sgdpllt.library.FunctorConstants;
import com.sri.ai.grinder.sgdpllt.rewriter.api.Simplifier;
import com.sri.ai.util.math.Rational;

/**
 * @author braz
 *
 */
@Beta
public class Exponentiation implements Simplifier {
	public static final Expression EXPONENTIATION_FUNCTOR = Expressions.makeSymbol(FunctorConstants.EXPONENTIATION);
	//
	private static int      maxAbsExponentSizeBeforeLoosePrecision = Math.min(Math.abs(Double.MAX_EXPONENT), Math.abs(Double.MIN_EXPONENT));
	private static Rational nonZeroMinAbsValue                     = new Rational(1).divide(new Rational(10).pow(324)); // Note: 324 is based on # digits in numerator of Double.MIN_VALUE
	private static Expression   nonZeroMinPosSymbol                = Expressions.makeSymbol(nonZeroMinAbsValue);
	private static Expression   nonZeroMinNegSymbol                = Expressions.makeSymbol(nonZeroMinAbsValue.negate());
	
	// Note: Experimental Code for determining precision bounds.
	public static void main(String[] args) {
		Rational maxValue = new Rational(Double.MAX_VALUE);
		Rational minValue = new Rational(Double.MIN_VALUE);
		Rational maxExp   = new Rational(Double.MAX_EXPONENT);
		Rational minExp   = new Rational(Double.MIN_EXPONENT);
		
		System.out.println("max value="+maxValue);
		System.out.println("min value="+minValue);
		System.out.println("max exp  ="+maxExp);
		System.out.println("min exp  ="+minExp);
		
		Rational nonZeroMinAbsValue = new Rational(1).divide(new Rational(10).pow(324));
		
		System.out.println("nonZeroMinAbsValue="+nonZeroMinAbsValue);
		
		evaluationTest(DefaultSymbol.createSymbol(new Rational(10).pow(1022)), "10^1022");
		evaluationTest(DefaultSymbol.createSymbol(Double.MAX_VALUE), "10^1023");
		evaluationTest(DefaultSymbol.createSymbol(Double.MAX_VALUE), "10^1024");
		evaluationTest(DefaultSymbol.createSymbol(new Rational(10).pow(-324)), "0.1^324");
		evaluationTest(DefaultSymbol.createSymbol(nonZeroMinAbsValue), "0.1^325");
		evaluationTest(DefaultSymbol.createSymbol(nonZeroMinAbsValue), "0.1^1022");
		evaluationTest(DefaultSymbol.createSymbol(nonZeroMinAbsValue), "0.1^1023");
		evaluationTest(DefaultSymbol.createSymbol(nonZeroMinAbsValue), "0.1^2000");
		evaluationTest(DefaultSymbol.createSymbol(new Rational(10).pow(-324)), "10^(-324)");
		evaluationTest(DefaultSymbol.createSymbol(nonZeroMinAbsValue), "10^(-325)");
		evaluationTest(DefaultSymbol.createSymbol(nonZeroMinAbsValue), "10^(-1022)");
		evaluationTest(DefaultSymbol.createSymbol(nonZeroMinAbsValue), "10^(-1023)");
		evaluationTest(DefaultSymbol.createSymbol(nonZeroMinAbsValue), "10^(-2000)");
		
		// (3^1000)^(1/1000) = 2.0355 instead of 3 due to 3^1000 > Double.MAX_VALUE.
		evaluationTest(DefaultSymbol.createSymbol(new Rational(1144786509939353L, 562949953421312L)), "1322070819480806636890455259752144365965422032752148167664920368226828597346704899540778313850608061963909777696872582355950954582100618911865342725257953674027620225198320803878014774228964841274390400117588618041128947815623094438061566173054086674490506178125480344405547054397038895817465368254916136220830268563778582290228416398307887896918556404084898937609373242171846359938695516765018940588109060426089671438864102814350385648747165832010614366132173102768902855220001^0.001");
	}
	
	private static void evaluationTest(Expression expected, String expressionString) {
		Expression actual = simplify(Expressions.parse(expressionString), new TrueContext());
		if (!expected.equals(actual)) {
			System.err.println("---------");
			System.err.println("Given   ="+expressionString);
			System.err.println("Expected="+expected);			
			System.err.println("Actual  ="+actual);
		}
	}
	
	/**
	 * Makes an exponentiation function application.
	 * @param base the base of the exponentiation
	 * @param power the power of the exponentiation
	 * @return the exponentiation function application
	 */
	public static Expression make(Expression base, Rational power) {
		Expression result = Expressions.apply(EXPONENTIATION_FUNCTOR, base, power);
		return result;
	}

	/**
	 * Makes an exponentiation function application.
	 * @param base the base of the exponentiation
	 * @param power the power of the exponentiation
	 * @return the exponentiation function application
	 */
	public static Expression make(Expression base, Expression power) {
		Expression result = Expressions.apply(EXPONENTIATION_FUNCTOR, base, power);
		return result;
	}

	@Override
	public Expression applySimplifier(Expression expression, Context context) {
		return simplify(expression, context);
	}
	
	/**
	 * @param expression
	 * @param context TODO
	 * @return
	 */
	public static Expression simplify(Expression expression, Context context) {
		Expression base = expression.get(0);
		Expression exponent = expression.get(1);

		if (Expressions.isNumber(exponent)) {
			Rational exponentValue = exponent.rationalValue();
			if (exponentValue.isZero()) {
				return Expressions.ONE;
			}
			if (exponentValue.isOne()) {
				return base;
			}
		}
		
		if (Expressions.isNumber(base)) {
			Rational baseValue = base.rationalValue();
			if (baseValue.isOne()) {
				return Expressions.ONE;
			}
			// we refrain from simplifying 0^x to 0, because x could be 0 itself.
			
			if (Expressions.isNumber(exponent)) {
				Expression result = null;
				boolean  losePrecision = false;
				try {
					int exponentIntValue = exponent.intValueExact();
					if (Math.abs(exponentIntValue) <= maxAbsExponentSizeBeforeLoosePrecision) {
						Rational ratValue = baseValue.pow(exponentIntValue);
						result = boundPrecision(ratValue);
					}
					else {
						// The value is going to be too large/small to be processed efficiently
						// therefore lose some of the precision now.
						losePrecision = true;
					}
				}
				catch (ArithmeticException e) {
					// Rational.pow does not work for non-int exponents, so we have no choice but lose precision here.
					losePrecision = true;						
				}
				if (losePrecision) {
					double exponentDoubleValue = exponent.doubleValue();
					double baseDoubleValue     = base.doubleValue();
					if (baseDoubleValue == Double.POSITIVE_INFINITY) {
						baseDoubleValue = Double.MAX_VALUE;
					}
					else if (baseDoubleValue == Double.NEGATIVE_INFINITY) {
						baseDoubleValue = -Double.MAX_VALUE;
					}
					
					double newValue = Math.pow(baseDoubleValue, exponentDoubleValue);
					
					if (newValue == 0 && !baseValue.isZero()) {
						result = nonZeroMinPosSymbol;
					}
					else if (newValue == Double.POSITIVE_INFINITY) {
						result = Expressions.makeSymbol(Double.MAX_VALUE);
					}
					else if (newValue == Double.NEGATIVE_INFINITY) {
						result = Expressions.makeSymbol(-Double.MAX_VALUE);
					}
					else {
						result = boundPrecision(new Rational(newValue));
					}
				}
				return result;
			}
		}

		return expression;
	}
	
	private static Expression boundPrecision(Rational ratValue) {
		Expression result = null;
		
		Rational ratValueAbs = ratValue.abs();
		if (ratValue.isZero()) {
			result = Expressions.ZERO;
		} else if (ratValueAbs.max(nonZeroMinAbsValue) == ratValueAbs) {
			result = Expressions.makeSymbol(ratValue);
		}
		else {
			if (ratValue.isPositive()) {
				result = nonZeroMinPosSymbol;
			}
			else {
				result = nonZeroMinNegSymbol;
			}
		}
		
		return result;
	}
}
