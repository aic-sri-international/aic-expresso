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
package com.sri.ai.grinder.library.number;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.core.AbstractRewriter;
import com.sri.ai.grinder.core.HasKind;
import com.sri.ai.grinder.core.HasNumberOfArguments;
import com.sri.ai.grinder.library.FunctorConstants;
import com.sri.ai.util.math.Rational;

/**
 * Implements a rewriter for the exponentiation operation.
 * 
 * @author braz
 *
 */
@Beta
public class Exponentiation extends AbstractRewriter {
	
	private int      maxAbsExponentSizeBeforeLoosePrecision = Math.min(Math.abs(Double.MAX_EXPONENT), Math.abs(Double.MIN_EXPONENT));
	private Rational nonZeroMinAbsValue                     = new Rational(1).divide(new Rational(10).pow(324)); // Note: 324 is based on # digits in numerator of Double.MIN_VALUE
	private Expression   nonZeroMinPosSymbol                = Expressions.makeSymbol(nonZeroMinAbsValue);
	private Expression   nonZeroMinNegSymbol                = Expressions.makeSymbol(nonZeroMinAbsValue.negate());
	
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
	}
	
	public Exponentiation() {
		this.setReifiedTests(new HasKind(FunctorConstants.EXPONENTIATION),
				             new HasNumberOfArguments(2));
	}

	@Override
	public Expression rewriteAfterBookkeeping(Expression expression, RewritingProcess process) {

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
					double newValue            = Math.pow(baseDoubleValue, exponentDoubleValue);
					
					if (newValue == 0 && !baseValue.isZero()) {
						result = nonZeroMinPosSymbol;
					}
					else if (newValue == Double.POSITIVE_INFINITY) {
						result = Expressions.makeSymbol(Double.MAX_VALUE);
					}
					else if (newValue == Double.NEGATIVE_INFINITY) {
						result = Expressions.makeSymbol(Double.MAX_VALUE * -1);
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
	
	private Expression boundPrecision(Rational ratValue) {
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
