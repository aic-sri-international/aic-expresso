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
package com.sri.ai.grinder.sgdpll.group;

import static com.sri.ai.expresso.helper.Expressions.ONE;
import static com.sri.ai.expresso.helper.Expressions.ZERO;
import static com.sri.ai.expresso.helper.Expressions.makeSymbol;
import static com.sri.ai.grinder.library.FunctorConstants.PRODUCT;
import static com.sri.ai.util.Util.arrayList;

import java.util.Random;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.IndexExpressionsSet;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.grinder.api.Context;
import com.sri.ai.grinder.library.number.Exponentiation;
import com.sri.ai.grinder.library.number.NumericSimplifier;
import com.sri.ai.grinder.library.number.Times;
import com.sri.ai.grinder.sgdpll.simplifier.api.Simplifier;
import com.sri.ai.util.base.Pair;

/**
 * Object representing a group on symbolic numbers with multiplication.
 * 
 * @author braz
 *
 */
@Beta
public class Product extends AbstractNumericGroup implements AssociativeCommutativeGroup {
	
	// TODO: abstract re-used code between this class and Sum
	// TODO: re-use from information from associate commutative rewriters, like identity and absorbing values.
	
	@Override
	public Expression additiveIdentityElement() {
		return ONE;
	}

	@Override
	public boolean isAdditiveAbsorbingElement(Expression value) {
		boolean result = value.equals(ZERO);
		return result;
	}

	@Override
	public Expression add(Expression value1, Expression value2, Context context) {
		Expression result;
		if (value1.getValue() instanceof Number && value2.getValue() instanceof Number) { // not necessary, as else clause is generic enough to deal with this case as well, but hopefully this saves time.
			result = Expressions.makeSymbol(value1.rationalValue().multiply(value2.rationalValue()));
		}
		else {
			Expression multiplication = Times.make(arrayList(value1, value2));
			result = numericSimplifier.apply(multiplication, context);
		}
		return result;
	}

	private static Simplifier numericSimplifier = new NumericSimplifier();
	
	@Override
	protected Expression addNTimesWithUnconditionalValueAndNDistinctFromZero(Expression valueToBeAdded, Expression n) {
		Expression result;
		if (valueToBeAdded.equals(ONE)) { // optimization
			result = ONE;
		}
		else if (valueToBeAdded.equals(ZERO) && ! n.equals(ZERO)) { // optimization
			result = ZERO;
		}
		else {
			result = Exponentiation.make(valueToBeAdded, n);
		}
		return result;
	}

	@Override
	public boolean isIdempotent() {
		return false;
	}

	@Override
	public Expression makeRandomConstant(Random random) {
		Expression result = makeSymbol(random.nextInt(10));
		return result;
	}

	@Override
	public Pair<Expression, IndexExpressionsSet> getExpressionAndIndexExpressionsFromProblemExpression(Expression expression, Context context) {
		return 
				FunctionApplicationProblemsUtil
				.staticGetExpressionAndIndexExpressionsFromProblemExpression(
						expression, 
						PRODUCT, 
						additiveIdentityElement());
	}

	@Override
	public Expression makeProblemExpression(Expression index, Expression indexType, Expression constraint, Expression body) {
		return 
				FunctionApplicationProblemsUtil
				.staticMakeProblemExpression(
						PRODUCT, 
						index, 
						indexType, 
						constraint, 
						body);
	}
}