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
package com.sri.ai.grinder.group;

import static com.sri.ai.expresso.helper.Expressions.INFINITY;
import static com.sri.ai.expresso.helper.Expressions.MINUS_INFINITY;
import static com.sri.ai.expresso.helper.Expressions.isMinusInfinity;
import static com.sri.ai.expresso.helper.Expressions.makeSymbol;
import static com.sri.ai.grinder.library.FunctorConstants.MAX;

import java.util.Random;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.grinder.api.Context;
import com.sri.ai.util.math.Rational;

/**
 * Object representing a group on symbolic numbers with the maximum operation.
 * 
 * @author braz
 *
 */
@Beta
public class Max extends AbstractFunctionBasedGroup {
	
	@Override
	public Expression additiveIdentityElement() {
		return MINUS_INFINITY;
	}

	@Override
	public boolean isAdditiveAbsorbingElement(Expression value) {
		boolean result = value.equals(INFINITY);
		return result;
	}

	@Override
	public Expression addAndPossiblySolveItDeprecated(Expression value1, Expression value2, Context context) {
		Expression result;
		if (value1.getValue() instanceof Number && value2.getValue() instanceof Number) {
			Rational rationalValue1 = value1.rationalValue();
			Rational rationalValue2 = value2.rationalValue();
			if (rationalValue1.compareTo(rationalValue2) > 0) {
				result = value1;
			}
			else {
				result = value2;
			}
		}
		else if (value1.equals(INFINITY) || value2.equals(INFINITY)) {
			result = INFINITY;
		}
		else if (isMinusInfinity(value1)) {
			result = value2;
		}
		else if (isMinusInfinity(value2)) {
			result = value1;
		}
		else {
			result = Expressions.apply(MAX, value1, value2);
		}
		return result;
	}

	@Override
	protected Expression addNTimesWithUnconditionalValueAndNDistinctFromZero(Expression valueToBeAdded, Expression n) {
		return valueToBeAdded;
	}

	@Override
	public boolean isIdempotent() {
		return true;
	}

	@Override
	public Expression makeRandomConstant(Random random) {
		Expression result = makeSymbol(random.nextInt(10));
		return result;
	}

	@Override
	public String getFunctionString() {
		return MAX;
	}
}