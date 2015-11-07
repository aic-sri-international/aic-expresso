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
package com.sri.ai.expresso.type;

import static com.sri.ai.expresso.helper.Expressions.makeSymbol;
import static com.sri.ai.util.collect.FunctionIterator.functionIterator;

import java.util.Iterator;
import java.util.Random;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.Symbol;
import com.sri.ai.expresso.api.Type;
import com.sri.ai.util.collect.IntegerIterator;
import com.sri.ai.util.math.Rational;

/**
 * Represents the Integer interval [0, 9].
 * 
 * @author braz
 */
@Beta
public class Integer0To9 implements Type {

	@Override
	public String getName() {
		return "Integer0to9";
	}

	/**
	 * Naturally, this iterator will never iterate over all integers.
	 * We iterate over non-negative integers only, as that may be more natural for most applications.
	 */
	@Override
	public Iterator<Expression> iterator() {
		return functionIterator(new IntegerIterator(0, 9), i -> makeSymbol(i));
	}

	@Override
	public boolean contains(Expression uniquelyNamedConstant) {
		boolean result =
				uniquelyNamedConstant.getValue() instanceof Rational
				&& ((Rational) uniquelyNamedConstant.getValue()).isInteger()
				&& ((Rational) uniquelyNamedConstant.getValue()).compareTo(0) >= 0
				&& ((Rational) uniquelyNamedConstant.getValue()).compareTo(10) < 0;
		return result;
	}

	@Override
	public Expression sampleConstant(Random random) {
		Symbol result = makeSymbol(new Rational(random.nextInt(10)));
		return result;
	}

	@Override
	public int size() {
		return 10;
	}
}
