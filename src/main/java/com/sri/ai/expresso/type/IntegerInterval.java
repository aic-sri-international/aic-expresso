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

import static com.sri.ai.expresso.helper.Expressions.INFINITY;
import static com.sri.ai.expresso.helper.Expressions.apply;
import static com.sri.ai.expresso.helper.Expressions.isNumber;
import static com.sri.ai.expresso.helper.Expressions.makeSymbol;
import static com.sri.ai.expresso.helper.Expressions.parse;
import static com.sri.ai.grinder.library.FunctorConstants.MINUS;
import static com.sri.ai.grinder.library.FunctorConstants.PLUS;
import static com.sri.ai.util.Util.myAssert;
import static com.sri.ai.util.collect.FunctionIterator.functionIterator;

import java.util.Iterator;
import java.util.Random;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.Symbol;
import com.sri.ai.expresso.api.Type;
import com.sri.ai.grinder.library.number.UnaryMinus;
import com.sri.ai.util.collect.BreadthFirstIterator;
import com.sri.ai.util.collect.IntegerIterator;
import com.sri.ai.util.math.Rational;

/**
 * Represents an Integer interval.
 * 
 * @author braz
 */
@Beta
public class IntegerInterval implements Type {

	private String name;

	private Expression nonStrictLowerBound;
	private Expression nonStrictUpperBound;
	
	@Override
	public String getName() {
		return name;
	}
	
	public IntegerInterval(String name) {
		this.name = name;
		Expression nameParse = parse(name);
		if (nameParse.equals("Integer")) {
			nonStrictLowerBound = UnaryMinus.make(INFINITY);
			nonStrictUpperBound = INFINITY;
		}
		else if (nameParse.hasFunctor("Integer") && nameParse.numberOfArguments() == 2) {
			nonStrictLowerBound = nameParse.get(0);
			nonStrictUpperBound = nameParse.get(1);
		}
		else {
			throw new Error(this.getClass() + " created with invalid name " + name + ". Must be either 'Integer' or 'Integer(nonStrictLowerBound, nonStrictUpperBound)'");
		}
	}

	public Expression getNonStrictLowerBound() {
		return nonStrictLowerBound;
	}

	public Expression getNonStrictUpperBound() {
		return nonStrictUpperBound;
	}

	/**
	 * If type is bounded, iterates over its elements.
	 * If it is unbounded on the positive side but bounded on the negative side,
	 * provides an iterator from the least element that never stops.
	 * If it is unbounded on the negative side but bounded on the positive side,
	 * provides a backward iterator from the maximum element that never stops.
	 * If it is unbounded on both sides, iterates over 0, 1, -1, 2, -2, indefinitely.
	 */
	@Override
	public Iterator<Expression> iterator() {
		Iterator<Integer> integerIterator;
		if (noLowerBound()) {
			if (noUpperBound()) {
				integerIterator = 
						new BreadthFirstIterator<Integer>( // alternates between the iterators given
								new IntegerIterator(0), // natural numbers
								new IntegerIterator(-1, 1 /* upper bound, never reached */, -1)); // backwards from -1
			}
			else {
				myAssert(() -> isNumber(nonStrictUpperBound), () -> "Cannot iterate over elements of undefined interval " + this);
				// interval is -infinity..nonStrictUpperBound, start from the latter backwards forever
				integerIterator = new IntegerIterator(nonStrictUpperBound.intValue(), nonStrictUpperBound.intValue() + 1 /* never reached */, -1);
			}
		}
		else {
			if (noUpperBound()) {
				myAssert(() -> isNumber(nonStrictLowerBound), () -> "Cannot iterate over elements of undefined interval " + this);
				// go from integer after strict lower bound, forward forever
				integerIterator = new IntegerIterator(nonStrictLowerBound.intValue());
			}
			else {
				myAssert(() -> isNumber(nonStrictLowerBound) && isNumber(nonStrictUpperBound), () -> "Cannot iterate over elements of undefined interval " + this);
				// just regular interval; note that IntegerIterator takes lower bound inclusive, upper bound exclusive, hence the +1's
				integerIterator = new IntegerIterator(nonStrictLowerBound.intValue(), nonStrictUpperBound.intValue() + 1);
			}
		}
		
		return functionIterator(integerIterator, i -> makeSymbol(i.intValue()));
	}

	@Override
	public boolean contains(Expression uniquelyNamedConstant) {
		boolean result =
				uniquelyNamedConstant.getValue() instanceof Rational
				&& ((Rational) uniquelyNamedConstant.getValue()).isInteger()
				&& (!noLowerBound() || ((Rational) uniquelyNamedConstant.getValue()).compareTo(nonStrictLowerBound) >= 0)
				&& (!noUpperBound() || ((Rational) uniquelyNamedConstant.getValue()).compareTo(nonStrictUpperBound) <= 0);
		return result;
	}

	@Override
	public Expression sampleUniquelyNamedConstant(Random random) {
		myAssert( () -> boundsAreConstants(), () -> "Cannot sample uniquely named constant from integer interval that is infinite and/or defined by variables: " + getName());
		Symbol result = makeSymbol(new Rational(nonStrictLowerBound.intValue() + random.nextInt(nonStrictUpperBound.intValue() - nonStrictLowerBound.intValue() + 1)));
		return result;
	}

	private Expression cachedCardinality = null;
	@Override
	public Expression cardinality() {
		if (cachedCardinality == null) {
			if (noLowerBound() || noUpperBound()) {
				cachedCardinality = INFINITY;
			}
			else if (isNumber(nonStrictLowerBound)) {
				if (isNumber(nonStrictUpperBound)) {
					cachedCardinality = makeSymbol(new Rational(nonStrictUpperBound.intValue() - nonStrictLowerBound.intValue() + 1));			
				}
				else {
					cachedCardinality = apply(MINUS, nonStrictUpperBound, makeSymbol(new Rational(nonStrictLowerBound.intValue() - 1)));
				}
			}
			else {
				if (isNumber(nonStrictUpperBound)) {
					cachedCardinality = apply(MINUS, makeSymbol(new Rational(nonStrictUpperBound.intValue() + 1)), nonStrictLowerBound);
				}
				else {
					cachedCardinality = apply(PLUS, apply(MINUS, nonStrictUpperBound, nonStrictLowerBound), 1);
				}
			}
		}
		return cachedCardinality;
	}

	private boolean noLowerBound() {
		return nonStrictLowerBound.equals(apply(MINUS, INFINITY));
	}

	private boolean noUpperBound() {
		return nonStrictUpperBound.equals(INFINITY);
	}

	private boolean boundsAreConstants() {
		return isNumber(nonStrictLowerBound) && isNumber(nonStrictUpperBound);
	}
	
	@Override
	public String toString() {
		return "]" + nonStrictLowerBound + ", " + nonStrictUpperBound + "[";
	}
}
