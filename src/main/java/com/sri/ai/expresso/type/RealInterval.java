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
import static com.sri.ai.expresso.helper.Expressions.MINUS_INFINITY;
import static com.sri.ai.expresso.helper.Expressions.ONE;
import static com.sri.ai.expresso.helper.Expressions.ZERO;
import static com.sri.ai.expresso.helper.Expressions.apply;
import static com.sri.ai.expresso.helper.Expressions.isNumber;
import static com.sri.ai.expresso.helper.Expressions.makeSymbol;
import static com.sri.ai.expresso.helper.Expressions.parse;
import static com.sri.ai.grinder.library.FunctorConstants.EQUAL;
import static com.sri.ai.grinder.library.FunctorConstants.GREATER_THAN;
import static com.sri.ai.grinder.library.FunctorConstants.REAL_INTERVAL_CLOSED_CLOSED;
import static com.sri.ai.grinder.library.FunctorConstants.REAL_INTERVAL_CLOSED_OPEN;
import static com.sri.ai.grinder.library.FunctorConstants.REAL_INTERVAL_OPEN_CLOSED;
import static com.sri.ai.grinder.library.FunctorConstants.REAL_INTERVAL_OPEN_OPEN;
import static com.sri.ai.util.Util.myAssert;

import java.util.Collections;
import java.util.Iterator;
import java.util.Random;
import java.util.Set;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.Symbol;
import com.sri.ai.expresso.api.Type;
import com.sri.ai.grinder.library.controlflow.IfThenElse;
import com.sri.ai.grinder.library.number.UnaryMinus;
import com.sri.ai.util.math.Rational;

/**
 * Represents a Real interval.
 * 
 * @author braz
 */
@Beta
public class RealInterval extends AbstractType {
	private static final long serialVersionUID = 1L;

	private String cachedString;

	private Expression lowerBound;
	private Expression upperBound;
	private boolean lowerBoundIsOpen;
	private boolean upperBoundIsOpen;
	
	@Override
	public String getName() {
		return toString();
	}
	
	public RealInterval(String name) {
		Expression nameParse = parse(name);
		
		if (nameParse == null) {
			throw new Error("Trying to build real interval from string '" + name + "' which does not parse as a real interval description (format is '[a;b]'");
		}
		
		if (nameParse.equals("Real")) {
			lowerBound = UnaryMinus.make(INFINITY);
			upperBound = INFINITY;
			lowerBoundIsOpen = true;
			upperBoundIsOpen = true;
		}
		else if (nameParse.hasFunctor(REAL_INTERVAL_OPEN_OPEN) && nameParse.numberOfArguments() == 2) {
			lowerBound = nameParse.get(0);
			upperBound = nameParse.get(1);
			lowerBoundIsOpen = true;
			upperBoundIsOpen = true;
		}
		else if (nameParse.hasFunctor(REAL_INTERVAL_OPEN_CLOSED) && nameParse.numberOfArguments() == 2) {
			lowerBound = nameParse.get(0);
			upperBound = nameParse.get(1);
			lowerBoundIsOpen = true;
			upperBoundIsOpen = false;
		}
		else if (nameParse.hasFunctor(REAL_INTERVAL_CLOSED_OPEN) && nameParse.numberOfArguments() == 2) {
			lowerBound = nameParse.get(0);
			upperBound = nameParse.get(1);
			lowerBoundIsOpen = false;
			upperBoundIsOpen = true;
		}
		else if (nameParse.hasFunctor(REAL_INTERVAL_CLOSED_CLOSED) && nameParse.numberOfArguments() == 2) {
			lowerBound = nameParse.get(0);
			upperBound = nameParse.get(1);
			lowerBoundIsOpen = false;
			upperBoundIsOpen = false;
		}
		else {
			throw new Error(this.getClass() + " created with invalid name " + name + ". Must be either 'Real' or '] lowerBound ; upperBound ['  or '] lowerBound ; upperBound ]'  or '[ lowerBound ; upperBound ['  or '[ lowerBound ; upperBound ]'");
		}
	}
	
	public RealInterval(Expression lowerBound, Expression upperBound, boolean lowerBoundIsOpen, boolean upperBoundIsOpen) {
		this.lowerBound = lowerBound;
		this.upperBound = upperBound;
		this.lowerBoundIsOpen = lowerBoundIsOpen;
		this.upperBoundIsOpen = upperBoundIsOpen;
	}

	public Expression getLowerBound() {
		return lowerBound;
	}

	public Expression getUpperBound() {
		return upperBound;
	}

	public boolean lowerBoundIsOpen() {
		return lowerBoundIsOpen;
	}

	public boolean upperBoundIsOpen() {
		return upperBoundIsOpen;
	}

	/**
	 * Throws an error because a real interval is not enumerable.
	 */
	@Override
	public Iterator<Expression> iterator() {
		throw new Error("Real intervals cannot be enumerated.");
	}

	@Override
	public boolean contains(Expression uniquelyNamedConstant) {
		boolean result;
		if (uniquelyNamedConstant.getValue() instanceof Rational) {
			Rational value = (Rational) uniquelyNamedConstant.getValue();
			result =
					(noLowerBound() || value.compareTo(lowerBound.getValue()) > 0 || (!lowerBoundIsOpen && value.compareTo(lowerBound.getValue()) == 0))
					&&
					(noUpperBound() || value.compareTo(upperBound.getValue()) < 0 || (!upperBoundIsOpen && value.compareTo(upperBound.getValue()) == 0));
		}
		else {
			result = false;
		}
		return result;
	}
	
	public boolean isSuperset(Expression lowerBound, Expression upperBound) {
		boolean result = true;
		if (lowerBound.equals(MINUS_INFINITY)) {
			if (!noLowerBound()) {
				result = false;
			}
		}
		else if (!lowerBound.equals(INFINITY)) { // if lower bound is infinity then upper bound must be infinity
			if (!contains(lowerBound)) {
				result = false;
			}
		}
		
		// If we know lower bound is good, then check upper bound
		if (result) {
			if (upperBound.equals(INFINITY)) {
				if (!noUpperBound()) {
					result = false;
				}
			}
			else if (!upperBound.equals(MINUS_INFINITY)) { // if upper bound is -infinity then lower bound must be the same and will have been tested
				if (!contains(upperBound)) {
					result = false;
				}
			}
		}
		
		return result;
	}

	private final int SAMPLING_RESOLUTION = 1000000;
	
	@Override
	public boolean isSampleUniquelyNamedConstantSupported() {
		return boundsAreConstants();
	}
	
	@Override
	public Expression sampleUniquelyNamedConstant(Random random) {
		myAssert( () -> boundsAreConstants(), () -> "Cannot sample uniquely named constant from real interval that is infinite and/or defined by variables: " + getName());
		
		// We split the interval into NUMBER_OF_SAMPLING_POINTS contiguous sub-intervals, delimited by (NUMBER_OF_SAMPLING_POINTS + 1) points.
		// We then pick one of these points, but exclude the extreme ones depending on whether bounds are open or closed. 
		int sampledPoint = (lowerBoundIsOpen? 1 : 0) + random.nextInt(getNumberOfAllowedSamplingPoints());

		// We compute the sampled element of the interval
		Rational intervalFraction = new Rational(sampledPoint, SAMPLING_RESOLUTION);
		Rational sampledElement = lowerBound.rationalValue().add(intervalFraction.multiply(getLength()));
		Symbol result = makeSymbol(sampledElement);
		return result;
	}

	private int cachedNumberOfAllowedSamplingPoints = -1;
	
	private int getNumberOfAllowedSamplingPoints() {
		if (cachedNumberOfAllowedSamplingPoints == -1) {
			cachedNumberOfAllowedSamplingPoints = SAMPLING_RESOLUTION + 1 - (lowerBoundIsOpen? 1 : 0) - (upperBoundIsOpen? 1 : 0);
		}
		return cachedNumberOfAllowedSamplingPoints;
	}
	
	private Rational cachedLength = null;
	
	private Rational getLength() {
		if (cachedLength == null) {
			cachedLength = upperBound.rationalValue().subtract(lowerBound.rationalValue());
		}
		return cachedLength;
	}
	
	private Expression cachedCardinality = null;
	@Override
	public Expression cardinality() {
		if (cachedCardinality == null) {
			if (noLowerBound() || noUpperBound()) {
				cachedCardinality = INFINITY;
			}
			else if (boundsAreConstants()) {
				Rational difference = upperBound.rationalValue().subtract(lowerBound.rationalValue());
				if (difference.isPositive()) {
					cachedCardinality = INFINITY;
				}
				else if (difference.isZero()) { // they are equal
					if (lowerBoundIsOpen || upperBoundIsOpen) {
						// bounds are not allowed to be equal
						cachedCardinality = ZERO;
					}
					else {
						// bounds are equal, and allowed to be; their value is the single value in interval
						cachedCardinality = ONE;
					}
				}
				else { // upper bound is less than lower bound
					cachedCardinality = ZERO;
				}
			}
			else {
				cachedCardinality = cardinalityWhenOneOfTheBoundsIsNotANumber();
			}
		}
		return cachedCardinality;
	}
	
	@Override
	public boolean isDiscrete() {
		return false;
	}
	
	@Override
	public boolean isFinite() {
		return false;
	}

	private Expression cardinalityWhenOneOfTheBoundsIsNotANumber() {
		if (lowerBoundIsOpen || upperBoundIsOpen) {
			// bounds are not allowed to be equal
			cachedCardinality =
					IfThenElse.make(apply(GREATER_THAN, lowerBound, upperBound), INFINITY, ZERO);
		}
		else {
			cachedCardinality =
					IfThenElse.make(
							apply(EQUAL, lowerBound, upperBound),
							ONE,
							IfThenElse.make(apply(GREATER_THAN, lowerBound, upperBound), INFINITY, ZERO));
		}
		return cachedCardinality;
	}

	public boolean noLowerBound() {
		return lowerBound.equals(MINUS_INFINITY);
	}

	public boolean noUpperBound() {
		return upperBound.equals(INFINITY);
	}

	public boolean boundsAreConstants() {
		return isNumber(lowerBound) && isNumber(upperBound);
	}
	
	@Override
	public String toString() {
		if (cachedString == null) {
			cachedString =
					(lowerBoundIsOpen? "]" : "[") + 
					lowerBound +
					"; " + 
					upperBound + 
					(upperBoundIsOpen? "[" : "]");
//			if (lowerBoundIsOpen && upperBoundIsOpen) {
//				cachedString = FunctorConstants.REAL_INTERVAL_OPEN_OPEN + "(" + lowerBound + ", " + upperBound + ")";
//			}
//			else if (lowerBoundIsOpen && !upperBoundIsOpen) {
//				cachedString = FunctorConstants.REAL_INTERVAL_OPEN_CLOSED + "(" + lowerBound + ", " + upperBound + ")";
//			}
//			else if (!lowerBoundIsOpen && upperBoundIsOpen) {
//				cachedString = FunctorConstants.REAL_INTERVAL_CLOSED_OPEN + "(" + lowerBound + ", " + upperBound + ")";
//			}
//			else if (!lowerBoundIsOpen && !upperBoundIsOpen) {
//				cachedString = FunctorConstants.REAL_INTERVAL_CLOSED_CLOSED + "(" + lowerBound + ", " + upperBound + ")";
//			}
		}
		return cachedString;
	}
	
	@Override
	public Set<Type> getEmbeddedTypes() {
		return Collections.emptySet();
	}
}
