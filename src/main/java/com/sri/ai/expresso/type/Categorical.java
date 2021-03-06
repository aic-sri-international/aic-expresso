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
import static com.sri.ai.util.Util.arrayListFrom;
import static com.sri.ai.util.Util.myAssert;
import static com.sri.ai.util.collect.FunctionIterator.functionIterator;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Iterator;
import java.util.Random;
import java.util.Set;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.Type;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.util.collect.FunctionIterator;
import com.sri.ai.util.collect.IntegerIterator;
import com.sri.ai.util.collect.NestedIterator;

/**
 * Represents categorical types.
 * 
 * @author braz
 */
@Beta
public class Categorical extends AbstractType {
	private static final long serialVersionUID = 1L;
	
	private String name;
	private String lowerCaseName;
	private int cardinality;
	private Expression cardinalityExpression;
	private ArrayList<Expression> knownConstants;
	
	/**
	 * Creates a categorical type of given name, cardinality and known constants;
	 * unknown constants, if requested, are named "<name>1", ... "<name>n",
	 * where name is the lower-case version of given name, and n is cardinality minus number of known constants.
	 * @param name
	 * @param cardinality number of elements in type (-1 if unknown, -2 if infinite)
	 * @param knownConstants
	 */
	public Categorical(String name, int cardinality, ArrayList<Expression> knownConstants) {
		myAssert(
				() -> cardinality == -1 || cardinality == -2 || knownConstants.size() <= cardinality,
				() -> "Cardinality of " + name + " is declared to be less than number of known uniquely named constants");
		this.name = name;
		this.lowerCaseName = name.toLowerCase();
		this.cardinality = cardinality;
		if (cardinality == -1) {
			this.cardinalityExpression = Expressions.UNKNOWN;
		}
		else if (cardinality == -2) {
			this.cardinalityExpression = Expressions.INFINITY;
		}
		else {
			this.cardinalityExpression = makeSymbol(cardinality);
		}
		this.knownConstants = knownConstants;
	}

	public Categorical(String name, int cardinality, Expression... knownConstants) {
		this(name, cardinality, new ArrayList<Expression>((Arrays.asList(knownConstants))));
	}
	
	/**
	 * Similar to {@link #Categorical(String, int, ArrayList)},
	 * but known constants are specified how many there are as well as a prefix for their name;
	 * for example, if the prefix is "a" and the number of known constants is specified as 100,
	 * then the known constants of this categorical type will be <code>a1,...,a100</code>.
	 * Currently, this simply creates all known constants explicitly in an array list;
	 * in the future, that may be replaced by some sort of virtual Collection implementation.
	 * @param name
	 * @param cardinality
	 * @param knownConstantPrefix
	 * @param numberOfKnownConstants
	 */
	public Categorical(String name, int cardinality, String knownConstantPrefix, int numberOfKnownConstants) {
		this(
				name,
				cardinality,
				arrayListFrom(
						functionIterator(
								new IntegerIterator(1, numberOfKnownConstants + 1),
								i -> makeSymbol(knownConstantPrefix + i))));
		// TODO: OPTIMIZATION: use some sort of "virtual" Collection implementation to represent this array of expressions.
	}
	
	@Override
	public Iterator<Expression> iterator() {
		return new NestedIterator<>(knownConstants, unknownConstantsIterator());
	}

	private Iterator<Expression> unknownConstantsIterator() {
		return FunctionIterator.make(
				new IntegerIterator(knownConstants.size() + 1, cardinality + 1),
				i -> makeSymbol(lowerCaseName + i));
	}

	@Override
	public boolean contains(Expression constant) {
		boolean result = knownConstants.contains(constant) || isUnknownConstant(constant);
		return result;
	}

	private boolean isUnknownConstant(Expression constant) {
		boolean result;
		try {
			int unknownConstantIndex = -1;
			result = constant.getValue() instanceof String &&
				((String) constant.getValue()).startsWith(lowerCaseName) &&
				(unknownConstantIndex = indexOfUniquelyNamedConstant(constant)) > knownConstants.size() &&
				unknownConstantIndex < cardinality + 1;
		}
		catch (NumberFormatException e) { result = false; }
				
		return result;
	}

	/**
	 * Returns the index of a given unknown constant, or throws a {@link NumberFormatException}.
	 * @param constant
	 * @return
	 */
	protected int indexOfUniquelyNamedConstant(Expression constant) {
		int result = Integer.valueOf(((String) constant.getValue()).substring(lowerCaseName.length())).intValue();
		return result;
	}
	
	@Override
	public boolean isSampleUniquelyNamedConstantSupported() {
		return !(cardinality().equals(Expressions.UNKNOWN) || cardinality().equals(Expressions.INFINITY));
	}

	@Override
	public Expression sampleUniquelyNamedConstant(Random random) {
		myAssert(this::isSampleUniquelyNamedConstantSupported,
				() -> "Sampling of constant not implemented for " + Categorical.class + " with unknown or infinity cardinality");
		
		Expression result;
		int index = random.nextInt(cardinality().intValue()) + 1;
		if (index <= knownConstants.size()) {
			result = knownConstants.get(index - 1);
		}
		else {
			result = makeSymbol(lowerCaseName + index);
		}
		return result;
	}

	@Override
	public Expression cardinality() {
		return cardinalityExpression;
	}
	
	@Override
	public boolean isDiscrete() {
		return true;
	}
	
	@Override
	public boolean isFinite() {
		return cardinality >= 0; // NOTE: -1 if unknown, -2 if infinite
	}

	@Override
	public String toString() {
		return name;
	}
	
	@Override
	public String getName() {
		return name;
	}
	
	@Override
	public Set<Type> getEmbeddedTypes() {
		return Collections.emptySet();
	}
}