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
package com.sri.ai.grinder.library.equality;

import java.util.ArrayList;
import java.util.List;
import java.util.Random;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.grinder.library.Equality;
import com.sri.ai.grinder.library.FunctorConstants;
import com.sri.ai.util.Util;
import com.sri.ai.util.collect.EZIterator;

/**
 * An iterator of random equality formulas, generated according to certain parameters
 * (see {@link #RandomEqualityFormulaGenerator(int, int, int, int)} for their description).
 * 
 * @author braz
 *
 */
public class RandomEqualityFormulaGenerator extends EZIterator<Expression> {
	
	private Random random;
	private int numberOfVariables, numberOfConstants, depth, breadth;
	private List<Expression> constantsAndVariables;
	private List<Expression> functors = Util.list(Expressions.makeSymbol(FunctorConstants.AND), Expressions.makeSymbol(FunctorConstants.OR), Expressions.makeSymbol(FunctorConstants.NOT));

	/**
	 * Creates an iterator over random equality formulas.
	 * A formula has equal probability of being a conjunction, disjunction or negation.
	 * 
	 * @param random a {@ink Random} number generator.
	 * @param numberOfVariables the (maximum) number of variables in the formula.
	 * @param numberOfConstants the (maximum) number of constants in the formula.
	 * @param depth the depth of the formula (all its sub-expressions with have depth equal to <code>depth - 1</code>).
	 * @param breadth the number of sub-expressions of conjunctions and disjunctions.
	 */
	public RandomEqualityFormulaGenerator(Random random, int numberOfVariables, int numberOfConstants, int depth, int breadth) {
		super();
		this.random = random;
		this.numberOfVariables = numberOfVariables;
		this.numberOfConstants = numberOfConstants;
		this.depth = depth;
		this.breadth = breadth;
		
		constantsAndVariables = new ArrayList<Expression>(numberOfConstants + numberOfVariables);
		for (int i = 0; i != numberOfConstants; i++) {
			constantsAndVariables.add(Expressions.makeSymbol("a" + i));
		}
		for (int i = 0; i != numberOfVariables; i++) {
			constantsAndVariables.add(Expressions.makeSymbol("X" + i));
		}
	}
	
	/**
	 * Returns a list with variables potentially used by generator (in constant time).
	 */
	public List<Expression> getVariables() {
		List<Expression> result = constantsAndVariables.subList(numberOfConstants, numberOfConstants + numberOfVariables);
		return result;
	}

	@Override
	protected Expression calculateNext() {
		Expression result = generate(depth);
		return result;
	}

	private Expression generate(int depth) {
		
		Expression result = null;
		
		if (depth == 0) {
			List<Expression> twoTerms = Util.randomPick(2, random, constantsAndVariables);
			result = Equality.make(twoTerms);
		}
		else {
			Expression            functor                = Util.randomPick(random, functors);
			int                   numberOfSubExpressions = functor.equals(FunctorConstants.NOT)? 1 : breadth;
			ArrayList<Expression> subExpressions         = new ArrayList<Expression>(numberOfSubExpressions);
			for (int i = 0; i != numberOfSubExpressions; i++) {
				Expression newSubExpression = generate(depth - 1);
				subExpressions.add(newSubExpression);
			}
			result = Expressions.apply(functor, subExpressions);
		}
		
		return result;
	}
	
	/** A simple test sampling 10 random formulas and printing them to the standard output. */
	public static void main(String[] args) {
		RandomEqualityFormulaGenerator iterator = new RandomEqualityFormulaGenerator(new Random(), 10, 5, 2, 2);
		for (int i = 0; i != 10; i++) {
			System.out.println(iterator.next());	
		}
	}
}
