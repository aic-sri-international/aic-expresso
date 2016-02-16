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
package com.sri.ai.grinder.sgdpll.tester;

import static com.sri.ai.expresso.helper.Expressions.makeSymbol;
import static com.sri.ai.util.Util.map;

import java.util.Map;
import java.util.Random;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.Type;
import com.sri.ai.expresso.type.IntegerInterval;
import com.sri.ai.grinder.api.Context;
import com.sri.ai.grinder.core.DefaultRewritingProcess;
import com.sri.ai.grinder.library.controlflow.IfThenElse;
import com.sri.ai.grinder.sgdpll.api.ConstraintTheory;
import com.sri.ai.grinder.sgdpll.theory.inequality.InequalityConstraintTheory;
import com.sri.ai.util.base.NullaryFunction;

/**
 * An implementation of {@link NullaryFunction}&lt;{@link Expression}> generating
 * random conditional expressions in a given theory and up to a given depth,
 * with leaves generated by another {@link NullaryFunction}&lt;{@link Expression}>.
 * <p>
 * The conditions of the tree are generated by {@link ConstraintTheory#makeRandomLiteral(Random, Context)}.
 * Typically, a constraint theory will use a default set of variables, constants, and types to generate these
 * literals, and often offer getters and setters to customize these.
 * See the documentation for the particular constraint theory you are using.
 * 
 * @author braz
 *
 */
@Beta
public class RandomConditionalExpressionGenerator implements NullaryFunction<Expression> {
	
	private Random random;
	private ConstraintTheory constraintTheory;
	private int depth;
	private NullaryFunction<Expression> leafGenerator;
	private Context context;
	
	/**
	 * Constructs a random conditional expression generator based on given parameters.
	 * @param random a random generator to be used throughout
	 * @param constraintTheory the constraint theory used to generate the literals in the conditional's conditions
	 * @param depth the depth of the conditional tree
	 * @param leafGenerator a generators for expressions appearing at the leaves of the conditional tree
	 * @param context
	 */
	public RandomConditionalExpressionGenerator(Random random, ConstraintTheory constraintTheory, int depth, NullaryFunction<Expression> leafGenerator, Context context) {
		super();
		this.random = random;
		this.constraintTheory = constraintTheory;
		this.depth = depth;
		this.leafGenerator = leafGenerator;
		this.context = context;
	}

	@Override
	public Expression apply() {
		Expression result = apply(depth);
		return result;
	}

	private Expression apply(int depth) {
		Expression result;
		if (depth == 0) {
			result = leafGenerator.apply();
		}
		else {
			Expression literal = constraintTheory.makeRandomLiteral(random, context);
			result = IfThenElse.make(literal, apply(depth - 1), apply(depth - 1));
		}
		return result;
	}
	
	public static void main(String[] args) {
		Random random = new Random();
		ConstraintTheory constraintTheory = new InequalityConstraintTheory(true, true);
		for (int numberOfVariables = 3; numberOfVariables != 5; numberOfVariables++) {
			Map<String, Type> variableNamesAndTypes = map();
			Type integerInterval = new IntegerInterval(0, 100);
			for (int v = 0; v != numberOfVariables; v++) {
				variableNamesAndTypes.put("v" + v, integerInterval);
			}
			constraintTheory.setVariableNamesAndTypesForTesting(variableNamesAndTypes);
			Context context = constraintTheory.extendWithTestingInformation(new DefaultRewritingProcess());
			RandomConditionalExpressionGenerator generator = 
					new RandomConditionalExpressionGenerator(
							random, 
							constraintTheory, 
							4, 
							() -> makeSymbol(random.nextDouble()), 
							context);
			
			System.out.println();
			System.out.println();
			for (Map.Entry<String, Type> variableNameAndType : variableNamesAndTypes.entrySet()) {
				Type type = variableNameAndType.getValue();
				IntegerInterval interval = (IntegerInterval) type;
				System.out.println(
						"random " + variableNameAndType.getKey() + ": " + 
								interval.getNonStrictLowerBound() + ".." + interval.getNonStrictUpperBound() + ";");
			}
			for (int i = 0; i != 5; i++) {
				Expression output = generator.apply();
				System.out.println(output + ";");
			}
		}
	}
}