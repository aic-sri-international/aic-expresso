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
package com.sri.ai.grinder.sgdpllt.group;

import java.util.Random;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.IndexExpressionsSet;
import com.sri.ai.grinder.sgdpllt.api.Context;
import com.sri.ai.util.base.Pair;

/**
 * Object representing an associative commutative group.
 * 
 * @author braz
 *
 */
@Beta
public interface AssociativeCommutativeGroup {
	
	/** The group identity element. */
	Expression additiveIdentityElement();

	/**
	 * Indicates whether given value is an absorbing element of the group's additive operation,
	 * that is, using the additive operation on it with any other value will produce itself.
	 * The reason this is a test on a value instead of a method providing the absorbing element
	 * is that a group is not required to have one, in which case a test can just return false 
	 * every time.
	 */
	boolean isAdditiveAbsorbingElement(Expression value);
	
	/**
	 * Performs the group's additive operation on two values.
	 */
	Expression add(Expression value1, Expression value2, Context context);

	/**
	 * The result of adding a value to itself n times, where both value and n may be symbolic expressions.
	 */
	Expression addNTimes(Expression value, Expression n, Context context);

	/** Indicates whether group's operator is idempotent. */
	boolean isIdempotent();
	
	/**
	 * Makes a random constant in the group's set of elements.
	 * This is useful for random test generation.
	 * @return
	 */
	Expression makeRandomConstant(Random random);

	/**
	 * Gets an expression passed to a rewriter solving this type of problem, and returns a pair containing the expression
	 * and indices for DPLL to solve.
	 * The index types are assumed to be stored in the context.
	 * @param expression
	 * @param context
	 * @return
	 */
	Pair<Expression, IndexExpressionsSet>
	getExpressionAndIndexExpressionsFromProblemExpression(Expression expression, Context context);

	/**
	 * Generates an expression representing of problem of this type, given its components. 
	 * @param index
	 * @param indexTypeExpression
	 * @param constraint
	 * @param body
	 * @return
	 */
	Expression makeProblemExpression(Expression index, Expression indexTypeExpression, Expression constraint, Expression body);
}
