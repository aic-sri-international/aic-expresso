/*
 * Copyright (c) 2016, SRI International
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
package com.sri.ai.grinder.sgdpllt.theory.tuple.rewriter;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.Tuple;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.grinder.sgdpllt.api.Context;
import com.sri.ai.grinder.sgdpllt.library.Equality;
import com.sri.ai.grinder.sgdpllt.library.FunctorConstants;
import com.sri.ai.grinder.sgdpllt.library.controlflow.IfThenElse;
import com.sri.ai.grinder.sgdpllt.rewriter.api.Simplifier;

/**
 * A simplifier that will rewrite a 'get' function application on a tuple to a conditional:<br>
 * 
 * <pre>
 * get((t_1,...,t_n), i)
 * ---->
 * if i = 1 then t_1 else if i = 2 then t_2 else ... t_n
 *    
 * with a shortcut if i is a constant.
 * 
 * </pre>
 * 
 * @author oreilly
 *
 */
public class TupleGetSimplifier implements Simplifier {

	@Override
	public Expression applySimplifier(Expression expression, Context context) {
		return simplify(expression, context);
	}
	
	public static Expression simplify(Expression expression, Context context) {
		Expression result = expression;
		
		if (Expressions.hasFunctor(expression, FunctorConstants.GET) 
				&& expression.numberOfArguments() == 2) {
			Expression tupleArg = expression.get(0);
			Expression indexArg = expression.get(1);
			if (Tuple.isTuple(tupleArg)) {
				// NOTE: Tuple's are indexed starting from 1 not 0.
// TODO - do we really want this ^^^
				if (Expressions.isNumber(indexArg)) {
					// We have a constant
					int constantIndex = indexArg.intValueExact();
					if (constantIndex > 0 && constantIndex <= tupleArg.numberOfArguments()) {
						result = tupleArg.get(constantIndex -1); // NOTE: the API is 0 based.
					}
					else {
						throw new IndexOutOfBoundsException("Index "+indexArg+" is out of bounds on "+tupleArg);
					}
				}
				else {
					// Construct a conditional
					result = constructConditional(tupleArg, indexArg, 0);
				}
			}
		}
		

		return result;
	}
	
	private static Expression constructConditional(Expression tuple, Expression indexArg, int index) {
		Expression result;
		if (index < (tuple.numberOfArguments() -1)) {
			Expression thenBranch = tuple.get(index);
			Expression elseBranch = constructConditional(tuple, indexArg, index+1);
			result = IfThenElse.make(Equality.make(indexArg, Expressions.makeSymbol(index+1)), thenBranch, elseBranch);
		}
		else {
			result = tuple.get(index);
		}
		return result;
	}
}
