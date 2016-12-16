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

import java.util.List;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.Tuple;
import com.sri.ai.grinder.sgdpllt.api.Context;
import com.sri.ai.grinder.sgdpllt.library.Equality;
import com.sri.ai.grinder.sgdpllt.library.boole.And;
import com.sri.ai.grinder.sgdpllt.rewriter.api.Simplifier;
import com.sri.ai.util.Util;

/**
 * A simplifier that will rewrite equalities between tuple expressions as follows:<br>
 * 
 * <pre>
 * (t_1,...,t_n) = (r_1,...,r_n)
 * ---->
 * t_1 = r_1 and ... and t_n = r_n
 * </pre>
 * 
 * @author oreilly
 */
public class TupleEquality implements Simplifier {

	@Override
	public Expression applySimplifier(Expression expression, Context context) {
		return simplify(expression, context);
	}
	
	public static Expression simplify(Expression expression, Context context) {
		Expression result = expression;
		if (Equality.isEquality(expression) && expression.numberOfArguments() == 2) {
			Expression leftHS  = expression.get(0);
			Expression rightHS = expression.get(1);
			// If we have: (t_1,...,t_n) = (r_1,...,r_n)
			if (Tuple.isTuple(leftHS) && Tuple.isTuple(rightHS) && leftHS.numberOfArguments() == rightHS.numberOfArguments()) {
				// Then we want to rewrite it as: 
				// t_1 = r_1 and ... and t_n = r_n
				List<Expression> conjuncts = Util.zipWith((tn, rn) -> Equality.make(tn, rn), leftHS.getArguments(), rightHS.getArguments());
				result = And.make(conjuncts);
			}
		}
		return result;
	}
}
