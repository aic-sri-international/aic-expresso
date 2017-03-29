/*
 * Copyright (c) 2017, SRI International
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
package com.sri.ai.grinder.sgdpllt.library.set.invsupport;

import java.util.ArrayList;
import java.util.List;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.grinder.sgdpllt.api.Context;
import com.sri.ai.grinder.sgdpllt.library.FunctorConstants;
import com.sri.ai.grinder.sgdpllt.library.set.Sets;
import com.sri.ai.grinder.sgdpllt.rewriter.api.Simplifier;

/**
 * Distribute intersection over union:
 * <pre>
 * Set1 intersection (Set2 union ... union Set_m)
 * --->
 * evaluate(Set1 intersection Set2) 
 * union 
 * ... 
 * union 
 * evaluate(Set1 intersection Set_m), again with short-circuiting
 * 
 * </pre>
 * 
 * @author oreilly
 */
public class DistributeIntersectionOverUnionSimplifier implements Simplifier {
	@Override
	public Expression applySimplifier(Expression expression, Context context) {
		return simplify(expression, context);
	}

	public static Expression simplify(Expression expression, Context context) {
		Expression result = expression;

		if (expression.hasFunctor(FunctorConstants.INTERSECTION)) {
			Expression       setArg      = null;
			Expression       unionArg    = null;
			List<Expression> otherArgs   = new ArrayList<>();
			boolean          emptySetArg = false;
			for (Expression intersectionArg : expression.getArguments()) {
				if (Sets.isEmptySet(intersectionArg)) {
					emptySetArg = true; // short-circuit
					break;
				}				
				if (unionArg == null && intersectionArg.hasFunctor(FunctorConstants.UNION)) {
					unionArg = intersectionArg;
				}
				else if (setArg == null && Sets.isSetLikeExpression(intersectionArg)) {
					setArg = intersectionArg;
				}
				else {
					otherArgs.add(intersectionArg);
				}
			}
			if (emptySetArg) {
				result = Sets.EMPTY_SET;
			}
			else if (setArg != null && unionArg != null) {
				List<Expression> resultUnionArgs = new ArrayList<>();
				for (Expression unionArgArg : unionArg.getArguments()) {
					List<Expression> intersectionArgs = new ArrayList<>();
					intersectionArgs.add(setArg);
					// Flatten intersection
					if (unionArgArg.hasFunctor(FunctorConstants.INTERSECTION)) {
						intersectionArgs.addAll(unionArgArg.getArguments());
					}
					else {
						intersectionArgs.add(unionArgArg);
					}
					Expression intersection   = Expressions.apply(FunctorConstants.INTERSECTION, intersectionArgs);
					Expression resultUnionArg = context.getTheory().evaluate(intersection, context);					
					resultUnionArgs.add(resultUnionArg);			
				}
				Expression resultUnion = Sets.makeUnion(resultUnionArgs.toArray(new Expression[resultUnionArgs.size()]));
				if (otherArgs.size() == 0) {
					result = resultUnion;
				}
				else {
					List<Expression> intersectionArgs = new ArrayList<>();
					intersectionArgs.add(resultUnion);
					for (Expression otherArg : otherArgs) {
						// Flatten intersections.
						if (otherArg.hasFunctor(FunctorConstants.INTERSECTION)) {
							intersectionArgs.addAll(otherArg.getArguments());
						}
						else {
							intersectionArgs.add(otherArg);
						}
					}
					Expression intersection = Sets.makeIntersection(intersectionArgs.toArray(new Expression[intersectionArgs.size()]));
					// We have an intersection so recursively call self
					// to see if we can distribute again.
					result = simplify(intersection, context);
				}
			}
		}
		
		return result;
	}
}