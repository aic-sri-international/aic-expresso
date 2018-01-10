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
package com.sri.ai.grinder.library.set.invsupport;

import java.util.ArrayList;
import java.util.List;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.IntensionalSet;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.grinder.api.Context;
import com.sri.ai.grinder.library.FunctorConstants;
import com.sri.ai.grinder.library.set.Sets;
import com.sri.ai.grinder.rewriter.api.Simplifier;

/**
 * <pre>
 * We will have intensional unions, which we need to convert to unions
 * of intensional sets of the base type (in the paper, tuples): 
 *
 * Base case: Union( {{ (on I)   { T } : C }} )  --->   {{ (on I) T : C }} 
 * 
 * Recursion: Union( {{ (on I)   Set1 union ... union Set_m : C }} ) 
 *         = 
 *         recurse(Union( {{ (on I) Set1 : C }} )) 
 *         union ... union 
 *         recurse(Union( {{ (on I) Set_m : C }} )) 
 * </pre>
 * 
 * @author oreilly
 *
 */
public class IntensionalUnionToUnionsOfIntensionalSetsOfBaseTypeSimplifier implements Simplifier {
	@Override
	public Expression applySimplifier(Expression expression, Context context) {
		return simplify(expression, context);
	}
	
	public static Expression simplify(Expression expression, Context context) {
		Expression result = expression;	
		if (Sets.isIntensionalUnion(expression)) {
			IntensionalSet intensionalMultiSet = (IntensionalSet) expression.get(0);
			Expression     intensionalHead     = intensionalMultiSet.getHead();
			// Determine if base case:
			// Union( {{ (on I)   { T } : C }} )  
			// --->   
			// {{ (on I) T : C }} 
// TODO - base case set is an extensional uniset?			
			if (Sets.isExtensionalSet(intensionalHead) && intensionalHead.numberOfArguments() == 1) {
				Expression baseTypeHead = intensionalHead.get(0);
				result = IntensionalSet.intensionalMultiSet(intensionalMultiSet.getIndexExpressions(), baseTypeHead, intensionalMultiSet.getCondition());
			}
			// Determine if recursive case
			else if (intensionalHead.hasFunctor(FunctorConstants.UNION)) { 
				// Union( {{ (on I)   Set1 union ... union Set_m : C }} )
				// --->
				// recurse(Union( {{ (on I) Set1 : C }} )) 
				//         union ... union 
				// recurse(Union( {{ (on I) Set_m : C }} ))
				List<Expression> recursedUnionArgs = new ArrayList<>();
				for (Expression setI : intensionalHead.getArguments()) {
					Expression setIUnionArg      = IntensionalSet.intensionalMultiSet(intensionalMultiSet.getIndexExpressions(), setI, intensionalMultiSet.getCondition());
					Expression setIUnion         = Expressions.apply(FunctorConstants.INTENSIONAL_UNION, setIUnionArg);					
					Expression setIRecurseResult = simplify(setIUnion, context);					
					recursedUnionArgs.add(setIRecurseResult);
				}
				result = Sets.makeUnion(recursedUnionArgs.toArray(new Expression[recursedUnionArgs.size()]));
			}
		}
		return result;
	}
}