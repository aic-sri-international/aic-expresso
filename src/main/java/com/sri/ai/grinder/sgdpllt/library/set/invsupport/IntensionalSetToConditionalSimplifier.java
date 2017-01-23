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

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.IntensionalSet;
import com.sri.ai.grinder.sgdpllt.api.Context;
import com.sri.ai.grinder.sgdpllt.library.boole.ThereExists;
import com.sri.ai.grinder.sgdpllt.library.controlflow.IfThenElse;
import com.sri.ai.grinder.sgdpllt.library.set.Sets;
import com.sri.ai.grinder.sgdpllt.rewriter.api.Simplifier;

/**
 * 
 * <pre>
 * {{ (on Indices in Types) Head | Condition }}    
 * --->
 * C = evaluate(there exists Indices in Types : Condition)
 * return IfThenElse.make(C, {{ (on Indices in Types) Head | Condition }}, {}, true) 
 * </pre>
 * 
 * @author oreilly
 *
 */

// TODO - consider doing an ad hoc caching due to this rewriter requiring 
// the repeated evaluation of there exists Indices in Types : Condition.
// NOTE: this could be tricky as will need to take context into account 
// (in particular with respect to free variables).
public class IntensionalSetToConditionalSimplifier implements Simplifier {
	@Override
	public Expression applySimplifier(Expression expression, Context context) {
		return simplify(expression, context);
	}
	
	public static Expression simplify(Expression expression, Context context) {
		Expression result = expression;
		if (Sets.isIntensionalSet(expression)) {
			IntensionalSet intensionalSet = (IntensionalSet) expression;
			Expression thereExistsCondition = ThereExists.make(intensionalSet.getIndexExpressions(), intensionalSet.getCondition());		
			Expression condition  = context.getTheory().evaluate(thereExistsCondition, context);			
			Expression thenBranch = expression;
			Expression elseBranch = Sets.EMPTY_SET;
			result = IfThenElse.make(condition, thenBranch, elseBranch, true);
		}
		return result;
	}
}