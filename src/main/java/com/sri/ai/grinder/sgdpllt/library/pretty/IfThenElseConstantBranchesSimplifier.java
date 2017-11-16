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
package com.sri.ai.grinder.sgdpllt.library.pretty;

import static com.sri.ai.expresso.helper.Expressions.FALSE;
import static com.sri.ai.expresso.helper.Expressions.TRUE;
import static com.sri.ai.grinder.sgdpllt.library.controlflow.IfThenElse.isIfThenElse;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.sgdpllt.api.Context;
import com.sri.ai.grinder.sgdpllt.library.boole.And;
import com.sri.ai.grinder.sgdpllt.library.boole.Not;
import com.sri.ai.grinder.sgdpllt.library.boole.Or;
import com.sri.ai.grinder.sgdpllt.library.controlflow.IfThenElse;
import com.sri.ai.grinder.sgdpllt.rewriter.api.Simplifier;

/**
 * Simplifies
 * <ul>
 * <li><code>if C then true  else D ---> C or D</code>
 * <li><code>if C then false else D ---> not C and D</code>
 * <li><code>if C then D else true  ---> not C or D</code>
 * <li><code>if C then D else false ---> C and D</code>
 * </ul>
 * @author braz
 *
 */
@Beta
public class IfThenElseConstantBranchesSimplifier implements Simplifier {

	@Override
	public Expression applySimplifier(Expression expression, Context context) {
		Expression result;
		if (isIfThenElse(expression)) {
			result = simplifyIfThenElse(expression);
		}
		else {
			result = expression;
		}
		return result;
	}

	private Expression simplifyIfThenElse(Expression expression) {
		Expression result;
		Expression condition = IfThenElse.condition(expression);
		Expression thenBranch = IfThenElse.thenBranch(expression);
		Expression elseBranch = IfThenElse.elseBranch(expression);
		
		if (thenBranch.equals(TRUE)) {
			result = Or.make(condition, elseBranch);
		}
		else if (thenBranch.equals(FALSE)) {
			result = And.make(Not.make(condition), elseBranch);
		}
		else if (elseBranch.equals(TRUE)) {
			result = Or.make(Not.make(condition), thenBranch);
		}
		else if (elseBranch.equals(FALSE)) {
			result = And.make(condition, thenBranch);
		}
		else {
			result = expression;
		}
		return result;
	}
}