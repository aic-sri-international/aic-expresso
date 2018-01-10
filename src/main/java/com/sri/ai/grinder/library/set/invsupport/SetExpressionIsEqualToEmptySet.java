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

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.grinder.api.Context;
import com.sri.ai.grinder.library.FunctorConstants;
import com.sri.ai.grinder.library.controlflow.IfThenElse;
import com.sri.ai.grinder.library.set.Sets;
import com.sri.ai.grinder.rewriter.api.Simplifier;

/**
 * Has the following pseudo-code: 
 * <pre>
 * if expression is SetExpression = {}
 *     setDNF = setDNFRewriter.apply(expression) // setDNFRewriter defined below
 *     return result <- replace leaves of setDNF equal to {} by "true", and other leaves by "false" 
 *
 * </pre>
 * 
 * @author oreilly
 *
 */
public class SetExpressionIsEqualToEmptySet implements Simplifier {
	@Override
	public Expression applySimplifier(Expression expression, Context context) {
		return simplify(expression, context);
	}

	public static Expression simplify(Expression expression, Context context) {
		Expression result = expression;
		
		if (expression.hasFunctor(FunctorConstants.EQUALITY) && expression.numberOfArguments() == 2) {
			Expression setExpression = null;
			Expression emptySet      = null;
			for (Expression equalityArg : expression.getArguments()) {				
				if (emptySet == null && Sets.isEmptySet(equalityArg)) {
					emptySet = equalityArg;
				}
				else if (setExpression == null && Sets.isSetLikeExpression(equalityArg)) {
					setExpression = equalityArg;
				}
			}
			if (setExpression != null && emptySet != null) {
				SetDNFRewriter setDNFRewriter = new SetDNFRewriter();
				Expression setDNF = setDNFRewriter.apply(expression, context);				
				result = replaceLeaves(setDNF);
			}
		}
		
		return result;
	}
	
	private static Expression replaceLeaves(Expression setDNF) {
		Expression result = setDNF;
		if (IfThenElse.isIfThenElse(setDNF)) {			
			Expression thenBranch = IfThenElse.thenBranch(setDNF);							
			Expression elseBranch = IfThenElse.elseBranch(setDNF);
			
			Expression updatedThenBranch = replaceLeaves(thenBranch);
			Expression updatedElseBranch = replaceLeaves(elseBranch);
			
			if (updatedThenBranch != thenBranch || updatedElseBranch != elseBranch) {
				result = IfThenElse.make(IfThenElse.condition(setDNF), updatedThenBranch, updatedElseBranch);
			}
		}
		else {
			result = updatePossibleLeaf(setDNF);
		}
		
		return result;
	}
	
	private static Expression updatePossibleLeaf(Expression possibleLeaf) {
		Expression result = possibleLeaf;
		if (!IfThenElse.isIfThenElse(possibleLeaf) 
				&& !Expressions.TRUE.equals(possibleLeaf) 
				&& !Expressions.FALSE.equals(possibleLeaf)) {
			if (Sets.isEmptySet(possibleLeaf)) {
				result = Expressions.TRUE;
			}
			else {
				result = Expressions.FALSE;
			}
		}
		
		return result;
	}
}
