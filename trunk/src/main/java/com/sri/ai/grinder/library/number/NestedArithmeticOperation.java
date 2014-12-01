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
package com.sri.ai.grinder.library.number;

import java.util.ArrayList;
import java.util.List;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.core.AbstractRewriter;
import com.sri.ai.grinder.library.CommutativeAssociative;
import com.sri.ai.grinder.library.FunctorConstants;
import com.sri.ai.util.base.Pair;

/**
 * A rewriter to simplify several nested arithmetical expressions. 
 * It simplifies the following case:
 * 		(A-B)-C is rewritten as A-(B+C)
 * 		A-(B-C) is rewritten as (A+C)-B
 * 		(A1+...+An+C1+...+Ck) - (B1+...+Bm+C1+...+Ck) is rewritten as (A1+...+An) - (B1+...+Bm)
 * 
 * 		(A/B)/C is rewritten as A/(B*C)
 * 		A/(B/C) is rewritten as (A*C)/B
 * 		(A1*...*An*C1*...*Ck) / (B1*...*Bm*C1*...*Ck) is rewritten as (A1*...*An) / (B1*...*Bm)
 * 
 * 		(A1+...An+(B1-C1)+...+(Bk-Ck)) is rewritten as (A1+...An+B1+...+Bk) - (C1+...+Ck)
 * 
 * 		(A1*...An*(B1/C1)*...*(Bk/Ck)) is rewritten as (A1*...An*B1*...*Bk) / (C1*...*Ck)
 * 
 * @author saadati
 *
 */
@Beta
public class NestedArithmeticOperation extends AbstractRewriter {
	@Override
	public Expression rewriteAfterBookkeeping(Expression expression, RewritingProcess process) {
		Expression result = expression;
		if ((expression.hasFunctor(FunctorConstants.MINUS) || expression.hasFunctor(FunctorConstants.DIVISION)) && expression.numberOfArguments() == 2) {
			Expression functor = expression.getFunctor();
			Pair<String, Expression> pair = getInverseAndIdentity(functor);
			String inverseFunctor = pair.first;
			Expression identity = pair.second;
			Expression first = expression.get(0);
			Expression second = expression.get(1);

			if ( first.hasFunctor(functor) ) { // expression is of the form (A-B)-C, or (A/B)/C
				Expression newFirst = first.get(0);
				Expression newSecond = Expressions.apply(inverseFunctor, first.get(1), second);
				result = Expressions.apply(functor, newFirst, newSecond);
			} 
			else if ( second.hasFunctor(functor) ) { // expression is of the form A-(B-C) , or A/(B/C)
				Expression newFirst = Expressions.apply(inverseFunctor, first, second.get(1));
				Expression newSecond = second.get(0);
				result = Expressions.apply(functor, newFirst, newSecond);
			} 
			else if ( first.hasFunctor(inverseFunctor) || second.hasFunctor(inverseFunctor) ) { // expression is of them form (A1+...+An) - (B1+...+Bm) 
				List<Expression> leftArguments  = new ArrayList<Expression>();
				List<Expression> rightArguments = new ArrayList<Expression>();
				if ( first.hasFunctor(inverseFunctor) ) {
					leftArguments.addAll(first.getArguments());
				} 
				else {
					leftArguments.add(first);
				}
				
				if ( second.hasFunctor(inverseFunctor) ) {
					rightArguments.addAll(second.getArguments());
				} 
				else {
					rightArguments.add(second);
				}
				List<Expression> duplicateArguments  = new ArrayList<Expression>();
				for (Expression rArg: rightArguments) {
					if ( leftArguments.contains(rArg) ) {
						duplicateArguments.add(rArg);
						leftArguments.remove(rArg);
					}
				}
				if ( !duplicateArguments.isEmpty() ) {
					for (Expression dup: duplicateArguments) {
						rightArguments.remove(dup);
					}
					Expression newFirst = CommutativeAssociative.make(inverseFunctor, leftArguments, identity, false);
					Expression newSecond = CommutativeAssociative.make(inverseFunctor, rightArguments, identity, false);
					result = Expressions.apply(functor, newFirst, newSecond);
				}
			}
		} 
		else if ( expression.hasFunctor(FunctorConstants.PLUS) || expression.hasFunctor(FunctorConstants.TIMES) ) {
			Expression functor = expression.getFunctor();
			Pair<String, Expression> pair = getInverseAndIdentity(functor);
			String inverseFunctor = pair.first;
			Expression identity = pair.second;
			List<Expression> leftArguments  = new ArrayList<Expression>();
			List<Expression> rightArguments = new ArrayList<Expression>();
			for (Expression term: expression.getArguments()) {
				if ( term.hasFunctor(inverseFunctor) ) {
					leftArguments.add(term.get(0));
					rightArguments.add(term.get(1));
				} 
				else {
					leftArguments.add(term);					
				}
			}
			if ( !rightArguments.isEmpty() ) { // expression is of the form (A + ... + (C-D) + ... + E)
				Expression newFirst = CommutativeAssociative.make(functor, leftArguments, identity, false);
				Expression newSecond = CommutativeAssociative.make(functor, rightArguments, identity, false);
				result = Expressions.apply(inverseFunctor, newFirst, newSecond);				
			}
		}
//		if ( !expression.equals(result) ) {
//			System.out.println("=> Expression " + expression + " was rewritten as " + result);
//		}
		return result;
	}
	
	private Pair<String, Expression> getInverseAndIdentity(Expression functor) {
		Pair<String, Expression> pair = new Pair<String, Expression>();
		String inverseFunctor = null;
		Expression identity = null;
		if ( functor.equals(FunctorConstants.MINUS) ) {
			inverseFunctor = FunctorConstants.PLUS;
			identity = Expressions.ZERO;
		} 
		else if ( functor.equals(FunctorConstants.DIVISION) ) {
			inverseFunctor = FunctorConstants.TIMES;
			identity = Expressions.ONE;				
		}
		else if ( functor.equals(FunctorConstants.PLUS) ) {
			inverseFunctor = FunctorConstants.MINUS;
			identity = Expressions.ZERO;
		} 
		else if ( functor.equals(FunctorConstants.TIMES) ) {
			inverseFunctor = FunctorConstants.DIVISION;
			identity = Expressions.ONE;				
		}		
		pair.first = inverseFunctor;
		pair.second = identity;
		return pair;
	}

}
