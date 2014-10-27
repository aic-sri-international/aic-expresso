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
package com.sri.ai.expresso.core;

import static com.sri.ai.util.Util.mapIntoArray;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.ExpressionAndContext;
import com.sri.ai.expresso.api.SyntacticFunctionApplication;
import com.sri.ai.expresso.api.SyntaxTree;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.core.AbstractNonQuantifiedExpression;
import com.sri.ai.grinder.library.FunctorConstants;

/**
 * A default implementation of a {@link SyntacticFunctionApplication}.
 * 
 * @author braz
 */
@Beta
public class DefaultSyntacticFunctionApplication extends AbstractNonQuantifiedExpression implements SyntacticFunctionApplication {

	private static final long serialVersionUID = 1L;
	
	private Expression                 syntacticfunctor;
	private ArrayList<Expression>      syntacticArguments;
	private SyntaxTree                 syntaxTree;
	private List<ExpressionAndContext> expressionAndContexts;
	
	public DefaultSyntacticFunctionApplication(Object functor, ArrayList<Expression> syntacticArguments) {
		super();
		setup(functor, syntacticArguments);
	}

	@SuppressWarnings("unchecked")
	public DefaultSyntacticFunctionApplication(Object functor, Expression... syntacticArguments) {
		if (syntacticArguments.length == 1 && syntacticArguments[0] instanceof ArrayList) {
			setup(functor, (ArrayList<Expression>) syntacticArguments[0]);
		}
		else if (syntacticArguments.length == 1 && syntacticArguments[0] instanceof Collection) {
			Collection<Expression> collection = (Collection) syntacticArguments[0];
			Expression[] array = (Expression[]) collection.toArray();
			List<Expression> list = Arrays.asList(array);
			setup(functor, new ArrayList<Expression>(list));
		}
		else {
			setup(functor, new ArrayList<Expression>(Arrays.asList(syntacticArguments)));
		}
	}
	
	private void setup(Object functor, ArrayList<Expression> syntacticArguments) {
		this.syntacticfunctor   = Expressions.wrap(functor);
		this.syntacticArguments = syntacticArguments;
		
		this.syntaxTree = new DefaultCompoundSyntaxTree(syntacticfunctor.getSyntaxTree(), mapIntoArray(syntacticArguments, e -> e == null? null : e.getSyntaxTree()));
		
		expressionAndContexts = new LinkedList<ExpressionAndContext>();
	}

	@Override
	public Expression getSyntacticFunctor() {
		return syntacticfunctor;
	}

	@Override
	public List<Expression> getSyntacticArguments() {
		return syntacticArguments;
	}

	@Override
	public int numberOfSyntacticArguments() {
		return syntacticArguments.size();
	}

	@Override
	public Expression getSyntacticArgument(int index) {
		if (index == -1) {
			return syntacticfunctor;
		}
		return syntacticArguments.get(index);
	}

	@Override
	public SyntacticFunctionApplication setSyntacticArgument(int i, Expression newIthArgument) {
		SyntacticFunctionApplication result;
		
		if (get(i) == newIthArgument) {
			result = this;
		}
		else {
			if (i == -1) {
				result = new DefaultSyntacticFunctionApplication(newIthArgument, syntacticArguments);
			}
			else {
				ArrayList<Expression> newArguments = new ArrayList<Expression>(syntacticArguments);
				newArguments.set(i, newIthArgument);
				result = new DefaultSyntacticFunctionApplication(syntacticfunctor, newArguments);
			}
		}
		
		return result;
	}

	@Override
	public Iterator<ExpressionAndContext> getImmediateSubExpressionsAndContextsIterator() {
		return expressionAndContexts.iterator();
	}

	@Override
	public Object getSyntacticFormType() {
		return "Syntactic function";
	}

	@Override
	public SyntaxTree getSyntaxTree() {
		return syntaxTree;
	}

	@Override
	public Expression renameSymbol(Expression symbol, Expression newSymbol, RewritingProcess process) {
		// TODO: incorrect! Must replace quantified symbols in sub-expressions too, this won't do it.
		Expression result = replaceAllOccurrences(symbol, newSymbol, process);
		return result;
	}

	@Override
	public Expression clone() {
		return new DefaultSyntacticFunctionApplication(getSyntacticFunctor(), (ArrayList<Expression>) getSyntacticArguments());
	}

	@Override
	public Expression set(int i, Expression newIthArgument) {
		throw new Error("Expression.set(int, Expression) not defined for " + SyntacticFunctionApplication.class.getSimpleName());
	}

	@Override
	public String makeToString() {
		return getSyntaxTree().toString();
	}

	public static DefaultSyntacticFunctionApplication make(Expression indexType) {
		return new DefaultSyntacticFunctionApplication(FunctorConstants.TYPE, indexType);
	}
}
