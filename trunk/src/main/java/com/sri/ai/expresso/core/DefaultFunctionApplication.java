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

import static com.sri.ai.util.Util.castOrThrowError;
import static com.sri.ai.util.Util.mapIntoArray;
import static com.sri.ai.util.Util.mapIntoList;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Set;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.ExpressionAndContext;
import com.sri.ai.expresso.api.FunctionApplication;
import com.sri.ai.expresso.api.SubExpressionAddress;
import com.sri.ai.expresso.api.Symbol;
import com.sri.ai.expresso.api.SyntaxTree;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.core.AbstractNonQuantifiedExpression;
import com.sri.ai.grinder.library.FunctorConstants;
import com.sri.ai.grinder.library.boole.Not;
import com.sri.ai.util.Util;

/**
 * A default implementation of a {@link FunctionApplication}.
 * 
 * @author braz
 */
@Beta
public class DefaultFunctionApplication extends AbstractNonQuantifiedExpression implements FunctionApplication {

	private static final long serialVersionUID = 1L;
	
	private Expression                 functor;
	private ArrayList<Expression>      arguments;
	private SyntaxTree                 syntaxTree;
	private List<ExpressionAndContext> expressionAndContexts;
	
	public DefaultFunctionApplication(Expression functor, ArrayList<Expression> arguments) {
		super();
		this.functor   = functor;
		this.arguments = arguments;
		
		this.syntaxTree = new DefaultCompoundSyntaxTree(functor.getSyntaxTree(), mapIntoArray(arguments, e -> e == null? null : e.getSyntaxTree()));
		
		expressionAndContexts = new LinkedList<ExpressionAndContext>();
		expressionAndContexts.add(new DefaultExpressionAndContext(functor, new IndexAddress(-1)));
		int i = 0;
		for (Expression argument : arguments) {
			Expression conditioningConstraint = getConditioningConstraint(argument, i);
			expressionAndContexts.add(new DefaultExpressionAndContext(argument, new IndexAddress(i++), Expressions.EMPTY_LIST, conditioningConstraint));
		}
	}

	/**
	 * A method determining the conditioning constraint of an argument.
	 * For now, it only returns something different from {@link Expressions#TRUE} when the functor is {@link FunctorConstants#IF_THEN_ELSE}.
	 * New functions will require changing this code.
	 * TODO: make it extensible by end user code, without the need to change the method directly.
	 * @param argument the argument for which to determine the conditioning constraint
	 * @param argumentIndex the index of the argument
	 * @return the conditioning constraint
	 */
	private Expression getConditioningConstraint(Expression argument, int argumentIndex) {
		Expression conditioningConstraint;
		if (functor.equals(FunctorConstants.IF_THEN_ELSE)) { // hard-coded for now
			conditioningConstraint = argumentIndex == 1? arguments.get(0) : argumentIndex == 2? Not.make(arguments.get(0)) : Expressions.TRUE;
		}
		else {
			conditioningConstraint = Expressions.TRUE;
		}
		return conditioningConstraint;
	}

	@Override
	public Expression getFunctor() {
		return functor;
	}

	@Override
	public List<Expression> getArguments() {
		return arguments;
	}

	@Override
	public int numberOfArguments() {
		return arguments.size();
	}

	@Override
	public Expression get(int index) {
		if (index == -1) {
			return functor;
		}
		return arguments.get(index);
	}

	@Override
	public Expression set(int i, Expression newIthArgument) {
		FunctionApplication result;
		
		if (get(i) == newIthArgument) {
			result = this;
		}
		else {
			if (i == -1) {
				result = new DefaultFunctionApplication(newIthArgument, arguments);
			}
			else {
				ArrayList<Expression> newArguments = new ArrayList<Expression>(arguments);
				newArguments.set(i, newIthArgument);
				result = new DefaultFunctionApplication(functor, newArguments);
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
		return "Function application";
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
	public Expression getFunctorOrSymbol() {
		return getFunctor();
	}

	@Override
	public Expression clone() {
		return new DefaultFunctionApplication(getFunctor(), (ArrayList<Expression>) getArguments());
	}
	
	private static class IndexAddress implements SubExpressionAddress {

		private int index;
		
		public IndexAddress(int index) {
			super();
			this.index = index;
		}

		@Override
		public Expression replace(Expression expression, Expression newSubExpression) {
			assert expression instanceof DefaultFunctionApplication : DefaultFunctionApplication.class.getSimpleName() + ".IndexAddress applied to expression " + expression + " of class " + expression.getClass();
			Expression result = expression.set(this.index, newSubExpression);
			return result;
		}

		@Override
		public Expression getSubExpressionOf(Expression expression) {
			FunctionApplication functionApplication = castOrThrowError(FunctionApplication.class, expression, "Attempt at obtaining " + index + "-th argument expression of %s which should be an instance of %s but is an instance of %s");
			Expression result = functionApplication.get(index);
			return result;
		}
	}

	Set<String> infixFunctionsStrings = Util.set(
			"+", "-", "*", "/", "^",
			"and", "or", "<=>", "=>",
			"=", "!=", ">", "<", "<=", ">=",
			"union", "intersection", "in", "\\"
			);
	
	@Override
	public String makeToString() {
		String result;
		if (hasFunctor(FunctorConstants.IF_THEN_ELSE)) {
			result = "if " + get(0) + " then " + get(1) + " else " + get(2);
		}
		else if (hasFunctor(FunctorConstants.CARDINALITY) && numberOfArguments() == 1) {
			result = "| " + get(0) + " |";
		}
		else {
			int precedence = getPrecedence(this);
			if (hasFunctor(FunctorConstants.MINUS) && numberOfArguments() == 1) {
				result = "-" + stringAsSubExpression(get(0), precedence);
			}
			else if (hasFunctor(FunctorConstants.NOT) && numberOfArguments() == 1) {
				result = "not " + stringAsSubExpression(get(0), precedence);
			}
			else if (infixFunctionsStrings.contains(getFunctor().toString())) {
				List<String> subExpressionsStrings = mapIntoList(getArguments(), e -> stringAsSubExpression(e, precedence));
				result = Util.join(" " + getFunctor() + " ", subExpressionsStrings);
			}
			else {
				String functorRepresentation = getFunctor() instanceof Symbol? getFunctor().toString() : "(" + getFunctor() + ")";
				String argumentsRepresentation = Util.join(", ", getArguments());
				result = functorRepresentation + "(" + argumentsRepresentation + ")";
			}
		}
		return result;
	}
	
	private static String stringAsSubExpression(Expression expression, int parentPrecedence) {
		String result = expression.toString();
		int precedence = getPrecedence(expression);
		if (parentPrecedence > precedence) {
			result = "(" + result + ")";
		}
		return result;
	}

	/**
	 * A method with hard-coded precedence for known operators.
	 * Needs to be modified when new functions are introduced (applications of unknown functions, or non-function applications, get 100).
	 * In the future, we may extend this to a soft-coded, extensible mechanism.
	 */
	public static int getPrecedence(Expression expression) {
		int result = 100;
		if (expression.getSyntacticFormType().equals("Function application")) {
			if (
					expression.hasFunctor("in")
					||
					expression.hasFunctor("=")
					||
					expression.hasFunctor("!=")
					||
					expression.hasFunctor("<")
					||
					expression.hasFunctor(">")
					||
					expression.hasFunctor("<=")
					||
					expression.hasFunctor(">=")
					||
					expression.hasFunctor("<=>")
					||
					expression.hasFunctor("=>")
					)
			{
				result = 1;
			}
			else if (
					expression.hasFunctor("+")
					||
					expression.hasFunctor("-") && expression.numberOfArguments() == 2
					||
					expression.hasFunctor("or")
					||
					expression.hasFunctor("union")
					)
			{
				result = 2;
			}
			else if (
					expression.hasFunctor("intersection")
					||
					expression.hasFunctor("*")
					||
					expression.hasFunctor("/")
					||
					expression.hasFunctor("and")
					)
			{
				result = 3;
			}
			else if (
					expression.hasFunctor("\\")  && expression.numberOfArguments() == 1
					||
					expression.hasFunctor("-")   && expression.numberOfArguments() == 1
					||
					expression.hasFunctor("not") && expression.numberOfArguments() == 1
					)
			{
				result = 4;
			}
			else if (expression.hasFunctor("^"))
			{
				result = 5;
			}
		}
		return result;
	}
}
