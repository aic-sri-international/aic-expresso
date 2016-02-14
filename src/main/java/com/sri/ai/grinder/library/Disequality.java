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
package com.sri.ai.grinder.library;

import java.util.ArrayList;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Set;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.SyntaxLeaf;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.core.AbstractRewriter;
import com.sri.ai.grinder.core.HasKind;
import com.sri.ai.grinder.core.HasNumberOfArguments;
import com.sri.ai.grinder.library.boole.And;
import com.sri.ai.grinder.library.boole.Not;
import com.sri.ai.grinder.library.boole.Or;
import com.sri.ai.util.Util;
import com.sri.ai.util.base.BinaryFunction;

/**
 * Implements disequality.
 * 
 * @author braz
 */
@Beta
public class Disequality extends AbstractRewriter {

	public  static final Expression FUNCTOR = Expressions.makeSymbol(FunctorConstants.INEQUALITY);
	//
	public Disequality() {
		this.setReifiedTests(new HasKind(FUNCTOR),
				             new HasNumberOfArguments(2));
	}
	
	@Override
	public Expression rewriteAfterBookkeeping(Expression expression, RewritingProcess process) {
	
		Expression equals = Equality.equalityResultIfItIsKnown(expression, process);
		if (equals != expression) {
			SyntaxLeaf equalsResult = (SyntaxLeaf) equals.getSyntaxTree();
			Boolean booleanObject = (Boolean) equalsResult.getValue();
			boolean booleanValue = booleanObject.booleanValue();
			return Expressions.makeSymbol(!booleanValue);
		}
		
		return expression;
	}

	@Override
	public Expression apply(Expression expression, RewritingProcess process) {
	
		Expression equals = Equality.equalityResultIfItIsKnown(expression, process);
		if (equals != expression) {
			SyntaxLeaf equalsResult = (SyntaxLeaf) equals.getSyntaxTree();
			Boolean booleanObject = (Boolean) equalsResult.getValue();
			boolean booleanValue = booleanObject.booleanValue();
			return Expressions.makeSymbol(!booleanValue);
		}
		
		return expression;
	}

	/**
	 * Returns FALSE if given disequality has equal arguments, TRUE if they contain distinct constants,
	 * and the disequality itself otherwise.
	 */
	public static Expression simplify(Expression disequality, RewritingProcess process) {
		Expression result;
		if (disequality.get(0).equals(disequality.get(1))) {
			result = Expressions.FALSE;
		}
		else {
			Set<Expression> constants = new LinkedHashSet<Expression>();
			Set<Expression> nonConstants = new LinkedHashSet<Expression>();
			Util.collect(disequality.getArguments(), constants, process.getIsUniquelyNamedConstantPredicate(), nonConstants);
			if (constants.size() > 1) {
				result = Expressions.TRUE;
			}
			else if (constants.size() == 1 && constants.contains(Expressions.FALSE)) {
				result = And.make(new ArrayList<Expression>(nonConstants));
			}
			else if (constants.size() == 1 && constants.contains(Expressions.TRUE)) {
				ArrayList<Expression> negatedNonConstants = Util.mapIntoArrayList(nonConstants, e -> Not.make(e));
				result = And.make(new ArrayList<Expression>(negatedNonConstants));
			}
			else {
				result = disequality;
			}
		}
		return result;
	}

	/**
	 * Makes a disequality application on two terms possibly simplifying it (taking constants into account).
	 */
	public static Expression makeWithConstantSimplification(Expression term1, Expression term2, RewritingProcess process) {
		Expression result;
		if (term1.equals(term2)) {
			result = Expressions.FALSE;
		}
		else if (process.isUniquelyNamedConstant(term1) && process.isUniquelyNamedConstant(term2)) {
			result = Expressions.TRUE;
		}
		else {
			result = make(term1, term2);
		}
		return result;
	}
	
	public static Expression conditionForSubExpressionsDisequality(
			Expression expression1, Expression expression2) {
		List<Expression> conditionsForSubExpressionsToBeDistinct = listOfDisequalitiesOfSubExpressions(
				expression1, expression2);
		Expression condition = Or.make(conditionsForSubExpressionsToBeDistinct);
		return condition;
	}

	public static List<Expression> listOfDisequalitiesOfSubExpressions(
			Expression expression1, Expression expression2) {
		List<Expression> conditionsForSubExpressionsToBeDistinct = Util
				.zipWith(MAKE_PAIR_DISEQUALITY, Util.listFrom(expression1
						.getImmediateSubExpressionsIterator()), Util
						.listFrom(expression2
								.getImmediateSubExpressionsIterator()));
		return conditionsForSubExpressionsToBeDistinct;
	}

	/**
	 * Makes a disequality of two expressions, returning Expressions.FALSE if
	 * the expressions are identical.
	 */
	public static Expression make(Object expression1Object,
			Object expression2Object) {
		Expression expression1 = Expressions.wrap(expression1Object);
		Expression expression2 = Expressions.wrap(expression2Object);
		if (expression1.equals(expression2)) {
			return Expressions.FALSE;
		}
		return Expressions.apply(FunctorConstants.INEQUALITY, expression1, expression2);
	}

	public static BinaryFunction<Expression, Expression, Expression> MAKE_PAIR_DISEQUALITY = new BinaryFunction<Expression, Expression, Expression>() {
		@Override
		public Expression apply(Expression e1, Expression e2) {
			return make(e1, e2);
		}
	};

	public static boolean isDisequality(Expression expression) {
		return expression.hasFunctor(FunctorConstants.INEQUALITY);
	}
	
	/**
	 * Returns X != c, if 'expression' is c != X, for X a variable and c a constant, or 'expression' otherwise. 
	 */
	public static Expression normalize(Expression expression, RewritingProcess process) {
		if (process.isUniquelyNamedConstant(expression.get(0))) {
			if ( ! process.isUniquelyNamedConstant(expression.get(1))) {
				Expression result = Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees(expression.getFunctor(), expression.get(1), expression.get(0));
				return result;
			}
		}
		return expression;
	}

	/**
	 * Returns an expression equivalent to disequality (and perhaps simpler) given equality between a variable and another term.
	 */
	public static Expression simplifyGivenEquality(Expression disequality, Expression variable, Expression otherTerm) {
		Expression result;
		if (disequality.getArguments().contains(variable) && disequality.getArguments().contains(otherTerm)) {
			result = Expressions.FALSE;
		}
		else {
			result = disequality;
		}
		return result;
	}

	/**
	 * Returns an expression equivalent to disequality (and perhaps simpler) given a disequality.
	 */
	public static Expression simplifyGivenDisequality(Expression disequality, Expression variable, Expression otherTerm) {
		Expression result;
		if (disequality.getArguments().contains(variable) && disequality.getArguments().contains(otherTerm)) {
			result = Expressions.TRUE;
		}
		else {
			result = disequality;
		}
		return result;
	}
}
