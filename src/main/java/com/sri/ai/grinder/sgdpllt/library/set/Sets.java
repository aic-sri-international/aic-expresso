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
package com.sri.ai.grinder.sgdpllt.library.set;

import static com.sri.ai.expresso.helper.Expressions.TRUE;
import static com.sri.ai.expresso.helper.Expressions.apply;
import static com.sri.ai.util.Util.myAssert;

import com.google.common.annotations.Beta;
import com.google.common.base.Predicate;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.IntensionalSet;
import com.sri.ai.expresso.api.SyntaxTree;
import com.sri.ai.expresso.core.ExtensionalIndexExpressionsSet;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.expresso.helper.SyntaxTrees;
import com.sri.ai.grinder.sgdpllt.library.FunctorConstants;
import com.sri.ai.grinder.sgdpllt.library.indexexpression.IndexExpressions;
import com.sri.ai.grinder.sgdpllt.library.set.extensional.ExtensionalSet;
import com.sri.ai.util.Util;

/**
 * A collection of utility routines around set expressions.
 * 
 * @author braz
 *
 */
@Beta
public class Sets {

	public static class IsSingletonExtensionalSet implements Predicate<Expression> {
		@Override
		public boolean apply(Expression expression) {
			return isSingletonExtensionalSet(expression);
		}
	}

	public static class IsNonEmptyUniSet implements Predicate<Expression> {

		@Override
		public boolean apply(Expression expression) {
			boolean result = isUniSet(expression) && ! isEmptySet(expression);
			return result;
		}
	}

	public static class IsExtensionalSet implements Predicate<Expression> {
		@Override
		public boolean apply(Expression expression) {
			return isExtensionalSet(expression);
		}
	}

	public static class IsIntensionalSet implements Predicate<Expression> {
		@Override
		public boolean apply(Expression expression) {
			return isIntensionalSet(expression);
		}
	}

	public static class IsExtensionalUniSet implements Predicate<Expression> {
		@Override
		public boolean apply(Expression expression) {
			return isExtensionalUniSet(expression);
		}
	}

	public static class IsIntensionalUniSet implements Predicate<Expression> {
		@Override
		public boolean apply(Expression expression) {
			return isIntensionalUniSet(expression);
		}
	}

	public static class IsEmptySet implements Predicate<Expression> {
		@Override
		public boolean apply(Expression expression) {
			return isEmptySet(expression);
		}
	}

	public static final Expression EMPTY_SET = ExtensionalSet.makeEmptySet();
	
	public static Predicate<Expression> IS_EMPTY_SET = new IsEmptySet();

	public static Predicate<Expression> IS_EXTENSIONAL_SET = new IsExtensionalSet();

	public static Predicate<Expression> IS_INTENSIONAL_SET = new IsIntensionalSet();

	public static Predicate<Expression> IS_EXTENSIONAL_UNI_SET = new IsExtensionalUniSet();

	public static Predicate<Expression> IS_INTENSIONAL_UNI_SET = new IsIntensionalUniSet();

	public static boolean isEmptySet(Expression expression) {
		return ExtensionalSet.isEmptySet(expression);
	}

	public static boolean isSingletonExtensionalSet(Expression expression) {
		boolean result = isExtensionalSet(expression) && ExtensionalSet.getElements(expression).size() == 1;
		return result;
	}

	public static boolean isExtensionalUniSet(Expression expression) {
		return
		expression != null &&
		expression.getSyntaxTree().getLabel().equals(ExtensionalSet.UNI_SET_LABEL)
		&& expression.getSyntaxTree().numberOfImmediateSubTrees() == 1; // does need to be sub tree
	}

	public static boolean isExtensionalMultiSet(Expression expression) {
		return
		expression != null &&
		expression.getSyntaxTree().getLabel().equals(ExtensionalSet.MULTI_SET_LABEL)
		&& expression.getSyntaxTree().numberOfImmediateSubTrees() == 1; // does need to be sub tree
	}
	
	public static boolean isIntensionalUniSet(Expression expression) {
		boolean result =
			expression != null &&
			expression.getSyntaxTree().getLabel().equals(IntensionalSet.UNI_SET_LABEL) &&
			expression.getSyntaxTree().numberOfImmediateSubTrees() == 3; // does need to be sub tree
		return result;
	}
	
	public static boolean isIntensionalMultiSet(Expression expression) {
		boolean result =
			expression != null &&
			expression.getSyntaxTree().getLabel().equals(IntensionalSet.MULTI_SET_LABEL) &&
			expression.getSyntaxTree().numberOfImmediateSubTrees() == 3; // does need to be sub tree
		return result;
	}
	
	public static boolean isUniSet(Expression expression) {
		return Sets.isExtensionalUniSet(expression) || Sets.isIntensionalUniSet(expression);
	}
	
	public static boolean isMultiSet(Expression expression) {
		return Sets.isExtensionalMultiSet(expression) || Sets.isIntensionalMultiSet(expression);
	}
	
	public static boolean isExtensionalSet(Expression expression) {
		boolean result =
			expression != null &&
			(
					expression.getSyntaxTree().getLabel().equals(ExtensionalSet.UNI_SET_LABEL) ||
					expression.getSyntaxTree().getLabel().equals(ExtensionalSet.MULTI_SET_LABEL)
			)
			&&
			expression.getSyntaxTree().numberOfImmediateSubTrees() == 1; // does need to be sub tree
		return result;
	}
	
	public static boolean isIntensionalSetWithoutRequirementOnStandardIndices(Expression expression) {
		boolean result =
			expression != null &&
			(
					expression.getSyntaxTree().getLabel().equals(IntensionalSet.UNI_SET_LABEL) ||
					expression.getSyntaxTree().getLabel().equals(IntensionalSet.MULTI_SET_LABEL)
			)
			&&
			expression.getSyntaxTree().numberOfImmediateSubTrees() == 3; // does need to be sub tree
		return result;
	}

	public static boolean isIntensionalSet(Expression expression) {
		boolean result =
			expression != null &&
			(
					expression.getSyntaxTree().getLabel().equals(IntensionalSet.UNI_SET_LABEL) ||
					expression.getSyntaxTree().getLabel().equals(IntensionalSet.MULTI_SET_LABEL)
			)
			&&
			expression.getSyntaxTree().numberOfImmediateSubTrees() == 3 // does need to be sub tree
;
//		&&
//			Util.forAll(
//					IntensionalSet.getIndexExpressions(expression),
//					new IndexExpressionIsValueOfSomethingOrSymbolOrFunctionApplication());
//		
		return result;
	}
	
	/**
	 * 
	 * @param expression 
	 *        the expression to test
	 * @return true if is an extensional, intensional set or an intersection, union, or intensional union.
	 */
	public static boolean isSetLikeExpression(Expression expression) {
		boolean result = 
				isSet(expression)
				|| expression.hasFunctor(FunctorConstants.INTERSECTION)
				|| expression.hasFunctor(FunctorConstants.UNION)
				|| expression.hasFunctor(FunctorConstants.INTENSIONAL_UNION)
				;
		return result;
	}
	
	public static boolean isIntensionalUnion(Expression expression) {
		boolean result = 
				Expressions.hasFunctor(expression, FunctorConstants.INTENSIONAL_UNION) 
				&& expression.numberOfArguments() == 1 
				&& isIntensionalMultiSet(expression.get(0));
		return result;
	}

	public static class IndexExpressionIsValueOfSomethingOrSymbolOrFunctionApplication implements Predicate {
		@Override
		public boolean apply(Object object) {
			Expression indexExpression = (Expression) object;
//			if (indexExpression.hasFunctor("value of")) {
//				return true;
//			}
			Expression index = IndexExpressions.getIndex(indexExpression);
			if (Expressions.isSymbolOrFunctionApplication(index)) {
				return true;
			}
			return false;
		}
	}

	public static String fromIntensionalToExtensionalSetSyntaxTreeLabel(Object label) {
		return Util.equals(label, IntensionalSet.MULTI_SET_LABEL)? ExtensionalSet.MULTI_SET_LABEL : ExtensionalSet.UNI_SET_LABEL;
	}

	public static boolean isSet(Expression expression) {
		return isExtensionalSet(expression) || isIntensionalSet(expression);
	}
	
	/**
	 * Returns the label of the syntactic tree representing the set.
	 * This is a symbol with the value of one of
	 * {@link IntensionalSet#MULTI_SET_LABEL}, {@link IntensionalSet#UNI_SET_LABEL},
	 * {@link ExtensionalSet#MULTI_SET_LABEL}, or {@link ExtensionalSet#UNI_SET_LABEL}.
	 */
	public static SyntaxTree getLabel(Expression setExpression) {
		SyntaxTree result = SyntaxTrees.wrap(setExpression.getSyntaxTree().getLabel());
		return result;
	}

	/**
	 * Transforms an application of a associative commutative function to an intensional set with one or more indices
	 * to an equivalent expression in which intensional sets have a single index each.
	 * An example is <code>sum( {{ (on X in 1..10, Y in 1..10) 3 | X != Y }} )</code>
	 * being transformed to <code>sum( {{ (on X in 1..10) sum({{ (on Y in 1..10) 3 | X != Y }}) | true }} )</code>
	 * @param functionApplicationOnIntensionalSet
	 * @return
	 */
	public static 
	Expression 
	expandApplicationOfAssociativeCommutativeFunction(Expression functionApplicationOnIntensionalSet) {
		IntensionalSet intensionalSet = (IntensionalSet) functionApplicationOnIntensionalSet.get(0);
		ExtensionalIndexExpressionsSet indexExpressions = (ExtensionalIndexExpressionsSet) intensionalSet.getIndexExpressions();
		myAssert( () -> indexExpressions.getList().size() != 0, () -> "There must be at least one index expression");
		Expression result;
		if (indexExpressions.getList().size() == 1) {
			result = functionApplicationOnIntensionalSet;
		}
		else {
			Expression functor = functionApplicationOnIntensionalSet.getFunctor();
			result = expandApplicationOfAssociativeCommutativeFunctionToIntensionalSetWithMultipleIndexExpressionsFrom(0, functor, intensionalSet, indexExpressions);
		}
		return result;
	}
	
	public static Expression makeUnion(Expression... sets) {
		if (sets.length == 0) {
			throw new IllegalArgumentException("No set arguments passed to construct union with.");
		}
		
		Expression result;
		if (sets.length == 1) {
			result = sets[0];			
		}
		else {
			result = Expressions.apply(FunctorConstants.UNION, sets);			
		}
		return result;
	}
	
	public static Expression makeIntersection(Expression... sets) {
		if (sets.length == 0) {
			throw new IllegalArgumentException("No set arguments passed to construct intersection with.");
		}
		
		Expression result;
		if (sets.length == 1) {
			result = sets[0];			
		}
		else {
			result = Expressions.apply(FunctorConstants.INTERSECTION, sets);
		}
		return result;
	}

	private static 
	Expression 
	expandApplicationOfAssociativeCommutativeFunctionToIntensionalSetWithMultipleIndexExpressionsFrom(
			int i,
			Expression functor,
			IntensionalSet intensionalSet, 
			ExtensionalIndexExpressionsSet indexExpressions) {
		
		Expression result;
		
		int numberOfIndexExpressions = indexExpressions.getList().size();
		if (i == numberOfIndexExpressions - 1) {
			Expression iThIndexExpression = indexExpressions.getList().get(i);
			ExtensionalIndexExpressionsSet indexExpressionsSetWithIthIndexExpressionOnly = new ExtensionalIndexExpressionsSet(iThIndexExpression);
			result = apply(functor, intensionalSet.setIndexExpressions(indexExpressionsSetWithIthIndexExpressionOnly));
		}
		else {
			Expression iThIndexExpression = indexExpressions.getList().get(i);
			ExtensionalIndexExpressionsSet indexExpressionsSetWithIthIndexExpressionOnly = new ExtensionalIndexExpressionsSet(iThIndexExpression);
			Expression innerHead = expandApplicationOfAssociativeCommutativeFunctionToIntensionalSetWithMultipleIndexExpressionsFrom(i + 1, functor, intensionalSet, indexExpressions);
			IntensionalSet innerSet = intensionalSet.setHeadAndCondition(innerHead, TRUE);
			innerSet = innerSet.setIndexExpressions(indexExpressionsSetWithIthIndexExpressionOnly);
			result = apply(functor, innerSet);
		}
		
		return result;
	}
}
