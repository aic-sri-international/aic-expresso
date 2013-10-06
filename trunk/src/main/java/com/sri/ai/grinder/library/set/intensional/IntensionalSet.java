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
package com.sri.ai.grinder.library.set.intensional;

import static com.sri.ai.expresso.helper.Expressions.apply;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.LinkedList;
import java.util.List;

import com.google.common.annotations.Beta;
import com.google.common.base.Function;
import com.google.common.base.Predicate;
import com.google.common.collect.Lists;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.SyntaxTree;
import com.sri.ai.expresso.core.AbstractSyntaxTree;
import com.sri.ai.expresso.core.DefaultCompoundSyntaxTree;
import com.sri.ai.expresso.core.DefaultSymbol;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.core.DefaultRewriterTest;
import com.sri.ai.grinder.core.KindAttribute;
import com.sri.ai.grinder.library.ScopedVariables;
import com.sri.ai.grinder.library.SemanticSubstitute;
import com.sri.ai.grinder.library.controlflow.IfThenElse;
import com.sri.ai.grinder.library.set.Sets;
import com.sri.ai.grinder.library.set.extensional.ExtensionalSet;
import com.sri.ai.util.Util;
import com.sri.ai.util.base.Pair;

/**
 * A intensional set is a set formed by the instantiations of a parametric
 * expression (the <i>head</i>) according to parameter assignments satisfying a
 * given condition. For example, <code>{(on x in Natural) f(x) | x != 2 }</code>
 * can be interpreted as the set <code>{f(1), f(3), f(4), ...}</code>. They are
 * represented by a function application of either <code>"{ . . . }"</code> or
 * <code>"{{ . . . }}"</code> (for uni and multisets, respectively) on three
 * arguments:
 * <ul>
 * <li>The first one is a <i>scoping expression</i>, an application of
 * {@link #SCOPED_VARIABLES_LABEL} applied to a list of <i>index
 * expressions</i>, each of them an application of <code>in</code> to a variable
 * (a parameter) and a domain set.
 * <li>The second one is the head expression
 * <li>The third one is an application of {@link #CONDITION_LABEL} to the the
 * condition that needs to be satisfied for a particular parameter assignment
 * for the corresponding element to be in the set, or simply <code>null</code>
 * if the condition is always true.
 * </ul>
 * 
 * @author braz
 */
@Beta
public class IntensionalSet extends AbstractScopedVariablesProviderAndRewriter {

	public static final String UNI_SET_LABEL          = "{ . . . }";
	public static final String MULTI_SET_LABEL        = "{{ . . . }}";
	public static final String SCOPED_VARIABLES_LABEL = "( on . )";
	public static final String CONDITION_LABEL        = "|";
	//
	public static final Expression EMPTY_SCOPING_EXPRESSION = makeScopingExpression(new ArrayList<Expression>());
	
	//
	private static final List<Integer> _pathToHead      = Collections.unmodifiableList(Arrays.asList(new Integer(1)));
	private static final List<Integer> _pathToCondition = Collections.unmodifiableList(Arrays.asList(new Integer(2), new Integer(0)));
	private static final List<Integer> _pathZero        = Collections.unmodifiableList(Arrays.asList(new Integer(0)));
	private static final List<Integer> _pathZeroZero    = Collections.unmodifiableList(Arrays.asList(new Integer(0), new Integer(0)));
	
	public IntensionalSet() {
		this.setReifiedTests(new DefaultRewriterTest(KindAttribute.INSTANCE, KindAttribute.VALUE_INTENSIONAL_SET));
	}

	@Override
	public Expression rewriteAfterBookkeeping(Expression expression, RewritingProcess process) {
		if (Sets.isIntensionalSet(expression)) {
			Expression condition = getCondition(expression);
			if (condition.equals("false")) {
				return ExtensionalSet.makeEmptySetExpression();
			}
		}
		return expression;
	}

	@Override
	public Expression getScopedVariablesAsExpression(Expression expression, RewritingProcess process) {
		if (Sets.isIntensionalSet(expression)) {
			return new DefaultCompoundSyntaxTree("list", getScopedVariables(expression).toArray());
		}
		return null;
	}

	/**
	 * Makes new intensional set expression reducing one of the indices to a value.
	 */
	public static Expression makeIntensionalSetWithIndexSubstitution(Expression index,
			Expression value, Expression intensionalSet,
			RewritingProcess process) {
		Expression indexValueExpression = index;
				Expression newHead =
					SemanticSubstitute.replace(IntensionalSet.getHead(intensionalSet), indexValueExpression, value, process);
				Expression conditionToUseAfterSubstitution =
					SemanticSubstitute.replace(IntensionalSet.getCondition(intensionalSet), indexValueExpression, value, process);
				List<Expression> indexExpressions = IntensionalSet.getIndexExpressions(intensionalSet);
				indexExpressions = Util.listCopyWithoutSatisfyingElementOrNull(indexExpressions, new IndexExpressionHasIndex(indexValueExpression));
				// at some point we should look for occurrences of the index in the domain of the other indices.
				Expression result1 =
					IntensionalSet.makeSetFromIndexExpressionsList(Sets.getLabel(intensionalSet), indexExpressions, newHead, conditionToUseAfterSubstitution);
		Expression result = result1;
		return result;
	}

	/**
	 * Makes new intensional set expression reducing one of the indices to a value.
	 * This version takes the condition to be used, even though the condition is already
	 * present, in case the user wants to use an alternative condition.
	 */
	public static Expression makeIntensionalSetWithIndexSubstitution(Expression index,
			Expression value, Expression intensionalSet,
			Expression conditionToUseBeforeSubstitution,
			RewritingProcess process) {
		Expression indexValueExpression = index;
		Expression newHead =
			SemanticSubstitute.replace(IntensionalSet.getHead(intensionalSet), indexValueExpression, value, process);
		Expression conditionToUseAfterSubstitution =
			SemanticSubstitute.replace(conditionToUseBeforeSubstitution, indexValueExpression, value, process);
		List<Expression> indexExpressions = IntensionalSet.getIndexExpressions(intensionalSet);
		indexExpressions = Util.listCopyWithoutSatisfyingElementOrNull(indexExpressions, new IndexExpressionHasIndex(indexValueExpression));
		// at some point we should look for occurrences of the index in the domain of the other indices.
		Expression result =
			IntensionalSet.makeSetFromIndexExpressionsList(Sets.getLabel(intensionalSet), indexExpressions, newHead, conditionToUseAfterSubstitution);
		return result;
	}

	public static Expression simplificationOfIntensionalSetWithoutIndices(Expression head, Expression condition) {
		// no indices, so we get a singleton with the head if condition is true, or the empty set otherwise
		Expression result = IfThenElse.make(
				condition,
				ExtensionalSet.makeUniSet(Lists.newArrayList(head)),
				ExtensionalSet.makeEmptySet());
		return result;
	}

	/** Returns the scoping expression, which is an application of "on" on a list of index expressions. */
	public static Expression getScopingExpression(Expression expression) {
		return expression.getSyntaxTree().getSubTree(0);
		// does need to be sub tree
	}

	/** Returns the scoping expression, which is an application of "on" on a list of index expressions. */
	public static Collection<Expression> getScopedVariables(Expression expression) {
		if (Sets.isIntensionalSet(expression)) {
			Collection<Expression> result =
				IntensionalSet.getIndexToDomainMap(expression).keySet();
			return result;
		}
		return null;
	}

	public static Collection<Expression> getIndexDomains(Expression expression) {
		if (Sets.isIntensionalSet(expression)) {
			Collection<Expression> result =
				IntensionalSet.getIndexToDomainMap(expression).values();
			return result;
		}
		return null;
	}

	public static Collection<Expression> getIndices(Expression intensionalSet) {
		if (Sets.isIntensionalSet(intensionalSet)) {
			Collection<Expression> result =
				IntensionalSet.getIndexToDomainMap(intensionalSet).keySet();
			return result;
		}
		return null;
	}

	public static class IsIndexIn implements Predicate<Expression> {
		private List<Expression> indexExpressions;
		
		public IsIndexIn(List<Expression> indexExpressions) {
			super();
			this.indexExpressions = indexExpressions;
		}

		@Override
		public boolean apply(Expression possibleIndex) {
			boolean result = Util.thereExists(indexExpressions, new IntensionalSet.IsIndexExpressionOnIndex(possibleIndex));
			return result;
		}
	}

	public static class IsIndexExpressionOnIndex implements Predicate<Expression> {

		private Expression index;

		public IsIndexExpressionOnIndex(Expression index) {
			super();
			this.index = index;
		}

		@Override
		public boolean apply(Expression indexExpression) {
			boolean result = IntensionalSet.getIndex(indexExpression).equals(index);
			return result;
		}

	}

	public static Expression getHead(Expression expression) {
		return expression.getSyntaxTree().getSubTree(1);
		 // does need to be sub tree
	}

	public static List<Integer> getPathToHead() {
		return IntensionalSet._pathToHead;
	}

	/**
	 * Gets the condition of the intensional set.
	 */
	public static Expression getCondition(Expression expression) {
		SyntaxTree result = expression.getSyntaxTree().getSubTree(2) != null? expression.getSyntaxTree().getSubTree(2).getSubTree(0) : Expressions.TRUE;
		return result;
		 // does need to be sub tree
	}

	public static List<Integer> getPathToCondition() {
		return IntensionalSet._pathToCondition;
	}

	public static Expression makeUniSet(Expression scopingExpression, Expression head, Expression condition) {
		return IntensionalSet.make(IntensionalSet.UNI_SET_LABEL, scopingExpression, head, condition);
	}

	public static Expression makeMultiSet(Expression scopingExpression, Expression head, Expression condition) {
		return IntensionalSet.make(IntensionalSet.MULTI_SET_LABEL, scopingExpression, head, condition);
	}

	/** Make either uni or multiset by using provided functor. */
	public static Expression make(Object label, Expression scopingExpression, Expression head, Expression condition) {
		AbstractSyntaxTree conditionExpression =
			(condition == null || condition.equals("true"))?
					null
					: new DefaultCompoundSyntaxTree(IntensionalSet.CONDITION_LABEL, condition);
		Expression result = new DefaultCompoundSyntaxTree(
				Expressions.wrap(label),
				scopingExpression,
				head,
				conditionExpression);
		return result;
	}

	public static Expression makeUniSetFromIndexExpressionsList(List<Expression> indexExpressionsList, Expression head, Expression condition) {
		Expression result = IntensionalSet.makeUniSet(
				IntensionalSet.makeScopingExpression(indexExpressionsList),
				head,
				condition);
		return result;
	}

	public static Expression makeMultiSetFromIndexExpressionsList(List<Expression> indexExpressionsList, Expression head, Expression condition) {
		Expression result = IntensionalSet.makeMultiSet(
				IntensionalSet.makeScopingExpression(indexExpressionsList),
				head,
				condition);
		return result;
	}

	/** Makes a scoping expression out of a list of scoping variables. */
	public static DefaultCompoundSyntaxTree makeScopingExpression(List<Expression> indexExpressionsList) {
		return new DefaultCompoundSyntaxTree(
				SCOPED_VARIABLES_LABEL, Expressions.makeKleeneListIfNeeded(indexExpressionsList)
		);
	}
	
	public static Expression copyWithNewIndexExpressionsList(Expression intensionalSetExpression, List<Expression> indexExpressionsList) {
		Expression label = Sets.getLabel(intensionalSetExpression);
		Expression head = IntensionalSet.getHead(intensionalSetExpression);
		Expression condition = IntensionalSet.getCondition(intensionalSetExpression);
		Expression result = IntensionalSet.makeSetFromIndexExpressionsList(label, indexExpressionsList, head, condition);
		return result;
	}

	public static Expression copyWithNewHead(Expression intensionalSet, Expression newHead) {
		Expression result = IntensionalSet.make(Sets.getLabel(intensionalSet), IntensionalSet.getScopingExpression(intensionalSet), newHead, IntensionalSet.getCondition(intensionalSet));
		return result;
	}

	public static Expression copyWithNewHeadAndCondition(Expression set, Expression newHead, Expression newCondition) {
		Expression result = make(Sets.getLabel(set), getScopingExpression(set), newHead, newCondition);
		return result;
	}

	public static Expression makeSetFromIndexExpressionsList(
			Object label, List<Expression> indexExpressionsList,
			Expression head, Expression condition) {
//		if (indexExpressionsList == null || indexExpressionsList.isEmpty()) {
//			String extensionalSetLabel = Sets.fromIntensionalToExtensionalSetSyntaxTreeLabel(functor);
//			return IfThenElse.make(condition, ExtensionalSet.make(extensionalSetLabel, Lists.newArrayList(head)), ExtensionalSet.makeEmptySet());
//		}
// the above is nice, but it can hurt algorithms expecting an intensional set back and getting an extensional one, conditional no less.
// Perhaps it's better to have it as a rewriter somewhere else.
		
		Expression result = make(
				Expressions.wrap(label),
				makeScopingExpression(indexExpressionsList),
				head,
				condition);
		return result;
	}

	/** Makes intensional uniset with a single index. */
	public static Expression makeUniSetWithASingleIndexExpression(
			Expression index, Expression indexDomain, Expression head, Expression condition) {
		Expression newIndexExpression = apply("in", index, indexDomain);
		Expression result = IntensionalSet.makeUniSetWithASingleIndexExpression(newIndexExpression, head, condition);
		return result;
	}

	public static Expression makeUniSetWithASingleIndexExpression(
			Expression indexExpression, Expression head, Expression condition) {
		Expression result = IntensionalSet.makeUniSet(
				new DefaultCompoundSyntaxTree(IntensionalSet.SCOPED_VARIABLES_LABEL, indexExpression),
				head,
				condition);
		return result;
	}

	/** Makes intensional multiset with a single index. */
	public static Expression makeMultiSetWithASingleIndexExpression(
			Expression index, Expression indexDomain, Expression head, Expression condition) {
		Expression newIndexExpression = apply("in", index, indexDomain);
		Expression result = IntensionalSet.makeMultiSetWithASingleIndexExpression(newIndexExpression, head, condition);
		return result;
	}

	public static Expression makeMultiSetWithASingleIndexExpression(
			Expression indexExpression, Expression head, Expression condition) {
		Expression result = IntensionalSet.makeMultiSet(
				new DefaultCompoundSyntaxTree(IntensionalSet.SCOPED_VARIABLES_LABEL, indexExpression),
				head,
				condition);
		return result;
	}

	public static boolean isScopeIndependent(Expression expression, Expression setExpression, RewritingProcess process) {
		boolean result = ScopedVariables.isKnownToBeIndependentOfIndices(expression, IntensionalSet.getScopedVariables(setExpression), process);
		return result;
	}

	public static LinkedHashMap<Expression, Expression> getIndexToDomainMap(Expression intensionalSetExpression) {
		LinkedHashMap<Expression, Expression> result =
			Expressions.getRelationalMap(
					IntensionalSet.getIndexExpressions(intensionalSetExpression),
					DefaultSymbol.createSymbol("in"),
					new TypeOfIndexInIndexExpression());
		return result;
	}

	/**
	 * Returns the "whole index expression", which is the argument of "on", possibly including "value of" prefixes.
	 */
	public static Expression getWholeIndexExpression(Expression intensionalSetExpression) {
		return IntensionalSet.getScopingExpression(intensionalSetExpression).getSyntaxTree().getSubTree(0); // does need to be sub tree
	}

	/**
	 * Indicates whether the index expression of the intensional set needs to be evaluated,
	 * that is, it is prefixed by "value of".
	 */
	public static boolean wholeIndexExpressionNeedsToBeEvaluated(Expression expression) {
		boolean result = expression.getSyntaxTree().getSubTree(0).getSubTree(0).hasFunctor("value of");
		// does need to be sub tree
		return result;
	}

	/**
	 * Returns list of index expressions or <null> if unresolved.
	 */
	public static List<Expression> getIndexExpressions(Expression intensionalSetExpression) {
		Expression scopingExpression = IntensionalSet.getScopingExpression(intensionalSetExpression);
		if (scopingExpression != null) {
			List<Expression> indexExpressions = Expressions.ensureListFromKleeneList(scopingExpression.getSyntaxTree().getSubTree(0));
			// does need to be sub tree
			return indexExpressions;
		}
		return new LinkedList<Expression>();
	}

	public static Expression copyIndexExpressionWithNewIndex(Expression indexExpression, Expression newIndex) {
		if (newIndex != IntensionalSet.getIndex(indexExpression)) {
			Expression result;
			if (indexExpression.hasFunctor("in")) {
				result = Expressions.apply("in", newIndex, indexExpression.get(1));
			}
			else {
				result = newIndex;
			}
			return result;
		}
		return indexExpression;
	}

	public static Iterator<Expression> getIndexExpressionsIterator(Expression intensionalSetExpression) {
		Expression scopingExpression = IntensionalSet.getScopingExpression(intensionalSetExpression);
		if (scopingExpression != null) {
			return Expressions.ensureListFromKleeneList(scopingExpression.getSyntaxTree().getSubTree(0)).iterator();
			// does need to be sub tree
		}
		return new LinkedList<Expression>().iterator();
	}

	public static List<Expression> getIndexExpressionsWithType(Expression expression) {
		List<Expression> indexExpressions = IntensionalSet.getIndexExpressions(expression);
		List<Expression> result = Util.replaceElementsNonDestructively(indexExpressions, IntensionalSet.getMakeIndexExpressionWithType());
		return result;
	}

	public static Function<Expression, Expression> getMakeIndexExpressionWithType() {
		return new Function<Expression, Expression>() {
			@Override
			public Expression apply(Expression indexExpression) {
				if (indexExpression.hasFunctor("in")) {
					return indexExpression;
				}
				return new DefaultCompoundSyntaxTree(
						"in",
						indexExpression,
						new DefaultCompoundSyntaxTree("type", indexExpression));
			}
		};
	}

	public static Expression makeIndexExpression(Expression index, Expression domain) {
		return Expressions.apply("in", index, domain);
	}

	private static class TypeOfIndexInIndexExpression implements Function<Expression, Expression> {
		@Override
		public Expression apply(Expression expression) {
			return new DefaultCompoundSyntaxTree("type", expression);
		}
	}
	
	/**
	 * Indicates whether a received index expression's index is in a given collection.
	 * @author braz
	 */
	public static class IndexExpressionHasIndexIn implements Predicate<Expression> {
		private Collection<Expression> indices;

		public IndexExpressionHasIndexIn(Collection<Expression> indices) {
			super();
			this.indices = indices;
		}

		@Override
		public boolean apply(Expression expression) {
			boolean result = indices.contains(getIndex(expression));
			return result;
		}
	}

	public static Pair<Expression, Expression> getIndexAndDomain(Expression indexExpression) {
		boolean bothIndexAndDomain = indexExpression.hasFunctor("in") && indexExpression.numberOfArguments() == 2;
		Expression index;
		Expression indexDomain;
		if (bothIndexAndDomain) {
			index = indexExpression.get(0);
			indexDomain = indexExpression.get(1);
		}
		else {
			index = indexExpression;
			indexDomain = IntensionalSet.type(index);
		}
		return new Pair<Expression, Expression>(index, indexDomain);
	}

	private static Expression type(Expression expression) {
		return new DefaultCompoundSyntaxTree("type", expression);
	}

	public static Expression getIndex(Expression indexExpression) {
		if (indexExpression.hasFunctor("in")) {
			return indexExpression.get(0);
		}
		return indexExpression;
	}

	public static Expression getDomain(Expression indexExpression) {
		Pair<Expression, Expression> indexAndDomain = IntensionalSet.getIndexAndDomain(indexExpression);
		return indexAndDomain.second;
	}

	public static boolean isIntensionalSet(Expression expression) {
		return Sets.isIntensionalSet(expression);
	}

	/**
	 * Provides an iterator for the pairs of to-be-evaluated sub-expressions and respective paths in the index expressions of the set definition.
	 * This includes the arguments of indices (for example, the X +1 in f(X+1)), the types of indices, if present,
	 * as well as indices themselves if they are of the form <code>value of</code>.
	 */
	public static Iterator<Pair<Expression, List<Integer>>> getSubExpressionsAndPathsFromIndexExpressionsIterator(Expression intensionalSet) {
		List<Expression> indexExpressions = IntensionalSet.getIndexExpressions(intensionalSet);
		List<Integer> basePath;
		if (indexExpressions.size() == 1) { // no kleene list operator, so base path is the one taking us to the 'on' operator.
			basePath = IntensionalSet._pathZero;
		}
		else {
			// there is a kleene list operator, so base path is the one taking us to the 'on' operator and then to the kleene list operator.
			basePath = IntensionalSet._pathZeroZero;
		}
		return IntensionalSet.getSubExpressionsAndPathsFromIndexExpressionsFromBasePathIterator(indexExpressions, basePath);
	}

	private static Iterator<Pair<Expression, List<Integer>>> getSubExpressionsAndPathsFromIndexExpressionsFromBasePathIterator(
			List<Expression> indexExpressions, List<Integer> basePath) {
		List<Pair<Expression, List<Integer>>> subExpressionsAndPaths = new LinkedList<Pair<Expression, List<Integer>>>();
		int indexExpressionIndex = 0;
		for (Expression indexExpression : indexExpressions) {
			// Determine path to index expression
			List<Integer> basePathPlusIndexExpressionIndex = new LinkedList<Integer>(basePath);
			basePathPlusIndexExpressionIndex.add(indexExpressionIndex);
			
			// Determine index and its path
			Expression index;
			List<Integer> pathToIndex;
			if (indexExpression.hasFunctor("in")) {
				index = indexExpression.get(0);
				pathToIndex = new LinkedList<Integer>(basePathPlusIndexExpressionIndex);
				pathToIndex.add(0);
			}
			else {
				index = indexExpression;
				pathToIndex = basePathPlusIndexExpressionIndex;
			}
			
			// Add index to sub-expressions, if it is to be evaluated
			if (index.hasFunctor("value of")) {
				IntensionalSet.addSubTreeWithIndexAndBasePathPlusArgumentIndex(index, 0, pathToIndex, subExpressionsAndPaths);
			}
			else {
				// else, at least the index arguments (if it is a function application index like f(x)) need to be evaluated
				for (int i = 0; i != index.numberOfArguments(); i++) {
					IntensionalSet.addSubTreeWithIndexAndBasePathPlusArgumentIndex(index, i, pathToIndex, subExpressionsAndPaths);
				}
			}
			
			// Add the type, if present.
			if (indexExpression.hasFunctor("in")) {
				IntensionalSet.addSubTreeWithIndexAndBasePathPlusArgumentIndex(indexExpression, 1, basePathPlusIndexExpressionIndex, subExpressionsAndPaths);
			}
			
			indexExpressionIndex++;
		}
		return subExpressionsAndPaths.iterator();
	}

	private static void addSubTreeWithIndexAndBasePathPlusArgumentIndex(
			Expression syntaxTree, int subTreeIndex, List<Integer> basePath,
			List<Pair<Expression, List<Integer>>> result) {
		Expression subExpression = syntaxTree.getSyntaxTree().getSubTree(subTreeIndex); // does need to be sub tree
		List<Integer> subExpressionPath = new LinkedList<Integer>(basePath);
		subExpressionPath.add(subTreeIndex);
		result.add(new Pair<Expression, List<Integer>>(subExpression, subExpressionPath));
	}
	
	public static class GetIndex implements Function<Expression, Expression> {
		@Override
		public Expression apply(Expression indexExpression) {
			return getIndex(indexExpression);
		}
	}

	public static class IndexExpressionHasIndex implements Predicate<Expression> {
	
		private Expression index;
	
		public IndexExpressionHasIndex(Expression index) {
			this.index = index;
		}
	
		@Override
		public boolean apply(Expression indexExpression) {
			if (getIndex(indexExpression).equals(index)) {
				return true;
			}
			return false;
		}
	}
}
