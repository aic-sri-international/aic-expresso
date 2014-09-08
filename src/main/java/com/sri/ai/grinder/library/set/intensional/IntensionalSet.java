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
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.SyntaxTree;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.expresso.helper.SyntaxTrees;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.core.DefaultRewriterTest;
import com.sri.ai.grinder.core.KindAttribute;
import com.sri.ai.grinder.library.ScopedVariables;
import com.sri.ai.grinder.library.SemanticSubstitute;
import com.sri.ai.grinder.library.indexexpression.IndexExpressions;
import com.sri.ai.grinder.library.set.Sets;
import com.sri.ai.grinder.library.set.extensional.ExtensionalSet;
import com.sri.ai.util.Util;
import com.sri.ai.util.base.Pair;
import com.sri.ai.util.collect.FunctionIterator;

/**
 * An <i>intensionally defined set</i>. or <i>intensional set</i> for short,
 * is a set formed by the instantiations of a parametric
 * expression (the <i>head</i>) according to parameter assignments satisfying a
 * given condition. For example, <code>{(on x in Natural) f(x) | x != 2 }</code>
 * can be interpreted as the set <code>{f(1), f(3), f(4), ...}</code>. They are
 * represented by a function application of either <code>"{ . . . }"</code> or
 * <code>"{{ . . . }}"</code> (for uni and multisets, respectively) on three
 * arguments:
 * <ul>
 * <li>The first one is a <i>scoping syntax tree</i>, a syntax tree with label
 * {@link #SCOPED_VARIABLES_LABEL} on a list of sub-trees <i>index
 * expressions</i>, each of them an application of <code>in</code> to a variable
 * (a parameter) and a type.
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
	public static final SyntaxTree EMPTY_SCOPING_SYNTAX_TREE = makeScopingSyntaxTree(new ArrayList<Expression>());
	
	//
	private static final List<Integer> _pathToHead      = Collections.unmodifiableList(Arrays.asList(new Integer(1)));
	private static final List<Integer> _pathToCondition = Collections.unmodifiableList(Arrays.asList(new Integer(2), new Integer(0)));
	private static final List<Integer> _pathZero        = Collections.unmodifiableList(Arrays.asList(new Integer(0)));
	private static final List<Integer> _pathZeroZero    = Collections.unmodifiableList(Arrays.asList(new Integer(0), new Integer(0)));
	
	public IntensionalSet() {
		this.setReifiedTests(new DefaultRewriterTest(KindAttribute.INSTANCE, KindAttribute.VALUE_INTENSIONAL_SET));
	}

	public static boolean isIntensionalSet(Expression expression) {
		return Sets.isIntensionalSet(expression);
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
			return Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("list", getScopedVariables(expression).toArray());
		}
		return null;
	}

	/**
	 * Makes new intensional set expression reducing one of the indices to a value.
	 */
	public static Expression makeIntensionalSetWithIndexSubstitution(
			Expression index,
			Expression value,
			Expression intensionalSet,
			RewritingProcess process) {
		Expression indexValueExpression = index;
		Expression newHead =
				SemanticSubstitute.replace(IntensionalSet.getHead(intensionalSet), indexValueExpression, value, process);
		Expression conditionToUseAfterSubstitution =
				SemanticSubstitute.replace(IntensionalSet.getCondition(intensionalSet), indexValueExpression, value, process);
		List<Expression> indexExpressions = IntensionalSet.getIndexExpressions(intensionalSet);
		indexExpressions = Util.listCopyWithoutSatisfyingElementOrNull(indexExpressions, new IndexExpressions.HasIndex(indexValueExpression));
		// at some point we should look for occurrences of the index in the type of the other indices.
		Expression result1 =
				IntensionalSet.makeSetFromIndexExpressionsList(Sets.getLabel(intensionalSet), indexExpressions, newHead, conditionToUseAfterSubstitution);
		Expression result = result1;
		return result;
	}

	/** Returns the scoping syntax tree, which is one with "on" as label on a list of index expressions syntax trees. */
	private static SyntaxTree getScopingSyntaxTree(Expression expression) {
		return expression.getSyntaxTree().getSubTree(0);
	}

	/** Returns the scoping expression, which is an application of "on" on a list of index expressions. */
	public static Collection<Expression> getScopedVariables(Expression expression) {
		if (Sets.isIntensionalSet(expression)) {
			Collection<Expression> result =
				IntensionalSet.getIndexToTypeMapWithDefaultNull(expression).keySet();
			return result;
		}
		return null;
	}

	public static Collection<Expression> getIndexDomains(Expression expression) {
		if (Sets.isIntensionalSet(expression)) {
			Collection<Expression> result = IntensionalSet.getIndexToTypeMapWithDefaultTypeOfIndex(expression).values();
			return result;
		}
		return null;
	}

	public static Expression getHead(Expression expression) {
		return Expressions.makeFromSyntaxTree(expression.getSyntaxTree().getSubTree(1));
		 // does need to be sub tree
	}

	public static List<Integer> getPathToHead() {
		return IntensionalSet._pathToHead;
	}

	/**
	 * Gets the condition of the intensional set.
	 */
	public static Expression getCondition(Expression expression) {
		Expression result = expression.getSyntaxTree().getSubTree(2) != null? Expressions.makeFromSyntaxTree(expression.getSyntaxTree().getSubTree(2).getSubTree(0)) : Expressions.TRUE;
		return result;
		 // does need to be sub tree
	}

	public static List<Integer> getPathToCondition() {
		return IntensionalSet._pathToCondition;
	}

	private static Expression makeUniSet(SyntaxTree scopingSyntaxTree, Expression head, Expression condition) {
		return IntensionalSet.make(IntensionalSet.UNI_SET_LABEL, scopingSyntaxTree, head, condition);
	}

	private static Expression makeMultiSet(SyntaxTree scopingSyntaxTree, Expression head, Expression condition) {
		return IntensionalSet.make(IntensionalSet.MULTI_SET_LABEL, scopingSyntaxTree, head, condition);
	}

	/** Make either uni or multiset by using provided label. */
	private static Expression make(Object label, SyntaxTree scopingSyntaxTree, Expression head, Expression condition) {
		SyntaxTree conditionSyntaxTree =
			(condition == null || condition.equals("true"))?
					null
					: SyntaxTrees.makeCompoundSyntaxTree(IntensionalSet.CONDITION_LABEL, condition.getSyntaxTree());
		Expression result = Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees(
				label,
				scopingSyntaxTree,
				head.getSyntaxTree(),
				conditionSyntaxTree);
		return result;
	}

	public static Expression makeUniSetFromIndexExpressionsList(List<Expression> indexExpressionsList, Expression head, Expression condition) {
		Expression result = IntensionalSet.makeUniSet(makeScopingSyntaxTree(indexExpressionsList), head, condition);
		return result;
	}

	public static Expression makeMultiSetFromIndexExpressionsList(List<Expression> indexExpressionsList, Expression head, Expression condition) {
		Expression result = IntensionalSet.makeMultiSet(
				IntensionalSet.makeScopingSyntaxTree(indexExpressionsList),
				head,
				condition);
		return result;
	}

	/** Makes a scoping expression out of a list of scoping variables. */
	public static SyntaxTree makeScopingSyntaxTree(List<Expression> indexExpressionsList) {
		Expression kleeneListExpression = Expressions.makeKleeneListIfNeeded(indexExpressionsList);
		SyntaxTree kleeneListSyntaxTree = kleeneListExpression.getSyntaxTree();
		SyntaxTree result = SyntaxTrees.makeCompoundSyntaxTree(SCOPED_VARIABLES_LABEL, kleeneListSyntaxTree);
		return result;
	}
	
	public static Expression copyWithNewIndexExpressionsList(Expression intensionalSetExpression, List<Expression> indexExpressionsList) {
		SyntaxTree label = Sets.getLabel(intensionalSetExpression);
		Expression head = IntensionalSet.getHead(intensionalSetExpression);
		Expression condition = IntensionalSet.getCondition(intensionalSetExpression);
		Expression result = IntensionalSet.makeSetFromIndexExpressionsList(label, indexExpressionsList, head, condition);
		return result;
	}

	public static Expression copyWithNewHead(Expression intensionalSet, Expression newHead) {
		Expression result = IntensionalSet.make(Sets.getLabel(intensionalSet), IntensionalSet.getScopingSyntaxTree(intensionalSet), newHead, IntensionalSet.getCondition(intensionalSet));
		return result;
	}

	public static Expression copyWithNewHeadAndCondition(Expression set, Expression newHead, Expression newCondition) {
		Expression result = make(Sets.getLabel(set), getScopingSyntaxTree(set), newHead, newCondition);
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
				makeScopingSyntaxTree(indexExpressionsList),
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

	public static Expression makeUniSetWithASingleIndexExpression(Expression indexExpression, Expression head, Expression condition) {
		Expression result = IntensionalSet.makeUniSetWithASingleIndexExpression(indexExpression, head, condition);
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
				SyntaxTrees.makeCompoundSyntaxTree(IntensionalSet.SCOPED_VARIABLES_LABEL, indexExpression.getSyntaxTree()),
				head,
				condition);
		return result;
	}

	public static boolean isScopeIndependent(Expression expression, Expression setExpression, RewritingProcess process) {
		boolean result = ScopedVariables.isKnownToBeIndependentOfIndices(expression, IntensionalSet.getScopedVariables(setExpression), process);
		return result;
	}

	public static LinkedHashMap<Expression, Expression> getIndexToTypeMapWithDefaultNull(Expression intensionalSetExpression) {
		List<Expression> indexExpressions = IntensionalSet.getIndexExpressions(intensionalSetExpression);
		return IndexExpressions.getIndexToTypeMapWithDefaultNull(indexExpressions);
	}

	public static LinkedHashMap<Expression, Expression> getIndexToTypeMapWithDefaultTypeOfIndex(Expression intensionalSetExpression) {
		List<Expression> indexExpressions = IntensionalSet.getIndexExpressions(intensionalSetExpression);
		return IndexExpressions.getIndexToTypeMapWithDefaultTypeOfIndex(indexExpressions);
	}

	/**
	 * Returns list of index expressions or <null> if unresolved.
	 */
	public static List<Expression> getIndexExpressions(Expression intensionalSetExpression) {
		if ( ! Sets.isEmptySet(intensionalSetExpression)) {
			SyntaxTree scopingSyntaxTree = IntensionalSet.getScopingSyntaxTree(intensionalSetExpression);
			if (scopingSyntaxTree != null) {
				List<SyntaxTree> indexExpressionsSyntaxTrees =
						SyntaxTrees.ensureListFromKleeneList(scopingSyntaxTree.getSubTree(0));
				List<Expression> indexExpressions =
						Collections.unmodifiableList(Expressions.makeListOfExpressions(indexExpressionsSyntaxTrees));
				return indexExpressions;
			}
		}
		return new LinkedList<Expression>();
	}

	public static List<Expression> getIndices(Expression set) {
		List<Expression> result = IndexExpressions.getIndices(getIndexExpressions(set));
		return result;
	}

	public static Iterator<Expression> getIndexExpressionsIterator(Expression intensionalSetExpression) {
		SyntaxTree scopingSyntaxTree = IntensionalSet.getScopingSyntaxTree(intensionalSetExpression);
		if (scopingSyntaxTree != null) {
			return Expressions.ensureListFromKleeneList(Expressions.makeFromSyntaxTree(scopingSyntaxTree.getSubTree(0))).iterator();
			// does need to be sub tree
		}
		return new LinkedList<Expression>().iterator();
	}

	public static List<Expression> getIndexExpressionsWithType(Expression expression) {
		List<Expression> indexExpressions = IntensionalSet.getIndexExpressions(expression);
		return IndexExpressions.getIndexExpressionsWithType(indexExpressions);
	}

	/**
	 * Provides an iterator for the pairs of to-be-evaluated sub-expressions and respective paths in the index expressions of the set definition.
	 * This includes the arguments of indices (for example, the X +1 in f(X+1)), the types of indices, if present,
	 * as well as indices themselves if they are of the form <code>value of</code>.
	 */
	public static Iterator<Pair<Expression, List<Integer>>> getSubExpressionsAndPathsFromIndexExpressionsIterator(Expression intensionalSet) {
		List<Expression> indexExpressions = IntensionalSet.getIndexExpressions(intensionalSet);
		return getSubExpressionsAndPathsFromIndexExpressionsIterator(indexExpressions);
	}

	public static Iterator<Pair<Expression, List<Integer>>> getSubExpressionsAndPathsFromIndexExpressionsIterator(List<Expression> indexExpressions) {
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
		List<Pair<SyntaxTree, List<Integer>>> subTreesAndPaths = new LinkedList<Pair<SyntaxTree, List<Integer>>>();
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
				IndexExpressions.addSubTreeWithIndexAndBasePathPlusArgumentIndex(index, 0, pathToIndex, subTreesAndPaths);
			}
			else {
				// else, at least the index arguments (if it is a function application index like f(x)) need to be evaluated
				for (int i = 0; i != index.numberOfArguments(); i++) {
					IndexExpressions.addSubTreeWithIndexAndBasePathPlusArgumentIndex(index, i, pathToIndex, subTreesAndPaths);
				}
			}
			
			// Add the type, if present.
			if (indexExpression.hasFunctor("in")) {
				IndexExpressions.addSubTreeWithIndexAndBasePathPlusArgumentIndex(indexExpression, 1, basePathPlusIndexExpressionIndex, subTreesAndPaths);
			}
			
			indexExpressionIndex++;
		}
		
		Iterator<Pair<Expression, List<Integer>>> result =
				new FunctionIterator<
				        Pair<SyntaxTree, List<Integer>>,
                        Pair<Expression, List<Integer>>>(
                        		subTreesAndPaths.iterator(),
                        		FROM_SYNTAX_TREE_AND_PATH_TO_EXPRESSION_AND_PATH);
		
		return result;
	}

	private static final Function<Pair<SyntaxTree, List<Integer>>, Pair<Expression, List<Integer>>>
	FROM_SYNTAX_TREE_AND_PATH_TO_EXPRESSION_AND_PATH = new Function<Pair<SyntaxTree, List<Integer>>, Pair<Expression, List<Integer>>>() {

		@Override
		public Pair<Expression, List<Integer>> apply(Pair<SyntaxTree, List<Integer>> input) {
			Pair<Expression, List<Integer>> result = Pair.make(Expressions.makeFromSyntaxTree(input.first), input.second);
			return result;
		}
		
	};
}
