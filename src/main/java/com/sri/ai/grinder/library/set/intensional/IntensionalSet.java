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


import java.util.ArrayList;
import java.util.Collection;
import java.util.LinkedHashMap;
import java.util.List;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.IntensionalSetInterface;
import com.sri.ai.expresso.api.SyntaxTree;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.expresso.helper.SyntaxTrees;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.core.HasKind;
import com.sri.ai.grinder.core.KindAttribute;
import com.sri.ai.grinder.library.ScopedVariables;
import com.sri.ai.grinder.library.indexexpression.IndexExpressions;
import com.sri.ai.grinder.library.set.Sets;
import com.sri.ai.grinder.library.set.extensional.ExtensionalSet;

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
	
	public IntensionalSet() {
		this.setReifiedTests(new HasKind(KindAttribute.VALUE_INTENSIONAL_SET));
	}

	@Override
	public Expression rewriteAfterBookkeeping(Expression expression, RewritingProcess process) {
		if (Sets.isIntensionalSet(expression)) {
			Expression condition = ((IntensionalSetInterface) expression).getCondition();
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

	/** Makes a scoping expression out of a list of scoping variables. */
	public static SyntaxTree makeScopingSyntaxTree(List<Expression> indexExpressionsList) {
		Expression kleeneListExpression = Expressions.makeKleeneListIfNeeded(indexExpressionsList);
		SyntaxTree kleeneListSyntaxTree = kleeneListExpression.getSyntaxTree();
		SyntaxTree result = SyntaxTrees.makeCompoundSyntaxTree(SCOPED_VARIABLES_LABEL, kleeneListSyntaxTree);
		return result;
	}
	
	public static Expression copyWithNewHeadAndCondition(Expression set, Expression newHead, Expression newCondition) {
		Expression result = make(Sets.getLabel(set), set.getSyntaxTree().getSubTree(0), newHead, newCondition);
		return result;
	}

	public static Expression makeSetFromIndexExpressionsList(
			Object label, List<Expression> indexExpressionsList,
			Expression head, Expression condition) {
		Expression result = make(
				Expressions.wrap(label),
				makeScopingSyntaxTree(indexExpressionsList),
				head,
				condition);
		return result;
	}

	public static boolean isScopeIndependent(Expression expression, Expression setExpression, RewritingProcess process) {
		boolean result = ScopedVariables.isKnownToBeIndependentOfIndices(expression, IntensionalSet.getScopedVariables(setExpression), process);
		return result;
	}

	public static LinkedHashMap<Expression, Expression> getIndexToTypeMapWithDefaultNull(Expression intensionalSetExpression) {
		List<Expression> indexExpressions = ((IntensionalSetInterface) intensionalSetExpression).getIndexExpressions();
		return IndexExpressions.getIndexToTypeMapWithDefaultNull(indexExpressions);
	}

	public static LinkedHashMap<Expression, Expression> getIndexToTypeMapWithDefaultTypeOfIndex(Expression intensionalSetExpression) {
		List<Expression> indexExpressions = ((IntensionalSetInterface) intensionalSetExpression).getIndexExpressions();
		return IndexExpressions.getIndexToTypeMapWithDefaultTypeOfIndex(indexExpressions);
	}
}
