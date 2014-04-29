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
package com.sri.ai.grinder.library.set.extensional;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.LinkedList;
import java.util.List;

import com.google.common.annotations.Beta;
import com.google.common.collect.Lists;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.core.AbstractSyntaxTree;
import com.sri.ai.expresso.core.DefaultCompoundSyntaxTree;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.grinder.library.set.Sets;
import com.sri.ai.util.Util;

/**
 * Utility class for working with Extensional Set expressions.
 * 
 * @author rbraz
 *
 */
@Beta
public class ExtensionalSet {

	public static final String SET_NORMALIZATION_STATE_KEY = "set normalization state";
	public static final String UNI_SET_LABEL = "{ . }";
	public static final String MULTI_SET_LABEL = "{{ . }}";

	public static AbstractSyntaxTree make(Object label, List<Expression> elements) {
		return new DefaultCompoundSyntaxTree(label, Expressions.makeKleeneListIfNeeded(elements));
	}
	
	public static Expression makeOfSameTypeAsIntensionalSet(Expression intensionalSetExpression, List<Expression> elements) {
		String extensionalSetLabel = Sets.isIntensionalUniSet(intensionalSetExpression)? UNI_SET_LABEL : MULTI_SET_LABEL;
		Expression result = make(extensionalSetLabel, elements);
		return result;
	}

	public static Expression makeOfSameTypeAs(Expression setExpression, List<Expression> elements) {
		Expression result = make(setExpression.getSyntaxTree().getLabel(), elements);
		return result;
	}

	public static AbstractSyntaxTree makeUniSet(List<Expression> elements) {
		return new DefaultCompoundSyntaxTree(UNI_SET_LABEL, Expressions.makeKleeneListIfNeeded(elements));
	}
	
	public static Expression makeUniSetExpression(List<Expression> elements) {
		return Expressions.makeFunctionApplication(UNI_SET_LABEL, Expressions.makeKleeneListIfNeeded(elements));
	}
	
	public static AbstractSyntaxTree makeUniSet(Expression... elements) {
		return new DefaultCompoundSyntaxTree(UNI_SET_LABEL, Expressions.makeKleeneListIfNeeded(Arrays.asList(elements)));
	}
	
	public static AbstractSyntaxTree makeMultiSet(List<Expression> elements) {
		return new DefaultCompoundSyntaxTree(MULTI_SET_LABEL,
				Expressions.makeKleeneListIfNeeded(elements));
	}
	
	public static AbstractSyntaxTree makeEmptySet() {
		return makeUniSet(new ArrayList<Expression>());
	}
	
	public static Expression makeEmptySetExpression() {
		return makeUniSetExpression(new ArrayList<Expression>());
	}
	
	public static List<Expression> getElements(Expression setExpression) {
		return Expressions.ensureListFromKleeneList(Expressions.makeFromSyntaxTree(setExpression.getSyntaxTree().getSubTree(0))); // does need to be sub tree
	}

	public static int cardinality(Expression expression) {
		return getElements(expression).size();
	}

	public static boolean isEmptySet(Expression expression) {
		return
		(expression.hasFunctor("list") && expression.numberOfArguments() == 0)
		||
		(Sets.isExtensionalSet(expression) && getElements(expression).isEmpty());
	}

	public static boolean isSingleton(Expression expression) {
		return cardinality(expression) == 1;
	}

	public static Expression removeNonDestructively(Expression set, int index) {
		Expression set1WithoutElement =
			make(
					Sets.getLabel(set),
					Util.removeNonDestructively(
							getElements(set), index));
		return set1WithoutElement;
	}

	public static Expression makeExpression(String functor, LinkedList<Expression> elements) {
		List<Expression> syntaxTrees = Util.mapIntoList(elements, Expression.GET_SYNTAX_TREE);
		Expression result = make(functor, syntaxTrees);
		return result;
	}

	public static boolean isExtensionalSet(Expression expression) {
		return Sets.isExtensionalSet(expression);
	}

	public static Expression makeSingleton(Expression element) {
		return makeUniSet(Lists.newArrayList(element));
	}

	public static Expression makeSingletonOfSameTypeAs(Expression set, Expression element) {
		return makeOfSameTypeAs(set, Lists.newArrayList(element));
	}
}
