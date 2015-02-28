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

import java.util.Arrays;
import java.util.List;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.SubExpressionAddress;
import com.sri.ai.expresso.api.SyntaxTree;
import com.sri.ai.expresso.helper.Expressions;

/**
 * A {@link SubExpressionAddress} based on a path over the syntax tree of an expression taking to the addressed sub-expression.
 * 
 * @author braz
 */
@Beta
public class SyntaxTreeBasedSubExpressionAddress implements SubExpressionAddress {

	private List<Integer> path;
	
	private SyntaxTreeBasedSubExpressionAddress(List<Integer> list) {
		this.path = list;
	}
	
	public static SubExpressionAddress get(List<Integer> path) {
		SubExpressionAddress result = new SyntaxTreeBasedSubExpressionAddress(path);
		return result;
	}
	
	public List<Integer> getPath() {
		return path;
	}

	@Override
	public Expression replace(Expression expression, Expression subExpression) {
		Expression result = replaceAtPath(expression.getSyntaxTree(), 0, subExpression);
		return result;
	}

	/**
	 * Makes a new Expression based on a syntax tree, but with the sub-expression at the end of this {@link SyntaxTreeBasedSubExpressionAddress}'s path (from the ith step on)
	 * (over syntax tree) equal to given subExpression (including a guarantee that it will be the same object instance).
	 * The path is a list of indices indicating a path in the expression tree.
	 * The path-i-sub-expression is the expression obtained by following the path from the position i on.
	 * If there are no indices to be followed (i is equal to the path's length), the sub-expression is returned.
	 * The method assumes the path describes an existing path-i-sub-expression.
	 */
	private Expression replaceAtPath(SyntaxTree syntaxTree, int i, Expression subExpression) {

		// This method is subtle; follow explanations below carefully.
		if (i != getPath().size()) {

			Object rootTreeOrExpression = syntaxTree.getRootTree();
			List<SyntaxTree> subTrees = syntaxTree.getImmediateSubTrees();
			Object[] subTreesOrSubExpressions = Arrays.copyOf(syntaxTree.getImmediateSubTrees().toArray(), subTrees.size());

			int index = getPath().get(i);
			// at this point, (rootTreeOrExpression, subTreesOrSubExpressions) contains only syntax trees, the sub-trees of syntaxTree.
			if (index == -1) { // replace the root tree
				rootTreeOrExpression = subExpression;
			}
			else {         // replace a sub-tree
				Expression subExpressionAtI = replaceAtPath((SyntaxTree) subTreesOrSubExpressions[index], i + 1, subExpression);
				// by recursion, subExpressionAtI is guaranteed to be based on the sub-tree on index with subExpression instance placed at the end of path.
				subTreesOrSubExpressions[index] = subExpressionAtI;
			}
			// now (rootTreeOrExpression, subTreesOrSubExpressions) contains an Expression, and not only SyntaxTrees.

			Expression result = Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees(rootTreeOrExpression, subTreesOrSubExpressions);
			// remember that constructors for expressions receive the *syntax tree* components, but when they receive Expressions,
			// they keep them around to make sure the corresponding sub-expressions use the same Expression instance.
			// This only holds for immediate sub-expressions, but the recursive class to replaceAtPath took care of the deeper ones.
			return result;
		}
		return subExpression;
	}


	@Override
	public boolean equals(Object another) {
		if (another instanceof SyntaxTreeBasedSubExpressionAddress) {
			return getPath().equals(((SyntaxTreeBasedSubExpressionAddress)another).getPath());
		}
		else {
			throw new Error("SyntaxTreeBasedSubExpressionAddress " + this + " being compared to non-SyntaxTreeBasedSubExpressionAddress " + another + " of class " + another.getClass());
		}
	}
	
	@Override
	public int hashCode() {
		return getPath().hashCode();
	}
	
	@Override
	public String toString() {
		return getPath().toString();
	}

	@Override
	public Expression getSubExpressionOf(Expression expression) {
		throw new Error("Do need to implement this");
	}
}
