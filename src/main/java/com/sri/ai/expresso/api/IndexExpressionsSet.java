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
package com.sri.ai.expresso.api;

import com.google.common.annotations.Beta;
import com.sri.ai.grinder.api.RewritingProcess;

/**
 * Represents an immutable set of index expressions of a {@link QuantifiedExpression}.
 * The representation may be either extensional or intensional or a mix (by using a union of sets).
 * 
 * @author braz
 */
@Beta
public interface IndexExpressionsSet {
	
	/**
	 * Provides the syntax-tree to be plugged into syntax trees of expressions with these indices;
	 * the particular syntax tree must be the same understood by {@link #makeFromSubSyntaxTree(SyntaxTree)}.
	 * @return
	 */
	SyntaxTree getSubSyntaxTree();

	/**
	 * Returns a {@link IndexExpressionsSet} corresponding to the sub-syntax-tree of an index expression,
	 * as produced by {@link #getSubSyntaxTree()}.
	 * @return
	 */
	static IndexExpressionsSet makeFromSubSyntaxTree(SyntaxTree syntaxTree) {
		return null;
	}

	/**
	 * Provides the string that represents these indices in the string of expressions with these indices;
	 * this must be consistent with whatever parser is being used.
	 * @return
	 */
	String getSubExpressionString();

	/**
	 * Returns a new {@link IndexExpressionsSet} (or the same, if no changes occurred),
	 * with a given symbol replaced by another.
	 * @param symbol
	 * @param newSymbol
	 * @return
	 */
	IndexExpressionsSet replaceSymbol(Expression symbol, Expression newSymbol, RewritingProcess process);
}
