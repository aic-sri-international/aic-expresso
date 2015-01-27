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

import static com.sri.ai.util.Util.mapIntoArrayList;
import static com.sri.ai.util.Util.replaceElementsNonDestructively;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import com.google.common.annotations.Beta;
import com.google.common.base.Function;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.IndexExpressionsSet;
import com.sri.ai.expresso.api.SyntaxTree;
import com.sri.ai.expresso.helper.SyntaxTrees;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.library.indexexpression.IndexExpressions;
import com.sri.ai.util.Util;

/**
 * A default implementation of a {@link IndexExpressionsSet}.
 * 
 * @author braz
 */
@Beta
public class ExtensionalIndexExpressionsSet implements IndexExpressionsSet {

	private List<Expression> list;
	
	public ExtensionalIndexExpressionsSet(List<Expression> indexExpressions) {
		super();
		list = new ArrayList<Expression>(indexExpressions);
	}
	
	public List<Expression> getList() {
		return Collections.unmodifiableList(list);
	}

	@Override
	public SyntaxTree getSubSyntaxTree() {
		List<SyntaxTree> indexExpressionsSyntaxTrees = mapIntoArrayList(getList(), Expression::getSyntaxTree);
		SyntaxTree result = SyntaxTrees.makeKleeneListIfNeeded(indexExpressionsSyntaxTrees);
		return result;
	}

	@Override
	public String getSubExpressionString() {
		String result = Util.join(", ", getList()); 
		return result;
	}

	@Override
	public IndexExpressionsSet replaceSymbol(Expression symbol, Expression newSymbol, RewritingProcess process) {
		Function<Expression, Expression> renameSymbol = e -> IndexExpressions.renameSymbol(e, symbol, newSymbol, process);
		List<Expression> newList = replaceElementsNonDestructively(getList(), renameSymbol);
		IndexExpressionsSet newIndexExpressions;
		if (newList != getList()) {
			newIndexExpressions = new ExtensionalIndexExpressionsSet(newList);
		}
		else {
			newIndexExpressions = this;
		}
		return newIndexExpressions;
	}
}
