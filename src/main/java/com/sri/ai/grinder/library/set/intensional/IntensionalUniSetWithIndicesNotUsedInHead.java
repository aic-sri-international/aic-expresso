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

import java.util.List;
import java.util.Set;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.core.AbstractRewriter;
import com.sri.ai.grinder.core.HasKind;
import com.sri.ai.grinder.core.KindAttribute;
import com.sri.ai.grinder.library.boole.ThereExists;
import com.sri.ai.grinder.library.indexexpression.IndexExpressions;
import com.sri.ai.grinder.library.set.Sets;
import com.sri.ai.util.Util;

/**
 * Implements the rewriting of
 * <code>{ (on I, I') Alpha(I) | C(I,I') } as { (on I) Alpha(I) | there exists I' : C(I,I') }</code>,
 * that is, it removes indices I' not used in set's head and replaces the condition by one on remaining indices I alone.
 * 
 * The present rewriter only correctly applies to symbol (not function application indices).
 * 
 * @author braz
 */
@Beta
public class IntensionalUniSetWithIndicesNotUsedInHead extends AbstractRewriter {
	
	public IntensionalUniSetWithIndicesNotUsedInHead() {
		// Note: While this rewriter works specifically with uni-intensional sets, it is ok
		// to setup a reified test on the more general intensional set kind (i.e. multi-sets as well)
		// as the inner uni-intensional set is still performed inside of this implementation's rewrite logic.
		this.setReifiedTests(new HasKind(KindAttribute.VALUE_INTENSIONAL_SET));
	}
	
	@Override
	public Expression rewriteAfterBookkeeping(Expression expression, RewritingProcess process) {
		return staticRewriteAfterBookkeeping(expression, process);
	}

	public static Expression staticRewriteAfterBookkeeping(Expression expression, RewritingProcess process) {
		if (Sets.isIntensionalUniSet(expression)) {
			Expression head = IntensionalSet.getHead(expression);
			Set<Expression> freeSymbolsInHead = Expressions.freeVariables(head, process);
			List<Expression> indexExpressions = IntensionalSet.getIndexExpressions(expression);
			List<Expression> indexExpressionsWhoseIndexIsNotInHead = Util.removeNonDestructively(indexExpressions, new IndexExpressions.IndexExpressionHasIndexIn(freeSymbolsInHead));
			if ( ! indexExpressionsWhoseIndexIsNotInHead.isEmpty()) {
				List<Expression> indexExpressionsWhoseIndexIsInHead = Util.subtract(indexExpressions, indexExpressionsWhoseIndexIsNotInHead);
				Expression newCondition = ThereExists.make(indexExpressionsWhoseIndexIsNotInHead, IntensionalSet.getCondition(expression));
				Expression result = IntensionalSet.makeUniSetFromIndexExpressionsList(indexExpressionsWhoseIndexIsInHead, head, newCondition);
				return result;
			}
		}
		return expression;
	}
}
