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

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.core.AbstractRewriter;
import com.sri.ai.grinder.core.DefaultRewriterTest;
import com.sri.ai.grinder.core.KindAttribute;
import com.sri.ai.grinder.helper.GrinderUtil;
import com.sri.ai.grinder.library.SemanticSubstitute;
import com.sri.ai.grinder.library.set.Sets;

/**
 * Rewriter of intensional sets with a condition that bounds one of its indices
 * to a version of the set after the removal of that index and its replacement by the valeu it's bound to.
 * @author braz
 */
@Beta
public class IntensionalSetWithBoundIndex extends AbstractRewriter {
	
	public IntensionalSetWithBoundIndex() {
		this.setReifiedTests(new DefaultRewriterTest(KindAttribute.INSTANCE, KindAttribute.VALUE_INTENSIONAL_SET));
	}
	
	@Override
	public Expression rewriteAfterBookkeeping(Expression expression, RewritingProcess process) {
		Expressions.BoundIndexInformation boundIndexInformation = null;
		if ((boundIndexInformation
					= Expressions.getBoundIndexInformation(
							IntensionalSet.getCondition(expression), IntensionalSet.getIndexExpressions(expression))
					)
					!= null) {

			Expression index = boundIndexInformation.index;
			Expression value = boundIndexInformation.value;
			Expression head = IntensionalSet.getHead(expression);
			Expression condition = IntensionalSet.getCondition(expression);

			RewritingProcess subProcessForHead      = GrinderUtil.extendContextualSymbolsAndConstraintWithIntensionalSet(expression, process);
			RewritingProcess subProcessForCondition = GrinderUtil.extendContextualSymbolsWithIntensionalSetIndices(expression, process);
			
			List<Expression> newIndexExpressions = boundIndexInformation.indexExpressionsWithoutBoundIndex;
			Expression       newHead             = SemanticSubstitute.replace(head,      index, value, subProcessForHead);
			Expression       newCondition        = SemanticSubstitute.replace(condition, index, value, subProcessForCondition);
			Expression       result              = IntensionalSet.makeSetFromIndexExpressionsList(Sets.getLabel(expression), newIndexExpressions, newHead, newCondition);
			return result;
		}
		return expression;
	}
}
