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
package com.sri.ai.grinder.library.equality.formula.helper;

import java.util.Arrays;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.api.Rewriter;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.core.AbstractRewriter;
import com.sri.ai.grinder.core.HasKind;
import com.sri.ai.grinder.core.KindAttribute;
import com.sri.ai.grinder.core.TotalRewriter;
import com.sri.ai.grinder.library.boole.ThereExists;

/**
 * Performs the drop existential quantifiers portion of the formula:
 * 
 * there exists X: X = Y -> X = Y
 * 
 */
@Beta
public class ExistentialOut extends AbstractRewriter {
	
	public ExistentialOut() {
		//this.setReifiedTests(new HasKind(KindAttribute.VALUE_THERE_EXISTS));
		// TODO: Reification not working for VALUE_THERE_EXISTS and VALUE_FOR_ALL -- see QuantifierEliminationWrapper
	}
	
	public static Expression existentialsOut(Expression formula, RewritingProcess process) {
		Expression result = formula;
// TODO - Step 1 - create skolem functions for existentials scoped by universals
		
		// Step 2 - The variables should already be standardized apart at this point
		// therefore just need to drop existentials as we want to introduce 
		// uniquely named free variables as opposed to constants in the 
		// translation of formulas due to the unique names assumption.
		TotalRewriter eoRewriter = new TotalRewriter(ExistentialOut.class.getName()+ " existentialsOut Total Rewriter",
			Arrays.asList((Rewriter)
				new ExistentialOut()
			));
		result = eoRewriter.rewrite(result, process);
		
// TODO - Step 3 - translate the uninterpreted skolem functions to an equivalent equality formula.
		
		return result;
	}
	
	@Override
	public Expression rewriteAfterBookkeeping(Expression expression, RewritingProcess process) {
		Expression result = expression;
		
		if (ThereExists.isThereExists(expression)) {
			result = ThereExists.getBody(expression);
		}
		
		return result;
	}
}
