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
package com.sri.ai.grinder.core.simplifier;

import java.util.Map;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.FunctionApplication;
import com.sri.ai.grinder.api.MapBasedSimplifier;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.api.Simplifier;

/**
 * A basic {@link MapBasedSimplifier} receiving its elementary simplifiers at construction time
 * and applying them only once to the top expression only.
 * 
 * @author braz
 *
 */
@Beta
public class BasedOnMaps implements MapBasedSimplifier {
	
	protected Map<String, Simplifier> functionApplicationSimplifiers;
	protected Map<String, Simplifier> syntacticFormTypeSimplifiers;

	public BasedOnMaps(
			Map<String, Simplifier> functionApplicationSimplifiers,
			Map<String, Simplifier> syntacticFormTypeSimplifiers) {
		
		super();
		this.functionApplicationSimplifiers = functionApplicationSimplifiers;
		this.syntacticFormTypeSimplifiers = syntacticFormTypeSimplifiers;
	}

	@Override
	public Map<String, Simplifier> getFunctionApplicationSimplifiers() {
		return functionApplicationSimplifiers;
	}

	@Override
	public Map<String, Simplifier> getSyntacticFormTypeSimplifiers() {
		return syntacticFormTypeSimplifiers;
	}

	public void setFunctionApplicationSimplifiers(Map<String, Simplifier> functionApplicationSimplifiers) {
		this.functionApplicationSimplifiers = functionApplicationSimplifiers;
	}

	public void setSyntacticFormTypeSimplifiers(Map<String, Simplifier> syntacticFormTypeSimplifiers) {
		this.syntacticFormTypeSimplifiers = syntacticFormTypeSimplifiers;
	}

	@Override
	public Expression apply(Expression expression, RewritingProcess process) {
		Simplifier simplifier;
		if (expression.getSyntacticFormType().equals(FunctionApplication.SYNTACTIC_FORM_TYPE)) {
			simplifier = getFunctionApplicationSimplifiers().get(expression.getFunctor().getValue());
		}
		else {
			simplifier = getSyntacticFormTypeSimplifiers().get(expression.getSyntacticFormType());
		}
		
		if (simplifier != null) {
			expression = simplifier.apply(expression, process);
		}
		
		return expression;
	}
}