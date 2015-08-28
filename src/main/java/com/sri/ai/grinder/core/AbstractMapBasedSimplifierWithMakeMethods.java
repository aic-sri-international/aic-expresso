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
package com.sri.ai.grinder.core;

import java.util.Map;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.api.Simplifier;

@Beta
/** 
 * Basic implementation of {@link MapBasedSimplifier}
 * delegating the creating of elementary simplifier maps to
 * two abstract methods.
 * This is useful if the elementary simplifiers depend on <code>this</code> during construction time,
 * which prevents them to be given to {@link DefaultMapBasedSimplifier} via <code>super</code>.
 */
abstract public class AbstractMapBasedSimplifierWithMakeMethods extends DefaultMapBasedSimplifier {

	public AbstractMapBasedSimplifierWithMakeMethods() {
		super(null, null);
	}
	
	/**
	 * Invoked only one to make a map from functors's getValue() values (Strings) to a function mapping a
	 * function application of that functor and a rewriting process to an equivalent, simplified expression.
	 * Only required if {@link #simplify(Expression, RewritingProcess)} is not overridden by code not using it. 
	 * @return
	 */
	abstract protected Map<String, Simplifier> makeFunctionApplicationSimplifiers();

	/**
	 * Invoked only one to make a map from syntactic form types (Strings) to a function mapping a
	 * function application of that functor and a rewriting process to an equivalent, simplified expression.
	 * Only required if {@link #simplify(Expression, RewritingProcess)} is not overridden by code not using it. 
	 * @return
	 */
	abstract protected Map<String, Simplifier> makeSyntacticFormTypeSimplifiers();

	@Override
	public Map<String, Simplifier> getFunctionApplicationSimplifiers() {
		if (functionApplicationSimplifiers == null) {
			functionApplicationSimplifiers = makeFunctionApplicationSimplifiers();
		}
		return functionApplicationSimplifiers;
	}

	@Override
	public Map<String, Simplifier> getSyntacticFormTypeSimplifiers() {
		if (syntacticFormTypeSimplifiers == null) {
			syntacticFormTypeSimplifiers = makeSyntacticFormTypeSimplifiers();
		}
		return syntacticFormTypeSimplifiers;
	}
}