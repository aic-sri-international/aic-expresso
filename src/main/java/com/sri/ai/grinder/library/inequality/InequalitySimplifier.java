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
package com.sri.ai.grinder.library.inequality;

import static com.sri.ai.grinder.library.FunctorConstants.GREATER_THAN;
import static com.sri.ai.grinder.library.FunctorConstants.GREATER_THAN_OR_EQUAL_TO;
import static com.sri.ai.grinder.library.FunctorConstants.LESS_THAN;
import static com.sri.ai.grinder.library.FunctorConstants.LESS_THAN_OR_EQUAL_TO;
import static com.sri.ai.util.Util.map;

import java.util.Map;

import com.google.common.annotations.Beta;
import com.sri.ai.grinder.library.number.GreaterThan;
import com.sri.ai.grinder.library.number.GreaterThanOrEqualTo;
import com.sri.ai.grinder.library.number.LessThan;
import com.sri.ai.grinder.library.number.LessThanOrEqualTo;
import com.sri.ai.grinder.sgdpll.simplifier.api.Simplifier;
import com.sri.ai.grinder.sgdpll.simplifier.core.RecursiveExhaustiveMapBasedSimplifier;

/**
 * A {@link Simplifier} with inequality functions (<code>> <, >=, <=</code>)
 * 
 * @author braz
 *
 */
@Beta
public class InequalitySimplifier extends RecursiveExhaustiveMapBasedSimplifier {
	
	public InequalitySimplifier() {
		super(makeFunctionApplicationSimplifiers(), makeSyntacticFormTypeSimplifiers());
	}
	
	public static Map<String, Simplifier> makeFunctionApplicationSimplifiers() {
		return map(
				LESS_THAN_OR_EQUAL_TO,    (Simplifier) (f, process) ->
				LessThanOrEqualTo.simplify(f, process),

				LESS_THAN,                (Simplifier) (f, process) ->
				LessThan.simplify(f, process),
				
				GREATER_THAN_OR_EQUAL_TO, (Simplifier) (f, process) ->
				GreaterThanOrEqualTo.simplify(f, process),

				GREATER_THAN,             (Simplifier) (f, process) ->
				GreaterThan.simplify(f, process)
				);
	}

	public static Map<String, Simplifier> makeSyntacticFormTypeSimplifiers() {
		return map();
	}
}