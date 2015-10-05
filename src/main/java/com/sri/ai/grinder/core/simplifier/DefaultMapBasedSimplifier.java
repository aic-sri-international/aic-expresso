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

import static com.sri.ai.util.Util.putAll;

import java.util.Map;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.api.MapBasedSimplifier;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.api.Simplifier;
import com.sri.ai.util.collect.StackedHashMap;

/**
 * A basic {@link MapBasedSimplifier} receiving its elementary simplifiers at construction time.
 * 
 * @author braz
 *
 */
@Beta
public class DefaultMapBasedSimplifier /* extends Recursive - see (1) */ implements MapBasedSimplifier {
	
	// (1) If you look at the apply method, you will see it builds a Recursive simplifier and invokes it.
	// In an attempt to simplify the code, I made this class extend Recursive so that, instead of building and using another Recursive,
	// it would *be* it, and simply re-use Recursive's apply method.
	// Instead of constructing a new Recursive in apply, the same parameters are provided in the constructor via super(...) (see commented line in constructor).
	//
	// However, to my great surprise, this makes the code *much* slower (running EqualityConstraintTest).
	// It seems that exactly the same operations would be performed, but the test takes 2-3 times longer.
	//
	// Just for a sanity check, I added a counter on BasedOnMaps and made sure that the number of basic simplifications
	// is the same in both cases (I first replaced occurrences of new Random() in that test by new Random(1) to make tests fixed.
	//
	// I also wondered if Java's overriding mechanism would somehow be slower (which would be a big surprise)
	// and wrote a simple test of that, which proved negative.
	
	protected Map<String, Simplifier> functionApplicationSimplifiers;
	protected Map<String, Simplifier> syntacticFormTypeSimplifiers;

	public DefaultMapBasedSimplifier(
			Map<String, Simplifier> functionApplicationSimplifiers,
			Map<String, Simplifier> syntacticFormTypeSimplifiers) {
		
//		super(new Exhaustive(new BasedOnMaps(functionApplicationSimplifiers, syntacticFormTypeSimplifiers)));
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
//		return super.apply(expression, process);
		Simplifier simplifier = new Recursive(new Exhaustive(new BasedOnMaps(functionApplicationSimplifiers, syntacticFormTypeSimplifiers)));
		Expression result = simplifier.apply(expression, process);
		return result;
	}

	/**
	 * Simplifies an expression based on two maps of simplifiers by creating an instance of {@link DefaultMapBasedSimplifier}
	 * with them and applies it to the expression.
	 * @param expression
	 * @param functionApplicationSimplifiers
	 * @param syntacticFormSimplifiers
	 * @param process
	 * @return
	 */
	public static Expression simplify(Expression expression, Map<String, Simplifier> functionApplicationSimplifiers, Map<String, Simplifier> syntacticFormSimplifiers, RewritingProcess process) {
		DefaultMapBasedSimplifier simplifier = new DefaultMapBasedSimplifier(functionApplicationSimplifiers, syntacticFormSimplifiers);
		Expression result = simplifier.apply(expression, process);
		return result;
	}

	/**
	 * Simplify an expression given maps of function application and syntactic form type simplifiers,
	 * and an extra simplifier for a given syntactic form type.
	 * @param expression
	 * @param functionApplicationSimplifiers
	 * @param syntacticFormTypeSimplifiers
	 * @param process
	 * @param additionalSyntacticFormTypesAndSimplifiers additional syntactic form types and corresponding simplifiers
	 * @return
	 */
	public static Expression simplifyWithExtraSyntacticFormTypeSimplifiers(
			Expression expression,
			Map<String, Simplifier> functionApplicationSimplifiers, Map<String, Simplifier> syntacticFormTypeSimplifiers,
			RewritingProcess process,
			Object... additionalSyntacticFormTypesAndSimplifiers) {
		
		Map<String, Simplifier>
		mySyntacticFormTypeSimplifiers = new StackedHashMap<String, Simplifier>(syntacticFormTypeSimplifiers);
		
		putAll(mySyntacticFormTypeSimplifiers, additionalSyntacticFormTypesAndSimplifiers);
		
		Expression result = simplify(expression, functionApplicationSimplifiers, mySyntacticFormTypeSimplifiers, process);
		return result;
	}
}