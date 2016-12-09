/*
 * Copyright (c) 2016, SRI International
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
package com.sri.ai.test.grinder.sgdpllt.library.lambda;

import static com.sri.ai.expresso.helper.Expressions.makeSymbol;
import static com.sri.ai.expresso.helper.Expressions.parse;
import static com.sri.ai.util.Util.arrayList;

import org.junit.Assert;
import org.junit.Test;

import com.sri.ai.expresso.api.Type;
import com.sri.ai.expresso.type.Categorical;
import com.sri.ai.grinder.sgdpllt.api.Context;
import com.sri.ai.grinder.sgdpllt.core.TrueContext;
import com.sri.ai.grinder.sgdpllt.library.lambda.LambdaBetaReductionSimplifier;
import com.sri.ai.grinder.sgdpllt.rewriter.api.Simplifier;

public class LambdaBetaReductionSimplifierTest {
	
	public Simplifier simplifier = (f, context) -> LambdaBetaReductionSimplifier.simplify(f, context);

	@Test
	public void testSimpleReduction() {
		Type peopleType = new Categorical("People", 4, arrayList(makeSymbol("ann"), makeSymbol("bob")));
		Context context = new TrueContext();
		context = context.add(peopleType);
		
		Assert.assertEquals(
			parse("if X = ann then 0 else if X = bob then 0 else 0"), 
			simplifier.apply(parse("(lambda : if X = ann then 0 else if X = bob then 0 else 0)()"), context));
		
		Assert.assertEquals(
			parse("if ann = ann then 0 else if ann = bob then 0 else 0"), 
			simplifier.apply(parse("(lambda X in People : if X = ann then 0 else if X = bob then 0 else 0)(ann)"), context));
		
		Assert.assertEquals(
			parse("if ann = ann then 0 else if bob = bob then 0 else 0"), 
			simplifier.apply(parse("(lambda X in People, Y in People : if X = ann then 0 else if Y = bob then 0 else 0)(ann, bob)"), context));
	}
	
	@Test
	public void testNoReduction() {
		Type peopleType = new Categorical("People", 4, arrayList(makeSymbol("ann"), makeSymbol("bob"), makeSymbol("tom")));
		Context context = new TrueContext();
		context = context.add(peopleType);
		
		Assert.assertEquals(
			parse("(lambda X in People : if X = ann then 0 else if X = bob then 0 else 0)()"), 
			simplifier.apply(parse("(lambda X in People : if X = ann then 0 else if X = bob then 0 else 0)()"), context));
		
		Assert.assertEquals(
			parse("(lambda X in People, Y in People : if X = ann then 0 else if Y = bob then 0 else 0)(ann)"), 
			simplifier.apply(parse("(lambda X in People, Y in People : if X = ann then 0 else if Y = bob then 0 else 0)(ann)"), context));
		
		Assert.assertEquals(
			parse("(lambda X in People, Y in People : if X = ann then 0 else if Y = bob then 0 else 0)(ann, bob, tom)"), 
			simplifier.apply(parse("(lambda X in People, Y in People : if X = ann then 0 else if Y = bob then 0 else 0)(ann, bob, tom)"), context));
	}
}
