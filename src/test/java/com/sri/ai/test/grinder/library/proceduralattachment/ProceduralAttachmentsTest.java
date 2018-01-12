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
package com.sri.ai.test.grinder.library.proceduralattachment;

import static com.sri.ai.expresso.helper.Expressions.apply;
import static com.sri.ai.expresso.helper.Expressions.makeSymbol;
import static com.sri.ai.expresso.helper.Expressions.parse;
import static com.sri.ai.grinder.library.proceduralattachment.ProceduralAttachments.getProceduralAttachmentsRewriter;
import static com.sri.ai.grinder.library.proceduralattachment.ProceduralAttachments.newContextWithNewProceduralAttachment;
import static com.sri.ai.util.Util.println;
import static org.junit.Assert.*;

import org.junit.Test;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.api.Context;
import com.sri.ai.grinder.core.TrueContext;
import com.sri.ai.grinder.library.commonrewriters.CommonSimplifier;
import com.sri.ai.grinder.rewriter.api.Rewriter;
import com.sri.ai.grinder.rewriter.api.Simplifier;
import com.sri.ai.grinder.rewriter.api.TopRewriter;
import com.sri.ai.grinder.rewriter.core.CombiningTopRewriter;
import com.sri.ai.grinder.rewriter.core.Exhaustive;
import com.sri.ai.grinder.rewriter.core.FunctorSwitch;
import com.sri.ai.grinder.rewriter.core.Recursive;
import com.sri.ai.grinder.rewriter.core.SymbolSwitch;

public class ProceduralAttachmentsTest {
	
	@Test
	public void test() {
		Context context;
		Expression input;
		Expression expected;
		
		context = new TrueContext();
		
		SymbolSwitch ten = new SymbolSwitch("ten", (Simplifier) (e, c) -> makeSymbol(10));
		FunctorSwitch g2 = new FunctorSwitch("g", 2, (Simplifier) (e, c) -> apply("h", e.getArguments()));

		context = newContextWithNewProceduralAttachment(context, ten);
		context = newContextWithNewProceduralAttachment(context, g2);

		input = parse("ten + 1 + g(1, 2) + g(1)");
		expected = parse("11 + h(1, 2) + g(1)");
		
		runTest(input, expected, context);
	}

	private void runTest(Expression input, Expression expected, Context context) {
		TopRewriter topRewriter;
		Rewriter evaluator;
		Expression output;
		topRewriter =
				new CombiningTopRewriter(
						new CommonSimplifier(), 
						getProceduralAttachmentsRewriter(context));

		evaluator = new Recursive(new Exhaustive(topRewriter));
		
		output = evaluator.apply(input, context);
		println(output);
		assertEquals(expected, output);
	}
}
