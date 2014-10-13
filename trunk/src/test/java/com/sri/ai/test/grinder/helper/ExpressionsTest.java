/*
 * Copyright (c) 2014, SRI International
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
package com.sri.ai.test.grinder.helper;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertSame;

import java.util.Collections;
import java.util.List;

import org.junit.Assert;
import org.junit.Test;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.core.DefaultRewritingProcess;
import com.sri.ai.grinder.library.Basic;
import com.sri.ai.test.grinder.AbstractGrinderTest;
import com.sri.ai.util.Util;

public class ExpressionsTest extends AbstractGrinderTest {
	
	@Override
	public RewritingProcess makeRewritingProcess(Expression topExpression) {
		return new DefaultRewritingProcess(topExpression, new Basic());
	}
	
	@Test
	public void testFreeSymbols() {
		RewritingProcess process = makeRewritingProcess(parse("true"));
				
		Expression e = parse("{(on X) X} and Y");
		Assert.assertEquals(Util.set(parse("and"), parse("Y")), Expressions.freeSymbols(e, process));
		
		e = parse("{(on X) X} and X");
		Assert.assertEquals(Util.set(parse("and"), parse("X")), Expressions.freeSymbols(e, process));
	}

	@Test
	public void testSubExpressionInstance() {
		// tests whether sub-expression instances are the same as the ones used for construction.
		
		Expression functor = Expressions.makeSymbol("f");
		
		expression = Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees(functor, 1, 2, 3);
		assertSame(functor, expression.getFunctor());
//		
//		List<Expression> indexExpressions = Util.list(Expressions.apply(FunctorConstants.IN, "X", "Domain"), Expressions.apply(FunctorConstants.IN, "Y", "Domain"));
//		Expression body = Equality.make("X", "Y");
//		expression = ThereExists.make(indexExpressions, body);
//		// multiple index expressions create nested "there exists" expressions
//		assertSame(indexExpressions.get(0), ThereExists.getIndexExpression(expression));
//		assertSame(indexExpressions.get(1), ThereExists.getIndexExpression(ThereExists.getBody(expression)));
//		assertSame(body, ThereExists.getBody(ThereExists.getBody(expression)));
	}

	@Test
	public void testComparison() {
		Expression a;
		Expression b;
		Expression c;
		List<Expression> list;
		
		a = Expressions.parse("a");
		b = Expressions.parse("b");
		c = Expressions.parse("c");
		list = Util.list(a, b, c);
		Collections.sort(list);
		assertEquals(Util.list(a, b, c), list);
		
		a = Expressions.parse("f()");
		b = Expressions.parse("f");
		c = Expressions.parse("g()");
		list = Util.list(a, b, c);
		Collections.sort(list);
		assertEquals(Util.list(b, a, c), list);
		
		a = Expressions.parse("f(a, b, c)");
		b = Expressions.parse("f(c, b, a)");
		c = Expressions.parse("g()");
		list = Util.list(a, b, c);
		Collections.sort(list);
		assertEquals(Util.list(a, b, c), list);
		
		a = Expressions.parse("f(a, b, c)");
		b = Expressions.parse("f(a)");
		c = Expressions.parse("g()");
		list = Util.list(a, b, c);
		Collections.sort(list);
		assertEquals(Util.list(b, a, c), list);
		
		a = Expressions.parse("f(a, b, c)");
		b = Expressions.parse("f(c)");
		c = Expressions.parse("g()");
		list = Util.list(a, b, c);
		Collections.sort(list);
		assertEquals(Util.list(a, b, c), list);
		
		a = Expressions.parse("f(a(c,b), b, c)");
		b = Expressions.parse("f(a(b,c), b, c)");
		c = Expressions.parse("g()");
		list = Util.list(a, b, c);
		Collections.sort(list);
		assertEquals(Util.list(b, a, c), list);
		
		a = Expressions.parse("f(a(b,c), f(a(b,a)), c)");
		b = Expressions.parse("f(a(b,c), f(a(a,b)), c)");
		c = Expressions.parse("g()");
		list = Util.list(a, b, c);
		Collections.sort(list);
		assertEquals(Util.list(b, a, c), list);
		
		// Symbol labels f and g come first
		a = Expressions.parse("f   (a(b,c), f(a(a,b)), c)");
		b = Expressions.parse("f(x)(a(a,a), f(a(a,b)), c)");
		c = Expressions.parse("g()");
		list = Util.list(a, b, c);
		Collections.sort(list);
		assertEquals(Util.list(a, c, b), list);
	}
}
