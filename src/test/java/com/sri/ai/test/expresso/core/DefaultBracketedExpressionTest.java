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
package com.sri.ai.test.expresso.core;

import static com.sri.ai.util.Util.list;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;

import java.util.Collection;
import java.util.Iterator;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.ExpressionAndContext;
import com.sri.ai.grinder.helper.FunctionSignature;
import com.sri.ai.grinder.parser.antlr.AntlrGrinderParserWrapper;

public class DefaultBracketedExpressionTest {

	@Before
	public void setUp() {
	}
	
	@After
	public void tearDown() {
	}
	
	@Test
	public void testBracketedExpression() {
		
		Expression expression;
		
		Collection<FunctionSignature> randomPredicatesSignatures = list(new FunctionSignature("p/1"), new FunctionSignature("q/1"));
		AntlrGrinderParserWrapper parser = new AntlrGrinderParserWrapper(randomPredicatesSignatures);
		
		expression = parser.parse("[ if p(X) and q(Y) then 1 else 0 ]");
		Iterator<ExpressionAndContext> iterator = expression.getImmediateSubExpressionsAndContextsIterator();
		ExpressionAndContext xAndContext = iterator.next();
		ExpressionAndContext yAndContext = iterator.next();
		assertFalse(iterator.hasNext());
		assertEquals(xAndContext.getExpression(), parser.parse("X"));
		assertEquals(yAndContext.getExpression(), parser.parse("Y"));
		
		Expression another = parser.parse("[ if p(Z) and q(Y) then 1 else 0 ]");
		assertEquals(parser.parse("Z"), xAndContext.getAddress().getSubExpressionOf(another));
		
		ExpressionAndContext wOverXContext = xAndContext.setExpression(parser.parse("W"));
		Expression anotherWithW = another.replace(wOverXContext);
		Expression anotherWithW2 = wOverXContext.getAddress().replace(another, parser.parse("W")); // different way of doing the same thing
		assertEquals(parser.parse("[ if p(W) and q(Y) then 1 else 0 ]"), anotherWithW);
		assertEquals(parser.parse("[ if p(W) and q(Y) then 1 else 0 ]"), anotherWithW2);
		
		ExpressionAndContext aOverYAndContext = yAndContext.setExpression(parser.parse("a"));
		Expression anotherWithWAndA = anotherWithW.replace(aOverYAndContext);
		Expression anotherWithWAndA2 = aOverYAndContext.getAddress().replace(anotherWithW, parser.parse("a")); // different way of doing the same thing
		assertEquals(parser.parse("[ if p(W) and q(a) then 1 else 0 ]"), anotherWithWAndA);
		assertEquals(parser.parse("[ if p(W) and q(a) then 1 else 0 ]"), anotherWithWAndA2);
	}
}
