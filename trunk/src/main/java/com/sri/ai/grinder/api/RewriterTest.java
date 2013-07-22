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
package com.sri.ai.grinder.api;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;

/**
 * Interface representing a reified test (of which there can be many) that a
 * Rewriter applies against a given expression (conjunctively) to determine
 * whether or not the Rewriter should be applied against the given expression.
 * These tests indicate the 'attribute' (i.e. RewriterTestAttribute) and 'value'
 * used in the test of an expression. For example, the exponentiation rewriter
 * tests the attribute 'kind' of the expression for its value 'application of
 * ^'.
 * 
 * @author braz
 * @author oreilly
 * 
 */
@Beta
public interface RewriterTest {
	/**
	 * 
	 * @return the Attribute part of the (attribute, value) pair this test
	 *         applies to.
	 */
	RewriterTestAttribute getAttribute();

	/**
	 * 
	 * @return the Value part of the (attribute, value) pair this test applies
	 *         to.
	 */
	Object getValue();

	/**
	 * Determine if the Rewriter that owns this test should be applied to the
	 * given expression based on the (attribute, value) pair that this test
	 * represents., i.e. attribute.getValue(expression) = value.
	 * 
	 * @param expression
	 *            an expression to test whether or not the Rewriter this test
	 *            belongs to should be applied to.
	 * @param process
	 *            the process in which rewriting is occurring.
	 * @return true if this tests (attribute,value) apply to the given
	 *         expression, otherwise return false.
	 */
	boolean apply(Expression expression, RewritingProcess process);
}
