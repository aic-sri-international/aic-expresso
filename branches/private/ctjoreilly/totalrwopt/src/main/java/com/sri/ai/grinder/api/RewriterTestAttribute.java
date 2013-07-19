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
 * Interface representing the attribute of an Expression that a Rewriter tests
 * to determine if its value indicates when or not the expression should be
 * rewritten.<br>
 * <br>
 * <b>NOTE:</b> Implementations of this class should be singletons so that
 * identity (i.e. ==) as opposed to by value (i.e. equals(obj)) testing can be
 * performed between attributes.
 * 
 * @author braz
 * @author oreilly
 * 
 */
@Beta
public interface RewriterTestAttribute {
	/**
	 * Retrieve the value associated with the passed in value that this
	 * attribute represents.
	 * 
	 * @param expression
	 *            an expression from which a value represented by this attribute
	 *            is to be extracted.
	 * @param process
	 *            the process in which rewriting is occurring.
	 * @return an object representing the value that this attribute represents
	 *         in the given expression (if nothing, a value still must be returned
	 *         indicating this - i.e. null is not allowed).
	 */
	Object getValue(Expression expression, RewritingProcess process);
}
