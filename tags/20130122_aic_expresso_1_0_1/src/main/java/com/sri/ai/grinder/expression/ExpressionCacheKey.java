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
package com.sri.ai.grinder.expression;

import com.google.common.annotations.Beta;
import com.google.common.base.Objects;
import com.sri.ai.expresso.api.Expression;

/**
 * A Key for items to be maintained in the {@link ExpressionCache}.
 * 
 * @author braz
 *
 */
@Beta
public class ExpressionCacheKey {
	private Expression expression;
	private Expression constraint;
	//
	private int hashCode = -1; // lazy init as this is an immmutable class
	
	public ExpressionCacheKey(Expression expression, Expression constraint) {
		this.expression = expression;
		this.constraint = constraint;
	}
	
	public Expression getExpression() {
		return expression;
	}
	
	public Expression getConstraint() {
		return constraint;
	}
	
	@Override
	public boolean equals(Object other) {
		boolean result = false;
		if (other != null && other instanceof ExpressionCacheKey) {
			ExpressionCacheKey eacOther = (ExpressionCacheKey) other;
			if (Objects.equal(expression, eacOther.expression) &&
				Objects.equal(constraint, eacOther.constraint)   ) {
				result = true;
			}
		}
		return result;
	}
	
	@Override
	public int hashCode() {
		if (hashCode == -1) {
			hashCode = Objects.hashCode(expression, constraint);
		}
		return hashCode;
	}
	
	@Override
	public String toString() {
		return "(" + expression + ", " + constraint + ")";
	}
}
