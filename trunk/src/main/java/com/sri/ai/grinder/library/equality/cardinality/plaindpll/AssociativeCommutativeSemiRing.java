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
package com.sri.ai.grinder.library.equality.cardinality.plaindpll;

import java.util.List;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.api.RewritingProcess;

/**
 * Object representing an associative commutative semi-ring.
 * 
 * @author braz
 *
 */
@Beta
public interface AssociativeCommutativeSemiRing extends AssociativeCommutativeGroup {

	/** Returns the functor of the multiplicative operation. */
	String multiplicativeFunctor();
	
	/** The multiplicative operation identity element. */
	Expression multiplicativeIdentityElement();

	/** 
	 * The multiplicative operation identity element.
	 * This reason this is <i>not</i> a test like {@link AssociativeCommutativeGroup#isAdditiveAbsorbingElement}
	 * is that semi-rings are required to have a multiplicative absorbing element,
	 * but not required to have an additive one.
	 */
	Expression multiplicativeAbsorbingElement();

	/**
	 * Given an expression, returns its arguments if it is an application of the multiplicative operation,
	 * or the expression itself otherwise.
	 * This is useful when the multiplicative operator is implicit when applied to a single argument.
	 */
	List<Expression> getFactors(Expression expression);
	
	/**
	 * Performs the semi-rings's multiplicative operation on a function application of the multiplicative operator.
	 */
	Expression multiply(Expression multiplication, RewritingProcess process);

	/**
	 * Returns the n-th root for an expression, if an exact value exists, or null otherwise. 
	 * @param n
	 * @param expression
	 * @return
	 */
	Expression getNthRoot(int n, Expression expression);

	/**
	 * The result of multiplying a value to itself n times, where both value and n may be symbolic expressions.
	 */
	Expression multiplyNTimes(Expression value, Expression n, RewritingProcess process);
}
