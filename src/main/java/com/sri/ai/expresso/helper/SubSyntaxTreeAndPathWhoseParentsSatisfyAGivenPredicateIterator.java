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
package com.sri.ai.expresso.helper;

import java.util.List;

import com.google.common.annotations.Beta;
import com.google.common.base.Predicate;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.util.base.Pair;
import com.sri.ai.util.collect.PredicateIterator;

/**
 * An iterator over the pairs of sub-expressions and paths of a given
 * expression, whose parents pairs satisfy a given predicate.
 * 
 * @author braz
 */
@Beta
public class SubSyntaxTreeAndPathWhoseParentsSatisfyAGivenPredicateIterator extends PredicateIterator<Pair<Expression, List<Integer>>> {
	
	public SubSyntaxTreeAndPathWhoseParentsSatisfyAGivenPredicateIterator(Expression expression, Predicate<Pair<Expression, List<Integer>>> parentsPredicate) {
		super(new SubSyntaxTreeAndPathMarkingThoseWhoseParentsSatisfyAGivenPredicateIterator(expression, parentsPredicate), null);
		predicate = ((SubSyntaxTreeAndPathMarkingThoseWhoseParentsSatisfyAGivenPredicateIterator)base).getIsMarkedPredicate();
		// We cannot provide the predicate to the super constructor because it depends on the base iterator being constructed during that call.
		// Note that this depends on the base iterator being depth-first,
		// because 'predicate' can only possibly be true for expressions whose parents have already been visited. 
	}
}
