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
package com.sri.ai.expresso.api;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.core.DefaultIntensionalMultiSet;
import com.sri.ai.expresso.core.DefaultIntensionalUniSet;

/**
 * An {@link Expression} that represents an intensionally defined set.
 * <p>
 * An object <code>E</code> belongs to an intensionally defined set <code>{ (on I) H | C }</code>
 * if and only if there is a value <code>I'</code> for the index expressions <code>I</code> such that <code>I'</code>
 * satisfies <code>C</code> and <code>E = H(I')</code>, where <code>H(I')</code> is the result of
 * replacing <code>I</code> by <code>I'</code> in <code>H</code>.
 * <p> 
 * 
 * @author braz
 */
@Beta
public interface IntensionalSet extends QuantifiedExpression {
	
	String SYNTACTIC_FORM_TYPE = "Intensional set";
	//
	String UNI_SET_LABEL          = "{ . . . }";
	String MULTI_SET_LABEL        = "{{ . . . }}";
	String CONDITION_LABEL        = ":";
	String SCOPED_VARIABLES_LABEL = "( on . )";

	/**
	 * Indicates whether the set is a uniset.
	 */
	boolean isUniSet();
	
	/**
	 * Indicates whether the set is a multiset.
	 */
	boolean isMultiSet();

	/**
	 * Returns the head <code>H</code> of the set <code>{ (on I) H | C }</code>
	 */
	Expression getHead();

	/**
	 * Returns the condition <code>C</code> of the set <code>{ (on I) H | C }</code>
	 */
	Expression getCondition();

	/**
	 * Returns a new instance with the head replaced by given new one.
	 */
	Expression setHead(Expression newHead);

	/**
	 * Returns a new instance with the condition replaced by given new one.
	 */
	Expression setCondition(Expression newCondition);

	/**
	 * Returns a new instance with the head and condition replaced by given new ones.
	 */
	IntensionalSet setHeadAndCondition(Expression newHead, Expression newCondition);

	@Override
	IntensionalSet setIndexExpressions(IndexExpressionsSet newIndexExpressions);
	
	/**
	 * Makes an intensional multi-set.
	 * @param indexExpressions
	 * @param head
	 * @param condition
	 * @return
	 */
	public static Expression intensionalMultiSet(IndexExpressionsSet indexExpressions, Expression head, Expression condition) {
		return make(MULTI_SET_LABEL, indexExpressions, head, condition);
	}

	/**
	 * Makes an intensional uni-set.
	 * @param indexExpressions
	 * @param head
	 * @param condition
	 * @return
	 */
	public static Expression intensionalUniSet(IndexExpressionsSet indexExpressions, Expression head, Expression condition) {
		return make(UNI_SET_LABEL, indexExpressions, head, condition);
	}

	public static Expression make(Object label, IndexExpressionsSet indexExpressions, Expression head, Expression condition) {
		Expression result;
		if (label.equals(UNI_SET_LABEL)) {
			result = new DefaultIntensionalUniSet(indexExpressions, head, condition);
		}
		else if (label.equals(MULTI_SET_LABEL)){
			result = new DefaultIntensionalMultiSet(indexExpressions, head, condition);
		}
		else {
			throw new Error("makeSetFromIndexExpressions requires UNI_SET_LABEL or MULTI_SET_LABEL");
		}
		return result;
	}
}
