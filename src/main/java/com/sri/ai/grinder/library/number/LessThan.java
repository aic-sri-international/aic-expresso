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
package com.sri.ai.grinder.library.number;

import static com.sri.ai.expresso.helper.Expressions.FALSE;
import static com.sri.ai.expresso.helper.Expressions.INFINITY;
import static com.sri.ai.expresso.helper.Expressions.MINUS_INFINITY;
import static com.sri.ai.expresso.helper.Expressions.TRUE;
import static com.sri.ai.expresso.helper.Expressions.isNumber;
import static com.sri.ai.expresso.helper.Expressions.makeSymbol;
import static com.sri.ai.util.Util.lessThan;

import java.util.LinkedHashSet;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.core.HasKind;
import com.sri.ai.grinder.library.BinaryOperator;
import com.sri.ai.grinder.library.FunctorConstants;
import com.sri.ai.util.Util;

/**
 * Implements a rewriter for the less than operation.
 * 
 * @author braz
 */
@Beta
public class LessThan extends BinaryOperator {

	public LessThan() {
		this.functors = new LinkedHashSet<Expression>(); 
		this.functors.add(Expressions.makeSymbol(FunctorConstants.LESS_THAN));
		//
		this.firstType  = Number.class;
		this.secondType = Number.class;
		
		this.setReifiedTests(new HasKind(FunctorConstants.LESS_THAN));
	}
	
	@Override
	protected Object operation(Expression expression1, Expression expression2) {
		return Util.lessThan(expression1.rationalValue(), expression2.rationalValue());
	}
	
	/**
	 * Receives an application of {@link FunctorConstants.LESS_THAN} and evaluates it if possible.
	 * @param lessThanApplication
	 * @param process TODO
	 * @return
	 */
	public static Expression simplify(Expression lessThanApplication, RewritingProcess process) {
		Expression result;
		if (lessThanApplication.get(0).equals(lessThanApplication.get(1))) {
			result = FALSE; // not less than itself
		}
		else if (isNumber(lessThanApplication.get(0)) && isNumber(lessThanApplication.get(1))) {
			result = makeSymbol(lessThan(lessThanApplication.get(0).rationalValue(), lessThanApplication.get(1).rationalValue()));
		}
		else if ( lessThanApplication.get(0).equals(MINUS_INFINITY) && !lessThanApplication.get(1).equals(MINUS_INFINITY)) {
			result = TRUE;
		}
		else if ( lessThanApplication.get(0).equals(INFINITY)       && !lessThanApplication.get(1).equals(INFINITY)) {
			result = FALSE;
		}
		else if (!lessThanApplication.get(0).equals(MINUS_INFINITY) &&  lessThanApplication.get(1).equals(MINUS_INFINITY)) {
			result = FALSE;
		}
		else if (!lessThanApplication.get(0).equals(INFINITY)       &&  lessThanApplication.get(1).equals(INFINITY)) {
			result = TRUE;
		}
		else {
			result = lessThanApplication;
		}
		return result;
	}
}
