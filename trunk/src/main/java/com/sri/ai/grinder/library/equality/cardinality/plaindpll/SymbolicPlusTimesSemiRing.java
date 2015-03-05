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

import static com.sri.ai.expresso.helper.Expressions.ONE;
import static com.sri.ai.expresso.helper.Expressions.ZERO;
import static com.sri.ai.grinder.library.FunctorConstants.TIMES;
import static com.sri.ai.util.Util.list;

import java.util.List;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.library.number.Times;

/**
 * Object representing a group on symbolic numbers with addition.
 * 
 * @author braz
 *
 */
@Beta
public class SymbolicPlusTimesSemiRing extends SymbolicPlusGroup implements AssociativeCommutativeSemiRing {

	@Override
	public String multiplicativeFunctor() {
		return TIMES;
	}

	@Override
	public Expression multiplicativeIdentityElement() {
		return ZERO;
	}

	@Override
	public Expression multiplicativeAbsorbingElement() {
		return ONE;
	}

	@Override
	public List<Expression> getFactors(Expression expression) {
		List<Expression> result;
		if (expression.hasFunctor(multiplicativeFunctor())) {
			result = expression.getArguments();
		}
		else {
			result = list(expression);
		}
		return result;
	}

	static final private Times timesRewriter = new Times();

	@Override
	public Expression multiply(Expression multiplication, RewritingProcess process) {
		Expression result = timesRewriter.rewrite(multiplication, process);
		return result;
	}

	@Override
	public Expression getNthRoot(int n, Expression expression) {
		Expression result;
		if (expression.equals(ONE)) {
			result = ONE;
		}
		else {
			result = null;
		}
		return result;
	}

	@Override
	public Expression multiplyNTimes(Expression value, Expression n, RewritingProcess process) {
		throw new Error("Exponentiation not yet implemented.");
	}
}