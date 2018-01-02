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
package com.sri.ai.grinder.sgdpllt.group;

import static com.sri.ai.expresso.helper.Expressions.FALSE;
import static com.sri.ai.expresso.helper.Expressions.TRUE;
import static com.sri.ai.expresso.helper.Expressions.ZERO;
import static com.sri.ai.expresso.helper.Expressions.apply;
import static com.sri.ai.expresso.helper.Expressions.makeSymbol;
import static com.sri.ai.grinder.sgdpllt.library.FunctorConstants.EQUAL;

import java.util.List;
import java.util.ListIterator;
import java.util.Random;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.core.DefaultUniversallyQuantifiedFormula;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.grinder.sgdpllt.api.Context;
import com.sri.ai.grinder.sgdpllt.library.boole.And;
import com.sri.ai.grinder.sgdpllt.library.controlflow.IfThenElse;

/**
 * Object representing a group on booleans and conjunction.
 * 
 * @author braz
 *
 */
@Beta
public class Conjunction extends AbstractQuantifierBasedGroup {
	
	@Override
	public Expression additiveIdentityElement() {
		return TRUE;
	}

	@Override
	public boolean isAdditiveAbsorbingElement(Expression value) {
		boolean result = value.equals(Expressions.FALSE);
		return result;
	}

	@Override
	public Expression add(Expression value1, Expression value2, Context context) {
		return And.make(value1, value2);
	}

	@Override
	public Expression addNTimes(Expression value, Expression n, Context context) {
		Expression result;
		if (value.equals(TRUE) || n.equals(ZERO)) {
			result = TRUE;
		}
		else if (n.getValue() instanceof Number) { // we already know value is not true and n is greater than zero from the previous condition having failed
			result = FALSE;
		}
		// n is a symbolic value, so now it all depends on its being greater than zero
		else if (IfThenElse.isIfThenElse(n)) {
			Expression condition  = IfThenElse.condition(n);
			Expression thenBranch = IfThenElse.thenBranch(n);
			Expression elseBranch = IfThenElse.elseBranch(n);
			Expression newThenBranch = addNTimes(value, thenBranch, context);
			Expression newElseBranch = addNTimes(value, elseBranch, context);
			result = IfThenElse.make(condition, newThenBranch, newElseBranch, false); // do not simplify to condition so it is normalized
		}
		else {
			// it will only be true if n is zero
			result = apply(EQUAL, n, Expressions.ZERO);
		}
		return result;
	}

	@Override
	public boolean isIdempotent() {
		return true;
	}
	
	@Override
	public Expression makeRandomConstant(Random random) {
		Expression result = makeSymbol(random.nextBoolean());
		return result;
	}

	@Override
	public Expression makeQuantifiedExpression(List<Expression> indexExpressions, Expression body) {
		Expression current = body;
		ListIterator<Expression> iterator = indexExpressions.listIterator(indexExpressions.size());
		while (iterator.hasPrevious()) {
			Expression indexExpression = iterator.previous();
			current = new DefaultUniversallyQuantifiedFormula(indexExpression, current);
		}
		return current;
	}
}
