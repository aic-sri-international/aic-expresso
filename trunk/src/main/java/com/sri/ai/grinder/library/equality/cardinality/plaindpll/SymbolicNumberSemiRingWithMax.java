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

import static com.sri.ai.expresso.helper.Expressions.ZERO;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.library.FunctorConstants;
import com.sri.ai.grinder.library.controlflow.IfThenElse;
import com.sri.ai.util.math.Rational;

/**
 * Object representing a symbolic numeric semiring (that is, numeric expressions do not need to be constant numbers) with the maximization operation.
 * 
 * @author braz
 *
 */
@Beta
public class SymbolicNumberSemiRingWithMax implements SemiRing {
	
	private static final Expression MINUS_INFINITY = Expressions.parse("-infinity");

	@Override
	public Expression additiveIdentityElement() {
		return MINUS_INFINITY;
	}

	@Override
	public boolean isAbsorbingElement(Expression value) {
		return false;
	}

	@Override
	public Expression add(Expression value1, Expression value2, RewritingProcess process) {
		Expression result;
		if (value1.getValue() instanceof Number && value2.getValue() instanceof Number) {
			Rational rationalValue1 = value1.rationalValue();
			Rational rationalValue2 = value2.rationalValue();
			if (rationalValue1.compareTo(rationalValue2) > 0) {
				result = value1;
			}
			else {
				result = value2;
			}
		}
		else {
			result = Expressions.apply(FunctorConstants.MAX, value1, value2);
		}
		return result;
	}

	@Override
	public Expression addNTimes(Expression valueToBeAdded, Expression n, RewritingProcess process) {
		Expression result;
		if (n.equals(ZERO)) {
			result = additiveIdentityElement();
		}
		else if (IfThenElse.isIfThenElse(n)) { // it is important that this condition is tested before the next, because n can be conditional on splitters while valueToBeSummed can be conditioned on conditions in the unconditional solution language (such as | Everything | - 1 > 0), and we want splitters to be over non-splitter conditions
			Expression condition  = IfThenElse.getCondition(n);
			Expression thenBranch = IfThenElse.getThenBranch(n);
			Expression elseBranch = IfThenElse.getElseBranch(n);
			Expression newThenBranch = addNTimes(valueToBeAdded, thenBranch, process);
			Expression newElseBranch = addNTimes(valueToBeAdded, elseBranch, process);
			result = IfThenElse.make(condition, newThenBranch, newElseBranch, false); // do not simplify to condition so it is a DPLL solution
		}
		else if (IfThenElse.isIfThenElse(valueToBeAdded)) {
			Expression condition = IfThenElse.getCondition(valueToBeAdded);
			Expression thenBranch = IfThenElse.getThenBranch(valueToBeAdded);
			Expression elseBranch = IfThenElse.getElseBranch(valueToBeAdded);
			
			Expression newThenBranch = addNTimes(thenBranch, n, process);
			Expression newElseBranch = addNTimes(elseBranch, n, process);
			
			result = IfThenElse.make(condition, newThenBranch, newElseBranch);
		}
		else {
			result = valueToBeAdded;
		}
		return result;
	}
}