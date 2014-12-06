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
import static com.sri.ai.util.Util.arrayList;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.grinder.api.Rewriter;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.core.TotalRewriter;
import com.sri.ai.grinder.library.controlflow.IfThenElse;
import com.sri.ai.grinder.library.number.FlattenMinusInPlus;
import com.sri.ai.grinder.library.number.Minus;
import com.sri.ai.grinder.library.number.Plus;
import com.sri.ai.grinder.library.number.UnaryMinus;

/**
 * Object representing a symbolic numeric semiring (that is, numeric expressions do not need to be constant numbers).
 * 
 * @author braz
 *
 */
@Beta
public class SymbolicNumberSemiRing implements SemiRing {
	
	@Override
	public Expression additiveIdentityElement() {
		return ZERO;
	}

	@Override
	public boolean isMaximum(Expression value) {
		return false;
	}

	@Override
	public Expression add(Expression value1, Expression value2, RewritingProcess process) {
		Expression result;
		if (value1.getValue() instanceof Number && value2.getValue() instanceof Number) { // not necessary, as else clause is generic enough to deal with this case as well, but hopefully this saves time.
			result = Expressions.makeSymbol(value1.rationalValue().add(value2.rationalValue()));
		}
		else {
			Expression sum = Plus.make(arrayList(value1, value2));
			result = plusAndMinusRewriter.rewrite(sum, process);
		}
		return result;
	}

	private static Rewriter plusAndMinusRewriter = new TotalRewriter(new Plus(), new Minus(), new UnaryMinus(), new FlattenMinusInPlus());
	
	@Override
	public Expression addNTimes(Expression valueToBeSummed, Expression n, RewritingProcess process) {
		Expression result;
		if (n.equals(ZERO)) {
			result = ZERO;
		}
		else if (IfThenElse.isIfThenElse(n)) { // it is important that this condition is tested before the next, because n can be conditional on splitters while valueToBeSummed can be conditioned on conditions in the unconditional solution language (such as | Everything | - 1 > 0), and we want splitters to be over non-splitter conditions
			Expression condition  = IfThenElse.getCondition(n);
			Expression thenBranch = IfThenElse.getThenBranch(n);
			Expression elseBranch = IfThenElse.getElseBranch(n);
			Expression newThenBranch = addNTimes(valueToBeSummed, thenBranch, process);
			Expression newElseBranch = addNTimes(valueToBeSummed, elseBranch, process);
			result = IfThenElse.make(condition, newThenBranch, newElseBranch, false); // do not simplify to condition so it is a DPLL solution
		}
		else if (IfThenElse.isIfThenElse(valueToBeSummed)) {
			Expression condition = IfThenElse.getCondition(valueToBeSummed);
			Expression thenBranch = IfThenElse.getThenBranch(valueToBeSummed);
			Expression elseBranch = IfThenElse.getElseBranch(valueToBeSummed);
			
			Expression newThenBranch = addNTimes(thenBranch, n, process);
			Expression newElseBranch = addNTimes(elseBranch, n, process);
			
			result = IfThenElse.make(condition, newThenBranch, newElseBranch);
		}
		else {
			if (valueToBeSummed.equals(ZERO)) { // optimization
				result = ZERO;
			}
			else if (valueToBeSummed.equals(ONE)) { // optimization
				result = n;
			}
			else {
				result = Expressions.makeSymbol(valueToBeSummed.rationalValue().multiply(n.rationalValue()));
			}
		}
		return result;
	}
}