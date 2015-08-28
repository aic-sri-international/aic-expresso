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
package com.sri.ai.grinder.plaindpll.theory;

import static com.sri.ai.util.Util.map;

import java.util.Map;

import com.google.common.annotations.Beta;
import com.google.common.base.Function;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.api.Rewriter;
import com.sri.ai.grinder.api.Simplifier;
import com.sri.ai.grinder.core.AbstractMapBasedSimplifierWithMakeMethods;
import com.sri.ai.grinder.library.Disequality;
import com.sri.ai.grinder.library.Equality;
import com.sri.ai.grinder.library.FunctorConstants;
import com.sri.ai.grinder.library.boole.And;
import com.sri.ai.grinder.library.boole.Equivalence;
import com.sri.ai.grinder.library.boole.ForAll;
import com.sri.ai.grinder.library.boole.Implication;
import com.sri.ai.grinder.library.boole.Not;
import com.sri.ai.grinder.library.boole.Or;
import com.sri.ai.grinder.library.boole.ThereExists;
import com.sri.ai.grinder.library.controlflow.IfThenElse;
import com.sri.ai.grinder.library.number.Division;
import com.sri.ai.grinder.library.number.GreaterThan;
import com.sri.ai.grinder.library.number.GreaterThanOrEqualTo;
import com.sri.ai.grinder.library.number.LessThan;
import com.sri.ai.grinder.library.number.LessThanOrEqualTo;
import com.sri.ai.grinder.library.number.Minus;
import com.sri.ai.grinder.library.number.Plus;
import com.sri.ai.grinder.library.number.Times;
import com.sri.ai.grinder.plaindpll.api.ConstraintTheory;
import com.sri.ai.grinder.plaindpll.api.InputTheory;
import com.sri.ai.grinder.plaindpll.core.SGDPLLT;
import com.sri.ai.grinder.plaindpll.problemtype.Satisfiability;
import com.sri.ai.grinder.plaindpll.problemtype.Validity;
import com.sri.ai.util.base.NullaryFunction;

/**
 * A default {@link InputTheory} with commonly used functions.
 * 
 * @author braz
 *
 */
@Beta
public class DefaultInputTheory extends AbstractMapBasedSimplifierWithMakeMethods implements InputTheory {
	
	protected ConstraintTheory constraintTheory;
	private static Rewriter plus = new Plus();
	
	public DefaultInputTheory(ConstraintTheory constraintTheory) {
		this.constraintTheory = constraintTheory;
	}

	@Override
	public ConstraintTheory getConstraintTheory() {
		return constraintTheory;
	}

	private static Rewriter times = new Times();

	@Override
	public Map<String, Simplifier> makeFunctionApplicationSimplifiers() {
		return map(
				FunctorConstants.EQUALITY,        (Simplifier) (f, process) ->
				Equality.simplify(f, process),

				FunctorConstants.DISEQUALITY,     (Simplifier) (f, process) ->
				Disequality.simplify(f, process),

				FunctorConstants.AND,             (Simplifier) (f, process) ->
				And.simplify(f),

				FunctorConstants.OR,              (Simplifier) (f, process) ->
				Or.simplify(f),

				FunctorConstants.NOT,             (Simplifier) (f, process) ->
				Not.simplify(f),

				FunctorConstants.IF_THEN_ELSE,    (Simplifier) (f, process) ->
				IfThenElse.simplify(f),

				FunctorConstants.EQUIVALENCE,     (Simplifier) (f, process) ->
				Equivalence.simplify(f),

				FunctorConstants.IMPLICATION,     (Simplifier) (f, process) ->
				Implication.simplify(f),

				FunctorConstants.CARDINALITY,     (Simplifier) (f, process) ->
				{ Expression type = (Expression) process.getGlobalObject(f); return type == null? f : type; },

				FunctorConstants.TIMES,           (Simplifier) (f, process) ->
				times.rewrite(f, process),

				FunctorConstants.DIVISION,        (Simplifier) (f, process) ->
				Division.simplify(f),

				FunctorConstants.PLUS,            (Simplifier) (f, process) ->
				plus.rewrite(f, process),

				FunctorConstants.MINUS,           (Simplifier) (f, process) ->
				(f.numberOfArguments() == 2? Minus.simplify(f) : f),

				FunctorConstants.LESS_THAN,                 (Simplifier) (f, process) ->
				LessThan.simplify(f),

				FunctorConstants.LESS_THAN_OR_EQUAL_TO,     (Simplifier) (f, process) ->
				LessThanOrEqualTo.simplify(f),

				FunctorConstants.GREATER_THAN,              (Simplifier) (f, process) ->
				GreaterThan.simplify(f),

				FunctorConstants.GREATER_THAN_OR_EQUAL_TO,  (Simplifier) (f, process) ->
				GreaterThanOrEqualTo.simplify(f)
				);
	}

	@Override
	public Map<String, Simplifier> makeSyntacticFormTypeSimplifiers() {
		return map(
				ForAll.SYNTACTIC_FORM_TYPE,                             (Simplifier) (f, process) ->
				(new SGDPLLT(this, new Validity())).rewrite(f, process),

				ThereExists.SYNTACTIC_FORM_TYPE,                        (Simplifier) (f, process) ->
				(new SGDPLLT(this, new Satisfiability())).rewrite(f, process)
				);
	}

	@Override
	public Expression getRandomInputExpression(
			String targetType,
			NullaryFunction<String> getType, Function<String, Expression> getVariable, Function<String, Expression> getConstant) {
		// TODO Auto-generated method stub
		return null;
	}
}