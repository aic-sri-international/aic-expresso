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
package com.sri.ai.grinder.library.boole;

import java.util.List;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.grinder.api.Context;
import com.sri.ai.grinder.api.ExpressionStepSolver;
import com.sri.ai.grinder.core.constraint.ContextSplitting;
import com.sri.ai.grinder.library.FunctorConstants;

/**
 * A step solver that evaluates general boolean formulas by splitting on entire sub-formulas.
 * For example, <code>(p or q) and (q and r)</code> may result in splitter <code>p or q</code>,
 * with sequel step solvers on <code>q and r</code> if the splitter is true, and <code>false</code> if the splitter is false.
 * 
 * Currently incomplete because we need more general contexts in order to split on splitters more general than literals.
 * 
 * @author braz
 *
 */
@Beta
public class INCOMPLETE_BooleanFormulaExpressionStepSolver implements ExpressionStepSolver {

	private Expression booleanFormula;
	
	public INCOMPLETE_BooleanFormulaExpressionStepSolver(Expression booleanFormula) {
		this.booleanFormula = booleanFormula;
	}

	@Override
	public Step step(Context context) {
		Step result;
		
		Expression splitter;
		Expression problemIfSplitterIsTrue;
		Expression problemIfSplitterIsFalse;
		
		Expression functor = booleanFormula.getFunctor();
		String functorString = functor == null? "" : functor.toString();
			switch (functorString) {
			case FunctorConstants.AND :
				List<Expression> arguments = booleanFormula.getArguments();
				int numberOfArguments = booleanFormula.numberOfArguments();
				splitter = arguments.get(0);
				List<Expression> rest = arguments.subList(1, numberOfArguments);
				problemIfSplitterIsTrue = And.make(rest);
				problemIfSplitterIsFalse = Expressions.FALSE;
				break;
			default:
				splitter = booleanFormula;
				problemIfSplitterIsTrue = Expressions.TRUE;
				problemIfSplitterIsFalse = Expressions.FALSE;
		}

		// IMPORTANT IMPORTANT IMPORTANT IMPORTANT IMPORTANT IMPORTANT IMPORTANT IMPORTANT
		// Need to extend context to non-conjunctive formulas so I can split on formulas below 
		ContextSplitting contextSplitting = new ContextSplitting(null, context);
		ExpressionStepSolver ifFirstConjunctIsTrue;
		ExpressionStepSolver ifFirstConjunctIsFalse;
		switch (contextSplitting.getResult()) {
		case LITERAL_IS_TRUE:
			ifFirstConjunctIsTrue  = new INCOMPLETE_BooleanFormulaExpressionStepSolver(problemIfSplitterIsTrue);
			result = ifFirstConjunctIsTrue.step(contextSplitting.getContextAndLiteral());
			break;
		case LITERAL_IS_FALSE:
			ifFirstConjunctIsFalse = new INCOMPLETE_BooleanFormulaExpressionStepSolver(problemIfSplitterIsFalse);
			result = ifFirstConjunctIsFalse.step(contextSplitting.getContextAndLiteralNegation());
			break;
		case LITERAL_IS_UNDEFINED:
			ifFirstConjunctIsTrue  = new INCOMPLETE_BooleanFormulaExpressionStepSolver(problemIfSplitterIsTrue);
			ifFirstConjunctIsFalse = new INCOMPLETE_BooleanFormulaExpressionStepSolver(problemIfSplitterIsFalse);
			result = new ItDependsOn(splitter, contextSplitting, ifFirstConjunctIsTrue, ifFirstConjunctIsFalse);
			break;
		case CONSTRAINT_IS_CONTRADICTORY:
			throw new Error("Should not be operating under contradictory context, but got " + context);
		default:
			throw new Error("Unexpected context splitting result: " + contextSplitting.getResult());
		}

		return result;
	}

	@Override
	public INCOMPLETE_BooleanFormulaExpressionStepSolver clone()  {
		try {
			return (INCOMPLETE_BooleanFormulaExpressionStepSolver) super.clone();
		}
		catch (CloneNotSupportedException e) {
			throw new Error(e);
		}
	}
}