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
package com.sri.ai.grinder.interpreter;

import static com.sri.ai.grinder.interpreter.SGDPLLT.solve;
import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.core.ExtensionalIndexExpressionsSet;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.helper.GrinderUtil;
import com.sri.ai.grinder.library.CommonSimplifier;
import com.sri.ai.grinder.plaindpll.group.AssociativeCommutativeGroup;
import com.sri.ai.grinder.sgdpll2.api.Constraint2;
import com.sri.ai.grinder.sgdpll2.api.ConstraintTheory;
import com.sri.ai.grinder.sgdpll2.core.constraint.CompleteMultiVariableConstraint;

/**
 * An extension of {@link AbstractCommonInterpreter} re-using {@link CommonSimplifier}
 * (provided by {@link #makeAnotherMapBasedSimplifier()},
 * and augmented with symbolic solvers for
 * summations, and universal and existentially quantified formulas.
 *
 * @author braz
 *
 */
@Beta
public class SymbolicCommonInterpreter extends AbstractCommonInterpreter {

	private ConstraintTheory constraintTheory;
	
	/**
	 * Constructs {@link SymbolicCommonInterpreter} with a constraint theory and
	 * <i>not</i> simplifying literals according to contextual constraint.
	 * @param constraintTheory
	 */
	public SymbolicCommonInterpreter(ConstraintTheory constraintTheory) {
		super();
		this.constraintTheory = constraintTheory;
	}

	public ConstraintTheory getConstraintTheory() {
		return constraintTheory;
	}
	
	@Override
	protected Expression evaluateAggregateOperation(
			AssociativeCommutativeGroup group,
			ExtensionalIndexExpressionsSet indexExpressions,
			Expression indicesConditions,
			Expression body,
			RewritingProcess process) throws Error {

		process = GrinderUtil.extendContextualSymbolsWithIndexExpressions(indexExpressions, process);
		
		// TODO: OPTIMIZATION: this *severely* slows down the algorithm
		// we need to organize things so that we do not depend of pre-simplifications like this
		// but do them once on the fly.
		Expression quantifierFreeBody = apply(body, process);
		Expression quantifierFreeIndicesCondition = apply(indicesConditions, process);

//		Expression quantifierFreeBody = body;
//		Expression quantifierFreeIndicesCondition = indicesConditions;

		Constraint2 contextualConstraint = 
				new CompleteMultiVariableConstraint(constraintTheory);

		Expression result =
				solve(
						group,
						getTopSimplifier(),
						indexExpressions,
						quantifierFreeIndicesCondition,
						quantifierFreeBody,
						contextualConstraint,
						process);
		
		return result;
	}
}