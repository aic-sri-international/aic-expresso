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
package com.sri.ai.grinder.sgdpll2.core.solver;

import java.util.Collection;
import java.util.Map;

import com.google.common.base.Predicate;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.Type;
import com.sri.ai.grinder.api.Constraint;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.api.TopSimplifier;
import com.sri.ai.grinder.core.AbstractSGVETQuantifierEliminator;
import com.sri.ai.grinder.interpreter.SGDPLLT;
import com.sri.ai.grinder.plaindpll.api.SemiRingProblemType;
import com.sri.ai.grinder.plaindpll.util.DPLLUtil;
import com.sri.ai.grinder.sgdpll2.api.ConstraintTheory;
import com.sri.ai.grinder.sgdpll2.core.constraint.CompleteMultiVariableConstraint;

/**
 * 
 * @author braz
 *
 */
public class SGVET extends AbstractSGVETQuantifierEliminator {

	private ConstraintTheory constraintTheory;
	
	public SGVET(TopSimplifier topSimplifier, SemiRingProblemType problemType, ConstraintTheory constraintTheory) {
		super(new SGDPLLT(topSimplifier, problemType, constraintTheory), problemType);
		this.constraintTheory = constraintTheory;
	}

	@Override
	public boolean isVariable(Expression expression, RewritingProcess process) {
		return constraintTheory.isVariable(expression, process);
	}

	@Override
	public Constraint makeTrueConstraint(Collection<Expression> indices) {
		return new CompleteMultiVariableConstraint(constraintTheory);
	}
	
	public ConstraintTheory getConstraintTheory() {
		return constraintTheory;
	}
	
	@Override
	public RewritingProcess makeProcess(
			Map<String, String> mapFromSymbolNameToTypeName, Map<String, String> mapFromCategoricalTypeNameToSizeString,
			Collection<Type> additionalTypes, Predicate<Expression> isUniquelyNamedConstantPredicate) {
		
		RewritingProcess result = DPLLUtil.makeProcess(
						mapFromSymbolNameToTypeName,
						mapFromCategoricalTypeNameToSizeString, additionalTypes,
						isUniquelyNamedConstantPredicate);
		return result;
	}
}