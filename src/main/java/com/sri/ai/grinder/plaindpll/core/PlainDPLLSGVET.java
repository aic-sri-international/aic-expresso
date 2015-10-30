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
package com.sri.ai.grinder.plaindpll.core;

import java.util.Collection;
import java.util.Map;

import com.google.common.base.Predicate;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.api.Constraint;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.core.AbstractSGVETQuantifierEliminatorWithSetup;
import com.sri.ai.grinder.library.equality.cardinality.core.CountsDeclaration;
import com.sri.ai.grinder.plaindpll.api.Constraint1;
import com.sri.ai.grinder.plaindpll.api.ConstraintTheory;
import com.sri.ai.grinder.plaindpll.api.InputTheory;
import com.sri.ai.grinder.plaindpll.api.SemiRingProblemType;
import com.sri.ai.grinder.plaindpll.util.DPLLUtil;

/**
 * 
 * @author braz
 *
 */
public class PlainDPLLSGVET extends AbstractSGVETQuantifierEliminatorWithSetup {

	private ConstraintTheory constraintTheory;
	private InputTheory inputTheory;
	
	public PlainDPLLSGVET(InputTheory inputTheory, SemiRingProblemType problemType) {
		this(inputTheory, problemType, null);
	}

	public PlainDPLLSGVET(InputTheory inputTheory, SemiRingProblemType problemType, CountsDeclaration countsDeclaration) {
		super(new PlainSGDPLLT(inputTheory, problemType, countsDeclaration), problemType);
		this.constraintTheory = inputTheory.getConstraintTheory();
		this.inputTheory = inputTheory;
	}

	@Override
	public boolean isVariable(Expression expression, RewritingProcess process) {
		return constraintTheory.isVariable(expression, process);
	}

	@Override
	public Constraint1 makeTrueConstraint(Collection<Expression> indices) {
		return constraintTheory.makeConstraint(indices);
	}
	
	
	public InputTheory getInputTheory() {
		return inputTheory;
	}
	
	public ConstraintTheory getConstraintTheory() {
		return constraintTheory;
	}
	
	@Override
	public	Expression simplify(Expression expression, RewritingProcess process) {
		return getInputTheory().simplify(expression, process);
	}

	@Override
	public RewritingProcess makeProcess(
			Constraint constraint,
			Map<String, String> mapFromSymbolNameToTypeName, Map<String, String> mapFromTypeNameToSizeString,
			Predicate<Expression> isUniquelyNamedConstantPredicate) {
		
		RewritingProcess result = DPLLUtil.makeProcess(
						(Constraint1) constraint,
						mapFromSymbolNameToTypeName, mapFromTypeNameToSizeString,
						isUniquelyNamedConstantPredicate);
		return result;
	}

}