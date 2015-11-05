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
package com.sri.ai.grinder.sgdpll2.theory.base;

import static com.sri.ai.grinder.helper.GrinderUtil.getTypeCardinality;
import static com.sri.ai.grinder.library.FunctorConstants.TYPE;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.core.DefaultSyntacticFunctionApplication;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.sgdpll2.api.ConstraintTheory;
import com.sri.ai.grinder.sgdpll2.core.constraint.AbstractSingleVariableConstraint;
import com.sri.ai.util.base.Pair;

/**
 * An abstract single-variable constraint solver providing
 * functionality for theories involving only
 * atoms which are binary function applications.
 * 
 * @author braz
 *
 */
@Beta
public abstract class AbstractSingleVariableConstraintWithBinaryFunctionApplicationAtoms extends AbstractSingleVariableConstraint {

	private static final long serialVersionUID = 1L;
	
	abstract protected Pair<Boolean, Expression> makeSignAndNormalizedAtom(Expression binaryFunctionApplicationLiteral, Expression variable, Expression other);

	/**
	 * If literal is a negation, return an equivalent literal that is a binary function application,
	 * or null if there is no such equivalent literal.
	 * @param literal
	 * @return the binary function application, or null if there is no such equivalent literal.
	 * @throws Error if literal is not a valid literal for this theory
	 */
	abstract protected Expression moveNotInOrNullIfNotPossible(Expression literal);

	public AbstractSingleVariableConstraintWithBinaryFunctionApplicationAtoms(Expression variable, ConstraintTheory constraintTheory) {
		super(variable, constraintTheory);
	}

	public AbstractSingleVariableConstraintWithBinaryFunctionApplicationAtoms(AbstractSingleVariableConstraintWithBinaryFunctionApplicationAtoms other) {
		super(other);
	}

	@Override
	protected Pair<Boolean, Expression> fromLiteralOnVariableToSignAndNormalizedAtom(Expression variable, Expression literal) {
	
		Pair<Boolean, Expression> result;
	
		Expression binaryFunctionApplicationLiteral = moveNotInOrNullIfNotPossible(literal);
		if (binaryFunctionApplicationLiteral == null) { // must be a negation (otherwise binaryFunctionApplicationLiteral == literal)
			result = Pair.make(false, literal.get(0));
		}
		else {
			Expression other = termToWhichVariableIsConstrainedWith(variable, binaryFunctionApplicationLiteral);
			result = makeSignAndNormalizedAtom(binaryFunctionApplicationLiteral, variable, other);
		}
		
		return result;
	}

	/**
	 * Returns the term against which the variable is constrained.
	 * @param variable
	 * @param binaryFunctionApplicationLiteral
	 * @return
	 */
	protected Expression termToWhichVariableIsConstrainedWith(Expression variable, Expression binaryFunctionApplicationLiteral) {
		Expression result;
		if (binaryFunctionApplicationLiteral.get(0).equals(variable)) {
			result = binaryFunctionApplicationLiteral.get(1);
		}
		else if (binaryFunctionApplicationLiteral.get(1).equals(variable)) {
			result = binaryFunctionApplicationLiteral.get(0);
		}
		else {
			throw new Error(getClass().getSimpleName() + ": invalid atom received: " + binaryFunctionApplicationLiteral);
		}
		return result;
	}

	protected long cachedIndexDomainSize = -1;

	public long getVariableDomainSize(RewritingProcess process) {
		if (cachedIndexDomainSize == -1) {
			cachedIndexDomainSize = getTypeCardinality(getVariable(), process);
		}
		return cachedIndexDomainSize;
	}

	public Expression getVariableDomain(RewritingProcess process) {
		Expression variableType = process.getContextualSymbolType(getVariable());
		if (variableType == null) {
			variableType = new DefaultSyntacticFunctionApplication(TYPE, getVariable());
		}
		return variableType;
	}
}