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

import java.util.Collection;
import java.util.LinkedHashMap;
import java.util.Map;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.util.Util;
import com.sri.ai.util.collect.StackedHashMap;

@SuppressWarnings("serial")
/**
 * Represents and manipulates constraints in the theory of equalities of symbols (variables and constants).
 */
@Beta
public class SymbolEqualityLogicConstraint extends LinkedHashMap<Expression, Collection<Expression>> implements TheoryConstraint {

	// This class delegates the task of representing equalities to {@link SymbolDisequalityConstraint},
	// and keeps only a map from variables to their "representatives".
	// A representative of a variable is either its constant value or
	// another variable preceding it in the order defined by
	// {@link SymbolDisequalityConstraint#whenChoosingValueForVariableOtherTermIsAlreadyDefined(Expression variable, Expression otherTerm, Collection<Expression> indices, RewritingProcess process)}.
	
	private Map<Expression, Expression> fromVariableToRepresentative;
	private SymbolDisequalityConstraint disequalities;
	
	public SymbolEqualityLogicConstraint() {
		super();
		fromVariableToRepresentative = new LinkedHashMap<Expression, Expression>();
		disequalities = new SymbolDisequalityConstraint();
	}

	public SymbolEqualityLogicConstraint(SymbolEqualityLogicConstraint another) {
		super();
		fromVariableToRepresentative = new StackedHashMap<Expression, Expression>(another.fromVariableToRepresentative);
		disequalities = another.disequalities;
	}

	@Override
	public Expression pickSplitter(Collection<Expression> indices, RewritingProcess process) {
		Collection<Expression> freeIndices = Util.subtract(indices, fromVariableToRepresentative.keySet()); // here's a reason for keeping indices inside constraints, so that we don't have to do this subtraction everytime.
		Expression result = disequalities.pickSplitter(freeIndices, process);
		return result;
	}

	@Override
	public TheoryConstraint applySplitter(Expression splitter, Collection<Expression> indices, RewritingProcess process) {
		SymbolEqualityLogicConstraint newConstraint = new SymbolEqualityLogicConstraint(this);
		newConstraint.fromVariableToRepresentative.put(splitter.get(0), splitter.get(1));
		newConstraint.disequalities = (SymbolDisequalityConstraint) disequalities.applySplitter(splitter, indices, process);
		if (newConstraint.disequalities == null) {
			newConstraint = null;
		}
		return newConstraint;
	}

	@Override
	public TheoryConstraint applySplitterNegation(Expression splitter, Collection<Expression> indices, RewritingProcess process) {
		SymbolEqualityLogicConstraint newConstraint = new SymbolEqualityLogicConstraint(this);
		newConstraint.disequalities = (SymbolDisequalityConstraint) disequalities.applySplitterNegation(splitter, indices, process);
		if (newConstraint.disequalities == null) {
			newConstraint = null;
		}
		return newConstraint;
	}

	@Override
	public Expression getIndexBoundBySplitterApplicationIfAny(Expression splitter, Collection<Expression> indices) {
		Expression result;
		Expression variable = splitter.get(0);
		if (indices.contains(variable)) {
			result = variable;
		}
		else {
			result = null;
		}
		return result;
	}

	@Override
	public Expression getIndexBoundBySplitterNegationApplicationIfAny(Expression splitter, Collection<Expression> indices) {
		return null;
	}

	@Override
	public Expression modelCount(Collection<Expression> indices, RewritingProcess process) {
		Collection<Expression> freeIndices = Util.subtract(indices, fromVariableToRepresentative.keySet()); // here's a reason for keeping indices inside constraints, so that we don't have to do this subtraction everytime.
		Expression result = disequalities.modelCount(freeIndices, process);
		return result;
	}

	@Override
	public Expression getMostRequiredSplitter(Expression splitterCandidate, Collection<Expression> indices, RewritingProcess process) {
		Expression result = disequalities.getMostRequiredSplitter(splitterCandidate, indices, process);
		return result;
	}
}