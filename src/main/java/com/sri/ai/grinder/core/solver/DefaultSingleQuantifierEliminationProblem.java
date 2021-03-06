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
package com.sri.ai.grinder.core.solver;

import static com.sri.ai.util.Util.list;

import java.util.List;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.api.Context;
import com.sri.ai.grinder.api.MultiQuantifierEliminationProblem;
import com.sri.ai.grinder.api.SingleQuantifierEliminationProblem;
import com.sri.ai.grinder.group.AssociativeCommutativeGroup;

@Beta
public class DefaultSingleQuantifierEliminationProblem extends AbstractMultiQuantifierEliminationProblem implements SingleQuantifierEliminationProblem {
	
	final public Expression index;
	final public Expression indexType;

	public DefaultSingleQuantifierEliminationProblem(AssociativeCommutativeGroup group, Expression index, Expression indexType, Expression constraint, Expression body) {
		super(group, constraint, body);
		this.index = index;
		this.indexType = indexType;
	}
	
	public DefaultSingleQuantifierEliminationProblem(AssociativeCommutativeGroup group, Expression index, Expression indexType, Expression body, Context context) {
		super(group, context.getTheory().makeSingleVariableConstraint(index, context), body);
		this.index = index;
		this.indexType = indexType;
	}
	
	public DefaultSingleQuantifierEliminationProblem(MultiQuantifierEliminationProblem problem) {
		this(problem.getGroup(), problem.getIndices().get(0), problem.getIndicesTypes().get(0), problem.getConstraint(), problem.getBody());
	}

	@Override
	public Expression getIndex() {
		return index;
	}

	@Override
	public Expression getIndexType() {
		return indexType;
	}

	@Override
	public List<Expression> getIndices() {
		return list(getIndex());
	}

	@Override
	public List<Expression> getIndicesTypes() {
		return list(getIndexType());
	}

	@Override
	public DefaultSingleQuantifierEliminationProblem makeWithNewIndexConstraint(Expression newConstraint) {
		return new DefaultSingleQuantifierEliminationProblem(group, index, indexType, newConstraint, body);
	}

	@Override
	public DefaultSingleQuantifierEliminationProblem makeWithNewBody(Expression newBody) {
		return new DefaultSingleQuantifierEliminationProblem(group, index, indexType, constraint, newBody);
	}

	@Override
	public SingleQuantifierEliminationProblem getFirstIndexVersion() {
		return this;
	}
}