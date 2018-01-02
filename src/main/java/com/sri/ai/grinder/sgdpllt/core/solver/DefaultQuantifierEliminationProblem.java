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
package com.sri.ai.grinder.sgdpllt.core.solver;

import static com.sri.ai.util.Util.list;

import java.util.List;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.sgdpllt.api.Context;
import com.sri.ai.grinder.sgdpllt.api.QuantifierEliminationProblem;
import com.sri.ai.grinder.sgdpllt.group.AssociativeCommutativeGroup;

@Beta
public class DefaultQuantifierEliminationProblem implements QuantifierEliminationProblem {
	
	final public AssociativeCommutativeGroup group;
	final public Expression index;
	final public Expression indexType;
	final public Expression constraint;
	final public Expression body;

	public DefaultQuantifierEliminationProblem(AssociativeCommutativeGroup group, Expression index, Expression indexType, Expression constraint, Expression body) {
		super();
		this.group = group;
		this.index = index;
		this.indexType = indexType;
		this.constraint = constraint;
		this.body = body;
	}
	
	public DefaultQuantifierEliminationProblem(AssociativeCommutativeGroup group, Expression index, Expression indexType, Expression body, Context context) {
		super();
		this.group = group;
		this.index = index;
		this.indexType = indexType;
		this.constraint = context.getTheory().makeSingleVariableConstraint(index, context);
		this.body = body;
	}
	
	@Override
	public AssociativeCommutativeGroup getGroup() {
		return group;
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
	public Expression getConstraint() {
		return constraint;
	}

	@Override
	public Expression getBody() {
		return body;
	}
	
	@Override
	public DefaultQuantifierEliminationProblem makeWithNewIndexConstraint(Expression newConstraint) {
		return new DefaultQuantifierEliminationProblem(group, index, indexType, newConstraint, body);
	}

	@Override
	public DefaultQuantifierEliminationProblem makeWithNewBody(Expression newBody) {
		return new DefaultQuantifierEliminationProblem(group, index, indexType, constraint, newBody);
	}

	@Override
	public String toString() {
		return "Quantifier elimination problem on " + group + ", " + index + ", " + constraint + ", " + body;
	}

	@Override
	public Expression toExpression() {
		Expression result = getGroup().makeProblemExpression(this);
		return result;
	}
}