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

import java.util.List;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.api.MultiQuantifierEliminationProblem;
import com.sri.ai.grinder.api.SingleQuantifierEliminationProblem;
import com.sri.ai.grinder.group.AssociativeCommutativeGroup;

@Beta
public abstract class MultiQuantifierEliminationProblemWrapper implements MultiQuantifierEliminationProblem {
	
	final public MultiQuantifierEliminationProblem baseProblem;

	public MultiQuantifierEliminationProblemWrapper(MultiQuantifierEliminationProblem baseProblem) {
		this.baseProblem = baseProblem;
	}
	
	/**
	 * A method replicating this instance, but with a new wrapped object;
	 * extending classes should override this method to define specific wrapping functionality.
	 * All wrappers should use something like this.
	 * @param problem
	 * @return
	 */
	abstract public MultiQuantifierEliminationProblemWrapper copyWithNewProblem(MultiQuantifierEliminationProblem problem);
	
	@Override
	abstract public SingleQuantifierEliminationProblem getFirstIndexVersion();

	@Override
	public AssociativeCommutativeGroup getGroup() {
		return baseProblem.getGroup();
	}

	@Override
	public List<Expression> getIndices() {
		return baseProblem.getIndices();
	}

	@Override
	public List<Expression> getIndicesTypes() {
		return baseProblem.getIndicesTypes();
	}

	@Override
	public Expression getConstraint() {
		return baseProblem.getConstraint();
	}

	@Override
	public Expression getBody() {
		return baseProblem.getBody();
	}
	
	@Override
	public MultiQuantifierEliminationProblemWrapper makeWithNewIndexConstraint(Expression newConstraint) {
		return copyWithNewProblem(baseProblem.makeWithNewIndexConstraint(newConstraint));
	}

	@Override
	public MultiQuantifierEliminationProblemWrapper makeWithNewBody(Expression newBody) {
		return copyWithNewProblem(baseProblem.makeWithNewBody(newBody));
	}

	@Override
	public String toString() {
		return baseProblem.toString();
	}

	@Override
	public Expression toExpression() {
		return baseProblem.toExpression();
	}
}