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
package com.sri.ai.grinder.plaindpll.problemtype;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.plaindpll.api.GroupProblemType;
import com.sri.ai.grinder.plaindpll.group.AssociativeCommutativeGroup;


/**
 * An abstract implementation of {@link GroupProblemType} providing basic functionality, such as a group property.
 * 
 * @author braz
 *
 */
abstract public class AbstractGroupProblemType implements GroupProblemType {
	
	// The reason we choose to implement {@link GroupProblemType}s by keeping an internal group field,
	// as opposed to extending {@link AssociativeCommutativeGroup} classes,
	// is that we want to write implementations for the methods of {@link GroupProblemType}
	// in a generic way, and only once, to be shared among the multiple implementations.
	// If we extended {@link AssociativeCommutativeGroup} classes, we would not
	// be able to also extend abstract {@link GroupProblemType} classes with this generic code.
	// By keeping instead a group field, we can have such abstract classes and define
	// an implementation of {@link GroupProblemType} for each type of group
	// that simply indicates which group goes with it.
	
	private AssociativeCommutativeGroup group;
	
	public AbstractGroupProblemType(AssociativeCommutativeGroup group) {
		this.group = group;
	}

	public AssociativeCommutativeGroup getGroup() {
		return group;
	}

	@Override
	public Expression additiveIdentityElement() { 
		return getGroup().additiveIdentityElement();
	}

	@Override
	public boolean isAdditiveAbsorbingElement(Expression value) {
		return getGroup().isAdditiveAbsorbingElement(value);
	}

	@Override
	public Expression add(Expression value1, Expression value2, RewritingProcess process) {
		return getGroup().add(value1, value2, process);
	}

	@Override
	public Expression addNTimes(Expression value, Expression n, RewritingProcess process) {
		return getGroup().addNTimes(value, n, process);
	}

	@Override
	public boolean isIdempotent() {
		return getGroup().isIdempotent();
	}
}
