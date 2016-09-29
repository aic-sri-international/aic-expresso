/*
 * Copyright (c) 2016, SRI International
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
package com.sri.ai.expresso.type;

import static com.sri.ai.expresso.helper.Expressions.apply;
import static com.sri.ai.grinder.sgdpllt.library.FunctorConstants.CARTESIAN_PRODUCT;
import static com.sri.ai.grinder.sgdpllt.library.FunctorConstants.FUNCTION_TYPE;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;
import java.util.Random;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.Type;

/**
 * Represents function types.
 * 
 * @author oreilly
 *
 */
@Beta
public class FunctionType implements Type, Serializable {
	private static final long serialVersionUID = 1L;
	
	private Type codomain;
	private List<Type> argumentTypes;
	//
	private String cachedString;
	
	public FunctionType(Type codomain, Type... argumentTypes) {
		this.codomain = codomain;	
		this.argumentTypes = Collections.unmodifiableList(Arrays.asList(argumentTypes));
	}
	
	public Type getCodomain() {
		return codomain;
	}
	
	public int getArity() {
		return getArgumentTypes().size();
	}
	
	public List<Type> getArgumentTypes() {
		return argumentTypes;
	}

	@Override
	public String getName() {
		return toString();
	}

	@Override
	public Iterator<Expression> iterator() {
		return codomain.iterator();
	}

	@Override
	public boolean contains(Expression uniquelyNamedConstant) {
		return codomain.contains(uniquelyNamedConstant);
	}

	@Override
	public Expression sampleUniquelyNamedConstant(Random random) {
		return codomain.sampleUniquelyNamedConstant(random);
	}

	@Override
	public Expression cardinality() {
		return codomain.cardinality();
	}
	
	@Override
	public String toString() {
		if (cachedString == null) {
			cachedString = apply(FUNCTION_TYPE, apply(CARTESIAN_PRODUCT, getArgumentTypes()), getCodomain()).toString();
		}
		return cachedString;
	}
	
	// NOTE: Only to be used under testing conditions.	
	protected void updateTestArgumentTypes(List<Type> updatedArgumentTypes) {
		if (updatedArgumentTypes.size() != getArity()) {
			throw new IllegalArgumentException("Update arguments #= "+updatedArgumentTypes.size()+" does not match function types arity of "+getArity());
		}
		this.argumentTypes = Collections.unmodifiableList(new ArrayList<>(updatedArgumentTypes));
		this.cachedString = null; // re-calculate just in case.
	}
}