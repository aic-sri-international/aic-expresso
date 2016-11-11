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
package com.sri.ai.expresso.api;

import java.util.Iterator;
import java.util.Random;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.helper.Expressions;

/**
 * Represents the type of expressions.
 * 
 * @author braz
 */
@Beta
public interface Type {

	/** The name of the type -- typically, the string used to describe it in a program. */
	String getName();
	// TODO: types should not have string names, but be described by an Expression
	
	/**
	 * Provides an iterator ranging over uniquely named constants for the elements of this type.
	 * @return
	 */
	Iterator<Expression> iterator();
	
	/**
	 * Indicates whether a uniquely named constant represents an element of this type.
	 * @param uniquelyNamedConstant
	 * @return
	 */
	boolean contains(Expression uniquelyNamedConstant);
	
	/**
	 * Samples one of the type's uniquely named constants.
	 * This is useful for generating synthetic problems.
	 * @return
	 */
	Expression sampleUniquelyNamedConstant(Random random);
	
	/**
	 * Returns an {@link Expression} with the number of elements in the type, if known,
	 * or {@link Expressions#INFINITY} or {@code | <type name> |} if unknown.
	 * @return
	 */
	Expression cardinality();
	
	/**
	 * 
	 * @return true if the type is discrete, (e.g like the integers).
	 */
	boolean isDiscrete();
	
	/**
	 * 
	 * @return true if the type is continuous, (e.g like the reals).
	 */
	default boolean isContinuous() {
		return !isDiscrete();
	}
	
	/**
	 * 
	 * @return true if the type is finite (i.e. discrete and a known cardinality). 
	 *         If the type is discrete but the cardinality is unknown this should return false.
	 */
	boolean isFinite();
	
	/**
	 * 
	 * @return true if the type is inifinte (e.g. the integer (discrete) or reals (continuous)).
	 */
	default boolean isInfinite() {
		return !isFinite();
	}
}
