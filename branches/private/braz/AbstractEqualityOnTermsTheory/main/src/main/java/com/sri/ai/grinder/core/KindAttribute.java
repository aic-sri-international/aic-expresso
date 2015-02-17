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
package com.sri.ai.grinder.core;

import com.google.common.annotations.Beta;
import com.google.common.base.Predicate;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.api.RewriterTestAttribute;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.library.FunctorConstants;
import com.sri.ai.grinder.library.boole.ForAll;
import com.sri.ai.grinder.library.boole.ThereExists;
import com.sri.ai.grinder.library.equality.cardinality.direct.core.CardinalityOfType;
import com.sri.ai.grinder.library.set.Sets;

/**
 * A RewriterTestAttribute used to represent the kind of an expression.
 * The kind of an expression is one of the constants defined in this class,
 * or the functor of the expression otherwise.
 * 
 * @author braz
 * @author oreilly
 *
 */
@Beta
public class KindAttribute implements RewriterTestAttribute {

	public static final KindAttribute INSTANCE = new KindAttribute();
	
	//
	// ALLOWED KNOWN KIND VALUES IN ADVANCE (i.e. functors are determined at runtime).	
	public static final Predicate<Expression> VALUE_FOR_ALL = new Predicate<Expression>() {
		@Override
		public boolean apply(Expression e) {
			boolean result =
					e.getSyntacticFormType().equals(ForAll.SYNTACTIC_FORM_TYPE)
					||
					e.hasFunctor(FunctorConstants.FOR_ALL);
			return result;
		}
		
		@Override
		public String toString() {
			return ForAll.SYNTACTIC_FORM_TYPE;
		}
	};
	//
	public static final Predicate<Expression> VALUE_THERE_EXISTS = new Predicate<Expression>() {
		@Override
		public boolean apply(Expression e) {
			boolean result =
					e.getSyntacticFormType().equals(ThereExists.SYNTACTIC_FORM_TYPE)
					||
					e.hasFunctor(FunctorConstants.THERE_EXISTS);
			return result;
		}
		
		@Override
		public String toString() {
			return ThereExists.SYNTACTIC_FORM_TYPE;
		}
	};
	//
	public static final Predicate<Expression> VALUE_EXTENSIONAL_SET = new Predicate<Expression>() {
		@Override
		public boolean apply(Expression e) {
			boolean result = Sets.isExtensionalSet(e);
			return result;
		}
		
		@Override
		public String toString() {
			return "extensional set";
		}
	};
	//
	public static final Predicate<Expression> VALUE_INTENSIONAL_SET = new Predicate<Expression>() {
		@Override
		public boolean apply(Expression e) {
			boolean result = Sets.isIntensionalSet(e);
			return result;
		}
		
		@Override
		public String toString() {
			return "intensional set";
		}
	};
	//
	public static final Predicate<Expression> VALUE_TYPE_SYNTACTIC_FUNCTION = new Predicate<Expression>() {
		@Override
		public boolean apply(Expression e) {
			boolean result =
					CardinalityOfType.isTypeSyntacticFunctionApplication(e);
			return result;
		}

		@Override
		public String toString() {
			return "type syntactic function";
		}
	};
	
	public static boolean isKindPredicate(Object kindValue) {
		boolean result = 
			kindValue == VALUE_FOR_ALL         || 
			kindValue == VALUE_THERE_EXISTS    || 
			kindValue == VALUE_EXTENSIONAL_SET ||
			kindValue == VALUE_INTENSIONAL_SET ||
			kindValue == VALUE_TYPE_SYNTACTIC_FUNCTION;
		return result;
	}
	
	//
	// START-RewriterTestAttribute
	@Override
	public Object getValue(Expression expression, RewritingProcess process) {
		Object result = null;
		
		// NOTE: Order values appropriately.
		if (VALUE_EXTENSIONAL_SET.apply(expression)) {
			result = VALUE_EXTENSIONAL_SET;
		}
		else if (VALUE_INTENSIONAL_SET.apply(expression)) {
			result = VALUE_INTENSIONAL_SET;
		}
		else if (VALUE_FOR_ALL.apply(expression)) {
			result = VALUE_FOR_ALL;
		}
		else if (VALUE_THERE_EXISTS.apply(expression)) {
			result = VALUE_THERE_EXISTS;
		}
		else if (VALUE_TYPE_SYNTACTIC_FUNCTION.apply(expression)) {
			result = VALUE_TYPE_SYNTACTIC_FUNCTION;
		}
		else if ((result = expression.getFunctor()) != null) {
			// We have a functor, so we will return it as the result as it will be used for the comparison
			// by the caller of this method.
		}
		else {
			// i.e. indicate the expression does not have a value for this attribute.
			result = VALUE_NONE;
		}		
		
		return result;
	}
	// END-ReriterTestAttribute
	//
	
	@Override
	public String toString() {
		return "kind";
	}
	
	//
	// PRIVATE
	//
	/**
	 * Private constructor so that only a singleton may be created.
	 */
	private KindAttribute() {
		
	}
	
	// i.e. used when the given expression does not have the attribute
	private static final Object VALUE_NONE = new Object() {
		@Override
		public boolean equals(Object o) {
			return this == o; 
		}
		
		@Override
		public String toString() {
			return "none";
		}
	};
}
