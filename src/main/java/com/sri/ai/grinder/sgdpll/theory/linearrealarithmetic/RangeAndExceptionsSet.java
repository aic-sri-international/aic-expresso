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
package com.sri.ai.grinder.sgdpll.theory.linearrealarithmetic;

import static com.sri.ai.expresso.helper.Expressions.apply;
import static com.sri.ai.grinder.library.FunctorConstants.MINUS;
import static com.sri.ai.util.Util.arrayList;
import static com.sri.ai.util.Util.list;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.core.DefaultExtensionalUniSet;
import com.sri.ai.expresso.helper.AbstractExpressionWrapper;

/**
 * An interface representing a set of integers by keeping the symbolic extremes of an aboveAndUpTo
 * and a set of expressions in the aboveAndUpTo but <i>not</i> in the denoted set.
 * 
 * @author braz
 *
 */
@Beta
public interface RangeAndExceptionsSet extends Expression {

	/**
	 * Indicates whether the denoted set is empty.
	 * @return
	 */
	boolean isEmpty();
	
	/**
	 * Indicates whether the denoted set is a singleton.
	 * @return
	 */
	boolean isSingleton();
	
	/**
	 * If the set is a singleton, returns its single value (undefined otherwise).
	 * @return
	 */
	Expression getSingleValue();
	
	/**
	 * If the set is neither empty nor a singleton,
	 * returns its strict lower bound (undefined otherwise).
	 * @return
	 */
	Expression getStrictLowerBound();
	
	/**
	 * If the set is neither empty nor a singleton,
	 * returns its non-strict upper bound (undefined otherwise).
	 * @return
	 */
	Expression getNonStrictUpperBound();
	
	/**
	 * If the set is neither empty nor a singleton,
	 * returns the set of values within bounds that does <i>not</i>
	 * belong to the set (undefined otherwise).
	 * @return
	 */
	Collection<Expression> getExceptions();
	
	/**
	 * An instance representing an empty set.
	 */
	static class Empty extends AbstractExpressionWrapper implements RangeAndExceptionsSet {

		private static final long serialVersionUID = 1L;

		@Override
		public boolean isEmpty() {
			return true;
		}

		@Override
		public boolean isSingleton() {
			return false;
		}

		@Override
		public Expression getSingleValue() {
			return null;
		}

		@Override
		public Expression getStrictLowerBound() {
			return null;
		}

		@Override
		public Expression getNonStrictUpperBound() {
			return null;
		}

		@Override
		public Collection<Expression> getExceptions() {
			return null;
		}
		
		@Override
		public String toString() {
			return "{}";
		}

		@Override
		protected Expression computeInnerExpression() {
			return new DefaultExtensionalUniSet(arrayList());
		}
	};
	
	public static RangeAndExceptionsSet EMPTY = new Empty();
	
	public static class Singleton extends AbstractExpressionWrapper implements RangeAndExceptionsSet {

		private static final long serialVersionUID = 1L;
		
		private Expression value;
		
		public Singleton(Expression value) {
			this.value = value;
		}
		
		@Override
		protected Expression computeInnerExpression() {
			return new DefaultExtensionalUniSet(arrayList(value));
		}

		@Override
		public boolean isEmpty() {
			return false;
		}

		@Override
		public boolean isSingleton() {
			return true;
		}

		@Override
		public Expression getSingleValue() {
			return value;
		}

		@Override
		public Expression getStrictLowerBound() {
			return null;
		}

		@Override
		public Expression getNonStrictUpperBound() {
			return null;
		}

		@Override
		public Collection<Expression> getExceptions() {
			return null;
		}
	}
	
	/**
	 * A method making an instance representing a singleton set.
	 */
	public static RangeAndExceptionsSet makeSingleton(Expression singleValue) {
		RangeAndExceptionsSet result = new Singleton(singleValue);
		return result;
	}

	public static class DefaultRangeAndExceptionsSet extends AbstractExpressionWrapper implements RangeAndExceptionsSet {

		private static final long serialVersionUID = 1L;

		private Expression strictLowerBound;
		private Expression nonStrictUpperBound;
		private Collection<Expression> exceptions;
		
		public DefaultRangeAndExceptionsSet(
				Expression strictLowerBound,
				Expression nonStrictUpperBound) {
			
			this(strictLowerBound, nonStrictUpperBound, list());
		}

		public DefaultRangeAndExceptionsSet(
				Expression strictLowerBound,
				Expression nonStrictUpperBound,
				Collection<Expression> exceptions) {
			
			super();
			this.strictLowerBound = strictLowerBound;
			this.nonStrictUpperBound = nonStrictUpperBound;
			this.exceptions = exceptions;
		}

		@Override
		public boolean isEmpty() {
			return false;
		}

		@Override
		public boolean isSingleton() {
			return false;
		}

		@Override
		public Expression getSingleValue() {
			return null;
		}

		@Override
		public Expression getStrictLowerBound() {
			return strictLowerBound;
		}

		@Override
		public Expression getNonStrictUpperBound() {
			return nonStrictUpperBound;
		}

		@Override
		public Collection<Expression> getExceptions() {
			return Collections.unmodifiableCollection(exceptions);
		}

		@Override
		protected Expression computeInnerExpression() {
			Expression result;
			if (exceptions.isEmpty()) {
				result = apply("aboveAndUpTo", strictLowerBound, nonStrictUpperBound);
			} else {
				result = apply(
					MINUS,
					apply("aboveAndUpTo", strictLowerBound, nonStrictUpperBound),
					new DefaultExtensionalUniSet(new ArrayList<>(exceptions)));
			}
			return result;
		}
	}
	
	/**
	 * A method making an instance representing a set of integers
	 * <code>]strictLowerBound, nonStrictUpperBound] \ exceptions</code>.
	 */
	public static RangeAndExceptionsSet make(
			Expression strictLowerBound, Expression nonStrictUpperBound,
			Collection<Expression> exceptions) {

		RangeAndExceptionsSet result = 
				new DefaultRangeAndExceptionsSet(strictLowerBound, nonStrictUpperBound, exceptions);
		return result;
	}
}