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
package com.sri.ai.brewer.parsingconstraint;


import com.google.common.annotations.Beta;
import com.google.common.base.Function;
import com.sri.ai.brewer.api.ParsingConstraint;
import com.sri.ai.brewer.api.ParsingExpression;
import com.sri.ai.expresso.core.ExpressionOnCompoundSyntaxTree;
import com.sri.ai.util.Util;

/**
 * A basic implementation for precedence conditions.
 * 
 * @author braz
 */
@Beta
public abstract class AbstractAtomicParsingConstraint extends ExpressionOnCompoundSyntaxTree implements ParsingConstraint {
	private static final long serialVersionUID = 1L;

	// For now, side information is not being used; it is left for possible future extensions
	public static enum Side {LEFT, RIGHT};
	private Side side;
	protected ParsingExpression disjunct;
	protected ParsingExpression disjunction;
	protected int indexOfDisjunctInDisjunction;
	protected int hashCode = 0;
	
	public AbstractAtomicParsingConstraint(ParsingExpression disjunct, ParsingExpression disjunction) {
		super("atomic parsing constraint", disjunct, disjunction);
		this.disjunct = disjunct;
		this.disjunction = disjunction;
		this.indexOfDisjunctInDisjunction = getIndexOf(disjunct);
		if (this.disjunct != null) {
			hashCode += this.disjunct.hashCode();
		}
		if (this.disjunction != null) {
			hashCode += this.disjunction.hashCode();
		}
	}

	public AbstractAtomicParsingConstraint(Side side, ParsingExpression disjunct, ParsingExpression disjunction) {
		this(disjunct, disjunction);
		this.side = side;
		if (this.side != null) {
			hashCode += this.side.hashCode();
		}
	}
	
	@Override
	public ParsingConstraint and(ParsingConstraint another) {
		return Conjunction.and(this, another);
	}
	
	public Side getSide() {
		return side;
	}

	public ParsingExpression getDisjunct() {
		return disjunct;
	}

	public ParsingExpression getDisjunction() {
		return disjunction;
	}

	public int getIndexOfDisjunctInDisjunction() {
		return indexOfDisjunctInDisjunction;
	}

	protected int getIndexOf(ParsingExpression parsingExpression) {
		return disjunction.getArguments().indexOf(parsingExpression);
	}
	
	@Override
	public boolean equals(Object another) {
		if (another == this) {
			return true;
		}

		if ( ! (another instanceof AbstractAtomicParsingConstraint)) {
			return super.equals(another);
		}
		
		AbstractAtomicParsingConstraint anotherCondition = (AbstractAtomicParsingConstraint) another;
		
		return 
		disjunction == anotherCondition.disjunction &&
		disjunct == anotherCondition.disjunct &&
		Util.equals(side, anotherCondition.side);
	}
	
	@Override
	public int hashCode() {
		return hashCode;
	}
	
	public static class GetIndex implements Function<AbstractAtomicParsingConstraint, Integer> {
		@Override
		public Integer apply(AbstractAtomicParsingConstraint conjunctObj) {
			return conjunctObj.getIndexOfDisjunctInDisjunction();
		}
	};
	
	public final static GetIndex GET_INDEX = new GetIndex();
}
