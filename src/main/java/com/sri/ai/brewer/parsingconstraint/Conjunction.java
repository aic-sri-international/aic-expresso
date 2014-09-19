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

import java.util.Collection;
import java.util.HashSet;


import com.google.common.annotations.Beta;
import com.google.common.collect.Lists;
import com.sri.ai.brewer.api.ParsingConstraint;
import com.sri.ai.brewer.api.ParsingExpression;
import com.sri.ai.util.Util;

/**
 * A conjunction of atomic precedence conditions, always kept in normalized form
 * (which means it must know the semantics of all types of conditions
 * potentially added to it, and updated accordingly with the creation of new
 * conditions).
 * 
 * @author braz
 */
@Beta
public class Conjunction extends HashSet<ParsingConstraint> implements ParsingConstraint {
	private static final long serialVersionUID = 1L;
	
	public Conjunction() {
	}

	public Conjunction(Conjunction conjunction) {
		super(conjunction);
	}

	@Override
	public boolean apply(ParsingExpression parsingExpression) {
		for (ParsingConstraint condition : this) {
			if (!condition.apply(parsingExpression)) {
				return false;
			}
		}
		return true;
	}
	
	@Override
	public boolean add(ParsingConstraint condition) {
		if (condition instanceof Conjunction) {
			boolean result = false;
			for (ParsingConstraint atomicCondition : ((Conjunction)condition)) {
				boolean addedThisCondition
				= addAtomicCondition((AbstractAtomicParsingConstraint) atomicCondition); 
				result = result || addedThisCondition;
			}
			return result;
		}

		return addAtomicCondition((AbstractAtomicParsingConstraint) condition);
	}
	
	public boolean addAtomicCondition(AbstractAtomicParsingConstraint newCondition) {
		// This function assumes that there is a single conjunct per disjunction in the normalized conjunction.
		// Therefore it works by picking the conjunct with the same disjunction as the new condition, if any,
		// merging them, and putting the result in the normalized conjunction.

		if (newCondition instanceof GreaterThanOrUnrelated) {
			ParsingExpression disjunction = newCondition.getDisjunction();
			ParsingExpression disjunct = (ParsingExpression) disjunction.get(1 + newCondition.getIndexOfDisjunctInDisjunction());
			newCondition = new GreaterThanOrEqualToOrUnrelated(newCondition.getSide(), disjunct, disjunction);
		}
		
		AbstractAtomicParsingConstraint sameDisjunctionAndSideIfAny =
			removeAndReturnConjunctWithSameDisjunctionAndSide(newCondition);

		AbstractAtomicParsingConstraint newConjunct;
		if (sameDisjunctionAndSideIfAny != null) {
			newConjunct = normalizePrecedenceConditionsOnSameDisjunctionAndSide(Lists.newArrayList(sameDisjunctionAndSideIfAny, newCondition));
		}
		else {
			newConjunct = newCondition;
		}
		return super.add(newConjunct);
	}
	
	private AbstractAtomicParsingConstraint removeAndReturnConjunctWithSameDisjunctionAndSide(
			AbstractAtomicParsingConstraint newCondition) {
		for (ParsingConstraint precedenceCondition : this) {
			AbstractAtomicParsingConstraint condition = (AbstractAtomicParsingConstraint) precedenceCondition;
			if (condition.getDisjunction() == newCondition.getDisjunction() &&
					condition.getSide() == newCondition.getSide()) {
				this.remove(condition);
				return condition;
			}
		}
		return null;
	}
	
	private static AbstractAtomicParsingConstraint normalizePrecedenceConditionsOnSameDisjunctionAndSide(Collection<AbstractAtomicParsingConstraint> conjunction) {
		AbstractAtomicParsingConstraint result = null;
		for (AbstractAtomicParsingConstraint condition : conjunction) {
			if (result == null || condition.getIndexOfDisjunctInDisjunction() > result.getIndexOfDisjunctInDisjunction()) {
				result = condition;
			}
		}
		return result;
	}
	
	@Override
	public ParsingConstraint and(ParsingConstraint another) {
		return and(this, another);
	}
	
	public static ParsingConstraint and(ParsingConstraint one, ParsingConstraint another) {
		Conjunction conjunction = new Conjunction();
		conjunction.add(one);
		conjunction.add(another);
		if (conjunction.size() > 1) {
			return conjunction;
		}
		return Util.getFirstOrNull(conjunction);
	}
}
