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

import java.util.Iterator;


import com.google.common.annotations.Beta;
import com.google.common.base.Predicate;
import com.sri.ai.brewer.api.ParsingConstraint;
import com.sri.ai.brewer.parsingconstraint.AbstractAtomicParsingConstraint.Side;
import com.sri.ai.util.Util;

/**
 * General utilities for parsing constraints.
 * 
 * @author braz
 */
@Beta
public class ParsingConstraints {

	public static Iterator<ParsingConstraint> conjunctsIterator(ParsingConstraint constraint) {
		if (constraint instanceof Conjunction) {
			return ((Conjunction)constraint).iterator();
		}
		return Util.iterator(constraint);
	}
	
	/**
	 * A predicate indicating whether a parsing constraint applies to a {@link Side}.
	 */
	public static class AppliesTo implements Predicate<AbstractAtomicParsingConstraint> {
		private Side side;

		public AppliesTo(Side side) {
			this.side = side;
		}
		
		@Override
		public boolean apply(AbstractAtomicParsingConstraint constraint) {
			return constraint instanceof AbstractAtomicParsingConstraint && constraint.getSide() == side;
		}
	}

	public static void collectSideConstraints(ParsingConstraint constraint,
			ParsingConstraint onLeft, ParsingConstraint onRight) {
		onLeft  = new Conjunction();
		onRight = new Conjunction();
		Iterator<ParsingConstraint> conjunctsIterator = conjunctsIterator(constraint);
		while (conjunctsIterator.hasNext()) {
			ParsingConstraint conjunct = conjunctsIterator.next();
			if (onLeft(conjunct)) {
				onLeft = onLeft.and(conjunct);
			}
			else if (onRight(conjunct)) {
				onRight = onRight.and(conjunct);
			}
		}
	}

	/** Indicates whether a constraint applies to the left {@link Side}. */
	public static boolean onLeft(ParsingConstraint constraint) {
		return constraint instanceof AbstractAtomicParsingConstraint &&
		((AbstractAtomicParsingConstraint) constraint).getSide() == Side.LEFT;
	}

	/** Indicates whether a constraint applies to the right {@link Side}. */
	public static boolean onRight(ParsingConstraint constraint) {
		return constraint instanceof AbstractAtomicParsingConstraint &&
		((AbstractAtomicParsingConstraint) constraint).getSide() == Side.RIGHT;
	}
}
