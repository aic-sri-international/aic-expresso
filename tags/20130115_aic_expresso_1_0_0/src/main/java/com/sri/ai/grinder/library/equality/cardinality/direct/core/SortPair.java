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
package com.sri.ai.grinder.library.equality.cardinality.direct.core;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.util.base.Pair;

/**
 * Default implementation of sort_pair(F1, F2).
 * 
 * @author oreilly
 *
 */
@Beta
public class SortPair {
	private PickCheapest pickCheapest = new PickCheapest();
	
	public SortPair() {
	}
	
	public PickCheapest getPickCheapest() {
		return pickCheapest;
	}
	
	public void setPickCheapest(PickCheapest pickCheapest) {
		this.pickCheapest = pickCheapest;
	}
	
	/**
	 * <pre>
	 * sort_pair(F1, F2)
	 * F1 and F2 are formulas
	 * Returns a tuple with the cheapest formula in the first position
	 * 
	 * (F', k) <- pick_cheapest( (F1, 1), (F2, 2) )
	 * if k = 1
	 *     return (F1, F2)
	 * return (F2, F1)
	 * </pre>
	 * 
	 * @param f1
	 *            a formula.
	 * @param f2
	 *            a formula.
	 * @return a pair (F1, F2) if F1 is cheaper than F2, otherwise a pair (F2,
	 *         F1).
	 */
	public Pair<Expression, Expression> sort(Expression f1, Expression f2) {
		Pair<Expression, Expression> result = null;
		
		// (F', k) <- pick_cheapest( (F1, 1), (F2, 2) )
		int k = pickCheapest.pickIndex(f1, f2);
		
		// if k = 1
		if (k == 0) {
			// return (F1, F2)
			result = new Pair<Expression, Expression>(f1, f2);
		} 
		else {
			// return (F2, F1)
			result = new Pair<Expression, Expression>(f2, f1);
		}
		
		return result;
	}
}
