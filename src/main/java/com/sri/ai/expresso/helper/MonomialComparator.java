/*
 * Copyright (c) 2015, SRI International
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
 * Neither the name of the aic-praise nor the names of its
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
package com.sri.ai.expresso.helper;

import java.util.Comparator;
import java.util.List;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.Monomial;
import com.sri.ai.util.math.Rational;

/**
 * A monomial <em>M1</em> <b>comes before</b> another monomial <em>M2</em>  
 * iff signature(M1, U) is lexicographically larger than signature(M2, U), 
 * where U = sortedTuple(vars(M1) union vars(M2)). 
 * 
 * @author oreilly
 *
 */
@Beta
public class MonomialComparator implements Comparator<Monomial> {
	
	@Override
	public int compare(Monomial m1, Monomial m2) {
		int result = 0;
		
		List<Expression> sortedUnionOfVariables = Monomial.unionVariablesLexicographically(m1, m2);
		
		List<Rational> m1Signature = m1.getSignature(sortedUnionOfVariables);
		List<Rational> m2Signature = m2.getSignature(sortedUnionOfVariables);
		
		// Note: Both signatures will be the same length
		int signatureLength = m1Signature.size();
		for (int i = 0; i < signatureLength; i++) {
			if ((result = m1Signature.get(i).compareTo(m2Signature.get(i))) != 0) {
				// if not = 0 we know that one is less than or greater than the other
				break;
			}
		}
		
		return result;
	}
}
