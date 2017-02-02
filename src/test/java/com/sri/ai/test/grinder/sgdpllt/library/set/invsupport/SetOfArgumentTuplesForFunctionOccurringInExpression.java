/*
 * Copyright (c) 2017, SRI International
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
package com.sri.ai.test.grinder.sgdpllt.library.set.invsupport;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.type.FunctionType;
import com.sri.ai.grinder.sgdpllt.library.set.Sets;

/**
 * The <b>set of argument tuples for</b> &fnof; : &Alpha; &rarr; &Beta; <b>occurring in an 
 * expression</b> E is denoted oc<sub>&fnof;</sub>[E] and inductively defined as follows:<br>
 * <ul>
 * <li>
 * if E does not contain &fnof;, oc<sub>&fnof;</sub>[E] is &empty;;
 * </li>
 * <li>
 * if E is &fnof;(t) for t a tuple, oc<sub>&fnof;</sub>[E] is {t};
 * </li>
 * <li>
 * if E if &fnof;, oc<sub>&fnof;</sub>[E] is &Alpha;;
 * </li>
 * <li>
 * if E is g(E&prime;) for g a function symbol distinct from &fnof; and t a k-tuple of expressions,<br>
 *     <ul>
 *     <li>
 *     if g(E&prime;) is if C the E<sub>1</sub> else E<sub>2</sub> and C does not contain &fnof;, 
 *     then oc<sub>&fnof;</sub>[E] is<br>
 *     if C then oc<sub>&fnof;</sub>[E<sub>1</sub>] else oc<sub>&fnof;</sub>[E<sub>2</sub>];
 *     </li>
 *     <li>
 *     otherwise, oc<sub>&fnof;</sub>[E] is oc<sub>&fnof;</sub>[t<sub>1</sub>] &cup; &hellip;	&cup; oc<sub>&fnof;</sub>[t<sub>k</sub>];
 *     </li>
 *     </ul>
 * </li>
 * <li>
 * if E is Q<sub>x:C</sub>E&prime; for Q an arbitrary quantifier, x a variable, 
 * C a boolean formula not containing &fnof;, and E&prime; the
 * quantified expression, then oc<sub>&fnof;</sub>[E] is<br> 
 * <big>&cup;</big><sub>x:C</sub>oc<sub>&fnof;</sub>[E&prime;];
 * </li>
 * <li>
 * if E is Q<sub>x:C</sub>E&prime; for Q an arbitrary quantifier, x a variable, 
 * C a boolean formula containing &fnof;, and E&prime; the
 * quantified expression, then oc<sub>&fnof;</sub>[E] is<br> 
 * oc<sub>&fnof;</sub>[C] &cup; <big>&cup;</big><sub>x</sub>oc<sub>&fnof;</sub>[E&prime;];
 * </li> 
 * <ul> 
 * 
 * @author oreilly
 *
 */
public class SetOfArgumentTuplesForFunctionOccurringInExpression {

	public static Expression compute(FunctionType f, Expression e) {
		Expression result = Sets.EMPTY_SET;
// TODO		
		return result;
	}
}
