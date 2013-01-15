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
package com.sri.ai.test.brewer;

import static org.junit.Assert.*;

import org.junit.Before;
import org.junit.Test;


import com.sri.ai.brewer.api.ParsingConstraint;
import com.sri.ai.brewer.api.ParsingExpression;
import com.sri.ai.brewer.parsingconstraint.Conjunction;
import com.sri.ai.brewer.parsingconstraint.GreaterThanOrEqualToOrUnrelated;
import com.sri.ai.brewer.parsingconstraint.GreaterThanOrUnrelated;
import com.sri.ai.brewer.parsingexpression.core.*;
import com.sri.ai.util.Util;

import static com.sri.ai.brewer.parsingconstraint.AbstractAtomicParsingConstraint.Side.*;

public class ConjunctionTest {

	@Before
	public void setUp() throws Exception {
	}

	@Test
	public void testAddPrecedenceCondition() {
		Conjunction conjunction;
		ParsingExpression disjunction1;
		ParsingExpression disjunction2;
		ParsingExpression disjunctA;
		ParsingExpression disjunctB;
		ParsingExpression disjunctC;
		ParsingExpression disjunctD;
		
		disjunction1 = new Disjunction(disjunctA = new Terminal("a"), disjunctB = new Terminal("b"));
		disjunction2 = new Disjunction(disjunctC = new Terminal("c"), disjunctD = new Terminal("d"));

		ParsingConstraint rightA1 = new GreaterThanOrEqualToOrUnrelated(RIGHT, disjunctA, disjunction1);
		ParsingConstraint rightB1 = new GreaterThanOrEqualToOrUnrelated(RIGHT, disjunctB, disjunction1);
		ParsingConstraint  leftA1 = new GreaterThanOrEqualToOrUnrelated( LEFT, disjunctA, disjunction1);
		ParsingConstraint  leftB1 = new GreaterThanOrEqualToOrUnrelated( LEFT, disjunctB, disjunction1);
		ParsingConstraint  leftB1Alt = new GreaterThanOrUnrelated( LEFT, disjunctA, disjunction1);
		// the above is equivalent to leftB1 and should be normalized to it.

		ParsingConstraint rightC2 = new GreaterThanOrEqualToOrUnrelated(RIGHT, disjunctC, disjunction2);
		ParsingConstraint rightD2 = new GreaterThanOrEqualToOrUnrelated(RIGHT, disjunctD, disjunction2);
		ParsingConstraint  leftC2 = new GreaterThanOrEqualToOrUnrelated( LEFT, disjunctC, disjunction2);
		ParsingConstraint  leftD2 = new GreaterThanOrEqualToOrUnrelated( LEFT, disjunctD, disjunction2);

		conjunction = new Conjunction();
		conjunction.add(rightB1);
		assertEquals(Util.set(rightB1), conjunction);
		conjunction.add(leftA1);
		assertEquals(Util.set(rightB1, leftA1), conjunction);
		conjunction.add(rightA1);
		assertEquals(Util.set(rightB1, leftA1), conjunction);
		conjunction.add(leftB1Alt); // should have the same effect as adding leftB1.
		assertEquals(Util.set(rightB1, leftB1), conjunction);

		conjunction.add(rightD2);
		assertEquals(Util.set(rightB1, leftB1, rightD2), conjunction);
		conjunction.add(leftC2);
		assertEquals(Util.set(rightB1, leftB1, rightD2, leftC2), conjunction);
		conjunction.add(rightC2);
		assertEquals(Util.set(rightB1, leftB1, rightD2, leftC2), conjunction);
		conjunction.add(leftD2);
		assertEquals(Util.set(rightB1, leftB1, rightD2, leftD2), conjunction);
	}

}
