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
package com.sri.ai.test.grinder.library.equality.cardinality;

import java.util.ArrayList;
import java.util.List;

import com.sri.ai.test.grinder.AbstractGrinderTest;
import com.sri.ai.test.grinder.library.equality.cardinality.AbstractCardinalityRewriterStressTests.CardinalityRewriter;
import com.sri.ai.test.grinder.library.equality.cardinality.AbstractCardinalityRewriterStressTests.CardinalityStressTest;
import com.sri.ai.util.base.Triple;

/**
 * A stress test formed of formulas generated according to parametric scheme.
 * @author oreilly
 */
public class ParametricStressTest implements CardinalityStressTest {
	// Note: if you want numberMembers > 6 extend these arrays.
	private String[] memberVarPrefixes  = new String[]{"X", "Y", "Z", "U", "V", "W"};
	private String[] memberConsPrefixes = new String[]{"a", "b", "c", "d", "f", "g"};
	//
	private String       title               = null;
	private List<String> cardExpressions     = new ArrayList<String>();
	private String[]     expectedExpressions = null;
	private int          maxFormulaLength    = 0;
	
	public ParametricStressTest(String formulaType, String equalityType, String termInBetweenOperator, String memberOperator, String memberInBetweenOperator,
			int cardTermsStart, int cardTermsIncrement, int cardTermsMax, int cardNumberMembers, int cardFreeVarsMax) {
		this.title = "Times Cardinality of "+formulaType+" on "+equalityType+" formulas with "+cardNumberMembers+" members";
		for (int f = 0; f <= cardFreeVarsMax; f++) {
			for (int t = cardTermsStart; t <= cardTermsMax; t += cardTermsIncrement) {
				String cardExpression = generateBasicCardinalityExpression(t, termInBetweenOperator, cardNumberMembers, memberOperator, memberInBetweenOperator, f);
				cardExpressions.add(cardExpression);
				//
				int length = cardExpression.length();
				if (length > maxFormulaLength) {
					maxFormulaLength = length;
				}
			}
			this.expectedExpressions = new String[cardExpressions.size()];
			for (int i = 0; i < this.expectedExpressions.length; i++) {
				this.expectedExpressions[i] = AbstractGrinderTest.IGNORE_EXPECTED;
			}
		}
	}
	
	public ParametricStressTest(String formulaType, String equalityType, String termInBetweenOperator, String memberOperator, String memberInBetweenOperator,
			int cardTermsStart, int cardTermsIncrement, int cardTermsMax, int cardNumberMembers, int cardFreeVarsMax,
			String[] expected) {
		this(formulaType, equalityType, termInBetweenOperator, memberOperator, memberInBetweenOperator,
				cardTermsStart, cardTermsIncrement, cardTermsMax, cardNumberMembers, cardFreeVarsMax);
		
		if (expected.length != this.expectedExpressions.length) {
			throw new IllegalArgumentException("expected is not the correct length, should be "+this.expectedExpressions.length);
		}
		for (int i = 0; i < this.expectedExpressions.length; i++) {
			this.expectedExpressions[i] = expected[i];
		}
	}
	
	@Override
	public String getTitle() {
		return title;
	}
	
	@Override
	public List<Triple<String, String, List<Long>>> stressTest(List<CardinalityRewriter> cardinalityRewriters) {
		List<Triple<String, String, List<Long>>> results = new ArrayList<Triple<String, String, List<Long>>>();
		for (int i = 0; i < cardExpressions.size(); i++) {
			String cardExpression = cardExpressions.get(i);
			String expected       = expectedExpressions[i];

			String resultTitle = String.format("Times for fixed %"+maxFormulaLength+"s = ", cardExpression);
			List<Long> times = new ArrayList<Long>();
			for (CardinalityRewriter cr : cardinalityRewriters) {
				System.out.println("Calling "+cr.getName()+" on "+cardExpression);
				times.add(cr.call(cardExpression, expected));
			}
			results.add(new Triple<String, String, List<Long>>(resultTitle, cardExpression, times));
		}
		
		return results;
	}
	
	//
	// PRIVATE METHODS
	//
	private String generateBasicFormula(int numberTerms, String termInbetweenOperator, int numberMembersPerTerm, String memberOperator, String memberInBetweenOperator) {
		StringBuilder sb = new StringBuilder();
		
		for (int t = 0; t < numberTerms; t++) {
			sb.append("(");
			for (int m = 0; m < numberMembersPerTerm; m++) {
				sb.append(memberVarPrefixes[m]);
				sb.append(t+1);
				sb.append(" ");
				sb.append(memberOperator);
				sb.append(" ");
				sb.append(memberConsPrefixes[m]);
				sb.append(t+1);
				if (m < (numberMembersPerTerm-1)) {
					sb.append(" ");
					sb.append(memberInBetweenOperator);
					sb.append(" ");
				}
			}
			sb.append(")");
			
			if (t < (numberTerms-1)) {
				sb.append(" ");
				sb.append(termInbetweenOperator);
				sb.append(" ");
			}
		}
		
		return sb.toString();
	}
	
	private String generateBasicCardinalityExpression(int numberTerms, String termInbetweenOperator, int numberMembersPerTerm, String memberOperator, String memberInBetweenOperator, int numberFreeVars) {
		StringBuilder sb = new StringBuilder();
		
		sb.append("| {{(on ");
		int maxNumVars = (numberTerms*numberMembersPerTerm) - numberFreeVars;
		int numVars    = 0;
		for (int t = 0; t < numberTerms; t++) {
			for (int m = 0; m < numberMembersPerTerm; m++) {
				if (numVars < maxNumVars) {
					if (!(t == 0 && m == 0)) {
						sb.append(", ");
					}
					sb.append(memberVarPrefixes[m]);
					sb.append(t+1);
				}
				numVars++;
			}
		}
		sb.append(") tuple(");
		numVars    = 0;
		for (int t = 0; t < numberTerms; t++) {
			for (int m = 0; m < numberMembersPerTerm; m++) {
				if (numVars < maxNumVars) {
					if (!(t == 0 && m == 0)) {
						sb.append(", ");
					}
					sb.append(memberVarPrefixes[m]);
					sb.append(t+1);
				}
				numVars++;
			}
		}
		
		sb.append(") | ");
		sb.append(generateBasicFormula(numberTerms, termInbetweenOperator, numberMembersPerTerm, memberOperator, memberInBetweenOperator));
		sb.append(" }} |");
		
		return sb.toString();
	}
}
