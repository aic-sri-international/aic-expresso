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
package com.sri.ai.test.grinder.library.equality.cardinality.direct;

import java.util.ArrayList;
import java.util.List;

import org.junit.Assume;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.core.DefaultRewritingProcess;
import com.sri.ai.grinder.helper.GrinderUtil;
import com.sri.ai.grinder.library.Basic;
import com.sri.ai.grinder.library.DirectCardinalityComputationFactory;
import com.sri.ai.grinder.library.equality.cardinality.direct.CardinalityRewriter;
import com.sri.ai.grinder.library.equality.cardinality.direct.core.CardinalityTypeOfLogicalVariable;
import com.sri.ai.test.grinder.AbstractGrinderTest;
import com.sri.ai.test.grinder.TestData;

public class DirectSimplifyStressIT extends AbstractGrinderTest {
	
	// Simplification settings
	private int simpTermsStart     = 2;
	private int simpTermsIncrement = 2;
	private int simpTermsMax       = 12;
	private int simpNumberMembers  = 2;

	// Note: if you want numberMembers > 6 extend these arrays.
	private String[] memberVarPrefixes  = new String[]{"X", "Y", "Z", "U", "V", "W"};
	private String[] memberConsPrefixes = new String[]{"a", "b", "c", "d", "f", "g"};
	
	@BeforeClass
	public static void setupForClass() {
		GrinderUtil.setMinimumOutputForProfiling();
	}
	
	@Before
	public void ignoreTest() {
		Assume.assumeFalse("Stress Tests Ignored.", Boolean.getBoolean("ignore.stress.tests"));
	}
	
	@Override
	public RewritingProcess makeRewritingProcess(Expression topExpression) {
		return new DefaultRewritingProcess(topExpression, new Basic());
	}

	@Test	
	public void stressTestSimplifyDNFForEqualities() {	
		List<Long> rewriteTimes = new ArrayList<Long>();
		for (int t = simpTermsStart; t <= simpTermsMax; t += simpTermsIncrement) {
			// i.e. of the form: (X1 = a1 and Y1 = b1) or (X2 = a2 and Y2 = b2)
			String dnfFormula = generateBasicFormula(t, "or", simpNumberMembers, "=", "and");
			rewriteTimes.add(perform(new TestData[] {
				new StressSimplifyData(false,
					dnfFormula,
					new CountsDeclaration(100),
					dnfFormula)	
			}));
		}
		
		System.out.println("Times Simplification of DNF on Equality formulas with "+simpNumberMembers+" members:");
		for (int i = 0, t = simpTermsStart; t <= simpTermsMax; i++, t += simpTermsIncrement) {
			System.out.println(""+t+" terms = "+rewriteTimes.get(i));
		}
	}
	
	@Test
	public void stressTestSimplifyDNFForInequalities() {	
		List<Long> rewriteTimes = new ArrayList<Long>();
		for (int t = simpTermsStart; t <= simpTermsMax; t += simpTermsIncrement) {
			// i.e. of the form: (X1 != a1 and Y1 != b1) or (X2 != a2 and Y2 != b2)
			String dnfFormula = generateBasicFormula(t, "or", simpNumberMembers, "!=", "and");
			rewriteTimes.add(perform(new TestData[] {
				new StressSimplifyData(false,
					dnfFormula,
					new CountsDeclaration(100),
					dnfFormula)	
			}));
		}
		
		System.out.println("Times Simplification of DNF on Inequality formulas with "+simpNumberMembers+" members:");
		for (int i = 0, t = simpTermsStart; t <= simpTermsMax; i++, t += simpTermsIncrement) {
			System.out.println(""+t+" terms = "+rewriteTimes.get(i));
		}
	}
	
	@Test
	public void stressTestSimplifyCNFForEqualities() {		
		//
		// CNF Formulas of the form: 
		// '(X1 or Y1) and (X2 or Y2) and ... and (Xn or Yn)'
		// which if translated to DNF will have 2^n terms
		List<Long> rewriteTimes = new ArrayList<Long>();
		for (int t = simpTermsStart; t <= simpTermsMax; t += simpTermsIncrement) {
			// i.e. of the form: (X1 = a1 or Y1 = b1) and (X2 = a2 or Y2 = b2)
			String dnfFormula = generateBasicFormula(t, "and", simpNumberMembers, "=", "or");
			rewriteTimes.add(perform(new TestData[] {
				new StressSimplifyData(false,
					dnfFormula,
					new CountsDeclaration(100),
					dnfFormula)	
			}));
		}
		
		System.out.println("Times Simplification of CNF on Equality formulas with "+simpNumberMembers+" members:");
		for (int i = 0, t = simpTermsStart; t <= simpTermsMax; i++, t += simpTermsIncrement) {
			System.out.println(""+t+" terms = "+rewriteTimes.get(i));
		}
	}

	@Test
	public void stressTestSimplifyCNFForInequalities() {		
		//
		// CNF Formulas of the form: 
		// '(X1 or Y1) and (X2 or Y2) and ... and (Xn or Yn)'
		// which if translated to DNF will have 2^n terms
		List<Long> rewriteTimes = new ArrayList<Long>();
		for (int t = simpTermsStart; t <= simpTermsMax; t += simpTermsIncrement) {
			// i.e. of the form: (X1 != a1 or Y1 != b1) and (X2 != a2 or Y2 != b2)
			String dnfFormula = generateBasicFormula(t, "and", simpNumberMembers, "!=", "or");
			rewriteTimes.add(perform(new TestData[] {
				new StressSimplifyData(false,
					dnfFormula,
					new CountsDeclaration(100),
					dnfFormula)	
			}));
		}
		
		System.out.println("Times Simplification of CNF on Inequality formulas with "+simpNumberMembers+" members:");
		for (int i = 0, t = simpTermsStart; t <= simpTermsMax; i++, t += simpTermsIncrement) {
			System.out.println(""+t+" terms = "+rewriteTimes.get(i));
		}
	}

	@Test
	public void stressTestSimplifyLBPTestEg1() {
		// Note: have not seen this return in LBPTest (no trace output while calculating this in formula simplification).
		// e.g.1  from LBPTest.testBelieftForLoopyModels (TrivialLoopyPQandb() - belief([p(X)])) which is failing to return from formula simplification:
		// | +R_prod_factor(( product({{ ( on F' in {{ ( on X''', Y'' ) ([ if p(X''') or p(Y'') then 1 else 0 ]) | (X''' = Y' and X'' != X''' or X''' = Y' and X''' != Y'') or Y' = Y'' and X'' != X''' }} ) (message to [ p(Y') ] from F') }}), ({{ ( ([ p(b) ]), ([ if p(b) or p(Y) then 1 else 0 ]) ), ( ([ if p(b) or p(Y) then 1 else 0 ]), ([ p(Y) ]) ), ( ([ p(Y) ]), ([ if p(X'') or p(Y') then 1 else 0 ]) ), ( ([ if p(X'') or p(Y') then 1 else 0 ]), ([ p(Y') ]) ) }}) )) - under context variables = [X'', Y', X', Y, X], constrained by (X = X' = Y' = b and X != Y) and X'' = Y and X'' != b and X'' != Y' or (X = X' = Y' = b and X != Y) and X'' = Y and X'' != Y' and X'' != b
		// |     prod_F in {{ F1 | C1 }}_I m_V<-F
		// |     C' <- R_formula_simplification(C1 and C)
		// Note: 2642ms
		perform(new TestData[] {
			new StressSimplifyData(false,
					// 7 variables, i.e.: X'', Y', X', Y, X, X''', Y'', 
					"((X''' = Y' and X'' != X''' or X''' = Y' and X''' != Y'') or Y' = Y'' and X'' != X''') and ((X = X' = Y' = b and X != Y) and X'' = Y and X'' != b and X'' != Y' or (X = X' = Y' = b and X != Y) and X'' = Y and X'' != Y' and X'' != b)",
					new CountsDeclaration(100),
					//"((X''' = Y' and X'' != X''' or X''' = Y' and X''' != Y'') or Y' = Y'' and X'' != X''') and (X = X' = Y' = b and X != Y and X'' = Y and X'' != b and X'' != Y' or X = X' = Y' = b and X != Y and X'' = Y and X'' != Y' and X'' != b)"
					   "(X''' = b or X''' = b and Y'' != b or Y'' = b and X'' != X''') and X = X' = Y' = b and Y != b and X'' = Y and X'' != b"
					)	
		});			
	}
	
	@Test
	public void stressTestSimplifyLBPTestEg2() {
		// Note: This returns but takes about 25+minutes (no trace output while calculating this in formula simplification).
		// e.g. 2 from LBPTest.testBelieftForLoopyModels (TrivialLoopyMisconceptionExample() - belief([misconception(X)])) which is failing to return from formula simplification
        // +R_m_to_f_from_v(( (message to [ if groupA(A'''') and groupB(A'''') then if misconception(A'''') then if misconception(A'''') then 10 else 1 else if misconception(A'''') then 5 else 30 else 1 ] from [ groupA(A'''') ]), ({{ ( ([ misconception(A) ]), ([ if groupA(A) and groupB(A) then if misconception(A) then if misconception(A) then 10 else 1 else if misconception(A) then 5 else 30 else 1 ]) ), ( ([ if groupA(A) and groupB(A) then if misconception(A) then if misconception(A) then 10 else 1 else if misconception(A) then 5 else 30 else 1 ]), ([ groupA(A) ]) ), ( ([ groupA(A) ]), ([ if groupA(A) and groupB(A') then if misconception(A) then if misconception(A') then 10 else 1 else if misconception(A') then 5 else 30 else 1 ]) ), ( ([ if groupA(A) and groupB(A') then if misconception(A) then if misconception(A') then 10 else 1 else if misconception(A') then 5 else 30 else 1 ]), ([ groupB(A') ]) ), ( ([ groupB(A') ]), ([ if groupA(A') and groupB(A') then if misconception(A') then if misconception(A') then 10 else 1 else if misconception(A') then 5 else 30 else 1 ]) ), ( ([ if groupA(A') and groupB(A') then if misconception(A') then if misconception(A') then 10 else 1 else if misconception(A') then 5 else 30 else 1 ]), ([ groupA(A') ]) ), ( ([ groupA(A') ]), ([ if groupA(A') and groupB(A'') then if misconception(A') then if misconception(A'') then 10 else 1 else if misconception(A'') then 5 else 30 else 1 ]) ), ( ([ if groupA(A') and groupB(A'') then if misconception(A') then if misconception(A'') then 10 else 1 else if misconception(A'') then 5 else 30 else 1 ]), ([ groupB(A'') ]) ), ( ([ groupB(A'') ]), ([ if groupA(A'') and groupB(A'') then if misconception(A'') then if misconception(A'') then 10 else 1 else if misconception(A'') then 5 else 30 else 1 ]) ), ( ([ if groupA(A'') and groupB(A'') then if misconception(A'') then if misconception(A'') then 10 else 1 else if misconception(A'') then 5 else 30 else 1 ]), ([ groupA(A'') ]) ), ( ([ groupA(A'') ]), ([ if groupA(A'') and groupB(A''') then if misconception(A'') then if misconception(A''') then 10 else 1 else if misconception(A''') then 5 else 30 else 1 ]) ), ( ([ if groupA(A'') and groupB(A''') then if misconception(A'') then if misconception(A''') then 10 else 1 else if misconception(A''') then 5 else 30 else 1 ]), ([ groupB(A''') ]) ), ( ([ groupB(A''') ]), ([ if groupA(A''') and groupB(A''') then if misconception(A''') then if misconception(A''') then 10 else 1 else if misconception(A''') then 5 else 30 else 1 ]) ), ( ([ if groupA(A''') and groupB(A''') then if misconception(A''') then if misconception(A''') then 10 else 1 else if misconception(A''') then 5 else 30 else 1 ]), ([ groupA(A''') ]) ), ( ([ groupA(A''') ]), ([ if groupA(A''') and groupB(A'''') then if misconception(A''') then if misconception(A'''') then 10 else 1 else if misconception(A'''') then 5 else 30 else 1 ]) ), ( ([ if groupA(A''') and groupB(A'''') then if misconception(A''') then if misconception(A'''') then 10 else 1 else if misconception(A'''') then 5 else 30 else 1 ]), ([ groupB(A'''') ]) ), ( ([ groupB(A'''') ]), ([ if groupA(A'''') and groupB(A'''') then if misconception(A'''') then if misconception(A'''') then 10 else 1 else if misconception(A'''') then 5 else 30 else 1 ]) ) }}) )) - under context variables = [A'', B''', A, A'''', B, A', B', A''', B'''', X, B''], constrained by (A = B = X and A != B' and A != A' and A != A'' and A != A''') and A' = B' and A' != B'' and A' != A'' and A' != A''' and A'' = B'' and A'' != B''' and A'' != A''' and not (A' = A''' = B''') and A''' = B''' and A''' != B'''' and A''' != A'''' and not (A' = A'''' = B'''') and not (A'' = A'''' = B'''') and A'''' = B''''
		// |   In <- R_in(m_V<-F in beingComputed)
        // ...
        // |     -R_in=A = A'''' or A' = A'''' or A'' = A''''[2722ms]
        // | return R_basic(if In then pm_F<-V else R_prod_factor(prod_F' in R_set_diff(R_neigh_v(Neigh(V))\{F}) m_V<-F', R_basic(beingComputed union {{(F,V)}})))
		// Note: 159ms
		perform(new TestData[] {
			new StressSimplifyData(false,
				// 11 variables, i.e.: A'', B''', A, A'''', B, A', B', A''', B'''', X, B''
				"(A = A'''' or A' = A'''' or A'' = A'''') and ((A = B = X and A != B' and A != A' and A != A'' and A != A''') and A' = B' and A' != B'' and A' != A'' and A' != A''' and A'' = B'' and A'' != B''' and A'' != A''' and not (A' = A''' = B''') and A''' = B''' and A''' != B'''' and A''' != A'''' and not (A' = A'''' = B'''') and not (A'' = A'''' = B'''') and A'''' = B'''')",
				new CountsDeclaration(100),
				//"(A = A'''' or A' = A'''' or A'' = A'''') and A = B = X and A != B' and A != A' and A != A'' and A != A''' and A' = B' and A' != B'' and A' != A'' and A' != A''' and A'' = B'' and A'' != B''' and A'' != A''' and (A' != A''' or A''' != B''') and A''' = B''' and A''' != B'''' and A''' != A'''' and (A' != A'''' or A'''' != B'''') and (A'' != A'''' or A'''' != B'''') and A'''' = B''''"
				  "(A = A'''' or A' = A'''' or A'' = A'''') and A = B = X and A != B' and A != A' and A != A'' and A != A''' and A' = B' and A' != B'' and A' != A'' and A' != A''' and A'' = B'' and A'' != B''' and A'' != A''' and not (A' = A''' = B''') and A''' = B''' and A''' != B'''' and A''' != A'''' and not (A' = A'''' = B'''') and not (A'' = A'''' = B'''') and A'''' = B''''")	
		});
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
	
	private class CountsDeclaration implements CardinalityTypeOfLogicalVariable.TypeSizeOfLogicalVariable {
		private Integer  allCounts          = null;
		public CountsDeclaration(int allCounts) {
			this.allCounts = allCounts;
		}
		
		//
		// START-TypeSizeOfLogicalVariable
		@Override
		public Integer size(Expression logicalVariable, RewritingProcess process) {
			return allCounts;
		}
		// END-TypeSizeOfLogicalVariable
		//
		
		public void setup(RewritingProcess process) {
			CardinalityTypeOfLogicalVariable.registerTypeSizeOfLogicalVariableWithProcess(this, process);
		}
	}
	
	private class StressSimplifyData extends TestData {
		private String E;
		private Expression exprE;
		private CountsDeclaration countsDeclaration = null;
		
		public StressSimplifyData(boolean isIllegalArgumentTest, String E, CountsDeclaration countsDeclaration, String expected) {
			super(isIllegalArgumentTest, expected);
			this.E = E;
			this.countsDeclaration = countsDeclaration;
		}
		
		@Override
		public Expression getTopExpression() {
			this.exprE = parse(E);
			
			return exprE;
		}
		
		@Override
		public Expression callRewrite(RewritingProcess process) {
			
			countsDeclaration.setup(process);
			
			Expression result = DirectCardinalityComputationFactory.newCardinalityProcess(exprE, process).rewrite(CardinalityRewriter.R_normalize, exprE);
			
			return result;
		}
	}
}
