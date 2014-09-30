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
package com.sri.ai.test.grinder.core;

import java.util.Arrays;
import java.util.List;

import org.junit.Test;

import com.sri.ai.grinder.api.Rewriter;
import com.sri.ai.grinder.core.CallRewriterDecisionTree;
import com.sri.ai.grinder.core.KindAttribute;
import com.sri.ai.grinder.library.AbsorbingElement;
import com.sri.ai.grinder.library.Associative;
import com.sri.ai.grinder.library.Disequality;
import com.sri.ai.grinder.library.Equality;
import com.sri.ai.grinder.library.FunctorConstants;
import com.sri.ai.grinder.library.PlainSubstitution;
import com.sri.ai.grinder.library.boole.And;
import com.sri.ai.grinder.library.boole.ContradictoryConjuncts;
import com.sri.ai.grinder.library.boole.Equivalence;
import com.sri.ai.grinder.library.boole.Implication;
import com.sri.ai.grinder.library.boole.Not;
import com.sri.ai.grinder.library.boole.Or;
import com.sri.ai.grinder.library.controlflow.IfThenElse;
import com.sri.ai.grinder.library.controlflow.IfThenElseBranchesAreBooleanConstants;
import com.sri.ai.grinder.library.controlflow.IfThenElseConditionIsTrueInThenBranchAndFalseInElseBranch;
import com.sri.ai.grinder.library.controlflow.IfThenElseExternalization;
import com.sri.ai.grinder.library.controlflow.IfThenElseIrrelevantCondition;
import com.sri.ai.grinder.library.controlflow.NormalizeDisequalities;
import com.sri.ai.grinder.library.controlflow.NormalizeEqualities;
import com.sri.ai.grinder.library.equality.NotOnDisequality;
import com.sri.ai.grinder.library.equality.NotOnEquality;
import com.sri.ai.grinder.library.equality.cardinality.direct.core.CardinalityTypeOfLogicalVariable;
import com.sri.ai.grinder.library.equality.cardinality.direct.core.ConjunctsHoldTrueForEachOther;
import com.sri.ai.grinder.library.equality.cardinality.direct.core.FromConditionalFormulaToFormula;
import com.sri.ai.grinder.library.equality.cardinality.direct.core.IncompleteTopImpliedCertainty;
import com.sri.ai.grinder.library.equality.cardinality.direct.core.QuantifierEliminationWrapper;
import com.sri.ai.grinder.library.equality.cardinality.direct.core.TopSimplifyWrapper;
import com.sri.ai.grinder.library.equality.cardinality.direct.core.TrivialForAllCases;
import com.sri.ai.grinder.library.equality.cardinality.direct.core.TrivialThereExistsCases;
import com.sri.ai.grinder.library.number.Division;
import com.sri.ai.grinder.library.number.Exponentiation;
import com.sri.ai.grinder.library.number.GreaterThan;
import com.sri.ai.grinder.library.number.Minus;
import com.sri.ai.grinder.library.number.NestedArithmeticOperation;
import com.sri.ai.grinder.library.number.Plus;
import com.sri.ai.grinder.library.number.Times;
import com.sri.ai.grinder.library.number.UnaryMinus;
import com.sri.ai.grinder.library.set.intensional.IntensionalSetWithBoundIndex;
import com.sri.ai.grinder.library.set.intensional.IntensionalUniSetWithIndicesNotUsedInHead;

public class CallRewriterDecisionTreeTest {

	@Test
	public void testToString() {
		// These are the list from R_normalize
		List<Rewriter> rewriters = Arrays.asList((Rewriter) 
					new PlainSubstitution(),
					new CardinalityTypeOfLogicalVariable(),
					new Plus(),
					new Division(),
					new Minus(),
					new UnaryMinus(),
					new NestedArithmeticOperation(),
					new Times(),
					new IfThenElse(),
					new Exponentiation(),
	
					new Equality(),
					new Disequality(),
					new NormalizeEqualities(),
					new NormalizeDisequalities(),
					new NotOnEquality(),
					new NotOnDisequality(), 
					new GreaterThan(), 
	
					new And(),
					new Or(),
					new Not(),  
					new ContradictoryConjuncts(),
					new Implication(), 
					new Equivalence(), 
					new AbsorbingElement(
							"and", "false"),
					new AbsorbingElement(
							"or", "true"),
					new AbsorbingElement(
							"*", "0"),
					new Associative("+"),
					new Associative("*"),
					new Associative("and"),
					
					new FromConditionalFormulaToFormula(),
					// new, cheap simplifiers to be used instead of full ImpliedCertainty
					new IncompleteTopImpliedCertainty(),
					new TrivialForAllCases(),
					new TrivialThereExistsCases(),
					new TopSimplifyWrapper(),
					new IntensionalSetWithBoundIndex(),
					new ConjunctsHoldTrueForEachOther(),
					
					//
					// Support for: Quantifier Elimination
					// e.g.:
					// there exists X : a = X             -> true
					// there exists X: (X = a) => (X = b) -> true
					new QuantifierEliminationWrapper(KindAttribute.VALUE_FOR_ALL),
					new QuantifierEliminationWrapper(KindAttribute.VALUE_THERE_EXISTS),
					new QuantifierEliminationWrapper(FunctorConstants.NOT),
					new QuantifierEliminationWrapper(FunctorConstants.IMPLICATION),
					new QuantifierEliminationWrapper(FunctorConstants.EQUIVALENCE),
					new QuantifierEliminationWrapper(FunctorConstants.AND),
					new QuantifierEliminationWrapper(FunctorConstants.OR),
					
					new IntensionalUniSetWithIndicesNotUsedInHead(),
					
					new IfThenElseIrrelevantCondition(),
					// new DisequalityToEqualityInIfThenElseCondition(),
					new IfThenElseBranchesAreBooleanConstants(),
					new IfThenElseConditionIsTrueInThenBranchAndFalseInElseBranch(),
					new IfThenElseExternalization()
				);
		
		CallRewriterDecisionTree decisionTree = new CallRewriterDecisionTree(rewriters);
		
		System.out.println("Total # Rewriters="+rewriters.size());
		System.out.println(decisionTree.toString());
	}
}
