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
package com.sri.ai.grinder.sgdpll2.theory.inequality;

import static com.sri.ai.expresso.helper.Expressions.apply;
import static com.sri.ai.expresso.helper.Expressions.makeSymbol;
import static com.sri.ai.grinder.library.FunctorConstants.DISEQUALITY;
import static com.sri.ai.grinder.library.FunctorConstants.EQUALITY;
import static com.sri.ai.grinder.library.FunctorConstants.GREATER_THAN;
import static com.sri.ai.grinder.library.FunctorConstants.GREATER_THAN_OR_EQUAL_TO;
import static com.sri.ai.grinder.library.FunctorConstants.LESS_THAN;
import static com.sri.ai.grinder.library.FunctorConstants.LESS_THAN_OR_EQUAL_TO;
import static com.sri.ai.grinder.library.FunctorConstants.MINUS;
import static com.sri.ai.util.Util.arrayList;
import static com.sri.ai.util.Util.list;
import static com.sri.ai.util.Util.map;
import static com.sri.ai.util.Util.set;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Map;
import java.util.Random;
import java.util.Set;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.type.Integer0To9;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.core.simplifier.RecursiveExhaustiveMergedMapBasedSimplifier;
import com.sri.ai.grinder.helper.GrinderUtil;
import com.sri.ai.grinder.library.FunctorConstants;
import com.sri.ai.grinder.library.boole.BooleanSimplifier;
import com.sri.ai.grinder.library.equality.EqualitySimplifier;
import com.sri.ai.grinder.library.inequality.InequalitySimplifier;
import com.sri.ai.grinder.library.number.Plus;
import com.sri.ai.grinder.sgdpll2.api.ConstraintTheory;
import com.sri.ai.grinder.sgdpll2.api.ContextDependentProblemStepSolver;
import com.sri.ai.grinder.sgdpll2.api.SingleVariableConstraint;
import com.sri.ai.grinder.sgdpll2.theory.base.AbstractConstrainTheoryWithBinaryRelations;
import com.sri.ai.grinder.sgdpll2.theory.compound.CompoundConstraintTheory;
import com.sri.ai.util.Util;


/** 
 * A {@link ConstraintTheory} for integer inequality literals.
 */
@Beta
public class InequalityConstraintTheory extends AbstractConstrainTheoryWithBinaryRelations {

	static final Map<String, String> negationFunctor =
	Util.map(
			EQUALITY,                 DISEQUALITY,
			DISEQUALITY,              EQUALITY,
			LESS_THAN,                GREATER_THAN_OR_EQUAL_TO,
			LESS_THAN_OR_EQUAL_TO,    GREATER_THAN,
			GREATER_THAN,             LESS_THAN_OR_EQUAL_TO,
			GREATER_THAN_OR_EQUAL_TO, LESS_THAN
			);

	/**
	 * 	 * Creates an inequality theory for integers that does <i>not</i> assume equality literals are literal of this theory
	 * (this is more expensive -- use for a more efficiency setting if all equalities belong to this theory).
	 */
	public InequalityConstraintTheory() {
		this(false);
	}
	
	/**
	 * Creates an inequality theory for integers.
	 * It takes an argument indicating whether all equalities and disequalities are literals in this theory;
	 * this may not be the case if a {@link CompoundConstraintTheory} mixing multiple theories involving
	 * equalities is being used.
	 * @param assumeAllTheoryFunctorApplicationsAreAtomsInThisTheory
	 * whether all equalities and disequalities can be safely assumed to belong to this theory
	 * (if you know all such expressions are literals in this theory, invoke this constructor with a <code>true</code> argument).
	 */
	public InequalityConstraintTheory(boolean assumeAllTheoryFunctorApplicationsAreAtomsInThisTheory) {
		super(
				negationFunctor.keySet(),
				assumeAllTheoryFunctorApplicationsAreAtomsInThisTheory,
						new RecursiveExhaustiveMergedMapBasedSimplifier(
								new EqualitySimplifier(), new InequalitySimplifier(), new BooleanSimplifier()));

		setTypesForTesting(list(new Integer0To9()));
		setVariableNamesAndTypeNamesForTesting(map("I", "Integer0to9", "J", "Integer0to9", "K", "Integer0to9"));
	}
	
	@Override
	protected boolean isValidArgument(Expression expression, RewritingProcess process) {
		Expression type = GrinderUtil.getType(expression, process);
		boolean result = type.equals("Integer");
		return result;
	}

	@Override
	protected Expression getNonTrivialAtomNegation(Expression atom) {
		String functorString = atom.getFunctor().toString();
		String negatedFunctor = negationFunctor.get(functorString);
		Expression result = apply(negatedFunctor, atom.get(0), atom.get(1));
		return result;
	}

	@Override
	public SingleVariableConstraint makeSingleVariableConstraint(Expression variable, ConstraintTheory constraintTheory, RewritingProcess process) {
		return new SingleVariableInequalityConstraint(variable, constraintTheory);
	}

	@Override
	public boolean singleVariableConstraintIsCompleteWithRespectToItsVariable() {
		return false; // SingleVariableInequalityConstraint is complete
	}

	@Override
	public ContextDependentProblemStepSolver getSingleVariableConstraintSatisfiabilityStepSolver(SingleVariableConstraint constraint, RewritingProcess process) {
		return null;//new SatisfiabilityOfSingleVariableInequalityConstraintStepSolver((SingleVariableInequalityConstraint) constraint);
	}

	@Override
	public ContextDependentProblemStepSolver getSingleVariableConstraintModelCountingStepSolver(SingleVariableConstraint constraint, RewritingProcess process) {
		return null;//new ModelCountingOfSingleVariableInequalityConstraintStepSolver((SingleVariableInequalityConstraint) constraint);
	}

//	/**
//	 * Makes a random atom on variable by summing or subtracting terms from two random atoms generated by super class implementation.
//	 */
//	@Override
//	public Expression makeRandomAtomOn(String variable, Random random, RewritingProcess process) {
//		
//		Expression atom1 = super.makeRandomAtomOn(variable, random, process);
//		Expression atom2 = super.makeRandomAtomOn(variable, random, process);
//		Expression atom3 = super.makeRandomAtomOn(variable, random, process);
//		
//		atom1 = rejectBooleanConstants(atom1, variable, random, process);
//		atom2 = rejectBooleanConstants(atom2, variable, random, process);
//		atom3 = rejectBooleanConstants(atom3, variable, random, process);
//		
//		Expression term1 = atom1.get(1);
//		Expression term2 = atom2.get(1);
//		Expression term3 = atom3.get(1);
//		
//		term1 = random.nextBoolean()? term1 : apply(MINUS, term1);
//		term2 = random.nextBoolean()? term2 : apply(MINUS, term2);
//		term3 = random.nextBoolean()? term3 : apply(MINUS, term3);
//		
//		Set<Expression> leftHandSideTerms  = set(makeSymbol(variable), term1);
//		Collection<Expression> rightHandSideTerms = Util.setDifference(set(term2, term3), leftHandSideTerms);
//
//		ArrayList<Expression> leftHandSideArguments = new ArrayList<>();
//		leftHandSideArguments.addAll(leftHandSideTerms);
//		ArrayList<Expression> rightHandSideArguments = new ArrayList<>();
//		leftHandSideArguments.addAll(rightHandSideTerms);
//		
//		Expression leftHandSide = Plus.make(leftHandSideArguments);
//		Expression rightHandSide = Plus.make(rightHandSideArguments);
//		
//		Expression result = apply(atom1.getFunctor(), leftHandSide, rightHandSide);
//		return result;
//	}
//
//	private Expression rejectBooleanConstants(Expression atom, String variable, Random random, RewritingProcess process) {
//		while (atom.getSyntacticFormType().equals("Symbol")) {
//			atom = super.makeRandomAtomOn(variable, random, process);
//		}
//		return atom;
//	}
}