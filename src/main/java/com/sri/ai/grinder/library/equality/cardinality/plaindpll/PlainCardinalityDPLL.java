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
package com.sri.ai.grinder.library.equality.cardinality.plaindpll;

import java.util.Collection;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;

import com.google.common.annotations.Beta;
import com.google.common.base.Function;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.expresso.helper.SubExpressionsDepthFirstIterator;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.core.DefaultRewritingProcess;
import com.sri.ai.grinder.helper.GrinderUtil;
import com.sri.ai.grinder.library.Disequality;
import com.sri.ai.grinder.library.Equality;
import com.sri.ai.grinder.library.FunctorConstants;
import com.sri.ai.grinder.library.IsVariable;
import com.sri.ai.grinder.library.boole.And;
import com.sri.ai.grinder.library.equality.cardinality.direct.core.CardinalityTypeOfLogicalVariable;
import com.sri.ai.grinder.library.equality.cardinality.direct.core.CardinalityTypeOfLogicalVariable.DomainSizeOfLogicalVariable;
import com.sri.ai.util.Util;
import com.sri.ai.util.base.Equals;
import com.sri.ai.util.base.Not;

@Beta
/** 
 * A class for plain, 
 * non-rewriter computation of cardinality of intensional sets with equality boolean formula conditions.
 */
public class PlainCardinalityDPLL {

	public static Expression count(Expression formula, DefaultRewritingProcess process) {
		Collection<Expression> indices = GrinderUtil.getAllVariables(formula, process);
		Expression result = count(formula, Expressions.TRUE, indices, process);
		return result;
	}

	public static Expression count(Expression formula, Expression constraint, Collection<Expression> indices, RewritingProcess process) {
		
		Expression result = null;
		
		Expression splitter = pickAtom(formula, constraint, process);

		if (splitter == null) {
			if (formula.equals(Expressions.FALSE)) {
				result = Expressions.ZERO;
			}
			else if (formula.equals(Expressions.TRUE)) {
				result = countConstraint(constraint, indices, process);
			}
		}
		else {
			Expression variable  = splitter.get(0);
			Expression otherTerm = splitter.get(1);

			Expression formula1 = applyEquality   (formula, variable, otherTerm, process);
			Expression formula2 = applyDisequality(formula, variable, otherTerm, process);
			
			Expression constraint1 = applyEqualityToConstraint   (constraint, variable, otherTerm, process);
			Expression constraint2 = applyDisequalityToConstraint(constraint, variable, otherTerm, process);
			
			Collection<Expression> indicesMinusVariable = new LinkedHashSet<Expression>(indices);
			indicesMinusVariable.remove(variable);
			
			Expression count1 = count(formula1, constraint1, indicesMinusVariable, process);
			Expression count2 = count(formula2, constraint2, indices,              process);
			
			result = Expressions.makeSymbol(count1.rationalValue().add(count2.rationalValue()));
		}
		
		return result;
	}

	private static Expression pickAtom(Expression formula, Expression constraint, RewritingProcess process) {
		Expression result = null;
		result = pickAtomFromFormula(formula, process);
		if (result == null) {
			result = pickAtomFromConstraint(constraint, process);
		}
		return result;
	}

	private static Expression pickAtomFromFormula(Expression formula, RewritingProcess process) {
		
		Expression result = null;
		
		Iterator<Expression> subExpressionIterator = new SubExpressionsDepthFirstIterator(formula);
		while (subExpressionIterator.hasNext()) {
			
			Expression subExpression = subExpressionIterator.next();
			
			if (subExpression.hasFunctor(FunctorConstants.EQUALITY) || 
					subExpression.hasFunctor(FunctorConstants.DISEQUALITY)) {
				
				Expression variable = Util.getFirstSatisfyingPredicateOrNull(
						subExpression.getArguments(), new IsVariable(process));
				
				Expression otherTerm = Util.getFirstSatisfyingPredicateOrNull(
						subExpression.getArguments(), Not.make(Equals.make(variable)));
				
				result = Equality.make(variable, otherTerm);
			}
		}
		
		return result;
	}

	private static Expression pickAtomFromConstraint(Expression constraint, RewritingProcess process) {
		Expression result = null;
		
		Map<Expression, Collection<Expression>> fromVariablesToDistinctPredefinedTerms = getFromVariablesDistinctPredefinedTerms(constraint, process);
		for (Map.Entry<Expression, Collection<Expression>> entryForVariable1 : fromVariablesToDistinctPredefinedTerms.entrySet()) {
			Collection<Expression> distinctPredefinedTermsForVariable1 = entryForVariable1.getValue();
			for (Expression distinctPredefinedVariable2 : distinctPredefinedTermsForVariable1) {
				if (process.isVariable(distinctPredefinedVariable2)) {
					Collection<Expression> distinctPredefinedTermsForVariable2 = fromVariablesToDistinctPredefinedTerms.get(distinctPredefinedVariable2);
					Expression distinctPredefinedTermForVariable1ThatIsNotVariable2AndIsNotDistinctPredefinedForVariable2 =
							Util.pickElementInFirstCollectionButNotSecondAndNotEqualTo(
									distinctPredefinedTermsForVariable1, distinctPredefinedTermsForVariable2, distinctPredefinedVariable2);
					if (distinctPredefinedTermForVariable1ThatIsNotVariable2AndIsNotDistinctPredefinedForVariable2 != null) {
						Expression atom = Equality.make(distinctPredefinedVariable2, distinctPredefinedTermForVariable1ThatIsNotVariable2AndIsNotDistinctPredefinedForVariable2);
						return atom;
					}
				}
			}
		}
		
		return result;
	}

	/**
	 * Assumes a conjunction of disequalities and builds a map from each variable to
	 * the terms constraint to be distinct from it that are defined before it in the choosing order
	 * used for counting the number of solutions.
	 * The ordering chosen is the String order for the String representation of the terms.
	 * This is useful because we use the Counting Principle for each variable in the total ordering chosen.
	 * For each variable, we have N - D options, where N is its domain size and D is the number of values it must be distinct from.
	 * These values are the constants it is constrained to be distinct from, and variables that have their <i>already chosen</i>
	 * (coming in the ordering first), assuming that their values do not overlap.
	 * This counting will only be correct, however, if there is a guarantee that these predefined distinct terms are constrained to be distinct from each
	 * other (a condition similar but not identical to what is called "normalized constraints" in the lifted inference literature).
	 * The algorithm does check and enforces this guarantee.
	 */
	private static Map<Expression, Collection<Expression>> getFromVariablesDistinctPredefinedTerms(Expression constraint, RewritingProcess process) {

		Map<Expression, Collection<Expression>> result = new LinkedHashMap<Expression, Collection<Expression>>();

		if ( ! constraint.equals(Expressions.TRUE) && ! constraint.equals(Expressions.FALSE)) {

			List<Expression> conjuncts = And.getConjuncts(constraint);

			for (Expression disequalityConjunct : conjuncts) {
				Expression term1 = disequalityConjunct.get(0);
				Expression term2 = disequalityConjunct.get(1);
				if (process.isVariable(term1) && firstTermAlreadyDefinedWhenChoosingValueForSecondOne(term2, term1, process)) {
					Util.addToCollectionValuePossiblyCreatingIt(result, term1, term2, LinkedHashSet.class);
				}
				if (process.isVariable(term2) && firstTermAlreadyDefinedWhenChoosingValueForSecondOne(term1, term2, process)) {
					Util.addToCollectionValuePossiblyCreatingIt(result, term2, term1, LinkedHashSet.class);
				}
			}
		}
		
		return result;
	}

	private static Expression applyEquality(Expression formula, Expression variable, Expression otherTerm, RewritingProcess process) {
		Expression result = formula.replaceAllOccurrences(variable, otherTerm, process);
		result = SimplifyFormula.simplify(result, process);
		return result;
	}

	private static Expression applyDisequality(Expression formula, final Expression variable, final Expression otherTerm, RewritingProcess process) {
		Expression result = formula.replaceAllOccurrences(new SimplifyAtomGivenDisequality(variable, otherTerm), process);
		result = SimplifyFormula.simplify(result, process);
		return result;
	}

	private static Expression applyEqualityToConstraint(Expression constraint, Expression variable, Expression otherTerm, RewritingProcess process) {
		Expression result = constraint.replaceAllOccurrences(variable, otherTerm, process);
		result = SimplifyFormula.simplify(result, process);
		return result;
	}

	private static Expression applyDisequalityToConstraint(Expression constraint, Expression variable, Expression otherTerm, RewritingProcess process) {
		Expression result = And.addConjunct(constraint, Disequality.make(variable, otherTerm));
		result = SimplifyFormula.simplify(result, process);
		return result;
	}

	private static class SimplifyAtomGivenDisequality implements Function<Expression, Expression> {
		
		private Expression variable;
		private Expression otherTerm;
		
		public SimplifyAtomGivenDisequality(Expression variable, Expression otherTerm) {
			super();
			this.variable = variable;
			this.otherTerm = otherTerm;
		}

		@Override
		public Expression apply(Expression input) {
			Expression result;
			if (Equality.isEquality(input)) {
				result = Equality.simplifyGivenDisequality(input, variable, otherTerm);
			}
			else if (Disequality.isDisequality(input)) {
				result = Disequality.simplifyGivenDisequality(input, variable, otherTerm);
			}
			else {
				result = input;
			}
			return result;
		}
	}

	private static Expression countConstraint(Expression conjunctionOfDisequalities, Collection<Expression> indices, RewritingProcess process) {
		
		long resultValue = 1;
		
		Map<Expression, Collection<Expression>> fromVariablesToPredefinedTerms = getFromVariablesDistinctPredefinedTerms(conjunctionOfDisequalities, process);
		for (Expression index : indices) {
			long domainSize = getDomainSize(index, process);
			Collection<Expression> setOfDistinctTerms = fromVariablesToPredefinedTerms.get(index);
			long numberOfnonAvailableValues = setOfDistinctTerms == null? 0 : (long) setOfDistinctTerms.size();
			resultValue *= domainSize - numberOfnonAvailableValues;
		}
		
		return Expressions.makeSymbol(resultValue);
	}

	private static long getDomainSize(Expression variable, RewritingProcess process) {
		DomainSizeOfLogicalVariable domainSizes = (DomainSizeOfLogicalVariable) process
				.getGlobalObject(CardinalityTypeOfLogicalVariable.PROCESS_GLOBAL_OBJECT_KEY_DOMAIN_SIZE_OF_LOGICAL_VARIABLE);
		long domainSize = domainSizes.size(variable, process);
		return domainSize;
	}

	private static boolean firstTermAlreadyDefinedWhenChoosingValueForSecondOne(Expression term, Expression variable, RewritingProcess process) {
		boolean result = process.isConstant(term) || variablePrecedesAnother(variable, term);
		return result;
	}

	/**
	 * Indicates whether variable1 precedes variable2 in the total ordering
	 */
	private static boolean variablePrecedesAnother(Expression variable1, Expression variable2) {
		boolean result = variable2.toString().compareTo(variable1.toString()) < 0;
		return result;
	}
}
