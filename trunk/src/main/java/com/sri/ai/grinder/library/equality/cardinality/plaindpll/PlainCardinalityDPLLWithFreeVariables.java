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
import java.util.Collections;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.expresso.helper.SubExpressionsDepthFirstIterator;
import com.sri.ai.grinder.api.Rewriter;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.core.AbstractHierarchicalRewriter;
import com.sri.ai.grinder.core.DefaultRewritingProcess;
import com.sri.ai.grinder.library.Disequality;
import com.sri.ai.grinder.library.Equality;
import com.sri.ai.grinder.library.FunctorConstants;
import com.sri.ai.grinder.library.IsVariable;
import com.sri.ai.grinder.library.boole.And;
import com.sri.ai.grinder.library.controlflow.IfThenElse;
import com.sri.ai.grinder.library.equality.cardinality.core.CountsDeclaration;
import com.sri.ai.grinder.library.equality.cardinality.direct.core.CardinalityTypeOfLogicalVariable;
import com.sri.ai.grinder.library.equality.cardinality.direct.core.CardinalityTypeOfLogicalVariable.DomainSizeOfLogicalVariable;
import com.sri.ai.grinder.library.equality.cardinality.direct.core.Simplify;
import com.sri.ai.grinder.library.set.intensional.IntensionalSet;
import com.sri.ai.util.Util;
import com.sri.ai.util.base.Equals;
import com.sri.ai.util.base.Not;

@Beta
/** 
 * A class for plain, 
 * non-rewriter computation of cardinality of intensional sets with equality boolean formula conditions.
 */
public class PlainCardinalityDPLLWithFreeVariables extends AbstractHierarchicalRewriter {
	
	/*
	 * Implementation notes.
	 * 
	 * Pseudo-code:
	 * 
	 * count(F, C, I): // assume F simplified in itself and with respect to C
	 * 
	 * if F is false, return 0
	 *     
	 * pick atom X = T, T a variable or constant, from formula or constraint  // linear in F size
	 * if no atom
	 *     return | C |_I // linear in number of symbols
	 * else
	 *     C1 = C[X/T] // linear in F size
	 *     C2 = C and X != T // amortized constant
	 *     F'  = simplify(F[X/T])
	 *     F'' = simplify(F with "X != T" replaced by true and "X = T" replaced by false)
	 *     return count( F' , C1, I - {X} ) + count( F'', C2 )
	 * 
	 * simplify must exploit short circuits and eliminate atoms already determined in the contextual constraint.
	 * 
	 * To compute | C |_I
	 * 
	 * C is a conjunction of disequalities, but represented in a map data structure for greater efficiency.
	 * Assume a total ordering of variables (we use alphabetical order)
	 * C is represented as a map that is null if C is false,
	 * or that maps each variable V to the set diseq(V) of terms it is constrained to be distinct,
	 * excluding variables V' > V.
	 * The idea is that values for variables are picked in a certain order and
	 * we exclude values already picked for variables constrained to be distinct and constants.
	 * Now,
	 * solution = 1
	 * For each variable V according to the total ordering
	 *     solution *= ( |domain(V)| - |diseq(V)| )
	 * return solution
	 * 
	 */

	protected CountsDeclaration countsDeclaration;

	/**
	 * Builds a rewriter for cardinality computation.
	 */
	public PlainCardinalityDPLLWithFreeVariables(CountsDeclaration countsDeclaration) {
		this.countsDeclaration = countsDeclaration;
	}

	@Override
	public RewritingProcess makeRewritingProcess(Expression expression) {
		Rewriter rewriterWithModules = new Simplify();
		RewritingProcess result = new DefaultRewritingProcess(expression, rewriterWithModules);
		result.notifyReadinessOfRewritingProcess();
		if (countsDeclaration != null) {
			countsDeclaration.setup(result);
		}
		return result;
	}

	@Override
	public Expression rewriteAfterBookkeeping(Expression expression, RewritingProcess process) {
		Expression result = count(expression, process);
		return result;
	}

	/**
	 * Rewrites a cardinality problem to its cardinality value.
	 * Assumes it is already simplified with respect to context.
	 */
	public Expression count(Expression cardinalityProblem, RewritingProcess process) {
		Expression set = cardinalityProblem.get(0);
		List<Expression> indices = IntensionalSet.getIndices(set);
		Expression formula = SimplifyFormula.simplify(IntensionalSet.getCondition(set), process);
		Expression result = count(formula, Expressions.TRUE, indices, process);
		return result;
	}

	/**
	 * Rewrites a cardinality problem to its cardinality value.
	 * Assumes it is already simplified with respect to context.
	 */
	public Expression count(Expression formula, Expression constraint, Collection<Expression> indices, RewritingProcess process) {
		Map<Expression, Collection<Expression>> constraintMap = makeConstraintMap(constraint, indices, process);
		Expression result = count(formula, constraintMap, indices, process);
		return result;
	}

	/**
	 * Rewrites a cardinality problem to its cardinality value.
	 * Assumes it is already simplified with respect to context.
	 */
	public Expression count(Expression formula, Map<Expression, Collection<Expression>> constraintMap, Collection<Expression> indices, RewritingProcess process) {
		
		Expression result = null;
		
		if (formula.equals(Expressions.FALSE) || constraintMap == null) {
			result = Expressions.ZERO;
		}
		else {
			Expression splitter = pickAtomFromFormula(formula, indices, process);
			if (splitter == null) { // formula is 'true'
				// check if we need to split in order for constraint to get ready to be counted
				splitter = pickAtomFromConstraintMap(constraintMap, process);
				// the splitting stops only when the formula has no atoms, *and* when the constraint satisfies some necessary conditions
				if (splitter == null) {
					// formula is 'true' and constraint is ready to be counted
					result = countConstraintMap(indices, constraintMap, process);
				}
			}

			if (splitter != null) {
				Expression variable  = splitter.get(0);
				Expression otherTerm = splitter.get(1);

				Expression formula1 = applyEquality   (formula, variable, otherTerm, process);
				Expression formula2 = applyDisequality(formula, variable, otherTerm, process);

				Map<Expression, Collection<Expression>> constraintMap1 = applyEqualityToConstraint   (constraintMap, variable, otherTerm, indices, process);
				Map<Expression, Collection<Expression>> constraintMap2 = applyDisequalityToConstraint(constraintMap, variable, otherTerm, indices, process);

				Collection<Expression> indicesUnderEquality = new LinkedHashSet<Expression>(indices);
				boolean conditionOnFreeVariable = false;
				if (indices.contains(variable)) {
					indicesUnderEquality.remove(variable);
				}
				else {
					conditionOnFreeVariable = true;
				}
				
				Expression count1;
				Expression count2;

				count1 = count(formula1, constraintMap1, indicesUnderEquality, process);
				count2 = count(formula2, constraintMap2, indices,              process);

				if (conditionOnFreeVariable) {
					result = IfThenElse.make(Equality.make(variable, otherTerm), count1, count2);
				}
				else {
					result = sumSymbolicCounts(count1, count2);
				}
			}
		}
		
		return result;
	}

	/**
	 * If counts are numerical expressions, simply sum them.
	 * If they are conditional, perform distributive on conditions.
	 */
	private Expression sumSymbolicCounts(Expression count1, Expression count2) {

		Expression result = null;
		if (IfThenElse.isIfThenElse(count1)) {
			Expression condition  = IfThenElse.getCondition(count1);
			Expression thenBranch = IfThenElse.getThenBranch(count1);
			Expression elseBranch = IfThenElse.getElseBranch(count1);
			Expression newThenBranch = sumSymbolicCounts(thenBranch, count2);
			Expression newElseBranch = sumSymbolicCounts(elseBranch, count2);
			result = IfThenElse.make(condition, newThenBranch, newElseBranch);
		}
		if (IfThenElse.isIfThenElse(count2)) {
			Expression condition  = IfThenElse.getCondition(count2);
			Expression thenBranch = IfThenElse.getThenBranch(count2);
			Expression elseBranch = IfThenElse.getElseBranch(count2);
			Expression newThenBranch = sumSymbolicCounts(count1, thenBranch);
			Expression newElseBranch = sumSymbolicCounts(count1, elseBranch);
			result = IfThenElse.make(condition, newThenBranch, newElseBranch);
		}
		else {
			result = Expressions.makeSymbol(count1.rationalValue().add(count2.rationalValue()));
		}

		return result;
	}

	private static Expression pickAtomFromFormula(Expression formula, Collection<Expression> indices, RewritingProcess process) {
		
		Expression result = null;
		
		Iterator<Expression> subExpressionIterator = new SubExpressionsDepthFirstIterator(formula);
		while (result == null && subExpressionIterator.hasNext()) {
			
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

	/**
	 * Not used anymore but left to show how the treatment of constraints were before they were maps but only a disjunction of equalities.
	 */
	@SuppressWarnings("unused")
	private static Expression applyEqualityToConstraint(Expression constraint, Expression variable, Expression otherTerm, RewritingProcess process) {
		Expression result = constraint.replaceAllOccurrences(variable, otherTerm, process);
		result = SimplifyFormula.simplify(result, process);
		return result;
	}

	/**
	 * Not used anymore but left to show how the treatment of constraints were before they were maps but only a disjunction of equalities.
	 */
	@SuppressWarnings("unused")
	private static Expression applyDisequalityToConstraint(Expression constraint, Expression variable, Expression otherTerm, RewritingProcess process) {
		Expression result = And.addConjunct(constraint, Disequality.make(variable, otherTerm));
		// no need to simplify; adding a disequality does not affect the other ones.
		return result;
	}

	private static Expression countConstraintMap(
			Collection<Expression> indices, Map<Expression, Collection<Expression>> constraintMap, RewritingProcess process) {
		
		long resultValue = 1;
		
		for (Expression index : indices) {
			long domainSize = getDomainSize(index, process);
			Collection<Expression> setOfDistinctTerms = constraintMap.get(index);
			long numberOfNonAvailableValues = setOfDistinctTerms == null? 0 : (long) setOfDistinctTerms.size();
			resultValue *= domainSize - numberOfNonAvailableValues;
		}
		
		return Expressions.makeSymbol(resultValue);
	}

	private static long getDomainSize(Expression variable, RewritingProcess process) {
		DomainSizeOfLogicalVariable domainSizes = (DomainSizeOfLogicalVariable) process
				.getGlobalObject(CardinalityTypeOfLogicalVariable.PROCESS_GLOBAL_OBJECT_KEY_DOMAIN_SIZE_OF_LOGICAL_VARIABLE);
		long domainSize = domainSizes.size(variable, process);
		return domainSize;
	}
	
	///// CONSTRAINT MAP METHODS

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
	private static Map<Expression, Collection<Expression>> makeConstraintMap(Expression constraint, Collection<Expression> indices, RewritingProcess process) {
	
		Map<Expression, Collection<Expression>> result = new LinkedHashMap<Expression, Collection<Expression>>();
	
		if ( ! constraint.equals(Expressions.TRUE) && ! constraint.equals(Expressions.FALSE)) {
	
			List<Expression> conjuncts = And.getConjuncts(constraint);
	
			for (Expression disequalityConjunct : conjuncts) {
				Expression term1 = disequalityConjunct.get(0);
				Expression term2 = disequalityConjunct.get(1);
				if (process.isVariable(term1) && firstTermAlreadyDefinedWhenChoosingValueForSecondOne(term2, term1, indices, process)) {
					Util.addToCollectionValuePossiblyCreatingIt(result, term1, term2, LinkedHashSet.class);
				}
				if (process.isVariable(term2) && firstTermAlreadyDefinedWhenChoosingValueForSecondOne(term1, term2, indices, process)) {
					Util.addToCollectionValuePossiblyCreatingIt(result, term2, term1, LinkedHashSet.class);
				}
			}
		}
		
		return result;
	}

	private static Expression pickAtomFromConstraintMap(Map<Expression, Collection<Expression>> constraintMap, RewritingProcess process) {
		Expression result = null;
		
		for (Map.Entry<Expression, Collection<Expression>> entryForVariable1 : constraintMap.entrySet()) {
			
			Collection<Expression> distinctPredefinedTermsForVariable1 = entryForVariable1.getValue();
			
			for (Expression distinctPredefinedVariable2 : distinctPredefinedTermsForVariable1) {
				if (process.isVariable(distinctPredefinedVariable2)) {
			
					Collection<Expression> distinctPredefinedTermsForVariable2 =
							getDistinctPredefinedTermsFrom(distinctPredefinedVariable2, constraintMap);
					Expression distinctPredefinedTermForVariable1ThatIsNotVariable2AndIsNotDistinctPredefinedForVariable2 =
							getDistinctPredefinedTermForVariable1ThatIsNotVariable2AndIsNotDistinctFromVariable2(
									distinctPredefinedTermsForVariable1, distinctPredefinedTermsForVariable2, distinctPredefinedVariable2, constraintMap);
					if (distinctPredefinedTermForVariable1ThatIsNotVariable2AndIsNotDistinctPredefinedForVariable2 != null) {
						Expression atom = Equality.make(
								distinctPredefinedVariable2, distinctPredefinedTermForVariable1ThatIsNotVariable2AndIsNotDistinctPredefinedForVariable2);
						return atom;
					}
					
				}
			}
		}
		
		return result;
	}

	private static Expression getDistinctPredefinedTermForVariable1ThatIsNotVariable2AndIsNotDistinctFromVariable2(
			Collection<Expression> distinctPredefinedTermsForVariable1,
			Collection<Expression> distinctPredefinedTermsForVariable2,
			Expression variable2,
			Map<Expression, Collection<Expression>> fromVariablesToDistinctPredefinedTerms) {
	
		for (Expression distinctPredefinedTermForVariable1 : distinctPredefinedTermsForVariable1) {
			if ( ! distinctPredefinedTermForVariable1.equals(variable2)) {
				if (distinctPredefinedTermForVariable1IsNotDistinctFromVariable2(distinctPredefinedTermForVariable1, variable2, distinctPredefinedTermsForVariable2, fromVariablesToDistinctPredefinedTerms)) {
					return distinctPredefinedTermForVariable1;
				}
			}
		}
		return null;
	}

	private static boolean distinctPredefinedTermForVariable1IsNotDistinctFromVariable2(Expression distinctPredefinedTermForVariable1, Expression variable2, Collection<Expression> distinctPredefinedTermsForVariable2, Map<Expression, Collection<Expression>> fromVariablesToDistinctPredefinedTerms) {
		if ( ! distinctPredefinedTermsForVariable2.contains(distinctPredefinedTermForVariable1)) {
			Collection<Expression> distinctPredefinedTermsForDistinctPredefinedTermForVariable1 =
					getDistinctPredefinedTermsFrom(distinctPredefinedTermForVariable1, fromVariablesToDistinctPredefinedTerms);
			if ( ! distinctPredefinedTermsForDistinctPredefinedTermForVariable1.contains(variable2)) {
				return true;
			}
		}
		return false;
	}

	private static Map<Expression, Collection<Expression>> applyEqualityToConstraint(Map<Expression, Collection<Expression>> constraintMap, Expression variable, Expression otherTerm, Collection<Expression> indices, RewritingProcess process) {
		
		if (equalityIsInconsistentWithConstraintMap(variable, otherTerm, constraintMap)) {
			return null;
		}
		
		Map<Expression, Collection<Expression>> newConstraintMap = new LinkedHashMap<Expression, Collection<Expression>>();
		addAllDisequalitiesFromVariableToOtherTermInNewConstraintMap(variable, otherTerm, newConstraintMap, constraintMap, indices, process);
		for (Map.Entry<Expression, Collection<Expression>> entry : constraintMap.entrySet()) {
			if ( ! entry.getKey().equals(variable) && ! entry.getKey().equals(otherTerm)) {
				if (entry.getValue().contains(variable)) {
					Set<Expression> newDistinctFromKey = new LinkedHashSet<Expression>(entry.getValue());
					newConstraintMap.put(entry.getKey(), newDistinctFromKey); // puts same disequalities in new map, but this incorrectly includes 'variable'
					newDistinctFromKey.remove(variable); // so we remove it from the set (it is already in 'newConstraintMap')
					addDisequalityToConstraintMapDestructively(entry.getKey(), otherTerm, newConstraintMap, indices, process); // add 'otherTerm' wherever appropriate.
				}
				else {
					newConstraintMap.put(entry.getKey(), entry.getValue()); // shares sets between constraint maps
				}
			}
		}
		
		return newConstraintMap;
	}

	private static boolean equalityIsInconsistentWithConstraintMap(Expression variable, Expression otherTerm, Map<Expression, Collection<Expression>> constraintMap) {
		return getDistinctPredefinedTermsFrom(variable, constraintMap).contains(otherTerm) ||
				getDistinctPredefinedTermsFrom(otherTerm, constraintMap).contains(variable);
	}

	private static Collection<Expression> getDistinctPredefinedTermsFrom(Expression variable, Map<Expression, Collection<Expression>> constraintMap) {
		return Util.getOrUseDefault(constraintMap, variable, Collections.<Expression> emptyList());
	}

	private static void addAllDisequalitiesFromVariableToOtherTermInNewConstraintMap(Expression variable, Expression otherTerm, Map<Expression, Collection<Expression>> newConstraintMap, Map<Expression, Collection<Expression>> constraintMap, Collection<Expression> indices, RewritingProcess process) {
		Collection<Expression> distinctOnesFromVariable = getDistinctPredefinedTermsFrom(variable, constraintMap);
		for (Expression distinctFromVariable : distinctOnesFromVariable) {
			addDisequalityToConstraintMapDestructively(otherTerm, distinctFromVariable, newConstraintMap, indices, process);
		}
	}

	/** Assumes at least one of the two terms is a variable. */
	private static void addDisequalityToConstraintMapDestructively(
			Expression term1, Expression term2, Map<Expression, Collection<Expression>> constraintMap, Collection<Expression> indices, RewritingProcess process) {
		if (firstTermAlreadyDefinedWhenChoosingValueForSecondOne(term2, term1, indices, process)) {
			addFirstTermToDistinctTermsFromSecondTermDestructively(term1, term2, constraintMap);
		}
		else {
			addFirstTermToDistinctTermsFromSecondTermDestructively(term2, term1, constraintMap);
		}
	}

	private static void addFirstTermToDistinctTermsFromSecondTermDestructively(Expression term1, Expression term2, Map<Expression, Collection<Expression>> constraintMap) {
		Set<Expression> distinctFromTerm1 = (Set<Expression>) Util.getValuePossiblyCreatingIt(constraintMap, term1, LinkedHashSet.class);
		distinctFromTerm1.add(term2);
	}

	private static Map<Expression, Collection<Expression>> applyDisequalityToConstraint(
			Map<Expression, Collection<Expression>> constraintMap, Expression variable, Expression otherTerm, Collection<Expression> indices, RewritingProcess process) {
		
		Map<Expression, Collection<Expression>> result = new LinkedHashMap<Expression, Collection<Expression>>(constraintMap);
		if (firstTermAlreadyDefinedWhenChoosingValueForSecondOne(otherTerm, variable, indices, process)) {
			addSetOfDistinctTermsForTerm1OnConstraintMapBasedOnOldConstraintMapAndIncludingDisequalityFromTerm2(
					result, variable, otherTerm, constraintMap);
		}
		else {
			addSetOfDistinctTermsForTerm1OnConstraintMapBasedOnOldConstraintMapAndIncludingDisequalityFromTerm2(
					result, otherTerm, variable, constraintMap);
		}
		return result;
	}

	private static void addSetOfDistinctTermsForTerm1OnConstraintMapBasedOnOldConstraintMapAndIncludingDisequalityFromTerm2(
			Map<Expression, Collection<Expression>> newConstraintMap, Expression term1, Expression term2, Map<Expression, Collection<Expression>> oldConstraintMap) {
		Set<Expression> distinctTermsFromTerm1 = new LinkedHashSet<Expression>(getDistinctPredefinedTermsFrom(term1, oldConstraintMap));
		distinctTermsFromTerm1.add(term2);
		newConstraintMap.put(term1, distinctTermsFromTerm1);
	}

	private static boolean firstTermAlreadyDefinedWhenChoosingValueForSecondOne(Expression term, Expression variable, Collection<Expression> indices, RewritingProcess process) {
		boolean result = process.isConstant(term) || variablePrecedesAnother(term, variable, indices);
		return result;
	}

	/**
	 * Indicates whether variable1 precedes variable2 in the total ordering
	 */
	private static boolean variablePrecedesAnother(Expression variable1, Expression variable2, Collection<Expression> indices) {
		boolean result;
		if (indices.contains(variable1)) { // index
			if ( ! indices.contains(variable2)) { // free variable
				result = false; // free variables always precedes indices
			}
			else { // both are indices
				result = variable2.toString().compareTo(variable1.toString()) < 0; // indices are compared alphabetically
			}
		}
		else if (indices.contains(variable2)) { // variable1 is free variable and variable2 is index
			result = true; // free variable always precedes indices
		}
		else { // neither is index
			result = variable2.toString().compareTo(variable1.toString()) < 0;	// alphabetically		
		}
		return result;
	}
}
