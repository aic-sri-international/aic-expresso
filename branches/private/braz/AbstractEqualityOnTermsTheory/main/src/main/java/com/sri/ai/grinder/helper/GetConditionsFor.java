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
package com.sri.ai.grinder.helper;

import com.google.common.annotations.Beta;
import com.google.common.base.Function;
import com.google.common.base.Predicate;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.expresso.helper.FunctionApplicationContainsArgumentSatisfying;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.library.Equality;
import com.sri.ai.grinder.library.IsVariable;
import com.sri.ai.grinder.library.boole.Not;
import com.sri.ai.grinder.library.controlflow.IfThenElse;
import com.sri.ai.grinder.library.equality.cardinality.direct.CardinalityRewriter;
import com.sri.ai.grinder.library.equality.formula.FormulaUtil;
import com.sri.ai.util.base.And;
import com.sri.ai.util.base.NotEquals;
import com.sri.ai.util.base.TernaryFunction;

/**
 * A library with a generic function for conditioning on atoms of a formula.
 * It also contains an algorithm for computing a BDD for the formula using the generic method. 
 * 
 * @author braz
 */
@Beta
public class GetConditionsFor {

	/**
	 * Computes a Binary Decision Diagram equivalent to a given quantifier-free formula.
	 * Any branch in it is "complete" in the sense that it will be "true" if tautological and "false" if contradictory.
	 * Our representation may contain formulas in the leaves instead of just true and false as in the usual definition of BDDs. 
	 */
	public static Expression getBDDOfQuantifierFreeFormula(Expression formula, final RewritingProcess process) {
		TernaryFunction<Expression, Expression, Expression, Expression> trivialCombinationOfBranches = new TernaryFunction<Expression, Expression, Expression, Expression>() {
			@Override
			public Expression apply(Expression splitter, Expression splitterIsTrue, Expression splitterIsFalse) {
				return IfThenElse.makeWithEqualityOrDisequalityConditionAndInvertedToEqualityIfNeeded(splitter, splitterIsTrue, splitterIsFalse);
				// this is "complete" in the sense that it will be "true" if tautological and "false" if contradictory, assuming the branches are also complete in this way.
				// add a 'false' flag at the end of the method invocation above to prevent leaves 'if C then true else false' to be reduced to 'C'.
				// It also normalizes the condition to equality for convenience.
			}
		};
		Function<Expression, Expression> simplify = new Function<Expression, Expression>() {
			@Override
			public Expression apply(Expression formula) {
				Expression result = process.rewrite(CardinalityRewriter.R_formula_simplify, formula);
				return result;
			}
		};
		Expression result = conditionOnAtomsSatisfyingPredicate(formula, Expressions.TRUE_PREDICATE, trivialCombinationOfBranches, simplify, process);
		
		return result;
	}
	
	/**
	 * Receives a formula and a variable of interest 
	 * and returns a if-then-else decision tree expression equivalent to the given formula,
	 * such that only the leaves may contain literals with the variable of interest,
	 * and such that the inner conditions contain only atoms with the other variables.
	 * This is useful for telling whether (and if so, to what) the value of a variable is implied to by the formula,
	 * under any assignment to the other variables.
	 */
	public static Expression getConditionsForVariable(Expression variable, Expression formula, final RewritingProcess process) {
		Predicate<Expression> isNotGivenVariable = NotEquals.make(variable);
		Predicate<Expression> isAnotherVariable = And.make(new IsVariable(process), isNotGivenVariable);
		Predicate<Expression> containsAnotherVariable = new FunctionApplicationContainsArgumentSatisfying(isAnotherVariable);
		TernaryFunction<Expression, Expression, Expression, Expression> makeInnerConditionNode = new TernaryFunction<Expression, Expression, Expression, Expression>() {
			@Override
			public Expression apply(Expression splitter, Expression splitterIsTrue, Expression splitterIsFalse) {
				return IfThenElse.makeWithEqualityOrDisequalityConditionAndInvertedToEqualityIfNeeded(splitter, splitterIsTrue, splitterIsFalse, false);
				// 'false' above does not allow simplification from 'if C then true else false' to 'C', lest conditioning atoms end up in leaves
			}
		};
		Function<Expression, Expression> simplifyFormulaOnVariableAlone = new Function<Expression, Expression>() {
			@Override
			public Expression apply(Expression formula) {
				// return process.rewrite(CardinalityRewriter.R_complete_normalize, formula);
				return processFormulaOnVariable(formula, process);
			}
		};
		Expression result = conditionOnAtomsSatisfyingPredicate(formula, containsAnotherVariable, makeInnerConditionNode, simplifyFormulaOnVariableAlone, process);
		
		return result;
	}
	
	private static Expression processFormulaOnVariable(Expression formula, final RewritingProcess process) {
		TernaryFunction<Expression, Expression, Expression, Expression> makeLeafNode = new TernaryFunction<Expression, Expression, Expression, Expression>() {
			@Override
			public Expression apply(Expression splitter, Expression splitterIsTrue, Expression splitterIsFalse) {
				Expression result = IfThenElse.makeWithEqualityOrDisequalityConditionAndInvertedToEqualityIfNeeded(splitter, splitterIsTrue, splitterIsFalse);
				// At this point, result is an atom or true or or false, or at least an if then else with one branch equal to an atom or true or false,
				// because distinct equalities on one variable cannot nest up. For example,
				// 'if X = a then if X = b then true else false else X = c' gets simplified to 'if X = a then true else X = c' 
				result = process.rewrite(CardinalityRewriter.R_formula_simplify, result);
				// then formulaSimplify cleans things up and converts things like 'if X = a then true else X = c' to the nicer 'X = a or X = c'
				// It also guarantees that formulas implying a single value v for the variable will be simplified to "X = v"
				return result;
			}
		};
		Expression result = conditionOnAtomsSatisfyingPredicate(formula, Expressions.TRUE_PREDICATE, makeLeafNode, Expressions.IDENTITY_FUNCTION, process);
		
		return result;
	}

	/**
	 * Receives a quantifier-free formula and returns an equivalent decision tree (nested if then elses)
	 * such that only a given type of atom is used for conditioning (that is, to be in the inner conditions of the tree).
	 * The leaves are guaranteed to be reachable, and to not contain conditioning atoms. 
	 * This is useful for determining the truth value of a group of atoms as a function of the truth values of the remaining ones.
	 */
	public static Expression conditionOnAtomsSatisfyingPredicate(
			Expression formula, 
			Predicate<Expression> isConditioningAtom, 
			TernaryFunction<Expression, Expression, Expression, Expression> assembleSplitterTree, 
			Function<Expression, Expression> processLeafFormula,
			RewritingProcess process) {
		formula = process.rewrite(CardinalityRewriter.R_quantifier_elimination, formula);
		Expression result = conditionQuantifierFreeFormulaOnAtomsSatisfyingPredicate(formula, isConditioningAtom, assembleSplitterTree, processLeafFormula, process);
		return result;
	}
	
	/**
	 * Receives a quantifier-free formula and returns an equivalent decision tree (nested if then elses)
	 * such that only a given type of atom is used for conditioning (that is, to be in the inner conditions of the tree).
	 * The leaves are guaranteed to be reachable, and to not contain conditioning atoms. 
	 * This is useful for determining the truth value of a group of atoms as a function of the truth values of the remaining ones.
	 */
	public static Expression conditionQuantifierFreeFormulaOnAtomsSatisfyingPredicate(
			Expression formula,
			Predicate<Expression> isConditioningAtom,
			TernaryFunction<Expression, Expression, Expression, Expression> assembleSplitterTree,
			Function<Expression, Expression> processLeafFormula,
			RewritingProcess process) {
		Expression result;
		
		Expression splitter = FormulaUtil.pickAtomSatisfyingPredicateFromQuantifierFreeFormula(formula, isConditioningAtom, process);
		
		if (splitter != null) {
			Expression splitterIsTrue  = conditionsUnderGivenCondition(         splitter,  formula, isConditioningAtom, assembleSplitterTree, processLeafFormula, process);
			Expression splitterIsFalse = conditionsUnderGivenCondition(Not.make(splitter), formula, isConditioningAtom, assembleSplitterTree, processLeafFormula, process);
			if ( ! splitterIsTrue.equals(splitterIsFalse)) {
				result = assembleSplitterTree.apply(splitter, splitterIsTrue, splitterIsFalse);
			}
			else {
				result = splitterIsTrue;
				// It is important that 'if L then true else true' cases (and similar ones) be simplified to 'true'
				// because there will be no further simplifications from now on!
				// Note that the lack of simplifications from now on does not violate the guarantee of completeness because
				// the condition L is guaranteed not to occur in the branches, and the branches themselves are completely normalized.
				// This means that the result is unsatisfiable/tautological if and only if the branches are both unsatisfiable/tautological as well,
				// as, as we just mentioned, IfThenElse.make makes that a 'false'/'true' respectively.
			}
		}
		else {
			result = processLeafFormula.apply(formula);			
		}
		
		return result;
	}

	private static Expression conditionsUnderGivenCondition(
			Expression givenCondition, 
			Expression formula, 
			Predicate<Expression> isConditioningAtom, 
			TernaryFunction<Expression, Expression, Expression, Expression> assembleSplitterTree, 
			Function<Expression, Expression> processLeafFormula,
			RewritingProcess process) {
		RewritingProcess subProcessGivenCondition = GrinderUtil.extendContextualConstraint(givenCondition, process);
		Expression formulaGivenCondition = subProcessGivenCondition.rewrite(CardinalityRewriter.R_formula_simplify, formula);
		// the simplification above eliminates the given condition from the formula and justifies the recursion below.
		// it does not need to be a complete simplification; that role is performed by the induction base case.
		formulaGivenCondition = conditionQuantifierFreeFormulaOnAtomsSatisfyingPredicate(formulaGivenCondition, isConditioningAtom, assembleSplitterTree, processLeafFormula, subProcessGivenCondition);
		return formulaGivenCondition;
	}

	/**
	 * Meant to replace pick single value but it does not actually work! See its tests.
	 */
	@Deprecated
	public static Expression getImpliedValueFromBDD(Expression variable, Expression bdd, RewritingProcess process) {
		Expression result;
	
		if (IfThenElse.isIfThenElse(bdd)) {
			Expression condition = IfThenElse.getCondition(bdd);
			Expression bddWhenVariableEqualValue    = IfThenElse.getThenBranch(bdd);
			Expression bddWhenVariableNotEqualValue = IfThenElse.getElseBranch(bdd);
			Expression valueOrNullIfNotEqualityOnVariable = Equality.getWhatExpressionIsComparedToIfUniqueOrNull(condition, variable);
			if (valueOrNullIfNotEqualityOnVariable != null) {
				Expression value = valueOrNullIfNotEqualityOnVariable;
				
				if (bddWhenVariableEqualValue.equals(Expressions.FALSE)) {
					// variable cannot be 'value', so answer is what the else branch says
					result = getImpliedValueFromBDD(variable, bddWhenVariableNotEqualValue, process);
				}
				else {
					// variable = value is acceptable. Now we check whether else branch (when variable != value) provides other acceptable values
					if ( ! bddWhenVariableNotEqualValue.equals(Expressions.FALSE)) {
						// there are alternative satisfying values, so there is no implied value
						result = null;
					}
					else {
						// ok, no conflicting alternatives, so the only implied value is 'value'
						result = value;
					}
				}
			}
			else {
				// condition is not on variable, so we just consider both scenarios 
				Expression thenBranch = getImpliedValueFromBDD(variable, bddWhenVariableEqualValue, process);
				if (thenBranch != null) {
					Expression elseBranch = getImpliedValueFromBDD(variable, bddWhenVariableNotEqualValue, process);
					if (elseBranch != null) {
						result = IfThenElse.make(condition, thenBranch, elseBranch);
					}
					else {
						result = null; // there is no implied value for the else branch
					}
				}
				else {
					result = null; // there is no implied value for the then branch
				}
			}
		}
		else {
			// BDD is either an atom or true or false
			// First we check if it is an equality on the variable
			Expression valueOrNullIfNotEqualityOnVariable = Equality.getWhatExpressionIsComparedToIfUniqueOrNull(bdd, variable);
			if (valueOrNullIfNotEqualityOnVariable != null) {
				result = valueOrNullIfNotEqualityOnVariable; // it is, so just return the value
			}
			else {
				// the BDD is either a tautology (accepting all values of the variable) or a contradiction (accepting none),
				// or a condition unrelated to variable.
				// For all these, there is no implied value.
				result = null;
			}
		}
		return result;
	}
}
