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
import com.google.common.base.Predicate;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.helper.FunctionApplicationContainsArgumentSatisfying;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.library.IsVariable;
import com.sri.ai.grinder.library.boole.Not;
import com.sri.ai.grinder.library.controlflow.IfThenElse;
import com.sri.ai.grinder.library.equality.cardinality.direct.CardinalityRewriter;
import com.sri.ai.grinder.library.equality.formula.FormulaUtil;
import com.sri.ai.util.base.And;
import com.sri.ai.util.base.NotEquals;

/**
 * A library for computing conditions in a formula for determining the truth value of another condition in the same formula. 
 * 
 * @author braz
 */
@Beta
public class GetConditionsFor {

	/**
	 * Receives a formula and a variable of interest 
	 * and returns a if-then-else decision tree expression equivalent to the given formula,
	 * such that only the leaves may contain literals with the variable of interest,
	 * and such that the inner conditions contain only atoms with the other variables.
	 * This is useful for telling whether (and if so, to what) the value of a variable is implied to by the formula,
	 * under any assignment to the other variables.
	 */
	public static Expression getConditionsForVariable(Expression variable, Expression formula, RewritingProcess process) {
		Predicate<Expression> isNotGivenVariable = NotEquals.make(variable);
		Predicate<Expression> isAnotherVariable = And.make(new IsVariable(process), isNotGivenVariable);
		Predicate<Expression> containsAnotherVariable = new FunctionApplicationContainsArgumentSatisfying(isAnotherVariable);

		Expression result = conditionOnAtomsSatisfyingPredicate(formula, containsAnotherVariable, process);
		
		return result;
	}

	/**
	 * Receives a quantifier-free formula and returns an equivalent decision tree (nested if then elses)
	 * such that only a given type of atom is used for conditioning (that is, to be in the inner conditions of the tree).
	 * The leaves are guaranteed to be reachable, and to not contain conditioning atoms. 
	 * This is useful for determining the truth value of a group of atoms as a function of the truth values of the remaining ones.
	 */
	public static Expression conditionOnAtomsSatisfyingPredicate(Expression formula, Predicate<Expression> isConditioningAtom, RewritingProcess process) {
		formula = process.rewrite(CardinalityRewriter.R_quantifier_elimination, formula);
		Expression result = conditionQuantifierFreeFormulaOnAtomsSatisfyingPredicate(formula, isConditioningAtom, process);
		return result;
	}
	
	/**
	 * Receives a quantifier-free formula and returns an equivalent decision tree (nested if then elses)
	 * such that only a given type of atom is used for conditioning (that is, to be in the inner conditions of the tree).
	 * The leaves are guaranteed to be reachable, and to not contain conditioning atoms. 
	 * This is useful for determining the truth value of a group of atoms as a function of the truth values of the remaining ones.
	 */
	public static Expression conditionQuantifierFreeFormulaOnAtomsSatisfyingPredicate(Expression formula, Predicate<Expression> isConditioningAtom, RewritingProcess process) {
		Expression result;
		
		Expression splitter = FormulaUtil.pickAtomOfInterestFromQuantifierFreeFormula(formula, isConditioningAtom, process);
		
		if (splitter != null) {
			Expression splitterIsTrue  = conditionsUnderGivenCondition(         splitter,  formula, isConditioningAtom, process);
			Expression splitterIsFalse = conditionsUnderGivenCondition(Not.make(splitter), formula, isConditioningAtom, process);
			if ( ! splitterIsTrue.equals(splitterIsFalse)) {
				result = IfThenElse.make(splitter, splitterIsTrue, splitterIsFalse, false);
				// 'false' above does not allow simplification to 'splitter', lest conditioning atom end up in leave
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
			result = process.rewrite(CardinalityRewriter.R_complete_normalize, formula);			
		}
		
		return result;
	}

	private static Expression conditionsUnderGivenCondition(Expression givenCondition, Expression formula, Predicate<Expression> isConditioningAtom, RewritingProcess process) {
		RewritingProcess subProcessGivenCondition = GrinderUtil.extendContextualConstraint(givenCondition, process);
		Expression formulaGivenCondition = subProcessGivenCondition.rewrite(CardinalityRewriter.R_formula_simplify, formula);
		// the simplification above eliminates the given condition from the formula and justifies the recursion below.
		// it does not need to be a complete simplification; that role is performed by the induction base case.
		formulaGivenCondition = conditionQuantifierFreeFormulaOnAtomsSatisfyingPredicate(formulaGivenCondition, isConditioningAtom, subProcessGivenCondition);
		return formulaGivenCondition;
	}
}
