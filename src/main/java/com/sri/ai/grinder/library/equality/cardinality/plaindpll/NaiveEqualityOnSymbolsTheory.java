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

import static com.sri.ai.util.Util.sortPairMakingFirstOneSatisfyPredicateIfPossible;

import java.util.Collection;
import java.util.Iterator;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.library.Disequality;
import com.sri.ai.grinder.library.Equality;
import com.sri.ai.grinder.library.FunctorConstants;
import com.sri.ai.grinder.library.IsVariable;
import com.sri.ai.grinder.library.boole.And;
import com.sri.ai.grinder.library.boole.Equivalence;
import com.sri.ai.grinder.library.boole.ForAll;
import com.sri.ai.grinder.library.boole.Implication;
import com.sri.ai.grinder.library.boole.Not;
import com.sri.ai.grinder.library.boole.Or;
import com.sri.ai.grinder.library.boole.ThereExists;
import com.sri.ai.grinder.library.controlflow.IfThenElse;
import com.sri.ai.util.Util;
import com.sri.ai.util.base.BinaryFunction;
import com.sri.ai.util.base.Equals;
import com.sri.ai.util.base.Pair;
import com.sri.ai.util.base.Triple;
import com.sri.ai.util.collect.EZIterator;

/**
 * 
 * @author braz
 *
 */
@Beta
public class NaiveEqualityOnSymbolsTheory extends AbstractTheory {

	private static Map<String, BinaryFunction<Expression, RewritingProcess, Expression>> functionApplicationSimplifiers =
			Util.<String, BinaryFunction<Expression, RewritingProcess, Expression>>map(
					FunctorConstants.EQUALITY,       (BinaryFunction<Expression, RewritingProcess, Expression>) (f, process) ->
					Equality.simplify(f, process),
					
					FunctorConstants.DISEQUALITY,    (BinaryFunction<Expression, RewritingProcess, Expression>) (f, process) ->
					Disequality.simplify(f, process),

					FunctorConstants.DISEQUALITY,    (BinaryFunction<Expression, RewritingProcess, Expression>) (f, process) ->
					Disequality.simplify(f, process),

					FunctorConstants.AND,            (BinaryFunction<Expression, RewritingProcess, Expression>) (f, process) ->
					And.simplify(f),

					FunctorConstants.OR,             (BinaryFunction<Expression, RewritingProcess, Expression>) (f, process) ->
					Or.simplify(f),

					FunctorConstants.NOT,            (BinaryFunction<Expression, RewritingProcess, Expression>) (f, process) ->
					Not.simplify(f),

					FunctorConstants.IF_THEN_ELSE,   (BinaryFunction<Expression, RewritingProcess, Expression>) (f, process) ->
					IfThenElse.simplify(f),

					FunctorConstants.EQUIVALENCE,    (BinaryFunction<Expression, RewritingProcess, Expression>) (f, process) ->
					Equivalence.simplify(f),

					FunctorConstants.IMPLICATION,    (BinaryFunction<Expression, RewritingProcess, Expression>) (f, process) ->
					Implication.simplify(f)
	);
	
	private static Map<String, BinaryFunction<Expression, RewritingProcess, Expression>> syntacticFormTypeSimplifiers =
			Util.<String, BinaryFunction<Expression, RewritingProcess, Expression>>map(
					ForAll.SYNTACTIC_FORM_TYPE,                             (BinaryFunction<Expression, RewritingProcess, Expression>) (f, process) ->
					(new SymbolEqualityTautologicalityDPLL()).rewrite(f, process),
 
					ThereExists.SYNTACTIC_FORM_TYPE,                        (BinaryFunction<Expression, RewritingProcess, Expression>) (f, process) ->
					(new SymbolicGenericDPLL(new SymbolEqualityTheory(), new Satisfiability())).rewrite(f, process)
	);

	@Override
	public Map<String, BinaryFunction<Expression, RewritingProcess, Expression>> getFunctionApplicationSimplifiers() {
		return functionApplicationSimplifiers;
	}

	@Override
	public Map<String, BinaryFunction<Expression, RewritingProcess, Expression>> getSyntacticFormTypeSimplifiers() {
		return syntacticFormTypeSimplifiers;
	}

	@Override
	public Expression makeSplitterIfPossible(Expression expression, Collection<Expression> indices, RewritingProcess process) {
		Expression result;
		expression = Equality.simplifyIfEqualityOrDisequality(expression, process);
		if (Equality.isEquality(expression) || Disequality.isDisequality(expression)) {
			// Remember that equality can have an arbitrary number of terms
			Expression variable  = Util.getFirstSatisfyingPredicateOrNull(expression.getArguments(), new IsVariable(process));
			Expression otherTerm = Util.getFirstSatisfyingPredicateOrNull(expression.getArguments(), com.sri.ai.util.base.Not.make(Equals.make(variable)));
			Pair<Expression, Expression> terms = sortPairMakingFirstOneSatisfyPredicateIfPossible(variable, otherTerm, t -> indices.contains(t));
			result = Equality.make(terms.first, terms.second);
		}
		else {
			result = null;
		}
		return result;
	}

	@Override
	public boolean splitterInvolvesIndex(Expression splitter, Collection<Expression> indices) {
		boolean result = indices.contains(splitter.get(0));
		return result;
	}

	@Override
	public Expression applySplitterToExpression(Expression splitter, Expression expression, RewritingProcess process) {
		Expression term1 = splitter.get(0);
		Expression term2 = splitter.get(1);
		Expression result = expression.replaceAllOccurrences(new SimplifyLiteralGivenEquality(term1, term2), process);
		result = simplify(result, process);
		return result;
	}

	@Override
	public Expression applySplitterNegationToExpression(Expression splitter, Expression expression, RewritingProcess process) {
		Expression term1 = splitter.get(0);
		Expression term2 = splitter.get(1);
		Expression result = expression.replaceAllOccurrences(new SimplifyLiteralGivenDisequality(term1, term2), process);
		result = simplify(result, process);
		return result;
	}

	@Override
	public TheoryConstraint makeConstraint() {
		return new Constraint();
	}
	
	private class Constraint implements TheoryConstraint {
		
		private Map<Expression, Expression>      fromVariableToValue;
		private Map<Expression, Set<Expression>> fromVariableToDisequals;

		@Override
		public Expression pickSplitter(Collection<Expression> indices, RewritingProcess process) {
//			Pair<Expression, Pair<Expression, Expression>> variableWithDisequalsUndeterminedToBeDisequalsThemselves =
//					Util.getFirstSatisfyingPredicateOrNull(
//							getVariableAndPairOfDisequalsIterator(),
//							new DisequalsUndeterminedToBeDisequalsThemselves());
//			Expression result;
//			if (variableWithDisequalsUndeterminedToBeDisequalsThemselves != null) {
//				Expression firstDisequal  = variableWithDisequalsUndeterminedToBeDisequalsThemselves.second.first;
//				Expression secondDisequal = variableWithDisequalsUndeterminedToBeDisequalsThemselves.second.second;
//				result = makeSplitterIfPossible(Equality.make(firstDisequal, secondDisequal), indices, process);
//			}
//			else {
//				result = null;
//			}
//			return result;
			return null;
		}

		@Override
		public TheoryConstraint applySplitter(Expression splitter, Collection<Expression> indices, RewritingProcess process) {
			// TODO Auto-generated method stub
			return null;
		}

		@Override
		public TheoryConstraint applySplitterNegation(Expression splitter, Collection<Expression> indices, RewritingProcess process) {
			// TODO Auto-generated method stub
			return null;
		}

		@Override
		public Expression getIndexBoundBySplitterIfAny(Expression splitter, Collection<Expression> indices) {
			// TODO Auto-generated method stub
			return null;
		}

		@Override
		public Expression getIndexBoundBySplitterNegationIfAny(Expression splitter, Collection<Expression> indices) {
			// TODO Auto-generated method stub
			return null;
		}

		@Override
		public Expression modelCount(Collection<Expression> indices, RewritingProcess process) {
			// TODO Auto-generated method stub
			return null;
		}

		@Override
		public Expression getMostRequiredSplitter(Expression splitterCandidate, Collection<Expression> indices, RewritingProcess process) {
			// TODO Auto-generated method stub
			return null;
		}
		
	}
}