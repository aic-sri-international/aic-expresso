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

import java.util.ArrayList;
import java.util.Collection;
import java.util.LinkedHashSet;
import java.util.Map;
import java.util.Set;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.library.FunctorConstants;
import com.sri.ai.grinder.library.boole.And;
import com.sri.ai.grinder.library.boole.Equivalence;
import com.sri.ai.grinder.library.boole.Implication;
import com.sri.ai.grinder.library.boole.Not;
import com.sri.ai.grinder.library.boole.Or;
import com.sri.ai.grinder.library.controlflow.IfThenElse;
import com.sri.ai.grinder.library.equality.formula.FormulaUtil;
import com.sri.ai.util.Util;
import com.sri.ai.util.base.BinaryFunction;
import com.sri.ai.util.math.Rational;

@Beta
/** 
 * A {@link Theory} for propositional logic.
 */
public class PropositionalTheory extends AbstractTheory {

	private static Map<String, BinaryFunction<Expression, RewritingProcess, Expression>> functionApplicationSimplifiers =
			Util.<String, BinaryFunction<Expression, RewritingProcess, Expression>>map(
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
	);

	@Override
	public Map<String, BinaryFunction<Expression, RewritingProcess, Expression>> getFunctionApplicationSimplifiers() {
		return functionApplicationSimplifiers;
	}

	@Override
	public Map<String, BinaryFunction<Expression, RewritingProcess, Expression>> getSyntacticFormTypeSimplifiers() {
		return syntacticFormTypeSimplifiers;
	}

	/**
	 * If expression is a proposition, it is a splitter.
	 * @param expression
	 * @param indices
	 * @param process
	 * @return
	 */
	@Override
	public Expression makeSplitterIfPossible(Expression expression, Collection<Expression> indices, RewritingProcess process) {
		Expression result;
		if (isProposition(expression)) {
			result = expression;
		}
		else {
			result = null;
		}
		return result;
	}

	/**
	 * @param expression
	 * @return
	 */
	protected boolean isProposition(Expression expression) {
		boolean result =
				expression.getSyntacticFormType().equals("Symbol")
				&&
				! FormulaUtil.EQUALITY_FORMULAS_PRIMITIVE_SYMBOLS.contains(expression);
		return result;
	}

	@Override
	public Expression applySplitterToExpression(Expression splitter, Expression expression, RewritingProcess process) {
		Expression result = expression.replaceAllOccurrences(splitter, Expressions.TRUE, process);
		result = simplify(result, process);
		return result;
	}

	@Override
	public Expression applySplitterNegationToExpression(Expression splitter, Expression expression, RewritingProcess process) {
		Expression result = expression.replaceAllOccurrences(splitter, Expressions.FALSE, process);
		result = simplify(result, process);
		return result;
	}

	@Override
	public boolean splitterDoesNotInvolveIndex(Expression splitter, Collection<Expression> indices) {
		boolean result = ! indices.contains(splitter);
		return result;
	}

	@Override
	public TheoryConstraint makeConstraint() {
		return new Constraint();
	}

	public static class Constraint implements TheoryConstraint {

		// Because applying either splitter or splitter negation always binds an index to a value,
		// we only need to remember asserted and negated free (that is, non-index) propositions.
		
		private Set<Expression> assertedFreePropositions = new LinkedHashSet<Expression>();
		private Set<Expression> negatedFreePropositions  = new LinkedHashSet<Expression>();
		
		public Constraint() {
		}
		
		public Constraint(Constraint another) {
			super();
			this.assertedFreePropositions = new LinkedHashSet<Expression>(another.assertedFreePropositions); // should be optimized to a copy-as-needed scheme.
			this.negatedFreePropositions = new LinkedHashSet<Expression>(another.negatedFreePropositions);
		}

		@Override
		public Expression pickSplitter(Collection<Expression> indices, RewritingProcess process) {
			return null; // we are always ready to provide a model count, so there is no need for extra splitters
		}

		@Override
		public TheoryConstraint applySplitter(Expression splitter, Collection<Expression> indices, RewritingProcess process) {
			Constraint result;
			if (indices.contains(splitter)) {
				result = this;
			}
			else {
				if (negatedFreePropositions.contains(splitter)) {
					result = null;
				}
				else {
					result = new Constraint(this);
					result.assertedFreePropositions.add(splitter);
				}
			}
			return result;
		}

		@Override
		public TheoryConstraint applySplitterNegation(Expression splitter, Collection<Expression> indices, RewritingProcess process) {
			Constraint result;
			if (indices.contains(splitter)) {
				result = this;
			}
			else {
				if (assertedFreePropositions.contains(splitter)) {
					result = null;
				}
				else {
					result = new Constraint(this);
					result.negatedFreePropositions.add(splitter);
				}
			}
			return result;
		}

		@Override
		public Expression getIndexBoundBySplitterApplicationIfAny(Expression splitter, Collection<Expression> indices) {
			return getIndexBoundByEitherSplitterOrItsNegation(splitter, indices);
		}

		@Override
		public Expression getIndexBoundBySplitterNegationApplicationIfAny(Expression splitter, Collection<Expression> indices) {
			return getIndexBoundByEitherSplitterOrItsNegation(splitter, indices);
		}

		private Expression getIndexBoundByEitherSplitterOrItsNegation(Expression splitter, Collection<Expression> indices) {
			Expression result;
			if (indices.contains(splitter)) {
				result = splitter;
			}
			else {
				result = null;
			}
			return result;
		}

		@Override
		public Expression modelCount(Collection<Expression> indices, RewritingProcess process) {
			Expression result = Expressions.makeSymbol(new Rational(2).pow(indices.size()));
			return result;
		}

		@Override
		public Expression getMostRequiredSplitter(Expression splitterCandidate, Collection<Expression> indices, RewritingProcess process) {
			return splitterCandidate; // a proposition can always be imposed without need for prior assumptions
		}
		
		@Override
		public String toString() {
			ArrayList<String> items = new ArrayList<String>();
			if ( ! assertedFreePropositions.isEmpty()) {
				items.add(Util.join(assertedFreePropositions) + " are true");
			}
			if ( ! negatedFreePropositions.isEmpty()) {
				items.add(Util.join(negatedFreePropositions) + " are false");
			}
			String result = "Free propositions " + Util.join(items);
			return result;
		}
	}
}
