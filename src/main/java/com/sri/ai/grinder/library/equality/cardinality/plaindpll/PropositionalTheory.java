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
	public Expression applySplitterToExpression(boolean splitterSign, Expression splitter, Expression expression, RewritingProcess process) {
		if ( ! splitterSign) {
			return applySplitterNegationToExpression(splitter, expression, process);
		}
		
		Expression result = expression.replaceAllOccurrences(splitter, Expressions.TRUE, process);
		result = simplify(result, process);
		return result;
	}

	private Expression applySplitterNegationToExpression(Expression splitter, Expression expression, RewritingProcess process) {
		Expression result = expression.replaceAllOccurrences(splitter, Expressions.FALSE, process);
		result = simplify(result, process);
		return result;
	}

	@Override
	public boolean splitterDependsOnIndex(Expression splitter, Collection<Expression> indices) {
		boolean result = indices.contains(splitter);
		return result;
	}

	@Override
	public boolean applicationOfConstraintOnSplitterAlwaysEitherTrivializesItOrEffectsNoChangeAtAll() {
		return true; // a proposition or its negation is either implied by a constraint, or not altered at all.
	}

	@Override
	public Constraint makeConstraint(Collection<Expression> indices) {
		return new Constraint(indices);
	}

	public static class Constraint implements Theory.Constraint {

		private Collection<Expression> indices;
		private int numberOfBoundIndices;
		
		private Set<Expression> assertedPropositions;
		private Set<Expression> negatedPropositions;
		
		public Constraint(Collection<Expression> indices) {
			this.indices = indices;
			this.numberOfBoundIndices = 0;
			assertedPropositions = new LinkedHashSet<Expression>();
			negatedPropositions  = new LinkedHashSet<Expression>();
		}
		
		public Constraint(Constraint another) {
			super();
			this.indices = another.indices;
			this.numberOfBoundIndices = another.numberOfBoundIndices;
			this.assertedPropositions = new LinkedHashSet<Expression>(another.assertedPropositions); // should be optimized to a copy-as-needed scheme.
			this.negatedPropositions = new LinkedHashSet<Expression>(another.negatedPropositions);
		}

		@Override
		public Collection<Expression> getIndices() {
			return indices;
		}

		@Override
		public Expression pickSplitter(RewritingProcess process) {
			return null; // we are always ready to provide a model count, so there is no need for extra splitters
		}

		@Override
		public Expression checkIfSplitterOrItsNegationIsImplied(Expression splitter, RewritingProcess process) {
			if (assertedPropositions.contains(splitter)) {
				return Expressions.TRUE;
			}
			if (negatedPropositions.contains(splitter)) {
				return Expressions.FALSE;
			}
			return splitter;
		}

		@Override
		public Constraint applySplitter(boolean splitterSign, Expression splitter, boolean guaranteed, RewritingProcess process) {
			if ( ! splitterSign) {
				return applySplitterNegation(splitter, process);
			}
			
			Constraint result;
			if (negatedPropositions.contains(splitter)) {
				result = null; // contradiction
			}
			else if (assertedPropositions.contains(splitter)) {
				result = this; // redundant
			}
			else {
				result = new Constraint(this);
				result.assertedPropositions.add(splitter);
				if (indices.contains(splitter)) {
					result.numberOfBoundIndices++;
				}
			}
			return result;
		}

		private Constraint applySplitterNegation(Expression splitter, RewritingProcess process) {
			Constraint result;
			if (assertedPropositions.contains(splitter)) {
				result = null; // contradiction
			}
			else if (negatedPropositions.contains(splitter)) {
				result = this; // redundant
			}
			else {
				result = new Constraint(this);
				result.negatedPropositions.add(splitter);
				if (indices.contains(splitter)) {
					result.numberOfBoundIndices++;
				}
			}
			return result;
		}

		@Override
		public Expression modelCount(RewritingProcess process) {
			Expression result = Expressions.makeSymbol(new Rational(2).pow(indices.size() - numberOfBoundIndices));
			return result;
		}

		@Override
		public Expression normalize(Expression expression, RewritingProcess process) {
			String syntacticTypeForm = "Symbol";
			BinaryFunction<Expression, RewritingProcess, Expression> simplifier =
					(BinaryFunction<Expression, RewritingProcess, Expression>)
					(s, p) -> assertedPropositions.contains(s)? Expressions.TRUE : negatedPropositions.contains(s)? Expressions.FALSE : s;

			Expression result = DPLLUtil.simplifyWithExtraSyntacticFormTypeSimplifier(
					expression,
					PropositionalTheory.functionApplicationSimplifiers,
					PropositionalTheory.syntacticFormTypeSimplifiers,
					syntacticTypeForm, simplifier,
					process);
			
			return result;
		}

		@Override
		public String toString() {
			ArrayList<String> items = new ArrayList<String>();
			items.add("Indices " + Util.join(indices));
			if ( ! assertedPropositions.isEmpty()) {
				items.add("asserted: " + Util.join(assertedPropositions));
			}
			if ( ! negatedPropositions.isEmpty()) {
				items.add("negated: " + Util.join(negatedPropositions));
			}
			String result = Util.join(items);
			return result;
		}
	}
}
