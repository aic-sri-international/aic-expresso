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
package com.sri.ai.grinder.plaindpll.core;

import static com.sri.ai.expresso.helper.Expressions.FALSE;
import static com.sri.ai.expresso.helper.Expressions.ONE;
import static com.sri.ai.expresso.helper.Expressions.TRUE;
import static com.sri.ai.expresso.helper.Expressions.ZERO;
import static com.sri.ai.expresso.helper.Expressions.applyJavaFunctionToArgumentsAndReAssembleFunctionApplication;
import static com.sri.ai.util.Util.getFirstNonNullResultOrNull;

import java.util.Collection;
import java.util.Collections;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.helper.AbstractExpressionWrapper;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.library.controlflow.IfThenElse;
import com.sri.ai.grinder.library.equality.formula.FormulaUtil;
import com.sri.ai.grinder.plaindpll.api.Constraint;
import com.sri.ai.grinder.plaindpll.api.Solver;
import com.sri.ai.grinder.plaindpll.api.Theory;
import com.sri.ai.grinder.plaindpll.problemtype.ModelCounting;

/**
 * An implementation of {@link Constraint} based on an expression representing it
 * (current only boolean formulas on the literals of a given theory are supported).
 * Note that this expressions or its sub-expressions can be implementations of {@link Constraint} themselves,
 * and when this is the case, this implementation exploits their efficient internal representations for its own purposes.
 * 
 * @author braz
 *
 */
@SuppressWarnings("serial")
public class ExpressionConstraint extends AbstractExpressionWrapper implements Constraint {

	private Expression expression;
	private Collection<Expression> supportedIndices;
	private Theory theory;
	
	public ExpressionConstraint(Theory theory, Collection<Expression> supportedIndices, Expression expression) {
		assert expression != null : getClass().getSimpleName() + " cannot wrap a null value.";
		this.expression = expression instanceof ExpressionConstraint? ((ExpressionConstraint) expression).expression : expression;
		this.supportedIndices = supportedIndices;
		this.theory = theory;
	}
	
	@Override
	public Expression clone() {
		return new ExpressionConstraint(theory, supportedIndices, expression);
	}

	@Override
	public Theory getTheory() {
		return theory;
	}

	@Override
	protected Expression computeInnerExpression() {
		return expression;
	}

	@Override
	public Collection<Expression> getSupportedIndices() {
		return Collections.unmodifiableCollection(supportedIndices);
	}

	@Override
	public Constraint incorporate(boolean splitterSign, Expression splitter, RewritingProcess process) {
		Constraint result;
		Expression splitterIfAny;
		if (expression instanceof Constraint) {
			result = ((Constraint) expression).incorporate(splitterSign, splitter, process);
		}
		else if (expression.equals(FALSE)) {
			result = wrap(FALSE);
		}
		else if (expression.equals(TRUE)) {
			Constraint newConstraint = theory.makeConstraint(supportedIndices);
			result = newConstraint.incorporate(splitterSign, splitter, process);
		}
		else if ((splitterIfAny = theory.makeSplitterIfPossible(expression, supportedIndices, process)) != null) {
			Constraint newConstraint = theory.makeConstraint(supportedIndices);
			newConstraint.incorporate(true, splitterIfAny, process);
			result = newConstraint.incorporate(splitterSign, splitter, process);
		}
		else { // only acceptable leaves are boolean constants and splitters, so at this point it must be a boolean connective.
			assert FormulaUtil.functorIsALogicalConnectiveIncludingConditionals(expression) : "Only boolean formulas on theory literals supported by " + getClass();
			result = wrap(applyJavaFunctionToArgumentsAndReAssembleFunctionApplication(
					subExpression -> wrap(subExpression).incorporate(splitterSign, splitter, process),
					expression));
		}
		return result;
	}

	@Override
	public Expression pickSplitter(Collection<Expression> indicesSubSet, RewritingProcess process) {
		Expression result;
		Expression splitterIfAny;
		if (expression instanceof Constraint) {
			result = ((Constraint) expression).pickSplitter(indicesSubSet, process);
		}
		else if (expression.equals(FALSE) || expression.equals(TRUE)) {
			result = null;
		}
		else if ((splitterIfAny = theory.makeSplitterIfPossible(expression, supportedIndices, process)) != null) {
			result = splitterIfAny;
		}
		else { // only acceptable leaves are boolean constants and splitters, so at this point it must be a boolean connective.
			assert FormulaUtil.functorIsALogicalConnectiveIncludingConditionals(expression) : "Only boolean formulas on theory literals supported by " + getClass();
			result = getFirstNonNullResultOrNull(expression.getArguments(), subExpression -> wrap(subExpression).pickSplitter(indicesSubSet, process));
		}
		return result;
	}

	@Override
	public Expression modelCount(Collection<Expression> indicesSubSet, RewritingProcess process) {
		Solver solver = getSolver();
		Expression result = solver.solve(IfThenElse.make(this, ONE, ZERO), indicesSubSet, process);
		return result;
	}

	private Solver cachedSolver;
	public Solver getSolver() {
		if (cachedSolver == null) {
			cachedSolver = new SGDPLLT(theory, new ModelCounting()); 
		}
		return cachedSolver;
	}

	/**
	 * If expression is {@link Constraint}, return it; otherwise, wrap it in {@link ExpressionConstraint} and return that.
	 * @param expression
	 * @return
	 */
	public Constraint wrap(Expression expression) {
		Constraint result;
		if (expression instanceof Constraint) {
			result = (Constraint) expression;
		}
		else {
			result = new ExpressionConstraint(theory, supportedIndices, expression);
		}
		return result;
	}
}