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
import static com.sri.ai.util.Util.myAssert;

import java.util.Collection;
import java.util.Collections;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.api.Solver;
import com.sri.ai.grinder.library.controlflow.IfThenElse;
import com.sri.ai.grinder.library.equality.formula.FormulaUtil;
import com.sri.ai.grinder.plaindpll.api.Constraint1;
import com.sri.ai.grinder.plaindpll.api.ConstraintTheory;
import com.sri.ai.grinder.plaindpll.problemtype.ModelCounting;
import com.sri.ai.grinder.plaindpll.theory.AbstractConstraint;
import com.sri.ai.grinder.plaindpll.theory.DefaultInputTheory;
import com.sri.ai.util.Util;

/**
 * An implementation of {@link Constraint1} based on an baseExpression representing it
 * (current only boolean formulas on the literals of a given constraintTheory are supported).
 * Note that this expressions or its sub-expressions can be implementations of {@link Constraint1} themselves,
 * and when this is the case, this implementation exploits their efficient internal representations for its own purposes.
 * 
 * @author braz
 *
 */
@SuppressWarnings("serial")
public class ExpressionConstraint extends AbstractConstraint {

	private Expression baseExpression;
	private Collection<Expression> supportedIndices;
	private ConstraintTheory theory;
	
	private ExpressionConstraint(ConstraintTheory constraintTheory, Collection<Expression> supportedIndices, Expression expression) {
		Util.myAssert(() -> expression != null, () -> getClass().getSimpleName() + " cannot wrap a null value.");
		this.baseExpression = expression instanceof ExpressionConstraint? ((ExpressionConstraint) expression).baseExpression : expression;
		this.supportedIndices = supportedIndices;
		this.theory = constraintTheory;
	}
	
	/**
	 * A "constructor" of a {@link Constraint1} based on an {@link Expression}.
	 * The reason this is not a regular Constraint is that if baseExpression is already a {@link Constraint1},
	 * it is directly returned, instead of wrapped in a new instance.
	 * In that case, no check is performed to see if the given constraintTheory and supported indices are
	 * the same as baseExpression's, but they should be.
	 * @param constraintTheory
	 * @param supportedIndices
	 * @param baseExpression
	 * @return
	 */
	public static Constraint1 wrap(ConstraintTheory theory, Collection<Expression> supportedIndices, Expression expression) {
		myAssert(() -> expression != null, () -> "ExpressionConstraint cannot wrap a null value.");
		Constraint1 result;
		if (expression instanceof Constraint1) {
			result = (Constraint1) expression;
		}
		else {
			result = new ExpressionConstraint(theory, supportedIndices, expression);
		}
		return result;
	}
	
	/**
	 * If baseExpression is {@link Constraint1}, return it; otherwise, wrap it in {@link ExpressionConstraint} and return that.
	 * @param baseExpression
	 * @return
	 */
	public Constraint1 wrap(Expression expression) {
		Constraint1 result = wrap(theory, supportedIndices, expression);
		return result;
	}

	@Override
	public ExpressionConstraint clone() {
		return new ExpressionConstraint(theory, supportedIndices, baseExpression);
	}

	@Override
	public ConstraintTheory getConstraintTheory() {
		return theory;
	}

	@Override
	protected Expression computeInnerExpression() {
		return baseExpression;
	}

	@Override
	public Collection<Expression> getSupportedIndices() {
		return Collections.unmodifiableCollection(supportedIndices);
	}

	@Override
	public Constraint1 incorporate(boolean splitterSign, Expression splitter, RewritingProcess process) {
		Constraint1 result;
		Expression splitterIfAny;
		if (baseExpression instanceof Constraint1) {
			result = ((Constraint1) baseExpression).incorporate(splitterSign, splitter, process);
		}
		else if (baseExpression.equals(FALSE)) {
			result = wrap(FALSE);
		}
		else if (baseExpression.equals(TRUE)) {
			Constraint1 newConstraint = theory.makeConstraint(supportedIndices);
			result = newConstraint.incorporate(splitterSign, splitter, process);
		}
		else if ((splitterIfAny = theory.makeSplitterIfPossible(baseExpression, supportedIndices, process)) != null) {
			Constraint1 newConstraint = theory.makeConstraint(supportedIndices);
			newConstraint.incorporate(true, splitterIfAny, process);
			result = newConstraint.incorporate(splitterSign, splitter, process);
		}
		else { // only acceptable leaves are boolean constants and splitters and those have already been tested,
			   // so at this point this object must be the application of a boolean connective,
			   // and therefore we apply the splitter to its arguments and apply the same connective on the updated arguments.
			Util.myAssert(() -> FormulaUtil.functorIsALogicalConnectiveIncludingConditionals(baseExpression), () -> "Only boolean formulas on constraintTheory literals supported by " + getClass());
			result = wrap(applyJavaFunctionToArgumentsAndReAssembleFunctionApplication(
					subExpression -> wrap(subExpression).incorporate(splitterSign, splitter, process),
					baseExpression));
		}
		return result;
	}

	@Override
	public Expression pickSplitter(Collection<Expression> indicesSubSet, RewritingProcess process) {
		Expression result;
		Expression splitterIfAny;
		if (baseExpression instanceof Constraint1) {
			result = ((Constraint1) baseExpression).pickSplitter(indicesSubSet, process);
		}
		else if (baseExpression.equals(FALSE) || baseExpression.equals(TRUE)) {
			result = null;
		}
		else if ((splitterIfAny = theory.makeSplitterIfPossible(baseExpression, supportedIndices, process)) != null) {
			result = splitterIfAny;
		}
		else { // only acceptable leaves are boolean constants and splitters, so at this point it must be a boolean connective.
			Util.myAssert(() -> FormulaUtil.functorIsALogicalConnectiveIncludingConditionals(baseExpression), () -> "Only boolean formulas on constraintTheory literals supported by " + getClass());
			result = getFirstNonNullResultOrNull(baseExpression.getArguments(), subExpression -> wrap(subExpression).pickSplitter(indicesSubSet, process));
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
			cachedSolver = new SGDPLLT(new DefaultInputTheory(theory), new ModelCounting()); 
		}
		return cachedSolver;
	}

	@Override
	public Expression normalizeSplitterGivenConstraint(Expression splitter, RewritingProcess process) {
		throw new Error("ExpressionConstraint.normalizeSplitterGivenConstraint not implemented yet -- and it is going to be expensive. Are you sure that's what you need?");
	}

	@Override
	public Expression normalizeExpressionWithoutLiterals(Expression expression, RewritingProcess process) {
		throw new Error("ExpressionConstraint.normalize not implemented yet -- and it is going to be expensive. Are you sure that's what you need?");
	}
}