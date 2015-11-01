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
package com.sri.ai.grinder.interpreter;

import static com.sri.ai.expresso.helper.Expressions.FALSE;
import static com.sri.ai.expresso.helper.Expressions.TRUE;

import java.util.Map;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.api.MapBasedSimplifier;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.api.Simplifier;
import com.sri.ai.grinder.api.SimplifierUnderContextualConstraint;
import com.sri.ai.grinder.core.simplifier.Exhaustive;
import com.sri.ai.grinder.core.simplifier.MergedMapBasedSimplifier;
import com.sri.ai.grinder.core.simplifier.Recursive;
import com.sri.ai.grinder.sgdpll2.api.Constraint2;
import com.sri.ai.grinder.sgdpll2.api.ConstraintTheory;
import com.sri.ai.grinder.sgdpll2.core.constraint.CompleteMultiVariableConstraint;
import com.sri.ai.grinder.sgdpll2.core.constraint.ConstraintSplitting;

/**
 * A {@link MapBasedSimplifier} able (optionally) to simplify expressions based
 * a contextual constraint stored in {@link RewritingProcess}'s global object under key
 * {@link #INTERPRETER_CONTEXTUAL_CONSTRAINT}.
 * <p>
 * This simplifier is based on the elementary simplifiers provided by abstract methods {@link #makeFunctionApplicationSimplifiers()},
 * the ones provided by {@link #makeSyntacticFormTypeSimplifiers()}, and another {@link MapBasedSimplifier}
 * provided by abstract method {@link #makeAnotherMapBasedSimplifier()}.
 * The simplifiers in the latter are overridden by the simplifiers in the first two to create
 * an overriding effect.
 *
 * @author braz
 *
 */
@Beta
public abstract class AbstractInterpreter implements MapBasedSimplifier, SimplifierUnderContextualConstraint {

	public static final String INTERPRETER_CONTEXTUAL_CONSTRAINT = "sgdpll2 contextual constraint";
	
	private MapBasedSimplifier basicSimplifier;
	private Simplifier simplifier;
	protected boolean simplifyGivenConstraint;
	protected ConstraintTheory constraintTheory;
	private Constraint2 trueConstraint;
	
	/**
	 * Constructs {@link AbstractInterpreter} with 
	 * <i>not</i> simplifying literals according to contextual constraint.
	 */
	public AbstractInterpreter(ConstraintTheory constraintTheory) {
		this(constraintTheory, false);
	}

	/**
	 * Constructs {@link AbstractInterpreter} that
	 * simplifies literals according to contextual constraint stored in
	 * <code>process</code>'s global object under {@link #INTERPRETER_CONTEXTUAL_CONSTRAINT}.
	 * @param simplifyGivenConstraint
	 */
	public AbstractInterpreter(ConstraintTheory constraintTheory, boolean simplifyGivenConstraint) {
		this.basicSimplifier = new InternalSimplifier();
		this.simplifier = new Recursive(new Exhaustive(this.basicSimplifier));
		this.simplifyGivenConstraint = simplifyGivenConstraint;
		this.constraintTheory = constraintTheory;
		this.trueConstraint = new CompleteMultiVariableConstraint(constraintTheory);
	}

	@Override
	public Map<String, Simplifier> getFunctionApplicationSimplifiers() {
		return basicSimplifier.getFunctionApplicationSimplifiers();
	}

	@Override
	public Map<String, Simplifier> getSyntacticFormTypeSimplifiers() {
		return basicSimplifier.getSyntacticFormTypeSimplifiers();
	}

	private class InternalSimplifier extends MergedMapBasedSimplifier {

		// The reason we use an internal simplifier as opposed to making {@link AbstractInterpreter} an extension of {@link MergedMapBasedSimplifier}
		// is that we want the makers of sub-simplifiers to be dynamic (non-static), so this class can be extended and they can be overridden.
		
		public InternalSimplifier() {
			super(makeFunctionApplicationSimplifiers(), makeSyntacticFormTypeSimplifiers(), makeAnotherMapBasedSimplifier());
		}

		@Override
		public Expression apply(Expression expression, RewritingProcess process) {
			if (simplifyGivenConstraint) {
				Constraint2 contextualConstraint = (Constraint2) process.getGlobalObject(INTERPRETER_CONTEXTUAL_CONSTRAINT);
				if (contextualConstraint == null) {
					contextualConstraint = trueConstraint;
				}
				if (contextualConstraint.getConstraintTheory().isLiteral(expression, process)) {
					ConstraintSplitting split = new ConstraintSplitting(contextualConstraint, expression, process);
					switch(split.getResult()) {
					case CONSTRAINT_IS_CONTRADICTORY:
						return null;
					case LITERAL_IS_TRUE:
						return TRUE;
					case LITERAL_IS_FALSE:
						return FALSE;
					case LITERAL_IS_UNDEFINED:
					default:
						break;
					}
				}
			}
			
			Expression result = super.apply(expression, process);

			return result;
		}
		
	}
	
	@Override
	public Expression apply(Expression expression, RewritingProcess process) {
		Expression result = simplifier.apply(expression, process);
		return result;
	}

	abstract public Map<String, Simplifier> makeFunctionApplicationSimplifiers();

	abstract public Map<String, Simplifier> makeSyntacticFormTypeSimplifiers();

	abstract public MapBasedSimplifier makeAnotherMapBasedSimplifier();

	/**
	 * Simplifies a given expression using enumeration under given contextual constraint.
	 * @param expression
	 * @param contextualConstraint
	 * @param process
	 * @return
	 */
	@Override
	public Expression simplifyUnderContextualConstraint(Expression expression, Constraint2 contextualConstraint, RewritingProcess process) {
		Object oldConstraint = process.getGlobalObject(INTERPRETER_CONTEXTUAL_CONSTRAINT);
		process.putGlobalObject(INTERPRETER_CONTEXTUAL_CONSTRAINT, contextualConstraint);
		Expression simplifiedBody = apply(expression, process);
		if (oldConstraint != null) {
			process.putGlobalObject(INTERPRETER_CONTEXTUAL_CONSTRAINT, oldConstraint);
		}
		return simplifiedBody;
	}
}