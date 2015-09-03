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
package com.sri.ai.grinder.sgdpll2.core;

import static com.sri.ai.expresso.helper.Expressions.parse;

import com.google.common.annotations.Beta;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.core.DefaultRewritingProcess;
import com.sri.ai.grinder.sgdpll2.api.ConstraintTheory;
import com.sri.ai.grinder.sgdpll2.api.MultiVariableConstraint;
import com.sri.ai.grinder.sgdpll2.theory.equality.EqualityConstraintTheory;
import com.sri.ai.grinder.sgdpll2.theory.propositional.PropositionalConstraintTheory;

/**
 * A multi-variable constraint whose {@link #conjoin(com.sri.ai.expresso.api.Expression, RewritingProcess)}
 * is guaranteed to return <code>null</code> if it becomes unsatisfiable.
 * 
 * @author braz
 *
 */
@Beta
public class CompleteMultiVariableConstraint extends MultiVariableConstraintWithCheckedProperty {

	private static final long serialVersionUID = 1L;

	public CompleteMultiVariableConstraint(ConstraintTheory constraintTheory) {
		super(constraintTheory, constraintTheory.getMakerOfSatisfiabilityOfSingleVariableConstraintProblem());
	}

	public static void main(String[] args) {
		MultiVariableConstraint constraint;
		RewritingProcess process;
		
		constraint = new CompleteMultiVariableConstraint(new EqualityConstraintTheory());
		process = new DefaultRewritingProcess(null);

		System.out.println("constraint: " + constraint);	
		constraint = constraint.conjoin(parse("Z != Y"), process);
		System.out.println("constraint: " + constraint);	
		constraint = constraint.conjoin(parse("W != Z"), process);
		System.out.println("constraint: " + constraint);	
		constraint = constraint.conjoin(parse("X = Y"), process);
		System.out.println("constraint: " + constraint);	
		constraint = constraint.conjoin(parse("X = W"), process);
		System.out.println("constraint: " + constraint);	
		constraint = constraint.conjoin(parse("X = Z"), process);
		System.out.println("constraint: " + constraint);	

		constraint = new CompleteMultiVariableConstraint(new PropositionalConstraintTheory());
		process = new DefaultRewritingProcess(null);

		System.out.println("constraint: " + constraint);	
		constraint = constraint.conjoin(parse("P"), process);
		System.out.println("constraint: " + constraint);	
		constraint = constraint.conjoin(parse("Q"), process);
		System.out.println("constraint: " + constraint);	
		constraint = constraint.conjoin(parse("not R"), process);
		System.out.println("constraint: " + constraint);	
		constraint = constraint.conjoin(parse("not S"), process);
		System.out.println("constraint: " + constraint);	
		constraint = constraint.conjoin(parse("not P"), process);
		System.out.println("constraint: " + constraint);	
	
	}
}