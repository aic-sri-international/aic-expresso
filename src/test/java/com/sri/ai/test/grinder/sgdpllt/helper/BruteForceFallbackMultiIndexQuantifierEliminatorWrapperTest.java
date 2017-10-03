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
package com.sri.ai.test.grinder.sgdpllt.helper;

import static com.sri.ai.expresso.helper.Expressions.parse;
import static com.sri.ai.util.Util.println;

import org.junit.Test;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.sgdpllt.api.Context;
import com.sri.ai.grinder.sgdpllt.api.ExpressionLiteralSplitterStepSolver;
import com.sri.ai.grinder.sgdpllt.api.MultiIndexQuantifierEliminator;
import com.sri.ai.grinder.sgdpllt.api.SingleVariableConstraint;
import com.sri.ai.grinder.sgdpllt.core.TrueContext;
import com.sri.ai.grinder.sgdpllt.core.solver.AbstractMultiIndexQuantifierEliminatorBasedOnSingleIndexQuantifierEliminator;
import com.sri.ai.grinder.sgdpllt.core.solver.AbstractQuantifierEliminationStepSolver;
import com.sri.ai.grinder.sgdpllt.core.solver.ExpressionStepSolverToLiteralSplitterStepSolverAdapter;
import com.sri.ai.grinder.sgdpllt.core.solver.QuantifierEliminationStepSolver;
import com.sri.ai.grinder.sgdpllt.group.AssociativeCommutativeGroup;
import com.sri.ai.grinder.sgdpllt.helper.BruteForceFallbackMultiIndexQuantifierEliminatorWrapper;
import com.sri.ai.grinder.sgdpllt.library.boole.ForAllRewriter;
import com.sri.ai.grinder.sgdpllt.library.boole.ThereExistsRewriter;
import com.sri.ai.grinder.sgdpllt.library.number.MaxRewriter;
import com.sri.ai.grinder.sgdpllt.library.number.ProductRewriter;
import com.sri.ai.grinder.sgdpllt.library.number.SummationRewriter;
import com.sri.ai.grinder.sgdpllt.library.set.CardinalityTopRewriter;
import com.sri.ai.grinder.sgdpllt.rewriter.api.Rewriter;
import com.sri.ai.grinder.sgdpllt.rewriter.core.CombiningTopRewriter;

@Beta
public class BruteForceFallbackMultiIndexQuantifierEliminatorWrapperTest {

	private static class DummyQuantifierEliminatorStepSolver extends AbstractQuantifierEliminationStepSolver {

		public DummyQuantifierEliminatorStepSolver(AssociativeCommutativeGroup group, SingleVariableConstraint indexConstraint, Expression body) {
			super(group, indexConstraint, body);
		}

		@Override
		protected Step eliminateQuantifierForLiteralFreeBodyAndSingleVariableConstraint(SingleVariableConstraint indexConstraint, Expression literalFreeBody, Context context) {
			throw new IllegalArgumentException();
		}

		@Override
		protected AbstractQuantifierEliminationStepSolver makeWithNewIndexConstraint(SingleVariableConstraint newIndexConstraint) {
			return new DummyQuantifierEliminatorStepSolver(group, newIndexConstraint, body);
		}
	}
	
	//@Test
	public void test() {
		MultiIndexQuantifierEliminator multiIndexQuantifierEliminator =
				new AbstractMultiIndexQuantifierEliminatorBasedOnSingleIndexQuantifierEliminator() {

			@Override
			protected ExpressionLiteralSplitterStepSolver getQuantifierEliminatorStepSolver(AssociativeCommutativeGroup group, SingleVariableConstraint indexConstraint, Expression body, Context context) {

				QuantifierEliminationStepSolver stepSolver = 
						new DummyQuantifierEliminatorStepSolver(group, indexConstraint, body);
				
				stepSolver = new BruteForceFallbackMultiIndexQuantifierEliminatorWrapper(stepSolver);
				
				ExpressionStepSolverToLiteralSplitterStepSolverAdapter result = 
						new ExpressionStepSolverToLiteralSplitterStepSolverAdapter(stepSolver);
				
				return result;
			}
		};

		Rewriter symbolicQuantifierEliminators = 
				new CombiningTopRewriter(
						new SummationRewriter(multiIndexQuantifierEliminator)
						,
						new ProductRewriter(multiIndexQuantifierEliminator)
						,
						new MaxRewriter(multiIndexQuantifierEliminator)
						,
						new CardinalityTopRewriter(multiIndexQuantifierEliminator)
						,
						new ForAllRewriter(multiIndexQuantifierEliminator)
						,
						new ThereExistsRewriter(multiIndexQuantifierEliminator)
						);
		
		println(symbolicQuantifierEliminators.apply(parse("sum({{(on I in 1..4) I}})"), new TrueContext()));
	}
}