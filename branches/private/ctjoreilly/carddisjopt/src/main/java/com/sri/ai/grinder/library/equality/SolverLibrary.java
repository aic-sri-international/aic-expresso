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
package com.sri.ai.grinder.library.equality;

import com.google.common.annotations.Beta;
import com.google.common.collect.Lists;
import com.sri.ai.expresso.helper.ExpressionKnowledgeModule;
import com.sri.ai.grinder.api.Rewriter;
import com.sri.ai.grinder.core.DefaultLibrary;
import com.sri.ai.grinder.library.AbsorbingElement;
import com.sri.ai.grinder.library.Associative;
import com.sri.ai.grinder.library.Disequality;
import com.sri.ai.grinder.library.Equality;
import com.sri.ai.grinder.library.ScopedVariables;
import com.sri.ai.grinder.library.SyntacticFunctionsSubExpressionsProvider;
import com.sri.ai.grinder.library.boole.And;
import com.sri.ai.grinder.library.boole.ContradictoryConjuncts;
import com.sri.ai.grinder.library.boole.Equivalence;
import com.sri.ai.grinder.library.boole.ForAllSubExpressionsAndScopedVariablesProvider;
import com.sri.ai.grinder.library.boole.Implication;
import com.sri.ai.grinder.library.boole.Not;
import com.sri.ai.grinder.library.boole.Or;
import com.sri.ai.grinder.library.boole.ThereExistsSubExpressionsAndScopedVariablesProvider;
import com.sri.ai.grinder.library.controlflow.IfThenElseConditionIsTrueInThenBranchAndFalseInElseBranch;
import com.sri.ai.grinder.library.controlflow.IfThenElseExternalization;
import com.sri.ai.grinder.library.controlflow.IfThenElseIrrelevantCondition;
import com.sri.ai.grinder.library.controlflow.IfThenElseSubExpressionsAndImposedConditionsProvider;
import com.sri.ai.grinder.library.controlflow.ImposedConditionsModule;
import com.sri.ai.grinder.library.number.NotOnGreaterThan;
import com.sri.ai.grinder.library.number.NotOnGreaterThanOrEqualTo;
import com.sri.ai.grinder.library.number.NotOnLessThan;
import com.sri.ai.grinder.library.number.NotOnLessThanOrEqualTo;

/**
 * 
 * @author braz
 *
 */
@Beta
public class SolverLibrary extends DefaultLibrary {
	private static final long serialVersionUID = 1L;

	public SolverLibrary() {
		super(Lists.newArrayList((Rewriter)
				new Equality(),
				new Disequality(),

				new And(),
				new Or(),
				new Not(),
				new ContradictoryConjuncts(),
				new Implication(),
				new Equivalence(),
				new AbsorbingElement(
						"and", "false",
						"or", "true"
				),
				new Associative("and"),
				new NotOnEquality(),
				new NotOnDisequality(),
				new NotOnGreaterThan(),
				new NotOnLessThan(),
				new NotOnLessThanOrEqualTo(),
				new NotOnGreaterThanOrEqualTo(),

				new ScopedVariables(),

				new IfThenElseIrrelevantCondition(),
				new IfThenElseConditionIsTrueInThenBranchAndFalseInElseBranch(),
				
				new IfThenElseExternalization(),

				// only modules from here on: they don't actually rewrite anything, so why test them sooner than needed?
				new ExpressionKnowledgeModule(),
				new ImposedConditionsModule(),
				new IfThenElseSubExpressionsAndImposedConditionsProvider(),
				new ForAllSubExpressionsAndScopedVariablesProvider(),
				new ThereExistsSubExpressionsAndScopedVariablesProvider(),
				new SyntacticFunctionsSubExpressionsProvider("type", "scoped variables")
		));
	}
}
