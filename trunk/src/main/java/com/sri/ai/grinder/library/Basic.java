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
package com.sri.ai.grinder.library;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import com.google.common.annotations.Beta;
import com.sri.ai.grinder.api.Rewriter;
import com.sri.ai.grinder.core.OpenInterpretationModule;
import com.sri.ai.grinder.core.TotalRewriter;
import com.sri.ai.grinder.library.boole.And;
import com.sri.ai.grinder.library.boole.ContradictoryConjuncts;
import com.sri.ai.grinder.library.boole.Equivalence;
import com.sri.ai.grinder.library.boole.Implication;
import com.sri.ai.grinder.library.boole.Not;
import com.sri.ai.grinder.library.boole.Or;
import com.sri.ai.grinder.library.controlflow.IfThenElse;
import com.sri.ai.grinder.library.controlflow.IfThenElseConditionIsTrueInThenBranchAndFalseInElseBranch;
import com.sri.ai.grinder.library.controlflow.IfThenElseExternalization;
import com.sri.ai.grinder.library.controlflow.IfThenElseIrrelevantCondition;
import com.sri.ai.grinder.library.controlflow.IfThenElseSubExpressionsAndImposedConditionsProvider;
import com.sri.ai.grinder.library.controlflow.ImposedConditionsModule;
import com.sri.ai.grinder.library.equality.NotOnDisequality;
import com.sri.ai.grinder.library.equality.NotOnEquality;
import com.sri.ai.grinder.library.equality.injective.DisequalityOnInjectiveSubExpressions;
import com.sri.ai.grinder.library.equality.injective.EqualityOnInjectiveSubExpressions;
import com.sri.ai.grinder.library.equality.injective.EqualityOnMutuallyExclusiveCoDomainExpressions;
import com.sri.ai.grinder.library.function.InjectiveModule;
import com.sri.ai.grinder.library.function.MutuallyExclusiveCoDomainsModule;
import com.sri.ai.grinder.library.function.SymmetricModule;
import com.sri.ai.grinder.library.number.Division;
import com.sri.ai.grinder.library.number.Exponentiation;
import com.sri.ai.grinder.library.number.Minus;
import com.sri.ai.grinder.library.number.NotOnGreaterThan;
import com.sri.ai.grinder.library.number.NotOnGreaterThanOrEqualTo;
import com.sri.ai.grinder.library.number.NotOnLessThan;
import com.sri.ai.grinder.library.number.NotOnLessThanOrEqualTo;
import com.sri.ai.grinder.library.number.Plus;
import com.sri.ai.grinder.library.number.Times;
import com.sri.ai.grinder.library.number.UnaryMinus;
import com.sri.ai.grinder.library.set.Partition;
import com.sri.ai.grinder.library.set.extensional.ProductOnExtensionalSet;
import com.sri.ai.grinder.library.set.extensional.UnionOnExtensionalSets;
import com.sri.ai.grinder.library.set.intensional.IntensionalSet;
import com.sri.ai.grinder.library.set.tuple.Tuple;

/**
 * <pre>
 * R_basic(E)
 * E is an expression involving basic numeric and boolean operators, variables and function applications
 * Returns a conditional numeric and boolean expression
 * Externalizes conditionals
 * Replaces function applications of numeric operations on actual numbers by its result
 * Replaces function applications of boolean operations on actual boolean values by its result
 * For each subexpression E' of E
 * Replaces 0*E' by 0, 1*E' by 1, 0+E' by E', 0^E' by 0, E'^0 by 1, E'^1 by E', E'/1 by E', --E' by E', E'-0 by E', 0-E' by -E' 
 * Replaces false and E' by false, true and E' by E', false or E' by E', true or E' by true, not not E' by E', not true by false, not false by true.
 * Replaces if true then E' else E'' by E', if false then E' else E'' by E'', if C then E' else E' by E'
 * </pre>
 * 
 * @author braz
 */
@Beta
public class Basic extends TotalRewriter {

	public Basic() {
		this(Basic.getDefaultRewriters());
	}

	public Basic(List<Rewriter> rewriters) {
		super("Basic", rewriters);
	}

	public static List<Rewriter> getDefaultRewriters() {
		return new ArrayList<Rewriter>(
				Arrays.asList(new Rewriter[] {
						new PlainSubstitution(),
						new Plus(),
						new Division(),
						new Minus(),
						new UnaryMinus(),
						new Times(),
						new IfThenElse(),
						new ProductOnExtensionalSet(),
						new IntensionalSet(),
						new EqualityOnInjectiveSubExpressions(), // these two
																	// are best
																	// before
																	// ExtensionalSet,
																	// so
																	// normalization
																	// of
																	// Extensional
																	// sets does
																	// not
																	// expand
																	// completely
																	// before
																	// equalities
																	// and
																	// disequalities
																	// are taken
																	// into
																	// account
						new DisequalityOnInjectiveSubExpressions(),
						new UnionOnExtensionalSets(),
						new Partition(),

						new Exponentiation(),

						new Equality(), // placing this ahead of boolean
						// connectives so that cheaper equality
						// constraint applier is used first.
						new Disequality(),

						new And(),
						new Or(),
						new Not(),
						new ContradictoryConjuncts(),
						new Implication(),
						new Equivalence(),
						new AbsorbingElement("and", "false", "or", "true", "*",
								"0"),
						new Associative("+", "*", "and"),
						new NotOnEquality(),
						new NotOnDisequality(),
						new NotOnGreaterThan(),
						new NotOnLessThan(),
						new NotOnLessThanOrEqualTo(),
						new NotOnGreaterThanOrEqualTo(),

						new IfThenElseIrrelevantCondition(),
						new IfThenElseConditionIsTrueInThenBranchAndFalseInElseBranch(),

						new IfThenElseExternalization(),

						new EqualityOnMutuallyExclusiveCoDomainExpressions(),

						// only modules from here on: they don't actually
						// rewrite anything, so why test them sooner than
						// needed?
						new ImposedConditionsModule(),
						new InjectiveModule(),
						new SymmetricModule(),
						new MutuallyExclusiveCoDomainsModule(),
						new IfThenElseSubExpressionsAndImposedConditionsProvider(),
						new Tuple(),
						new SyntacticFunctionsSubExpressionsProvider("type"),
						new OpenInterpretationModule() }));
	}
}
