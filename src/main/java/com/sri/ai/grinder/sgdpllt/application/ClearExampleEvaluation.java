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
package com.sri.ai.grinder.sgdpllt.application;

import static com.sri.ai.expresso.helper.Expressions.apply;
import static com.sri.ai.expresso.helper.Expressions.makeSymbol;
import static com.sri.ai.expresso.helper.Expressions.parse;
import static com.sri.ai.grinder.helper.GrinderUtil.BOOLEAN_TYPE;
import static com.sri.ai.grinder.sgdpllt.library.FunctorConstants.IN;
import static com.sri.ai.grinder.sgdpllt.library.FunctorConstants.SUM;
import static com.sri.ai.util.Util.println;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.IndexExpressionsSet;
import com.sri.ai.expresso.api.IntensionalSet;
import com.sri.ai.expresso.core.ExtensionalIndexExpressionsSet;
import com.sri.ai.grinder.sgdpllt.api.Context;
import com.sri.ai.grinder.sgdpllt.api.Theory;
import com.sri.ai.grinder.sgdpllt.core.TrueContext;
// import com.sri.ai.grinder.sgdpllt.library.set.extensional.ExtensionalSet;
import com.sri.ai.grinder.sgdpllt.theory.compound.CompoundTheory;
import com.sri.ai.grinder.sgdpllt.theory.differencearithmetic.DifferenceArithmeticTheory;
import com.sri.ai.grinder.sgdpllt.theory.equality.EqualityTheory;
import com.sri.ai.grinder.sgdpllt.theory.linearrealarithmetic.LinearRealArithmeticTheory;
import com.sri.ai.grinder.sgdpllt.theory.propositional.PropositionalTheory;
import com.sri.ai.grinder.sgdpllt.theory.tuple.TupleTheory;

/**
 * A collection of examples on how to use the Expresso API.
 * @author braz
 *
 */
public class ClearExampleEvaluation {
	
	
	public static void main(String[] args) {

		///// Evaluating expressions
		
		// The above code shows how to deal with the syntax of expressions.
		// Evaluating expressions requires knowing about the semantics, that is, to what functions each operator corresponds to ("+" to addition, etc).
		// This is provided by a theory, which for now it suffices to know is a collection of methods for evaluating expressions
		// according to an interpretation to some symbols.
		
		Theory theory = new CompoundTheory(
				new EqualityTheory(false, true),
				new DifferenceArithmeticTheory(false, false),
				new LinearRealArithmeticTheory(false, false),
				new TupleTheory(),
				new PropositionalTheory());
		
		// Because this evaluation is symbolic, evaluated expressions may involve free variables.
		// In this case, the result of the evaluation will be a simplified expression that
		// is equivalent to the original expression for all possible assignments to the free variables.
		// For example, X + 0*Y is evaluate to X because, for any assignment to (X,Y), X + 0*Y = X.

		Context context = new TrueContext(theory); // true context: all assignments to free variables are of interest
		// We will later see how we can use contexts that restrict the free variable assignments of interest.
		context = context.makeCloneWithAddedType(BOOLEAN_TYPE);
		context=context.extendWithSymbolsAndTypes("B", "Integer");
		context=context.extendWithSymbolsAndTypes("J", "Integer");
		
		// Now that we have a theory and a context, we can evaluate expressions:
		println("1 + 0*X + 1  =  " + theory.evaluate(parse("1 + 1"), context));
		
		/*evaluate(new String[] {
				"sum({{ (on C in Boolean) (if C then if A then 50 else 50 else if A then 50 else 50) * (if C then if B then 60 else 40 else if B then 40 else 60) }})", "",
		}, theory, context);
		*/
		
		
		Expression test = theory.evaluate(parse("sum({{ (on C in Boolean) (if C then if A then 50 else 50 else if A then 50 else 50) * (if C then if B then 60 else 40 else if B then 40 else 60) }})"), context);
		println(test);
		
		String str = "sum({{ (on I in 1..10) I : I != J }})";
		Expression expr = parse(str);
		Expression test2 = theory.evaluate(expr, context);
		println(test2);
		

		// Here's how to do it from scratch, but see next the way we typically actually do it.
		Expression p = makeSymbol("P");
		context=context.extendWithSymbolsAndTypes("P", "Integer");
		IndexExpressionsSet indices = new ExtensionalIndexExpressionsSet(apply(IN, p, parse("1..4")));
		
		
		println("plop");
		// The "extensional" in ExtensionalIndexExpressionsSet means that the list/set of indices is extensionally defined,
		// even though they will be the indices of an intensionally defined set.
		Expression intensionalUniSet = 
				IntensionalSet.makeMultiSet( // IntensionalSet.intensionalUniSet, or simply intensionalUniSet, also works
						indices, parse("5"), parse("true")); 
		// Note that Equality.make(p, "Rodrigo") is the same as apply(FunctorConstants.EQUAL, p, "Rodrigo").
		// We often have 'make' methods for many operators: And.make, Or.make and so on.
		// packages in com.sri.ai.expresso.grinder.sgdpllt.library have many such operator-specific classes.
		println(intensionalUniSet);
		Expression sum =apply(SUM, intensionalUniSet);
		println(sum);
		Expression resultat = theory.evaluate(sum, context);
		println(resultat);
		
		
		
	}
	
	public static void evaluate(String[] inputsAndOutputs, Theory theory, Context context) {
		for (int i = 0; i != inputsAndOutputs.length/2; i++) {
			String input = inputsAndOutputs[2*i];
			String expected = inputsAndOutputs[2*i + 1];
			Expression output = theory.evaluate(parse(input), context);
			println(input + "  =  " + output);
			if ( ! output.equals(parse(expected))) {
				println("Error: " + input + " should have been evaluated to " + expected + ", but was evaluated to " + output);
			}
		}
	}
}
