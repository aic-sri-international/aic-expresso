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
package com.sri.ai.grinder.sgdpll.application;

import static com.sri.ai.expresso.helper.Expressions.makeSymbol;
import static com.sri.ai.expresso.helper.Expressions.parse;
import static com.sri.ai.grinder.helper.GrinderUtil.BOOLEAN_TYPE;
import static com.sri.ai.grinder.sgdpll.core.solver.ContextDependentExpressionProblemSolver.solve;
import static com.sri.ai.util.Util.join;
import static com.sri.ai.util.Util.list;
import static com.sri.ai.util.Util.map;
import static com.sri.ai.util.Util.mapIntoList;

import java.util.Collection;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.type.Categorical;
import com.sri.ai.expresso.type.IntegerInterval;
import com.sri.ai.grinder.api.Context;
import com.sri.ai.grinder.core.TypeContext;
import com.sri.ai.grinder.sgdpll.api.ConstraintTheory;
import com.sri.ai.grinder.sgdpll.core.solver.EvaluatorStepSolver;
import com.sri.ai.grinder.sgdpll.interpreter.SymbolicCommonInterpreter;
import com.sri.ai.grinder.sgdpll.simplifier.api.Simplifier;
import com.sri.ai.grinder.sgdpll.theory.compound.CompoundConstraintTheory;
import com.sri.ai.grinder.sgdpll.theory.equality.EqualityConstraintTheory;
import com.sri.ai.grinder.sgdpll.theory.inequality.InequalityConstraintTheory;
import com.sri.ai.grinder.sgdpll.theory.propositional.PropositionalConstraintTheory;
import com.sri.ai.util.console.ConsoleIterator;
import com.sri.ai.util.console.DefaultConsoleIterator;

/**
 * Provides a shell for use of {@link SymbolicCommonInterpreter}.
 * 
 * @author braz
 *
 */
@Beta
public class SymbolicShell {

	private static boolean debug = false;
	
	public static void main(String[] args) {

		CompoundConstraintTheory constraintTheory = new CompoundConstraintTheory(
				new EqualityConstraintTheory(false, true),
				new InequalityConstraintTheory(false, false),
				new PropositionalConstraintTheory());
		Simplifier evaluator = makeEvaluator(constraintTheory);
		
		Context context = new TypeContext(constraintTheory);
		context = context.add(BOOLEAN_TYPE);
		context = context.add(new Categorical("People",  1000000, makeSymbol("ann"), makeSymbol("bob"), makeSymbol("ciaran")));
		context = context.add(new IntegerInterval("Integer"));
		
		context = context.registerIndicesAndTypes(map(makeSymbol("P"), makeSymbol("Boolean")));
		context = context.registerIndicesAndTypes(map(makeSymbol("Q"), makeSymbol("Boolean")));
		context = context.registerIndicesAndTypes(map(makeSymbol("R"), makeSymbol("Boolean")));
		context = context.registerIndicesAndTypes(map(makeSymbol("S"), makeSymbol("Boolean")));

		context = context.registerIndicesAndTypes(map(makeSymbol("X"), makeSymbol("People")));
		context = context.registerIndicesAndTypes(map(makeSymbol("Y"), makeSymbol("People")));
		context = context.registerIndicesAndTypes(map(makeSymbol("Z"), makeSymbol("People")));

		context = context.registerIndicesAndTypes(map(makeSymbol("I"), makeSymbol("Integer")));
		context = context.registerIndicesAndTypes(map(makeSymbol("J"), makeSymbol("Integer")));
		context = context.registerIndicesAndTypes(map(makeSymbol("K"), makeSymbol("Integer")));
		
		help();

		ConsoleIterator consoleIterator = new DefaultConsoleIterator();
		
		Collection<String> examples = list(
				"sum({{ (on X in People)  3 }})",
				"sum({{ (on X in People)  3 |  X != Y }})",
				"product({{ (on X in People)  3 |  X != Y }})",
				"| {{ (on X in People)  3 |  X != Y }} |",
				"max({{ (on X in People)  3 |  X != Y }})",
				"sum({{ (on X in People, Y in People)  3 |  X != Y }})",
				"sum({{ (on X in People)  3 |  X != Y and X != ann }})",
				"sum({{ (on X in People, P in Boolean)  3 |  X != ann }})",
				"sum({{ (on X in People, P in Boolean)  3 |  X != ann and not P }})",
				"sum({{ (on X in People, Y in People)  if X = ann and Y != bob then 2 else 0  |  for all Z in People : Z = ann => X = Z }})"
				
				, "sum({{ (on I in 1..100)  I }})"
				, "sum({{ (on I in 1..100)  I | I != 3 and I != 5 and I != 500 }})"
				, "sum({{ (on I in 1..100)  I | I != J and I != 5 and I != 500 }})"
				, "sum({{ (on I in 1..100)  (I - J)^2 }})"
				, "sum({{ (on I in 1..100)  if I != K then (I - J)^2 else 0 }})"
				
				);
		for (String example : examples) {
			System.out.println(consoleIterator.getPrompt() + example);
			evaluate(evaluator, example, context);
			System.out.println("\n\n\n\n\n");
		}

		while (consoleIterator.hasNext()) {
			String input = consoleIterator.next();
			if (input.equals("")) {
				System.out.println();	
			}
			else if (input.startsWith("show")) {
				System.out.println("\n" +
						join(mapIntoList(context.getSymbolsAndTypes().entrySet(), e -> e.getKey() + ": " + e.getValue()), ", ") + "\n");	
			}
			else if (input.equals("debug")) {
				debug = !debug;
				System.out.println("\nDebug toggled to " + debug + "\n");	
			}
			else if (input.equals("help")) {
				help();
			}
			else {
				context = evaluate(evaluator, input, context);
			}
		}
		
		System.out.println("\nGood bye.");	
	}

	/**
	 * Makes a {@link Simplifier} based on a {@link EvaluatorStepSolver} for a given constraint theory.
	 * @param constraintTheory
	 * @return
	 */
	public static Simplifier makeEvaluator(ConstraintTheory constraintTheory) {
		return (e, c) -> solve(new EvaluatorStepSolver(e, constraintTheory.getTopSimplifier()), c);
	}

	/**
	 * @param evaluator
	 * @param inputString
	 * @param context
	 * @return 
	 */
	private static Context evaluate(Simplifier evaluator, String inputString, Context context) {
		
		try {
			Expression input = parse(inputString, (errorMessage) -> {throw new Error("Syntax error: " + errorMessage);});
			if (input.hasFunctor("var")) {
				Expression variable = input.get(0);
				Expression type = input.get(1);
				context = context.registerIndicesAndTypes(map(variable, type));
				System.out.println();	
				return context;
			}
			Expression result = evaluator.apply(input, context);
			System.out.println("\n" + result + "\n");
		} catch (Error e) {
			dealWith(e);
		} catch (Exception e) {
			dealWith(e);
		}
		return context;
	}

	private static void dealWith(Throwable e) {
		if (debug) {
			e.printStackTrace();
		}
		else {
			System.out.println("\n" + throwableMessage(e) + "\n");
		}
	}

	private static String throwableMessage(Throwable e) {
		String message;
		if (e.getMessage() == null || e.getMessage().equals("null")) {
			message = "Sorry, an error without a message occurred\n";
		}
		else {
			message = e.getMessage();
		}
		return message;
	}

	/**
	 * 
	 */
	private static void help() {
		System.out.println("***********************************************************************************");
		System.out.println("");
		System.out.println("Welcome to SRI AIC expresso symbolic interpreter");
		System.out.println("");
		System.out.println("Pre-defined types are:");
		System.out.println("- 'Boolean' with constants 'true' and 'false',");
		System.out.println("                 pre-defined variables P, Q, R, S");
		System.out.println("- 'Integer' with pre-defined variables I, J, K");
		System.out.println("- Integer intevals can be used in summations: sum({{(on I in 1..10) I}});");
		System.out.println("- 'People' with 1,000,000 elements and constants 'ann', 'bob', and 'ciaran',");
		System.out.println("                                       pre-defined variables X, Y, Z");
		System.out.println("");
		System.out.println("Capitalized symbols (other than types) are considered variables");
		System.out.println("");
		System.out.println("The language includes:");
		System.out.println("");
		System.out.println("- if-then-else");
		System.out.println("- equality (=, !=)");
		System.out.println("- boolean operators: 'and', 'or', 'not', '=>', '<=>'");
		System.out.println("- numeric operators: +, -, *, /, ^, <, >, <=, >=");
		System.out.println("");
		System.out.println("- universal and existential quantification:");
		System.out.println("- for all X in <Type> : <Formula>");
		System.out.println("- there exists X in <Type> : <Formula>");
		System.out.println("");
		System.out.println("- aggregates over intensionally-defined multi-sets:");
		System.out.println("-     sum({{ (on X in <Type>, Y in <Type>, ...)  <Number-valued> : <Condition> }})");
		System.out.println("- product({{ (on X in <Type>, Y in <Type>, ...)  <Number-valued> : <Condition> }})");
		System.out.println("-     max({{ (on X in <Type>, Y in <Type>, ...)  <Number-valued> : <Condition> }})");
		System.out.println("- the 'on' clause indicates the set indices; all other variables are free variables");
		System.out.println("  and the result may depend on them");
		System.out.println("");
		System.out.println("- cardinality over intensionally-defined multi-sets:");
		System.out.println("-      | ({{ (on X in <Type>, Y in <Type>, ...)  <Number-valued> : <Condition> }}) |");
		System.out.println("");
		System.out.println("Global inference only works on equality and propositions");
		System.out.println("This means the system knows P and (P => Q) implies Q,");
		System.out.println("and that X != Y and Y = Z implies X != Z,");
		System.out.println("but does not know that X < Y and Y < Z implies X < Z.");
		System.out.println("");
		System.out.println("'show' shows declared variables and their types");
		System.out.println("'debug' toggles debugging information");
		System.out.println("'quit', 'exit', 'hasta la vista, baby', among others, leave the application");
		System.out.println("'help' shows this information again");
		System.out.println("");
		System.out.println("***********************************************************************************");
		System.out.println("");
	}
}
