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

import java.util.Arrays;
import java.util.Collection;
import java.util.List;

import joptsimple.OptionParser;
import joptsimple.OptionSet;
import joptsimple.OptionSpec;

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
import com.sri.ai.grinder.sgdpll.theory.differencearithmetic.DifferenceArithmeticConstraintTheory;
import com.sri.ai.grinder.sgdpll.theory.equality.EqualityConstraintTheory;
import com.sri.ai.grinder.sgdpll.theory.propositional.PropositionalConstraintTheory;
import com.sri.ai.util.console.ConsoleIterator;
import com.sri.ai.util.console.DefaultConsoleIterator;
import com.sri.ai.util.console.gui.GUIConsoleIterator;

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
				new DifferenceArithmeticConstraintTheory(false, false),
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
		
		ConsoleIterator consoleIterator = getConsole(args);
		
		help(consoleIterator);
		
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
				
				, "sum({{ (on I in 1..100)  I | I >= 3 and I < 21 }})"
				, "sum({{ (on I in 1..100)  I | I > J and I < 5 and I < 500 }})"
				, "sum({{ (on I in 1..100)  (I - J)^2 | I < 50 }})"
				
				);
		for (String example : examples) {
			consoleIterator.getOutputWriter().println(consoleIterator.getPrompt() + example);
			evaluate(consoleIterator, evaluator, example, context);
			consoleIterator.getOutputWriter().println("\n");
		}

		while (consoleIterator.hasNext()) {
			String input = consoleIterator.next();
			if (input.equals("")) {
				consoleIterator.getOutputWriter().println();	
			}
			else if (input.startsWith("show")) {
				consoleIterator.getOutputWriter().println("\n" +
						join(mapIntoList(context.getSymbolsAndTypes().entrySet(), e -> e.getKey() + ": " + e.getValue()), ", ") + "\n");	
			}
			else if (input.equals("debug")) {
				debug = !debug;
				consoleIterator.getOutputWriter().println("\nDebug toggled to " + debug + "\n");	
			}
			else if (input.equals("help")) {
				help(consoleIterator);
			}
			else {
				context = evaluate(consoleIterator, evaluator, input, context);
			}
		}
		
		consoleIterator.getOutputWriter().println("\nGood bye.");	
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
	private static Context evaluate(ConsoleIterator consoleIterator, Simplifier evaluator, String inputString, Context context) {
		
		try {
			Expression input = parse(inputString, (errorMessage) -> {throw new Error("Syntax error: " + errorMessage);});
			if (input.hasFunctor("var")) {
				Expression variable = input.get(0);
				Expression type = input.get(1);
				context = context.registerIndicesAndTypes(map(variable, type));
				consoleIterator.getOutputWriter().println();	
				return context;
			}
			Expression result = evaluator.apply(input, context);
			consoleIterator.getOutputWriter().println("\n" + result + "\n");
		} catch (Error e) {
			dealWith(consoleIterator, e);
		} catch (Exception e) {
			dealWith(consoleIterator, e);
		}
		return context;
	}

	private static void dealWith(ConsoleIterator consoleIterator, Throwable e) {
		if (debug) {
			e.printStackTrace(consoleIterator.getErrorWriter());
		}
		else {
			consoleIterator.getErrorWriter().println("\n" + throwableMessage(e) + "\n");
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
	
	private static ConsoleIterator getConsole(String[] args) {
		ConsoleIterator result = null;
		
		OptionParser parser = new OptionParser();
		
		OptionSpec<String> consoleType = parser.accepts("console", "Console type <gui or default>").withRequiredArg().ofType(String.class);
		OptionSet options = parser.parse(args);
		if (options.has(consoleType)) {
			if ("gui".equalsIgnoreCase(options.valueOf(consoleType))) {
				result = new GUIConsoleIterator();
			}
		}
		
		if (result == null) {
			result = new DefaultConsoleIterator();
		}
		
		return result;
	}

	/**
	 * 
	 */
	private static void help(ConsoleIterator consoleIterator) {
		List<String> helpLines = Arrays.asList(
				"***********************************************************************************",
				"",
				"Welcome to SRI AIC expresso symbolic interpreter",
				"",
				"Pre-defined types are:",
				"- 'Boolean' with constants 'true' and 'false',",
				"                 pre-defined variables P, Q, R, S",
				"- 'Integer' with pre-defined variables I, J, K",
				"- Integer intevals can be used in summations: sum({{(on I in 1..10) I}});",
				"- 'People' with 1,000,000 elements and constants 'ann', 'bob', and 'ciaran',",
				"                                       pre-defined variables X, Y, Z",
				"",
				"Capitalized symbols (other than types) are considered variables",
				"",
				"The language includes:",
				"",
				"- if-then-else",
				"- equality (=, !=)",
				"- boolean operators: 'and', 'or', 'not', '=>', '<=>'",
				"- numeric operators: +, -, *, /, ^, <, >, <=, >=",
				"",
				"- universal and existential quantification:",
				"- for all X in <Type> : <Formula>",
				"- there exists X in <Type> : <Formula>",
				"",
				"- aggregates over intensionally-defined multi-sets:",
				"-     sum({{ (on X in <Type>, Y in <Type>, ...)  <Number-valued> | <Condition> }})",
				"- product({{ (on X in <Type>, Y in <Type>, ...)  <Number-valued> | <Condition> }})",
				"-     max({{ (on X in <Type>, Y in <Type>, ...)  <Number-valued> | <Condition> }})",
				"- the 'on' clause indicates the set indices; all other variables are free variables",
				"  and the result may depend on them",
				"",
				"- cardinality over intensionally-defined multi-sets:",
				"-      | ({{ (on X in <Type>, Y in <Type>, ...)  <Number-valued> : <Condition> }}) |",
				"",
				"Inference only works on equality, propositions and difference arithmetic on integers.",
				"This means that equalities (=), disequalities (!=) and inequalities (<, >, <=, >=) over integers",
				"can involve at most two variables, they must not be multiplied by anything and have opposite signs when moved to the same side of operator.",
				"For example, \"I - J > 2\", \"-I = -J\", and \"I > 3\" are all difference arithmetic literals,",
				"but \"2*I = J\", \"I + J < 3\", or \"I -J + K = 0\" are not.",
				"",
				"'show' shows declared variables and their types",
				"'debug' toggles debugging information",
				"'quit', 'exit', 'hasta la vista, baby', among others, leave the application",
				"'help' shows this information again",
				"",
				"***********************************************************************************",
				"");
		helpLines.forEach(line -> consoleIterator.getOutputWriter().println(line));
	}
}
