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

import static com.sri.ai.expresso.helper.Expressions.makeSymbol;
import static com.sri.ai.expresso.helper.Expressions.parse;
import static com.sri.ai.grinder.helper.GrinderUtil.BOOLEAN_TYPE;
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
import com.sri.ai.expresso.type.RealInterval;
import com.sri.ai.grinder.sgdpllt.api.Context;
import com.sri.ai.grinder.sgdpllt.api.Theory;
import com.sri.ai.grinder.sgdpllt.core.TrueContext;
import com.sri.ai.grinder.sgdpllt.rewriter.api.Simplifier;
import com.sri.ai.grinder.sgdpllt.theory.compound.CompoundTheory;
import com.sri.ai.grinder.sgdpllt.theory.differencearithmetic.DifferenceArithmeticTheory;
import com.sri.ai.grinder.sgdpllt.theory.equality.EqualityTheory;
import com.sri.ai.grinder.sgdpllt.theory.linearrealarithmetic.LinearRealArithmeticTheory;
import com.sri.ai.grinder.sgdpllt.theory.propositional.PropositionalTheory;
import com.sri.ai.util.console.ConsoleIterator;
import com.sri.ai.util.console.DefaultConsoleIterator;
import com.sri.ai.util.console.gui.GUIConsoleIterator;

/**
 * Provides a shell for evaluating expressions.
 * 
 * @author braz
 *
 */
@Beta
public class SymbolicShell {

	private static boolean debug = false;
	
	public static void main(String[] args) {

		CompoundTheory theory = new CompoundTheory(
				new EqualityTheory(false, true),
				new DifferenceArithmeticTheory(false, false),
				new LinearRealArithmeticTheory(false, false),
				new PropositionalTheory());
		Simplifier evaluator = (e, c) -> theory.evaluate(e, c);
//		Simplifier evaluator = new Evaluator(theory);
		
		Context context = new TrueContext(theory);
		context = context.add(BOOLEAN_TYPE);
		context = context.add(new Categorical("People",  1000000, makeSymbol("ann"), makeSymbol("bob"), makeSymbol("ciaran")));
		context = context.add(new IntegerInterval("Integer"));
		context = context.add(new RealInterval("Real"));
		
		context = context.registerAdditionalSymbolsAndTypes(map(makeSymbol("P"), makeSymbol("Boolean")));
		context = context.registerAdditionalSymbolsAndTypes(map(makeSymbol("Q"), makeSymbol("Boolean")));
		context = context.registerAdditionalSymbolsAndTypes(map(makeSymbol("R"), makeSymbol("Boolean")));
		context = context.registerAdditionalSymbolsAndTypes(map(makeSymbol("S"), makeSymbol("Boolean")));

		context = context.registerAdditionalSymbolsAndTypes(map(makeSymbol("C"), makeSymbol("People")));
		context = context.registerAdditionalSymbolsAndTypes(map(makeSymbol("D"), makeSymbol("People")));
		context = context.registerAdditionalSymbolsAndTypes(map(makeSymbol("E"), makeSymbol("People")));

		context = context.registerAdditionalSymbolsAndTypes(map(makeSymbol("I"), makeSymbol("Integer")));
		context = context.registerAdditionalSymbolsAndTypes(map(makeSymbol("J"), makeSymbol("Integer")));
		context = context.registerAdditionalSymbolsAndTypes(map(makeSymbol("K"), makeSymbol("Integer")));

		context = context.registerAdditionalSymbolsAndTypes(map(makeSymbol("X"), makeSymbol("Real")));
		context = context.registerAdditionalSymbolsAndTypes(map(makeSymbol("Y"), makeSymbol("Real")));
		context = context.registerAdditionalSymbolsAndTypes(map(makeSymbol("Z"), makeSymbol("Real")));
		
		ConsoleIterator consoleIterator = getConsole(args);
		
		help(consoleIterator);
		
		Collection<String> examples = list(
				"sum({{ (on C in People)  3 }})",
				"sum({{ (on C in People)  3 :  C != D }})",
				"product({{ (on C in People)  3 :  C != D }})",
				"| {{ (on C in People)  3 :  C != D }} |",
				"| { (on C in People)  tuple(C) :  C != D } |",
				"max({{ (on C in People)  3 :  C != D }})",
				"sum({{ (on C in People, D in People)  3 :  C != D }})",
				"sum({{ (on C in People)  3 :  C != D and C != ann }})",
				"sum({{ (on C in People, P in Boolean)  3 :  C != ann }})",
				"sum({{ (on C in People, P in Boolean)  3 :  C != ann and not P }})",
				"sum({{ (on C in People, D in People)  if C = ann and D != bob then 2 else 0  :  for all E in People : E = ann => C = E }})"
				
				, "sum({{ (on I in 1..100)  I }})"
				, "sum({{ (on I in 1..100)  I : I != 3 and I != 5 and I != 500 }})"
				, "sum({{ (on I in 1..100)  I : I != J and I != 5 and I != 500 }})"
				, "sum({{ (on I in 1..100)  (I - J)^2 }})"
				, "sum({{ (on I in 1..100)  if I != K then (I - J)^2 else 0 }})"
				
				, "sum({{ (on I in 1..100)  I : I >= 3 and I < 21 }})"
				, "sum({{ (on I in 1..100)  I : I > J and I < 5 and I < 500 }})"
				, "sum({{ (on I in 1..100)  (I - J)^2 : I < 50 }})"
				
				, "sum({{ (on X in [0;100])  1 }})"
				, "sum({{ (on X in [0;100[)  1 }})"
				, "sum({{ (on X in ]0;100])  1 }})"
				, "sum({{ (on X in [0;100])  Y }})"
				, "sum({{ (on X in [0;100])  X }})"
				, "sum({{ (on X in [0;100])  X^2 }})"
				, "sum({{ (on X in [0;100])  X + Y }})"
				, "sum({{ (on X in [0;100])  1 : Y < X and X < Z}})"
				, "sum({{ (on X in Real)  1 : 0 <= X and X <= 100 and Y < X and X < Z}})"
				, "for all X in Real : X > 0 or X <= 0"
				, "for all X in ]0;10] : X > 0"
				, "for all X in [0;10] : X > 0"
				, "| X in 1..10 : X < 4 or X > 8 |"
				, "| X in 1..10, Y in 3..5 : (X < 4 or X > 8) and Y != 5 |"
				);
		for (String example : examples) {
			consoleIterator.getOutputWriter().println(consoleIterator.getPrompt() + example);
			evaluate(consoleIterator, evaluator, theory, example, context);
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
				context = evaluate(consoleIterator, evaluator, theory, input, context);
			}
		}
		
		consoleIterator.getOutputWriter().println("\nGood bye.");	
	}

	/**
	 * @param evaluator
	 * @param theory TODO
	 * @param inputString
	 * @param context
	 * @return 
	 */
	private static Context evaluate(ConsoleIterator consoleIterator, Simplifier evaluator, Theory theory, String inputString, Context context) {
		
		try {
			Expression input = parse(inputString, (errorMessage) -> {throw new Error("Syntax error: " + errorMessage);});
			if (input.hasFunctor("var")) {
				Expression variable = input.get(0);
				Expression type = input.get(1);
				context = context.registerAdditionalSymbolsAndTypes(map(variable, type));
				consoleIterator.getOutputWriter().println();	
				return context;
			}
			Expression result = theory.evaluate(input, context);//evaluator.apply(input, context);
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
				"- 'Real' with pre-defined variables X, Y, Z",
				"- Integer intervals can be used in summations: sum({{(on I in 1..10) I}});",
				"- Real intervals can be used in integrals: sum({{(on X in [0;100]) X}});",
				"- 'People' with 1,000,000 elements and constants 'ann', 'bob', and 'ciaran',",
				"                                       pre-defined variables C, D, E",
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
				"    for all <Variable> in <Type> : <Formula>",
				"    there exists <Variable> in <Type> : <Formula>",
				"",
				"- aggregates over intensionally-defined multi-sets:",
				"        sum({{ (on <Variable1> in <Type>, <Variable2> in <Type>, ...)  <Number-valued> : <Condition> }})",
				"    product({{ (on <Variable1> in <Type>, <Variable2> in <Type>, ...)  <Number-valued> : <Condition> }})",
				"        max({{ (on <Variable1> in <Type>, <Variable2> in <Type>, ...)  <Number-valued> : <Condition> }})",
				"  the 'on' clause indicates the set indices; all other variables are free variables",
				"  and the result may depend on them",
				"",
				"- cardinality over intensionally-defined multi-sets:",
				"       | {{ (on <Variable1> in <Type>, <Variable2> in <Type>, ...)  <Expression> : <Condition> }} |",
				"       (note that the result independs of <Expression>)",
				"",
				"- counting formulas:",
				"       | <Variable1> in <Type>, <Variable2> in <Type>, ... :  <Condition> |",
				"  which are a short-hand for the above with any <Expression> (which is irrelevant anyway)",
				"",
				"- cardinality over intensionally-defined uni-sets with head equal to tuple of indices:",
				"       | { (on <Variable1> in <Type>, <Variable2> in <Type>, ...)  (<Variable1>, <Variable2>, ...) : <Condition> } |",
				"       (which is equivalent to the corresponding multi-set)",
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
