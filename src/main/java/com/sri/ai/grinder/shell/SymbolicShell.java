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
package com.sri.ai.grinder.shell;

import static com.sri.ai.expresso.helper.Expressions.makeSymbol;
import static com.sri.ai.expresso.helper.Expressions.parse;
import static com.sri.ai.util.Util.join;
import static com.sri.ai.util.Util.list;
import static com.sri.ai.util.Util.map;
import static com.sri.ai.util.Util.mapIntoList;

import java.util.Collection;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.type.Categorical;
import com.sri.ai.expresso.type.IntegerInterval;
import com.sri.ai.grinder.GrinderConfiguration;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.core.DefaultRewritingProcess;
import com.sri.ai.grinder.helper.GrinderUtil;
import com.sri.ai.grinder.interpreter.SymbolicCommonInterpreter;
import com.sri.ai.grinder.interpreter.SymbolicCommonInterpreterWithLiteralConditioning;
import com.sri.ai.grinder.sgdpll2.theory.compound.CompoundConstraintTheory;
import com.sri.ai.grinder.sgdpll2.theory.equality.EqualityConstraintTheory;
import com.sri.ai.grinder.sgdpll2.theory.inequality.InequalityConstraintTheory;
import com.sri.ai.grinder.sgdpll2.theory.propositional.PropositionalConstraintTheory;
import com.sri.ai.util.collect.ConsoleIterator;

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

		GrinderConfiguration.disableTrace();
		
		SymbolicCommonInterpreter evaluator =
				new SymbolicCommonInterpreterWithLiteralConditioning(
						new CompoundConstraintTheory(
								new EqualityConstraintTheory(false, false),
								new InequalityConstraintTheory(false, false),
								new PropositionalConstraintTheory()));
		
		RewritingProcess process = new DefaultRewritingProcess(null);
		process = process.put(new Categorical("Boolean", 2, makeSymbol("true"), makeSymbol("false")));
		process = process.put(new Categorical("People",  1000000, makeSymbol("ann"), makeSymbol("bob"), makeSymbol("ciaran")));
		process = process.put(new IntegerInterval("Integer"));
		
		process = GrinderUtil.extendContextualSymbols(map(makeSymbol("P"), makeSymbol("Boolean")), process);
		process = GrinderUtil.extendContextualSymbols(map(makeSymbol("Q"), makeSymbol("Boolean")), process);
		process = GrinderUtil.extendContextualSymbols(map(makeSymbol("R"), makeSymbol("Boolean")), process);
		process = GrinderUtil.extendContextualSymbols(map(makeSymbol("S"), makeSymbol("Boolean")), process);

		process = GrinderUtil.extendContextualSymbols(map(makeSymbol("X"), makeSymbol("People")), process);
		process = GrinderUtil.extendContextualSymbols(map(makeSymbol("Y"), makeSymbol("People")), process);
		process = GrinderUtil.extendContextualSymbols(map(makeSymbol("Z"), makeSymbol("People")), process);

		process = GrinderUtil.extendContextualSymbols(map(makeSymbol("I"), makeSymbol("Integer")), process);
		process = GrinderUtil.extendContextualSymbols(map(makeSymbol("J"), makeSymbol("Integer")), process);
		process = GrinderUtil.extendContextualSymbols(map(makeSymbol("K"), makeSymbol("Integer")), process);
		
		help();

		ConsoleIterator consoleIterator = new ConsoleIterator();
		
		Collection<String> examples = list(
				"sum({{ (on X in People)  3 }})",
				"sum({{ (on X in People)  3 |  X != Y }})",
				"sum({{ (on X in People, Y in People)  3 |  X != Y }})",
				"sum({{ (on X in People)  3 |  X != Y and X != ann }})",
				"sum({{ (on X in People, P in Boolean)  3 |  X != ann }})",
				"sum({{ (on X in People, P in Boolean)  3 |  X != ann and not P }})",
				"sum({{ (on X in People, Y in People)  if X = ann and Y != bob then 2 else 0  |  for all Z in People : Z = ann => X = Z }})"
				);
		for (String example : examples) {
			System.out.println(consoleIterator.getPrompt() + example);
			evaluate(evaluator, example, process);
		}

		while (consoleIterator.hasNext()) {
			String input = consoleIterator.next();
			if (input.equals("")) {
				System.out.println();	
			}
			else if (input.startsWith("show")) {
				System.out.println("\n" +
						join(mapIntoList(process.getContextualSymbolsAndTypes().entrySet(), e -> e.getKey() + ": " + e.getValue()), ", ") + "\n");	
			}
			else if (input.equals("debug")) {
				debug = !debug;
				System.out.println("\nDebug toggled to " + debug + "\n");	
			}
			else if (input.equals("help")) {
				help();
			}
			else {
				process = evaluate(evaluator, input, process);
			}
		}
		
		System.out.println("\nGood bye.");	
	}

	/**
	 * @param evaluator
	 * @param inputString
	 * @param process
	 * @return 
	 */
	private static RewritingProcess evaluate(SymbolicCommonInterpreter evaluator, String inputString, RewritingProcess process) {
		try {
			Expression input = parse(inputString, (errorMessage) -> {throw new Error("Syntax error: " + errorMessage);});
			if (input.hasFunctor("var")) {
				Expression variable = input.get(0);
				Expression type = input.get(1);
				process = GrinderUtil.extendContextualSymbols(map(variable, type), process);
				System.out.println();	
				return process;
			}
			Expression result = evaluator.apply(input, process);
			System.out.println("\n" + result + "\n");
		} catch (Error e) {
			dealWith(e);
		} catch (Exception e) {
			dealWith(e);
		}
		return process;
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
