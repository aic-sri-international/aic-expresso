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

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.type.Categorical;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.core.DefaultRewritingProcess;
import com.sri.ai.grinder.interpreter.SymbolicCommonInterpreter;
import com.sri.ai.grinder.sgdpll2.theory.compound.CompoundConstraintTheory;
import com.sri.ai.grinder.sgdpll2.theory.equality.EqualityConstraintTheory;
import com.sri.ai.grinder.sgdpll2.theory.propositional.PropositionalConstraintTheory;
import com.sri.ai.util.collect.ConsoleIterator;

/**
 * Provides a shell for use of {@link SymbolicCommonInterpreter}.
 * 
 * @author braz
 *
 */
@Beta
public class SGShell {

	private static boolean debug = false;
	
	public static void main(String[] args) {
		
		SymbolicCommonInterpreter evaluator =
				new SymbolicCommonInterpreter(
						new CompoundConstraintTheory(
								new EqualityConstraintTheory(false),
								new PropositionalConstraintTheory()));
		
		RewritingProcess process = new DefaultRewritingProcess(null);
		process = process.put(new Categorical("Boolean", 2, makeSymbol("true"), makeSymbol("false")));
		process = process.put(new Categorical("People",  1000000, makeSymbol("ann"), makeSymbol("bob"), makeSymbol("ciaran")));
		
		help();

		ConsoleIterator consoleIterator = new ConsoleIterator();
		
		String example = "sum({{ (on X in People, Y in People)  if X = ann and Y != bob then 2 else 0  |  for all Z in People : Z = ann => X = Z }})";
		System.out.println(consoleIterator.getPrompt() + " " + example);
		evaluate(evaluator, example, process);

		while (consoleIterator.hasNext()) {
			String input = consoleIterator.next();
			if (input.equals("")) {
				System.out.println();	
			}
			else if (input.equals("debug")) {
				debug = !debug;
				System.out.println("Debug toggled to " + debug);	
			}
			else if (input.equals("help")) {
				help();
			}
			else {
				evaluate(evaluator, input, process);
			}
		}
		
		System.out.println("\nGood bye.");	
	}

	/**
	 * @param evaluator
	 * @param input
	 * @param process
	 */
	private static void evaluate(SymbolicCommonInterpreter evaluator, String input, RewritingProcess process) {
		try {
			Expression parse = parse(input);
			Expression result = evaluator.apply(parse, process);
			System.out.println("\n" + result + "\n");
		} catch (Error e) {
			dealWith(e);
		} catch (Exception e) {
			dealWith(e);
		}
	}

	private static void dealWith(Throwable e) {
		if (debug) {
			e.printStackTrace();
		}
		else {
			System.err.println("\n" + throwableMessage(e) + "\n");
		}
	}

	private static String throwableMessage(Throwable e) {
		return e.getMessage() == null || e.getMessage().equals("null") ? "Sorry, an error without a message occurred\n" : e.getMessage();
	}

	/**
	 * 
	 */
	private static void help() {
		System.out.println("********************************************************************************");
		System.out.println("");
		System.out.println("Welcome to SRI AIC expresso symbolic interpreter");
		System.out.println("");
		System.out.println("Pre-defined types are:");
		System.out.println("- 'Boolean' with constants 'true' and 'false'");
		System.out.println("- 'People' with 1,000,000 elements and constants 'ann', 'bob', and 'ciaran'");
		System.out.println("");
		System.out.println("Capitalized symbols (other than types) are considered variables");
		System.out.println("");
		System.out.println("The language includes:");
		System.out.println("- if-then-else");
		System.out.println("- equality (=, !=)");
		System.out.println("- boolean operators: 'and', 'or', 'not', '=>', '<=>'");
		System.out.println("- numeric operators: +, -, *, /, ^, <, >, <=, >=");
		System.out.println("- universal and existential quantification:");
		System.out.println("- for all X in <Type> : <Formula>");
		System.out.println("- there exists X in <Type> : <Formula>");
		System.out.println("- aggregates over multi-sets (denoted by {{ (Indices) Element | Condition }}):");
		System.out.println("-     sum({{ (on X in <Type>, Y in <Type>, ...)  <Number-valued> : <Formula> }})");
		System.out.println("- product({{ (on X in <Type>, Y in <Type>, ...)  <Number-valued> : <Formula> }})");
		System.out.println("-     max({{ (on X in <Type>, Y in <Type>, ...)  <Number-valued> : <Formula> }})");
		System.out.println("");
		System.out.println("Global inference only works on equality and propositions");
		System.out.println("This means the system knows P and (P => Q) implies Q,");
		System.out.println("and that X != Y and Y = Z implies X != Z,");
		System.out.println("but does not know that X < Y and Y < Z implies X < Z.");
		System.out.println("");
		System.out.println("'debug' toggles debugging information");
		System.out.println("'quit', 'exit', 'hasta la vista, baby', among others, leave the application");
		System.out.println("'help' shows this information again");
		System.out.println("");
		System.out.println("********************************************************************************");
		System.out.println("");
	}
}
