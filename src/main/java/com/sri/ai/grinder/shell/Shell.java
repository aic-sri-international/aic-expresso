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

import com.google.common.annotations.Beta;
import com.sri.ai.brewer.BrewerConfiguration;
import com.sri.ai.brewer.api.Grammar;
import com.sri.ai.brewer.api.Writer;
import com.sri.ai.brewer.core.CommonGrammar;
import com.sri.ai.brewer.core.DefaultWriter;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.api.Library;
import com.sri.ai.grinder.api.Rewriter;
import com.sri.ai.grinder.core.ExhaustiveRewriter;
import com.sri.ai.grinder.library.CommonLibrary;
import com.sri.ai.util.Configuration;
import com.sri.ai.util.collect.ConsoleIterator;

/**
 * 
 * @author braz
 *
 */
@Beta
public class Shell {

	public static void main(String[] args) {
		run(new CommonGrammar(), new CommonLibrary());
	}

	public static void run(Grammar grammar, Library library) {
		// Ensure the grammar class passed in is used where necessary.
		Configuration.setProperty(BrewerConfiguration.KEY_DEFAULT_GRAMMAR_CLASS, grammar.getClass().getName());
		
		Writer writer = DefaultWriter.newDefaultConfiguredWriter();
		Rewriter evaluator = new ExhaustiveRewriter(library);
		ConsoleIterator consoleIterator = new ConsoleIterator();
		
		while (consoleIterator.hasNext()) {
			String command = consoleIterator.next();
			Expression parse = grammar.parse(command);
			Expression result = evaluator.rewrite(parse);
			System.out.println(writer.toString(result.getSyntaxTree()));
		}
	}
}
