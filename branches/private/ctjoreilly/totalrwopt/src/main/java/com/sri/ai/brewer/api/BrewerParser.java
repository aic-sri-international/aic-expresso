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
package com.sri.ai.brewer.api;

import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.TimeUnit;

import com.google.common.annotations.Beta;
import com.google.common.base.Stopwatch;
import com.sri.ai.brewer.BrewerConfiguration;
import com.sri.ai.brewer.core.DefaultParsingProcess;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.library.set.extensional.ExtensionalSet;
import com.sri.ai.util.Util;

/**
 * An experimental hand-crafted expression parser.
 *
 * @author braz
 *
 */
@Beta
public class BrewerParser implements Parser {

	private Grammar grammar;
	
	public BrewerParser(Grammar grammar) {
		this.grammar = grammar;
		initializeParsingCache();
	}
	
	@Override
	public void close() {
		saveParsingCache();
	}
	
	/**
	 * This cache is a temporary solution for the inefficiency of the current
	 * parser. It saves strings between executions of the program via
	 * serialization. When the grammar is changed, the file needs to be deleted
	 * so that the cache is not incorrectly used.
	 */
	private Map<String, Expression> cache = new HashMap<String, Expression>();

	@SuppressWarnings("unchecked")
	private void initializeParsingCache() {
		if (BrewerConfiguration.isUseParsingCache()) {
			try {
				FileInputStream fileInputStream = new FileInputStream("parsing cache");
				ObjectInputStream objectInputStream = new ObjectInputStream(fileInputStream);
				cache = (Map<String, Expression>) objectInputStream.readObject();
				objectInputStream.close();
				fileInputStream.close();
			} catch (FileNotFoundException e) {
				cache = new HashMap<String, Expression>();
			} catch (IOException e) {
				cache = new HashMap<String, Expression>();
			} catch (ClassNotFoundException e) {
				cache = new HashMap<String, Expression>();
			}
		}
	}

	@Override
	public Expression parse(String expressionString) {
		ParsingProcess process = new DefaultParsingProcess(expressionString, grammar);
		if (BrewerConfiguration.isOutputParsingTimeInfo()) {
			System.out.println("Parsing: " + expressionString);
		}
		Stopwatch stopwatch = new Stopwatch().start();

		Expression result;
		
		// TODO - this is a workaround for the parser not being able to
		// parse empty multisets currently.
		if (expressionString.equals("{{}}") || expressionString.equals("{{ }}")) {
			result = ExtensionalSet.makeMultiSet(new ArrayList<Expression>());
		} 
		else {
			result = cache.get(expressionString);
		}
		
		if (result == null) {
			result = process.parseOfNonTerminal("Expression");
			cache.put(expressionString, result);
		}

		long parsingTime = stopwatch.elapsed(TimeUnit.MILLISECONDS);
		if (BrewerConfiguration.isOutputParsingTimeInfo()) {
			System.out.println("Parsed: " + result);
			System.out.println("Parsing time: " + parsingTime + " ms");
		}
		return result;
	}

	private void saveParsingCache() {
		if (BrewerConfiguration.isUseParsingCache()) {
			try {
				FileOutputStream fileOutputStream = new FileOutputStream("parsing cache");
				ObjectOutputStream objectOutputStream = new ObjectOutputStream(fileOutputStream);
				objectOutputStream.writeObject(cache);
				objectOutputStream.close();
				fileOutputStream.close();
			} catch (FileNotFoundException e) {
				Util.fatalError("Parsing cache could not be saved. " + e);
			} catch (IOException e) {
				Util.fatalError("Parsing cache could not be saved. " + e);
			}
		}
	}
}
