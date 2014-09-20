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
package com.sri.ai.test.grinder;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.fail;

import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.TimeUnit;

import org.junit.After;
import org.junit.Assert;
import org.junit.Before;

import com.google.common.base.Stopwatch;
import com.sri.ai.brewer.BrewerConfiguration;
import com.sri.ai.brewer.api.BrewerParser;
import com.sri.ai.brewer.api.Grammar;
import com.sri.ai.brewer.api.Parser;
import com.sri.ai.brewer.api.Writer;
import com.sri.ai.brewer.core.DefaultWriter;
import com.sri.ai.expresso.api.CompoundSyntaxTree;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.SyntaxLeaf;
import com.sri.ai.expresso.api.SyntaxTree;
import com.sri.ai.expresso.helper.SyntaxTrees;
import com.sri.ai.grinder.GrinderConfiguration;
import com.sri.ai.grinder.api.Rewriter;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.core.DefaultRewritingProcess;
import com.sri.ai.grinder.helper.GrinderUtil;
import com.sri.ai.grinder.library.Basic;
import com.sri.ai.grinder.library.set.tuple.Tuple;
import com.sri.ai.grinder.parser.antlr.AntlrGrinderParserWrapper;
import com.sri.ai.grinder.ui.TreeUtil;
import com.sri.ai.util.Configuration;
import com.sri.ai.util.concurrent.BranchAndMerge;


abstract public class AbstractGrinderTest {
	public static final String IGNORE_EXPECTED = "IGNORE_EXPECTED";
	public static final String ANY_OF = "anyof";
	
	private static final boolean _dontEndProgram = false;

	protected Expression expression;
	protected String expressionString;
	protected Expression expected;
	protected Expression actual;
	protected Rewriter evaluator;
	protected Grammar grammar;
	protected Parser parser;
	protected Writer writer;
	protected Map<Object, Object> globalObjects;
	protected int maxCounter = 2;

	public AbstractGrinderTest() {
		super();
	}

	@Before
	public void setUp() {
		TreeUtil.flushData();
		Configuration.clear();
		SyntaxTrees.flushGlobalSymbolTable();
		BranchAndMerge.reset();
		
		//System.out.println("Creating grammar...");
		grammar = makeGrammar();
		// Ensure the grammar class passed in is used where necessary.
		Configuration.setProperty(BrewerConfiguration.KEY_DEFAULT_GRAMMAR_CLASS, grammar.getClass().getName());
		
		//System.out.println("Creating parser...");
		parser = makeParser();
		
		//System.out.println("Creating writer...");				
		writer = DefaultWriter.newDefaultConfiguredWriter();
		
		//System.out.println("Creating global objects...");
		globalObjects = new LinkedHashMap<Object, Object>();
	}

	@After
	public void tearDown() {
		parser.close();
		Configuration.clear();
	}

	public void dontEndProgram() {
		while (true) {
			try {
				Thread.sleep(5000);
			} catch (InterruptedException e) {
				e.printStackTrace();
			}
		}
	}

	/**
	 * A method providing a grammar to be used to create {@link #writer}, as well as used by
	 * the default implementation of {@link #makeParser()}
	 * to construct a {@link BrewerParser}.
	 * If an extending class overrides {@link #makeParser()}, the resulting parser must be compatible
	 * with the provided grammar, in the sense that the output S of an expression E by {@link #writer}
	 * should be parsed by the parser as E again.
	 */
	abstract public Grammar makeGrammar();
	
	/**
	 * A method providing a rewriting process to be used during the test.
	 * Rewriting processes created for specific tests will extend this one.
	 * This process used to be hard coded in this class and used to be of a simple kind using {@link Basic}
	 * as a root rewriter.
	 * However, this caused problems because Basic does not include some module providers
	 * (the one that specifically created awareness of this problem was {@link Tuple}).
	 * Even though later rewriting processes created for specific tests were of the right kind and included the necessary providers,
	 * it would not suffice because the first rewriting process was kept as the global rewriting processes for all
	 * expressions in this thread (see {@link DefaultRewritingProcess#getGlobalRewritingProcessForKnowledgeBasedExpressions()}),
	 * and its modules were the ones used.
	 * By introducing this abstract method, we give specific tests a chance to create a first rewriting process of the right kind.
	 * Using a global rewriting process is a temporary hack, and when this is corrected this abstract method
	 * will probably become unnecessary.
	 */
	abstract public RewritingProcess makeRewritingProcess(Expression topExpression);

	public Parser makeParser() {
//		return new BrewerParser(grammar);
		return new AntlrGrinderParserWrapper();
	}
	
	// Runs test on current expression, actual, expected fields, assuming contextual constraint "true".
	protected void evaluationTest() {
		evaluationTest(evaluator);
	}

	// Runs test on current expression, actual, expected fields, assuming contextual constraint "true".
	protected void evaluationTest(Rewriter evaluator) {
		evaluationTest(evaluator, globalObjects, new DefaultRewritingProcess(evaluator));
	}
	
	// Runs test on current expression, actual, expected fields, assuming contextual constraint "true".
	protected void evaluationTest(RewritingProcess process) {
		evaluationTest(evaluator, globalObjects, process);
	}
	
	// Runs test on current expression, actual, expected fields, assuming contextual constraint "true".
	protected void evaluationTest(Rewriter evaluator, Map<Object, Object> globalObjects, RewritingProcess process) {
		System.out.println("Solving " + expressionString);

		expression = parse(expressionString);
		System.out.println();
		System.out.println(expression);
		System.out.println();

		process.getGlobalObjects().putAll(globalObjects);
		
		process = GrinderUtil.extendContextualSymbolsWithFreeSymbolsInExpressionwithUnknownTypeForSetUpPurposesOnly(expression, process);
		// the above would have to include the contextual constraint (say, in a Tuple with expression), if it were not known to be "true".
		
		Stopwatch stopwatch = Stopwatch.createStarted();
		actual = evaluator.rewrite(expression, process);
		long evaluationTime = stopwatch.elapsed(TimeUnit.MILLISECONDS);
		System.out.println("Evaluation time: " + evaluationTime + " ms");
	
		boolean succeded = areEqual(actual, expected);
		if ( succeded ) {
			System.out.println("Test successful.");
		} 
		else {
			System.out.println("Expected: " + writer.toString(expected));
			System.out.println("Actual  : " + writer.toString(actual));
//			System.out.println("Expected: "
//					+ writer.toString(DefaultWriter
//							.getSyntaxTreeOrNullIfNull(expected)));
//			System.out.println("Actual  : "
//					+ writer.toString(DefaultWriter
//							.getSyntaxTreeOrNullIfNull(actual)));
			// System.out.println("Grinder test: raw expected: " +
			// expected.defaultToString());
			// System.out.println("Grinder test: raw actual  : " +
			// actual.defaultToString());
		}
		System.out.println("\n************************\n");
		if (_dontEndProgram) {
			dontEndProgram();
		}

		if ( !succeded ) {
			assertEquals(expected, actual); // no real need to compare them
											// again, but this generates a
											// clearer unit testing message.
		}
	}
	
	protected long perform(TestData[] tests) {
		return perform(tests, 0, tests.length);
	}

	protected long perform(TestData[] tests, int begin, int end) {
		long totalRewriteTime = 0;
		
		Expression topExpression;
		RewritingProcess process;
		Expression actual;
		
		String assertFailed = null;
		int run = 0;
		for (int i = begin; i < end; i++) {
			topExpression = tests[i].getTopExpression();
			process = makeRewritingProcess(topExpression);

			process = GrinderUtil.extendContextualSymbolsWithFreeSymbolsInExpressionwithUnknownTypeForSetUpPurposesOnly(
					Tuple.make(topExpression, tests[i].contextualConstraint),
					process);
			
			process = GrinderUtil.extendContextualConstraint(tests[i].contextualConstraint, process);

			Expression expectedExpressions = parse(tests[i].expected);
			Assert.assertNotNull("Unable to parse expected expression: "+tests[i].expected, expectedExpressions);
			if (tests[i].isIllegalArgumentTest) {
				try {
					actual = tests[i].callRewrite(process);
					fail("tests[i]=" + i + ", " + topExpression + " should have thrown an IllegalArgumentException.");
				} catch (IllegalArgumentException iae) {
					// ok this is expected
					com.sri.ai.grinder.helper.Trace.setTraceLevel(0);
					com.sri.ai.grinder.helper.Trace.log("-R_: expected IllegalArgumentException thrown:"+iae.getMessage());
				}
			}
			else {
				System.out.println("tests["+ i +"] rewriting "+topExpression);
				long startTime = System.currentTimeMillis();
				actual = tests[i].callRewrite(process);
				long rewroteIn = System.currentTimeMillis()-startTime;
				totalRewriteTime += rewroteIn;
				System.out.println("tests["+ i +" rewrote in "+(rewroteIn)+"ms]:"+topExpression + "\n--->\n" + actual);
				boolean succeded = tests[i].expected.equals(IGNORE_EXPECTED) || areEqual(actual, expectedExpressions);
				
				if ( !succeded ) {
					assertFailed = "ERROR tests[i]=" + i + ", " + topExpression 
							+ "\nexpected=" + tests[i].expected
							+ "\n but was=" + actual;
					if (GrinderConfiguration.isWaitUntilUIClosedEnabled()) {					
						System.err.println(assertFailed);
					}
					else {
						Assert.fail(assertFailed);
					}
					break;
				}
			}
			run++;
		}

		if (!GrinderConfiguration.isWaitUntilUIClosedEnabled()) {
			assertEquals("Ensure you run all the tests", end-begin, run);
		}
		
		GrinderUtil.doTreeUtilWaitUntilClosed();
		
		if (assertFailed != null) {
			Assert.fail(assertFailed);
		}
		return totalRewriteTime;
	}
	
	/**
	 * Returns true if e1 is equal to e2. e2 can have sub-syntax trees defined by anyof().
	 * For instance, if e2 is 'anyof(1, 3 + anyof(X, Y))', then e1 can be any of '1', '3+X', or '3+Y'.
	 * 
	 * @param e1
	 * @param e2
	 * @return true if e1 and e2 are equal, considering that e2 may be defined using anyof()
	 */
	protected boolean areEqual(Expression e1, Expression e2) {
		boolean result = areEqual(e1.getSyntaxTree(), e2.getSyntaxTree());
		return result;
	}
	
	/**
	 * Returns true if e1 is equal to e2. e2 can have sub-syntax trees defined by anyof().
	 * For instance, if e2 is 'anyof(1, 3 + anyof(X, Y))', then e1 can be any of '1', '3+X', or '3+Y'.
	 * 
	 * @param syntaxTree1
	 * @param syntaxTree2
	 * @return true if e1 and e2 are equal, considering that e2 may be defined using anyof()
	 */
	protected boolean areEqual(SyntaxTree syntaxTree1, SyntaxTree syntaxTree2) {
		boolean succeeded = false;
		if ( syntaxTree1 == null ) {
			succeeded = syntaxTree2 == null;
		} 
		else if ( syntaxTree2 != null && syntaxTree2.getLabel().equals(ANY_OF) ) {
			succeeded = isEqualToAny(syntaxTree1, syntaxTree2.getImmediateSubTrees());
		}
		else if ( syntaxTree1 instanceof SyntaxLeaf && syntaxTree2 instanceof SyntaxLeaf ) {
			succeeded = ((SyntaxLeaf)syntaxTree1).getValue().equals(((SyntaxLeaf)syntaxTree2).getValue());
		}
		else if ( syntaxTree1 instanceof CompoundSyntaxTree && syntaxTree2 instanceof CompoundSyntaxTree ) {
			CompoundSyntaxTree e1fa = (CompoundSyntaxTree) syntaxTree1;
			CompoundSyntaxTree e2fa = (CompoundSyntaxTree) syntaxTree2;
			if ( syntaxTree1.getLabel().equals(syntaxTree2.getLabel()) && e1fa.getImmediateSubTrees().size() == e2fa.getImmediateSubTrees().size() ) {
				succeeded = true;
				List<SyntaxTree> e1SubTrees = e1fa.getImmediateSubTrees();
				List<SyntaxTree> e2SubTrees = e2fa.getImmediateSubTrees();
				for (int i=0; i<e1SubTrees.size(); i++) {
					if ( !areEqual(e1SubTrees.get(i), e2SubTrees.get(i)) ) {
						succeeded = false;
						break;
					}
				}
			}
		}
		else {
			succeeded = syntaxTree1.equals(syntaxTree2);
		}
		return succeeded;
	}
		
	protected boolean isEqualToAny(SyntaxTree e1, List<SyntaxTree> choices) {
		for (SyntaxTree choice: choices) {
			if ( areEqual(e1, choice) ) {
				return true;
			}
		}
		return false;
	}

	protected Expression parse(String expressionString) {
		return parser.parse(expressionString);
	}

	/**
	 * @deprecated Use {@link GrinderUtil#doTreeUtilWaitUntilClosed()} instead
	 */
	@Deprecated
	public static void doTreeUtilWaitUnilClosed() {
		GrinderUtil.doTreeUtilWaitUntilClosed();
	}
}
