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
package com.sri.ai.brewer.parsingexpression.core;

import java.util.Collections;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Set;
import java.util.Stack;

import com.google.common.annotations.Beta;
import com.sri.ai.brewer.api.BasicParsingExpression;
import com.sri.ai.brewer.api.ParsingExpression;
import com.sri.ai.brewer.api.ParsingProcess;
import com.sri.ai.brewer.api.Writer;
import com.sri.ai.brewer.core.DefaultParsingProcess;
import com.sri.ai.brewer.core.DefaultParsingResult;
import com.sri.ai.brewer.core.DefaultWriter;
import com.sri.ai.brewer.core.ParserFlags;
import com.sri.ai.brewer.core.ParsingResult;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.SyntaxTree;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.util.Util;
import com.sri.ai.util.collect.FunctionIterator;

/**
 * A parsing expression consisting of a (possibly empty) sequence of parsing
 * expressions. It is, in a way, the most important parsing expression since it
 * forms basic operator applications. It parses successfully if its components
 * are successively successfully parsed. Its parse is <code>null</code> if it is
 * the empty sequence. Otherwise, it forms a root tree name string by
 * concatenating
 * <ul>
 * <li>the terminal representation, if the component is a terminal
 * <li>the character ".", if the component is not a terminal.
 * </ul>
 * (a single space is inserted whenever it is needed to avoid two identifiers
 * from becoming one, and to keep dots separated) and then, for <code>X</code> a
 * non-empty string of non-dot tokens, replacing <code>". X ."</code> and
 * <code>"X . . . (...) ."</code> by <code>"X"</code>. Note that this applies
 * only for X a non-empty string of non-dot tokens, therefore names such as
 * ". ate . with ." are not not replaced by "ate . with", or ". ate . with".
 * Spaces are also trimmed off the functor name.
 * <p>
 * These are examples of root tree names formed this way:
 * <ul>
 * <li> <code>add Expression to Expression  -->  "add . to ."</code>
 * <li>
 * <code>items Expression Expression with Expression -->  "items . . to ."</code>
 * <li> <code>items Expression Expression -->  "items"</code>
 * <li> <code>Expression + Expression  -->  "+"</code>
 * <li> <code>log Expression  -->  "log"</code>
 * <li> <code>++Expression  -->  "++"</code>
 * <li> <code>Expression++  -->  ".++"</code>
 * <li> <code>Expression Expression  -->  ". ."</code>
 * </ul>
 * <p>
 * It also forms an sub-tree list out of the parses of sub-parsing expressions.
 * 
 * @author braz
 */
@Beta
public class Sequence extends AbstractParsingExpression implements BasicParsingExpression {
	private static final long serialVersionUID = 1L;
	
	private String rootTreeName = null;
	private int arity;
	
	public Sequence(Object... args) {
		super("sequence", args);
	}

	@Override
	public String getRootTreeName() {
		if (rootTreeName == null) {
			determineRootTreeNameAndArity();
		}
		return rootTreeName;
	}

	@Override
	public int getArity() {
		if (rootTreeName == null) { // we use 'rootTreeName' as the flag indicating this processing.
			determineRootTreeNameAndArity();
		}
		return arity;
	}

	private void determineRootTreeNameAndArity() {
		StringBuffer rootTree = new StringBuffer();
	
		// The following variables keep track of whether we have the pattern Terminal SubExpression Terminal,
		// which prevents trimming in the case described as below:
		//
		// for <code>X</code> a string of non-dot tokens,
		// replacing <code>". X ."</code> and <code>"X . . . (...) ."</code> by <code>"X"</code>.
		// Note that this applies only for X a string of non-dot tokens,
		// therefore names such as ". ate . with ." are not not replaced by "ate . with", or ". ate . with".
		//
		// If X has a dot in the middle, we will have a terminal-SubExpression-terminal in the sequence.
		boolean terminal = false;
		boolean terminalSubExpression = false;
		boolean terminalSubExpressionTerminal = false;
		
		arity = 0;
		for (Expression subParsingExpression : getArguments()) {
			if (subParsingExpression.hasFunctor("terminal")) {
				append(rootTree, subParsingExpression.get(0));
				terminal = true;
				terminalSubExpressionTerminal = terminalSubExpression;
			}
			else {
				append(rootTree, ".");
				terminalSubExpression = terminal;
				arity++;
			}
		}
		
		rootTreeName = rootTree.toString();
		if (! terminalSubExpressionTerminal) {
			rootTreeName = trimDots(trimSpaces(rootTreeName));
		}
	}
	
	@Override
	protected ParsingResult parsingResultAfterBookkeeping(ParsingProcess process) {
		List<ParsingResult> results = new LinkedList<ParsingResult>();
		boolean tokenPositionLimitInfluencedResult = false;
		
		int index = 0;
		for (ParsingExpression subParsingExpression : getParsingExpressionArguments()) {
			ParsingResult subParsingExpressionParsingResult;
			if (ParserFlags.lookAhead && process.currentTokenizerPositionLimit() != -1) {
				subParsingExpressionParsingResult = parsingResultOfSubParsingExpressionWithTerminalLimits(subParsingExpression, index, process);
			}
			else {
				subParsingExpressionParsingResult = parsingResultOfSubParsingExpression(subParsingExpression, index, process);
			}

			tokenPositionLimitInfluencedResult =
				tokenPositionLimitInfluencedResult ||
				subParsingExpressionParsingResult.tokenPositionLimitInfluencedResult();

			if (subParsingExpressionParsingResult.isSuccessful()) { // we found a parse for the sub-parsing expression
				results.add(subParsingExpressionParsingResult);
				index++;
			}
			else { // we failed
				retract(results, process);
				return DefaultParsingResult.makeFailedParsingResult(tokenPositionLimitInfluencedResult);
			}
		}
		return makeParsingResult(results, tokenPositionLimitInfluencedResult);
	}

	/**
	 * Tries to parse sub-parsing expression by limiting the search to the last occurrence of a necessary terminal,
	 * if there is one (and failing straight away if there is none).
	 * This method assumes that there is a current limit already, otherwise it would be impossible to find
	 * the last occurrence of a terminal in the input tokens.
	 */
	public ParsingResult parsingResultOfSubParsingExpressionWithTerminalLimits(
			ParsingExpression subParsingExpression, int index,
			ParsingProcess process) {

		process.logln("Parsing sequence sub-expression " + subParsingExpression);
		process.pushLevel();
		int newLimit = -1;
		ParsingProcess.CloserTokensResult closerTokensResult = getLimitFromCloserTokenEncirclingParentheticallyWellFormedSegment(index, process);
		if (closerTokensResult != null) {
			if (closerTokensResult.success) {
				newLimit = closerTokensResult.position;
			}
			else {
				boolean tokenPositionLimitInfluencedResult =
					closerTokensResult instanceof ParsingProcess.LimitOrEndOfTokensReached;
				// otherwise, the failure was due to a mismatched closer, in which case the limit had nothing to do with it.
				return DefaultParsingResult.makeFailedParsingResult(tokenPositionLimitInfluencedResult);
			}
		}
		else {
			newLimit = getLimitToTokensEqualToNextExpectedTerminal(index, process); // this is -1 if the next sub-parsing expression is not a terminal or does not exist
		}
//		limit = -1;// DEBUGGING: annulling the effects of looking ahead

		process.logln("Current limit is " + process.currentTokenizerPositionLimit());
		boolean newLimitFound = newLimit != -1 && newLimit != process.currentTokenizerPositionLimit();
		if (newLimitFound) { // there is a following terminal, a corresponding token found, and thus a new limit
			process.pushTokenizerPositionLimit(newLimit);
			process.logln("Setting limit to " + newLimit);
		}
		else {
			process.logln("Leaving limit at current level");
		}
		
		ParsingResult result = parsingResultOfSubParsingExpression(subParsingExpression, index, process);

		if (newLimitFound) {
			process.popTokenizerPositionLimit();
		}

		if (DefaultParsingResult.isSuccessful(result)) {
			// it succeeded, so set the subParsingExpression result to this one
			process.logln("Parsed " + result);
		}

		if (newLimitFound) {
			process.logln("Limit back to " + process.currentTokenizerPositionLimit());
		}
		
		process.popLevel();
		return result;
	}
	
	/**
	 * Returns a {@link ParsingProcess.CloserTokensResult} if the current token is a parenthetical opener, and <code>null</code> otherwise.
	 */
	private ParsingProcess.CloserTokensResult getLimitFromCloserTokenEncirclingParentheticallyWellFormedSegment(int index, ParsingProcess process) {
		if (ParserFlags.setLimitBasedOnParentheticalTokens && process.hasNextTokenAccordingToCurrentConditions()) {
			ParsingExpression atIndex = getParsingExpressionArguments().get(index);
			Set<String> closerTokens;
			if (atIndex instanceof Terminal) {
				String terminalValue = (String) atIndex.get(0).getValue();
				String token = process.nextTokenAccordingToCurrentConditions();
				if (token.equals(terminalValue) 
						&& (closerTokens = process.getCloserTokensOf(terminalValue)) != null) {
					
//						System.out.println("Found opener " + terminalValue + ", position " + (process.getTokenPosition()-1) + ". Looking for one of its closers " + Util.join(closerTokens));
						ParsingProcess.CloserTokensResult result = process.moveUpToFirstCloserTokenOrMisplacedCloserTokenOrLimit(closerTokens);
						process.putBack(result.tokens);
						process.putBack(token);
//						if (result.success) {
//							System.out.println("Found at end of " + Util.join(" ", result.tokens));
//							System.out.println("New limit found " + result.position);
//						}
//						else if (result instanceof ParsingProcess.LimitOrEndOfTokensReached){
//							System.out.println("Did not find closer before current limit.");
//						}
//						else if (result instanceof ParsingProcess.MismatchedCloserTokenFound){
//							System.out.println("Found unonpened closer " + ((ParsingProcess.MismatchedCloserTokenFound)result).token + " at end of " + Util.join(" ", result.tokens));
//						}
						return result;
				}
				process.putBack(token);
			}
		}
		return null;
	}
	
	private int getLimitToTokensEqualToNextExpectedTerminal(int index, ParsingProcess process) {
		int limit = process.currentTokenizerPositionLimit();
		int result;
		ParsingExpression nextParsingExpression;
		if (index + 1 < getParsingExpressionArguments().size() // if there is a following sub-parsing expression
				&& (nextParsingExpression = getParsingExpressionArguments().get(index + 1)) instanceof Terminal) { // and it is a terminal
			Terminal terminal = (Terminal) nextParsingExpression;
			String token = terminal.get(0).toString();
			int indexOfLastTokenOccurrenceBeforeLimit = ((DefaultParsingProcess) process).lookAheadForTokenBackwardsFromGivenPosition(token, limit);
//			int indexOfLastTokenOccurrenceBeforeLimitOld = ((DefaultParsingProcess) process).lookAheadForTokenBackwardsFromGivenPositionOld(token, limit);
//			if (indexOfLastTokenOccurrenceBeforeLimitOld != indexOfLastTokenOccurrenceBeforeLimit) {
//				System.out.println("Difference in indexOfLastTokenOccurrenceBeforeLimit");
//				System.out.println("new: " + indexOfLastTokenOccurrenceBeforeLimit);
//				System.out.println("old: " + indexOfLastTokenOccurrenceBeforeLimitOld);
//				Util.fatalError("Difference.");
//			}
			if (indexOfLastTokenOccurrenceBeforeLimit != -1) {
				process.logln("Found token \"" + token + "\" at position " + indexOfLastTokenOccurrenceBeforeLimit);
				result = indexOfLastTokenOccurrenceBeforeLimit;
			}
			else {
				process.logln("There are no tokens of type " + token + " within current limit, so we fail");
				result = -1;
			}
		}
		else {
			result = limit; // default means business as usual -- keep the previous limit
			process.logln("Not followed by terminal, so not changing limit based on token, but simply using pre-existing limit " + limit);
		}
		return result;
	}

	private ParsingResult parsingResultOfSubParsingExpression(
			ParsingExpression subParsingExpression, int index, ParsingProcess process) {
		
		final int lastIndex = numberOfArguments() - 1;

		if (index != 0) { // only first position inherits conditions on first parse, others get clean ones
			process.pushNewConjunctionOfPrecedenceConditionsOnFirstParse();
		}
		if (index != lastIndex) { // only last position inherits conditions on last parse, others get clean ones
			process.pushNewConjunctionOfPrecedenceConditionsOnLastParse();
		}
		
		ParsingResult result = subParsingExpression.parsingResult(process);
		
		if (index != 0) { // get back to previous situation
			process.popConjunctionOfPrecedenceConditionsOnFirstParse();
		}
		if (index != lastIndex) {
			process.popConjunctionOfPrecedenceConditionsOnLastParse();
		}
		return result;
	}

	private ParsingResult makeParsingResult(List<ParsingResult> results, boolean tokenPositionLimitInfluencedResult) {
		if (results.size() == 0) {
			List<String> emptyList = Collections.emptyList();
			return new DefaultParsingResult(this, emptyList, null, tokenPositionLimitInfluencedResult);
		}

		List<Object> subTrees = new LinkedList<Object>();

		for (ParsingResult result : results) {
			if ( ! result.getParsingExpression().hasFunctor("terminal")) {
				Expression subParse = result.getParse();
				subTrees.add(subParse);
			}
		}
		
		String rootTreeName = getRootTreeName();
		
		Expression parse;
		if (subTrees.size() != 0) {
			parse = Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees(rootTreeName, subTrees.toArray());
		}
		else {
			parse = Expressions.createSymbol(rootTreeName);
		}
		
		return new DefaultParsingResult(this, tokens(results), parse, tokenPositionLimitInfluencedResult);
	}	
	
	protected void retract(List<ParsingResult> results, ParsingProcess process) {
		for (int i = results.size(); i != 0; i--) {
			ParsingResult parsingResult = results.get(i - 1);
			parsingResult.putBack(process);
		}
	}

	private void append(StringBuffer rootTree, Object object) {
		String objectString = object.toString();
		if (rootTree.length() != 0 && objectString.length() != 0) {
			rootTree.append(' ');
		}
		rootTree.append(objectString);
	}

	private String trimSpaces(String rootTreeName) {
		int begin, end;

		for (
				end = rootTreeName.length();
				end != 0 && isSpace(rootTreeName, end - 1);
				end--);

		for (
				begin = 0;
				begin != rootTreeName.length() && isSpace(rootTreeName, begin);
				begin++);

		if (begin >= end) {
			return "";
		}
		return rootTreeName.substring(begin, end);
	}

	private String trimDots(String rootTreeName) {
		int begin, end;

		// advance 'begin' to the first position that is not trimmable
		int prefixDotCount = 0;
		for (
				begin = 0;
				begin != rootTreeName.length() && trimmable(rootTreeName, begin);
				begin++) {
			if (rootTreeName.charAt(begin) == '.') {
				prefixDotCount++;
			}
		}

		// recede 'end - 1' to the last position that is not trimmable
		int postfixDotCount = 0;
		for (
				end = rootTreeName.length();
				end != 0 && trimmable(rootTreeName, end - 1);
				end--) {
			if (rootTreeName.charAt(end - 1) == '.') {
				postfixDotCount++;
			}
		}

		// only trim prefix dots if there is a single one on each side
		if ( !(prefixDotCount == 1 && postfixDotCount == 1)) {
			begin = 0;
		}
		
		if (begin >= end) {
			return trimSpaces(rootTreeName);
		}
		
		return rootTreeName.substring(begin, end);
	}

	private boolean isSpace(String rootTreeName, int index) {
		return Character.isSpaceChar(rootTreeName.charAt(index));
	}

	/** Indicates whether a particular position in a string is trimmable. */
	private boolean trimmable(String rootTreeName, int index) {
		return Character.isSpaceChar(rootTreeName.charAt(index)) || rootTreeName.charAt(index) == '.';
	}

	@SuppressWarnings("unchecked")
	@Override
	public String toString(SyntaxTree syntaxTree, Writer writer) {
		StringBuffer result = new StringBuffer();
		int subParsingExpressionIndex = 0;
		int subTreeIndex = 0;
		for (Expression subParsingExpressionAsExpression : getArguments()) {
			ParsingExpression subParsingExpression = (ParsingExpression) subParsingExpressionAsExpression;
			String toBeAppended = null;

			if (subParsingExpression.hasFunctor("terminal")) {
				toBeAppended = subParsingExpression.get(0).toString();
			} 
			else {
				if (subParsingExpression.hasFunctor("kleene")) {
					Expression kleeneList = Expressions.makeFromSyntaxTree(syntaxTree.getSubTree(subTreeIndex));
					Iterator<String> listElementsRepresentationIterator =
						new FunctionIterator(
								Expressions.ensureListFromKleeneList(kleeneList),
								new DefaultWriter.SubTreeRepresentation(
										syntaxTree, writer, subParsingExpressionIndex == 0, false));
					toBeAppended = Util.join(", ", listElementsRepresentationIterator);
				}
				else if (subParsingExpression.hasFunctor("optional") &&
						syntaxTree.getSubTree(subTreeIndex) == null) {
					toBeAppended = "";
				}
				else {
					toBeAppended =
						DefaultWriter.subTreeRepresentation(
								syntaxTree,
								syntaxTree.getSubTree(subTreeIndex), // does need to be sub tree
								writer,
								neighborsAreTerminals(subParsingExpressionIndex)); // no need for parentheses if surrounded by terminals
				}
			}


			// everything is an argument, but terminals.
			if ( ! subParsingExpression.hasFunctor("terminal")) {
				subTreeIndex++;
			}

			
			if ( ! toBeAppended.equals("")) {
				appendToBasicParsingExpressionToStringOutput(result, toBeAppended);
			}
			
			subParsingExpressionIndex++;
		}
		return result.toString();
	}
	
	/** Indicates whether neighboring sub parsing expressions of a given index are terminals. */
	private boolean neighborsAreTerminals(int subParsingExpressionIndex) {
		if (subParsingExpressionIndex - 1 >= 0 && subParsingExpressionIndex + 1 < numberOfArguments()) {
			boolean result =
				hasFunctor(subParsingExpressionIndex - 1, "terminal") &&
				hasFunctor(subParsingExpressionIndex + 1, "terminal");
			return result;
		}
		return false;
	}

	/** Returns whether functor of sub parsing expression (possibly null) of index i is equal to a given one. */
	private boolean hasFunctor(int i, Object functor) {
		Expression subParsingExpression = get(i);
		boolean result = Expressions.hasFunctor(subParsingExpression, functor);
		return result;
	}

	/** Appends a string to a buffer, adding a space if necessary. */
	private void appendToBasicParsingExpressionToStringOutput(StringBuffer buffer, String toAppend) {
		if (needsSpace(buffer, toAppend)) {
			buffer.append(" ");
		}
		buffer.append(toAppend);
	}
	
	/**
	 * Indicates whether a space is needed between current last character in buffer
	 * and new item to be appended.
	 * Currently, this is done every time the buffer is not empty
	 * and the string being appended is not empty either.
	 */
	private boolean needsSpace(StringBuffer buffer, String toAppend) {
		if (buffer.length() == 0 || toAppend.length() == 0) {
			return false;
		}
		return true;
	}

	@Override
	public int computeLengthLowerBoundAfterBookkeeping(Stack<ParsingExpression> beingComputed, ParsingProcess process) {
		int result = 0;
		for (Expression subExpression : this.getArguments()) {
			ParsingExpression subParsingExpression = (ParsingExpression) subExpression;
			int subMinimumLength = subParsingExpression.computeLengthLowerBound(beingComputed, process);
			result = result + subMinimumLength;
		}
		return result;
	}
}
