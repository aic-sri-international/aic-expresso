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
package com.sri.ai.brewer.core;

import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import com.google.common.annotations.Beta;
import com.sri.ai.brewer.api.BasicParsingExpression;
import com.sri.ai.brewer.api.Grammar;
import com.sri.ai.brewer.api.ParsingExpression;
import com.sri.ai.brewer.api.ParsingProcess;
import com.sri.ai.brewer.parsingexpression.core.Disjunction;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.SyntaxTree;
import com.sri.ai.expresso.core.DefaultSymbol2;
import com.sri.ai.expresso.helper.SubExpressionsDepthFirstIterator;
import com.sri.ai.util.Util;
import com.sri.ai.util.base.BinaryPredicate;
import com.sri.ai.util.base.LessThanOnIntegers;
import com.sri.ai.util.base.LessThanOrEqualToOnIntegers;
import com.sri.ai.util.base.Pair;
import com.sri.ai.util.collect.ManyToManyRelation;

/**
 * A default implementation for grammars.
 * 
 * @author braz
 */
@Beta
public class DefaultGrammar extends HashMap<String, ParsingExpression> implements Grammar {
	private static final long serialVersionUID = 1L;

	private Map<Pair<SyntaxTree,Integer>, BasicParsingExpression> basicParsingExpressionOfRootTreeNameAndArity = new HashMap<Pair<SyntaxTree,Integer>, BasicParsingExpression>();

	private ManyToManyRelation<ParsingExpression, ParsingExpression> disjunctsAndDisjunctions = new ManyToManyRelation<ParsingExpression, ParsingExpression>();
	private Map<List<ParsingExpression>, Integer> indexOfDisjunctInDisjunction = new HashMap<List<ParsingExpression>, Integer>();

	private String initialNonTerminal;
	
	@Override
	public ParsingExpression put(String name, ParsingExpression parsingExpression) {
		register(parsingExpression);
		return super.put(name, parsingExpression);
	}

	private void register(ParsingExpression parsingExpression) {
		registerRootTreeNamesAndArities(parsingExpression);
		registerDisjunctsAndDisjunctions(parsingExpression);
	}

	private void registerRootTreeNamesAndArities(ParsingExpression parsingExpression) {
		SubExpressionsDepthFirstIterator subParsingExpressionIterator = new SubExpressionsDepthFirstIterator(parsingExpression);
		while (subParsingExpressionIterator.hasNext()) {
			Expression subParsingExpression = subParsingExpressionIterator.next();
			if (subParsingExpression instanceof BasicParsingExpression) {
				BasicParsingExpression basicParsingExpression = (BasicParsingExpression) subParsingExpression;
				Pair<SyntaxTree, Integer> pair = makeRootTreeAndArityPair(basicParsingExpression);
				basicParsingExpressionOfRootTreeNameAndArity.put(
						pair,
						basicParsingExpression);
			}
		}
	}

	private void registerDisjunctsAndDisjunctions(ParsingExpression parsingExpression) {
		SubExpressionsDepthFirstIterator subParsingExpressionIterator = new SubExpressionsDepthFirstIterator(parsingExpression);
		while (subParsingExpressionIterator.hasNext()) {
			Expression subParsingExpression = subParsingExpressionIterator.next();
			if (subParsingExpression instanceof Disjunction) {
				registerDisjunction((Disjunction) subParsingExpression);
			}
		}
	}

	private void registerDisjunction(Disjunction disjunction) {
		int index = 0;
		for (Expression disjunct : disjunction.getArguments()) {
			disjunctsAndDisjunctions.add((ParsingExpression) disjunct, disjunction);
			List<ParsingExpression> key = Util.list((ParsingExpression) disjunct, disjunction);
			indexOfDisjunctInDisjunction.put(key, index);
			index++;
		}
	}

	@Override
	public ParsingExpression remove(Object name) {
		unregister(name);
		return super.remove(name);
	}

	private void unregister(Object name) {
		unregisterRootTreeNamesAndArities(get(name));
		unregisterDisjunctsAndDisjunctions(get(name));
	}
	
	private void unregisterRootTreeNamesAndArities(ParsingExpression parsingExpression) {
		SubExpressionsDepthFirstIterator subParsingExpressionIterator = new SubExpressionsDepthFirstIterator(parsingExpression);
		while (subParsingExpressionIterator.hasNext()) {
			Expression subParsingExpression = subParsingExpressionIterator.next();
			if (subParsingExpression instanceof BasicParsingExpression) {
				BasicParsingExpression basicParsingExpression = (BasicParsingExpression) subParsingExpression;
				Pair<SyntaxTree, Integer> pair = makeRootTreeAndArityPair(basicParsingExpression);
				basicParsingExpressionOfRootTreeNameAndArity.remove(pair);
			}
		}
	}
	
	private void unregisterDisjunctsAndDisjunctions(ParsingExpression parsingExpression) {
		SubExpressionsDepthFirstIterator subParsingExpression = new SubExpressionsDepthFirstIterator(parsingExpression);
		while (subParsingExpression.hasNext()) {
			Expression subExpression = subParsingExpression.next();
			if (subExpression instanceof Disjunction) {
				unregisterDisjunction((Disjunction) subExpression);
			}
		}
	}

	private void unregisterDisjunction(Disjunction disjunction) {
		for (Expression disjunct : disjunction.getArguments()) {
			disjunctsAndDisjunctions.remove((ParsingExpression) disjunct, disjunction);
			indexOfDisjunctInDisjunction.remove(Util.list(disjunct, disjunction));
		}
	}

	private Pair<SyntaxTree, Integer> makeRootTreeAndArityPair(
			BasicParsingExpression basicParsingExpression) {
		SyntaxTree rootTree = DefaultSymbol2.createSymbol(basicParsingExpression.getRootTreeName());
		Pair<SyntaxTree, Integer> pair = Pair.make(rootTree, basicParsingExpression.getArity());
		return pair;
	}

	@Override
	public BasicParsingExpression getBasicParsingExpressionFor(SyntaxTree syntaxTree) {
		SyntaxTree rootTree;
		if (syntaxTree == null ||
				syntaxTree.numberOfImmediateSubTrees() == 0 ||
				(rootTree = syntaxTree.getRootTree()) == null) {
			return null;
		}
		
		Pair<SyntaxTree, Integer> arityPair =
			Pair.make(rootTree, syntaxTree.numberOfImmediateSubTrees());
		BasicParsingExpression result =
			basicParsingExpressionOfRootTreeNameAndArity.get(arityPair);
		if (result == null) {
			// not found under a specific arity; check to see if this functor has variable arity.
			Pair<SyntaxTree, Integer> variableArityPair = Pair.make(rootTree, -1);
			result =
				basicParsingExpressionOfRootTreeNameAndArity.get(variableArityPair);
		}
		return result;
	}


	@Override
	public boolean precedenceIsLessThan(SyntaxTree syntaxTree1, SyntaxTree syntaxTree2) {
		return precedenceComparison(syntaxTree1, syntaxTree2, new LessThanOnIntegers());
	}

	@Override
	public boolean precedenceIsLessThanOrEqualTo(SyntaxTree syntaxTree1, SyntaxTree syntaxTree2) {
		return precedenceComparison(syntaxTree1, syntaxTree2, new LessThanOrEqualToOnIntegers());
	}

	private boolean precedenceComparison(SyntaxTree syntaxTree1, SyntaxTree syntaxTree2, BinaryPredicate<Integer,Integer> comparison) {
		ParsingExpression parsingExpression1 = getBasicParsingExpressionFor(syntaxTree1);
		if (parsingExpression1 == null) {
			return false;
		}
		
		ParsingExpression parsingExpression2 = getBasicParsingExpressionFor(syntaxTree2);
		if (parsingExpression2 == null) {
			return false;
		}
		
		Collection<ParsingExpression> disjunctions1 = disjunctsAndDisjunctions.getBsOfA(parsingExpression1);
		if (disjunctions1.isEmpty()) {
			return false;
		}
		Collection<ParsingExpression> disjunctions2 = disjunctsAndDisjunctions.getBsOfA(parsingExpression2);
		if (disjunctions2.isEmpty()) {
			return false;
		}
		Collection<ParsingExpression> intersection = Util.intersection(disjunctions1, disjunctions2);
		if (intersection.isEmpty()) {
			return false;
		}
		boolean precedenceIsLessThan = false; // this initial value is irrelevant, since it will be overwritten for sure.
		boolean foundPositiveCase    = false; // indicates whether precedence        satisfies comparison for some disjunction seen so far.
		boolean foundNegativeCase    = false; // indicates whether precedence does not satisfy comparison for some disjunction seen so far.
		for (ParsingExpression disjunction : intersection) {
			int indexOfDisjunct1 = indexOfDisjunctInDisjunction.get(Util.list(parsingExpression1, disjunction));
			int indexOfDisjunct2 = indexOfDisjunctInDisjunction.get(Util.list(parsingExpression2, disjunction));
			boolean comparisonResult = comparison.apply(indexOfDisjunct1, indexOfDisjunct2);
			if ((comparisonResult && foundNegativeCase) ||
					(!comparisonResult && foundPositiveCase)) {
				throw new Error("CommonGrammar.precedenceComparison: " + parsingExpression1 + " and " + parsingExpression2 + " have different precedence relation depending on which disjunction is being considered: " + Util.join(", ", intersection));
			}
			// keeps track of what has been seen so far:
			foundPositiveCase = foundPositiveCase ||  comparisonResult;
			foundNegativeCase = foundNegativeCase || !comparisonResult;
			
			// updates precedenceIsLessThan but goes on with loop to make sure all disjunctions agree.
			precedenceIsLessThan = comparisonResult;
		}
		return precedenceIsLessThan; // returns value indicated by all disjunctions.
	}

	@Override
	public String getInitialNonTerminal() {
		return initialNonTerminal;
	}

	@Override
	public void setInitialNonTerminal(String nonTerminal) {
		this.initialNonTerminal = nonTerminal;		
	}

	@Override
	public Expression parse(String expressionString) {
		ParsingProcess process = new DefaultParsingProcess(expressionString, this);
		Expression result = process.parseOfNonTerminal(initialNonTerminal);
		return result;
	}
}
