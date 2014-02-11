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
package com.sri.ai.grinder.library.boole.core;

import java.util.ArrayList;
import java.util.Collections;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Random;
import java.util.Set;

import com.google.common.annotations.Beta;
import com.sri.ai.brewer.BrewerConfiguration;
import com.sri.ai.brewer.api.Grammar;
import com.sri.ai.brewer.api.Writer;
import com.sri.ai.brewer.core.CommonGrammar;
import com.sri.ai.brewer.core.DefaultWriter;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.core.DefaultSymbol;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.grinder.library.Disequality;
import com.sri.ai.grinder.library.Equality;
import com.sri.ai.grinder.library.boole.And;
import com.sri.ai.grinder.library.boole.Equivalence;
import com.sri.ai.grinder.library.boole.Implication;
import com.sri.ai.grinder.library.boole.Not;
import com.sri.ai.grinder.library.boole.Or;
import com.sri.ai.grinder.library.equality.cardinality.CardinalityUtil;

/**
 * A simple formula generator that creates random CNF and DNF formulas with
 * equality.
 * 
 * @author saadati
 * 
 */
@Beta
public class SimpleFormulaGenerator {
	private Random rand = new Random();
	private List<String> constants = null;
	private List<String> variables = null;
	private ArrayList<String> terms = new ArrayList<String>();
	private static boolean ALLOW_IMPLICATION_AND_EQUIVALENCE = true;
	private static final int MAXIMUM_NUMBER_OF_TYPES_OF_FORMULAS = 5;
	private static final int USE_NEGATION = 0;
	private static final int USE_IMPLICATION = 1;
	private static final int USE_EQUIVALENCE = 2;
	private static final int USE_CNF = 3;
	private static final int USE_DNF = 4;
	
	public static void main(String[] args) {
		Grammar grammar = new CommonGrammar();
		BrewerConfiguration.setProperty(BrewerConfiguration.KEY_DEFAULT_GRAMMAR_CLASS, grammar.getClass().getName());
		Writer writer = DefaultWriter.newDefaultConfiguredWriter();
		
		SimpleFormulaGenerator gn = new SimpleFormulaGenerator(20, 20);
		for (int i = 30; i<90; i+=3) {
			Expression formula = gn.generateRandomFormulas(i);
			Expression ex = gn.generateRandomCardinality(formula, false);
			System.out.println("\"" + writer.toString(ex) + "\",");
		}
		System.out.println("\n");
		for (int i = 20; i<60; i+= 3) {
			Expression formula = gn.generateConjunctionOfDisequalities(i);
			Expression ex = gn.generateRandomCardinality(formula, false);
			System.out.println("\"" + writer.toString(ex) + "\",");
		}	
		
		for (int i = 30; i<60; i+= 3) {
			Expression formula = gn.generateCNF(i, i);
			Expression ex = gn.generateRandomCardinality(formula, false);
			System.out.println("\"" + writer.toString(ex) + "\",");
		}	
	}
	
	public SimpleFormulaGenerator(int constN, int varN) {
		constants = new ArrayList<String>();
		variables = new ArrayList<String>();
		for (int i=0; i<constN; i++) {
			constants.add("a" + (i+1));
		}
		for (int i=0; i<varN; i++) {
			variables.add("X" + (i+1));
		}
		terms.addAll(constants);
		terms.addAll(variables);
		Collections.shuffle(terms);		
	}
	
	public SimpleFormulaGenerator(List<String> c, List<String> v) {
		constants = c;
		variables = v;
		terms.addAll(constants);
		terms.addAll(variables);
		Collections.shuffle(terms);
	}

	/**
	 * Create a string that is represents the cardinality of an intensional set
	 * @param formula
	 * 			the formula that represents the intentional set
	 * @param possibleFreeVariables
	 * 			if true, the final expression may have some free variables
	 * @return the expression representing the cardinality of an intensional set          
	 */
	public Expression generateRandomCardinality(Expression formula, boolean possibleFreeVariables) {
		List<Expression> indices = new ArrayList<Expression>();
		if ( possibleFreeVariables ) {
			for (String v: variables) {
				if ( rand.nextInt(3) != 0 ) {
					indices.add(DefaultSymbol.createSymbol(v));
				}
			}
		}
		else {
			indices.addAll(getVariables(formula));
		}
		Expression result = CardinalityUtil.makeCardinalityOfIndexedFormulaExpression(formula, indices.toArray(new Expression[indices.size()]));
		return result;
	}
	
	/**
	 * Create a random quantifier-free formula in equality logic
	 * @param approximateLength
	 * 			specifies the approximate length for the formula
	 * @return the expression representing a quantifier-free formula in equality logic         
	 */
	public Expression generateRandomFormulas(int approximateLength) {
		Expression result = null;
		int selector = rand.nextInt(MAXIMUM_NUMBER_OF_TYPES_OF_FORMULAS);
		if ( approximateLength < 10 ) {
			do {
				selector = rand.nextInt(2) + 3;
			} while ( ALLOW_IMPLICATION_AND_EQUIVALENCE || (selector != USE_IMPLICATION && selector != USE_EQUIVALENCE) );
		}
		switch(selector) {
		case USE_NEGATION:
			result = generateRandomFormulas(approximateLength - 2);
			break;
		case USE_IMPLICATION:
			Expression f1 = generateRandomFormulas(approximateLength / 2);
			Expression f2 = generateRandomFormulas(approximateLength / 2);
			result = generateImplication(f1, f2);
			break;
		case USE_EQUIVALENCE:
			Expression e1 = generateRandomFormulas(approximateLength / 2);
			Expression e2 = generateRandomFormulas(approximateLength / 2);
			result = generateEquivalence(e1, e2);
			break;
		case USE_CNF:
			int sz = (int) Math.sqrt(approximateLength);
			result = generateCNF(sz + 1, sz);
			break;
		case USE_DNF:
			int dz = (int) Math.sqrt(approximateLength);
			result = generateDNF(dz + 1, dz);
			break;
			
		}
		return result;
	}
	

	/**
	 * Create a random conjunction of disequalities in equality logic
	 * @param numberOfDisequalities
	 * 			number of disequalities in the formula
	 * @return the expression representing a conjunction of disequalities in equality logic         
	 */
	public Expression generateConjunctionOfDisequalities(int numberOfDisequalities) {
		Expression[] dis = new Expression[numberOfDisequalities];
		for (int i=0; i<numberOfDisequalities; i++) {
			
			dis[i] = Disequality.make(getRandomTerm(), getRandomTerm());
		}
		return And.make(dis);
	}
	
	/**
	 * Create a random CNF formula in equality logic
	 * @param numberOfLiterals
	 * 			number of literals in the formula
	 * @return the expression representing a CNF formula in equality logic         
	 */
	public Expression generateCNF(int numberOfLiterals) {
		Expression result = generateXNF(numberOfLiterals, USE_CNF);
		return result;
	}
	
	/**
	 * Create a random DNF formula in equality logic
	 * @param numberOfLiterals
	 * 			number of literals in the formula
	 * @param maxLiteralsInEachClause
	 * 			maximum number of literals in each clause
	 * @return the expression representing a DNF formula in equality logic         
	 */
	public Expression generateDNF(int numberOfLiterals) {
		Expression result = generateXNF(numberOfLiterals, USE_DNF);
		return result;
	}
	
	/**
	 * Create a random CNF formula in equality logic
	 * @param numberOfClauses
	 * 			number of clauses in the formula
	 * @param maxLiteralsInEachClause
	 * 			maximum number of literals in each clause
	 * @return the expression representing a CNF formula in equality logic         
	 */
	public Expression generateCNF(int numberOfClauses, int maxLiteralsInEachClause) {
		Expression result = generateXNF(numberOfClauses, maxLiteralsInEachClause, USE_CNF);
		return result;
	}
	
	/**
	 * Create a random DNF formula in equality logic
	 * @param numberOfClauses
	 * 			number of clauses in the formula
	 * @param maxLiteralsInEachClause
	 * 			maximum number of literals in each clause
	 * @return the expression representing a DNF formula in equality logic         
	 */
	public Expression generateDNF(int numberOfClauses, int maxLiteralsInEachClause) {
		Expression result = generateXNF(numberOfClauses, maxLiteralsInEachClause, USE_DNF);
		return result;
	}
	
	protected Expression generateNegation(Expression expression) {
		Expression result = Not.make(expression);
		return result;
	}
	
	protected Expression generateSimpleImplication() {
		Expression result = Implication.make(generateLiteral() , generateLiteral());
		return result;
	}
	
	protected Expression generateImplication(Expression f1, Expression f2) {
		Expression result = Implication.make(f1, f2);
		return result;
	}
	
	protected Expression generateSimpleEquivalence() {
		Expression result = Expressions.make(Equivalence.FUNCTOR, generateLiteral() , generateLiteral());
		return result;
	}
	
	protected Expression generateEquivalence(Expression f1, Expression f2) {
		Expression result = Expressions.make(Equivalence.FUNCTOR, f1, f2);
		return result;
	}
	
	protected Expression generateXNF(int numberOfClauses, int literalsInEachClause, int type) {
		Expression result = null;
		Object clauseConnector = null, literalConnector = null;
		switch ( type ) {
		case USE_CNF:
			clauseConnector = And.FUNCTOR;
			literalConnector = Or.FUNCTOR;
			break;
		case USE_DNF:
			clauseConnector = Or.FUNCTOR;
			literalConnector = And.FUNCTOR;
			break;
		}
		ArrayList<Expression> clauses = new ArrayList<Expression>();
		for (int i=0; i<numberOfClauses; i++) {
			Expression clause = generateClause(literalsInEachClause, literalConnector);
			clauses.add(clause);
		}

		if ( clauses.size()==1 ) {
			result = clauses.get(0);
		} 
		else if ( clauses.size()==0 ) {
			if ( clauses.equals(And.FUNCTOR) ) {
				result = Expressions.TRUE;
			} 
			else {
				result = Expressions.FALSE;
			}
		} 
		else {
			result = Expressions.make(clauseConnector, clauses.toArray());
		}
		
		return result;
	}
	
	protected Expression generateXNF(int numberOfLiterals, int type) {
		Expression result = null;
		Object clauseConnector = null, literalConnector = null;
		switch ( type ) {
		case USE_CNF:
			clauseConnector = And.FUNCTOR;
			literalConnector = Or.FUNCTOR;
			break;
		case USE_DNF:
			clauseConnector = Or.FUNCTOR;
			literalConnector = And.FUNCTOR;
			break;
		}
		ArrayList<Expression> literals = new ArrayList<Expression>();
		for (int i=0; i<numberOfLiterals; i++) {
			Expression literal = generateLiteral();
			literals.add(literal);
		}
		int numberOfClauses = rand.nextInt(numberOfLiterals) + 1;
		ArrayList<ArrayList<Expression>> clausesList = new ArrayList<ArrayList<Expression>>();
		for (int i=0; i< numberOfClauses; i++) {
			clausesList.add(new ArrayList<Expression>());
			clausesList.get(i).add(literals.get(i));
		}
		for (int i=numberOfClauses; i<numberOfLiterals; i++) {
			int k = rand.nextInt(numberOfClauses);
			clausesList.get(k).add(literals.get(i));			
		}
		
		ArrayList<Expression> clauses = new ArrayList<Expression>();
		for (int i=0; i<numberOfClauses; i++) {
			
			Expression clause = null;
			if ( clausesList.get(i).size() == 1 ) {
				clause = clausesList.get(i).get(0);
			}
			else {
				clause = Expressions.make(literalConnector, clausesList.get(i).toArray());
			}
			clauses.add(clause);
		}

		if ( clauses.size()==1 ) {
			result = clauses.get(0);
		} 
		else if ( clauses.size()==0 ) {
			if ( clauses.equals(And.FUNCTOR) ) {
				result = Expressions.TRUE;
			} 
			else {
				result = Expressions.FALSE;
			}
		} 
		else {
			result = Expressions.make(clauseConnector, clauses.toArray());
		}
		
		return result;
	}
	
	protected Set<Expression> getVariables(Expression formula) {
		Set<Expression> vars = new LinkedHashSet<Expression>();
		if ( formula.hasFunctor(Equality.FUNCTOR) || formula.hasFunctor(Disequality.FUNCTOR) ) {
			if ( variables.contains(formula.get(0).toString()) ) {
				vars.add(formula.get(0));
			}
			if ( variables.contains(formula.get(1).toString()) ) {
				vars.add(formula.get(1));
			}
		} 
		else {
			for (Expression subformula: formula.getArguments()) {
				vars.addAll(getVariables(subformula));
			}
		}
		return vars;
	}
	
	protected Expression generateLiteral() {
		Expression literal = null;
		int termsN = terms.size();
		while ( literal == null ) {
			int decider = rand.nextInt(3);
			if ( decider != 0 ) {
				int x = rand.nextInt(variables.size());
				int y = rand.nextInt(termsN);
				if ( x != y ) {
					String term1 = variables.get(x);  // getRandomTerm();
					String term2 = terms.get(y);
					if ( !term1.equals(term2) ) {
						if ( decider == 1 ) {
							literal = Expressions.apply("=", term1, term2);
						}
						else {
							literal = Expressions.apply("!=", term1, term2);
						}
					}
				}
			}
		}
		return literal;
	}
	
	protected String getRandomTerm() {
		int x = rand.nextInt(terms.size());
		String term = terms.get(x);
		return term;
	}
	
	protected Expression generateClause(int literalsInClause, Object connector) {
		Expression result = null;
		int numberOfLiterals = literalsInClause; //rand.nextInt(maxLiteralsInClause) + 1;
		ArrayList<Expression> literals = new ArrayList<Expression>();
		while ( literals.size() < numberOfLiterals ) {
			Expression literal = generateLiteral();
			literals.add(literal);
		}
		if ( literals.size()==1 ) {
			result = literals.get(0);
		} 
		else if ( literals.size()==0 ) {
			if ( connector.equals(And.FUNCTOR) ) {
				result = Expressions.TRUE;
			} 
			else {
				result = Expressions.FALSE;
			}
		} 
		else {
			result = Expressions.make(connector, literals.toArray());
		}
		return result;
	}
	
}
