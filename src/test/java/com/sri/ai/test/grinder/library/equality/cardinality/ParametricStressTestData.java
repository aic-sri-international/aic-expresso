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
package com.sri.ai.test.grinder.library.equality.cardinality;

import com.sri.ai.test.grinder.AbstractGrinderTest;

/**
 * A stress test formed of formulas generated according to a parametric scheme.
 * <p>
 * The generated formulas are either CNF or DNF formulas using a predicate which is either "=" or "!=".
 * Each clause is of the type <code>X1_j = a1_j and X2_j = a2_j ...</code> where <code>j</code> is the clause index
 * (or variants with "!=", or <code>or</code> in the case of CNFs).
 * <p>
 * A maximum number of <code>maximumNumberOfFreeVariables</code> free variables is specified;
 * the generation iterates from 0 to <code>maximumNumberOfFreeVariables</code> free variables
 * (always the last ones in the enumeration of variables).
 * 
 * @author oreilly
 */
public class ParametricStressTestData extends DefaultCardinalityStressTestData {
	
	public ParametricStressTestData(
			String formulaType, String equalityOrInequality, String interClauseOperator, String predicate, String clauseOperator,
			int initialNumberOfClauses, int maximumNumberOfClauses, int clauseSize, int maximumNumberOfFreeVariables) {
		
		this.title = "Times Cardinality of " + formulaType + " on " + equalityOrInequality + " formulas with clauses with two " +
				clauseSize + " literals and up to " + maximumNumberOfFreeVariables + " free variables";
		
		for (int numberOfFreeVariables = 0; numberOfFreeVariables <= maximumNumberOfFreeVariables; numberOfFreeVariables++) {
			for (int cardinalityIndex = initialNumberOfClauses; cardinalityIndex <= maximumNumberOfClauses; cardinalityIndex++) {
				String cardinalityExpression =
						generateBasicCardinalityExpression(
								cardinalityIndex, interClauseOperator, clauseSize, predicate, clauseOperator, numberOfFreeVariables);
				cardinalityExpressions.add(cardinalityExpression);
			}
		}
		
		this.expectedExpressions = new String[cardinalityExpressions.size()];
		for (int i = 0; i < this.expectedExpressions.length; i++) {
			this.expectedExpressions[i] = AbstractGrinderTest.IGNORE_EXPECTED;
		}
		
		computeMaximumFormulaLength();
	}
	
	public ParametricStressTestData(
			String formulaType, String equalityOrInequality, String interClauseOperator, String predicate, String clauseOperator,
			int initialNumberOfClauses, int maximumNumberOfClauses, int clauseSize, int maximumNumberOfFreeVariables, String[] expected) {
		
		this(formulaType, equalityOrInequality, interClauseOperator, predicate, clauseOperator,
				initialNumberOfClauses, maximumNumberOfClauses, clauseSize, maximumNumberOfFreeVariables);
		
		if (expected.length != this.expectedExpressions.length) {
			throw new IllegalArgumentException("'expected' is not the correct length; it should be " + this.expectedExpressions.length);
		}
		
		for (int i = 0; i < this.expectedExpressions.length; i++) {
			this.expectedExpressions[i] = expected[i];
		}
	}
	
	private String generateBasicCardinalityExpression(
			int numberOfClauses, String interClauseOperator, int clauseSize, String predicate, String clauseOperator, int numberOfFreeVariables) {
		
		StringBuilder stringBuilder = new StringBuilder();
		
		stringBuilder.append("| {{(on ");
		int numberOfCardinalityIndices = (numberOfClauses * clauseSize) - numberOfFreeVariables;
		int numberOfAlreadyGeneratedIndices = 0;
		for (int clauseIndex = 0; clauseIndex < numberOfClauses; clauseIndex++) {
			for (int prefixIndex = 0; prefixIndex < clauseSize; prefixIndex++) {
				if (numberOfAlreadyGeneratedIndices < numberOfCardinalityIndices) {
					if (!(clauseIndex == 0 && prefixIndex == 0)) {
						stringBuilder.append(", ");
					}
					stringBuilder.append("X" + prefixIndex + "_");
					stringBuilder.append(clauseIndex + 1);
				}
				numberOfAlreadyGeneratedIndices++;
			}
		}
		
		stringBuilder.append(") tuple(");
		numberOfAlreadyGeneratedIndices = 0;
		for (int clauseIndex = 0; clauseIndex < numberOfClauses; clauseIndex++) {
			for (int prefixIndex = 0; prefixIndex < clauseSize; prefixIndex++) {
				if (numberOfAlreadyGeneratedIndices < numberOfCardinalityIndices) {
					if (!(clauseIndex == 0 && prefixIndex == 0)) {
						stringBuilder.append(", ");
					}
					stringBuilder.append("X" + prefixIndex + "_");
					stringBuilder.append(clauseIndex + 1);
				}
				numberOfAlreadyGeneratedIndices++;
			}
		}
		
		stringBuilder.append(") | ");
		stringBuilder.append(generateBasicFormula(numberOfClauses, interClauseOperator, clauseSize, predicate, clauseOperator));
		stringBuilder.append(" }} |");
		
		return stringBuilder.toString();
	}

	//
	// PRIVATE METHODS
	//
	private String generateBasicFormula(int numberOfClauses, String interClauseOperator, int clauseSize, String predicate, String clauseOperator) {
		StringBuilder stringBuilder = new StringBuilder();
		
		for (int clauseIndex = 0; clauseIndex < numberOfClauses; clauseIndex++) {
			stringBuilder.append("(");
			for (int literalIndex = 0; literalIndex < clauseSize; literalIndex++) {
				stringBuilder.append("X" + literalIndex + "_");
				stringBuilder.append(clauseIndex + 1);
				stringBuilder.append(" ");
				stringBuilder.append(predicate);
				stringBuilder.append(" ");
				stringBuilder.append("a" + literalIndex + "_");
				stringBuilder.append(clauseIndex + 1);
				if (literalIndex < (clauseSize - 1)) {
					stringBuilder.append(" ");
					stringBuilder.append(clauseOperator);
					stringBuilder.append(" ");
				}
			}
			stringBuilder.append(")");
			
			if (clauseIndex < (numberOfClauses - 1)) {
				stringBuilder.append(" ");
				stringBuilder.append(interClauseOperator);
				stringBuilder.append(" ");
			}
		}
		
		return stringBuilder.toString();
	}
}
