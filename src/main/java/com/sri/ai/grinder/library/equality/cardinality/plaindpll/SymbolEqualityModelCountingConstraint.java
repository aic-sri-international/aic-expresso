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
package com.sri.ai.grinder.library.equality.cardinality.plaindpll;

import static com.sri.ai.expresso.helper.Expressions.apply;
import static com.sri.ai.expresso.helper.Expressions.makeSymbol;
import static com.sri.ai.grinder.library.FunctorConstants.CARDINALITY;
import static com.sri.ai.util.Util.getOrUseDefault;
import static java.util.Collections.emptyList;

import java.util.ArrayList;
import java.util.Collection;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.helper.GrinderUtil;
import com.sri.ai.grinder.library.number.Minus;
import com.sri.ai.grinder.library.number.Times;

@SuppressWarnings("serial")
/**
 * Represents and manipulates constraints in the theory of equalities of symbols (variables and constants) for model counting purposes.
 * 
 * It does this by keeping the equivalent of a conjunction of disequalities in a map from each variable to
 * the terms constraint to be distinct from it that are defined before it in the choosing order
 * used for counting the number of solutions.
 * The ordering chosen is the String order for the String representation of the terms.
 * This is useful because we use the Counting Principle for each variable in the total ordering chosen.
 * For each variable, we have N - D options, where N is its type size and D is the number of values it must be distinct from.
 * These values are the constants it is constrained to be distinct from, and variables that have their <i>already chosen</i>
 * (coming in the ordering first), assuming that their values do not overlap.
 * This counting will only be correct, however, if there is a guarantee that these predefined distinct terms are constrained to be distinct from each
 * other (a condition similar but not identical to what is called "normalized constraints" in the lifted inference literature).
 * The algorithm does check and enforces this guarantee.
 */
@Beta
public class SymbolEqualityModelCountingConstraint extends AbstractSymbolEqualityConstraint {

	@Override
	public AbstractSymbolEqualityConstraint make() {
		return new SymbolEqualityModelCountingConstraint();
	}

	@Override
	public AbstractSymbolEqualityConstraint make(AbstractSymbolEqualityConstraint another) {
		return new SymbolEqualityModelCountingConstraint(this);
	}

	public SymbolEqualityModelCountingConstraint() {
		super();
	}

	public SymbolEqualityModelCountingConstraint(SymbolEqualityModelCountingConstraint another) {
		super(another);
	}

	public SymbolEqualityModelCountingConstraint(Expression disequalitiesConjunction, Collection<Expression> indices, RewritingProcess process) {
		super(disequalitiesConjunction, indices, process);
	}

	@Override
	public Expression pickSplitter(Collection<Expression> indices, RewritingProcess process) {

		for (Expression index : indices) {
			
			Collection<Expression> distinctPredefinedTermsForVariable1 = getDistinctPredefinedTermsFrom(index, this);
			
			for (Expression distinctPredefinedVariable2 : distinctPredefinedTermsForVariable1) {
				if (process.isVariable(distinctPredefinedVariable2)) {
			
					Expression distinctPredefinedTermForVariable1ThatIsNotVariable2AndIsNotDistinctPredefinedForVariable2 =
							lookForTermInCollectionThatIsNotVariableAndIsNotDistinctFromVariable(distinctPredefinedTermsForVariable1, distinctPredefinedVariable2);
					
					if (distinctPredefinedTermForVariable1ThatIsNotVariable2AndIsNotDistinctPredefinedForVariable2 != null) {
						
						Expression atom =
								PlainCardinalityDPLL
								.makeSplitterWithIndexIfAnyComingFirst(
										distinctPredefinedVariable2,
										distinctPredefinedTermForVariable1ThatIsNotVariable2AndIsNotDistinctPredefinedForVariable2, indices);

						return atom;
					}
				}
			}
		}
		
		return null;
	}

	private Expression lookForTermInCollectionThatIsNotVariableAndIsNotDistinctFromVariable(Collection<Expression> terms, Expression variable) {
		Collection<Expression> distinctPredefinedTermsForVariable2 = getDistinctPredefinedTermsFrom(variable, this);
		Expression result =
				getDistinctPredefinedTermForVariable1ThatIsNotVariable2AndIsNotDistinctFromVariable2(
						terms, distinctPredefinedTermsForVariable2, variable, this);
		return result;
	}

	private static Times timesRewriter = new Times();

	@Override
	public Expression solution(Collection<Expression> indices, RewritingProcess process) {
		
		ArrayList<Expression> indexNumbersOfPossibleValues = new ArrayList<Expression>(indices.size());
		
		for (Expression index : indices) {
			Collection<Expression> setOfDistinctTerms = get(index);
			long numberOfNonAvailableValues = setOfDistinctTerms == null? 0 : (long) setOfDistinctTerms.size();

			long typeSize = GrinderUtil.getTypeCardinality(index, process);
			Expression indexNumberOfPossibleValues;
			if (typeSize == -1) {
				Expression indexType = process.getContextualSymbolType(index);
				if (indexType == null) {
					throw new Error("Type of " + index + " unknown but needed for symbolic cardinality computation.");
				}
				Expression indexTypeCardinality = apply(CARDINALITY, indexType);
				indexNumberOfPossibleValues = Minus.make(indexTypeCardinality, Expressions.makeSymbol(numberOfNonAvailableValues));
			}
			else {
				indexNumberOfPossibleValues = makeSymbol(typeSize - numberOfNonAvailableValues);
			}
			
			indexNumbersOfPossibleValues.add(indexNumberOfPossibleValues);
		}
		
		Expression result = Times.make(indexNumbersOfPossibleValues);
		result = timesRewriter.rewrite(result, process);
		
		return result;
	}

	private static Expression getDistinctPredefinedTermForVariable1ThatIsNotVariable2AndIsNotDistinctFromVariable2(
			Collection<Expression> distinctPredefinedTermsForVariable1,
			Collection<Expression> distinctPredefinedTermsForVariable2,
			Expression variable2,
			TheoryConstraint constraint) {
	
		for (Expression distinctPredefinedTermForVariable1 : distinctPredefinedTermsForVariable1) {
			if ( ! distinctPredefinedTermForVariable1.equals(variable2)) {
				if (distinctPredefinedTermForVariable1IsNotDistinctFromVariable2(distinctPredefinedTermForVariable1, variable2, distinctPredefinedTermsForVariable2, constraint)) {
					return distinctPredefinedTermForVariable1;
				}
			}
		}
		return null;
	}

	private static boolean distinctPredefinedTermForVariable1IsNotDistinctFromVariable2(
			Expression distinctPredefinedTermForVariable1, Expression variable2,
			Collection<Expression> distinctPredefinedTermsForVariable2, TheoryConstraint constraint) {
	
		if ( ! distinctPredefinedTermsForVariable2.contains(distinctPredefinedTermForVariable1)) {
			Collection<Expression> distinctPredefinedTermsForDistinctPredefinedTermForVariable1 =
					getDistinctPredefinedTermsFrom(distinctPredefinedTermForVariable1, constraint);
			if ( ! distinctPredefinedTermsForDistinctPredefinedTermForVariable1.contains(variable2)) {
				return true;
			}
		}
		return false;
	}

	private static Collection<Expression> getDistinctPredefinedTermsFrom(Expression variable, TheoryConstraint constraint) {
		Collection<Expression> result = getOrUseDefault(((SymbolEqualityModelCountingConstraint) constraint), variable, emptyList());
		return result;
	}
}