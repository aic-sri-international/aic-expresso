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
package com.sri.ai.grinder.library;

import java.util.HashMap;
import java.util.Map;

import com.google.common.annotations.Beta;
import com.google.common.base.Predicate;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.grinder.api.Rewriter;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.core.DefaultRewriterLookup;
import com.sri.ai.grinder.core.DefaultRewritingProcess;
import com.sri.ai.grinder.core.PrologConstantPredicate;
import com.sri.ai.grinder.library.equality.cardinality.direct.CardinalityConfiguration;
import com.sri.ai.grinder.library.equality.cardinality.direct.CardinalityRewriter;
import com.sri.ai.grinder.library.equality.cardinality.direct.core.Cardinality;
import com.sri.ai.grinder.library.equality.cardinality.direct.core.CardinalityConjunction;
import com.sri.ai.grinder.library.equality.cardinality.direct.core.CardinalityConjunctionOfDisequalities;
import com.sri.ai.grinder.library.equality.cardinality.direct.core.CardinalityDisjunction;
import com.sri.ai.grinder.library.equality.cardinality.direct.core.CardinalityEquivalence;
import com.sri.ai.grinder.library.equality.cardinality.direct.core.CardinalityExtensionalSet;
import com.sri.ai.grinder.library.equality.cardinality.direct.core.CardinalityImplication;
import com.sri.ai.grinder.library.equality.cardinality.direct.core.CompleteNormalize;
import com.sri.ai.grinder.library.equality.cardinality.direct.core.DefaultCardinalityConfiguration;
import com.sri.ai.grinder.library.equality.cardinality.direct.core.EqualityInConjunction;
import com.sri.ai.grinder.library.equality.cardinality.direct.core.IncompleteTopImpliedCertainty;
import com.sri.ai.grinder.library.equality.cardinality.direct.core.MoveNotIn;
import com.sri.ai.grinder.library.equality.cardinality.direct.core.PickCheapest;
import com.sri.ai.grinder.library.equality.cardinality.direct.core.QuantifierElimination;
import com.sri.ai.grinder.library.equality.cardinality.direct.core.Normalize;
import com.sri.ai.grinder.library.equality.cardinality.direct.core.Simplify;
import com.sri.ai.grinder.library.equality.cardinality.direct.core.SortPair;
import com.sri.ai.grinder.library.equality.cardinality.direct.core.SumOverOneVariable;
import com.sri.ai.grinder.library.equality.cardinality.direct.core.TopImpliedCertainty;
import com.sri.ai.grinder.library.equality.cardinality.direct.core.TopQuantifierElimination;
import com.sri.ai.grinder.library.equality.cardinality.direct.core.TopSimplify;
import com.sri.ai.grinder.library.equality.cardinality.direct.core.TopSimplifyConjunction;
import com.sri.ai.grinder.library.equality.cardinality.direct.core.TopSimplifyDisjunction;

/**
 * A factory class for instantiating rewriting processes capable of performing a
 * cardinality computation on a given formula.
 * 
 * @author oreilly
 * 
 */
@Beta
public class DirectCardinalityComputationFactory {
	
	public static Rewriter getRootRewriter() {
		return (new Simplify()).getRootRewriter();
	}
	
	public static Rewriter newNormalize() {
		Normalize normalize = new Normalize();
		return normalize;
	}
	
	public static CardinalityConfiguration newCardinalityConfiguration() {
		return new DefaultCardinalityConfiguration();
	}
	
	public static RewritingProcess newCardinalityProcess(Expression rootExpression) {
		return newCardinalityProcess(rootExpression, newCardinalityConfiguration());
	}
	
	public static RewritingProcess newCardinalityProcess(Expression rootExpression, CardinalityConfiguration configuration) {
		return newCardinalityProcess(rootExpression, configuration, null);
	}
	
	public static RewritingProcess newCardinalityProcess(Expression rootExpression, RewritingProcess parentProcess) {
		return newCardinalityProcess(rootExpression, newCardinalityConfiguration(), parentProcess);
	}
	
	public static RewritingProcess newCardinalityProcess(Expression rootExpression, CardinalityConfiguration configuration, RewritingProcess parentProcess) {
		DefaultRewriterLookup cardinalityRewriterLookup = new DefaultRewriterLookup(getCardinalityRewritersMap(configuration));
		
		Map<Expression, Expression> contextualVariablesAndDomains = null;
		Expression contextualConstraint                           = null;
		Predicate<Expression> isConstantPredicate                 = null;
		Map<Object, Object>   globalObjects                       = null;
		
		if (parentProcess != null) {
			contextualVariablesAndDomains = parentProcess.getContextualVariablesAndDomains();
			contextualConstraint          = parentProcess.getContextualConstraint();
			isConstantPredicate           = parentProcess.getIsConstantPredicate();
			globalObjects                 = parentProcess.getGlobalObjects();
		}
		else {
			contextualVariablesAndDomains = new HashMap<Expression, Expression>();
			contextualConstraint          = Expressions.TRUE;
			isConstantPredicate           = new PrologConstantPredicate();
			globalObjects                 = new HashMap<Object, Object>();
		}
		
		DefaultRewritingProcess cardinalityProcess = new DefaultRewritingProcess(
				rootExpression,
				getRootRewriter(),
				cardinalityRewriterLookup,
				contextualVariablesAndDomains,
				contextualConstraint,
				isConstantPredicate,
				globalObjects);

		return cardinalityProcess;
	}

	public static Map<String, Rewriter> getCardinalityRewritersMap() {
		return getCardinalityRewritersMap(newCardinalityConfiguration());
	}
	
	public static Map<String, Rewriter> getCardinalityRewritersMap(CardinalityConfiguration configuration) {
		Map<String, Rewriter> cardRewriters = new HashMap<String, Rewriter>();
		
		// Supporting Classes
		PickCheapest pickCheapest = new PickCheapest();
		pickCheapest.setPickCheapestTopLevelCostFunction(configuration.getPickCheapestTopLevelCostFunction());
		SortPair sortPair = new SortPair();
		sortPair.setPickCheapest(pickCheapest);
		
		
		cardRewriters.put(CardinalityRewriter.R_card,  new Cardinality());
		
	    CardinalityConjunction rCardinalityConjunction = new CardinalityConjunction();
	    rCardinalityConjunction.setPickCheapest(pickCheapest);
	    cardRewriters.put(CardinalityRewriter.R_card_conjunction, rCardinalityConjunction);
	    
	    cardRewriters.put(CardinalityRewriter.R_card_conjunction_of_disequalities, new CardinalityConjunctionOfDisequalities());
	    
	    CardinalityDisjunction rCardinalityDisjunction = new CardinalityDisjunction();
	    rCardinalityDisjunction.setSortPair(sortPair);
	    cardRewriters.put(CardinalityRewriter.R_card_disjunction, rCardinalityDisjunction);
	    
	    cardRewriters.put(CardinalityRewriter.R_card_equivalence,             new CardinalityEquivalence());
	    cardRewriters.put(CardinalityRewriter.R_cardExtensionalSet,           new CardinalityExtensionalSet());
	    cardRewriters.put(CardinalityRewriter.R_card_implication,             new CardinalityImplication());
	    cardRewriters.put(CardinalityRewriter.R_normalize,                    new Normalize());
	    cardRewriters.put(CardinalityRewriter.R_complete_normalize,           new CompleteNormalize());
	    cardRewriters.put(CardinalityRewriter.R_equality_in_conjunction,      new EqualityInConjunction());
	    cardRewriters.put(CardinalityRewriter.R_move_not_in,                  new MoveNotIn());
	    cardRewriters.put(CardinalityRewriter.R_quantifier_elimination,       new QuantifierElimination());
	    cardRewriters.put(CardinalityRewriter.R_sum_over_one_variable,        new SumOverOneVariable());
	    cardRewriters.put(CardinalityRewriter.R_top_implied_certainty,        new TopImpliedCertainty()); 
	    cardRewriters.put(CardinalityRewriter.R_top_quantifier_elimination,   new TopQuantifierElimination());
	    cardRewriters.put(CardinalityRewriter.R_top_simplify,                 new TopSimplify());
	    cardRewriters.put(CardinalityRewriter.R_top_simplify_conjunction,     new TopSimplifyConjunction());
	    cardRewriters.put(CardinalityRewriter.R_top_simplify_disjunction,     new TopSimplifyDisjunction());
	    cardRewriters.put(CardinalityRewriter.R_incomplete_implied_certainty, new IncompleteTopImpliedCertainty());
		
		return cardRewriters;
	}
	
}
