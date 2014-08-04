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
package com.sri.ai.grinder.library.equality.cardinality.direct.core;

import java.util.ArrayList;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Set;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.helper.GrinderUtil;
import com.sri.ai.grinder.helper.Trace;
import com.sri.ai.grinder.helper.concurrent.BranchRewriteTask;
import com.sri.ai.grinder.helper.concurrent.RewriteOnBranch;
import com.sri.ai.grinder.library.FunctorConstants;
import com.sri.ai.grinder.library.boole.And;
import com.sri.ai.grinder.library.boole.BooleanUtil;
import com.sri.ai.grinder.library.boole.ForAll;
import com.sri.ai.grinder.library.boole.Or;
import com.sri.ai.grinder.library.boole.ThereExists;
import com.sri.ai.grinder.library.controlflow.IfThenElse;
import com.sri.ai.grinder.library.equality.cardinality.CardinalityUtil;
import com.sri.ai.grinder.library.equality.cardinality.direct.AbstractCardinalityRewriter;
import com.sri.ai.grinder.library.equality.cardinality.direct.CardinalityRewriter;
import com.sri.ai.grinder.library.indexexpression.IndexExpressions;
import com.sri.ai.grinder.library.number.Plus;
import com.sri.ai.grinder.library.number.Times;
import com.sri.ai.grinder.library.set.intensional.IntensionalSet;
import com.sri.ai.grinder.library.set.tuple.Tuple;
import com.sri.ai.util.base.Pair;

/**
 * Experimental DPLL implementation of R_card_conjunction( | F |_X, quantification ).
 * TODO - if this logic replaces CardinalityConjunction.java then move the java doc below to CardinalityRewriter and
 * indicate that this is now the default implementation.
 * <pre>
 * R_card_conjunction( | F |_X, quantification )
 * F is False, True, a literal, a multi-equality, or a conjunction.
 * "quantification" is either "there exists", "for all", or "none".
 * 
 * Returns a basic expression equivalent to | F |_X, if quantification is "none".
 * If quantification is "for all", returns a counting-solution, the leaves of which may differ from the exact one in the following way: when the exact one is not ||X||, it may be any value but ||X||.
 * If quantification is "there exists", returns a counting-solution, the leaves of which may differ from the exact one in the following way: when the exact one is not 0, it may be any value but 0.
 * 
 * if F is a conjunction which can be partitioned into two or more independent sub problems
 * 	// i.e. there is a partition {I_1, ..., I_k} of indices such that there is a partition 
 *  { C_1, ..., C_k } of the conjuncts of F where indices in I_j occur in C_j only, for every j:
 *  	if I_1 is empty:
 *  		return if C_1 then R_normalize( R_card(|R_top_simplify(C_2)|_I_2, quantification)  * ... *  R_card(|R_top_simplify(C_k)|_I_k, quantification) ) else 0
 *  	else:
 *     		return R_normalize( R_card(|R_top_simplify(C_1)|_I_1, quantification)  * ... *  R_card(|R_top_simplify(C_k)|_I_k, quantification) )
 * 
 * if F is True or empty conjunction
 *     return ||X||
 * 
 * if F is False
 *     return 0
 * 
 * if F is F1 and F2 where F1 is a formula independent of all x's in X
 *    // note that F1 is the conjunction of all conjuncts of F that are independent of X, or F itself if it is X-free
 *    F2 = R_top_simplify_conjunction(F2)
 *    return R_normalize( if F1 then R_card_conjunction(| F2 |_X, quantification) else 0 )
 * 
 * if F is a conjunction of the form 'x = t and Phi', where x is one of the index variables in X
 *     return R_equality_in_conjunction(| F |_X)
 * 
 * if F is x1 != t1 and ... xn != tk // F is a conjunction of disequalities
 *     return R_card_conjunction_of_disequalities(| F |_X, quantification)
 * 
 * Candidates <- { (Fi, i) : Fi satisfies one of the cases below }
 * (Fi, i) <- pick_cheapest( Candidates )
 * 
 * if Fi is "not G"
 *     return R_card(
 *             |replace_conjunct_and_top_simplify(R_move_not_in(Fi), i, F)|_X, quantification )
 * 
 * if Fi is (nested) conjunction (G1 and ... and Gk)
 *     return R_card(
 *             | R_top_simplify_conjunction(
 *                      F1 and ... Fi-1 and G1 and ... and Gk and Fi+1 and ... and Fn) |_X, quantification )
 * 
 * if Fi is G1 => G2
 *     return R_card(
 *             | R_top_simplify_disjunction(
 *                 replace_conjunct_and_top_simplify((not G1), i, F)
 *                      or
 *                 replace_conjunct_and_top_simplify(G2, i, F)) 
 *             |_X, quantification )
 * 
 * if Fi is G1 <=> G2
 *     return R_card(| replace_conjunct_and_top_simplify(G1 and G2), i, F) |_X, quantification) + 
 *     		  R_card(| replace_conjunct_and_top_simplify(not G1 and not G2), i, F) |_X, quantification)
 * 
 * if Fi is (F1 or F2)
 *     select literal Alpha from Fi
 *     return dpllConditioning(F, Alpha, X, quantification)
 * 
 * if Fi is Q y : G
 *     return R_card(
 *             | replace_conjunct_and_top_simplify(R_top_quantifier_elimination(Fi), i, F) |_X, quantification )
 * </pre>
 * 
 * @author oreilly
 *
 */
@Beta
public class CardinalityDPLLConjunction extends AbstractCardinalityRewriter {
	
	private SortPair        sortPair                   = new SortPair();
	private PickCheapest    pickCheapest               = new PickCheapest();
	private RewriteOnBranch rewriteCardinalityOnBranch = new RewriteOnBranch() {
				@Override
				public Expression rewrite(Expression[] expressions, RewritingProcess process) {
					Expression result = process.rewrite(R_card, 
											CardinalityUtil.argForCardinalityWithQuantifierSpecifiedCall(
													expressions[0], 
													Quantification.getQuantificationForSymbol(expressions[1])));
					
					return result;
				}
			};
	
	public CardinalityDPLLConjunction() {
		sortPair.setPickCheapest(pickCheapest);
	}

	@Override
	public String getName() {
		return R_card_conjunction;
	}
	
	public PickCheapest getPickCheapest() {
		return pickCheapest;
	}
	
	public void setPickCheapest(PickCheapest pickCheapest) {
		this.pickCheapest = pickCheapest;
		this.sortPair.setPickCheapest(pickCheapest);
	}

	/**
	 * @see CardinalityRewriter#R_card_conjunction
	 */
	@Override
	public Expression rewriteAfterBookkeeping(Expression expression, RewritingProcess process) {
		Expression result = null;
		
		// Assert input arguments, (| F |_X, quantification)
		if (!(Tuple.isTuple(expression) &&
			  Tuple.size(expression) == 2)
			  ) {
			throw new IllegalArgumentException("Invalid input argument expression, expect (| F |_X, quantification):"+expression);
		}
		Expression cardinalityOfIndexedFormulaExpression = Tuple.get(expression, 0);
		Expression quantificationSymbol                  = Tuple.get(expression, 1);
		// 
		CardinalityUtil.assertIsCardinalityOfIndexedFormulaExpression(cardinalityOfIndexedFormulaExpression);
		// | {(on I1,..., In)(x1, ..., xn) | F} |, with Ii = "xi", or Ii = "xi in Di"
		Expression       intensionalSet   = cardinalityOfIndexedFormulaExpression.get(0);
		Expression       f                = IntensionalSet.getCondition(intensionalSet);
		List<Expression> indexExpressions = IntensionalSet.getIndexExpressions(intensionalSet);
		RewritingProcess subProcess = GrinderUtil.extendContextualVariablesWithIntensionalSetIndices(intensionalSet, process);
		
		CardinalityRewriter.Quantification quantification = CardinalityRewriter.Quantification.getQuantificationForSymbol(quantificationSymbol);
		if (quantification == null) {
			throw new IllegalArgumentException("Invalid quantification symbol:"+quantificationSymbol);
		}
		List<Pair<Set<Expression>, List<Expression>>> problems = CardinalityUtil.findIndependentProblemsInConjunction(f, indexExpressions, subProcess);
		
		if ( problems.isEmpty() ) {
			result = rewriteUnseparableConjunction(cardinalityOfIndexedFormulaExpression, quantification, process);
		} 
		else {
			Trace.log("if F is a conjunction which can be partitioned into two or more independent sub problems");
			Trace.log("// i.e. there is a partition {I_1, ..., I_k} of indices such that there is a partition { C_1, ..., C_k } of the conjuncts of F where indices in I_j occur in C_j only, for every j:");
			Expression independent = null;
			List<BranchRewriteTask> taskRewriters = new ArrayList<BranchRewriteTask>();
			for (Pair<Set<Expression>, List<Expression>> problem: problems) {
				List<Expression> conjuncts = problem.second;
				Set<Expression> indexes    = problem.first;
				Expression subConjunct     = And.make(conjuncts.toArray(new Expression[conjuncts.size()]));
				subConjunct                = subProcess.rewrite(R_top_simplify, subConjunct);
				if ( indexes.isEmpty() ) {
					independent = subConjunct;
				} 
				else {
					Expression subConjunctCard = CardinalityUtil.makeCardinalityOfIndexedFormulaExpression(subConjunct, indexes.toArray(new Expression[indexes.size()]));
					taskRewriters.add(new BranchRewriteTask(rewriteCardinalityOnBranch, new Expression[] {subConjunctCard, quantification.getQuantificationSymbol()}));
				}
			}
			
			List<Expression> problemsCards = GrinderUtil.branchAndMergeTasks(taskRewriters.toArray(
														new BranchRewriteTask[taskRewriters.size()]), 
														// Short circuit on 0.
														Expressions.ZERO, process);
			
			result = Times.make(problemsCards);
			if ( independent != null ) {
				Trace.log("    if I_1 is empty:");				
				Trace.log("        return if C_1 then R_normalize( R_card(|R_top_simplify(C_2)|_I_2, quantification)  * ... *  R_card(|R_top_simplify(C_k)|_I_k, quantification) ) else 0");				
				result = IfThenElse.make(independent, result, Expressions.ZERO);
			} 
			else {
				Trace.log("    return R_normalize( R_card(|R_top_simplify(C_1)|_I_1, quantification)  * ... *  R_card(|R_top_simplify(C_k)|_I_k, quantification) )");				
			}
			result = process.rewrite(R_normalize, result);
		}
		
		return result;
	}
		
	protected Expression rewriteUnseparableConjunction(Expression cardinalityOfIndexedFormulaExpression, CardinalityRewriter.Quantification quantification, RewritingProcess process) {
		Expression       result           = null;
		Expression       intensionalSet   = cardinalityOfIndexedFormulaExpression.get(0);
		Expression       f                = IntensionalSet.getCondition(intensionalSet);
		List<Expression> indexExpressions = IntensionalSet.getIndexExpressions(intensionalSet);
		RewritingProcess subProcess       = GrinderUtil.extendContextualVariablesWithIntensionalSetIndices(intensionalSet, process);
		
		Expression[] indexExpressionsAsArray = indexExpressions.toArray(new Expression[indexExpressions.size()]);
		List<Expression> indices = IndexExpressions.getIndices(indexExpressions);
		Pair<Expression, Expression> independentAndDependentConjuncts = CardinalityUtil.separateIndependentAndDependent(f, indices, Expressions.TRUE, subProcess);
		//
		Expression independentConjunction = independentAndDependentConjuncts.first;
		Expression dependentConjunction   = independentAndDependentConjuncts.second;
	
		if (f.equals(Expressions.TRUE) || 
			(And.isConjunction(f) && f.numberOfArguments() == 0)) {
			Trace.log("if F is True or empty conjunction");
			Trace.log("    return ||X||");
			Expression cardinalityValueOfIndices = CardinalityUtil.makeCardinalityOfIndexExpressions(indexExpressions);
			// Need to do this to get | type(X) | converted to its known value, e.g.: 10
			cardinalityValueOfIndices = process.rewrite(R_normalize, cardinalityValueOfIndices);
			result = cardinalityValueOfIndices;
		}
		else if (f.equals(Expressions.FALSE)) {
			Trace.log("if F is False");
			Trace.log("    return 0");
			result = Expressions.ZERO;
		} 
		else if ( ! independentConjunction.equals(Expressions.TRUE) ) {
			Trace.log("if F is F1 and F2 where F1 is a formula independent of all x's in X");
			// Note: The logic for independent sub-problems does not encompass this as you can have something like this:
			// | { ( on Z ) ( Z ) | Y != a and Z != Y and Z != b } |
			// Y != a is independent of the indices but the conjuncts are transitively linked via Z != Y,
			// so they are not independent sub-problems.
			Trace.log("   F2 = R_top_simplify_conjunction(F2)");
			dependentConjunction = subProcess.rewrite(R_top_simplify_conjunction, dependentConjunction);
			Trace.log("   return R_normalize( if F1 then R_card_conjunction(| F2 |_X, quantification) else 0 )");
			Expression dependentCardinalityProblem = CardinalityUtil.makeCardinalityOfIndexedFormulaExpression(dependentConjunction, indexExpressionsAsArray);
			Expression dependentCardinalityResult  = process.rewrite(R_card, CardinalityUtil.argForCardinalityWithQuantifierSpecifiedCall(dependentCardinalityProblem, quantification));
			Expression ifThenElse = IfThenElse.make(independentConjunction, dependentCardinalityResult, Expressions.ZERO);
			result = process.rewrite(R_normalize, ifThenElse);
		} 
		else if (EqualityInConjunction.isOptimizable(cardinalityOfIndexedFormulaExpression, process)) {
			Trace.log("if F is a conjunction of the form 'x = t and Phi', where x is one of the index variables in X");
			Trace.log("    return R_equality_in_conjunction(| F |_X, quantification)");
			result = process.rewrite(R_equality_in_conjunction, CardinalityUtil.argForEqualityInConjunctionCall(cardinalityOfIndexedFormulaExpression, quantification));
		}
		else if ( isConjunctionOfDisequalities(f, process) ) {
			Trace.log("if F is x1 != t1 and ... xn != tk // F is a conjunction of disequalities");
			Trace.log("    return R_card_conjunction_of_disequalities(| F |_X, quantification)");
			result = process.rewrite(R_card_conjunction_of_disequalities, CardinalityUtil.argForCardinalityConjunctionOfDisequalitiesCall(cardinalityOfIndexedFormulaExpression, quantification));
		}
		else if ( And.isConjunction(f) ) {
			Trace.log("Candidates <- { (Fi, i) : Fi satisfies one of the cases below }");
			Set<Expression> candidateSet = new LinkedHashSet<Expression>();
			for (Expression fi : f.getArguments()) {
				if ((fi.hasFunctor(FunctorConstants.NOT) && fi.numberOfArguments() == 1)  ||
				    And.isConjunction(fi)                                                 ||
				    fi.hasFunctor(FunctorConstants.IMPLICATION)                           ||
				    fi.hasFunctor(FunctorConstants.EQUIVALENCE)                           ||
				    Or.isDisjunction(fi)                                                  ||
				    ForAll.isForAll(fi)                                                   ||
				    ThereExists.isThereExists(fi)
					) {
					candidateSet.add(fi);
				}
			}
			
			if (candidateSet.size() == 0) {
				throw new IllegalStateException("No Fi satisfies any of the required cases: F="+f);
			}
			
			// Note: Doing the following to try and avoid the overhead of creating temporary tuples 
			// of the for (Fi, i), however, still have the overhead of finding the correct index i again. 
			Expression[] candidates    = (new ArrayList<Expression>(candidateSet)).toArray(new Expression[candidateSet.size()]);
			Trace.log("(Fi, i) <- pick_cheapest( Candidates )");
			int cheapestIndex = pickCheapest.pickIndex(candidates);
			Expression fi     = candidates[cheapestIndex];
			Trace.log("// Fi={}", fi);
			List<Expression> fConjuncts = new ArrayList<Expression>(f.getArguments());
			int indexI = -1;
			for (int i = 0; i < fConjuncts.size(); i++) {
				if (fi.equals(fConjuncts.get(i))) {
					indexI = i;
					break;
				}
			}
			if (indexI == -1) {
				throw new IllegalStateException("Unable to find cheapest index:"+fi);
			}
			
			if (fi.hasFunctor(FunctorConstants.NOT)) {
				Trace.log("if Fi is \"not G\"");
				Trace.log("    return R_card(|replace_conjunct_and_top_simplify(R_move_not_in(Fi), i, F)|_X, quantification)");
				Expression movedNotIn                     = subProcess.rewrite(R_move_not_in, fi);
				Expression replacedConjunct               = ReplaceConjunctAndTopSimplify.replaceConjunctAndTopSimplify(movedNotIn, indexI, f, subProcess); 
				Expression cardReplacedConjunctIndexedByX = CardinalityUtil.makeCardinalityOfIndexedFormulaExpression(replacedConjunct, indexExpressionsAsArray);
				
				result = process.rewrite(R_card, CardinalityUtil.argForCardinalityWithQuantifierSpecifiedCall(cardReplacedConjunctIndexedByX, quantification));
			}
			else if (And.isConjunction(fi)) {
				Trace.log("if Fi is (nested) conjunction (G1 and ... and Gk)");
				Trace.log("    return R_card(| R_top_simplify_conjunction(F1 and ... Fi-1 and G1 and ... and Gk and Fi+1 and ... and Fn) |_X, quantification)");
				List<Expression> newConjuncts = new ArrayList<Expression>();
				
				newConjuncts.addAll(fConjuncts.subList(0, indexI));
				newConjuncts.addAll(fi.getArguments());
				newConjuncts.addAll(fConjuncts.subList(indexI + 1, fConjuncts.size()));
				
				Expression fAndGConjunction         = And.make(newConjuncts);
				Expression simplifiedConjunction    = subProcess.rewrite(R_top_simplify_conjunction, fAndGConjunction);
				Expression cardSimplifedConjunction = CardinalityUtil.makeCardinalityOfIndexedFormulaExpression(simplifiedConjunction, indexExpressionsAsArray);
				
				result = process.rewrite(R_card, CardinalityUtil.argForCardinalityWithQuantifierSpecifiedCall(cardSimplifedConjunction, quantification));
			}
			else if (fi.hasFunctor(FunctorConstants.IMPLICATION)) {
				Trace.log("if Fi is G1 => G2");
				Trace.log("    return R_card(| R_top_simplify_disjunction(replace_conjunct_and_top_simplify((not G1), i, F) or replace_conjunct_and_top_simplify(G2, i, F)) |_X, quantification)");
				Expression g1    = fi.get(0);
				Expression g2    = fi.get(1);
				Expression notG1 = CardinalityUtil.makeNot(g1);
				
				Expression replacedNotG1  = ReplaceConjunctAndTopSimplify.replaceConjunctAndTopSimplify(notG1, indexI, f, subProcess);
				Expression replacedG2     = ReplaceConjunctAndTopSimplify.replaceConjunctAndTopSimplify(   g2, indexI, f, subProcess);
				
				Expression orReplacements     = CardinalityUtil.makeOr(replacedNotG1, replacedG2);
				orReplacements                = subProcess.rewrite(R_top_simplify_disjunction, orReplacements);
				Expression cardOrReplacements = CardinalityUtil.makeCardinalityOfIndexedFormulaExpression(orReplacements, indexExpressionsAsArray);
				
				result = process.rewrite(R_card, CardinalityUtil.argForCardinalityWithQuantifierSpecifiedCall(cardOrReplacements, quantification));
			}
			else if (fi.hasFunctor(FunctorConstants.EQUIVALENCE)) {
				Trace.log("if Fi is G1 <=> G2");
				Trace.log("    return R_card(| replace_conjunct_and_top_simplify(G1 and G2), i, F) |_X, quantification) + R_card(| replace_conjunct_and_top_simplify(not G1 and not G2), i, F) |_X, quantification)");
				Expression g1                = fi.get(0);
				Expression g2                = fi.get(1);
				Expression notG1             = CardinalityUtil.makeNot(g1);
				Expression notG2             = CardinalityUtil.makeNot(g2);
				Expression g1AndG2           = CardinalityUtil.makeAnd(g1, g2);
				Expression notG1AndNotG2     = CardinalityUtil.makeAnd(notG1, notG2);
				
				Expression part1             = ReplaceConjunctAndTopSimplify.replaceConjunctAndTopSimplify(g1AndG2, indexI, f, subProcess);
				Expression part1Card         = CardinalityUtil.makeCardinalityOfIndexedFormulaExpression(part1, indexExpressionsAsArray);
				Expression part1CardComputed = process.rewrite(R_card, CardinalityUtil.argForCardinalityWithQuantifierSpecifiedCall(part1Card, quantification));
				
				Expression part2             = ReplaceConjunctAndTopSimplify.replaceConjunctAndTopSimplify(notG1AndNotG2, indexI, f, subProcess);
				Expression part2Card         = CardinalityUtil.makeCardinalityOfIndexedFormulaExpression(part2, indexExpressionsAsArray);
				Expression part2CardComputed = process.rewrite(R_card, CardinalityUtil.argForCardinalityWithQuantifierSpecifiedCall(part2Card, quantification));
				
				ArrayList<Expression> additionArguments   = new ArrayList<Expression>();
				additionArguments.add(part1CardComputed);
				additionArguments.add(part2CardComputed);
				result = Plus.make(additionArguments);
				result = process.rewrite(R_normalize, result);
			}
			else if (Or.isDisjunction(fi)) {
				Trace.log("if Fi is (F1 or F2)");
				Trace.log("    select literal Alpha from Fi");
				Expression alpha = CardinalityUtil.pickCheapestLiteral(fi);
				Trace.log("    return dpllConditioning(F, Alpha, X, quantification)");
				result = CardinalityUtil.dpllConditioning(f, alpha, indexExpressionsAsArray, quantification, sortPair, subProcess);
			} 
			else if (ForAll.isForAll(fi) || ThereExists.isThereExists(fi)) {
				Trace.log("if Fi is Q y : G");
				Trace.log("    return R_card(| replace_conjunct_and_top_simplify(R_top_quantifier_elimination(Q y : G), i, F) |_X, quantification)");
				
				Expression quantifierEliminated = subProcess.rewrite(R_top_quantifier_elimination, fi);
				Expression replacement          = ReplaceConjunctAndTopSimplify.replaceConjunctAndTopSimplify(quantifierEliminated, indexI, f, subProcess);
				
				Expression cardReplacements     = CardinalityUtil.makeCardinalityOfIndexedFormulaExpression(replacement, indexExpressionsAsArray);
				
				result = process.rewrite(R_card, CardinalityUtil.argForCardinalityWithQuantifierSpecifiedCall(cardReplacements, quantification));	
			} 
			else {
				throw new IllegalStateException("Unhandled Fi case:"+fi);
			}
		}
		else {
			throw new IllegalArgumentException("F is not a conjunction:"+f);
		}
		
		return result;
	}
	
	
	
	
	//
	// PRIVATE METHODS
	//
	private boolean isConjunctionOfDisequalities(Expression f, RewritingProcess process) {
		boolean result = true;
		if ( And.isConjunction(f) ) {
			for (Expression conjunct: f.getArguments()) {
				if ( !BooleanUtil.isNotEquality(conjunct) ) {
					result = false;
					break;
				}
			}
		} 
		else {
			result = BooleanUtil.isNotEquality(f);
		}
		return result;
	}
	

}
