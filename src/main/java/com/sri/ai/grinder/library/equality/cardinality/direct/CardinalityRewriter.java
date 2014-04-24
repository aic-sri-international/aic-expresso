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
package com.sri.ai.grinder.library.equality.cardinality.direct;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.helper.Expressions;

/**
 * <b>Rewriters for Computing the cardinality of a set</b><br>
 * Let L be a language with no function and a single predicate representing
 * equality, but it may have distinct constant symbols. Let F be a quantifier
 * formula in L. Let |F|_{x1, ..., xn} denote the cardinality of the set {(x1,
 * ..., xn) | F}{x1, ..., xn}. We shall define a series of rewriters to compute
 * |F|_{x1, ..., xn}.<br>
 * <br>
 * We need the following definition for our process:<br>
 * Definition: An expression E is Easily Summable (ES) with respect to variables
 * x1, ..., xn iff it has one of the following forms:<br>
 * <ul>
 * <li>E is some constant c</li>
 * <li>E is a variable other than x1, ..., xn</li>
 * <li>E is of the form "if C then t1 else t2" where both t1 and t2 are ES with
 * respect to x1,...,xn and C is a quantifier-free formula in L.</li>
 * <li>E is of the form f(t1, ..., tn) where t1, …, tn are ES with respect to
 * x1,...,xn.</li>
 * </ul>
 * For a set of index variables X, ||X|| denotes the product of the
 * cardinalities of domain sizes of index variables in X. For instance, if X =
 * {x, y}, then ||X|| = |type(x)| * |type(y)|
 * 
 * @author oreilly
 * 
 */
@Beta
public interface CardinalityRewriter {

	//
	// START - Parameters
	enum Quantification {
		FOR_ALL(Expressions.createSymbol("for all")), THERE_EXISTS(
				Expressions.createSymbol("there exists")), NONE(Expressions.createSymbol("none"));

		private Expression quantificationSymbol = null;

		public Expression getQuantificationSymbol() {
			return quantificationSymbol;
		}

		public static Quantification getOpposite(Quantification quantification) {
			Quantification result = NONE;
			if (quantification == FOR_ALL) {
				result = THERE_EXISTS;
			} 
			else if (quantification == THERE_EXISTS) {
				result = FOR_ALL;
			}

			return result;
		}

		public static Quantification getQuantificationForSymbol(
				Expression symbol) {
			Quantification result = null;

			for (Quantification quantification : Quantification.values()) {
				if (quantification.getQuantificationSymbol().equals(symbol)) {
					result = quantification;
					break;
				}
			}

			if (result == null) {
				throw new IllegalArgumentException(
						"Symbol is not a legal value for a quantification:"
								+ symbol);
			}

			return result;
		}

		private Quantification(Expression quantificationSymbol) {
			this.quantificationSymbol = quantificationSymbol;
		}

	}

	// END - Parameters
	//
	
	/**
	 * The name space that the Cardinality rewriters belong to.
	 */
	String CARDINALITY_NAMESPACE = "card.";
	
	/**
	 * <pre>
	 * R_card(| F |_{x1, ..., xn})
	 * F a first-order formula on equalities, i.e. F is a boolean combination of equalities and disequalities
	 * of variables and constants. F may have quantifiers.
	 * Returns a basic expression representing | F |.
	 * 
	 * F <- R_top_simplify(F) // this is the only function that does not assume its input is top-simplified
	 * 
	 * if n = 0
	 *     return R_normalize(if F then 1 else 0)
	 * if n > 0
	 * 	  if negationHasLessNumberOfDisjuncts(F):
	 *	 	 return R_normalize(||X|| - R_card( | not F |_X, "none" ) ) 
	 *	  else
	 *       return R_card( | F |_X, "none")
	 * </pre>
	 * 
	 * <pre>
	 * R_card( | F |_X, quantification )
	 * F is any top-simplified formula
	 * "quantification" is either "there exists", "for all", or "none", and indicates whether we are solving "there exists x1, ..., xn : F", "for all x1, ..., xn : F" where X = {x1, ..., xn} , or neither of those.
	 * 
	 * Returns a basic expression equivalent to | F |_X, if quantification is "none".
	 * If quantification is "for all", returns a counting-solution, the leaves of which may differ from the exact one in the following way: when the exact one is not ||X||, it may be any value but ||X||.
	 * If quantification is "there exists", returns a counting-solution, the leaves of which may differ from the exact one in the following way: when the exact one is not 0, it may be any value but 0.
	 * 
	 * if F is True
	 * 		return R_normalize(||X||)
	 * 
	 * if F is False
	 * 		return 0
	 * 
	 * if x does not occur in F for any x in X
	 *     return R_normalize(if F then ||X|| else 0)
	 * 
	 * if F is a conjunction // including F being a literal or a multi-equality
	 *     return R_card_conjunction(|F|_X, quantification)
	 * 
	 * if F is disjunction
	 *     return R_card_disjunction(|F|_X, quantification)
	 * 
	 * if F is "not G"
	 *     F' <- R_top_simplify(R_move_not_in(F))
	 *     return R_card(| F' |_X, quantification)
	 * 
	 * if F is an implication of the form G => H
	 *     return R_card_implication( | F |_X, quantification )
	 * 
	 * if F is an equivalence of the form G <=> H
	 *     return R_card_equivalence( | F |_X, quantification)
	 *            
	 * if F is Q y : G
	 *     return R_card( | R_top_quantifier_elimination(Q y : G) |_X, quantification )
	 * </pre>
	 */
	String R_card = CARDINALITY_NAMESPACE+"R_card";

	/**
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
	 *     return R_card(
	 *             | R_top_simplify_disjunction(
	 *                 replace_conjunct_and_top_simplify(F1, i, F)
	 *                      or
	 *                 replace_conjunct_and_top_simplify(F2, i, F)) 
	 *             |_X, quantification )
	 * 
	 * if Fi is Q y : G
	 *     return R_card(
	 *             | replace_conjunct_and_top_simplify(R_top_quantifier_elimination(Fi), i, F) |_X, quantification )
	 * </pre>
	 */
	String R_card_conjunction = CARDINALITY_NAMESPACE+"R_card_conjunction";
	
	/**
	 * <pre>
	 * R_card_conjunction_of_disequalities( | F |_X, quantification )
	 * F is a conjunction of disequalities. Every disequality in F contains at least one index variable in X.
	 * "quantification" is either "there exists", "for all", or "none.
	 * 
	 * Returns a basic expression equivalent to | F |_X, if quantification is "none".
	 * If quantification is "for all", returns a counting-solution, the leaves of which may differ from the exact one in the following way: when the exact one is not ||X||, it may be any value but ||X||.
	 * If quantification is "there exists", returns a counting-solution, the leaves of which may differ from the exact one in the following way: when the exact one is not 0, it may be any value but 0.
	 * 
	 * if X = {x}
	 *     	if quantification is "for all" and (ASSUME_DOMAIN_ALWAYS_LARGE or |type(x)| > 0)
	 *      	return 0
	 *     	if quantification is "there exists" and (ASSUME_DOMAIN_ALWAYS_LARGE or |type(x)| > k)
	 *         	return R_normalize(|type(x)|)
	 *     	if quantification is "none"
	 *     		return R_normalize(|type(x)| - R_cardExtensionalSet(|{t1,...,tk}|))
	 * if X = {x1, ..., xn}
	 *     	return R_sum_over_one_variable(sum_{x1:True} R_card( | F |_{x2, ..., xn} ))
	 * 
	 * </pre>
	 */
	String R_card_conjunction_of_disequalities = CARDINALITY_NAMESPACE+"R_card_conjunction_of_disequalities";
	
	/**
	 * <pre>
	 * R_card_disjunction(| F |_X, quantification)
	 * F is a disjunction.
	 * "quantification" is either "there exists", "for all", or "none".
	 * 
	 * Returns a basic expression equivalent to | F |_x, if quantification is "none".
	 * If quantification is "for all", returns a counting-solution, the leaves of which may differ from the exact one in the following way: when the exact one is not ||X||, it may be any value but ||X||.
	 * If quantification is "there exists", returns a counting-solution, the leaves of which may differ from the exact one in the following way: when the exact one is not 0, it may be any value but 0.
	 * 
	 * if F is a disjunction which can be partitioned into a few subproblems
	 * such that F can be partitioned into D, D_1, and D_2; where I_1 and I_2 are two partitions of the indices in X, 
	 * and the index variables in D_1 and D_2 are I_1 and I_2 respectively, and D is independent of X. Either D or D_2 may be empty:
	 *   D_1 = R_top_simplify(D_1)
	 * 	 if D_2 is empty:
	 * 		R <- R_card(| D_1 |_X, quantification)
	 *   else:
	 *      D_2 = R_top_simplify(D_2)
	 *      R <- R_normalize(R_card(| D_1 |_I_1, quantification)*||I_2|| + R_card(| D_2 |_I2, quantification)*||I_1|| - R_card(| D_1 |_I_1, quantification)* R_card(| D_2 |_I_2, quantification))
	 *   if D is empty:
	 *      return R
	 *   else:
	 *      return R_normalize(if D then ||X|| else R)
	 * 
	 * // Assume F is of the form F1 or F2
	 * // | F1 or F2 |_x = | F1 |_x + | F2 |_x - | F1 and F2 |_x
	 * // but we do not compute it like that, see below for optimized version.
	 * 
	 * (F1, F2) <- split_disjuncts_on_X(F) as follows:
	 * if F1 contains all the disjuncts independent of X (not empty)
	 * 		return R_normalize(if F1 then ||X|| else R_card(|F2|_X, quantification))
	 * 
	 * otherwise (if all disjuncts of F have some index variable occurring in them):
	 * F1 <- first disjunct in F
	 * F2 <- remaining disjuncts in F
	 * 
	 * F1 <- R_top_simplify(F1)
	 * F2 <- R_top_simplify(F2)
	 * 
	 * if quantification is "for all"
	 *     return R_normalize(
	 *     			if R_card(| R_top_simplify_conjunction(not F1 and not F2)  |_X, "there exists") > 0 then 0 else ||X||)
	 *     
	 * (F1, F2) <- sort_pair(F1, F2) 
	 * 
	 * N1 <- R_card(| F1 |_X, quantification)
	 *     
	 * if N1 = 0
	 *     return R_card(| F2 |_X, quantification) // | F1 and F2 |_X is 0
	 * if N1 = ||X||
	 *     return ||X|| // | F2 |_X = | F1 and F2 |_X and cancel out
	 * 
	 * N2 <- R_card(| F2 |_X, quantification)
	 * 
	 * if N2 = 0
	 *     return N1 // | F1 and F2 |_X is 0
	 * if N2 = ||X||
	 *     return ||X|| // N1 = | F1 |_X = | F1 and F2 |_X and cancel out
	 *     
	 * if quantification is "there exists"
	 *     // there is no need to compute N3, since it is enough that there is x for either F1 or F2
	 *     return R_normalize(if N1 > 0 or N2 > 0 then ||X|| else 0)
	 * 
	 * // quantification is guaranteed to be "none" because otherwise it will have returned by now
	 * 
	 * N3 <- R_card( | R_top_simplify_conjunction(F1 and F2) |_X, "none" )
	 *    
	 * return R_normalize(N1 + N2 - N3)
	 * // possible further optimization since F1 and F2 are already top-simplified
	 * </pre>
	 */
	String R_card_disjunction = CARDINALITY_NAMESPACE+"R_card_disjunction";

	/**
	 * <pre>
	 * R_card_equivalence(| F |_X, quantification)
	 * F is an equivalence of the form "G <=> H".
	 * "quantification" is either "there exists", "for all", or "none".
	 * 
	 * Returns a basic expression equivalent to | F |_x, if quantification is "none".
	 * If quantification is "for all", returns a counting-solution, the leaves of which may differ from the exact one in the following way: when the exact one is not ||X||, it may be any value but ||X||.
	 * If quantification is "there exists", returns a counting-solution, the leaves of which may differ from the exact one in the following way: when the exact one is not 0, it may be any value but 0.
	 * 
	 * G <- R_top_simplify(G)
	 * H <- R_top_simplify(H)
	 * return R_card(| R_top_simplify_conjunction(G and H) |_X, quantification) + 
	 * 		  R_card(| R_top_simplify_conjunction(not G and not H) |_X, quantification)
	 * </pre>
	 */	
	String R_card_equivalence = CARDINALITY_NAMESPACE+"R_card_equivalence";

	/**
	 * <pre>
	 * R_cardExtensionalSet( | {t1,...,tn} | )
	 * Each ti is an expression
	 * Returns a basic expression equivalent to |{t1,...,tn}|
	 * 
	 * if n = 0 // empty set
	 *     return 0
	 * // cardinality of the set expression without the first element expression
	 * N <- R_cardExtensionalSet( | {t2,...,tk} | ) 
	 * Irrelevant <- R_normalize(t1 = t2 or ... or t1 = tn)
	 * return if Irrelevant then N else plusOne(N)
	 * </pre>
	 */
	String R_cardExtensionalSet = CARDINALITY_NAMESPACE+"R_cardExtensionalSet";
	
	/**
	 * <pre>
	 * R_card_implication(| F |_X, quantification)
	 * F is an implication of the form “G => H”.
	 * "quantification" is either "there exists", "for all", or "none".
	 * 
	 * Returns a basic expression equivalent to | F |_x, if quantification is "none".
	 * If quantification is "for all", returns a counting-solution, the leaves of which may differ from the exact one in the following way: when the exact one is not ||X||, it may be any value but ||X||.
	 * If quantification is "there exists", returns a counting-solution, the leaves of which may differ from the exact one in the following way: when the exact one is not 0, it may be any value but 0.
	 * 
	 * G <- R_top_simplify(G)
	 * H <- R_top_simplify(H)
	 * return R_card( | R_top_simplify_disjunction(not G or H) |_X, quantification)
	 * </pre>
	 */
	String R_card_implication = CARDINALITY_NAMESPACE+"R_card_implication";

	
	/**
	 * A rewriter to be used to check if a branch is reachable when calling
	 * branch and merge logic.
	 */
	String R_check_branch_reachable = CARDINALITY_NAMESPACE+"R_complete_normalize";

	/**
	 * <pre>
	 * R_equality_in_conjunction(| x_i = t and Phi |_X, quantification)
	 * Phi is a formula, x_i is one of the index variables in X, t is a variable or a constant.
	 * “quantification” is either “there exists”, “for all”, or “none”.
	 * 
	 * Returns a basic expression equivalent to | F |_X, if quantification is “none”.
	 * If quantification is “for all”, returns a counting-solution, the leaves of which may differ from the exact one in the following way: when the exact one is not ||X||, it may be any value but ||X||.
	 * If quantification is “there exists”, returns a counting-solution, the leaves of which may differ from the exact one in the following way: when the exact one is not 0, it may be any value but 0.
	 *
	 * if t is the same expression as x_i
	 *    return R_card(| Phi |_X, quantification)
	 * else return R_card(| R_normalize(Phi[x_i / t]) |_X\{xi}, quantification)
	 * </pre>
	 */
	String R_equality_in_conjunction = CARDINALITY_NAMESPACE+"R_equality_in_conjunction";

	/**
	 * <pre>
	 * R_move_not_in(F)
	 * F is a negation
	 * Returns a formula that is not a negation and is equivalent to "F"
	 * 
	 * Cases for input:
	 * F is "not FALSE"
	 *     return TRUE
	 * F is "not TRUE"
	 *     return FALSE
	 * F is "not (t1 = t2 = ... = tn)"
	 *     return t1 != t2 or ... or t_{n-1} != tn
	 * F is "not x != t"
	 *     return x = t
	 * F is "not not G"
	 *     G <- R_top_simplify(G)
	 *     return G
	 * F is "not (G1 and ... and Gn)"
	 *     (G1 and ... and Gn) <- R_top_simplify_conjunction(G1 and ... and Gn)
	 *     return not G1 or ... or not Gn
	 * F is "not (G1 or ... or Gn)"
	 *     (G1 or ... or Gn) <- R_top_simplify_disjunction(G1 or ... or Gn)
	 *     return not G1 and ... and not Gn
	 * F is "not (G => H)"
	 *     return R_top_simplify_conjunction(G and not H)
	 * F is "not (G <=> H)"
	 *     return R_top_simplify_disjunction(
	 *                  R_top_simplify_conjunction(G and not H)
	 *                  or
	 *                  R_top_simplify_conjunction(not G and H))
	 * F is "not (Qx G)" for Q a quantifier
	 *     return Q'x not G
	 *         where Q' is the opposite quantifier of Q
	 * </pre>
	 */
	String R_move_not_in = CARDINALITY_NAMESPACE+"R_move_not_in";

	/**
	 * <pre>
	 * R_quantifier_elimination(F)
	 * F is any formula
	 * Returns a quantifier-free formula equivalent to F
	 * 
	 * if F is "for all x: Y"
	 *     return R_normalize(R_card(|R_top_simplify(Y)|_X, "for all") = |type(x)| )
	 * if F is "there exists x: Y"
	 *     return R_normalize(R_card(|R_top_simplify(Y)|_X, "there exists") > 0)
	 * if F is "not G"
	 *     return R_quantifier_elimination(R_move_not_in(not G))
	 * if F is G => H
	 *     return R_quantifier_elimination(R_top_simplify_disjunction(not G or H))
	 * if F is G <=> H
	 *     return R_quantifier_elimination(
	 *             R_top_simplify_conjunction((not G or H) and (G or not H)))
	 * if F is a conjunction F1 and ... and Fn
	 *     F' <- True
	 *     i  <- 1
	 *     while F' is not "False" and i <= n
	 *         Gi <- R_quantifier_elimination(Fi)
	 *         F' <- add_conjunct_and_top_simplify(Gi, F')
	 *         i <- i + 1
	 *     return F'
	 * if F is a disjunction F1 or ... or Fn
	 *     F' <- False
	 *     i  <- 1
	 *     while F' is not "True" and i <= n
	 *         Gi <- R_quantifier_elimination(Fi)
	 *         F' <- add_disjunct_and_top_simplify(Gi, F')
	 *         i  <- i + 1
	 *     return F'
	 * return F
	 * </pre>
	 */
	String R_quantifier_elimination = CARDINALITY_NAMESPACE+"R_quantifier_elimination";

	/**
	 * R_normalize(E).<br>
	 * Interface for R_normalize(E) functionality used by the direct cardinality
	 * computation routines, that will not necessarily use full satisfiability
	 * testing as part of its simplification logic.
	 */
	String R_normalize = CARDINALITY_NAMESPACE+"R_normalize";

	/**
	 * R_complete_normalize(E).<br>
	 * Interface for R_complete_normalize(E) that extends the R_normalize
	 * functionality used by the direct cardinality computation routines, which
	 * will use full satisfiability testing as part of its simplification logic.
	 */
	String R_complete_normalize = CARDINALITY_NAMESPACE+"R_complete_normalize";

	/**
	 * R_simplify(E).<br>
	 * Interface for R_simplify(E) functionality used by R_normalize.
	 * It performs simplifications of expressions based on full or partial evaluation of known functions.
	 */
	String R_simplify = CARDINALITY_NAMESPACE+"R_simplify";

	/**
	 * R_complete_simplify(E).<br>
	 * Complete version of R_simplify(E), that is,
	 * guaranteeing that tautologies and contradictions are replaced by true and false respectively.
	 */
	String R_complete_simplify = CARDINALITY_NAMESPACE+"R_complete_simplify";

	/**
	 * Same as R_simplify(E), but assumes input is formula.
	 */
	String R_formula_simplify = CARDINALITY_NAMESPACE+"R_formula_simplify";

	/**
	 * <pre>
	 * R_sum_over_one_variable(sum_{x: Cx} S)
	 * Cx is a formula in boolean logic, constraining the values of the summation index x
	 * S is an Easily-Summable expression with respect to x, that is:
	 * 		if S is not an if-then-else expression, then x does not occur in S.
	 * 		if S is an expression of the form 'if Q then E1 else E2' where E1 and E2 are Easily-Summable with respect to x.
	 * Returns a counting solution
	 * Cases for input:
	 * S is "if F then S1 else S2".
	 *     return R_normalize(R_sum_over_one_variable(sum_{x:Cx and F} S1) + R_sum_over_one_variable(sum_{x:Cx and not F} S2)))
	 * S is a numeric constant expression.
	 *     if S is 0
	 *         return 0
	 *     else
	 *         return R_normalize(R_card(|Cx|_{x}) * S)
	 * </pre>
	 */
	String R_sum_over_one_variable = CARDINALITY_NAMESPACE+"R_sum_over_one_variable";

	/**
	 * <pre>
	 * R_top_implied_certainty(F, C)
	 * F is a formula under context C
	 * 
	 * if F is a formula
	 *     if is_tautology(C => F)
	 *         return true
	 *     if is_tautology(C => not F)
	 *         return false
	 * 
	 * return F
	 * </pre>
	 */
	String R_top_implied_certainty = CARDINALITY_NAMESPACE+"R_top_implied_certainty";

	/**
	 * <pre>
	 * R_top_quantifier_elimination(Qx F)
	 * Q is a quantifier ("there exists", or "for all") over x, F is boolean formula on equalities.
	 * Returns an equivalent formula to Qx F by eliminating the quantification Qx.
	 * 
	 * F <- R_top_simplify(F)
	 * if Q is "for all"
	 *    return R_normalize( R_card(|F|_X, Q) = |type(x)| )
	 * if Q is "there exists"
	 *    return R_normalize( R_card(|F|_X, Q) > 0 )
	 * </pre>
	 */
	String R_top_quantifier_elimination = CARDINALITY_NAMESPACE+"R_top_quantifier_elimination";

	/**
	 * <pre>
	 * R_top_simplify(F)
	 * if F is a conjunction
	 *     F <- R_top_simplify_conjunction(F)
	 * if F is a disjunction
	 *     F <- R_top_simplify_disjunction(F)
	 * return F
	 * </pre>
	 */
	String R_top_simplify = CARDINALITY_NAMESPACE+"R_top_simplify";

	/**
	 * <pre>
	 * R_top_simplify_conjunction(F1 and ... and Fn)
	 * Each Fi is any formula
	 * Returns simplification of top level of conjunction.
	 * 
	 * T <- empty tuple // T will contain conjuncts of F not equal to True or False
	 * for all i
	 *     if Fi is False or Alpha != Alpha
	 *         return False
	 *     if Fi is not "True" and Fi is not Alpha = Alpha
	 *         add Fi to T
	 * 
	 * T' <- empty tuple // T' will contain the elements of T that are needed in the final conjunction
	 * irrelevant <- empty set
	 * for all i in 1,..., |T|
	 *     if Ti is in irrelevant
	 *         continue to next i
	 *     i_is_irrelevant <- false
	 *     for all j in i + 1, ..., |T|
	 *         if incomplete_linear_implies(Tj, Ti)
	 *             i_is_irrelevant <- true
	 *             continue to next i
	 *         if incomplete_linear_implies(Ti, Tj)
	 *             irrelevant <- irrelevant union { Tj }
	 *         else if incomplete_linear_implies(Ti, not Tj)
	 *                 or incomplete_linear_implies(Tj, not Ti)
	 *             return False // conjunction is contradiction
	 *     if not i_is_irrelevant
	 *         add Ti to T'
	 * 
	 * return conjunction on elements of T'
	 * </pre>
	 */
	String R_top_simplify_conjunction = CARDINALITY_NAMESPACE+"R_top_simplify_conjunction";

	/**
	 * <pre>
	 * R_top_simplify_disjunction(F1 or ... or Fn)
	 * Each Fi is any formula
	 * Returns simplification of top level of disjunction.
	 * 
	 * T <- empty tuple // T will contain disjuncts of F not equal to True or False
	 * for all i
	 *     if Fi is True or Alpha = Alpha
	 *         return True
	 *     if Fi is not "False" and Fi is not Alpha != Alpha
	 *         add Fi to T
	 * 
	 * T' <- empty tuple // T' will contain the elements of T that are needed in the final disjunction
	 * irrelevant <- empty set
	 * for all i in 1,..., |T|
	 *     if Ti is in irrelevant
	 *         continue to next i
	 *     i_is_irrelevant <- false
	 *     for all j in i + 1, ..., |T|
	 *         if incomplete_linear_implies(Ti, Tj)
	 *             i_is_irrelevant <- true
	 *             continue to next i
	 *         if incomplete_linear_implies(Tj, Ti)
	 *             irrelevant <- irrelevant union { Tj }
	 *         else if incomplete_linear_implies(not Ti, Tj)
	 *                 or incomplete_linear_implies(not Tj, Ti)
	 *             return True // disjunction is tautology
	 *     if not i_is_irrelevant
	 *         add Ti to T'
	 * 
	 * return disjunction on elements of T'
	 * </pre>
	 */
	String R_top_simplify_disjunction = CARDINALITY_NAMESPACE+"R_top_simplify_disjunction";
	
	String R_incomplete_implied_certainty = CARDINALITY_NAMESPACE+"R_incomplete_implied_certainty";
}
