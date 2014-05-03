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
package com.sri.ai.grinder.library.equality.cardinality;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.grinder.api.Rewriter;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.core.TotalRewriter;
import com.sri.ai.grinder.helper.Trace;
import com.sri.ai.grinder.library.Disequality;
import com.sri.ai.grinder.library.Equality;
import com.sri.ai.grinder.library.FunctorConstants;
import com.sri.ai.grinder.library.boole.And;
import com.sri.ai.grinder.library.boole.BooleanUtil;
import com.sri.ai.grinder.library.boole.Not;
import com.sri.ai.grinder.library.boole.Or;
import com.sri.ai.grinder.library.controlflow.IfThenElse;
import com.sri.ai.grinder.library.equality.cardinality.direct.CardinalityRewriter;
import com.sri.ai.grinder.library.equality.cardinality.direct.CardinalityRewriter.Quantification;
import com.sri.ai.grinder.library.equality.cardinality.direct.core.CardinalityTypeOfLogicalVariable;
import com.sri.ai.grinder.library.equality.cardinality.direct.core.FromConditionalFormulaToFormula;
import com.sri.ai.grinder.library.equality.cardinality.direct.core.SortPair;
import com.sri.ai.grinder.library.equality.formula.FormulaUtil;
import com.sri.ai.grinder.library.indexexpression.IndexExpressions;
import com.sri.ai.grinder.library.number.Plus;
import com.sri.ai.grinder.library.number.Times;
import com.sri.ai.grinder.library.set.Sets;
import com.sri.ai.grinder.library.set.intensional.IntensionalSet;
import com.sri.ai.grinder.library.set.tuple.Tuple;
import com.sri.ai.util.base.Pair;
import com.sri.ai.util.collect.DisjointSets;

/**
 * A collection of utility routines specific to cardinality computations.
 * 
 * @author oreilly
 *
 */
@Beta
public class CardinalityUtil {
	
	public static Expression makeNot(Expression expression) {
		Expression result = null;
		if (expression.hasFunctor(FunctorConstants.NOT)) {
			result = expression.get(0);
		}
		else {
			result = Not.make(expression);
		}
		return result;
	}
	
	public static Expression makeAnd(Expression conjunct1, Expression conjunct2) {
		Expression result = null;
		List<Expression> conjuncts = new ArrayList<Expression>();
		if (And.isConjunction(conjunct1)) {
			conjuncts.addAll(conjunct1.getArguments());
		}
		else {
			conjuncts.add(conjunct1);
		}
		
		if (And.isConjunction(conjunct2)) {
			conjuncts.addAll(conjunct2.getArguments());
		}
		else {
			conjuncts.add(conjunct2);
		}
		
		result = And.make(conjuncts);
		
		return result;
	}
	
	public static Expression makeOr(Expression disjunct1, Expression disjunct2) {
		Expression result = null;
		List<Expression> disjuncts = new ArrayList<Expression>();
		if (Or.isDisjunction(disjunct1)) {
			disjuncts.addAll(disjunct1.getArguments());
		}
		else {
			disjuncts.add(disjunct1);
		}
		
		if (Or.isDisjunction(disjunct2)) {
			disjuncts.addAll(disjunct2.getArguments());
		}
		else {
			disjuncts.add(disjunct2);
		}
		
		result = Or.make(disjuncts);
		
		return result;
	}
	
	/**
	 * Let F be a quantifier formula in L. Let |F|_{I1, ..., In} denote the
	 * cardinality of the set {(on I1,..., In) (x1, ..., xn) | F},
	 * where xi is the index in I_i. 
	 * 
	 * @param formulaF
	 *            a quantifier formula.
	 * @param indexExpressions
	 *            the index expressions I1 to In.
	 * @return an expression of the form | { (on I1,..., In) (x1, ..., xn) | F} |
	 */
	public static Expression makeCardinalityOfIndexedFormulaExpression(Expression formulaF, Expression... indexExpressions) {
		Expression result = null;
		
		List<Expression> indexExpressionsList  = Arrays.asList(indexExpressions);
		
		List<Expression> indicesList  = IndexExpressions.getIndices(indexExpressionsList);
		Expression       indicesTuple = Tuple.make(indicesList);
		
		result = Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees(FunctorConstants.CARDINALITY, IntensionalSet.makeUniSetFromIndexExpressionsList(indexExpressionsList, indicesTuple, formulaF));
		
		return result;
	}
	
	/**
	 * Determine whether or not the input expression is of the form:<br> 
	 * | {(on x1,..., xn) (x1, ..., xn) | F} |<br>
	 * or<br>
	 * | {{(on x1,..., xn) (x1, ..., xn) | F}} |<br>
	 * or<br>
	 * | {{(on x1,..., xn) E | F}} |<br>
	 * 
	 * Note: | {(on x1,..., xn) E | F} | is currently not supported as it
	 * requires an analysis of E to see which instantiations turn out to 
	 * be the same, and  we don't have code to do 
	 * @param expression
	 *            the expression to be checked.
	 * @return if the expression is of the correct form, false otherwise.           
	 */
	public static boolean isCardinalityOfIndexedFormulaExpression(Expression expression) {
		boolean result = false;
		if (expression.hasFunctor(FunctorConstants.CARDINALITY) && expression.numberOfArguments() == 1) {

			Expression intensionalSet = expression.get(0);
			
			if (Sets.isEmptySet(intensionalSet)) {
				result = true;
			}
			else if (Sets.isIntensionalMultiSet(intensionalSet)) {
				// | {{(x1, ..., xn) | F}{x1,..., xn}} | and | {{ E | F}{x1,..., xn}} |
				// are legal expressions
				result = true;
			}
			else if (Sets.isIntensionalUniSet(intensionalSet)) {
				// For unisets, only | {(I1, ..., In) | F}{x1,..., xn} |
				// is legal as | {E | F}{I1, ..., In} | 
				// requires an analysis of E to see which instantiations 
				// turn out to be equal, and  we don't have code to do 
				// that at this time
				Expression intensionalSetHead = IntensionalSet.getHead(intensionalSet);
			    if (Tuple.isTuple(intensionalSetHead)) {	    	
			    	Set<Expression> intensionalSetIndices = new LinkedHashSet<Expression>(IndexExpressions.getIndices(IntensionalSet.getIndexExpressions(intensionalSet)));
			    	Set<Expression> tupleIndices          = new LinkedHashSet<Expression>(Tuple.getElements(intensionalSetHead));
			    	// The tuple and the indices on the uni-intensional set need to match
			    	if (intensionalSetIndices.equals(tupleIndices)) {
			    		result = true;
			    	}
			    }
			}
		}
		return result;
	}
	
	/**
	 * Assert whether or not the input expression is of the form:<br> 
	 * | {(on x1,..., xn) (x1, ..., xn) | F} |<br>
	 * or<br>
	 * | {{(on x1,..., xn) (x1, ..., xn) | F}} |<br>
	 * or<br>
	 * | {{(on x1,..., xn) E | F}} |<br>
	 * 
	 * @param expression
	 *            the expression to be checked.
	 * @throws IllegalArgumentException
	 *             if the expression is not of the correct form.
	 */
	public static void assertIsCardinalityOfIndexedFormulaExpression(Expression expression) {
		if (!isCardinalityOfIndexedFormulaExpression(expression)) {
			throw new IllegalArgumentException("Argument is not a legal cardinality of indexed formula expression:"+expression);
		}
	}
		
	/**
	 * Receives index expressions X1 [in D1], ..., Xn [in Dn] and
	 * makes expression of the form | D1 | * ... * | Dn |,
	 * assuming Di to be "type(Xi)" if the index expression is Xi alone.
	 */
	public static Expression makeCardinalityOfIndexExpressions(List<Expression> indexExpressions) {
		// TODO: When ALBP-119 is resolved, we will be able to take the domain of the variable from the quantified expression.
		Expression result = Expressions.ONE;
		if ( indexExpressions.size() > 0 ) {
			ArrayList<Expression> cardinalities = new ArrayList<Expression>();
			for (Expression indexExpression: indexExpressions) {
				Expression set;
				if (indexExpression.hasFunctor(FunctorConstants.IN)){
					set = indexExpression.get(1);
				}
				else {
					set = Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees(CardinalityTypeOfLogicalVariable.FUNCTOR_TYPE, indexExpression);
				}
				Expression cardinality = Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees(FunctorConstants.CARDINALITY, set);
				cardinalities.add(cardinality);
			}
			result = Times.make(cardinalities);
		}
		return result;
	}
	
	/**
	 * Make an expression of the form: sum_{x: Cx} S.
	 * 
	 * @param indexX
	 *            x, a single index expression.
	 * @param constraintsOnX
	 *            Cx, a quantifier-free formula constraining the values of the
	 *            summation index x.
	 * @param countingSolutionS
	 *            S, a counting solution with respect to x.
	 * @return an expression of the form: sum_{x: Cx} S.
	 */
	public static Expression makeSummationExpression(Expression indexX, Expression constraintsOnX, Expression countingSolutionS) {
		Expression result = Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees(
				FunctorConstants.SUM,
				IntensionalSet.makeMultiSetWithASingleIndexExpression(indexX, countingSolutionS, constraintsOnX));
		return result;
	}
	
	/**
	 * Determine whether or not the input expression is of the form:<br> 
	 * sum_{x: Cx} S, which corresponds to the 'sum' function on a multiset with a single index.
	 * 
	 * @param expression
	 *            the expression to be checked.
	 * @return if the expression is of the correct form, false otherwise.           
	 */
	public static boolean isSummationExpression(Expression expression) {
		boolean result = false;
		
		if (expression.hasFunctor(FunctorConstants.SUM) &&
		    expression.numberOfArguments() == 1) {
			Expression sumArgument = expression.get(0);
			if (Sets.isIntensionalMultiSet(sumArgument) && IntensionalSet.getIndexExpressions(sumArgument).size() == 1) {
				result = true;
			}
		}
		
		return result;
	}
	
	/**
	 * Assert whether or not the input expression is of the form:<br> 
	 * sum_{x: Cx} S, which corresponds to the 'sum' function on a multiset with a single index.
	 * 
	 * @param expression
	 *            the expression to be checked.
	 * @throws IllegalArgumentException
	 *             if the expression is not of the correct form.
	 */
	public static void assertSummationExpression(Expression expression) {
		if (!isSummationExpression(expression)) {
			throw new IllegalArgumentException("Argument is not a summation expression sum_{x: Cx} S (it must be an application of 'sum' on a multiset): " + expression);
		}
	}
	
	/**
	 * Get the index expression x from the expression: sum_{x: Cx} S.<br>
	 * Note: isSummationExpression() or assertSummationExpression() should be
	 * called before using this.
	 * 
	 * @param expression
	 *            an expression of the form sum_{x: Cx} S.
	 * @return the index expression x.
	 */
	public static Expression getIndexXFromSummation(Expression expression) {
		Expression result = IntensionalSet.getIndexExpressions(expression.get(0)).get(0);
		
		return result;
	}
	
	/**
	 * Get the constraints expression Cx on the index x from the expression:
	 * sum_{x: Cx} S.<br>
	 * Note: isSummationExpression() or assertSummationExpression() should be
	 * called before using this.
	 * 
	 * @param expression
	 *            an expression of the form sum_{x: Cx} S.
	 * @return the constraints expression Cx on the index x.
	 */
	public static Expression getConstraintsOnXFromSummation(Expression expression) {
		Expression result = IntensionalSet.getCondition(expression.get(0));
		
		return result;
	}
	
	/**
	 * Get the counting-solution expression S from the expression:
	 * sum_{x: Cx} S.<br>
	 * Note: isSummationExpression() or assertSummationExpression() should be
	 * called before using this.
	 * 
	 * @param expression
	 *            an expression of the form sum_{x: Cx} S.
	 * @return the counting-solution S.
	 */
	public static Expression getCountingSolutionSFromSummation(Expression expression) {
		Expression result = IntensionalSet.getHead(expression.get(0));
		
		return result;
	}
	
	/**
	 * Determines if an expression is an explicit conjunction 'and(...)' or an
	 * equality or inequality formula (i.e. literal), or the boolean constant
	 * formulas true and false, which are considered an implied conjunctions.
	 * 
	 * @param expression
	 *            the expression to be tested for whether or not it is to be
	 *            considered a conjunction.
	 * @param process
	 *            the rewriting process in which the rewriting is occurring.
	 * @return true if should be considered as a conjunction, false otherwise.
	 */
	public static boolean isConjunctionOrImpliedConjunction(Expression expression, RewritingProcess process) {
		boolean result = false;
		if (And.isConjunction(expression)        ||
			expression.equals(Expressions.FALSE) ||
			expression.equals(Expressions.TRUE)  ||
			((expression.hasFunctor(FunctorConstants.EQUAL) || expression.hasFunctor(FunctorConstants.INEQUALITY)) 
					&& FormulaUtil.isFormula(expression, process))
			) {
			result = true;
		}
		return result;
	}
	
	/**
	 * Utility to allow working with arbitrary sized disjunctions so that it can
	 * be assumed that a disjunction is of the form (F1 or F2).
	 * 
	 * @param disjunction
	 *            a disjunction of 0 or more disjuncts.
	 * @return return a disjunct representing F1.
	 */
	public static Expression getF1FromDisjunction(Expression disjunction) {
		Expression result = Expressions.FALSE;
		if (Or.isDisjunction(disjunction) && disjunction.numberOfArguments() > 0) {
			result = disjunction.get(0);
		}
		else {
			throw new IllegalArgumentException("Is not a disjunction:"+disjunction);
		}
		return result;
	}
	
	/**
	 * Utility to allow working with arbitrary sized disjunctions so that it can
	 * be assumed that a disjunction is of the form (F1 or F2).
	 * 
	 * @param disjunction
	 *            a disjunction of 0 or more disjuncts.
	 * @return return a disjunct representing F2.
	 */
	public static Expression getF2FromDisjunction(Expression disjunction) {
		Expression result = Expressions.FALSE;
		if (Or.isDisjunction(disjunction)) {
			if (disjunction.numberOfArguments() == 2) {
				result = disjunction.get(1);
			}
			else if (disjunction.numberOfArguments() > 2) {
				result = Or.make(disjunction.getArguments().subList(1, disjunction.numberOfArguments()));
			}
		}
		else {
			throw new IllegalArgumentException("Is not a disjunction:"+disjunction);
		}
		return result;
	}
	
	/**
	 * 
	 * @param expression
	 *            the expression to be tested.
	 * @return true if the expressions is of the form 'Alpha = Alpha', false
	 *         otherwise.
	 */
	public static boolean isEqualityOnSameTerms(Expression expression) {
		boolean result = false;
		if (expression.hasFunctor(FunctorConstants.EQUAL)) {
			Set<Expression> terms = new LinkedHashSet<Expression>(expression.getArguments());
			if (terms.size() == 1) {
				result = true;
			}
		}		
		return result;
	}
	
	/**
	 * 
	 * @param expression
	 *            the expression to be tested.
	 * @return true if the expressions is of the form 'Alpha != Alpha', false
	 *         otherwise.
	 */
	public static boolean isDisequalityOnSameTerms(Expression expression) {
		boolean result = false;
		
		if (expression.hasFunctor(FunctorConstants.INEQUALITY) &&
		    expression.numberOfArguments() == 2) {
			result = expression.get(0).equals(expression.get(1));
		}
		
		return result;
	}
	
	/**
	 * Determine if the index variable x is in the formula F.
	 * 
	 * @param indexX
	 *            the index variable x.
	 * @param f
	 *            the formula F.
	 * @return true if x is in the formula F, false otherwise.
	 */
	public static boolean isIndexXNotInF(Expression indexX, Expression f, RewritingProcess process) {
		boolean result = true;
		
		Set<Expression> freeVariables = Expressions.freeVariables(f, process);
		if (freeVariables.contains(indexX)) {
			result = false;
		}
		
		return result;
	}
	
	/**
	 * Based on the idea that independent problems linked by conjunction have a
	 * number of solutions equal to the result of the solutions of independent problems.
	 * For example:<br>
	 * 
	 * <pre>
	 * | X = a and Y != b |_X,Y 
	 * = 
	 * |X=a|_X * |Y!=b|_Y
	 * 
	 * and
	 * 
	 * | Z != c |_X,Y 
	 * = 
	 * |T|_X * |T|_Y * | Z != c |0
	 * </pre>
	 * 
	 * This utility routine checks if the expression F passed in is a
	 * conjunction, and there is a partition {I_1, ..., I_k} of indices such that
	 * there is a partition { C_1, ..., C_k } of the conjuncts of F, where
	 * indices in I_j occur in C_j only, for every j.
	 * 
	 * @param f
	 *            the expression F to be tested for whether or not it is a
	 *            conjunction that is partitioned on indices.
	 * @param indexExpressions
	 *            the index expressions, the indices of which we must partition F on.
	 * @param process
	 *            the process in which the rewriting is occurring.
	 * @return a list of independent problems of the form Pair<Set<Expression>,
	 *         List<Expression>>, where Pair.first are the indices associated
	 *         with the independent problem and Pair.second are the conjuncts
	 *         from F that are partitioned by these indices. Note: if there
	 *         are no independent problems an empty list will be returned.
	 */
	public static List<Pair<Set<Expression>, List<Expression>>> findIndependentProblemsInConjunction(Expression f, List<Expression> indexExpressions, RewritingProcess process) {
		List<Pair<Set<Expression>, List<Expression>>> result = null;
		if ( isConjunctionOrImpliedConjunction(f, process) ) {
			List<Expression> conjuncts = And.getConjuncts(f);
			result = findIndependentProblems(conjuncts, indexExpressions, process);
		}
		else {
			result = new ArrayList<Pair<Set<Expression>, List<Expression>>>(); // FIXME: wouldn't it more efficient just to return null? Why construct a list?
		}
		return result;
	}
	
	/**
	 * Based on the idea that independent problems linked by disjunction have a
	 * number of solutions equal to the result of the solutions of independent problems.
	 * For example:<br>
	 * 
	 * <pre>
	 * | X = a or Y != b |_X,Y 
	 * = 
	 * |X=a|_X*|type(Y)| + |Y!=b|_Y*|type(X)| - |X=a|_X*|Y!=b|_Y
	 * 
	 * </pre>
	 * 
	 * This utility routine checks if the expression F passed in is a
	 * disjunction, and there is a partition {I_2, I_3} of indices such that
	 * there is a partition { D1, D2, D3 } of the disjuncts of F, where
	 * indices in I_j occur in C_j only, for every j. Note that D1 is a partition with no index variables
	 * and it may or may not exist. So the final list could be of length 2 or 3 depending on whether there
	 * is a partition without any index variables.
	 * 
	 * @param f
	 *            the expression F to be tested for whether or not it is a
	 *            disjunction that is partitioned on indices.
	 * @param indexExpressions
	 *            the index expressions, on which indices to partition F on.
	 * @param process
	 *            the process in which the rewriting is occurring.
	 * @return a list of independent problems of the form Pair<Set<Expression>,
	 *         List<Expression>>, where Pair.first are the indices associated
	 *         with the independent problem and Pair.second are the conjuncts disjuncts
	 *         from F that are partitioned by these indices. Note: if there
	 *         are no independent problems an empty list will be returned.
	 *         Also, there will be at most 3 partitions:
	 *         the first partition may be one with no indices. If the first partition
	 *         has no indices, then there will be at most 2 other partitions with indices.
	 */
	public static List<Pair<Set<Expression>, List<Expression>>> findIndependentProblemsInDisjunction(Expression f, List<Expression> indexExpressions, RewritingProcess process) {
		List<Pair<Set<Expression>, List<Expression>>> result = null;
		if ( Or.isDisjunction(f) ) {
			List<Expression> subFormulas = new ArrayList<Expression>();
			subFormulas.addAll(f.getArguments());
			result = findIndependentProblems(subFormulas, indexExpressions, process);		
			// For a disjunction, we can have at most three partitions: one partition with no indices, and at most two disjoint partitions:
			if ( result.size() > 1 ) {
				Integer emptyIndex = -1, maxIndex = -1, maxElements = -1;
				Pair<Set<Expression>, List<Expression>> emptyOne = null, maxOne = null;
				for (int i=0; i<result.size(); i++) {
					Pair<Set<Expression>, List<Expression>> pair = result.get(i);
					int indicesSize = pair.first.size();
					if ( indicesSize == 0 ) {
						emptyIndex = i;
					}
					if ( indicesSize > maxElements ) {
						maxElements = indicesSize;
						maxIndex = i;
					}
				}
				if ( emptyIndex > -1 ) {
					emptyOne = result.get(emptyIndex);
					result.remove(emptyOne);
				}
				if ( maxIndex > -1 ) {
					maxOne = result.get(maxIndex);
					result.remove(maxOne);
				}
				if ( result.size() > 1 ) {
					Pair<Set<Expression>, List<Expression>> mainOne = result.get(0);
					for (int i=1; i<result.size(); i++) {
						Pair<Set<Expression>, List<Expression>> toRemove = result.get(i);
						mainOne.first.addAll(toRemove.first);
						mainOne.second.addAll(toRemove.second);
					}
					result.clear();
					result.add(mainOne);
					if ( maxOne!=null ) {
						result.add(0, maxOne);
					}
					if ( emptyOne!=null ) {
						result.add(0, emptyOne);
					}
				}
				else {
					if ( maxOne!= null ) {
						result.add(0, maxOne);
					}
					if ( emptyOne!= null ) {
						result.add(0, emptyOne);
					}
				}
			}
			
		}
		else {
			result = new ArrayList<Pair<Set<Expression>, List<Expression>>>();
		}
		return result;
	}
	
	/**
	 * Takes a conjunction or disjunction expression and separates its arguments into those dependent and independent on given variables,
	 * returning a pair of expressions of the same type, the first with the arguments independent of the variables, and the second on the arguments dependent of the variables.
	 * The expression can also be a literal, a multi-equality, or even TRUE/FALSE.
	 * Throws an exception if expression is neither conjunction or disjunction.
	 */
	public static Pair<Expression, Expression> separateIndependentAndDependent(Expression expression, List<Expression> variables, Expression emptyCase, RewritingProcess process) {
		Pair<Expression, Expression> pair = new Pair<Expression, Expression>();
		if ( BooleanUtil.isMultiEquality(expression) ) {
			expression = BooleanUtil.expandMultiEquality(expression, expression.get(0));
		}
		Expression functor = expression.getFunctor();
		if (Or.isDisjunction(expression) || And.isConjunction(expression)) {
			List<Expression> independentFormulas = new ArrayList<Expression>();
			List<Expression> dependentFormulas = new ArrayList<Expression>();
			for (Expression sub: expression.getArguments()) {
				boolean independent = true;
				Set<Expression> freeVariables = Expressions.freeVariables(sub, process);
				for (Expression variable: variables) {
					if ( freeVariables.contains(variable) ) {
						independent = false;
						break;						
					}
				}
				if ( independent ) {
					independentFormulas.add(sub);
				}
				else {
					dependentFormulas.add(sub);
				}
			}
			if ( independentFormulas.isEmpty() ) {
				pair.first = emptyCase;
			}
			else if ( independentFormulas.size()==1 ) {
				pair.first = independentFormulas.get(0);
			}
			else {
				pair.first = Expressions.apply(functor, independentFormulas.toArray());
			}
			
			if ( dependentFormulas.isEmpty() ) {
				pair.second = emptyCase;
			}
			else if ( dependentFormulas.size()==1 ) {
				pair.second = dependentFormulas.get(0);
			}
			else {
				pair.second = Expressions.apply(functor, dependentFormulas.toArray());
			}
		}
		else if ( BooleanUtil.isEquality(expression) || BooleanUtil.isNotEquality(expression) ) {
			boolean independent = true;
			Set<Expression> freeVariables = Expressions.freeVariables(expression, process);
			for (Expression variable: variables) {
				if ( freeVariables.contains(variable) ) {
					independent = false;
					break;						
				}
			}
			if ( independent ) {
				pair.first = expression;
				pair.second = emptyCase;
			}
			else {
				pair.first = emptyCase;
				pair.second = expression;
			}
		}
		else if ( expression.equals(Expressions.TRUE) || expression.equals(Expressions.FALSE) ) {
			pair.first = expression;
			pair.second = emptyCase;			
		}
		else {
			throw new IllegalArgumentException("The expression should be a conjunction or a disjunction, but is " + expression);
		}
		return pair;
	}
	
	/**
	 * Convenience method for constructing a tuple argument of the form:<br> 
	 * (| F |_X, quantification)<br>
	 * when calling:<br>
	 * R_card( | F |_X, quantification )
	 * @param expression
	 * 			an expression of the form | F |_X. F is a boolean
	 *          formula (possibly with quantifiers) on equalities. It may
	 *          contain free variables with respect to x1, ..., xn.
	 * @param quantification
	 *        is 'none', 'for all', or 'there exists'. 
	 * @return a tuple argument of the form: (| F |_X, quantification)
	 */
	public static Expression argForCardinalityWithQuantifierSpecifiedCall(Expression expression, Quantification quantification) {
		Expression result = Tuple.make(expression, quantification.getQuantificationSymbol());
		return result;
	}
	
	/**
	 * Convenience method for constructing a tuple argument of the form:<br> 
	 * (| F |_X, quantification)<br>
	 * when calling:<br>
	 * R_card_conjunction( | F |_X, quantification )
	 * @param expression
	 * 			an expression of the form | F |_X. F is a boolean
	 *          formula (possibly with quantifiers) on equalities. It may
	 *          contain free variables with respect to x1, ..., xn.
	 * @param quantification
	 *        is 'none', 'for all', or 'there exists'. 
	 * @return a tuple argument of the form: (| F |_X, quantification)
	 */
	public static Expression argForCardinalityConjunctionCall(Expression expression, Quantification quantification) {
		Expression result = Tuple.make(expression, quantification.getQuantificationSymbol());
		return result;
	}
	
	/**
	 * Convenience method for constructing a tuple argument of the form:<br> 
	 * (| F |_X, quantification)<br>
	 * when calling:<br>
	 * R_card_conjunction_of_disequalities( | F |_X, quantification )
	 * @param expression
	 * 			an expression of the form | F |_X. F is a boolean
	 *          formula (possibly with quantifiers) on equalities. It may
	 *          contain free variables with respect to x1, ..., xn.
	 * @param quantification
	 *        is 'none', 'for all', or 'there exists'. 
	 * @return a tuple argument of the form: (| F |_X, quantification)
	 */
	public static Expression argForCardinalityConjunctionOfDisequalitiesCall(Expression expression, Quantification quantification) {
		Expression result = Tuple.make(expression, quantification.getQuantificationSymbol());
		return result;
	}
	
	/**
	 * Convenience method for constructing a tuple argument of the form:<br> 
	 * (| F |_X, quantification)<br>
	 * when calling:<br>
	 * R_card_disjunction(| F |_X, quantification)
	 * @param expression
	 * 			an expression of the form | F |_X. F is a boolean
	 *          formula (possibly with quantifiers) on equalities. It may
	 *          contain free variables with respect to x1, ..., xn.
	 * @param quantification
	 *        is 'none', 'for all', or 'there exists'. 
	 * @return a tuple argument of the form: (| F |_X, quantification)
	 */
	public static Expression argForCardinalityDisjunctionCall(Expression expression, Quantification quantification) {
		Expression result = Tuple.make(expression, quantification.getQuantificationSymbol());
		return result;
	}
	
	/**
	 * Convenience method for constructing a tuple argument of the form:<br> 
	 * (| F |_X, quantification)<br>
	 * when calling:<br>
	 * R_card_implication(| F |_X, quantification)
	 * @param expression
	 * 			an expression of the form | F |_X. F is a boolean
	 *          formula (possibly with quantifiers) on equalities. It may
	 *          contain free variables with respect to x1, ..., xn.
	 * @param quantification
	 *        is 'none', 'for all', or 'there exists'. 
	 * @return a tuple argument of the form: (| F |_X, quantification)
	 */
	public static Expression argForCardinalityImplicationCall(Expression expression, Quantification quantification) {
		Expression result = Tuple.make(expression, quantification.getQuantificationSymbol());
		return result;
	}	
	
	/**
	 * Convenience method for constructing a tuple argument of the form:<br> 
	 * (| F |_X, quantification)<br>
	 * when calling:<br>
	 * R_card_equivalence(| F |_X, quantification)
	 * @param expression
	 * 			an expression of the form | F |_X. F is a boolean
	 *          formula (possibly with quantifiers) on equalities. It may
	 *          contain free variables with respect to x1, ..., xn.
	 * @param quantification
	 *        is 'none', 'for all', or 'there exists'. 
	 * @return a tuple argument of the form: (| F |_X, quantification)
	 */
	public static Expression argForCardinalityEquivalenceCall(Expression expression, Quantification quantification) {
		Expression result = Tuple.make(expression, quantification.getQuantificationSymbol());
		return result;
	}
	
	/**
	 * Convenience method for constructing a tuple argument of the form:<br>
	 * (| x_i = t and Phi |_X, quantification)<br>
	 * when calling:<br>
	 * R_equality_in_conjunction(| x_i = t and Phi |_X, quantification)
	 * 
	 * @param expression
	 *            an expression of the form: | x_i = t and Phi |_X.<br>
	 *            Phi is a formula, x_i is one of the index variables in X, t is
	 *            a variable or a constant
	 * @param quantification
	 *            is 'none', 'for all', or 'there exists'.
	 * @return a tuple argument of the form: (| x_i = t and Phi |_X, quantification)
	 */
	public static Expression argForEqualityInConjunctionCall(Expression expression, Quantification quantification) {
		Expression result = Tuple.make(expression, quantification.getQuantificationSymbol());
		return result;
	}
	
	/**
	 * DPLL Style conditioning on the expression in order to calculate the
	 * cardinality (intended to avoid exponential memory usage).
	 * 
	 * <pre>
	 * dpllConditioning(F, Alpha, X, quantification)
	 * 
	 * G1 <- R_normalize(F and Alpha)
     * G2 <- R_normalize(F and not Alpha)
     *
     * (G1, G2) <- sort(G1, G2)
     *
     * N1 <- R_card(| G1 |_X, quantification)
     *
     * if quantification is "there exists" and every leaf of N1 is a numeric constant > 0 // same as R_normalize(N1 > 0) is "true"
     *     return || X ||
     *
     * N2 <- R_card(| G2 |_X, quantification)
     *
     * if quantification is "there exists" and every leaf of N2 is a numeric constant > 0 // same as R_normalize(N2 > 0) is "true"
     *    return || X ||
     *
     * return R_normalize(N1 + N2)
	 * </pre>
	 * 
	 * @param formulaF
	 *        the formula F from the original cardinality expression: | F |_X.
	 * @param literalAlpha
	 *        a literal.
	 * @param sortPair
	 *        an implementation of the SortPair interface, which sorts the cheapest first.
	 * @param quantification
	 *        is either "there exists", "for all", or "none".
	 * @param indicesX
	 *        the indices from the original cardinality expression: | F |_X.
	 * @param process
	 *        the current rewriting process.
	 * @return the cardinality of | F and Alpha |_X + | F and not Alpha |_X.
	 */
	public static Expression dpllConditioning(Expression formulaF, Expression literalAlpha, Expression[] indicesX, Quantification quantification, SortPair sortPair, RewritingProcess process) {
		Expression result = null;
		
		Trace.log("G1 <- R_normalize(F and Alpha)");
		Expression g1 = process.rewrite(CardinalityRewriter.R_normalize, makeAnd(formulaF, literalAlpha));
		Trace.log("G2 <- R_normalize(F and not Alpha)");
		Expression g2 = process.rewrite(CardinalityRewriter.R_normalize, makeAnd(formulaF, makeNot(literalAlpha)));
		
		Trace.log("(G1, G2) <- sort(G1, G2)");
		Pair<Expression, Expression> sortedPair = sortPair.sort(g1, g2);
		g1 = sortedPair.first;
		g2 = sortedPair.second;
		
		Trace.log("N1 <- R_card(| G1 |_X, quantification)");
		Expression n1, n2;
		Expression cardG1 = makeCardinalityOfIndexedFormulaExpression(g1, indicesX);
		n1 = process.rewrite(CardinalityRewriter.R_card, argForCardinalityWithQuantifierSpecifiedCall(cardG1, quantification));		
		if (quantification == Quantification.THERE_EXISTS && everyLeafIsConstantGreaterThanZero(n1)) {
			Trace.log("if quantification is \"there exists\" and every leaf of N1 is a numeric constant > 0 // same as R_normalize(N1 > 0) is \"true\"");
			Trace.log("    return || X ||");
			result = process.rewrite(CardinalityRewriter.R_normalize, CardinalityUtil.makeCardinalityOfIndexExpressions(Arrays.asList(indicesX)));
		}
		else {
			Trace.log("N2 <- R_card(| G2 |_X, quantification)");
			Expression cardG2 = makeCardinalityOfIndexedFormulaExpression(g2, indicesX);
			n2 = process.rewrite(CardinalityRewriter.R_card, argForCardinalityWithQuantifierSpecifiedCall(cardG2, quantification));		
			if (quantification == Quantification.THERE_EXISTS && everyLeafIsConstantGreaterThanZero(n2)) {
				Trace.log("if quantification is \"there exists\" and every leaf of N2 is a numeric constant > 0 // same as R_normalize(N2 > 0) is \"true\"");
				Trace.log("    return || X ||");
				result = process.rewrite(CardinalityRewriter.R_normalize, CardinalityUtil.makeCardinalityOfIndexExpressions(Arrays.asList(indicesX)));
			}
			else {
				Trace.log("return R_normalize(N1 + N2)");
				List<Expression> arguments = new ArrayList<Expression>();
				arguments.add(n1);
				arguments.add(n2);
				result = process.rewrite(CardinalityRewriter.R_normalize, Plus.make(arguments));
			}
		}
		
		return result;
	}
	
	public static boolean everyLeafIsConstantGreaterThanZero(Expression expression) {
		boolean result = false;
		
		if (IfThenElse.isIfThenElse(expression)) {
			result = everyLeafIsConstantGreaterThanZero(IfThenElse.getThenBranch(expression)) && everyLeafIsConstantGreaterThanZero(IfThenElse.getElseBranch(expression));
		}
		else if (expression.getSyntacticFormType().equals("Symbol")) {
			if (expression.getValue() instanceof Number) {
				if (expression.rationalValue().isPositive()) {
					result = true;
				}
			}
		}
		
		return result;
	}
	
	public static Expression pickCheapestLiteral(Expression formula) {
// TODO - remove: Original Trivial Implementation.		
		Expression result = null;
		
		if (Equality.isEquality(formula) ||  Disequality.isDisequality(formula)) {
			result = formula;
		}
		else {
			result = pickCheapestLiteral(formula.get(0));
		}
	
// Experimental Version:
//		Expression result = null;
//		
//		Set<Expression> excludeLiterals = new HashSet<Expression>();
//		if (And.isConjunction(formula)) {
//			for (Expression conjunct: And.getConjuncts(formula)) {
//				if (Equality.isEquality(conjunct) ||  Disequality.isDisequality(conjunct)) {
//					excludeLiterals.add(conjunct);
//				}
//			}
//		}
//		
//		Map<Expression, Integer> literalCounts = new LinkedHashMap<Expression, Integer>();
//		SubExpressionsDepthFirstIterator subei = new SubExpressionsDepthFirstIterator(formula);
//		while (subei.hasNext()) {
//			Expression e = subei.next();
//			if (Equality.isEquality(e) || Disequality.isDisequality(e)) {
//				Integer count = literalCounts.get(e);
//				if (count == null) {
//					count = 0;
//				}
//				count = count+1;
//				literalCounts.put(e, count);
//			}
//		}
//		
//		Map.Entry<Expression, Integer> cheapestCount = null;
//		for (Map.Entry<Expression, Integer> entry : literalCounts.entrySet()) {
//			if (excludeLiterals.contains(entry.getKey())) {
//				continue;
//			}
//			if (cheapestCount == null) {
//				cheapestCount = entry;
//			}
//			else {
//				if (entry.getValue() < cheapestCount.getValue()) {
//					cheapestCount = entry;
//				}
//			}
//		}
//		
//		if (cheapestCount != null) {
//			result = cheapestCount.getKey();
//		}
//		else {
//			// i.e. already in the top level conjunct.
//			result = Expressions.TRUE;
//		}
		
		return result;
	}
	
	//
	// PRIVATE
	//
	protected  static List<Pair<Set<Expression>, List<Expression>>> findIndependentProblems(List<Expression> conjuncts, List<Expression> indexExpressions, RewritingProcess process) {
		// indices and corresponding conjuncts
		List<Pair<Set<Expression>, List<Expression>>> result = new ArrayList<Pair<Set<Expression>, List<Expression>>>();
		
		List<Expression> indices = IndexExpressions.getIndices(indexExpressions);
			
		// For efficiency work with a set of the indices
		Set<Expression>                   indicesSet                  = new LinkedHashSet<Expression>(indices);
		// Track conjuncts and their corresponding variables in the index
		Map<Expression, List<Expression>> fromConjunctToItsVariables  = new LinkedHashMap<Expression, List<Expression>>();
		// Track disjoint variable sets.
		DisjointSets<Expression>          disjointVariableSets        = new DisjointSets<Expression>();
		// Initialize the disjoint sets with the known indexes up front and track the set
		// of known variables seen so far (i.e. in the indices and free).
		Set<Expression>                   knownVariables              = new LinkedHashSet<Expression>(indices);
		
		for (Expression index : indices) {
			disjointVariableSets.makeSet(index);
		}
		
		collectConjunctToItsVariablesMapAndDisjointSetsOfVariables(
				conjuncts, fromConjunctToItsVariables, knownVariables, disjointVariableSets, process);
		
		if (disjointVariableSets.numberDisjointSets() > 1) {
			Map<Expression, Set<Expression>>       indexToDisjointSetMap = disjointVariableSets.getElementToDisjointSet();
			// Determine which conjuncts belong to which disjoint set of variables.
			Map<Set<Expression>, List<Expression>> indexSetsToConjunctsMap = new LinkedHashMap<Set<Expression>, List<Expression>>();
			for (Map.Entry<Expression, List<Expression>> conjunctVariables : fromConjunctToItsVariables.entrySet()) {
				
				Set<Expression> disjointIndexSet;
				if (conjunctVariables.getValue().size() == 0) {
					// Handle the case where there are no variables in the conjunct.
					// e.g. a = a, true, false, etc...
					disjointIndexSet = Collections.emptySet();
				} 
				else {
					// The conjunct has variables.
					disjointIndexSet = indexToDisjointSetMap.get(conjunctVariables.getValue().get(0));
				}
				List<Expression> conjunctsForThisDisjointSet = indexSetsToConjunctsMap.get(disjointIndexSet);
				if (conjunctsForThisDisjointSet == null) {
					conjunctsForThisDisjointSet = new ArrayList<Expression>();
					indexSetsToConjunctsMap.put(disjointIndexSet, conjunctsForThisDisjointSet);
				}
				conjunctsForThisDisjointSet.add(conjunctVariables.getKey());
			}
			
			List<Expression> conjunctsWithNoLinksToIndices = new ArrayList<Expression>();
			// Add the conjuncts with variables
			for (Map.Entry<Set<Expression>, List<Expression>> indexesToConjuncts : indexSetsToConjunctsMap.entrySet()) {
				// indices and corresponding conjuncts
				Set<Expression> indexes = new LinkedHashSet<Expression>(indexesToConjuncts.getKey());
				// Remove indexes explicitly associated with conjuncts from the available
				// set of disjoint indexes.
				for (Expression index : indexes) {
					indexToDisjointSetMap.remove(index);
				}
				// Only retain variables that were in the original indices
				indexes.retainAll(indicesSet);		
				
				if (indexes.size() == 0) {
					conjunctsWithNoLinksToIndices.addAll(indexesToConjuncts.getValue());	
				}
				else {
					result.add(new Pair<Set<Expression>, List<Expression>>(indexes, indexesToConjuncts.getValue()));
				}
			}
			if (conjunctsWithNoLinksToIndices.size() > 0) {
				Set<Expression> emptySet = Collections.emptySet();
				result.add(new Pair<Set<Expression>, List<Expression>>(emptySet, conjunctsWithNoLinksToIndices));
			}
			
			// Ensure any remaining disjoint sets are treated as independent on true, i.e. the index is
			// not referred to by any of the conjuncts so should be treated as | True |_X
			Set<Set<Expression>> remainingDisjointSets = new LinkedHashSet<Set<Expression>>(indexToDisjointSetMap.values());
			for (Set<Expression> disjointVariables : remainingDisjointSets) {
				result.add(new Pair<Set<Expression>, List<Expression>>(disjointVariables, new ArrayList<Expression>()));
			}
		}
			
		return result;
	}

	private static void collectConjunctToItsVariablesMapAndDisjointSetsOfVariables(List<Expression> conjuncts, Map<Expression, List<Expression>> fromConjunctToItsVariables, Set<Expression> knownVariables, DisjointSets<Expression> disjointVariableSets, RewritingProcess process) {
		for (Expression conjunct : conjuncts) {
			Set<Expression> variablesInConjunct = Expressions.freeVariables(conjunct, process);
			fromConjunctToItsVariables.put(conjunct, new ArrayList<Expression>(variablesInConjunct));
			Expression first = null;
			for (Expression variable : variablesInConjunct) {
				// Also set up disjoint sets for variables
				// not in the indices (i.e. free).
				if (!knownVariables.contains(variable)) {
					disjointVariableSets.makeSet(variable);
					knownVariables.add(variable);
				}
				
				// Now handle the unioning of variables that
				// occur in the same conjunct.
				if (first == null) {
					first = variable;
				}
				else {
					// Union the first variable with all of the
					// other variables (DisjointSets handles
					// transitive closure).
					disjointVariableSets.union(first, variable);
				}
			}
		}
	}

	private static Rewriter ifThenElseRemover;
	/**
	 * Removes if then elses from a formula.
	 * @param expression a formula
	 * @param process
	 * @return an equivalent formula without if then elses.
	 */
	public static Expression removeIfThenElsesFromFormula(Expression expression, RewritingProcess process) {
		if (CardinalityUtil.ifThenElseRemover == null) {
			CardinalityUtil.ifThenElseRemover = new TotalRewriter(FromConditionalFormulaToFormula.class.getName() + " Total Rewriter", new FromConditionalFormulaToFormula());
		}
		expression = CardinalityUtil.ifThenElseRemover.rewrite(expression, process);
		return expression;
	}
} 
