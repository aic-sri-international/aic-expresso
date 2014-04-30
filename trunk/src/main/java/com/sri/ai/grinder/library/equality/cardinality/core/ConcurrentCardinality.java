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
package com.sri.ai.grinder.library.equality.cardinality.core;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedHashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Set;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.grinder.api.Rewriter;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.core.AbstractHierarchicalRewriter;
import com.sri.ai.grinder.helper.GrinderUtil;
import com.sri.ai.grinder.library.Disequality;
import com.sri.ai.grinder.library.Equality;
import com.sri.ai.grinder.library.FunctorConstants;
import com.sri.ai.grinder.library.SemanticSubstitute;
import com.sri.ai.grinder.library.boole.And;
import com.sri.ai.grinder.library.boole.BooleanUtil;
import com.sri.ai.grinder.library.boole.ForAll;
import com.sri.ai.grinder.library.boole.Not;
import com.sri.ai.grinder.library.boole.Or;
import com.sri.ai.grinder.library.boole.ThereExists;
import com.sri.ai.grinder.library.controlflow.IfThenElse;
import com.sri.ai.grinder.library.equality.cardinality.direct.CardinalityRewriter;
import com.sri.ai.grinder.library.equality.cardinality.direct.core.CardinalityTypeOfLogicalVariable;
import com.sri.ai.grinder.library.indexexpression.IndexExpressions;
import com.sri.ai.grinder.library.number.Minus;
import com.sri.ai.grinder.library.number.Plus;
import com.sri.ai.grinder.library.number.Times;
import com.sri.ai.grinder.library.set.Sets;
import com.sri.ai.grinder.library.set.intensional.IntensionalSet;
import com.sri.ai.util.Util;

/**
 * An experimental class for performing cardinality computations.
 * 
 * @author saadati
 *
 */
@Beta
public class ConcurrentCardinality extends AbstractHierarchicalRewriter {
	private Rewriter rCardinalityExtensionalSet = new DefaultCardinalityExtensionalSet();
	private CardinalityTypeOfLogicalVariable cardLogical = new CardinalityTypeOfLogicalVariable();

	public static ConcurrentCardinality newCardinality() {
		return new ConcurrentCardinality();
	}
	
	@Override
	public Expression rewriteAfterBookkeeping(Expression expression, RewritingProcess process) {
		Expression result = null;
		if ( expression.hasFunctor(FunctorConstants.CARDINALITY) ) {
			Expression theSet = expression.get(0);
			if ( Sets.isIntensionalSet(theSet) ) {
				Expression             condition = IntensionalSet.getCondition(theSet);
				Collection<Expression> indexExpressions   = IntensionalSet.getIndexExpressions(theSet);
				RewritingProcess subProcess = GrinderUtil.extendContextualVariablesWithIntensionalSetIndices(theSet, process);
				HashSet<Expression> indexExpressionsSet = new LinkedHashSet<Expression>(indexExpressions);
				result = cardinalityCompute(condition, indexExpressionsSet, subProcess);
				result = process.rewrite(CardinalityRewriter.R_complete_normalize, result);
			} 
			else if ( Sets.isExtensionalSet(theSet) ) {
				result = rCardinalityExtensionalSet.rewrite(theSet, process);
			} 
			else {
				throw new IllegalArgumentException("Expression " + expression + " does not have the proper form");
			}
		} 
		else {
			throw new IllegalArgumentException("Expression " + expression + " does not have the proper form");
		}
		return result;

	}

	private Expression cardinalityCompute(Expression condition, Set<Expression> indexExpressions, RewritingProcess process) {
		Expression result = null;
		Set<Expression> freeVariables = Expressions.freeVariables(condition, process);
		Set<Expression> intersectionOfFreeAndIndices = new LinkedHashSet<Expression>(freeVariables);
		intersectionOfFreeAndIndices.retainAll(IndexExpressions.getIndices(new LinkedList<Expression>(indexExpressions))); // The intersection of the two sets
		if ( intersectionOfFreeAndIndices.isEmpty() ) {
			result = cardinalityIndependentCondition( condition, indexExpressions, process);
		} 
		else {
			if ( BooleanUtil.isEquality(condition) ) {
				if ( indexExpressions.size() == 0 ) {
					result = cardinalityIndependentCondition( Expressions.TRUE, indexExpressions, process);
				} 
				else if ( indexExpressions.size() == 1 ) {
					Expression simplified = process.rewrite(CardinalityRewriter.R_complete_normalize, condition);
					if ( Expressions.TRUE.equals(simplified) ) {
						result = cardinalityIndependentCondition(Expressions.TRUE, indexExpressions, process);
					} 
					else {
						result = Expressions.ONE;
					}
				} 
				else {
					Iterator<Expression> it = intersectionOfFreeAndIndices.iterator();
					Expression term1 = it.next();
					Util.removeElementsSatisfying(indexExpressions, new IndexExpressions.HasIndex(term1));
					result = cardinalityIndependentCondition(Expressions.TRUE, indexExpressions, process);
				}
			} 
			else if ( BooleanUtil.isNotEquality(condition) ) {
				Expression simplified = process.rewrite(CardinalityRewriter.R_complete_normalize, condition);
				if ( Expressions.FALSE.equals(simplified) ) {
					result = Expressions.ZERO;
				} 
				else {
					Expression max = cardinalityIndependentCondition( Expressions.TRUE, indexExpressions, process);
					Iterator<Expression> it = intersectionOfFreeAndIndices.iterator();
					Expression term1 = it.next();
					Util.removeElementsSatisfying(indexExpressions, new IndexExpressions.HasIndex(term1));
					Expression remainder = cardinalityIndependentCondition( Expressions.TRUE, indexExpressions, process);
					result = Minus.make(max, remainder);
				}
			} 
			else if ( BooleanUtil.isMultiEquality(condition) ) {
				condition = BooleanUtil.expandMultiEquality(condition, condition.get(0));
				result = cardinalityOfConjunction(condition, indexExpressions, process);
			} 
			else if ( BooleanUtil.isConjunction(condition) ) {
				result = cardinalityOfConjunction(condition, indexExpressions, process);
			} 
			else if ( BooleanUtil.isDisjunction(condition) ) {
				result = cardinalityOfDisjunction(condition, indexExpressions, process);
			} 
			else if ( BooleanUtil.isImplication(condition) ) {
				result = cardinalityOfImplication(condition, indexExpressions, process);
			} 
			else if ( BooleanUtil.isEquivalence(condition) ) {
				result = cardinalityOfEquivalence(condition, indexExpressions, process);
			} 
			else if ( BooleanUtil.isNegation(condition) ) {
				result = cardinalityOfNegation(condition, indexExpressions, process);
			} 
			else if ( ForAll.isForAll(condition) ) {
				Expression indexExpression = ForAll.getIndexExpression(condition);
				Expression body            = ForAll.getBody(condition);
				HashSet<Expression> newIndexExpression = new LinkedHashSet<Expression>();
				newIndexExpression.add(indexExpression);
				RewritingProcess subProcess = GrinderUtil.extendContextualVariablesWithIndexExpressions(Util.list(ForAll.getIndexExpression(condition)), process);
				Expression allCard = cardinalityCompute(body, newIndexExpression, subProcess);
				Expression newCondition = process.rewrite(CardinalityRewriter.R_complete_normalize, Equality.make(allCard, sizeof(indexExpression, process)));
				result = cardinalityCompute(newCondition, indexExpressions, process);				
			} 
			else if ( ThereExists.isThereExists(condition) ) {
				Expression indexExpression = ThereExists.getIndexExpression(condition);
				Expression body            = ThereExists.getBody(condition);
				HashSet<Expression> newIndexExpression = new LinkedHashSet<Expression>();
				newIndexExpression.add(indexExpression);
				RewritingProcess subProcess = GrinderUtil.extendContextualVariablesWithIndexExpressions(Util.list(ForAll.getIndexExpression(condition)), process);
				Expression existCard = cardinalityCompute(body, newIndexExpression, subProcess);
				Expression newCondition = process.rewrite(CardinalityRewriter.R_complete_normalize, Disequality.make(existCard, Expressions.ZERO));
				result = cardinalityCompute(newCondition, indexExpressions, process);
			} 
			else if ( IfThenElse.isIfThenElse(condition) ) {
				Expression conditionCondition = IfThenElse.getCondition(condition);
				Expression thenCondition = IfThenElse.getThenBranch(condition);
				Expression elseCondition = IfThenElse.getElseBranch(condition);
				Expression conditionAndThen = And.make(conditionCondition, thenCondition);
				Expression conditionAndElse = And.make(Not.make(conditionCondition), elseCondition);
				Expression thenCard = cardinalityCompute(conditionAndThen, duplicate(indexExpressions), process);
				Expression elseCard = cardinalityCompute(conditionAndElse, duplicate(indexExpressions), process);
				ArrayList<Expression> plusArguments = new ArrayList<Expression>();
				plusArguments.add(thenCard);
				plusArguments.add(elseCard);
				result = Plus.make(plusArguments);
			}
		}
		return result;
	}
	
	private Expression cardinalityOfConjunction(Expression condition, Set<Expression> indexExpressions, RewritingProcess process) {
		Expression result = null;

		List<Expression> conjuncts = condition.getArguments();
		if ( conjuncts.isEmpty() ) {
			return cardinalityIndependentCondition( Expressions.TRUE, indexExpressions, process);
		}
		
		List<Expression> independentConjuncts = new ArrayList<Expression>();
		List<Expression> dependentConjuncts = new ArrayList<Expression>();
		for (Expression arg: conjuncts) {
			Set<Expression> freeVariables = Expressions.freeVariables(arg, process);
			Set<Expression> intersectionOfFreeAndIndices = duplicate(IndexExpressions.getIndices(indexExpressions));
			intersectionOfFreeAndIndices.retainAll(freeVariables); // The intersection of the two sets	
			if ( intersectionOfFreeAndIndices.isEmpty() ) {
				if ( !independentConjuncts.contains(arg) ) {
					independentConjuncts.add(arg);
				}
			} 
			else {
				if ( !dependentConjuncts.contains(arg) ) {
					dependentConjuncts.add(arg);
				}
			}
		}
		
		Expression dependentCardinality = null;
		if ( dependentConjuncts.isEmpty() ) {
			dependentCardinality = cardinalityCompute(Expressions.TRUE, duplicate(indexExpressions), process);
		} 
		else if ( dependentConjuncts.size() == 1 ) {
			dependentCardinality = cardinalityCompute(dependentConjuncts.get(0), duplicate(indexExpressions), process);
		} 
		else {
			dependentCardinality = cardinalityOfConjunctionVariableInAllConjuncts(And.make(dependentConjuncts), duplicate(indexExpressions), process);
		}
		
		if ( independentConjuncts.isEmpty() ) {
			result = dependentCardinality;
		} 
		else {
			result = IfThenElse.make(And.make(independentConjuncts), dependentCardinality, Expressions.ZERO);
		}
		
		return result;
	}
	
	private Expression cardinalityOfConjunctionVariableInAllConjuncts(Expression condition, Set<Expression> indexExpressions, RewritingProcess process) {
		Expression result = null;
		List<Expression> conjuncts = new ArrayList<Expression>(condition.getArguments());
		int counter = 0, firstGoodEquality = -1, lastDisjunction = -1;
		
		for (Expression arg: conjuncts) {
			if ( BooleanUtil.isEquality(arg) ) {
				firstGoodEquality = counter;
				break;
			} 
			else if ( BooleanUtil.isNotEquality(arg) ) {
				
			} 
			else if ( BooleanUtil.isDisjunction(arg) ) {
				lastDisjunction = counter;
			}
			counter++;
		}
		if ( firstGoodEquality > -1 ) {
			Expression equality = conjuncts.get(firstGoodEquality);
			if ( equality.hasFunctor(Not.FUNCTOR) ) {
				equality = process.rewrite(CardinalityRewriter.R_move_not_in, equality);
			}
			List<Expression> terms = equality.getArguments();
			Expression replaceVar, replacedBy;
			if ( IndexExpressions.indexExpressionsContainIndex(indexExpressions, terms.get(0)) ) {
				replaceVar = terms.get(0);
				replacedBy = terms.get(1);
			} 
			else {
				replaceVar = terms.get(1);
				replacedBy = terms.get(0);				
			}
			conjuncts.remove(firstGoodEquality);
			Expression newConjunction = And.make(conjuncts);
			Expression newCondition = SemanticSubstitute.replace(newConjunction, replaceVar, replacedBy, process);
			Util.removeElementsSatisfying(indexExpressions, new IndexExpressions.HasIndex(replaceVar));
			result = cardinalityCompute(newCondition, indexExpressions, process);
		} 
		else if ( lastDisjunction > -1 ) {
			
			ArrayList<Expression> otherConjuncts = new ArrayList<Expression>();
			for (int index = 0; index < conjuncts.size(); index++) {
				if ( index != lastDisjunction ) {
					otherConjuncts.add(conjuncts.get(index));
				}
			}
			List<Expression> disjuncts = conjuncts.get(lastDisjunction).getArguments();
			ArrayList<Expression> newDisjunctsArgs = new ArrayList<Expression>();
			for (Expression disjunct: disjuncts) {
				otherConjuncts.add(0, disjunct);
				Expression newConjunct = Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees(And.FUNCTOR, otherConjuncts);
				newDisjunctsArgs.add(newConjunct);
				otherConjuncts.remove(0);
			}
			Expression newDisjunct = Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees(Or.FUNCTOR, newDisjunctsArgs);
			
			result = cardinalityCompute(newDisjunct, indexExpressions, process);
		} 
		else { // All disequalities
			boolean negateThemAll = true;
			if ( negateThemAll ) {
				Expression negation = process.rewrite(CardinalityRewriter.R_move_not_in, Not.make(condition));
				Expression negationCard = cardinalityCompute(negation, duplicate(indexExpressions), process);
				Expression max = cardinalityIndependentCondition( Expressions.TRUE, indexExpressions, process);
				result = Minus.make(max, negationCard);
			} 
			else { // negate the first one, and attach to the second one.
				Expression first = conjuncts.get(0);
				Expression nFirst = process.rewrite(CardinalityRewriter.R_move_not_in, Not.make(first));
				List<Expression> rest = conjuncts.subList(1, conjuncts.size());
				Expression c1 = And.make(rest);
				Expression c1Card = null;
				if ( BooleanUtil.isLiteral(c1) ) {
					c1Card = cardinalityCompute(c1, duplicate(indexExpressions), process);
				} 
				else {
					c1Card = cardinalityOfConjunctionVariableInAllConjuncts(c1, duplicate(indexExpressions), process);
				}
				rest.add(0, nFirst);
				Expression c2 = And.make(rest);
				Expression c2Card = cardinalityOfConjunctionVariableInAllConjuncts(c2, duplicate(indexExpressions), process);
				result = Minus.make(c1Card, c2Card);
			}
		}
			
		result = process.rewrite(CardinalityRewriter.R_complete_normalize, result);		
		return result;
	}
	
	private Expression cardinalityOfDisjunction(Expression condition, Set<Expression> indexExpressions, RewritingProcess process) { 
		Expression result = null;
		List<Expression> disjuncts = condition.getArguments();
		List<Expression> independentDisjuncts = new ArrayList<Expression>();
		List<Expression> dependentDisjuncts = new ArrayList<Expression>();
		
		for (Expression disjunct: disjuncts) {
			Set<Expression> freeVariables = Expressions.freeVariables(disjunct, process);
			Set<Expression> intersectionOfFreeAndIndices = duplicate(IndexExpressions.getIndices(indexExpressions));
			intersectionOfFreeAndIndices.retainAll(freeVariables); // The intersection of the two sets	
			
			if ( intersectionOfFreeAndIndices.isEmpty() ) {
				independentDisjuncts.add(disjunct);
			} 
			else {
				dependentDisjuncts.add(disjunct);
			}
		}
		Expression dependentCardinality = null;
		if ( dependentDisjuncts.isEmpty() ) {
			dependentCardinality = Expressions.ZERO;
		} 
		else if ( dependentDisjuncts.size()==1 ) {
			dependentCardinality = cardinalityCompute(dependentDisjuncts.get(0), indexExpressions, process);
		} 
		else {
			dependentCardinality = cardinalityOfDisjunctionVariableInAllDisjuncts(Or.make(dependentDisjuncts), indexExpressions, process);
		}
		if ( independentDisjuncts.isEmpty() ) {
			result = dependentCardinality;
		} 
		else {
			Expression whenTrueCardinality = cardinalityIndependentCondition( Expressions.TRUE, indexExpressions, process);
			result = IfThenElse.make(Or.make(independentDisjuncts), whenTrueCardinality, dependentCardinality);
		}		
		return result;
	}

	
	private Expression cardinalityOfDisjunctionVariableInAllDisjuncts(Expression condition, Set<Expression> indexExpressions, RewritingProcess process) {
		Expression result = null;
		List<Expression> disjuncts = condition.getArguments();
		boolean useNegation = false;
		for (Expression disjunct: disjuncts) {
			if ( BooleanUtil.isNotEquality(disjunct) ) {
				useNegation = true;
				break;
			}
		}
		if ( useNegation ) {
			Expression negation = process.rewrite(CardinalityRewriter.R_move_not_in, Not.make(condition));
			Expression trueCardinality = cardinalityIndependentCondition(Expressions.TRUE, indexExpressions, process);
			Expression negationCard = cardinalityOfConjunctionVariableInAllConjuncts(negation, indexExpressions, process);
			result = Minus.make(trueCardinality, negationCard);
		} 
		else {
			Expression first = disjuncts.get(0);
			Expression newOr = Or.make(disjuncts.subList(1, disjuncts.size()));
			Expression newAnd = null;
			if ( first.hasFunctor(Not.FUNCTOR) && newOr.hasFunctor(Not.FUNCTOR) ) {
				newAnd = And.make(process.rewrite(CardinalityRewriter.R_move_not_in, first), process.rewrite(CardinalityRewriter.R_move_not_in, newOr));
			} 
			else {
				newAnd = And.make(first, newOr);
				
			}
			newAnd = simplifyTop(newAnd, process);
			//System.out.println("newAnd is " + newAnd);
			Expression card1 = cardinalityCompute(first, duplicate(indexExpressions), process); 
			Expression card2 = cardinalityCompute(newOr, duplicate(indexExpressions), process); 
			Expression card3 = cardinalityCompute(newAnd, duplicate(indexExpressions), process); 
			Expression c1c2 = Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("+", card1, card2);
			result = Minus.make(c1c2, card3);
		}
		result = process.rewrite(CardinalityRewriter.R_complete_normalize, result);
		return result;
	}
	
	private Expression cardinalityOfNegation(Expression condition, Set<Expression> indexExpressions, RewritingProcess process) {
		Expression result = null;
		Expression innerFormula = condition.get(0);
		Expression max = cardinalityIndependentCondition(Expressions.TRUE, indexExpressions, process);
		Expression innerCard = cardinalityCompute(innerFormula, indexExpressions, process);
		result = process.rewrite(CardinalityRewriter.R_complete_normalize, Minus.make(max, innerCard));
		return result;
	}
	
	private Expression cardinalityOfImplication(Expression condition, Set<Expression> indexExpressions, RewritingProcess process) {
		Expression first  = Not.make(condition.get(0));
		Expression second = condition.get(1);
		Expression newOr = Or.make(first, second);
		
		Expression result = cardinalityOfDisjunction(newOr, indexExpressions, process);
		return result;
	}

	private Expression cardinalityOfEquivalence(Expression condition, Set<Expression> indexExpressions, RewritingProcess process) {
		Expression left  = condition.get(0);
		Expression right = condition.get(1);
		Expression first = And.make(left, right);
		//Expression second = And.make(moveNotIn.rewrite(Not.make(left), process), moveNotIn.rewrite(Not.make(right), process));
		//Expression second = And.make(Not.make(left), Not.make(right));
		Expression second = Not.make(Or.make(left, right));
		
		Expression firstCard  = cardinalityOfConjunction(first, duplicate(indexExpressions), process);
		Expression secondCard = cardinalityOfNegation(second, duplicate(indexExpressions), process);
		
		
		Expression result = process.rewrite(CardinalityRewriter.R_complete_normalize, Expressions.apply("+", firstCard, secondCard));
		return result;
	}
	
	private Expression cardinalityIndependentCondition(Expression condition, Set<Expression> indexExpressions, RewritingProcess process) {
		Expression result = null;
		int size = indexExpressions.size();
		if ( size == 0 ) {
			result = Expressions.ONE;
		} 
		else if ( size == 1 ) {
			Expression indexExpression = indexExpressions.iterator().next();
			result = sizeof(indexExpression, process);
		} 
		else {
			ArrayList<Expression> elements = new ArrayList<Expression>();
			for (Expression indexExpression: indexExpressions) {
				elements.add(sizeof(indexExpression, process));
			}
			result = Times.make(elements);
		}
		
		//
		if ( condition.equals(Expressions.TRUE) ) {
			// TODO ?
		} 
		else if ( condition.equals(Expressions.FALSE) ) {
			result = Expressions.ZERO;
		} 
		else {
			result = IfThenElse.make(condition, result, Expressions.ZERO);
		}
		
		return result;
	}
	
	public Expression sizeof(Expression indexExpression, RewritingProcess process) {
		Expression domain;
		if (indexExpression.hasFunctor(FunctorConstants.IN)) {
			domain = indexExpression.get(1);
		}
		else {
			domain = Expressions.apply("type", indexExpression);
		}
		Expression card = Expressions.apply(FunctorConstants.CARDINALITY, domain);
		Expression result = cardLogical.rewrite(card, process);
		return result;
	}

	public String getName() {
		return "R_card_concurrent";
	}
	
	private Set<Expression> duplicate(Set<Expression> indexExpressions) {
		return new LinkedHashSet<Expression>(indexExpressions);
	}
	
	private Expression simplifyTop(Expression expression, RewritingProcess process) {
		Expression result = expression;
		if ( result.hasFunctor(And.FUNCTOR) || result.hasFunctor(Or.FUNCTOR) ) {
			Expression functor = result.getFunctor();
			Set<Expression> arguments = new LinkedHashSet<Expression>();
			for (Expression arg: expression.getArguments()) {
				Expression simplified = simplifyTop(arg, process);
				if ( simplified.hasFunctor(functor) ) {
					arguments.addAll(simplified.getArguments());
				} 
				else {
					arguments.add(simplified);
				}
			}
			result = Expressions.apply(functor, arguments.toArray());
		}
		
		return result;
	}
}
