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
package com.sri.ai.grinder.plaindpll.util;

import static com.sri.ai.expresso.helper.Expressions.FALSE;
import static com.sri.ai.expresso.helper.Expressions.TRUE;
import static com.sri.ai.expresso.helper.Expressions.freeVariablesAndTypes;
import static com.sri.ai.expresso.helper.Expressions.makeSymbol;
import static com.sri.ai.expresso.helper.Expressions.parse;
import static com.sri.ai.grinder.library.indexexpression.IndexExpressions.getIndexExpressionsFromSymbolsAndTypes;
import static com.sri.ai.util.Util.arrayList;
import static com.sri.ai.util.Util.filter;
import static com.sri.ai.util.Util.list;
import static com.sri.ai.util.Util.myAssert;
import static java.lang.Integer.parseInt;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

import com.google.common.annotations.Beta;
import com.google.common.base.Predicate;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.Type;
import com.sri.ai.expresso.core.DefaultUniversallyQuantifiedFormula;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.expresso.type.Categorical;
import com.sri.ai.grinder.api.Rewriter;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.core.DefaultRewritingProcess;
import com.sri.ai.grinder.core.PrologConstantPredicate;
import com.sri.ai.grinder.helper.GrinderUtil;
import com.sri.ai.grinder.library.boole.Implication;
import com.sri.ai.grinder.library.boole.Not;
import com.sri.ai.grinder.library.controlflow.IfThenElse;
import com.sri.ai.grinder.plaindpll.api.Constraint1;
import com.sri.ai.grinder.plaindpll.api.ConstraintTheory;
import com.sri.ai.grinder.plaindpll.core.PlainSGDPLLT;
import com.sri.ai.grinder.plaindpll.problemtype.Validity;
import com.sri.ai.grinder.plaindpll.theory.DefaultInputTheory;
import com.sri.ai.grinder.plaindpll.theory.EqualityConstraintTheory;
import com.sri.ai.grinder.plaindpll.theory.term.SymbolTermTheory;
import com.sri.ai.util.Util;

/**
 * Implements utility methods to be used by {@link PlainSGDPLLT} and associated classes.
 * <p>
 * Several of these methods could be more naturally seen as methods of the interfaces themselves
 * (for example, {@link DPLLUtil#getIndexBoundBySplitterApplicationIfAny(Expression splitter, Collection<Expression> indices, ConstraintTheory constraintTheoryWithEquality)}
 * could be a method in interface {@link ConstraintTheory}),
 * but are included here instead because their functionality depends on a more basic method of those interfaces
 * (in that case, on {@link ConstraintTheory#makeConstraint()});
 * including them there would place the burden on users to make sure the implementations of these multiple methods are mutually consistent.
 * Default interface methods would not be a good solution either because the burden would still be there for the user not to override
 * the default method, or if they did, to make sure they are consistent (there are no "final default" interface methods).
 * By placing these methods here, users implementing an interface can more easily simply implement each method as a primitive
 * consistent with its pre- and post-conditions only, and have those primitive methods be automatically used by the convenience ones here.
 *   
 * @author braz
 *
 */
@Beta
public class DPLLUtil {

	/**
	 * A method provided for use with {@link Rewriter} code using contextual constraints.
	 * Will probably be discarded at some point.
	 * @param solution
	 * @param constraint
	 * @param process
	 * @return
	 */
	public static Expression simplifySolutionUnderConstraint(Expression solution, Expression constraint, RewritingProcess process) {
		Expression result = null;
		
		if (constraint.equals(Expressions.TRUE)) {
			result = solution;
		}
		else {
			result = simplifySolutionUnderNonTrivialConstraint(solution, constraint, process);
		}
	
		return result;
	}

	/**
	 * A method provided for use with {@link Rewriter} code using contextual constraints.
	 * Will probably be discarded at some point.
	 * @param solution
	 * @param constraint
	 * @param process
	 * @return
	 */
	public static Expression simplifySolutionUnderNonTrivialConstraint(Expression solution, Expression constraint, RewritingProcess process) {
		Expression result = null;
		
		if (IfThenElse.isIfThenElse(solution)) {
			Expression newCondition = impliesExpressionOrItsNegationOrNeither(IfThenElse.condition(solution), constraint, process);
			if (newCondition.equals(Expressions.TRUE)) {
				result = simplifySolutionUnderNonTrivialConstraint(IfThenElse.thenBranch(solution), constraint, process);
			}
			else if (newCondition.equals(Expressions.FALSE)) {
				result = simplifySolutionUnderNonTrivialConstraint(IfThenElse.elseBranch(solution), constraint, process);
			}
			else {
				Expression newThenBranch = simplifySolutionUnderNonTrivialConstraint(IfThenElse.thenBranch(solution), constraint, process);
				Expression newElseBranch = simplifySolutionUnderNonTrivialConstraint(IfThenElse.elseBranch(solution), constraint, process);
				result = IfThenElse.makeIfDistinctFrom(solution, newCondition, newThenBranch, newElseBranch, false);
			}
		}
		else {
			result = solution;
		}
		return result;
	}

	/**
	 * Returns 'true' if expression is tautologically implied by constraint,
	 * 'false' if its negation is tautologically implied by constraint,
	 * and expression itself otherwise.
	 * @param expression
	 * @param constraint
	 * @param process
	 * @return
	 */
	public static Expression impliesExpressionOrItsNegationOrNeither(Expression expression, Expression constraint, RewritingProcess process) {
		Expression result = null;
	
		Expression constraintImpliesExpression = Implication.make(constraint, expression);
		List<Expression> freeVariablesIndexExpressions = getIndexExpressionsFromSymbolsAndTypes(freeVariablesAndTypes(constraintImpliesExpression, process)).getList();
	
		Expression closedConstraintImpliedExpression = new DefaultUniversallyQuantifiedFormula(freeVariablesIndexExpressions, constraintImpliesExpression);
		Expression alwaysImpliesExpression = (new PlainSGDPLLT(new DefaultInputTheory(new EqualityConstraintTheory(new SymbolTermTheory())), new Validity(), null)).rewrite(closedConstraintImpliedExpression, process);
		if (alwaysImpliesExpression.equals(Expressions.TRUE)) {
			result = Expressions.TRUE;
		}
		else {
			Expression constraintImpliesNegationOfExpression = Implication.make(constraint, Not.make(expression));
			Expression closedConstraintImpliesNegationOfExpression = new DefaultUniversallyQuantifiedFormula(freeVariablesIndexExpressions, constraintImpliesNegationOfExpression);
			Expression alwaysImpliesNegationOfExpression = (new PlainSGDPLLT(new DefaultInputTheory(new EqualityConstraintTheory(new SymbolTermTheory())), new Validity(), null)).rewrite(closedConstraintImpliesNegationOfExpression, process);
			if (alwaysImpliesNegationOfExpression.equals(Expressions.TRUE)) {
				result = Expressions.FALSE;
			}
			else {
				result = expression;
			}
		}
	
		return result;
	}

	/**
	 * @param solution
	 * @param process
	 * @return
	 */
	public static boolean isConditionalSolution(Expression solution, ConstraintTheory theory, RewritingProcess process) {
		boolean result = IfThenElse.isIfThenElse(solution) && isSplitter(IfThenElse.condition(solution), theory, process);
		return result;
	}

	public static boolean isSplitter(Expression literal, ConstraintTheory theory, RewritingProcess process) {
		boolean result = theory.makeSplitterIfPossible(literal, Util.list(), process) != null;
		return result;
	}

	/**
	 * Applies a constraint equivalent to given signed splitter using
	 * {@link ConstraintTheory#applyConstraintToSolution(com.sri.ai.grinder.plaindpll.api.ConstraintTheory.Constraint1, Expression, RewritingProcess)}.
	 * @param splitterSign
	 * @param splitter
	 * @param solution
	 * @param constraintTheoryWithEquality
	 * @param process
	 * @return an equivalent solution
	 */
	public static Expression applySplitterToSolution(boolean splitterSign, Expression splitter, Expression solution, ConstraintTheory theory, RewritingProcess process) {
		Constraint1 constraint = theory.makeConstraint(Collections.emptyList()); // no indices in solutions
		constraint = constraint.incorporate(splitterSign, splitter, process);
		Expression result = theory.applyConstraintToSolution(constraint, solution, process);
		return result;
	}

	
	public static RewritingProcess makeProcess(ConstraintTheory theory, Map<String, String> mapFromSymbolNameToTypeName, Map<String, String> mapFromCategoricalTypeNameToSizeString, Collection<Type> additionalTypes) {
		return makeProcess(theory, mapFromSymbolNameToTypeName, mapFromCategoricalTypeNameToSizeString, additionalTypes, new PrologConstantPredicate());
	}

	public static RewritingProcess makeProcess(ConstraintTheory constraintTheory, Map<String, String> mapFromSymbolNameToTypeName, Map<String, String> mapFromCategoricalTypeNameToSizeString, Collection<Type> additionalTypes, Predicate<Expression> isUniquelyNamedConstantPredicate) {
		Constraint1 trueConstraintOnNoIndices = constraintTheory.makeConstraint(list());
		return makeProcess(trueConstraintOnNoIndices, mapFromSymbolNameToTypeName, mapFromCategoricalTypeNameToSizeString, additionalTypes, isUniquelyNamedConstantPredicate);
	}

	public static RewritingProcess makeProcess(Constraint1 trueConstraintOnNoIndices, Map<String, String> mapFromSymbolNameToTypeName, Map<String, String> mapFromCategoricalTypeNameToSizeString, Collection<Type> additionalTypes, Predicate<Expression> isUniquelyNamedConstantPredicate) {
		RewritingProcess result = extendProcessWith(mapFromSymbolNameToTypeName, list(), mapFromCategoricalTypeNameToSizeString, isUniquelyNamedConstantPredicate, new DefaultRewritingProcess(null));			
		result.setIsUniquelyNamedConstantPredicate(isUniquelyNamedConstantPredicate);
		result.initializeDPLLContextualConstraint(trueConstraintOnNoIndices);
		return result;
	}

	public static RewritingProcess makeProcess(Map<String, String> mapFromSymbolNameToTypeName, Map<String, String> mapFromCategoricalTypeNameToSizeString, Collection<Type> additionalTypes, Predicate<Expression> isUniquelyNamedConstantPredicate) {
		RewritingProcess result = extendProcessWith(mapFromSymbolNameToTypeName, additionalTypes, mapFromCategoricalTypeNameToSizeString, isUniquelyNamedConstantPredicate, new DefaultRewritingProcess(null));			
		result.setIsUniquelyNamedConstantPredicate(isUniquelyNamedConstantPredicate);
		return result;
	}

	/**
	 * @param mapFromSymbolNameToTypeName
	 * @param additionalTypes TODO
	 * @param mapFromCategoricalTypeNameToSizeString
	 * @param process
	 * @return
	 */
	public static RewritingProcess extendProcessWith(Map<String, String> mapFromSymbolNameToTypeName, Collection<Type> additionalTypes, Map<String, String> mapFromCategoricalTypeNameToSizeString, Predicate<Expression> isUniquelyNamedConstantPredicate, RewritingProcess process) {
		Collection<Type> allTypes =
				getCategoricalTypes(
						mapFromSymbolNameToTypeName,
						mapFromCategoricalTypeNameToSizeString,
						isUniquelyNamedConstantPredicate,
						process);
		allTypes.addAll(additionalTypes);
		
		return extendProcessWith(mapFromSymbolNameToTypeName, allTypes, process);
	}

	/**
	 * @param mapFromSymbolNameToTypeName
	 * @param types
	 * @param process
	 * @return
	 */
	public static RewritingProcess extendProcessWith(Map<String, String> mapFromSymbolNameToTypeName, Collection<? extends Type> types, RewritingProcess process) {
		List<Expression> symbolDeclarations = new ArrayList<>();
		for (Map.Entry<String, String> variableNameAndTypeName : mapFromSymbolNameToTypeName.entrySet()) {
			String symbolName = variableNameAndTypeName.getKey();
			String typeName   = variableNameAndTypeName.getValue();
			
			symbolDeclarations.add(parse(symbolName + " in " + typeName));
		}
		process = GrinderUtil.extendContextualSymbolsWithIndexExpressions(symbolDeclarations, process);
					
		for (Type type : types) {
			process = process.put(type);
			process.putGlobalObject(parse("|" + type.getName() + "|"), type.cardinality());
		}
		
		return process;
	}

	/**
	 * @param mapFromSymbolNameToTypeName
	 * @param mapFromCategoricalTypeNameToSizeString
	 * @param isUniquelyNamedConstantPredicate
	 * @param process
	 * @return
	 */
	public static Collection<Type> getCategoricalTypes(
			Map<String, String> mapFromSymbolNameToTypeName,
			Map<String, String> mapFromCategoricalTypeNameToSizeString,
			Predicate<Expression> isUniquelyNamedConstantPredicate,
			RewritingProcess process) {
		
		Collection<Type> categoricalTypes = new LinkedList<Type>();
		for (Map.Entry<String, String> typeNameAndSizeString : mapFromCategoricalTypeNameToSizeString.entrySet()) {
			String typeExpressionString = typeNameAndSizeString.getKey();
			String sizeString = typeNameAndSizeString.getValue();
			
			// check if already present and, if not, make it
			Categorical type = (Categorical) process.getType(typeExpressionString);
			if (type == null) {
				if (typeExpressionString.equals("Boolean")) {
					type = new Categorical("Boolean", 2, arrayList(TRUE, FALSE));
				}
				else {
					ArrayList<Expression> knownConstants = 
							getKnownUniquelyNamedConstaintsOf(
									typeExpressionString,
									mapFromSymbolNameToTypeName,
									isUniquelyNamedConstantPredicate,
									process);
					type = 
							new Categorical(
									typeExpressionString,
									parseInt(sizeString),
									knownConstants);
				}
			}
			categoricalTypes.add(type);
		}
		return categoricalTypes;
	}

	/**
	 * @param typeName
	 * @param mapFromSymbolNameToTypeName
	 * @param process
	 * @return
	 */
	public static ArrayList<Expression> getKnownUniquelyNamedConstaintsOf(String typeName, Map<String, String> mapFromSymbolNameToTypeName, Predicate<Expression> isUniquelyNamedConstantPredicate, RewritingProcess process) {
		ArrayList<Expression> knownConstants = new ArrayList<Expression>();
		for (Map.Entry<String, String> symbolNameAndTypeName : mapFromSymbolNameToTypeName.entrySet()) {
			if (symbolNameAndTypeName.getValue().equals(typeName)) {
				Expression symbol = makeSymbol(symbolNameAndTypeName.getKey());
				if (isUniquelyNamedConstantPredicate.apply(symbol)) {
					knownConstants.add(symbol);
				}
			}
		}
		return knownConstants;
	}

	/**
	 * Given a collection of splitters, returns a collection with those not yet satisfied by process's DPLL contextual constraint.
	 * @param splitters
	 * @param process
	 * @return
	 */
	public static Collection<Expression> keepSplittersUnsatisfiedByContextualConstraint(Collection<Expression> splitters, RewritingProcess process) {
		Predicate<Expression> keepUnsatisfiedSplitters = s -> splitterIsNotSatisfiedFromContextualConstraintAlready(true,  s, process);
		Collection<Expression> undeterminedSplittersThatNeedToBeTrue = filter(splitters, keepUnsatisfiedSplitters);
		return undeterminedSplittersThatNeedToBeTrue;
	}

	/**
	 * Given a collection of splitters, returns a collection with those <i>the negations of which</i>
	 * are not yet satisfied by process's DPLL contextual constraint.
	 * @param splitters
	 * @param process
	 * @return
	 */
	public static Collection<Expression> keepSplitterTheNegationsOfWhichAreUnsatisfiedByContextualConstraint(Collection<Expression> splitters, RewritingProcess process) {
		Predicate<Expression> keepUnsatisfiedSplitterNegations = s -> splitterIsNotSatisfiedFromContextualConstraintAlready(false, s, process);
		Collection<Expression> undeterminedSplittersThatNeedToBeFalse = filter(splitters, keepUnsatisfiedSplitterNegations);
		return undeterminedSplittersThatNeedToBeFalse;
	}

	/**
	 * Indicates whether a splitter does not hold according to process's DPLL contextual constraint.
	 * @param splitterSign
	 * @param splitter
	 * @param process
	 * @return
	 */
	public static boolean splitterIsNotSatisfiedFromContextualConstraintAlready(boolean splitterSign, Expression splitter, RewritingProcess process) {
		boolean result;
		Expression splitterNormalizedByContextualConstraint = process.getDPLLContextualConstraint().normalizeSplitterGivenConstraint(splitter, process);
		myAssert(() -> ! splitterNormalizedByContextualConstraint.equals( ! splitterSign), () -> "required splitter must be satisfiable under contextual constraint");
		result = ! splitterNormalizedByContextualConstraint.equals(splitterSign); // if splitter is implied TRUE by contextual constraint, it is superfluous
		return result;
	}
}