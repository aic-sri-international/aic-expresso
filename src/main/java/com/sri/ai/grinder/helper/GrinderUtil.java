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
package com.sri.ai.grinder.helper;

import static com.sri.ai.expresso.helper.Expressions.FALSE;
import static com.sri.ai.expresso.helper.Expressions.INFINITY;
import static com.sri.ai.expresso.helper.Expressions.MINUS_INFINITY;
import static com.sri.ai.expresso.helper.Expressions.TRUE;
import static com.sri.ai.expresso.helper.Expressions.apply;
import static com.sri.ai.expresso.helper.Expressions.makeSymbol;
import static com.sri.ai.expresso.helper.Expressions.parse;
import static com.sri.ai.grinder.library.FunctorConstants.CARDINALITY;
import static com.sri.ai.grinder.library.FunctorConstants.CARTESIAN_PRODUCT;
import static com.sri.ai.grinder.library.FunctorConstants.DIVISION;
import static com.sri.ai.grinder.library.FunctorConstants.EXPONENTIATION;
import static com.sri.ai.grinder.library.FunctorConstants.GREATER_THAN;
import static com.sri.ai.grinder.library.FunctorConstants.GREATER_THAN_OR_EQUAL_TO;
import static com.sri.ai.grinder.library.FunctorConstants.INTEGER_INTERVAL;
import static com.sri.ai.grinder.library.FunctorConstants.LESS_THAN;
import static com.sri.ai.grinder.library.FunctorConstants.LESS_THAN_OR_EQUAL_TO;
import static com.sri.ai.grinder.library.FunctorConstants.MAX;
import static com.sri.ai.grinder.library.FunctorConstants.MINUS;
import static com.sri.ai.grinder.library.FunctorConstants.PLUS;
import static com.sri.ai.grinder.library.FunctorConstants.PRODUCT;
import static com.sri.ai.grinder.library.FunctorConstants.SUM;
import static com.sri.ai.grinder.library.FunctorConstants.TIMES;
import static com.sri.ai.util.Util.arrayList;
import static com.sri.ai.util.Util.getFirstOrNull;
import static com.sri.ai.util.Util.getFirstSatisfyingPredicateOrNull;
import static com.sri.ai.util.Util.list;

import java.util.ArrayList;
import java.util.Collection;
import java.util.LinkedHashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Set;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.IndexExpressionsSet;
import com.sri.ai.expresso.api.IntensionalSet;
import com.sri.ai.expresso.api.QuantifiedExpressionWithABody;
import com.sri.ai.expresso.api.Type;
import com.sri.ai.expresso.core.AbstractExtensionalSet;
import com.sri.ai.expresso.core.DefaultUniversallyQuantifiedFormula;
import com.sri.ai.expresso.core.ExtensionalIndexExpressionsSet;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.expresso.type.Categorical;
import com.sri.ai.expresso.type.IntegerExpressoType;
import com.sri.ai.expresso.type.IntegerInterval;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.library.Disequality;
import com.sri.ai.grinder.library.Equality;
import com.sri.ai.grinder.library.FormulaUtil;
import com.sri.ai.grinder.library.FunctorConstants;
import com.sri.ai.grinder.library.boole.And;
import com.sri.ai.grinder.library.controlflow.IfThenElse;
import com.sri.ai.grinder.library.indexexpression.IndexExpressions;
import com.sri.ai.grinder.library.number.GreaterThan;
import com.sri.ai.grinder.library.number.LessThan;
import com.sri.ai.grinder.library.set.tuple.Tuple;
import com.sri.ai.util.Util;
import com.sri.ai.util.base.NotContainedBy;
import com.sri.ai.util.base.Pair;
import com.sri.ai.util.collect.StackedHashMap;
import com.sri.ai.util.math.Rational;

/**
 * General purpose utility routines related to grinder libraries.
 * 
 * @author oreilly
 */
@Beta
public class GrinderUtil {

	/**
	 * The key of a global object of rewriting processes that prevents the check again free variables in additional constraints
	 * (that is, that are not in the contextual symbols).
	 * This is used for when global free variables are being determined. It should not be used in normal circumstances.
	 */
	public static final String DO_NOT_REQUIRE_ADDED_CONTEXTUAL_CONSTRAINT_FREE_SYMBOLS_TO_BE_IN_CONTEXTUAL_VARIABLES = "Do not require added contextual constraint free variables to be in contextual symbols";

	/**
	 * Takes an expression and, if it is an if then else, rearranges it so that
	 * conditions on logical variables are separated from other tests and on top
	 * if then else's. This assumes that only logical variables are arguments to
	 * equalities and disequalities, an assumption that will have to be reviewed
	 * later.
	 * 
	 * @param expressions
	 *            the expression to be tested.
	 * @param process
	 *            the process in which the rewriting is occurring.
	 * @return a rewritten expression if expression was a conditional on logical
	 *         variables that needed to be separated out, otherwise expression
	 *         unchanged is returned.
	 *         More specifically, if the input expression is of the form
	 *         if LV and Rest then Alpha else Beta,
	 *         where "LV and Rest" is a possible decomposition of the condition into
	 *         a logical variable equalities formula and a remainder,
	 *         it returns
	 *         if LV then if Rest then Alpha else Beta else Beta  
	 */
	public static Expression makeSureConditionsOnLogicalVariablesAreSeparatedAndOnTop(
			Expression expression, RewritingProcess process) {
		if (IfThenElse.isIfThenElse(expression)) {
			Expression condition = IfThenElse.condition(expression);
			Pair<Expression, Expression> constraintsAndRest = Expressions
					.separateEqualityFormulasOnAtomicSymbolsFromRest(condition, process);
			// If either of these are the expression "true" then I don't need to make a change.
			if (!Expressions.TRUE.equals(constraintsAndRest.first)
					&& !Expressions.TRUE.equals(constraintsAndRest.second)) {
				Expression thenBranch = makeSureConditionsOnLogicalVariablesAreSeparatedAndOnTop(
						IfThenElse.thenBranch(expression), process);
				Expression elseBranch = makeSureConditionsOnLogicalVariablesAreSeparatedAndOnTop(
						IfThenElse.elseBranch(expression), process);
				Expression result = IfThenElse.make(constraintsAndRest.first,
						IfThenElse.make(constraintsAndRest.second, thenBranch,
								elseBranch), elseBranch);
				return result;
			}
		}
		return expression;
	}
	
	/**
	 * Returns a rewriting process with contextual symbols extended by a list of index expressions.
	 */
	public static RewritingProcess extendContextualSymbolsWithIndexExpressions(IndexExpressionsSet indexExpressions, RewritingProcess process) {
		Map<Expression, Expression> indexToTypeMap = IndexExpressions.getIndexToTypeMapWithDefaultNull(indexExpressions);
		RewritingProcess result = GrinderUtil.extendContextualSymbolsAndConstraint(indexToTypeMap, Expressions.TRUE, process);
		return result;
	}

	/**
	 * Returns a rewriting process with contextual symbols extended by a list of index expressions.
	 */
	public static RewritingProcess extendContextualSymbolsWithIndexExpressions(List<Expression> indexExpressions, RewritingProcess process) {
		RewritingProcess result = 
				extendContextualSymbolsWithIndexExpressions(
						new ExtensionalIndexExpressionsSet(indexExpressions),
						process);
		return result;
	}

	/**
	 * Extend the rewriting processes's contextual constraint by an additional
	 * constraint.
	 * 
	 * @param additionalConstraints
	 *            additional context to extend the contextual constraint by.
	 * @param process
	 *            the process in which the rewriting is occurring and whose
	 *            contextual constraint is to be updated.
	 * @return a sub-rewriting process constrained by
	 *         'process.getContexualConstraint() and additionalConstraints'.
	 */
	public static RewritingProcess extendContextualConstraint(Expression additionalConstraints, RewritingProcess process) {
		
		return extendContextualSymbolsAndConstraint(
				new LinkedHashMap<Expression, Expression>(),
				additionalConstraints,
				process);
	}
	
	/**
	 * Extends a process's contextual symbols with free variables found in a given expression and returns the new resulting process.
	 * This method should be used only as a setup method; during ordinary processing, all free variables should already be in the context.
	 * IMPORTANT: if a problem is defined by a few separate expressions that may share a free variable
	 * (typical case is a unit test with an input expression, a contextual constraint and perhaps an expected result)
	 * then one must NOT extend the context with the free variables from these separate expressions one expression at a time,
	 * because this will consider the occurrences of that shared free variable as of distinct variables,
	 * and each extension will shadow the previous ones.
	 * Instead, one must create a tuple of expressions and extend the context with them all at the same time.
	 */
	public static RewritingProcess extendContextualSymbolsWithFreeSymbolsInExpressionwithUnknownTypeForSetUpPurposesOnly(Expression expression, RewritingProcess process) {
		Set<Expression> freeSymbols = Expressions.freeSymbols(expression, process);
		Map<Expression, Expression> fromSymbolToType = new LinkedHashMap<Expression, Expression>();
		for (Expression symbol : freeSymbols) {
			fromSymbolToType.put(symbol, null);
		}
		RewritingProcess result = extendContextualSymbols(fromSymbolToType, process);
		return result;
	}

	/**
	 * Same as {@link #extendContextualSymbolsAndConstraint(Map<Expression, Expression>, Expression, RewritingProcess)},
	 * assuming a true constraint.
	 */
	public static RewritingProcess extendContextualSymbols(Map<Expression, Expression> freeSymbolsAndTypes, RewritingProcess process) {
		RewritingProcess result = extendContextualSymbolsAndConstraint(freeSymbolsAndTypes, Expressions.TRUE, process);
		return result;
	}

	/**
	 * Gets a map from indices to their types and returns a map from their functors-or-symbols
	 * (that is, their functors if they are function application, and themselves if they are symbols)
	 * to their types.
	 * A function has a type of the form <code>'->'('x'(T1, ..., Tn), R)</code>, where <code>T1,...,Tn</code>
	 * are the types of their arguments and <code>R</code> are their co-domain.
	 */
	public static Map<Expression, Expression> getTypesOfIndicesFunctorsOrSymbols(Map<Expression, Expression> fromIndicesToType, RewritingProcess process) {
		Map<Expression, Expression> result = new LinkedHashMap<Expression, Expression>();
		for (Map.Entry<Expression, Expression> entry : fromIndicesToType.entrySet()) {
			Expression index     = entry.getKey();
			Expression indexType = entry.getValue();
			if (index.getSyntacticFormType().equals("Symbol")) {
				result.put(index, indexType);
			}
			else if (index.getSyntacticFormType().equals("Function application")) {
				Expression typeOfFunctor = getTypeOfFunctor(index, indexType, process);
				result.put(index.getFunctorOrSymbol(), typeOfFunctor);
			}
			else {
				throw new Error("getTypesOfIndicesFunctorsOrSymbols not supported for expressions other than symbols and function applications, but invoked on " + index);
			}
		}
		return result;
	}
	
	/**
	 * Gets a function application and its type <code>T</code>, and returns the inferred type of its functor,
	 * which is <code>'->'('x'(T1, ..., Tn), T)</code>, where <code>T1,...,Tn</code> are the types.
	 */
	public static Expression getTypeOfFunctor(Expression functionApplication, Expression functionApplicationType, RewritingProcess process) {
		Expression result;
		if (functionApplication.getSyntacticFormType().equals("Function application")) {
			List<Expression> argumentTypes = Util.mapIntoArrayList(functionApplication.getArguments(), new GetType(process));
			if (argumentTypes.contains(null)) {
				result = null; // unknown type
			}
			else {
				Expression argumentTypesTuple = Tuple.make(argumentTypes);
				result = Expressions.apply("->", argumentTypesTuple, functionApplication);
			}
		}
		else {
			throw new Error("getTypeOfFunctor applicable to function applications only, but invoked on " + functionApplication);
		}
		return result;
	}

	/**
	 * Returns the type of given expression according to process.
	 */
	public static Expression getType(Expression expression, RewritingProcess process) {
		Expression result;
		
		// TODO: this method is horribly hard-coded to a specific language; need to clean this up
		
		if (FormulaUtil.isApplicationOfBooleanConnective(expression)) {
			result = makeSymbol("Boolean");
		}
		else if (expression.getSyntacticFormType().equals("Function application") &&
				list(SUM, PRODUCT, MAX).contains(expression.getFunctor().toString())) {
			Expression argument = expression.get(0);
			if (argument.getSyntacticFormType().equals("Intensional set")) {
				Expression head = ((IntensionalSet)argument).getHead();
				result = getType(head, process);
			}
			else if (argument.getSyntacticFormType().equals("Extensional set")) {
				List<Expression> arguments = ((AbstractExtensionalSet)argument).getElementsDefinitions();
				result = getTypeOfCollectionOfNumericExpressionsWithDefaultInteger(arguments, process);
			}
			else if (expression.hasFunctor(MAX)) { // MAX can also be applied to a bunch of numbers
				result = getTypeOfCollectionOfNumericExpressionsWithDefaultInteger(expression.getArguments(), process);
			}
			else {
				throw new Error(expression.getFunctor() + " defined for sets only but got " + expression.get(0));
			}
		}
		else if (Equality.isEquality(expression) || Disequality.isDisequality(expression)) {
			result = makeSymbol("Boolean");
		}
		else if (IfThenElse.isIfThenElse(expression)) {
			Expression thenType = getType(IfThenElse.thenBranch(expression), process);
			Expression elseType = getType(IfThenElse.elseBranch(expression), process);
			if (thenType != null && elseType != null && (thenType.equals("Number") && isIntegerOrReal(elseType) || isIntegerOrReal(thenType) && elseType.equals("Number"))) {
				result = makeSymbol("Number");
			}
			else if (thenType != null && elseType != null && (thenType.equals("Integer") && elseType.equals("Real") || thenType.equals("Real") && elseType.equals("Integer"))) {
				result = makeSymbol("Real");
			}
			else if (thenType != null && (elseType == null || thenType.equals(elseType))) {
				result = thenType;
			}
			else if (elseType != null && (thenType == null || elseType.equals(thenType))) {
				result = elseType;
			}
			else if (thenType == null) {
				throw new Error("Could not determine the types of then and else branches of '" + expression + "'.");
			}
			else if (thenType.equals("Integer") && elseType.hasFunctor(INTEGER_INTERVAL)) { // TODO: I know, I know, this treatment of integers and interval is terrible... will fix at some point
				result = thenType;
			}
			else if (thenType.hasFunctor(INTEGER_INTERVAL) && elseType.equals("Integer")) {
				result = elseType;
			}
			else if (thenType.hasFunctor(INTEGER_INTERVAL) && elseType.hasFunctor(INTEGER_INTERVAL)) {
				IntegerInterval thenInterval = (IntegerInterval) thenType;
				IntegerInterval elseInterval = (IntegerInterval) elseType;
				Expression minimumLowerBound = 
						LessThan.simplify(apply(LESS_THAN, thenInterval.getNonStrictLowerBound(), elseInterval.getNonStrictLowerBound()), process).booleanValue()
						? thenInterval.getNonStrictLowerBound()
								: elseInterval.getNonStrictLowerBound();
				Expression maximumUpperBound =
						GreaterThan.simplify(apply(GREATER_THAN, thenInterval.getNonStrictUpperBound(), elseInterval.getNonStrictUpperBound()), process).booleanValue()
						? thenInterval.getNonStrictUpperBound()
								: elseInterval.getNonStrictUpperBound();
				if (minimumLowerBound.equals(MINUS_INFINITY) && maximumUpperBound.equals(INFINITY)) {
					result = makeSymbol("Integer");
				}
				else {
					result = apply(INTEGER_INTERVAL, minimumLowerBound, maximumUpperBound);
				}
			}
			else {
				throw new Error("'" + expression + "' then and else branches have different types (" + thenType + " and " + elseType + " respectively).");
			}
		}
		else if (expression.hasFunctor(CARDINALITY)) {
			result = makeSymbol("Integer");
		}
		else if (isNumericFunctionApplication(expression)) {
			if (Util.thereExists(expression.getArguments(), e -> Util.equals(getType(e, process), "Number"))) {
				result = makeSymbol("Number");
			}
			else if (Util.thereExists(expression.getArguments(), e -> Util.equals(getType(e, process), "Real"))) {
				result = makeSymbol("Real");
			}
			else {
				result = makeSymbol("Integer");
			}
		}
		else if (isComparisonFunctionApplication(expression)) {
				result = makeSymbol("Boolean");
		}
		else if (expression.getSyntacticFormType().equals("Symbol")) {
			if (expression.getValue() instanceof Integer) {
				result = makeSymbol("Integer");
			}
			else if (expression.getValue() instanceof Double) {
				result = makeSymbol("Real");
			}
			else if (expression.getValue() instanceof Rational) {
				Rational rational = (Rational) expression.getValue();
				boolean isInteger = rational.isInteger();
				result = makeSymbol(isInteger? "Integer" : "Real");
			}
			else if (expression.getValue() instanceof Number) {
				result = makeSymbol("Number");
			}
			else if (expression.getValue() instanceof String && expression.isStringLiteral()) {
				result = makeSymbol("String");
			}
			else if (expression.getValue() instanceof Boolean) {
				result = makeSymbol("Boolean");
			}
			else if (expression.equals(Expressions.INFINITY) || expression.equals(Expressions.MINUS_INFINITY)) {
				result = makeSymbol("Number");
			}
			else {
				result = process.getContextualSymbolType(expression);

				if (result == null) {
					Type type = getFirstSatisfyingPredicateOrNull(process.getTypes(), t -> t.contains(expression));
					if (type != null) {
						result = parse(type.getName());
					}
				}
			}
		}
		else if (expression.getSyntacticFormType().equals("Function application")) {
			Expression functionType = getType(expression.getFunctor(), process);
			if (functionType == null) {
				throw new Error("Type of '" + expression.getFunctor() + "' required, but unknown to rewriting process.");
			}
			Util.myAssert(() -> functionType.hasFunctor("->"), () -> "Functor " + expression.getFunctor() + " in expression " + expression + " should have functional type be an expression with functor '->', but has type instead equal to " + functionType);
			
			List<Expression> argumentsTypesList;
			Expression coDomain;
			if (functionType.numberOfArguments() == 1) {
				argumentsTypesList = Util.list();
				coDomain = functionType.get(0);
			}
			else {
				Expression argumentsType = functionType.get(0);
				boolean multipleArguments = argumentsType.hasFunctor(CARTESIAN_PRODUCT);
				argumentsTypesList = multipleArguments? argumentsType.getArguments() : list(argumentsType);
				coDomain = functionType.get(1);
			}
			Util.myAssert(() -> Util.mapIntoList(expression.getArguments(), new GetType(process)).equals(argumentsTypesList), () -> "Function " + expression.getFunctor() + " is of type " + functionType + " but is applied to " + expression.getArguments() + " which are of types " + Util.mapIntoList(expression.getArguments(), new GetType(process)));

			result = coDomain;
		}
		else if (expression instanceof QuantifiedExpressionWithABody){
			return getType(((QuantifiedExpressionWithABody) expression).getBody(), process);
		}
		else {
			throw new Error("GrinderUtil.getType does not yet know how to determine the type of this sort of expression: " + expression);
		}
		return result;
	}

	/**
	 * @param type
	 * @return
	 */
	public static boolean isIntegerOrReal(Expression type) {
		return type.equals("Integer") || type.equals("Real");
	}

	/**
	 * @param arguments
	 * @param process
	 * @return
	 */
	private static Expression getTypeOfCollectionOfNumericExpressionsWithDefaultInteger(List<Expression> arguments, RewritingProcess process) {
		Expression result;
		Expression first = getFirstOrNull(arguments);
		if (first == null) {
			result = makeSymbol("Integer");
		}
		else {
			result = getType(first, process);
		}
		return result;
	}

	private static Collection<String> arithmeticFunctors = Util.set(PLUS, TIMES, MINUS, DIVISION, EXPONENTIATION);
	
	public static boolean isNumericFunctionApplication(Expression expression) {
		boolean result =
				expression.getSyntacticFormType().equals("Function application")
				&& arithmeticFunctors.contains(expression.getFunctor().toString());
		return result;
	}

	private static Collection<String> comparisonFunctors = Util.set(LESS_THAN, LESS_THAN_OR_EQUAL_TO, GREATER_THAN, GREATER_THAN_OR_EQUAL_TO);
	
	public static boolean isComparisonFunctionApplication(Expression expression) {
		boolean result =
				expression.getSyntacticFormType().equals("Function application")
				&& comparisonFunctors.contains(expression.getFunctor().toString());
		return result;
	}

	/**
	 * Extend the rewriting processes's contextual symbols and constraints.
	 * Returns the same process instance if there are no changes.
	 * 
	 * @param extendingContextualSymbolsAndTypes
	 *            a map from variables to their types that
	 *            should be added to the a new sub-process of the process passed
	 *            in.
	 * @param additionalConstraints
	 *            additional context (i.e. a formula) to extend the contextual 
	 *            constraint by.
	 * @param process
	 *            the process in which the rewriting is occurring and whose
	 *            contextual constraint is to be updated.
	 * @return a sub-rewriting process with its contextual symbols and
	 *         constraints extended by the arguments passed in,
	 *         possibly the same as input process if no changes are made.
	 */
	public static RewritingProcess extendContextualSymbolsAndConstraint(
			Map<Expression, Expression> extendingContextualSymbolsAndTypes,
			Expression additionalConstraints, 
			RewritingProcess process) {
		
		if (extendingContextualSymbolsAndTypes.isEmpty() && additionalConstraints.equals(Expressions.TRUE)) { // nothing to do
			return process;
		}
		
		doNotAcceptTypesContainingTypeExpression(extendingContextualSymbolsAndTypes);
		
		process = renameExistingContextualSymbolsIfThereAreCollisions(extendingContextualSymbolsAndTypes, process);
		
		StackedHashMap<Expression, Expression> newMapOfContextualSymbolsAndTypes = createNewMapOfContextualSymbolsAndTypes(extendingContextualSymbolsAndTypes, process);
		// Note: StackedHashMap shares original entries with the original process's map
		
		Expression newContextualConstraint = checkAndAddNewConstraints(additionalConstraints, newMapOfContextualSymbolsAndTypes, process);
		
		RewritingProcess result = process.newSubProcessWithContext(newMapOfContextualSymbolsAndTypes, newContextualConstraint);

		return result;
	}

	private static RewritingProcess renameExistingContextualSymbolsIfThereAreCollisions(Map<Expression, Expression> extendingcontextualSymbolsAndTypes, RewritingProcess process) {
		for (Map.Entry<Expression, Expression> extendingContextualSymbolAndType : extendingcontextualSymbolsAndTypes.entrySet()) {
			Expression extendingContextualSymbol = extendingContextualSymbolAndType.getKey();
			if (process.getContextualSymbols().contains(extendingContextualSymbol)) {
				process = shadowContextualSymbol(extendingContextualSymbol, process);
			}
		}
		return process;
	}

	/** Replaces all occurrences of contextualSymbol in process' contextual symbols and constraint. */
	private static RewritingProcess shadowContextualSymbol(Expression contextualSymbol, RewritingProcess process) {
		// determines new unique name for contextualSymbol -- important: needs to start with capital letter to keep being recognized as a variable, not a constant!
		Expression newContextualSymbol = Expressions.prefixedUntilUnique(contextualSymbol, "Shadowed ", new NotContainedBy<Expression>(process.getContextualSymbols()));
		
		// makes new contextual symbols and types map
		Map<Expression, Expression> newcontextualSymbolsAndTypes = new LinkedHashMap<Expression, Expression>(process.getContextualSymbolsAndTypes());

		// replaces occurrences of contextualSymbol in types;
		// variables do not occur in types at this point (Jan/2014) but will when system is more general
		// needs to be on syntax tree because otherwise process will be extended during Expression.replace and we may be in infinite loop.
		for (Map.Entry<Expression, Expression> someContextualSymbolAndType : process.getContextualSymbolsAndTypes().entrySet()) {
			Expression type = someContextualSymbolAndType.getValue();
			if (type != null) {
				Expression someContextualSymbol = someContextualSymbolAndType.getKey();
				Expression newType = type.replaceAllOccurrences(contextualSymbol, newContextualSymbol, process);
				if (newType != type) {
					newcontextualSymbolsAndTypes.put(someContextualSymbol, newType);
				}
			}
		}
		
		// replaces key in contextualSymbol's entry by its new symbol
		newcontextualSymbolsAndTypes.put(newContextualSymbol, newcontextualSymbolsAndTypes.get(contextualSymbol));
		newcontextualSymbolsAndTypes.remove(contextualSymbol);
		
		// replaces contextualSymbol in the constraint
		Expression newContextualConstraint = process.getContextualConstraint().replaceAllOccurrences(contextualSymbol, newContextualSymbol, process);
		
		// assembles new process
		RewritingProcess newProcess = process.newSubProcessWithContext(newcontextualSymbolsAndTypes, newContextualConstraint);
		
		return newProcess;
	}

	private static void doNotAcceptTypesContainingTypeExpression(Map<Expression, Expression> extendingcontextualSymbolsAndTypes) throws Error {
		for (Map.Entry entry : extendingcontextualSymbolsAndTypes.entrySet()) {
			if (entry.getValue() != null && ((Expression)entry.getValue()).hasFunctor("type")) {
				throw new Error("'type' occurring in types extending context: " + entry);
			}
		}
	}

	private static StackedHashMap<Expression, Expression> createNewMapOfContextualSymbolsAndTypes(Map<Expression, Expression> indexToTypeMap, RewritingProcess process) {
		StackedHashMap<Expression, Expression> newMapOfContextualSymbolsAndTypes = new StackedHashMap<Expression, Expression>(process.getContextualSymbolsAndTypes());
		if (indexToTypeMap != null) {
			Map<Expression, Expression> symbolsToTypeMap2 = getTypesOfIndicesFunctorsOrSymbols(indexToTypeMap, process);
			//System.out.println("Symbols to type: " + symbolsToTypeMap2);	
			newMapOfContextualSymbolsAndTypes.putAll(symbolsToTypeMap2);
		}
		return newMapOfContextualSymbolsAndTypes;
	}

	private static Expression checkAndAddNewConstraints(Expression additionalConstraints, StackedHashMap<Expression, Expression> newMapOfcontextualSymbolsAndTypes, RewritingProcess process) throws Error {
		Expression newContextualConstraint = process.getContextualConstraint();
		// Only extend the contextual constraint with formulas
		if (!additionalConstraints.equals(Expressions.TRUE) && FormulaUtil.isFormula(additionalConstraints, process)) {
			checkThatAllFreeSymbolsInAdditionalConstraintsAreInContext(additionalConstraints, newMapOfcontextualSymbolsAndTypes, process);
			// Construct a conjunct of contextual constraints extended by the additional context
			newContextualConstraint = GrinderUtil.makeAnd(newContextualConstraint, additionalConstraints);
		} 
		else {
			// Note: commenting out for now due to the bloat caused in the trace output.
			// Trace.log("INFO: Not a formula to extend contextual constraint by: {}", additionalConstraints);
		}
		return newContextualConstraint;
	}

	private static void checkThatAllFreeSymbolsInAdditionalConstraintsAreInContext(Expression additionalConstraints, Map<Expression, Expression> newMapOfContextualSymbolsAndTypes, RewritingProcess process) throws Error {
		
		// For now, we are only adding the free variables (rather than free symbols as indicated by the method's name)
		// because we want to have the flexibility of manipulating constants without them having to have been added to context in advance,
		// which requires primitive constants registration, user constant registration/declaration, and either type declaration or inference,
		// all of these things being either burdensome or yet to be implemented.

		Set<Expression> freeSymbolsInAdditionalConstraints = Expressions.freeVariables(additionalConstraints, process);
		if (! newMapOfContextualSymbolsAndTypes.keySet().containsAll(freeSymbolsInAdditionalConstraints) &&
				! process.containsGlobalObjectKey(DO_NOT_REQUIRE_ADDED_CONTEXTUAL_CONSTRAINT_FREE_SYMBOLS_TO_BE_IN_CONTEXTUAL_VARIABLES)) {

			String message =
					"Extending contextual constraint with additional constraint <" + additionalConstraints +
					"> containing unknown free symbol " + Util.join(Util.subtract(freeSymbolsInAdditionalConstraints, newMapOfContextualSymbolsAndTypes.keySet())) + 
					" (current contextual symbols are {" + Util.join(newMapOfContextualSymbolsAndTypes.keySet()) + "})";
			throw new Error(message);
		}
		// The check above ensures that the additional constraints only use variables that are already known.
		// When this fails, the cause is a failure in the code somewhere to extend the process with scoping variables.
		// The easiest way to debug this is to place a breakpoint at the line above and, when it is reached, inspect the stack,
		// looking for the point in which the expression being process involves variables not in the process contextual symbols.
		// That point will be the spot where the process should have been extended.
	}

	/**
	 * Returns a list of index expressions corresponding to the free variables in an expressions and their types per the context, if any.
	 */
	public static IndexExpressionsSet getIndexExpressionsOfFreeVariablesIn(Expression expression, RewritingProcess process) {
		Set<Expression> freeVariables = Expressions.freeVariables(expression, process);
		IndexExpressionsSet result = makeIndexExpressionsForIndicesInListAndTypesInContext(freeVariables, process);
		return result;
	}

	/**
	 * Returns a list of index expressions corresponding to the given indices and their types per the context, if any.
	 */
	public static ExtensionalIndexExpressionsSet makeIndexExpressionsForIndicesInListAndTypesInContext(Collection<Expression> indices, RewritingProcess process) {
		List<Expression> indexExpressions = new LinkedList<Expression>();
		for (Expression index : indices) {
			Expression type = process.getContextualSymbolType(index);
			Expression indexExpression = IndexExpressions.makeIndexExpression(index, type);
			indexExpressions.add(indexExpression);
		}
		return new ExtensionalIndexExpressionsSet(indexExpressions);
	}

	/**
	 * Returns the cardinality of the type of a given variable in the given process,
	 * looking for <code>| Type |</code>, for <code>Type</code> the type of the variable,
	 * in the process global objects.
	 * If the size cannot be determined, returns -1.
	 * If the size is infinite, returns -2.
	 * @param symbol a variable
	 * @param process the rewriting process
	 * @return the cardinality of the type of the variable according to the process or -1 if it cannot be determined.
	 */
	public static long getTypeCardinality(Expression symbol, RewritingProcess process) {
		long result = -1;
	
		Expression variableType = process.getContextualSymbolType(symbol);
		if (variableType != null) {
			Expression typeCardinality = Expressions.apply(FunctorConstants.CARDINALITY, variableType);
			Expression typeCardinalityValue = (Expression) process.getGlobalObject(typeCardinality);
			if (typeCardinalityValue != null) {
				result = typeCardinalityValue.intValueExact();
			}
		}
		
		// If that didn't work, we try find the Type object:
		if (result == -1) {
			variableType = process.getContextualSymbolType(symbol);
			if (variableType != null) {
				Type type = process.getType(variableType);
				if (type != null) {
					Expression sizeExpression = type.cardinality();
					if (sizeExpression.equals(apply(CARDINALITY, type.getName()))) {
						result = -1;
					}
					else if (sizeExpression.equals(Expressions.INFINITY)) {
						result = -2;
					}
					else {
						result = sizeExpression.intValue();
					}
				}
			}
		}
		
		return result;
	}

	
	static final Expression _booleanType1 = parse("Boolean");
	static final Expression _booleanType2 = parse("'->'(Boolean)");
	static final Expression _booleanType3 = parse("bool");
	static final Expression _booleanType4 = parse("boolean");
	/**
	 * Indicates whether an expression is boolean-typed by having its {@link getType}
	 * type be "Boolean", "'->'(Boolean)", or "bool", or "boolean".
	 * @param expression
	 * @param process
	 * @return
	 */
	public static boolean isBooleanTyped(Expression expression, RewritingProcess process) {
		Expression type = getType(expression, process);
		boolean result =
				type != null &&
				(
				type.equals(_booleanType1) ||
				type.equals(_booleanType2) ||
				type.equals(_booleanType3) ||
				type.equals(_booleanType4));
		return result;
	}

	/**
	 * Returns a universal quantification of given expression over its free variables,
	 * with types as registered in rewriting process.
	 * @param expression
	 * @param process
	 * @return
	 */
	public static Expression universallyQuantifyFreeVariables(Expression expression, RewritingProcess process) {
		IndexExpressionsSet indexExpressions = getIndexExpressionsOfFreeVariablesIn(expression, process);
		Expression universallyQuantified = new DefaultUniversallyQuantifiedFormula(indexExpressions, expression);
		return universallyQuantified;
	}

	public static final Categorical BOOLEAN_TYPE = new Categorical("Boolean", 2, arrayList(TRUE, FALSE));
	public static final IntegerExpressoType INTEGER_TYPE = new IntegerExpressoType();
	
	/**
	 * A method mapping type expressions to their intrinsic {@link Type} objects,
	 * where "intrinsic" means there is only one possible {@link Type} object
	 * for them in the context of grinder
	 * (therefore, it cannot be used for, say, categorical types defined
	 * by the user and registered in the rewriting process by name only).
	 * Current recognized type expressions are
	 * <code>Boolean</code>, <code>Integer</code>, and function applications
	 * of the type <code>m..n</code>.
	 * If there is no such meaning, the method returns <code>null</code>.
	 * @param typeExpression
	 * @return
	 */
	public static Type fromTypeExpressionToItsIntrinsicMeaning(Expression typeExpression) throws Error {
		Type type;
		if (typeExpression.equals("Boolean")) {
			type = BOOLEAN_TYPE;
		}
		else if (typeExpression.equals("Integer")) {
			type = INTEGER_TYPE;
		}
		else if (typeExpression.hasFunctor(INTEGER_INTERVAL) && typeExpression.numberOfArguments() == 2) {
			type = new IntegerInterval(typeExpression.get(0), typeExpression.get(1));
		}
		else {
			type = null;
		}
		return type;
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
}
