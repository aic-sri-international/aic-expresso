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
import static com.sri.ai.grinder.sgdpllt.library.FunctorConstants.CARDINALITY;
import static com.sri.ai.grinder.sgdpllt.library.FunctorConstants.CARTESIAN_PRODUCT;
import static com.sri.ai.grinder.sgdpllt.library.FunctorConstants.DIVISION;
import static com.sri.ai.grinder.sgdpllt.library.FunctorConstants.EXPONENTIATION;
import static com.sri.ai.grinder.sgdpllt.library.FunctorConstants.GREATER_THAN;
import static com.sri.ai.grinder.sgdpllt.library.FunctorConstants.GREATER_THAN_OR_EQUAL_TO;
import static com.sri.ai.grinder.sgdpllt.library.FunctorConstants.INTEGER_INTERVAL;
import static com.sri.ai.grinder.sgdpllt.library.FunctorConstants.LESS_THAN;
import static com.sri.ai.grinder.sgdpllt.library.FunctorConstants.LESS_THAN_OR_EQUAL_TO;
import static com.sri.ai.grinder.sgdpllt.library.FunctorConstants.MAX;
import static com.sri.ai.grinder.sgdpllt.library.FunctorConstants.MINUS;
import static com.sri.ai.grinder.sgdpllt.library.FunctorConstants.PLUS;
import static com.sri.ai.grinder.sgdpllt.library.FunctorConstants.PRODUCT;
import static com.sri.ai.grinder.sgdpllt.library.FunctorConstants.SUM;
import static com.sri.ai.grinder.sgdpllt.library.FunctorConstants.TIMES;
import static com.sri.ai.util.Util.arrayList;
import static com.sri.ai.util.Util.getFirstOrNull;
import static com.sri.ai.util.Util.getFirstSatisfyingPredicateOrNull;
import static com.sri.ai.util.Util.list;
import static com.sri.ai.util.Util.mapIntoList;
import static java.lang.Integer.parseInt;

import java.util.ArrayList;
import java.util.Collection;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Set;

import com.google.common.annotations.Beta;
import com.google.common.base.Predicate;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.IndexExpressionsSet;
import com.sri.ai.expresso.api.IntensionalSet;
import com.sri.ai.expresso.api.QuantifiedExpressionWithABody;
import com.sri.ai.expresso.api.Type;
import com.sri.ai.expresso.core.AbstractExtensionalSet;
import com.sri.ai.expresso.core.DefaultIntensionalMultiSet;
import com.sri.ai.expresso.core.DefaultUniversallyQuantifiedFormula;
import com.sri.ai.expresso.core.ExtensionalIndexExpressionsSet;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.expresso.type.Categorical;
import com.sri.ai.expresso.type.IntegerExpressoType;
import com.sri.ai.expresso.type.IntegerInterval;
import com.sri.ai.expresso.type.RealExpressoType;
import com.sri.ai.expresso.type.RealInterval;
import com.sri.ai.grinder.api.Registry;
import com.sri.ai.grinder.sgdpllt.library.Disequality;
import com.sri.ai.grinder.sgdpllt.library.Equality;
import com.sri.ai.grinder.sgdpllt.library.FormulaUtil;
import com.sri.ai.grinder.sgdpllt.library.FunctorConstants;
import com.sri.ai.grinder.sgdpllt.library.controlflow.IfThenElse;
import com.sri.ai.grinder.sgdpllt.library.indexexpression.IndexExpressions;
import com.sri.ai.grinder.sgdpllt.library.number.GreaterThan;
import com.sri.ai.grinder.sgdpllt.library.number.LessThan;
import com.sri.ai.grinder.sgdpllt.library.set.Sets;
import com.sri.ai.grinder.sgdpllt.library.set.tuple.Tuple;
import com.sri.ai.util.Util;
import com.sri.ai.util.math.Rational;

/**
 * General purpose utility routines related to grinder libraries.
 * 
 * @author oreilly
 */
@Beta
public class GrinderUtil {

	/**
	 * Returns a registry with symbols extended by a list of index expressions.
	 */
	public static Registry extendRegistryWithIndexExpressions(IndexExpressionsSet indexExpressions, Registry registry) {
		Map<Expression, Expression> indexToTypeMap = IndexExpressions.getIndexToTypeMapWithDefaultNull(indexExpressions);
		Registry result = registry.registerIndicesAndTypes(indexToTypeMap);
		return result;
	}

	/**
	 * Returns a registry with symbols extended by a list of index expressions.
	 */
	public static Registry extendSymbolsWithIndexExpressions(List<Expression> indexExpressions, Registry registry) {
		Registry result = 
				extendRegistryWithIndexExpressions(
						new ExtensionalIndexExpressionsSet(indexExpressions),
						registry);
		return result;
	}

	/**
	 * Returns a list of index expressions corresponding to the free variables in an expressions and their types per the registry, if any.
	 */
	public static IndexExpressionsSet getIndexExpressionsOfFreeVariablesIn(Expression expression, Registry registry) {
		Set<Expression> freeVariables = Expressions.freeVariables(expression, registry);
		IndexExpressionsSet result = makeIndexExpressionsForIndicesInListAndTypesInRegistry(freeVariables, registry);
		return result;
	}

	/**
	 * Returns a list of index expressions corresponding to the given indices and their types per the registry, if any.
	 */
	public static ExtensionalIndexExpressionsSet makeIndexExpressionsForIndicesInListAndTypesInRegistry(Collection<Expression> indices, Registry registry) {
		List<Expression> indexExpressions = new LinkedList<Expression>();
		for (Expression index : indices) {
			Expression type = registry.getTypeOfRegisteredSymbol(index);
			Expression indexExpression = IndexExpressions.makeIndexExpression(index, type);
			indexExpressions.add(indexExpression);
		}
		return new ExtensionalIndexExpressionsSet(indexExpressions);
	}

	/**
	 * @param mapFromSymbolNameToTypeName
	 * @param additionalTypes
	 * @param mapFromCategoricalTypeNameToSizeString
	 * @param registry
	 * @return
	 */
	public static Registry extendRegistryWith(Map<String, String> mapFromSymbolNameToTypeName, Collection<Type> additionalTypes, Map<String, String> mapFromCategoricalTypeNameToSizeString, Predicate<Expression> isUniquelyNamedConstantPredicate, Registry registry) {
		Collection<Type> allTypes =
				getCategoricalTypes(
						mapFromSymbolNameToTypeName,
						mapFromCategoricalTypeNameToSizeString,
						isUniquelyNamedConstantPredicate,
						registry);
		allTypes.addAll(additionalTypes);
		
		return extendRegistryWith(mapFromSymbolNameToTypeName, allTypes, registry);
	}

	/**
	 * @param mapFromSymbolNameToTypeName
	 * @param types
	 * @param registry
	 * @return
	 */
	public static Registry extendRegistryWith(Map<String, String> mapFromSymbolNameToTypeName, Collection<? extends Type> types, Registry registry) {
		List<Expression> symbolDeclarations = new ArrayList<>();
		for (Map.Entry<String, String> variableNameAndTypeName : mapFromSymbolNameToTypeName.entrySet()) {
			String symbolName = variableNameAndTypeName.getKey();
			String typeName   = variableNameAndTypeName.getValue();
			
			symbolDeclarations.add(parse(symbolName + " in " + typeName));
		}
		registry = extendSymbolsWithIndexExpressions(symbolDeclarations, registry);
					
		for (Type type : types) {
			registry = registry.add(type);
			registry = registry.putGlobalObject(parse("|" + type.getName() + "|"), type.cardinality());
		}
		
		return registry;
	}

	/**
	 * Returns a universal quantification of given expression over its free variables,
	 * with types as registered in registry.
	 * @param expression
	 * @param registry
	 * @return
	 */
	public static Expression universallyQuantifyFreeVariables(Expression expression, Registry registry) {
		IndexExpressionsSet indexExpressions = getIndexExpressionsOfFreeVariablesIn(expression, registry);
		Expression universallyQuantified = new DefaultUniversallyQuantifiedFormula(indexExpressions, expression);
		return universallyQuantified;
	}

	/**
	 * @param mapFromSymbolNameToTypeName
	 * @param mapFromCategoricalTypeNameToSizeString
	 * @param isUniquelyNamedConstantPredicate
	 * @param registry
	 * @return
	 */
	public static Collection<Type> getCategoricalTypes(
			Map<String, String> mapFromSymbolNameToTypeName,
			Map<String, String> mapFromCategoricalTypeNameToSizeString,
			Predicate<Expression> isUniquelyNamedConstantPredicate,
			Registry registry) {
		
		Collection<Type> categoricalTypes = new LinkedList<Type>();
		for (Map.Entry<String, String> typeNameAndSizeString : mapFromCategoricalTypeNameToSizeString.entrySet()) {
			String typeExpressionString = typeNameAndSizeString.getKey();
			String sizeString = typeNameAndSizeString.getValue();
			
			// check if already present and, if not, make it
			Categorical type = (Categorical) registry.getType(typeExpressionString);
			if (type == null) {
				if (typeExpressionString.equals("Boolean")) {
					type = BOOLEAN_TYPE;
				}
				else {
					ArrayList<Expression> knownConstants = 
							getKnownUniquelyNamedConstaintsOf(
									typeExpressionString,
									mapFromSymbolNameToTypeName,
									isUniquelyNamedConstantPredicate,
									registry);
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
	 * @param registry
	 * @return
	 */
	public static ArrayList<Expression> getKnownUniquelyNamedConstaintsOf(String typeName, Map<String, String> mapFromSymbolNameToTypeName, Predicate<Expression> isUniquelyNamedConstantPredicate, Registry registry) {
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
	 * Gets a function application and its type <code>T</code>, and returns the inferred type of its functor,
	 * which is <code>'->'('x'(T1, ..., Tn), T)</code>, where <code>T1,...,Tn</code> are the types.
	 */
	public static Expression getTypeOfFunctor(Expression functionApplication, Expression functionApplicationType, Registry registry) {
		Expression result;
		if (functionApplication.getSyntacticFormType().equals("Function application")) {
			List<Expression> argumentTypes = Util.mapIntoArrayList(functionApplication.getArguments(), new GetType(registry));
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
	 * Returns the type of given expression according to registry.
	 */
	public static Expression getType(Expression expression, Registry registry) {
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
				result = getType(head, registry);
			}
			else if (argument.getSyntacticFormType().equals("Extensional set")) {
				List<Expression> arguments = ((AbstractExtensionalSet)argument).getElementsDefinitions();
				result = getTypeOfCollectionOfNumericExpressionsWithDefaultInteger(arguments, registry);
			}
			else if (expression.hasFunctor(MAX)) { // MAX can also be applied to a bunch of numbers
				result = getTypeOfCollectionOfNumericExpressionsWithDefaultInteger(expression.getArguments(), registry);
			}
			else {
				throw new Error(expression.getFunctor() + " defined for sets only but got " + expression.get(0));
			}
		}
		else if (Equality.isEquality(expression) || Disequality.isDisequality(expression)) {
			result = makeSymbol("Boolean");
		}
		else if (
				expression.equals(FunctorConstants.REAL_INTERVAL_CLOSED_CLOSED) ||
				expression.equals(FunctorConstants.REAL_INTERVAL_CLOSED_OPEN) ||
				expression.equals(FunctorConstants.REAL_INTERVAL_OPEN_CLOSED) ||
				expression.equals(FunctorConstants.REAL_INTERVAL_OPEN_OPEN)
				) {
			result = Expressions.apply("->", Tuple.make("Number", "Number"), "Set");
		}
		else if (IfThenElse.isIfThenElse(expression)) {
			Expression thenType = getType(IfThenElse.thenBranch(expression), registry);
			Expression elseType = getType(IfThenElse.elseBranch(expression), registry);
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
						LessThan.simplify(apply(LESS_THAN, thenInterval.getNonStrictLowerBound(), elseInterval.getNonStrictLowerBound()), registry).booleanValue()
						? thenInterval.getNonStrictLowerBound()
								: elseInterval.getNonStrictLowerBound();
				Expression maximumUpperBound =
						GreaterThan.simplify(apply(GREATER_THAN, thenInterval.getNonStrictUpperBound(), elseInterval.getNonStrictUpperBound()), registry).booleanValue()
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
			if (Util.thereExists(expression.getArguments(), e -> Util.equals(getType(e, registry), "Number"))) {
				result = makeSymbol("Number");
			}
			else if (Util.thereExists(expression.getArguments(), e -> Util.equals(getType(e, registry), "Real"))) {
				result = makeSymbol("Real");
			}
			else if (Util.thereExists(expression.getArguments(), e -> isRealInterval(getType(e, registry)))) {
				result = makeSymbol("Real");
			}
			else {
				result = makeSymbol("Integer");
			}
		}
		else if (
				expression.hasFunctor(FunctorConstants.INTEGER_INTERVAL) ||
				expression.hasFunctor(FunctorConstants.REAL_INTERVAL_CLOSED_CLOSED) ||
				expression.hasFunctor(FunctorConstants.REAL_INTERVAL_OPEN_CLOSED) ||
				expression.hasFunctor(FunctorConstants.REAL_INTERVAL_CLOSED_OPEN) ||
				expression.hasFunctor(FunctorConstants.REAL_INTERVAL_OPEN_OPEN)
				) {
			result = makeSymbol("Set");
		}
		else if (isComparisonFunctionApplication(expression)) {
				result = makeSymbol("Boolean");
		}
		else if (Sets.isIntensionalMultiSet(expression)) {
			IntensionalSet set = (IntensionalSet) expression;
			Expression headType = getType(set.getHead(), registry);
			result = new DefaultIntensionalMultiSet(list(), headType, TRUE);
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
				result = registry.getTypeOfRegisteredSymbol(expression);

				if (result == null) {
					Type type = getFirstSatisfyingPredicateOrNull(registry.getTypes(), t -> t.contains(expression));
					if (type != null) {
						result = parse(type.getName());
					}
				}
			}
		}
		else if (expression.getSyntacticFormType().equals("Function application")) {
			Expression functionType = getType(expression.getFunctor(), registry);
			if (functionType == null) {
				throw new Error("Type of '" + expression.getFunctor() + "' required, but unknown to registry.");
			}
			
			if (!functionType.hasFunctor("->")) {
				throw new Error("Functor " + expression.getFunctor() + " in expression " + expression + " should have functional type (that is, have functor '->'), but has type instead equal to " + functionType);
			}
			
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

			if (!mapIntoList(expression.getArguments(), new GetType(registry)).equals(argumentsTypesList)) {
				throw new Error("Function " + expression.getFunctor() + " is of type " + functionType + " but is applied to " + expression.getArguments() + " which are of types " + Util.mapIntoList(expression.getArguments(), new GetType(registry)));
			}

			result = coDomain;
		}
		else if (expression instanceof QuantifiedExpressionWithABody){
			return getType(((QuantifiedExpressionWithABody) expression).getBody(), registry);
		}
		else {
			throw new Error("GrinderUtil.getType does not yet know how to determine the type of this sort of expression: " + expression);
		}
		return result;
	}

	/**
	 * @param e
	 * @return
	 */
	private static boolean isRealInterval(Expression e) {
		return e.hasFunctor(FunctorConstants.REAL_INTERVAL_CLOSED_CLOSED)
		|| e.hasFunctor(FunctorConstants.REAL_INTERVAL_OPEN_CLOSED)
		|| e.hasFunctor(FunctorConstants.REAL_INTERVAL_CLOSED_OPEN)
		|| e.hasFunctor(FunctorConstants.REAL_INTERVAL_OPEN_OPEN);
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
	 * @param registry
	 * @return
	 */
	private static Expression getTypeOfCollectionOfNumericExpressionsWithDefaultInteger(List<Expression> arguments, Registry registry) {
		Expression result;
		Expression first = getFirstOrNull(arguments);
		if (first == null) {
			result = makeSymbol("Integer");
		}
		else {
			result = getType(first, registry);
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
	 * Returns the cardinality of the type of a given variable in the given registry,
	 * looking for <code>| Type |</code>, for <code>Type</code> the type of the variable,
	 * in the registry global objects.
	 * If the size cannot be determined, returns -1.
	 * If the size is infinite, returns -2.
	 * @param symbol a variable
	 * @param registry the registry
	 * @return the cardinality of the type of the variable according to the registry or -1 if it cannot be determined.
	 */
	public static long getTypeCardinality(Expression symbol, Registry registry) {
		long result = -1;
	
		Expression variableType = registry.getTypeOfRegisteredSymbol(symbol);
		if (variableType != null) {
			Expression typeCardinality = Expressions.apply(FunctorConstants.CARDINALITY, variableType);
			Expression typeCardinalityValue = (Expression) registry.getGlobalObject(typeCardinality);
			if (typeCardinalityValue != null) {
				result = typeCardinalityValue.intValueExact();
			}
		}
		
		// If that didn't work, we try find the Type object:
		if (result == -1) {
			variableType = registry.getTypeOfRegisteredSymbol(symbol);
			if (variableType != null) {
				Type type = registry.getType(variableType);
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
	 * @param registry
	 * @return
	 */
	public static boolean isBooleanTyped(Expression expression, Registry registry) {
		Expression type = getType(expression, registry);
		boolean result =
				type != null &&
				(
				type.equals(_booleanType1) ||
				type.equals(_booleanType2) ||
				type.equals(_booleanType3) ||
				type.equals(_booleanType4));
		return result;
	}

	public static final Categorical BOOLEAN_TYPE = new Categorical("Boolean", 2, arrayList(TRUE, FALSE));
	public static final IntegerExpressoType INTEGER_TYPE = new IntegerExpressoType();
	public static final RealExpressoType REAL_TYPE = new RealExpressoType();
	
	/**
	 * A method mapping type expressions to their intrinsic {@link Type} objects,
	 * where "intrinsic" means there is only one possible {@link Type} object
	 * for them in the registry of grinder
	 * (therefore, it cannot be used for, say, categorical types defined
	 * by the user and registered in the registry by name only).
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
		else if (typeExpression.equals("Real")) {
			type = REAL_TYPE;
		}
		else if (typeExpression.hasFunctor(INTEGER_INTERVAL) && typeExpression.numberOfArguments() == 2) {
			type = new IntegerInterval(typeExpression.get(0), typeExpression.get(1));
		}
		else if (
				(
						typeExpression.hasFunctor(FunctorConstants.REAL_INTERVAL_CLOSED_CLOSED) ||
						typeExpression.hasFunctor(FunctorConstants.REAL_INTERVAL_OPEN_CLOSED) ||
						typeExpression.hasFunctor(FunctorConstants.REAL_INTERVAL_CLOSED_OPEN) ||
						typeExpression.hasFunctor(FunctorConstants.REAL_INTERVAL_OPEN_OPEN)
				)
				&& typeExpression.numberOfArguments() == 2) {
			type = new RealInterval(typeExpression.toString());
		}
		else {
			type = null;
		}
		return type;
	}
}
