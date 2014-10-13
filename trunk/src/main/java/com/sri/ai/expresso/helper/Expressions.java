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
package com.sri.ai.expresso.helper;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.Stack;

import com.google.common.annotations.Beta;
import com.google.common.base.Function;
import com.google.common.base.Predicate;
import com.google.common.collect.Lists;
import com.sri.ai.expresso.api.CompoundSyntaxTree;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.ExpressionAndContext;
import com.sri.ai.expresso.api.LambdaExpression;
import com.sri.ai.expresso.api.Parser;
import com.sri.ai.expresso.api.Symbol;
import com.sri.ai.expresso.api.SyntaxLeaf;
import com.sri.ai.expresso.api.SyntaxTree;
import com.sri.ai.expresso.core.DefaultBracketedExpression;
import com.sri.ai.expresso.core.DefaultExistentiallyQuantifiedFormula;
import com.sri.ai.expresso.core.DefaultExpressionAndContext;
import com.sri.ai.expresso.core.DefaultExtensionalMultiSet;
import com.sri.ai.expresso.core.DefaultExtensionalUniSet;
import com.sri.ai.expresso.core.DefaultFunctionApplication;
import com.sri.ai.expresso.core.DefaultIntensionalMultiSet;
import com.sri.ai.expresso.core.DefaultIntensionalUniSet;
import com.sri.ai.expresso.core.DefaultLambdaExpression;
import com.sri.ai.expresso.core.DefaultSymbol;
import com.sri.ai.expresso.core.DefaultSyntacticFunctionApplication;
import com.sri.ai.expresso.core.DefaultTuple;
import com.sri.ai.expresso.core.DefaultUniversallyQuantifiedFormula;
import com.sri.ai.grinder.api.Rewriter;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.core.DefaultRewritingProcess;
import com.sri.ai.grinder.core.PruningPredicate;
import com.sri.ai.grinder.helper.FunctionSignature;
import com.sri.ai.grinder.helper.RewriterFunction;
import com.sri.ai.grinder.library.Equality;
import com.sri.ai.grinder.library.FunctorConstants;
import com.sri.ai.grinder.library.IsVariable;
import com.sri.ai.grinder.library.boole.And;
import com.sri.ai.grinder.library.boole.ForAll;
import com.sri.ai.grinder.library.boole.ThereExists;
import com.sri.ai.grinder.library.equality.cardinality.direct.core.CardinalityTypeOfLogicalVariable;
import com.sri.ai.grinder.library.indexexpression.IndexExpressions;
import com.sri.ai.grinder.library.set.extensional.ExtensionalSet;
import com.sri.ai.grinder.library.set.intensional.IntensionalSet;
import com.sri.ai.grinder.library.set.tuple.Tuple;
import com.sri.ai.grinder.parser.antlr.AntlrGrinderParserWrapper;
import com.sri.ai.util.Util;
import com.sri.ai.util.base.Equals;
import com.sri.ai.util.base.GetFirstOfPair;
import com.sri.ai.util.base.GetSecondOfPair;
import com.sri.ai.util.base.NotContainedBy;
import com.sri.ai.util.base.Pair;
import com.sri.ai.util.base.SingletonListMaker;
import com.sri.ai.util.collect.FunctionIterator;
import com.sri.ai.util.collect.IntegerIterator;
import com.sri.ai.util.collect.ZipIterator;
import com.sri.ai.util.math.Rational;

/**
 * A class of helper methods for {@link Expression}s.
 * 
 * @author braz
 */
@Beta
public class Expressions {
	
	/**
	 * An unmodifiable empty list of expressions.
	 */
	public static final List<Expression> EMPTY_LIST = Collections.unmodifiableList(new ArrayList<Expression>());
	
	public static final Expression TRUE            = Expressions.makeSymbol("true");
	public static final Expression FALSE           = Expressions.makeSymbol("false");
	public static final Expression ZERO            = Expressions.makeSymbol(0);
	public static final Expression ZERO_POINT_FIVE = Expressions.makeSymbol(0.5);
	public static final Expression ONE             = Expressions.makeSymbol(1);
	public static final Expression TWO             = Expressions.makeSymbol(2);
	public static final Expression THREE           = Expressions.makeSymbol(3);
	//
	private static final SingletonListMaker<Integer> INTEGER_SINGLETON_LIST_MAKER = new SingletonListMaker<Integer>();
	
	/**
	 * Returns an expression represented by a given syntax tree.
	 * Scheduled to be removed once expressions are no longer based on syntax trees.
	 */
	@Deprecated
	public static Expression makeFromSyntaxTree(SyntaxTree syntaxTree) {
		if (syntaxTree instanceof CompoundSyntaxTree) {
			Expression result = makeExpressionOnSyntaxTreeWithLabelAndSubTrees(syntaxTree.getLabel(), syntaxTree.getImmediateSubTrees().toArray());
//			Expression result = new ExpressionOnCompoundSyntaxTree(syntaxTree);
			return result;
		}
		if (syntaxTree instanceof SyntaxLeaf) {
			Expression result = Expressions.makeSymbol(syntaxTree.getValue());
//			Expression result = new ExpressionOnSyntaxLeaf(syntaxTree);
			return result;
		}
		throw new Error("Syntax tree " + syntaxTree + " should be either a CompoundSyntaxTree or a Symbol");
	}

	public static final Function<SyntaxTree, Expression> SYNTAX_TREE_TO_EXPRESSION = new Function<SyntaxTree, Expression>() {
		@Override
		public Expression apply(SyntaxTree input) {
			Expression result = makeFromSyntaxTree(input);
			return result;
		}
	};
	
	/**
	 * Makes Expression based on a syntax tree with given label and sub-trees, or {@link Expression}s from whose syntax trees must be used.
	 */
	public static Expression makeExpressionOnSyntaxTreeWithLabelAndSubTrees(Object label, Object... subTreeObjects) {
		return makeExpressionOnSyntaxTreeWithLabelAndSubTreesWithRandomPredicatesSignatures(null, label, subTreeObjects);
	}

	public static Expression makeExpressionOnSyntaxTreeWithLabelAndSubTreesWithRandomPredicatesSignatures(Collection<FunctionSignature> randomPredicatesSignatures, Object label, Object... subTreeObjects) {
		Expression result;
		if (label.equals("[ . ]")) {
			result = makeDefaultBracketedExpressionFromLabelAndSubTrees(randomPredicatesSignatures, label, subTreeObjects);
		}
		else if (label.equals(ForAll.LABEL)) {
			result = makeDefaultUniversallyQuantifiedFormulaFromLabelAndSubTrees(label, subTreeObjects);
		}
		else if (label.equals(ThereExists.LABEL)) {
			result = makeDefaultExistentiallyQuantifiedFormulaFromLabelAndSubTrees(label, subTreeObjects);
		}
		else if (label.equals(LambdaExpression.ROOT)) {
			result = makeDefaultLambdaExpressionFromLabelAndSubTrees(label, subTreeObjects);
		}
		else if (label.equals(Tuple.TUPLE_LABEL) || label.equals("tuple")) {
			result = makeDefaultTupleFromLabelAndSubTrees(label, subTreeObjects);
		}
		else if (label.equals(ExtensionalSet.UNI_SET_LABEL)) {
			result = makeDefaultExtensionalUniSetFromLabelAndSubTrees(label, subTreeObjects);
		}
		else if (label.equals(ExtensionalSet.MULTI_SET_LABEL)) {
			result = makeDefaultExtensionalMultiSetFromLabelAndSubTrees(label, subTreeObjects);
		}
		else if (label.equals(IntensionalSet.UNI_SET_LABEL)) {
			result = makeDefaultIntensionalUniSetFromLabelAndSubTrees(label, subTreeObjects);
		}
		else if (label.equals(IntensionalSet.MULTI_SET_LABEL)) {
			result = makeDefaultIntensionalMultiSetFromLabelAndSubTrees(label, subTreeObjects);
		}
		else if (label.equals(CardinalityTypeOfLogicalVariable.TYPE_LABEL)) {
			result = makeDefaultSyntacticFunctionApplicationFromLabelAndSubTrees(label, subTreeObjects);
		}
		else {
			result = makeDefaultFunctionApplicationFromLabelAndSubTrees(label, subTreeObjects);
		}
		return result;
	}
	
	/**
	 * Makes Expression based on a syntax tree with given label and sub-trees, or {@link Expression}s from whose syntax trees must be used.
	 */
	public static Expression makeDefaultBracketedExpressionFromLabelAndSubTrees(Collection<FunctionSignature> randomPredicatesSignatures, Object label, Object... subTreeObjects) {
//		if (randomPredicatesSignatures == null) {
//			randomPredicatesSignatures = DefaultBracketedExpression.defaultPredicateSignatures;
//		}
//
//		if (randomPredicatesSignatures == null) {
//			throw new Error("Making bracketed expression without providing random predicates");
//		}
		
		Expression result;
		if (subTreeObjects.length == 1 && subTreeObjects[0] instanceof Collection) {
			subTreeObjects = ((Collection) subTreeObjects[0]).toArray();
		}
		ArrayList<Expression> subTreeExpressions = Util.mapIntoArrayList(subTreeObjects, Expressions::makeFromObject);
		result = new DefaultBracketedExpression(subTreeExpressions.get(0), randomPredicatesSignatures);
		return result;
	}

	private static Expression makeDefaultLambdaExpressionFromLabelAndSubTrees(Object label, Object[] subTreeObjects) {
		ArrayList<Expression> subTreeExpressions = Util.mapIntoArrayList(subTreeObjects, Expressions::makeFromObject);
		Expression indexExpressionsKleeneList = subTreeExpressions.get(0);
		List<Expression> indexExpressions = ensureListFromKleeneList(indexExpressionsKleeneList);
		Expression body = subTreeExpressions.get(1);
		Expression result = new DefaultLambdaExpression(indexExpressions, body);
		return result;
	}

	private static Expression makeDefaultUniversallyQuantifiedFormulaFromLabelAndSubTrees(Object label, Object[] subTreeObjects) {
		ArrayList<Expression> subTreeExpressions = Util.mapIntoArrayList(subTreeObjects, Expressions::makeFromObject);
		Expression indexExpressionsKleeneList = subTreeExpressions.get(0);
		List<Expression> indexExpressions = ensureListFromKleeneList(indexExpressionsKleeneList);
		Expression body = subTreeExpressions.get(1);
		Expression result = new DefaultUniversallyQuantifiedFormula(indexExpressions, body);
		return result;
	}

	private static Expression makeDefaultExistentiallyQuantifiedFormulaFromLabelAndSubTrees(Object label, Object[] subTreeObjects) {
		ArrayList<Expression> subTreeExpressions = Util.mapIntoArrayList(subTreeObjects, Expressions::makeFromObject);
		Expression indexExpressionsKleeneList = subTreeExpressions.get(0);
		List<Expression> indexExpressions = ensureListFromKleeneList(indexExpressionsKleeneList);
		Expression body = subTreeExpressions.get(1);
		Expression result = new DefaultExistentiallyQuantifiedFormula(indexExpressions, body);
		return result;
	}

	private static Expression makeDefaultFunctionApplicationFromLabelAndSubTrees(Object label, Object[] subTreeObjects) {
		if (subTreeObjects.length == 1 && subTreeObjects[0] instanceof Collection) {
			subTreeObjects = ((Collection) subTreeObjects[0]).toArray();
		}
		Expression labelExpression = makeFromObject(label);
		ArrayList<Expression> subTreeExpressions = Util.mapIntoArrayList(subTreeObjects, Expressions::makeFromObject);
		Expression result = new DefaultFunctionApplication(labelExpression, subTreeExpressions);
		return result;
	}

	private static Expression makeDefaultSyntacticFunctionApplicationFromLabelAndSubTrees(Object label, Object[] subTreeObjects) {
		if (subTreeObjects.length == 1 && subTreeObjects[0] instanceof Collection) {
			subTreeObjects = ((Collection) subTreeObjects[0]).toArray();
		}
		Expression labelExpression = makeFromObject(label);
		ArrayList<Expression> subTreeExpressions = Util.mapIntoArrayList(subTreeObjects, Expressions::makeFromObject);
		Expression result = new DefaultSyntacticFunctionApplication(labelExpression, subTreeExpressions);
		return result;
	}

	private static Expression makeDefaultTupleFromLabelAndSubTrees(Object label, Object[] subTreeObjects) {
		if (subTreeObjects.length == 1 && subTreeObjects[0] instanceof Collection) {
			subTreeObjects = ((Collection) subTreeObjects[0]).toArray();
		}
		ArrayList<Expression> subTreeExpressions = Util.mapIntoArrayList(subTreeObjects, Expressions::makeFromObject);
		if (subTreeExpressions.size() == 1) {
			subTreeExpressions = new ArrayList<Expression>(Expressions.ensureListFromKleeneList(subTreeExpressions.get(0)));
		}
		Expression result = new DefaultTuple(subTreeExpressions);
		return result;
	}

	private static Expression makeDefaultExtensionalUniSetFromLabelAndSubTrees(Object label, Object[] subTreeObjects) {
		if (subTreeObjects.length == 1 && subTreeObjects[0] instanceof Collection) {
			subTreeObjects = ((Collection) subTreeObjects[0]).toArray();
		}
		ArrayList<Expression> subTreeExpressions = Util.mapIntoArrayList(subTreeObjects, Expressions::makeFromObject);
		if (subTreeExpressions.size() == 1) {
			subTreeExpressions = new ArrayList<Expression>(Expressions.ensureListFromKleeneList(subTreeExpressions.get(0)));
		}
		Expression result = new DefaultExtensionalUniSet(subTreeExpressions);
		return result;
	}

	private static Expression makeDefaultExtensionalMultiSetFromLabelAndSubTrees(Object label, Object[] subTreeObjects) {
		if (subTreeObjects.length == 1 && subTreeObjects[0] instanceof Collection) {
			subTreeObjects = ((Collection) subTreeObjects[0]).toArray();
		}
		ArrayList<Expression> subTreeExpressions = Util.mapIntoArrayList(subTreeObjects, Expressions::makeFromObject);
		if (subTreeExpressions.size() == 1) {
			subTreeExpressions = new ArrayList<Expression>(Expressions.ensureListFromKleeneList(subTreeExpressions.get(0)));
		}
		Expression result = new DefaultExtensionalMultiSet(subTreeExpressions);
		return result;
	}

	private static Expression makeDefaultIntensionalUniSetFromLabelAndSubTrees(Object label, Object[] subTreeObjects) {
		if (subTreeObjects.length == 1 && subTreeObjects[0] instanceof Collection) {
			subTreeObjects = ((Collection) subTreeObjects[0]).toArray();
		}
		ArrayList<Expression> subTreeExpressions = Util.mapIntoArrayList(subTreeObjects, Expressions::makeFromObject);
		if (subTreeExpressions.size() == 1) {
			subTreeExpressions = new ArrayList<Expression>(Expressions.ensureListFromKleeneList(subTreeExpressions.get(0)));
		}
		
		Expression scopingExpression = subTreeExpressions.get(0);
		List<Expression> indexExpressions =
				(scopingExpression == null || scopingExpression.numberOfArguments() == 0)?
						Util.list()
						: new ArrayList<Expression>(Expressions.ensureListFromKleeneList(scopingExpression.get(0)));
		
		Expression conditioningSyntaxTree = subTreeExpressions.get(2);
		Expression condition = conditioningSyntaxTree == null? Expressions.TRUE : conditioningSyntaxTree.get(0);
		
		Expression result = new DefaultIntensionalUniSet(indexExpressions, subTreeExpressions.get(1), condition);
		return result;
	}

	private static Expression makeDefaultIntensionalMultiSetFromLabelAndSubTrees(Object label, Object[] subTreeObjects) {
		if (subTreeObjects.length == 1 && subTreeObjects[0] instanceof Collection) {
			subTreeObjects = ((Collection) subTreeObjects[0]).toArray();
		}
		ArrayList<Expression> subTreeExpressions = Util.mapIntoArrayList(subTreeObjects, Expressions::makeFromObject);
		if (subTreeExpressions.size() == 1) {
			subTreeExpressions = new ArrayList<Expression>(Expressions.ensureListFromKleeneList(subTreeExpressions.get(0)));
		}
		
		Expression scopingExpression = subTreeExpressions.get(0);
		List<Expression> indexExpressions =
				(scopingExpression == null || scopingExpression.numberOfArguments() == 0)?
						Util.list()
						: new ArrayList<Expression>(Expressions.ensureListFromKleeneList(scopingExpression.get(0)));
		
		Expression conditioningSyntaxTree = subTreeExpressions.get(2);
		Expression condition = conditioningSyntaxTree == null? Expressions.TRUE : conditioningSyntaxTree.get(0);
		
		Expression result = new DefaultIntensionalMultiSet(indexExpressions, subTreeExpressions.get(1), condition);
		return result;
	}

	/**
	 * @param object
	 */
	public static Expression makeFromObject(Object object) {
		Expression result;
		if (object == null) { // this is here for backwards compatibility with syntax-tree-based expressions, should be gone when they are.
			result = null;
		}
		else if (object instanceof SyntaxTree) {
			result = makeFromSyntaxTree((SyntaxTree) object);
		}
		else if (object instanceof Expression) {
			result = (Expression) object;
		}
		else {
			result = DefaultSymbol.createSymbol(object);
		}
		return result;
	}

	/**
	 * Makes an expression based on a symbol with given value.
	 */
	public static Symbol makeSymbol(Object object) {
		return DefaultSymbol.createSymbol(object);
//		return ExpressionOnSyntaxLeaf.createSymbol(object);
	}
	
	static private Parser parser = new AntlrGrinderParserWrapper();

	/**
	 * Parse a string into an expression using {@link AntlrGrinderParserWrapper}.
	 */
	public static Expression parse(String string) {
		Expression result = parser.parse(string);
		return result;
	}
	
	/**
	 * If argument is a "kleene list" application, returns a {@link List} of its arguments;
	 * otherwise, returns a {@link List} containing this argument.
	 */
	public
	static List<Expression> ensureListFromKleeneList(Expression listOrSingleElementOfList) {
		boolean isKleeneList = listOrSingleElementOfList != null && listOrSingleElementOfList.hasFunctor("kleene list");
		return (isKleeneList ? listOrSingleElementOfList.getArguments() : Lists.newArrayList(listOrSingleElementOfList));
	}

	/**
	 * Returns a "kleene list" application if given list of expressions is not singleton,
	 * and the single element itself otherwise.
	 * This is a inverse operation of {@ #ensureList(Expression)}.
	 */
	public static Expression makeKleeneListIfNeeded(List<Expression> list) {
		if (list.size() == 1) {
			return list.get(0);
		}
		return Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("kleene list", list);
	}

	/**
	 * Given a symbol assumed to be an identifier,
	 * returns a symbol with a minimum 0 or more prime ("'") characters appended to it
	 * to make it unique, according to a given predicate indicating uniqueness.
	 */
	public static Expression primedUntilUnique(Expression symbol, Predicate<Expression> isUnique) {
		while (! isUnique.apply(symbol)) {
			symbol = Expressions.makeSymbol(symbol + "'");
		}
		return symbol;
	}
	
	/**
	 * Given a symbol assumed to be an identifier,
	 * returns a symbol with a minimum 0 or more instances of a given prefix 
	 * to make it unique, according to a given predicate indicating uniqueness.
	 */
	public static Expression prefixedUntilUnique(Expression symbol, String prefix, Predicate<Expression> isUnique) {
		while (! isUnique.apply(symbol)) {
			symbol = Expressions.makeSymbol(prefix + symbol);
		}
		return symbol;
	}
	
	/**
	 * Given a symbol assumed to be an identifier,
	 * returns a symbol with a minimum 0 or more prime ("'") characters appended to it
	 * to make it unique in a given expression.
	 */
	public static Expression primedUntilUnique(Expression symbol, Expression expression, RewritingProcess process) {
		LinkedHashSet<Expression> variables = Expressions.getVariables(expression, process.getIsConstantPredicate());
		Predicate<Expression> isUnique = new NotContainedBy<Expression>(variables);
		Expression result = Expressions.primedUntilUnique(symbol, isUnique);
		return result;
	}

	/**
	 * Given a proposed name for a variable ensure that a valid unique variable
	 * is returned.
	 * 
	 * @param variableName
	 *            a proposed name for the variable.
	 * @param expression
	 *            an expression to check for other variables in order to ensure
	 *            uniqueness.
	 * @param process
	 *            the rewriting process that the variable is being created in.
	 * @return a uniquely named variable in the context of the passed in
	 *         expression.
	 */
	public static Expression makeUniqueVariable(
			String variableName, Expression expression, RewritingProcess process) {
		// Variables have a leading captial
		if (variableName.length() > 0) {
			String leadingChar = variableName.substring(0, 1);
			if (!leadingChar.equals(leadingChar.toUpperCase())) {
				variableName = leadingChar.toUpperCase() + variableName.substring(1);
			}
		} 
		else {
			variableName = "V";
		}

		Expression result = primedUntilUnique(Expressions.makeSymbol(variableName), expression, process);
		return result;
	}
	
	/**
	 * Given a collection of expressions <code>c</code>,
	 * returns a map with the an entry for each expression <code>e</code> in <code>c</code>,
	 * defined as follows:
	 * <code>(key, value)</code>, if <code>e</code> is a binary application of <code>functor</code> on <code>(key, value)</code>,
	 * and <code>(e, defaultValueGivenKey.evaluate(e))</code> otherwise.
	 * This was originally motivated by the problem of mapping collections such as <code>X in Integer, Y, Z in Real</code>
	 * to something like <code>X -> Integer, Y -> type(Y), Z -> Real</code>.
	 */
	public static LinkedHashMap<Expression, Expression> getRelationalMap(Collection<Expression> expressions, Expression functor, Function<Expression, Expression> defaultValueGivenKey) {
		LinkedHashMap<Expression, Expression> result = new LinkedHashMap<Expression, Expression>();
		for (Expression expression : expressions) {
			Expression key;
			Expression value;
			if (expression.hasFunctor(functor)) {
				key = expression.get(0);
				value = expression.get(1);
			}
			else {
				key   = expression;
				value = defaultValueGivenKey.apply(key);
			}
			result.put(key, value);
		}
		return result;
	}

	/** Indicates whether an expression is a Symbol representing a numeric constant. */
	public static boolean isNumber(Expression expression) {
		boolean result = expression.getSyntacticFormType().equals("Symbol") &&
				((Symbol) expression).getValue() instanceof Number;
		return result;
	}
	
	/** Assumes the expression is a Symbol and returns its value as Number. */
	public static Number asNumber(Expression expression) {
		return (Number) ((SyntaxLeaf) expression.getSyntaxTree()).getValue();
	}

	/** Gets an object and returns it if it is an expression, or an atomic expression containing it as value. */
	public static Expression wrap(Object object) {
		if (object == null || object instanceof Expression) {
			return (Expression) object;
		}
		return makeSymbol(object);
	}

	/** The array version of {@link #wrap(Object)}. */
	public static List<Expression> wrap(Object[] array) {
		LinkedList<Expression> result = new LinkedList<Expression>();
		for (int i = 0; i!= array.length; i++) {
			Expression wrap = wrap(array[i]);
			result.add(wrap);
		}
		return result;
	}
	
	/** A version of {@link #wrap(Object)} getting an iterator and returning a list. */
	public static List<Expression> wrap(Iterator<Object> iterator) {
		List<Expression> result = Util.listFrom(new FunctionIterator<Object, Expression>(iterator, WRAPPER));
		return result;
	}

	/** A {@link Function} version of {@link #wrap(Object)}. */
	public static final Function<Object, Expression> WRAPPER = new Function<Object, Expression>() {
		@Override
		public Expression apply(Object object) {
			return wrap(object);
		}
	};
	
	public static Map<Expression, Expression> wrapAsMap(Object... pairs) {
		return Util.map(Expressions.wrap(pairs).toArray());
	}

	public static boolean isSubExpressionOf(Expression searched, Expression expression) {
		boolean result = Util.thereExists(new SubExpressionsDepthFirstIterator(expression), new Equals<Expression>(searched));
		return result;
	}

	/**
	 * Indicates whether any of <code>expressions</code> is a sub-expression of <code>expression</code>.
	 */
	public static boolean containsAnyOfGivenCollectionAsSubExpression(Expression expression, Collection<Expression> expressions) {
		return Util.thereExists(expressions, new IsSubExpressionOf(expression));
	}


	/**
	 * Tests whether the functor of a given (possibly null) expression equals a given object.
	 */
	public static boolean hasFunctor(Expression expression, Object functor) {
		boolean result = expression != null && expression.hasFunctor(functor);
		return result;
	}

	public static Expression addExpressionToArgumentsOfFunctionApplication(Expression expression, Object newArgument) {
		ArrayList<Expression> newArguments = new ArrayList<Expression>(expression.getArguments());
		newArguments.add(wrap(newArgument));
		Expression result = Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees(expression.getFunctor(), newArguments);
		return result;
	}

	public static Expression apply(Object functor, Object... arguments) {
		Expression result = makeExpressionOnSyntaxTreeWithLabelAndSubTrees(functor, arguments);
		return result;
	}

	/**
	 * Indicates whether this expression is a symbol, which is the same thing as a function application with no arguments.
	 */
	public static boolean isSymbol(Expression expression) {
		return expression.numberOfArguments() == 0;
	}
	
	public static boolean isSymbolOrFunctionApplication(Expression expression) {
		boolean result =
			expression.getSyntacticFormType().equals("Function application") ||
			expression.getSyntacticFormType().equals("Symbol");
		return result;
	}

	public static boolean isFunctionApplicationWithArguments(Expression expression) {
		boolean result =
			expression.getSyntacticFormType().equals("Function application") &&
			expression.numberOfArguments() > 0;
		return result;
	}
	
	public static final Function<Expression, Expression> GET_FUNCTOR_OR_SYMBOL = new Function<Expression, Expression>() {

		@Override
		public Expression apply(Expression object) {
			Expression expression = object;
			return expression.getFunctorOrSymbol();
		}
	};
	
	/**
	 * Returns a copy of an expressions with its arguments replaced by new arguments,
	 * with same expression returned if the new arguments turn out to be the same
	 * objects as the current arguments.
	 */
	public static Expression replaceArguments(Expression expression, List<Expression> newArguments) {
		Iterator<Expression> newArgumentsIterator = newArguments.iterator();
		for (int i = 0; i != expression.numberOfArguments(); i++) {
			Expression newIthArgument = newArgumentsIterator.next();
			expression.set(i, newIthArgument);
		}
		return expression;
	}

	public static boolean isBooleanOperatorApplication(Expression condition) {
		boolean result = condition.getFunctor() != null &&
				FunctorConstants.BOOLEAN_FUNCTORS.contains(condition.getFunctor().toString());
		return result;
	}

	/**
	 * This method decomposes an expression into a pair of conjuncts,
	 * the first of them a formula on equalities (or disequalities) on atomic symbols,
	 * (ideally the largest possible such formula),
	 * and the second of them another formula,
	 * such that their conjunction is equivalent to the original condition.
	 */
	public static Pair<Expression, Expression> separateEqualityFormulasOnAtomicSymbolsFromRest(
			Expression expression, RewritingProcess process) {
		
		// deal with conjunctions first because we can separate their conjuncts into equality formulas and non-equality formulas. 
		if (expression.hasFunctor("and")) {
			List<Expression> subExpressions = Util.listFrom(expression.getImmediateSubExpressionsIterator());
			subExpressions.remove(0); // we don't process the functor.
			List<Pair<Expression, Expression>> splits = Util.mapIntoList(subExpressions, new SeparateEqualityFormulasOnAtomicSymbolsFromRest(process));
			List<Expression> constraintParts    = Util.mapIntoList(splits, new GetFirstOfPair<Pair<Expression, Expression>, Expression>());
			List<Expression> nonConstraintParts = Util.mapIntoList(splits, new GetSecondOfPair<Pair<Expression, Expression>, Expression>());
	
			Pair<Expression, Expression> trivialResult =
				Expressions.checkForTrivialResult(expression, constraintParts, nonConstraintParts, process);
			if (trivialResult != null) {
				return trivialResult;
			}
	
			Expression equalityFormulaOnAtomicSymbols    = And.make(constraintParts);
			Expression nonEqualityFormulaOnAtomicSymbols = And.make(nonConstraintParts);
			Pair<Expression, Expression> result =
				new Pair<Expression, Expression>(
						equalityFormulaOnAtomicSymbols, nonEqualityFormulaOnAtomicSymbols);
			return result;
		}
		else if (isEqualityFormulaOnAtomicSymbols(expression)) {
			Pair<Expression, Expression> result =
				new Pair<Expression, Expression>(expression, Expressions.TRUE);
			return result;
		}
		else {
			return new Pair<Expression, Expression>(TRUE, expression); // we don't know how to separate them, so we don't.
		}
	}
	
	final private static class SeparateEqualityFormulasOnAtomicSymbolsFromRest implements Function<Expression, Pair<Expression, Expression>> {
		private RewritingProcess process;

		public SeparateEqualityFormulasOnAtomicSymbolsFromRest(RewritingProcess process) {
			super();
			this.process = process;
		}

		@Override
		public Pair<Expression, Expression> apply(Expression expression) {
			return separateEqualityFormulasOnAtomicSymbolsFromRest(expression, process);
		}
	};

	public static Pair<Expression, Expression> checkForTrivialResult(
			Expression condition, List<Expression> equalityFormulaOnAtomicSymbols,
			List<Expression> nonEqualityFormulasOnAtomicSymbols,
			RewritingProcess process) {
		
		Pair<Expression, Expression> trivialResult = null;
		Equals<Expression> equalsTrue = new Equals<Expression>(TRUE);
		boolean allAreConstraintParts = Util.forAll(nonEqualityFormulasOnAtomicSymbols, equalsTrue);
		if (allAreConstraintParts) {
			trivialResult = new Pair<Expression, Expression>(condition, TRUE);
		}
		boolean allAreNonConstraintParts = Util.forAll(equalityFormulaOnAtomicSymbols, equalsTrue);
		if (allAreNonConstraintParts) {
			trivialResult = new Pair<Expression, Expression>(TRUE, condition);
		}
		return trivialResult;
	}

	public static boolean isEqualityFormulaOnAtomicSymbols(Expression expression) {
		 if ( ! expression.getSyntacticFormType().equals("Function application")) {
			 return false;
		 }
		 if (expression.hasFunctor("=") || expression.hasFunctor("!=")) {
			 return true;
		 }
		 if (FunctorConstants.BOOLEAN_FUNCTORS.contains(expression.getFunctor().toString())) {
			 boolean result = Util.forAll(expression.getArguments(), isEqualityFormulaOnAtomicSymbols);
			 return result;
		 }
		 return false;
	}
	
	private static Predicate<Expression> isEqualityFormulaOnAtomicSymbols = new Predicate<Expression>() {
		@Override
		public boolean apply(Expression expression) {
			return isEqualityFormulaOnAtomicSymbols(expression);
		}
	};
	
	/**
	 * Replaces all numeric symbols in expressions by  a rounded value according to a precision (a number of significant digits to be kept). 
	 */
	public static Expression roundToAGivenPrecision(Expression expression, final int precision, RewritingProcess process) {
		Function<Expression, Expression> rounder = new Function<Expression, Expression>() {
			
			@Override
			public Expression apply(Expression expression) {
				Expression result = round(expression, precision);
				return result;
			}
		};
		
		Expression result = expression.replaceAllOccurrences(rounder, process);
		return result;
	}

	/**
	 * Takes an expression and, if it is numeric symbol,
	 * replaces it by a rounded value according to a precision (a number of significant digits to be kept); 
	 * otherwise, return the expression itself.
	 */
	public  static Expression round(Expression expression, int precision) {
		if (isNumber(expression)) {
			Rational value = expression.rationalValue();
			String rounded = "";
			if (value.isInteger()) {
				rounded = value.toString();
			} 
			else {
				rounded = value.toStringDotRelative(precision);
			}
			return Expressions.makeSymbol(rounded);
		}
		return expression;
	}

	public
	static Iterator<ExpressionAndContext> getSubExpressionsAndContextsIteratorFromImmediateSubTrees(
			Expression expression, RewritingProcess process) {
		if (expression == null) {
			List<ExpressionAndContext> emptyList = Collections.emptyList();
			return emptyList.iterator();
		}
	
		Iterator<? extends SyntaxTree> subTreesIterator = expression.getSyntaxTree().getImmediateSubTreesIterator();
		FunctionIterator<Integer, List<Integer>> pathsIterator = Expressions.makeSingleIndexPathsIteratorFromTo(0, expression.getSyntaxTree().numberOfImmediateSubTrees());
		
		Iterator<ExpressionAndContext> result =
				Expressions.makeIteratorOfSubExpressionsAndContextsFromIteratorsOnSubTreesAndPathsWithGivenQuantifiedVariables(
						subTreesIterator, pathsIterator, process);
	
		return result;
	}

	public
	static Iterator<ExpressionAndContext> getSubExpressionsAndContextsIteratorFromImmediateSubTreesIncludingRootOne(
			Expression expression, RewritingProcess process) {
		if (expression == null) {
			List<ExpressionAndContext> emptyList = Collections.emptyList();
			return emptyList.iterator();
		}
	
		Iterator<? extends SyntaxTree> subTreesIterator = expression.getSyntaxTree().getImmediateSubTreesIncludingRootOneIterator();
		// does need to be sub tree
		FunctionIterator<Integer, List<Integer>> pathsIterator = Expressions.makeSingleIndexPathsIterator(expression);
		
		Iterator<ExpressionAndContext> result = Expressions.makeIteratorOfSubExpressionsAndContextsFromIteratorsOnSubTreesAndPathsWithGivenQuantifiedVariables(
				subTreesIterator, pathsIterator, process);
	
		return result;
	}

	/**
	 * A static method returning selected sub-expressions
	 * in a given expressions, according to a given predicate.
	 */
	public static LinkedHashSet<Expression> getSubExpressionsSatisfying(Expression expression, Predicate<Expression> predicate) {
		LinkedHashSet<Expression> results = new LinkedHashSet<Expression>();
		Util.collect(new SubExpressionsDepthFirstIterator(expression), results, predicate);
		return results;
	}

	/**
	 * Makes an iterator of sub-expressions and their contexts built from two iterators,
	 * one on the sub-trees and another on their paths, with no index expressions.
	 * The quantified variables of the sub expressions will be the one provided as argument.
	 */
	public
	static Iterator<ExpressionAndContext> makeIteratorOfSubExpressionsAndContextsFromIteratorsOnSubTreesAndPathsWithGivenQuantifiedVariables(
			Iterator<? extends SyntaxTree> subTreesIterator, 
			FunctionIterator<Integer, List<Integer>> pathsIterator,
			RewritingProcess process) {
		
		Iterator<ExpressionAndContext> result =
			new FunctionIterator<List<Object>, ExpressionAndContext>(
					new ZipIterator(subTreesIterator, pathsIterator),
					new DefaultExpressionAndContext.MakerFromSyntaxTreeAndPathList(Collections.<Expression> emptyList()));
	
		return result;
	}

	public static FunctionIterator<Integer, List<Integer>> makeSingleIndexPathsIterator(Expression expression) {
		int start = -1;
		int end = expression.getSyntaxTree().numberOfImmediateSubTreesIncludingRootOneIterator() - 1;
		FunctionIterator<Integer, List<Integer>> result = Expressions.makeSingleIndexPathsIteratorFromTo(start, end);
		return result;
	}

	/**
	 * Makes iterator over paths with a single index, starting from <code>start</code> to <code>end</code> exclusive.
	 */
	public static FunctionIterator<Integer, List<Integer>> makeSingleIndexPathsIteratorFromTo(int start, int end) {
		IntegerIterator integerIterator = new IntegerIterator(start, end);
		FunctionIterator<Integer, List<Integer>> result = new FunctionIterator<Integer, List<Integer>>(integerIterator, INTEGER_SINGLETON_LIST_MAKER);
		return result;
	}

	/**
	 * Takes a formula and, if it is a conjunction, returns the list of conjuncts and,
	 * if it is not, returns the formula itself (thus considering it a 1-conjunct conjunction).
	 */
	public static List<Expression> takeFormulaAsConjunctionAndReturnConjuncts(Expression expression) {
		if (And.isConjunction(expression)) {
			return expression.getArguments();
		}
		else {
			return Util.list(expression);
		}
	}

	/**
	 * Returns a list of applications of a given functor to the corresponding elements in two lists
	 * (two elements are correspondent if they have the same indices).
	 */
	public static List<Expression> makePairwiseApplications(Object functor, List<Expression> list1, List<Expression> list2) {
		if (list1.size() != list2.size()) {
			throw new Error("Expressions.makePairwiseApplications(Object, List<Expression>, List<Expression>) expects two lists of same size.");
		}
		List<Expression> result = new LinkedList<Expression>();
		for (int i = 0; i != list1.size(); i++) {
			result.add(Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees(functor, list1.get(i), list2.get(i)));
		}
		return result;
	}

	/**
	 * A small class for gathering the information regarding an index bound to some value in a given condition
	 * @author braz
	 */
	public static class BoundIndexInformation {
		/** The bound index */
		public Expression index;
		
		/** One of the values the index is bound to. */
		public Expression value;
		
		/** The index expressions minus the one on the bound index. */
		public List<Expression> indexExpressionsWithoutBoundIndex;
	}

	/**
	 * Returns a {@link BoundIndexInformation} object for the first index (among those defined in indexExpressions)
	 * bound in a given formula.
	 */
	public static BoundIndexInformation getBoundIndexInformation(Expression formula, List<Expression> indexExpressions) {
		BoundIndexInformation result = null;
		for (Expression conjunct : And.getConjuncts(formula)) {
			if (Equality.isEquality(conjunct)) {
				Collection<Expression> indexOrNothing = Util.list();
				Collection<Expression> remaining = Util.list();
		
				Util.collectFirstN(
						Equality.getSymbolsBoundToSomethingElse(conjunct),
						1,
						new IndexExpressions.IsIndexIn(indexExpressions),
						indexOrNothing,
						remaining);
				
				if (indexOrNothing.size() == 1) {
					result = new BoundIndexInformation();
					result.index = Util.getFirst(indexOrNothing);
					result.value = Util.getFirst(remaining);
					result.indexExpressionsWithoutBoundIndex =
							Util.removeNonDestructively(
									indexExpressions, new IndexExpressions.HasIndex(result.index));
					break;
				}
			}
		}
		return result;
	}

	/**
	 * Return a function application identical to the one received but for the i-th argument being removed.
	 */
	public static Expression removeIthArgument(Expression expression, int i) {
		List<Expression> newArguments = Util.removeNonDestructively(expression.getArguments(), i);
		Expression result = Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees(expression.getFunctor(), newArguments);
		return result;
	}

	/**
	 * A static method returning the variables
	 * in a given expression, for a certain predicate indicating constants.
	 */
	public static LinkedHashSet<Expression> getVariables(Expression argument, Predicate<Expression> isConstantPredicate) {
		return Expressions.getSubExpressionsSatisfying(argument, new IsVariable(isConstantPredicate));
	}

	/**
	 * A static method returning the variables
	 * in a given expression, in a certain process.
	 */
	public static LinkedHashSet<Expression> getVariables(Expression argument, RewritingProcess process) {
		return getVariables(argument, process.getIsConstantPredicate());
	}

	/** Returns the set of free variables in an expression, according to a given process. */
	public static Set<Expression> freeVariables(Expression expression, RewritingProcess process) {
		Set<Expression> freeVariables       = new LinkedHashSet<Expression>(); 
		Set<Expression> quantifiedVariables = new LinkedHashSet<Expression>();
		
		Expressions.freeVariables(expression, freeVariables, quantifiedVariables, process);
		
		return freeVariables;
	}

	/** Returns the set of free symbols in an expression, according to a given process. */
	public static Set<Expression> freeSymbols(Expression expression, RewritingProcess process) {
		Set<Expression> freeSymbols = new LinkedHashSet<Expression>(); 
		Stack<Expression> quantifiedSymbols = new Stack<Expression>();
		
		Expressions.freeSymbols(expression, freeSymbols, quantifiedSymbols, process);
		
		return freeSymbols;
	}

	//
	// PRIVATE METHODS
	//
	private static void freeVariables(Expression expression, Set<Expression> freeVariables, Set<Expression> quantifiedVariables, RewritingProcess process) {
		// Note: this used to be duplicating Expression.replace a bit, although in a lighter-weight, more efficient manner.
		// However, since the changes that include a check against unregistered variables during contextual expansion
		// (that is, constraints expanding the contextual expansion cannot contains variables that are not already in the contextual symbols),
		// this method became more fundamentally distinct since Expression.replace uses contextual expansion and therefore these checks,
		// while this method here does not perform such checks.
		
		if (expression.getSyntacticFormType().equals("Symbol")) {
			if (process.isVariable(expression)) {
				if (!quantifiedVariables.contains(expression)) {
					freeVariables.add(expression);
				}
			}
		} 
		else {
			Iterator<ExpressionAndContext> subExpressionAndContextsIterator = expression.getImmediateSubExpressionsAndContextsIterator();
			
			Set<Expression> newLocalQuantifiedVariables = null;
			while (subExpressionAndContextsIterator.hasNext()) {
				ExpressionAndContext subExpressionAndContext = subExpressionAndContextsIterator.next();
				
				// initialize newLocalQuantifiedVariables with an empty collection
				if (newLocalQuantifiedVariables == null) {
					// For efficiency, only instantiate once
					newLocalQuantifiedVariables = new LinkedHashSet<Expression>();
				}
				else {
					newLocalQuantifiedVariables.clear();
				}
				
				for (Expression localVariable : subExpressionAndContext.getIndices()) {
					if (quantifiedVariables.add(localVariable)) {
						newLocalQuantifiedVariables.add(localVariable);
					}
				}
	
				freeVariables(subExpressionAndContext.getExpression(), freeVariables, quantifiedVariables, process);
				
				// Backtrack to what quantifiedVariables was at the beginning of this call; perhaps it would be more efficient to keep this on a stack?
				quantifiedVariables.removeAll(newLocalQuantifiedVariables);
			}
		}
	
		return;
	}

	private static void freeSymbols(Expression expression, Set<Expression> freeSymbols, Stack<Expression> quantifiedSymbols, RewritingProcess process) {
		
		if (expression.getSyntacticFormType().equals("Symbol")) {
			if (!quantifiedSymbols.contains(expression)) {
				freeSymbols.add(expression);
			}
		} 
		else {
			Iterator<ExpressionAndContext> subExpressionAndContextsIterator = expression.getImmediateSubExpressionsAndContextsIterator();
			
			while (subExpressionAndContextsIterator.hasNext()) {
				ExpressionAndContext subExpressionAndContext = subExpressionAndContextsIterator.next();
				
				List<Expression> indexExpressions = subExpressionAndContext.getIndexExpressions();
				List<Expression> newQuantifiedSymbols = Util.mapIntoList(indexExpressions, IndexExpressions.GET_INDEX);
				int numberOfPushed = Util.pushAll(quantifiedSymbols, newQuantifiedSymbols);
				
				freeSymbols(subExpressionAndContext.getExpression(), freeSymbols, quantifiedSymbols, process);
				
				Util.popAll(quantifiedSymbols, numberOfPushed);
			}
		}
	
		return;
	}

	public static Expression opposite(Expression booleanConstant) {
		Expression result;
		if (booleanConstant.equals(TRUE)){
			result = FALSE;
		}
		else if (booleanConstant.equals(FALSE)){
			result = TRUE;
		}
		else {
			throw new Error("Expressions.opposite should take a boolean constant but got " + booleanConstant);
		}
		return result;
	}

	/**
	 * Determine if all expressions given occur as free variables in another given expression.
	 */
	public static boolean expressionsDoNotOccurInAnotherExpressionAsFreeVariables(List<Expression> expressions, Expression anotherExpression, RewritingProcess process) {
		boolean result = true;
		
		Set<Expression> freeVariables = freeVariables(anotherExpression, process);
		for (Expression index: expressions) {
			if (freeVariables.contains(index)) {
				result = false;
				break;
			}
		}
		return result;
	}

	/** Given an expression, returns the number of top applications of a given (unary) functor and the underlying argument. */
	public static Pair<Integer, Expression> getNumberOfConsecutiveApplicationsOfUnaryFunctorAndUnderlyingArgument(Expression expression, Expression unaryFunctor) {
		int i = 0;
		while (expression.hasFunctor(unaryFunctor)) {
			i++;
			expression = expression.get(0);
		}
		Pair<Integer, Expression> result = Pair.make(i, expression);
		return result;
	}

	//
	public static final PruningPredicate TRUE_PRUNING_PREDICATE = new PruningPredicate() {
		@Override
		public boolean apply(Expression o1, Function<Expression, Expression> replacementFunction, RewritingProcess o2) {
			return true;
		}
	};

	public static RewritingProcess getProcess() {
		return DefaultRewritingProcess.getGlobalRewritingProcessForKnowledgeBasedExpressions();
	}

	public static List<Expression> makeListOfExpressions(List<SyntaxTree> syntaxTrees) {
		List<Expression> result = Util.mapIntoArrayList(syntaxTrees, SYNTAX_TREE_TO_EXPRESSION);
		return result;
	}

	public static Function<Object, Object> MAKE_SURE_IT_IS_SYNTAX_TREE_OR_NON_EXPRESSION_OBJECT = new Function<Object, Object>() {
		@Override
		public Object apply(Object input) {
			return Expressions.makeSureItIsSyntaxTreeOrNonExpressionObject(input);
		}
	};

	public static Object[] makeSureItIsSyntaxTreeOrNonExpressionObject(Object[] input) {
		Object[] result =
				Util
				.mapIntoArrayList(Arrays.asList(input), MAKE_SURE_IT_IS_SYNTAX_TREE_OR_NON_EXPRESSION_OBJECT)
				.toArray();
		return result;
	}

	public static <T> List<Object> makeSureItIsSyntaxTreeOrNonExpressionObject(List<T> list) {
		List<Object> result =
				Util
				.mapIntoArrayList(list, MAKE_SURE_IT_IS_SYNTAX_TREE_OR_NON_EXPRESSION_OBJECT);
		return result;
	}

	public static Object makeSureItIsSyntaxTreeOrNonExpressionObject(Object input) {
		if (input instanceof Expression) {
			input = ((Expression)input).getSyntaxTree();
		}
		return input;
	}

	/**
	 * Given a function <code>F</code> and a function application <code>f(a1, ..., an)</code>,
	 * returns <code>f(F(a1), ..., F(an))</code>.
	 */
	public static Expression passThroughFunctionApplication(Function<Expression, Expression> function, Expression expression) {
		Expression       result;
		Expression       functor            = expression.getFunctor();
		List<Expression> arguments          = expression.getArguments();
		List<Expression> resultingArguments = Util.mapIntoArrayList(arguments, function);
		if (Util.sameInstancesInSameIterableOrder(arguments, resultingArguments)) {
			result = expression;
		}
		else {
			result = apply(functor, resultingArguments);
		}
		return result;
	}

	/**
	 * Given a rewriter <code>R</code> and a function application <code>f(a1, ..., an)</code>,
	 * returns <code>f(R(a1), ..., R(an))</code>.
	 */
	public static Expression passThroughFunctionApplication(Rewriter rewriter, Expression expression, RewritingProcess process) {
		RewriterFunction thisRewriterFunction = new RewriterFunction(rewriter, process);
		Expression result = passThroughFunctionApplication(thisRewriterFunction, expression);
		return result;
	}

	/**
	 * Given a rewriter <code>R</code> and a list <code>(a1, ..., an)</code>,
	 * returns <code>(R(a1), ..., R(an))</code>.
	 */
	public static List<Expression> passThrough(Rewriter rewriter, List<Expression> expressions, RewritingProcess process) {
		RewriterFunction thisRewriterFunction = new RewriterFunction(rewriter, process);
		List<Expression> rewrittenExpressions = Util.mapIntoArrayList(expressions, thisRewriterFunction);
		return rewrittenExpressions;
	}

	public static final Function<Expression, Expression> IDENTITY_FUNCTION = new Function<Expression, Expression>() {
		@Override
		public Expression apply(Expression expression) {
			return expression;
		}
	};

	public static final Predicate<Expression> TRUE_PREDICATE = new Predicate<Expression>() {
		@Override
		public boolean apply(Expression input) {
			return true;
		}
	};

	/**
	 * If expression is a function application of given functor, return a new one with same arguments and new argument given.
	 * If it is not a function application, create a function application with given functor, the given expression and the new argument.
	 * This is useful for keeping, for example, a disjuction
	 * (X = b or X = c, or, X = a) -> X = b or X = c or X = a 
	 * (X = b and X = c, or, X = a) -> (X = b and X = c) or X = a 
	 * (true, or, X = a) -> true or X = a 
	 */
	public static Expression addExpressionToArgumentsOfFunctionApplicationOrCreateIt(Expression expression, String functor, Expression newArgument) {
		Expression result;
		if (expression.hasFunctor(functor)) {
			result = addExpressionToArgumentsOfFunctionApplication(expression, newArgument);
		}
		else {
			result = Expressions.apply(functor, expression, newArgument);
		}
		return result;
	}


	/**
	 * Replaces immediate subexpressions by versions provided by a replacement function, conserving original instance if
	 * function returns original subexpressions.
	 */
	public static Expression replaceImmediateSubexpressions(Expression expression, Function<Expression, Expression> replacementFunction) {
		Iterator<ExpressionAndContext> expressionAndContextIterator = expression.getImmediateSubExpressionsAndContextsIterator();
		while (expressionAndContextIterator.hasNext()) {
			ExpressionAndContext expressionAndContext = expressionAndContextIterator.next();
			Expression newExpression = replacementFunction.apply(expressionAndContext.getExpression());
			Expression result = expressionAndContext.getAddress().replace(expression, newExpression);
			expression = result;
		}
		return expression;
	}
}
