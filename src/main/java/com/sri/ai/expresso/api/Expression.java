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
package com.sri.ai.expresso.api;

import java.io.Serializable;
import java.util.Iterator;
import java.util.List;

import com.google.common.annotations.Beta;
import com.google.common.base.Function;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.core.PruningPredicate;
import com.sri.ai.grinder.core.PruningPredicateMaker;
import com.sri.ai.grinder.core.ReplacementFunctionMaker;
import com.sri.ai.util.base.TernaryProcedure;
import com.sri.ai.util.math.Rational;

/**
 * An Expression is an algebraic expression based on a syntax tree, and wrapped up with
 * information so it can be used by rewriters.
 * 
 * @author braz
 */
@Beta
public interface Expression extends Cloneable, Serializable, Comparable<Object> {
	
	/**
	 * Provides the sub-expression of this expression.
	 * They must always be the same objects.
	 */
	Iterator<ExpressionAndContext> getImmediateSubExpressionsAndContextsIterator();
	Expression replace(ExpressionAndContext replacement);
	
	List<Expression> getSubExpressions();
	
	/**
	 * Returns scoped expressions (that is, indices and quantified variables) introduced by this expression
	 * according to given process.
	 */
	List<Expression> getScopedExpressions(RewritingProcess process);
	
	Expression replaceFirstOccurrence(Expression replaced, Expression replacement, RewritingProcess process);
	Expression replaceAllOccurrences(Expression replaced, Expression replacement, RewritingProcess process);
	Expression replaceFirstOccurrence(Expression replaced, Expression replacement, PruningPredicate prunePredicate, RewritingProcess process);
	Expression replaceAllOccurrences(Expression replaced, Expression replacement, PruningPredicate prunePredicate, RewritingProcess process);
	Expression replaceFirstOccurrence(Function<Expression, Expression> replacementFunction, RewritingProcess process);
	Expression replaceAllOccurrences(Function<Expression, Expression> replacementFunction, RewritingProcess process);
	Expression replaceFirstOccurrence(Function<Expression, Expression> replacementFunction, PruningPredicate prunePredicate, RewritingProcess process);
	Expression replaceAllOccurrences(Function<Expression, Expression> replacementFunction, PruningPredicate prunePredicate, RewritingProcess process);
	Expression replaceFirstOccurrence(Function<Expression, Expression> replacementFunction, ReplacementFunctionMaker makeSpecificSubExpressionAndContextReplacementFunction, PruningPredicate prunePredicate, PruningPredicateMaker makeSpecificSubExpressionAndContextPrunePredicate, RewritingProcess process);
	Expression replaceAllOccurrences(Function<Expression, Expression> replacementFunction, ReplacementFunctionMaker makeSpecificSubExpressionAndContextReplacementFunction, PruningPredicate prunePredicate, PruningPredicateMaker makeSpecificSubExpressionAndContextPrunePredicate, RewritingProcess process);
	Expression replaceFirstOccurrence(Function<Expression, Expression> replacementFunction, TernaryProcedure<Expression, Expression, RewritingProcess> listener, RewritingProcess process);
	Expression replaceAllOccurrences(Function<Expression, Expression> replacementFunction, TernaryProcedure<Expression, Expression, RewritingProcess> listener, RewritingProcess process);
	Expression replaceFirstOccurrence(Expression replaced, Expression replacement, TernaryProcedure<Expression, Expression, RewritingProcess> listener, RewritingProcess process);
	Expression replaceAllOccurrences(Expression replaced, Expression replacement, TernaryProcedure<Expression, Expression, RewritingProcess> listener, RewritingProcess process);
	Expression replaceFirstOccurrence(Expression replaced, Expression replacement, PruningPredicate prunePredicate, TernaryProcedure<Expression, Expression, RewritingProcess> listener, RewritingProcess process);
	Expression replaceAllOccurrences(Expression replaced, Expression replacement, PruningPredicate prunePredicate, TernaryProcedure<Expression, Expression, RewritingProcess> listener, RewritingProcess process);
	Expression replaceFirstOccurrence(Function<Expression, Expression> replacementFunction, PruningPredicate prunePredicate, TernaryProcedure<Expression, Expression, RewritingProcess> listener, RewritingProcess process);
	Expression replaceAllOccurrences(Function<Expression, Expression> replacementFunction, PruningPredicate prunePredicate, TernaryProcedure<Expression, Expression, RewritingProcess> listener, RewritingProcess process);
	Expression replace(Function<Expression, Expression> replacementFunction, boolean onlyTheFirstOne, PruningPredicate prunePredicate, boolean ignoreTopExpression, TernaryProcedure<Expression, Expression, RewritingProcess> listener, RewritingProcess process);
	
	/**
	 * Returns the result of replacing one or all sub-expressions of this expression
	 * according to a replacement function.
	 * 
	 * The method works by traversing every sub-expression of this expression (including the expression itself, unless the argument ignoreTopExpression is true) and
	 * calling a replacement function on it.
	 * If the replacement function returns exactly the same expression object, then nothing happens.
	 * If a new expression object is returned by the replacement function, then it replaces the original sub-expression.
	 * Note that, because expression objects are immutable, replacing a sub-expression S by a new sub-expression S'
	 * will necessarily replace its parent P as well by a new version P' that is equal to P but for having S' where S used to be.
	 * The parent's replacement trigger its own parent to be replaced and so on, all the way to the root.
	 * The replacement of parents happens automatically and the user need not worry about making it happen.
	 * 
	 * Note that new sub-expressions replacing old ones are not further examined and left as is. If they contain sub-expressions that would be replaced themselves by the replacement function,
	 * these sub-expressions are not replaced.
	 * For example, if a replacement function replaces expressions "a" by an expression "f(a)", calling this method with this replacement function on "g(a,a)" produces "g(f(a),f(a))", 
	 * with the "a"s in the result not being replaced.
	 * If the user wishes this to happen, she must re-run the function on the result.
	 * 
	 * To this basic functionality the method adds several more detailed options (note the many auxiliary methods that do not require some of these options to be specified, using defaults for them):
	 * 
	 * A prune predicate can be provided that will be invoked on a sub-expression and its context and indicates whether it (and its descendants) should be ignored.
	 * A context is the condition holding for the free variables of an expression (see {@link ExpressionAndContext} for more details),
	 * and is automatically updated as the replace method traverses the sub-expressions.
	 * 
	 * Sometimes we wish the replacement and prune functions to be updated or changed accordingly to the characteristics of the sub-expressions or trees they are being used on.
	 * For this reason, the replace method can take two function arguments, makeSpecificSubExpressionAndContextReplacementFunction and makeSpecificSubExpressionAndContextPrunePredicate,
	 * which take this expression, the replacement function or pruning predicate, and the current sub-expression, and provides a new replacement function or prune predicate to be used
	 * on that sub-expression and its descendants. This is a more advanced and less commonly used feature.
	 * 
	 * The argument onlyTheFirstOne allows us to choose to replace only the first sub-expression for which the replacement function returns a distinct object, or to continue examining all sub-expressions. 
	 * 
	 * We can provide a listener procedure that gets notified of every replacement.
	 * @param makeSpecificSubExpressionAndContextPrunePredicate Takes the current expression, the current replacement function and the sub-expression and its context about to be processed (the top one inclusive), and returns the pruning predicate to be used for that specific sub-expression.
	 * @param replaceOnChildrenBeforeTopExpression indicate whether to replace in sub-expression before replacing top expression.
	 * @param replacementFunction takes a expression and returns a new expression, or itself in case no replacement is warranted. Make it an instance of {@link ReplacementFunctionWithContextuallyUpdatedProcess} if it uses the contextual symbols and variables, so that the process gets properly extended.
	 * @param makeSpecificSubExpressionAndContextReplacementFunction: Takes the current expression, the current replacement function and the sub-expression and its context about to be processed (the top one inclusive), and returns the replacement function to be used for that specific sub-expression.
	 * @param prunePredicate a predicate evaluating as true for sub-expressions that should be pruned (that is, ignored).
	 * @param makeSpecificSubExpressionAndContextPrunePredicate: Takes the current prune predicate and the sub-expression and its context about to be processed (the top one inclusive), and returns the prune predicate to be used for that specific sub-expression.
	 * @param onlyTheFirstOne if true, replaces at most one sub-expression.
	 * @param ignoreTopExpression does not try to replace this expression as a whole; examines sub-expressions only.
	 * @param replaceOnChildrenBeforeTopExpression recurse replacement function on sub-expressions before using it on top expression.
	 * @param listener binary procedure receiving original and replacement expression every time such a replacement occurs. If a sub-expression is replaced, it is invoked for that sub-expression as well as for all its "super-expressions", since they are all being replaced by a new expression.
	 * @param process the rewriting process, used here for defining what is a sub-expression of what.
	 */
	Expression replace(Function<Expression, Expression> replacementFunction,
			           ReplacementFunctionMaker makeSpecificSubExpressionAndContextReplacementFunction, 
			           PruningPredicate prunePredicate,
			           PruningPredicateMaker makeSpecificSubExpressionAndContextPrunePredicate, 
			           boolean onlyTheFirstOne,
			           boolean ignoreTopExpression,
			           boolean replaceOnChildrenBeforeTopExpression,
			           TernaryProcedure<Expression, Expression, RewritingProcess> listener,
			           RewritingProcess process);

	/**
	 * Indicates what syntactic form the expression is.
	 * Syntactic forms are the primitive types of expressions in a logic.
	 * For example, in FOL we have the forms: term, predicate, simple formula, quantified formula, etc.
	 * HOL typically has function applications and lambda expressions as its basic syntactic forms.
	 */
	public Object getSyntacticFormType();

	public SyntaxTree getSyntaxTree();

	public Iterator<Expression> getImmediateSubExpressionsIterator();
	
	/**
	 * Renames all occurrences of a symbol, including when it is declared.
	 * For example, renaming <code>p</code> by <code>q</code> in <code>for all p(X) in People : happy(p(X))</code>
	 * results in <code>for all q(X) in People : happy(q(X))</code>.
	 * @param symbol
	 * @param newSymbol
	 * @param process
	 * @return the result of renaming <code>symbol</code> as <code>newSymbol</code> everywhere in <code>expression</code>.
	 */
	public Expression replaceSymbol(Expression symbol, Expression newSymbol, RewritingProcess process);
	
	///////////////////////// FUNCTION APPLICATION METHODS //////////////////////
	// The following methods are only valid for function applications.
	// They are undefined for other types of expressions, but for error detection purposes it is
	// useful that they return <code>null</code> or throw an exception.
	// Perhaps in the future there will be an extension of Expression for them,
	// in which case these methods would be present only there.
	// Or, instead, because function applications are the most common type of expression,
	// we will leave them here for convenience.
	
	/** Returns the functor if the expression is a function application, or <code>null</code> otherwise. */
	public Expression getFunctor();
	
	/**
	 * Returns the functor if the expression is a function application, or the symbol otherwise.
	 * It assumes that the expression is of one or the other type.
	 */
	public Expression getFunctorOrSymbol();
	
	/** Indicates whether expression is a function application with given functor. */
	public boolean hasFunctor(Object functor);
	
	/**
	 * Returns the arguments of a function application expression if this is one.
	 */
	public List<Expression> getArguments();
	
	/**
	 * Same as {@link #getArguments()}<code>.size()</code>, but potentially more efficient.
	 */
	public int numberOfArguments();
	
	/**
	 * Same as {@link #getArguments()}<code>.get(i)</code>, but potentially more efficient.
	 */
	public Expression get(int i);
	
	/**
	 * If this is a function application,
	 * returns an expression equal to this one, but for replacing the i-th argument by the given one.
	 * Generates an error otherwise.
	 */
	Expression set(int i, Expression newIthArgument);
	
	///////////////////////// SYMBOL METHODS //////////////////////

	/**
	 * Returns the value of an expression if it is a {@link Symbol},
	 * and <code>null</code> otherwise.
	 * The reason it is available at the {@link Expression} is
	 * that is it a very commonly used method and casting makes the code harder to read.
	 */
	public Object getValue();

	/**
	 * Determine if the value of the Symbol is a string literal.
	 *
	 * @return true if the Symbol represents a String literal 
	 * (i.e. parsed with surrounding double quotes,
	 *  quoted symbols use single quotes and are not string literals), false otherwise.
	 */
	boolean isStringLiteral();

	/** 
	 * Returns the value of the symbol as a boolean.
	 * An error is thrown if the value of the symbol is not a boolean.
	 */
	boolean booleanValue();

	/** 
	 * Returns the value of the symbol as an int. This may involve rounding or truncation.
	 * An error is thrown if the value of the symbol is not a number.
	 */
	int intValue();

	/** 
	 * Returns the value of the symbol as a long. This may involve rounding or truncation.
	 * An error is thrown if the value of the symbol is not a number.
	 */
	long longValue();

	/** 
	 * Returns the value of the symbol as an int. This may involve rounding or truncation.
	 * An {@link java.lang.ArithmeticException.ArithmeticException} is thrown if the value of the symbol is not a number OR not an integral value.
	 */
	int intValueExact() throws ArithmeticException;

	/** 
	 * Returns the value of the symbol as a double. This may involve rounding or truncation.
	 * An error is thrown if the value of the symbol is not a number.
	 */
	double doubleValue();

	/** 
	 * Returns the value of the symbol as a Rational.
	 * An error is thrown if the value of the symbol is not a number.
	 */
	Rational rationalValue();
	
	/**
	 * Indicates whether two expressions are syntactically equal, allowing comparisons to Strings as well as a convenience
	 * (this is equivalent to comparing to the {@link Symbol} obtained from the String).
	 * Note that the comparison to String violates the specification of equals, since <code>s.equals(e)</code> for <code>s</code> and <code>e</code>
	 * String and Expression respectively will always return false, even if <code>e.equals(s)</code> returns true.
	 * However, this should not be a problem if Strings and Expressions are not mixed in containers or algorithms relying on equals,
	 * which would not be possible even if the comparison to String convenience were omitted.
	 * TODO: In the future we may replace this convenience by a method specific to comparing to Strings, which would be more explicit.
	 * This will take some work as all invocations of equals on Strings would have to be replaced by this new method.
	 * @param object the object to compare this expression to.
	 * @return whether the two objects are considered equal.
	 */
	@Override
	boolean equals(Object object);
}
