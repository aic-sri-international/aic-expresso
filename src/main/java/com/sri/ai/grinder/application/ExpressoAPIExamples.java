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
package com.sri.ai.grinder.application;

import static com.sri.ai.expresso.helper.Expressions.apply;
import static com.sri.ai.expresso.helper.Expressions.makeSymbol;
import static com.sri.ai.expresso.helper.Expressions.parse;
import static com.sri.ai.grinder.library.FunctorConstants.AND;
import static com.sri.ai.grinder.library.FunctorConstants.IN;
import static com.sri.ai.grinder.library.FunctorConstants.NOT;
import static com.sri.ai.grinder.library.FunctorConstants.OR;
import static com.sri.ai.grinder.library.FunctorConstants.PLUS;
import static com.sri.ai.grinder.library.FunctorConstants.PRODUCT;
import static com.sri.ai.grinder.library.FunctorConstants.SUM;
import static com.sri.ai.grinder.library.FunctorConstants.TIMES;
import static com.sri.ai.util.Util.list;
import static com.sri.ai.util.Util.println;
import static com.sri.ai.util.Util.set;

import java.util.Iterator;
import java.util.Set;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.IndexExpressionsSet;
import com.sri.ai.expresso.api.IntensionalSet;
import com.sri.ai.expresso.api.Type;
import com.sri.ai.expresso.core.DefaultFunctionApplication;
import com.sri.ai.expresso.core.DefaultSymbol;
import com.sri.ai.expresso.core.ExtensionalIndexExpressionsSet;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.grinder.api.Context;
import com.sri.ai.grinder.api.Theory;
import com.sri.ai.grinder.core.TrueContext;
import com.sri.ai.grinder.helper.UniquelyNamedConstantAreAllSymbolsNotIn;
import com.sri.ai.grinder.helper.UniquelyNamedConstantIncludingBooleansAndNumbersPredicate;
import com.sri.ai.grinder.library.Equality;
import com.sri.ai.grinder.library.FunctorConstants;
import com.sri.ai.grinder.library.set.extensional.ExtensionalSets;
import com.sri.ai.grinder.theory.compound.CompoundTheory;
import com.sri.ai.grinder.theory.differencearithmetic.DifferenceArithmeticTheory;
import com.sri.ai.grinder.theory.equality.EqualityTheory;
import com.sri.ai.grinder.theory.linearrealarithmetic.LinearRealArithmeticTheory;
import com.sri.ai.grinder.theory.propositional.PropositionalTheory;
import com.sri.ai.grinder.theory.tuple.TupleTheory;
import com.sri.ai.util.Util;

/**
 * A collection of examples on how to use the Expresso API.
 * @author braz
 *
 */
public class ExpressoAPIExamples {

	public static class Car {
		public static final Car ferrari = new Car("Ferrari");

		private String name;

		public Car(String name) {
			this.name = name;
		}
		
		@Override
		public String toString() {
			return name;
		}
	}
	
	public static void main(String[] args) {
		// Symbols are expressions representing Java values, such as a string,
		// a number, a boolean, and even any other objects (Expressions themselves will throw an error, though, to prevent common mistakes)
		Expression a = DefaultSymbol.createSymbol("a");
		Expression ten = DefaultSymbol.createSymbol(10);
		Expression trueValue = DefaultSymbol.createSymbol(true);
		Expression object = DefaultSymbol.createSymbol(Car.ferrari);
		
		// Below, we use Util.println to avoid having to write System.out.println.
		// Util has lots and lots and lots of very useful methods that eliminate boring Java chores.
		// We definitely recommend studying it carefully!
		// To easily write just "println" and have it work, write "println" and use Ctrl-1 to be offered the option
		// of statically importing Util.println.
		// You can statically import an identifier in Eclipse by placing the cursor on it and pressing Shift-Ctrl-M in Windows.
		println("a        : " + a);
		println("ten      : " + ten);
		println("trueValue: " + trueValue);
		println("object   : " + object);
		
		// It is easier to remember to make symbols using Expressions.makeSymbol:
		// 'Expressions' is a class with lots of useful static methods for working with expressions.
		// This of it as the counterpart of 'Util', but for expressions.
		// Having many useful expression classes in a single place makes it easier to remember them and access them through code completion.
		// As with 'Util', we often import its methods statically.
		a = makeSymbol("a");
		ten = makeSymbol(10);
		trueValue = makeSymbol(true);
		object = makeSymbol(Car.ferrari);
		println("a        : " + a);
		println("ten      : " + ten);
		println("trueValue: " + trueValue);
		println("object   : " + object);

		// The second most important type of expressions are function applications.
		// They consist of a functor (an expression representing a function, typically a symbol) applied to zero or more arguments:
		Expression f = makeSymbol("f");
		Expression g = makeSymbol("g");
		Expression fATen = new DefaultFunctionApplication(f, list(a, ten));
		Expression gOnNothing = new DefaultFunctionApplication(g, list());
		
		println("function f applied to a and ten: " + fATen);
		println("function g applied to nothing: " + gOnNothing);
		
		// It is much easier to use Expressions.apply:
		fATen = apply(f, a, ten);
		gOnNothing = apply(g);
		println("function f applied to a and ten: " + fATen);
		println("function g applied to nothing: " + gOnNothing);
		
		// Naturally, function applications can be applied to any expression, including other function applications:
		Expression gFATen = apply(g, apply(f, a, ten));
		println("function g applied to function f applied to a and ten: " + gFATen);

		// If we do not create symbols for Java values, apply does it automatically for us:
		gFATen = apply("g", apply("f", "a", 10));
		println("function g applied to function f applied to a and ten: " + gFATen);
		
		// Some operators are output in special infix notation for readability:
		Expression twoPlusTwoPlusThree = apply("+", 2, 2, 3); // again not creating symbols first

		println("two plus two plus three: " + twoPlusTwoPlusThree);
		
		Expression arithmetic1 = apply("*", 2, apply("+", 2, 3));
		Expression arithmetic2 = apply("+", 2, apply("*", 2, 3));

		println("Arithmetic gets printed while respecting usual precedence rules by using parentheses: " + arithmetic1);
		println("Arithmetic gets printed while respecting usual precedence rules by using parentheses: " + arithmetic2);
		
		// Same for logic:
		Expression logic1 = apply("and", "p", apply("or", "q", "r"));
		Expression logic2 = apply("or", "p", apply("and", "q", "r"));

		println("Same for logic: " + logic1);
		println("Same for logic: " + logic2);
		
		// It is not good practice to use separate strings for referring to the same operators.
		// In the future, we may decide to change the string associated to an operator
		// and have that string used in many places would make that hard to effect.
		// FunctorConstants is a class with lots of static fields for operator strings:
		arithmetic1 = apply(FunctorConstants.TIMES, 2,  apply(FunctorConstants.PLUS, 2, 3));
		logic1      = apply(FunctorConstants.AND,  "p", apply(FunctorConstants.OR, "q", "r"));

		// Using static imports makes it more readable (in Eclipse, place the cursor on the field/method/constant and press Shift-Ctrl-M):
		arithmetic1 = apply(TIMES, 2,  apply(PLUS, 2, 3));
		logic1      = apply(AND,  "p", apply(OR, "q", "r"));
		
		// We can access the functor and arguments of a function application:
		println("The functor of " + gFATen + " is " + gFATen.getFunctor());
		println("The second argument of " + fATen + " is " + fATen.get(1));
		println("All arguments of " + fATen + " are " + fATen.getArguments()); // returns a List<Expression>
		
		// We can also set new functors or arguments.
		// IMPORTANT: expressions are IMMUTABLE, so this creates a new expression,
		// although it does re-use the unchanged parts.
		Expression newArgument = gFATen.set(0, a);
		println("Changed first argument of " + gFATen + " to " + a + " and obtained " + newArgument);
		println("Original expression continues the same, since they are immutable: " + gFATen);
		
		// getFunctor returns a symbol, so to check if it is, for example, "f", we need
		// to write expression.getFunctor().equals(makeSymbol("f"))
		// which is too long.
		// Instead, we can use 'hasFunctor'
		println(fATen + " has functor \"f\": " + fATen.hasFunctor("f"));
		
		// Finally, we can parse expressions for strings.
		// BUT we should never use that to construct expressions if we have the sub-expressions already represented as Java objects.
		// For example, don't do this: parse("f(" + a + ", " + b + ")");
		// Instead, use apply("f", a, b);
		ten = parse("10");
		trueValue = parse("true");
		fATen = parse("f(a,10)");
		arithmetic1 = parse("2*(2 + 3)");
		arithmetic2 = parse("2+(2 * 3)");
		println(ten);
		println(trueValue);
		println(fATen);
		println(arithmetic1);
		println(arithmetic2);

		// Another important type of expression is sets.
		// There are two dimensions for sets: they can be uni- or multi-sets, and they can be extensionally or intensionally defined.
		//
		// A uni-set has at most one instance of each element in it. This is the typical mathematical set.
		// A multi-set may have multiple instances of the same element in it.
		// For example, the multi-set {1,2,2,3} is distinct from multi-set {1,2,2,2,3}.
		// In Expresso, we use double-brackets for denoting multi-sets:  {{ 1, 2, 2, 3 }}.
		// {{ }} denotes the empty multi-set.
		// The singleton uni-set with an empty set in it is denoted { {} }.
		// You need a space between the brackets to avoid them being parsed as a double bracket.
		//
		// An extensionally defined set is an explicit enumeration of its elements: {1, 2, 3}, {{1, 2, 2, 3}}, {}, {{ }}.
		// An intensionally defined set is defined by a condition: { (on I in Integer)  I^2 : I > 3 and I <= 100 }, for example,
		// which is equal to { 16, 25, 36, ..., 10000 }.
		// The general form of an intensionally defined set (or, less precisely but more succinctly, an intensional set) is
		// { (on Index1 in Index1Domain, Index2 in Index2Domain, ..., Index_n in Index_nDomain)   Head   :  Condition }
		// We can also have intensionally defined multi-sets using double brackets.
		
		// Here are some ways of constructing sets:
		a = makeSymbol("a");
		Expression b = makeSymbol("b");
		Expression c = makeSymbol("c");
		Expression d = makeSymbol("d");
		Expression extensionalUniSet = ExtensionalSets.makeUniSet(a, b, c, d);
		Expression extensionalMultiSet = ExtensionalSets.makeMultiSet(a, b, c, d);
		println(extensionalUniSet);
		println(extensionalMultiSet);
		
		// Creating an intensionally defined set programmatically (as opposed to parsing a string description of it)
		// is a bit of work (this will be shown below).
		// Here's an example of parsing one:
		Expression intensionalUniSet = parse("{ ( on P in People, F in Foods ) eats(P, F) : not (P = Rodrigo and F = shrimp) }");
		println(intensionalUniSet);
		
		// Here's how to do it from scratch, but see next the way we typically actually do it.
		Expression p = makeSymbol("P");
		Expression people = makeSymbol("People");
		f = makeSymbol("F");
		Expression foods = makeSymbol("Foods");
		IndexExpressionsSet indices = new ExtensionalIndexExpressionsSet(apply(IN, p, people), apply(IN, f, foods));
		
		
		
		
		// The "extensional" in ExtensionalIndexExpressionsSet means that the list/set of indices is extensionally defined,
		// even though they will be the indices of an intensionally defined set.
		intensionalUniSet = 
				IntensionalSet.makeUniSet( // IntensionalSet.intensionalUniSet, or simply intensionalUniSet, also works
						indices, 
						apply("eats", p, f), 
						apply(NOT, 
								apply(AND, Equality.make(p, "Rodrigo"), Equality.make(f, "shrimp")))); 
		// Note that Equality.make(p, "Rodrigo") is the same as apply(FunctorConstants.EQUAL, p, "Rodrigo").
		// We often have 'make' methods for many operators: And.make, Or.make and so on.
		// packages in com.sri.ai.expresso.grinder.sgdpllt.library have many such operator-specific classes.
		println(intensionalUniSet);
		
		// When writing code on sets, we typically are modifying an existing set expression, so we can re-use its parts,
		// by using special part-replacement methods.
		// This requires the variable to implement the IntensionalSet interface, though.
		// IMPORTANT: expressions are IMMUTABLE, so setCondition and other part-replacement methods return a NEW expression,
		// although the parts not replaced are re-used.
		IntensionalSet intensionalSetCast = (IntensionalSet) intensionalUniSet;
		Expression noCondition = intensionalSetCast.setCondition(makeSymbol(true));
		println("Set with no condition: " + noCondition);
		
		Expression headSaysLoveInsteadOfEats = intensionalSetCast.setHead(apply("loves", p, f));
		println("Set with new head: " + headSaysLoveInsteadOfEats);
		
		Expression withNewIndices = 
				intensionalSetCast.setIndexExpressions(
						new ExtensionalIndexExpressionsSet(
								apply(IN, p, people), apply(IN, f, foods), apply(IN, "D", "Days")));
		println("Set with new indices: " + withNewIndices);
		
		// summations and products are just function applications of FunctorConstants.SUM and FunctorConstants.PRODUCT on intensional multi-sets.
		// sum( {{ (on Indices)  Head  : Condition }} ) represents the summation (in Latex notation) sum_{Indices : Condition} Head
		Expression summation = apply(SUM, intensionalSetCast);
		println(summation);
		
		Expression product = apply(PRODUCT, intensionalSetCast);
		println(product);
		
		
		///// Evaluating expressions
		
		// The above code shows how to deal with the syntax of expressions.
		// Evaluating expressions requires knowing about the semantics, that is, to what functions each operator corresponds to ("+" to addition, etc).
		// This is provided by a theory, which for now it suffices to know is a collection of methods for evaluating expressions
		// according to an interpretation to some symbols.
		
		Theory theory = new CompoundTheory(
				new EqualityTheory(false, true),
				new DifferenceArithmeticTheory(false, false),
				new LinearRealArithmeticTheory(false, false),
				new TupleTheory(),
				new PropositionalTheory());
		
		// Because this evaluation is symbolic, evaluated expressions may involve free variables.
		// In this case, the result of the evaluation will be a simplified expression that
		// is equivalent to the original expression for all possible assignments to the free variables.
		// For example, X + 0*Y is evaluate to X because, for any assignment to (X,Y), X + 0*Y = X.

		Context context = new TrueContext(theory); // true context: all assignments to free variables are of interest
		// We will later see how we can use contexts that restrict the free variable assignments of interest.
		
		// Now that we have a theory and a context, we can evaluate expressions:
		println("1 + 0*X + 1  =  " + theory.evaluate(parse("1 + 1"), context));
		                   
		evaluate(new String[] {
				"1 + 1", "2",
				"X + 1 + 1", "X + 2",
				"sum({{ (on I in 1..10) I }})", "55",
				"product({{ (on I in 1..5) 2 : I != 3 and I != 5 }})", "8",
				"|{{ (on I in 1..5) X : I != 3 and I != 5 }}|", "3",
				
				// see many more examples in SymbolicShell.java
		}, theory, context);
		
		// now let us assume we have a free variable J which is an integer
		// Contexts are, like expressions, also IMMUTABLE:
		Context context2 = context.extendWithSymbolsAndTypes("J", "Integer");
		// However, here we just want to use the same variable 'context' all along, so we keep the updated context in it:
		context = context2;
		// Because we store the reference to the modified context in the same variable, we lose the reference to the original one,
		// but, if we wanted, we could keep contexts in a stack, for example,
		// so that we could always easily revert back to a previous context if needed.
		evaluate(new String[] {
				"X + 1 + 1 + J", "X + 2 + J",
				"sum({{ (on I in 1..10) I : I != J }})", "if J > 0 then if J <= 10 then -1 * J + 55 else 55 else 55",
				// see many more examples in SymbolicShell.java
		}, theory, context);

		// now let us assume we have a free variable J which is an integer
		// The context is also a boolean formula (a constraint)
		// Current, its value is "true", but we can conjoin it with a literal J < 0
		context = context.conjoin(parse("J < 0"));
		evaluate(new String[] {
				"J < 1", "true",
				"sum({{ (on I in 1..1000) I : I != J }})", "500500", // J is irrelevant because it is out of the range of I
				// see many more examples in SymbolicShell.java
		}, theory, context);

		// we now add another symbol and constraint
		context = context.extendWithSymbolsAndTypes("K", "Integer");
		context = context.conjoin(parse("K > 0"));
		evaluate(new String[] {
				"J < K", "true",
				// see many more examples in SymbolicShell.java
		}, theory, context);
		
		// Obtaining all free variables in an expression.
		// In order to obtain all free variables appearing in an expression
		// (and therefore excluding quantified variables (for all X, there exists X) and set indices ({ (on Z in Real) Z }))
		// we must traverse the expression and select its sub-expressions that are variables.
		// However, we need to know what a variable is.
		// It is not enough to say that any symbol is a variable, because "1" and "true" are symbols, but not variables.
		// It is not enough to say that any symbols that is an identifier (starting with an alphabet letter) is a variable,
		// because we may have uniquely named constants such as "john" and "bob" that are not to be treated as variables
		// (we want "john = bob" to be evaluated to "false", and if they were variable, this would not happen.
		// The way Expresso deals with this question is by letting the user define a predicate in the context that
		// encoded what a uniquely named constant is, and considering any other symbol to be considered a variable.
		
		// By default, Expresso follows the Prolog convention of capitalized variables: X is a variable, x is not.
		// Note how this takes "Real" to be a variable!
		
		context = new TrueContext();
		Expression expression = parse("X + f(g(x, Y, 1, true, false, 10, bob, john, there exists Z in Real : 10, { (on W in Real) 1 } ))");
		Set<Expression> variablesInExpression = Expressions.freeVariables(expression, context);
		println("variables in " + expression + " by Prolog standard: " + variablesInExpression); // outputs [X, Y, Real]
		
		// More recently, we have adopted the practice of not caring about capitalization.
		// This means that we may, for example, define uniquely named constants to be any symbols that are not in a given set of variables.
		Set<Expression> allVariables = set(parse("x"), parse("X"), parse("Y"), parse("Z"), parse("W"));
		context = context.setIsUniquelyNamedConstantPredicate(new UniquelyNamedConstantAreAllSymbolsNotIn(allVariables));
		variablesInExpression = Expressions.freeVariables(expression, context);
		println(
				"variables in " + expression + " if all variables is " + 
						allVariables + ": " + variablesInExpression); // outputs [x, X, Y]
		
		// Sometimes, it is useful to replace subexpressions in a given expression by another subexpression:
		expression = parse("f(f(f(X))) + X");
		Expression valueOfX = parse("10");
		Expression replacementOfFirstOccurrenceOnly = expression.replaceFirstOccurrence(parse("X"), valueOfX, context);
		println("Replacing only the first occurrence of X by its value gives " + replacementOfFirstOccurrenceOnly);
		Expression replacementOfAllOccurrences = expression.replaceAllOccurrences(parse("X"), valueOfX, context);
		println("Replacing all occurrences of X by its value gives " + replacementOfAllOccurrences);
		// There are a LOT of variants of these functions in Expression (not Expressions),
		// including some very flexible ones that allow the user to provide a function for determining the replacement.

		// Here's how to decide if a point is in the convex hull of other two points:
		// BUGGY : doesn't evaluate but it should
		Context convexityBaseContext = new TrueContext(theory);
		convexityBaseContext = convexityBaseContext.setIsUniquelyNamedConstantPredicate(new UniquelyNamedConstantIncludingBooleansAndNumbersPredicate(set()));
		convexityBaseContext = convexityBaseContext.extendWithSymbolsAndTypes("p", "Real", "p1", "Real", "p2", "Real", "X", "Integer");
		Expression pValue;
		Expression p1Value;
		Expression p2Value;
		String examples[] = {
				"4", "3", "5",
				"6", "3", "5",
				// this case is still buggy:
				//"if X = 1 then 2 else 3", "if X = 2 then 1 else 5", "if X = 10 then 0 else 10",
		};
		for (int i = 0; i != examples.length; i += 3) {
			pValue  = parse(examples[i]);
			p1Value = parse(examples[i + 1]);
			p2Value = parse(examples[i + 2]);
			Context convexityContext;
			convexityContext = convexityBaseContext.conjoin(parse("p  = " + pValue));
			convexityContext =     convexityContext.conjoin(parse("p1 = " + p1Value));
			convexityContext =     convexityContext.conjoin(parse("p2 = " + p2Value));
			Expression isInConvexHull = 
					parse("there exists c1 in [0;1] : there exists c2 in [0;1] : c1 + c2 = 1 and p = c1*p1 + c2*p2");
			Expression result = theory.evaluate(isInConvexHull, convexityContext);
			println("p (value " + pValue + ") is in the convex hull of p1 and p2 (" + p1Value + ", " + p2Value + "): " + result);
		}
		// --- HERE, problem with the example above? (should be false on the second case ...)
		
		// Even coding the values directly still results in a bug.
//		Expression isInConvexHull = 
//				parse("there exists c1 in [0;1] : there exists c2 in [0;1] : c1 + c2 = 1 and (if X = 1 then 2 else 3) = c1*(if X = 2 then 1 else 3) + c2*(if X = 10 then 0 else 10)");
//		Expression result = theory.evaluate(isInConvexHull, convexityBaseContext);
//		println("if X = 1 then 2 else 3 is in the convex hull of if X = 2 then 1 else 3 and if X = 10 then 0 else 10: " + result);

		// Obtaining and using types:
		context = new TrueContext();
		context = context.extendWithSymbolsAndTypes("I", "3..8", "P", "Boolean");
		println(context.getSymbolsAndTypes());
		println(context.getMandatoryTypeExpressionOfRegisteredSymbol(parse("I")));
		
		Expression typeExpression = context.getMandatoryTypeExpressionOfRegisteredSymbol(parse("I"));
		Type type = context.getTypeOfRegisteredSymbol(parse("I"));
		Iterator<Expression> iteratorToValuesInType = type.iterator();
		println("All values of the type " + typeExpression + " of I: " + Util.join(iteratorToValuesInType));

		typeExpression = context.getMandatoryTypeExpressionOfRegisteredSymbol(parse("P"));
		type = context.getTypeOfRegisteredSymbol(parse("P"));
		iteratorToValuesInType = type.iterator();
		println("All values of the type " + typeExpression + " of P: " + Util.join(iteratorToValuesInType));
		
	}

	/**
	 * Evaluates a series of tests and prints the results.
	 * @param inputsAndOutputs
	 * @param theory
	 * @param context
	 */
	public static void evaluate(String[] inputsAndOutputs, Theory theory, Context context) {
		for (int i = 0; i != inputsAndOutputs.length/2; i++) {
			String input = inputsAndOutputs[2*i];
			String expected = inputsAndOutputs[2*i + 1];
			Expression output = theory.evaluate(parse(input), context);
			println(input + "  =  " + output);
			if ( ! output.equals(parse(expected))) {
				println("Error: " + input + " should have been evaluated to " + expected + ", but was evaluated to " + output);
			}
		}
	}
}
