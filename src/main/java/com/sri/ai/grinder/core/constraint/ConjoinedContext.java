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
package com.sri.ai.grinder.core.constraint;

import static com.sri.ai.expresso.helper.Expressions.FALSE;
import static com.sri.ai.expresso.helper.Expressions.contains;
import static com.sri.ai.expresso.helper.Expressions.isBooleanSymbol;
import static com.sri.ai.util.Util.myAssert;
import static com.sri.ai.util.base.Pair.pair;
import static com.sri.ai.util.explanation.logging.api.ThreadExplanationLogger.RESULT;
import static com.sri.ai.util.explanation.logging.api.ThreadExplanationLogger.code;
import static com.sri.ai.util.explanation.logging.api.ThreadExplanationLogger.explain;
import static com.sri.ai.util.explanation.logging.api.ThreadExplanationLogger.explanationBlock;

import java.util.Collection;
import java.util.Map;
import java.util.Set;

import com.google.common.annotations.Beta;
import com.google.common.base.Predicate;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.Type;
import com.sri.ai.grinder.api.Context;
import com.sri.ai.grinder.api.ExpressionLiteralSplitterStepSolver;
import com.sri.ai.grinder.api.SingleVariableConstraint;
import com.sri.ai.grinder.api.Theory;
import com.sri.ai.grinder.core.TrueContext;
import com.sri.ai.grinder.library.boole.And;
import com.sri.ai.util.base.BinaryFunction;
import com.sri.ai.util.base.Pair;

// ISSUES
// Expansion of constraint into expression: too expensive
// Unchecked assumption that head and tail do not share variables

/**
 * An {@link Context} representing a conjunction.
 * 
 * @author braz
 *
 */
@Beta
public class ConjoinedContext extends AbstractConstraint implements Context {

	private static final long serialVersionUID = 1L;
	
	private SingleVariableConstraint head; // constraint on last variable
	private Context tail; // constraint on variables but the last one; works as context for head when checking property

	/**
	 * A {@link BinaryFunction} making a {@link SingleVariableConstraint} for a given
	 * variable and current context.
	 * @author braz
	 *
	 */
	public static
	interface ConjoinedContextPropertyCheckerStepSolverMaker
	extends BinaryFunction<SingleVariableConstraint, Context, ExpressionLiteralSplitterStepSolver> {}
	
	ConjoinedContextPropertyCheckerStepSolverMaker propertyCheckerStepSolverMaker; // a step solver for checking some property on the head
	
	private boolean checked;
	
	/**
	 * Makes a {@link Context} from a literal and a given {@link TrueContext}.
	 * @param literal
	 * @param trueContext
	 * @param propertyCheckerStepSolverMaker
	 * @param theory
	 */
	public static Context conjoinTrueContextWithLiteral(
			Expression literal,
			Theory theory,
			TrueContext trueContext) {
		
		ConjoinedContextPropertyCheckerStepSolverMaker contextDependentProblemStepSolverMaker =
				new CompleteConjoinedContextPropertyCheckerStepSolverMaker(theory);
		Context result = 
				conjoinTrueContextWithLiteral(
						literal, trueContext, contextDependentProblemStepSolverMaker, theory);
		return result;
	}

	/**
	 * Makes a {@link ConjoinedContext} from a literal and a given {@link TrueContext}.
	 * @param literal
	 * @param trueContext
	 * @param contextDependentProblemStepSolverMaker
	 * @param theory
	 */
	public static Context conjoinTrueContextWithLiteral(
			Expression literal,
			TrueContext trueContext,
			ConjoinedContextPropertyCheckerStepSolverMaker contextDependentProblemStepSolverMaker,
			Theory theory) {
		
		Context result = null;
		Collection<Expression> variablesInLiteral = trueContext.getTheory().getVariablesIn(literal, trueContext);
		// TODO: literals should be simplified right when detected, and arguably trivial literals should not be considered literals in the first place
		if (variablesInLiteral.isEmpty()) {
			result = 
					conjoinTrueContextWithLiteralWithoutAnyVariables(literal, trueContext);
		}
		else {
			result = 
					conjoinTrueContextWithLiteralContainingVariables(
							literal, variablesInLiteral, trueContext, contextDependentProblemStepSolverMaker, theory);
		}
		return result;
	}

	private static Context conjoinTrueContextWithLiteralWithoutAnyVariables(Expression literal, TrueContext trueContext) {
		Context result;
		Expression simplifiedLiteral = trueContext.simplify(literal);
		myAssert(isBooleanSymbol(simplifiedLiteral), () -> "Literal without variables " + literal + " did not get simplified to a boolean constant, but to " + simplifiedLiteral + " instead. This is likely due to an incorrect uniquely named constant predicate or theory configuration");
		result = trueContext.conjoin(simplifiedLiteral);
		return result;
	}

	private static Context conjoinTrueContextWithLiteralContainingVariables(
			Expression literal,
			Collection<Expression> variablesInLiteral,
			TrueContext trueContext,
			ConjoinedContextPropertyCheckerStepSolverMaker contextDependentProblemStepSolverMaker,
			Theory theory) {
		
		Context result;
		SingleVariableConstraint head = theory.makeNewSingleVariableConstraintOnSomeVariableOfLiteral(literal, variablesInLiteral, trueContext);
		if (head.isContradiction()) {
			result = trueContext.makeContradiction();
		}
		else {
			result = makeAndCheck(theory, head, trueContext, contextDependentProblemStepSolverMaker, trueContext);
		}
		return result;
	}

	/**
	 * Creates a new {@link Context} from a {@link SingleVariableConstraint} and a {@link Context},
	 * by either returning a contradiction if either is contradictory,
	 * or a new {@link ConjoinedContext} otherwise.
	 * @param theory
	 * @param head
	 * @param tail
	 * @param context
	 * @return
	 */
	private static Context makeAndCheck(
			Theory theory,
			SingleVariableConstraint head,
			Context tail,
			ConjoinedContextPropertyCheckerStepSolverMaker contextDependentProblemStepSolverMaker, 
			Context context) {

		return explanationBlock("Making new MultiVariableContextWithCheckedProperty and checking property", code( () -> {

			Context result;
			if (head.isContradiction() || tail.isContradiction()) {
				result = tail.makeContradiction();
			}
			else {
				result = makeAndCheckOutOfConsistentHeadAndTail(head, tail, contextDependentProblemStepSolverMaker, context);
			}

			return result;

		}), "Result is ", RESULT);
	}

	private static Context makeAndCheckOutOfConsistentHeadAndTail(
			SingleVariableConstraint head, 
			Context tail,
			ConjoinedContextPropertyCheckerStepSolverMaker contextDependentProblemStepSolverMaker, 
			Context context) {
		
		Context result;
		Theory theory = tail.getTheory();
		ConjoinedContext unchecked = new ConjoinedContext(head, tail, contextDependentProblemStepSolverMaker, theory);
		result = unchecked.check(context);
		return result;
	}

	/**
	 * Constructs a {@link ConjoinedContext} from a head and a tail constraints,
	 * which is only correct if the {@link SingleVariableConstraint}'s variable does not appear
	 * in the tail constraint.
	 * Note also that this does not check the checked property.
	 * Because of these choices, the constructor is private.
	 * @param head
	 * @param tail
	 */
	private ConjoinedContext(
			SingleVariableConstraint head,
			Context tail,
			ConjoinedContextPropertyCheckerStepSolverMaker contextDependentProblemStepSolverMaker,
			Theory theory) {
		super(theory);
		this.tail = tail;
		this.head = head;
		this.checked = false;
		this.propertyCheckerStepSolverMaker = contextDependentProblemStepSolverMaker;
	}
	
	@Override
	public Context conjoin(Expression formula, Context context) {
		Context result;
		
		Context specializedResult = conjoinSpecializedForConstraintsIfApplicable(formula, context);
		
		if (specializedResult != null) {
			result = specializedResult;
		}
		else { // fall back to default implementation
			result = Context.super.conjoin(formula, context);
		}
		
		return result;
	}

	/**
	 * Returns the result of conjoining a formula and this context under a given context if a specialized routine is available,
	 * or null otherwise.
	 */
	private Context conjoinSpecializedForConstraintsIfApplicable(Expression formula, Context context) {
		Context result;
		
		if (formula instanceof SingleVariableConstraint) {
			SingleVariableConstraint formulaAsSingleVariableConstraint = (SingleVariableConstraint) formula;
			boolean formulaIsSingleVariableConstraintOnNewVariable = checkIfFormulaISingleVariableConstraintOnNewVariable(formulaAsSingleVariableConstraint);
			if ( formulaIsSingleVariableConstraintOnNewVariable) {
				result = conjoinWithSingleVariableConstraintOnANewVariable(formulaAsSingleVariableConstraint, context);
			}
			else {
				// otherwise we won't be able to use the single variable constraint structure in any special way
				result = null;
			}
		}
		else if (formula instanceof ConjoinedContext) {
			ConjoinedContext formulaAsMultiVariableConstraint = (ConjoinedContext) formula;
			result = conjoinWithMultiVariableContextWithCheckedProperty(formulaAsMultiVariableConstraint, context);
		}
		else {
			result = null;
		}
		
		return result;
	}

	private boolean checkIfFormulaISingleVariableConstraintOnNewVariable(SingleVariableConstraint formulaAsSingleVariableConstraint) {
		Expression variable = formulaAsSingleVariableConstraint.getVariable();
		boolean variableAlreadyConstrainedInThis = contains(this, variable); // TODO: this forces expression representation to be generated, which can be expensive. Better write a method that checks it on the constraint representation itself
		boolean formulaIsSingleVariableConstraintOnNewVariable = ! variableAlreadyConstrainedInThis;
		return formulaIsSingleVariableConstraintOnNewVariable;
	}

	private Context conjoinWithSingleVariableConstraintOnANewVariable(
			SingleVariableConstraint formulaAsSingleVariableConstraint,
			Context context) {
		
		ConjoinedContext tailOfNewContext = this;
		Context newContext = makeAndCheck(getTheory(), formulaAsSingleVariableConstraint, tailOfNewContext, propertyCheckerStepSolverMaker, context);
		return newContext;
	}

	private Context conjoinWithMultiVariableContextWithCheckedProperty(ConjoinedContext formulaAsMultiVariableConstraint, Context context) {
		
		// if formula is itself a MultiVariableContextWithCheckedProperty,
		// we conjoin its two known parts individually.
		// Their own inner structure will also be efficiently exploited by these conjunctions.
		
		Context currentConjunction = this;
		
		if (formulaAsMultiVariableConstraint.tail != null) {
			currentConjunction = currentConjunction.conjoin(formulaAsMultiVariableConstraint.tail, context);
		}
		
		currentConjunction = currentConjunction.conjoin(formulaAsMultiVariableConstraint.head, context);
		
		return currentConjunction;
	}

	@Override
	public Context conjoinWithLiteral(Expression literal, Context context) {
		return explanationBlock(getClass().getSimpleName(), ".conjoin on literal ", literal, " to ", this, code( () -> {
		
			Context result;
			Collection<Expression> variablesInLiteral = getTheory().getVariablesIn(literal, context);
			explain("Variables in literal: ", variablesInLiteral);
			
			if (variablesInLiteral.isEmpty()) {
				result = conjoinWithLiteralWithoutVariables(literal, context);
			}
			else {
				result = conjointNonTrivialLiteral(literal, variablesInLiteral, context);
			}
			return result;
		
		}), "Result is ", RESULT);
	}

	private Context conjoinWithLiteralWithoutVariables(Expression literal, Context context) {
		return explanationBlock("Conjoining with literal without variables", literal, " to ", this, code( () -> {
		
			Expression literalSimplifiedToConstant = getTheory().simplify(literal, context);
			myAssert(literalSimplifiedToConstant != literal, () -> "Literal " + literal + " should have been simplified to a boolean constant, but was not. Sometimes this is caused by using a symbol as a variable, but which has not been declared as a variable in the context, or has been declared as a uniquely named constant in the Context (for example by constructing the Context with the default PrologConstantPredicate as a default predicate for recognizing constants, which recognizes all non-capitalized identifiers as such)");
			Context result = conjoinWithLiteral(literalSimplifiedToConstant, context);
			return result;

		}), "Result is ", RESULT);
	}

	private Context conjointNonTrivialLiteral(Expression literal, Collection<Expression> variablesInLiteral, Context context) {

		return explanationBlock("There is a head, dealing with that.", code( () -> {

			explain("Head: ", head);
			explain("Tail: ", tail);

			Pair<SingleVariableConstraint, Context> newHeadAndNewTail = makeNewHeadAndNewTail(literal, variablesInLiteral, context);

			explain("New head: ", newHeadAndNewTail.first);
			explain("New tail: ", newHeadAndNewTail.second);
			
			newHeadAndNewTail = propagateNewHeadExternalLiterals(newHeadAndNewTail, context);

			Context result = makeNewConjoinedContextFromNewHeadAndNewTail(newHeadAndNewTail, context);
			return result;

		}), "Result is ", RESULT);
}

	private Pair<SingleVariableConstraint, Context> makeNewHeadAndNewTail(Expression literal, Collection<Expression> variablesInLiteral, Context context) {
		return explanationBlock("Making new head and tail by conjoining ", literal, code( () -> {

			SingleVariableConstraint newHead;
			Context newTail;
			if (variablesInLiteral.contains(head.getVariable())) {
				explain("Head variable ", head.getVariable(), " is in literal, so conjoining to head");
				newHead = head.conjoin(literal, context);
				newTail = tail;
			}
			else {
				explain("Head variable ", head.getVariable(), " NOT in literal, so conjoining to tail");
				newHead = head;
				newTail = tail.conjoin(literal, context);
			}
			Pair<SingleVariableConstraint, Context> newHeadAndNewTail = pair(newHead, newTail);
			return newHeadAndNewTail;

		}), "Result is ", RESULT);
	}

	private Pair<SingleVariableConstraint, Context> propagateNewHeadExternalLiterals(Pair<SingleVariableConstraint, Context> newHeadAndNewTail, Context context) {
		return explanationBlock("Propagating external literals in ", this, code( () -> {

			// optional, but good:
			// we propagate external literals from head
			// up the chain so they are integrated and simplified in the corresponding single-variable constraints
			SingleVariableConstraint currentNewHead = newHeadAndNewTail.first;
			Context currentNewTail = newHeadAndNewTail.second;
			if ( ! currentNewHead.isContradiction()) {
				for (Expression externalLiteral : currentNewHead.getExternalLiterals()) {
					if ( ! currentNewTail.isContradiction()) {
						currentNewTail = currentNewTail.conjoin(externalLiteral, context);
					}
				}
				currentNewHead = currentNewHead.makeSimplificationWithoutExternalLiterals();
			}
			return pair(currentNewHead, currentNewTail);

		}), "Result is ", RESULT);
}

	private Context makeNewConjoinedContextFromNewHeadAndNewTail(Pair<SingleVariableConstraint, Context> newHeadAndNewTail, Context context) {

		SingleVariableConstraint newHead = newHeadAndNewTail.first;
		Context newTail = newHeadAndNewTail.second;
		
		Context result;
		if (newHead == head && newTail == tail) { // in case nothing changed
			result = this;
		}
		else {
			result = 
					makeAndCheck(
							getTheory(), 
							newHead, 
							newTail, 
							propertyCheckerStepSolverMaker,
							context);
		}
		return result;
	}

	@Override
	public ConjoinedContext makeContradiction() {
		return (ConjoinedContext) super.makeContradiction();
	}
	
	private ConjoinedContext check(Context context) {
		ConjoinedContext result;
		if (checked) {
			result = this;
		}
		else {
			result = performCheck(context);
		}
		return result;
	}

	private ConjoinedContext performCheck(Context context) {
		return explanationBlock("Performing check to ", this, code( () -> {

			ConjoinedContext result;
			ExpressionLiteralSplitterStepSolver problem = propertyCheckerStepSolverMaker.apply(head, context);
			Expression solution = problem.solve(tail);
			if (solution.equals(FALSE)) { // the head constraint does not exhibit the property in all contexts, so the total constraint does not either.
				result = makeContradiction();
			}
			else {
				this.checked = true;
				result = this;
			}
			return result;

		}), "Result is ", RESULT);
	}
	
	@Override
	protected Expression computeInnerExpressionIfNotContradiction() {
		Expression result = And.make(tail, head);
		return result;
	}

	@Override
	public Expression binding(Expression variable) {
		Expression result;
		if (head.getVariable().equals(variable)) {
			result = head.binding();
		}
		else {
			result = tail.binding(variable);
		}
		return result;
	}

	@Override
	public ConjoinedContext clone() {
		return (ConjoinedContext) super.clone();
	}

	/////////// Context methods
	
	@Override
	public Predicate<Expression> getIsUniquelyNamedConstantPredicate() {
		return tail.getIsUniquelyNamedConstantPredicate();
	}

	@Override
	public ConjoinedContext setIsUniquelyNamedConstantPredicate(Predicate<Expression> isUniquelyNamedConstantPredicate) {
		ConjoinedContext result = clone();
		Context newTail = tail.setIsUniquelyNamedConstantPredicate(isUniquelyNamedConstantPredicate);
		result.tail = newTail;
		return result;
	}

	@Override
	public boolean isUniquelyNamedConstant(Expression expression) {
		return tail.isUniquelyNamedConstant(expression);
	}

	@Override
	public boolean isVariable(Expression expression) {
		return tail.isVariable(expression);
	}

	@Override
	public Set<Expression> getSymbols() {
		return tail.getSymbols();
	}

	@Override
	public Map<Expression, Expression> getSymbolsAndTypes() {
		return tail.getSymbolsAndTypes();
	}

	@Override
	public ConjoinedContext setSymbolsAndTypes(Map<Expression, Expression> newSymbolsAndTypes) {
		ConjoinedContext result = clone();
		result.tail = (Context) tail.setSymbolsAndTypes(newSymbolsAndTypes);
		return result;
	}

	@Override
	public boolean containsSymbol(Expression symbol) {
		return tail.containsSymbol(symbol);
	}
	
	@Override
	public Expression getTypeExpressionOfRegisteredSymbol(Expression symbol) {
		return tail.getTypeExpressionOfRegisteredSymbol(symbol);
	}

	@Override
	public ConjoinedContext makeCloneWithAdditionalRegisteredSymbolsAndTypes(Map<Expression, Expression> indicesAndTypes) {
		ConjoinedContext result = clone();
		Context newTail = tail.makeCloneWithAdditionalRegisteredSymbolsAndTypes(indicesAndTypes);
		result.tail = newTail;
		return result;
	}

	@Override
	public ConjoinedContext putAllGlobalObjects(Map<Object, Object> objects) {
		ConjoinedContext result = clone();
		Context newTail = tail.putAllGlobalObjects(objects);
		result.tail = newTail;
		return result;
	}

	@Override
	public Map<Object, Object> getGlobalObjects() {
		return tail.getGlobalObjects();
	}

	@Override
	public ConjoinedContext putGlobalObject(Object key, Object value) {
		ConjoinedContext result = clone();
		Context newTail = tail.putGlobalObject(key, value);
		result.tail = newTail;
		return result;
	}

	@Override
	public void putInplaceGlobalObject(Object key, Object value) {
		tail.putInplaceGlobalObject(key, value);
	}

	@Override
	public boolean containsGlobalObjectKey(Object key) {
		return tail.containsGlobalObjectKey(key);
	}

	@Override
	public Object getGlobalObject(Object key) {
		return tail.getGlobalObject(key);
	}

	@Override
	public ConjoinedContext makeNewContextWithAddedType(Type type) {
		ConjoinedContext result = clone();
		Context newTail = tail.makeNewContextWithAddedType(type);
		result.tail = newTail;
		return result;
	}

	@Override
	public Type getType(String typeStringRepresentation) {
		return tail.getType(typeStringRepresentation);
	}

	@Override
	public Type getTypeFromTypeExpression(Expression typeExpression) {
		return tail.getTypeFromTypeExpression(typeExpression);
	}

	@Override
	public Collection<Type> getTypes() {
		return tail.getTypes();
	}
}