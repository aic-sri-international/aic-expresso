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
package com.sri.ai.grinder.sgdpllt.core.constraint;

import static com.sri.ai.expresso.helper.Expressions.FALSE;
import static com.sri.ai.expresso.helper.Expressions.TRUE;
import static com.sri.ai.expresso.helper.Expressions.contains;
import static com.sri.ai.util.Util.getFirstOrNull;
import static com.sri.ai.util.base.Pair.pair;

import java.util.Collection;
import java.util.Map;
import java.util.Set;

import com.google.common.annotations.Beta;
import com.google.common.base.Predicate;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.Type;
import com.sri.ai.grinder.sgdpllt.api.Context;
import com.sri.ai.grinder.sgdpllt.api.ExpressionLiteralSplitterStepSolver;
import com.sri.ai.grinder.sgdpllt.api.ExpressionStepSolver;
import com.sri.ai.grinder.sgdpllt.api.SingleVariableConstraint;
import com.sri.ai.grinder.sgdpllt.api.Theory;
import com.sri.ai.grinder.sgdpllt.library.boole.And;
import com.sri.ai.util.base.BinaryFunction;
import com.sri.ai.util.base.Pair;

/**
 * An {@link Context} on multiple variables,
 * with the ability to ensure all single-variable constraints that are part of it
 * have a property determined by a {@link ExpressionStepSolver},
 * or otherwise the total constraint is deemed unsatisfiable.
 * 
 * @author braz
 *
 */
@Beta
public class MultiVariableContextWithCheckedProperty extends AbstractConstraint implements Context {

	private static final long serialVersionUID = 1L;
	
	private SingleVariableConstraint head; // constraint on last variable
	private Context tail; // constraint on variables but the last one; works as context for head when checking property
	private boolean checked;
	
	/**
	 * A {@link BinaryFunction} making a {@link SingleVariableConstraint} for a given
	 * variable and current context.
	 * @author braz
	 *
	 */
	public static
	interface ContextDependentProblemStepSolverMaker
	extends BinaryFunction<SingleVariableConstraint, Context, ExpressionLiteralSplitterStepSolver> {}
	
	ContextDependentProblemStepSolverMaker contextDependentProblemStepSolverMaker;
	
	/**
	 * Creates a new {@link Context} from a {@link SingleVariableConstraint} and a {@link Context},
	 * by either returning a contradiction if either is contradictory,
	 * or a new {@link MultiVariableContextWithCheckedProperty} otherwise.
	 * @param theory
	 * @param head
	 * @param tail
	 * @param context
	 * @return
	 */
	public static Context makeAndCheck(
			Theory theory,
			SingleVariableConstraint head,
			Context tail,
			ContextDependentProblemStepSolverMaker contextDependentProblemStepSolverMaker, 
			Context context) {
	
		Context result;
		if (head.isContradiction() || tail.isContradiction()) {
			result = tail.makeContradiction();
		}
		else {
			MultiVariableContextWithCheckedProperty 
			uncheckedMultiVariableConstraintWithCheckedProperty = 
			new MultiVariableContextWithCheckedProperty(
					tail.getTheory(), 
					head, 
					tail, 
					contextDependentProblemStepSolverMaker);
			result = 
					uncheckedMultiVariableConstraintWithCheckedProperty
					.check(context);
		}

		return result;
	}

	public MultiVariableContextWithCheckedProperty(
			Theory theory, 
			ContextDependentProblemStepSolverMaker contextDependentProblemMaker, 
			Context context) {
		
		this(
				theory, 
				null, // no head at first
				context, // initial tail is the context 
				contextDependentProblemMaker);
	}
	
	/**
	 * Constructs a {@link MultiVariableContextWithCheckedProperty} from a head and a tail constraints,
	 * which is only correct if the {@link SingleVariableConstraint}'s variable does not appear
	 * in the tail constraint.
	 * Note also that this does not check the checked property.
	 * Because of these issues, the constructor is private.
	 * @param head
	 * @param tail
	 */
	private MultiVariableContextWithCheckedProperty(
			Theory theory,
			SingleVariableConstraint head,
			Context tail,
			ContextDependentProblemStepSolverMaker contextDependentProblemMaker) {
		super(theory);
		this.tail = tail;
		this.head = head;
		this.checked = false;
		this.contextDependentProblemStepSolverMaker = contextDependentProblemMaker;
	}
	
	@Override
	public Context conjoin(Expression formula, Context context) {
		Context result;
		
		Pair<Boolean, Context> specializedResult
		= conjoinSpecializedForConstraintsIfApplicable(formula, context);
		
		if (specializedResult.first) {
			result = specializedResult.second;
		}
		else { // fall back to default implementation
			result = Context.super.conjoin(formula, context);
		}
		
		return result;
	}

	/**
	 * Returns a pair indicating whether specialized conjoin for constraints applies to this case and,
	 * if so, provides the result of this conjoining.
	 * @param formula
	 * @param context
	 * @return
	 */
	private Pair<Boolean, Context> conjoinSpecializedForConstraintsIfApplicable(Expression formula, Context context) {
		Pair<Boolean, Context> result;
		
		if (formula instanceof SingleVariableConstraint) {
			SingleVariableConstraint formulaAsSingleVariableConstraint = (SingleVariableConstraint) formula;
			Expression variable = formulaAsSingleVariableConstraint.getVariable();
			boolean variableAlreadyConstrainedInThis = contains(this, variable); // TODO: this forces expression representation to be generated, which can be expensive. Better write a method that checks it on the constraint representation itself
			if ( ! variableAlreadyConstrainedInThis) {
				// if the variable is new to this constraint, we can simply tack on its constraint on it. 
				Context newContext = makeAndCheck(
						getTheory(), 
						formulaAsSingleVariableConstraint, 
						this, 
						contextDependentProblemStepSolverMaker,
						context);
				result = pair(true, newContext);
			}
			else {
				// otherwise we won't be able to use the single variable constraint structure in any special way
				result = pair(false, null);
			}
		}
		else if (formula instanceof MultiVariableContextWithCheckedProperty) {
			MultiVariableContextWithCheckedProperty formulaAsMultiVariableConstraint = (MultiVariableContextWithCheckedProperty) formula;
			// if formula is itself a MultiVariableContextWithCheckedProperty,
			// we conjoin its two known parts individually.
			// Their own inner structure will also be efficiently exploited by these conjunctions.
			Context conjunction = this;
			if (formulaAsMultiVariableConstraint.tail != null) {
				conjunction = conjunction.conjoin(formulaAsMultiVariableConstraint.tail, context);
			}
			if (formulaAsMultiVariableConstraint.head != null) {
				conjunction = conjunction.conjoin(formulaAsMultiVariableConstraint.head, context);
			}
			result = pair(true, conjunction);
		}
		else {
			// the formula does not have a recognizable structure we can exploit
			result = pair(false, null);
		}
		
		return result;
	}

	@Override
	public Context conjoinWithLiteral(Expression literal, Context context) {
		Context result;
		if (literal.equals(TRUE)) {
			result = this;
		}
		else if (literal.equals(FALSE)) {
			result = makeContradiction();
		}
		else {
			Collection<Expression> variablesInLiteral = getTheory().getVariablesIn(literal, context);
			if (variablesInLiteral.isEmpty()) {
				Expression literalSimplifiedToConstant = getTheory().simplify(literal, context);
				if (literalSimplifiedToConstant == literal) {
					throw new Error("Literal " + literal + " should have been simplified to a boolean constant, but was not. Sometimes this is caused by using a symbol as a variable, but which has not been declared as a variable in the context, or has been declared as a uniquely named constant in the Context (for example by constructing the Context with the default PrologConstantPredicate as a default predicate for recognizing constants, which recognizes all non-capitalized identifiers as such)");
				}
				result = conjoinWithLiteral(literalSimplifiedToConstant, context);
			}
			else if (head != null) {
				SingleVariableConstraint newHead;
				Context newTail;
				if (variablesInLiteral.contains(head.getVariable())) {
					newHead = head.conjoin(literal, context);
					newTail = tail;
				}
				else {
					newHead = head;
					newTail = tail.conjoin(literal, context);
				}
		
				// optional, but good:
				// we propagate external literals from head
				// up the chain so they are integrated and simplified in the corresponding single-variable constraints
				if ( ! newHead.isContradiction()) {
					for (Expression externalLiteral : newHead.getExternalLiterals()) {
						if ( ! newTail.isContradiction()) {
							newTail = newTail.conjoin(externalLiteral, context);
						}
					}
					newHead = newHead.makeSimplificationWithoutExternalLiterals();
				}

				if (newHead == head && newTail == tail) { // in case nothing changed
					result = this;
				}
				else {
					result = 
							makeAndCheck(
									getTheory(), 
									newHead, 
									newTail, 
									contextDependentProblemStepSolverMaker,
									context);
				}
			}
			else {
				Expression firstVariable = getFirstOrNull(variablesInLiteral);
				SingleVariableConstraint newSingleVariableConstraint = getTheory().makeSingleVariableConstraint(firstVariable, getTheory(), context);
				newSingleVariableConstraint = newSingleVariableConstraint.conjoin(literal, context);
				result = 
						makeAndCheck(
								getTheory(), 
								newSingleVariableConstraint, 
								this, 
								contextDependentProblemStepSolverMaker,
								context);
			}
		}
		return result;
	}

	/**
	 * @return
	 */
	@Override
	public MultiVariableContextWithCheckedProperty makeContradiction() {
		return (MultiVariableContextWithCheckedProperty) super.makeContradiction();
	}
	
	private MultiVariableContextWithCheckedProperty check(Context context) {
		MultiVariableContextWithCheckedProperty result;
		if (checked) {
			result = this;
		}
		else {
			ExpressionLiteralSplitterStepSolver problem = contextDependentProblemStepSolverMaker.apply(head, context);
			Expression solution = problem.solve(tail);
			if (solution == null) { // tail is found to be inconsistent with given context
				result = makeContradiction();
			}
			else if (solution.equals(FALSE)) { // the head constraint does not exhibit the property in all contexts, so the total constraint does not either.
				result = makeContradiction();
			}
			else {
				this.checked = true;
				result = this;
			}
		}
		return result;
	}
	
	@Override
	protected Expression computeInnerExpressionIfNotContradiction() {
		Expression result;
		if (head == null) {
			result = tail;
		}
		else {
			result = And.make(tail, head);
		}
		return result;
	}

	@Override
	public Expression binding(Expression variable) {
		Expression result;
		if ( head != null && head.getVariable().equals(variable)) {
			result = head.binding();
		}
		else {
			result = tail.binding(variable);
		}
		return result;
	}

	@Override
	public MultiVariableContextWithCheckedProperty clone() {
		return (MultiVariableContextWithCheckedProperty) super.clone();
	}

	/////////// Context methods
	
	@Override
	public Predicate<Expression> getIsUniquelyNamedConstantPredicate() {
		return tail.getIsUniquelyNamedConstantPredicate();
	}

	@Override
	public MultiVariableContextWithCheckedProperty setIsUniquelyNamedConstantPredicate(Predicate<Expression> isUniquelyNamedConstantPredicate) {
		MultiVariableContextWithCheckedProperty result = clone();
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
	public MultiVariableContextWithCheckedProperty setSymbolsAndTypes(Map<Expression, Expression> newSymbolsAndTypes) {
		MultiVariableContextWithCheckedProperty result = clone();
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
	public MultiVariableContextWithCheckedProperty makeNewRegistryWithRegisteredAdditionalSymbolsAndTypes(Map<Expression, Expression> indicesAndTypes) {
		MultiVariableContextWithCheckedProperty result = clone();
		Context newTail = tail.makeNewRegistryWithRegisteredAdditionalSymbolsAndTypes(indicesAndTypes);
		result.tail = newTail;
		return result;
	}

	@Override
	public MultiVariableContextWithCheckedProperty putAllGlobalObjects(Map<Object, Object> objects) {
		MultiVariableContextWithCheckedProperty result = clone();
		Context newTail = tail.putAllGlobalObjects(objects);
		result.tail = newTail;
		return result;
	}

	@Override
	public Map<Object, Object> getGlobalObjects() {
		return tail.getGlobalObjects();
	}

	@Override
	public MultiVariableContextWithCheckedProperty putGlobalObject(Object key, Object value) {
		MultiVariableContextWithCheckedProperty result = clone();
		Context newTail = tail.putGlobalObject(key, value);
		result.tail = newTail;
		return result;
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
	public MultiVariableContextWithCheckedProperty add(Type type) {
		MultiVariableContextWithCheckedProperty result = clone();
		Context newTail = tail.add(type);
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