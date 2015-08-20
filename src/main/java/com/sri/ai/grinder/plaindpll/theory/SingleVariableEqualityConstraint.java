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
package com.sri.ai.grinder.plaindpll.theory;

import static com.sri.ai.expresso.helper.Expressions.FALSE;
import static com.sri.ai.expresso.helper.Expressions.TRUE;
import static com.sri.ai.expresso.helper.Expressions.apply;
import static com.sri.ai.grinder.helper.GrinderUtil.getType;
import static com.sri.ai.grinder.helper.GrinderUtil.getTypeCardinality;
import static com.sri.ai.grinder.library.FunctorConstants.DISEQUALITY;
import static com.sri.ai.grinder.library.FunctorConstants.EQUALITY;
import static com.sri.ai.grinder.library.FunctorConstants.TYPE;
import static com.sri.ai.util.Util.list;
import static com.sri.ai.util.Util.thereExists;

import java.util.Collection;
import java.util.Collections;
import java.util.LinkedHashSet;
import java.util.List;

import com.google.common.base.Predicate;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.core.DefaultSyntacticFunctionApplication;
import com.sri.ai.expresso.helper.AbstractExpressionWrapper;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.library.Disequality;
import com.sri.ai.grinder.library.Equality;
import com.sri.ai.grinder.library.boole.And;
import com.sri.ai.grinder.plaindpll.api.ConstraintTheory;
import com.sri.ai.grinder.plaindpll.api.SingleVariableConstraint;
import com.sri.ai.grinder.plaindpll.core.SGDPLLT;
import com.sri.ai.util.Util;
import com.sri.ai.util.base.BinaryFunction;

/**
 * An {@link Expression} with efficient internal representation for operations on being expanded by a splitter (literal in constraintTheory) and
 * model counting.
 * This interface is defined for use primarily by {@link SGDPLLT}.
 * 
 * @author braz
 *
 */
public class SingleVariableEqualityConstraint extends AbstractExpressionWrapper implements SingleVariableConstraint {

	private static final long serialVersionUID = 1L;
	
	private Expression variable;
	private Collection<Expression> equals;
	private Collection<Expression> disequals;
	private Collection<Expression> externalLiterals; // literals not on variable
	private int numberOfUniquelyNamedConstantDisequals;
	private EqualityConstraintTheory constraintTheory;
	
	public SingleVariableEqualityConstraint(Expression variable) {
		this(variable, Util.set(), Util.set(), Util.set(), 0);
	}
	
	public SingleVariableEqualityConstraint(Expression variable, Collection<Expression> equals, Collection<Expression> disequals, Collection<Expression> externalLiterals, int numberOfUniquelyNamedConstantDisequals) {
		this.variable = variable;
		this.equals = equals;
		this.disequals = disequals;
		this.externalLiterals = externalLiterals;
		this.numberOfUniquelyNamedConstantDisequals = numberOfUniquelyNamedConstantDisequals;
		this.constraintTheory = new EqualityConstraintTheory(null);
	}
	
	/**
	 * Returns the expressions the variable is constrained to be disequal from.
	 * @return
	 */
	public Collection<Expression> getEquals() {
		return Collections.unmodifiableCollection(equals);
	}
	
	/**
	 * Returns the expressions the variable is constrained to be disequal from.
	 * @return
	 */
	public Collection<Expression> getDisequals() {
		return Collections.unmodifiableCollection(disequals);
	}
	
	/**
	 * Returns the literals that are part of this constraint, but not defined on the variable.
	 * @return
	 */
	public Collection<Expression> getExternalLiterals() {
		return Collections.unmodifiableCollection(externalLiterals);
	}

	private SingleVariableEqualityConstraint makeWithNewExternalLiterals(Collection<Expression> newExternalLiterals) {
		if (newExternalLiterals == externalLiterals) {
			return this;
		}
		else {
			SingleVariableEqualityConstraint result = 
					new SingleVariableEqualityConstraint(
							variable, equals, disequals, newExternalLiterals, numberOfUniquelyNamedConstantDisequals);
			return result;
		}
	}

	private SingleVariableEqualityConstraint makeWithAdditionalExternalLiteral(Expression additionalExternalLiteral) {
		Collection<Expression> newExternalLiterals = new LinkedHashSet<>(externalLiterals);
		newExternalLiterals.add(additionalExternalLiteral);
		SingleVariableEqualityConstraint result = makeWithNewExternalLiterals(newExternalLiterals);
		return result;
	}
	
	@Override
	public ConstraintTheory getConstraintTheory() {
		return constraintTheory;
	}
	
	@Override
	public Expression getVariable() {
		return variable;
	}

	@Override
	public SingleVariableEqualityConstraint simplifyGiven(Expression externalLiteral, RewritingProcess process) {
		SingleVariableEqualityConstraint result;
		
		if (externalLiterals.contains(externalLiteral)) {
			Collection<Expression> newExternalLiterals = new LinkedHashSet<>(externalLiterals);
			newExternalLiterals.remove(externalLiteral);
			result = makeWithNewExternalLiterals(newExternalLiterals);
		}
		else {
			result = this;
		}
		
		return result;
	}

	@Override
	public SingleVariableEqualityConstraint conjoin(Expression literal, RewritingProcess process) {
		SingleVariableEqualityConstraint result;

		if (externalLiterals.contains(literal)) {
			result = this;
		}
		else if ( ! literal.hasFunctor(EQUALITY) && ! literal.hasFunctor(DISEQUALITY)) {
			if (literal.equals(TRUE)) {
				result = this;
			}
			else if (literal.equals(FALSE)) {
				result = null;
			}
			else {
				result = makeWithAdditionalExternalLiteral(literal);
			}
		}
		else {
			Expression other = termToWhichVariableIsEqualedToOrNull(literal);
			if (other == null) {
				result = makeWithAdditionalExternalLiteral(literal);
			}
			else {
				boolean splitterIsEquality = literal.hasFunctor(EQUALITY);
				if (splitterIsEquality ) { // X = T
					result = conjoinEqualityOnVariable(other, process);
				}
				else { // X != T
					result = conjoinDisequalityOnVariable(other, process);
				}
			}
		}

		return result;
	}

	/**
	 * @param other
	 * @param process
	 * @return
	 */
	protected SingleVariableEqualityConstraint conjoinEqualityOnVariable(Expression other, RewritingProcess process) {
		SingleVariableEqualityConstraint result;
		boolean otherIsConstant;
		if (equals.contains(other)) {
			result = this; // already present; redundant
		}
		else if (disequals.contains(other)) {
			result = null; // constrained to be both equal and disequal to T, contradiction
		}
		else if ((otherIsConstant = process.isUniquelyNamedConstant(other)) && thereExists(equals, e -> process.isUniquelyNamedConstant(e))) {
			result = null; // constrained to be equal to two distinct uniquely named constants, contradiction
		}
		else {
			Collection<Expression> newDisequals;
			int newNumberOfUniquelyNamedConstantDisequals;
			if (otherIsConstant) {
				// disequalities to other constants become irrelevant
				newDisequals = Util.removeNonDestructively(disequals, (Predicate<Expression>) d -> process.isUniquelyNamedConstant(d));
				newNumberOfUniquelyNamedConstantDisequals = 0;
			}
			else {
				newDisequals = disequals;
				newNumberOfUniquelyNamedConstantDisequals = numberOfUniquelyNamedConstantDisequals;
			}
			
			Collection<Expression> newEquals = new LinkedHashSet<Expression>(equals);
			newEquals.add(other);
			result = new SingleVariableEqualityConstraint(
					variable, newEquals, newDisequals, externalLiterals, newNumberOfUniquelyNamedConstantDisequals);
		}
		return result;
	}

	/**
	 * @param other
	 * @param process
	 * @return
	 */
	protected SingleVariableEqualityConstraint conjoinDisequalityOnVariable(Expression other, RewritingProcess process) {
		SingleVariableEqualityConstraint result;
		boolean otherIsConstant;
		if (equals.contains(other)) {
			result = null; // constrained to be both equal and disequal to T, contradiction
		}
		else if (disequals.contains(other)) {
			result = this; // already present; redundant
		}
		else if ((otherIsConstant = process.isUniquelyNamedConstant(other)) && thereExists(equals, e -> process.isUniquelyNamedConstant(e))) {
			result = this; // redundant because variable already constrained to be another constant.
		}
		else if (otherIsConstant && numberOfUniquelyNamedConstantDisequals == getVariableDomainSize(process) - 1) {
			result = null; // contradictory because there will be no value left in domain
		}
		else {
			Collection<Expression> newDisequals = new LinkedHashSet<Expression>(disequals);
			newDisequals.add(other);
			int newNumberOfUniquelyNamedConstantDisequals = numberOfUniquelyNamedConstantDisequals + (otherIsConstant? 1 : 0);
			result = new SingleVariableEqualityConstraint(variable, equals, newDisequals, externalLiterals, newNumberOfUniquelyNamedConstantDisequals);
		}
		return result;
	}

	private Expression termToWhichVariableIsEqualedToOrNull(Expression equalityLiteral) {
		Expression result;
		if (equalityLiteral.get(0).equals(variable)) {
			result = equalityLiteral.get(1);
		}
		else if (equalityLiteral.get(1).equals(variable)) {
			result = equalityLiteral.get(0);
		}
		else {
			result = null;
		}
		return result;
	}

	/**
	 * @param process
	 * @return
	 */
	protected BinaryFunction<Expression, Expression, Expression> equalityMaker(RewritingProcess process) {
		return (t1, t2) -> Equality.makeWithConstantSimplification(t1, t2, process);
	}

	/**
	 * @param process
	 * @return
	 */
	protected BinaryFunction<Expression, Expression, Expression> disequalityMaker(RewritingProcess process) {
		return (t1, t2) -> Disequality.makeWithConstantSimplification(t1, t2, process);
	}

	@Override
	protected Expression computeInnerExpression() {
		List<Expression> conjuncts = list();
		Util.mapIntoList(equals,    d -> apply(EQUALITY,    variable, d), conjuncts);
		Util.mapIntoList(disequals, d -> apply(DISEQUALITY, variable, d), conjuncts);
		conjuncts.addAll(externalLiterals);
		Expression result = And.make(conjuncts);
		return result;
	}

	@Override
	public SingleVariableEqualityConstraint clone() {
		throw new Error("Not implemented");
	}

	@Override
	public Expression getVariableDomain(RewritingProcess process) {
		Expression variableType = getType(variable, process);
		if (variableType == null) {
			variableType = new DefaultSyntacticFunctionApplication(TYPE, variable);
		}
		return variableType;
	}
	
	protected long cachedIndexDomainSize = -1;

	@Override
	public long getVariableDomainSize(RewritingProcess process) {
		if (cachedIndexDomainSize == -1) {
			cachedIndexDomainSize = getTypeCardinality(variable, process);
		}
		return cachedIndexDomainSize;
	}

	@Override
	public Expression pickSplitter(RewritingProcess process) {
		throw new Error("Not implemented yet.");
	}

	@Override
	public Expression modelCount(RewritingProcess process) {
		throw new Error("Not implemented yet.");
	}

	@Override
	public String debuggingDescription(RewritingProcess process) {
		return toString() + ", #constantDisequals/domainSize: " + numberOfUniquelyNamedConstantDisequals + "/" + getVariableDomainSize(process);
	}
}

//public class SingleVariableEqualityConstraint extends AbstractExpressionWrapper implements SingleVariableConstraint {
//
//	private static final long serialVersionUID = 1L;
//	
//	private Expression variable;
//	private Expression boundValue;
//	private ArrayList<Expression> disequals;
//	private Collection<Expression> externalLiterals; // literals not on variable
//	private int numberOfUniquelyNamedConstantDisequals; // -1 if not computed yet
//
//	private EqualityConstraintTheory constraintTheory;
//	
//	public SingleVariableEqualityConstraint(Expression variable) {
//		this(variable, null, new ArrayList<Expression>(), Util.set(), 0, null);
//	}
//	
//	public SingleVariableEqualityConstraint(Expression variable, Expression boundValue) {
//		this(variable, boundValue, null, Util.set(), 0, null);
//	}
//	
//	public SingleVariableEqualityConstraint(Expression variable, ArrayList<Expression> disequals, int numberOfUniquelyNamedConstantDisequals) {
//		this(variable, null, disequals, Util.set(), numberOfUniquelyNamedConstantDisequals, null);
//	}
//
//	public SingleVariableEqualityConstraint(Expression variable, Expression boundValue, ArrayList<Expression> disequals, Collection<Expression> externalLiterals, int numberOfUniquelyNamedConstantDisequals, TermTheory termTheory) {
//		this.variable = variable;
//		this.boundValue = boundValue;
//		this.disequals = disequals;
//		this.externalLiterals = externalLiterals;
//		this.numberOfUniquelyNamedConstantDisequals = numberOfUniquelyNamedConstantDisequals;
//		this.constraintTheory = new EqualityConstraintTheory(termTheory);
//	}
//	
//	/**
//	 * Returns the expression the variable is bound to, or <code>null</code> if none.
//	 */
//	public Expression getBoundValue() {
//		return boundValue;
//	}
//	
//	/**
//	 * Returns the expressions the variable is constrained to be disequal from.
//	 * @return
//	 */
//	public List<Expression> getDisequals() {
//		return Collections.unmodifiableList(disequals);
//	}
//	
//	/**
//	 * Returns the literals that are part of this constraint, but not defined on the variable.
//	 * @return
//	 */
//	public Collection<Expression> getExternalLiterals() {
//		return Collections.unmodifiableCollection(externalLiterals);
//	}
//
//	private SingleVariableEqualityConstraint makeWithNewExternalLiterals(Collection<Expression> newExternalLiterals) {
//		if (newExternalLiterals == externalLiterals) {
//			return this;
//		}
//		else {
//			SingleVariableEqualityConstraint result = 
//					new SingleVariableEqualityConstraint(
//							variable, boundValue, disequals, newExternalLiterals, numberOfUniquelyNamedConstantDisequals, constraintTheory.getTermTheory());
//			return result;
//		}
//	}
//
//	private SingleVariableEqualityConstraint makeWithAdditionalExternalLiteral(Expression additionalExternalLiteral) {
//		Collection<Expression> newExternalLiterals = new LinkedHashSet<>(externalLiterals);
//		newExternalLiterals.add(additionalExternalLiteral);
//		SingleVariableEqualityConstraint result = makeWithNewExternalLiterals(newExternalLiterals);
//		return result;
//	}
//	
//	private SingleVariableEqualityConstraint makeWithAdditionalExternalLiterals(Collection<Expression> additionalExternalLiterals) {
//		Collection<Expression> newExternalLiterals = new LinkedHashSet<>(externalLiterals);
//		newExternalLiterals.addAll(additionalExternalLiterals);
//		SingleVariableEqualityConstraint result = makeWithNewExternalLiterals(newExternalLiterals);
//		return result;
//	}
//	
//	@Override
//	public ConstraintTheory getConstraintTheory() {
//		return constraintTheory;
//	}
//	
//	@Override
//	public Expression getVariable() {
//		return variable;
//	}
//
//	@Override
//	public Expression pickSplitter(RewritingProcess process) {
//		Expression result;
//		Expression externalLiteral = getFirst(getExternalLiterals());
//		if (externalLiteral != null) {
//			result = externalLiteral;
//		}
//		else if (getBoundValue() != null) {
//			result = null; // there are no disequals stored if we already have a binding
//		}
//		else {
//			// before we count the models, we need to know that all disequals are disequal to each other too.
//			Constraint contextualConstraint = process.getDPLLContextualConstraint();
//			List<Expression> disequals = getDisequals();
//			for (int i = 0; i != disequals.size(); i++) {
//				for (int j = i + 1; j != disequals.size(); j++) {
//					Expression disequality = Disequality.makeWithConstantSimplification(disequals.get(i), disequals.get(j), process);
//					if ( ! disequality.equals(FALSE) && ! contextualConstraint.directlyImpliesNonTrivialLiteral(disequality, process)) {
//						result = apply(EQUALITY, disequals.get(i), disequals.get(j));
//						break;
//					}
//				}
//			}
//			result = null;
//		}
//		return result;
//	}
//
//	@Override
//	public SingleVariableEqualityConstraint simplifyGiven(Expression externalLiteral, RewritingProcess process) {
//		SingleVariableEqualityConstraint result;
//		
//		if (externalLiterals.contains(externalLiteral)) {
//			Collection<Expression> newExternalLiterals = new LinkedHashSet<>(externalLiterals);
//			newExternalLiterals.remove(externalLiteral);
//			result = makeWithNewExternalLiterals(newExternalLiterals);
//		}
//		else {
//			result = this;
//		}
//		
//		return result;
//	}
//
//	@Override
//	public SingleVariableEqualityConstraint conjoin(Expression literal, RewritingProcess process) {
//		SingleVariableEqualityConstraint result;
//		
//		if (externalLiterals.contains(literal)) {
//			result = this;
//		}
//		else if ( ! literal.hasFunctor(EQUALITY) && ! literal.hasFunctor(DISEQUALITY)) {
//			if (literal.equals(TRUE)) {
//				result = this;
//			}
//			else if (literal.equals(FALSE)) {
//				result = null;
//			}
//			else {
//				result = makeWithAdditionalExternalLiteral(literal);
//			}
//		}
//		else {
//			Expression other = termToWhichVariableIsEqualedToOrNull(literal);
//			if (other == null) {
//				result = makeWithAdditionalExternalLiteral(literal);
//			}
//			else {
//				boolean variableIsBound = boundValue != null;
//				boolean splitterIsEquality = literal.hasFunctor(EQUALITY);
//				if (variableIsBound) {
//					if (splitterIsEquality ) {
//						if (other.equals(boundValue)) {
//							result = this;
//						}
//						else {
//							result = keepValueAndEnforceOtherToBeEqualToIt(other, process);
//						}
//					}
//					else { // splitter is disequality
//						if (other.equals(boundValue)) {
//							result = null; // contradiction, for splitter is disequality to the same boundValue the variable is bound to
//						}
//						else { 
//							result = keepValueAndEnforceOtherToBeNotEqualToIt(other, process);
//						}
//					}
//				}
//				else { // variable is not bound, and the only constraints, if any, are disequalities
//					if (splitterIsEquality) {
//						result = takeValueAndEnforceDisequalsToBeNotEqualToIt(other, process);
//					}
//					else { 
//						result = takeNewDisequal(other, process);
//					}
//				}
//			}
//		}
//
//		return result;
//	}
//
//	private Expression termToWhichVariableIsEqualedToOrNull(Expression equalityLiteral) {
//		Expression result;
//		if (equalityLiteral.get(0).equals(variable)) {
//			result = equalityLiteral.get(1);
//		}
//		else if (equalityLiteral.get(1).equals(variable)) {
//			result = equalityLiteral.get(0);
//		}
//		else {
//			result = null;
//		}
//		return result;
//	}
//
//	/**
//	 * @param other
//	 * @param process
//	 * @return
//	 */
//	protected SingleVariableEqualityConstraint keepValueAndEnforceOtherToBeEqualToIt(Expression other, RewritingProcess process) {
//		SingleVariableEqualityConstraint result;
//		Expression impliedEquality = Equality.makeWithConstantSimplification(boundValue, other, process);
//		if (impliedEquality.equals(TRUE)) {
//			result = this;
//		}
//		else if (impliedEquality.equals(FALSE)) {
//			result = null;
//		}
//		else {
//			result = makeWithAdditionalExternalLiteral(impliedEquality); // implies boundValue = other
//		}
//		return result;
//	}
//
//	/**
//	 * @param other
//	 * @param process
//	 * @return
//	 */
//	protected SingleVariableEqualityConstraint keepValueAndEnforceOtherToBeNotEqualToIt(Expression other, RewritingProcess process) {
//		SingleVariableEqualityConstraint result;
//		Collection<Expression> impliedLiterals = nonTrivialFormulasBuiltFromTerm1AndEachTerm2OrNull(boundValue, list(other), disequalityMaker(process));
//		if (impliedLiterals == null) {
//			result = null;
//		}
//		else {
//			result = makeWithAdditionalExternalLiterals(impliedLiterals); // implies boundValue != other
//		}
//		return result;
//	}
//
//	/**
//	 * @param other
//	 * @param process
//	 * @return
//	 */
//	protected SingleVariableEqualityConstraint takeValueAndEnforceDisequalsToBeNotEqualToIt(Expression other, RewritingProcess process) {
//		SingleVariableEqualityConstraint result;
//		Collection<Expression> impliedLiterals = nonTrivialFormulasBuiltFromTerm1AndEachTerm2OrNull(other, disequals, disequalityMaker(process));
//		if (impliedLiterals == null) {
//			result = null;
//		}
//		else {
//			SingleVariableEqualityConstraint constraintBindingVariableToOther = new SingleVariableEqualityConstraint(variable, other);
//			result = constraintBindingVariableToOther.makeWithAdditionalExternalLiterals(impliedLiterals);
//		}
//		return result;
//	}
//
//	/**
//	 * @param other
//	 * @return
//	 */
//	protected SingleVariableEqualityConstraint takeNewDisequal(Expression other, RewritingProcess process) {
//		SingleVariableEqualityConstraint result;
//		if (disequals.contains(other)) {
//			result = this; // redundant
//		}
//		else {
//			if (process.isUniquelyNamedConstant(other)) {
//				incrementNumberOfUniquelyNamedConstantDisequals(process);
//			}
//			
//			if (isInconsistentBecauseVariableIsDisequalToANumberOfUniquelyNamedConstantsEqualToDomainSize(process)) {
//				result = null;
//			}
//			else {
//				ArrayList<Expression> newDisequals = new ArrayList<>(disequals);
//				newDisequals.add(other);
//				SingleVariableEqualityConstraint newConstraint = new SingleVariableEqualityConstraint(variable, newDisequals, getNumberOfUniquelyNamedConstantDisequals(process));
//				result = newConstraint;
//			}
//		}
//		return result;
//	}
//
//	/**
//	 * @param process
//	 * @return
//	 */
//	protected boolean isInconsistentBecauseVariableIsDisequalToANumberOfUniquelyNamedConstantsEqualToDomainSize(RewritingProcess process) {
//		long variableDomainSize = getVariableDomainSize(process);
//		boolean result = variableDomainSize != -1 && getNumberOfUniquelyNamedConstantDisequals(process) == variableDomainSize;
//		return result;
//	}
//
//	private int getNumberOfUniquelyNamedConstantDisequals(RewritingProcess process) {
//		if (numberOfUniquelyNamedConstantDisequals == -1) {
//			numberOfUniquelyNamedConstantDisequals = Util.count(disequals, process.getIsUniquelyNamedConstantPredicate());
//		}
//		return numberOfUniquelyNamedConstantDisequals;
//	}
//
//	/**
//	 * @param process
//	 * @return
//	 */
//	protected int incrementNumberOfUniquelyNamedConstantDisequals(RewritingProcess process) {
//		return numberOfUniquelyNamedConstantDisequals = getNumberOfUniquelyNamedConstantDisequals(process) + 1;
//	}
//
//	/**
//	 * @param process
//	 * @return
//	 */
//	protected BinaryFunction<Expression, Expression, Expression> equalityMaker(RewritingProcess process) {
//		return (t1, t2) -> Equality.makeWithConstantSimplification(t1, t2, process);
//	}
//
//	/**
//	 * @param process
//	 * @return
//	 */
//	protected BinaryFunction<Expression, Expression, Expression> disequalityMaker(RewritingProcess process) {
//		return (t1, t2) -> Disequality.makeWithConstantSimplification(t1, t2, process);
//	}
//
//	/**
//	 * Applies formula builder to term1 and each term in terms2 and returns that collection if none is false, or null as soon as a false one is obtained.
//	 * @param term1
//	 * @param terms2
//	 * @param formulaBuilder
//	 * @return a collection of built formulas if none is false, null otherwise.
//	 */
//	protected static Collection<Expression> nonTrivialFormulasBuiltFromTerm1AndEachTerm2OrNull(
//			Expression term1, Collection<Expression> terms2, BinaryFunction<Expression, Expression, Expression> formulaBuilder) {
//		
//		Collection<Expression> result = list();
//		for (Expression anotherTerm : terms2) {
//			Expression literal = formulaBuilder.apply(term1, anotherTerm);
//			if (literal.equals(TRUE)) {
//			}
//			else if (literal.equals(FALSE)) {
//				result = null;
//				break;
//			}
//			else {
//				result.add(literal);
//			}
//		}
//		return result;
//	}
//
//	@Override
//	protected Expression computeInnerExpression() {
//		List<Expression> conjuncts = list();
//		if (boundValue != null) {
//			conjuncts.add(apply(EQUALITY, variable, boundValue));
//		}
//		else {
//			Util.mapIntoList(disequals, d -> apply(DISEQUALITY, variable, d), conjuncts);
//		}
//		conjuncts.addAll(externalLiterals);
//		Expression result = And.make(conjuncts);
//		return result;
//	}
//
//	@Override
//	public SingleVariableEqualityConstraint clone() {
//		SingleVariableEqualityConstraint result = new SingleVariableEqualityConstraint(variable, boundValue, disequals, Util.set(), numberOfUniquelyNamedConstantDisequals, constraintTheory.getTermTheory());
//		return result;
//	}
//
//	@Override
//	public Expression modelCount(RewritingProcess process) {
//		Expression result;
//		if (getBoundValue() != null) {
//			result = ONE;
//		}
//		else {
//			long numberOfNonAvailableValues = getDisequals().size();
//			long variableDomainSize = getVariableDomainSize(process);
//			if (variableDomainSize == -1) {
//				Expression variableDomain = getVariableDomain(process);
//				Expression variableDomainCardinality = apply(CARDINALITY, variableDomain);
//				result = Minus.make(variableDomainCardinality, makeSymbol(numberOfNonAvailableValues));
//			}
//			else {
//				result = makeSymbol(max(0, variableDomainSize - numberOfNonAvailableValues));
//			}
//		}
//		return result;
//	}
//
//	@Override
//	public Expression getVariableDomain(RewritingProcess process) {
//		Expression variableType = getType(variable, process);
//		if (variableType == null) {
//			variableType = new DefaultSyntacticFunctionApplication(TYPE, variable);
//		}
//		return variableType;
//	}
//	
//	protected long cachedIndexDomainSize = -1;
//
//	@Override
//	public long getVariableDomainSize(RewritingProcess process) {
//		if (cachedIndexDomainSize == -1) {
//			cachedIndexDomainSize = getTypeCardinality(variable, process);
//		}
//		return cachedIndexDomainSize;
//	}
//}