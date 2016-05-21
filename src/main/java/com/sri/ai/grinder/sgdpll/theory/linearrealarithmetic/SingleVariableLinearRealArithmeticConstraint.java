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
package com.sri.ai.grinder.sgdpll.theory.linearrealarithmetic;

import static com.sri.ai.expresso.helper.Expressions.INFINITY;
import static com.sri.ai.expresso.helper.Expressions.apply;
import static com.sri.ai.grinder.library.FunctorConstants.GREATER_THAN;
import static com.sri.ai.grinder.library.FunctorConstants.GREATER_THAN_OR_EQUAL_TO;
import static com.sri.ai.grinder.library.FunctorConstants.LESS_THAN;
import static com.sri.ai.grinder.library.FunctorConstants.LESS_THAN_OR_EQUAL_TO;
import static com.sri.ai.util.Util.iterator;
import static com.sri.ai.util.Util.list;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.Type;
import com.sri.ai.expresso.type.IntegerInterval;
import com.sri.ai.expresso.type.RealExpressoType;
import com.sri.ai.expresso.type.RealInterval;
import com.sri.ai.grinder.api.Context;
import com.sri.ai.grinder.library.number.UnaryMinus;
import com.sri.ai.grinder.sgdpll.api.ConstraintTheory;
import com.sri.ai.grinder.sgdpll.theory.numeric.AbstractSingleVariableNumericConstraint;

/**
 * A linear real arithmetic single-variable constraint solver.
 *
 * @author braz
 *
 */
@Beta
public class SingleVariableLinearRealArithmeticConstraint extends AbstractSingleVariableNumericConstraint {

	private static final long serialVersionUID = 1L;
	
	public SingleVariableLinearRealArithmeticConstraint(
			Expression variable,
			boolean propagateAllLiteralsWhenVariableIsBound,
			ConstraintTheory constraintTheory) {
		
		super(variable, propagateAllLiteralsWhenVariableIsBound, constraintTheory);
	}

	private SingleVariableLinearRealArithmeticConstraint(
			Expression variable,
			ArrayList<Expression> positiveNormalizedAtoms,
			ArrayList<Expression> negativeNormalizedAtoms,
			List<Expression> externalLiterals,
			boolean propagateAllLiteralsWhenVariableIsBound,
			ConstraintTheory constraintTheory) {
		
		super(variable, positiveNormalizedAtoms, negativeNormalizedAtoms, externalLiterals, propagateAllLiteralsWhenVariableIsBound, constraintTheory);
	}

	public SingleVariableLinearRealArithmeticConstraint(SingleVariableLinearRealArithmeticConstraint other) {
		super(other);
	}

	@Override
	public SingleVariableLinearRealArithmeticConstraint clone() {
		SingleVariableLinearRealArithmeticConstraint result = new SingleVariableLinearRealArithmeticConstraint(this);
		return result;
	}

	@Override
	protected SingleVariableLinearRealArithmeticConstraint makeSimplification(ArrayList<Expression> positiveNormalizedAtoms, ArrayList<Expression> negativeNormalizedAtoms, List<Expression> externalLiterals) {
		// no special bookkeeping to be retained in simplifications, so we just make a new constraint.
		SingleVariableLinearRealArithmeticConstraint result = new SingleVariableLinearRealArithmeticConstraint(getVariable(), positiveNormalizedAtoms, negativeNormalizedAtoms, externalLiterals, getPropagateAllLiteralsWhenVariableIsBound(), getConstraintTheory());
		return result;
	}

	@Override
	protected Expression isolateVariable(Expression atom, Context context) {
		Expression result = LinearRealArithmeticUtil.isolateVariable(atom, getVariable());
		return result;
	}

	@Override
	/**
	 * Returns empty-range iterator; only implicit literals in these theories
	 * are from bounds, but those result in <i>negative</i> normalized implicit atoms, not positive.
	 */
	protected Iterator<Expression> getImplicitPositiveNormalizedAtomsIterator(Context context) {
		return iterator();
	}

	List<Expression> cachedImplicitNegativeNormalizedAtoms;
	@Override
	/**
	 * Returns iterator ranging over implicit normalized atoms representing variable bounds.
	 */
	protected Iterator<Expression> getImplicitNegativeNormalizedAtomsIterator(Context context) {
		
		// TODO: revise this method for linear real arithmetic
		
		if (cachedImplicitNegativeNormalizedAtoms == null) {
			RealInterval interval = getType(context);
			
			Expression lowerBound = interval.getLowerBound();
			String lowerBoundOperator = interval.lowerBoundIsOpen()? LESS_THAN : LESS_THAN_OR_EQUAL_TO;

			Expression upperBound = interval.getUpperBound();
			String upperBoundOperator = interval.upperBoundIsOpen()? GREATER_THAN : GREATER_THAN_OR_EQUAL_TO;

			cachedImplicitNegativeNormalizedAtoms = list();
			if (!lowerBound.equals("unknown") && !lowerBound.equals(UnaryMinus.make(INFINITY))) {
				cachedImplicitNegativeNormalizedAtoms.add(apply(lowerBoundOperator, getVariable(), lowerBound));
				// this is the negation of variable >= nonStrictLowerBound. We need to use a negative normalized atom because applications of >= are not considered normalized atoms
			}
			if (!upperBound.equals("unknown") && !upperBound.equals(INFINITY)) {
				cachedImplicitNegativeNormalizedAtoms.add(apply(upperBoundOperator, getVariable(), upperBound));
				// this is the negation of variable <= nonStrictUpperBound. We need to use a negative normalized atom because applications of <= are not considered normalized atoms
			}
		}
		return cachedImplicitNegativeNormalizedAtoms.iterator();
	}

	private RealInterval cachedType;
	
	/**
	 * Returns the {@link IntegerInterval} type of the constraint's variable.
	 * @param context
	 * @return
	 */
	public RealInterval getType(Context context) {
		if (cachedType == null) {
			Type type = context.getType(getVariableTypeExpression(context));
			if (type instanceof RealExpressoType) {
				cachedType = new RealInterval("]-infinity;infinity[");
				// represents Real as real interval for uniformity
			}
			else {
				cachedType = (RealInterval) type;
			}
		}
		return cachedType ;
	}
}