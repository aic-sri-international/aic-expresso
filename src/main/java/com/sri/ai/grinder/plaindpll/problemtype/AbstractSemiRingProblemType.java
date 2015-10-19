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
package com.sri.ai.grinder.plaindpll.problemtype;

import static com.sri.ai.util.Util.myAssert;

import java.util.List;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.IndexExpressionsSet;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.plaindpll.api.GroupProblemType;
import com.sri.ai.grinder.plaindpll.api.SemiRingProblemType;
import com.sri.ai.grinder.plaindpll.group.AssociativeCommutativeSemiRing;
import com.sri.ai.util.base.Pair;

/**
 * An abstract implementation of {@link SemiRingProblemType} based on an internal
 * {@link AbstractGroupProblemType}
 * (whose group must be an instance of {@link AssociativeCommutativeSemiRing} or an error is thrown).
 * <p>
 * This allows a simulation of multiple inheritance of methods from both semi ring implementations
 * and group problem type implementations.
 * The alternative to having a class such as this would be to extend {@link SemiRingProblemType} implementation
 * from either {@link AssociativeCommutativeSemiRing} or {@link GroupProblemType} classes, but
 * either of those would have to re-define the methods of the other.
 * 
 * @author braz
 *
 */
public abstract class AbstractSemiRingProblemType implements SemiRingProblemType {

	private AbstractGroupProblemType groupProblemType;
	
	/**
	 * Takes a {@link AbstractGroupProblemType} whose {@link AbstractGroupProblemType#getGroup()}
	 * object can be dynamically cast to an {@link AssociativeCommutativeSemiRing},
	 * an an error is thrown. 
	 * @param groupProblemTypeDefinedOnASemiRing
	 */
	public AbstractSemiRingProblemType(AbstractGroupProblemType groupProblemTypeDefinedOnASemiRing) {
		myAssert(
				() -> groupProblemTypeDefinedOnASemiRing.getGroup() instanceof AssociativeCommutativeSemiRing,
				() -> AbstractSemiRingProblemType.class + " constructor requires an " + AbstractGroupProblemType.class + " object " + 
				" defined on a semi-ring group (that is, its method getGroup() must provide an object that can be dynamically cast " +
				" to a " + AssociativeCommutativeSemiRing.class + " instance");
		this.groupProblemType = groupProblemTypeDefinedOnASemiRing;
	}
	
	public AssociativeCommutativeSemiRing getSemiRing() {
		return (AssociativeCommutativeSemiRing) groupProblemType.getGroup();
	}

	@Override
	public String multiplicativeFunctor() {
		return getSemiRing().multiplicativeFunctor();
	}

	@Override
	public Expression multiplicativeIdentityElement() {
		return getSemiRing().multiplicativeIdentityElement();
	}

	@Override
	public Expression multiplicativeAbsorbingElement() {
		return getSemiRing().multiplicativeAbsorbingElement();
	}

	@Override
	public List<Expression> getFactors(Expression expression) {
		return getSemiRing().getFactors(expression);
	}

	@Override
	public Expression multiply(Expression multiplication, RewritingProcess process) {
		return getSemiRing().multiply(multiplication, process);
	}

	@Override
	public Expression getNthRoot(int n, Expression expression) {
		return getSemiRing().getNthRoot(n, expression);
	}

	@Override
	public Expression multiplyNTimes(Expression value, Expression n, RewritingProcess process) {
		return getSemiRing().multiplyNTimes(value, n, process);
	}

	@Override
	public Expression additiveIdentityElement() {
		return getSemiRing().additiveIdentityElement();
	}

	@Override
	public boolean isAdditiveAbsorbingElement(Expression value) {
		return getSemiRing().isAdditiveAbsorbingElement(value);
	}

	@Override
	public Expression add(Expression value1, Expression value2, RewritingProcess process) {
		return getSemiRing().add(value1, value2, process);
	}

	@Override
	public Expression addNTimes(Expression value, Expression n, RewritingProcess process) {
		return getSemiRing().addNTimes(value, n, process);
	}

	@Override
	public boolean isIdempotent() {
		return getSemiRing().isIdempotent();
	}

	@Override
	public Pair<Expression, IndexExpressionsSet> getExpressionAndIndexExpressionsFromRewriterProblemArgument(Expression expression, RewritingProcess process) {
		return groupProblemType.getExpressionAndIndexExpressionsFromRewriterProblemArgument(expression, process);
	}

	@Override
	public Expression makeProblemExpression(Expression index, Expression indexType, Expression constraint, Expression body) {
		return groupProblemType.makeProblemExpression(index, indexType, constraint, body);
	}
}
