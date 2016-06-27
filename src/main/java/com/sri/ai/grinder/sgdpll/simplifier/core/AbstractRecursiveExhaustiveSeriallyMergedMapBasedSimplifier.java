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
package com.sri.ai.grinder.sgdpll.simplifier.core;

import java.util.Map;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.api.Context;
import com.sri.ai.grinder.sgdpll.interpreter.AbstractCommonInterpreter;
import com.sri.ai.grinder.sgdpll.simplifier.api.MapBasedSimplifier;
import com.sri.ai.grinder.sgdpll.simplifier.api.Simplifier;

/**
 * A {@link MapBasedSimplifier} based on the elementary simplifiers provided by abstract methods
 * {@link #makeFunctionApplicationSimplifiers()},
 * the ones provided by {@link #makeSyntacticFormTypeSimplifiers()}, and another {@link MapBasedSimplifier}
 * provided by abstract method {@link #makeAnotherMapBasedSimplifier()}.
 * The simplifiers in the latter are supplemented, not overridden, by the simplifiers in the first two to create
 * an overriding effect, 
 * in the sense that the new ones only act once the older one has nothing else to say about the expression
 * (that is, returns the same instance).
 * <p>
 * This is very similar to {@link RecursiveExhaustiveSeriallyMergedMapBasedSimplifier},
 * but receives its elementary simplifiers through implementations of abstract methods,
 * as opposed to through a constructor.
 * This makes its usage a bit more laborious but also more flexible because
 * its extending classes can provide simplifiers based on the extending class' own abstract methods.
 * This is used by {@link AbstractCommonInterpreter} to leave the treatment of quantified expressions
 * open, according to an abstract method.
 *
 * @author braz
 *
 */
@Beta
public abstract class AbstractRecursiveExhaustiveSeriallyMergedMapBasedSimplifier implements MapBasedSimplifier {

	private SeriallyMergedMapBasedTopSimplifier topSimplifier;
	private Simplifier simplifier;
	
	public AbstractRecursiveExhaustiveSeriallyMergedMapBasedSimplifier() {
		// do not invoke initializeSimplifiers() here,
		// because this would invoke the implementation of abstract methods extending classes
		// before their constructors were done running.
	}

	/**
	 * This must be invoked after construction so that extending classes constructions
	 * have a chance to run before abstract methods are invoked.
	 */
	private void initializeSimplifiers() {
		this.topSimplifier = 
				new SeriallyMergedMapBasedTopSimplifier(
						makeFunctionApplicationSimplifiers(), 
						makeSyntacticFormTypeSimplifiers(), 
						makeAnotherMapBasedSimplifier());
		this.simplifier = new Recursive(new TopExhaustive(this.topSimplifier));
	}
	
	@Override
	public SeriallyMergedMapBasedTopSimplifier getTopSimplifier() {
		if (topSimplifier == null) {
			initializeSimplifiers();
		}
		return topSimplifier;
	}

	public Simplifier getSimplifier() {
		if (simplifier == null) {
			initializeSimplifiers();
		}
		return simplifier;
	}

	@Override
	public Map<String, Simplifier> getFunctionApplicationSimplifiers() {
		return getTopSimplifier().getFunctionApplicationSimplifiers();
	}

	@Override
	public Map<String, Simplifier> getSyntacticFormTypeSimplifiers() {
		return getTopSimplifier().getSyntacticFormTypeSimplifiers();
	}

	@Override
	public Expression apply(Expression expression, Context context) {
		Expression result = getSimplifier().apply(expression, context);
		return result;
	}

	abstract public Map<String, Simplifier> makeFunctionApplicationSimplifiers();

	abstract public Map<String, Simplifier> makeSyntacticFormTypeSimplifiers();

	abstract public MapBasedSimplifier makeAnotherMapBasedSimplifier();
}