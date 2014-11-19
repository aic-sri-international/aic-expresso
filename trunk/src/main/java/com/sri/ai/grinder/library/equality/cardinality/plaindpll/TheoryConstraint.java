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
package com.sri.ai.grinder.library.equality.cardinality.plaindpll;

import java.util.Collection;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.api.RewritingProcess;

/**
 * An interface for theory-specific representations of the current constraint in DPLL.
 * 
 * @author braz
 *
 */
@Beta
public interface TheoryConstraint {
	/**
	 * Provides a splitter needed toward state
	 * for which a model count can be computed in polynomial time, or null if it is already in such a state.
	 */
	Expression pickSplitter(Collection<Expression> indices, RewritingProcess process);
	
	/**
	 * Generates new constraint representing conjunction of this constraint and given splitter.
	 */
	TheoryConstraint applySplitter(Expression splitter, Collection<Expression> indices, RewritingProcess process);

	/**
	 * Generates new constraint representing conjunction of this constraint and the negation of given splitter.
	 */
	TheoryConstraint applySplitterNegation(Expression splitter, Collection<Expression> indices, RewritingProcess process);

	/**
	 * Returns which index gets bound by the assertion of a splitter, or <code>null</code> if none.
	 * Note: this method is "static in spirit", as it should not depend on the actual state of the instance.
	 * This is so because static methods cannot be overridden in Java.
	 * @param splitter
	 * @param indices
	 * @return
	 */
	Expression getIndexBoundBySplitterIfAny(Expression splitter, Collection<Expression> indices);
	
	/**
	 * Returns which index gets bound by the negation of a splitter, or <code>null</code> if none.
	 * Note: this method is "static in spirit", as it should not depend on the actual state of the instance.
	 * This is so because static methods cannot be overridden in Java.
	 * @param splitter
	 * @return
	 */
	Expression getIndexBoundBySplitterNegationIfAny(Expression splitter, Collection<Expression> indices);
	
	/**
	 * Computes model count for constraint, given a set of indices, in polynomial time.
	 * Assumes that {@link #pickSplitter(Collection, RewritingProcess)} returns <code>null</code>,
	 * that is, the constraint is in such a state and context that allows the determination of a unique model count.
	 */
	Expression modelCount(Collection<Expression> indices, RewritingProcess process);

	/**
	 * Given a splitter candidate, returns the same, or another, splitter
	 * guaranteed to be satisfiable under the constraint, no matter what assignment to free variables.
	 * This is also required to make any splitter candidate have this property itself after a finite number of invocations.
	 * @param splitterCandidate
	 * @param indices
	 * @param process
	 * @return
	 */
	Expression getMostRequiredSplitter(Expression splitterCandidate, Collection<Expression> indices, RewritingProcess process);
}