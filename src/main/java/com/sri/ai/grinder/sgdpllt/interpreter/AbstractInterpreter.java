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
package com.sri.ai.grinder.sgdpllt.interpreter;

import com.google.common.annotations.Beta;
import com.sri.ai.grinder.sgdpllt.api.MultiQuantifierEliminator;
import com.sri.ai.grinder.sgdpllt.library.boole.ForAllRewriter;
import com.sri.ai.grinder.sgdpllt.library.boole.ThereExistsRewriter;
import com.sri.ai.grinder.sgdpllt.library.number.MaxRewriter;
import com.sri.ai.grinder.sgdpllt.library.number.ProductRewriter;
import com.sri.ai.grinder.sgdpllt.library.number.SummationRewriter;
import com.sri.ai.grinder.sgdpllt.library.set.CardinalityByBruteForce;
import com.sri.ai.grinder.sgdpllt.rewriter.api.Rewriter;
import com.sri.ai.grinder.sgdpllt.rewriter.api.TopRewriter;
import com.sri.ai.grinder.sgdpllt.rewriter.core.Exhaustive;
import com.sri.ai.grinder.sgdpllt.rewriter.core.Recursive;
import com.sri.ai.grinder.sgdpllt.rewriter.help.RedirectingRewriter;

/**
 * A {@link Rewriter} based on a {@link TopRewriter},
 * adding the functionality of solving quantified and aggregate expressions with a {@link MultiQuantifierEliminator}
 * provided by the implementation of an abstract method.
 *
 * @author braz
 *
 */
@Beta
public abstract class AbstractInterpreter extends RedirectingRewriter {

	/**
	 * Must provide a {@link MultiQuantifierEliminator} based on given {@link TopRewriterUsingContextAssignments}.
	 * @param topRewriterUsingContextAssignments
	 * @return
	 */
	abstract protected MultiQuantifierEliminator makeQuantifierEliminator(TopRewriterUsingContextAssignments topRewriterUsingContextAssignments);
	
	public AbstractInterpreter() {
	}
	
	public void setBaseTopRewriter(TopRewriter baseTopRewriter) {
		Rewriter result = new Recursive(new Exhaustive(new TopRewriterUsingQuantifierEliminatorAndContextAssignments(baseTopRewriter)));
		setBaseRewriter(result);
	}
	
	protected class TopRewriterUsingQuantifierEliminatorAndContextAssignments extends TopRewriterUsingContextAssignments {

		private TopRewriter originalBaseTopRewriter;
		
		public TopRewriterUsingQuantifierEliminatorAndContextAssignments(TopRewriter baseTopRewriter) {
			super();
			this.originalBaseTopRewriter = baseTopRewriter;
			MultiQuantifierEliminator quantifierEliminator = makeQuantifierEliminator(this);
			setBaseRewriter(
					TopRewriter.merge(
							baseTopRewriter,

							new SummationRewriter(quantifierEliminator),
							new ProductRewriter(quantifierEliminator),
							new MaxRewriter(quantifierEliminator),

							new ThereExistsRewriter(quantifierEliminator),
							new ForAllRewriter(quantifierEliminator),

							new CardinalityByBruteForce(quantifierEliminator)
							));
		}
		
		@Override
		public String toString() {
			return "TopRewriter adding quantifier eliminators and use of context assignments to original top base rewriter " + originalBaseTopRewriter;
		}
	}
}