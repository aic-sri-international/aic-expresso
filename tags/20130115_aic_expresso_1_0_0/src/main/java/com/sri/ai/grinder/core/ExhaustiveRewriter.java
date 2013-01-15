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
package com.sri.ai.grinder.core;

import java.util.Iterator;
import java.util.List;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.api.Library;
import com.sri.ai.grinder.api.Rewriter;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.util.Util;

/**
 * A Rewriter that exhaustively applies a base rewriter until no changes occur
 * any more. The default base rewriter is a {@link RewriteOnce}.
 * 
 * @author braz
 */
@Beta
public class ExhaustiveRewriter extends AbstractRewriter {

	private int counter    = 0;
	private int maxCounter = -1;
	
	private Rewriter baseRewriter;

	public ExhaustiveRewriter(Rewriter baseRewriter) {
		this(baseRewriter, -1);
	}
	
	public ExhaustiveRewriter(Rewriter baseRewriter, int maxCounter) {
		super();
		this.baseRewriter = baseRewriter;
		this.maxCounter   = maxCounter;
	}

	/**
	 * Uses a {@link RewriteOnce} built on given list of rewriters.
	 */
	public ExhaustiveRewriter(List<Rewriter> rewriters) {
		this(new RewriteOnce(rewriters));
	}

	public ExhaustiveRewriter(Library library) {
		this(new RewriteOnce(library));
	}

	public ExhaustiveRewriter(Library library, int maxCount) {
		this(new RewriteOnce(library), maxCount);
	}

	//
	// START-Rewriter
	@Override
	/**
	 * Returns an iterator ranging over the base rewriter, the only child of ExhaustiveRewriter.
	 */
	public Iterator<Rewriter> getChildrenIterator() {
		return Util.iterator(baseRewriter);
	}
	
	// END-Rewriter
	//
	@Override
	public Expression rewriteAfterBookkeeping(Expression expression, RewritingProcess process) {
		Expression previous = null;
		Expression current  = expression;
		do {
			previous = current;
			current  = baseRewriter.rewrite(previous, process);
			if (current != previous) {
				counter++;
			}
		} while (current != previous && (maxCounter == -1 || counter < maxCounter));
		
		return current;
	}
}
