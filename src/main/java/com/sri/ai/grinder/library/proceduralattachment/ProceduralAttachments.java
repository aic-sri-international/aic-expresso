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
package com.sri.ai.grinder.library.proceduralattachment;

import java.util.List;

import com.google.common.annotations.Beta;
import com.google.common.base.Function;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.api.Context;
import com.sri.ai.grinder.rewriter.api.TopRewriter;
import com.sri.ai.grinder.rewriter.core.CombiningTopRewriter;
import com.sri.ai.grinder.rewriter.core.FirstOf;
import com.sri.ai.grinder.rewriter.core.FunctorSwitch;
import com.sri.ai.grinder.rewriter.core.SymbolSwitch;
import com.sri.ai.util.base.NullaryFunction;

/**
 * A class with a method for registering procedural attachments in a context
 * and another for obtaining a {@link TopRewriter} with all procedural attachments.
 * @author braz
 *
 */
@Beta
public class ProceduralAttachments {
	
	private static final String PROCEDURAL_ATTACHEMENTS_KEY = "Procedural attachment rewriter key";
	private static NullaryFunction<TopRewriter> EMPTY_REWRITER_MAKER = () -> new FirstOf();
	
	public static TopRewriter getProceduralAttachmentsTopRewriter(Context context) {
		TopRewriter result = (TopRewriter) context.getInplaceGlobalObject(PROCEDURAL_ATTACHEMENTS_KEY);
		return result;
	}
	
	private static Function<TopRewriter, TopRewriter> combine(TopRewriter proceduralAttachment) {
		return 
				proceduralAttachments 
				-> new CombiningTopRewriter(proceduralAttachments, proceduralAttachment);
	}

	/**
	 * This method registers a procedural attachment top rewriter to a function with a given arity;
	 * the check of functor and arity will be added to the given top rewriter, so it does not need to do that.
	 * @param functor
	 * @param proceduralAttachmentParameters
	 * @param topRewriter
	 * @param context
	 * @return
	 */
	public static void registerProceduralAttachment(
			Expression functor, int arity, TopRewriter topRewriter, Context context) {

		TopRewriter proceduralAttachment = new FunctorSwitch(functor.toString(), arity, topRewriter);

		context.updateInplaceGlobalObject(PROCEDURAL_ATTACHEMENTS_KEY, EMPTY_REWRITER_MAKER, combine(proceduralAttachment));
	}

	/**
	 * Same as {@link #registerProceduralAttachment(Expression, List, TopRewriter, Context)},
	 * but for a symbol.
	 * @param symbol
	 * @param topRewriter
	 * @param context
	 * @return
	 */
	public static void registerProceduralAttachment(
			Expression symbol, TopRewriter topRewriter, Context context) {

		TopRewriter proceduralAttachment = new SymbolSwitch(symbol.toString(), topRewriter);

		context.updateInplaceGlobalObject(PROCEDURAL_ATTACHEMENTS_KEY, EMPTY_REWRITER_MAKER, combine(proceduralAttachment));
	}
}