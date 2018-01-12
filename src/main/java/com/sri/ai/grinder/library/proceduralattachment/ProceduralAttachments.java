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

import com.google.common.annotations.Beta;
import com.google.common.base.Function;
import com.sri.ai.grinder.api.Context;
import com.sri.ai.grinder.rewriter.api.TopRewriter;
import com.sri.ai.grinder.rewriter.core.CombiningTopRewriter;
import com.sri.ai.grinder.rewriter.core.FirstOf;
import com.sri.ai.util.base.NullaryFunction;

/**
 * A class with a method for registering procedural attachments in a context
 * and another for obtaining a {@link TopRewriter} with all procedural attachments.
 * @author braz
 *
 */
@Beta
public class ProceduralAttachments {
	
	private static String KEY = "Procedural attachment rewriters key";
	private static NullaryFunction<TopRewriter> EMPTY_REWRITER_MAKER = () -> new FirstOf();
	
	public static TopRewriter getProceduralAttachmentsRewriter(Context context) {
		TopRewriter result = (TopRewriter) context.getGlobalObject(KEY);
		return result;
	}
	
	public static Context newContextWithNewProceduralAttachment(Context context, TopRewriter proceduralAttachmentRewriter) {
		Function<TopRewriter, TopRewriter> update = makeUpdate(proceduralAttachmentRewriter);
		Context result = (Context) context.updateGlobalObject(KEY, EMPTY_REWRITER_MAKER, update);
		return result;
	}

	private static Function<TopRewriter, TopRewriter> makeUpdate(TopRewriter proceduralAttachment) {
		return 
				proceduralAttachments 
				-> new CombiningTopRewriter(proceduralAttachments, proceduralAttachment);
	}
}