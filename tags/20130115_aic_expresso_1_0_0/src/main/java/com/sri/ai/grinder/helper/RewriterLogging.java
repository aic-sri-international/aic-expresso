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
package com.sri.ai.grinder.helper;

import org.slf4j.MDC;
import org.slf4j.Marker;
import org.slf4j.MarkerFactory;

import com.google.common.annotations.Beta;

/**
 * An abstract base class for common utilities for outputting different types of
 * logging messages associated with rewriters.
 * 
 * @author oreilly
 * 
 */
@Beta
public abstract class RewriterLogging {
	public static final Marker REWRITER_PROFILE_INFO = MarkerFactory.getMarker("RewriterLogging:ProfileInfo");
	//
	private static final String MDC_KEY_UUID                                  = "RewriterLogging:UUID";
	private static final String MDC_KEY_REWRITER_NAME                         = "RewriterLogging:Name";
	
	/**
	 * 
	 * @return the MDC Key used to store and retrieve the uuid assocaited with
	 *         rewriter logging.
	 */
	public static String getMDCUUIDKey() {
		return MDC_KEY_UUID;
	}
	
	/**
	 * 
	 * @return the universal unique identifier for the rewrter logging on this
	 *         thread.
	 */
	public static String getUUID() {
		return MDC.get(MDC_KEY_UUID);
	}
	
	/**
	 * Set the universal unique id to be associated with rewriter logging
	 * statements on this thread.
	 * 
	 * @param uuid
	 *            the uuid to be set.
	 */
	public static void setUUID(String uuid) {
		MDC.put(MDC_KEY_UUID, uuid);
	}
	
	/**
	 * 
	 * @return The name of the rewriter to last be called within the current
	 *         thread.
	 */
	public static String getCurrentRewriterName() {
		String rewriterName = MDC.get(MDC_KEY_REWRITER_NAME);

		return rewriterName;
	}
	
	/**
	 * Set the current rewriter name on the current thread.
	 * 
	 * @param rewriterName
	 *            the current rewriter name to use.
	 * @return the previous rewriter name that had been set on the current
	 *         thread.
	 */
	public static String setCurrentRewriterName(String rewriterName) {
		String previousRewriterName = MDC.get(MDC_KEY_REWRITER_NAME);
		if (previousRewriterName == null) {
			previousRewriterName = "";
		}
		MDC.put(MDC_KEY_REWRITER_NAME, rewriterName);
		return previousRewriterName;
	}
}
