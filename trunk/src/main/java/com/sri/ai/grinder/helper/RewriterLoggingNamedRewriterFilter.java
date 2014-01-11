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

import java.util.HashSet;
import java.util.Set;

import com.google.common.annotations.Beta;
import com.sri.ai.grinder.GrinderConfiguration;

import ch.qos.logback.classic.spi.ILoggingEvent;
import ch.qos.logback.core.filter.Filter;
import ch.qos.logback.core.spi.FilterReply;

/**
 * A logback filter for removing named Rewriter output.
 * 
 * @author oreilly
 *
 */
@Beta
public class RewriterLoggingNamedRewriterFilter extends Filter<ILoggingEvent> {

	private Set<String> rewritersToFilter = new HashSet<String>();

	public RewriterLoggingNamedRewriterFilter() {
		String rewriterNames = GrinderConfiguration
				.getFilterOutLoggingByRewritersNamed();
		String[] namesToFilter = rewriterNames.split(":");
		for (String rewriterName : namesToFilter) {	
			if (!rewriterName.trim().equals("")) {
				rewritersToFilter.add(rewriterName.trim());
			}
		}
	}
	
	public boolean isRewriterFiltered(String rewriterName) {
		boolean result = rewritersToFilter.contains(rewriterName);
		
		return result;
	}

	@Override
	public FilterReply decide(ILoggingEvent event) {
		if (rewritersToFilter
				.contains(RewriterLogging.getCurrentRewriterName())) {	
			return FilterReply.DENY;
		} 
		else {
			return FilterReply.NEUTRAL;
		}
	}
}
