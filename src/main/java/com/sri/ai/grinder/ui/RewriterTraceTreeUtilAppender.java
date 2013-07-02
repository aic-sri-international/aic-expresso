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
package com.sri.ai.grinder.ui;

import ch.qos.logback.classic.spi.ILoggingEvent;

import com.google.common.annotations.Beta;
import com.sri.ai.util.log.LogX;

/**
 * 
 * @author oreilly
 *
 */
@Beta
public class RewriterTraceTreeUtilAppender extends BaseTreeUtilAppender {
	//
	protected int currentIndentLevel = 0;
	//
	private boolean firstTime = true;

	public RewriterTraceTreeUtilAppender() {
		super();
		currentIndentLevel = 0;
	}

	@Override
	protected void append(ILoggingEvent eventObject) {
		if (TreeUtil.isShowing()) {

			String msg = eventObject.getFormattedMessage();
			Object[] args = eventObject.getArgumentArray();

			int indentLevel = LogX.getTraceLevel(eventObject.getLoggerName());

			while (indentLevel > currentIndentLevel) {
				if (!firstTime) {
					TreeUtil.addTrace(">>");
				}
				TreeUtil.startTraceLevel();
				currentIndentLevel++;
			}
			
			firstTime = false;

			while (indentLevel < currentIndentLevel) {
				TreeUtil.endTraceLevel();
				currentIndentLevel--;
			}
			
			// Suffix the profiler information to the message
			// if available.
			String profileString = "";
			Long profileInfo = LogX.getProfileInfo(eventObject.getLoggerName());
			if (profileInfo != null) {
				profileString = "[" + profileInfo/1000000 + "ms]";
			}

			if (msg != null && !msg.equals("") && outputFormattedMessage(msg, args)) {
				TreeUtil.addTrace(profileString + " " + msg);
			}

			if (args != null) {
				for (Object arg : args) {
					TreeUtil.addTrace(arg);
				}
			}
		}
	}
}
