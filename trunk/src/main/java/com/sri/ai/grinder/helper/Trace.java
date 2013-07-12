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

import org.slf4j.Marker;

import com.google.common.annotations.Beta;
import com.sri.ai.util.log.LogX;
import com.sri.ai.util.log.LogXFactory;

/**
 * Helper class for obtaining and calling methods statically on the LogX
 * instance to be used exclusively by Rewriters for outputing a human readable
 * trace of their execution.
 * 
 * @see LogX
 * 
 * @author oreilly
 * 
 */
@Beta
public class Trace extends RewriterLogging {

	private static final LogX _defaultRewriterTrace;
	static {
		_defaultRewriterTrace = LogXFactory.getLogX(
			getDefaultLoggerName(), Trace.class.getName());
	}
	
	/**
	 * 
	 * @return the default LogX to be used by Rewriters.
	 */
	public static LogX getDefaultLogX() {
		return _defaultRewriterTrace;
	}

	/**
	 * 
	 * @return the name of the Logger used by the default LogX for rewriters.
	 */
	public static String getDefaultLoggerName() {
		return Trace.class.getName();
	}
	
	/**
	 * 
	 * @return true if Rewriter Traces are enabled and should be written
	 *         out.
	 */
	public static boolean isEnabled() {
		return getDefaultLogX().isTraceEnabled();
	}

	/**
	 * 
	 * @return the current trace level for RewriterTrace.
	 */
	public static int getTraceLevel() {
		return _defaultRewriterTrace.getTraceLevel();
	}

	/**
	 * Set the trace level to a specified value. Should normally only be called
	 * if getTraceLevel() was called and an exception was thrown while
	 * performing rewriting.
	 * 
	 * @param level
	 *            the level the RewriterTrace should be set to.
	 */
	public static void setTraceLevel(int level) {
		_defaultRewriterTrace.setTraceLevel(level);
	}

	/**
	 * 
	 * @see LogX#in
	 */
	public static void in(String msg, Object... args) {
		in(null, msg, args);
	}
	
	/**
	 * 
	 * @see LogX#in
	 */
	public static void in(Marker marker, String msg, Object... args) {
		getDefaultLogX().indent(marker, msg, args);
	}

	/**
	 * 
	 * @see LogX#out
	 */
	public static void out(String msg, Object... args) {
		out(null, msg, args);
	}
	
	/**
	 * 
	 * @see LogX#out
	 */
	public static void out(Marker marker, String msg, Object... args) {
		getDefaultLogX().outdent(marker, msg, args);
	}

	/**
	 * 
	 * @see LogX#log
	 */
	public static void log(String msg, Object... args) {
		log(null, msg, args);
	}
	
	/**
	 * 
	 * @see LogX#log
	 */
	public static void log(Marker marker, String msg, Object... args) {
		getDefaultLogX().trace(marker, msg, args);
	}
}
