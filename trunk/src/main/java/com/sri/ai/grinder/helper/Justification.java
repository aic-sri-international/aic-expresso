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
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.util.log.LogX;
import com.sri.ai.util.log.LogXFactory;

/**
 * Helper class for obtaining and calling methods statically on the LogX
 * instance to be used exclusively by Rewriters for outputting high level human
 * readable justifications in a formal proof format, detailing how the
 * expressions are being rewritten. An example of how this kind of proof should
 * look is as follows: <br>
 * 
 * <pre>
 * belief([p(X)]) 
 * = (definition of belief) 
 * normalize(product( {{ (on F in Neigh([p(X)])) message to [p(X)] from F }} ) )
 * = (neighbors of random variable) 
 * ...
 * </pre>
 * 
 * This is done in the following way:
 * 
 * First, we use {@link #current(Expression)} to show the current expression. This is typically used at the start of sequence of manipulations.
 * Then, we use {@link #beginStep(String justification)} to indicate that a manipulation is doing to start by providing its description.
 * After that we perform the computations needed. They may involve nested steps, that will show in a nested way in the output.
 * When the result of the manipulation is obtained, we indicate it with {@link #endStep(Object result)}.
 * 
 * Nested steps will typically reproduce the stages above, starting with their own call to {@link #current(Expression)};
 * 
 * If arguments being passed to these methods are expensive to compute, it is advisable to test whether
 * justifications are enabled with {@link #isEnabled()} first.
 * 
 * There are versions of the methods above accepting a {@link Marker} argument.

 * @see LogX
 * 
 * @author oreilly
 * 
 */
@Beta
public class Justification extends RewriterLogging {

	private static final LogX _defaultRewriterJustification;
	static {
		_defaultRewriterJustification = LogXFactory.getLogX(
				getDefaultLoggerName(), Justification.class.getName());

		_defaultRewriterJustification.setIndentIndentationContent(false);
	}

	/**
	 * 
	 * @return the default LogX to be used by rewriters for justification
	 *         purposes.
	 */
	public static LogX getDefaultLogX() {
		return _defaultRewriterJustification;
	}

	/**
	 * 
	 * @return the name of the Logger used by the default LogX for rewriters for
	 *         justification purposes.
	 */
	public static String getDefaultLoggerName() {
		return Justification.class.getName();
	}

	/**
	 * 
	 * @return the current justification level.
	 */
	public static int getJustificationLevel() {
		return _defaultRewriterJustification.getTraceLevel();
	}

	/**
	 * Set the justification level to a specified value. Should normally only be
	 * called if getJustificationLevel() was called and an exception was thrown
	 * while performing rewriting.
	 * 
	 * @param level
	 *            the justification level the RewriterTrace should be set to.
	 */
	public static void setJustificationLevel(int level) {
		_defaultRewriterJustification.setTraceLevel(level);
	}

	/**
	 * 
	 * @return true of Rewriter Justifications are enabled and should be written
	 *         out.
	 */
	public static boolean isEnabled() {
		return getDefaultLogX().isTraceEnabled();
	}

	/**
	 * Start a sequence of justification statements at the same level.
	 */
	public static void current(String format, Object... args) {
		current(null, format, args);
	}
	
	/**
	 * Start a sequence of justification statements at the same level.
	 */
	public static void current(Expression lhs) {
		current(null, lhs);
	}
	
	public static void beginStep(String justification) {
		beginStep(null, justification);
	}
	
	public static void endStep(Object rhs) {
		endStep(null, rhs);
	}
	
	public static void current(Marker marker, String format, Object... args) {
		if (isEnabled()) {
			getDefaultLogX().trace(marker, format, args);
		}
	}

	public static void current(Marker marker, Expression lhs) {
		if (isEnabled()) {
			getDefaultLogX().trace(marker, "{}", lhs);
		}
	}

	public static void beginStep(Marker marker, String justification) {
		if (isEnabled()) {
			getDefaultLogX().trace(marker, "= (" + justification + ")");
			getDefaultLogX().setTraceLevel(getDefaultLogX().getTraceLevel() + 1);
		}
	}

	public static void endStep(Marker marker, Object rhs) {
		if (isEnabled()) {
			getDefaultLogX().setTraceLevel(getDefaultLogX().getTraceLevel() - 1);
			getDefaultLogX().trace(marker, "{}", rhs);
		}
	}
}
