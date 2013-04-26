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
package com.sri.ai.grinder;

import com.google.common.annotations.Beta;
import com.sri.ai.grinder.library.equality.sat.EqualityLogicSATSolver;
import com.sri.ai.util.Configuration;
import com.sri.ai.util.cache.CacheMap;

/**
 * Configuration information for the classes in the grinder package.
 * 
 * @author oreilly
 *
 */
@Beta
public class GrinderConfiguration extends Configuration {
	//
	public static final String  KEY_DISPLAY_TREE_UTIL_UI                                        = "grinder.display.tree.util.ui";
	public static final Boolean DEFAULT_VALUE_DISPLAY_TREE_UTIL_UI                              = Boolean.TRUE;
	//
	public static final String  KEY_WAIT_UNTIL_UI_CLOSED_ENABLED                                = "grinder.wait.until.ui.closed.enabled";
	public static final Boolean DEFAULT_VALUE_WAIT_UNTIL_UI_CLOSED_ENABLED                      = Boolean.FALSE;
	//
	public static final String  KEY_ASSUME_DOMAIN_ALWAYS_LARGE                                  = "grinder.cardinalilty.assume.domains.always.large";
	public static final Boolean DEFAULT_VALUE_ASSUME_DOMAIN_ALWAYS_LARGE                        = Boolean.FALSE;
	//
	public static final String  KEY_COMPLETE_SIMPLIFY_USE_SAT_SOLVER_SOLVER                     = "grinder.complete.simplify.use.sat.solver";
	public static final Boolean DEFAULT_VALUE_COMPLETE_SIMPLIFY_USE_SAT_SOLVER                  = Boolean.FALSE;
	//
	public static final String  KEY_DEFAULT_SAT_SOLVER_CLASS                                    = "grinder.default.sat.solver.class";
	public static final String  DEFAULT_VALUE_SAT_SOLVER_CLASS                                  = EqualityLogicSATSolver.class.getName();
	//
	public static final String  KEY_REPLACE_VARIABLE_WITH_CONSTANT_IT_IS_BOUND_TO               = "grinder.replace.variable.by.constant.it.is.bound.to";
	public static final Boolean DEFAULT_VALUE_REPLACE_VARIABLE_WITH_CONSTANT_IT_IS_BOUND_TO     = Boolean.TRUE;
	//
	public static final String  KEY_REWRITE_DEAD_ENDS_CACHE_MAXIMUM_SIZE                        = "grinder.rewrite.dead.ends.cache.maximum.size";
	public static final Long    DEFAULT_VALUE_REWRITE_DEAD_ENDS_CACHE_MAXIMUM_SIZE              = 100L;
	//
	public static final String  KEY_REWRITE_DEAD_ENDS_CACHE_GARBAGE_COLLECTION_PERIOD           = "grinder.rewrite.dead.ends.cache.garbage.collection.period";
	public static final Integer DEFAULT_VALUE_REWRITE_DEAD_ENDS_CACHE_GARBAGE_COLLECTION_PERIOD = CacheMap.NO_GARBAGE_COLLECTION; //new Integer(10000);
	//
	public static final String  KEY_REWRITING_PROCESS_CACHE_MAXIMUM_SIZE                        = "grinder.rewriting.process.cache.maximum.size";
	public static final Long    DEFAULT_VALUE_REWRITING_PROCESS_CACHE_MAXIMUM_SIZE              = 100L;
	//
	public static final String  KEY_REWRITING_PROCESS_CACHE_GARBAGE_COLLECTION_PERIOD           = "grinder.rewriting.process.cache.garbage.collection.period";
	public static final Integer DEFAULT_VALUE_REWRITING_PROCESS_CACHE_GARBAGE_COLLECTION_PERIOD = CacheMap.NO_GARBAGE_COLLECTION; //new Integer(5000);
	// A colon separated string of rewrites who should have their output excluded.
	public static final String  KEY_FILTER_OUT_LOGGING_BY_REWRITERS_NAMED                       = "grinder.rewriting.filter.out.logging.by.rewriters.named";
	public static final String  DEFAULT_VALUE_FILTER_OUT_LOGGING_BY_REWRITERS_NAMED             = ""; 
	//
	public static final String  KEY_DEMO_APP_DEFAULT_LOOK_AND_FEEL                              = "grinder.demo.ui.default.look.and.feel";
	public static final String  DEFAULT_VALUE_DEMO_APP_DEFAULT_LOOK_AND_FEEL                    = "Nimbus"; // available as of JDK 1.6u10, see: http://docs.oracle.com/javase/tutorial/uiswing/lookandfeel/nimbus.html
	
	/**
	 * Enable the output of trace information.
	 * Note: must be called before any references to Trace APIs are made.
	 */
	public static void enableTrace() {
		System.setProperty("trace.level", "trace");
	}
	
	/**
	 * Disable the output of trace information.
	 * Note: must be called before any references to Trace APIs are made.
	 */
	public static void disableTrace() {
		System.setProperty("trace.level", "off");	
	}	
	
	/**
	 * Enable the output of justification information.
 	 * Note: must be called before any references to Justification APIs are made.
	 */
	public static void enableJustification() {
		System.setProperty("justification.level", "trace");
	}
	
	/**
	 * Disable the output of justification information.
	 * Note: must be called before any references to Justification APIs are made.
	 */
	public static void disableJustification() {
		System.setProperty("justification.level", "off");
	}
	
	public static boolean isDisplayTreeUtilUI() {
		boolean result = getBoolean(KEY_DISPLAY_TREE_UTIL_UI, DEFAULT_VALUE_DISPLAY_TREE_UTIL_UI);
		
		return result;
	}
	
	public static boolean isWaitUntilUIClosedEnabled() {
		boolean result = getBoolean(KEY_WAIT_UNTIL_UI_CLOSED_ENABLED, DEFAULT_VALUE_WAIT_UNTIL_UI_CLOSED_ENABLED);
		
		return result;
	}
	
	public static boolean isAssumeDomainsAlwaysLarge() {
		boolean result = getBoolean(KEY_ASSUME_DOMAIN_ALWAYS_LARGE, DEFAULT_VALUE_ASSUME_DOMAIN_ALWAYS_LARGE);
		
		return result;
	}
	
	public static boolean isCompleteSimplifyUseSATSolver() {
		boolean result = getBoolean(KEY_COMPLETE_SIMPLIFY_USE_SAT_SOLVER_SOLVER, DEFAULT_VALUE_COMPLETE_SIMPLIFY_USE_SAT_SOLVER);
		
		return result;
	}
	
	public static String getDefaultSATSolverClass() {
		String result = getString(KEY_DEFAULT_SAT_SOLVER_CLASS, DEFAULT_VALUE_SAT_SOLVER_CLASS);
		
		return result;
	}
	
	public static boolean isReplaceVariableWithConstantItIsBoundTo() {
		boolean result = getBoolean(KEY_REPLACE_VARIABLE_WITH_CONSTANT_IT_IS_BOUND_TO, DEFAULT_VALUE_REPLACE_VARIABLE_WITH_CONSTANT_IT_IS_BOUND_TO);
		
		return result;
	}
	
	public static long getRewriteDeadEndsCacheMaximumSize() {
		long result = getLong(KEY_REWRITE_DEAD_ENDS_CACHE_MAXIMUM_SIZE, DEFAULT_VALUE_REWRITE_DEAD_ENDS_CACHE_MAXIMUM_SIZE);
		
		return result;
	}
	
	public static int getRewriteDeadEndsCacheGarbageCollectionPeriod() {
		int result = getInt(KEY_REWRITE_DEAD_ENDS_CACHE_GARBAGE_COLLECTION_PERIOD, DEFAULT_VALUE_REWRITE_DEAD_ENDS_CACHE_GARBAGE_COLLECTION_PERIOD);
		
		return result;
	}
	
	public static long getRewritingProcessCacheMaximumSize() {
		long result = getLong(KEY_REWRITING_PROCESS_CACHE_MAXIMUM_SIZE, DEFAULT_VALUE_REWRITING_PROCESS_CACHE_MAXIMUM_SIZE);
		
		return result;
	}
	
	public static int getRewritingProcessCacheGarbageCollectionPeriod() {
		int result = getInt(KEY_REWRITING_PROCESS_CACHE_GARBAGE_COLLECTION_PERIOD, DEFAULT_VALUE_REWRITING_PROCESS_CACHE_GARBAGE_COLLECTION_PERIOD);
		
		return result;
	}
	
	public static String getFilterOutLoggingByRewritersNamed() {
		String result = getString(KEY_FILTER_OUT_LOGGING_BY_REWRITERS_NAMED, DEFAULT_VALUE_FILTER_OUT_LOGGING_BY_REWRITERS_NAMED);
		
		return result;
	}
	
	public static String getDemoAppDefaultLookAndFeel() {
		String result = getString(KEY_DEMO_APP_DEFAULT_LOOK_AND_FEEL, DEFAULT_VALUE_DEMO_APP_DEFAULT_LOOK_AND_FEEL);
		
		return result;
	}
}
