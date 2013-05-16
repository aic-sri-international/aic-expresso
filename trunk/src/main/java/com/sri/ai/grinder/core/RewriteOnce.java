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

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

import com.google.common.annotations.Beta;
import com.google.common.base.Function;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.ReplacementFunctionWithContextuallyUpdatedProcess;
import com.sri.ai.grinder.GrinderConfiguration;
import com.sri.ai.grinder.api.NoOpRewriter;
import com.sri.ai.grinder.api.Rewriter;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.helper.Justification;
import com.sri.ai.grinder.helper.Trace;
import com.sri.ai.grinder.library.CommonLibrary;
import com.sri.ai.util.AICUtilConfiguration;
import com.sri.ai.util.Util;
import com.sri.ai.util.base.IdentityWrapper;
import com.sri.ai.util.base.NullaryFunction;
import com.sri.ai.util.base.Pair;
import com.sri.ai.util.base.TernaryProcedure;
import com.sri.ai.util.cache.CacheMap;
import com.sri.ai.util.cache.DefaultCacheMap;
import com.sri.ai.util.collect.FunctionIterator;

/**
 * A Rewriter based on a set of Rewriters (with a default detailed below) that
 * applies it to an expression until it is rewritten at most once once.
 * 
 * The default is a set of all rewrite rules defined so far.
 * 
 * @author braz
 */
@Beta
public class RewriteOnce extends AbstractRewriter {

	private List<Rewriter> rewriters;
	private List<RewriteOnceWithinProcessFunction> rewriterReplacementFunctions = new ArrayList<RewriteOnceWithinProcessFunction>();
	private int totalNumberOfSelections = 0;
	private int rewritingCount = 0; 
	private ConcurrentHashMap<Rewriter, CacheMap<Pair<IdentityWrapper, Expression>, Object>> deadEndsByRewriter = new ConcurrentHashMap<Rewriter, CacheMap<Pair<IdentityWrapper, Expression>, Object>>();
	private long rewriteOnceDeadEndsCacheMaximumSize = GrinderConfiguration.getRewriteDeadEndsCacheMaximumSize();
	private int rewriteOnceDeadEndsCacheGarbageCollectionPeriod = GrinderConfiguration.getRewriteDeadEndsCacheGarbageCollectionPeriod();
	
	public RewriteOnce() {
		this(CommonLibrary.INSTANCE);
	}

	public RewriteOnce(List<Rewriter> rewriters) {
		this("RewriteOnce", rewriters);
	}
	
	public RewriteOnce(String name, List<Rewriter> rewriters) {
		super();
		setName(name);
		this.rewriters = rewriters;
		for (Rewriter rewriter : rewriters) {
			if (rewriter instanceof NoOpRewriter) {
				// Don't waste my time calling NoOpRewriters as there
				// is a significant overhead involved with calling 
				// expression.replace().
				continue;
			}
			rewriterReplacementFunctions.add(new RewriteOnceWithinProcessFunction(rewriter));
		}
	}

	//
	// START-Rewriter
	/**
	 * Returns an iterator ranging over the base rewriters.
	 */
	@Override
	public Iterator<Rewriter> getChildrenIterator() {
		return rewriters.iterator();
	}
	
	@Override
	public void rewritingProcessInitiated(RewritingProcess process) {
		totalNumberOfSelections = 0;
		deadEndsByRewriter      = new ConcurrentHashMap<Rewriter, CacheMap<Pair<IdentityWrapper, Expression>, Object>>();
	}

	@Override
	public void rewritingProcessFinalized(RewritingProcess process) {
		if (AICUtilConfiguration.isRecordCacheStatistics()) {
			for (Map.Entry<Rewriter, CacheMap<Pair<IdentityWrapper, Expression>, Object>> entry : deadEndsByRewriter.entrySet()) {
				System.out.println(String.format("Rewrite Once Dead Ends Cache Stats for %-80s are %s", entry.getKey().getName(), entry.getValue().stats()));
			}
		}
		deadEndsByRewriter.clear();
	}
	
	// END-Rewriter
	//
	
	
	@Override
	public Expression rewriteAfterBookkeeping(Expression expression, RewritingProcess process) {
		Expression   result             = expression;
		Expression   original           = expression;
		int          numberOfSelections = 0;
		long         start              = System.currentTimeMillis();
		Rewriter     rewroteWith        = null;
		Expression   rewroteFrom        = null;
		Expression   rewroteTo          = null;
		if (expression != null) {
			for (RewriteOnceWithinProcessFunction replacementFunction : rewriterReplacementFunctions) {
				Rewriter rewriter = replacementFunction.getRewriter();
				
				CacheMap<Pair<IdentityWrapper, Expression>, Object> deadEndsCache 
					= getDeadEndsCacheMap(rewriter, process);
				TernaryProcedure<Expression, Expression, RewritingProcess> listener 
					= new DeadEndListener(deadEndsCache, rewriter);
				PruningPredicate pruner 
					= new DeadEndPruner(deadEndsCache, rewriter);
				
				expression = expression.replaceFirstOccurrence(replacementFunction, pruner, listener, process);
				numberOfSelections += replacementFunction.getNumberOfSelections();
				if (expression != original) {
					result      = expression;
					rewroteWith = rewriter;
					rewroteFrom = replacementFunction.getRewroteFrom();
					rewroteTo   = replacementFunction.getRewroteTo();
					break;
				}
			}
		}
		totalNumberOfSelections += numberOfSelections;
		if (result != original) {

			Justification.log(original);
			Justification.beginEqualityStep(rewroteWith.getName());
			
			boolean isWholeExpressionRewrite = rewroteFrom == original;
			String indent = "";
			if (isWholeExpressionRewrite) {
				if (Trace.isEnabled()) {
					Trace.log(indent+"Rewriting whole expression:");
				}
			} 
			else {
				indent = "    ";
				if (Trace.isEnabled()) {
					Trace.log(indent+"Rewriting sub-expression:");
					Trace.log(indent+"{}", rewroteFrom);
				}
			}
			long relativeTime = System.currentTimeMillis() - start;
			
			if (Trace.isEnabled()) {
				Trace.log(indent+"   ----> ("+rewroteWith.getName()+",  "+relativeTime+" ms, #"+(++rewritingCount)+", "+numberOfSelections+" rewriter selections ("+totalNumberOfSelections+" since start))");
			}
			if (isWholeExpressionRewrite) {
				if (Trace.isEnabled()) {
					Trace.log(indent+"{}", result);
				}
			} 
			else {
				if (Trace.isEnabled()) {
					Trace.log(indent+"{}", rewroteTo);
					Trace.log("{}", result);
				}
			}
			
			Justification.endEqualityStep(result);
		}
		
		return result;
	}

	//
	// PRIVATE METHODS
	//
	private CacheMap<Pair<IdentityWrapper, Expression>, Object> getDeadEndsCacheMap(Rewriter rewriter, final RewritingProcess process) {
		CacheMap<Pair<IdentityWrapper, Expression>, Object> result =
			Util.getValuePossiblyCreatingIt(deadEndsByRewriter, rewriter, new RewriterDeadEndsCacheMaker(rewriteOnceDeadEndsCacheMaximumSize, rewriteOnceDeadEndsCacheGarbageCollectionPeriod, process));
		return result;
	}
	
	private static final class RewriterDeadEndsCacheMaker implements Function<Rewriter, CacheMap<Pair<IdentityWrapper, Expression>, Object>> {
		private final long rewriteOnceDeadEndsCacheMaximumSize;
		private final int rewriteOnceDeadEndsCacheGarbageCollectionPeriod;
		private final RewritingProcess process;

		private RewriterDeadEndsCacheMaker(long rewriteOnceDeadEndsCacheMaximumSize, int rewriteOnceDeadEndsCacheGarbageCollectionPeriod, RewritingProcess process) {
			this.rewriteOnceDeadEndsCacheMaximumSize = rewriteOnceDeadEndsCacheMaximumSize;
			this.rewriteOnceDeadEndsCacheGarbageCollectionPeriod = rewriteOnceDeadEndsCacheGarbageCollectionPeriod;
			this.process = process;
		}

		private class EquivalenceClassFunction implements Function<Expression, Pair<IdentityWrapper, Expression>> {
			@Override
			public Pair<IdentityWrapper, Expression> apply(Expression expression) {
				Pair<IdentityWrapper, Expression> result = process.getExpressionEquivalenceClassForDeadEnd(expression);
				return result;
			}
		}

		private class EquivalenceClassIteratorMakerBasedOnExpressionsIteratorMaker implements NullaryFunction<Iterator<Pair<IdentityWrapper, Expression>>> {
			private NullaryFunction<Iterator<Expression>> expressionsIteratorMaker;

			public EquivalenceClassIteratorMakerBasedOnExpressionsIteratorMaker(
					NullaryFunction<Iterator<Expression>> expressionsIteratorMaker) {
				super();
				this.expressionsIteratorMaker = expressionsIteratorMaker;
			}

			@Override
			public Iterator<Pair<IdentityWrapper, Expression>> apply() {
				Iterator<Expression> expressionsIterator =  expressionsIteratorMaker.apply();
				FunctionIterator<Expression, Pair<IdentityWrapper, Expression>> result = new FunctionIterator<Expression, Pair<IdentityWrapper, Expression>>(expressionsIterator, new EquivalenceClassFunction());
				return result;
			}
		}
		
		@Override
		public CacheMap<Pair<IdentityWrapper, Expression>, Object> apply(Rewriter rewriter) {
			return new DefaultCacheMap<Pair<IdentityWrapper, Expression>, Object>(
					rewriteOnceDeadEndsCacheMaximumSize,
					new EquivalenceClassIteratorMakerBasedOnExpressionsIteratorMaker(
							new RewritingProcessReachableExpressionsIteratorMaker(process)),
					rewriteOnceDeadEndsCacheGarbageCollectionPeriod);
		}
	}

	private static class DeadEndListener implements TernaryProcedure<Expression, Expression, RewritingProcess> {
		private CacheMap<Pair<IdentityWrapper, Expression>, Object> cache;
//		private Rewriter rewriter;
		
		public DeadEndListener(CacheMap<Pair<IdentityWrapper, Expression>, Object> cache, Rewriter rewriter) {
			super();
			this.cache = cache;
//			this.rewriter = rewriter;
		}

		@Override
		public void apply(Expression o1, Expression o2, RewritingProcess process) {
			if (o1 == o2) {
				Pair<IdentityWrapper, Expression> equivalenceClass = process.getExpressionEquivalenceClassForDeadEnd(o1);
				cache.put(equivalenceClass, equivalenceClass);
//				System.out.println("Dead end found: " + rewriter);
//				System.out.println("Dead end found: " + equivalenceClass);
//				System.out.println();
			}
		}
	}
	
	private static class DeadEndPruner implements PruningPredicate {
		private CacheMap<Pair<IdentityWrapper, Expression>, Object> cache;
//		private Rewriter rewriter;
			
		public DeadEndPruner(CacheMap<Pair<IdentityWrapper, Expression>, Object> cache, Rewriter rewriter) {
			super();
			this.cache = cache;
//			this.rewriter = rewriter;
		}

		@Override
		public boolean apply(Expression expression, Function<Expression, Expression> replacementFunction, RewritingProcess process) {
			Object equivalenceClass = process.getExpressionEquivalenceClassForDeadEnd(expression);
//			System.out.println("DeadEndPruner: " + rewriter);
//			System.out.println("Checking for pruning:\n" + equivalenceClass);
			boolean result = cache.containsKey(equivalenceClass);
//			System.out.println("Pruned.");
//			System.out.println();
			return result;

//			return false;
		}
	}

//	private Set<ExpressionWithAnnotationsWrapper> memory = new HashSet<ExpressionWithAnnotationsWrapper>();
	
	/**
	 * An extension to {@link RewriterWithinProcessFunction} that registers the trace
	 * of the rewriting happening.
	 */
	private static class RewriteOnceWithinProcessFunction implements ReplacementFunctionWithContextuallyUpdatedProcess {
		private Rewriter   rewriter           = null;
		private int        numberOfSelections = 0;
		private Expression rewroteFrom        = null;
		private Expression rewroteTo          = null;
		
		public RewriteOnceWithinProcessFunction(Rewriter rewriter) {
			this.rewriter = rewriter;
		}

		@Override
		public Expression apply(Expression expression) {
			throw new UnsupportedOperationException("evaluate(Object expression) should not be called.");
		}

		@Override
		public Expression apply(Expression expression, RewritingProcess process) {
			rewroteFrom = expression;
			rewroteTo   = rewriter.rewrite(rewroteFrom, process);
			numberOfSelections++;
			return rewroteTo;
		}
		
		public Rewriter getRewriter() {
			return rewriter;
		}

		public int getNumberOfSelections() {
			return numberOfSelections;
		}
		
		public Expression getRewroteFrom() {
			return rewroteFrom;
		}
		
		public Expression getRewroteTo() {
			return rewroteTo;
		}
	}
}
