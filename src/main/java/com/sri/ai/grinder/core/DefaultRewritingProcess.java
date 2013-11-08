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
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.atomic.AtomicLong;

import com.google.common.annotations.Beta;
import com.google.common.base.Function;
import com.google.common.base.Predicate;
import com.google.common.cache.Cache;
import com.google.common.cache.CacheBuilder;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.grinder.GrinderConfiguration;
import com.sri.ai.grinder.api.ChildRewriterCallIntercepter;
import com.sri.ai.grinder.api.Rewriter;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.expression.ExpressionCache;
import com.sri.ai.grinder.expression.ExpressionCacheKey;
import com.sri.ai.grinder.library.IsVariable;
import com.sri.ai.util.AICUtilConfiguration;
import com.sri.ai.util.Util;
import com.sri.ai.util.base.IdentityWrapper;
import com.sri.ai.util.base.IsInstanceOf;
import com.sri.ai.util.base.NullaryFunction;
import com.sri.ai.util.base.Pair;

/**
 * A default implementation of {@link RewritingProcess}. By default, the
 * predicate indicating variables uses {@link PrologVariableConvention}.
 * 
 * @author braz
 * @author oreilly
 */
@Beta
public class DefaultRewritingProcess implements RewritingProcess {
	
	/**
	 * An interface to be implemented by classes that can provide rewriters
	 * based on name to the RewritingProcess for use by its rewrite() methods.
	 * 
	 * @author oreilly
	 * 
	 */
	public interface RewriterLookup {
		/**
		 * 
		 * @param rewriterName
		 *            the name of the rewriter to be looked up.
		 * @return the rewriter corresponding to the rewriterName provided.
		 */
		Rewriter getRewriterFor(String rewriterName);
	}
	
	/**
	 * An iterator over the rewriters of this process. The reason this is public
	 * is mostly technical.
	 */
	public static class RewriterDepthFirstIterator extends
			com.sri.ai.util.collect.DepthFirstIterator<Rewriter> {

		// Note: Rewriters may be connected to each other via their children
		// (i.e. a graph of rewriters as opposed to a tree).
		// This ensures we only visit each child rewriter once.
		private Set<Rewriter> seenAlready = new HashSet<Rewriter>();

		// The super class needs this to be public so the constructor is
		// accessible through reflection,
		// a pre-requisite of the super class.
		public RewriterDepthFirstIterator(Rewriter rootRewriter) {
			super(rootRewriter);
		}
		
		@Override
		public com.sri.ai.util.collect.DepthFirstIterator<Rewriter> newInstance(Rewriter object) {
			return new RewriterDepthFirstIterator(object);
		}

		@Override
		public Iterator<Rewriter> getChildrenIterator(Rewriter rewriter) {
			List<Rewriter> childrenNotSeenAlready = new ArrayList<Rewriter>();
			Iterator<Rewriter> childIterator = rewriter.getChildrenIterator();
			while (childIterator.hasNext()) {
				Rewriter child = childIterator.next();
				if (!seenAlready.contains(child)) {
					childrenNotSeenAlready.add(child);
					seenAlready.add(child);
				}
			}

			return childrenNotSeenAlready.iterator();
		}
	}
	
	// TODO: Ideally this should not be tied to the process (Note however: ExpressionKnowledgeModule Provider API
	// and BracketedExpressionSubExpressionsProvider are dependent on the process, so not sure how to decouple yet).
	//
	// Note: Using a weak Keys cache so that any no longer used threads can be cleaned up properly
	private static Cache<Thread, DefaultRewritingProcess> threadToGlobalRewritingProcessForKnowledgeBasedExpressions = CacheBuilder.newBuilder()
			.weakKeys()
			.build();
	// Used to assign unique ids to rewrite processes.
	private static final AtomicLong  _uniqueIdGenerator = new AtomicLong(0);
	//
	private long                         id                                                                    = 0L;
	private DefaultRewritingProcess      parentProcess                                                         = null;
	private Expression                   rootExpression                                                        = null;
	private Rewriter                     rootRewriter                                                          = null;
	private RewriterLookup               rewriterLookup                                                        = null;
	private ChildRewriterCallIntercepter childCallIntercepter                                                  = null;
	private Map<Expression, Expression>  contextualVariablesAndDomains                                         = null;
	private Expression                   contextualConstraint                                                  = Expressions.TRUE;
	private Predicate<Expression>        isConstantPredicate                                                   = null;
	private boolean                      isResponsibleForNotifyingRewritersOfBeginningAndEndOfRewritingProcess = true;
	private int                          recursionLevel                                                        = 0;
	private boolean                      interrupted                                                           = false;
	//
	private ConcurrentHashMap<Object, Object>          globalObjects       = null;
	private ConcurrentHashMap<Class<?>, Rewriter>      lookedUpModuleCache = null;
	private ConcurrentHashMap<String, ExpressionCache> rewriterCaches      = null;
	//
	private long rewritingProcessCacheMaximumSize             = GrinderConfiguration.getRewritingProcessCacheMaximumSize();
	private int  rewritingProcessCacheGarbageCollectionPeriod = GrinderConfiguration.getRewritingProcessCacheGarbageCollectionPeriod();
	//
	private NullaryFunction<Iterator<ExpressionCacheKey>> reachableExpressionsIteratorMaker = new NullaryFunction<Iterator<ExpressionCacheKey>>() {
		@Override
		public Iterator<ExpressionCacheKey> apply() {		
			Iterator<ExpressionCacheKey> iterator = ExpressionCache.makeIteratorFor(getRootExpression(), DefaultRewritingProcess.this);
			return iterator;
		}
	};
	private Function<String, ExpressionCache> cacheMaker = new Function<String, ExpressionCache>() {
		@Override
		public ExpressionCache apply(String rewriterName) {
			return new ExpressionCache(rewritingProcessCacheMaximumSize,
					reachableExpressionsIteratorMaker,
					rewritingProcessCacheGarbageCollectionPeriod);
		}
	};

	//
	// START - Constructors

	public DefaultRewritingProcess(Rewriter rootRewriter) {
		this(null, rootRewriter, null, Util.<Expression, Expression>map(), new PrologConstantPredicate(), new HashMap<Object, Object>());
	}
	
	public DefaultRewritingProcess(Rewriter rootRewriter, RewriterLookup rewriterLookup) {
		this(null, rootRewriter, rewriterLookup, Util.<Expression, Expression>map(), new PrologConstantPredicate(), new HashMap<Object, Object>());
	}
	
	public DefaultRewritingProcess(Expression rootExpression, Rewriter rootRewriter) {
		this(rootExpression, rootRewriter, null, Util.<Expression, Expression>map(), new PrologConstantPredicate(), new HashMap<Object, Object>());
	}

	public DefaultRewritingProcess(Expression rootExpression,
			Rewriter rootRewriter, Map<Object, Object> globalObjects) {
		this(rootExpression, rootRewriter, null, Util.<Expression, Expression>map(), new PrologConstantPredicate(), globalObjects);
	}

	public DefaultRewritingProcess(Expression rootExpression,
			Rewriter rootRewriter, Map<Expression, Expression> contextualVariablesAndDomains, Predicate<Expression> isConstantPredicate,
			Map<Object, Object> globalObjects) {
		this(rootExpression, rootRewriter, null, contextualVariablesAndDomains, isConstantPredicate, globalObjects);
	}
	
	public DefaultRewritingProcess(Expression rootExpression,
			Rewriter rootRewriter, RewriterLookup rewriterLookup,
			Map<Expression, Expression> contextualVariablesAndDomains,
			Predicate<Expression> isConstantPredicate,
			Map<Object, Object> globalObjects) {
		
		initialize(null,
				rootExpression, 
				rootRewriter,
				rewriterLookup,
				null,
				contextualVariablesAndDomains,
				Expressions.TRUE,
				isConstantPredicate, 
				new ConcurrentHashMap<Object, Object>(globalObjects), 
				new ConcurrentHashMap<String, ExpressionCache>(),
				new ConcurrentHashMap<Class<?>, Rewriter>(),
				false, 
				true);
	}

	// END-Constructors
	//
	
	public long getId() {
		return id;
	}
	
	public RewriterLookup getRewriterLookup() {
		return rewriterLookup;
	}
	
	public void setRewriterLookup(RewriterLookup rewriterLookup) {
		this.rewriterLookup = rewriterLookup;
	}
	
	public static RewritingProcess getGlobalRewritingProcessForKnowledgeBasedExpressions() {
		return threadToGlobalRewritingProcessForKnowledgeBasedExpressions.getIfPresent(Thread.currentThread());
	}
	
	public static void setGlobalRewritingProcessForKnowledgeBasedExpressions(RewritingProcess process) {
		threadToGlobalRewritingProcessForKnowledgeBasedExpressions.put(Thread.currentThread(), (DefaultRewritingProcess) process);
	}
	
	public static void cleanUpGlobalRewritingProcessForKnowledgeBasedExpressions() {
		List<Thread> toRemove = new ArrayList<Thread>();
		for (Thread t : threadToGlobalRewritingProcessForKnowledgeBasedExpressions.asMap().keySet()) {
			if (!t.isAlive()) {
				toRemove.add(t);
			}
		}
		if (toRemove.size() > 0) {
			for (Thread t : toRemove) {				
				threadToGlobalRewritingProcessForKnowledgeBasedExpressions.invalidate(t);
			}
			threadToGlobalRewritingProcessForKnowledgeBasedExpressions.cleanUp();
		}
	}

	public void setRecursionLevel(int recursiveLevel) {
		this.recursionLevel = recursiveLevel;
	}

	//
	// START-RewritingProcess
	@Override
	public boolean isConstant(Expression expression) {
		return getIsConstantPredicate().apply(expression);
	}
	
	@Override
	public boolean isVariable(Expression expression) {
		boolean result = IsVariable.isVariable(expression,
				isConstantPredicate);
		return result;
	}

	@Override
	public Predicate<Expression> getIsConstantPredicate() {
		return isConstantPredicate;
	}

	@Override
	public void setIsConstantPredicate(Predicate<Expression> isConstantPredicate) {
		this.isConstantPredicate = isConstantPredicate;
	}

	@Override
	public Expression getRootExpression() {
		return rootExpression;
	}

	@Override
	public void setRootExpression(Expression newRoot) {
		this.rootExpression = newRoot;
	}

	@Override
	public Rewriter getRootRewriter() {
		return rootRewriter;
	}

	@Override
	public Rewriter setRootRewriter(Rewriter newRootRewriter) {
		if (this.isResponsibleForNotifyingRewritersOfBeginningAndEndOfRewritingProcess) {
			notifyEndOfRewritingProcess();
		}
		rootRewriter = newRootRewriter;
		if (this.isResponsibleForNotifyingRewritersOfBeginningAndEndOfRewritingProcess) {
			notifyReadinessOfRewritingProcess();
		}
		return rootRewriter;
	}
	
	@Override
	public Expression rewrite(String rewriterName, Expression expression) {
		
		checkInterrupted();
		
		Rewriter   rewriter = rewriterLookup.getRewriterFor(rewriterName);
		Expression result   = null;
		
		// If child calls are to be intercepted
		if (childCallIntercepter != null) {
			// NOTE: The following logic supports chaining child rewriter call intercepters, that is when an intercepter
			// calls a rewriter that is itself an intercepter, in the following way:
			// 1. When a child process is created from a parent process it will inherit the parents child call intercepter.
			//    This allows rewriters to create sub-processes within their own logic without breaking the interception
			//    logic (i.e. you could have an ancestry of rewriting processes that have the same intercepter associated
			//    with them - however, in most cases this will usually be no longer than 2 in length as rewriters tend
			//    to only extend the rewriting process passed to them in special cases just before they call a child
			//    rewriter).
			// 2. In the simple case (when no chaining needs to be supported), create a child process that
			//    does not contain a child rewriter call intercepter and use it as the process that
			//    is passed to the intercepter. This stops the intercepter intercepting its own forwarded rewrite calls,
			//    i.e. when its intercepted a rewrite and it decides to forward on the rewrite request to the
			//    intended child rewriter. In addition, this ends up marking a break in a chain of intercepters.
			// 3. Detect if we have an intercepter chain, this is done by looking back up this rewriting process's
			//    ancestors. If an ancestor does not have a child rewriter call intercepter then we don't have
			//    a chain and we can just call the rewriter without one (see step 2 for how a break in the sequence of
			//    ancestors can occur). However, if an ancestor does have a child rewriter call intercepter and its 
			//    different than this one, then we do have a chain and we use the ancestors child call intercepter 
			//    when making the child process that will be passed to this process's child rewriter call intercepter.
			//    That way if this process's child rewriter call intercepter forwards an intercepted rewrite call
			//    this will end up getting intercepted by the outer child rewriter call intercepter. 
			ChildRewriterCallIntercepter outerChildCallIntercepter = null;
			// Look up this process's ancestors to see if we have an intercepter chain
			DefaultRewritingProcess rewritingProcessToCheckForOuterChildCallIntercepter = this.parentProcess;
			while (rewritingProcessToCheckForOuterChildCallIntercepter != null) {
				if (rewritingProcessToCheckForOuterChildCallIntercepter.childCallIntercepter != null &&
					rewritingProcessToCheckForOuterChildCallIntercepter.childCallIntercepter != this.childCallIntercepter) {
					// We have an intercepter chain (as an ancestor in an unbroken chain has a different intercepter
					// associated with it).
					outerChildCallIntercepter = rewritingProcessToCheckForOuterChildCallIntercepter.childCallIntercepter;
					rewritingProcessToCheckForOuterChildCallIntercepter = null;
				}
				else if (rewritingProcessToCheckForOuterChildCallIntercepter.childCallIntercepter == null) {
					// There is a break in child intercepters (see point 2 above) in the ancestry.
					// Therefore, we don't have an intercepter chain.
					rewritingProcessToCheckForOuterChildCallIntercepter = null;
				}
				else {
					// Check the next ancestor.
					rewritingProcessToCheckForOuterChildCallIntercepter = rewritingProcessToCheckForOuterChildCallIntercepter.parentProcess;
				}
			}
		
			// Create a child process for this intercept call so that the correct child call intercepter (if a chain exists) is passed to or
			// no intercepter if there is no chain.
			DefaultRewritingProcess childCallProcess = new DefaultRewritingProcess(this, outerChildCallIntercepter, this.contextualVariablesAndDomains, this.contextualConstraint);
			// Call the intercepter and return its result.
			result = childCallIntercepter.intercept(rewriterName, expression, childCallProcess);
		}
		else {
			// No interception needs to be handled, can just call directly with this process (no need to create a child process in this instance).
			result = rewriter.rewrite(expression, this);
		}
		
		return result;
	}
	
	@Override
	public Expression rewrite(String rewriterName, Expression expression, ChildRewriterCallIntercepter childCallIntercepter) {
		
		checkInterrupted();
		
		Rewriter                rewriter         = rewriterLookup.getRewriterFor(rewriterName);
		// Create a sub-process with the specified child intercepter.
		DefaultRewritingProcess childCallProcess = new DefaultRewritingProcess(this, childCallIntercepter, this.contextualVariablesAndDomains, this.contextualConstraint);
		// Call rewrite on the specified rewriter with this child process
		Expression result = rewriter.rewrite(expression, childCallProcess);
		
		return result;
	}

	@Override
	public Set<Expression> getContextualVariables() {
		return contextualVariablesAndDomains.keySet();
	}

	@Override
	public Map<Expression, Expression> getContextualVariablesAndDomains() {
		return contextualVariablesAndDomains;
	}

	@Override
	public Expression getContextualVariableDomain(Expression variable) {
		return contextualVariablesAndDomains.get(variable);
	}

	@Override
	public Expression getContextualConstraint() {
		return contextualConstraint;
	}

	@Override
	public RewritingProcess newSubProcessWithContext(
			Map<Expression, Expression> subProcessContextualVariablesAndDomains, Expression subProcessContextualConstraint) {

		DefaultRewritingProcess result = new DefaultRewritingProcess(this, 
				this.childCallIntercepter,
				subProcessContextualVariablesAndDomains,
				subProcessContextualConstraint);
		
		return result;
	}

	@Override
	public Expression rewritingPreProcessing(Rewriter rewriter, Expression expression) {
		Expression cached = getCached(rewriter, expression);
		
		// this will be null if there is no cached value, and
		// then the value will be computed
		return cached; 
	}
	
	@Override
	public void rewritingPostProcessing(Rewriter rewriter,
			Expression expression, Expression result) {

		putInCache(rewriter, expression, result);

		if (isResponsibleForNotifyingRewritersOfBeginningAndEndOfRewritingProcess
				&& rewriter == rootRewriter) {
			notifyEndOfRewritingProcess();
		}
	}
	
	@Override
	public void notifyReadinessOfRewritingProcess() {
		Iterator<Rewriter> rewriterIterator = new RewriterDepthFirstIterator(
				rootRewriter);
		while (rewriterIterator.hasNext()) {
			Rewriter rewriter = rewriterIterator.next();
			rewriter.rewritingProcessInitiated(this);
		}
	}
	
	@Override
	public void notifyEndOfRewritingProcess() {
		if (AICUtilConfiguration.isRecordCacheStatistics() && rewriterCaches != null) {
			for (Map.Entry<String, ExpressionCache> entry : rewriterCaches.entrySet()) {
				System.out.println(String.format("RewritingProcess Cache Stats for %-80s are %s", entry.getKey(), entry.getValue().stats()));
			}
		}
		Iterator<Rewriter> rewriterIterator = new RewriterDepthFirstIterator(
				rootRewriter);
		while (rewriterIterator.hasNext()) {
			Rewriter rewriter = rewriterIterator.next();
			rewriter.rewritingProcessFinalized(this);
		}
	}

	@Override
	public Rewriter findModule(Predicate<Rewriter> predicate) {
		Iterator<Rewriter> rewriterIterator = new RewriterDepthFirstIterator(
				rootRewriter);
		return Util.getFirstSatisfyingPredicateOrNull(
				rewriterIterator, predicate);
	}

	@Override
	public Rewriter findModule(Class<?> clazz) {
		Rewriter foundModule = lookedUpModuleCache.get(clazz);
		if (foundModule == null) {
			foundModule = findModule(new IsInstanceOf<Rewriter>(clazz));
			if (foundModule != null) {
				lookedUpModuleCache.put(clazz, foundModule);
			}
		}
		return foundModule;
	}
	
	@Override
	public Map<Object, Object> getGlobalObjects() {
		return globalObjects;
	}

	@Override
	public void setGlobalObjects(Map<Object, Object> newMap) {
		globalObjects.clear();
		globalObjects.putAll(newMap);
	}

	@Override
	public Object putGlobalObject(Object key, Object value) {
		return globalObjects.put(key, value);
	}

	@Override
	public Object removeGlobalObject(Object key) {
		return globalObjects.remove(key);
	}
	
	@Override
	public boolean containsGlobalObjectKey(Object key) {
		return globalObjects.containsKey(key);
	}
	
	@Override
	public Object getGlobalObject(Object key) {
		return globalObjects.get(key);
	}

	@Override
	public boolean isRecursive() {
		return recursionLevel > 0;
	}

	@Override
	public int getRecursionLevel() {
		return recursionLevel;
	}

	/**
	 * A default equivalence class for each expression equal to the expression
	 * instance itself.
	 */
	@Override
	public Pair<IdentityWrapper, Expression> getExpressionEquivalenceClassForDeadEnd(Expression expression) {
		IdentityWrapper expressionIdentity = new IdentityWrapper(expression);
		Expression contextualConstraint = getContextualConstraint();
		Pair<IdentityWrapper, Expression> result = new Pair<IdentityWrapper, Expression>(
				expressionIdentity, contextualConstraint);
		return result;
	}
	
	@Override
	public void interrupt() {
		interrupted = true;
	}
	
	// END-RewritingProcess
	//
	
	//
	//  PROTECTED METHODS
	//
	protected Expression getCached(Rewriter rewriter, Expression expression) {
		checkInterrupted();
		
		ExpressionCache rewriterCache = getRewriterCache(rewriter);
		Expression cachedItem         = rewriterCache.get(rewriterCache.getCacheKeyFor(expression, this));
		
		return cachedItem;
	}

	protected void putInCache(Rewriter rewriter, Expression expression, Expression resultingExpression) {		
		ExpressionCache rewriterCache = getRewriterCache(rewriter);
		rewriterCache.put(rewriterCache.getCacheKeyFor(expression, this), resultingExpression);
	}
	
	//
	// PRIVATE METHODS
	//
	
	// Note: private constructors for sub-processes			                        
	private DefaultRewritingProcess(DefaultRewritingProcess parentProcess,
			ChildRewriterCallIntercepter childCallIntercepter,
			Map<Expression, Expression> contextualVariablesAndDomains,
			Expression contextualConstraint) {
		initialize(parentProcess,
				parentProcess.rootExpression,
				parentProcess.rootRewriter,
				parentProcess.rewriterLookup,
				childCallIntercepter,
				contextualVariablesAndDomains,
				contextualConstraint,
				parentProcess.isConstantPredicate, 
				parentProcess.globalObjects,
				parentProcess.rewriterCaches,
				parentProcess.lookedUpModuleCache,
				parentProcess.interrupted, 
				false /* isResponsibleForNotifyingRewritersOfBeginningAndEndOfRewritingProcess */				
				);
		
	}
	
	private void initialize(DefaultRewritingProcess parentProcess,
			Expression rootExpression,
			Rewriter rootRewriter,
			RewriterLookup rewriterLookup,
			ChildRewriterCallIntercepter childCallIntercepter,
			Map<Expression, Expression> contextualVariablesAndDomains,
			Expression contextualConstraint,
			Predicate<Expression> isConstantPredicate,
			ConcurrentHashMap<Object, Object> globalObjects,
			ConcurrentHashMap<String, ExpressionCache> rewriterCaches,
			ConcurrentHashMap<Class<?>, Rewriter> lookedUpModuleCache,
			boolean interrupted,
			boolean isResponsibleForNotifyingRewritersOfBeginningAndEndOfRewritingProcess) {
		this.id                   = _uniqueIdGenerator.addAndGet(1L);
		this.parentProcess        = parentProcess;
		this.rootExpression       = rootExpression;
		this.rootRewriter         = rootRewriter;
		this.rewriterLookup       = rewriterLookup;
		this.childCallIntercepter = childCallIntercepter;
		//
		this.contextualVariablesAndDomains = contextualVariablesAndDomains;
		this.contextualConstraint       = contextualConstraint;
		this.isConstantPredicate        = isConstantPredicate;
		//
		this.globalObjects        = globalObjects;
		this.rewriterCaches       = rewriterCaches;
		this.lookedUpModuleCache  = lookedUpModuleCache;
		this.interrupted          = interrupted;
		//
		this.isResponsibleForNotifyingRewritersOfBeginningAndEndOfRewritingProcess = isResponsibleForNotifyingRewritersOfBeginningAndEndOfRewritingProcess;
		if (parentProcess != null) {
			// rewriters must have already been notified and been initialized.
			setRecursionLevel(parentProcess.getRecursionLevel()+1);
		}
		setGlobalRewritingProcessForKnowledgeBasedExpressions(this);
		if (this.isResponsibleForNotifyingRewritersOfBeginningAndEndOfRewritingProcess) {
			notifyReadinessOfRewritingProcess();
		}
	}
	
	private ExpressionCache getRewriterCache(Rewriter rewriter) {
		ExpressionCache rewriterCache = Util.getValuePossiblyCreatingIt(rewriterCaches, rewriter.getName(), cacheMaker);
		return rewriterCache;
	}
	
	private void checkInterrupted() {
		boolean interrupt = false;
		if (interrupted) {
			interrupt = true;
		}
		else {
			// Check if parent process interrupted
			DefaultRewritingProcess parent = this.parentProcess;
			while (parent != null) {
				if (parent.interrupted) {
					interrupt = true;
					break;
				}
				parent = parent.parentProcess;
			}
		}
		
		if (interrupt) {
			throw new RuntimeException("Rewriting Process Interrupted.");
		}
	}
	
	public String toString() {
		return "Rewriting process with context " + getContextualVariablesAndDomains() + ", " + getContextualConstraint();
	}
}
