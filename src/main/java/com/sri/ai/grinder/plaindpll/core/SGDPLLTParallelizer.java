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
package com.sri.ai.grinder.plaindpll.core;

import java.util.Collection;
import java.util.Iterator;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.library.equality.cardinality.core.CountsDeclaration;
import com.sri.ai.grinder.plaindpll.api.Constraint;
import com.sri.ai.grinder.plaindpll.api.ConstraintTheory;
import com.sri.ai.grinder.plaindpll.api.GroupProblemType;
import com.sri.ai.util.collect.PredicateIterator;

/**
 * An extension of {@link SGDPLLT} that operates as usual until one of two things happen:
 * either a certain depth is reached, or we have no index-containing splitter literals left.
 * At this point, instead of invoking itself recursively, it invokes a given collector function on that sub-problem
 * and determines a zero "don't care solution" (therefore, the returned expression will not be a solution to the original problem).
 * This allows the collections of sub-problems at a certain depth while using {@link SGDPLLT}'s machinery for problem splitting.
 * These sub-problems can then be solved and their solutions combined in some alternative way (for example, on a cluster with map-reduce).
 * The splitting is restricted to index-containing splitters only, because the sub-solutions from a splitting on free variables
 * need to be combined as the branches of an if-then-else solutions, with a condition that would <i>not</i> be available to
 * whatever routine is combining them later on.
 * 
 * @author braz
 *
 */
public class SGDPLLTParallelizer extends SGDPLLT {

	/**
	 * An interface for the collector functor to be provided to this collecting solver.
	 * The method <code>collect</code> will be invoked when the solver reaches a given depth,
	 * instead of recursively invoking itself as done by the super class.
	 * @author braz
	 *
	 */
	public static interface Collector {
		void collect(Expression expression, Collection<Expression> indices, Constraint constraint, RewritingProcess process);
	}
	
	private Collector collector;
	
	private int collectingDepth;
	
	/**
	 * Constructor similar to {@link SGDPLLT#SGDPLLT(ConstraintTheory, GroupProblemType),
	 * but receiving the collector function and the collecting depth.
	 * @param theory
	 * @param problemType
	 * @param collector
	 * @param collectingDepth
	 */
	public SGDPLLTParallelizer(ConstraintTheory theory, GroupProblemType problemType, Collector collector, int collectingDepth) {
		this(theory, problemType, null, collector, collectingDepth);
	}

	/**
	 * Constructor similar to {@link SGDPLLT#SGDPLLT(ConstraintTheory, GroupProblemType, CountsDeclaration),
	 * but receiving the collector function and the collecting depth.
	 * @param theory
	 * @param problemType
	 * @param collector
	 * @param collectingDepth
	 */
	public SGDPLLTParallelizer(ConstraintTheory theory, GroupProblemType problemType, CountsDeclaration countsDeclaration, Collector collector, int collectingDepth) {
		super(theory, problemType, countsDeclaration);
		this.collector = collector;
		this.collectingDepth = collectingDepth;
	}

	public Collector getCollector() {
		return collector;
	}

	public void setCollector(Collector collector) {
		this.collector = collector;
	}

	public int getCollectingDepth() {
		return collectingDepth;
	}

	public void setCollectingDepth(int collectingDepth) {
		this.collectingDepth = collectingDepth;
	}

	/**
	 *  Restricts splitters to index-containing ones only.
	 */
	protected Iterator<Expression> getSplittersIterator(Expression expression, Collection<Expression> indices, Constraint constraint, RewritingProcess process) {
		Iterator<Expression> splittersIterator = super.getSplittersIterator(expression, indices, constraint, process);
		Iterator<Expression> indexSplittersIterator = new PredicateIterator<>(splittersIterator, e -> containsIndex(e, indices));
		return indexSplittersIterator;
	}

	public Expression solve(Expression expression, Collection<Expression> indices, Constraint constraint, RewritingProcess process) {
		Expression result;
		int level = getLevel(process);
		if (itIsTimeToCollect(level, expression, indices, constraint, process)) {
			collector.collect(expression, indices, constraint, process);
			result = problemType.getGroup().additiveIdentityElement(); // dummy solution
		}
		else {
			result = super.solve(expression, indices, constraint, process);
		}
		return result;
	}

	/**
	 * Decides whether a problem should be given to collector rather than further split.
	 * @param level
	 * @param expression
	 * @param indices
	 * @param constraint
	 * @param process
	 * @return
	 */
	protected boolean itIsTimeToCollect(int level, Expression expression, Collection<Expression> indices, Constraint constraint, RewritingProcess process) {
		boolean result = pickSplitter(expression, indices, constraint, process) == null || level == collectingDepth;
		return result;
	}
	
	@Override
	public String toString() {
		return "SGDPLL(T) Parallelizer";
	}
}
