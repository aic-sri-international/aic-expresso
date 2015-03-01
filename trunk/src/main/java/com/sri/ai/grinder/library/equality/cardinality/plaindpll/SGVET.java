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
package com.sri.ai.grinder.library.equality.cardinality.plaindpll;

import static com.sri.ai.expresso.helper.Expressions.apply;
import static com.sri.ai.expresso.helper.Expressions.isSubExpressionOf;
import static com.sri.ai.expresso.helper.Expressions.makeSymbol;
import static com.sri.ai.util.Util.collectToLists;
import static com.sri.ai.util.Util.getFirst;
import static com.sri.ai.util.Util.list;
import static com.sri.ai.util.Util.removeNonDestructively;

import java.util.Collection;
import java.util.List;

import com.google.common.base.Predicate;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.library.equality.cardinality.core.CountsDeclaration;
import com.sri.ai.util.base.PairOf;

/**
 * A Variable Elimination algorithm generalized in the same manner
 * {@link SGDPLLT} is generalized from DPLL,
 * that is, it can produce symbolic answers and it does not need
 * to only solve problems with the operations from its classic version
 * (for the case of VE, sum and product, or max and product).
 * <p>
 * It relies on a partial decomposition of the problem, producing instances
 * to be solved by a {@link SGDPLLT}, in the following manner:
 * <pre>
 * sum_{i1,...,i_n} prod_j f_j(args_j)
 * =
 * sum_{i1,...,i_{n-1}} prod_{j : args_j does *not* contain i_n} f_j(args_j) sum_{i_n} prod_{j : args_j contains i_n} f_j(args_j)
 * </pre>
 * and then solving
 * <pre>
 * sum_{i_n} prod_{j : args_j contains i_n} f_j(args_j)
 * </pre>
 * with {@link SGDPLLT}.
 * Note that the symbolic capability of {@link SGDPLLT} is crucial here, as
 * args_j for the various functions f_j will typically involve other indices which,
 * at the level of the sub-problem, are free variables.
 * 
 * @author braz
 *
 */
public class SGVET extends AbstractSolver {
	
	private Expression multiplicativeFunction;
	private Solver subSolver;
	
	public SGVET(String multiplicativeFunctionString, Theory theory, ProblemType problemType) {
		this(multiplicativeFunctionString, theory, problemType, null);
	}

	public SGVET(String multiplicativeFunctionString, Theory theory, ProblemType problemType, CountsDeclaration countsDeclaration) {
		super(theory, problemType, countsDeclaration);
		this.multiplicativeFunction = makeSymbol(multiplicativeFunctionString);
		this.subSolver = new SGDPLLT(theory, problemType, countsDeclaration);
	}

	private static class Partition {
		private List<Expression> index;
		private List<Expression> remainingIndices;
		private PairOf<List<Expression>> expressionsOnIndexAndNot;

		public Partition(Expression index, List<Expression> remainingIndices, PairOf<List<Expression>> expressionsOnIndex) {
			super();
			this.index = list(index);
			this.remainingIndices = remainingIndices;
			this.expressionsOnIndexAndNot = expressionsOnIndex;
		}
	}
	
	@Override
	protected Expression solveAfterBookkeeping(Expression expression, Collection<Expression> indices, Constraint constraint, RewritingProcess process) {
		Expression result;
		Partition partition = pickPartition(getFactors(expression), indices, constraint, process);
		if (partition == null) {
			result = subSolver.solve(expression, indices, constraint, process);
		}
		else {
			Expression indexSubProblemExpression = product(partition.expressionsOnIndexAndNot.first);
			Expression indexSubProblemSolution   = subSolver.solve(indexSubProblemExpression, partition.index, constraint, process);
			
			partition.expressionsOnIndexAndNot.second.add(indexSubProblemSolution);
			Expression remainingSubProblemExpression = product(partition.expressionsOnIndexAndNot.second);
			Constraint constraintOnRemainingIndices = constraint.project(partition.index, process);
			result = solve(remainingSubProblemExpression, partition.remainingIndices, constraintOnRemainingIndices, process);
		}
		
		return result;
	}

	private Partition pickPartition(List<Expression> expressions, Collection<Expression> indices, Constraint constraint, RewritingProcess process) {
		Partition result;
		if (indices.isEmpty()) {
			result = null;
		}
		else {
			Expression index = getFirst(indices);
			List<Expression> remainingIndices = removeNonDestructively(indices, index);
			Predicate<Expression> containsIndex = e -> isSubExpressionOf(index, e);
			PairOf<List<Expression>> onIndexAndNot = collectToLists(expressions, containsIndex);
			result = new Partition(index, remainingIndices, onIndexAndNot);
		}
		return result;
	}

	private Expression product(Collection<Expression> onIndex) {
		return apply(multiplicativeFunction, onIndex);
	}
	
	private List<Expression> getFactors(Expression expression) {
		List<Expression> result;
		if (expression.hasFunctor(multiplicativeFunction)) {
			result = expression.getArguments();
		}
		else {
			result = list(expression);
		}
		return result;
	}
}