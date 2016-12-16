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
package com.sri.ai.grinder.sgdpllt.core.solver;

import static com.sri.ai.expresso.helper.Expressions.apply;
import static com.sri.ai.expresso.helper.Expressions.isSubExpressionOf;
import static com.sri.ai.grinder.sgdpllt.library.boole.And.getConjuncts;
import static com.sri.ai.grinder.sgdpllt.library.boole.And.isConjunction;
import static com.sri.ai.grinder.sgdpllt.library.controlflow.IfThenElse.condition;
import static com.sri.ai.grinder.sgdpllt.library.controlflow.IfThenElse.elseBranch;
import static com.sri.ai.grinder.sgdpllt.library.controlflow.IfThenElse.isIfThenElse;
import static com.sri.ai.grinder.sgdpllt.library.controlflow.IfThenElse.thenBranch;
import static com.sri.ai.util.Util.argmin;
import static com.sri.ai.util.Util.collectToLists;
import static com.sri.ai.util.Util.getFirst;
import static com.sri.ai.util.Util.list;
import static com.sri.ai.util.Util.mapIntoList;
import static com.sri.ai.util.Util.myAssert;
import static com.sri.ai.util.Util.nonDestructivelyExpandElementsIfFunctionReturnsNonNullCollection;
import static com.sri.ai.util.Util.removeNonDestructively;

import java.util.Collection;
import java.util.Iterator;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Set;

import com.google.common.base.Function;
import com.google.common.base.Predicate;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.helper.SubExpressionsDepthFirstIterator;
import com.sri.ai.grinder.sgdpllt.api.Constraint;
import com.sri.ai.grinder.sgdpllt.api.Context;
import com.sri.ai.grinder.sgdpllt.api.QuantifierEliminator;
import com.sri.ai.grinder.sgdpllt.group.AssociativeCommutativeGroup;
import com.sri.ai.grinder.sgdpllt.group.AssociativeCommutativeSemiRing;
import com.sri.ai.grinder.sgdpllt.library.controlflow.IfThenElse;
import com.sri.ai.util.base.PairOf;

/**
 * A {@link QuantifierEliminator} generalizing the Variable Elimination algorithm in the same manner
 * {@link SGDPLLT} is generalized from DPLL,
 * that is, it can produce symbolic answers and it does not need
 * to only solve problems with the operations from its classic version
 * (for the case of VE, sum and product, or max and product).
 * <p>
 * It relies on a partial decomposition of the problem, producing instances
 * to be solved by a <code>SGDPLL(T)</code> solver, in the following manner:
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
 * Note that the symbolic capability of <code>SGDPLL(T)</code> is crucial here, as
 * args_j for the various functions f_j will typically involve other indices which,
 * at the level of the sub-problem, are free variables.
 * 
 * @author braz
 *
 */
public class AbstractSGVET extends AbstractQuantifierEliminator {

	protected QuantifierEliminator subSolver;
	
	public AbstractSGVET(QuantifierEliminator subSolver, AssociativeCommutativeSemiRing semiRing) {
		myAssert(() -> subSolver.getGroup() instanceof AssociativeCommutativeSemiRing, () -> "AbstractSGVET must receive group that is actually a semiring, but got " + subSolver.getGroup() + " instead");
		this.subSolver = subSolver;
	}

	public boolean isVariable(Expression expression, Context context) {
		return context.getTheory().isVariable(expression, context);
	}

	public AssociativeCommutativeGroup getGroup() {
		return subSolver.getGroup();
	}
	
	public AssociativeCommutativeSemiRing getSemiRing() {
		return (AssociativeCommutativeSemiRing) getGroup();
	}
	
	@Override
	public void interrupt() {
		super.interrupt();
		subSolver.interrupt();
	}
	
	@Override
	public Expression solve(AssociativeCommutativeGroup group, Collection<Expression> indices, Expression indicesConstraint, Expression body, Context context) {
			
		checkInterrupted();
		
		Expression result;
		if (getDebug()) {
			System.out.println("SGVE(T) input: " + body);	
			System.out.println("Width        : " + width(body, context));
		}

		Partition partition;
		if (indices.size() < 1) {
			partition = null;
		}
		else {
			Expression factoredConditionalsExpression =
					factoredConditionalsWithAbsorbingElseClause(body, context);
			partition = pickPartition(factoredConditionalsExpression, indices, context);
		}

		if (partition == null) {
			if (basicOutput) {
				System.out.println("No partition");	
			}
			result = subSolver.solve(group, indices, indicesConstraint, body, context);
		}
		else {
			Expression indexSubProblemExpression = product(partition.expressionsOnIndexAndNot.first, context);
			if (basicOutput) {
				System.out.println("Eliminating: " + getFirst(partition.index));	
				System.out.println("From       : " + indexSubProblemExpression);	
				System.out.println("Width      : " + width(indexSubProblemExpression, context) + " out of " + indices.size() + " indices");	
			}

			// We now invoke the subsolver for summing the index out of the factors it is in.
			// Ideally, we would reuse the current constraint, but the set of index has changed and the current constraint may
			// use an internal representation that depends on its previous set of indices.
			// In the future, we should try to re-use that internal representation and re-index it appropriately, but for now
			// we rewrite the program in a way that the current constraint becomes a part of the input expression.
			// This will be equivalent to using it as a constraint, but will cause the constraint to be re-built.
			// BTW, the call to "project" below will also re-context the constraint for the same reason: re-indexing.
			// In the future it should also re-use the representation.
			// The following transformation is:  sum_C E   =   sum_{true} if C then E else 0
			Expression indexSubProblemExpressionWithConstraint = IfThenElse.make(indicesConstraint, indexSubProblemExpression, getSemiRing().multiplicativeAbsorbingElement());
			Expression indexSubProblemSolution = subSolver.solve(group, partition.index, indexSubProblemExpressionWithConstraint, context);

			if (basicOutput) {
				System.out.println("Solution   : " + indexSubProblemSolution + "\n");	
			}

			partition.expressionsOnIndexAndNot.second.add(indexSubProblemSolution);
			Expression remainingSubProblemExpression = product(partition.expressionsOnIndexAndNot.second, context);
			Constraint constraintOnRemainingIndices = context; // the constraint is already represented in indexSubProblemSolution
			result = solve(group, partition.remainingIndices, constraintOnRemainingIndices, remainingSubProblemExpression, context);
			result = getSemiRing().multiply(result, context);
		}

		return result;
	}

	public boolean basicOutput = false;

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
		
		public boolean isTrivial() {
			boolean result = expressionsOnIndexAndNot.first.isEmpty() || expressionsOnIndexAndNot.second.isEmpty();
			return result;
		}
	}
	
	private Partition pickPartition(Expression expression, Collection<Expression> indices, Context context) {
		Partition result;
		if (indices.isEmpty()) {
			result = null;
		}
		else {
			List<Expression> factors = getSemiRing().getFactors(expression);
			List<Partition> allPartitions = mapIntoList(indices, makePartition(indices, factors));
			result = argmin(allPartitions, width(context)); // min-fill heuristics
			if (result.isTrivial()) {
				result = null; // no need to incur in the overhead for partitioning
			}
		}
		return result;
	}

	public Function<Expression, Partition> makePartition(Collection<Expression> indices, List<Expression> expressions) {
		return index -> pickPartitionForIndex(index, indices, expressions);
	}

	public Partition pickPartitionForIndex(Expression index, Collection<Expression> indices, List<Expression> expressions) {
		Partition result;
		List<Expression> remainingIndices = removeNonDestructively(indices, index);
		Predicate<Expression> containsIndex = e -> isSubExpressionOf(index, e);
		PairOf<List<Expression>> onIndexAndNot = collectToLists(expressions, containsIndex);
		result = new Partition(index, remainingIndices, onIndexAndNot);
		return result;
	}
	
	public Function<Partition, Integer> width(Context context) {
		return partition -> width(partition, context);
	}

	private int width(Partition partition, Context context) {
		Expression product = product(partition.expressionsOnIndexAndNot.first, context);
		int result = width(product, context);
		return result;
	}

	private int width(Expression expression, Context context) {
		Set<Expression> variables = new LinkedHashSet<Expression>();
		Iterator<Expression> iterator = new SubExpressionsDepthFirstIterator(expression);
		while (iterator.hasNext()) {
			Expression subExpression = iterator.next();
			if (isVariable(subExpression, context)) {
				variables.add(subExpression);
			}
		}
		int result = variables.size();
		return result;
	}

	public Expression factoredConditionalsWithAbsorbingElseClause(Expression expression, Context context) {
		List<Expression> factors = getSemiRing().getFactors(expression);
		List<Expression> factorsAfterFactoringConditionals = factoredConditionalsWithAbsorbingElseClause(factors);
		Expression result;
		if (factorsAfterFactoringConditionals == factors) {
			result = expression;
		}
		else {
			result = product(factorsAfterFactoringConditionals, context);
		}
		return result;
	}
	
	private List<Expression> factoredConditionalsWithAbsorbingElseClause(List<Expression> factors) {
		List<Expression> result =
				nonDestructivelyExpandElementsIfFunctionReturnsNonNullCollection(
						factors,
						this::factorConditionalIfPossible);
		return result;
	}
	
	private List<Expression> factorConditionalIfPossible(Expression expression) {
		List<Expression> result = null;
		Expression nthRoot;
		if (isIfThenElse(expression) && elseBranchIsAbsorbing(expression)
				&&
				conditionIsConjunction(expression)
				&&
				(nthRoot = getNthRoot(numberOfConjuncts(expression), thenBranch(expression)))
				!= null) {

			result = mapIntoList(
					getConjuncts(condition(expression)),
					(Expression c) -> IfThenElse.make(c, nthRoot, multiplicativeAbsorbingElement()));
		}
		return result; 
	}

	public boolean conditionIsConjunction(Expression expression) {
		return isConjunction(condition(expression));
	}

	public boolean elseBranchIsAbsorbing(Expression expression) {
		return elseBranch(expression).equals(multiplicativeAbsorbingElement());
	}

	public Expression multiplicativeAbsorbingElement() {
		return getSemiRing().multiplicativeAbsorbingElement();
	}

	public int numberOfConjuncts(Expression expression) {
		return condition(expression).numberOfArguments();
	}

	private Expression getNthRoot(int n, Expression expression) {
		return getSemiRing().getNthRoot(n, expression);
	}

	private Expression product(Collection<Expression> factors, Context context) {
		Expression multiplication = apply(getSemiRing().multiplicativeFunctor(), factors);
		Expression result = getSemiRing().multiply(multiplication, context);
		return result;
	}
	
	@Override
	public String toString() {
		return "SGVE(T)";
	}
}