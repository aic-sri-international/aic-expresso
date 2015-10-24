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

import static com.sri.ai.expresso.helper.Expressions.apply;
import static com.sri.ai.expresso.helper.Expressions.isSubExpressionOf;
import static com.sri.ai.grinder.library.boole.And.getConjuncts;
import static com.sri.ai.grinder.library.boole.And.isConjunction;
import static com.sri.ai.grinder.library.controlflow.IfThenElse.condition;
import static com.sri.ai.grinder.library.controlflow.IfThenElse.elseBranch;
import static com.sri.ai.grinder.library.controlflow.IfThenElse.isIfThenElse;
import static com.sri.ai.grinder.library.controlflow.IfThenElse.thenBranch;
import static com.sri.ai.util.Util.argmin;
import static com.sri.ai.util.Util.collectToLists;
import static com.sri.ai.util.Util.getFirst;
import static com.sri.ai.util.Util.list;
import static com.sri.ai.util.Util.mapIntoList;
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
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.api.Solver;
import com.sri.ai.grinder.library.controlflow.IfThenElse;
import com.sri.ai.grinder.plaindpll.api.Constraint1;
import com.sri.ai.grinder.plaindpll.api.SemiRingProblemType;
import com.sri.ai.grinder.plaindpll.core.AbstractPlainDPLLSolver;
import com.sri.ai.grinder.plaindpll.core.SGDPLLT;
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
 * <p>
 * This used to be an extension of {@link AbstractPlainDPLLSolver} but was abstracted for reuse by
 * implementations of {@link Solver} that are not extensions of {@link AbstractPlainDPLLSolver};
 * it now only concerns itself with implementing ways to do Variable Elimination,
 * without committing to the specifics of {@link AbstractPlainDPLLSolver}.
 * This way, it can be used as an internal field of extensions of {@link AbstractPlainDPLLSolver},
 * but can also be used in the same way by implementations of {@link Solver}
 * that are not extensions of {@link AbstractPlainDPLLSolver} and which choose to implement
 * the remaining functionality of Solver in other ways.
 * This can be seen as a simulated form of multiple inheritance
 * (some classes will inherit from {@link AbstractPlainDPLLSolver} and "inherit" from this class by encapsulated object).
 * 
 * @author braz
 *
 */
public abstract class AbstractSGVETFunctionality {

	private Solver subSolver;
	private SemiRingProblemType problemType;
	private boolean debug;
	
	public AbstractSGVETFunctionality(Solver subSolver, SemiRingProblemType problemType) {
		this.subSolver = subSolver;
		this.problemType = problemType;
	}

	/**
	 * Indicates whether an expression is a variable in the constraint theory used by this solver.
	 * @param expression
	 * @param process
	 * @return
	 */
	public abstract boolean isVariable(Expression subExpression, RewritingProcess process);

	abstract protected Constraint1 makeTrueConstraint(List<Expression> remainingIndices);

	public boolean getDebug() {
		return debug;
	}
	
	public void setDebug(boolean newValue) {
		debug = newValue;;
	}
	
	public SemiRingProblemType getProblemType() {
		return problemType;
	}
	
	public void interrupt() {
		subSolver.interrupt();
	}
	
	public Expression solve(Expression expression, Collection<Expression> indices, Constraint1 constraint, RewritingProcess process) {
			
			Expression result;
			if (getDebug()) {
				System.out.println("SGVE(T) input: " + expression);	
				System.out.println("Width        : " + width(expression, process));
			}
			
			Partition partition;
			if (indices.size() < 1) {
				partition = null;
			}
			else {
				Expression factoredConditionalsExpression =
						factoredConditionalsWithAbsorbingElseClause(expression, process);
				partition = pickPartition(factoredConditionalsExpression, indices, process);
			}
			
			if (partition == null) {
				if (basicOutput) {
					System.out.println("No partition");	
				}
				result = subSolver.solve(expression, indices, constraint, process);
			}
			else {
				Expression indexSubProblemExpression = product(partition.expressionsOnIndexAndNot.first, process);
				if (basicOutput) {
					System.out.println("Eliminating: " + getFirst(partition.index));	
					System.out.println("From       : " + indexSubProblemExpression);	
					System.out.println("Width      : " + width(indexSubProblemExpression, process) + " out of " + indices.size() + " indices");	
				}
	
				// We now invoke the subsolver for summing the index out of the factors it is in.
				// Ideally, we would reuse the current constraint, but the set of index has changed and the current constraint may
				// use an internal representation that depends on its previous set of indices.
				// In the future, we should try to re-use that internal representation and re-index it appropriately, but for now
				// we rewrite the program in a way that the current constraint becomes a part of the input expression.
				// This will be equivalent to using it as a constraint, but will cause the constraint to be re-built.
				// BTW, the call to "project" below will also re-process the constraint for the same reason: re-indexing.
				// In the future it should also re-use the representation.
				// The following transformation is:  sum_C E   =   sum_{true} if C then E else 0
				Expression indexSubProblemExpressionWithConstraint = IfThenElse.make(constraint, indexSubProblemExpression, getProblemType().multiplicativeAbsorbingElement());
				Expression indexSubProblemSolution = subSolver.solve(indexSubProblemExpressionWithConstraint, partition.index, process);
				
				if (basicOutput) {
					System.out.println("Solution   : " + indexSubProblemSolution + "\n");	
				}
				
				partition.expressionsOnIndexAndNot.second.add(indexSubProblemSolution);
				Expression remainingSubProblemExpression = product(partition.expressionsOnIndexAndNot.second, process);
				Constraint1 trueConstraintOnRemainingIndices = makeTrueConstraint(partition.remainingIndices);
				Constraint1 constraintOnRemainingIndices = trueConstraintOnRemainingIndices; // the constraint is already represented in indexSubProblemSolution
				result = solve(remainingSubProblemExpression, partition.remainingIndices, constraintOnRemainingIndices, process);
				result = getProblemType().multiply(result, process);
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
	
	private Partition pickPartition(Expression expression, Collection<Expression> indices, RewritingProcess process) {
		Partition result;
		if (indices.isEmpty()) {
			result = null;
		}
		else {
			List<Expression> factors = getProblemType().getFactors(expression);
			List<Partition> allPartitions = mapIntoList(indices, makePartition(indices, factors));
			result = argmin(allPartitions, width(process)); // min-fill heuristics
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
	
	public Function<Partition, Integer> width(RewritingProcess process) {
		return partition -> width(partition, process);
	}

	private int width(Partition partition, RewritingProcess process) {
		Expression product = product(partition.expressionsOnIndexAndNot.first, process);
		int result = width(product, process);
		return result;
	}

	private int width(Expression expression, RewritingProcess process) {
		Set<Expression> variables = new LinkedHashSet<Expression>();
		Iterator<Expression> iterator = new SubExpressionsDepthFirstIterator(expression);
		while (iterator.hasNext()) {
			Expression subExpression = iterator.next();
			if (isVariable(subExpression, process)) {
				variables.add(subExpression);
			}
		}
		int result = variables.size();
		return result;
	}

	public Expression factoredConditionalsWithAbsorbingElseClause(Expression expression, RewritingProcess process) {
		List<Expression> factors = getProblemType().getFactors(expression);
		List<Expression> factorsAfterFactoringConditionals = factoredConditionalsWithAbsorbingElseClause(factors);
		Expression result;
		if (factorsAfterFactoringConditionals == factors) {
			result = expression;
		}
		else {
			result = product(factorsAfterFactoringConditionals, process);
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
		return getProblemType().multiplicativeAbsorbingElement();
	}

	public int numberOfConjuncts(Expression expression) {
		return condition(expression).numberOfArguments();
	}

	private Expression getNthRoot(int n, Expression expression) {
		return getProblemType().getNthRoot(n, expression);
	}

	private Expression product(Collection<Expression> factors, RewritingProcess process) {
		Expression multiplication = apply(getProblemType().multiplicativeFunctor(), factors);
		Expression result = getProblemType().multiply(multiplication, process);
		return result;
	}
	
	@Override
	public String toString() {
		return "SGVE(T)";
	}
}