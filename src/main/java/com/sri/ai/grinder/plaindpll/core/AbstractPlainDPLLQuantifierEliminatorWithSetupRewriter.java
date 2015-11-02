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

import static com.sri.ai.expresso.helper.Expressions.FALSE;
import static com.sri.ai.expresso.helper.Expressions.TRUE;
import static com.sri.ai.util.Util.list;

import java.util.Collection;
import java.util.List;
import java.util.Map;

import com.google.common.base.Predicate;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.IndexExpressionsSet;
import com.sri.ai.grinder.api.Constraint;
import com.sri.ai.grinder.api.Rewriter;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.api.QuantifierEliminatorWithSetup;
import com.sri.ai.grinder.core.AbstractHierarchicalRewriter;
import com.sri.ai.grinder.core.DefaultRewritingProcess;
import com.sri.ai.grinder.helper.GrinderUtil;
import com.sri.ai.grinder.library.controlflow.IfThenElse;
import com.sri.ai.grinder.library.equality.cardinality.core.CountsDeclaration;
import com.sri.ai.grinder.library.equality.cardinality.direct.core.Simplify;
import com.sri.ai.grinder.library.indexexpression.IndexExpressions;
import com.sri.ai.grinder.plaindpll.api.Constraint1;
import com.sri.ai.grinder.plaindpll.api.ConstraintTheory;
import com.sri.ai.grinder.plaindpll.api.GroupProblemType;
import com.sri.ai.grinder.plaindpll.api.InputTheory;
import com.sri.ai.grinder.plaindpll.util.DPLLUtil;
import com.sri.ai.util.Util;
import com.sri.ai.util.base.Pair;

/**
 * A "plain" implementation of symbolic generalized DPLL (without using Grinder-style contexts and simplifications)
 * that is more ad hoc than rewriter-based approaches but also much faster.
 * 
 * This generic skeleton allows the algorithm to be used for different tasks.
 * 
 * @author braz
 *
 */
abstract public class AbstractPlainDPLLQuantifierEliminatorWithSetupRewriter extends AbstractHierarchicalRewriter implements QuantifierEliminatorWithSetup {

	private boolean debug = false;
	
	private RewritingProcess topLevelRewritingProcess;
	private boolean interrupted = false;
	
	@Override
	public boolean getDebug() {
		return debug;
	}

	@Override
	public void setDebug(boolean newValue) {
		this.debug = newValue;
	}
	
	/** The background theory for the solver's input in the algorithm. */
	protected InputTheory inputTheory;
	
	/** The background theory for constraints in the algorithm. */
	protected ConstraintTheory constraintTheory;
	
	/** The problem type being solved. */
	protected GroupProblemType problemType;

	/** A {@link CountsDeclaration} encapsulating sort size information. */
	protected CountsDeclaration countsDeclaration;
	
	public AbstractPlainDPLLQuantifierEliminatorWithSetupRewriter(InputTheory inputTheory, GroupProblemType problemType) {
		this(inputTheory, problemType, null);
	}

	public AbstractPlainDPLLQuantifierEliminatorWithSetupRewriter(InputTheory inputTheory, GroupProblemType problemType, CountsDeclaration countsDeclaration) {
		this.inputTheory = inputTheory;
		this.constraintTheory = inputTheory.getConstraintTheory();
		this.problemType = problemType;
		this.countsDeclaration = countsDeclaration;
	}
	
	public InputTheory getInputTheory() {
		return inputTheory;
	}
	
	public ConstraintTheory getConstraintTheory() {
		return constraintTheory;
	}
	
	public GroupProblemType getProblemType() {
		return problemType;
	}
	
	@Override
	public	Expression simplify(Expression expression, RewritingProcess process) {
		return getInputTheory().simplify(expression, process);
	}

	@Override
	public Expression getAdditiveIdentityElement() {
		return problemType.additiveIdentityElement();
	}
	
	@Override
	public Constraint1 makeTrueConstraint(Collection<Expression> indices) {
		return getConstraintTheory().makeConstraint(indices);
	}

	@Override
	public RewritingProcess makeProcess(
			Constraint constraint,
			Map<String, String> mapFromSymbolNameToTypeName, Map<String, String> mapFromTypeNameToSizeString,
			Predicate<Expression> isUniquelyNamedConstantPredicate) {
		
		RewritingProcess result = DPLLUtil.makeProcess(
						(Constraint1) constraint,
						mapFromSymbolNameToTypeName, mapFromTypeNameToSizeString,
						isUniquelyNamedConstantPredicate);
		return result;
	}

	@Override
	public RewritingProcess makeRewritingProcess(Expression expression) {
		Rewriter rewriterWithModules = new Simplify();
		RewritingProcess result = new DefaultRewritingProcess(expression, rewriterWithModules);
		result.notifyReadinessOfRewritingProcess();
		if (countsDeclaration != null) {
			countsDeclaration.setup(result);
		}
		return result;
	}

	/**
	 * Checks if the expression is a constraint and the constraint is TRUE,
	 * and if so inverts them so as to not have to form the body constraint all over again,
	 * and then invokes {@link #solveAfterBookkeepingAndBodyConstraintCheck(Collection, Constraint1, Expression, RewritingProcess)}.
	 * Extensions which know that the process already has the contextual constraint
	 * and that the expression is already simplified should invoke this method
	 * when needing to solve sub-problems, instead of
	 * {@link #solve(Collection, Constraint1, Expression, RewritingProcess)}
	 * in order not to repeat that effort.
	 */
	protected Expression solveAfterBookkeeping(
			Collection<Expression> indices, Constraint1 constraint, Expression body, RewritingProcess process) {
		
		checkInterrupted();
		
		Expression result;
		if (body instanceof Constraint1 && constraint.equals(TRUE)) {
			result = solveAfterBookkeepingAndBodyConstraintCheck(indices, (Constraint1) body, TRUE, process);
			// OPTIMIZATION: the right way to do this would be to allow splitters to be whole Constraints when found,
			// and have constraint conjunction to recognize that case.
		}
		else {
			result = solveAfterBookkeepingAndBodyConstraintCheck(indices, constraint, body, process);
		}
		
		return result;
	}
	
	/**
	 * @param indices
	 * @param constraint
	 * @param body
	 * @param process
	 * @return
	 */
	public Expression solve(Collection<Expression> indices, Constraint constraint, Expression body, RewritingProcess process) {
		// TODO: should replace this oldConstraint by a copy constructor creating a sub-process, but surprisingly there is no complete copy constructor available in DefaultRewritingProcess.
		Constraint1 oldConstraint = process.getDPLLContextualConstraint();
		Constraint1 contextualConstraint = getConstraintTheory().makeConstraint(Util.list()); // contextual constraint does not involve any indices -- defined on free variables only
		process.initializeDPLLContextualConstraint(contextualConstraint);

		Expression simplifiedInput = simplify(body, process);
		Expression result = solveAfterBookkeeping(indices, (Constraint1) constraint, simplifiedInput, process);
		if (result == null) { // constraint is unsatisfiable, so result is identity element.
			result = getAdditiveIdentityElement();
		}
		
		process.initializeDPLLContextualConstraint(oldConstraint);
		return result;
	}


	/**
	 * The actual solving method provided by specific solvers.
	 * Must use the contextual constraint in the process provided by {@link RewritingProcess#getDPLLContextualConstraint()}.
	 * May assume expression is already simplified.
	 * May assume expression is not an instance of Constraint.
	 * Returns null if the contextual constraint is found to be contradictory.
	 * @param indices
	 * @param constraint
	 * @param body
	 * @param process
	 * @return
	 */
	protected abstract Expression solveAfterBookkeepingAndBodyConstraintCheck(Collection<Expression> indices, Constraint1 constraint, Expression body, RewritingProcess process);

	/**
	 * Same as {@link addSymbolicResults(Expression, Expression, RewritingProcess)}
	 * called with a newly constructed default rewriting process.
	 */
	protected Expression addSymbolicResults(Expression solution1, Expression solution2) {
		DefaultRewritingProcess process = new DefaultRewritingProcess(null);
		process.initializeDPLLContextualConstraint(getConstraintTheory().makeConstraint(list()));
		return addSymbolicResults(solution1, solution2, process);
	}

	/**
	 * If solutions are unconditional expressions, simply add them.
	 * If they are conditional (symbolic), perform distributive on conditions.
	 */
	protected Expression addSymbolicResults(Expression solution1, Expression solution2, RewritingProcess process) {

		Expression result;
		
		if (solution1 == null) {
			result = solution2;
		}
		else if (solution2 == null) {
			result = solution1;
		}
		else if (process.getDPLLContextualConstraint() == null) {
			result = null;
		}
		else if (solution1.equals(problemType.additiveIdentityElement())) {
			result = constraintTheory.applyConstraintToSolution(process.getDPLLContextualConstraint(), solution2, process);
		}
		else if (solution2.equals(problemType.additiveIdentityElement())) {
			result = constraintTheory.applyConstraintToSolution(process.getDPLLContextualConstraint(), solution1, process);
		}
		else if (DPLLUtil.isConditionalSolution(solution1, constraintTheory, process)) {
			Expression splitter   = IfThenElse.condition (solution1);
			Expression thenBranch = IfThenElse.thenBranch(solution1);
			Expression elseBranch = IfThenElse.elseBranch(solution1);

			Constraint1 constraint = process.getDPLLContextualConstraint();
			Expression normalizedSplitter = constraint.normalizeSplitterGivenConstraint(splitter, process);

			if (normalizedSplitter.equals(TRUE)) {
				result = addSymbolicResults(thenBranch, solution2, process);
			}
			else if (normalizedSplitter.equals(FALSE)) {
				result = addSymbolicResults(elseBranch, solution2, process);
			}
			else {
				RewritingProcess processUnderSplitterAssertion = process.extendDPLLContextualConstraint(true,  normalizedSplitter);
				RewritingProcess processUnderSplitterNegation  = process.extendDPLLContextualConstraint(false, normalizedSplitter);
				
				Expression newThenBranch = addSymbolicResults(thenBranch, solution2, processUnderSplitterAssertion);
				Expression newElseBranch = addSymbolicResults(elseBranch, solution2, processUnderSplitterNegation);
				
				if (newThenBranch == null) {
					result = newElseBranch;
					// myAssert(newElseBranch != null, () -> "It should never be the case that the splitter *and* its negation are inconsistent with the contextual constraint.");
					// Not true; due to incompleteness, the contextual constraint might have been inconsistent but that only got detected now.
					// In this case, both then and else branches are null and the result will be null.
				}
				else if (newElseBranch == null) {
					result = newThenBranch;
					// myAssert(newThenBranch != null, () -> "It should never be the case that the splitter *and* its negation are inconsistent with the contextual constraint.");
					// Not true; due to incompleteness, the contextual constraint might have been inconsistent but that only got detected now.
					// In this case, both then and else branches are null and the result will be null.
				}
				else {
					result = IfThenElse.make(normalizedSplitter, newThenBranch, newElseBranch, false /* no simplification to condition */);
				}
			}
		}
		else if (DPLLUtil.isConditionalSolution(solution2, constraintTheory, process)) {
			Expression splitter   = IfThenElse.condition (solution2);
			Expression thenBranch = IfThenElse.thenBranch(solution2);
			Expression elseBranch = IfThenElse.elseBranch(solution2);

			Constraint1 constraint = process.getDPLLContextualConstraint();
			Expression normalizedSplitter = constraint.normalizeSplitterGivenConstraint(splitter, process);

			if (normalizedSplitter.equals(TRUE)) {
				result = addSymbolicResults(solution1, thenBranch, process);
			}
			else if (normalizedSplitter.equals(FALSE)) {
				result = addSymbolicResults(solution1, elseBranch, process);
			}
			else {
				RewritingProcess processUnderSplitterAssertion = process.extendDPLLContextualConstraint(true,  splitter);
				RewritingProcess processUnderSplitterNegation  = process.extendDPLLContextualConstraint(false, splitter);

				Expression newThenBranch = addSymbolicResults(solution1, thenBranch, processUnderSplitterAssertion);
				Expression newElseBranch = addSymbolicResults(solution1, elseBranch, processUnderSplitterNegation);
				
				if (newThenBranch == null) {
					result = newElseBranch;
					// myAssert(newElseBranch != null, () -> "It should never be the case that the splitter *and* its negation are inconsistent with the contextual constraint.");
					// Not true; due to incompleteness, the contextual constraint might have been inconsistent but that only got detected now.
					// In this case, both then and else branches are null and the result will be null.
				}
				else if (newElseBranch == null) {
					result = newThenBranch;
					// myAssert(newThenBranch != null, () -> "It should never be the case that the splitter *and* its negation are inconsistent with the contextual constraint.");
					// Not true; due to incompleteness, the contextual constraint might have been inconsistent but that only got detected now.
					// In this case, both then and else branches are null and the result will be null.
				}
				else {
					result = IfThenElse.make(normalizedSplitter, newThenBranch, newElseBranch, false /* no simplification to condition */);
				}
			}
		}
		else {
			result = problemType.add(solution1, solution2, process);
		}
	
		return result;
	}
	
	@Override
	public void interrupt() {
		interrupted = true;
		RewritingProcess topProcess = topLevelRewritingProcess;
		if (topProcess != null) {
			topProcess.interrupt();
		}
	}
	
	protected void checkInterrupted() {
		if (interrupted) {
			throw new RuntimeException("Solver Interrupted");
		}
	}

	// TODO: must remove; this is implemented Rewriter for some old-style tests
	@Override
	public Expression rewriteAfterBookkeeping(Expression expression, RewritingProcess process) {
		Pair<Expression, IndexExpressionsSet> inputAndIndexExpressions = problemType.getExpressionAndIndexExpressionsFromRewriterProblemArgument(expression, process);
		Expression input = inputAndIndexExpressions.first;
		IndexExpressionsSet indexExpressions = inputAndIndexExpressions.second;
		Expression simplifiedInput = inputTheory.simplify(input, process); // TODO: eventually this should not be needed as simplification should be lazy 
		List<Expression> indices = IndexExpressions.getIndices(indexExpressions);
		RewritingProcess subProcess = GrinderUtil.extendContextualSymbolsWithIndexExpressions(indexExpressions, process);
		Expression result = solve(simplifiedInput, indices, subProcess);
		return result;
	}
}