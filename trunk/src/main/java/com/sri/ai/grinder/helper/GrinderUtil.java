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

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

import com.google.common.annotations.Beta;
import com.google.common.base.Throwables;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.ExpressionAndContext;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.grinder.api.Rewriter;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.helper.concurrent.BranchRewriteTask;
import com.sri.ai.grinder.helper.concurrent.CallableRewriteOnBranch;
import com.sri.ai.grinder.helper.concurrent.CallableRewriteOnConditionedBranch;
import com.sri.ai.grinder.helper.concurrent.RewriteOnBranch;
import com.sri.ai.grinder.helper.concurrent.ShortCircuitOnValue;
import com.sri.ai.grinder.library.IsVariable;
import com.sri.ai.grinder.library.boole.And;
import com.sri.ai.grinder.library.boole.Not;
import com.sri.ai.grinder.library.boole.Or;
import com.sri.ai.grinder.library.controlflow.IfThenElse;
import com.sri.ai.grinder.library.equality.cardinality.CardinalityUtil;
import com.sri.ai.grinder.library.equality.formula.FormulaUtil;
import com.sri.ai.grinder.library.indexexpression.IndexExpressions;
import com.sri.ai.grinder.library.set.intensional.IntensionalSet;
import com.sri.ai.util.Util;
import com.sri.ai.util.base.Pair;
import com.sri.ai.util.collect.StackedHashMap;
import com.sri.ai.util.concurrent.BranchAndMerge;
import com.sri.ai.util.concurrent.CancelOutstandingOnFailure;
import com.sri.ai.util.concurrent.CancelOutstandingOnSuccess;

/**
 * General purpose utility routines related to grinder libraries.
 * 
 * @author oreilly
 */
@Beta
public class GrinderUtil {

	/**
	 * The key of a global object of rewriting processes that prevents the check again free variables in additional constraints
	 * (that is, that are not in the contextual variables).
	 * This is used for when global free variables are being determined. It should not be used in normal circumstances.
	 */
	public static final String DO_NOT_REQUIRE_ADDED_CONTEXTUAL_CONSTRAINT_FREE_VARIABLES_TO_BE_IN_CONTEXTUAL_VARIABLES = "Do not require added contextual constraint free variables to be in contextual variables";

	/**
	 * Takes an expression and, if it is an if then else, rearranges it so that
	 * conditions on logical variables are separated from other tests and on top
	 * if then else's. This assumes that only logical variables are arguments to
	 * equalities and disequalities, an assumption that will have to be reviewed
	 * later.
	 * 
	 * @param expressions
	 *            the expression to be tested.
	 * @param process
	 *            the process in which the rewriting is occurring.
	 * @return a rewritten expression if expression was a conditional on logical
	 *         variables that needed to be separated out, otherwise expression
	 *         unchanged is returned.
	 *         More specifically, if the input expression is of the form
	 *         if LV and Rest then Alpha else Beta,
	 *         where "LV and Rest" is a possible decomposition of the condition into
	 *         a logical variable equalities formula and a remainder,
	 *         it returns
	 *         if LV then if Rest then Alpha else Beta else Beta  
	 */
	public static Expression makeSureConditionsOnLogicalVariablesAreSeparatedAndOnTop(
			Expression expression, RewritingProcess process) {
		if (IfThenElse.isIfThenElse(expression)) {
			Expression condition = IfThenElse.getCondition(expression);
			Pair<Expression, Expression> constraintsAndRest = Expressions
					.separateEqualityFormulasOnAtomicSymbolsFromRest(condition, process);
			// If either of these are the expression "true" then I don't need to make a change.
			if (!Expressions.TRUE.equals(constraintsAndRest.first)
					&& !Expressions.TRUE.equals(constraintsAndRest.second)) {
				Expression thenBranch = makeSureConditionsOnLogicalVariablesAreSeparatedAndOnTop(
						IfThenElse.getThenBranch(expression), process);
				Expression elseBranch = makeSureConditionsOnLogicalVariablesAreSeparatedAndOnTop(
						IfThenElse.getElseBranch(expression), process);
				Expression result = IfThenElse.make(constraintsAndRest.first,
						IfThenElse.make(constraintsAndRest.second, thenBranch,
								elseBranch), elseBranch);
				return result;
			}
		}
		return expression;
	}
	
	/**
	 * Determine whether an expression is a conditional on logical variables.
	 * For e.g.:
	 * 
	 * <pre>
	 * if X = a then 1 else 2
	 * 
	 * and
	 * 
	 * if X = a and p(X) then 1 else 2
	 * </pre>
	 * 
	 * are conditionals that are conditioned on the logical variable X.
	 * 
	 * @param expressions
	 *            the expression to be tested.
	 * @param process
	 *            the process in which the rewriting is occurring.
	 * @return true if the passed in expression is a conditional on logical
	 *         variables, false otherwise.
	 */
	public static boolean isConditionalOnLogicalVariables(
			Expression expression, RewritingProcess process) {
		boolean result = false;
	
		if (IfThenElse.isIfThenElse(expression)) {
			Expression condition = IfThenElse.getCondition(expression);
			if (condition.equals(Expressions.TRUE) || condition.equals(Expressions.FALSE)) {
				// This is a degenerate case, i.e. no logical variables but
				// constants for truth or falsehood are present,
				// which we will consider to be true
				result = true;
				// Generate a warning as this shouldn't happen in practice if
				// expressions have been rewritten correctly before calling
				// this.
				Trace.log("WARNING: receiving conditionals with true or false in their conditions, these should already be rewritten: {}", expression);
			} 
			else {
				Pair<Expression, Expression> constraintsAndRest =
					Expressions.separateEqualityFormulasOnAtomicSymbolsFromRest(condition, process);
				// If this is constraintsAndRest.first = True it means we are not able to handle
				if ( ! constraintsAndRest.first.equals(Expressions.TRUE)) {
					result = true;
				}
			}
		}
	
		return result;
	}
	
	/**
	 * Utility routine for branching a rewrite process based on a condition and
	 * then merging their results into an if . then . else . structure to the
	 * caller based on the rewriting that occurs in each branch. Each branch's
	 * expressions will be constrained appropriately by the condition to ensure
	 * irrelevant messages are not introduced which can potentially cause cycles
	 * in acyclic lifted factor graphs.
	 * 
	 * @param condition
	 *            the condition on which the branching of the rewriting is
	 *            occurring.
	 * @param thenRewriter
	 *            The function to perform the rewriting on the then branch.
	 * @param thenRewriteExpressionArguments
	 *            The expressions to be used in the then branch computation
	 *            based on the value of condition = true.
	 * @param elseRewriter
	 *            The function to perform the rewriting on the else branch.
	 * @param elseRewriteExpressionArguments
	 *            The expressions to be used in the else branch computation
	 *            based on the value of condition = false.
	 * @param rewriterNameToCheckBranchReachable
	 *            an optional name for a rewriter that will be used to simplify 
	 *            any extensions to the contextual constraints.
	 * @param rewriterNameOnResultBeforeReturn
	 *            an optional name for a rewriter that will be the final rewriter 
	 *            to be applied to the result before it is returned by this function.
	 * @param process
	 *            the process in which the rewriting is occurring.
	 * @return a conditional (possibly simplified to no longer be a conditional
	 *         if the value of the condition is known) containing the results of
	 *         branching the rewriting process based on the condition.
	 */
	public static Expression branchAndMergeOnACondition(
			Expression condition,
			RewriteOnBranch thenRewriter,
			Expression[] thenRewriteExpressionArguments,
			RewriteOnBranch elseRewriter,
			Expression[] elseRewriteExpressionArguments,
			String rewriterNameToCheckBranchReachable,
			String rewriterNameOnResultBeforeReturn, 
			RewritingProcess process) {

		Expression result = null;
		
		if (IfThenElse.isIfThenElse(condition)) {
			// Externalize the condition expression
			Expression conditionalCondition = IfThenElse.getCondition(condition);
			Expression thenCondition        = IfThenElse.getThenBranch(condition);
			Expression elseCondition        = IfThenElse.getElseBranch(condition);
			
			// Handle the then branch
			RewritingProcess thenProcess = extendContextualConstraint(conditionalCondition, process);
			
			Expression thenBranch = branchAndMergeOnACondition(thenCondition,
					thenRewriter, thenRewriteExpressionArguments,
					elseRewriter, elseRewriteExpressionArguments,
					rewriterNameToCheckBranchReachable, rewriterNameOnResultBeforeReturn, thenProcess);
			
			// Handle the else branch
			Expression       notConditionalCondition = Not.make(conditionalCondition);
			RewritingProcess elseProcess = extendContextualConstraint(notConditionalCondition, process);
			
			Expression elseBranch = branchAndMergeOnACondition(elseCondition,
					thenRewriter, thenRewriteExpressionArguments,
					elseRewriter, elseRewriteExpressionArguments,
					rewriterNameToCheckBranchReachable, rewriterNameOnResultBeforeReturn, elseProcess);
			
			result = IfThenElse.make(conditionalCondition, thenBranch, elseBranch);
		} 
		else {			
			List<CallableRewriteOnConditionedBranch> rewriteTasks = new ArrayList<CallableRewriteOnConditionedBranch>();
			// Create the then branch rewriter task if necessary
			CallableRewriteOnConditionedBranch thenRewriteTask = new CallableRewriteOnConditionedBranch(
					!condition.equals(Expressions.FALSE),
					CallableRewriteOnBranch.BRANCH_TYPE_THEN,
					condition,
					thenRewriter,
					thenRewriteExpressionArguments,
					rewriterNameToCheckBranchReachable,
					process);
			
			rewriteTasks.add(thenRewriteTask);
				
			// Create the else branch rewriter task if necessary
			CallableRewriteOnConditionedBranch elseRewriteTask = new CallableRewriteOnConditionedBranch(
					!condition.equals(Expressions.TRUE),
					CallableRewriteOnBranch.BRANCH_TYPE_ELSE,
					Not.make(condition),
					elseRewriter,
					elseRewriteExpressionArguments,
					rewriterNameToCheckBranchReachable,
					process);
			
			rewriteTasks.add(elseRewriteTask);
			
			CancelOutstandingOnFailure failurePredicate = new CancelOutstandingOnFailure(true);
			BranchAndMerge.Result<List<Expression>> branchResults = 
					BranchAndMerge.execute(rewriteTasks,
							new CancelOutstandingOnSuccess<Expression>(false),
							failurePredicate);
			
			if (branchResults.failureOccurred()) {
				throw Throwables.propagate(failurePredicate.getThrowable());
			}
			
			List<Expression> results = branchResults.getResult();
			Expression thenBranch = results.get(0);
			Expression elseBranch = results.get(1);
			
			if (thenBranch == null && elseBranch == null) {
				// Neither branch could be traversed into based on the
				// current condition and contextual constraint
				result = Rewriter.FALSE_CONTEXTUAL_CONTRAINT_RETURN_VALUE;
			} 
			else if (thenBranch != null && elseBranch != null) {
				// Create if then else, if condition is unknown in advance.
				// and both branches can be traversed into based on the
				// current contextual constraint.
				result = IfThenElse.make(condition, thenBranch, elseBranch);
			}
			else if (thenBranch != null) {
				result = thenBranch;
			}
			else {
				result = elseBranch;
			}
		}

		if (rewriterNameOnResultBeforeReturn != null) {
			result = process.rewrite(rewriterNameOnResultBeforeReturn, result);
		}

		return result;
	}
	
	/**
	 * Utility routine for branching on a set of disjunct rewrite tasks and
	 * merging their results into a disjunction. Will short-circuit execution if
	 * any of the disjunct rewrite tasks return true as their result.
	 * 
	 * @param disjunctRewriters
	 *            the rewriters for each disjunct.
	 * @param process
	 *            the current rewriting process.
	 * @return a disjunction based on the results of the individual disjunct
	 *         rewriter calls.
	 */
	public static Expression branchAndMergeOnADisjunction(BranchRewriteTask[] disjunctRewriters,
			RewritingProcess process) {
		Expression result = Expressions.FALSE; // no disjuncts is equivalent to false.
		
		if (disjunctRewriters.length > 0) {
			List<CallableRewriteOnBranch> disjunctTasks = new ArrayList<CallableRewriteOnBranch>();
			for (int i = 0; i < disjunctRewriters.length; i++) {
				disjunctTasks.add(new CallableRewriteOnBranch(CallableRewriteOnBranch.BRANCH_TYPE_OR,
						disjunctRewriters[i].getRewriteOnBranch(),
						disjunctRewriters[i].getArguments(),
						process));
			}
			
			ShortCircuitOnValue shortCircuitOnTrue = new ShortCircuitOnValue(Expressions.TRUE);
			
			CancelOutstandingOnFailure failurePredicate = new CancelOutstandingOnFailure(true);
			BranchAndMerge.Result<List<Expression>> branchResults = 
					BranchAndMerge.execute(disjunctTasks,
							shortCircuitOnTrue,
							failurePredicate);
			
			if (branchResults.failureOccurred()) {
				throw Throwables.propagate(failurePredicate.getThrowable());
			} 
			
			if (shortCircuitOnTrue.isShortCircuited()) {
				result = Expressions.TRUE;
			} 
			else {
				result = Or.make(branchResults.getResult());
			}
		}
		
		return result;
	}
	
	/**
	 * Utility routine for branching on a set of conjunct rewrite tasks and
	 * merging their results into a conjunction. Will short-circuit execution if
	 * any of the conjunct rewrite tasks return false as their result.
	 * 
	 * @param conjunctRewriters
	 *            the rewriters for each conjunct.
	 * @param process
	 *            the current rewriting process.
	 * @return a conjunction based on the results of the individual conjunct
	 *         rewriter calls.
	 */
	public static Expression branchAndMergeOnAConjunction(BranchRewriteTask[] conjunctRewriters,
			RewritingProcess process) {
		Expression result = Expressions.TRUE; // no conjuncts is equivalent to true.
		
		if (conjunctRewriters.length > 0) {
			List<CallableRewriteOnBranch> conjunctTasks = new ArrayList<CallableRewriteOnBranch>();
			for (int i = 0; i < conjunctRewriters.length; i++) {
				conjunctTasks.add(new CallableRewriteOnBranch(CallableRewriteOnBranch.BRANCH_TYPE_AND,
						conjunctRewriters[i].getRewriteOnBranch(),
						conjunctRewriters[i].getArguments(),
						process));
			}
			
			ShortCircuitOnValue shortCircuitOnFalse = new ShortCircuitOnValue(Expressions.FALSE);
			
			CancelOutstandingOnFailure failurePredicate = new CancelOutstandingOnFailure(true);
			BranchAndMerge.Result<List<Expression>> branchResults = 
					BranchAndMerge.execute(conjunctTasks,
							shortCircuitOnFalse,
							failurePredicate);
			
			if (branchResults.failureOccurred()) {
				throw Throwables.propagate(failurePredicate.getThrowable());
			} 
			
			if (shortCircuitOnFalse.isShortCircuited()) {
				result = Expressions.FALSE;
			} 
			else {
				result = And.make(branchResults.getResult());
			}
		}
		
		return result;
	}

	/**
	 * Utility routine for branching on a set of arbitrary rewrite tasks and
	 * returning their results (ordered the same as the rewrite tasks).
	 * 
	 * @param taskRewriters
	 *            the rewriters for each tasg.
	 * @param process
	 *            the current rewriting process.
	 * @return a list of all of the results (ordered the same as the rewrite
	 *         tasks) from the rewrite tasks.
	 */
	public static List<Expression> branchAndMergeTasks(BranchRewriteTask[] taskRewriters,
			RewritingProcess process) {
		List<Expression> result = new ArrayList<Expression>();
		
		if (taskRewriters.length > 0) {
			List<CallableRewriteOnBranch> rewriteTasks = new ArrayList<CallableRewriteOnBranch>();
			for (int i = 0; i < taskRewriters.length; i++) {
				rewriteTasks.add(new CallableRewriteOnBranch(CallableRewriteOnBranch.BRANCH_TYPE_TASK,
						taskRewriters[i].getRewriteOnBranch(),
						taskRewriters[i].getArguments(),
						process));
			}
			
			CancelOutstandingOnFailure failurePredicate = new CancelOutstandingOnFailure(true);
			BranchAndMerge.Result<List<Expression>> branchResults = 
					BranchAndMerge.execute(rewriteTasks,
							new CancelOutstandingOnSuccess<Expression>(false),
							failurePredicate);
			
			if (branchResults.failureOccurred()) {
				throw Throwables.propagate(failurePredicate.getThrowable());
			} 
			
			result.addAll(branchResults.getResult());
		}
		
		return result;
	}
	
	/**
	 * Utility routine for branching on a set of arbitrary rewrite tasks and
	 * returning their results (ordered the same as the rewrite tasks).
	 * 
	 * @param taskRewriters
	 *            the rewriters for each tasg.
	 * @param shortCircuitValue
	 *            a value to short circuit on.
	 * @param process
	 *            the current rewriting process.
	 * @return a list of all of the results (ordered the same as the rewrite
	 *         tasks) from the rewrite tasks.
	 */
	public static List<Expression> branchAndMergeTasks(BranchRewriteTask[] taskRewriters,
			Expression shortCircuitValue,
			RewritingProcess process) {
		List<Expression> result = new ArrayList<Expression>();
		
		if (taskRewriters.length > 0) {
			List<CallableRewriteOnBranch> rewriteTasks = new ArrayList<CallableRewriteOnBranch>();
			for (int i = 0; i < taskRewriters.length; i++) {
				rewriteTasks.add(new CallableRewriteOnBranch(CallableRewriteOnBranch.BRANCH_TYPE_TASK,
						taskRewriters[i].getRewriteOnBranch(),
						taskRewriters[i].getArguments(),
						process));
			}
			
			ShortCircuitOnValue shortCircuitOnValue = new ShortCircuitOnValue(shortCircuitValue);
			
			CancelOutstandingOnFailure failurePredicate = new CancelOutstandingOnFailure(true);
			BranchAndMerge.Result<List<Expression>> branchResults = 
					BranchAndMerge.execute(rewriteTasks,
							shortCircuitOnValue,
							failurePredicate);
			
			if (branchResults.failureOccurred()) {
				throw Throwables.propagate(failurePredicate.getThrowable());
			} 
			
			if (shortCircuitOnValue.isShortCircuited()) {
				result.add(shortCircuitValue);
			} 
			else {
				result.addAll(branchResults.getResult());
			}
		}
		
		return result;
	}

	/**
	 * Same as {@link #extendContextualVariablesAndConstraintWithIntensionalSetInferringDomainsFromUsageInRandomVariables(Expression, RewritingProcess)},
	 * but only for the indices (that is, it does not extend the contextual constraint with the intensional set's condition).
	 */
	public static RewritingProcess extendContextualVariablesWithIntensionalSetIndices(Expression intensionalSet, RewritingProcess process) {
		Map<Expression, Expression> quantifiedVariablesAndDomains = IntensionalSet.getIndexToDomainMapWithDefaultNull(intensionalSet);
		RewritingProcess result = GrinderUtil.extendContextualVariablesAndConstraint(quantifiedVariablesAndDomains, Expressions.TRUE, process);
		return result;
	}

	/**
	 * Same as {@link #extendContextualVariablesAndConstraintWithIntensionalSetInferringDomainsFromUsageInRandomVariables(Expression, RewritingProcess)},
	 * but given the index expressions only.
	 */
	public static RewritingProcess extendContextualVariablesWithIndexExpressions(Collection<Expression> indexExpressions, RewritingProcess process) {
		Map<Expression, Expression> quantifiedVariablesAndDomains = IndexExpressions.getIndexToDomainMapWithDefaultNull(indexExpressions);
		RewritingProcess result = GrinderUtil.extendContextualVariablesAndConstraint(quantifiedVariablesAndDomains, Expressions.TRUE, process);
		return result;
	}

	/**
	 * Same as {@link #extendContextualVariablesWithIndexExpressions(Collection<Expression>, RewritingProcess)},
	 * but given a single index expression only.
	 */
	public static RewritingProcess extendContextualVariablesWithIndexExpression(Expression indexExpression, RewritingProcess process) {
		return extendContextualVariablesWithIndexExpressions(Util.list(indexExpression), process);
	}

	/**
	 * Extend the rewriting processes's contextual variables and constraints
	 * with the indices and condition from an intensionally defined set.
	 * 
	 * @param intensionalSet
	 * @param process
	 *            the process in which the rewriting is occurring and whose
	 *            contextual constraint is to be updated.
	 * @return a sub-rewriting process with its contextual variables and
	 *         constraints extended by the indices and condition of the intensionally defined set passed in.
	 */
	public static RewritingProcess extendContextualVariablesAndConstraintWithIntensionalSet(
			Expression intensionalSet, RewritingProcess process) {
		Map<Expression, Expression> quantifiedVariablesAndDomains = IntensionalSet.getIndexToDomainMapWithDefaultNull(intensionalSet);
		Expression conditionOnExpansion = IntensionalSet.getCondition(intensionalSet);
		RewritingProcess result = GrinderUtil.extendContextualVariablesAndConstraint(quantifiedVariablesAndDomains, conditionOnExpansion, process);
		return result;
	}

	/**
	 * Extend the rewriting processes's contextual constraint by an additional
	 * context. This essentially creates a new expression which is a conjunction
	 * of both constraints and then performs R_formula_simplification on the
	 * conjunction before setting it as the processes new contextual constraint.
	 * 
	 * @param additionalConstraints
	 *            additional context to extend the contextual constraint by.
	 * @param process
	 *            the process in which the rewriting is occurring and whose
	 *            contextual constraint is to be updated.
	 * @return a sub-rewriting process constrained by
	 *         'process.getContexualConstraint() and additionalContext'.
	 */
	public static RewritingProcess extendContextualConstraint(Expression additionalConstraints, RewritingProcess process) {
		
		return extendContextualVariablesAndConstraint(
				new HashMap<Expression, Expression>(),
				additionalConstraints,
				process);
	}
	
	/**
	 * Extend the rewriting processes's contextual variables and constraints.
	 * 
	 * @param expressionAndContext
	 *            an expression that possibly contains free variables that
	 *            should be added to the a new sub-process of the process passed
	 *            in, with a context that encodes a condition on that context.
	 * @param process
	 *            the process in which the rewriting is occurring and whose
	 *            contextual constraint is to be updated.
	 * @return a sub-rewriting process with its contextual variables and
	 *         constraints extended by the expression and context passed in.
	 */
	public static RewritingProcess extendContextualVariablesAndConstraint(
			ExpressionAndContext expressionAndContext, 
			RewritingProcess process) {
		Map<Expression, Expression> quantifiedVariablesDomains = IndexExpressions.getIndexToDomainMapWithDefaultNull(expressionAndContext.getIndexExpressions());
		Expression                  conditionOnExpression      = expressionAndContext.getConstrainingCondition();
		RewritingProcess result = extendContextualVariablesAndConstraint(quantifiedVariablesDomains, conditionOnExpression, process);
		return result;
	}
	
	/**
	 * Extends a process's contextual variables with free variables found in a given expression and returns the new resulting process.
	 */
	public static RewritingProcess extendContextualVariablesWithFreeVariablesInExpressionWithUnknownDomain(Expression expression, RewritingProcess process) {
		Set<Expression> freeVariables = Expressions.freeVariables(expression, process);
		process = extendContextualVariablesWithUnknownDomain(freeVariables, process);
		return process;
	}

	public static RewritingProcess extendContextualVariablesWithUnknownDomain(Set<Expression> variables, RewritingProcess process) {
		Map<Expression, Expression> fromVariableToDomain = new HashMap<Expression, Expression>();
		for (Expression variable : variables) {
			fromVariableToDomain.put(variable, null);
		}
		RewritingProcess result = extendContextualVariables(fromVariableToDomain, process);
		return result;
	}

	/**
	 * Same as {@link #extendContextualVariablesAndConstraint(Map<Expression, Expression>, Expression, RewritingProcess)},
	 * assuming a true constraint.
	 */
	public static RewritingProcess extendContextualVariables(Map<Expression, Expression> freeVariablesAndDomains, RewritingProcess process) {
		RewritingProcess result = extendContextualVariablesAndConstraint(freeVariablesAndDomains, Expressions.TRUE, process);
		return result;
	}

	/**
	 * Extend the rewriting processes's contextual variables and constraints.
	 * Returns the same process instance if there are no changes.
	 * 
	 * @param extendingContextualVariablesAndDomains
	 *            a map from variables to their domains that
	 *            should be added to the a new sub-process of the process passed
	 *            in.
	 * @param additionalConstraints
	 *            additional context (i.e. a formula) to extend the contextual 
	 *            constraint by.
	 * @param process
	 *            the process in which the rewriting is occurring and whose
	 *            contextual constraint is to be updated.
	 * @return a sub-rewriting process with its contextual variables and
	 *         constraints extended by the arguments passed in,
	 *         possibly the same as input process if no changes are made.
	 */
	public static RewritingProcess extendContextualVariablesAndConstraint(
			Map<Expression, Expression> extendingContextualVariablesAndDomains,
			Expression additionalConstraints, 
			RewritingProcess process) {
		
		if (extendingContextualVariablesAndDomains.isEmpty() && additionalConstraints.equals(Expressions.TRUE)) { // nothing to do
			return process;
		}
		
		for (Map.Entry entry : extendingContextualVariablesAndDomains.entrySet()) {
			if (entry.getValue() != null && ((Expression)entry.getValue()).hasFunctor("type")) {
				throw new Error("'type' occurring in domains extending context: " + entry);
			}
		}
		
		Map<Expression, Expression> newContextualVariablesAndDomains = new StackedHashMap<Expression, Expression>(process.getContextualVariablesAndDomains());
		if (extendingContextualVariablesAndDomains != null) {
			// we take only the logical variables; this is a current limitation of the system and should eventually be removed.
			Collection<Expression> newFreeVariablesWhichAreLogicalVariables = Util.filter(extendingContextualVariablesAndDomains.keySet(), new IsVariable(process));
			for (Expression newLogicalFreeVariable : newFreeVariablesWhichAreLogicalVariables) {
				newContextualVariablesAndDomains.put(newLogicalFreeVariable, extendingContextualVariablesAndDomains.get(newLogicalFreeVariable));
			}
		}
		
		Expression contextualConstraintPrime = process.getContextualConstraint();
		
		// Only extend the contextual constraint with formulas
		if (!additionalConstraints.equals(Expressions.TRUE) && FormulaUtil.isFormula(additionalConstraints, process)) {
			// Ensure any variables mentioned in the additional constraint are already present in the contextual variables set.
			Set<Expression> freeVariablesInAdditionalConstraints = Expressions.freeVariables(additionalConstraints, process);
			if ( ! newContextualVariablesAndDomains.keySet().containsAll(freeVariablesInAdditionalConstraints) &&
					! process.containsGlobalObjectKey("Do not require added contextual constraint free variables to be in contextual variables")) {
				String message =
						"Extending contextual constraint " + additionalConstraints +
						" containing unknown variables {" + Util.join(Util.subtract(freeVariablesInAdditionalConstraints, newContextualVariablesAndDomains.keySet())) + 
						"} (current contextual variables are {" + Util.join(newContextualVariablesAndDomains.keySet()) + "})";
				throw new Error(message);
			}
			// The check above ensures that the additional constraints only use variables that are already known.
			// When this fails, the cause is a failure in the code somewhere to extend the process with scoping variables.
			// The easiest way to debug this is to place a breakpoint at the line above and, when it is reached, inspect the stack,
			// looking for the point in which the expression being process involves variables not in the process contextual variables.
			// This point will be the spot where the process should have been extended.
			
			// Construct a conjunct of contextual constraints extended by the additional context
			contextualConstraintPrime = CardinalityUtil.makeAnd(process.getContextualConstraint(), additionalConstraints);
		} 
		else {
			// Note: commenting out for now due to the bloat caused in the trace output.
			// Trace.log("INFO: Not a formula to extend contextual constraint by: {}", additionalConstraints);
		}
		
		for (Expression v : newContextualVariablesAndDomains.keySet()) {
			if (!process.isVariable(v)) {
				throw new IllegalArgumentException("Illegal argument to extend contextual variables with:"+v);
			}
		}
		
		RewritingProcess subRewritingProcess = process.newSubProcessWithContext(newContextualVariablesAndDomains, contextualConstraintPrime);
		
		return subRewritingProcess;	
	}

	public static Expression currentContextBranchReachable(String rewriterNameToCheckBranchReachable, RewritingProcess parentProcess, RewritingProcess childProcess) {
		Expression result = Expressions.TRUE;
		
		// if the child process is false or the parent and child process are the
		// same then just return the child processes value for this (as if
		// the processes are the same the parent should ideally have already
		// been simplified).
		if (childProcess.getContextualConstraint().equals(Expressions.FALSE) ||
			childProcess == parentProcess) {
			result = childProcess.getContextualConstraint();
		} 
		else {
			if (rewriterNameToCheckBranchReachable != null) {
				// Note: check if the child branch is reachable based on the parent context.
				result = parentProcess.rewrite(rewriterNameToCheckBranchReachable, childProcess.getContextualConstraint());
			}
		}
		
		return result;
	}

	public static List<Rewriter> addRewritersBefore(List<Rewriter> rewriters,
			Pair<Class<?>, Rewriter>... rwsBefore) {
		for (int i = 0; i < rwsBefore.length; i++) {
			addRewriterBefore(rewriters, rwsBefore[i].first, rwsBefore[i].second);
		}
		return rewriters;
	}

	public static List<Rewriter> addRewriterBefore(List<Rewriter> rewriters,
			Class<?> clazzBefore, Rewriter addR) {
		boolean found = false;
		for (int i = 0; i < rewriters.size(); i++) {
			if (clazzBefore == rewriters.get(i).getClass()) {
				rewriters.add(i, addR);
				found = true;
				break;
			}
		}
	
		if (!found) {
			throw new IllegalArgumentException(
					"Cannot find rewriter to add new rewriter before:"
							+ clazzBefore.getName());
		}
	
		return rewriters;
	}
}
