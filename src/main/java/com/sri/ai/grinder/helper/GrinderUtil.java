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

import static com.sri.ai.expresso.helper.Expressions.parse;
import static com.sri.ai.grinder.library.FunctorConstants.CARTESIAN_PRODUCT;
import static com.sri.ai.util.Util.list;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Set;

import com.google.common.annotations.Beta;
import com.google.common.base.Throwables;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.ExpressionAndContext;
import com.sri.ai.expresso.api.IndexExpressionsSet;
import com.sri.ai.expresso.api.IntensionalSet;
import com.sri.ai.expresso.api.LambdaExpression;
import com.sri.ai.expresso.api.QuantifiedExpression;
import com.sri.ai.expresso.core.ExtensionalIndexExpressionsSet;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.expresso.helper.SubExpressionsDepthFirstIterator;
import com.sri.ai.grinder.GrinderConfiguration;
import com.sri.ai.grinder.api.Rewriter;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.core.DefaultRewritingProcess;
import com.sri.ai.grinder.helper.concurrent.BranchRewriteTask;
import com.sri.ai.grinder.helper.concurrent.CallableRewriteOnBranch;
import com.sri.ai.grinder.helper.concurrent.CallableRewriteOnConditionedBranch;
import com.sri.ai.grinder.helper.concurrent.RewriteOnBranch;
import com.sri.ai.grinder.helper.concurrent.ShortCircuitOnValue;
import com.sri.ai.grinder.library.FunctorConstants;
import com.sri.ai.grinder.library.IsVariable;
import com.sri.ai.grinder.library.SemanticSubstitute;
import com.sri.ai.grinder.library.Unification;
import com.sri.ai.grinder.library.boole.And;
import com.sri.ai.grinder.library.boole.Not;
import com.sri.ai.grinder.library.boole.Or;
import com.sri.ai.grinder.library.controlflow.IfThenElse;
import com.sri.ai.grinder.library.equality.cardinality.CardinalityUtil;
import com.sri.ai.grinder.library.equality.cardinality.direct.core.CardinalityOfType;
import com.sri.ai.grinder.library.equality.cardinality.direct.core.CardinalityOfType.TypeSizeOfSymbolOrType;
import com.sri.ai.grinder.library.equality.formula.FormulaUtil;
import com.sri.ai.grinder.library.function.InjectiveModule;
import com.sri.ai.grinder.library.indexexpression.IndexExpressions;
import com.sri.ai.grinder.library.set.tuple.Tuple;
import com.sri.ai.grinder.ui.TreeUtil;
import com.sri.ai.util.AICUtilConfiguration;
import com.sri.ai.util.Configuration;
import com.sri.ai.util.Util;
import com.sri.ai.util.base.NotContainedBy;
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

	public static boolean usePlain = true;

	/**
	 * The key of a global object of rewriting processes that prevents the check again free variables in additional constraints
	 * (that is, that are not in the contextual symbols).
	 * This is used for when global free variables are being determined. It should not be used in normal circumstances.
	 */
	public static final String DO_NOT_REQUIRE_ADDED_CONTEXTUAL_CONSTRAINT_FREE_SYMBOLS_TO_BE_IN_CONTEXTUAL_VARIABLES = "Do not require added contextual constraint free variables to be in contextual symbols";

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
	 * Returns a rewriting process with contextual symbols extended by an intensional set's indices.
	 */
	public static RewritingProcess extendContextualSymbolsWithIntensionalSetIndices(Expression intensionalSet, RewritingProcess process) {
		Map<Expression, Expression> indexToTypeMap = IndexExpressions.getIndexToTypeMapWithDefaultNull(intensionalSet);
		RewritingProcess result = GrinderUtil.extendContextualSymbolsAndConstraint(indexToTypeMap, Expressions.TRUE, process);
		return result;
	}

	/**
	 * Returns a rewriting process with contextual symbols extended by a list of index expressions.
	 */
	public static RewritingProcess extendContextualSymbolsWithIndexExpressions(IndexExpressionsSet indexExpressions, RewritingProcess process) {
		Map<Expression, Expression> indexToTypeMap = IndexExpressions.getIndexToTypeMapWithDefaultNull(indexExpressions);
		RewritingProcess result = GrinderUtil.extendContextualSymbolsAndConstraint(indexToTypeMap, Expressions.TRUE, process);
		return result;
	}

	/**
	 * Returns a rewriting process with contextual symbols extended by a list of index expressions.
	 */
	public static RewritingProcess extendContextualSymbolsWithIndexExpressions(List<Expression> indexExpressions, RewritingProcess process) {
		return extendContextualSymbolsWithIndexExpressions(new ExtensionalIndexExpressionsSet(indexExpressions), process);
	}

	/**
	 * Same as {@link #extendContextualSymbolsWithIndexExpressions(Collection<Expression>, RewritingProcess)},
	 * but given a single index expression only.
	 */
	public static RewritingProcess extendContextualSymbolsWithIndexExpression(Expression indexExpression, RewritingProcess process) {
		return extendContextualSymbolsWithIndexExpressions(Util.list(indexExpression), process);
	}

	/**
	 * Extend the rewriting processes's contextual symbols and constraints
	 * with the indices and condition from an intensionally defined set.
	 * 
	 * @param intensionalSet
	 * @param process
	 *            the process in which the rewriting is occurring and whose
	 *            contextual constraint is to be updated.
	 * @return a sub-rewriting process with its contextual symbols and
	 *         constraints extended by the indices and condition of the intensionally defined set passed in.
	 */
	public static RewritingProcess extendContextualSymbolsAndConstraintWithIntensionalSet(
			Expression intensionalSet, RewritingProcess process) {
		Map<Expression, Expression> indexToTypeMap = IndexExpressions.getIndexToTypeMapWithDefaultNull(intensionalSet);
		Expression conditionOnExpansion = ((IntensionalSet) intensionalSet).getCondition();
		RewritingProcess result = GrinderUtil.extendContextualSymbolsAndConstraint(indexToTypeMap, conditionOnExpansion, process);
		return result;
	}

	/**
	 * Extend the rewriting processes's contextual constraint by an additional
	 * constraint.
	 * 
	 * @param additionalConstraints
	 *            additional context to extend the contextual constraint by.
	 * @param process
	 *            the process in which the rewriting is occurring and whose
	 *            contextual constraint is to be updated.
	 * @return a sub-rewriting process constrained by
	 *         'process.getContexualConstraint() and additionalConstraints'.
	 */
	public static RewritingProcess extendContextualConstraint(Expression additionalConstraints, RewritingProcess process) {
		
		return extendContextualSymbolsAndConstraint(
				new LinkedHashMap<Expression, Expression>(),
				additionalConstraints,
				process);
	}
	
	/**
	 * Extend the rewriting processes's contextual symbols and constraints.
	 * 
	 * @param expressionAndContext
	 *            an expression that possibly scopes new symbols that
	 *            should be added to the a new sub-process of the process passed
	 *            in, with a context that encodes a condition on that context.
	 * @param process
	 *            the process in which the rewriting is occurring and whose
	 *            contextual constraint is to be updated.
	 * @return a sub-rewriting process with its contextual symbols and
	 *         constraints extended by the expression and context passed in.
	 */
	public static RewritingProcess extendContextualSymbolsAndConstraint(
			ExpressionAndContext expressionAndContext, 
			RewritingProcess process) {
		Map<Expression, Expression> fromIndexToTypeMap = IndexExpressions.getIndexToTypeMapWithDefaultNull(expressionAndContext.getIndexExpressions());
		Expression conditionOnExpression = expressionAndContext.getConstrainingCondition();
		RewritingProcess result = extendContextualSymbolsAndConstraint(fromIndexToTypeMap, conditionOnExpression, process);
		return result;
	}
	
	/**
	 * Extends a process's contextual symbols with free variables found in a given expression and returns the new resulting process.
	 * This method should be used only as a setup method; during ordinary processing, all free variables should already be in the context.
	 * IMPORTANT: if a problem is defined by a few separate expressions that may share a free variable
	 * (typical case is a unit test with an input expression, a contextual constraint and perhaps an expected result)
	 * then one must NOT extend the context with the free variables from these separate expressions one expression at a time,
	 * because this will consider the occurrences of that shared free variable as of distinct variables,
	 * and each extension will shadow the previous ones.
	 * Instead, one must create a tuple of expressions and extend the context with them all at the same time.
	 */
	public static RewritingProcess extendContextualSymbolsWithFreeSymbolsInExpressionwithUnknownTypeForSetUpPurposesOnly(Expression expression, RewritingProcess process) {
		Set<Expression> freeSymbols = Expressions.freeSymbols(expression, process);
		Map<Expression, Expression> fromSymbolToType = new LinkedHashMap<Expression, Expression>();
		for (Expression symbol : freeSymbols) {
			fromSymbolToType.put(symbol, null);
		}
		RewritingProcess result = extendContextualSymbols(fromSymbolToType, process);
		return result;
	}

	/**
	 * Same as {@link #extendContextualSymbolsAndConstraint(Map<Expression, Expression>, Expression, RewritingProcess)},
	 * assuming a true constraint.
	 */
	public static RewritingProcess extendContextualSymbols(Map<Expression, Expression> freeSymbolsAndTypes, RewritingProcess process) {
		RewritingProcess result = extendContextualSymbolsAndConstraint(freeSymbolsAndTypes, Expressions.TRUE, process);
		return result;
	}

	/**
	 * Gets a map from indices to their types and returns a map from their functors-or-symbols
	 * (that is, their functors if they are function application, and themselves if they are symbols)
	 * to their types.
	 * A function has a type of the form <code>'->'('x'(T1, ..., Tn), R)</code>, where <code>T1,...,Tn</code>
	 * are the types of their arguments and <code>R</code> are their co-domain.
	 */
	public static Map<Expression, Expression> getTypesOfIndicesFunctorsOrSymbols(Map<Expression, Expression> fromIndicesToType, RewritingProcess process) {
		Map<Expression, Expression> result = new LinkedHashMap<Expression, Expression>();
		for (Map.Entry<Expression, Expression> entry : fromIndicesToType.entrySet()) {
			Expression index     = entry.getKey();
			Expression indexType = entry.getValue();
			if (index.getSyntacticFormType().equals("Symbol")) {
				result.put(index, indexType);
			}
			else if (index.getSyntacticFormType().equals("Function application")) {
				Expression typeOfFunctor = getTypeOfFunctor(index, indexType, process);
				result.put(index.getFunctorOrSymbol(), typeOfFunctor);
			}
			else {
				throw new Error("getTypesOfIndicesFunctorsOrSymbols not supported for expressions other than symbols and function applications, but invoked on " + index);
			}
		}
		return result;
	}
	
	/**
	 * Gets a function application and its type <code>T</code>, and returns the inferred type of its functor,
	 * which is <code>'->'('x'(T1, ..., Tn), T)</code>, where <code>T1,...,Tn</code> are the types.
	 */
	public static Expression getTypeOfFunctor(Expression functionApplication, Expression functionApplicationType, RewritingProcess process) {
		Expression result;
		if (functionApplication.getSyntacticFormType().equals("Function application")) {
			List<Expression> argumentTypes = Util.mapIntoArrayList(functionApplication.getArguments(), new GetType(process));
			if (argumentTypes.contains(null)) {
				result = null; // unknown type
			}
			else {
				Expression argumentTypesTuple = Tuple.make(argumentTypes);
				result = Expressions.apply("->", argumentTypesTuple, functionApplication);
			}
		}
		else {
			throw new Error("getTypeOfFunctor applicable to function applications only, but invoked on " + functionApplication);
		}
		return result;
	}

	/**
	 * Returns the type of given expression according to process.
	 */
	public static Expression getType(Expression expression, RewritingProcess process) {
		Expression result;
		if (expression.getSyntacticFormType().equals("Symbol")) {
			result = process.getContextualSymbolType(expression);
		}
		else if (expression.getSyntacticFormType().equals("Function application")) {
			Expression functionType = getType(expression.getFunctor(), process);
			assert functionType.hasFunctor("->") : "Functor " + expression.getFunctor() + " in expression " + expression + " should have functional type be an expression with functor '->', but has type instead equal to " + functionType;
			
			List<Expression> argumentsTypesList;
			Expression coDomain;
			if (functionType.numberOfArguments() == 1) {
				argumentsTypesList = Util.list();
				coDomain = functionType.get(0);
			}
			else {
				Expression argumentsType = functionType.get(0);
				boolean multipleArguments = argumentsType.hasFunctor(CARTESIAN_PRODUCT);
				argumentsTypesList = multipleArguments? argumentsType.getArguments() : list(argumentsType);
				coDomain = functionType.get(1);
			}
			assert Util.mapIntoList(expression.getArguments(), new GetType(process)).equals(argumentsTypesList) : "Function " + expression.getFunctor() + " is of type " + functionType + " but is applied to " + expression.getArguments() + " which are of types " + Util.mapIntoList(expression.getArguments(), new GetType(process));

			result = coDomain;
		}
		else {
			throw new Error("Type implemented only for symbols and function applications, but attempted on " + expression);
		}
		return result;
	}

	/**
	 * Extend the rewriting processes's contextual symbols and constraints.
	 * Returns the same process instance if there are no changes.
	 * 
	 * @param extendingContextualSymbolsAndTypes
	 *            a map from variables to their types that
	 *            should be added to the a new sub-process of the process passed
	 *            in.
	 * @param additionalConstraints
	 *            additional context (i.e. a formula) to extend the contextual 
	 *            constraint by.
	 * @param process
	 *            the process in which the rewriting is occurring and whose
	 *            contextual constraint is to be updated.
	 * @return a sub-rewriting process with its contextual symbols and
	 *         constraints extended by the arguments passed in,
	 *         possibly the same as input process if no changes are made.
	 */
	public static RewritingProcess extendContextualSymbolsAndConstraint(
			Map<Expression, Expression> extendingContextualSymbolsAndTypes,
			Expression additionalConstraints, 
			RewritingProcess process) {
		
		if (extendingContextualSymbolsAndTypes.isEmpty() && additionalConstraints.equals(Expressions.TRUE)) { // nothing to do
			return process;
		}
		
		doNotAcceptTypesContainingTypeExpression(extendingContextualSymbolsAndTypes);
		
		process = renameExistingContextualSymbolsIfThereAreCollisions(extendingContextualSymbolsAndTypes, process);
		
		StackedHashMap<Expression, Expression> newMapOfContextualSymbolsAndTypes = createNewMapOfContextualSymbolsAndTypes(extendingContextualSymbolsAndTypes, process);
		// Note: StackedHashMap shares original entries with the original process's map
		
		Expression newContextualConstraint = checkAndAddNewConstraints(additionalConstraints, newMapOfContextualSymbolsAndTypes, process);
		
		RewritingProcess result = process.newSubProcessWithContext(newMapOfContextualSymbolsAndTypes, newContextualConstraint);

		return result;
	}

	private static RewritingProcess renameExistingContextualSymbolsIfThereAreCollisions(Map<Expression, Expression> extendingcontextualSymbolsAndTypes, RewritingProcess process) {
		for (Map.Entry<Expression, Expression> extendingContextualSymbolAndType : extendingcontextualSymbolsAndTypes.entrySet()) {
			Expression extendingContextualSymbol = extendingContextualSymbolAndType.getKey();
			if (process.getContextualSymbols().contains(extendingContextualSymbol)) {
				process = shadowContextualSymbol(extendingContextualSymbol, process);
			}
		}
		return process;
	}

	/** Replaces all occurrences of contextualSymbol in process' contextual symbols and constraint. */
	private static RewritingProcess shadowContextualSymbol(Expression contextualSymbol, RewritingProcess process) {
		// determines new unique name for contextualSymbol -- important: needs to start with capital letter to keep being recognized as a variable, not a constant!
		Expression newContextualSymbol = Expressions.prefixedUntilUnique(contextualSymbol, "Shadowed ", new NotContainedBy<Expression>(process.getContextualSymbols()));
		
		// makes new contextual symbols and types map
		Map<Expression, Expression> newcontextualSymbolsAndTypes = new LinkedHashMap<Expression, Expression>(process.getContextualSymbolsAndTypes());

		// replaces occurrences of contextualSymbol in types;
		// variables do not occur in types at this point (Jan/2014) but will when system is more general
		// needs to be on syntax tree because otherwise process will be extended during Expression.replace and we may be in infinite loop.
		for (Map.Entry<Expression, Expression> someContextualSymbolAndType : process.getContextualSymbolsAndTypes().entrySet()) {
			Expression type = someContextualSymbolAndType.getValue();
			if (type != null) {
				Expression someContextualSymbol = someContextualSymbolAndType.getKey();
				Expression newType = type.replaceAllOccurrences(contextualSymbol, newContextualSymbol, process);
				if (newType != type) {
					newcontextualSymbolsAndTypes.put(someContextualSymbol, newType);
				}
			}
		}
		
		// replaces key in contextualSymbol's entry by its new symbol
		newcontextualSymbolsAndTypes.put(newContextualSymbol, newcontextualSymbolsAndTypes.get(contextualSymbol));
		newcontextualSymbolsAndTypes.remove(contextualSymbol);
		
		// replaces contextualSymbol in the constraint
		Expression newContextualConstraint = process.getContextualConstraint().replaceAllOccurrences(contextualSymbol, newContextualSymbol, process);
		
		// assembles new process
		RewritingProcess newProcess = process.newSubProcessWithContext(newcontextualSymbolsAndTypes, newContextualConstraint);
		
		return newProcess;
	}

	private static void doNotAcceptTypesContainingTypeExpression(Map<Expression, Expression> extendingcontextualSymbolsAndTypes) throws Error {
		for (Map.Entry entry : extendingcontextualSymbolsAndTypes.entrySet()) {
			if (entry.getValue() != null && ((Expression)entry.getValue()).hasFunctor("type")) {
				throw new Error("'type' occurring in types extending context: " + entry);
			}
		}
	}

	private static StackedHashMap<Expression, Expression> createNewMapOfContextualSymbolsAndTypes(Map<Expression, Expression> indexToTypeMap, RewritingProcess process) {
		StackedHashMap<Expression, Expression> newMapOfContextualSymbolsAndTypes = new StackedHashMap<Expression, Expression>(process.getContextualSymbolsAndTypes());
		if (indexToTypeMap != null) {
			Map<Expression, Expression> symbolsToTypeMap2 = getTypesOfIndicesFunctorsOrSymbols(indexToTypeMap, process);
			//System.out.println("Symbols to type: " + symbolsToTypeMap2);	
			newMapOfContextualSymbolsAndTypes.putAll(symbolsToTypeMap2);
		}
		return newMapOfContextualSymbolsAndTypes;
	}

	private static Expression checkAndAddNewConstraints(Expression additionalConstraints, StackedHashMap<Expression, Expression> newMapOfcontextualSymbolsAndTypes, RewritingProcess process) throws Error {
		Expression newContextualConstraint = process.getContextualConstraint();
		// Only extend the contextual constraint with formulas
		if (!additionalConstraints.equals(Expressions.TRUE) && FormulaUtil.isFormula(additionalConstraints, process)) {
			checkThatAllFreeSymbolsInAdditionalConstraintsAreInContext(additionalConstraints, newMapOfcontextualSymbolsAndTypes, process);
			// Construct a conjunct of contextual constraints extended by the additional context
			newContextualConstraint = CardinalityUtil.makeAnd(newContextualConstraint, additionalConstraints);
		} 
		else {
			// Note: commenting out for now due to the bloat caused in the trace output.
			// Trace.log("INFO: Not a formula to extend contextual constraint by: {}", additionalConstraints);
		}
		return newContextualConstraint;
	}

	private static void checkThatAllFreeSymbolsInAdditionalConstraintsAreInContext(Expression additionalConstraints, Map<Expression, Expression> newMapOfContextualSymbolsAndTypes, RewritingProcess process) throws Error {
		
		// For now, we are only adding the free variables (rather than free symbols as indicated by the method's name)
		// because we want to have the flexibility of manipulating constants without them having to have been added to context in advance,
		// which requires primitive constants registration, user constant registration/declaration, and either type declaration or inference,
		// all of these things being either burdensome or yet to be implemented.

		Set<Expression> freeSymbolsInAdditionalConstraints = Expressions.freeVariables(additionalConstraints, process);
		if (! newMapOfContextualSymbolsAndTypes.keySet().containsAll(freeSymbolsInAdditionalConstraints) &&
				! process.containsGlobalObjectKey(DO_NOT_REQUIRE_ADDED_CONTEXTUAL_CONSTRAINT_FREE_SYMBOLS_TO_BE_IN_CONTEXTUAL_VARIABLES)) {

			String message =
					"Extending contextual constraint with additional constraint <" + additionalConstraints +
					"> containing unknown free symbol " + Util.join(Util.subtract(freeSymbolsInAdditionalConstraints, newMapOfContextualSymbolsAndTypes.keySet())) + 
					" (current contextual symbols are {" + Util.join(newMapOfContextualSymbolsAndTypes.keySet()) + "})";
			throw new Error(message);
		}
		// The check above ensures that the additional constraints only use variables that are already known.
		// When this fails, the cause is a failure in the code somewhere to extend the process with scoping variables.
		// The easiest way to debug this is to place a breakpoint at the line above and, when it is reached, inspect the stack,
		// looking for the point in which the expression being process involves variables not in the process contextual symbols.
		// That point will be the spot where the process should have been extended.
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
			Pair<Class<?>, Rewriter>... rewritersBefore) {
		for (int i = 0; i < rewritersBefore.length; i++) {
			addRewriterBefore(rewriters, rewritersBefore[i].first, rewritersBefore[i].second);
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
	
	/**
	 * Returns a list of index expressions corresponding to the free variables in an expressions and their types per the context, if any.
	 */
	public static IndexExpressionsSet getIndexExpressionsOfFreeVariablesIn(Expression expression, RewritingProcess process) {
		Set<Expression> freeVariables = Expressions.freeVariables(expression, process);
		IndexExpressionsSet result = makeIndexExpressionsForIndicesInListAndTypesInContext(freeVariables, process);
		return result;
	}

	/**
	 * Returns a list of index expressions corresponding to the given indices and their types per the context, if any.
	 */
	public static IndexExpressionsSet makeIndexExpressionsForIndicesInListAndTypesInContext(Collection<Expression> indices, RewritingProcess process) {
		List<Expression> indexExpressions = new LinkedList<Expression>();
		for (Expression index : indices) {
			Expression type = process.getContextualSymbolType(index);
			Expression indexExpression = IndexExpressions.makeIndexExpression(index, type);
			indexExpressions.add(indexExpression);
		}
		return new ExtensionalIndexExpressionsSet(indexExpressions);
	}

	public static void doTreeUtilWaitUntilClosed() {
		if (GrinderConfiguration.isWaitUntilUIClosedEnabled()) {
			TreeUtil.waitUntilUIClosed();
		}
	}

	/**
	 * Assumes two given expressions are (possible multiple-level) injective expressions on symbols (variables or constants),
	 * or symbols themselves,
	 * and returns a pair of lists <code>(L1,L2)</code> such that <code>L1 = L2</code> must hold for the expressions to unify,
	 * and <code>null</code> if their injective expression structure is not the same.
	 */
	public static Pair<List<Expression>, List<Expression>> getListsOfElementsToBeUnifiedInInjectiveExpressions(Expression expression1, Expression expression2, RewritingProcess process) {
		InjectiveModule module = (InjectiveModule) process.findModule(InjectiveModule.class);
		Pair<List<Expression>, List<Expression>> result = getListsOfElementsToBeUnifiedInInjectiveExpressionsWithModule(expression1, expression2, module, process);
		return result;
	}

	/**
	 * Version of {@link #getListsOfElementsToBeUnifiedInInjectiveExpressions(Expression, Expression, RewritingProcess)}
	 * that receives an injective module rather than finds it in a rewriting process.
	 * This is useful because the method is recursive and we do not want to look up the module at every recursive call.
	 */
	private static Pair<List<Expression>, List<Expression>> getListsOfElementsToBeUnifiedInInjectiveExpressionsWithModule(Expression expression1, Expression expression2, InjectiveModule module, RewritingProcess process) {
		
		Pair<List<Expression>, List<Expression>> result;
	
		if (expression1.getSyntacticFormType().equals("Symbol") && expression2.getSyntacticFormType().equals("Symbol")) {
			result = Pair.make(Util.list(expression1), Util.list(expression2));
		}
		else {
	
			if ( ! module.injectiveFunctionTokensAreEqual(expression1, expression2, process)) {
				return null;
			}
			
			Iterator<Expression> subExpression1Iterator = expression1.getSubExpressions().iterator();
			Iterator<Expression> subExpression2Iterator = expression2.getSubExpressions().iterator();
			
			List<Expression> list1 = new LinkedList<Expression>();
			List<Expression> list2 = new LinkedList<Expression>();
			
			while (subExpression1Iterator.hasNext()) {
				Expression subExpression1 = subExpression1Iterator.next();
				Expression subExpression2 = subExpression2Iterator.next();
				
				Pair<List<Expression>, List<Expression>> subLists = getListsOfElementsToBeUnifiedInInjectiveExpressionsWithModule(subExpression1, subExpression2, module, process);
				if (subLists == null) {
					return null;
				}
				
				list1.addAll(subLists.first);
				list2.addAll(subLists.second);
			}
	
			result = Pair.make(list1, list2);
		}
		return result;
	}

	/**
	 * Disables tracing and justification output, and turned concurrency off.
	 * This is useful for running tests.
	 */
	public static void setTraceAndJustificationOffAndTurnOffConcurrency() {
		// This is equivalent to using command line options:
		// -Dgrinder.display.tree.util.ui=false
		// -Dtrace.level=off
		// -Djustification.level=off
		// -Dsriutil.branch.and.merge.threading.enabled=false
		// Setting here explicitly so its not forgotten.
		Configuration.setProperty(GrinderConfiguration.KEY_DISPLAY_TREE_UTIL_UI, "false");
		GrinderConfiguration.disableTrace();
		GrinderConfiguration.disableJustification();
		Configuration.setProperty(AICUtilConfiguration.KEY_BRANCH_AND_MERGE_THREADING_ENABLED, "false");
		BranchAndMerge.reset();
	}

	/**
	 * Returns all sub-expressions that are a logical variable.
	 */
	public static Collection<Expression> getAllVariables(Expression expression, DefaultRewritingProcess process) {
		Collection<Expression> result = new LinkedHashSet<Expression>();
		Util.collect(new SubExpressionsDepthFirstIterator(expression), result, new IsVariable(process));
		return result;
	}

	/**
	 * Indicates whether a expression is <i>known</i> (from a syntactic point of view)
	 * to be independent from all indices of a scoping expression around it.
	 * Therefore it can detect that <code>j</code> is independent of <code>i</code> in <code>{ (on i) j }</code>,
	 * for example, and that <code>g(X)</code> is independent of <code>f(X)</code> in <code>{ (on f(X)) g(X) }</code>,
	 * but will not detect that
	 * <code>f(a)</code> is independent of <code>f(X)</code> in <code>{ (on f(X)) f(a) | X != a }</code>.
	 * <p>
	 * Note that <b>not known</b> to be independent is not the same as dependent!
	 * Therefore, a return value of <code>false</code> does not imply dependence.
	 */
	public static boolean isKnownToBeIndependentOfScopeIn(Expression expression, Expression scopingExpression, RewritingProcess process) {
		List<Expression> indices = scopingExpression.getScopedExpressions(process);
		boolean result = isKnownToBeIndependentOfIndices(expression, indices, process);
		return result;
	}

	public static boolean isKnownToBeIndependentOfIndices(Expression expression, Collection<Expression> indices, RewritingProcess process) {
		for (Expression index : indices) {
			boolean isKnownToBeIndependentOfIndex = isKnownToBeIndependentOfIndex(expression, index, process);
			if ( ! isKnownToBeIndependentOfIndex) {
				return false;
			}
		}
		return true;
	}

	public static boolean isKnownToBeIndependentOfIndex(Expression expression, Expression index, RewritingProcess process) {
		Iterator<Expression> subExpressionsIterator = new SubExpressionsDepthFirstIterator(expression);
		while (subExpressionsIterator.hasNext()) {
			Expression subExpression = subExpressionsIterator.next();
			boolean subExpressionIsKnownToBeIndependentOfIndex =
				topExpressionIsKnownToBeIndependentOfIndex(subExpression, index, process);
			if ( ! subExpressionIsKnownToBeIndependentOfIndex) {
				return false;
			}
		}
		return true;
	}

	private static boolean topExpressionIsKnownToBeIndependentOfIndex(Expression expression, Expression index, RewritingProcess process) {
		// As the index changes its value, the only way an expression can always coincide with it
		// is by being the index itself; all others will remain with the same value as the index changes,
		// and will therefore be independent from that change.
		Expression expressionFunctorOrSymbol = expression.getFunctorOrSymbol();
		Expression indexFunctorOrSymbol = index.getFunctorOrSymbol();
		if ( ! expressionFunctorOrSymbol.equals(indexFunctorOrSymbol)) {
			return true; 
		}
		
		if (expression.numberOfArguments() != index.numberOfArguments()) {
			return false; // this case actually means that they are dependent for sure, but the method's interface does not allow us to state this, only to state that they are not known to be independent.
		}
		
		// At this point we know the roots are the same and will unify,
		// so this is really about unifying the arguments.
		// Two function applications with the same functors will be independent if it is impossible for them to unify.
		// If it is possible for them to unify (with condition below being TRUE or something not false, and thus possibly satisfiable)
		// then it is possible that they are dependent.
		Expression condition = Unification.unificationCondition(expression, index, process);
		boolean result = condition.equals(Expressions.FALSE);
		return result;
	}

	public static List<Expression> getParameters(QuantifiedExpression quantifiedExpression) {
		IndexExpressionsSet indexExpressions = quantifiedExpression.getIndexExpressions();
		List<Expression> result = new LinkedList<Expression>(IndexExpressions.getIndexToTypeMapWithDefaultTypeOfIndex(indexExpressions).keySet());
		return result;
	}

	/**
	 * Checks for equality of two lambda expressions up to renaming of
	 * single-variable parameters.
	 */
	public static boolean lambdasAreKnownToBeEqualUpToParameterRenaming(Expression lambda1, Expression lambda2, RewritingProcess process) {
		List<Expression> parameters1 = getParameters((QuantifiedExpression) lambda1);
		List<Expression> parameters2 = getParameters((QuantifiedExpression) lambda2);
		if (parameters1.size() != parameters2.size()) {
			return false;
		}
		Expression body1 = ((LambdaExpression) lambda1).getBody();
		Expression body2 = ((LambdaExpression) lambda2).getBody();
		if ( ! parameters1.equals(parameters2)) {
			Iterator<Expression> parameter1Iterator = parameters1.iterator();
			Iterator<Expression> parameter2Iterator = parameters2.iterator();
			while (parameter1Iterator.hasNext()) {
				Expression parameter1 = parameter1Iterator.next();
				Expression parameter2 = parameter2Iterator.next();
				if ( ! parameter1.equals(parameter2)) {
					if (parameter1.getSyntacticFormType().equals("Symbol") && parameter2.getSyntacticFormType().equals("Symbol")) {
						body2 = SemanticSubstitute.replace(body2, parameter2, parameter1, process);
					}
					else {
						return false; // not renaming function applications, so just give up.
					}
				}
			}
		}
		boolean result = body1.equals(body2);
		return result;
		// We still need to take into account the situation in which a variable appears in different positions,
		// such as lambda X,Y ... and lambda Y,Z ...
		// This requires standardizing apart.
	}

	/**
	 * Returns the cardinality of the type of a given variable in the given process,
	 * trying to find {@link TypeSizeOfSymbolOrType} object in the process global objects
	 * under key {@link CardinalityOfType#PROCESS_GLOBAL_OBJECT_KEY_DOMAIN_SIZE_OF_SYMBOL_OR_TYPE}
	 * or, failing that, to look for <code>| Type |</code>, for <code>Type</code> the type of the variable,
	 * in the process global objects.
	 * If the size cannot be determined, returns -1.
	 * @param symbol a variable
	 * @param process the rewriting process
	 * @return the cardinality of the type of the variable according to the process or -1 if it cannot be determined.
	 */
	public static long getTypeCardinality(Expression symbol, RewritingProcess process) {
		long result = -1;
	
		// first, we try to determine the cardinality of variable's type from {@link TypeSizeOfLogicalVariable) information.
		TypeSizeOfSymbolOrType typeSizeOfSymbolOrType =
				(TypeSizeOfSymbolOrType) process
				.getGlobalObject(CardinalityOfType.PROCESS_GLOBAL_OBJECT_KEY_DOMAIN_SIZE_OF_SYMBOL_OR_TYPE);
		
		if (typeSizeOfSymbolOrType != null) {
			Integer size = typeSizeOfSymbolOrType.getSize(symbol, process);
			if (size != null) {
				result = size;
			}
		}
	
		// If that didn't work, we try to find a value for | Type | in the process's global objects.
		if (result == -1) {
			Expression variableType = process.getContextualSymbolType(symbol);
			if (variableType != null) {
				Expression typeCardinality = Expressions.apply(FunctorConstants.CARDINALITY, variableType);
				Expression typeCardinalityValue = (Expression) process.getGlobalObject(typeCardinality);
				if (typeCardinalityValue != null) {
					result = typeCardinalityValue.intValueExact();
				}
			}
		}
		
		return result;
	}

	/**
	 * Indicates whether an expression is boolean-typed by having its {@link getType}
	 * type be "Boolean", "'->'(Boolean)", or "bool", or "boolean".
	 * @param expression
	 * @param process
	 * @return
	 */
	public static boolean isBooleanTyped(Expression expression, RewritingProcess process) {
		Expression type = getType(expression, process);
		boolean result =
				type != null &&
				(
				type.equals(parse("Boolean")) ||
				type.equals(parse("'->'(Boolean)")) ||
				type.equals(parse("bool")) ||
				type.equals(parse("boolean")));
		return result;
	}
}
