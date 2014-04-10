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
import java.util.Collection;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import com.google.common.annotations.Beta;
import com.google.common.base.Predicate;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.api.Rewriter;
import com.sri.ai.grinder.api.RewriterTest;
import com.sri.ai.grinder.api.RewriterTestAttribute;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.util.Util;
import com.sri.ai.util.base.Pair;

/**
 * A Decision Tree that determines the best best way to call Rewriters based on
 * their reified tests. Outline of algorithm is as follows:<br>
 * <p>
 * Assume we have an ordered list of rewriters R1,...,Rn. Rewriter Ri provides
 * the reified tests (A_i, 1, V_i,1), ..., (A_i, m_i, V_i,m_i), where a test
 * A_i,j, V_i,j means that the test is satisfied if A_i,j(E) = V_i,j, that is,
 * whether the given attribute A_i,j has value V_i,j for the expression E. Given
 * an expression E, we can see the rewriting of this expression by this list of
 * rewriters as equivalent to.
 * </p>
 * <p>
 * if A1,1(E)=V_1,1 and ... and A1,m_1(E)=V_1,m_1 and C_1(E) then R1(E) else if
 * A2,1(E)=V2,1 and ... and A_2,m_2(E)=V_2,m_2 and C_2(E) then R2(E) else ...
 * (let's call this expression (I) -- note this is not an "Expresso expression",
 * just a math concept for our explanation).
 * </p>
 * <p>
 * where C_i(E) represents the internal, non-reified conditions of rewriter R_i.
 * We need this because it may be that R_i returns E itself, meaning that it
 * does not apply to it, in which case we must keep going down the list. If we
 * did not have C_i, then the above expression would result in the value R_i(E)
 * for the first R_i for whose reified conditions are satisfied, even if it
 * returned E itself. Note that neither C_i nor expression (I) is actually
 * represented as a Java object, they are simply conceptual aids.
 * </p>
 * <p>
 * Once we are given the A_i,j, V_i,j, C_i and R_i, we need to create a decision
 * tree. This means picking up an attribute to be the root test of the decision
 * tree.
 * </p>
 * <p>
 * Let us consider an arbitrary attribute A and an incoming expression E. We
 * want to know if A is a good root attribute. This will depend on how well it
 * splits the list of rewriters. So let us see first how an attribute does this
 * splitting. Let's consider a value for A, say V. For each test A_i,j=V_i,j
 * such that A_i,j is equal to A, A=V implies A_i,j=V_i,j to be either true or
 * false, depending on whether V_i,j is equal or different from V, respectively.
 * So if we assume A=V, we can rewrite expression (I) by replacing tests
 * A_i,j=V_i,j such that A_i,j = A by either true or false. The ones made true
 * can be simply eliminated from the conjunction they are in; the ones made
 * false make the entire conjunction false and their respective rewriter can be
 * removed from (I).
 * </p>
 * <p>
 * Now, remember that (I) is not actually represented as an expression; it is
 * simple a way to think about the meaning of a list of rewriters with their
 * respective reified tests. So really we get a list of rewriters and respective
 * reified tests, and, given a certain attribute A and value V, create a new,
 * smaller list of rewriters and reified tests that can safely replace the
 * original one when we know that A=V.
 * </p>
 * <p>
 * However, we need to consider all possible values of A. Let us name them V_1,
 * ..., V_k (the values that occur in the original reified tests). For each V_j,
 * we generate the new list of rewriters and reified tests under A=V_j. The
 * *cost* of A is the average length of the lists under A=V_j, over all j. We
 * pick the root attribute to be the A_i,j with the smaller cost. Note that
 * multiple A_i,j are actually the same attribute, and the cost and splitting of
 * each of them should be computed only once.
 * </p>
 * <p>
 * Once we decide on the root attribute, we have under each possible value a
 * different list of rewriters with their reified tests. So now we run the
 * algorithm recursively for each of those lists. The base case occurs when
 * there are no more reified tests to use or, if there are, that no attribute
 * actually does any splitting.
 * </p>
 * Now let's express all these ideas in pseudo-code.<br>
 * We represent a list of rewriters with their reified tests as ( ( (A_i,j,
 * V_i,j) )_j, R_i)_i <br>
 * The notation (alpha_i)_i means the list of alpha_i for all i. <br>
 * The notation { alpha_i }_i means the intensional set of alpha_i for all i. <br>
 * A decision tree is either a list of rewriters with their reified tests (a
 * leaf), or an object of a class "decision tree" storing an attribute A and a
 * map from each value V of A to a decision tree (the children).<br>
 * 
 * <pre>
 * make_decision_tree( ( ( (A_i,j, V_i,j) )_j, R_i)_i )
 * 
 *     for each (A,V) in {A_i,j}_i,j x {V_i,j}_i,j // cartesian product of attributes and tested values.
 *         rewriterList(A,V)  =   ( ( (A_i,j, V_i,j) )_j, R_i)_i,
 *                                removing (A_i,j, V_i,j) from ( (A_i,j, V_i,j) )_j if A_i,j is A and V_i,j is V, 
 *                                and removing ( ( (A_i,j, V_i,j) )_j, R_i) from ( ( (A_i,j, V_i,j) )_j, R_i)_i if A_i,j *is* A and V_i,j is *not* V
 *         sum(A) += length of ( ( (A_i,j, V_i,j) )_j, R_i)_i
 *         count(A)++
 * 
 *     if count(A) = 0 for all a
 *         return ( (A_i,j, V_i,j) )_j, R_i)_i // no splitting
 * 
 *     for each A, cost(A) = sum(A)/count(A)
 * 
 *     A = argmin_A cost(A)
 *     cost = min_A cost(A)
 * 
 *     if cost >= length of ( (A_i,j, V_i,j) )_j, R_i)_i
 *         return ( (A_i,j, V_i,j) )_j, R_i)_i    // no splitting
 * 
 *     return decision_tree with root A with map from each V to make_decision_tree(rewriterList(A,V))
 * 
 * rewrite(decision_tree, E)
 * 
 *     if decision_tree is of the form ( ( (A_i,j, V_i,j) )_j, R_i)_i
 *         rewrite E using the first applicable R_i // as usual
 * 
 *     if decision_tree if of the form decision_tree with root A with map from each V to make_decision_tree(rewriterList(A,V))
 *         return rewrite(map(value of A in E), E)
 * </pre>
 * 
 * @author braz
 * @author oreilly
 * 
 */
@Beta
public class CallRewriterDecisionTree {
	private Node rootNode = null;
	/**
	 * Constructor.
	 * 
	 * @param rewriters
	 *            a list of rewriters for which a decision tree is to be
	 *            constructed.
	 */
	public CallRewriterDecisionTree(List<Rewriter> rewriters) {
		// Construct the initial data structure from which to construct
		// a decision tree.
		List<RewriterWithReifiedTests> rewritersWithReifiedTests = new ArrayList<RewriterWithReifiedTests>();
		for (Rewriter r : rewriters) {
			rewritersWithReifiedTests.add(new RewriterWithReifiedTests(r, r.getReifiedTests()));
		}
		
		rootNode = makeDecisionTree(rewritersWithReifiedTests);
	}
	
	/**
	 * Rewrite the given expression within the context of the provided process.
	 * 
	 * @param expression
	 *            the expression to be rewritten.
	 * @param process
	 *            the process under which the rewriting is to occur.
	 * @return a (rewriter, expression) pair. If no rewriter rewrote the given
	 *         expression then the rewriter argument will be null, otherwise it
	 *         will be the first rewriter that rewrote the given expression. The
	 *         returned expression will be the first rewritten expression be a
	 *         rewriter or the input expression if no rewriter rewrote it.
	 */
	public Pair<Rewriter, Expression> rewrite(Expression expression, RewritingProcess process) {
		Pair<Rewriter, Expression> result = rootNode.rewrite(expression, process);
		return result;
	}
	
	/**
	 * @return a String representation of the decision tree.
	 */
	@Override
	public String toString() {
		StringBuilder sb = new StringBuilder();
		rootNode.toString(sb, "");
		return sb.toString();
	}
	
	//
	// PRIVATE
	//
	private Node makeDecisionTree(List<RewriterWithReifiedTests> rewritersWithReifiedTests) {
		Node result = null;
		
		Map<RewriterTest, List<RewriterWithReifiedTests>>          A_V = new LinkedHashMap<RewriterTest, List<RewriterWithReifiedTests>>();
		Map<RewriterTestAttribute, List<RewriterWithReifiedTests>> A_O = new LinkedHashMap<RewriterTestAttribute, List<RewriterWithReifiedTests>>();
		Map<RewriterTestAttribute, Double> countA = new LinkedHashMap<RewriterTestAttribute, Double>();
		Map<RewriterTestAttribute, Double> sumA   = new LinkedHashMap<RewriterTestAttribute, Double>();
		// for each (A,V) in {A_i,j}_i,j x {V_i,j}_i,j // cartesian product of attributes and tested values
		//     rewriterList(A,V)  =   ( ( (A_i,j, V_i,j) )_j, R_i)_i,
		// First: get all the (A,V) pairs.
		for (RewriterWithReifiedTests rwrts : rewritersWithReifiedTests) {
			for (RewriterTest a_v : rwrts.tests) {			
				List<RewriterWithReifiedTests> rewriterListA_V = A_V.get(a_v);
				if (rewriterListA_V == null) {
					rewriterListA_V = new ArrayList<RewriterWithReifiedTests>();
					A_V.put(a_v, rewriterListA_V);
				}
			}
		}
		// Second: Walk thru all A_V pairs adding relevant and writers to each pair.
		for (RewriterTest a_v : A_V.keySet()) {
			List<RewriterWithReifiedTests> rewriterListA_O = A_O.get(a_v.getAttribute());					
			if (rewriterListA_O == null) {
				rewriterListA_O = new ArrayList<RewriterWithReifiedTests>();
				A_O.put(a_v.getAttribute(), rewriterListA_O);
			}
			
			List<RewriterWithReifiedTests> rewriterListA_V = A_V.get(a_v);
			for (RewriterWithReifiedTests rwrts : rewritersWithReifiedTests) {
				// removing ( ( (A_i,j, V_i,j) )_j, R_i) from ( ( (A_i,j, V_i,j) )_j, R_i)_i 
				// if A_i,j *is* A and V_i,j is *not* V
				boolean dropRewriterWithDifferentValuedAttribute = false;
				boolean rewriterHasAttribute                     = false;
				for (RewriterTest possibleConflictingTest : rwrts.tests) {
					if (a_v.getAttribute() == possibleConflictingTest.getAttribute()) {
						rewriterHasAttribute = true;
						if (!a_v.getValue().equals(possibleConflictingTest.getValue())) {
							dropRewriterWithDifferentValuedAttribute = true;
							break;
						}
					}
				}
				
				if (!dropRewriterWithDifferentValuedAttribute) {
					// and
					// removing (A_i,j, V_i,j) from ( (A_i,j, V_i,j) )_j if A_i,j is A and V_i,j is V
					Set<RewriterTest> remainingTests = new LinkedHashSet<RewriterTest>(rwrts.tests);
					remainingTests.remove(a_v);
					
					RewriterWithReifiedTests crwrts = new RewriterWithReifiedTests(rwrts.rewriter, remainingTests);
					rewriterListA_V.add(crwrts);
					updateMetrics(a_v.getAttribute(), countA, sumA, crwrts);
				}
				
				// Handle the otherwise case (i.e. for rewriters that do not have the attribute
				// in their list of reified tests).
				if (!rewriterHasAttribute) {
					// Only have to add the first time we see the rewriter for this attribute
					boolean alreadyHasRewriter = false;
					for (RewriterWithReifiedTests orwrts : rewriterListA_O) {
						if (orwrts.rewriter == rwrts.rewriter) {
							alreadyHasRewriter = true;
							break;
						}
					}
					if (!alreadyHasRewriter) {
						RewriterWithReifiedTests orwrts = new RewriterWithReifiedTests(rwrts.rewriter, rwrts.tests);
						rewriterListA_O.add(orwrts);
						updateMetrics(a_v.getAttribute(), countA, sumA, orwrts);
					}
				}
			}
		}
		
		// if count(A) = 0 for all a
		boolean countA_allZeros = countA.size() == 0 || Util.forAll(countA.values(), new Predicate<Double>() {
			public boolean apply(Double d) {
				return d.intValue() == 0;
			}
		});
		if (countA_allZeros) {
			//  return ( (A_i,j, V_i,j) )_j, R_i)_i // i.e. no splitting
			result = new LeafNode(rewritersWithReifiedTests);
		}
		else {
			// for each A, cost(A) = sum(A)/count(A)
			// A = argmin_A cost(A)
			RewriterTestAttribute argminA = null;
		    // cost = min_A cost(A)
			double cost = Double.MAX_VALUE;
			for (RewriterTestAttribute a : countA.keySet()) {
				double costA = sumA.get(a) / countA.get(a);
				if (costA < cost) {
					argminA = a;
					cost    = costA;
				}
			}
			
			Map<Object, List<RewriterWithReifiedTests>> valueToRewritersWithReifiedTests = new LinkedHashMap<Object, List<RewriterWithReifiedTests>>();
			// if cost >= length of ( (A_i,j, V_i,j) )_j, R_i)_i
			double length = 0.0;
			for (RewriterTest a_v : A_V.keySet()) {
				if (argminA == a_v.getAttribute()) {
					length += A_V.get(a_v).size();
					valueToRewritersWithReifiedTests.put(a_v.getValue(), A_V.get(a_v));
				}
			}

			if (cost >= length) {				
				// return ( (A_i,j, V_i,j) )_j, R_i)_i // i.e. no splitting 
				result = new LeafNode(rewritersWithReifiedTests);
			}
			else {
				// return decision_tree with root A with map from each V to make_decision_tree(rewriterList(A,V)) 
				result = new BranchNode(argminA, valueToRewritersWithReifiedTests, A_O.get(argminA));
			}
		}
				
		return result;
	}
	
	private void updateMetrics(RewriterTestAttribute a, Map<RewriterTestAttribute, Double> countA, 
			Map<RewriterTestAttribute, Double> sumA, 
			RewriterWithReifiedTests rewriterWithReifiedTests) {
	
		Double count = countA.get(a);
		if (count == null) {
			count = 0.0;
		}
		count = count+1;
		countA.put(a, count);
		
		Double sum = sumA.get(a);
		if (sum == null) {
			sum = 0.0;
		}
		
		sum = sum + rewriterWithReifiedTests.tests.size(); 					
		sumA.put(a, sum);
	}
		
	private class RewriterWithReifiedTests {
		public Rewriter          rewriter = null;
		public Set<RewriterTest> tests    = new LinkedHashSet<RewriterTest>();
		
		public RewriterWithReifiedTests(Rewriter rewriter, Collection<RewriterTest> tests) {
			this.rewriter = rewriter;
			this.tests.addAll(tests);
		}
		
		public String toString() {
			String result = "RewriterWithReifiedTests on " + rewriter;
			return result;
		}
	}
	
	private abstract class Node {
		public abstract Pair<Rewriter, Expression> rewrite(Expression expression, RewritingProcess process);
		public abstract void toString(StringBuilder sb, String indent);
	}
	
	private class BranchNode extends Node {
		private RewriterTestAttribute attribute    = null;
		private Map<Object, Node>     valueToNode  = new LinkedHashMap<Object, Node>();
		private Node                  otherwise    = null;
	
		public BranchNode(RewriterTestAttribute a, 
				          Map<Object, List<RewriterWithReifiedTests>> valueToRewritersWithReifiedTests,
				          List<RewriterWithReifiedTests> noValueRewritersWithReifiedTests) {
			this.attribute = a;
			for (Object value : valueToRewritersWithReifiedTests.keySet()) {
				Node childNode = makeDecisionTree(valueToRewritersWithReifiedTests.get(value));
				this.valueToNode.put(value, childNode);
			}
			otherwise = makeDecisionTree(noValueRewritersWithReifiedTests);
		}
		
		@Override
		public Pair<Rewriter, Expression> rewrite(Expression expression, RewritingProcess process) {
			Object attributeValueForExpression = attribute.getValue(expression, process);
			Node nodeToRewrite = valueToNode.get(attributeValueForExpression);
			if (nodeToRewrite == null) {
				// i.e. none of the values on the expression match this attribute
				nodeToRewrite = otherwise;
			}
			
			Pair<Rewriter, Expression> result = nodeToRewrite.rewrite(expression, process);
			
			return result;
		}		
		
		@Override
		public void toString(StringBuilder stringBuilder, String indent) {
			stringBuilder.append(indent);
			stringBuilder.append("+Branch: attribute:");
			stringBuilder.append(attribute);
			stringBuilder.append(", #children=");
			stringBuilder.append(valueToNode.size()+1); // i.e. include otherwise
			stringBuilder.append("\n");
			for (Object v : valueToNode.keySet()) {
				Node childNode = valueToNode.get(v);
				stringBuilder.append(indent);
				stringBuilder.append("    ");
				stringBuilder.append("-value:"+v);
				stringBuilder.append("\n");
				childNode.toString(stringBuilder, indent+"        ");				
			}
			stringBuilder.append(indent);
			stringBuilder.append("    ");
			stringBuilder.append("-otherwise:");
			stringBuilder.append("\n");
			otherwise.toString(stringBuilder, indent+"        ");
			stringBuilder.append("\n");
		}
	}
	
	private class LeafNode extends Node {
		private List<RewriterWithReifiedTests> rewritersWithReifiedTests = null;
		
		public LeafNode(List<RewriterWithReifiedTests> rewritersWithReifiedTests) {
			this.rewritersWithReifiedTests = rewritersWithReifiedTests;
		}
		
		@Override
		public Pair<Rewriter, Expression> rewrite(Expression expression, RewritingProcess process) {
			Rewriter   rewriterProducingTheResult   = null;
			Expression resultExpression = expression;
			
			for (RewriterWithReifiedTests rewriterWithReifiedTests : rewritersWithReifiedTests) {
				// Ensure the rewriter is applicable
				boolean applicable = true;
				for (RewriterTest test : rewriterWithReifiedTests.tests) {
					if (!test.apply(expression, process)) {
						applicable = false;
						break;
					}
				}
				if (applicable) {
					// Call rewriter, indicating it should bypass its reified tests.
					resultExpression = rewriterWithReifiedTests.rewriter.rewrite(expression, process, true);
					if (resultExpression != expression) {
						rewriterProducingTheResult = rewriterWithReifiedTests.rewriter;
						break;
					}
				}
			}
			
			Pair<Rewriter, Expression> result = new Pair<Rewriter, Expression>(rewriterProducingTheResult, resultExpression);

			return result;
		}
		
		@Override
		public void toString(StringBuilder sb, String indent) {
			sb.append(indent);
			sb.append("+Leaf: #rewriters=");
			sb.append(rewritersWithReifiedTests.size());
			sb.append("\n");
			for (RewriterWithReifiedTests rwrts : rewritersWithReifiedTests) {
				sb.append(indent);
				sb.append("rewriter=");
				sb.append(rwrts.rewriter.getName());
				sb.append(", #tests=");
				sb.append(rwrts.tests.size());
				sb.append("\n");
				for (RewriterTest test : rwrts.tests) {
					sb.append(indent);
					sb.append("    ");
					sb.append(test.toString());
					sb.append("\n");
				}
			}
		}
	}
}