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

/**
 * A Decision Tree that determines the best best way to call Rewriters based on
 * their reified tests.
 * 
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
	
	public Expression rewrite(Expression expression, RewritingProcess process) {
		Expression result = rootNode.rewrite(expression, process);
		return result;
	}
	
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
				
				// Handle the otherwise case
				if (!rewriterHasAttribute) {
					List<RewriterWithReifiedTests> rewriterListA_O = A_O.get(a_v.getAttribute());					
					if (rewriterListA_O == null) {
						rewriterListA_O = new ArrayList<RewriterWithReifiedTests>();
						A_O.put(a_v.getAttribute(), rewriterListA_O);
					}
					
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
	}
	
	private abstract class Node {
		public abstract Expression rewrite(Expression expression, RewritingProcess process);
		public abstract void toString(StringBuilder sb, String indent);
	}
	
	private class BranchNode extends Node {
		private RewriterTestAttribute a            = null;
		private Map<Object, Node>     valueToNode  = new LinkedHashMap<Object, Node>();
		private Node                  otherwise    = null;
	
		public BranchNode(RewriterTestAttribute a, 
				          Map<Object, List<RewriterWithReifiedTests>> valueToRewritersWithReifiedTests,
				          List<RewriterWithReifiedTests> noValueRewritersWithReifiedTests) {
			this.a = a;
			for (Object value : valueToRewritersWithReifiedTests.keySet()) {
				Node childNode = makeDecisionTree(valueToRewritersWithReifiedTests.get(value));
				this.valueToNode.put(value, childNode);
			}
			otherwise = makeDecisionTree(noValueRewritersWithReifiedTests);
		}
		
		@Override
		public Expression rewrite(Expression expression, RewritingProcess process) {
			Expression result = expression;
			Node nodeToRewrite = valueToNode.get(a.getValue(expression, process));
			if (nodeToRewrite == null) {
				// i.e. none of the values on the expression match this attribute
				nodeToRewrite = otherwise;
			}
			result = nodeToRewrite.rewrite(expression, process);
			
			return result;
		}		
		
		@Override
		public void toString(StringBuilder sb, String indent) {
			sb.append(indent);
			sb.append("+Branch: attribute:");
			sb.append(a);
			sb.append(", #children=");
			sb.append(valueToNode.size()+1); // i.e. include otherwise
			sb.append("\n");
			for (Object v : valueToNode.keySet()) {
				Node childNode = valueToNode.get(v);
				sb.append(indent);
				sb.append("    ");
				sb.append("-value:"+v);
				sb.append("\n");
				childNode.toString(sb, indent+"        ");				
			}
			sb.append(indent);
			sb.append("    ");
			sb.append("-otherwise:");
			sb.append("\n");
			otherwise.toString(sb, indent+"        ");
			sb.append("\n");
		}
	}
	
	private class LeafNode extends Node {
		private List<RewriterWithReifiedTests> rewritersWithReifiedTests = null;
		
		public LeafNode(List<RewriterWithReifiedTests> rewritersWithReifiedTests) {
			this.rewritersWithReifiedTests = rewritersWithReifiedTests;
		}
		
		@Override
		public Expression rewrite(Expression expression, RewritingProcess process) {
			Expression result = expression;
			
			for (RewriterWithReifiedTests rwrts : rewritersWithReifiedTests) {
				// Ensure the rewriter is applicable
				boolean applicable = true;
				for (RewriterTest test : rwrts.tests) {
					if (!test.apply(expression, process)) {
						applicable = false;
						break;
					}
				}
				if (applicable) {
					// Call rewriter, indicating it should bypass its reified tests.
					result = rwrts.rewriter.rewrite(expression, process, true);
					if (result != expression) {
						// We have rewritten
						break;
					}
				}
			}

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