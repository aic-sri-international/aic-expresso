/*
 * Copyright (c) 2017, SRI International
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
package com.sri.ai.grinder.sgdpllt.library.set.invsupport;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

import com.google.common.base.Predicate;
import com.sri.ai.expresso.api.CountingFormula;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.IndexExpressionsSet;
import com.sri.ai.expresso.api.IntensionalSet;
import com.sri.ai.expresso.api.LambdaExpression;
import com.sri.ai.expresso.api.QuantifiedExpression;
import com.sri.ai.expresso.api.QuantifiedExpressionWithABody;
import com.sri.ai.expresso.api.Type;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.expresso.helper.SubExpressionsDepthFirstIterator;
import com.sri.ai.expresso.type.FunctionType;
import com.sri.ai.grinder.sgdpllt.library.FunctorConstants;
import com.sri.ai.grinder.sgdpllt.library.boole.ForAll;
import com.sri.ai.grinder.sgdpllt.library.boole.ThereExists;
import com.sri.ai.grinder.sgdpllt.library.controlflow.IfThenElse;
import com.sri.ai.grinder.sgdpllt.library.set.Sets;
import com.sri.ai.grinder.sgdpllt.library.set.extensional.ExtensionalSet;
import com.sri.ai.util.Util;

/**
 * The <b>set of argument tuples for</b> &fnof; : &Alpha; &rarr; &Beta; <b>occurring in an 
 * expression</b> E is denoted oc<sub>&fnof;</sub>[E] and inductively defined as follows:<br>
 * <ul>
 * <li>
 * if E does not contain &fnof;, oc<sub>&fnof;</sub>[E] is &empty;
 * </li>
 * <li>
 * if E is &fnof;(t) for t a tuple, oc<sub>&fnof;</sub>[E] is {t}
 * </li>
 * <li>
 * if E is &fnof;, oc<sub>&fnof;</sub>[E] is &Alpha;
 * </li>
 * <li>
 * if E is g(E&prime;) for g a function symbol distinct from &fnof; and t a k-tuple of expressions,<br>
 *     <ul>
 *     <li>
 *     if g(E&prime;) is if C the E<sub>1</sub> else E<sub>2</sub> and C does not contain &fnof;, 
 *     then oc<sub>&fnof;</sub>[E] is<br>
 *     if C then oc<sub>&fnof;</sub>[E<sub>1</sub>] else oc<sub>&fnof;</sub>[E<sub>2</sub>]
 *     </li>
 *     <li>
 *     otherwise, oc<sub>&fnof;</sub>[E] is oc<sub>&fnof;</sub>[t<sub>1</sub>] &cup; &hellip;	&cup; oc<sub>&fnof;</sub>[t<sub>k</sub>]
 *     </li>
 *     </ul>
 * </li>
 * <li>
 * if E is Q<sub>x:C</sub>E&prime; for Q an arbitrary quantifier, x a variable, 
 * C a boolean formula not containing &fnof;, and E&prime; the
 * quantified expression, then oc<sub>&fnof;</sub>[E] is<br> 
 * &bigcup;<sub>x:C</sub>oc<sub>&fnof;</sub>[E&prime;]
 * </li>
 * <li>
 * if E is Q<sub>x:C</sub>E&prime; for Q an arbitrary quantifier, x a variable, 
 * C a boolean formula containing &fnof;, and E&prime; the
 * quantified expression, then oc<sub>&fnof;</sub>[E] is<br> 
 * &bigcup;<sub>x</sub>(oc<sub>&fnof;</sub>[C] &cup; oc<sub>&fnof;</sub>[E&prime;])
 * </li> 
 * <ul> 
 * 
 * @author oreilly
 *
 */
public class SetOfArgumentTuplesForFunctionOccurringInExpression {

	public static Expression compute(Expression fName, FunctionType fType, Expression e) {
		
		List<Expression> unionArgs = new ArrayList<>();
		
		Predicate<Expression> isF =
				expr -> expr.equals(fName);
				
		Predicate<Expression> isFApplication = 
				expr -> expr.hasFunctor(fName) && expr.numberOfArguments() == fType.getArity();
				
		computeUnionArgs(unionArgs, isF, fType, isFApplication, e);
		
		Expression result = makeUnion(unionArgs);
				
		return result;
	}
	
	private static void computeUnionArgs(List<Expression> unionArgs, Predicate<Expression> isF, FunctionType fType, Predicate<Expression> isFApplication, Expression e) {
		// if E does not contain &fnof;, oc<sub>&fnof;</sub>[E] is &empty;
		if (!containsF(e, isF)) {
			unionArgs.add(Sets.EMPTY_SET);
		}
		// if E is &fnof;(t) for t a tuple, oc<sub>&fnof;</sub>[E] is {t}
		else if (isFApplication.apply(e)) {
			Expression tupleT = Expressions.makeTuple(e.getArguments());
			Expression setT   = ExtensionalSet.makeUniSet(tupleT);
			unionArgs.add(setT);
		}
		// if E is &fnof;, oc<sub>&fnof;</sub>[E] is &Alpha;
		else if (isF.apply(e)) {
			List<Expression> domainTypes = new ArrayList<>();
			for (Type domainType : fType.getArgumentTypes()) {
				domainTypes.add(Expressions.parse(domainType.getName()));
			}
			
			Expression tupleT = Expressions.makeTuple(domainTypes);
			Expression setT   = ExtensionalSet.makeUniSet(tupleT);
			unionArgs.add(setT);
		}
		// if E is g(E&prime;) for g a function symbol distinct from &fnof; and t a k-tuple of expressions
		else  if (Expressions.isFunctionApplicationWithArguments(e)) { // NOTE: we already know its not f(...) as tested beforehand
			// if g(E&prime;) is if C the E<sub>1</sub> else E<sub>2</sub> and C does not contain &fnof;
			if (IfThenElse.isIfThenElse(e) && !containsF(IfThenElse.condition(e), isF)) {
				// then oc<sub>&fnof;</sub>[E] is<br>
				// if C then oc<sub>&fnof;</sub>[E<sub>1</sub>] else oc<sub>&fnof;</sub>[E<sub>2</sub>]				
				Expression e1 = IfThenElse.thenBranch(e);				
				List<Expression> thenUnionArgs = new ArrayList<>();
				computeUnionArgs(thenUnionArgs, isF, fType, isFApplication, e1);
				Expression thenBranch = makeUnion(thenUnionArgs);
				
				Expression e2 = IfThenElse.elseBranch(e);
				List<Expression> elseUnionArgs = new ArrayList<>();
				computeUnionArgs(elseUnionArgs, isF, fType, isFApplication, e2);
				Expression elseBranch = makeUnion(elseUnionArgs);
				
				Expression ifThenElse = IfThenElse.make(IfThenElse.condition(e), thenBranch, elseBranch);
				
				unionArgs.add(ifThenElse);			
			}
			else {
				// otherwise, oc<sub>&fnof;</sub>[E] is 
				// oc<sub>&fnof;</sub>[t<sub>1</sub>] &cup; &hellip;	&cup; oc<sub>&fnof;</sub>[t<sub>k</sub>]
				for (Expression t : e.getArguments()) {
					computeUnionArgs(unionArgs, isF, fType, isFApplication, t);
				}
			}
		}
		// if E is Q<sub>x:C</sub>E&prime; for Q an arbitrary quantifier, x a variable, 
		// C a boolean formula not containing &fnof;, and E&prime; the
		// quantified expression, 
		else if (isArbitraryQuantifier(e) && !containsF(getQuantifierCondition(e), isF)) {
			// then oc<sub>&fnof;</sub>[E] is<br> 
			// &bigcup;<sub>x:C</sub>oc<sub>&fnof;</sub>[E&prime;]
			QuantifiedExpression q = (QuantifiedExpression) e;
			IndexExpressionsSet  x = q.getIndexExpressions();
			Expression           c = getQuantifierCondition(e);
			Expression      ePrime = getQuantifiedExpression(q);
			
			List<Expression> ePrimeUnionArgs = new ArrayList<>();
			computeUnionArgs(ePrimeUnionArgs, isF, fType, isFApplication, ePrime);			
			Expression ocfOfEPrime = makeUnion(ePrimeUnionArgs);
			
			Expression intensionalMultiSet = IntensionalSet.intensionalMultiSet(x, ocfOfEPrime, c);
			Expression intensionalUnion = Expressions.apply(FunctorConstants.INTENSIONAL_UNION, intensionalMultiSet);
			
			unionArgs.add(intensionalUnion);			
		}
		// if E is Q<sub>x:C</sub>E&prime; for Q an arbitrary quantifier, x a variable, 
		// C a boolean formula containing &fnof;, and E&prime; the
		// quantified expression,
		else if (isArbitraryQuantifier(e)) {
			// then oc<sub>&fnof;</sub>[E] is<br> 
			// &bigcup;<sub>x</sub>(oc<sub>&fnof;</sub>[C] &cup; oc<sub>&fnof;</sub>[E&prime;])
			QuantifiedExpression q = (QuantifiedExpression) e;
			IndexExpressionsSet  x = q.getIndexExpressions();
			Expression           c = getQuantifierCondition(e);
			Expression      ePrime = getQuantifiedExpression(q);
			
			List<Expression> conditionUnionArgs = new ArrayList<>();
			computeUnionArgs(conditionUnionArgs, isF, fType, isFApplication, c);
			
			List<Expression> ePrimeUnionArgs = new ArrayList<>();
			computeUnionArgs(ePrimeUnionArgs, isF, fType, isFApplication, ePrime);			
			
			List<Expression> combinedUnionArgs = new ArrayList<>();
			combinedUnionArgs.addAll(conditionUnionArgs);
			combinedUnionArgs.addAll(ePrimeUnionArgs);			
			Expression combinedUnion = makeUnion(combinedUnionArgs);
			
			Expression intensionalMultiSet = IntensionalSet.intensionalMultiSet(x, combinedUnion, Expressions.TRUE);
			Expression intensionalUnion = Expressions.apply(FunctorConstants.INTENSIONAL_UNION, intensionalMultiSet);
			
			unionArgs.add(intensionalUnion);
		}
		else {
			throw new UnsupportedOperationException("Do not have logic for handling expression of the form: "+e);
		}
	}
	
	private static boolean containsF(Expression e, Predicate<Expression> isF) {
		boolean result = Util.thereExists(new SubExpressionsDepthFirstIterator(e), isF);
		return result;
	}
	
	private static boolean isArbitraryQuantifier(Expression e) {
		boolean result = e instanceof QuantifiedExpression;
		return result;
	}
	
	private static Expression getQuantifierCondition(Expression quantifier) {
		Expression result;
		if (quantifier instanceof IntensionalSet) {
			result = ((IntensionalSet)quantifier).getCondition();
		}
		else if (quantifier instanceof CountingFormula) {
			result = ((CountingFormula)quantifier).getBody();
		}
		else if (ForAll.isForAll(quantifier) 
				|| ThereExists.isThereExists(quantifier) 
				|| quantifier instanceof LambdaExpression) {
			// No conditions on these quantifiers, just default to true
			result = Expressions.TRUE; 
		}
		else {
			throw new UnsupportedOperationException("Quantifer currently not supported: "+quantifier);
		}
		
		return result;
	}
	
	private static Expression getQuantifiedExpression(QuantifiedExpression q) {
		Expression result;
		if (q instanceof IntensionalSet) {
			result = ((IntensionalSet)q).getHead();
		}
		else if (q instanceof CountingFormula) {
			result = Expressions.ONE;
		}
		else if (q instanceof QuantifiedExpressionWithABody) {
			result = ((QuantifiedExpressionWithABody)q).getBody();
		}
		else {
			throw new UnsupportedOperationException("Quantified expression currently not supported: "+q);
		}
		return result;
	}
	
	private static Expression makeUnion(List<Expression> allUnionArgs) {
		// Perform trivial simplification by excluding empty set union args
		List<Expression> finalUnionArgs = allUnionArgs.stream()
				.filter(arg -> !Sets.isEmptySet(arg))
				.collect(Collectors.toList());
		Expression result;
		if (finalUnionArgs.size() == 0) {
			result = Sets.EMPTY_SET;
		}
		else {
			result = Sets.makeUnion(finalUnionArgs.toArray(new Expression[finalUnionArgs.size()]));
		}
		return result;
	}
}
