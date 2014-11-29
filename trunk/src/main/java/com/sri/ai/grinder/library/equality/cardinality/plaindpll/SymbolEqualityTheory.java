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

import java.util.Collection;
import java.util.Map;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.library.Disequality;
import com.sri.ai.grinder.library.Equality;
import com.sri.ai.grinder.library.FunctorConstants;
import com.sri.ai.grinder.library.IsVariable;
import com.sri.ai.grinder.library.boole.And;
import com.sri.ai.grinder.library.boole.Equivalence;
import com.sri.ai.grinder.library.boole.ForAll;
import com.sri.ai.grinder.library.boole.Implication;
import com.sri.ai.grinder.library.boole.Not;
import com.sri.ai.grinder.library.boole.Or;
import com.sri.ai.grinder.library.boole.ThereExists;
import com.sri.ai.grinder.library.controlflow.IfThenElse;
import com.sri.ai.util.Util;
import com.sri.ai.util.base.BinaryFunction;
import com.sri.ai.util.base.Equals;

@Beta
/** 
 * A {@link Theory} for equality literals.
 */
public class SymbolEqualityTheory extends AbstractTheory {

	private static Map<String, BinaryFunction<Expression, RewritingProcess, Expression>> functionApplicationSimplifiers =
			Util.<String, BinaryFunction<Expression, RewritingProcess, Expression>>map(
					FunctorConstants.EQUALITY,       (BinaryFunction<Expression, RewritingProcess, Expression>) (f, process) ->
					Equality.simplify(f, process),
					
					FunctorConstants.DISEQUALITY,    (BinaryFunction<Expression, RewritingProcess, Expression>) (f, process) ->
					Disequality.simplify(f, process),

					FunctorConstants.DISEQUALITY,    (BinaryFunction<Expression, RewritingProcess, Expression>) (f, process) ->
					Disequality.simplify(f, process),

					FunctorConstants.AND,            (BinaryFunction<Expression, RewritingProcess, Expression>) (f, process) ->
					And.simplify(f),

					FunctorConstants.OR,             (BinaryFunction<Expression, RewritingProcess, Expression>) (f, process) ->
					Or.simplify(f),

					FunctorConstants.NOT,            (BinaryFunction<Expression, RewritingProcess, Expression>) (f, process) ->
					Not.simplify(f),

					FunctorConstants.IF_THEN_ELSE,   (BinaryFunction<Expression, RewritingProcess, Expression>) (f, process) ->
					IfThenElse.simplify(f),

					FunctorConstants.EQUIVALENCE,    (BinaryFunction<Expression, RewritingProcess, Expression>) (f, process) ->
					Equivalence.simplify(f),

					FunctorConstants.IMPLICATION,    (BinaryFunction<Expression, RewritingProcess, Expression>) (f, process) ->
					Implication.simplify(f)
	);
	
	private static Map<String, BinaryFunction<Expression, RewritingProcess, Expression>> syntacticFormTypeSimplifiers =
			Util.<String, BinaryFunction<Expression, RewritingProcess, Expression>>map(
					ForAll.SYNTACTIC_FORM_TYPE,                             (BinaryFunction<Expression, RewritingProcess, Expression>) (f, process) ->
					(new SymbolEqualityTautologicalityDPLL()).rewrite(f, process),
 
					ThereExists.SYNTACTIC_FORM_TYPE,                        (BinaryFunction<Expression, RewritingProcess, Expression>) (f, process) ->
					(new DPLLGeneralizedAndSymbolic(new SymbolEqualityTheory(), new Satisfiability())).rewrite(f, process)
	);

	@Override
	public Map<String, BinaryFunction<Expression, RewritingProcess, Expression>> getFunctionApplicationSimplifiers() {
		return functionApplicationSimplifiers;
	}

	@Override
	public Map<String, BinaryFunction<Expression, RewritingProcess, Expression>> getSyntacticFormTypeSimplifiers() {
		return syntacticFormTypeSimplifiers;
	}

	/**
	 * If expression is a literal with at least one variable, turns it into a valid splitter, or returns null otherwise.
	 * @param expression
	 * @param indices
	 * @param process
	 * @return
	 */
	@Override
	public Expression makeSplitterIfPossible(Expression expression, Collection<Expression> indices, RewritingProcess process) {
		Expression result = null;
		if (expression.hasFunctor(FunctorConstants.EQUALITY) || expression.hasFunctor(FunctorConstants.DISEQUALITY)) {
			// remember that equality can have an arbitrary number of terms
			Expression variable = Util.getFirstSatisfyingPredicateOrNull(expression.getArguments(), new IsVariable(process));
			Expression otherTerm = Util.getFirstSatisfyingPredicateOrNull(expression.getArguments(), com.sri.ai.util.base.Not.make(Equals.make(variable)));
			result = makeSplitterWithIndexIfAnyComingFirst(variable, otherTerm, indices);
		}
		return result;
	}

	protected static Expression makeSplitterWithIndexIfAnyComingFirst(Expression variable, Expression otherTerm, Collection<Expression> indices) {
		Expression result;
		// if variable is a free variable or constant and other term is an index, we invert them because
		// the algorithm requires the first term to be an index if there are any indices in the atom.
		if ( ! indices.contains(variable) && indices.contains(otherTerm) ) {
			result = Equality.make(otherTerm, variable);
		}
		else {
			result = Equality.make(variable, otherTerm);
		}
		return result;
	}

	protected static Expression makeSplitterFromTwoTerms(Expression term1, Expression term2, Collection<Expression> indices, RewritingProcess process) {
		Expression result;
		// if variable is a free variable or constant and other term is an index, we invert them because
		// the algorithm requires the first term to be an index if there are any indices in the atom.
		if (indices.contains(term1)) {
			result = Equality.make(term1, term2);
		}
		else if (indices.contains(term2)) {
			result = Equality.make(term2, term1);
		}
		else if (process.isVariable(term1)) {
			result = Equality.make(term1, term2);
		}
		else {
			result = Equality.make(term2, term1);
		}
		return result;
	}

	@Override
	public Expression applySplitterToExpression(Expression splitter, Expression expression, RewritingProcess process) {
		Expression term1 = splitter.get(0);
		Expression term2 = splitter.get(1);
		Expression result = expression.replaceAllOccurrences(term1, term2, process);
		result = simplify(result, process);
		return result;
	}

	@Override
	public Expression applySplitterNegationToExpression(Expression splitter, Expression expression, RewritingProcess process) {
		Expression term1 = splitter.get(0);
		Expression term2 = splitter.get(1);
		Expression result = expression.replaceAllOccurrences(new SimplifyLiteralGivenDisequality(term1, term2), process);
		result = simplify(result, process);
		return result;
	}

	@Override
	public boolean splitterInvolvesIndex(Expression splitter, Collection<Expression> indices) {
		boolean result = indices.contains(splitter.get(0));
		return result;
	}

	private boolean generic = false;
	
	@Override
	public Expression applySplitterToSolution(boolean splitterSign, Expression splitter, Expression solution, RewritingProcess process) {
		if (generic) {
			return super.applySplitterToSolution(splitterSign, splitter, solution, process);
		}
		
		if ( ! splitterSign) {
			return applySplitterNegationToSolution(splitter, solution, process);
		}
		
		// Some notes about the development of this and the next method are at the bottom of the file.
		// They discuss the main ideas, but the implementation still turned out a little different from them.
		
		Expression result = solution;
		
		if (IfThenElse.isIfThenElse(solution)) {
	
			Expression solutionSplitter = IfThenElse.getCondition (solution);
			Expression thenBranch = IfThenElse.getThenBranch(solution);
			Expression elseBranch = IfThenElse.getElseBranch(solution);
	
			Expression solutionSplitterSimplification = applySplitterToExpression(splitter /* being applied to expression */, solutionSplitter /* expression */, process);
			
			if (solutionSplitterSimplification.equals(Expressions.TRUE)) {
				result = applySplitterToSolution(splitterSign, splitter, thenBranch, process);
			}
			else if (solutionSplitterSimplification.equals(Expressions.FALSE)) {
				result = applySplitterToSolution(splitterSign, splitter, elseBranch, process);
			}
			else {
				Expression newThenBranch = applySplitterToSolution(splitterSign, splitter, thenBranch, process);
				Expression newElseBranch = applySplitterToSolution(splitterSign, splitter, elseBranch, process);
				
				// solutions conditions must always have a variable as first argument
				Expression newSolutionSplitter = makeSolutionSplitterFromNonTrivialSolutionSplitterSimplificationByAnotherSolutionSplitter(solutionSplitterSimplification, process);

				// Simplification may have create opportunities for original splitter (or its simplification, at this point)
				// to be able to simplify solution even further.
				newThenBranch = applySplitterToSolution(splitterSign, newSolutionSplitter, newThenBranch, process);
				// It is important to realize why this second transformation on the then branch
				// does not invalidate the guarantees given by the first one,
				// as well as why individual completeness for equalityOfTwoTerms and newCondition
				// imply completeness with respect to their *conjunction*.
				// The guarantees of the first complete simplification given equalityOfTwoTerms are not lost because,
				// if they were, there would be a condition that could be replaced by true or false given equalityOfTwoTerms.
				// This however would require the first variable in equalityOfTwoTerms to be present, and it is not
				// because it was eliminated by the first complete simplification and it does not get re-introduced by the second one.
				// The completeness with respect to the conjunction comes from the fact that the only possible facts implied
				// by a conjunction of equalities that could simplify a condition while these individual equalities could not,
				// would be a consequence of them, and the only consequences of them are transitive consequences.
				// For example, X = Z can be simplified by the conjunction (X = Y and Y = Z), even though it cannot be simplified
				// by either X = Y or by Y = Z, but it can be simplified by X = Z which is a transitive consequence of the two.
				// However, such transitive consequences are explicitly produced by replacing the first argument of an equality
				// during the simplification. Using X = Y to replace all Y by X will replace Y in Y = Z and produce a new condition X = Z,
				// which represents the transitive consequence explicitly and which will simplify whatever conditions depend on it.
				//
				// Another approach is to consider every possible type of condition configuration.
				// It is more detailed and takes more work to implement, but it would save some unnecessary substitutions.
				// A schema of these substitutions is described in the file SimplifyFormulacompleteSimplifySolutionGivenEqualitySubstitutionSchemas.jpg
				// stored in the same directory as this file.
				newElseBranch = applySplitterNegationToSolution(newSolutionSplitter, newElseBranch, process);
			
				result = IfThenElse.makeIfDistinctFrom(solution, newSolutionSplitter, newThenBranch, newElseBranch, false /* no simplification to condition */);
			}
		}
		else {
			result = applySplitterNegationToExpression(splitter, solution, process);
		}

		return result;
	}

	protected Expression makeSolutionSplitterFromNonTrivialSolutionSplitterSimplificationByAnotherSolutionSplitter(Expression solutionSplitterSimplification, RewritingProcess process) {
		return Equality.makeSureFirstArgumentIsNotAConstant(solutionSplitterSimplification, process);
	}

	/**
	 * Overridden because in in this theory splitters simplified by splitter negations are either trivial or do not change.
	 */
	public Expression applySplitterNegationToSolution(Expression splitter, Expression solution, RewritingProcess process) {
		if (generic) {
			return super.applySplitterNegationToSolution(splitter, solution, process);
		}

		Expression result = solution;
		
		if (IfThenElse.isIfThenElse(solution)) {
	
			Expression solutionSplitter  = IfThenElse.getCondition(solution);
			Expression thenBranch = IfThenElse.getThenBranch(solution);
			Expression elseBranch = IfThenElse.getElseBranch(solution);
	
			Expression solutionSplitterSimplification = applySplitterNegationToExpression(splitter, solutionSplitter, process);
			
			if (solutionSplitterSimplification.equals(Expressions.TRUE)) {
				result = applySplitterToSolution(false, splitter, thenBranch, process);
			}
			else if (solutionSplitterSimplification.equals(Expressions.FALSE)) {
				result = applySplitterToSolution(false, splitter, elseBranch, process);
			}
			else {
				// Note that, at this point, solutionSplitterSimplification is the same as solutionSplitter because
				// the only simplification that a disequality performs when applied to a condition is to transform it to either true or false,
				// and this has already been tested to not have occurred.
				
				Expression splitterUnderSolutionSplitter;
				splitterUnderSolutionSplitter = adaptSplitterNegationToBeUsedUnderSolutionSplitter(splitter, solutionSplitter, process);

				Expression newThenBranch = thenBranch;
				if ( ! splitterUnderSolutionSplitter.equals(Expressions.FALSE)) {
					newThenBranch = applySplitterToSolution(false, splitterUnderSolutionSplitter, thenBranch, process);
				}
				Expression newElseBranch = applySplitterToSolution(false, splitter, elseBranch, process);
				
				result = IfThenElse.makeIfDistinctFrom(solution, solutionSplitterSimplification, newThenBranch, newElseBranch, false /* no simplification to condition */);
			}
		}
		else {
			result = applySplitterNegationToExpression(splitter, solution, process);
		}

//		Expression superResult = super.applySplitterNegationToSolution(splitter, solution, process);
//		if ( ! superResult.equals(result)) {
//			System.out.println("Difference from two versions detected.");
//		}

		return result;
	}

	/**
	 * Takes a non-trivial simplification of a splitter by another splitter's negation and normalize it into a proper splitter if needed.
	 * @param solutionSplitterSimplification
	 * @param process
	 * @return
	 */
	protected Expression makeSolutionSplitterFromNonTrivialSolutionSplitterSimplificationByAnotherSolutionSplitterNegation(Expression solutionSplitterSimplification, RewritingProcess process) {
		// for this theory, simplification by a splitter negation either trivializes another splitter, or does not change it at all.
		return solutionSplitterSimplification;
	}

	/**
	 * We need to simplify the disequality with the condition because the first variable in the condition is not present in the then branch;
	 * therefore, if the disequality is about that first variable, it will be ineffectual as-is and needs to be replaced
	 * by an disequality that is translated by the term used instead inside the then branch.
	 * For example, if disequality is X != a and the condition is X = Y,
	 * then the then branch does not contain X, but Y instead which represents X.
	 * We must then apply the disequality Y != a.
	 * @param splitter
	 * @param solutionSplitter
	 * @param process
	 * @return
	 */
	private Expression adaptSplitterNegationToBeUsedUnderSolutionSplitter(Expression splitter, Expression solutionSplitter, RewritingProcess process) {
		Expression splitterNegation = Disequality.make(splitter.get(0), splitter.get(1));
		Expression splitterNegationUnderSolutionSplitter = applySplitterToExpression(solutionSplitter, splitterNegation, process);

		assert ! splitterNegationUnderSolutionSplitter.equals(Expressions.FALSE): "Splitter negation cannot be false in this function";
		
		Expression splitterUnderSolutionSplitter;
		if (splitterNegationUnderSolutionSplitter.equals(Expressions.TRUE)) {
			splitterUnderSolutionSplitter = Expressions.FALSE;
		}
		else {
			splitterUnderSolutionSplitter = Equality.make(splitterNegationUnderSolutionSplitter.get(0), splitterNegationUnderSolutionSplitter.get(1));
		}
		return splitterUnderSolutionSplitter;
	}

	@Override
	public TheoryConstraint makeConstraint(Collection<Expression> indices) {
		return new EqualityOnSymbolsConstraint(indices);
	}
}

/*
 * Some notes written while developing the complete simplification methods.
 * 
 * Theorem: Given a condition C and a solution S,
the simplification S[C] does not contain tautological or contradictory conditions.

C is of the form X = T.
If S is a leaf, the theorem holds trivially.
If S is "if C1 then S1 else S2"
    if C1 is X = T1
        S[C] is if T = T1 then S1[C] else S2[C]
        if T = T1 gets simplified to true or false, theorem holds
        Otherwise, we need to prove that S1[C] is complete
           under X = T and T = T1, and S2[C] is complete
           under X = T and T != T1.
        S1[C]: X does not occur in S1, only T1 and possibly T
        Example: S1 is (T = T1), then S1[C] is S1,
        so S[C] is if T = T1 then T = T1 then S2[C], not complete!

Counter example then is
C is X = T
S is if X = T1 then T = T1 else S2
S[C] thus is if T = T1 then T = T1 else S2[C], not complete.

Create model counting test for this externalization of the sum of two solutions

(if X = T then 1 else 0) + (if X = T1 then if T = T1 then 1 else 0 else 0)

In model counting, the above can arise from a splitting on an atom, say, Y = a, that works as a selector:

(Y = a and X = T) or (Y != a and X = T1 and T = T1)

Fix:
create an algorithm for applying condition that takes heed of new created conditions. Still need to prove that applying one condition at a time is enough.

Special case in which we don't need completeness (because, say, we are within DPLL so all conditions will be split anyway) still possibly desirable.
Can improve anyway by simplifying during condition application. It could be that simplifying to completeness right away is more efficient anyway.
At first I thought that perhaps that simple application is complete when inside DPLL anyway, but that is not true, counter-example above could occur.

Candidate equality application algorithm:
Let C be an equality X = T
Let S be a complete solution.

if S is a constant
    return S
else if S is "if C1 then S1 else S2"
    C1' <- atom-apply C to C1
    if C1' is true
        S <- apply C to S1
    else if C1' false
        S <- apply C to S2
    else if C1' is not C1
        S1 <- apply C to S1
        S2 <- apply C to S2
        S1 <- apply C1' to S1
        S2 <- apply C1' to S2
        S <- if C1' then S1 else S2

    return S
    
Post-conditions:
- the result is equivalent to S in the C-space. In other words,
for every model M that satisfies C, S and the result evaluate to the same value in M.

- Every condition in the result that is not 'true' or 'false' is both satisfied by some model in its own context,
and falsified by some other model in its own context,
including the top context being the models of C.

It seems hard to prove it because Si could be non-trivial for C, meaning it is true and false for different models of C,
and the same for C1', but still not be both true and false for different models of the *intersection* of C and C1'.
Perhaps this never happens for equality theory but we would need to take the theory into account to prove that.

Another approach is to represent the entire context as a graph of variables and disequalities and go down the formula applying it.
The graph is a consolidated representation of context and makes it easy to decide whether any atom is implied or not.
Then we have the following algorithm:

Joint application algorithm:
*** NEW *** C is a context graph
Let S be a complete solution.

if S is a constant
    return S
else if S is "if C1 then S1 else S2"
    if C1 is true in C
        S <- apply C to S1     // 10/2014: do we really need to apply C to S1 if S1 was already complete wrt C1?
    else if C1 is false in C
        S <- apply C to S2
    else
        CC1 <- apply C1 to constraint C // CC1 is constraint graph
        S1 <- apply CC1 to S1
        CnotC1 <- apply not C1 to constraint C
        S2 <- apply CnotC1 to S2
        S <- if C1 then S1 else S2

    return S

This is better but it rebuilds a lot of the context graph that had already been solved when S was built.
Can we leverage the fact that we know S is already solved and complete?

Let us consider case by case.

C is X = Y, C1 is Z = W
    can we bypass C1 because it is unaffected by C? What if we have Y = W and X = Z in the else clause of C1 (where Z != W)? Then that should be false, but bypassing does not pick it. We need the information that Z != W. However bypassing is safe in the then clause because Z is not present in it (the fact that Z = W is implicit everywhere).

C is X = Y, C1 is Y = W
    apply X = W to then clause
    apply to else clause keeping X != W.

C is X = Y, C1 is X = W
    if Y is W, return S1
    if Y cannot be W, return S2
    otherwise, need to apply Y = W to then clause
    apply to else clause keeping X != W.

C is X = Y, C1 is Z = Y
    bypass then clause, apply to else clause

C is X = Y, C1 is Z = X
    bypass then clause, apply to else clause

As we can see, we still need to keep the constraint graph for the else clause but can do a little better with the then clause.

What if C is a disequality?

C is X != Y, C1 is Z = W
    bypass C to then clause
    apply to else clause keeping C1 negation

C is X != Y, C1 is Y = W
    apply X != W to then clause
    apply to else clause keeping C1 negation.

C is X != Y, C1 is X = W
    if Y is W, return S2, because X = W <=> X = Y <=> false
    if Y cannot be W, return S because X != Y is irrelevant
    otherwise, need to apply W != Y to then clause
    apply to else clause keeping C1 negation.

C is X != Y, C1 is Z = Y
    apply X != Z to then clause
    apply to else clause keeping C1 negation.

C is X != Y, C1 is Z = X
    apply Z != Y to then clause
    apply to else clause keeping C1 negation.

Clearly this is a lot more efficient than the joint application shown above.
Besides, the joint application requires a constraint map that keeps equalities as well, which we don't have so far
(and which would be less efficient as well).
So this is the way to go.
This depends on the equalities being such that the first term does not appear in the then clause!

*/