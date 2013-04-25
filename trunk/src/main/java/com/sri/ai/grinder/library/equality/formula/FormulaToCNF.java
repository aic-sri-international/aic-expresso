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
package com.sri.ai.grinder.library.equality.formula;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashSet;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Set;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.grinder.api.Rewriter;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.core.AbstractRewriter;
import com.sri.ai.grinder.core.RewriteOnce;
import com.sri.ai.grinder.core.TotalRewriter;
import com.sri.ai.grinder.helper.GrinderUtil;
import com.sri.ai.grinder.library.Disequality;
import com.sri.ai.grinder.library.Equality;
import com.sri.ai.grinder.library.FunctorConstants;
import com.sri.ai.grinder.library.StandardizedApartFrom;
import com.sri.ai.grinder.library.boole.And;
import com.sri.ai.grinder.library.boole.ForAll;
import com.sri.ai.grinder.library.boole.Not;
import com.sri.ai.grinder.library.boole.Or;
import com.sri.ai.grinder.library.boole.ThereExists;
import com.sri.ai.grinder.library.set.extensional.ExtensionalSet;

@Beta
public class FormulaToCNF {

	/**
	 * Convert a formula into an inferentially equivalent Conjunctive Normal
	 * Form Expression. Transformation rules are based on the INSEADO method
	 * outlined in:
	 * 
	 * <a href=
	 * "http://logic.stanford.edu/classes/cs157/2012/lectures/lecture09.pdf"
	 * >INSEADO Rules (slide 6 to 9</a>
	 * 
	 * 
	 * @param formula
	 *            a formula.
	 * @param process
	 *            the rewriting process
	 * @return false, true, or a conjunction of clauses.
	 * @throws IllegalArgumentException
	 *             if the input formula expression is not actually a formula.
	 * @see FormulaUtil#isFormula(Expression, RewritingProcess)
	 */
	public static Expression convertToCNF(Expression formula, RewritingProcess process) {
		Expression result = formula;

		if (!FormulaUtil.isFormula(formula, process)) {
			throw new IllegalArgumentException(
					"Expression to be converted is not a formula: " + formula);
		}
		
		// I)NSEADO 
		result = implicationsOut(formula, process);
		
		// IN)SEADO
		result = negationsIn(result, process);
		
		// INS)EADO
		result = standardizeVariables(result, process);
		
		// INSE)ADO
		result = existentialsOut(result, process);
		
		// INSEA)DO
		result = allsOut(result, process);
		
		// INSEAD)O
		result = distribution(result, process);
		
		// INSEADO) - Operators Out, we don't do
		// as we want to keep as a conjunction of disjunctions.
			
		if (FormulaUtil.isLiteral(result)) {
			result = Expressions.make(And.FUNCTOR, Expressions.make(Or.FUNCTOR, result));
		}
		else if (Or.isDisjunction(result)) {
			result = Expressions.make(And.FUNCTOR, result);
		}
		
		if (!(result.equals(Expressions.TRUE) || result.equals(Expressions.FALSE))) {
			if (!FormulaUtil.isCNF(result)) {
				throw new IllegalStateException("Failed to convert to CNF: "+result);
			}
		}

		return result;
	}

	//
	// PRIVATE
	//
	private static Expression implicationsOut(Expression formula, RewritingProcess process) {
		TotalRewriter cnfRewriter = new TotalRewriter(Arrays.asList((Rewriter)
				// Want to ensure the following normalizations
				// are applied to ensure the final CNF form is easier
				// to work with.
				new NormalizeOrRewriter(),
				new NormalizeAndRewriter(),
				new NormalizeEqualitiesRewriter(),
				// I)NSEADO 
				new ImplicationsOutRewriter()
			));
		Expression result = cnfRewriter.rewrite(formula, process);	
		return result;
	}
	
	private static Expression negationsIn(Expression formula, RewritingProcess process) {
		TotalRewriter cnfRewriter = new TotalRewriter(Arrays.asList((Rewriter)
				// Want to ensure the following normalizations
				// are applied to ensure the final CNF form is easier
				// to work with.
				new NormalizeOrRewriter(),
				new NormalizeAndRewriter(),
				new NormalizeEqualitiesRewriter(),
				// IN)SEADO
				new NegationsInRewriter()
			));
		Expression result = cnfRewriter.rewrite(formula, process);	
		return result;
	}
	
	private static Expression standardizeVariables(Expression formula, RewritingProcess process) {
		Expression input  = formula;
		Expression result = formula;
		do {
			RewriteOnce svRewriter = new RewriteOnce(Arrays.asList((Rewriter)
					// INS)EADO
					new StandardizeVariablesRewriter()
				));
			input  = result;
			result = svRewriter.rewrite(input, process);
		} while (result != input);
		
		return result;
	}
	
	private static Expression existentialsOut(Expression formula, RewritingProcess process) {
		Expression result = formula;
// TODO - Step 1 - create skolem functions for existentials scoped by universals
		
		// Step 2 - The variables should already be standardized apart at this point
		// therefore just need to drop existentials as we want to introduce 
		// uniquely named free variables as opposed to constants in the 
		// translation of formulas due to the unique names assumption.
		TotalRewriter eoRewriter = new TotalRewriter(Arrays.asList((Rewriter)
				// INSE)ADO
				new ExistentialsOutRewriter()
			));
		result = eoRewriter.rewrite(result, process);
		
// TODO - Step 3 - translate the uninterpreted skolem functions to an equivalent equality formula.
		
		return result;
	}
	
	private static Expression allsOut(Expression formula, RewritingProcess process) {
		TotalRewriter aoRewriter = new TotalRewriter(Arrays.asList((Rewriter)
				// Want to ensure the following normalizations
				// are applied to ensure the final CNF form is easier
				// to work with.
				new NormalizeOrRewriter(),
				new NormalizeAndRewriter(),
				new NormalizeEqualitiesRewriter(),
				// IN)SEADO
				new AllOutRewriter()
			));
		Expression result = aoRewriter.rewrite(formula, process);	
		return result;
	}
	
	private static Expression distribution(Expression formula, RewritingProcess process) {
		TotalRewriter cnfRewriter = new TotalRewriter(Arrays.asList((Rewriter)
				// INSEAD)O
				new DistributionRewriter()
				
			));
		Expression result = cnfRewriter.rewrite(formula, process);	
		return result;
	}
	
	/**
	 * Performs the following normalizations on the formula:
	 * 
	 * or()                   -> false
	 * or(..., true, ...)     -> true
	 * or(..., false, ...)    -> or(..., ...)
	 * or(X = Y, X = Y)       -> or(X = Y)
	 * or(X = Y, ..., X != Y) -> true
	 * 
	 */
	private static class NormalizeOrRewriter extends AbstractRewriter {
			
		@Override
		public Expression rewriteAfterBookkeeping(Expression expression,
				RewritingProcess process) {
			Expression result = expression;
			
			if (Or.isDisjunction(expression)) {
				// or() -> false
				if (expression.numberOfArguments() == 0) {
					result = Expressions.FALSE;
				}
				else {
					// or(..., true, ...)  -> true
					// or(..., false, ...) -> or(..., ...)
					// or(X = Y, X = Y)    -> or(X = Y)
					Set<Expression> literalSet = new LinkedHashSet<Expression>();
					for (Expression disjunct : expression.getArguments()) {
						if (disjunct.equals(Expressions.TRUE)) {
							result = Expressions.TRUE;
							break;
						}
						else {
							if (!disjunct.equals(Expressions.FALSE)) {
								literalSet.add(disjunct);
							}
						}
					}
					if (!result.equals(Expressions.TRUE)) {
						List<Expression> literals = new ArrayList<Expression>(literalSet);
						if (literals.size() < expression.numberOfArguments()) {
							result = Or.make(literals);
						}
						else {
							// or(X = Y, ..., X != Y) -> true
							for (int i = 0; i < literals.size(); i++) {
								for (int j = i+1; j < literals.size(); j++) {
									if (FormulaUtil.isLiteral(literals.get(i)) && 
									    FormulaUtil.isLiteral(literals.get(j)) &&
										!literals.get(i).getFunctor().equals(literals.get(j).getFunctor()) &&
										literals.get(i).getArguments().equals(literals.get(j).getArguments())) {
										result = Expressions.TRUE;
									}
								}
							}
						}
					}
				}
			}
			
			return result;
		}
	}

	/**
	 * Performs the following normalizations on the formula:
	 * and()                   -> true
	 * and(..., true, ...)     -> and(..., ...)
	 * and(X = Y, X = Y)       -> and(X = Y)
	 * and(..., false, ...)    -> false
	 * and(X = Y, ..., X != Y) -> false
	 */
	private static class NormalizeAndRewriter extends AbstractRewriter {
		@Override
		public Expression rewriteAfterBookkeeping(Expression expression,
				RewritingProcess process) {
			Expression result = expression;
			
			if (And.isConjunction(expression)) {
				// and() -> true
				if (expression.numberOfArguments() == 0) {
					result = Expressions.TRUE;
				}
				else {
					// and(..., true, ...)  -> and(..., ...)
					// and(X = Y, X = Y)    -> and(X = Y)
					// and(..., false, ...) -> false
					Set<Expression> literalSet = new LinkedHashSet<Expression>();
					for (Expression conjunct : expression.getArguments()) {
						if (conjunct.equals(Expressions.FALSE)) {
							result = Expressions.FALSE;
							break;
						}
						else {
							if (!conjunct.equals(Expressions.TRUE)) {
								literalSet.add(conjunct);
							}
						}
					}
					if (!result.equals(Expressions.FALSE)) {
						List<Expression> literals = new ArrayList<Expression>(literalSet);
						if (literals.size() < expression.numberOfArguments()) {
							result = And.make(literals);
						}
						else {
							// and(X = Y, ..., X != Y) -> false
							for (int i = 0; i < literals.size(); i++) {
								for (int j = i+1; j < literals.size(); j++) {
									if (FormulaUtil.isLiteral(literals.get(i)) && 
										FormulaUtil.isLiteral(literals.get(j)) &&
										!literals.get(i).getFunctor().equals(literals.get(j).getFunctor()) &&
										literals.get(i).getArguments().equals(literals.get(j).getArguments())) {
										result = Expressions.FALSE;
									}
								}
							}
						}
					}
				}
			}
			
			return result;
		}
	}
	
	/**
	 * Performs the following normalizations on equalities:
	 * 
	 * X = Y = Z = -> X = Y and Y = Z
	 * a = X       -> X = a
	 * a != X      -> X != a
	 * B = A       -> A = B
	 * B != A      -> A != B
	 * X = X       -> true
	 * X != X      -> false
	 * a = b       -> false
	 * a != b      -> true
	 */
	private static class NormalizeEqualitiesRewriter extends AbstractRewriter {
		@Override
		public Expression rewriteAfterBookkeeping(Expression expression,
				RewritingProcess process) {
			Expression result = expression;			
			if (Equality.isEquality(expression) || Disequality.isDisequality(expression)) {
				// X = Y = Z -> X = Y and Y = Z
				if (Equality.isEquality(expression) && expression.numberOfArguments() > 2) {
					List<Expression> conjuncts = new ArrayList<Expression>();
					for (int i = 0; i < expression.numberOfArguments()-1; i++) {
						conjuncts.add(Equality.make(expression.get(i), expression.get(i+1)));
					}
					result = And.make(conjuncts);
				}
				else {
					// a = X  -> X = a
					// a != X -> X != a
					Expression normalized = Equality.normalize(expression, process);
					if (normalized != expression) {
						result = normalized;
					}
					else {
						// B = A  -> A = B
						// B != A -> A != B
						String e0 = expression.get(0).toString();
						String e1 = expression.get(1).toString();
					    if (e0.compareTo(e1) > 0 && process.isVariable(expression.get(0)) && process.isVariable(expression.get(1))) {
					    	result = Expressions.make(expression.getFunctor(), expression.get(1), expression.get(0));
					    }
						// X = X  -> true
						// X != X -> false
					    else if (expression.get(0).equals(expression.get(1))) {
							if (Equality.isEquality(expression)) {
								result = Expressions.TRUE;
							}
							else {
								result = Expressions.FALSE;
							}
						}
						else {
							// a = b  -> false
							// a != b -> true
							if (process.isConstant(expression.get(0)) && process.isConstant(expression.get(1))) {
								if (Equality.isEquality(expression)) {
									result = Expressions.FALSE;
								}
								else {
									result = Expressions.TRUE;
								}
							}
						}
					}
				}
			}
			return result;
		}
	}
	
	/**
	 * Performs the Implications out portion of the formula conversion to CNF:
	 * 
	 * F1 => F2  -> not(F1) or F2
	 * F1 <=> F2 -> (not(F1) or F2) and (F1 or not(F2))
	 */
	private static class ImplicationsOutRewriter extends AbstractRewriter {
		
		@Override
		public Expression rewriteAfterBookkeeping(Expression expression,
				RewritingProcess process) {
			Expression result = expression;
			
			if (expression.hasFunctor(FunctorConstants.IMPLICATION)) {
				// F1 => F2  -> not(F1) or F2
				result = Or.make(Not.make(expression.get(0)), expression.get(1));
			}
			else if (expression.hasFunctor(FunctorConstants.EQUIVALENCE)) {
				// F1 <=> F2 -> (not(F1) or F2) and (F1 or not(F2))
				result = And.make(Or.make(Not.make(expression.get(0)), expression.get(1)),
						          Or.make(expression.get(0), Not.make(expression.get(1))));
			}
			
			return result;
		}
	}
	
	/**
	 * Performs the Negations In portion of the formula conversion to CNF: 
	 * 
	 * not(X = Y)              -> X != Y
	 * not(X != Y)             -> X = Y
	 * not(not(F))             -> F
	 * not(F1 and F2)          -> not(F1) or not(F2)
	 * not(F1 or F2)           -> not(F1) and not(F2)
	 * not(for all X : F)      -> there exists X : not(F)
	 * not(there exists X : F) -> for all X : not(F)
	 */
	private static class NegationsInRewriter extends AbstractRewriter {
		
		@Override
		public Expression rewriteAfterBookkeeping(Expression expression,
				RewritingProcess process) {
			Expression result = expression;
			
			if (expression.hasFunctor(FunctorConstants.NOT)) {
				Expression negated = expression.get(0);
				// not(X = Y) -> X != Y
				if (Equality.isEquality(negated) && negated.numberOfArguments() == 2) {
					result = Disequality.make(negated.get(0), negated.get(1));
				} // not(X != Y) -> X = Y
				else if (Disequality.isDisequality(negated)) {
					result = Equality.make(negated.get(0), negated.get(1));
				} // not(not(F)) -> F
				else if (negated.hasFunctor(FunctorConstants.NOT)) {
					result = negated.get(0);
				} // not(F1 and F2) -> not(F1) or not(F2)
				else if (And.isConjunction(negated) && negated.numberOfArguments() > 0) {
					List<Expression> negatedConjuncts = new ArrayList<Expression>();
					for (Expression conjunct : negated.getArguments()) {
						negatedConjuncts.add(Not.make(conjunct));
					}
					result = Or.make(negatedConjuncts);
				} // not(F1 or F2) -> not(F1) and not(F2)
				else if (Or.isDisjunction(negated) && negated.numberOfArguments() > 0) {
					List<Expression> negatedDisjuncts = new ArrayList<Expression>();
					for (Expression disjunct : negated.getArguments()) {
						negatedDisjuncts.add(Not.make(disjunct));
					}
					result = And.make(negatedDisjuncts);
				} // not(for all X : F) -> there exists X : not(F)
				else if (ForAll.isForAll(negated)) {
					result = ThereExists.make(ForAll.getIndex(negated), Not.make(ForAll.getBody(negated)));
				} // not(there exists X : F) -> for all X : not(F)
				else if (ThereExists.isThereExists(negated)) {
					result = ForAll.make(ThereExists.getIndex(negated), Not.make(ThereExists.getBody(negated)));
				}
			}
			
			return result;
		}
	}
	
	/**
	 * Performs the Standardize Variables portion of the formula conversion to CNF, e.g.:
	 * 
	 * for all X: (there exists Y: X = Y) or (there exists Y: X != Y)
	 * ->
	 * for all X: (there exists Y: X = Y) or (there exists Y': X != Y')
	 * 
	 * Basically quantifiers in the formula using the same index name are made unique.
	 */
	private static class StandardizeVariablesRewriter extends AbstractRewriter {
		
		private Set<Expression> seenIndices = new HashSet<Expression>();
		
		@Override
		public Expression rewriteAfterBookkeeping(Expression expression,
				RewritingProcess process) {
			Expression result = expression;
		
			Expression index = null;
			if (ForAll.isForAll(expression)) {
				index = ForAll.getIndex(expression);
			}
			else if (ThereExists.isThereExists(expression)) {
				index = ThereExists.getIndex(expression);
			}
			
			if (index != null) {
				if (seenIndices.contains(index)) {
					RewritingProcess saProcess = GrinderUtil.extendContextualVariables(ExtensionalSet.makeUniSetExpression(new ArrayList<Expression>(seenIndices)), process);
					
					result = StandardizedApartFrom.standardizedApartFrom(expression, expression, saProcess);
				}
				else {
					seenIndices.add(index);
				}
			}
			
			return result;
		}
	}
	
	/**
	 * Performs the drop existential quantifiers portion of the formula conversion to CNF:
	 * 
	 * there exists X: X = Y -> X = Y
	 * 
	 */
	private static class ExistentialsOutRewriter extends AbstractRewriter {
		
		@Override
		public Expression rewriteAfterBookkeeping(Expression expression,
				RewritingProcess process) {
			Expression result = expression;
			
			if (ThereExists.isThereExists(expression)) {
				result = ThereExists.getBody(expression);
			}
			
			return result;
		}
	}
	
	/**
	 * Performs the drop universal quantifiers portion of the formula conversion to CNF:
	 * 
	 * for all X: X = Y -> X = Y
	 * 
	 */
	private static class AllOutRewriter extends AbstractRewriter {
		
		@Override
		public Expression rewriteAfterBookkeeping(Expression expression,
				RewritingProcess process) {
			Expression result = expression;
			
			if (ForAll.isForAll(expression)) {
				result = ForAll.getBody(expression);
			}
			
			return result;
		}
	}
	
	/**
	 * Performs the Or Distribution portion of the formula conversion to CNF:
	 *
	 * F1 or (F1 and F2)          -> (F1 or F2) and (F1 or F3)
	 * (F1 and F2) or F3          -> (F1 or F3) and (F2 or F3)
	 * F0 or (F1 or ... or Fn)    -> (F0 or F1 or ... or Fn)
	 * (F1 or ... or Fn) or F0    -> (F1 or ... or Fn or F0)
	 * F0 and (F1 and ... and Fn) -> (F0 and F1 and ... and Fn)
	 * (F1 and ... and Fn) and F0 -> (F1 and ... and Fn and F0) 
	 * L1 and L2                  -> and(or(L1), or(L2))
	 */
	private static class DistributionRewriter extends AbstractRewriter {
		
		@Override
		public Expression rewriteAfterBookkeeping(Expression expression,
				RewritingProcess process) {
			Expression result = expression;
			
			if (Or.isDisjunction(expression) && expression.numberOfArguments() > 0) {
				// F1 or (F1 and F2) -> (F1 or F2) and (F1 or F3)
				// (F1 and F2) or F3 -> (F1 or F3) and (F2 or F3)
				for (Expression disjunct : expression.getArguments()) {
					if (And.isConjunction(disjunct) && disjunct.numberOfArguments() > 0) {
						List<Expression> otherDisjuncts = new ArrayList<Expression>(expression.getArguments());
						otherDisjuncts.remove(disjunct);
						
						List<Expression> conjuncts = new ArrayList<Expression>();
						for (Expression conjunct : disjunct.getArguments()) {
							ArrayList<Expression> disjuncts = new ArrayList<Expression>(otherDisjuncts);
							disjuncts.add(conjunct);
							conjuncts.add(Or.make(disjuncts));
						}
						result = And.make(conjuncts);
						break;
					}
				}
				
				if (result == expression) {
					// F0 or (F1 or ... or Fn) -> (F0 or F1 or ... or Fn)
					// (F1 or ... or Fn) or F0 -> (F1 or ... or Fn or F0)
					boolean nestedDisjuncts = false;
					List<Expression> disjuncts = new ArrayList<Expression>();
					for (Expression disjunct : expression.getArguments()) {
						if (Or.isDisjunction(disjunct)) {
							nestedDisjuncts = true;
							disjuncts.addAll(disjunct.getArguments());
						}
						else {
							disjuncts.add(disjunct);
						}
					}
					if (nestedDisjuncts) {
						result = Or.make(disjuncts);
					}
				}
			}
			else if (And.isConjunction(expression) && expression.numberOfArguments() > 0) {
				// F0 and (F1 and ... and Fn) -> (F0 and F1 and ... and Fn)
				// (F1 and ... and Fn) and F0 -> (F1 and ... and Fn and F0)
				// L1 and L2                  -> and(or(L1), or(L2))
				boolean newConjunct = false;
				List<Expression> conjuncts = new ArrayList<Expression>();
				for (Expression conjunct : expression.getArguments()) {
					if (And.isConjunction(conjunct)) {
						newConjunct = true;
						conjuncts.addAll(conjunct.getArguments());
					}
					else {
						if (FormulaUtil.isLiteral(conjunct)) {
							newConjunct = true;
							conjuncts.add(Expressions.make(Or.FUNCTOR, conjunct));
						}
						else {
							conjuncts.add(conjunct);
						}
					}
				}
				if (newConjunct) {
					result = And.make(conjuncts);
				}
			}
			
			return result;
		}
 	}
}
