package com.sri.ai.grinder.helper;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Set;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.IndexExpressionsSet;
import com.sri.ai.expresso.api.IntensionalSet;
import com.sri.ai.expresso.core.DefaultIntensionalUniSet;
import com.sri.ai.expresso.core.ExtensionalIndexExpressionsSet;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.grinder.api.Rewriter;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.core.ExhaustiveRewriter;
import com.sri.ai.grinder.library.FunctorConstants;
import com.sri.ai.grinder.library.SemanticSubstitute;
import com.sri.ai.grinder.library.boole.And;
import com.sri.ai.grinder.library.boole.Not;
import com.sri.ai.grinder.library.boole.Or;
import com.sri.ai.grinder.library.boole.ThereExists;
import com.sri.ai.grinder.library.controlflow.IfThenElse;
import com.sri.ai.grinder.library.equality.cardinality.CardinalityUtil;
import com.sri.ai.grinder.library.equality.cardinality.direct.CardinalityRewriter;
import com.sri.ai.grinder.library.equality.formula.FormulaUtil;
import com.sri.ai.grinder.library.indexexpression.IndexExpressions;
import com.sri.ai.grinder.library.set.Sets;
import com.sri.ai.grinder.library.set.intensional.IntensionalSetWithBoundIndex;
import com.sri.ai.util.Util;

public class PickSingleElement {

	/**
	 * <pre>
	 * pick_single_element({ (on I) Alpha | C })
	 * Returns its singleton element if the given uniset is a singleton, or null otherwise.
	 * R <- supportedIndices in I that Alpha depends on 
	 * if R is empty, return Alpha.
	 * let SomeIndex be an index in R
	 * let R' be R - {SomeIndex}
	 * value = pick_value(SomeIndex, R', C)
	 * if value is null, return null
	 * return pick_single_element({ (on R') Alpha[SomeIndex/value] | C[SomeIndex/value] })
	 * </pre>
	 * 
	 * @param intensionalSet
	 *            an intensional uniset that is assumed to be a singleton.
	 * @param process
	 *            the process in which the rewriting is occurring.
	 * @return its singleton element or null if one cannot be determined..
	 */
	public static Expression pickSingleElement(Expression intensionalSet, RewritingProcess process) {
		Expression result = null;
		
		// intensionalSet is of the form: { (on I) Alpha | C }
		if (!Sets.isIntensionalUniSet(intensionalSet)) {
			throw new IllegalArgumentException("Not a valid intensional set expression:"+intensionalSet);
		}
		
		Trace.in("+pick_single_element({}) constrained by {}", intensionalSet, process.getContextualConstraint());
		
		intensionalSet = process.rewrite(eliminatesBoundIndices, intensionalSet);
		
		Expression       alpha       = ((IntensionalSet) intensionalSet).getHead();
		IndexExpressionsSet indexExpressionsSet = ((IntensionalSet) intensionalSet).getIndexExpressions();
		assert indexExpressionsSet instanceof ExtensionalIndexExpressionsSet : "PickSingleElement not implemented for intensional sets with non-extensional index expressions"; 
		List<Expression> indexExpressions = new ArrayList<Expression>(((ExtensionalIndexExpressionsSet) indexExpressionsSet).getList());
		Trace.log("R <- supportedIndices in {} that {} depends on", indexExpressions, alpha);
		Set<Expression>  alphaVars   = Expressions.freeVariables(alpha, process);
		List<Expression> indicesI    = new ArrayList<Expression>(IndexExpressions.getIndices(indexExpressionsSet));
		Set<Expression>  tempIndices = new LinkedHashSet<Expression>(indicesI);
		tempIndices.retainAll(alphaVars);
		List<Expression> indicesR  = new ArrayList<Expression>(tempIndices);
		Trace.log("// R = {}", indicesR);
		
		if (indicesR.size() == 0) {
			Trace.log("if R is empty");
			Trace.log("    return Alpha // Alpha = {}", alpha);
			result = alpha;
		} 
		else {
			Trace.log("let SomeIndex be an index in R");
			Expression someIndex = indicesR.get(0); 
			Trace.log("// SomeIndex = {}", someIndex);
			Trace.log("let R' be R - {SomeIndex}");
			List<Expression> indexExpressionsRPrime = new ArrayList<Expression>(indexExpressions);
			Util.removeElementsSatisfying(indexExpressionsRPrime, new IndexExpressions.HasIndex(someIndex));
			Trace.log("// R' = {}", indexExpressionsRPrime);
			
			Trace.log("value = pick_value(SomeIndex, R', C)");
			Expression formulaC   = ((IntensionalSet) intensionalSet).getCondition();
			RewritingProcess subProcess = GrinderUtil.extendContextualSymbolsWithIntensionalSetIndices(intensionalSet, process);
			Expression value = pickValue(someIndex, indexExpressionsRPrime, formulaC, subProcess);
			Trace.log("// value = {}", value);
			
			if (value == null) {
				Trace.log("if value is null, return null");
			}
			else {
				Trace.log("return pick_single_element({ (on R') Alpha[X/value] | C[X/value] })");
				Expression alphaSubX          = SemanticSubstitute.replace(alpha, someIndex, value, subProcess);
				Expression formulaCSubX       = SemanticSubstitute.replace(formulaC, someIndex, value, subProcess);
				Expression intensionalSetSubX = new DefaultIntensionalUniSet(indexExpressionsRPrime, alphaSubX, formulaCSubX);
	
				result = pickSingleElement(intensionalSetSubX, process);
			}
		}
		
		Trace.out("-pick_single_element={}", result);
		
		return result;
	}

	public final static Rewriter eliminatesBoundIndices = new ExhaustiveRewriter(new IntensionalSetWithBoundIndex());

	/**
	 * <pre>
	 * pick_value(X, I, C)
	 * Takes a variable X, a set of variables I (not containing X), and a formula C.
	 * Note that C may contain other variables not in I.
	 * Assumes that there is a single assignment to X and I satisfying C.
	 * Assumes that C is not a conjunction with conjunct X = v.
	 * Returns a value v such that there exists I : C <=> X = v or null if it cannot be determined.
	 * Example:
	 * if Y = d then X = a else X = b
	 * implies that X is equal to the conditional value
	 * if Y = d then a else b
	 * 
	 * formula_on_X = R_formula_simplification(there exists I’ : C) // where I' is I \ {X}
	 * if X = value can be unambiguously extracted from formula_on_X
	 *     return value
	 *      
	 * // Need to use full satisfiability check to pick value
	 * possible_determined_values <- (constants of C) union ( (free variables of formula_on_X and context) \ {X}) // (1)
	 * result <- get_conditional_single_value_or_null_if_not_defined_in_all_contexts(X, formula_on_X, possible_determined_values)
	 * return result
	 * 
	 * Implementation Notes:
	 * (1) Preference is to check free variables before constants as it may yield more general answers.
	 * For example, (if Z = d then X = a and X = Y else X = b and X = Y) implies both
	 * X = if Z = d then a else b
	 * and X = Y
	 * and the latter is computationally more efficient. </pre>
	 * 
	 * @param variableX
	 *            a variable X
	 * @param variablesI
	 *            a list of index variables I
	 * @param formulaC
	 *            a formula C
	 * @param process
	 *            the process in which the rewriting is occurring.
	 * @return a value v such that there exists I : C <=> X = v or null if not
	 *         able to pick.
	 */
	public static Expression pickValue(Expression variableX, List<Expression> variablesI, Expression formulaC, final RewritingProcess process) {
		Expression result = null;
		
		Trace.in("+pick_value({}, {}, {})", variableX, variablesI, formulaC);
	
		Trace.log("formula_on_X = R_formula_simplification(there exists I : C)"); 
		List<Expression> variablesIPrime = new ArrayList<Expression>();
		for (Expression i : variablesI) {
			// where I' is I \ {X}
			if (!variableX.equals(i)) {
				variablesIPrime.add(i);
			}
		}
	
		IndexExpressionsSet indexExpressions = GrinderUtil.makeIndexExpressionsForIndicesInListAndTypesInContext(variablesIPrime, process);
	
		Expression thereExists = ThereExists.make(indexExpressions, formulaC);
		Expression formulaOnX  = process.rewrite(CardinalityRewriter.R_normalize, thereExists);
	
		result = extractValueForXFromFormula(variableX, variablesI, formulaOnX, process);
		if (result != null) {
			Trace.log("if X = value can be unambiguously extracted from formula_on_X");
			Trace.log("    return value");
		}
		else {
			// Need to use full satisfiability check to pick value
	
			Trace.log("possible_determined_values <- (free variables of formula_on_X and context) union ( (constants of formula_on_X and context) \\ {X})");
	
			// (1) Preference is to check free variables before constants.
			Set<Expression> possibleDeterminedValues = new LinkedHashSet<Expression>();
			Expression formulaOnXAndContext = CardinalityUtil.makeAnd(formulaOnX, process.getContextualConstraint());
			possibleDeterminedValues.addAll(Expressions.freeVariables(formulaOnXAndContext, process));
			possibleDeterminedValues.addAll(FormulaUtil.getConstants(formulaOnXAndContext, process));
			possibleDeterminedValues.remove(variableX);
			Trace.log("// possible_determined_values = {}", possibleDeterminedValues);
	
			Trace.log("result <- get_conditional_single_value_or_null_if_not_defined_in_all_contexts(X, formula_on_X, possible_determined_values)");
			result = getConditionalSingleValueOrNullIfNotDefinedInAllContexts(variableX, formulaOnX, possibleDeterminedValues, process);
			// Unnecessary since getConditionalSingleValueOrNullIfNotDefinedInAllContexts already returns it completely normalized.
			//				if (result == null) {
			//					Trace.log("if result is null, return null");
			//				}
			//				else {
			//					Trace.log("return R_complete_normalize(result)");
			//					result = process.rewrite(LBPRewriter.R_complete_normalize, result);		
			//				}
	
		}
	
		Trace.log("return result: {}", result);
		Trace.out("-pick_value={}", result);
		
		return result;
	}

	public static Set<Expression> getPossibleValuesForVariableInFormulaAndContext(Expression variable, Expression formula, RewritingProcess process) {
		Set<Expression> result = new LinkedHashSet<Expression>();
		Expression formulaAndContext = CardinalityUtil.makeAnd(formula, process.getContextualConstraint());
		result.addAll(FormulaUtil.getConstants(formulaAndContext, process));
		result.addAll(Expressions.freeVariables(formulaAndContext, process));
		result.remove(variable);
		return result;
	}

	/**
	 * Same as {@link LPIUtil#getConditionalSingleValueOrNullIfNotDefinedInAllContexts(Expression, Expression, Set, RewritingProcess)},
	 * but computing the possible values from the formula.
	 */
	public static Expression getConditionalSingleValueOrNullIfNotDefinedInAllContexts(Expression variableX, Expression formulaOnX, RewritingProcess process) {
		Set<Expression> possibleValues = getPossibleValuesForVariableInFormulaAndContext(variableX, formulaOnX, process);
		Expression result = getConditionalSingleValueOrNullIfNotDefinedInAllContexts(variableX, formulaOnX, possibleValues, process);
		return result;
	}

	/**
	 * <pre>
	 * get_conditional_single_value_or_null_if_not_defined_in_all_contexts(X, formula_on_X, possible_values)
	 * 
	 * inputs: Takes a variable X, a formula involving X, and a set of possible determined values for X
	 * (by "determined values", we mean values identified by a constant or free variable).
	 * The formula is assumed to have the following property:
	 * For each assignment to all variables but X, there is at most one value for X (among the given possibilities)
	 * that satisfies the formula.
	 * Returns: a (conditional) value for X guaranteed to be the only value satisfying the formula under its context
	 * or null if, for some context, there is not a single determined value satisfying the formula.
	 * More technically, it returns a conditional value V such that formula_on_X => X = V.
	 * For example, (Y = a and X = b) or (Y = c and X = d) implies X = (if Y = a then b else d).
	 * Note that the case Y = c does not need to be tested if Y != a,
	 * because the original formula makes that true.  
	 * Note also that the algorithm does not detect the case in which the
	 * assumption is violated by some context accepting multiple values of X.
	 * If the assumption is violated in this way, the returned value is undefined.
	 *  
	 * if possible_determined_values is empty
	 *     return null
	 * (first, remaining_possible_values) <- get_first_and_remaining(possible_determined_values)
	 * condition_for_first <- R_complete_normalize(formula_on_X[X/first])
	 * remaining_space <- R_complete_normalize(formula_on_X and not condition_for_first)
	 * if remaining_space is 'false'
	 *     if condition_for_first is 'false'
	 *         return null
	 *     else
	 *         return first
	 * among_remaining <-
	 *     get_conditional_single_value_or_null_if_not_defined_in_all_contexts(X, remaining_space, remaining_possible_values)
	 * if among_remaining is null
	 *     return null
	 * return if condition_for_first then first else among_remaining
	 * </pre>
	 * 
	 * @param variableX
	 *            a variable X
	 * @param formulaOnX
	 *            a formula on X
	 * @param possibleValues
	 *            possible values for X
	 * @param process
	 *            the process in which the rewriting is occurring.
	 * @return a conditional single value for X or null if not defined in all contexts.
	 */
	public static Expression getConditionalSingleValueOrNullIfNotDefinedInAllContexts(Expression variableX, Expression formulaOnX, Set<Expression> possibleValues, RewritingProcess process) {
		Expression result = getConditionalSingleValueOrNullIfNotDefinedInAllContexts(variableX, formulaOnX, possibleValues.iterator(), process);
		return result;
	}

	public static Expression getConditionalSingleValueOrNullIfNotDefinedInAllContexts(Expression variableX, Expression formulaOnX, Iterator<Expression> possibleValues, RewritingProcess process) {
		Expression result = null;
		
		Trace.in("+get_conditional_single_value_or_null_if_not_defined_in_all_contexts({}, {}, {}) under: {}", variableX, formulaOnX, possibleValues, process.getContextualConstraint());
		
		if ( ! possibleValues.hasNext()) {
			Trace.log("if possible_values is empty, return null // X can be any value not forbidden by context, so it is not constrained to a single value");
			result = null;
		}
		else {
			Trace.log("(first, remaining_possible_values) <- get_first_and_remaining(possible_values)");
			Expression first = possibleValues.next();
			Trace.log("// first                    : {}", first);
			
			Trace.log("condition_for_first <- R_complete_normalize(formula_on_X[X/first])");
			Expression conditionForFirst = formulaOnX.replaceAllOccurrences(variableX, first, process);
			conditionForFirst = process.rewrite(CardinalityRewriter.R_complete_normalize, conditionForFirst);
			Trace.log("// condition_for_first: {}, first: {}", conditionForFirst, first);
			Trace.log("remaining_space <- R_complete_normalize(formula_on_X and not condition_for_first)");
			Expression remainingSpace = And.make(formulaOnX, Not.make(conditionForFirst));
			remainingSpace = process.rewrite(CardinalityRewriter.R_complete_normalize, remainingSpace);
			Trace.log("// remaining_space: {}:", remainingSpace, first);
			if (remainingSpace.equals(Expressions.FALSE)) {
				Trace.log("if remaining_space is \"false\"");
				Trace.log("    if condition_for_first is \"false\"");
				Trace.log("        return null");
				Trace.log("    else");
				Trace.log("        return first");
				if (conditionForFirst.equals(Expressions.FALSE)) {
					result = null;
				}
				else {
					result = first;
				}
			}
			else {
				Trace.log("among_remaining <- get_conditional_single_value_or_null_if_not_defined_in_all_contexts(X, remaining_space, remaining_possible_values) under context extended by not(condition_for_first)");
				Expression amongRemaining = getConditionalSingleValueOrNullIfNotDefinedInAllContexts(variableX, remainingSpace, possibleValues, process);
				if (amongRemaining == null) {
					Trace.log("if among_remaining is null");
					Trace.log("    return null");
					result = null;
				}
				else {
					Trace.log("return if condition_for_first then first else among_remaining");
					result = IfThenElse.make(conditionForFirst, first, amongRemaining);
				}				
			}
		}
				
		Trace.out("-get_conditional_single_value_or_null_if_not_defined_in_all_contexts={}", result);
		
		return result;
	}

	public static Set<Expression> extractValuesForXFromEquality(Expression variableX, Expression equality, RewritingProcess process) {
		Set<Expression> result = new LinkedHashSet<Expression>();
		
		boolean containsX = false;
		// Want to handle equalities on more than two variables, e.g.:
		// X = X' = constant
		for (Expression possibleValue : equality.getArguments()) {
			if (possibleValue.equals(variableX)) {
				containsX = true;
			} 
			else {
				result.add(possibleValue);
			}
		}
		
		if (!containsX) {
			result.clear();
		}
		
		return result;
	}

	public static Expression extractValueForXFromFormula(Expression variableX, List<Expression> variablesI, Expression formula, RewritingProcess process) {
		Expression result = null;
		
		for (Expression possibleValue : extractPossibleValuesForXFromFormula(variableX, formula, process)) {
			if (process.isUniquelyNamedConstant(possibleValue)) {
				result = possibleValue;
			} 
			else {
				// if assigned already, then let constants or prior
				// variable assignments take precedence
				if (result == null) {
					// Only assign free variables
					if (isIndependentOf(possibleValue, variablesI, process)) {					
						result = possibleValue;
					}
				}
			}
		}
		
		return result;
	}

	public static Set<Expression> extractPossibleValuesForXFromFormula(Expression variableX, Expression formula, RewritingProcess process) {
		Set<Expression> result = new LinkedHashSet<Expression>();
		
		if (And.isConjunction(formula)) {
			for (Expression conjunct : And.getConjuncts(formula)) {
				result.addAll(extractPossibleValuesForXFromFormula(variableX, conjunct, process));
			}
		} 
		else if (formula.hasFunctor(FunctorConstants.EQUAL)) {
			result = extractValuesForXFromEquality(variableX, formula, process);
		} 
		else if (Or.isDisjunction(formula)) {
			// This logic is to handle expressions of the form:
			// X = X' = person1 or X = X' = person2 or X = X' = person3
			// which, if variableX = X should give me:
			// X' 
			// as its common to all 3 disjuncts.
			boolean first = true;
			for (Expression disjunct : formula.getArguments()) {
				Set<Expression> disjunctResult = extractPossibleValuesForXFromFormula(variableX, disjunct, process);
				if (first) {
					first = false;
					result.addAll(disjunctResult);
				} 
				else {
					result.retainAll(disjunctResult);
					if (result.isEmpty()) {
						break;
					}
				}
			}
		}
		
		return result;
	}

	public static boolean isIndependentOf(Expression alpha, List<Expression> indicesI, RewritingProcess process) {
		boolean result = false;
		
		Set<Expression> alphaFreeVariables            = new LinkedHashSet<Expression>();
		Set<Expression> indexExpressionsFreeVariables = new LinkedHashSet<Expression>();
		
		// Collect the free variables
		alphaFreeVariables.addAll(Expressions.freeVariables(alpha, process));
		indexExpressionsFreeVariables.addAll(indicesI);
		
		// check the intersection
		alphaFreeVariables.retainAll(indexExpressionsFreeVariables);
		if (alphaFreeVariables.size() == 0) {
			// to see if its empty
			result = true; // is independent if intersection if empty
		}
		
		return result;
	}

}
