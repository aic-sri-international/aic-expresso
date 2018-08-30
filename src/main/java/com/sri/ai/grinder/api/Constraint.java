package com.sri.ai.grinder.api;

import static com.sri.ai.expresso.helper.Expressions.FALSE;
import static com.sri.ai.expresso.helper.Expressions.TRUE;
import static com.sri.ai.grinder.library.boole.And.getConjuncts;
import static com.sri.ai.grinder.library.boole.And.isConjunction;
import static com.sri.ai.util.Util.myAssert;
import static com.sri.ai.util.explanation.logging.api.ThreadExplanationLogger.RESULT;
import static com.sri.ai.util.explanation.logging.api.ThreadExplanationLogger.code;
import static com.sri.ai.util.explanation.logging.api.ThreadExplanationLogger.explain;
import static com.sri.ai.util.explanation.logging.api.ThreadExplanationLogger.explanationBlock;

import java.util.List;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.tester.SGDPLLTTester;

/**
 * An {@link Expression} with efficient internal representation
 * for satisfiability.
 * 
 * As of August 2016, only implementations are conjunctive clauses,
 * but eventually it should be expanded to arbitrary formulas.
 * 
 * @author braz
 *
 */
public interface Constraint extends Expression {

	/**
	 * Returns the general theory used in the application.
	 * 
	 * TODO
	 * This method should probably be eliminated.
	 * It used to be "the constraint's theory", but that meaning got blurred when compound constraint theories got introduced,
	 * because then the application theory could be a compound theory, whereas a single-variable constraint
	 * could be specific to just one of the sub-constraint theories.
	 * This caused problems because throughout the code we rely on this method to obtain the application's theory;
	 * when dealing with a single-variable constraint, this would instead give us its specific sub-theory.
	 * We changed it by making the method hold the general application theory even for single-variable constraints.
	 * This solved the problem but rendered the method misleading, since it does not represent a property of the constraint itself.
	 * It is now just a convenience method that spares us the trouble of passing the theory everywhere
	 * but, like stated above, at the cost of being misleading.
	 * 
	 * @return the application's theory.
	 */
	Theory getTheory();
	
	/**
		 * Returns an {@link SGDPLLTTester} representing the conjunction of this constraint and
		 * a given formula, or null if they are contradictory.
		 * <p>
		 * At this point, the formula should be either a literal, or a {@link Constraint}.
		 * <p>
		 * Extensions may want to override this method if there are more efficient ways
		 * of conjoining with (certain types of) constraints than simply treating them as a formula
		 * 
		 * @param formula the formula to be conjoined.
		 * @param context the context
		 * @return the application result or <code>null</code> if contradiction.
		 */
		default Constraint conjoin(Expression formula, Context context) {
			
			return explanationBlock("Constraint.conjoin of ", this, " with formula ", formula, " under ", context, code( () -> {
			
			myAssert(
					() -> isValidConjoinant(formula, context),
					() -> this.getClass() + " currently only supports conjoining with literals, conjunctive clauses, and constraints, but received " + formula);
			
			Constraint result;
		
			if (isContradiction() || isContradiction(formula)) {
				explain("This constraint or conjoined formula is contradictory, so result is a contradiction.");
				result = makeContradiction();
			}
			else if (formula.equals(TRUE)) {
				result = this;
			}
	//      Warning: tempting, but inefficient because equals(TRUE) forces constraint's expression to be generated, which may be expensive if it's not being generated anywhere else. If going this route, may make sense to define a isTautology method that tests the same thing without generating expression
	//		else if (formula instanceof Constraint && this.equals(TRUE)) {
	//			result = (Constraint) formula;
	//		}
			else if (formula instanceof Constraint || isConjunction(formula)) {
				explain("Formula is a ", (formula instanceof Constraint? "Constraint" : "conjunctive clause"), " so we will use conjoinWithConjunctiveClause.");
				result = conjoinWithConjunctiveClause(formula, context); // for now, all Constraints are conjunctions. This will probably change in the future.
			}
			else {
				explain("Formula is a literal, so we will use ", getClass().getSimpleName(), ".conjoinWithLiteral.");
				result = conjoinWithLiteral(formula, context);
			}
	
			return result;
			
			}), "Results in ", RESULT);
		}

	/**
	 * Returns an {@link SGDPLLTTester} representing the conjunction of this constraint and
	 * a given literal, or null if they are contradictory.
	 * <p>
	 * At this point, the formula should be either a literal, or a {@link Constraint}.
	 * 
	 * @param literal the literal to be conjoined.
	 * @param context the context
	 * @return the application result or <code>null</code> if contradiction.
	 */
	Constraint conjoinWithLiteral(Expression literal, Context context);
	
	/**
	 * Tests whether an expression is a contradiction (that is, equal to false),
	 * by first checking if it is a {@link Constraint} and using {@link #isContradiction()},
	 * and only if that fails, using <code>equals(FALSE)</code>.
	 * This avoids the unnecessary generation of an {@link Expression} form for {@link Constraint}s
	 * that only make it under demand.
	 * @param formula
	 * @return
	 */
	default boolean isContradiction(Expression formula) {
		return (formula instanceof Constraint && ((Constraint)formula).isContradiction()) || formula.equals(FALSE);
	}

	/**
	 * @param formula
	 * @param context
	 * @return
	 */
	default boolean isValidConjoinant(Expression formula, Context context) {
		boolean result =
				formula == null
				|| formula instanceof Constraint
				|| getTheory().isConjunctiveClause(formula, context);
		return result;
	}

	/**
	 * Returns the result of conjoining this constraint with all literal conjuncts of a given conjunctive clause
	 * (note that if <code>conjunction</code> is not an application of <code>and</code>,
	 * it will be considered a unit conjunction with itself the only conjunct.
	 * @param conjunctiveClause
	 * @param context
	 * @return the result of conjoining this constraint with all conjuncts of a given conjunction
	 */
	default Constraint conjoinWithConjunctiveClause(Expression conjunctiveClause, Context context) {
		Constraint result;

		List<Expression> conjuncts = getConjuncts(conjunctiveClause);
		if (conjuncts.size() == 1) { // this is necessary to avoid an infinite loop
			result = conjoinWithLiteral(conjuncts.get(0), context);
		}
		else {
			result = this;
			for (Expression literal : conjuncts) {
				result = result.conjoin(literal, context);
				if (result == null) {
					break;
				}
			}
		}

		return result;
	}
	
	/**
	 * Returns an expression to which the given variable is bound to
	 * under this constraint, if there is such a value and it can be determined by the implementation.
	 * @param variable
	 * @return an expression to which variable is bound, or null if there is no such value
	 */
	Expression binding(Expression variable);
	
	/**
	 * Indicates whether constraint is contradiction.
	 * @return
	 */
	boolean isContradiction();
	
	/**
	 * Make contradictory version of this constraint;
	 * this means the contradiction is of the same "type" as this contradiction
	 * (for example, same theory, among other details),
	 * but is a contradiction.
	 * @return
	 */
	Constraint makeContradiction();

//	/**
//	 * Returns, in time constant in the size of the constraint,
//	 * a pair of {@link Constraint}s whose conjunction is equivalent to this constraint,
//	 * such that the given variables only occur in the second one.
//	 * <p>
//	 * Note that there may be multiple such decompositions,
//	 * and that <code>Pair(new {@link ExpressionConstraint}(TRUE), this)</code> is such a decomposition.
//	 * Implementations must seek to minimize the size of the second constraint while keeping
//	 * time constant in the size of the original constraint.
//	 * <p>
//	 * The point of this operation is to isolate the variables in the second constraint,
//	 * while preserving as much internal efficient representation about the remaining variables
//	 * in the first constraint.
//	 * <p>
//	 * For example, suppose we have a complex, efficiently represented constraint
//	 * <code>C</code> on variables <code>X,Y</code>, which gets conjoined with an
//	 * also efficiently represented constraint <code>C'</code> in <code>Z</code>
//	 * (which could involve <code>X</code> or <code>Y</code> or both),
//	 * producing a new constraint <code>C''</code>.
//	 * Ideally, the internal representation of <code>C''</code> preserves
//	 * the original efficient representations.
//	 * If now we want to compute, say, <code>there exists Z : C''</code>
//	 * @return
//	 */
//	PairOf<Constraint> decomposeInConstantTime(Collection<Expression> variables);
	
//	
//	/**
//	 * Given a sub-set of supported indices, projects the constraint onto the remaining ones.
//	 * Resulting constraint still supports all original indices.
//	 * Default implementation uses symbolic satisfiability through {@link SGDPLLT}.
//	 * Specific constraint implementations will typically have more efficient ways to do it.
//	 */
//	default Constraint project(Collection<Expression> eliminatedIndices, Context context) {
//		Expression resultExpression =
//				SymbolicSolver.solve(
//						new Conjunction(),
//						eliminatedIndices,
//						condition,
//						body,
//						getTheory().makeSingleVariableConstraint(null),
//						context);
//		// note that solvers should be aware that their input or part of their input may be a Constraint, and take advantage of the internal representations already present in them, instead of simply converting them to an Expression and redoing all the work.
//		Collection<Expression> remainingSupportedIndices = Util.subtract(getSupportedIndices(), eliminatedIndices);
//		Constraint result = ExpressionConstraint.wrap(getTheory(), remainingSupportedIndices, resultExpression);
//		return result;
//	}
}