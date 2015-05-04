package com.sri.ai.grinder.plaindpll.theory;

import static com.sri.ai.expresso.helper.Expressions.apply;
import static com.sri.ai.expresso.helper.Expressions.equalArgumentsInSameOrder;
import static com.sri.ai.grinder.library.FunctorConstants.DISEQUALITY;
import static com.sri.ai.grinder.library.FunctorConstants.EQUALITY;
import static com.sri.ai.grinder.library.FunctorConstants.GREATER_THAN;
import static com.sri.ai.grinder.library.FunctorConstants.GREATER_THAN_OR_EQUAL_TO;
import static com.sri.ai.grinder.library.FunctorConstants.LESS_THAN;
import static com.sri.ai.grinder.library.FunctorConstants.LESS_THAN_OR_EQUAL_TO;
import static com.sri.ai.util.Util.getFirst;
import static com.sri.ai.util.Util.getFirstSatisfyingPredicateOrNull;
import static com.sri.ai.util.Util.join;
import static com.sri.ai.util.Util.list;
import static com.sri.ai.util.Util.myAssert;

import java.util.Arrays;
import java.util.Collection;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Set;

import com.google.common.base.Function;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.library.Equality;
import com.sri.ai.grinder.library.FunctorConstants;
import com.sri.ai.grinder.plaindpll.api.Constraint;
import com.sri.ai.grinder.plaindpll.core.Contradiction;
import com.sri.ai.util.Util;

/**
 * An implementation of {@link NonEqualitiesForSingleTerm} in which the constraints are inequalities and disequalities.
 * @author braz
 *
 */
@SuppressWarnings("serial")
public class InequalitiesAndDisequalitiesConstraintForSingleVariable extends AbstractNonEqualitiesConstraintForSingleVariable {

	private DisequalitiesConstraintForSingleVariable disequalities; // the basis for this constraint, which only adds the inequalities. This still qualifies this class as keeping its "own representation" because its operation is not simply based on a translation of splitters and solutions.
	private Collection<Expression> lowerBounds; // known lower bounds (<=) for variable; if empty, we assume type lower separator.
	private Collection<Expression> upperBounds; // known upper bounds (>=) for variable; if empty, we assume type upper separator.
	private Collection<Expression> strictLowerBounds; // known strict lower bounds (<) for variable
	private Collection<Expression> strictUpperBounds; // known strict upper bounds (>) for variable
	
	public InequalitiesAndDisequalitiesConstraintForSingleVariable(Expression variable, EqualityConstraintTheory theory, Collection<Expression> supportedIndices) {
		super(variable, theory, supportedIndices);
		disequalities = new DisequalitiesConstraintForSingleVariable(cachedInnerExpression, null, supportedIndices);
	}

	@SuppressWarnings("unchecked")
	@Override
	public Expression pickSplitterGivenExternalConstraint(Collection<Expression> indicesSubSet, Constraint externalConstraint, RewritingProcess process) {
		Expression splitter = null;
		
		// lower separator needs to be unique
		if (lowerBounds.size() > 1) {
			Iterator<Expression> lowerBoundsIterator = lowerBounds.iterator();
			Expression first  = lowerBoundsIterator.next();
			Expression second = lowerBoundsIterator.next();
			splitter = apply(LESS_THAN_OR_EQUAL_TO, first, second); // order is irrelevant, we simply want them to be ordered somehow
			return splitter; // we don't like to use early return statements, but here it will make the code cleaner and faster.
		}
		
		if (upperBounds.size() > 1) {
			Iterator<Expression> upperBoundsIterator = lowerBounds.iterator();
			Expression first  = upperBoundsIterator.next();
			Expression second = upperBoundsIterator.next();
			splitter = apply(LESS_THAN_OR_EQUAL_TO, first, second); // order is irrelevant, we simply want them to be ordered somehow
			return splitter;
		}
		
		splitter = disequalities.pickSplitterGivenExternalConstraint(indicesSubSet, externalConstraint, process);
		if (splitter != null) {
			return splitter;
		}

		// At this point lowerBounds has at most one element each.
		// If they have a lower separator, it must be constrained to be less than disequals and upper bounds.
		// If they don't have a lower separator, it is the type separator and we don't need to check the following constraints, as they are implicitly true.
		// The analogous facts hold for upper bounds.

		if (lowerBounds.size() == 1) {
			Expression min = getFirst(lowerBounds);
			for (Collection<Expression> terms : list(disequalities.getDisequals(), upperBounds)) {
				splitter = getSplitterOfTermNotConstrainedToSatisfyOpenBound(terms, GREATER_THAN, min, externalConstraint, process);
				if (splitter != null) {
					return splitter;
				}
			}
		}

		if (upperBounds.size() == 1) {
			Expression max = getFirst(upperBounds);
			for (Collection<Expression> terms : list(disequalities.getDisequals(), lowerBounds)) {
				splitter = getSplitterOfTermNotConstrainedToSatisfyOpenBound(terms, LESS_THAN, max, externalConstraint, process);
				if (splitter != null) {
					return splitter;
				}
			}
		}
		
		return splitter;
	}

	private Expression getSplitterOfTermNotConstrainedToSatisfyOpenBound(Collection<Expression> terms, String relation, Expression bound, Constraint externalConstraint, RewritingProcess process) {
		Expression splitter;
		Expression termNotConstrainedToSatisfyOpenBound = getTermNotConstrainedToSatisfyOpenBoundOrNull(terms, relation, bound, externalConstraint, process);
		if (termNotConstrainedToSatisfyOpenBound != null) {
			splitter = apply(relation, bound, termNotConstrainedToSatisfyOpenBound);
		}
		else {
			splitter = null;
		}
		return splitter;
	}

	private Expression getTermNotConstrainedToSatisfyOpenBoundOrNull(Collection<Expression> terms, String relation, Expression bound, Constraint externalConstraint, RewritingProcess process) {
		Expression termNotConstrainedToSatisfyOpenBound = 
				getFirstSatisfyingPredicateOrNull(
						terms,
						d -> ! externalConstraint.directlyImpliesLiteral(apply(relation, bound, d), process));
		return termNotConstrainedToSatisfyOpenBound;
	}

	@Override
	public void informDestructively(Expression literal, RewritingProcess process) {
		// TODO Auto-generated method stub
		
	}

	@Override
	public void incorporateDestructively(boolean splitterSign, Expression splitter, Constraint externalConstraint, RewritingProcess process) {
		// TODO Auto-generated method stub
		
	}

	@Override
	public void updateRepresentativesDestructively(Function<Expression, Expression> getRepresentative, NonEqualitiesConstraint externalConstraint, RewritingProcess process) {
		// TODO Auto-generated method stub
		
	}

	@Override
	public boolean directlyImpliesDisequalityOfVariableAnd(Expression term, RewritingProcess process) {
		// TODO Auto-generated method stub
		return false;
	}

	@Override
	public List<Expression> getSplittersToBeSatisfied() {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public List<Expression> getSplittersToBeNotSatisfied() {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public Expression modelCount(Collection<Expression> indicesSubSet, RewritingProcess process) {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public Expression normalizeSplitterGivenConstraint(Expression splitter, RewritingProcess process) {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public AbstractNonEqualitiesConstraintForSingleVariable clone() {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	protected Expression computeInnerExpression() {
		// TODO Auto-generated method stub
		return null;
	}
	
	/**
	 * A result of {@link InequalitiesAndDisequalitiesConstraintForSingleVariable#threeWayLocalInference(Expression[])}
	 * indicating which literals must be removed and which ones must be added given three literals in the
	 * equalities and inequalities theory (note that that methods can also throw a {@link Contradiction}.
	 * @author braz
	 *
	 */
	public static class LocalInferenceConclusion {
		public Expression[] originalLiterals;
		public Collection<Expression> addedLiterals;
		public boolean[] keep;
		public LocalInferenceConclusion(Expression[] originalLiterals, boolean[] keep, Collection<Expression> addedLiterals) {
			this.originalLiterals = originalLiterals;
			this.addedLiterals = addedLiterals;
			this.keep = keep;
		}

		@Override
		public String toString() {
			List<Expression> removed = new LinkedList<Expression>();
			for (int i = 0 ; i != keep.length; i++) {
				if (!keep[i]) {
					removed.add(originalLiterals[i]);
				}
			}
			List<String> result = list();
			if ( ! addedLiterals.isEmpty()) {
				result.add("add " + Util.join(addedLiterals));
			}
			if ( ! removed.isEmpty()) {
				result.add("remove " + Util.join(removed));
			}
			return join(result);
		}

		@Override
		public boolean equals(Object another) {
			try {
				LocalInferenceConclusion anotherConclusion = (LocalInferenceConclusion) another;
				boolean result = 
						addedLiterals.equals(anotherConclusion.addedLiterals) && 
						Arrays.equals(keep, anotherConclusion.keep);
				return result;
			}
			catch (ClassCastException e) {
				return false;
			}
		}
		
		@Override
		public int hashCode() {
			return toString().hashCode();
		}
	}
	
	/**
	 * Given two literals in the equalities and inequalities theory,
	 * indicates a simplification {@link #LocalInferenceConclusion} by indicating which ones can be removed and which literals need to be added,
	 * returns null if no simplification known to this method applies, and throws a {@link Contradiction} in case the conjunction of the literals is contradictory.
	 * This method assumes the literals are not trivial (such as 3 < 10, or 1000 < 2).
	 * <p>
	 * The simplifications known to this method are summarized by these rules:
	 * <pre>
Splitter x < y
Literals:
x < y: don't add splitter
x <= y: remove this, add splitter
x != y: remove this, add splitter
x >= y: inconsistent
x > y: inconsistent

Analogous for x > y.

Splitter x <= y
Literals:
x < y: don't add splitter
x <= y: don't add splitter
x != y: remove this, add x < y, don't add splitter
x >= y: remove this, add x = y, don't add splitter
x > y: inconsistent

Analogous for x >= y.

Splitter x != y
Literals:
x < y: don't add splitter
x <= y: remove this, add x < y, don't add splitter
x != y: redundant
x >= y: remove this, add x > y, don't add splitter
x > y: don't add splitter
	 * </pre>
	 * <p>
	 * Important: the literals order does not matter; all permutations are checked until a conclusion can be drawn.
	 * @param literals
	 * @return
	 */
	public static LocalInferenceConclusion twoWayLocalInference(Expression[] literals) {
		
		myAssert(() -> literals.length == 2, () -> "Requires two literals exactly.");
		
		LocalInferenceConclusion result = null;
		
		Expression splitter = literals[0];  // renaming to make it easier to relate to documentation
		Expression theOther = literals[1];
		
		if (equalArgumentsInSameOrder(splitter, theOther)) {
			// good
		}
		else if (equalTwoArgumentsInSwappedOrder(splitter, theOther)) {
			theOther = flip(theOther);
		}
		else {
			return null; // this method only applies to two literals on the same arguments.
		}
		
		// now they both apply to the same two arguments x and y, in the same order.
		
		Expression x = splitter.get(0); // renaming to make it easier to relate to documentation
		Expression y = splitter.get(1);

		boolean[] keepOriginalLiterals;
		List<Expression> addedLiterals;

		if (splitter.hasFunctor(LESS_THAN)) {  // x < y
			switch (theOther.getFunctor().toString()) {
			case LESS_THAN: // x < y, x < y
				keepOriginalLiterals = new boolean[]{false, true}; // keep the other only
				addedLiterals = list();
				result = new LocalInferenceConclusion(literals, keepOriginalLiterals, addedLiterals);
				break;
			case LESS_THAN_OR_EQUAL_TO: // x < y, x <= y 
				keepOriginalLiterals = new boolean[]{true, false}; // splitter is stronger, keep it alone
				addedLiterals = list();
				result = new LocalInferenceConclusion(literals, keepOriginalLiterals, addedLiterals);
				break;
			case DISEQUALITY: // x < y, x != y
				keepOriginalLiterals = new boolean[]{true, false}; // splitter is stronger, keep it alone
				addedLiterals = list();
				result = new LocalInferenceConclusion(literals, keepOriginalLiterals, addedLiterals);
				break;
			case GREATER_THAN_OR_EQUAL_TO: // x < y, x >= y 
				throw new Contradiction();
			case GREATER_THAN: // x < y, x > y
				throw new Contradiction();
			default:
				throw new Error("Should only see disequalities and inequalities at this point, but got " + theOther);
			}
		}
		else if (splitter.hasFunctor(LESS_THAN_OR_EQUAL_TO)) { // x <= y
			switch (theOther.getFunctor().toString()) {
			case LESS_THAN: // x <= y, x < y
				keepOriginalLiterals = new boolean[]{false, true}; // the other is either the same or stronger, keep it alone
				addedLiterals = list();
				result = new LocalInferenceConclusion(literals, keepOriginalLiterals, addedLiterals);
				break;
			case LESS_THAN_OR_EQUAL_TO: // x <= y, x <= y 
				keepOriginalLiterals = new boolean[]{false, true}; // the other is either the same or stronger, keep it alone
				addedLiterals = list();
				result = new LocalInferenceConclusion(literals, keepOriginalLiterals, addedLiterals);
				break;
			case DISEQUALITY: // x <= y, x != y
				keepOriginalLiterals = new boolean[]{false, false}; // replace both by the equivalent x < y
				addedLiterals = list(apply(LESS_THAN, x, y));
				result = new LocalInferenceConclusion(literals, keepOriginalLiterals, addedLiterals);
				break;
			case GREATER_THAN_OR_EQUAL_TO: // x <= y, x >= y 
				keepOriginalLiterals = new boolean[]{false, false}; // replace both by the equivalent x = y
				addedLiterals = list(apply(EQUALITY, x, y));
				result = new LocalInferenceConclusion(literals, keepOriginalLiterals, addedLiterals);
				break;
			case GREATER_THAN: // x <= y, x > y
				throw new Contradiction();
			default:
				throw new Error("Should only see disequalities and inequalities at this point, but got " + theOther);
			}
		}
		else if (splitter.hasFunctor(DISEQUALITY)) { // x != y
			switch (theOther.getFunctor().toString()) {
			case LESS_THAN: // x != y, x < y
				keepOriginalLiterals = new boolean[]{false, true}; // the other is stronger, keep it alone
				addedLiterals = list();
				result = new LocalInferenceConclusion(literals, keepOriginalLiterals, addedLiterals);
				break;
			case LESS_THAN_OR_EQUAL_TO: // x != y, x <= y 
				keepOriginalLiterals = new boolean[]{false, false}; // replace both by the equivalent x < y
				addedLiterals = list(apply(LESS_THAN, x, y));
				result = new LocalInferenceConclusion(literals, keepOriginalLiterals, addedLiterals);
				break;
			case DISEQUALITY: // x != y, x != y
				keepOriginalLiterals = new boolean[]{false, true}; // keep the other alone, they are equivalent.
				addedLiterals = list();
				result = new LocalInferenceConclusion(literals, keepOriginalLiterals, addedLiterals);
				break;
			case GREATER_THAN_OR_EQUAL_TO: // x != y, x >= y 
				keepOriginalLiterals = new boolean[]{false, true}; // the other is stronger, keep it alone
				addedLiterals = list();
				result = new LocalInferenceConclusion(literals, keepOriginalLiterals, addedLiterals);
				break;
			case GREATER_THAN: // x != y, x > y
				keepOriginalLiterals = new boolean[]{false, false}; // replace both by the equivalent x > y
				addedLiterals = list(apply(GREATER_THAN, x, y));
				result = new LocalInferenceConclusion(literals, keepOriginalLiterals, addedLiterals);
				break;
			default:
				throw new Error("Should only see disequalities and inequalities at this point, but got " + theOther);
			}
		}
		else if (isDecreasingInequality(splitter)) { // x > y, or x >= y
			// simply flip the splitter to fall under above cases
			Expression flippedSplitter = flip(splitter);
			result = twoWayLocalInference(new Expression[]{flippedSplitter, theOther});
		}
		
		return result;
	}

	/**
	 * Given three literals in the equalities and inequalities theory,
	 * indicates a simplification {@link #LocalInferenceConclusion} by indicating which ones can be removed and which literals need to be added,
	 * returns null if no simplification known to this method applies, and throws a {@link Contradiction} in case the conjunction of the literals is contradictory.
	 * This method assumes the literals are not trivial (such as 3 < 10, or 1000 < 2).
	 * <p>
	 * The simplifications known to this method are summarized by these rules:
	 * <ul>
	 * <li> x <. b <. y and x != y --> remove x != y
	 * <li> x <. b <. y and x <. y --> remove x <. y
	 * <li> x <. b <. y and x >. y --> inconsistency if at least one strict
	 * <li> x <. b <. y and x >= y --> x = b = y if all non-strict
	 * </ul>
	 * where <code><.</code> stand for either <code><</code> or <code><=</code>.
	 * <p>
	 * Important: the literals order does not matter; all permutations are checked until a conclusion can be drawn.
	 * @param literals
	 * @return
	 */
	public static LocalInferenceConclusion threeWayLocalInference(Expression[] literals) {
		
		myAssert(() -> literals.length == 3, () -> "Requires three literals exactly.");
		
		LocalInferenceConclusion result = null;
		
		Expression[] increasingLiterals = new Expression[literals.length];
		for (int i = 0; i != increasingLiterals.length; i++) {
			increasingLiterals[i] = flipIfNeededToGetIncreasing(literals[i]);
		}
		
		for (int[] permutation : permutations) {
			if (result != null) { // we can leave as soon as we find an inference conclusion
				break;
			}
			Expression l0 = increasingLiterals[permutation[0]];
			Expression l1 = increasingLiterals[permutation[1]];
			Expression l2 = increasingLiterals[permutation[2]];
			Separation separation;
			if ((separation = twoValuesSeparatedByAThird(l0, l1)) != null) { // eg, x < b and b <= y
				boolean l2RelatesLeastAndGreatestInSeparation = l2.get(0).equals(separation.least) && l2.get(1).equals(separation.greatest);
				if (l2RelatesLeastAndGreatestInSeparation) {
					if (isInequality(l2)     // x <. b, b <. y, and x <. y, so last one is redundant
							|| (l2.hasFunctor(DISEQUALITY) && separation.strict)) { // x < b, b < y, and x != y. Because disequalities do not admit x = y, last one is redundant too.  
						boolean[] keepOriginalLiterals = invert(permutation, true, true, false);
						List<Expression> addedLiterals = list();
						result = new LocalInferenceConclusion(literals, keepOriginalLiterals, addedLiterals);
					}
					else if (l2.hasFunctor(DISEQUALITY)) {
						if (! separation.strict) { // x <= b, b <= y, and x != y. Because disequalities admit x = y, last one is NOT redundant.
							boolean[] keepOriginalLiterals = invert(permutation, true, true, true);
							List<Expression> addedLiterals = list();
							result = new LocalInferenceConclusion(literals, keepOriginalLiterals, addedLiterals);
						}
					}
					else {
						throw new Error("Literal must be inequality or disequality, but was " + l2);
					}
				}
				else {
					boolean l2RelatesGreatestAndLeastInSeparation = l2.get(0).equals(separation.greatest) && l2.get(1).equals(separation.least);
					if (l2RelatesGreatestAndLeastInSeparation) {
						if (isInequality(l2)) {
							if (isStrictInequality(l2) || separation.strict) {  // x <. b, b <. y, but y .> x, with some strict inequality in there
								throw new Contradiction();
							}
							else {
								boolean[] keepOriginalLiterals = invert(permutation, false, false, false);
								Collection<Expression> addedLiterals = list(Equality.make(separation.least, separation.separator), Equality.make(separation.separator, separation.greatest));
								result = new LocalInferenceConclusion(literals, keepOriginalLiterals, addedLiterals); // x <= b, b <= y, but y >= x, all literal can be replaced by equalities x = y and y = b
							}
						}
						else if (l2.hasFunctor(DISEQUALITY) && separation.strict) {
							boolean[] keepOriginalLiterals = invert(permutation, true, true, false);
							List<Expression> addedLiterals = list();
							result = new LocalInferenceConclusion(literals, keepOriginalLiterals, addedLiterals); // x <. b, b <. y for one of them strict, and y != x, third one is redundant
						}
						else {
							throw new Error("Literal must be inequality or disequality, but was " + l2);
						}
					}
					else {
						// keep them all
					}
				}
			}
		}
		return result;
	}
	
	private static class Separation {
		public Expression least;
		public Expression separator;
		public Expression greatest;
		public boolean    strict;
		public Separation(Expression least, Expression separator, Expression greatest, boolean strict) {
			super();
			this.least = least;
			this.separator = separator;
			this.greatest = greatest;
			this.strict = strict;
		}
		public String toString() {
			return Util.join(list(least, separator, greatest)) + (strict? "strict" : "");
		}
	}

	/**
	 * Assumes given literals are increasing,
	 * and detects whether they are of the form x <. b and b <. y (or the permutation),
	 * and returns a {@link InequalitiesAndDisequalitiesConstraintForSingleVariable#Separation}
	 * indicating x, y, and whether the two inequalities combined are strict.
	 * @param literal1
	 * @param literal2
	 * @return
	 */
	public static Separation twoValuesSeparatedByAThird(final Expression literal1, final Expression literal2) {
		Separation result;
		
		if ( ! isInequality(literal1) || ! isInequality(literal2)) { // both need to be inequalities
			result = null;
		}
		else {
			myAssert(() -> isIncreasing(literal1) && isIncreasing(literal2), () -> "Inequalities are required to be increasing ones.");
			
			Expression first;
			Expression second;
			boolean noSeparator;

			if (literal1.get(1).equals(literal2.get(0))) {
				first  = literal1;
				second = literal2;
				noSeparator = false;
			}
			else if (literal1.get(0).equals(literal2.get(1))) {
				first  = literal2;
				second = literal1;
				noSeparator = false;
			}
			else {
				first = null;
				second = null;
				noSeparator = true;
			}
			
			if (noSeparator) {
				result = null;
			}
			else {
				boolean strict = isStrictInequality(literal1) || isStrictInequality(literal2);
				result = new Separation(first.get(0), first.get(1), second.get(1), strict);
			}
		}
			
		return result;
	}
	
	/**
	 * Takes a literal and return an equivalent one guaranteed to be an increasing inequality if the original is an inequality. 
	 * @param literal
	 * @return
	 */
	public static Expression flipIfNeededToGetIncreasing(Expression literal) {
		if (!isInequality(literal) || isIncreasing(literal)) {
			return literal;
		}
		else {
			return flip(literal);
		}
	}
	
	public static boolean haveOppositeDirections(Expression inequalityLiteral1, Expression inequalityLiteral2) {
		boolean result = ! haveSameDirection(inequalityLiteral1, inequalityLiteral2);
		return result;
	}

	public static boolean haveSameDirection(Expression inequalityLiteral1, Expression inequalityLiteral2) {
		boolean result = isIncreasing(inequalityLiteral1) == isIncreasing(inequalityLiteral2);
		return result;
	}

	private static boolean isIncreasing(Expression inequalityLiteral) {
		boolean result = inequalityLiteral.getFunctor().equals(LESS_THAN) || inequalityLiteral.getFunctor().equals(LESS_THAN_OR_EQUAL_TO);
		return result;
	}
	
	private static boolean isDecreasing(Expression inequalityLiteral) {
		boolean result = inequalityLiteral.getFunctor().equals(GREATER_THAN) || inequalityLiteral.getFunctor().equals(GREATER_THAN_OR_EQUAL_TO);
		return result;
	}
	
	public static boolean isInequality(Expression literal) {
		boolean result = inequalityFunctors.contains(literal.getFunctor().toString());
		return result;
	}
	
	private static boolean isIncreasingInequality(Expression literal) {
		return isInequality(literal) && isIncreasing(literal);
	}

	private static boolean isDecreasingInequality(Expression literal) {
		return isInequality(literal) && isDecreasing(literal);
	}

	public static boolean isInequalityOrEqualTo(Expression literal) {
		boolean result = inequalityOrEqualToFunctors.contains(literal.getFunctor().toString());
		return result;
	}
	
	public static boolean isStrictInequality(Expression literal) {
		boolean result = isInequality(literal) && ! isInequalityOrEqualTo(literal);
		return result;
	}
	
	public static String inequalityFunctorNegation(Expression inequalityFunctor) {
		return inequalityFunctorNegations.get(inequalityFunctor.toString());
	}
	
	public static String functorFlip(Expression inequalityFunctor) {
		return functorFlips.get(inequalityFunctor.toString());
	}
	
	public static Expression flip(Expression literal) {
		Expression result = apply(functorFlip(literal.getFunctor()), literal.get(1), literal.get(0));
		return result;
	}
	
	private static int[][] permutations = {
		{0, 1, 2},
		{0, 2, 1},
		{1, 0, 2},
		{1, 2, 0},
		{2, 0, 1},
		{2, 1, 0},
	};
	
	private static boolean equalTwoArgumentsInSwappedOrder(Expression literal1, Expression literal2) {
		boolean result =
				literal1.numberOfArguments() == 2 && literal2.numberOfArguments() == 2
				&& literal1.get(0).equals(literal2.get(1)) && literal1.get(1).equals(literal2.get(0));
		return result;
	}

	public static boolean[] permute(int[] permutation, boolean... array) {
		boolean[] result = new boolean[array.length];
		for (int i = 0; i != array.length; i++) {
			result[i] = array[permutation[i]];
		}
		return result;
	}
	
	public static boolean[] invert(int[] permutation, boolean... array) {
		boolean[] result = new boolean[array.length];
		for (int i = 0; i != array.length; i++) {
			result[permutation[i]] = array[i];
		}
		return result;
	}
	
	private static Set<String> inequalityFunctors = Util.set(LESS_THAN, LESS_THAN_OR_EQUAL_TO, GREATER_THAN, GREATER_THAN_OR_EQUAL_TO);
	private static Set<String> inequalityOrEqualToFunctors = Util.set(LESS_THAN_OR_EQUAL_TO, GREATER_THAN_OR_EQUAL_TO);
	private static Map<String, String> inequalityFunctorNegations = Util.map(
			LESS_THAN,                GREATER_THAN_OR_EQUAL_TO,
			LESS_THAN_OR_EQUAL_TO,    GREATER_THAN,
			GREATER_THAN,             LESS_THAN_OR_EQUAL_TO,
			GREATER_THAN_OR_EQUAL_TO, LESS_THAN);
	private static Map<String, String> functorFlips = Util.map(
			DISEQUALITY,              DISEQUALITY,
			LESS_THAN,                GREATER_THAN,
			LESS_THAN_OR_EQUAL_TO,    GREATER_THAN_OR_EQUAL_TO,
			GREATER_THAN,             LESS_THAN,
			GREATER_THAN_OR_EQUAL_TO, LESS_THAN_OR_EQUAL_TO);
}

