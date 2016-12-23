package com.sri.ai.grinder.sgdpllt.theory.base;

import static com.sri.ai.util.Util.check;
import static com.sri.ai.util.Util.forAll;

import java.util.Collection;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.Type;
import com.sri.ai.grinder.helper.GrinderUtil;
import com.sri.ai.grinder.sgdpllt.api.Context;
import com.sri.ai.grinder.sgdpllt.core.constraint.AbstractTheory;

public abstract class AbstractTheoryWithBinaryAtoms extends AbstractTheory {

	/**
	 * Indicates whether the theory can safely assume that all applications of its theory functors are atoms in it,
	 * regardless of their argument types (this spares the theory to do the checking).
	 */
	protected boolean assumeAllTheoryFunctorApplicationsAreAtomsInThisTheory;

	/**
	 * The strings of the theory functors
	 */
	Collection<String> theoryFunctors; // NOTE: package protected so AbstractTheoryWithBinaryAtomsTestingSupport can access

	/**
	 * Constructor taking the theory's functor strings,
	 * a boolean indicating whether any application of this functors, regardless of their arguments types,
	 * are to be considered atoms in this theory (for efficiency purposes),
	 * and a simplifier for these functions.
	 * @param theoryFunctors
	 * @param assumeAllTheoryFunctorApplicationsAreAtomsInThisTheory
	 * @param simplifier
	 */
	public AbstractTheoryWithBinaryAtoms(
			Collection<String> theoryFunctors,
			boolean assumeAllTheoryFunctorApplicationsAreAtomsInThisTheory) {
		super();
		this.assumeAllTheoryFunctorApplicationsAreAtomsInThisTheory = assumeAllTheoryFunctorApplicationsAreAtomsInThisTheory;
		this.theoryFunctors = theoryFunctors;
	}

	/**
	 * Indicates whether an argument to the theory functors is a valid argument to form a literal in this theory.
	 * By default, the type of theory functor arguments is only tested by this method if
	 * {@link #assumeAllTheoryFunctorApplicationsAreAtomsInThisTheory} is true.
	 * On the other hand, {@link #isSuitableFor(Expression, Type)} always uses this test
	 * for deciding whether this theory is suitable for a variable (which is passed as the argument here).
	 * @param expression
	 * @param type TODO
	 * @param context TODO
	 * @return
	 */
	protected abstract boolean isValidArgument(Expression expression, Type type, Context context);

	/**
	 * Indicates whether an expression is a function application of one of the theory functors.
	 * @param expression
	 * @return
	 */
	protected boolean isApplicationOfTheoryFunctor(Expression expression) {
		boolean result = expression.getFunctor() != null && theoryFunctors.contains(expression.getFunctor().toString());
		return result;
	}

	@Override
	public boolean isInterpretedInThisTheoryBesidesBooleanConnectives(Expression expression) {
		boolean result = isApplicationOfTheoryFunctor(expression) || theoryFunctors.contains(expression.toString()); 
		return result;
	}

	/**
	 * Implements decision of whether an expression is a non-trivial atom by checking
	 * if it is a function application of one of the theory functors and,
	 * if {@link #assumeAllTheoryFunctorApplicationsAreAtomsInThisTheory} is true,
	 * whether its arguments are valid according to {@link #isValidArgument(Expression, Type, Context)}.
	 */
	@Override
	public boolean isNonConstantAtom(Expression expression, Context context) {
		boolean result;
	
		boolean hasTheoryFunctor = isApplicationOfTheoryFunctor(expression);
		
		if (assumeAllTheoryFunctorApplicationsAreAtomsInThisTheory) {
			result = hasTheoryFunctor;
		}
		else {
			// the following is good, but expensive
			result = hasTheoryFunctor
					&&
					forAll(expression.getArguments(),
							e -> {
								Expression typeExpression = GrinderUtil.getType(e, context);
								check(() -> typeExpression != null, () -> e + " has undefined type");
								String typeName = typeExpression.toString();
								Type eType = context.getType(typeName);
								return isValidArgument(e, eType, context);
							});
		}
		
		return result;
	}
}