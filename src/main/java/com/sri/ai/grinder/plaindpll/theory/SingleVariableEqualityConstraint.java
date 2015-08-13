package com.sri.ai.grinder.plaindpll.theory;

import static com.sri.ai.expresso.helper.Expressions.FALSE;
import static com.sri.ai.expresso.helper.Expressions.ONE;
import static com.sri.ai.expresso.helper.Expressions.TRUE;
import static com.sri.ai.expresso.helper.Expressions.apply;
import static com.sri.ai.expresso.helper.Expressions.makeSymbol;
import static com.sri.ai.grinder.helper.GrinderUtil.getType;
import static com.sri.ai.grinder.helper.GrinderUtil.getTypeCardinality;
import static com.sri.ai.grinder.library.FunctorConstants.CARDINALITY;
import static com.sri.ai.grinder.library.FunctorConstants.DISEQUALITY;
import static com.sri.ai.grinder.library.FunctorConstants.EQUALITY;
import static com.sri.ai.grinder.library.FunctorConstants.TYPE;
import static com.sri.ai.util.Util.getFirst;
import static com.sri.ai.util.Util.list;
import static java.lang.Math.max;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.LinkedHashSet;
import java.util.List;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.core.DefaultSyntacticFunctionApplication;
import com.sri.ai.expresso.helper.AbstractExpressionWrapper;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.library.Disequality;
import com.sri.ai.grinder.library.Equality;
import com.sri.ai.grinder.library.boole.And;
import com.sri.ai.grinder.library.number.Minus;
import com.sri.ai.grinder.plaindpll.api.Constraint;
import com.sri.ai.grinder.plaindpll.api.ConstraintTheory;
import com.sri.ai.grinder.plaindpll.api.SingleVariableConstraint;
import com.sri.ai.grinder.plaindpll.api.TermTheory;
import com.sri.ai.grinder.plaindpll.core.SGDPLLT;
import com.sri.ai.util.Util;
import com.sri.ai.util.base.BinaryFunction;

/**
 * An {@link Expression} with efficient internal representation for operations on being expanded by a splitter (literal in constraintTheory) and
 * model counting.
 * This interface is defined for use primarily by {@link SGDPLLT}.
 * 
 * @author braz
 *
 */
public class SingleVariableEqualityConstraint extends AbstractExpressionWrapper implements SingleVariableConstraint {

	private static final long serialVersionUID = 1L;
	
	private Expression variable;
	private Expression boundValue;
	private ArrayList<Expression> disequals;
	private Collection<Expression> externalLiterals; // literals not on variable
	private int numberOfUniquelyNamedConstantDisequals; // -1 if not computed yet

	private EqualityConstraintTheory constraintTheory;
	
	public SingleVariableEqualityConstraint(Expression variable, TermTheory termTheory) {
		this(variable, null, new ArrayList<Expression>(), Util.set(), 0, termTheory);
	}
	
	public SingleVariableEqualityConstraint(Expression variable, Expression boundValue, TermTheory termTheory) {
		this(variable, boundValue, null, Util.set(), 0, termTheory);
	}
	
	public SingleVariableEqualityConstraint(Expression variable, ArrayList<Expression> disequals, int numberOfUniquelyNamedConstantDisequals, TermTheory termTheory) {
		this(variable, null, disequals, Util.set(), numberOfUniquelyNamedConstantDisequals, termTheory);
	}

	public SingleVariableEqualityConstraint(Expression variable, Expression boundValue, ArrayList<Expression> disequals, Collection<Expression> externalLiterals, int numberOfUniquelyNamedConstantDisequals, TermTheory termTheory) {
		this.variable = variable;
		this.boundValue = boundValue;
		this.disequals = disequals;
		this.externalLiterals = externalLiterals;
		this.numberOfUniquelyNamedConstantDisequals = numberOfUniquelyNamedConstantDisequals;
		this.constraintTheory = new EqualityConstraintTheory(termTheory);
	}
	
	/**
	 * Returns the expression the variable is bound to, or <code>null</code> if none.
	 */
	public Expression getBoundValue() {
		return boundValue;
	}
	
	/**
	 * Returns the expressions the variable is constrained to be disequal from.
	 * @return
	 */
	public List<Expression> getDisequals() {
		return Collections.unmodifiableList(disequals);
	}
	
	/**
	 * Returns the literals that are part of this constraint, but not defined on the variable.
	 * @return
	 */
	public Collection<Expression> getExternalLiterals() {
		return Collections.unmodifiableCollection(externalLiterals);
	}

	private SingleVariableEqualityConstraint makeWithNewExternalLiterals(Collection<Expression> newExternalLiterals) {
		if (newExternalLiterals == externalLiterals) {
			return this;
		}
		else {
			SingleVariableEqualityConstraint result = 
					new SingleVariableEqualityConstraint(
							variable, boundValue, disequals, newExternalLiterals, numberOfUniquelyNamedConstantDisequals, constraintTheory.getTermTheory());
			return result;
		}
	}

	private SingleVariableEqualityConstraint makeWithAdditionalExternalLiteral(Expression additionalExternalLiteral) {
		Collection<Expression> newExternalLiterals = new LinkedHashSet<>(externalLiterals);
		newExternalLiterals.add(additionalExternalLiteral);
		SingleVariableEqualityConstraint result = makeWithNewExternalLiterals(newExternalLiterals);
		return result;
	}
	
	private SingleVariableEqualityConstraint makeWithAdditionalExternalLiterals(Collection<Expression> additionalExternalLiterals) {
		Collection<Expression> newExternalLiterals = new LinkedHashSet<>(externalLiterals);
		newExternalLiterals.addAll(additionalExternalLiterals);
		SingleVariableEqualityConstraint result = makeWithNewExternalLiterals(newExternalLiterals);
		return result;
	}
	
	@Override
	public ConstraintTheory getConstraintTheory() {
		return constraintTheory;
	}
	
	@Override
	public Expression getVariable() {
		return variable;
	}

	@Override
	public Expression pickSplitter(RewritingProcess process) {
		Expression result;
		Expression externalLiteral = getFirst(getExternalLiterals());
		if (externalLiteral != null) {
			result = externalLiteral;
		}
		else if (getBoundValue() != null) {
			result = null; // there are no disequals stored if we already have a binding
		}
		else {
			// before we count the models, we need to know that all disequals are disequal to each other too.
			Constraint contextualConstraint = process.getDPLLContextualConstraint();
			List<Expression> disequals = getDisequals();
			for (int i = 0; i != disequals.size(); i++) {
				for (int j = i + 1; j != disequals.size(); j++) {
					Expression disequality = Disequality.makeWithConstantSimplification(disequals.get(i), disequals.get(j), process);
					if ( ! disequality.equals(FALSE) && ! contextualConstraint.directlyImpliesNonTrivialLiteral(disequality, process)) {
						result = apply(EQUALITY, disequals.get(i), disequals.get(j));
						break;
					}
				}
			}
			result = null;
		}
		return result;
	}

	@Override
	public SingleVariableEqualityConstraint simplifyGiven(Expression externalLiteral, RewritingProcess process) {
		SingleVariableEqualityConstraint result;
		
		if (externalLiterals.contains(externalLiteral)) {
			Collection<Expression> newExternalLiterals = new LinkedHashSet<>(externalLiterals);
			newExternalLiterals.remove(externalLiteral);
			result = makeWithNewExternalLiterals(newExternalLiterals);
		}
		else {
			result = this;
		}
		
		return result;
	}

	@Override
	public SingleVariableEqualityConstraint conjoin(Expression literal, RewritingProcess process) {
		SingleVariableEqualityConstraint result;
		
		if (externalLiterals.contains(literal)) {
			result = this;
		}
		else if ( ! literal.hasFunctor(EQUALITY) && ! literal.hasFunctor(DISEQUALITY)) {
			if (literal.equals(TRUE)) {
				result = this;
			}
			else if (literal.equals(FALSE)) {
				result = null;
			}
			else {
				result = makeWithAdditionalExternalLiteral(literal);
			}
		}
		else {
			Expression other = termToWhichVariableIsEqualedToOrNull(literal);
			if (other == null) {
				result = makeWithAdditionalExternalLiteral(literal);
			}
			else {
				boolean variableIsBound = boundValue != null;
				boolean splitterIsEquality = literal.hasFunctor(EQUALITY);
				if (variableIsBound) {
					if (splitterIsEquality ) {
						if (other.equals(boundValue)) {
							result = this;
						}
						else {
							result = keepValueAndEnforceOtherToBeEqualToIt(other, process);
						}
					}
					else { // splitter is disequality
						if (other.equals(boundValue)) {
							result = null; // contradiction, for splitter is disequality to the same boundValue the variable is bound to
						}
						else { 
							result = keepValueAndEnforceOtherToBeNotEqualToIt(other, process);
						}
					}
				}
				else { // variable is not bound, and the only constraints, if any, are disequalities
					if (splitterIsEquality) {
						result = takeValueAndEnforceDisequalsToBeNotEqualToIt(other, process);
					}
					else { 
						result = takeNewDisequal(other, process);
					}
				}
			}
		}

		return result;
	}

	private Expression termToWhichVariableIsEqualedToOrNull(Expression equalityLiteral) {
		Expression result;
		if (equalityLiteral.get(0).equals(variable)) {
			result = equalityLiteral.get(1);
		}
		else if (equalityLiteral.get(1).equals(variable)) {
			result = equalityLiteral.get(0);
		}
		else {
			result = null;
		}
		return result;
	}

	/**
	 * @param other
	 * @param process
	 * @return
	 */
	protected SingleVariableEqualityConstraint keepValueAndEnforceOtherToBeEqualToIt(Expression other, RewritingProcess process) {
		SingleVariableEqualityConstraint result;
		Expression impliedEquality = Equality.makeWithConstantSimplification(boundValue, other, process);
		if (impliedEquality.equals(TRUE)) {
			result = this;
		}
		else if (impliedEquality.equals(FALSE)) {
			result = null;
		}
		else {
			result = makeWithAdditionalExternalLiteral(impliedEquality); // implies boundValue = other
		}
		return result;
	}

	/**
	 * @param other
	 * @param process
	 * @return
	 */
	protected SingleVariableEqualityConstraint keepValueAndEnforceOtherToBeNotEqualToIt(Expression other, RewritingProcess process) {
		SingleVariableEqualityConstraint result;
		Collection<Expression> impliedLiterals = nonTrivialFormulasBuiltFromTerm1AndEachTerm2OrNull(boundValue, list(other), disequalityMaker(process));
		if (impliedLiterals == null) {
			result = null;
		}
		else {
			result = makeWithAdditionalExternalLiterals(impliedLiterals); // implies boundValue != other
		}
		return result;
	}

	/**
	 * @param other
	 * @param process
	 * @return
	 */
	protected SingleVariableEqualityConstraint takeValueAndEnforceDisequalsToBeNotEqualToIt(Expression other, RewritingProcess process) {
		SingleVariableEqualityConstraint result;
		Collection<Expression> impliedLiterals = nonTrivialFormulasBuiltFromTerm1AndEachTerm2OrNull(other, disequals, disequalityMaker(process));
		if (impliedLiterals == null) {
			result = null;
		}
		else {
			SingleVariableEqualityConstraint constraintBindingVariableToOther = new SingleVariableEqualityConstraint(variable, other, constraintTheory.getTermTheory());
			result = constraintBindingVariableToOther.makeWithAdditionalExternalLiterals(impliedLiterals);
		}
		return result;
	}

	/**
	 * @param other
	 * @return
	 */
	protected SingleVariableEqualityConstraint takeNewDisequal(Expression other, RewritingProcess process) {
		SingleVariableEqualityConstraint result;
		if (disequals.contains(other)) {
			result = this; // redundant
		}
		else {
			if (process.isUniquelyNamedConstant(other)) {
				incrementNumberOfUniquelyNamedConstantDisequals(process);
			}
			
			if (isInconsistentBecauseVariableIsDisequalToANumberOfUniquelyNamedConstantsEqualToDomainSize(process)) {
				result = null;
			}
			else {
				ArrayList<Expression> newDisequals = new ArrayList<>(disequals);
				newDisequals.add(other);
				SingleVariableEqualityConstraint newConstraint = new SingleVariableEqualityConstraint(variable, newDisequals, getNumberOfUniquelyNamedConstantDisequals(process), constraintTheory.getTermTheory());
				result = newConstraint;
			}
		}
		return result;
	}

	/**
	 * @param process
	 * @return
	 */
	protected boolean isInconsistentBecauseVariableIsDisequalToANumberOfUniquelyNamedConstantsEqualToDomainSize(RewritingProcess process) {
		long variableDomainSize = getVariableDomainSize(process);
		boolean result = variableDomainSize != -1 && getNumberOfUniquelyNamedConstantDisequals(process) == variableDomainSize;
		return result;
	}

	private int getNumberOfUniquelyNamedConstantDisequals(RewritingProcess process) {
		if (numberOfUniquelyNamedConstantDisequals == -1) {
			numberOfUniquelyNamedConstantDisequals = Util.count(disequals, process.getIsUniquelyNamedConstantPredicate());
		}
		return numberOfUniquelyNamedConstantDisequals;
	}

	/**
	 * @param process
	 * @return
	 */
	protected int incrementNumberOfUniquelyNamedConstantDisequals(RewritingProcess process) {
		return numberOfUniquelyNamedConstantDisequals = getNumberOfUniquelyNamedConstantDisequals(process) + 1;
	}

	/**
	 * @param process
	 * @return
	 */
	protected BinaryFunction<Expression, Expression, Expression> equalityMaker(RewritingProcess process) {
		return (t1, t2) -> Equality.makeWithConstantSimplification(t1, t2, process);
	}

	/**
	 * @param process
	 * @return
	 */
	protected BinaryFunction<Expression, Expression, Expression> disequalityMaker(RewritingProcess process) {
		return (t1, t2) -> Disequality.makeWithConstantSimplification(t1, t2, process);
	}

	/**
	 * Applies formula builder to term1 and each term in terms2 and returns that collection if none is false, or null as soon as a false one is obtained.
	 * @param term1
	 * @param terms2
	 * @param formulaBuilder
	 * @return a collection of built formulas if none is false, null otherwise.
	 */
	protected static Collection<Expression> nonTrivialFormulasBuiltFromTerm1AndEachTerm2OrNull(
			Expression term1, Collection<Expression> terms2, BinaryFunction<Expression, Expression, Expression> formulaBuilder) {
		
		Collection<Expression> result = list();
		for (Expression anotherTerm : terms2) {
			Expression literal = formulaBuilder.apply(term1, anotherTerm);
			if (literal.equals(TRUE)) {
			}
			else if (literal.equals(FALSE)) {
				result = null;
				break;
			}
			else {
				result.add(literal);
			}
		}
		return result;
	}

	@Override
	protected Expression computeInnerExpression() {
		List<Expression> conjuncts = list();
		if (boundValue != null) {
			conjuncts.add(apply(EQUALITY, variable, boundValue));
		}
		else {
			Util.mapIntoList(disequals, d -> apply(DISEQUALITY, variable, d), conjuncts);
		}
		conjuncts.addAll(externalLiterals);
		Expression result = And.make(conjuncts);
		return result;
	}

	@Override
	public SingleVariableEqualityConstraint clone() {
		SingleVariableEqualityConstraint result = new SingleVariableEqualityConstraint(variable, boundValue, disequals, Util.set(), numberOfUniquelyNamedConstantDisequals, constraintTheory.getTermTheory());
		return result;
	}

	@Override
	public Expression modelCount(RewritingProcess process) {
		Expression result;
		if (getBoundValue() != null) {
			result = ONE;
		}
		else {
			long numberOfNonAvailableValues = getDisequals().size();
			long variableDomainSize = getVariableDomainSize(process);
			if (variableDomainSize == -1) {
				Expression variableDomain = getVariableDomain(process);
				Expression variableDomainCardinality = apply(CARDINALITY, variableDomain);
				result = Minus.make(variableDomainCardinality, makeSymbol(numberOfNonAvailableValues));
			}
			else {
				result = makeSymbol(max(0, variableDomainSize - numberOfNonAvailableValues));
			}
		}
		return result;
	}

	@Override
	public Expression getVariableDomain(RewritingProcess process) {
		Expression variableType = getType(variable, process);
		if (variableType == null) {
			variableType = new DefaultSyntacticFunctionApplication(TYPE, variable);
		}
		return variableType;
	}
	
	protected long cachedIndexDomainSize = -1;

	@Override
	public long getVariableDomainSize(RewritingProcess process) {
		if (cachedIndexDomainSize == -1) {
			cachedIndexDomainSize = getTypeCardinality(variable, process);
		}
		return cachedIndexDomainSize;
	}
}