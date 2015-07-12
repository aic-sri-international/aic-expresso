package com.sri.ai.grinder.plaindpll.theory;

import static com.sri.ai.expresso.helper.Expressions.FALSE;
import static com.sri.ai.expresso.helper.Expressions.ONE;
import static com.sri.ai.expresso.helper.Expressions.TRUE;
import static com.sri.ai.expresso.helper.Expressions.apply;
import static com.sri.ai.expresso.helper.Expressions.makeSymbol;
import static com.sri.ai.grinder.helper.GrinderUtil.getTypeCardinality;
import static com.sri.ai.grinder.library.FunctorConstants.CARDINALITY;
import static com.sri.ai.grinder.library.FunctorConstants.DISEQUALITY;
import static com.sri.ai.grinder.library.FunctorConstants.EQUALITY;
import static com.sri.ai.grinder.library.FunctorConstants.TYPE;
import static com.sri.ai.util.Util.list;
import static java.lang.Math.max;
import static java.util.Collections.emptyList;

import java.util.ArrayList;
import java.util.Collection;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.core.DefaultSyntacticFunctionApplication;
import com.sri.ai.expresso.helper.AbstractExpressionWrapper;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.library.Disequality;
import com.sri.ai.grinder.library.Equality;
import com.sri.ai.grinder.library.boole.And;
import com.sri.ai.grinder.library.number.Minus;
import com.sri.ai.grinder.plaindpll.api.Constraint;
import com.sri.ai.grinder.plaindpll.api.SingleVariableConstraint;
import com.sri.ai.grinder.plaindpll.core.SGDPLLT;
import com.sri.ai.util.Util;
import com.sri.ai.util.base.BinaryFunction;
import com.sri.ai.util.collect.FunctionIterator;

/**
 * An {@link Expression} with efficient internal representation for operations on being expanded by a splitter (literal in constraint constraintTheory) and
 * model counting.
 * This interface is defined for use primarily by {@link SGDPLLT}.
 * 
 * @author braz
 *
 */
public class SingleVariableEqualityConstraint extends AbstractExpressionWrapper implements SingleVariableConstraint {

	private static final long serialVersionUID = 1L;
	
	private Expression variable;
	private Expression value;
	private ArrayList<Expression> disequals;
	private int numberOfUniquelyNamedConstantDisequals; // -1 if not computed yet
	
	public SingleVariableEqualityConstraint(Expression variable) {
		this(variable, null, new ArrayList<Expression>(), 0);
	}
	
	public SingleVariableEqualityConstraint(Expression variable, Expression value) {
		this(variable, value, null, 0);
	}
	
	public SingleVariableEqualityConstraint(Expression variable, ArrayList<Expression> disequals, int numberOfUniquelyNamedConstantDisequals) {
		this(variable, null, disequals, numberOfUniquelyNamedConstantDisequals);
	}

	public SingleVariableEqualityConstraint(Expression variable, Expression value, ArrayList<Expression> disequals, int numberOfUniquelyNamedConstantDisequals) {
		this.variable = variable;
		this.value = value;
		this.disequals = disequals;
		this.numberOfUniquelyNamedConstantDisequals = numberOfUniquelyNamedConstantDisequals;
	}

	@Override
	public Expression getVariable() {
		return variable;
	}

	@Override
	public Expression pickSplitter(RewritingProcess process) {
		Expression result;
		if (value != null) {
			result = null;
		}
		else {
			// before we count the models, we need to know that all disequals are disequal to each other too.
			Constraint contextualConstraint = process.getDPLLContextualConstraint();
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
	public ApplicationResult incorporate(Expression literal, RewritingProcess process) {
		ApplicationResult result;
		if ( ! literal.hasFunctor(EQUALITY) && ! literal.hasFunctor(DISEQUALITY)) {
			if (literal.equals(TRUE)) {
				result = redundantLiteralResult();
			}
			else if (literal.equals(FALSE)) {
				result = null;
			}
			else {
				result = new DefaultApplicationResult(this, list(literal));
			}
		}
		else {
			Expression other = termToWhichVariableIsEqualedToOrNull(literal);
			if (other == null) {
				result = new DefaultApplicationResult(this, list(literal));
			}
			else {
				boolean variableIsBound = value != null;
				boolean splitterIsEquality = literal.hasFunctor(EQUALITY);
				if (variableIsBound) {
					if (splitterIsEquality ) {
						if (other.equals(value)) {
							result = redundantLiteralResult();
						}
						else {
							result = keepValueAndEnforceOtherToBeEqualToIt(other, process);
						}
					}
					else { // splitter is disequality
						if (other.equals(value)) {
							result = null; // contradiction, for splitter is disequality to the same value the variable is bound to
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
	 * @return
	 */
	protected DefaultApplicationResult redundantLiteralResult() {
		return new DefaultApplicationResult(this, emptyList());
	}

	/**
	 * @param other
	 * @param process
	 * @return
	 */
	protected ApplicationResult keepValueAndEnforceOtherToBeEqualToIt(Expression other, RewritingProcess process) {
		ApplicationResult result;
		Expression impliedEquality = Equality.makeWithConstantSimplification(value, other, process);
		if (impliedEquality.equals(TRUE)) {
			result = redundantLiteralResult();
		}
		else if (impliedEquality.equals(FALSE)) {
			result = null;
		}
		else {
			result = new DefaultApplicationResult(this, list(impliedEquality)); // implies value = other
		}
		return result;
	}

	/**
	 * @param other
	 * @param process
	 * @return
	 */
	protected ApplicationResult keepValueAndEnforceOtherToBeNotEqualToIt(Expression other, RewritingProcess process) {
		ApplicationResult result;
		Collection<Expression> impliedLiterals = nonTrivialFormulasBuiltFromTerm1AndEachTerm2OrNull(value, list(other), disequalityMaker(process));
		if (impliedLiterals == null) {
			result = null;
		}
		else {
			result = new DefaultApplicationResult(this, impliedLiterals); // implies value != other
		}
		return result;
	}

	/**
	 * @param other
	 * @param process
	 * @return
	 */
	protected ApplicationResult takeValueAndEnforceDisequalsToBeNotEqualToIt(Expression other, RewritingProcess process) {
		ApplicationResult result;
		Collection<Expression> impliedLiterals = nonTrivialFormulasBuiltFromTerm1AndEachTerm2OrNull(other, disequals, disequalityMaker(process));
		if (impliedLiterals == null) {
			result = null;
		}
		else {
			result = new DefaultApplicationResult(new SingleVariableEqualityConstraint(variable, other), impliedLiterals);
		}
		return result;
	}

	/**
	 * @param other
	 * @return
	 */
	protected ApplicationResult takeNewDisequal(Expression other, RewritingProcess process) {
		ApplicationResult result;
		if (disequals.contains(other)) {
			result = redundantLiteralResult(); // redundant
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
				SingleVariableEqualityConstraint newConstraint = new SingleVariableEqualityConstraint(variable, newDisequals, getNumberOfUniquelyNamedConstantDisequals(process));
				result = new DefaultApplicationResult(newConstraint, emptyList());
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
		Expression result;
		if (value != null) {
			result = apply(EQUALITY, variable, value);
		}
		else {
			result = And.make(new FunctionIterator<Expression, Expression>(disequals, d -> apply(DISEQUALITY, variable, d)));
		}
		return result;
	}

	@Override
	public Expression clone() {
		SingleVariableEqualityConstraint result = new SingleVariableEqualityConstraint(variable, value, disequals, numberOfUniquelyNamedConstantDisequals);
		return result;
	}

	@Override
	public Expression modelCount(RewritingProcess process) {
		Expression result;
		if (value != null) {
			result = ONE;
		}
		else {
			long numberOfNonAvailableValues = disequals.size();
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

	protected Expression getVariableDomain(RewritingProcess process) {
		Expression variableType = process.getContextualSymbolType(variable);
		if (variableType == null) {
			variableType = new DefaultSyntacticFunctionApplication(TYPE, variable);
		}
		return variableType;
	}
	
	protected long cachedIndexDomainSize = -1;

	/**
	 * Returns the variable domain size, if determined, or -1 otherwise.
	 * @param process
	 * @return
	 */
	protected long getVariableDomainSize(RewritingProcess process) {
		if (cachedIndexDomainSize == -1) {
			cachedIndexDomainSize = getTypeCardinality(variable, process);
		}
		return cachedIndexDomainSize;
	}
}