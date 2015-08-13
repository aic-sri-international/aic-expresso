package com.sri.ai.grinder.plaindpll.theory;

import static com.sri.ai.expresso.helper.Expressions.FALSE;
import static com.sri.ai.expresso.helper.Expressions.ONE;
import static com.sri.ai.expresso.helper.Expressions.apply;
import static com.sri.ai.expresso.helper.Expressions.makeSymbol;
import static com.sri.ai.grinder.library.FunctorConstants.CARDINALITY;
import static com.sri.ai.grinder.library.FunctorConstants.EQUALITY;
import static com.sri.ai.util.Util.getFirst;
import static java.lang.Math.max;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.helper.AbstractExpressionWrapper;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.library.Disequality;
import com.sri.ai.grinder.library.number.Minus;
import com.sri.ai.grinder.plaindpll.api.Constraint;
import com.sri.ai.grinder.plaindpll.api.ConstraintTheory;
import com.sri.ai.grinder.plaindpll.api.TermTheory;
import com.sri.ai.util.Util;
import com.sri.ai.util.base.NullaryFunction;

/**
 * 
 * @author braz
 *
 */
public class SingleVariableEqualityConstraintWithModelCounting extends AbstractExpressionWrapper implements SingleVariableConstraintWithModelCounting {
	
	private static final long serialVersionUID = 1L;
	
	private SingleVariableEqualityConstraint innerConstraint;
	
	public SingleVariableEqualityConstraintWithModelCounting(SingleVariableEqualityConstraint innerConstraint) {
		this.innerConstraint = innerConstraint;
	}
	
	public SingleVariableEqualityConstraintWithModelCounting(Expression variable, TermTheory termTheory) {
		this.innerConstraint = new SingleVariableEqualityConstraint(variable, null, new ArrayList<Expression>(), Util.set(), 0, termTheory);
	}
	
	public SingleVariableEqualityConstraintWithModelCounting(Expression variable, Expression boundValue, TermTheory termTheory) {
		this.innerConstraint = new SingleVariableEqualityConstraint(variable, boundValue, null, Util.set(), 0, termTheory);
	}
	
	public SingleVariableEqualityConstraintWithModelCounting(Expression variable, ArrayList<Expression> disequals, int numberOfUniquelyNamedConstantDisequals, TermTheory termTheory) {
		this.innerConstraint = new SingleVariableEqualityConstraint(variable, null, disequals, Util.set(), numberOfUniquelyNamedConstantDisequals, termTheory);
	}

	public SingleVariableEqualityConstraintWithModelCounting(Expression variable, Expression boundValue, ArrayList<Expression> disequals, Collection<Expression> externalLiterals, int numberOfUniquelyNamedConstantDisequals, TermTheory termTheory) {
		this.innerConstraint = new SingleVariableEqualityConstraint(variable, boundValue, disequals, externalLiterals, numberOfUniquelyNamedConstantDisequals, termTheory);
	}

	/**
	 * Picks a splitter whose value is necessary in order to determine the model count of the constraint.
	 * @param process the current rewriting process
	 * @return the splitter
	 */
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

	/**
	 * The number of assignments to the variable that satisfies the constraint.
	 * @param process the current rewriting process
	 * @return an expression representing the model count
	 */
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
	public ConstraintTheory getConstraintTheory() {
		return innerConstraint.getConstraintTheory();
	}

	@Override
	public Expression getVariable() {
		return innerConstraint.getVariable();
	}

	@Override
	public Expression getVariableDomain(RewritingProcess process) {
		return innerConstraint.getVariableDomain(process);
	}

	@Override
	public long getVariableDomainSize(RewritingProcess process) {
		return innerConstraint.getVariableDomainSize(process);
	}

	public Expression getBoundValue() {
		return innerConstraint.getBoundValue();
	}

	public List<Expression> getDisequals() {
		return innerConstraint.getDisequals();
	}

	@Override
	public Collection<Expression> getExternalLiterals() {
		return innerConstraint.getExternalLiterals();
	}

	@Override
	public SingleVariableEqualityConstraintWithModelCounting simplifyGiven(Expression externalLiteral, RewritingProcess process) {
		return passThrough(() -> innerConstraint.simplifyGiven(externalLiteral, process));
	}

	@Override
	public SingleVariableConstraintWithModelCounting conjoin(Expression literal, RewritingProcess process) {
		return passThrough(() -> innerConstraint.conjoin(literal, process));
	}

	/**
	 * @param getNewInnerConstraint
	 * @return
	 */
	protected SingleVariableEqualityConstraintWithModelCounting passThrough(NullaryFunction<SingleVariableEqualityConstraint> getNewInnerConstraint) {
		SingleVariableEqualityConstraintWithModelCounting result;
		SingleVariableEqualityConstraint newInnerConstraint = getNewInnerConstraint.apply();
		if (newInnerConstraint != innerConstraint) {
			result = new SingleVariableEqualityConstraintWithModelCounting(newInnerConstraint);
		}
		else {
			result = this;
		}
		return result;
	}

	@Override
	protected Expression computeInnerExpression() {
		return innerConstraint;
	}

	@Override
	public SingleVariableEqualityConstraintWithModelCounting clone() {
		SingleVariableEqualityConstraint newInnerExpression = (SingleVariableEqualityConstraint) innerConstraint.clone();
		SingleVariableEqualityConstraintWithModelCounting result = new SingleVariableEqualityConstraintWithModelCounting(newInnerExpression);
		return result;
	}
}