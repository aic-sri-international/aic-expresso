package com.sri.ai.grinder.sgdpllt.theory.differencearithmetic;

import java.util.LinkedHashSet;
import java.util.Set;

import com.sri.ai.expresso.api.Expression;

/** 
 * A triple containing the components of a difference arithmetic literal side:
 * the positive, negative and constant terms in it.
 * @author braz
 *
 */
public class DifferenceArithmeticLiteralSide {
	Set<Expression> positives;
	Set<Expression> negatives;
	int constant;
	
	public DifferenceArithmeticLiteralSide() {
		this(new LinkedHashSet<>(), new LinkedHashSet<>(), 0);
	}
	
	public DifferenceArithmeticLiteralSide(Set<Expression> positiveTerms, Set<Expression> negativeTerms, int constant) {
		this.positives = positiveTerms;
		this.negatives = negativeTerms;
		this.constant  = constant;
	}
}