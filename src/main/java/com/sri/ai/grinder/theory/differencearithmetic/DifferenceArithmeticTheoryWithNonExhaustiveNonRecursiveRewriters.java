package com.sri.ai.grinder.theory.differencearithmetic;

import static com.sri.ai.grinder.rewriter.core.Switch.FUNCTOR;
import static com.sri.ai.util.Util.map;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.api.ExpressionLiteralSplitterStepSolver;
import com.sri.ai.grinder.library.FunctorConstants;
import com.sri.ai.grinder.library.controlflow.IfThenElseRewriter;
import com.sri.ai.grinder.rewriter.api.Simplifier;
import com.sri.ai.grinder.rewriter.api.TopRewriter;
import com.sri.ai.grinder.rewriter.core.Switch;

public class DifferenceArithmeticTheoryWithNonExhaustiveNonRecursiveRewriters extends DifferenceArithmeticTheory {
	
	TopRewriter stepSolverSwitch;
	
	//I didn't spend time to understand these arguments just FYI
	public DifferenceArithmeticTheoryWithNonExhaustiveNonRecursiveRewriters(boolean atomFunctorsAreUniqueToThisTheory, boolean propagateAllLiteralsWhenVariableIsBound) {
		super(
				atomFunctorsAreUniqueToThisTheory,
				propagateAllLiteralsWhenVariableIsBound);
		//Switch(Function<Expression, T> keyMaker, Map<T, ? extends Rewriter> fromKeyToRewriter)
		stepSolverSwitch = new Switch<>(
				FUNCTOR,
				map(
						FunctorConstants.IF_THEN_ELSE,		new IfThenElseRewriter(),
						FunctorConstants.EQUALITY,			(Simplifier) (expression, context) -> expression
						//FunctorConstants.SUM,				//
				)
		);
	}
	
	@Override
	public ExpressionLiteralSplitterStepSolver makeEvaluatorStepSolver(Expression expression) {
		ExpressionLiteralSplitterStepSolver result = stepSolverSwitch.makeStepSolver(expression);
		return result;
	}

}
