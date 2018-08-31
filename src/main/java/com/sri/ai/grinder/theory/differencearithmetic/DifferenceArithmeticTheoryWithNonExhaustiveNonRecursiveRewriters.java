package com.sri.ai.grinder.theory.differencearithmetic;

import static com.sri.ai.grinder.library.FunctorConstants.IF_THEN_ELSE;
import static com.sri.ai.grinder.library.FunctorConstants.SUM;
import static com.sri.ai.grinder.rewriter.core.Switch.FUNCTOR;
import static com.sri.ai.util.Util.map;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.api.ExpressionLiteralSplitterStepSolver;
import com.sri.ai.grinder.core.solver.SGVET;
import com.sri.ai.grinder.core.solver.SimplifierForAggregateFunctionOnIntensionalSet;
import com.sri.ai.grinder.group.SumProduct;
import com.sri.ai.grinder.library.controlflow.IfThenElseRewriter;
import com.sri.ai.grinder.rewriter.api.TopRewriter;
import com.sri.ai.grinder.rewriter.core.FirstOf;
import com.sri.ai.grinder.rewriter.core.Switch;
import com.sri.ai.grinder.rewriter.help.LiteralRewriter;

public class DifferenceArithmeticTheoryWithNonExhaustiveNonRecursiveRewriters extends DifferenceArithmeticTheory {
	
	TopRewriter stepSolverSwitch; //ifThenElseAndSummationStepSolverSwitchWithLiteralExternalization
	
	public DifferenceArithmeticTheoryWithNonExhaustiveNonRecursiveRewriters(boolean atomFunctorsAreUniqueToThisTheory, boolean propagateAllLiteralsWhenVariableIsBound) {
		super(atomFunctorsAreUniqueToThisTheory, propagateAllLiteralsWhenVariableIsBound);
		
		TopRewriter ifThenElseAndSummationSwitch = new Switch<>(
				FUNCTOR,
				map(	IF_THEN_ELSE, new IfThenElseRewriter(),
						SUM, new SimplifierForAggregateFunctionOnIntensionalSet(new SumProduct(), new SGVET()))
		);
		
		stepSolverSwitch = new FirstOf(
				ifThenElseAndSummationSwitch + " with literal externalization",
				ifThenElseAndSummationSwitch, 
				new LiteralRewriter(ifThenElseAndSummationSwitch)
		);
		
	}
	
	@Override
	public ExpressionLiteralSplitterStepSolver makeEvaluatorStepSolver(Expression expression) {
		ExpressionLiteralSplitterStepSolver result = stepSolverSwitch.makeStepSolver(expression);
		return result;
	}

}
