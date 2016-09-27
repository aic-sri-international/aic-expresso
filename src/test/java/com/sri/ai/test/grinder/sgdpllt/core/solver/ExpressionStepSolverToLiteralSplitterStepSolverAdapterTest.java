package com.sri.ai.test.grinder.sgdpllt.core.solver;

import static com.sri.ai.expresso.helper.Expressions.parse;
import static com.sri.ai.grinder.helper.GrinderUtil.BOOLEAN_TYPE;
import static com.sri.ai.util.Util.arrayList;
import static com.sri.ai.util.Util.map;
import static org.junit.Assert.assertEquals;

import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Random;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

import org.junit.Ignore;
import org.junit.Test;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.Type;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.expresso.type.Categorical;
import com.sri.ai.expresso.type.IntegerInterval;
import com.sri.ai.grinder.sgdpllt.api.Context;
import com.sri.ai.grinder.sgdpllt.api.ExpressionStepSolver;
import com.sri.ai.grinder.sgdpllt.api.Theory;
import com.sri.ai.grinder.sgdpllt.core.constraint.ContextSplitting;
import com.sri.ai.grinder.sgdpllt.core.solver.ContextDependentExpressionProblemSolver;
import com.sri.ai.grinder.sgdpllt.core.solver.Evaluator;
import com.sri.ai.grinder.sgdpllt.core.solver.EvaluatorStepSolver;
import com.sri.ai.grinder.sgdpllt.core.solver.ExpressionStepSolverToLiteralSplitterStepSolverAdapter;
import com.sri.ai.grinder.sgdpllt.library.boole.And;
import com.sri.ai.grinder.sgdpllt.library.boole.Not;
import com.sri.ai.grinder.sgdpllt.library.boole.Or;
import com.sri.ai.grinder.sgdpllt.theory.compound.CompoundTheory;
import com.sri.ai.grinder.sgdpllt.theory.differencearithmetic.DifferenceArithmeticTheory;
import com.sri.ai.grinder.sgdpllt.theory.equality.EqualityTheory;
import com.sri.ai.grinder.sgdpllt.theory.linearrealarithmetic.LinearRealArithmeticTheory;
import com.sri.ai.grinder.sgdpllt.theory.propositional.PropositionalTheory;

public class ExpressionStepSolverToLiteralSplitterStepSolverAdapterTest {
	
	@Test
	public void testPropositionalTheoryWithFixedDisjunctiveFormulas() {
		Theory theory = new PropositionalTheory();
		
		Map<String, Type> variablesAndTypes = new LinkedHashMap<>(theory.getVariableNamesAndTypesForTesting());
		Type booleanType = variablesAndTypes.get("P");
		variablesAndTypes.put("S", booleanType);
		variablesAndTypes.put("T", booleanType);
		theory.setVariableNamesAndTypesForTesting(variablesAndTypes);
		
		Context context = theory.makeContextWithTestingInformation();
	
		runTest(theory, new GeneralFormulaExpressionTestStepSolver(Expressions.parse("P"), Expressions.parse("Q")), 
				Expressions.parse("if P then if Q then P and Q else P and not Q else if Q then not P and Q else not P and not Q"),
				context);
		
		runTest(theory, new GeneralFormulaExpressionTestStepSolver(Expressions.parse("P or Q"), Expressions.parse("R or Q")), 
				Expressions.parse("if P then if R then (P or Q) and (R or Q) else if Q then (P or Q) and (R or Q) else (P or Q) and not (R or Q) else if Q then (P or Q) and (R or Q) else if R then not (P or Q) and (R or Q) else not (P or Q) and not (R or Q)"),
				context);
		
		runTest(theory, new GeneralFormulaExpressionTestStepSolver(Expressions.parse("P or Q"), Expressions.parse("R or S")), 
				Expressions.parse("if P then if R then (P or Q) and (R or S) else if S then (P or Q) and (R or S) else (P or Q) and not (R or S) else if Q then if R then (P or Q) and (R or S) else if S then (P or Q) and (R or S) else (P or Q) and not (R or S) else if R then not (P or Q) and (R or S) else if S then not (P or Q) and (R or S) else not (P or Q) and not (R or S)"),
				context);
	}
	
	@Test
	public void testPropositionalTheoryWithRandomDisjunctiveFormulas() {
		Theory theory = new PropositionalTheory();		
		extendTestingVaribles("P", theory, "S", "T", "U", "V", "W", "X", "Y", "Z");
		
		runRandomDisjunctiveFormulasTest(theory);
	}
	
	@Ignore("Random generation of linear real arithmetic not yet implemented")
	@Test
	public void testLinearRealArithmeticTheoryWithRandomDisjunctiveFormulas() {
		Theory theory = new LinearRealArithmeticTheory(true, true);
	
		extendTestingVaribles("X", theory, "S", "T", "U", "V", "W");
		
		runRandomDisjunctiveFormulasTest(theory);
	}
	
	@Test
	public void testEqualityTheoryWithoutPropagationOfAllLiteralsWhenBoundWithRandomDisjunctiveFormulas() {
		Theory theory = new EqualityTheory(true, false);
	
		extendTestingVaribles("X", theory, "S", "T", "U", "V", "W");
		
		runRandomDisjunctiveFormulasTest(theory);
	}
	
	@Test
	public void testEqualityTheoryWithPropagationOfAllLiteralsWhenBoundWithRandomDisjunctiveFormulas() {
		Theory theory = new EqualityTheory(true, true);
	
		extendTestingVaribles("X", theory, "S", "T", "U", "V", "W");
		
		runRandomDisjunctiveFormulasTest(theory);
	}
	
	@Test
	public void testDifferenceArithmeticTheoryWithoutPropagationOfAllLiteralsWhenBoundWithRandomDisjunctiveFormulas() {
		Theory theory = new DifferenceArithmeticTheory(true, false);
	
		extendTestingVaribles("K", theory, "L", "M", "N", "O", "P");
		
		runRandomDisjunctiveFormulasTest(theory);
	}
	
	@Test
	public void testDifferenceArithmeticTheoryWithPropagationOfAllLiteralsWhenBoundWithRandomDisjunctiveFormulas() {
		Theory theory = new DifferenceArithmeticTheory(true, true);
	
		extendTestingVaribles("K", theory, "L", "M", "N", "O", "P");
		
		runRandomDisjunctiveFormulasTest(theory);
	}
	
	@Test
	public void testCompoundTheoryWithDifferenceArithmeticWithRandomDisjunctiveFormulas() {
		CompoundTheory compoundTheory = new CompoundTheory(
				new EqualityTheory(false, true),
				new DifferenceArithmeticTheory(false, true),
				new PropositionalTheory());
		
		// using different testing variables and types to test distribution of testing information
		// to sub constraint theories.
		
		Categorical booleanType = BOOLEAN_TYPE;
		Categorical dogsType    = new Categorical("Dogs", 4, arrayList(parse("fido"), parse("rex")));
		IntegerInterval oneTwoThree = new IntegerInterval(1, 3);
		
		Map<String, Type> variablesAndTypes =
				map(
						"F", booleanType,
						"G", booleanType,
						"R", dogsType,
						"S", dogsType,
						"T", oneTwoThree,
						"U", oneTwoThree
						);
		
		compoundTheory.setVariableNamesAndTypesForTesting(variablesAndTypes);
		
		runRandomDisjunctiveFormulasTest(compoundTheory);
	}
	
	@Test
	public void testCompoundTheoryWithoutDifferenceArithmeticWithRandomDisjunctiveFormulas() {
		CompoundTheory compoundTheory = new CompoundTheory(
				new EqualityTheory(false, true),
				new DifferenceArithmeticTheory(false, true),
				new PropositionalTheory());
		
		runRandomDisjunctiveFormulasTest(compoundTheory);
	}
	
	//
	//
	
	private void extendTestingVaribles(String variableToBaseTypeOn, Theory theory, String... newVariables) {
		Map<String, Type> variablesAndTypes = new LinkedHashMap<>(theory.getVariableNamesAndTypesForTesting());
		Type type = variablesAndTypes.get(variableToBaseTypeOn);
		if (type == null) {
			throw new IllegalArgumentException("variableToBaseTypeOn="+variableToBaseTypeOn+", is not already part of the theory.");
		}
		for (String newVariable : newVariables) {
			variablesAndTypes.put(newVariable, type);
		}
		theory.setVariableNamesAndTypesForTesting(variablesAndTypes);
	}
	
	private void runRandomDisjunctiveFormulasTest(Theory theory) {
		for (int numberDisjuncts = 1; numberDisjuncts < 4; numberDisjuncts++) {
			// NOTE: we want to start with # literals per disjunct = 1 so that the expression step solver
			// acts like a literal step solver for this case.
			for (int numberLiteralsPerDisjunct = 1; numberLiteralsPerDisjunct < 4; numberLiteralsPerDisjunct++) {
				// # tests to run for combination.
				for (int j = 0; j < 3; j++) {
					Context context = theory.makeContextWithTestingInformation();
					Random random = new Random();
					
					runTest(theory, new GeneralFormulaExpressionTestStepSolver(theory, random, context, numberDisjuncts, numberLiteralsPerDisjunct), null, context);
				}
			}
		}
	}
	
	private void runTest(Theory theory, GeneralFormulaExpressionTestStepSolver generalFormulaExpressionTestStepSolver, Expression expected, Context context) {
		ExpressionStepSolverToLiteralSplitterStepSolverAdapter stepSolver = new ExpressionStepSolverToLiteralSplitterStepSolverAdapter(generalFormulaExpressionTestStepSolver);
		System.out.println("Evaluating " + generalFormulaExpressionTestStepSolver.getExpressionToSolve());
		Expression solution = ContextDependentExpressionProblemSolver.staticSolve(stepSolver, context);
		System.out.println(generalFormulaExpressionTestStepSolver.getExpressionToSolve() + " -----> " + solution + "\n");
		
		if (expected != null) {
			assertEquals(expected, solution);
		}
		
		assertEquals(Expressions.TRUE, new Evaluator(theory).apply(solution, context));
	}
	
	static class GeneralFormulaExpressionTestStepSolver implements ExpressionStepSolver {
		private List<Expression> problemConjuncts  = new ArrayList<>();
		private List<Expression> solutionConjuncts = new ArrayList<>();
		
		public GeneralFormulaExpressionTestStepSolver(Expression... formulas) {
			for (Expression conjunct : formulas) {
				problemConjuncts.add(conjunct);
			}
		}
		
		public GeneralFormulaExpressionTestStepSolver(Theory theory, Random random, Context context, int numberDisjuncts, int numberLiteralsPerDisjunct) {			
			for (int i = 0; i < numberDisjuncts; i++) {
				problemConjuncts.add(Or.make(IntStream.range(0, numberLiteralsPerDisjunct).mapToObj(idx -> theory.makeRandomLiteral(random, context)).collect(Collectors.toList())));
			}
		}
		
		public Expression getExpressionToSolve() {
			return And.make(problemConjuncts);
		}
		
		@Override
		public GeneralFormulaExpressionTestStepSolver clone()  {
			try {
				return (GeneralFormulaExpressionTestStepSolver) super.clone();
			}
			catch (CloneNotSupportedException e) {
				throw new Error(e);
			}
		}
		
		@Override
		public Step step(Context context) {
			Step result = null;
			List<Expression> stepSolutionConjuncts = new ArrayList<>(solutionConjuncts);
			for (int i = stepSolutionConjuncts.size(); i < problemConjuncts.size(); i++) {
				Expression conjunct = problemConjuncts.get(i);
				EvaluatorStepSolver evaluatorStepSolver = new EvaluatorStepSolver(conjunct);
				Expression conjunctResult = evaluatorStepSolver.solve(context);
				if (Expressions.TRUE.equals(conjunctResult)) {
					stepSolutionConjuncts.add(conjunct);
				}
				else if (Expressions.FALSE.equals(conjunctResult)) {
					stepSolutionConjuncts.add(Not.make(conjunct));
				}
				else {	
					GeneralFormulaExpressionTestStepSolver ifTrue = this.clone();
					ifTrue.solutionConjuncts = new ArrayList<>(stepSolutionConjuncts);
					ifTrue.solutionConjuncts.add(conjunct);
					
					GeneralFormulaExpressionTestStepSolver ifFalse = this.clone();
					ifFalse.solutionConjuncts = new ArrayList<>(stepSolutionConjuncts);
					ifFalse.solutionConjuncts.add(Not.make(conjunct));
					
					ContextSplitting contextSplitting = null;
					// If the splitter is a literal then we want to include the context splitting
					// information for the literal.
					if (context.getTheory().isLiteral(conjunct, context)) {
						contextSplitting = new ContextSplitting(conjunct, context);
					}
					
					result = new ExpressionStepSolver.ItDependsOn(conjunct, contextSplitting, ifTrue, ifFalse);
					break;
				}
			}
			
			if (result == null) {
				result = new ExpressionStepSolver.Solution(And.make(stepSolutionConjuncts));
			}
				
			return result;
		}
	}
}