package com.sri.ai.test.grinder.core.solver;

import static com.sri.ai.expresso.helper.Expressions.parse;
import static com.sri.ai.grinder.core.solver.ExpressionStepSolverToLiteralSplitterStepSolverAdapter.toExpressionLiteralSplitterStepSolver;
import static com.sri.ai.grinder.helper.GrinderUtil.BOOLEAN_TYPE;
import static com.sri.ai.util.Util.arrayList;
import static com.sri.ai.util.Util.map;
import static com.sri.ai.util.Util.println;
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
import com.sri.ai.grinder.api.Context;
import com.sri.ai.grinder.api.ExpressionLiteralSplitterStepSolver;
import com.sri.ai.grinder.api.ExpressionStepSolver;
import com.sri.ai.grinder.core.constraint.ContextSplitting;
import com.sri.ai.grinder.core.solver.ContextDependentExpressionProblemSolver;
import com.sri.ai.grinder.library.boole.And;
import com.sri.ai.grinder.library.boole.Not;
import com.sri.ai.grinder.library.boole.Or;
import com.sri.ai.grinder.tester.TheoryTestingSupport;
import com.sri.ai.grinder.theory.compound.CompoundTheory;
import com.sri.ai.grinder.theory.differencearithmetic.DifferenceArithmeticTheory;
import com.sri.ai.grinder.theory.equality.EqualityTheory;
import com.sri.ai.grinder.theory.linearrealarithmetic.LinearRealArithmeticTheory;
import com.sri.ai.grinder.theory.propositional.PropositionalTheory;

public class ExpressionStepSolverToLiteralSplitterStepSolverAdapterTest {
	
	public Random makeRandom() {
		return new Random();
	}
	
	@Test
	public void testPropositionalTheoryWithFixedDisjunctiveFormulas() {
		TheoryTestingSupport theoryTestingSupport = 
				TheoryTestingSupport.make(
						makeRandom(), 
						new PropositionalTheory());	
		extendTestingVariables("P", theoryTestingSupport, "S", "T");
		
		Context context = theoryTestingSupport.makeContextWithTestingInformation();
	
		runTest(theoryTestingSupport, new GeneralFormulaExpressionTestStepSolver(Expressions.parse("P"), Expressions.parse("Q")), 
				Expressions.parse("if P then if Q then P and Q else P and not Q else if Q then not P and Q else not P and not Q"),
				context);
		
		runTest(theoryTestingSupport, new GeneralFormulaExpressionTestStepSolver(Expressions.parse("P or Q"), Expressions.parse("R or Q")), 
				Expressions.parse("if P then if R then (P or Q) and (R or Q) else if Q then (P or Q) and (R or Q) else (P or Q) and not (R or Q) else if Q then (P or Q) and (R or Q) else if R then not (P or Q) and (R or Q) else not (P or Q) and not (R or Q)"),
				context);
		
		runTest(theoryTestingSupport, new GeneralFormulaExpressionTestStepSolver(Expressions.parse("P or Q"), Expressions.parse("R or S")), 
				Expressions.parse("if P then if R then (P or Q) and (R or S) else if S then (P or Q) and (R or S) else (P or Q) and not (R or S) else if Q then if R then (P or Q) and (R or S) else if S then (P or Q) and (R or S) else (P or Q) and not (R or S) else if R then not (P or Q) and (R or S) else if S then not (P or Q) and (R or S) else not (P or Q) and not (R or S)"),
				context);
	}
	
	@Test
	public void testPropositionalTheoryWithRandomDisjunctiveFormulas() {
		TheoryTestingSupport theoryTestingSupport = TheoryTestingSupport.make(makeRandom(), new PropositionalTheory());		
		extendTestingVariables("P", theoryTestingSupport, "S", "T", "U", "V", "W", "X", "Y", "Z");
		
		runRandomDisjunctiveFormulasTest(theoryTestingSupport);
	}
	
	@Ignore("Random generation of linear real arithmetic not yet implemented")
	@Test
	public void testLinearRealArithmeticTheoryWithRandomDisjunctiveFormulas() {
		TheoryTestingSupport theoryTestingSupport = TheoryTestingSupport.make(makeRandom(), new LinearRealArithmeticTheory(true, true));
	
		extendTestingVariables("X", theoryTestingSupport, "S", "T", "U", "V", "W");
		
		runRandomDisjunctiveFormulasTest(theoryTestingSupport);
	}
	
	@Test
	public void testEqualityTheoryWithoutPropagationOfAllLiteralsWhenBoundWithRandomDisjunctiveFormulas() {
		TheoryTestingSupport theoryTestingSupport = TheoryTestingSupport.make(makeRandom(), new EqualityTheory(true, false));
	
		extendTestingVariables("X", theoryTestingSupport, "S", "T", "U", "V", "W");
		
		runRandomDisjunctiveFormulasTest(theoryTestingSupport);
	}
	
	@Test
	public void testEqualityTheoryWithPropagationOfAllLiteralsWhenBoundWithRandomDisjunctiveFormulas() {
		TheoryTestingSupport theoryTestingSupport = TheoryTestingSupport.make(makeRandom(), new EqualityTheory(true, true));
	
		extendTestingVariables("X", theoryTestingSupport, "S", "T", "U", "V", "W");
		
		runRandomDisjunctiveFormulasTest(theoryTestingSupport);
	}
	
	@Test
	public void testDifferenceArithmeticTheoryWithoutPropagationOfAllLiteralsWhenBoundWithRandomDisjunctiveFormulas() {
		TheoryTestingSupport theoryTestingSupport = TheoryTestingSupport.make(makeRandom(), new DifferenceArithmeticTheory(true, false));
	
		extendTestingVariables("K", theoryTestingSupport, "L", "M", "N", "O", "P");
		
		runRandomDisjunctiveFormulasTest(theoryTestingSupport);
	}
	
	@Test
	public void testDifferenceArithmeticTheoryWithPropagationOfAllLiteralsWhenBoundWithRandomDisjunctiveFormulas() {
		TheoryTestingSupport theoryTestingSupport = TheoryTestingSupport.make(makeRandom(), new DifferenceArithmeticTheory(true, true));
	
		extendTestingVariables("K", theoryTestingSupport, "L", "M", "N", "O", "P");
		
		runRandomDisjunctiveFormulasTest(theoryTestingSupport);
	}
	
	@Test
	public void testCompoundTheoryWithDifferenceArithmeticWithRandomDisjunctiveFormulas() {
		TheoryTestingSupport theoryTestingSupport = TheoryTestingSupport.make(makeRandom(), new CompoundTheory(
				new EqualityTheory(false, true),
				new DifferenceArithmeticTheory(false, true),
				new PropositionalTheory()));
		
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
		
		theoryTestingSupport.setVariableNamesAndTypesForTesting(variablesAndTypes);
		
		runRandomDisjunctiveFormulasTest(theoryTestingSupport);
	}
	
	@Test
	public void testCompoundTheoryWithoutDifferenceArithmeticWithRandomDisjunctiveFormulas() {
		TheoryTestingSupport theoryTestingSupport = TheoryTestingSupport.make(makeRandom(), new CompoundTheory(
				new EqualityTheory(false, true),
				new DifferenceArithmeticTheory(false, true),
				new PropositionalTheory()));
		
		runRandomDisjunctiveFormulasTest(theoryTestingSupport);
	}
	
	//
	//
	
	private void extendTestingVariables(String variableToBaseTypeOn, TheoryTestingSupport theoryTestingSupport, String... newVariables) {
		Map<String, Type> variablesAndTypes = new LinkedHashMap<>(theoryTestingSupport.getVariableNamesAndTypesForTesting());
		Type type = variablesAndTypes.get(variableToBaseTypeOn);
		if (type == null) {
			throw new IllegalArgumentException("variableToBaseTypeOn="+variableToBaseTypeOn+", is not already part of the theory.");
		}
		for (String newVariable : newVariables) {
			variablesAndTypes.put(newVariable, type);
		}
		theoryTestingSupport.setVariableNamesAndTypesForTesting(variablesAndTypes);
	}
	
	private void runRandomDisjunctiveFormulasTest(TheoryTestingSupport theoryTestingSupport) {
		for (int numberDisjuncts = 1; numberDisjuncts < 4; numberDisjuncts++) {
			// NOTE: we want to start with # literals per disjunct = 1 so that the expression step solver
			// acts like a literal step solver for this case.
			for (int numberLiteralsPerDisjunct = 1; numberLiteralsPerDisjunct < 4; numberLiteralsPerDisjunct++) {
				// # tests to run for combination.
				for (int j = 0; j < 3; j++) {
					Context context = theoryTestingSupport.makeContextWithTestingInformation();					
					runTest(theoryTestingSupport, new GeneralFormulaExpressionTestStepSolver(theoryTestingSupport, context, numberDisjuncts, numberLiteralsPerDisjunct), null, context);
				}
			}
		}
	}
	
	private void runTest(TheoryTestingSupport theoryTestingSupport, GeneralFormulaExpressionTestStepSolver generalFormulaExpressionTestStepSolver, Expression expected, Context context) {
		ExpressionLiteralSplitterStepSolver stepSolver = toExpressionLiteralSplitterStepSolver(generalFormulaExpressionTestStepSolver);
		println("Evaluating " + generalFormulaExpressionTestStepSolver.getExpressionToSolve());
		println("based in theory only (no simplification is performed and we get a tree of literals).");
		Expression solution = ContextDependentExpressionProblemSolver.staticSolve(stepSolver, context);
		println(generalFormulaExpressionTestStepSolver.getExpressionToSolve() + " -----> " + solution + "\n");
		
		if (expected != null) {
			assertEquals(expected, solution);
		}
		
		println("Evaluating solution " + solution);
		println("Under context " + context);
		println("This test *incorrectly* assumes this should result in 'true'. This follows from GeneralFormulaExpressionTestStepSolver being incorrect.");
		println("It needs to be replaced by a comparison of expression-step-solver-based and literal-step-solver-based solutions");
		Expression evaluation = theoryTestingSupport.getTheory().evaluate(solution, context);
		println("Result: " + evaluation + "\n");
		assertEquals(Expressions.TRUE, evaluation);
	}
	
	static class GeneralFormulaExpressionTestStepSolver implements ExpressionStepSolver {
		private List<Expression> problemConjuncts  = new ArrayList<>();
		private List<Expression> solutionConjuncts = new ArrayList<>();
		
		public GeneralFormulaExpressionTestStepSolver(Expression... formulas) {
			for (Expression conjunct : formulas) {
				problemConjuncts.add(conjunct);
			}
		}
		
		public GeneralFormulaExpressionTestStepSolver(TheoryTestingSupport theoryTestingSupport, Context context, int numberDisjuncts, int numberLiteralsPerDisjunct) {			
			for (int i = 0; i < numberDisjuncts; i++) {
				problemConjuncts.add(Or.make(IntStream.range(0, numberLiteralsPerDisjunct).mapToObj(idx -> theoryTestingSupport.makeRandomLiteral(context)).collect(Collectors.toList())));
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
				ExpressionLiteralSplitterStepSolver evaluatorStepSolver = context.getTheory().makeEvaluatorStepSolver(conjunct);
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
					if (context.getTheory().isLiteralOrBooleanConstant(conjunct, context)) {
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