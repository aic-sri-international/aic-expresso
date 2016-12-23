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
import com.sri.ai.grinder.sgdpllt.api.ExpressionLiteralSplitterStepSolver;
import com.sri.ai.grinder.sgdpllt.api.ExpressionStepSolver;
import com.sri.ai.grinder.sgdpllt.core.constraint.ContextSplitting;
import com.sri.ai.grinder.sgdpllt.core.solver.ContextDependentExpressionProblemSolver;
import com.sri.ai.grinder.sgdpllt.core.solver.ExpressionStepSolverToLiteralSplitterStepSolverAdapter;
import com.sri.ai.grinder.sgdpllt.library.boole.And;
import com.sri.ai.grinder.sgdpllt.library.boole.Not;
import com.sri.ai.grinder.sgdpllt.library.boole.Or;
import com.sri.ai.grinder.sgdpllt.tester.TheoryTestingSupport;
import com.sri.ai.grinder.sgdpllt.theory.compound.CompoundTheory;
import com.sri.ai.grinder.sgdpllt.theory.differencearithmetic.DifferenceArithmeticTheory;
import com.sri.ai.grinder.sgdpllt.theory.equality.EqualityTheory;
import com.sri.ai.grinder.sgdpllt.theory.linearrealarithmetic.LinearRealArithmeticTheory;
import com.sri.ai.grinder.sgdpllt.theory.propositional.PropositionalTheory;

public class ExpressionStepSolverToLiteralSplitterStepSolverAdapterTest {
	
	@Test
	public void testPropositionalTheoryWithFixedDisjunctiveFormulas() {
		TheoryTestingSupport theoryTestingSupport = TheoryTestingSupport.make(new PropositionalTheory());	
		extendTestingVaribles("P", theoryTestingSupport, "S", "T");
		
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
		TheoryTestingSupport theoryTestingSupport = TheoryTestingSupport.make(new PropositionalTheory());		
		extendTestingVaribles("P", theoryTestingSupport, "S", "T", "U", "V", "W", "X", "Y", "Z");
		
		runRandomDisjunctiveFormulasTest(theoryTestingSupport);
	}
	
	@Ignore("Random generation of linear real arithmetic not yet implemented")
	@Test
	public void testLinearRealArithmeticTheoryWithRandomDisjunctiveFormulas() {
		TheoryTestingSupport theoryTestingSupport = TheoryTestingSupport.make(new LinearRealArithmeticTheory(true, true));
	
		extendTestingVaribles("X", theoryTestingSupport, "S", "T", "U", "V", "W");
		
		runRandomDisjunctiveFormulasTest(theoryTestingSupport);
	}
	
	@Test
	public void testEqualityTheoryWithoutPropagationOfAllLiteralsWhenBoundWithRandomDisjunctiveFormulas() {
		TheoryTestingSupport theoryTestingSupport = TheoryTestingSupport.make(new EqualityTheory(true, false));
	
		extendTestingVaribles("X", theoryTestingSupport, "S", "T", "U", "V", "W");
		
		runRandomDisjunctiveFormulasTest(theoryTestingSupport);
	}
	
	@Test
	public void testEqualityTheoryWithPropagationOfAllLiteralsWhenBoundWithRandomDisjunctiveFormulas() {
		TheoryTestingSupport theoryTestingSupport = TheoryTestingSupport.make(new EqualityTheory(true, true));
	
		extendTestingVaribles("X", theoryTestingSupport, "S", "T", "U", "V", "W");
		
		runRandomDisjunctiveFormulasTest(theoryTestingSupport);
	}
	
	@Test
	public void testDifferenceArithmeticTheoryWithoutPropagationOfAllLiteralsWhenBoundWithRandomDisjunctiveFormulas() {
		TheoryTestingSupport theoryTestingSupport = TheoryTestingSupport.make(new DifferenceArithmeticTheory(true, false));
	
		extendTestingVaribles("K", theoryTestingSupport, "L", "M", "N", "O", "P");
		
		runRandomDisjunctiveFormulasTest(theoryTestingSupport);
	}
	
	@Test
	public void testDifferenceArithmeticTheoryWithPropagationOfAllLiteralsWhenBoundWithRandomDisjunctiveFormulas() {
		TheoryTestingSupport theoryTestingSupport = TheoryTestingSupport.make(new DifferenceArithmeticTheory(true, true));
	
		extendTestingVaribles("K", theoryTestingSupport, "L", "M", "N", "O", "P");
		
		runRandomDisjunctiveFormulasTest(theoryTestingSupport);
	}
	
	@Test
	public void testCompoundTheoryWithDifferenceArithmeticWithRandomDisjunctiveFormulas() {
		TheoryTestingSupport theoryTestingSupport = TheoryTestingSupport.make(new CompoundTheory(
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
		TheoryTestingSupport theoryTestingSupport = TheoryTestingSupport.make(new CompoundTheory(
				new EqualityTheory(false, true),
				new DifferenceArithmeticTheory(false, true),
				new PropositionalTheory()));
		
		runRandomDisjunctiveFormulasTest(theoryTestingSupport);
	}
	
	//
	//
	
	private void extendTestingVaribles(String variableToBaseTypeOn, TheoryTestingSupport theoryTestingSupport, String... newVariables) {
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
		ExpressionStepSolverToLiteralSplitterStepSolverAdapter stepSolver = new ExpressionStepSolverToLiteralSplitterStepSolverAdapter(generalFormulaExpressionTestStepSolver);
		System.out.println("Evaluating " + generalFormulaExpressionTestStepSolver.getExpressionToSolve());
		Expression solution = ContextDependentExpressionProblemSolver.staticSolve(stepSolver, context);
		System.out.println(generalFormulaExpressionTestStepSolver.getExpressionToSolve() + " -----> " + solution + "\n");
		
		if (expected != null) {
			assertEquals(expected, solution);
		}
		
		Expression evaluation = theoryTestingSupport.getTheory().evaluate(solution, context);
//		Expression evaluation = new Evaluator(theoryTestingSupport.getTheory()).apply(solution, context);
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