package anytimeExactBeliefPropagation;

import static com.sri.ai.expresso.helper.Expressions.apply;
import static com.sri.ai.expresso.helper.Expressions.makeSymbol;
import static com.sri.ai.expresso.helper.Expressions.parse;
import static com.sri.ai.grinder.helper.GrinderUtil.getIndexExpressionsOfFreeVariablesIn;
import static com.sri.ai.grinder.library.FunctorConstants.EQUAL;
import static com.sri.ai.grinder.library.FunctorConstants.IF_THEN_ELSE;
import static com.sri.ai.grinder.library.FunctorConstants.SUM;
import static com.sri.ai.grinder.library.FunctorConstants.TIMES;
import static com.sri.ai.util.Util.in;
import static com.sri.ai.util.Util.println;
import static java.lang.Math.min;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Random;
import java.util.Set;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.IndexExpressionsSet;
import com.sri.ai.expresso.api.IntensionalSet;
import com.sri.ai.expresso.api.Type;
import com.sri.ai.expresso.core.DefaultExtensionalMultiSet;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.grinder.api.Context;
import com.sri.ai.grinder.api.Theory;
import com.sri.ai.grinder.library.bounds.Bound;
import com.sri.ai.grinder.library.bounds.Bounds;
import com.sri.ai.grinder.library.bounds.DefaultExtensionalBound;
import com.sri.ai.util.base.Pair;
import com.sri.ai.util.base.Triple;
import com.sri.ai.util.collect.ManyToManyRelation;

import anytimeExactBeliefPropagation.Model.Model;
import anytimeExactBeliefPropagation.Model.Node.FactorNode;
import anytimeExactBeliefPropagation.Model.Node.VariableNode;

/**
 *  A class that provides a handful number of graphical models, under the form of {@link Set}s of  {@link Expression}s
 * 
 * TODO Add "seeds" for repeatability
 * TODO for binary query nodes, add max-min probability of truth
 * 
 * @author Gabriel Azevedo Ferreira
 */
@Beta
public class ModelGenerator {
	
	private static Expression randomNumberGenerator() {
		 Random randomGenerator = new Random();
		 return makeSymbol(randomGenerator.nextInt(9));
	}
	
	private static Expression generateProbability(Context context,int i, Expression... listOfVariables) {
		if (listOfVariables.length == 0 ) {
			return null;
		}
		Expression result = makeSymbol(0);// randomNumberGenerator();
		
		Expression variable = listOfVariables[i];
		// listOfVariables.remove(0);
		Type type = context.getTypeOfRegisteredSymbol(variable);
		Iterator<Expression>  iteratorToValuesInType = type.iterator();
		
		if (listOfVariables.length == i+1) {
			for (Expression value : in(iteratorToValuesInType)) {
				Expression varEqualsValue = apply(EQUAL,variable,value);
				Expression randProbability = randomNumberGenerator();
				result = apply(IF_THEN_ELSE,varEqualsValue, randProbability, result);
			}
		}
		else{
			for (Expression value : in(iteratorToValuesInType)) {
				Expression varEqualsValue = apply(EQUAL,variable,value);
				Expression randProbability = generateProbability(context, i+1, listOfVariables);
				result = apply(IF_THEN_ELSE,varEqualsValue, randProbability, result);
			}
		}
		return result;
	}

	/**
	 * Generates a random factor (a non normalized probability distribution) over a set of variables
	 * @param context
	 * @param listOfVariables
	 * @return
	 */
	public static Expression generateProbability(Context context, Expression... listOfVariables) {
		return generateProbability(context, 0, listOfVariables);
	}
	
	/**
	 * Generates an Ising model with random probabilities
	 * @param nLines
	 * @param nCols
	 * @param context
	 * @param possibleValues 
	 * 			Example: Boolean, 1..6
	 * @return
	 */
	public static Triple<Set<Expression>,Context,Expression> IsingModel(int nLines, int nCols, Context context, Expression possibleValues) {
		Set<Expression> factorsInModel = new HashSet<Expression>();
		
		Expression[][] a = new Expression[nLines][nCols];
		
		for (int i = 0; i < nLines; i++) {
			for (int j = 0; j < nCols; j++) {
				a[i][j] = makeSymbol("A_"+i+"_"+j);
				context = context.extendWithSymbolsAndTypes(a[i][j],possibleValues);
			}
		}
		
		for (int i = 0; i < nLines; i++) {
			for (int j = 0; j < nCols; j++) {
				if (j < nCols - 1) {
					Expression fHor = generateProbability(context, a[i][j], a[i][j+1]);
					factorsInModel.add(fHor);
				}
				if (i < nLines-1) {
					Expression fVer = generateProbability(context, a[i][j], a[i+1][j]);
					factorsInModel.add(fVer);
				}
			}
		}		
		Triple<Set<Expression>,Context,Expression> result = new Triple<>(factorsInModel,context,a[0][0]);
		return result;
	}
	
	/**
	 * Generates a random model with a given number of variables
	 * 
	 * @param nVariables
	 * @param nFactors
	 * @param context
	 * @param possibleValues
	 * @return
	 */
	public static Triple<Set<Expression>,Context,Expression> randomModel(int nVariables, int nFactors, Context context, Expression possibleValues) {
		Set<Expression> factorsInModel = new HashSet<Expression>();
		
		Expression[] a = new Expression[nVariables];
		for (int i = 0; i < nVariables; i++) {
			a[i]= makeSymbol("A_"+i);
			context = context.extendWithSymbolsAndTypes(a[i],possibleValues);
		}
		Random rand = new Random();
		
		for (int i = 0; i < nFactors; i++) {
			int n = rand.nextInt(min(15,nVariables));
			Expression[] varOfF = new Expression[n];
			for (int j = 0; j < varOfF.length; j++) {
				varOfF[j] = a[rand.nextInt(nVariables)];
			}
			Expression f = generateProbability(context, varOfF);
			factorsInModel.add(f);
		}
		Triple<Set<Expression>,Context,Expression> result = new Triple<>(factorsInModel,context,a[0]);
		return result;
		
	}

	/**
	 * Creates a model that looks like a queue of factors and variables
	 * 		Q - F - V - F -...- F - V 
	 * @param nVariables
	 * @param context
	 * @param possibleValues
	 * @return
	 */
	public static Triple<Set<Expression>,Context,Expression> lineModel(int nVariables, Context context, Expression possibleValues) {
		Set<Expression> factorsInModel = new HashSet<Expression>();
		
		Expression[] a = new Expression[nVariables];
		for (int i = 0; i < nVariables; i++) {
			a[i]= makeSymbol("A_"+i);
			context = context.extendWithSymbolsAndTypes(a[i],possibleValues);
		}
		for (int i = 0; i < a.length - 1; i++) {
			Expression factor = generateProbability(context, a[i],a[i+1]);
			factorsInModel.add(factor);
		}
		Triple<Set<Expression>,Context,Expression> result = new Triple<>(factorsInModel,context,a[0]);
		return result;
	}
	
	/**
	 * creates a tree with n children per node
	 * @param depth
	 * @param numberOfChildren
	 * @param context
	 * @param possibleValues
	 * @return
	 */
	public static Triple<Set<Expression>,Context,Expression> nTreeModel(int depth , int numberOfChildren, Context context, Expression possibleValues) {
		Set<Expression> factorsInModel = new HashSet<Expression>();
		
		int nCols = 1;
		for (int i = 0; i < depth; i++) {
			nCols *= numberOfChildren; 
		}
		
		Expression[][] a = new Expression[depth][nCols];
		for (int i = 0; i < depth; i++) {
			for (int j = 0; j < nCols; j++) {				
				a[i][j]= makeSymbol("A_" + i + "_" + j);
				context = context.extendWithSymbolsAndTypes(a[i][j],possibleValues);
			}
		}
		
		int colMax = 1;
		for (int i = 0; i < depth - 1; i++) {
			for (int j = 0; j < colMax; j++) {
				for (int k = j * numberOfChildren; k < (j+1) * numberOfChildren; k++) {
					Expression factor = generateProbability(context, a[i][j],a[i+1][k]);
					factorsInModel.add(factor);
				}
			}
			colMax*=numberOfChildren;
		}
		Triple<Set<Expression>,Context,Expression> result = new Triple<>(factorsInModel,context,a[0][0]);
		return result;
	}
	
	/**
	 * Lifited variabble Elimination
	 * this method just creates the inference equation and asks SGDPLL to solve it
	 * @param m
	 * @return
	 */
	public static Expression lveCalculation(Model m) {
		return LVECalculation(m.getEntireGraph().getBs(),m.getQuery().getValue(),m.getContext(),m.getTheory());
	}
	
	public static Expression LVECalculation(Collection<FactorNode> factorNodes, Expression query, Context context, Theory theory) {
		Set<Expression> factorExpressions = new HashSet<>();
		for (FactorNode f : factorNodes) {
			factorExpressions.add(f.getValue());
		}
		Expression product = apply(TIMES, factorExpressions);
		Set<Expression> freevariables = Expressions.freeVariables(product, context);
		freevariables.remove(query);
		
		ArrayList<Expression> varToSumOut = new ArrayList<>();
		varToSumOut.addAll(freevariables);
		Expression variablesToBeSummedOut = new DefaultExtensionalMultiSet(varToSumOut);
		
		IndexExpressionsSet indices = getIndexExpressionsOfFreeVariablesIn(variablesToBeSummedOut, context);
		
		Expression setOfFactorInstantiations = IntensionalSet.makeMultiSet(
				indices,
				product,// head
				makeSymbol(true)// No Condition
				);
		
		Expression sumOnPhi = apply(SUM, setOfFactorInstantiations);
		Expression evaluation = theory.evaluate(sumOnPhi, context);
		
		Expression result = Bounds.normalizeSingleExpression(evaluation, theory, context);
		
		return result;
	}
	
	public static void printModel(Model m, boolean entire) {
		ManyToManyRelation<VariableNode, FactorNode> graph = entire ? m.getEntireGraph() : m.getExploredGraph();
		for (Pair<VariableNode, FactorNode> e : in(graph.iterator())) {
			println(e.second.getValue() + " -> " + e.first.getValue());
		}
	}
	
	public static void printFactors(Set<FactorNode> factorsInModel) {
		for (FactorNode e : factorsInModel) {
			println(e.getValue());
		}
	}
	
	public static void printModel(Set<Expression> factorsInModel) {
		for (Expression e : factorsInModel) {
			println(e);
		}
	}

	public static Pair<Double,Double> maxMinProbability(Bound b,Model m) {
		Context context = m.getContext();
		Theory theory = m.getTheory();
		Expression query = m.getQuery().getValue();
		Type type = context.getTypeOfRegisteredSymbol(query);
		
		if (type.getName().equals("Boolean")) {
			double maxProbabilityOfTrue = -1;
			double minProbabilityOfTrue = 10;
			
			if (b.isExtensionalBound()) {
				DefaultExtensionalBound extensionalBound = (DefaultExtensionalBound) b;
				List<Expression> listOfElements = extensionalBound.getArguments();
				for (Expression distribution : listOfElements) {
					// replace and evaluate
					Expression replacingQueryByTrue = distribution.replaceAllOccurrences(query, parse("true"), context);
					Expression evaluating = theory.evaluate(replacingQueryByTrue, context);
					// convert to double
					double value = evaluating.doubleValue();
					// update max and min
					if (value > maxProbabilityOfTrue) {
						maxProbabilityOfTrue = value;
					}
					if (value < minProbabilityOfTrue) {
						minProbabilityOfTrue = value;
					}
				}
				Pair<Double,Double> result = new Pair<>(minProbabilityOfTrue,maxProbabilityOfTrue);
				return result;
			}
			else if (b.isIntensionalBound()) {
				
			}
			
		}
		
		return null;		
	}
}
