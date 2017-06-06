package com.sri.ai.grinder.sgdpllt.anytime;

import com.sri.ai.util.collect.ManyToManyRelation;

import static com.sri.ai.grinder.helper.GrinderUtil.BOOLEAN_TYPE;
import static com.sri.ai.expresso.helper.Expressions.apply;
import static com.sri.ai.grinder.sgdpllt.library.FunctorConstants.TIMES;
import static com.sri.ai.expresso.helper.Expressions.parse;

import java.util.Collection;
import java.util.HashSet;
import java.util.Set;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.core.DefaultSymbol;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.grinder.sgdpllt.api.Context;
import com.sri.ai.grinder.sgdpllt.api.Theory;
import com.sri.ai.grinder.sgdpllt.core.TrueContext;
import com.sri.ai.grinder.sgdpllt.library.controlflow.IfThenElse;
import com.sri.ai.grinder.sgdpllt.theory.compound.CompoundTheory;
import com.sri.ai.grinder.sgdpllt.theory.differencearithmetic.DifferenceArithmeticTheory;
import com.sri.ai.grinder.sgdpllt.theory.equality.EqualityTheory;
import com.sri.ai.grinder.sgdpllt.theory.linearrealarithmetic.LinearRealArithmeticTheory;
import com.sri.ai.grinder.sgdpllt.theory.propositional.PropositionalTheory;
import com.sri.ai.grinder.sgdpllt.theory.tuple.TupleTheory;

public class Model {
	public ManyToManyRelation<Expression, Expression> map;
	//public ManyToManyRelation<Expression, String> mapVariablesToType;
	//public ManyToManyRelation<Expression, Expression> mapVariablesToValuesTaken;
	public Set<VariableComponent> initializeVariableComponent;
	public Set<FactorComponent> initializeFactorComponent;
	public Context context;
	public Theory theory;

	public Model(Set<Expression> Factor) {
		this.map = new ManyToManyRelation<Expression, Expression>();
		//this.mapVariablesToType	 = new ManyToManyRelation<Expression, String>();
		//this.mapVariablesToValuesTaken	 = new ManyToManyRelation<Expression, Expression>();
		this.initializeFactorComponent = new HashSet<FactorComponent>();
		this.initializeVariableComponent = new HashSet<VariableComponent>();
		this.theory = new CompoundTheory(
				new EqualityTheory(false, true),
				new DifferenceArithmeticTheory(false, false),
				new LinearRealArithmeticTheory(false, false),
				new TupleTheory(),
				new PropositionalTheory());
		this.context = new TrueContext(theory);			
		context = context.add(BOOLEAN_TYPE);
		

		Context context = new TrueContext();
		for (Expression factor : Factor) {
			for (Expression variable : Expressions.freeVariables(factor, context)) {
				map.add(factor, variable);
			}
		}
	}

	/*public void setType(Expression expression, String typeOfVariable) {
		this.mapVariablesToType.add(expression, typeOfVariable);
	}*/
	
	/*public void setValues(Expression expression, Expression ValuesTakenByVariable) {
		this.mapVariablesToValuesTaken.add(expression, ValuesTakenByVariable);
	}*/
	
	/*public String getType(Expression expression) {
		Collection < String > collectionOfString =this.mapVariablesToType.getBsOfA(expression);
		for(String string : collectionOfString){
			return string;
		}
		return("no type has been defined for this variable");		
	}*/
	
	public Expression getValues(Expression variable) {
		Expression values = this.context.getTypeExpressionOfRegisteredSymbol(variable); //this.getValues(variable);
		return values;
	}
	
	public void extendModelWithSymbolsAndTypes(String symbol, String values){
		this.context = this.context.extendWithSymbolsAndTypes(symbol, values);
	}

	
	public Set<Expression> getNeighbors(Expression expression) {
		Set<Expression> neighbors = new HashSet<Expression>();
		if (expression.getFunctor() == null) {
			neighbors.addAll(map.getAsOfB(expression));
			return neighbors;
		} else {
			neighbors.addAll(map.getBsOfA(expression));
			return neighbors;
		}
	}

	public Set<Expression> getNeighborsOfSet(Set<Expression> expressionSet) {
		Set<Expression> neighbors = new HashSet<Expression>();
		if (expressionSet == null) {
			return neighbors;
		}
		for (Expression expression : expressionSet) {
			neighbors.addAll(getNeighbors(expression));
		}
		return neighbors;
	}

	public Collection<Expression> getFactor() {
		return map.getAs();
	}

	public Collection<Expression> getVariable() {
		return map.getBs();
	}

	public void printInitialized() {
		System.out.println("Variables : ");
		for (VariableComponent variable : this.initializeVariableComponent) {
			System.out.println("\t" + variable.variable);
			System.out.println("\t\t" + "Parents" + variable.parent);
			System.out.println("\t\t" + "cutset Outside SubModel" + variable.cutsetOutsideSubModel);
			System.out.println("\t\t" + "cutset Inside SubModel" + variable.cutsetInsideSubModel);
		}
		System.out.println("Factor : ");
		for (FactorComponent factor : this.initializeFactorComponent) {
			System.out.println("\t" + factor.phi);
			System.out.println("\t\t" + "Parents" + factor.parent);
			System.out.println("\t\t" + "cutset Outside SubModel" + factor.cutsetOutsideSubModel);
			System.out.println("\t\t" + "cutset Inside SubModel" + factor.cutsetInsideSubModel);
		}

	}

	public Set<Expression> getInitializedFactor() {
		Set<Expression> factorSet = new HashSet<Expression>();
		for (FactorComponent factor : this.initializeFactorComponent) {

			factorSet.add(factor.phi);

		}
		return factorSet;
	}

	public Set<Expression> getInitializedVariable() {
		Set<Expression> variableSet = new HashSet<Expression>();
		for (VariableComponent variable : this.initializeVariableComponent) {
			variableSet.add(variable.variable);
		}
		return variableSet;
	}
	
	public void addConditions(Set<Expression> condition){
		Set<Expression> Factor = new HashSet<Expression>(this.getFactor());
		for(Expression singleCondition : condition){
			Expression factorCondition = IfThenElse.make(singleCondition, parse("1"), parse("0"));
			Factor.add(factorCondition);
		}
		if(!condition.isEmpty()){
			Model m = new Model(Factor);
			this.map = m.map;
		}
	}

	public Expression naiveCalculation(Expression query){

		Expression factorProduct = parse("1");
		for (Expression factor : this.getFactor()){
				factorProduct = apply(TIMES, factor, factorProduct);
		}
		
		Expression summedProduct = factorProduct;
		for (Expression variable : this.getVariable()){
			if (variable != query){
				Expression values = this.context.getTypeExpressionOfRegisteredSymbol(variable); //this.getValues(variable);
				//to change
				String string = "sum({{ (on " + variable + " in " + values +" ) " + summedProduct + " }})";
				summedProduct = theory.evaluate(parse(string), context);
			}
		}
		return summedProduct;
	}
}
