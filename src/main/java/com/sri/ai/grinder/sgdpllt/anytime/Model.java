package com.sri.ai.grinder.sgdpllt.anytime;

import com.sri.ai.util.collect.ManyToManyRelation;

import java.util.Collection;
import java.util.HashSet;
import java.util.Set;

import com.sri.ai.expresso.api.Expression;

import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.grinder.sgdpllt.api.Context;
import com.sri.ai.grinder.sgdpllt.core.TrueContext;

public class Model {
	public ManyToManyRelation<Expression, Expression> map;
	public Set<VariableComponent> initializeVComponent;
	public Set<FactorComponent> initializeFComponent;

	public Model(Set<Expression> Factor) {
		this.map = new ManyToManyRelation<Expression, Expression>();
		this.initializeFComponent = new HashSet<FactorComponent>();
		this.initializeVComponent = new HashSet<VariableComponent>();

		Context context = new TrueContext();
		for (Expression factor : Factor) {
			for (Expression variable : Expressions.freeVariables(factor, context)) {
				map.add(factor, variable);
			}
		}
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
		for (VariableComponent variable : this.initializeVComponent) {
			System.out.println("\t" + variable.V);
			System.out.println("\t\t" + "Parents" + variable.parent);
			System.out.println("\t\t" + "Dext" + variable.Dext);
			System.out.println("\t\t" + "D" + variable.D);
		}
		System.out.println("Factor : ");
		for (FactorComponent factor : this.initializeFComponent) {
			System.out.println("\t" + factor.phi);
			System.out.println("\t\t" + "Parents" + factor.parent);
			System.out.println("\t\t" + "Dext" + factor.Dext);
			System.out.println("\t\t" + "D" + factor.D);
		}

	}

	public Set<Expression> getInitializedFactor() {
		Set<Expression> factorSet = new HashSet<Expression>();
		for (FactorComponent factor : this.initializeFComponent) {

			factorSet.add(factor.phi);

		}
		return factorSet;
	}

	public Set<Expression> getInitializedVariable() {
		Set<Expression> variableSet = new HashSet<Expression>();
		for (VariableComponent variable : this.initializeVComponent) {
			variableSet.add(variable.V);
		}
		return variableSet;
	}
}
