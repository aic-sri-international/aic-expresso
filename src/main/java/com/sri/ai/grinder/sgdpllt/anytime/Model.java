package com.sri.ai.grinder.sgdpllt.anytime;

import com.sri.ai.util.collect.ManyToManyRelation;
import static com.sri.ai.expresso.helper.Expressions.apply;
import static com.sri.ai.expresso.helper.Expressions.makeSymbol;
import static com.sri.ai.grinder.sgdpllt.library.FunctorConstants.IF_THEN_ELSE;
import static com.sri.ai.grinder.sgdpllt.library.FunctorConstants.EQUAL;
import static com.sri.ai.grinder.sgdpllt.library.FunctorConstants.GREATER_THAN;

import java.util.List;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashSet;
import java.util.Set;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.core.DefaultSymbol;
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
		for (Expression f : Factor) {
			for (Expression v : Expressions.freeVariables(f, context)) {
				map.add(f, v);
			}
		}
	}

	public static Set<Expression> getVariables(Expression f) {
		Set<Expression> res = new HashSet<Expression>();
		for (Expression e : f.getArguments()) {
			if (e.getFunctor() == null) {
				res.add(e);
			} else {
				res.addAll(getVariables(e));
			}
		}
		return res;
	}

	public Set<Expression> getNeighbors(Expression e) {
		Set<Expression> res = new HashSet<Expression>();
		if (e.getFunctor() == null) {
			res.addAll(map.getAsOfB(e));
			return res;
		} else {
			res.addAll(map.getBsOfA(e));
			return res;
		}
	}

	public Set<Expression> getNeighborsOfSet(Set<Expression> E) {
		Set<Expression> res = new HashSet<Expression>();
		if (E == null) {
			return res;
		}
		for (Expression e : E) {
			res.addAll(getNeighbors(e));
		}
		return res;
	}

	public Collection<Expression> getFactor() {
		return map.getAs();
	}

	public Collection<Expression> getVariable() {
		return map.getBs();
	}

	public void printInitialized() {
		System.out.println("Variables : ");
		for (VariableComponent v : this.initializeVComponent) {
			System.out.println("\t" + v.V);
			System.out.println("\t\t" + "Parents" + v.parent);
			System.out.println("\t\t" + "Dext" + v.Dext);
			System.out.println("\t\t" + "D" + v.D);
		}
		System.out.println("Factor : ");
		for (FactorComponent p : this.initializeFComponent) {
			System.out.println("\t" + p.phi);
			System.out.println("\t\t" + "Parents" + p.parent);
			System.out.println("\t\t" + "Dext" + p.Dext);
			System.out.println("\t\t" + "D" + p.D);
		}

	}

	public Set<Expression> getInitializedFactor() {
		Set<Expression> res = new HashSet<Expression>();
		for (FactorComponent f : this.initializeFComponent) {
			res.add(f.phi);
		}
		return res;
	}

	public Set<Expression> getInitializedVariable() {
		Set<Expression> res = new HashSet<Expression>();
		for (VariableComponent v : this.initializeVComponent) {
			res.add(v.V);
		}
		return res;
	}
}
