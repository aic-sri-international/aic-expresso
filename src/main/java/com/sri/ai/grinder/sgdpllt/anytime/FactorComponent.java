package com.sri.ai.grinder.sgdpllt.anytime;

import static com.sri.ai.expresso.helper.Expressions.apply;
import static com.sri.ai.grinder.sgdpllt.library.FunctorConstants.EQUAL;
import static com.sri.ai.grinder.sgdpllt.library.FunctorConstants.GREATER_THAN;
import static com.sri.ai.grinder.sgdpllt.library.FunctorConstants.IF_THEN_ELSE;

import java.lang.reflect.Array;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashSet;
import java.util.Random;
import java.util.Set;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.core.DefaultSymbol;

public class FactorComponent {

	public Model M;
	public Expression Phi;
	public Set<Expression> Parent;
	public ArrayList<VariableComponent> children;
	public Set<Expression> Dext;
	public Set<Expression> D;
	// public Bound B;
	public Set<Expression> Pint;

	public FactorComponent(Expression Phi, Expression Parent, Model M, Set<Expression> Pext) {

		this.Pint = new HashSet<Expression>();
		this.M = M;
		this.children = new ArrayList<VariableComponent>();
		this.Parent = new HashSet<Expression>();
		this.Parent.add(Parent);
		this.D = new HashSet<Expression>();
		this.Dext = new HashSet<Expression>();
		// this.B = new HashSet<Expression>();
		// this.B = Simplex(V)

		this.Phi = Phi;
		this.Pint.add(Phi);

		Set<Expression> intersection = new HashSet<Expression>();
		intersection.addAll(M.getNeighborsOfSet(M.getInitializedFactor()));
		Collection<Expression> S = M.getNeighbors(Phi);
		for (Expression e : this.Parent) {
			S.remove(e);
		}
		S.retainAll(intersection);
		this.Dext.addAll(S);
		M.initializeFComponent.add(this);

	}

	public void update(Set<Expression> Pext) {

		if (this.children.isEmpty()) {
			for (Expression e : this.M.getNeighbors(Phi)) {
				if (!this.Parent.contains(e)) {
					Set<Expression> union = new HashSet<Expression>(Pext);
					union.add(Phi);

					boolean test = false;

					for (VariableComponent c : M.initializeVComponent) {
						if (c.V.equals(e)) {
							test = true;
							this.Parent.add(c.V);
						}
					}

					if (test == false) {
						VariableComponent newV = new VariableComponent(e, Phi, M, union);
						this.children.add(newV);
						Set<Expression> intersection = new HashSet<Expression>(newV.Dext);
						intersection.retainAll(M.getNeighborsOfSet(Pext));
						Dext.addAll(intersection);

						D.addAll(newV.Dext);
					}
				}

				D.removeAll(Dext);
			}
		} else {
			int j = this.choose();
			Set<Expression> union = new HashSet<Expression>(Pext);
			for (int i = 0; i < this.children.size(); i++) {
				union.addAll(this.children.get(i).Pint);
			}
			this.children.get(j).update(union);

			Set<Expression> intersection = new HashSet<Expression>(this.children.get(j).Dext);
			intersection.retainAll(M.getNeighborsOfSet(Pext));
			Dext.addAll(intersection);

			D.addAll(this.children.get(j).Dext);
			D.removeAll(Dext);

			Pint.addAll(this.children.get(j).Pint);

			// B =
		}
	}

	public int choose() {
		Random rn = new Random();
		return rn.nextInt(this.children.size());
		// return 0;
	}

	public void print(int tabs) {
		String tab = new String();
		for (int i = 0; i < tabs; i++) {
			tab += "\t";
		}
		System.out.println(tab + "Factor : " + Phi);
		System.out.println(tab + "Dext : " + Dext);
		System.out.println(tab + "D : " + D);

		for (VariableComponent c : this.children) {
			c.print(tabs + 1);
		}

	}

}
