package com.sri.ai.grinder.sgdpllt.anytime;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashSet;
import java.util.Random;
import java.util.Set;

import com.sri.ai.expresso.api.Expression;

public class VariableComponent {

	public Model M;
	public Expression V;
	public Set<Expression> parent;
	public ArrayList<FactorComponent> children;
	public Set<Expression> Dext;
	public Set<Expression> D;
	public Set<Expression> B;
	public Set<Expression> Pint;

	public VariableComponent(Expression V, Expression Parent, Model M, Set<Expression> Pext) {

		this.M = M;
		this.V = V;
		this.parent = new HashSet<Expression>();
		this.parent.add(Parent);
		this.Pint = new HashSet<Expression>();
		this.children = new ArrayList<FactorComponent>();
		this.D = new HashSet<Expression>();
		this.Dext = new HashSet<Expression>();
		this.B = new HashSet<Expression>();

		Set<Expression> intersection = new HashSet<Expression>();
		intersection.addAll(M.getNeighborsOfSet(M.getInitializedVariable()));
		Collection<Expression> S = M.getNeighbors(V);
		for (Expression e : this.parent) {
			S.remove(e);
		}
		S.retainAll(intersection);
		if (!S.isEmpty()) {
			this.Dext.add(V);
		}

		M.initializeVComponent.add(this);

		// this.B =
	}

	public void update(Set<Expression> Pext) {

		if (this.children.isEmpty()) {
			for (Expression e : this.M.getNeighbors(V)) {
				if (!this.parent.contains(e)) {

					boolean test = false;

					for (FactorComponent c : M.initializeFComponent) {
						if (c.phi.equals(e)) {
							test = true;
							this.parent.add(c.phi);
						}
					}

					if (test == false) {
						FactorComponent newC = new FactorComponent(e, V, M, Pext);
						this.children.add(newC);
						Set<Expression> intersection = new HashSet<Expression>();
						intersection.addAll(newC.Dext);
						intersection.retainAll(M.getNeighborsOfSet(Pext));

						Dext.addAll(intersection);

						D.addAll(newC.Dext);
					}

				}
			}

			D.removeAll(Dext);
		} else {
			int j = this.choose();
			Set<Expression> union = new HashSet<Expression>(Pext);
			for (int i = 0; i < this.children.size(); i++) {
				union.addAll(this.children.get(i).Pint);
			}
			this.children.get(j).update(union);

			Set<Expression> intersection = new HashSet<Expression>();
			intersection.addAll(this.children.get(j).Dext);
			intersection.retainAll(M.getNeighborsOfSet(Pext));

			Dext.addAll(intersection);

			D.addAll(this.children.get(j).Dext);
			D.removeAll(Dext);

			Pint.addAll(this.children.get(j).Pint);

			// B = sum_{D} product Bk
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
		System.out.println(tab + "Variable : " + V);
		System.out.println(tab + "Dext : " + Dext);
		System.out.println(tab + "D : " + D);

		for (FactorComponent c : this.children) {
			c.print(tabs + 1);
		}

	}

}
