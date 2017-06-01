package com.sri.ai.grinder.sgdpllt.anytime;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashSet;
import java.util.Random;
import java.util.Set;

import com.sri.ai.expresso.api.Expression;

public class VariableComponent {

	public Model model;
	public Expression variable;
	public Set<Expression> parent;
	public ArrayList<FactorComponent> children;
	public Set<Expression> cutsetOutsideSubModel;
	public Set<Expression> cutsetInsideSubModel;
	public Set<Expression> bound;
	public Set<Expression> phiInsideSubModel;

	public VariableComponent(Expression variable, Expression Parent, Model model, Set<Expression> Pext) {

		this.model = model;
		this.variable = variable;
		this.parent = new HashSet<Expression>();
		this.parent.add(Parent);
		this.phiInsideSubModel = new HashSet<Expression>();
		this.children = new ArrayList<FactorComponent>();
		this.cutsetInsideSubModel = new HashSet<Expression>();
		this.cutsetOutsideSubModel = new HashSet<Expression>();
		this.bound = new HashSet<Expression>();

		Set<Expression> intersection = new HashSet<Expression>();
		intersection.addAll(model.getNeighborsOfSet(model.getInitializedVariable()));
		Collection<Expression> S = model.getNeighbors(variable);
		for (Expression e : this.parent) {
			S.remove(e);
		}
		S.retainAll(intersection);
		if (!S.isEmpty()) {
			this.cutsetOutsideSubModel.add(variable);
		}

		model.initializeVariableComponent.add(this);
	}

	public void update(Set<Expression> Pext) {

		if (this.children.isEmpty()) {
			for (Expression e : this.model.getNeighbors(variable)) {
				if (!this.parent.contains(e)) {

					boolean test = false;

					for (FactorComponent c : model.initializeFactorComponent) {

						if (c.phi.equals(e)) {
							test = true;
							this.parent.add(c.phi);

						}
					}

					if (test == false) {
						FactorComponent newC = new FactorComponent(e, variable, model, Pext);
						this.children.add(newC);
						Set<Expression> intersection = new HashSet<Expression>();
						intersection.addAll(newC.cutsetOutsideSubModel);
						intersection.retainAll(model.getNeighborsOfSet(Pext));

						cutsetOutsideSubModel.addAll(intersection);

						cutsetInsideSubModel.addAll(newC.cutsetOutsideSubModel);
					}

				}
			}

			cutsetInsideSubModel.removeAll(cutsetOutsideSubModel);
		} else {
			int j = this.choose();
			Set<Expression> union = new HashSet<Expression>(Pext);
			for (int i = 0; i < this.children.size(); i++) {
				union.addAll(this.children.get(i).phiInsideSubModel);
			}
			this.children.get(j).update(union);

			Set<Expression> intersection = new HashSet<Expression>();
			intersection.addAll(this.children.get(j).cutsetOutsideSubModel);
			intersection.retainAll(model.getNeighborsOfSet(Pext));

			cutsetOutsideSubModel.addAll(intersection);

			cutsetInsideSubModel.addAll(this.children.get(j).cutsetOutsideSubModel);
			cutsetInsideSubModel.removeAll(cutsetOutsideSubModel);

			phiInsideSubModel.addAll(this.children.get(j).phiInsideSubModel);

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
		System.out.println(tab + "Variable : " + variable);
		System.out.println(tab + "cutset Outside SubModel : " + cutsetOutsideSubModel);
		System.out.println(tab + "cutset Inside SubModel : " + cutsetInsideSubModel);

		for (FactorComponent c : this.children) {
			c.print(tabs + 1);
		}

	}

}
