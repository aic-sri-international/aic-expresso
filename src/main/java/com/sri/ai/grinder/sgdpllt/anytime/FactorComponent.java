package com.sri.ai.grinder.sgdpllt.anytime;


import java.util.ArrayList;
import java.util.Collection;
import java.util.HashSet;
import java.util.Set;

import com.sri.ai.expresso.api.Expression;

public class FactorComponent {

	public Model model;
	public Expression phi;
	public Set<Expression> parent;
	public boolean entirelyDiscover;
	public ArrayList<VariableComponent> children;
	public Set<Expression> cutsetOutsideSubModel;
	public Set<Expression> cutsetInsideSubModel;
	// public Bound B;
	public Set<Expression> phiInsideSubModel;

	public FactorComponent(Expression phi, Expression Parent, Model model, Set<Expression> Pext) {

		this.phiInsideSubModel = new HashSet<Expression>();
		this.model = model;
		this.entirelyDiscover = false;
		this.children = new ArrayList<VariableComponent>();
		this.parent = new HashSet<Expression>();
		this.parent.add(Parent);
		this.cutsetInsideSubModel = new HashSet<Expression>();
		this.cutsetOutsideSubModel = new HashSet<Expression>();
		// this.B = new HashSet<Expression>();
		// this.B = Simplex(V)

		this.phi = phi;
		this.phiInsideSubModel.add(phi);

		Set<Expression> intersection = new HashSet<Expression>();
		intersection.addAll(model.getNeighborsOfSet(model.getInitializedFactor()));
		Collection<Expression> S = model.getNeighbors(phi);
		for (Expression e : this.parent) {
			S.remove(e);
		}
		if (S.isEmpty()){
			this.entirelyDiscover = true;
		}
		S.retainAll(intersection);
		this.cutsetOutsideSubModel.addAll(S);
		model.initializeFactorComponent.add(this);

	}

	public void update(Set<Expression> Pext) {

		if (this.children.isEmpty()) {
			for (Expression e : this.model.getNeighbors(phi)) {
				if (!this.parent.contains(e)) {
					Set<Expression> union = new HashSet<Expression>(Pext);
					union.add(phi);

					boolean test = false;

					for (VariableComponent c : model.initializeVariableComponent) {
						if (c.variable.equals(e)) {
							test = true;
							this.parent.add(c.variable);
						}
					}

					if (test == false) {
						VariableComponent newV = new VariableComponent(e, phi, model, union);
						this.children.add(newV);
						Set<Expression> intersection = new HashSet<Expression>(newV.cutsetOutsideSubModel);
						intersection.retainAll(model.getNeighborsOfSet(Pext));
						cutsetOutsideSubModel.addAll(intersection);

						cutsetInsideSubModel.addAll(newV.cutsetOutsideSubModel);
					}
				}

				cutsetInsideSubModel.removeAll(cutsetOutsideSubModel);
			}
		} else {
			int j = this.choose();
			Set<Expression> union = new HashSet<Expression>(Pext);
			for (int i = 0; i < this.children.size(); i++) {
				union.addAll(this.children.get(i).cutsetInsideSubModel);
			}
			this.children.get(j).update(union);

			Set<Expression> intersection = new HashSet<Expression>(this.children.get(j).cutsetOutsideSubModel);
			intersection.retainAll(model.getNeighborsOfSet(Pext));
			cutsetOutsideSubModel.addAll(intersection);

			cutsetInsideSubModel.addAll(this.children.get(j).cutsetOutsideSubModel);
			cutsetInsideSubModel.removeAll(cutsetOutsideSubModel);

			phiInsideSubModel.addAll(this.children.get(j).cutsetInsideSubModel);

			// B =
		}
		
		
		boolean isChildrenDiscovered = true;
		for (VariableComponent children : this.children){
			isChildrenDiscovered = isChildrenDiscovered && children.entirelyDiscover;
		}
		this.entirelyDiscover = isChildrenDiscovered;

	}

	public int choose() {
		for (int j = 0; j<this.children.size(); j++){
			if (!this.children.get(j).entirelyDiscover){
				return j;
			}
		}
		return 0;
	}
	
	public void print(int tabs) {
		String tab = new String();
		for (int i = 0; i < tabs; i++) {
			tab += "\t";
		}
		System.out.println(tab + "Factor : " + phi);
		System.out.println(tab + "cutset Outside SubModel : " + cutsetOutsideSubModel);
		System.out.println(tab + "cutset Inside SubModel : " + cutsetInsideSubModel);
		System.out.println(tab + "Entirely discover : " + this.entirelyDiscover);

		for (VariableComponent c : this.children) {
			c.print(tabs + 1);
		}
	}
}
