package com.sri.ai.grinder.sgdpllt.anytime;


import static com.sri.ai.grinder.sgdpllt.library.FunctorConstants.TIMES;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashSet;
import java.util.Set;

import static com.sri.ai.expresso.helper.Expressions.apply;
import static com.sri.ai.expresso.helper.Expressions.parse;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.sgdpllt.api.Context;
import com.sri.ai.grinder.sgdpllt.api.Theory;

public class VariableComponent {

	public Model model;
	public Expression variable;
	public Set<Expression> parent;
	public boolean entirelyDiscover;
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
		this.entirelyDiscover = false;
		this.phiInsideSubModel = new HashSet<Expression>();
		this.children = new ArrayList<FactorComponent>();
		this.cutsetInsideSubModel = new HashSet<Expression>();
		this.cutsetOutsideSubModel = new HashSet<Expression>();
		this.bound = new HashSet<Expression>();
		this.model.context = this.model.context.extendWithSymbolsAndTypes(this.variable.toString(), "Boolean");

		Set<Expression> intersection = new HashSet<Expression>();
		intersection.addAll(model.getNeighborsOfSet(model.getInitializedVariable()));
		Collection<Expression> S = model.getNeighbors(variable);
		for (Expression e : this.parent) {
			S.remove(e);
		}
		if (S.isEmpty()){
			this.entirelyDiscover = true;
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
		
		boolean isChildrenDiscovered = true;
		for (FactorComponent children : this.children){
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
		System.out.println(tab + "Variable : " + variable);
		System.out.println(tab + "cutset Outside SubModel : " + cutsetOutsideSubModel);
		System.out.println(tab + "cutset Inside SubModel : " + cutsetInsideSubModel);
		System.out.println(tab + "Entirely discover : " + this.entirelyDiscover);

		for (FactorComponent c : this.children) {
			c.print(tabs + 1);
		}

	}

	public Expression calculate(){
		Theory theory = this.model.theory;
		Context context = this.model.context;		
		
		Expression childrenMessage = parse("1");
		
		for(FactorComponent children : this.children){
			childrenMessage = apply(TIMES, childrenMessage, children.calculate());
			childrenMessage = theory.evaluate(childrenMessage, context);
		}
		
		
		for (Expression cutset : this.cutsetInsideSubModel){
			childrenMessage = theory.evaluate(childrenMessage, context);
			String str = "sum({{ (on " + cutset + " in Boolean ) " + childrenMessage + " }})";
			childrenMessage = parse(str);
		}
		
		return 	theory.evaluate(childrenMessage, context);

	}

	public Expression naiveCalcul(){
		String string = "(" + this.model.naiveCalculation(this.variable) + ")/sum({{ (on "  + this.variable + " in Boolean) " + this.model.naiveCalculation(this.variable) + " }})";
		return this.model.theory.evaluate(parse(string), this.model.context);
	}
	
}
