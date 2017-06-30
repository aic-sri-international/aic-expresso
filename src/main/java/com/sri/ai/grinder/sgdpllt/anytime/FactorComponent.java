package com.sri.ai.grinder.sgdpllt.anytime;

import static com.sri.ai.expresso.helper.Expressions.apply;
import static com.sri.ai.expresso.helper.Expressions.parse;
import static com.sri.ai.grinder.sgdpllt.library.FunctorConstants.IN;
import static com.sri.ai.grinder.sgdpllt.library.FunctorConstants.SUM;
import static com.sri.ai.grinder.sgdpllt.library.FunctorConstants.TIMES;

import static com.sri.ai.util.Util.list;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashSet;
import java.util.List;
import java.util.Random;
import java.util.Scanner;
import java.util.Set;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.IndexExpressionsSet;
import com.sri.ai.expresso.api.IntensionalSet;
import com.sri.ai.expresso.core.DefaultExtensionalUniSet;
import com.sri.ai.expresso.core.ExtensionalIndexExpressionsSet;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.grinder.sgdpllt.api.Context;
import com.sri.ai.grinder.sgdpllt.api.Theory;
import com.sri.ai.grinder.sgdpllt.library.bounds.Bound;
import com.sri.ai.grinder.sgdpllt.library.bounds.Bounds;
import com.sri.ai.grinder.sgdpllt.library.bounds.DefaultExtensionalBound;
import com.sri.ai.grinder.sgdpllt.library.set.extensional.ExtensionalSets;

public class FactorComponent {

	public Model model;
	public Expression phi;
	public Set<VariableComponent> parent;
	public ArrayList<VariableComponent> children;
	public Set<Expression> cutsetOutsideSubModel;
	public Set<Expression> cutsetInsideSubModel;
	public Set<Expression> SummedOutCutset;
	public Bound bound;
	public Set<Expression> phiInsideSubModel;
	public Integer childToUpdate;
	public boolean isExtensionalBound;
	public Set<Expression> schema;
	public boolean entirelyDiscover;

	public FactorComponent(Expression phi, VariableComponent Parent, boolean isExtensionalBound) {

		this.model = Parent.model;

		this.phi = phi;

		this.children = new ArrayList<VariableComponent>();

		this.parent = new HashSet<VariableComponent>();
		this.parent.add(Parent);

		this.phiInsideSubModel = new HashSet<Expression>();
		this.phiInsideSubModel.add(phi);

		this.entirelyDiscover = false;
		this.childToUpdate = 0;
		this.cutsetInsideSubModel = new HashSet<Expression>();
		this.cutsetOutsideSubModel = new HashSet<Expression>();
		this.schema = new HashSet<Expression>();
		List<Expression> VariableParent = new ArrayList<Expression>();
		VariableParent.add(Parent.variable);
		this.SummedOutCutset = new HashSet<Expression>();
		this.SummedOutCutset.addAll(Parent.SummedOutCutset);
		this.SummedOutCutset.addAll(Parent.cutsetInsideSubModel);
		this.bound = Bounds.simplex(VariableParent, model, isExtensionalBound); // true
																				// says
																				// that
																				// it
																				// is
																				// a
																				// extensional
																				// simplex.
																				// TODO
																				// :
																				// add
																				// a
																				// constructor
																				// that
																				// chooses
																				// the
																				// kind
																				// of
																				// bound

		this.isExtensionalBound = isExtensionalBound;

		this.schema.addAll(Expressions.freeVariables(this.phi, this.model.context));
		this.schema.remove(Parent);

		Set<Expression> intersection = new HashSet<Expression>();
		intersection.addAll(model.getNeighborsOfSet(model.getInitializedFactor()));
		Collection<Expression> S = model.getNeighbors(phi);
		S.remove(Parent.variable);
		if (S.isEmpty()) {
			this.entirelyDiscover = true;
			this.bound = Bounds.makeSingleElementBound(this.phi, isExtensionalBound);
		}
		S.retainAll(intersection);
		this.cutsetOutsideSubModel.addAll(S);
		for (Expression cutset : S) {
			Set<VariableComponent> AllInitializedVariableComponent = new HashSet<VariableComponent>();
			AllInitializedVariableComponent.addAll(this.model.initializeVariableComponent);
			boolean test = true;
			for (VariableComponent InitializedVariableComponent : AllInitializedVariableComponent) {
				if (InitializedVariableComponent.variable == cutset) {
					InitializedVariableComponent.cutsetOutsideSubModel.add(cutset);
					InitializedVariableComponent.isCutset = true;
					InitializedVariableComponent.updateParentCutset(this);
					test = false;
				}
			}
			if (test) {
				VariableComponent newV = new VariableComponent(cutset, this, isExtensionalBound);
				newV.cutsetOutsideSubModel.add(newV.variable);
				newV.isCutset = true;
			}
		}

		model.initializeFactorComponent.add(this);

	}

	public void update(Boolean withBound, String chooseFunction) {

		Set<Expression> Pext = new HashSet<Expression>();
		Pext.addAll(this.model.getInitializedFactor());
		Pext.removeAll(this.phiInsideSubModel);

		Set<Expression> ExpressionParent = new HashSet<Expression>();
		for (VariableComponent Parent : this.parent) {
			ExpressionParent.add(Parent.variable);
			this.SummedOutCutset.addAll(Parent.SummedOutCutset);
			this.SummedOutCutset.addAll(Parent.cutsetInsideSubModel);
		}
		

		if (this.children.isEmpty()) {
			Set<Expression> union = new HashSet<Expression>(Pext);
			union.add(phi);

			for (Expression variableInvolvedInPhi : this.model.getNeighbors(phi)) {
				if (!ExpressionParent.contains(variableInvolvedInPhi)) {// if
																		// the
																		// variable
																		// variableInvolvedInPhi
																		// is
																		// not
																		// contain
																		// in
																		// parent,
																		// it is
																		// a new
																		// variable

					boolean test = false; // boolean to test if the variable as
											// already been initialized

					// all variable are checked to see if the variable
					// variableInvolvedInPhi has already been discovered
					for (VariableComponent variableComponentAlreadyInitialized : model.initializeVariableComponent) {
						if (variableComponentAlreadyInitialized.variable.equals(variableInvolvedInPhi)) {
							test = true;
							variableComponentAlreadyInitialized.parent.add(this);
							this.children.add(variableComponentAlreadyInitialized);
							Set<Expression> intersection = new HashSet<Expression>(
									variableComponentAlreadyInitialized.cutsetOutsideSubModel);
							intersection.retainAll(model.getNeighborsOfSet(Pext));
							cutsetOutsideSubModel.addAll(intersection);

							cutsetInsideSubModel.addAll(variableComponentAlreadyInitialized.cutsetOutsideSubModel);
							cutsetInsideSubModel.removeAll(SummedOutCutset);
							variableComponentAlreadyInitialized.updateParentCutset(this);
						}
					}

					if (test == false) {
						VariableComponent newV = new VariableComponent(variableInvolvedInPhi, this, isExtensionalBound);
						this.children.add(newV);
						Set<Expression> intersection = new HashSet<Expression>(newV.cutsetOutsideSubModel);
						intersection.retainAll(model.getNeighborsOfSet(Pext));
						cutsetOutsideSubModel.addAll(intersection);

						cutsetInsideSubModel.addAll(newV.cutsetOutsideSubModel);
					}
				}

				cutsetInsideSubModel.removeAll(cutsetOutsideSubModel);
				cutsetInsideSubModel.removeAll(SummedOutCutset);

			}
		} else {
			int j = 0;
			if (chooseFunction == "BFS") {
				j = this.chooseBreadthFirst();
			} else if (chooseFunction == "DFS") {
				j = this.chooseDepthFirst();

			} else if (chooseFunction == "myself") {
				j = this.chooseMySelf();

			} else {
				System.out.println("Unexistant Choose Function");
				j = -1;
			}

			
			this.children.get(j).update(withBound, chooseFunction);

			Set<Expression> intersection = new HashSet<Expression>(this.children.get(j).cutsetOutsideSubModel);
			intersection.retainAll(model.getNeighborsOfSet(Pext));
			cutsetOutsideSubModel.addAll(intersection);

			cutsetInsideSubModel.addAll(this.children.get(j).cutsetOutsideSubModel);
			cutsetInsideSubModel.removeAll(cutsetOutsideSubModel);
			cutsetInsideSubModel.removeAll(SummedOutCutset);

			phiInsideSubModel.addAll(this.children.get(j).phiInsideSubModel);

		}

		boolean isChildrenDiscovered = true;
		for (VariableComponent children : this.children) {
			isChildrenDiscovered = isChildrenDiscovered && children.entirelyDiscover;
		}
		this.entirelyDiscover = isChildrenDiscovered;

		if (withBound) {
			this.calculateBound();
		} else {
			this.calculateSchema();
		}
	}
	
	public void updateParentCutset(VariableComponent CallingParent){
		Set<Expression> Pext = new HashSet<Expression>();
		Pext.addAll(this.model.getInitializedFactor());
		Pext.removeAll(this.phiInsideSubModel);

		Set<Expression> childrenCutset = new HashSet<Expression>();
		
		for (VariableComponent children : this.children){
			childrenCutset.addAll(children.cutsetOutsideSubModel);
		}
		

		this.cutsetInsideSubModel.removeAll(cutsetInsideSubModel);
		this.cutsetOutsideSubModel.removeAll(cutsetOutsideSubModel);
		this.cutsetInsideSubModel.addAll(childrenCutset);
		childrenCutset.retainAll(model.getNeighborsOfSet(Pext));
		cutsetOutsideSubModel.addAll(childrenCutset);
		this.cutsetInsideSubModel.removeAll(cutsetOutsideSubModel);
		
		this.calculateBound();
		
		
		for(VariableComponent parent : this.parent){
			if(!parent.equals(CallingParent)){
				parent.updateParentCutset(null);
			}
			this.SummedOutCutset.addAll(parent.SummedOutCutset);
			this.SummedOutCutset.addAll(parent.cutsetInsideSubModel);
		}
		this.cutsetInsideSubModel.removeAll(this.SummedOutCutset);
		
	}
	
	public int chooseDepthFirst() {
		for (int j = 0; j < this.children.size(); j++) {
			if (!this.children.get(j).entirelyDiscover) {
				return j;
			}
		}
		return 0;
	}

	public int chooseBreadthFirst() {

		int result = childToUpdate;

		int size = children.size();
		if (size > 1) {
			do {
				childToUpdate++;
				childToUpdate %= size;
			} while (childToUpdate != result && this.children.get(childToUpdate).entirelyDiscover);
		}

		return result;
		// for (int j = lastUpdatedChild ; j<this.children.size(); j++){
		// if (!this.children.get(j).entirelyDiscover){
		// this.lastUpdatedChild = j;
		// return j;
		// }
		// }
		// for (int j = 0; j<lastUpdatedChild; j++){
		// if (!this.children.get(j).entirelyDiscover){
		// this.lastUpdatedChild = j;
		// return j;
		// }
		// }
		// return lastUpdatedChild;
	}

	public int chooseMySelf() {
		System.out.println("Choose next factor for factor " + this.phi + " : ");
		for (int i = 0; i < this.children.size(); i++) {
			System.out.println("Choice " + i + " = " + this.children.get(i).variable);
		}
		Scanner reader = new Scanner(System.in); // Reading from System.in
		System.out.println("Enter a choice: ");
		int n = reader.nextInt(); // Scans the next token of the input as an
									// int.
		return n;
	}

	public void print(int tabs) {
		String tab = new String();
		for (int i = 0; i < tabs; i++) {
			tab += "\t";
		}
		System.out.println(tab + "Factor : " + phi);
		System.out.println(tab + "Children : " + children);
		System.out.println(tab + "cutset Outside SubModel : " + cutsetOutsideSubModel);
		System.out.println(tab + "cutset Inside SubModel : " + cutsetInsideSubModel);
		System.out.println(tab + "Bound : " + this.bound);
		System.out.println(tab + "Schema : " + this.schema);
		System.out.println(tab + "Entirely discover : " + this.entirelyDiscover);

		for (VariableComponent c : this.children) {
			c.print(tabs + 1);
		}
	}

	public void calculateBound() {

		Set<Expression> ExpressionParent = new HashSet<Expression>();
		for (VariableComponent Parent : this.parent) {
			ExpressionParent.add(Parent.variable);
		}

		Theory theory = this.model.theory;
		Context context = this.model.context;

		// Expression childrenBound = parse("{1}");

		// for(VariableComponent children : this.children){
		// childrenBound = Bounds.boundProduct(this.model.theory,
		// this.model.context, childrenBound, children.bound);
		// }

		Bound[] cildrenArray = new Bound[children.size()];
		int i = 0;
		for (VariableComponent children : this.children) {
			if (!children.principalParent.equals(this)) {
				cildrenArray[i] = Bounds.makeSingleElementBound(parse("1"), isExtensionalBound);
			} else {
				cildrenArray[i] = children.bound;
			}
			i++;
		}
		Bound childrenBound = Bounds.boundProduct(this.model.theory, this.model.context, isExtensionalBound,
				cildrenArray);

		Set<Expression> toSum = model.getNeighbors(phi);
		for (Expression e : ExpressionParent) {
			toSum.remove(e);
		}
		for (Expression e : this.cutsetOutsideSubModel) {
			toSum.remove(e);
		}
		toSum.addAll(this.cutsetInsideSubModel);

		
		ArrayList<Expression> variablesToBeSummedOut = new ArrayList<>(toSum.size());
		variablesToBeSummedOut.addAll(toSum);

		// We want sum other toSum of Phi*childrenBound
		DefaultExtensionalUniSet varToSum = new DefaultExtensionalUniSet(variablesToBeSummedOut);
		bound = childrenBound.summingPhiTimesBound(varToSum, phi, context, theory);
	}

	public void calculateSchema() {
		Set<Expression> ExpressionParent = new HashSet<Expression>();
		for (VariableComponent Parent : this.parent) {
			ExpressionParent.add(Parent.variable);
		}
		Theory theory = this.model.theory;
		Context context = this.model.context;
		Set<Expression> freeVariables = new HashSet<Expression>();
		freeVariables.addAll(Expressions.freeVariables(this.phi, context));
		for (VariableComponent children : this.children) {
			freeVariables.addAll(children.schema);
		}
		Set<Expression> toSum = model.getNeighbors(phi);
		for (Expression e : ExpressionParent) {
			toSum.remove(e);
		}
		for (Expression e : this.cutsetOutsideSubModel) {
			toSum.remove(e);
		}
		toSum.addAll(this.cutsetInsideSubModel);

		freeVariables.removeAll(toSum);

		this.schema = freeVariables;
	}

	public Expression calculate() {
		Set<Expression> ExpressionParent = new HashSet<Expression>();
		for (VariableComponent Parent : this.parent) {
			ExpressionParent.add(Parent.variable);
		}
		Theory theory = this.model.theory;
		Context context = this.model.context;

		Expression childrenMessage = parse("{ 1 }");

		for (VariableComponent children : this.children) {
			childrenMessage = apply(TIMES, childrenMessage, children.calculate());
		}

		childrenMessage = apply(TIMES, childrenMessage, this.phi);

		for (Expression cutset : this.cutsetInsideSubModel) {
			// String str = "sum({{ (on " + cutset + " in Boolean ) " +
			// childrenMessage + " }})";
			// childrenMessage = parse(str);
			// childrenMessage = theory.evaluate(childrenMessage, context);
			Expression valuesTakenByVariableToSum = this.model.getValues(cutset);
			IndexExpressionsSet indices = new ExtensionalIndexExpressionsSet(
					apply(IN, cutset, valuesTakenByVariableToSum));
			Expression intensionalMultiSet = IntensionalSet.makeMultiSet(indices, childrenMessage, parse("true"));
			Expression summation = apply(SUM, intensionalMultiSet);
			childrenMessage = summation;
			// String str = "sum({{ (on " + variableToSum + " in " +
			// this.model.getValues(variableToSum) +" ) " + childrenMessage + "
			// }})";
			// childrenMessage = parse(str);

		}

		Set<Expression> toSum = model.getNeighbors(phi);
		for (Expression e : ExpressionParent) {
			toSum.remove(e);
		}
		toSum.removeAll(this.cutsetOutsideSubModel);
		toSum.removeAll(this.cutsetInsideSubModel);

		for (Expression variableToSum : toSum) {
			Expression expressionToSum = theory.evaluate(childrenMessage, context);
			Expression valuesTakenByVariableToSum = this.model.getValues(variableToSum);
			IndexExpressionsSet indices = new ExtensionalIndexExpressionsSet(
					apply(IN, variableToSum, valuesTakenByVariableToSum));
			Expression intensionalMultiSet = IntensionalSet.makeMultiSet(indices, expressionToSum, parse("true"));
			Expression summation = apply(SUM, intensionalMultiSet);
			// String str = "sum({{ (on " + variableToSum + " in " +
			// this.model.getValues(variableToSum) +" ) " + childrenMessage + "
			// }})";
			// childrenMessage = parse(str);
			childrenMessage = summation;
		}
		return theory.evaluate(childrenMessage, context);

	}

}
