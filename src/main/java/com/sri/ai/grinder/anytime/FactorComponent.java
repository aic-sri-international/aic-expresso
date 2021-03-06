package com.sri.ai.grinder.anytime;


import static com.sri.ai.expresso.helper.Expressions.apply;
import static com.sri.ai.expresso.helper.Expressions.parse;
import static com.sri.ai.grinder.library.FunctorConstants.IN;
import static com.sri.ai.grinder.library.FunctorConstants.SUM;
import static com.sri.ai.grinder.library.FunctorConstants.TIMES;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashSet;
import java.util.Scanner;
import java.util.Set;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.IndexExpressionsSet;
import com.sri.ai.expresso.api.IntensionalSet;
import com.sri.ai.expresso.core.DefaultExtensionalUniSet;
import com.sri.ai.expresso.core.ExtensionalIndexExpressionsSet;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.grinder.api.Context;
import com.sri.ai.grinder.api.Theory;
import com.sri.ai.grinder.library.bounds.Bound;
import com.sri.ai.grinder.library.bounds.Bounds;


public class FactorComponent {

	public Model model;
	public Expression phi;
	public Set<Expression> parent;
	public boolean entirelyDiscover;
	public ArrayList<VariableComponent> children;
	public Set<Expression> cutsetOutsideSubModel;
	public Set<Expression> cutsetInsideSubModel;
	public Bound bound;
	public Set<Expression> phiInsideSubModel;
	public Integer childToUpdate;
	public boolean isExtensionalBound;
	public Set<Expression> schema;

	public FactorComponent(Expression phi, Expression Parent, Model model, Set<Expression> Pext, boolean isExtensionalBound) {

		this.phiInsideSubModel = new HashSet<Expression>();
		this.model = model;
		this.entirelyDiscover = false;
		this.childToUpdate = 0;
		this.children = new ArrayList<VariableComponent>();
		this.parent = new HashSet<Expression>();
		this.parent.add(Parent);
		this.cutsetInsideSubModel = new HashSet<Expression>();
		this.cutsetOutsideSubModel = new HashSet<Expression>();
		this.schema = new HashSet<Expression>();
		this.bound = Bounds.simplex(new ArrayList<Expression>(this.parent), model,isExtensionalBound); // true says that it is a extensional simplex. TODO : add a constructor that chooses the kind of bound
		this.phi = phi;
		this.phiInsideSubModel.add(phi);
		this.isExtensionalBound = isExtensionalBound;
		
		this.schema.addAll(Expressions.freeVariables(this.phi, this.model.context));
		this.schema.remove(Parent);

		Set<Expression> intersection = new HashSet<Expression>();
		intersection.addAll(model.getNeighborsOfSet(model.getInitializedFactor()));
		Collection<Expression> S = model.getNeighbors(phi);
		for (Expression e : this.parent) {
			S.remove(e);
		}
		if (S.isEmpty()) {
			this.entirelyDiscover = true;
			this.bound = Bounds.makeSingleElementBound(this.phi, isExtensionalBound);
		}
		S.retainAll(intersection);
		this.cutsetOutsideSubModel.addAll(S);
		model.initializeFactorComponent.add(this);

	}

	public void update(Set<Expression> Pext, Boolean withBound) {

		if (this.children.isEmpty()) {
			for (Expression variableInvolvedInPhi : this.model.getNeighbors(phi)) {
				if (!this.parent.contains(variableInvolvedInPhi)) {// if the variable variableInvolvedInPhi is not contain in parent, it is a new variable
					Set<Expression> union = new HashSet<Expression>(Pext); 
					union.add(phi);

					boolean test = false; // boolean to test if the variable as already been initialized

					// all variable are checked to see if the variable variableInvolvedInPhi has already been discovered
					for (VariableComponent variableComponentAlreadyInitialized : model.initializeVariableComponent) {
						if (variableComponentAlreadyInitialized.variable.equals(variableInvolvedInPhi)) {
							test = true;
							System.out.println("refind variable already initialized : " + variableComponentAlreadyInitialized.variable );
							// this.parent.add(variableComponentAlreadyInitialized.variable);
							variableComponentAlreadyInitialized.parent.add(this.phi);
							// if (variableComponentAlreadyInitialized.isCutset == true) {
								System.out.println("isCutset is used");
								variableComponentAlreadyInitialized.updateCutset();
							//}
						}
					}

					if (test == false) {
						VariableComponent newV = new VariableComponent(variableInvolvedInPhi, phi, model, union,isExtensionalBound);
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
			// int j = this.chooseBreadthFirst();
			int j = this.chooseBreadthFirst();
			// int j = chooseBreadthFirst();
			Set<Expression> union = new HashSet<Expression>(Pext);
			for (int i = 0; i < this.children.size(); i++) {
				if (j!=i) {
					union.addAll(this.children.get(i).cutsetInsideSubModel);
				}
			}
			this.children.get(j).update(union, withBound);

			Set<Expression> intersection = new HashSet<Expression>(this.children.get(j).cutsetOutsideSubModel);
			intersection.retainAll(model.getNeighborsOfSet(Pext));
			cutsetOutsideSubModel.addAll(intersection);

			cutsetInsideSubModel.addAll(this.children.get(j).cutsetOutsideSubModel);
			cutsetInsideSubModel.removeAll(cutsetOutsideSubModel);

			phiInsideSubModel.addAll(this.children.get(j).phiInsideSubModel);

		}
		
		
		boolean isChildrenDiscovered = true;
		for (VariableComponent children : this.children) {
			isChildrenDiscovered = isChildrenDiscovered && children.entirelyDiscover;
		}
		this.entirelyDiscover = isChildrenDiscovered;
			
		if (withBound) {
			this.calculateBound();
		}
		else{
			this.calculateSchema();
		}
	}

	
	public void updateCutset() {
		System.out.println("Used by factor : " + this.phi);
		Set<Expression> Pext = new HashSet<Expression>();
		for (FactorComponent factor : this.model.initializeFactorComponent) {
			Pext.add(factor.phi);
		}
		Pext.removeAll(phiInsideSubModel);
		
		Set<Expression> intersection = new HashSet<Expression>();
		for (VariableComponent children : this.children) {
			intersection.addAll(children.cutsetOutsideSubModel);
		}
		intersection.retainAll(model.getNeighborsOfSet(Pext));

		cutsetOutsideSubModel = intersection;
		for (VariableComponent children : this.children) {
			cutsetInsideSubModel.addAll(children.cutsetOutsideSubModel);
		}
		cutsetInsideSubModel.removeAll(cutsetOutsideSubModel);

		for (Expression parent : this.parent) {
				for (VariableComponent c : model.initializeVariableComponent) {
					if (c.variable.equals(parent)) {
						c.updateCutset();
					}
				}
			
		}
	}


	public int chooseDepthFirst() {
		for (int j = 0; j<this.children.size(); j++) {
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
			do{
				childToUpdate ++;
				childToUpdate %= size;
			}while (childToUpdate != result && 
					this.children.get(childToUpdate).entirelyDiscover);
		}
		
		return result;
//		for (int j = lastUpdatedChild ; j<this.children.size(); j++) {
//			if (!this.children.get(j).entirelyDiscover) {
//				this.lastUpdatedChild = j;
//				return j;
//			}
//		}
//		for (int j = 0; j<lastUpdatedChild; j++) {
//			if (!this.children.get(j).entirelyDiscover) {
//				this.lastUpdatedChild = j;
//				return j;
//			}
//		}
//		return lastUpdatedChild;
	}
	
	public int chooseMySelf() {
		System.out.println("Choose next factor for factor " + this.phi + " : ");
		for (int i = 0; i < this.children.size(); i++) {
			System.out.println("Choice " + i + " = " + this.children.get(i).variable);
		}
		Scanner reader = new Scanner(System.in);  // Reading from System.in
		System.out.println("Enter a choice: ");
		int n = reader.nextInt(); // Scans the next token of the input as an int.
		reader.close();
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
		Theory theory = this.model.theory;
		Context context = this.model.context;		
		
		// Expression childrenBound = parse("{1}");

		// for (VariableComponent children : this.children) {
		//	childrenBound = Bounds.boundProduct(this.model.theory, this.model.context, childrenBound, children.bound);
		//}
		
		Bound[] cildrenArray = new Bound[children.size()];
		int i = 0;
		for (VariableComponent children : this.children) {
			cildrenArray[i] = children.bound;
			i++;
		}
		Bound childrenBound = Bounds.boundProduct(this.model.theory, this.model.context, isExtensionalBound,cildrenArray);
		
		Set<Expression> toSum = model.getNeighbors(phi);
		for (Expression e : this.parent) {
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
		bound = childrenBound.sumOutProductByFactor(varToSum, phi, context, theory);
	}
	
	public void calculateSchema() {
		Context context = this.model.context;		
		Set<Expression> freeVariables =new HashSet<Expression>();
		freeVariables.addAll(Expressions.freeVariables(this.phi, context));
		for (VariableComponent children : this.children) {
			freeVariables.addAll(children.schema);
		}
		Set<Expression> toSum = model.getNeighbors(phi);
		for (Expression e : this.parent) {
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
		Theory theory = this.model.theory;
		Context context = this.model.context;		
		
		Expression childrenMessage = parse("{ 1 }");
		
		for (VariableComponent children : this.children) {
			childrenMessage = apply(TIMES, childrenMessage, children.calculate());
		}
		
		childrenMessage = apply(TIMES, childrenMessage, this.phi);
		
		
		for (Expression cutset : this.cutsetInsideSubModel) {
			// String str = "sum({{ (on " + cutset + " in Boolean ) " + childrenMessage + " }})";
			// childrenMessage = parse(str);
			// childrenMessage = theory.evaluate(childrenMessage, context);
			Expression valuesTakenByVariableToSum = this.model.getValues(cutset);
			IndexExpressionsSet indices = new ExtensionalIndexExpressionsSet(apply(IN, cutset, valuesTakenByVariableToSum ));
			Expression intensionalMultiSet = IntensionalSet.makeMultiSet(indices, childrenMessage, parse("true")); 
			Expression summation = apply(SUM, intensionalMultiSet);
			childrenMessage=summation;
			// String str = "sum({{ (on " + variableToSum + " in " + this.model.getValues(variableToSum) +" ) " + childrenMessage + " }})";
			// childrenMessage = parse(str);
			
		}
		
		Set<Expression> toSum = model.getNeighbors(phi);
		for (Expression e : this.parent) {
			toSum.remove(e);
		}
		toSum.removeAll(this.cutsetOutsideSubModel);
		toSum.removeAll(this.cutsetInsideSubModel);
		
		for (Expression variableToSum : toSum) {
			Expression expressionToSum = theory.evaluate(childrenMessage, context);
			Expression valuesTakenByVariableToSum = this.model.getValues(variableToSum);
			IndexExpressionsSet indices = new ExtensionalIndexExpressionsSet(apply(IN, variableToSum, valuesTakenByVariableToSum ));
			Expression intensionalMultiSet = IntensionalSet.makeMultiSet(indices, expressionToSum, parse("true")); 
			Expression summation = apply(SUM, intensionalMultiSet);
			// String str = "sum({{ (on " + variableToSum + " in " + this.model.getValues(variableToSum) +" ) " + childrenMessage + " }})";
			// childrenMessage = parse(str);
			childrenMessage=summation;
		}
		return 	theory.evaluate(childrenMessage, context);

	}
}
