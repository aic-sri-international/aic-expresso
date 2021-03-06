package com.sri.ai.grinder.anytime;

import static com.sri.ai.grinder.helper.GrinderUtil.getIndexExpressionsOfFreeVariablesIn;
import static com.sri.ai.grinder.library.FunctorConstants.IN;
import static com.sri.ai.grinder.library.FunctorConstants.SUM;
import static com.sri.ai.grinder.library.FunctorConstants.TIMES;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Scanner;
import java.util.Set;

import static com.sri.ai.expresso.helper.Expressions.apply;
import static com.sri.ai.expresso.helper.Expressions.makeSymbol;
import static com.sri.ai.expresso.helper.Expressions.parse;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.IndexExpressionsSet;
import com.sri.ai.expresso.api.IntensionalSet;
import com.sri.ai.expresso.core.DefaultExtensionalUniSet;
import com.sri.ai.expresso.core.ExtensionalIndexExpressionsSet;
import com.sri.ai.grinder.api.Context;
import com.sri.ai.grinder.api.Theory;
import com.sri.ai.grinder.library.bounds.Bound;
import com.sri.ai.grinder.library.bounds.Bounds;
import com.sri.ai.util.Util;

public class VariableComponent {

	public Model model;
	public Expression variable;
	public Set<Expression> parent;
	public boolean entirelyDiscover;
	public ArrayList<FactorComponent> children;
	public Set<Expression> cutsetOutsideSubModel;
	public Set<Expression> cutsetInsideSubModel;
	public Set<Expression> schema;
	public Bound bound;
	public Set<Expression> phiInsideSubModel;
	public Integer childToUpdate;
	public boolean isExtensionalBound;
	public boolean isCutset;

	public VariableComponent(Expression variable, Expression Parent, Model model, Set<Expression> Pext, boolean isExtensionalBound) {
// we don't get to use Pext
		this.model = model;
		this.variable = variable;
		this.childToUpdate = 0;
		this.parent = new HashSet<Expression>();
		this.parent.add(Parent);
		this.entirelyDiscover = false;
		this.phiInsideSubModel = new HashSet<Expression>();
		this.children = new ArrayList<FactorComponent>();
		this.cutsetInsideSubModel = new HashSet<Expression>();
		this.cutsetOutsideSubModel = new HashSet<Expression>();
		this.schema = new HashSet<Expression>();
		this.schema.add(this.variable);
		this.bound = Bounds.simplex(new ArrayList<Expression>(Arrays.asList(this.variable)), this.model,isExtensionalBound);
		this.isExtensionalBound = isExtensionalBound;
		this.isCutset = false;

		Set<Expression> intersection = new HashSet<Expression>();
		intersection.addAll(model.getNeighborsOfSet(model.getInitializedVariable()));
		Collection<Expression> S = model.getNeighbors(variable);
		for (Expression e : this.parent) {
			S.remove(e);
		}
		if (S.isEmpty()) {
			this.entirelyDiscover = true;
			this.bound= Bounds.makeSingleElementBound(makeSymbol(1), isExtensionalBound);
		}
		S.retainAll(intersection);
		if (!S.isEmpty()) {
			this.cutsetOutsideSubModel.add(variable);
			this.isCutset = true;
		}

		model.initializeVariableComponent.add(this);
		
		// we add the varaible to the context and tell of which type is the variable add
		// Expression typeOfVariable = this.model.getValues(variable);
		// this.model.context = this.model.context.extendWithSymbolsAndTypes(this.variable.toString(), typeOfVariable);
	}

	public void update(Set<Expression> Pext, Boolean withBound) {

		if (this.children.isEmpty()) {
			for (Expression factorInvolvingVariable : this.model.getNeighbors(variable)) {
				if (!this.parent.contains(factorInvolvingVariable)) {
					boolean test = false;// test = isEAlreadyUncovered = isEAParent
					
					for (FactorComponent factorComponentAlreadyInitialized : model.initializeFactorComponent) {
						if (factorComponentAlreadyInitialized.phi.equals(factorInvolvingVariable)) {
							test = true;
							this.parent.add(factorComponentAlreadyInitialized.phi);
							
							factorComponentAlreadyInitialized.updateCutset();
						}
					}
					if (test == false) {
						FactorComponent newC = new FactorComponent(factorInvolvingVariable, variable, model, Pext, isExtensionalBound);
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
			if (cutsetOutsideSubModel.contains(this.variable)) {
				this.isCutset = true;
			}
		}
		else {
			// int j = this.chooseDepthImportantFirst();
			int j = this.chooseBreadthFirst();
			// int j = chooseBreadthFirst();
			Set<Expression> union = new HashSet<Expression>(Pext);
			for (int i = 0; i < this.children.size(); i++) {
				if (j!=i) {
					union.addAll(this.children.get(i).cutsetInsideSubModel);
				}
			}
			this.children.get(j).update(union, withBound);

			Set<Expression> intersection = new HashSet<Expression>();
			intersection.addAll(this.children.get(j).cutsetOutsideSubModel);
			intersection.retainAll(model.getNeighborsOfSet(Pext));

			cutsetOutsideSubModel.addAll(intersection);

			cutsetInsideSubModel.addAll(this.children.get(j).cutsetOutsideSubModel);
			cutsetInsideSubModel.removeAll(cutsetOutsideSubModel);

			phiInsideSubModel.addAll(this.children.get(j).phiInsideSubModel);
			if (cutsetOutsideSubModel.contains(this.variable)) {
				this.isCutset = true;
			}

		}
		
		boolean isChildrenDiscovered = true;
		for (FactorComponent children : this.children) {
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
		System.out.println("Used by variable : " + this.variable);
		Set<Expression> Pext = new HashSet<Expression>();
		for (FactorComponent factor : this.model.initializeFactorComponent) {
			Pext.add(factor.phi);
		}
		Pext.removeAll(phiInsideSubModel);
		
		Set<Expression> intersection = new HashSet<Expression>();
		for (FactorComponent children : this.children) {
			intersection.addAll(children.cutsetOutsideSubModel);
		}
		intersection.retainAll(model.getNeighborsOfSet(Pext));
		System.out.println("Intersection = " + intersection);
		System.out.println("Pext = " + Pext);

		cutsetOutsideSubModel = intersection;
		if (this.isCutset) {
			this.cutsetOutsideSubModel.add(this.variable);
		}
		for (FactorComponent children : this.children) {
			cutsetInsideSubModel.addAll(children.cutsetOutsideSubModel);
		}
		cutsetInsideSubModel.removeAll(cutsetOutsideSubModel);

		System.out.println(this.parent);
		
		
		for (Expression parent : this.parent) {
			if (parent==null) {
				return;
			}
				for (FactorComponent c : model.initializeFactorComponent) {
					if (c.phi.equals(parent)) {
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
//		for (int j = childToUpdate + 1; j<this.children.size(); j++) {
//			if (!this.children.get(j).entirelyDiscover) {
//				this.childToUpdate = j;
//				return j;
//			}
//		}
//		for (int j = 0; j<childToUpdate; j++) {
//			if (!this.children.get(j).entirelyDiscover) {
//				this.childToUpdate = j;
//				return j;
//			}
//		}
//		return childToUpdate;
	}

	public int chooseMySelf() {
		System.out.println("Choose next factor for variable " + this.variable + " : ");
		for (int i = 0; i < this.children.size(); i++) {
			System.out.println("Choice " + i + " = " + this.children.get(i).phi);
		}
		Scanner reader = new Scanner(System.in);  // Reading from System.in
		System.out.println("Enter a choice: ");
		int n = reader.nextInt(); // Scans the next token of the input as an int.
		reader.close();
		return n;
	}
	
	public int chooseDepthImportantFirst() {
		Expression min = parse("{ 1 }");
		Integer jToReturn = 0;
		for (int j = 0; j<this.children.size(); j++) {
			if (!this.children.get(j).entirelyDiscover) {
				
				Set<Expression> toMultiply = this.model.getNeighbors(this.children.get(j).phi);
				toMultiply.removeAll(this.children.get(j).parent);
				
				DefaultExtensionalUniSet varToMultiply = new DefaultExtensionalUniSet(new ArrayList<>(toMultiply));
				
				IndexExpressionsSet indices = getIndexExpressionsOfFreeVariablesIn(varToMultiply, this.model.context);
				// TODO Find a way to do : intersection (variablesToBeSummedOut, FreeVariables(phi))
				
				Expression setOfFactorInstantiations = IntensionalSet.makeMultiSet(
						indices,
						Bounds.normalizeSingleExpression(this.children.get(j).phi, this.model.theory, this.model.context),// head
						makeSymbol(true)// No Condition
						);
				
				Expression firstProduct = apply(TIMES, setOfFactorInstantiations);
				Expression evaluation = this.model.theory.evaluate(firstProduct, this.model.context);
				
				DefaultExtensionalUniSet varToSum = new DefaultExtensionalUniSet(new ArrayList<>(this.children.get(j).parent));
				
				indices = getIndexExpressionsOfFreeVariablesIn(varToSum, this.model.context);
				
				setOfFactorInstantiations = IntensionalSet.makeMultiSet(
						indices,
						evaluation,// head
						makeSymbol(true)// No Condition
						);
				
				Expression sumOnPhi = apply(SUM, setOfFactorInstantiations);
				evaluation = this.model.theory.evaluate(sumOnPhi, this.model.context);
				
				if (evaluation.compareTo(min) < 0) {
					min = evaluation;
					jToReturn = j;
				}
				
			}

		}
		return jToReturn;
	}

	
	public void print(int tabs) {
		String tab = new String();
		for (int i = 0; i < tabs; i++) {
			tab += "\t";
		}
		System.out.println(tab + "Variable : " + variable);
		System.out.println(tab + "cutset Outside SubModel : " + cutsetOutsideSubModel);
		System.out.println(tab + "cutset Inside SubModel : " + cutsetInsideSubModel);
		System.out.println(tab + "Bound : " + this.bound);
		System.out.println(tab + "Schema : " + this.schema);
		System.out.println(tab + "isCutset : " + this.isCutset);
		System.out.println(tab + "Entirely discover : " + this.entirelyDiscover);
		

		for (FactorComponent c : this.children) {
			c.print(tabs + 1);
		}

	}

	public void printTotal() {
		String tab = new String();
		Set<Expression> Pext = new HashSet<Expression>();
		for (FactorComponent factor : this.model.initializeFactorComponent) {
			Pext.add(factor.phi);
		}
		Pext.removeAll(phiInsideSubModel);
		System.out.println(tab + "Variable : " + variable);
		System.out.println(tab + "cutset Outside SubModel : " + cutsetOutsideSubModel);
		System.out.println(tab + "cutset Inside SubModel : " + cutsetInsideSubModel);
		System.out.println(tab + "Bound : " + this.bound);
		System.out.println(tab + "Schema : " + this.schema);
		System.out.println(tab + "isCutset : " + this.isCutset);
		System.out.println(tab + "Entirely discover : " + this.entirelyDiscover);
		System.out.println(tab + "Phi Inside Submodel : " + this.phiInsideSubModel);
		System.out.println(tab + "Pext : " + Pext);

	}

	
	public Expression calculate() {
		Theory theory = this.model.theory;
		Context context = this.model.context;		
		
		Expression childrenMessage = parse("1");
		
		for (FactorComponent children : this.children) {
			childrenMessage = apply(TIMES, childrenMessage, children.calculate());
			childrenMessage = theory.evaluate(childrenMessage, context);
		}
		
		
		for (Expression cutsetVariable : this.cutsetInsideSubModel) {
			// childrenMessage = theory.evaluate(childrenMessage, context);
			// String str = "sum({{ (on " + cutsetVariable + " in " + this.model.getValues(cutsetVariable) +" ) " + childrenMessage + " }})";
			// childrenMessage = parse(str);
			Expression valuesTakenByVariableToSum = this.model.getValues(cutsetVariable);
			IndexExpressionsSet indices = new ExtensionalIndexExpressionsSet(apply(IN, cutsetVariable, valuesTakenByVariableToSum ));
			Expression intensionalMultiSet = IntensionalSet.makeMultiSet(indices, childrenMessage, parse("true")); 
			Expression summation = apply(SUM,  intensionalMultiSet);
			childrenMessage=summation;
		}
		
		return 	theory.evaluate(childrenMessage, context);

	}


	public void calculateBound() {
		Theory theory = this.model.theory;
		Context context = this.model.context;		
		
		// Expression childrenBound = parse("{ 1 }");
		
		// for (FactorComponent children : this.children) {
		//	childrenBound = Bounds.boundProduct(theory, context, childrenBound, children.bound);
		//}
		
		Bound[] childrenArray = new Bound[children.size()];
		int i = 0;
		for (FactorComponent children : this.children) {
			childrenArray[i] = children.bound;
			i++;
		}
		Bound childrenBound;
		if (childrenArray.length != 0) {
			childrenBound = Bounds.boundProduct(this.model.theory, this.model.context, isExtensionalBound, childrenArray);
		}
		else{
			childrenBound = Bounds.makeSingleElementBound(makeSymbol(1), isExtensionalBound);
		}
		
		Iterator<Expression> iteratorToVariables = this.cutsetInsideSubModel.iterator();
		ArrayList<Expression> variablesToBeSummedOut = new ArrayList<>(this.cutsetInsideSubModel.size());
		for (Expression var : Util.in(iteratorToVariables)) {
			variablesToBeSummedOut.add(var);
		}
		// We want sum other toSum of Phi*childrenBound
		DefaultExtensionalUniSet varToSum = new DefaultExtensionalUniSet(variablesToBeSummedOut);
		
		bound = childrenBound.sumOut(varToSum, context, theory);
		
	}
	
	public void calculateSchema() {
		Set<Expression> freeVariables =new HashSet<Expression>();
		freeVariables.add(this.variable);
		for (FactorComponent children : this.children) {
			freeVariables.addAll(children.schema);
		}
		
		freeVariables.removeAll(this.cutsetInsideSubModel);
		
		this.schema = freeVariables;
	}
	
	public Expression naiveCalcul() {
		Expression expressiontoSum = this.model.naiveCalculation(this.variable);
		return expressiontoSum;
//		// String values = this.model.getValues(this.variable);
//		// String string = "(" + expression + ")/sum({{ (on "  + this.variable + " in " + values +" ) " + expression  + " }})";
//		
//		// Expression expressionToSum = theory.evaluate(childrenMessage, context);
//		Expression valuesTakenByVariableToSum = this.model.getValues(this.variable);
//		IndexExpressionsSet indices = new ExtensionalIndexExpressionsSet(apply(IN, this.variable, valuesTakenByVariableToSum ));
//		Expression intensionalMultiSet = IntensionalSet.makeMultiSet(indices, expressiontoSum, parse("true")); 
//		Expression summation = apply(SUM, intensionalMultiSet);
//		// childrenMessage=summation;
//		
//		Expression normalizalisation = this.model.theory.evaluate(summation, this.model.context);
//		Expression result = apply("/", expressiontoSum, normalizalisation);
//		result = this.model.theory.evaluate(result, this.model.context);
//		
//		return result;
	}
	
}
