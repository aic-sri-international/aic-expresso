package com.sri.ai.grinder.sgdpllt.anytime;


import static com.sri.ai.grinder.helper.GrinderUtil.getIndexExpressionsOfFreeVariablesIn;
import static com.sri.ai.grinder.sgdpllt.library.FunctorConstants.IN;
//import static com.sri.ai.grinder.sgdpllt.library.FunctorConstants.PRODUCT;
import static com.sri.ai.grinder.sgdpllt.library.FunctorConstants.SUM;
import static com.sri.ai.grinder.sgdpllt.library.FunctorConstants.TIMES;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Set;

import static com.sri.ai.expresso.helper.Expressions.apply;
import static com.sri.ai.expresso.helper.Expressions.makeSymbol;
import static com.sri.ai.expresso.helper.Expressions.parse;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.IndexExpressionsSet;
import com.sri.ai.expresso.api.IntensionalSet;
import com.sri.ai.expresso.core.DefaultExtensionalUniSet;
import com.sri.ai.expresso.core.ExtensionalIndexExpressionsSet;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.grinder.sgdpllt.api.Context;
import com.sri.ai.grinder.sgdpllt.api.Theory;
import com.sri.ai.grinder.sgdpllt.library.bounds.Bounds;
import com.sri.ai.grinder.sgdpllt.library.bounds.Bound;
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
	public Integer lastUpdatedChild;
	public boolean isExtensionalBound;

	public VariableComponent(Expression variable, Expression Parent, Model model, Set<Expression> Pext, boolean isExtensionalBound) {

		this.model = model;
		this.variable = variable;
		this.lastUpdatedChild = 0;
		this.parent = new HashSet<Expression>();
		this.parent.add(Parent);
		this.entirelyDiscover = false;
		this.phiInsideSubModel = new HashSet<Expression>();
		this.children = new ArrayList<FactorComponent>();
		this.cutsetInsideSubModel = new HashSet<Expression>();
		this.cutsetOutsideSubModel = new HashSet<Expression>();
		this.schema = new HashSet<Expression>();
		this.bound = Bounds.simplex(new ArrayList<Expression>(Arrays.asList(this.variable)), this.model,isExtensionalBound);
		this.model.context = this.model.context.extendWithSymbolsAndTypes(this.variable.toString(), "Boolean");
		this.isExtensionalBound = isExtensionalBound;

		Set<Expression> intersection = new HashSet<Expression>();
		intersection.addAll(model.getNeighborsOfSet(model.getInitializedVariable()));
		Collection<Expression> S = model.getNeighbors(variable);
		for (Expression e : this.parent) {
			S.remove(e);
		}
		if (S.isEmpty()){
			this.entirelyDiscover = true;
			this.bound= Bounds.makeSingleElementBound(makeSymbol(1), isExtensionalBound);
		}
		S.retainAll(intersection);
		if (!S.isEmpty()) {
			this.cutsetOutsideSubModel.add(variable);
		}

		model.initializeVariableComponent.add(this);
		
		//we add the varaible to the context and tell of which type is the variable add
		//Expression typeOfVariable = this.model.getValues(variable);
		//this.model.context = this.model.context.extendWithSymbolsAndTypes(this.variable.toString(), typeOfVariable);
	}

	public void update(Set<Expression> Pext, Boolean withBound) {

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
						FactorComponent newC = new FactorComponent(e, variable, model, Pext, isExtensionalBound);
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
			int j = this.chooseDepthImportantFirst();
			Set<Expression> union = new HashSet<Expression>(Pext);
			for (int i = 0; i < this.children.size(); i++) {
				union.addAll(this.children.get(i).phiInsideSubModel);
			}
			this.children.get(j).update(union, withBound);

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
		
		if(withBound){
			this.calculateBound();
		}
		else{
			this.calculateSchema();
		}
	}

	public int chooseDepthFirst() {
		for (int j = 0; j<this.children.size(); j++){
			if (!this.children.get(j).entirelyDiscover){
				return j;
			}
		}
		return 0;
	}

	public int chooseBreadthFirst() {
		for (int j = lastUpdatedChild + 1; j<this.children.size(); j++){
			if (!this.children.get(j).entirelyDiscover){
				this.lastUpdatedChild = j;
				return j;
			}
		}
		for (int j = 0; j<lastUpdatedChild; j++){
			if (!this.children.get(j).entirelyDiscover){
				this.lastUpdatedChild = j;
				return j;
			}
		}
		return lastUpdatedChild;
	}

	public int chooseDepthImportantFirst() {
		Expression min = parse("{ 1 }");
		Integer jToReturn = 0;
		for (int j = 0; j<this.children.size(); j++){
			if (!this.children.get(j).entirelyDiscover){
				
				Set<Expression> toMultiply = this.model.getNeighbors(this.children.get(j).phi);
				toMultiply.removeAll(this.children.get(j).parent);
				
				DefaultExtensionalUniSet varToMultiply = new DefaultExtensionalUniSet(new ArrayList(toMultiply));
				
				IndexExpressionsSet indices = getIndexExpressionsOfFreeVariablesIn(varToMultiply, this.model.context);
				//TODO Find a way to do : intersection (variablesToBeSummedOut, FreeVariables(phi))
				
				Expression setOfFactorInstantiations = IntensionalSet.makeMultiSet(
						indices,
						Bounds.normalizeSingleExpression(this.children.get(j).phi, this.model.theory, this.model.context),//head
						makeSymbol(true)//No Condition
						);
				
				Expression firstProduct = apply(TIMES, setOfFactorInstantiations);
				Expression evaluation = this.model.theory.evaluate(firstProduct, this.model.context);
				
				DefaultExtensionalUniSet varToSum = new DefaultExtensionalUniSet(new ArrayList(this.children.get(j).parent));
				
				indices = getIndexExpressionsOfFreeVariablesIn(varToSum, this.model.context);
				
				setOfFactorInstantiations = IntensionalSet.makeMultiSet(
						indices,
						evaluation,//head
						makeSymbol(true)//No Condition
						);
				
				Expression sumOnPhi = apply(SUM, setOfFactorInstantiations);
				evaluation = this.model.theory.evaluate(sumOnPhi, this.model.context);
				
				if(evaluation.compareTo(min) < 0){
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
		
		
		for (Expression cutsetVariable : this.cutsetInsideSubModel){
			//childrenMessage = theory.evaluate(childrenMessage, context);
			//String str = "sum({{ (on " + cutsetVariable + " in " + this.model.getValues(cutsetVariable) +" ) " + childrenMessage + " }})";
			//childrenMessage = parse(str);
			Expression valuesTakenByVariableToSum = this.model.getValues(cutsetVariable);
			IndexExpressionsSet indices = new ExtensionalIndexExpressionsSet(apply(IN, cutsetVariable, valuesTakenByVariableToSum ));
			Expression intensionalMultiSet = IntensionalSet.makeMultiSet(indices, childrenMessage, parse("true")); 
			Expression summation = apply(SUM,  intensionalMultiSet);
			childrenMessage=summation;
		}
		
		return 	theory.evaluate(childrenMessage, context);

	}

	
	public void calculateBound(){
		Theory theory = this.model.theory;
		Context context = this.model.context;		
		
		//Expression childrenBound = parse("{ 1 }");
		
		//for(FactorComponent children : this.children){
		//	childrenBound = Bounds.boundProduct(theory, context, childrenBound, children.bound);
		//}
		
		Bound[] childrenArray = new Bound[children.size()];
		int i = 0;
		for(FactorComponent children : this.children){
			childrenArray[i] = children.bound;
			i++;
		}
		Bound childrenBound;
		if(childrenArray.length != 0){
			childrenBound = Bounds.boundProduct(this.model.theory, this.model.context, isExtensionalBound, childrenArray);
		}
		else{
			childrenBound = Bounds.makeSingleElementBound(makeSymbol(1), true);
		}
		
		Iterator<Expression> iteratorToVariables = this.cutsetInsideSubModel.iterator();
		ArrayList<Expression> variablesToBeSummedOut = new ArrayList<>(this.cutsetInsideSubModel.size());
		for(Expression var : Util.in(iteratorToVariables)){
			variablesToBeSummedOut.add(var);
		}
		//We want sum other toSum of Phi*childrenBound
		DefaultExtensionalUniSet varToSum = new DefaultExtensionalUniSet(variablesToBeSummedOut);
		
		bound = childrenBound.summingBound(varToSum, context, theory);
		
	}
	
	public void calculateSchema(){
		Theory theory = this.model.theory;
		Context context = this.model.context;		
		Set<Expression> freeVariables =new HashSet<Expression>();
		freeVariables.add(this.variable);
		for(FactorComponent children : this.children){
			freeVariables.addAll(Expressions.freeVariables(children.bound, context));
		}
		
		freeVariables.removeAll(this.cutsetInsideSubModel);
		
		this.schema = freeVariables;
	}
	
	public Expression naiveCalcul(){
		Expression expressiontoSum = this.model.naiveCalculation(this.variable);
		//String values = this.model.getValues(this.variable);
		//String string = "(" + expression + ")/sum({{ (on "  + this.variable + " in " + values +" ) " + expression  + " }})";
		
		//Expression expressionToSum = theory.evaluate(childrenMessage, context);
		Expression valuesTakenByVariableToSum = this.model.getValues(this.variable);
		IndexExpressionsSet indices = new ExtensionalIndexExpressionsSet(apply(IN, this.variable, valuesTakenByVariableToSum ));
		Expression intensionalMultiSet = IntensionalSet.makeMultiSet(indices, expressiontoSum, parse("true")); 
		Expression summation = apply(SUM, intensionalMultiSet);
		//childrenMessage=summation;
		
		Expression normalizalisation = this.model.theory.evaluate(summation, this.model.context);
		Expression result = apply("/", expressiontoSum, normalizalisation);
		result = this.model.theory.evaluate(result, this.model.context);
		
		return result;
	}
	
}
