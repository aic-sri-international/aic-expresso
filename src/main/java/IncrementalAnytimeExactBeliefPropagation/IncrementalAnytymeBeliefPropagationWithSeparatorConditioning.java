package IncrementalAnytimeExactBeliefPropagation;

import static com.sri.ai.util.Util.arrayList;
import static com.sri.ai.util.Util.println;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Set;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.core.DefaultExtensionalMultiSet;
import com.sri.ai.grinder.sgdpllt.library.bounds.Bound;
import com.sri.ai.grinder.sgdpllt.library.bounds.Bounds;
import com.sri.ai.util.base.PairOf;

import IncrementalAnytimeExactBeliefPropagation.Model.Model;
import IncrementalAnytimeExactBeliefPropagation.Model.Node.FactorNode;
import IncrementalAnytimeExactBeliefPropagation.Model.Node.VariableNode;

public class IncrementalAnytymeBeliefPropagationWithSeparatorConditioning {
	private Model model;
	private boolean AllExplored;
	public PartitionTree partitionTree;
	private Iterator<PartitionTree> partitionTreeIterator; 
	
	public IncrementalAnytymeBeliefPropagationWithSeparatorConditioning(Model model, Iterator<PartitionTree> partitionTreeIterator) {
		this.model = model;
		AllExplored = false;
		this.partitionTreeIterator = partitionTreeIterator;
		if (partitionTreeIterator.hasNext()){
			partitionTree = partitionTreeIterator.next();
		}
		else{
			partitionTree = null;
		}
	}
	
	public boolean AllExplored(){
		return !partitionTreeIterator.hasNext();
	}
	
	public Bound ExpandAndComputeInference(){
		if(partitionTreeIterator.hasNext()){
			PartitionTree nextFactorPartitionTree = partitionTreeIterator.next();
			FactorNode nextFactor = (FactorNode) nextFactorPartitionTree.node;
			
			model.ExpandModel(nextFactor);
			
			updatePartitionTree(nextFactorPartitionTree);
			
			Bound result = partitionTree.node.getBound();
			
			return result;
		}
		return null;
	}
	
	public void updatePartitionTree(PartitionTree p){
		FactorNode newFactor = (FactorNode) p.node;
		Collection<VariableNode> variablesOfNewFactor = model.getVariablesOfAFactor(newFactor);
		
		updateSetOfFactorsInPartitionTree(p,newFactor);		
		updateSetOfVariablesInPartitionTree(p, variablesOfNewFactor);
   		
		updateCutSet(p,newFactor);
   		updateBounds();
   	}
	
	
 /*------------------------------------------------------------------------------------------------------------------------*/
   	
 	public void updateSetOfFactorsInPartitionTree(PartitionTree p,FactorNode newFactor){	
   		while(p != null){
   			p.setOfFactors.add(newFactor);
   			p = p.parent;
   		}
   	}
 /*------------------------------------------------------------------------------------------------------------------------*/
 	public void updateSetOfVariablesInPartitionTree(PartitionTree p,Collection<VariableNode> variablesOfNewFactor){	
   		while(p != null){
   			p.setOfVariables.addAll(variablesOfNewFactor);
   			p.setOfVariables.remove(p.node);
   			p = p.parent;
   		}
   	}
   	
 /*------------------------------------------------------------------------------------------------------------------------*/
 
 	private void updateCutSet(PartitionTree newFactorPartition,FactorNode newFactor){
 	
   		//we take all variables of this factor, and remove those that haven't appeared in other parts of the graph
   		Collection<VariableNode> newSeparatorVariables = model.getVariablesOfAFactor(newFactor);
   			//we remove the children
   		for(PartitionTree p : newFactorPartition.children){
   			newSeparatorVariables.remove(p.node);
   		}
   			//and the parent. 
   		newSeparatorVariables.remove(newFactorPartition.parent.node);
   		
   		//Update this cutset, and all above together
   		addingToCutSet(newFactorPartition, newSeparatorVariables, null);
   		   		
   		//Update cutset of "Virgin Variables" . acho que isso vem depois do addingToCutset
   		for(PartitionTree p : newFactorPartition.children){
   			Set<VariableNode> SeparatorOfAllAboveForChildrenOfNewFactor = new HashSet<>();
   			SeparatorOfAllAboveForChildrenOfNewFactor.addAll(newFactorPartition.Separator);
   			SeparatorOfAllAboveForChildrenOfNewFactor.addAll(newFactorPartition.cutsetOfAllLevelsAbove);
   			
   			p.cutsetOfAllLevelsAbove = SeparatorOfAllAboveForChildrenOfNewFactor; 
   		}
   	}

   	private void addingToCutSet(PartitionTree currentNode, Collection<VariableNode> toAdd, PartitionTree notToUpdate){
   		// chamar no conjunto de cima.
   		if(currentNode != null && currentNode.parent != null){
   			//Chama a funcao pro pai, e atualiza o LAS
   			addingToCutSet(currentNode.parent,toAdd,currentNode);
   			
   			currentNode.cutsetOfAllLevelsAbove.addAll(currentNode.parent.Separator);
   			currentNode.cutsetOfAllLevelsAbove.addAll(currentNode.parent.cutsetOfAllLevelsAbove);
   			
   			currentNode.Separator.removeAll(currentNode.cutsetOfAllLevelsAbove);
   		}
   		
   		currentNode.recomputeBound = true;
   		toAdd.removeAll(currentNode.Separator);
   		if(toAdd.isEmpty()){
   			return;
   		}
   		
   		List<Set<VariableNode>> listOfMiniCutsets = new ArrayList<>();
   		
   		for (PartitionTree p : currentNode.children){
   			if(!p.equals(notToUpdate)){
	   			HashSet<VariableNode> toAddInThisChild = new HashSet<>();
	   			toAddInThisChild.addAll(toAdd);
	   			toAddInThisChild.retainAll(p.setOfVariables);
	   			//toAddInThisChild.removeAll(currentNode.Separator);
	   			
	   			updateLASandSeparator(p, toAddInThisChild);//essa funcao so desce na arvore e adiciona toAdd no LAS e tira toAdd do separator.
	   			
	   			listOfMiniCutsets.add(toAddInThisChild); // melhorar nome
   			}
   		}
   		
   		for(Set<VariableNode> miniCutset : listOfMiniCutsets){
   			currentNode.Separator.addAll(miniCutset);
   			toAdd.removeAll(miniCutset);
   		}
   	}
   	
   	public void updateLASandSeparator(PartitionTree partition ,Set<VariableNode> toAdd){
   		partition.recomputeBound = true;
   		//intercessao de to add e separator
   		toAdd.removeAll(partition.Separator);
   		
   		//subtrai to add das variav
   		partition.Separator.removeAll(toAdd);
   		partition.cutsetOfAllLevelsAbove.addAll(toAdd);
   		
   		//se toAdd naoVazio!
   		if(!toAdd.isEmpty()){
   			for(PartitionTree p : partition.children){
   				Set<VariableNode> toAddAux = new HashSet<>();
   				toAddAux.addAll(toAdd);
   				p.updateLASandSeparator(toAddAux);
   			}
   		}
   	}
/*------------------------------------------------------------------------------------------------------------------------*/   	
   	
   	public void updateBounds(){
   		updateBounds(partitionTree);
   	}
	public void updateBounds(PartitionTree currentNode){
		if(!currentNode.recomputeBound){
			return;
		}
		
		currentNode.recomputeBound = false;
		
		//if variable and not exhauted, simplex
		if(currentNode.node.isVariable() && !model.isExhausted((VariableNode) currentNode.node)){
			Expression var = currentNode.node.getValue();
			Bound bound = Bounds.simplex(arrayList(var), model.getTheory(), model.getContext(), model.isExtensional());
			currentNode.node.setBound(bound);
			return;
		}
		
		//for all children, recompute their bounds
		for(PartitionTree p : currentNode.children){
			updateBounds(p);
		}
		
		if(currentNode.node.isFactor()){
			Bound b = factorMessage(currentNode);
			currentNode.node.setBound(b);
		}
		
		if(currentNode.node.isVariable()){
			Bound b = variableMessage(currentNode);
			currentNode.node.setBound(b);
		}
	}
	
	private Bound factorMessage(PartitionTree currentNode){
		Set<Expression> variablesToSumOut = new HashSet<>();
		for(VariableNode v : currentNode.Separator){
			variablesToSumOut.add(v.getValue());
		}
		
		Bound[] boundsOfChildrenMessages = new Bound[currentNode.children.size()];
		int i = 0;
		for(PartitionTree p : currentNode.children){
			Bound boundInP = p.node.getBound();
			boundsOfChildrenMessages[i] = boundInP;
			variablesToSumOut.add(p.node.getValue());
			i++;
		}
		
		for(VariableNode v : currentNode.cutsetOfAllLevelsAbove){
			variablesToSumOut.remove(v.getValue());
		}
		 
		Bound bound = Bounds.boundProduct(model.getTheory(), model.getContext(), model.isExtensional(), boundsOfChildrenMessages);
		
		ArrayList<Expression> varToSumOutList = new ArrayList<>();
		varToSumOutList.addAll(variablesToSumOut);
		Expression varToSumOut = new DefaultExtensionalMultiSet(varToSumOutList);
		
		bound = bound.summingPhiTimesBound(varToSumOut, currentNode.node.getValue(), model.getContext(), model.getTheory());
		return bound;
	}
	
	public Bound variableMessage(PartitionTree currentNode){
		Set<Expression> variablesToSumOut = new HashSet<>();
		for(VariableNode v : currentNode.Separator){
			variablesToSumOut.add(v.getValue());
		}
		
		Bound[]  boundsOfChildrenMessages = new Bound[currentNode.children.size()];
		int i = 0;
		for(PartitionTree p : currentNode.children){
			Bound boundInP = p.node.getBound();
			boundsOfChildrenMessages[i] = boundInP;
			i++;
		}
		
		Bound bound = Bounds.boundProduct(model.getTheory(), model.getContext(), model.isExtensional(), boundsOfChildrenMessages);
		
		ArrayList<Expression> varToSumOutList = new ArrayList<>();
		varToSumOutList.addAll(variablesToSumOut);
		Expression varToSumOut = new DefaultExtensionalMultiSet(varToSumOutList);
		
		bound = bound.summingBound(varToSumOut, model.getContext(), model.getTheory());
		
		return bound;
	}
   	
 /*------------------------------------------------------------------------------------------------------------------------*/   	
	
	public Bound InferenceOverEntireModel(){
		model.SetExploredGraphToEntireGraph();
		Bound result = inference();
		return result;
	}
	
	public Bound inference(){
		VariableNode query = model.getQuery();
		this.partitionTree = new PartitionTree(query,model);
		
		AllExplored = model.AllExplored();
		
		Bound result = variableMessage(partitionTree, new HashSet<VariableNode>());
		return result;
	}
		
	public Bound variableMessage(PartitionTree partitionInAVariableNode, Set<VariableNode> SeparatorVariablesOnLevelAbove){//or notToSumVariables
		if(!partitionInAVariableNode.node.isVariable()){
			println("error in S-BP!!!");
			return null;
		}

		PairOf<Set<VariableNode>> sep = ComputeSeparatorOnThisLevelAndSeparatorOnLevelsBelow(partitionInAVariableNode, SeparatorVariablesOnLevelAbove);
		Set<VariableNode> SeparatorOnThisLevel = sep.first;
		Set<VariableNode> SeparatorForNextLevels = sep.second;
		
		// calling children partitions. for each partition, call message passing, 
		// store bound
		Bound[]  boundsOfChildrenMessages = new Bound[partitionInAVariableNode.children.size()];
		Set<Expression> variablesToSumOut = new HashSet<>();
		for(VariableNode v : SeparatorOnThisLevel){
			variablesToSumOut.add(v.getValue());
		}
		
		// if this node is not exhausted (see definition in Model) it means that the message coming to it is the 
		// simplex, no matter how it is what comes below in the partition.
		// obs. it can be equivalently thought as attaching a "simplex factor" to non exhausted nodes.
		if(!AllExplored && !model.isExhausted((VariableNode) partitionInAVariableNode.node)){
			Expression var = partitionInAVariableNode.node.getValue();
			Bound bound = Bounds.simplex(arrayList(var), model.getTheory(), model.getContext(), model.isExtensional());
//			partitionInAVariableNode.node.setBound(bound);
			return bound;
		}
		
		int i = 0;
		for(PartitionTree p : partitionInAVariableNode.children){
			Bound boundInP = factorMessage(p,SeparatorForNextLevels);
			//Bound boundInP = p.node.getBound();
			boundsOfChildrenMessages[i] = boundInP;
			i++;
		}
		
		Bound bound = Bounds.boundProduct(model.getTheory(), model.getContext(), model.isExtensional(), boundsOfChildrenMessages);
		
		ArrayList<Expression> varToSumOutList = new ArrayList<>();
		varToSumOutList.addAll(variablesToSumOut);
		Expression varToSumOut = new DefaultExtensionalMultiSet(varToSumOutList);
		
		bound = bound.summingBound(varToSumOut, model.getContext(), model.getTheory());
		
		return bound;
		//partitionInAVariableNode.node.setBound(bound);
	}

	public Bound factorMessage(PartitionTree partitionInAFactorNode, Set<VariableNode> SeparatorVariablesOnLevelAbove){
		if(!partitionInAFactorNode.node.isFactor()){
			println("error in S-BP!!!");
			return null;
		}
		
		PairOf<Set<VariableNode>> sep = ComputeSeparatorOnThisLevelAndSeparatorOnLevelsBelow(partitionInAFactorNode, SeparatorVariablesOnLevelAbove);
		Set<VariableNode> SeparatorOnThisLevel = sep.first;
		Set<VariableNode> SeparatorForNextLevels = sep.second;
		
		// calling children partitions. for each partition, call message passing, 
		// store VariableNode (we are going to sum them all out) and
		// store bound
		Bound[]  boundsOfChildrenMessages = new Bound[partitionInAFactorNode.children.size()];
		Set<Expression> variablesToSumOut = new HashSet<>();
		for(VariableNode v : SeparatorOnThisLevel){
			variablesToSumOut.add(v.getValue());
		}
		
		int i =0;
		for(PartitionTree p : partitionInAFactorNode.children){
			Bound boundInP = variableMessage(p,SeparatorForNextLevels);
			//Bound boundInP = p.node.getBound();
			boundsOfChildrenMessages[i] = boundInP;
			variablesToSumOut.add(p.node.getValue());
			i++;
		}
		
		for(VariableNode v : SeparatorVariablesOnLevelAbove){
			variablesToSumOut.remove(v.getValue());
		}
		
		 
		Bound bound = Bounds.boundProduct(model.getTheory(), model.getContext(), model.isExtensional(), boundsOfChildrenMessages);
		
		ArrayList<Expression> varToSumOutList = new ArrayList<>();
		varToSumOutList.addAll(variablesToSumOut);
		Expression varToSumOut = new DefaultExtensionalMultiSet(varToSumOutList);
		
		bound = bound.summingPhiTimesBound(varToSumOut, partitionInAFactorNode.node.getValue(), model.getContext(), model.getTheory());
		return bound;
		//partitionInAFactorNode.node.setBound(bound);
	}
	
	/**
	 * Given the partition, compute the separator. TODO more efficient implementation
	 * @param p
	 * @return
	 */
	private Set<VariableNode> ComputeSeparator(PartitionTree pTree){
		//Create sets with the variables in each partition
		List<Set<VariableNode>> VariablePartition = new ArrayList<Set<VariableNode>>();
		for(PartitionTree p : pTree.children){
			Set<VariableNode> variablesOfP = new HashSet<>();
			for(FactorNode phi : p.setOfFactors){
				Collection<VariableNode> VarsOfPhi= model.getExploredGraph().getAsOfB(phi);
				variablesOfP.addAll(VarsOfPhi);
			}
			VariablePartition.add(variablesOfP);
		}
		//take the variables that compose the intersection of those sets
		Set<VariableNode> separatorVariables = new HashSet<>();
		
		for (int i = 0; i < VariablePartition.size(); i++) {
			for (int j = i+1; j <VariablePartition.size(); j++) {
				Set<VariableNode> intersectionAti = new HashSet<>();
				intersectionAti.addAll(VariablePartition.get(i));
				intersectionAti.retainAll(VariablePartition.get(j));
				
				separatorVariables.addAll(intersectionAti);
			}
		}
		return separatorVariables;
	}
	
	private PairOf<Set<VariableNode>> ComputeSeparatorOnThisLevelAndSeparatorOnLevelsBelow(PartitionTree partition, Set<VariableNode> SeparatorVariablesOnLevelAbove){
		/** 
		 * compute the separator. 3 types:
		 * 						separators for levels above this 	(SeparatorVariablesOnLevelAbove)
		 * 						separators for this level 			(SeparatorOnThisLevel)
		 * 						separators for levels below this 	(SeparatorForNextLevels)
		 */
		Set<VariableNode> SeparatorOnThisLevel = ComputeSeparator(partition);
		if(partition.node.isVariable()){
			SeparatorOnThisLevel.remove((VariableNode) partition.node);
		}
		//exclude the variables on other levels. they will be summed afterwards
		SeparatorOnThisLevel.removeAll(SeparatorVariablesOnLevelAbove);
		
		Set<VariableNode> SeparatorForNextLevels = new HashSet<>();
		SeparatorForNextLevels.addAll(SeparatorOnThisLevel);
		SeparatorForNextLevels.addAll(SeparatorVariablesOnLevelAbove);
		
		PairOf<Set<VariableNode>> result = 
				new PairOf<>(SeparatorOnThisLevel,SeparatorForNextLevels);
		return result;
	}
}
