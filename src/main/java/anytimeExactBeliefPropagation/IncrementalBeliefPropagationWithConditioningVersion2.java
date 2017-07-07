package anytimeExactBeliefPropagation;

import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;

import com.sri.ai.grinder.sgdpllt.library.bounds.Bound;

import anytimeExactBeliefPropagation.Model.Model;
import anytimeExactBeliefPropagation.Model.Node.FactorNode;
import anytimeExactBeliefPropagation.Model.Node.Node;
import anytimeExactBeliefPropagation.Model.Node.VariableNode;

public class IncrementalBeliefPropagationWithConditioningVersion2 {
	private Model model;
	public PartitionTree partitionTree;
	
	private Map<Node,PartitionTree> findPartitionGivenANode;
	
	public IncrementalBeliefPropagationWithConditioningVersion2(Model model) {
		this.model = model;
		findPartitionGivenANode = new HashMap<>();
		partitionTree = new PartitionTree(model.getQuery());
	}
	
	public Bound ExpandAndComputeInference(Iterator<FactorNode> it){
		if(it.hasNext()){
			PartitionTree newFactorPartition = ExpandModel(it);
			//TODO update the tree
			
			Bound result = inference();
			return result;
		}
		return null;
	}

	public PartitionTree ExpandModel(Iterator<FactorNode> it){
		if(it.hasNext()){
			//get factor to add
			FactorNode newfactor = it.next();
			//expand the model (add new factor to the explored part of the graph)
			model.ExpandModel(newfactor);
			//create partition tree of new Factor
			PartitionTree newPartition = new PartitionTree(newfactor);
			findPartitionGivenANode.put(newfactor, newPartition);
			Collection<VariableNode> exploredVariablesInModel = model.getExploredVariables();
			
			//get variables to be put below new factor in the three
			//adding them to the tree
			Collection<VariableNode> variablesToBePutItTheThree = model.getVariablesOfAFactor(newfactor);
			variablesToBePutItTheThree.retainAll(exploredVariablesInModel);
			for(VariableNode v : variablesToBePutItTheThree){
				PartitionTree p = new PartitionTree(v);
				newPartition.children.add(p);
			}
			
			//get variables to be put below new factor in the tree	
			Collection<VariableNode> variablesInTheTreeLinkedToFactor = model.getVariablesOfAFactor(newfactor);
			variablesInTheTreeLinkedToFactor.removeAll(exploredVariablesInModel);
			
			if(variablesInTheTreeLinkedToFactor.isEmpty()){
				return null;
			}
			
			//get parent and link new factor to his parent
			VariableNode var = variablesInTheTreeLinkedToFactor.iterator().next();
			PartitionTree parentOfNewFactor = findPartitionGivenANode.get(var);
			parentOfNewFactor.children.add(newPartition);
			newPartition.parent = parentOfNewFactor;
			
			return newPartition;
		}
		return null;
	}
	
	public Bound inference(){
		//TODO Do inference
		
		
		return null;
	}

//	/**
//	 * Given the partition, compute the separator. TODO more efficient implementation
//	 * @param p
//	 * @return
//	 */
//	private Set<VariableNode> ComputeSeparator(PartitionTree pTree){
//		//Create sets with the variables in each partition
//		List<Set<VariableNode>> VariablePartition = new ArrayList<Set<VariableNode>>();
//		for(PartitionTree p : pTree.children){
//			Set<VariableNode> variablesOfP = new HashSet<>();
//			for(FactorNode phi : p.setOfFactorsInsidePartition){
//				Collection<VariableNode> VarsOfPhi= model.getExploredGraph().getAsOfB(phi);
//				variablesOfP.addAll(VarsOfPhi);
//			}
//			VariablePartition.add(variablesOfP);
//		}
//		//take the variables that compose the intersection of those sets
//		Set<VariableNode> separatorVariables = new HashSet<>();
//		
//		for (int i = 0; i < VariablePartition.size(); i++) {
//			for (int j = i+1; j <VariablePartition.size(); j++) {
//				Set<VariableNode> intersectionAti = new HashSet<>();
//				intersectionAti.addAll(VariablePartition.get(i));
//				intersectionAti.retainAll(VariablePartition.get(j));
//				
//				separatorVariables.addAll(intersectionAti);
//			}
//		}
//		return separatorVariables;
//	}
//	
//	private PairOf<Set<VariableNode>> ComputeSeparatorOnThisLevelAndSeparatorOnLevelsBelow(PartitionTree partition, Set<VariableNode> SeparatorVariablesOnLevelAbove){
//		/** 
//		 * compute the separator. 3 types:
//		 * 						separators for levels above this 	(SeparatorVariablesOnLevelAbove)
//		 * 						separators for this level 			(SeparatorOnThisLevel)
//		 * 						separators for levels below this 	(SeparatorForNextLevels)
//		 */
//		Set<VariableNode> SeparatorOnThisLevel = ComputeSeparator(partition);
//		if(partition.node.isVariable()){
//			SeparatorOnThisLevel.remove((VariableNode) partition.node);
//		}
//		//exclude the variables on other levels. they will be summed afterwards
//		SeparatorOnThisLevel.removeAll(SeparatorVariablesOnLevelAbove);
//		
//		Set<VariableNode> SeparatorForNextLevels = new HashSet<>();
//		SeparatorForNextLevels.addAll(SeparatorOnThisLevel);
//		SeparatorForNextLevels.addAll(SeparatorVariablesOnLevelAbove);
//		
//		PairOf<Set<VariableNode>> result = 
//				new PairOf<>(SeparatorOnThisLevel,SeparatorForNextLevels);
//		return result;
//	}

}
