package anytimeExactBeliefPropagation;

import static com.sri.ai.util.Util.println;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.core.DefaultExtensionalMultiSet;
import com.sri.ai.grinder.sgdpllt.library.bounds.Bound;
import com.sri.ai.grinder.sgdpllt.library.bounds.Bounds;

import anytimeExactBeliefPropagation.Model.Model;
import anytimeExactBeliefPropagation.Model.Node.FactorNode;
import anytimeExactBeliefPropagation.Model.Node.VariableNode;

public class BeliefPropagationWithConditioning {
	private Model model;
	public PartitionTree partitionTree;
	
	public BeliefPropagationWithConditioning(Model model) {
		this.model = model;
		
		VariableNode query = model.getQuery();
		this.partitionTree = new PartitionTree(query,model);
	}
	
	public Bound inference(){	
		variableMessage(partitionTree, new HashSet<VariableNode>());
		Bound result = partitionTree.node.getBound();
		return result;
	}
	
	public void variableMessage(PartitionTree partitionInAVariableNode, Set<VariableNode> SeparatorVariablesOnLevelAbove){//or notToSumVariables
		if(!partitionInAVariableNode.node.isVariable()){
			println("erro no -S-BP!!!");
			return;
		}
		/** 
		 * compute the separator. 3 types:
		 * 						separators for levels above this 	(SeparatorVariablesOnLevelAbove)
		 * 						separators for this level 			(SeparatorOnThisLevel)
		 * 						separators for levels below this 	(SeparatorForNextLevels)
		 */
		Set<VariableNode> SeparatorOnThisLevel = ComputeSeparator(partitionInAVariableNode);
		//exclude the variables on other levels. they will be summed afterwards(TODO not so sure about it...)
		SeparatorOnThisLevel.removeAll(SeparatorVariablesOnLevelAbove);
		
		Set<VariableNode> SeparatorForNextLevels = new HashSet<>();
		SeparatorForNextLevels.addAll(SeparatorOnThisLevel);
		SeparatorForNextLevels.addAll(SeparatorVariablesOnLevelAbove);
		
		// calling children partitions. for each partition, call message passing, 
		// store bound
		Bound[]  boundsOfChildrenMessages = new Bound[partitionInAVariableNode.partition.size()];
		Set<Expression> variablesToSumOut = new HashSet<>();
		for(VariableNode v : SeparatorOnThisLevel){
			variablesToSumOut.add(v.getValue());
		}
		
		int i = 0;
		for(PartitionTree p : partitionInAVariableNode.partition){
			factorMessage(p,SeparatorForNextLevels);
			Bound boundInP = p.node.getBound();
			boundsOfChildrenMessages[i] = boundInP;
			i++;
		}
		 
		Bound bound = Bounds.boundProduct(model.getTheory(), model.getContext(), model.isExtensional(), boundsOfChildrenMessages);
		
		//NOT SURE :(
		ArrayList<Expression> varToSumOutList = new ArrayList<>();
		varToSumOutList.addAll(variablesToSumOut);
		Expression varToSumOut = new DefaultExtensionalMultiSet(varToSumOutList);
		
		bound = bound.summingBound(varToSumOut, model.getContext(), model.getTheory());
		
		partitionInAVariableNode.node.setBound(bound);
	}
	public void factorMessage(PartitionTree partitionInAFactorNode, Set<VariableNode> SeparatorVariablesOnLevelAbove){
		if(!partitionInAFactorNode.node.isFactor()){
			println("erro no -S-BP!!!");
			return;
		}
		/** 
		 * compute the separator. 3 types:
		 * 						separators for levels above this 	(SeparatorVariablesOnLevelAbove)
		 * 						separators for this level 			(SeparatorOnThisLevel)
		 * 						separators for levels below this 	(SeparatorForNextLevels)
		 */
		Set<VariableNode> SeparatorOnThisLevel = ComputeSeparator(partitionInAFactorNode);
		//exclude the variables on other levels. they will be summed afterwards(TODO not so sure about it...)
		SeparatorOnThisLevel.removeAll(SeparatorVariablesOnLevelAbove);
		
		Set<VariableNode> SeparatorForNextLevels = new HashSet<>();
		SeparatorForNextLevels.addAll(SeparatorOnThisLevel);
		SeparatorForNextLevels.addAll(SeparatorVariablesOnLevelAbove);
		
		// calling children partitions. for each partition, call message passing, 
		// store VariableNode (we are going to sum them all out) and
		// store bound
		Bound[]  boundsOfChildrenMessages = new Bound[partitionInAFactorNode.partition.size()];
		Set<Expression> variablesToSumOut = new HashSet<>();
		for(VariableNode v : SeparatorOnThisLevel){
			variablesToSumOut.add(v.getValue());
		}
		
		int i =0;
		for(PartitionTree p : partitionInAFactorNode.partition){
			variableMessage(p,SeparatorForNextLevels);
			Bound boundInP = p.node.getBound();
			boundsOfChildrenMessages[i] = boundInP;
			variablesToSumOut.add(p.node.getValue());
			i++;
		}
		 
		Bound bound = Bounds.boundProduct(model.getTheory(), model.getContext(), model.isExtensional(), boundsOfChildrenMessages);
		
		//NOT SURE :(
		ArrayList<Expression> varToSumOutList = new ArrayList<>();
		varToSumOutList.addAll(variablesToSumOut);
		Expression varToSumOut = new DefaultExtensionalMultiSet(varToSumOutList);
		
		bound = bound.summingPhiTimesBound(varToSumOut, partitionInAFactorNode.node.getValue(), model.getContext(), model.getTheory());
		
		partitionInAFactorNode.node.setBound(bound);
	}
	
	
	/**
	 * Given the partition, compute the separator. TODO more efficient implementation
	 * @param p
	 * @return
	 */
	public Set<VariableNode> ComputeSeparator(PartitionTree pTree){
		//Create sets with the variables in each partition
		List<Set<VariableNode>> VariablePartition = new ArrayList<Set<VariableNode>>();
		for(PartitionTree p : pTree.partition){
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
}
