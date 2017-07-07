package anytimeExactBeliefPropagation;

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

import anytimeExactBeliefPropagation.Model.Model;
import anytimeExactBeliefPropagation.Model.Node.FactorNode;
import anytimeExactBeliefPropagation.Model.Node.VariableNode;

public class IncrementalBeliefPropagationWithConditioning {
	private boolean AllExplored;
	public PartitionTree partitionTree;
	
	
	public IncrementalBeliefPropagationWithConditioning(Model model) {
		AllExplored = false;
		this.partitionTree=new PartitionTree(model);
	}
	
	public Bound ExpandAndComputeInference(Iterator<FactorNode> it){
		if(it.hasNext()){
			this.partitionTree.model.ExpandModel(it);
			Bound result = inference();
			return result;
		}
		return null;
	}
	
	public Bound InferenceOverEntireModel(){
		this.partitionTree.model.SetExploredGraphToEntireGraph();
		Bound result = inference();
		return result;
	}
	
	public Bound inference(){
		VariableNode query = this.partitionTree.model.getQuery();
		this.partitionTree = new PartitionTree(query,this.partitionTree.model);
		
		AllExplored = this.partitionTree.model.AllExplored();
		
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
		if(!AllExplored && !this.partitionTree.model.isExhausted((VariableNode) partitionInAVariableNode.node)){
			Expression var = partitionInAVariableNode.node.getValue();
			Bound bound = Bounds.simplex(arrayList(var), this.partitionTree.model.getTheory(), this.partitionTree.model.getContext(), this.partitionTree.model.isExtensional());
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
		
		Bound bound = Bounds.boundProduct(this.partitionTree.model.getTheory(), this.partitionTree.model.getContext(), this.partitionTree.model.isExtensional(), boundsOfChildrenMessages);
		
		ArrayList<Expression> varToSumOutList = new ArrayList<>();
		varToSumOutList.addAll(variablesToSumOut);
		Expression varToSumOut = new DefaultExtensionalMultiSet(varToSumOutList);
		
		bound = bound.summingBound(varToSumOut, this.partitionTree.model.getContext(), this.partitionTree.model.getTheory());
		
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
		
		 
		Bound bound = Bounds.boundProduct(this.partitionTree.model.getTheory(), this.partitionTree.model.getContext(), this.partitionTree.model.isExtensional(), boundsOfChildrenMessages);
		
		ArrayList<Expression> varToSumOutList = new ArrayList<>();
		varToSumOutList.addAll(variablesToSumOut);
		Expression varToSumOut = new DefaultExtensionalMultiSet(varToSumOutList);
		
		bound = bound.summingPhiTimesBound(varToSumOut, partitionInAFactorNode.node.getValue(), this.partitionTree.model.getContext(), this.partitionTree.model.getTheory());
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
			for(FactorNode phi : p.setOfFactorsInsidePartition){
				Collection<VariableNode> VarsOfPhi= this.partitionTree.model.getExploredGraph().getAsOfB(phi);
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
