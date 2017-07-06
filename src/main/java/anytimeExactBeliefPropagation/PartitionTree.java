package anytimeExactBeliefPropagation;

import static com.sri.ai.util.Util.println;

import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.Queue;
import java.util.Set;

import anytimeExactBeliefPropagation.Model.Model;
import anytimeExactBeliefPropagation.Model.Node.FactorNode;
import anytimeExactBeliefPropagation.Model.Node.Node;
import anytimeExactBeliefPropagation.Model.Node.VariableNode;

/**
 * This class defines a partition tree for factors of a given {@link Model}.
 * 
 * In the S-BP algorithm, each {@link Node} has it's own partition, and sees a given part of the graph.
 * This is shown here by the attributes {@code node} and {@code setOfFactorsInsidePartition}.
 * Those a partition of those {@code setOfFactorsInsidePartition} is found in the attribute {@code parents}. 
 * This means that the Union of p.setOfFactorsInsidePartition, for p in partition is equal to this.setOfFactorsInsidePartition. 
 * 
 * @author ferreira
 *
 */


public class PartitionTree {
	public Set<FactorNode> setOfFactorsInsidePartition;
	public Set<VariableNode> setOfVariablesInsidePartition;
	public Set<PartitionTree> children;
	public Node node;
	public PartitionTree parent;
	public Set<VariableNode> Separator;
	
   	public PartitionTree(Node node) {
		this.node = node;
		
		children = new HashSet<>();
		
	}
	
   	public PartitionTree(Node root, Model model) {
   		this(root);
		CreatePartitionTreeWithBFS(model);
		CompleteTree();
	}
   	
//   	public void add(FactorNode newFactor){
//   		PartitionTree p = choosePlaceToPlaceFactor();
////   		for each variable of p, if variable does not belong to any other factor in the tree,
////   		 then add variable below p.
//   		
//   		
//   		
//   		p.addPartitionToPartitionTreeAndUpdatePArtitionTree();
//   	}
   	
   	private void addPartitionToPartitionTreeAndUpdatePArtitionTree(){
   		FactorNode newFactor = (FactorNode) this.node;
   		updateSetOfFactors(newFactor);
   		updateCutSet();
   		updateBound();
   	}
   	public void updateSetOfFactors(FactorNode newFactor){
   		this.setOfFactorsInsidePartition.add(newFactor);
   		if(this.parent!=null){
   			this.parent.updateSetOfFactors(newFactor);
   		}
   	}
   	public void updateCutSet(){
   		if (this.parent!=null){
   			this.parent.updateCutSet();
   		}
   		this.Separator = this.findCutSet();
   	}
   	
   	public Set<VariableNode> findCutSet(){
   		Set<VariableNode> setOfCusets = new HashSet<VariableNode>();
   		for(PartitionTree child1 : this.children){
   	   		for(PartitionTree child2 : this.children){
   	   			if(!child1.equals(child2)){
   	   			Set<VariableNode> copyOfVariablesInChild1 = copySet(child1.setOfVariablesInsidePartition);
   	   			copyOfVariablesInChild1.retainAll(child2.setOfVariablesInsidePartition);
	   	   		setOfCusets.addAll(copyOfVariablesInChild1);
   	   			}
   	   		}
   		}
   		return setOfCusets;
   	}
   	
   	public static Set<VariableNode> copySet(Set<VariableNode> setOfVariablesToCopy){
   		Set<VariableNode> setOfVariables = new HashSet<VariableNode>();
   		for (VariableNode variableNode : setOfVariablesToCopy){
   			setOfVariables.add(variableNode);
   		}
   		return setOfVariables;
   	}
   	
   	public void updateBound(){
   		
   	}
   	
   	//TODO : change way of expanding model
   	
   	/**
   	 * This class creates a partition based on a BFS process.
   	 * 
   	 * There are various ways to create the partition tree.
   	 * 
   	 * In fact any partition tree will provide an exact inference. But some of them will
   	 * generate so many separator variables (or cutset variables) that the inference time
   	 * can be close to the Naive computation.  
   	 * 
   	 * The study of the complexity of choosing the optimal tree (or a good heuristic) 
   	 * still needs to be done in the project. (TODO)
   	 *  
   	 * The choice of a BFS seems intuitively good because it distributes the nodes more or less
   	 * evenly among the partitions 
   	 * 
   	 * @param model
   	 */
	public void CreatePartitionTreeWithBFS(Model model){
		HashMap<Node, PartitionTree> hashTable= new HashMap<>();
	    Set<Node> visited = new HashSet<>();
	    Queue<Node> queue = new LinkedList<>();
	    
	    visited.add(node);
	    queue.add(node);
	    hashTable.put(this.node, this);
	    
	    while(!queue.isEmpty()){
	    	Node n = queue.remove();
	    	PartitionTree parentPartition = hashTable.get(n);
	    	
	    	Set<Node> neighbors = new HashSet<>();
	    	
	    	if(n.isFactor()){
				Collection<VariableNode> variableNeighbors = model.getExploredGraph().getAsOfB((FactorNode) n);
				neighbors.addAll(variableNeighbors);
	    	}
	    	else{
	    		Collection<FactorNode> factorNeighbors = model.getExploredGraph().getBsOfA((VariableNode) n);
				neighbors.addAll(factorNeighbors);
	    	}
	    	
		    	for(Node neighbor: neighbors){
		    		if (!visited.contains(neighbor)) {
	                    queue.add(neighbor);
	                    visited.add(neighbor);
	                    
	                    PartitionTree pChild = new PartitionTree(neighbor);
	 					pChild.parent = parentPartition;
	 					hashTable.put(neighbor, pChild);
						parentPartition.children.add(pChild);
	       		 	}
		    	}
	    }
	}
	
	/**
	 * This will print on the standard output the nodes of the partition tree in a tree indentation.
	 * One if one wishes to print the setOfFactorsInsidePartition of each node, set {@code PrintSetOfFactorsAlso} to {@code true}, and otherwise {@code false}
	 *  
	 * @param PrintSetOfFactorsAlso
	 */
	public void printTree(boolean PrintSetOfFactorsAlso){
		printTree(0,PrintSetOfFactorsAlso);
	}
	private void printTree(int i,boolean PrintSetOfFactorsAlso){
		String indentation = "";
		for (int j = 0; j < i; j++) {
			indentation = indentation + "\t";
		}
		println(indentation + node);
		if(PrintSetOfFactorsAlso){
			println(indentation + setOfFactorsInsidePartition.size());
		}
		
		i++;
		for(PartitionTree p : children){
			p.printTree(i+1, PrintSetOfFactorsAlso);
		}
	}

	private void CompleteTree(){
		setOfFactorsInsidePartition = new HashSet<>();
		if(node.isFactor()){
			setOfFactorsInsidePartition.add((FactorNode)this.node);
		}
		if(children.size() == 0){
			return;
		}
		
		for(PartitionTree p : children){
			p.CompleteTree();
		}
		
		if(node.isFactor()){
			for(PartitionTree p : children){
				setOfFactorsInsidePartition.addAll(p.setOfFactorsInsidePartition);
			}
		}
		if(node.isVariable()){
			for(PartitionTree p : children){
				setOfFactorsInsidePartition.addAll(p.setOfFactorsInsidePartition);
				//setOfFactorsInsidePartition.add((FactorNode) p.node);
			}
		}
	}
}
