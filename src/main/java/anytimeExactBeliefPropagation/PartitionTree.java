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
 * This is shown here by the attributes {@code node} and {@code setOfFactors}.
 * Those a partition of those {@code setOfFactors} is found in the attribute {@code parents}. 
 * This means that the Union of p.setOfFactors, for p in partition is equal to this.setOfFactors. 
 * 
 * @author ferreira
 *
 */


public class PartitionTree {
	public Set<FactorNode> setOfFactors;
	public Set<PartitionTree> children;
	public Node node;
	public PartitionTree parent;
	
   	public PartitionTree(Node node) {
		this.node = node;
		
		children = new HashSet<>();
		
	}
	
   	public PartitionTree(Node root, Model model) {
   		this(root);
		CreatePartitionTreeWithBFS(model);
		CompleteTree();
	}
   	
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
	 * One if one wishes to print the setOfFactors of each node, set {@code PrintSetOfFactorsAlso} to {@code true}, and otherwise {@code false}
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
			println(indentation + setOfFactors.size());
		}
		
		i++;
		for(PartitionTree p : children){
			p.printTree(i+1, PrintSetOfFactorsAlso);
		}
	}

	private void CompleteTree(){
		setOfFactors = new HashSet<>();
		if(node.isFactor()){
			setOfFactors.add((FactorNode)this.node);
		}
		if(children.size() == 0){
			return;
		}
		
		for(PartitionTree p : children){
			p.CompleteTree();
		}
		
		if(node.isFactor()){
			for(PartitionTree p : children){
				setOfFactors.addAll(p.setOfFactors);
			}
		}
		if(node.isVariable()){
			for(PartitionTree p : children){
				setOfFactors.addAll(p.setOfFactors);
				//setOfFactors.add((FactorNode) p.node);
			}
		}
	}
}
