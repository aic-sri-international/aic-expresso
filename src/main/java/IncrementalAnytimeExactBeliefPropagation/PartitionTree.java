package IncrementalAnytimeExactBeliefPropagation;

import static com.sri.ai.util.Util.println;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Queue;
import java.util.Set;

import IncrementalAnytimeExactBeliefPropagation.Model.Model;
import IncrementalAnytimeExactBeliefPropagation.Model.Node.FactorNode;
import IncrementalAnytimeExactBeliefPropagation.Model.Node.Node;
import IncrementalAnytimeExactBeliefPropagation.Model.Node.VariableNode;

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
	public Set<FactorNode> setOfFactors;
	public Set<VariableNode> setOfVariables;
	public Set<PartitionTree> children;
	public Node node;
	public PartitionTree parent;
	public Set<VariableNode> Separator;//TODO replace Separator by a set of Expression
	public Set<VariableNode> cutsetOfAllLevelsAbove;
	public boolean recomputeBound;
	
		
	public PartitionTree(Node node) {
		this.node = node;
		children = new HashSet<>();
		this.Separator = new HashSet<VariableNode>();
		this.cutsetOfAllLevelsAbove = new HashSet<VariableNode>();
		this.setOfFactors= new HashSet<FactorNode>();
		this.setOfVariables= new HashSet<VariableNode>();
		recomputeBound = true;
	}

	public PartitionTree(Node node,PartitionTree parent) {
		this(node);
		this.parent = parent;
		if (parent != null){
			parent.children.add(this);
		}
	}
	
   	public PartitionTree(Node root, Model model) {
   		this(root);
		CreatePartitionTreeWithBFS(model);
		CompleteTree();
	}
   	   	
  
   	
 /*------------------------------------------------------------------------------------------------------------------------*/
   	
   
   	
 /*------------------------------------------------------------------------------------------------------------------------*/
 

/*------------------------------------------------------------------------------------------------------------------------*/
 //Alternative iplementation of updating cutset:
   	public void updateCutSet2(Model m){
   		FactorNode factor = (FactorNode) this.node;
   		//we take all variables of this factor, and remove those that haven't appeared in other parts of the graph
   		Collection<VariableNode> newSeparatorVariables = m.getVariablesOfAFactor(factor);
   		newSeparatorVariables.removeAll(this.children);
   		newSeparatorVariables.remove(this.parent);
   		
   		//Update cutset of "Virgin Variables"
   		//TODO
   		
   		
   		//Update this cutset, and all above together
   		addingToCutSet(newSeparatorVariables, null);
   	}

   	private Set<VariableNode> addingToCutSet(Collection<VariableNode> toAdd, PartitionTree notToUpdate){
   		// chamar no conjunto de cima.
   		if(this != null && this.parent != null){
   			//Chama a funcao pro pai, e atualiza o LAS
   			Set<VariableNode> LevelAbobeSeparator = parent.addingToCutSet(toAdd,this);
   			this.cutsetOfAllLevelsAbove = LevelAbobeSeparator;
   			this.Separator.removeAll(this.cutsetOfAllLevelsAbove);
   		}
   		
   		List<Set<VariableNode>> listOfMiniCutsets = new ArrayList<>();
   		
   		for (PartitionTree p : this.children){
   			if(!p.equals(notToUpdate)){
	   			HashSet<VariableNode> toAddInThisChild = new HashSet<>();
	   			toAddInThisChild.addAll(toAdd);
	   			toAddInThisChild.retainAll(p.setOfVariables);
	   			toAddInThisChild.removeAll(this.Separator);
	   			
	   			p.updateLASandSeparator(toAddInThisChild);//essa funcao so desce na arvore e adiciona toAdd no LAS e tira toAdd do separator.
	   			
	   			listOfMiniCutsets.add(toAddInThisChild); // melhorar nome
   			}
   		}
   		
   		for(Set<VariableNode> miniCutset : listOfMiniCutsets){
   			this.Separator.addAll(miniCutset);
   		}
   		
   		toAdd.removeAll(this.Separator); // acho que ta certo, mas talvez tenha que ser so remover a uniao do minisets
   		//this.LAS = parent.LAS Union 
   		
   		
   		//retornar this.Separator + this. LAS (acho)
   		Set<VariableNode> result = new HashSet<>();
   		result.addAll(this.Separator);
   		result.addAll(this.cutsetOfAllLevelsAbove);
   		return result;
   	}
   	
   	public void updateLASandSeparator(Set<VariableNode> toAdd){
   		//intercessao de to add e separator
   		toAdd.removeAll(this.Separator);
   		
   		//subtrai to add das variav
   		this.Separator.removeAll(toAdd);
   		this.cutsetOfAllLevelsAbove.addAll(toAdd);
   		
   		//se toAdd naoVazio!
   		if(!toAdd.isEmpty()){
   			for(PartitionTree p : this.children){
   				Set<VariableNode> toAddAux = new HashSet<>();
   				toAddAux.addAll(toAdd);
   				p.updateLASandSeparator(toAddAux);
   			}
   		}
   	}
   	
   	


/*------------------------------------------------------------------------------------------------------------------------*/
 

   	
/*------------------------------------------------------------------------------------------------------------------------*/
   	
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
				//setOfFactorsInsidePartition.add((FactorNode) p.node);
			}
		}
	}
}
