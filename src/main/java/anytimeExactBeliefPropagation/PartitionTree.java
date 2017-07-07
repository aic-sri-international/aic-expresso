package anytimeExactBeliefPropagation;

import static com.sri.ai.util.Util.println;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.Queue;
import java.util.Set;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.sgdpllt.api.Context;
import com.sri.ai.grinder.sgdpllt.api.Theory;
import com.sri.ai.grinder.sgdpllt.library.bounds.Bound;

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
	public Set<VariableNode> Separator;//TODO replace Separator by a set of Expression
	public Set<VariableNode> cutsetOfAllLevelsAbove;
	
   	public PartitionTree(Node node) {
		this.node = node;
		
		children = new HashSet<>();
		this.Separator = new HashSet<VariableNode>();
		this.cutsetOfAllLevelsAbove = new HashSet<VariableNode>();
		
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
   	
   	private void addPartitionToPartitionTreeAndUpdatePArtitionTree(Model model){
   		FactorNode newFactor = (FactorNode) this.node;
   		updateSetsOfFactorsAndVariables(newFactor, model);
   		updateCutSet();
   		updateBounds(model);
   	}
   	
 /*------------------------------------------------------------------------------------------------------------------------*/
   	
   	public void updateSetsOfFactorsAndVariables(FactorNode newFactor, Model model){
   		Set<VariableNode> newVariables = new HashSet<VariableNode>();
   		newVariables.addAll(model.getExploredGraph().getAsOfB(newFactor));//we look at the variables involved in the factor
   		newVariables.remove(this.parent.node.getValue());//we remove the parent, which is already in the variable set
   		this.updateSetsOfFactorsAndVariables(newFactor, newVariables);
   	}
   	
   	public void updateSetsOfFactorsAndVariables(FactorNode newFactor, Set<VariableNode>  newVariables){
   		this.setOfFactorsInsidePartition.add(newFactor);
   		this.setOfVariablesInsidePartition.addAll(newVariables);
   		if(this.parent!=null){
   			this.parent.updateSetsOfFactorsAndVariables(newFactor, newVariables);
   		}
   	}
   	
 /*------------------------------------------------------------------------------------------------------------------------*/
 
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
   	   				copyOfVariablesInChild1.retainAll(child2.setOfVariablesInsidePartition);//we keep all the variables that the two children have in common
   	   	   			copyOfVariablesInChild1.removeAll(cutsetOfAllLevelsAbove);//we remove the variables which have been taken as cutset in levels above
   	   	   			//be careful here maybe we have to remove more variables   	   					
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

/*------------------------------------------------------------------------------------------------------------------------*/
   	
   	public void updateBounds(Model model){
   		this.updateCurrentBound(model);
   		this.updateBoundOfParent(model);
   	}
   	
	public void updateCurrentBound(Model model){
   		Bound childrenProduct = this.childrenProduct(model);
   		if (this.node.isVariable()){
   			Bound newBound=this.sum(model, childrenProduct);
   			this.node.setBound(newBound);
   		}else{
   			Bound newBound=this.messageFromVariableToFactor(model, childrenProduct);
   			this.node.setBound(newBound);
   		}
   	}
   	
   	public void updateBoundOfParent(Model model){
   		if(this.parent!=null){
   			this.parent.updateBounds(model);
   		}
   	}
   	
   	public Bound messageFromVariableToFactor(Model model, Bound childrenProduct){
   		Context context = model.getContext();
   		Theory theory = model.getTheory();
   		ArrayList<Expression> varToSum = this.getVarToSumInMessageFromVariableToFactor();
   		return childrenProduct.summingBound(varToSum, context, theory);
   	}
   	
   	public ArrayList<Expression> getVarToSumInMessageFromVariableToFactor(){
   		ArrayList<Expression> result = new ArrayList<>();
   		//add all children
   		for(PartitionTree p : this.children){
   			result.add(p.node.getValue());
   		}
   		
   		//add this level separator variables 
   		for(VariableNode v : this.Separator){
   			result.add(v.getValue());
   		}
   		
   		//take above variables out.
   		for(VariableNode v : this.cutsetOfAllLevelsAbove){
   			result.remove(v.getValue());
   		}
   		return result;
   	}
   	
   	public Bound sum(Model model, Bound childrenProduct){
   		Context context = model.getContext();
   		Theory theory = model.getTheory();
   
   		ArrayList<Expression> varToSum = this.copySetOfVariableToListOfExpression();
   		return childrenProduct.summingBound(varToSum, context, theory);
   	}
   	
   	public ArrayList<Expression> copySetOfVariableToListOfExpression(){
   		ArrayList<Expression> varToSum = new ArrayList<>();
   		//varToSum.addAll(this.Separator);
   		for(VariableNode v : this.Separator){
   			varToSum.add(v.getValue());
   		}
   		return varToSum;
   	}
   	
   	public Bound childrenProduct(Model model){
   		Theory theory = model.getTheory();
		Context context = model.getContext();	
	
		Bound[] childrenArray = new Bound[children.size()];
		int i = 0;
		for(PartitionTree children : this.children){
			childrenArray[i] = children.node.getBound();
			i++;
		}
		//Bound childrenBound = Bounds.boundProduct(theory, context, isExtensionalBound,childrenArray);//to modify
		//return childrenBound;
		return null;
   	}
   	
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
