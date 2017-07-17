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
/**
 * This class does AnytymeBeliefPropagationWithSeparatorConditioning in an incremental way.
 * The way AnytymeBeliefPropagationWithSeparatorConditioning works is as follows:
 * 		We begin with a explored part of the graph
 * 		We add one Factor Node to this graph
 * 		We assign bounds to Variable Nodes that are not fully explored, that is have not been exhausted (See @c{@code isExhasuted} in {@link Model})
 * 		We Apply the Inference Algorithm known as Belief propagation with separator conditioning (S-BP). for this, it is necessary to :
 * 			Define a {@link PartitionTree}
 * 			do the message passing step, from the leafs to the query
 * 
 * The way the Incremental version works is as follows: 
 * 		In order for S-BP to work, a {@link PartitionTree} structure have to be defined before the message passing process starts.
 * 		This version of the AnytymeBeliefPropagationWithSeparatorConditioning algorithm manages to reuse the {@link PartitionTree} defined in a previous iteration
 * in order to save computation time used to build it. Instead, we just update certain nodes influenced by the new added factor. 
 * (P.S. This approach may have a down Side : There are many possibilities of partition threes, and the choice of the a good one entirely determines the complexity of the inference process. 
 *  On the other hand, computing a partition tree is a relatively cheap operation. Therefore, buy not rebuilding a partition tree, can lead to a much longer computation time during the S-BP step,
 *  even though time is saved in the partition tree definition phase.
 * 
 * 
 * HOW TO USE THIS CLASS?
 * 	First one has to have a iterator for the partition tree ( PS: the nodes given by the iterator must be already connected among them)
 *  One has to create a model containing all the factor nodes that are going to be (eventually) explored.
 *  In order to do the inference, of the query, the user has three options:
 *  		For incremental anytime S-BP : {@code ExpandAndComputeInference}
 *  		For anytime S-BP (rebuild partition graph at each step) : {@code ExpandAndComputeInferenceByRebuildingPartitionTree}
 *  		For inference over the whole model at once: {@code InferenceOverEntireModel}
 *  
 * @author ferreira
 *
 */
public class IncrementalAnytimeBeliefPropagationWithSeparatorConditioning {
	private Model model;
	private boolean allExplored;
	public PartitionTree partitionTree;
	private Iterator<PartitionTree> partitionTreeIterator;	// on the first iteration, it.next() gives the query (a variable node)
															// after the first iteration, it.next returns factors to be added in the partition three
	
	public IncrementalAnytimeBeliefPropagationWithSeparatorConditioning(Model model, Iterator<PartitionTree> partitionTreeIterator) {
		this.model = model;
		allExplored = false;
		this.partitionTreeIterator = partitionTreeIterator;
		if (partitionTreeIterator.hasNext()) {
			partitionTree = partitionTreeIterator.next();
		}
		else{
			partitionTree = null;
		}
	}
	
	public boolean isAllExplored() {
		return !partitionTreeIterator.hasNext();
	}
	
	public Bound expandAndComputeInferenceByRebuildingPartitionTree() {
		if (partitionTreeIterator.hasNext()) {
			PartitionTree nextFactorPartitionTree = partitionTreeIterator.next();
			FactorNode nextFactor = (FactorNode) nextFactorPartitionTree.node;
			
			model.ExpandModel(nextFactor);
			Bound result = inference();
			
			return result;
		}
		return null;
	}
		
	public Bound expandAndComputeInference() {
		if (partitionTreeIterator.hasNext()) {
			PartitionTree nextFactorPartitionTree = partitionTreeIterator.next();
			FactorNode nextFactor = (FactorNode) nextFactorPartitionTree.node;
			
			model.ExpandModel(nextFactor);
			
			updatePartitionTree(nextFactorPartitionTree);
			
			Bound result = partitionTree.node.getBound();
			
			return result;
		}
		return null;
	}
	
	private void updatePartitionTree(PartitionTree p) {
		FactorNode newFactor = (FactorNode) p.node;
		Collection<VariableNode> variablesOfNewFactor = model.getVariablesOfAFactor(newFactor);
		
		updateSetOfFactorsInPartitionTree(p,newFactor);		
		updateSetOfVariablesInPartitionTree(p, variablesOfNewFactor);
   		
		updateCutSet(p,newFactor);
   		updateBounds();
   	}
	
 /*------------------------------------------------------------------------------------------------------------------------*/
   	
 	private void updateSetOfFactorsInPartitionTree(PartitionTree p,FactorNode newFactor) {	
   		while (p != null) {
   			p.setOfFactors.add(newFactor);
   			p = p.parent;
   		}
   	}
 
 /*------------------------------------------------------------------------------------------------------------------------*/
 	private void updateSetOfVariablesInPartitionTree(PartitionTree p,Collection<VariableNode> variablesOfNewFactor) {	
   		while (p != null) {
   			p.setOfVariables.addAll(variablesOfNewFactor);
   			p.setOfVariables.remove(p.node);
   			p = p.parent;
   		}
   	}
   	
 /*------------------------------------------------------------------------------------------------------------------------*/
 
 	private void updateCutSet(PartitionTree newFactorPartition,FactorNode newFactor) {
 	// when a factor is added, it is possible that the separator of many variables have to be updated
 	// One of the guaranties that we have is that the new cutset variables of each node are certain to be among the variables of the newFactor
 	// we remove the children of the new factor because those are certain not to be in the rest of the graph (are not arguments of other factors)
 	// we remove the parent because it is not new to the graph.
 		
 		
   		// we take all variables of this factor, and remove those that haven't appeared in other parts of the graph
   		Collection<VariableNode> newSeparatorVariables = model.getVariablesOfAFactor(newFactor);
   		// we remove the children
   		for (PartitionTree p : newFactorPartition.children) {
   			newSeparatorVariables.remove(p.node);
   		}
   		// and the parent. 
   		newSeparatorVariables.remove(newFactorPartition.parent.node);
   		
   		// Update this cutset, and all above together
   		addingToCutSet(newFactorPartition, newSeparatorVariables, null);
   	}

   	private void addingToCutSet(PartitionTree currentNode, Collection<VariableNode> toAddtoSeparator, PartitionTree notToUpdate) {
   		if (currentNode != null && currentNode.parent != null) {
   			// Call to the parent then update the node.
   			addingToCutSet(currentNode.parent, toAddtoSeparator, currentNode);
   			
   			currentNode.cutsetOfAllLevelsAbove.addAll(currentNode.parent.separator);
   			currentNode.cutsetOfAllLevelsAbove.addAll(currentNode.parent.cutsetOfAllLevelsAbove);
   			currentNode.separator.removeAll(currentNode.cutsetOfAllLevelsAbove);
   		}

   		List<VariableNode> newCutSetVariables = new ArrayList<>();
   				
   		for (PartitionTree p : currentNode.children) {
   			if (!p.equals(notToUpdate)) {
	   			HashSet<VariableNode> toAddInThisChild = new HashSet<>();
	   			toAddInThisChild.addAll(toAddtoSeparator);
	   			toAddInThisChild.retainAll(p.setOfVariables);
	   			newCutSetVariables.addAll(toAddInThisChild);
   			}
   		}

   		toAddtoSeparator.removeAll(newCutSetVariables);
   		
   		currentNode.separator.addAll(newCutSetVariables);
   		currentNode.recomputeBound = true;
   		for (PartitionTree p : currentNode.children) {
   			if (!p.equals(notToUpdate)) {
	   			updateLASandSeparator(p);
   			}
   		}
   		
   	}
   	
	private void updateLASandSeparator(PartitionTree partition) {
		partition.recomputeBound = true;
		Set<VariableNode> newCutsetAbove = new HashSet<>();
		if (partition.parent == null) {
			return;
		}
		newCutsetAbove.addAll(partition.parent.cutsetOfAllLevelsAbove);
		newCutsetAbove.addAll(partition.parent.separator);
		
		if (!thisSetIncreasestheLAS(newCutsetAbove,partition)) {
			return;
		}
		partition.cutsetOfAllLevelsAbove.addAll(newCutsetAbove);
		partition.separator.removeAll(newCutsetAbove);
		
		for (PartitionTree p : partition.children) {	
			updateLASandSeparator(p);
		}
	}
	
	private boolean thisSetIncreasestheLAS(Collection<VariableNode> newCutsetOfAllAbove,PartitionTree partition) {
		Set<VariableNode> v = new HashSet<>();
		v.addAll(newCutsetOfAllAbove);
		v.removeAll(partition.cutsetOfAllLevelsAbove);
		return !v.isEmpty();
	}
   	
/*------------------------------------------------------------------------------------------------------------------------*/   	
   	/**
   	 * The Updadte Bounds is the traditional message passing that we have in S-BP. The difference is that, when the cutsets
   	 * were updated, a tag "recompute" bound was assigned to all nodes whose separator have been updated 
   	 */
   	private void updateBounds() {
   		updateBounds(partitionTree);
   	}
	private void updateBounds(PartitionTree currentNode) {
		if (!currentNode.recomputeBound) {
			return;
		}
		
		currentNode.recomputeBound = false;
		
		// if variable and not exhausted, simplex
		if (currentNode.node.isVariable() && !model.isExhausted((VariableNode) currentNode.node)) {
			Expression var = currentNode.node.getValue();
			Bound bound = Bounds.simplex(arrayList(var), model.getTheory(), model.getContext(), model.isExtensional());
			currentNode.node.setBound(bound);
			return;
		}
		
		// for all children, recompute their bounds
		for (PartitionTree p : currentNode.children) {
			updateBounds(p);
		}
		
		if (currentNode.node.isFactor()) {
			Bound b = factorMessage(currentNode);
			currentNode.node.setBound(b);
		}
		
		if (currentNode.node.isVariable()) {
			Bound b = variableMessage(currentNode);
			currentNode.node.setBound(b);
		}
	}
	
	private Bound factorMessage(PartitionTree currentNode) {
		Set<Expression> variablesToSumOut = new HashSet<>();
		for (VariableNode v : currentNode.separator) {
			variablesToSumOut.add(v.getValue());
		}
		
		Bound[] boundsOfChildrenMessages = new Bound[currentNode.children.size()];
		int i = 0;
		for (PartitionTree p : currentNode.children) {
			Bound boundInP = p.node.getBound();
			boundsOfChildrenMessages[i] = boundInP;
			variablesToSumOut.add(p.node.getValue());
			i++;
		}
		
		for (VariableNode v : currentNode.cutsetOfAllLevelsAbove) {
			variablesToSumOut.remove(v.getValue());
		}
		 
		Bound bound = Bounds.boundProduct(model.getTheory(), model.getContext(), model.isExtensional(), boundsOfChildrenMessages);
		
		ArrayList<Expression> varToSumOutList = new ArrayList<>();
		varToSumOutList.addAll(variablesToSumOut);
		Expression varToSumOut = new DefaultExtensionalMultiSet(varToSumOutList);
		
		bound = bound.summingPhiTimesBound(varToSumOut, currentNode.node.getValue(), model.getContext(), model.getTheory());
		return bound;
	}
	
	private Bound variableMessage(PartitionTree currentNode) {
		Set<Expression> variablesToSumOut = new HashSet<>();
		for (VariableNode v : currentNode.separator) {
			variablesToSumOut.add(v.getValue());
		}
		
		Bound[]  boundsOfChildrenMessages = new Bound[currentNode.children.size()];
		int i = 0;
		for (PartitionTree p : currentNode.children) {
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
/**
 * from here the code corresponds to anytimeBeliefPropagation (re-do partitioning each time)
 * 
 *  PROS: 	MUCH easier to understand!
 *  			Splits the process of expanding the graph from the one of defining a partition tree of the graph 
 *  				(DECIDING THE OPTIMAL PARTITION TREE IS NP HARD, SO IT MAKES COMPLETE SENSE TO SEPARATE THOSE TWO PROCESSES)	
 *  
 *  CONS:	If the implementation of both the Partition Tree and the Expansion of the graph s done with a BFS (which is the current solution)
 *  			it becomes less costly to profit from the already built Partition Tree (that is to use the IncrementalVesion).  
 */
	
	
	public Bound inferenceOverEntireModel() {
		model.SetExploredGraphToEntireGraph();
		Bound result = inference();
		return result;
	}
	
	public Bound inference() {
		VariableNode query = model.getQuery();
		this.partitionTree = new PartitionTree(query,model);
		
		allExplored = model.AllExplored();
		
		Bound result = variableMessage(partitionTree, new HashSet<VariableNode>());
		return result;
	}
		
	private Bound variableMessage(PartitionTree partitionInAVariableNode, Set<VariableNode> SeparatorVariablesOnLevelAbove) {// or notToSumVariables
		if (!partitionInAVariableNode.node.isVariable()) {
			println("error in S-BP!!!");
			return null;
		}

		PairOf<Set<VariableNode>> sep = computeSeparatorOnThisLevelAndSeparatorOnLevelsBelow(partitionInAVariableNode, SeparatorVariablesOnLevelAbove);
		Set<VariableNode> SeparatorOnThisLevel = sep.first;
		Set<VariableNode> SeparatorForNextLevels = sep.second;
		
		// calling children partitions. for each partition, call message passing, 
		// store bound
		Bound[]  boundsOfChildrenMessages = new Bound[partitionInAVariableNode.children.size()];
		Set<Expression> variablesToSumOut = new HashSet<>();
		for (VariableNode v : SeparatorOnThisLevel) {
			variablesToSumOut.add(v.getValue());
		}
		
		// if this node is not exhausted (see definition in Model) it means that the message coming to it is the 
		// simplex, no matter how it is what comes below in the partition.
		// obs. it can be equivalently thought as attaching a "simplex factor" to non exhausted nodes.
		if (!allExplored && !model.isExhausted((VariableNode) partitionInAVariableNode.node)) {
			Expression var = partitionInAVariableNode.node.getValue();
			Bound bound = Bounds.simplex(arrayList(var), model.getTheory(), model.getContext(), model.isExtensional());
//			partitionInAVariableNode.node.setBound(bound);
			return bound;
		}
		
		int i = 0;
		for (PartitionTree p : partitionInAVariableNode.children) {
			Bound boundInP = factorMessage(p,SeparatorForNextLevels);
			// Bound boundInP = p.node.getBound();
			boundsOfChildrenMessages[i] = boundInP;
			i++;
		}
		
		Bound bound = Bounds.boundProduct(model.getTheory(), model.getContext(), model.isExtensional(), boundsOfChildrenMessages);
		
		ArrayList<Expression> varToSumOutList = new ArrayList<>();
		varToSumOutList.addAll(variablesToSumOut);
		Expression varToSumOut = new DefaultExtensionalMultiSet(varToSumOutList);
		
		bound = bound.summingBound(varToSumOut, model.getContext(), model.getTheory());
		
		return bound;
		// partitionInAVariableNode.node.setBound(bound);
	}

	private Bound factorMessage(PartitionTree partitionInAFactorNode, Set<VariableNode> SeparatorVariablesOnLevelAbove) {
		if (!partitionInAFactorNode.node.isFactor()) {
			println("error in S-BP!!!");
			return null;
		}
		
		PairOf<Set<VariableNode>> sep = computeSeparatorOnThisLevelAndSeparatorOnLevelsBelow(partitionInAFactorNode, SeparatorVariablesOnLevelAbove);
		Set<VariableNode> SeparatorOnThisLevel = sep.first;
		Set<VariableNode> SeparatorForNextLevels = sep.second;
		
		// calling children partitions. for each partition, call message passing, 
		// store VariableNode (we are going to sum them all out) and
		// store bound
		Bound[]  boundsOfChildrenMessages = new Bound[partitionInAFactorNode.children.size()];
		Set<Expression> variablesToSumOut = new HashSet<>();
		for (VariableNode v : SeparatorOnThisLevel) {
			variablesToSumOut.add(v.getValue());
		}
		
		int i = 0;
		for (PartitionTree p : partitionInAFactorNode.children) {
			Bound boundInP = variableMessage(p,SeparatorForNextLevels);
			// Bound boundInP = p.node.getBound();
			boundsOfChildrenMessages[i] = boundInP;
			variablesToSumOut.add(p.node.getValue());
			i++;
		}
		
		for (VariableNode v : SeparatorVariablesOnLevelAbove) {
			variablesToSumOut.remove(v.getValue());
		}
		
		 
		Bound bound = Bounds.boundProduct(model.getTheory(), model.getContext(), model.isExtensional(), boundsOfChildrenMessages);
		
		ArrayList<Expression> varToSumOutList = new ArrayList<>();
		varToSumOutList.addAll(variablesToSumOut);
		Expression varToSumOut = new DefaultExtensionalMultiSet(varToSumOutList);
		
		bound = bound.summingPhiTimesBound(varToSumOut, partitionInAFactorNode.node.getValue(), model.getContext(), model.getTheory());
		return bound;
		// partitionInAFactorNode.node.setBound(bound);
	}
	
	/**
	 * Given the partition, compute the separator. TODO more efficient implementation
	 * @param p
	 * @return
	 */
	private Set<VariableNode> computeSeparator(PartitionTree pTree) {
		// Create sets with the variables in each partition
		List<Set<VariableNode>> VariablePartition = new ArrayList<Set<VariableNode>>();
		for (PartitionTree p : pTree.children) {
			Set<VariableNode> variablesOfP = new HashSet<>();
			for (FactorNode phi : p.setOfFactors) {
				Collection<VariableNode> VarsOfPhi= model.getExploredGraph().getAsOfB(phi);
				variablesOfP.addAll(VarsOfPhi);
			}
			VariablePartition.add(variablesOfP);
		}
		// take the variables that compose the intersection of those sets
		Set<VariableNode> separatorVariables = new HashSet<>();
		
		for (int i = 0; i < VariablePartition.size(); i++) {
			for (int j = i + 1; j <VariablePartition.size(); j++) {
				Set<VariableNode> intersectionAti = new HashSet<>();
				intersectionAti.addAll(VariablePartition.get(i));
				intersectionAti.retainAll(VariablePartition.get(j));
				
				separatorVariables.addAll(intersectionAti);
			}
		}
		return separatorVariables;
	}
	
	private PairOf<Set<VariableNode>> computeSeparatorOnThisLevelAndSeparatorOnLevelsBelow(PartitionTree partition, Set<VariableNode> SeparatorVariablesOnLevelAbove) {
		/** 
		 * compute the separator. 3 types:
		 * 						separators for levels above this 	(SeparatorVariablesOnLevelAbove)
		 * 						separators for this level 			(SeparatorOnThisLevel)
		 * 						separators for levels below this 	(SeparatorForNextLevels)
		 */
		Set<VariableNode> SeparatorOnThisLevel = computeSeparator(partition);
		if (partition.node.isVariable()) {
			SeparatorOnThisLevel.remove((VariableNode) partition.node);
		}
		// exclude the variables on other levels. they will be summed afterwards
		SeparatorOnThisLevel.removeAll(SeparatorVariablesOnLevelAbove);
		
		Set<VariableNode> SeparatorForNextLevels = new HashSet<>();
		SeparatorForNextLevels.addAll(SeparatorOnThisLevel);
		SeparatorForNextLevels.addAll(SeparatorVariablesOnLevelAbove);
		
		PairOf<Set<VariableNode>> result = 
				new PairOf<>(SeparatorOnThisLevel,SeparatorForNextLevels);
		return result;
	}
}
