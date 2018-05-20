package IncrementalAnytimeExactBeliefPropagation.Model;

import static com.sri.ai.util.Util.myAssert;

import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.NoSuchElementException;
import java.util.Queue;
import java.util.Set;

import IncrementalAnytimeExactBeliefPropagation.PartitionTree;
import IncrementalAnytimeExactBeliefPropagation.Model.Node.FactorNode;
import IncrementalAnytimeExactBeliefPropagation.Model.Node.Node;
import IncrementalAnytimeExactBeliefPropagation.Model.Node.VariableNode;

import com.sri.ai.util.collect.ManyToManyRelation;

public class BFS implements Iterator<PartitionTree> {
	
    private Set<FactorNode> visited = new HashSet<>();
    private Queue<FactorNode> queue = new LinkedList<>();
    private ManyToManyRelation<VariableNode, FactorNode> graph;
    private HashMap<Node, PartitionTree> fromNodeToPartition = new HashMap<>();
    
    private boolean first = true;
    PartitionTree partitionQuery;

    public BFS(ManyToManyRelation<VariableNode, FactorNode> graph, VariableNode query) {

    	myAssert(graph.containsA(query), () -> "Graph does not contain query " + query);

    	this.graph = graph;
    	this.partitionQuery = new PartitionTree(query);
    	
    	fromNodeToPartition.put(query, partitionQuery);

    	Set<FactorNode> factorsOnQuery = new HashSet<>();

    	for (FactorNode factorOnQuery : graph.getBsOfA(query)) {
    		factorsOnQuery.add(factorOnQuery);
    		PartitionTree factorOnQueryPartition = new PartitionTree(factorOnQuery, partitionQuery);
    		fromNodeToPartition.put(factorOnQuery, factorOnQueryPartition);
    	}

    	this.queue.addAll(factorsOnQuery);
    	this.visited.addAll(factorsOnQuery);
    }

    public BFS(Model model) {
    	this(model.getEntireGraph(), model.getQuery());
    }

    @Override
    public void remove() {
    	throw new UnsupportedOperationException();
    }

    @Override
    public boolean hasNext() {
        boolean result = ! queue.isEmpty();
		return result;
    }

    @Override
    public PartitionTree next() {
    	if (first) {
    		first = false;
    		return this.partitionQuery;
    	}
    	
        if (!hasNext())
            throw new NoSuchElementException();
        // removes from front of queue
        FactorNode nextFactor = queue.remove();
        PartitionTree nextFactorPartition = fromNodeToPartition.get(nextFactor);
 
        for (VariableNode neighborVariable : graph.getAsOfB(nextFactor)) {
        		PartitionTree neighborVariablePartition = fromNodeToPartition.get(neighborVariable);
        		if (neighborVariablePartition == null) {
        			neighborVariablePartition = new PartitionTree(neighborVariable,nextFactorPartition);
        			fromNodeToPartition.put(neighborVariable, neighborVariablePartition);
        		}
	        	for (FactorNode neighborFactor : graph.getBsOfA(neighborVariable)) {
	        		if ( ! visited.contains(neighborFactor)) {
	        			PartitionTree neighborFactorPartition = new PartitionTree(neighborFactor, neighborVariablePartition);
	        			fromNodeToPartition.put(neighborFactor, neighborFactorPartition);
	        			queue.add(neighborFactor);
	        			visited.add(neighborFactor);
	        		}
	        	}
        }
        return nextFactorPartition;
    }
}