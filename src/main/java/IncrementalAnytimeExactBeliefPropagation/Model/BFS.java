package IncrementalAnytimeExactBeliefPropagation.Model;

import java.util.*;

import com.sri.ai.util.collect.ManyToManyRelation;

import IncrementalAnytimeExactBeliefPropagation.Model.Node.FactorNode;
import IncrementalAnytimeExactBeliefPropagation.Model.Node.VariableNode;
import IncrementalAnytimeExactBeliefPropagation.PartitionTree;
import IncrementalAnytimeExactBeliefPropagation.Model.Node.Node;

public class BFS implements Iterator<PartitionTree> {
    private Set<FactorNode> visited = new HashSet<>();
    private Queue<FactorNode> queue = new LinkedList<>();
    private ManyToManyRelation<VariableNode,FactorNode> graph;
    private HashMap<Node, PartitionTree> getPartitionOfANode = new HashMap<>();
    
    private boolean first = true;
    PartitionTree partitionQuery;

    public BFS(ManyToManyRelation<VariableNode,FactorNode> g, VariableNode query) {
        if(g.containsA(query)) {
            this.graph = g;
            
            PartitionTree queryPartitionTree = new PartitionTree(query);
            this.partitionQuery = queryPartitionTree;
            getPartitionOfANode.put(query, queryPartitionTree);
            
            Set<FactorNode> factorsLinkedToQuery = new HashSet<>();
            
            for(FactorNode childOfQuery : graph.getBsOfA(query)){
            		factorsLinkedToQuery.add(childOfQuery);
            		PartitionTree childOfQueryPartition = new PartitionTree(childOfQuery,queryPartitionTree);
            		getPartitionOfANode.put(childOfQuery, childOfQueryPartition);
            }
            
            this.queue.addAll(factorsLinkedToQuery);
            this.visited.addAll(factorsLinkedToQuery);
        }else{
            throw new IllegalArgumentException("Vertext does not exits");
        }
    }
    
    public BFS(Model m){
    		this(m.getEntireGraph(),m.getQuery());
    }
    
    @Override
    public void remove() {
        throw new UnsupportedOperationException();
    }

    @Override
    public boolean hasNext() {
        return !this.queue.isEmpty();
    }

    @Override
    public PartitionTree next() {
    	if(first){
    		first = false;
    		return this.partitionQuery;
    	}
    	
        if(!hasNext())
            throw new NoSuchElementException();
        //removes from front of queue
        FactorNode nextFactor = queue.remove();
        PartitionTree nextFactorPartition = getPartitionOfANode.get(nextFactor);
 
        for(VariableNode neighborVariable : graph.getAsOfB(nextFactor)){
        		PartitionTree neighborVariablePartition = getPartitionOfANode.get(neighborVariable);
        		if(neighborVariablePartition == null){
        			neighborVariablePartition = new PartitionTree(neighborVariable,nextFactorPartition);
        			getPartitionOfANode.put(neighborVariable, neighborVariablePartition);
        		}
	        	for(FactorNode neighborFactor : graph.getBsOfA(neighborVariable)){
	        		if (!this.visited.contains(neighborFactor)) {
	        			PartitionTree neighborFactorPartition = new PartitionTree(neighborFactor,neighborVariablePartition);
	        			getPartitionOfANode.put(neighborFactor, neighborFactorPartition);
	        			
	        			this.queue.add(neighborFactor);
	        			this.visited.add(neighborFactor);
	        		}
	        	}
        }
        return nextFactorPartition;
    }

}
//
//public class BFS<F,V> implements Iterator<F> {
//    private Set<F> visited = new HashSet<>();
//    private Queue<F> queue = new LinkedList<>();
//    private ManyToManyRelation<V,F> graph;
//
//    public BFS(ManyToManyRelation<V,F> g, V query) {
//        if(g.containsA(query)) {
//            this.graph = g;
//            Set<F> factorsLinkedToQuery = new HashSet<>();
//            factorsLinkedToQuery.addAll(graph.getBsOfA(query));
//            this.queue.addAll(factorsLinkedToQuery);
//            this.visited.addAll(factorsLinkedToQuery);
//        }else{
//            throw new IllegalArgumentException("Vertext does not exits");
//        }
//    }
//    
//    @Override
//    public void remove() {
//        throw new UnsupportedOperationException();
//    }
//
//    @Override
//    public boolean hasNext() {
//        return !this.queue.isEmpty();
//    }
//
//    @Override
//    public F next() {
//        if(!hasNext())
//            throw new NoSuchElementException();
//        //removes from front of queue
//        F next = queue.remove();
//        for(V neighorVariable : graph.getAsOfB(next)){
//	        	for(F neighborFactor : graph.getBsOfA(neighorVariable)){
//	        		 if (!this.visited.contains(neighborFactor)) {
//	                     this.queue.add(neighborFactor);
//	                     this.visited.add(neighborFactor);
//	        		 }
//	        	}
//        }
//        return next;
//    }
//
//}