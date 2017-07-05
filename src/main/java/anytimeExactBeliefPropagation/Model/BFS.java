package anytimeExactBeliefPropagation.Model;

import java.util.*;

import com.sri.ai.util.collect.ManyToManyRelation;

public class BFS<F,V> implements Iterator<F> {
    private Set<F> visited = new HashSet<>();
    private Queue<F> queue = new LinkedList<>();
    private ManyToManyRelation<V,F> graph;

    public BFS(ManyToManyRelation<V,F> g, V query) {
        if(g.containsA(query)) {
            this.graph = g;
            Set<F> factorsLinkedToQuery = new HashSet<>();
            factorsLinkedToQuery.addAll(graph.getBsOfA(query));
            this.queue.addAll(factorsLinkedToQuery);
            this.visited.addAll(factorsLinkedToQuery);
        }else{
            throw new IllegalArgumentException("Vertext does not exits");
        }
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
    public F next() {
        if(!hasNext())
            throw new NoSuchElementException();
        //removes from front of queue
        F next = queue.remove();
        for(V neighorVariable : graph.getAsOfB(next)){
        	for(F neighborFactor : graph.getBsOfA(neighorVariable)){
        		 if (!this.visited.contains(neighborFactor)) {
                     this.queue.add(neighborFactor);
                     this.visited.add(neighborFactor);
                 }
        	}
        }
        return next;
    }

}