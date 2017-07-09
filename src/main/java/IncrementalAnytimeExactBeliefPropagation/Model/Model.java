package IncrementalAnytimeExactBeliefPropagation.Model;

import java.util.Collection;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Set;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.grinder.sgdpllt.api.Context;
import com.sri.ai.grinder.sgdpllt.api.Theory;
import com.sri.ai.grinder.sgdpllt.library.bounds.Bound;
import com.sri.ai.grinder.sgdpllt.library.bounds.DefaultExtensionalBound;
import com.sri.ai.grinder.sgdpllt.library.bounds.DefaultIntensionalBound;
import com.sri.ai.util.base.Triple;
import com.sri.ai.util.collect.ManyToManyRelation;

import IncrementalAnytimeExactBeliefPropagation.Model.Node.FactorNode;
import IncrementalAnytimeExactBeliefPropagation.Model.Node.VariableNode;

/**
 * 
 * This class creates a model of a factor graph that is incrementally explored. 
 * 
 * It contains a {@code graphicalModel} as an attribute, which represents the entire graph
 * and remains unchanged during the inference process.
 * 
 * It also contains a {@code exploredGraphicalModel} attribute, that stores the part of the 
 * graph seen so far. The inference (done by the {@link BeliefPropagationWithConditioning}
 * class) is always done over the partial graph, which is then expanded through the method
 * {@code ExpandModel}.
 * 
 * @author ferreira
 *
 */

public class Model {
	private ManyToManyRelation<VariableNode, FactorNode> graphicalModel;
	private ManyToManyRelation<VariableNode, FactorNode> exploredGraphicalModel;
	private VariableNode query;
	
	private Context context;
	private Theory theory;
	
	private boolean isExtensional;
	
	public Model(Theory theory, Context context, boolean isExtensional, VariableNode query) {
		this.graphicalModel = new ManyToManyRelation<>();
		this.exploredGraphicalModel= new ManyToManyRelation<>();
		this.theory = theory;
		this.context = context;
		this.isExtensional = isExtensional;
		this.query = query;	
	}
	public Model(Set<Expression> Factors,Theory theory, Context context, boolean isExtensional, VariableNode query) {
		this(theory, context, isExtensional, query);
		for (Expression factor : Factors) {
			FactorNode f = new FactorNode(factor,isExtensional,theory,context);
			if(factor != null){
				for (Expression variable : Expressions.freeVariables(factor, context)) {
					VariableNode v = new VariableNode(variable, isExtensional,theory,context);
					graphicalModel.add(v,f);
				}
			}
		}
	}
	public Model(Set<Expression> Factors,Theory theory, Context context, boolean isExtensional, Expression query){
		this(Factors,theory, context, isExtensional, new VariableNode(query, isExtensional, theory, context));
	}
	public Model(Triple<Set<Expression>,Context,Expression> factorsTheoryAndQuery,Theory theory, boolean isExtensional){
		this(factorsTheoryAndQuery.first, theory, factorsTheoryAndQuery.second, isExtensional, factorsTheoryAndQuery.third);
	}

	/**
	 * This method receives as input an {@code Iterator<FactorNode>} object and expands the 
	 * {@code exploredGraphicalModel} by adding ONE FACTOR to it. 
	 * 
	 * There are various ways of doing such expansion, and it is then given to the User the 
	 * choice on which way to go.
	 * 
	 * @param it
	 */
	public void ExpandModel(Iterator<FactorNode> it){
		//BFS, DFS,...
		if(it.hasNext()){
			FactorNode newFactorNode = it.next();
			for (Expression variable : Expressions.freeVariables(newFactorNode.getValue(), context)) {
				VariableNode v = new VariableNode(variable, isExtensional,theory,context);
				exploredGraphicalModel.add(v,newFactorNode);
			}
		}
	}
	
	public void ExpandModel(FactorNode newFactorNode){
		//BFS, DFS,...
		for (Expression variable : Expressions.freeVariables(newFactorNode.getValue(), context)) {
			VariableNode v = new VariableNode(variable, isExtensional,theory,context);
			exploredGraphicalModel.add(v,newFactorNode);
		}	
	}
	
	/**
	 * Returns true if exploredGraphicalModel == graphicalModel
	 * @return
	 */
	public boolean AllExplored(){
		boolean result = graphicalModel.equals(exploredGraphicalModel);
		return result;
	}
	
	/**
	 * A {@link VariableNode} is said to be exhausted if and only if all the factors directly
	 * linked to it are present in the {@code exploredGraphicalModel}. 
	 * 
	 * This means that there is no information directly related to it that we do not know 
	 * already.
	 * @param variable
	 * @return
	 */
	public boolean isExhausted(VariableNode variable){
		Set<FactorNode> factorsLinkedToVariable = new HashSet<>();
		factorsLinkedToVariable.addAll(graphicalModel.getBsOfA(variable));
		
		Set<FactorNode> exploredFactorsLinkedToVariable = new HashSet<>();
		exploredFactorsLinkedToVariable.addAll(exploredGraphicalModel.getBsOfA(variable));
		
		boolean result = exploredFactorsLinkedToVariable.containsAll(factorsLinkedToVariable);
		return result;
	}
	
	public ManyToManyRelation<VariableNode, FactorNode> getEntireGraph() {
		return graphicalModel;
	}
	public ManyToManyRelation<VariableNode, FactorNode> getExploredGraph() {
		return exploredGraphicalModel;
	}
	
	public void setEntireGraph(ManyToManyRelation<VariableNode, FactorNode> graph) {
		this.graphicalModel = graph;
	}
	public void setExploredGraph(ManyToManyRelation<VariableNode, FactorNode> graph) {
		this.exploredGraphicalModel = graph;
	}
	
	/**
	 * If we wish to restart the incremental inference process (maybe to test other forms of
	 * graph expansion or other partition trees), it suffices to clear the explored graph 
	 */
	public void clearExploredGraph(){
		exploredGraphicalModel = new ManyToManyRelation<>();
	}
	
	public Context getContext() {
		return context;
	}
	public void setContext(Context context) {
		this.context = context;
	}
	
	public Theory getTheory() {
		return theory;
	}
	public void setTheory(Theory theory) {
		this.theory = theory;
	}

	public VariableNode getQuery() {
		return query;
	}
	public void setQuery(VariableNode query) {
		this.query = query;
	}
	
	/**
	 * This method says whether the {@link Bound}for the nodes in the graph are 
	 * {@link DefaultExtensionalBound}s or {@link DefaultIntensionalBound}s.
	 * @return
	 */
	public boolean isExtensional(){
		return isExtensional;
	}
	
	public void SetExploredGraphToEntireGraph(){
		exploredGraphicalModel = graphicalModel;
	}

	public Collection<VariableNode> getVariablesOfAFactor(FactorNode factor){
		return graphicalModel.getAsOfB(factor);
	}
	
	public Collection<FactorNode> getFactorsLinkedToAVariable(VariableNode variable){
		return graphicalModel.getBsOfA(variable);
	}
	
	public Collection<FactorNode> getExploredFactors(){
		return exploredGraphicalModel.getBs();
	}
	
	public Collection<VariableNode> getExploredVariables(){
		return exploredGraphicalModel.getAs();
	}
	
	public Collection<VariableNode> getAllFactors(){
		return graphicalModel.getAs();
	}
	
	public Collection<FactorNode> getAllVariables(){
		return graphicalModel.getBs();
	}
}