package com.sri.ai.grinder.helper;

import java.util.List;
import java.util.concurrent.ConcurrentHashMap;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.library.set.tuple.Tuple;

/**
 * A class keeping a list of intensional sets to be looked up in the manner of {@link SymbolicInjectiveLookUp}.
 * The sets are supposed to be disjoint and this allows it to cache which intensional set corresponds to a given expression's key.
 * 
 * @author braz
 *
 */
public class ListOfDisjointIntensionalSetsForSymbolicInjectiveLookUp {
	
	private List<Expression> intensionalSets;
	private SymbolicInjectiveLookUp symbolicLookUp;
	private ExpressionToIntensionalSetIndexCache expressionToIntensionalSetCache;

	public ListOfDisjointIntensionalSetsForSymbolicInjectiveLookUp(List<Expression> intensionalSets, SymbolicInjectiveLookUp symbolicLookUp, boolean useCache) {
		super();
		this.intensionalSets = intensionalSets;
		this.symbolicLookUp = symbolicLookUp;
		this.expressionToIntensionalSetCache = new ExpressionToIntensionalSetIndexCache(useCache);
	}
	
	private ListOfDisjointIntensionalSetsForSymbolicInjectiveLookUp(List<Expression> intensionalSets, SymbolicInjectiveLookUp symbolicLookUp, ExpressionToIntensionalSetIndexCache expressionToIntensionalSetCache) {
		super();
		this.intensionalSets = intensionalSets;
		this.symbolicLookUp = symbolicLookUp;
		this.expressionToIntensionalSetCache = expressionToIntensionalSetCache;
	}
	
	public List<Expression> getIntensionalSets() {
		return intensionalSets;
	}
	
	public ListOfDisjointIntensionalSetsForSymbolicInjectiveLookUp replaceIntensionalSets(List<Expression> intensionalSets) {
		ListOfDisjointIntensionalSetsForSymbolicInjectiveLookUp result = new ListOfDisjointIntensionalSetsForSymbolicInjectiveLookUp(intensionalSets, symbolicLookUp, expressionToIntensionalSetCache);
		return result;
	}
	
	public void putIntensionalSetWithKeyKnownToBeDisjointFromOthers(Expression intensionalSet, RewritingProcess process) {
		intensionalSets.add(intensionalSet);
	}
	
	public Expression lookUp(Expression expression, RewritingProcess process) {
		
		Trace.in("+lookUp({},{})", expression, Expressions.apply("list", intensionalSets));

		Expression result = null; // the method should never actually return null, but the compiler doesn't know that.
		
		Expression intensionalSet = null;
		
		Integer cachedIndex  = expressionToIntensionalSetCache.getIntensionalSetIndex(expression, process);
		// if index cached
		if (cachedIndex != null) {
			// index <- index from cache
			int index = cachedIndex;
			intensionalSet = intensionalSets.get(index);
			Trace.log("Set of intensional set index cached for {} under contextual constraint {}: {}", expression, process.getContextualConstraint(), intensionalSet);
			result = symbolicLookUp.lookUp(expression, intensionalSet, process);
			// no need to check for successful lookup; the fact that the index was cached implies that.
		} 
		else {
			Trace.log("No cached intensional set index for {}", expression);
			// for each i in 1..length(msg_values)
			for (int i = 0; i < intensionalSets.size(); i++) {
				Trace.log("Examining candidate {}", intensionalSets.get(i));
				intensionalSet = intensionalSets.get(i);
				result = symbolicLookUp.lookUp(expression, intensionalSet, process);
				if (result != expression) {
					Trace.log("Look up found for {}", intensionalSet);
					int index = i;
					// cache index
					expressionToIntensionalSetCache.addIntensionalSetIndex(process.getContextualConstraint(), expression, index);
					// exit "for each" loop
					break;
				}
			}
		}
		
		Trace.out("-lookUp = {}", result);

		return result;
	}
	
	@Override
	public boolean equals(Object another) {
		boolean result = false;;
		if (another instanceof ListOfDisjointIntensionalSetsForSymbolicInjectiveLookUp) {
			ListOfDisjointIntensionalSetsForSymbolicInjectiveLookUp anotherMap = (ListOfDisjointIntensionalSetsForSymbolicInjectiveLookUp) another;
			result = intensionalSets.equals(anotherMap.intensionalSets);
		}
		return result;
	}

	private static class ExpressionToIntensionalSetIndexCache {
		boolean useCache = true;

		// key= (context, expression), value = intensional_set_index to use.
		ConcurrentHashMap<Expression, Integer> intensionalSetIndexMap = new ConcurrentHashMap<Expression, Integer>();
		
		public ExpressionToIntensionalSetIndexCache(boolean useCache) {
			this.useCache = useCache;
		}
		
		public void addIntensionalSetIndex(Expression contextualConstraint, Expression expression, int intensionalSetIndex) {
			if (useCache) {
				intensionalSetIndexMap.put(Tuple.make(contextualConstraint, expression), intensionalSetIndex);
			}
		}
		
		public Integer getIntensionalSetIndex(Expression expression, RewritingProcess process) {
			return intensionalSetIndexMap.get(Tuple.make(process.getContextualConstraint(), expression));
		}
	}
}