package com.sri.ai.expresso.core;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.ExpressionAndSyntacticContext;
import com.sri.ai.expresso.api.ExtensionalSet;
import com.sri.ai.expresso.api.SubExpressionAddress;
import com.sri.ai.expresso.api.SyntaxTree;
import com.sri.ai.expresso.helper.SyntaxTrees;
import com.sri.ai.grinder.api.Registry;
import com.sri.ai.grinder.core.AbstractNonQuantifiedExpression;
import com.sri.ai.grinder.library.set.extensional.ExtensionalSets;
import com.sri.ai.util.Util;

public abstract class AbstractExtensionalSet extends AbstractNonQuantifiedExpression implements ExtensionalSet {

	private static final long serialVersionUID = 1L;
	
	protected ArrayList<Expression>      elementsDefinitions;
	protected SyntaxTree                 syntaxTree;
	protected List<ExpressionAndSyntacticContext> expressionAndSyntacticContexts;

	protected abstract String getLabel();
	
	protected abstract ExtensionalSet make(ArrayList<Expression> elementsDefinitions);
	
	public AbstractExtensionalSet(ArrayList<Expression> elementsDefinitions) {
		super();
		this.elementsDefinitions = elementsDefinitions;
		
		this.syntaxTree = makeSyntaxTree();
		
		expressionAndSyntacticContexts = new LinkedList<ExpressionAndSyntacticContext>();
		int i = 0;
		for (Expression element : elementsDefinitions) {
			expressionAndSyntacticContexts.add(new DefaultExpressionAndSyntacticContext(element, new IndexAddress(i++)));
		}
	}

	protected static class IndexAddress implements SubExpressionAddress {
	
			private int index;
			
			public IndexAddress(int index) {
				super();
				this.index = index;
			}
	
			@Override
			public Expression replace(Expression expression, Expression newSubExpression) {
				Util.myAssert(() -> expression instanceof AbstractExtensionalSet, () -> getClass().getSimpleName() + ".IndexAddress applied to expression " + expression + " of class " + expression.getClass());
				Expression result = ((ExtensionalSet)expression).setElementDefinition(this.index, newSubExpression);
				return result;
			}

			@Override
			public Expression getSubExpressionOf(Expression expression) {
				Util.myAssert(() -> expression instanceof AbstractExtensionalSet, () -> getClass().getSimpleName() + ".IndexAddress applied to expression " + expression + " of class " + expression.getClass());
				Expression result = ((AbstractExtensionalSet) expression).getElementDefinition(index);
				return result;
			}
		}

	protected DefaultCompoundSyntaxTree makeSyntaxTree() {
		DefaultCompoundSyntaxTree result;
		ArrayList<SyntaxTree> elementsSyntaxTrees = Util.mapIntoArrayList(elementsDefinitions, e -> e == null? null : e.getSyntaxTree());
		SyntaxTree kleeneList = SyntaxTrees.makeKleeneListIfNeeded(elementsSyntaxTrees);
		result = new DefaultCompoundSyntaxTree(getLabel(), kleeneList);
		return result;
	}

	@Override
	public Expression get(int index) {
		return elementsDefinitions.get(index);
	}

	@Override
	public Expression setElementDefinition(int i, Expression newIthElement) {
		ExtensionalSet result;
		
		if (get(i) == newIthElement) {
			result = this;
		}
		else {
			ArrayList<Expression> newElements = new ArrayList<Expression>(elementsDefinitions);
			newElements.set(i, newIthElement);
			result = make(newElements);
		}
		
		return result;
	}

	@Override
	public Iterator<ExpressionAndSyntacticContext> getImmediateSubExpressionsAndContextsIterator() {
		return expressionAndSyntacticContexts.iterator();
	}

	@Override
	public Object getSyntacticFormType() {
		return ExtensionalSets.SYNTACTIC_FORM_TYPE;
	}

	@Override
	public SyntaxTree getSyntaxTree() {
		return syntaxTree;
	}

	@Override
	public Expression replaceSymbol(Expression symbol, Expression newSymbol, Registry registry) {
		// TODO: incorrect! Must replace quantified symbols in sub-expressions too, this won't do it.
		Expression result = replaceAllOccurrences(symbol, newSymbol, registry);
		return result;
	}

	@Override
	public Expression clone() {
		return make((ArrayList<Expression>) getElementsDefinitions());
	}

	@Override
	public Expression set(int i, Expression newIthArgument) {
		Expression result = setElementDefinition(i, newIthArgument);
		return result;
	}
	
	@Override
	public List<Expression> getArguments() {
		return getElementsDefinitions();
	}

	@Override
	public String makeToString() {
		String result = getOpeningBrackets() + " " + Util.join(", ", getElementsDefinitions()) + " " + getClosingBrackets();
		return result;
	}

	abstract protected String getOpeningBrackets();

	abstract protected String getClosingBrackets();

	@Override
	public Expression getFunctor() {
		return null;
	}
}