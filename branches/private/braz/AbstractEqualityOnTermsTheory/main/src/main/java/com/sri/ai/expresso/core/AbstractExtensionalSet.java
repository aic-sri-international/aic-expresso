package com.sri.ai.expresso.core;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.ExpressionAndContext;
import com.sri.ai.expresso.api.ExtensionalSetInterface;
import com.sri.ai.expresso.api.SubExpressionAddress;
import com.sri.ai.expresso.api.SyntaxTree;
import com.sri.ai.expresso.helper.SyntaxTrees;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.core.AbstractNonQuantifiedExpression;
import com.sri.ai.util.Util;

public abstract class AbstractExtensionalSet extends AbstractNonQuantifiedExpression implements ExtensionalSetInterface {

	private static final long serialVersionUID = 1L;
	
	protected ArrayList<Expression>      elementsDefinitions;
	protected SyntaxTree                 syntaxTree;
	protected List<ExpressionAndContext> expressionAndContexts;

	protected abstract String getLabel();
	
	protected abstract ExtensionalSetInterface make(ArrayList<Expression> elementsDefinitions);
	
	public AbstractExtensionalSet(ArrayList<Expression> elementsDefinitions) {
		super();
		this.elementsDefinitions = elementsDefinitions;
		
		this.syntaxTree = makeSyntaxTree();
		
		expressionAndContexts = new LinkedList<ExpressionAndContext>();
		int i = 0;
		for (Expression element : elementsDefinitions) {
			expressionAndContexts.add(new DefaultExpressionAndContext(element, new IndexAddress(i++)));
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
				assert expression instanceof AbstractExtensionalSet : getClass().getSimpleName() + ".IndexAddress applied to expression " + expression + " of class " + expression.getClass();
				Expression result = ((ExtensionalSetInterface)expression).setElementDefinition(this.index, newSubExpression);
				return result;
			}

			@Override
			public Expression getSubExpressionOf(Expression expression) {
				assert expression instanceof AbstractExtensionalSet : getClass().getSimpleName() + ".IndexAddress applied to expression " + expression + " of class " + expression.getClass();
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
		ExtensionalSetInterface result;
		
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
	public Iterator<ExpressionAndContext> getImmediateSubExpressionsAndContextsIterator() {
		return expressionAndContexts.iterator();
	}

	@Override
	public Object getSyntacticFormType() {
		return "Extensional set";
	}

	@Override
	public SyntaxTree getSyntaxTree() {
		return syntaxTree;
	}

	@Override
	public Expression replaceSymbol(Expression symbol, Expression newSymbol, RewritingProcess process) {
		// TODO: incorrect! Must replace quantified symbols in sub-expressions too, this won't do it.
		Expression result = replaceAllOccurrences(symbol, newSymbol, process);
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

	public String makeToString() {
		String result = getOpeningBrackets() + " " + Util.join(", ", getElementsDefinitions()) + " " + getClosingBrackets();
		return result;
	}

	abstract protected String getOpeningBrackets();

	abstract protected String getClosingBrackets();
}