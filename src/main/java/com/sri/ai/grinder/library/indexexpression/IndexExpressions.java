package com.sri.ai.grinder.library.indexexpression;

import java.util.Collection;
import java.util.LinkedHashMap;
import java.util.LinkedList;
import java.util.List;

import com.google.common.base.Function;
import com.google.common.base.Predicate;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.core.DefaultCompoundSyntaxTree;
import com.sri.ai.expresso.core.DefaultSymbol;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.util.Util;
import com.sri.ai.util.base.Pair;

public class IndexExpressions {

	public static LinkedHashMap<Expression, Expression> getIndexToDomainMap(List<Expression> indexExpressions) {
		LinkedHashMap<Expression, Expression> result =
			Expressions.getRelationalMap(
					indexExpressions,
					DefaultSymbol.createSymbol("in"),
					new TypeOfIndexInIndexExpression());
		return result;
	}

	public static Expression copyIndexExpressionWithNewIndex(Expression indexExpression, Expression newIndex) {
		if (newIndex != IndexExpressions.getIndex(indexExpression)) {
			Expression result;
			if (indexExpression.hasFunctor("in")) {
				result = Expressions.apply("in", newIndex, indexExpression.get(1));
			}
			else {
				result = newIndex;
			}
			return result;
		}
		return indexExpression;
	}

	public static List<Expression> getIndexExpressionsWithType(List<Expression> indexExpressions) {
		List<Expression> result = Util.replaceElementsNonDestructively(indexExpressions, IndexExpressions.getMakeIndexExpressionWithType());
		return result;
	}

	public static Function<Expression, Expression> getMakeIndexExpressionWithType() {
		return new Function<Expression, Expression>() {
			@Override
			public Expression apply(Expression indexExpression) {
				if (indexExpression.hasFunctor("in")) {
					return indexExpression;
				}
				return new DefaultCompoundSyntaxTree(
						"in",
						indexExpression,
						new DefaultCompoundSyntaxTree("type", indexExpression));
			}
		};
	}

	private static class TypeOfIndexInIndexExpression implements Function<Expression, Expression> {
		@Override
		public Expression apply(Expression expression) {
			return new DefaultCompoundSyntaxTree("type", expression);
		}
	}

	/**
	 * Indicates whether a received index expression's index is in a given collection.
	 * @author braz
	 */
	public static class IndexExpressionHasIndexIn implements Predicate<Expression> {
		private Collection<Expression> indices;
	
		public IndexExpressionHasIndexIn(Collection<Expression> indices) {
			super();
			this.indices = indices;
		}
	
		@Override
		public boolean apply(Expression expression) {
			boolean result = indices.contains(getIndex(expression));
			return result;
		}
	}

	public static class GetIndex implements Function<Expression, Expression> {
		@Override
		public Expression apply(Expression indexExpression) {
			return getIndex(indexExpression);
		}
	}

	public static class IsIndexIn implements Predicate<Expression> {
		private List<Expression> indexExpressions;
		
		public IsIndexIn(List<Expression> indexExpressions) {
			super();
			this.indexExpressions = indexExpressions;
		}
	
		@Override
		public boolean apply(Expression possibleIndex) {
			boolean result = Util.thereExists(indexExpressions, new IsIndexExpressionOnIndex(possibleIndex));
			return result;
		}
	}

	public static class IsIndexExpressionOnIndex implements Predicate<Expression> {
	
		private Expression index;
	
		public IsIndexExpressionOnIndex(Expression index) {
			super();
			this.index = index;
		}
	
		@Override
		public boolean apply(Expression indexExpression) {
			boolean result = getIndex(indexExpression).equals(index);
			return result;
		}
	
	}

	public static class IndexExpressionHasIndex implements Predicate<Expression> {
	
		private Expression index;
	
		public IndexExpressionHasIndex(Expression index) {
			this.index = index;
		}
	
		@Override
		public boolean apply(Expression indexExpression) {
			if (getIndex(indexExpression).equals(index)) {
				return true;
			}
			return false;
		}
	}

	public static Expression makeIndexExpression(Expression index, Expression domain) {
		return Expressions.apply("in", index, domain);
	}

	public static Pair<Expression, Expression> getIndexAndDomain(Expression indexExpression) {
		boolean bothIndexAndDomain = indexExpression.hasFunctor("in") && indexExpression.numberOfArguments() == 2;
		Expression index;
		Expression indexDomain;
		if (bothIndexAndDomain) {
			index = indexExpression.get(0);
			indexDomain = indexExpression.get(1);
		}
		else {
			index = indexExpression;
			indexDomain = type(index);
		}
		return new Pair<Expression, Expression>(index, indexDomain);
	}

	public static Expression type(Expression expression) {
		return new DefaultCompoundSyntaxTree("type", expression);
	}

	public static Expression getIndex(Expression indexExpression) {
		if (indexExpression.hasFunctor("in")) {
			return indexExpression.get(0);
		}
		return indexExpression;
	}

	public static Expression getDomain(Expression indexExpression) {
		Pair<Expression, Expression> indexAndDomain = getIndexAndDomain(indexExpression);
		return indexAndDomain.second;
	}

	public static void addSubTreeWithIndexAndBasePathPlusArgumentIndex(
			Expression syntaxTree, int subTreeIndex, List<Integer> basePath,
			List<Pair<Expression, List<Integer>>> result) {
		Expression subExpression = syntaxTree.getSyntaxTree().getSubTree(subTreeIndex); // does need to be sub tree
		List<Integer> subExpressionPath = new LinkedList<Integer>(basePath);
		subExpressionPath.add(subTreeIndex);
		result.add(new Pair<Expression, List<Integer>>(subExpression, subExpressionPath));
	}
	
}
