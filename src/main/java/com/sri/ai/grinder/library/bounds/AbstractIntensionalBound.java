package com.sri.ai.grinder.library.bounds;

import java.util.List;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.IndexExpressionsSet;
import com.sri.ai.expresso.core.DefaultIntensionalUniSet;
import com.sri.ai.expresso.core.ExtensionalIndexExpressionsSet;

public abstract class AbstractIntensionalBound extends DefaultIntensionalUniSet implements Bound{

	private static final long serialVersionUID = 1L;
	
	public AbstractIntensionalBound(IndexExpressionsSet indexExpressions, Expression head, Expression condition) {
		super(indexExpressions, head, condition);
	}

	public AbstractIntensionalBound(List<Expression> indexExpressionsList, Expression head, Expression condition) {
		this(new ExtensionalIndexExpressionsSet(indexExpressionsList), head, condition);
	}
	
	public boolean isExtensionalBound() {
		return false;
	}
	
	public boolean isIntensionalBound() {
		return true;
	}
}
