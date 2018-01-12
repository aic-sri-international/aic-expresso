package com.sri.ai.grinder.library.bounds;

import static com.sri.ai.util.Util.arrayList;
import static com.sri.ai.expresso.helper.Expressions.makeSymbol;


import java.util.ArrayList;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.core.DefaultExtensionalUniSet;

public abstract class AbstractExtensionalBound extends DefaultExtensionalUniSet implements Bound {
	
	private static final long serialVersionUID = 1L;
	
	public AbstractExtensionalBound(ArrayList<Expression> elementsDefinitions) {
		super(elementsDefinitions);
	}

	public AbstractExtensionalBound(Expression singleElement) {
		super(arrayList(singleElement));
	}
	
	public AbstractExtensionalBound() {
		super(arrayList(makeSymbol("0"))); // TODO
	}
	
	public boolean isExtensionalBound() {
		return true;
	}
	
	public boolean isIntensionalBound() {
		return false;
	}
}
