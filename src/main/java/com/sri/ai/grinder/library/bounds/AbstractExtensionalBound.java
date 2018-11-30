package com.sri.ai.grinder.library.bounds;

import static com.sri.ai.util.Util.arrayList;

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
	
	@Override
	public boolean isExtensionalBound() {
		return true;
	}
	
	@Override
	public boolean isIntensionalBound() {
		return false;
	}
}
