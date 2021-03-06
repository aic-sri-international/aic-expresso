package com.sri.ai.grinder.library.set.invsupport;

import static com.sri.ai.util.Util.map;

import com.sri.ai.grinder.library.FunctorConstants;
import com.sri.ai.grinder.rewriter.core.Switch;

public class ElementOfIntensionalUnionTopRewriter extends Switch<String> {
	
	public ElementOfIntensionalUnionTopRewriter() {
		super(Switch.FUNCTOR,
				map(FunctorConstants.IN, new ElementOfIntensionalUnionSimplifier()));
	}
}
