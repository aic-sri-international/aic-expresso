package com.sri.ai.grinder.sgdpllt.library.set.invsupport;

import static com.sri.ai.util.Util.map;

import com.sri.ai.grinder.sgdpllt.library.FunctorConstants;
import com.sri.ai.grinder.sgdpllt.rewriter.core.Switch;

public class IntensionalUnionIntersectionEqualToEmptySetTopRewriter extends Switch<String>  {
	
	public IntensionalUnionIntersectionEqualToEmptySetTopRewriter() {
		super(Switch.FUNCTOR,
				map(FunctorConstants.EQUAL, new IntensionalUnionIntersectionEqualToEmptySetSimplifier()));
	}
}
