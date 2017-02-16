package com.sri.ai.grinder.helper;

import static com.sri.ai.util.Util.map;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.FunctionApplication;
import com.sri.ai.grinder.sgdpllt.rewriter.api.Simplifier;
import com.sri.ai.grinder.sgdpllt.rewriter.core.Switch;

public class LazySampledFunctionApplicationTopRewriter extends Switch<Object> {
	public LazySampledFunctionApplicationTopRewriter() {
		super(
				Switch.SYNTACTIC_FORM_TYPE,
				map(
						FunctionApplication.SYNTACTIC_FORM_TYPE,
						(Simplifier) (e, c) -> {			
							Expression result = e;
							if (e.getFunctor() instanceof LazySampledFunction) {	
								LazySampledFunction lazySampledFunctor = (LazySampledFunction) e.getFunctor();
								result = lazySampledFunctor.sampleApplication(e.getArguments(), c);								
							}							
							return result;
						}));
	}
}