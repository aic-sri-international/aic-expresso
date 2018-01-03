/*
 * Copyright (c) 2017, SRI International
 * All rights reserved.
 * Licensed under the The BSD 3-Clause License;
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at:
 * 
 * http://opensource.org/licenses/BSD-3-Clause
 * 
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 
 * Redistributions of source code must retain the above copyright
 * notice, this list of conditions and the following disclaimer.
 * 
 * Redistributions in binary form must reproduce the above copyright
 * notice, this list of conditions and the following disclaimer in the
 * documentation and/or other materials provided with the distribution.
 * 
 * Neither the name of the aic-expresso nor the names of its
 * contributors may be used to endorse or promote products derived from
 * this software without specific prior written permission.
 * 
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
 * FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
 * COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, 
 * INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES 
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
 * SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) 
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, 
 * STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) 
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED
 * OF THE POSSIBILITY OF SUCH DAMAGE.
 */
package com.sri.ai.grinder.sgdpllt.core.solver;

import static com.sri.ai.expresso.api.IntensionalSet.intensionalMultiSet;
import static com.sri.ai.util.Util.myAssert;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.IndexExpressionsSet;
import com.sri.ai.expresso.core.ExtensionalIndexExpressionsSet;
import com.sri.ai.grinder.sgdpllt.api.Context;
import com.sri.ai.grinder.sgdpllt.api.MultiQuantifierEliminationProblem;
import com.sri.ai.grinder.sgdpllt.library.indexexpression.IndexExpressions;
import com.sri.ai.grinder.sgdpllt.library.set.Measure;
import com.sri.ai.util.math.Rational;

/**
 * A {@link MultiQuantifierEliminationProblem} that knows how to compute the measure
 * of the domain satisfying its constraint.
 */
public class MeasurableMultiQuantifierEliminationProblem extends MultiQuantifierEliminationProblemWrapper {

	private Rational measure;
	
	public MeasurableMultiQuantifierEliminationProblem(MultiQuantifierEliminationProblem problem) {
		super(problem);
	}

	@Override
	public MeasurableMultiQuantifierEliminationProblem copyWithNewProblem(MultiQuantifierEliminationProblem problem) {
		return new MeasurableMultiQuantifierEliminationProblem(problem);
	}

	public Rational getMeasure(Context context) {
		if (measure == null) {
			measure = computeMeasure(this, context);
		}
		return measure;
	}

	/**
	 * Method with package visibility for reuse in {@link MeasurableSingleQuantifierEliminationProblem},
	 * as a way of simulating multiple inheritance, which already extends {@link SingleQuantifierEliminationProblemWrapper}.
	 * @param problem
	 * @param context
	 * @return
	 */
	static Rational computeMeasure(MultiQuantifierEliminationProblem problem, Context context) {
		myAssert(problem.getIndices().size() == 1, () -> problem.getClass() + " currently supports single indices only, but got " + problem);
		Expression intensionalSetOfAllIndexValues = getIntensionalSet(problem);
		Rational measure = Measure.get(intensionalSetOfAllIndexValues, context);
		// TODO: need to revisit Measure and see if it is doing the right thing, especially when generalizing to multiple indices.
		return measure;
	}

	private static Expression getIntensionalSet(MultiQuantifierEliminationProblem problem) {
		IndexExpressionsSet indexExpressionsSet = new ExtensionalIndexExpressionsSet(IndexExpressions.makeIndexExpression(problem.getIndices().get(0), problem.getIndicesTypes().get(0)));
		Expression intensionalSet = intensionalMultiSet(indexExpressionsSet, problem.getIndices().get(0), problem.getConstraint());
		return intensionalSet;
	}

	@Override
	public MeasurableSingleQuantifierEliminationProblem getFirstIndexVersion() {
		MeasurableSingleQuantifierEliminationProblem firstIndexVersion = 
				new MeasurableSingleQuantifierEliminationProblem(baseProblem.getFirstIndexVersion());
		if (measure != null && getIndices().size() == 1) {
			firstIndexVersion.measure = measure;
		}
		return firstIndexVersion;
	}
}
