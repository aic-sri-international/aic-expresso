package com.sri.ai.test.grinder.interpreter;

import static com.sri.ai.expresso.helper.Expressions.parse;
import static com.sri.ai.util.Util.list;
import static com.sri.ai.util.Util.println;

import java.util.Random;

import org.junit.Test;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.api.Context;
import com.sri.ai.grinder.api.MultiQuantifierEliminationProblem;
import com.sri.ai.grinder.core.TrueContext;
import com.sri.ai.grinder.core.solver.DefaultMultiQuantifierEliminationProblem;
import com.sri.ai.grinder.group.Sum;
import com.sri.ai.grinder.interpreter.SamplingProceduralAttachmentSingleQuantifierEliminator;
import com.sri.ai.grinder.library.commonrewriters.CommonSimplifier;
import com.sri.ai.grinder.library.proceduralattachment.ProceduralAttachments;
import com.sri.ai.grinder.rewriter.api.Rewriter;
import com.sri.ai.grinder.rewriter.core.CombiningTopRewriter;
import com.sri.ai.grinder.theory.differencearithmetic.DifferenceArithmeticTheory;


public class SamplingProceduralAttachmentSingleQuantifierEliminatorTest {

	@Test
	public void test() {
		
		String intervalString = "0..10";

		Context context = new TrueContext(new DifferenceArithmeticTheory(true, true));
		
		MultiQuantifierEliminationProblem problem =
				new DefaultMultiQuantifierEliminationProblem(
						new Sum(),
						list(parse("I")),
						list(parse(intervalString)),
						parse("I != 3"),
						parse("I^2")
						);
		
		SamplingProceduralAttachmentSingleQuantifierEliminator eliminator =
				new SamplingProceduralAttachmentSingleQuantifierEliminator(new CommonSimplifier(), new Random());
		
		context = problem.extend(context); // SHOULD NOT HAVE TO
		Expression sampler = eliminator.solve(problem, context);
		
		println(sampler);

		Rewriter rewriter =
				new CombiningTopRewriter(
						new CommonSimplifier(),
						ProceduralAttachments.getProceduralAttachmentsTopRewriter(context));
		
		println(rewriter.apply(sampler, context));
		println(rewriter.apply(sampler, context));
		println(rewriter.apply(sampler, context));
		println(rewriter.apply(sampler, context));
		println(rewriter.apply(sampler, context));
		println(rewriter.apply(sampler, context));
		println(rewriter.apply(sampler, context));
		println(rewriter.apply(sampler, context));
		println(rewriter.apply(sampler, context));
		println(rewriter.apply(sampler, context));
		println(rewriter.apply(sampler, context));
		println(rewriter.apply(sampler, context));
		println(rewriter.apply(sampler, context));
		println(rewriter.apply(sampler, context));
		println(rewriter.apply(sampler, context));
		println(rewriter.apply(sampler, context));
		println(rewriter.apply(sampler, context));
		println(rewriter.apply(sampler, context));
		println(rewriter.apply(sampler, context));
		println(rewriter.apply(sampler, context));
		println(rewriter.apply(sampler, context));
		println(rewriter.apply(sampler, context));
		println(rewriter.apply(sampler, context));
	}
}
