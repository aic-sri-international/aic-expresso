package com.sri.ai.grinder.core;

import static com.sri.ai.util.Util.println;

import java.text.DecimalFormat;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.api.Theory;
import com.sri.ai.grinder.core.solver.DefaultExpressionEvaluator;

public class TrueContextUsingExpressionEvaluator extends TrueContext {

	private static final long serialVersionUID = 1L;
	
	private static long totalEvaluateTime = 0;
	private static final DefaultExpressionEvaluator trueContextAnalogToSMTBasedEvaluator = new DefaultExpressionEvaluator();

	public TrueContextUsingExpressionEvaluator(Theory theory) {
		super(theory);
	}
	
	@Override
	public Expression evaluate(Expression expression) {
		long initialTime = System.currentTimeMillis();
		Expression result = trueContextAnalogToSMTBasedEvaluator.eval(expression, this);
		long finalTime = System.currentTimeMillis();
		totalEvaluateTime += (finalTime - initialTime);
		return result;
	}
	public void printEvaluateTimeBreakdown() {
		DecimalFormat df = new DecimalFormat();
		df.setMaximumFractionDigits(2);
		long topRewriteTime = trueContextAnalogToSMTBasedEvaluator.getTopRewriteTime();
		long literalNegationTime = trueContextAnalogToSMTBasedEvaluator.getLiteralNegationTime();
		long expressionEqualsTime = trueContextAnalogToSMTBasedEvaluator.getExpressionEqualsTime();
		long isVariableOrConstantTime = trueContextAnalogToSMTBasedEvaluator.getIsVariableOrConstantTime();
		long isContradictionTime = trueContextAnalogToSMTBasedEvaluator.getIsContradictionTime();
		long isLiteralTime = trueContextAnalogToSMTBasedEvaluator.getIsLiteralTime();
		println("                                  Total smtContext.evaluate() time:  " + totalEvaluateTime);
		println("                             Total theory.topRewriter.apply() time:  " + topRewriteTime);
		println("                                   Percentage of topRewrite() time:  " + df.format(  100.0*topRewriteTime/totalEvaluateTime ) + "%");
		println("                            Total theory.getLiteralNegation() time:  " + literalNegationTime);
		println("                           Percentage of getLiteralNegation() time:  " + df.format(  100.0*literalNegationTime/totalEvaluateTime ) + "%");
		println("                                    Total expression.equals() time:  " + expressionEqualsTime);
		println("                                       Percentage of equals() time:  " + df.format(  100.0*expressionEqualsTime/totalEvaluateTime ) + "%");
		println(" Total context.isVariable()+context.isUniquelyNamedConstant() time:  " + isVariableOrConstantTime);
		println("         Percentage of isVariable()+isUniquelyNamedConstant() time:  " + df.format(  100.0*isVariableOrConstantTime/totalEvaluateTime ) + "%");
		println("                              Total context.isContradiction() time:  " + isContradictionTime);
		println("                              Percentage of isContradiction() time:  " + df.format(  100.0*isContradictionTime/totalEvaluateTime ) + "%");
		println("                                    Total context.isLiteral() time:  " + isLiteralTime);
		println("                                    Percentage of isLiteral() time:  " + df.format(  100.0*isLiteralTime/totalEvaluateTime ) + "%");
	}
}
