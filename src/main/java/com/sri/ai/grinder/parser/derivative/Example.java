package com.sri.ai.grinder.parser.derivative;
import static com.sri.ai.expresso.helper.Expressions.parse;

import java.util.HashSet;
import java.util.Set;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.core.DefaultSymbol;
import com.sri.ai.grinder.sgdpllt.anytime.Model;
import com.sri.ai.grinder.sgdpllt.anytime.VariableComponent;
import com.sri.ai.grinder.sgdpllt.api.Context;
import com.sri.ai.grinder.sgdpllt.api.Theory;
import com.sri.ai.grinder.sgdpllt.library.controlflow.IfThenElse;


public class Example {
	
	public static VariableComponent TreeModel() {
		Expression a = DefaultSymbol.createSymbol("A");
		Expression b = DefaultSymbol.createSymbol("B");
		Expression q = DefaultSymbol.createSymbol("Q");

		Expression trueValue = DefaultSymbol.createSymbol(true);
		Expression f1 = IfThenElse.make(a, IfThenElse.make(q, parse("0.1"), parse("0.9")), IfThenElse.make(q, parse("0.8"), parse("0.2")));
		
		
		Set<Expression> Factor = new HashSet<Expression>();
		Factor.add(f1);
		
		Model m = new Model(Factor);
		m.extendModelWithSymbolsAndTypes("A", "Boolean");
		m.extendModelWithSymbolsAndTypes("B", "Boolean");
		m.extendModelWithSymbolsAndTypes("Q", "Boolean");

		VariableComponent ComponentResultat = new VariableComponent(q, null, m, new HashSet<Expression>(), true);

		return ComponentResultat;
	}
	

	public static void main(String[] args) {
		VariableComponent v = TreeModel();
		Context context = v.model.context;
		Expression expression = parse("if Q then if A then if B then 4 else 2 else if B then 6 else 7 else if A then if B then 0 else 2 else if B then 1 else 1");
		System.out.println(Derivative.derivativesOfFactor(expression, parse("Q"), context));
	}
	

}
