package com.sri.ai.grinder.parser.derivative;
import static com.sri.ai.expresso.helper.Expressions.parse;

import java.util.HashSet;
import java.util.Set;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.core.DefaultSymbol;
import com.sri.ai.grinder.sgdpllt.anytime.Model;
import com.sri.ai.grinder.sgdpllt.anytime.VariableComponent;
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

		VariableComponent ComponentResultat = new VariableComponent(q, null, m, new HashSet<Expression>());
		return ComponentResultat;
	}
	

	public static void main(String[] args) {
		VariableComponent ComponentResultat = TreeModel();
		
		Expression expression = parse("if Q then if A then if B then 0.2 else 0.7 else if B then 0.5 else 0.5 else if A then if A then 0.8 else 0.3 else if B then 0.4 else 0.6");
		System.out.println(Derivative.derivativeSingleExpression(expression, parse("Q"), ComponentResultat.model));
	}

}
