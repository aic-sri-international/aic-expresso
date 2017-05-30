package com.sri.ai.grinder.sgdpllt.anytime;

import java.util.HashSet;
import java.util.Set;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.core.DefaultSymbol;

public class component {

	public Expression V;
	public Set<component> children;
	public Set<Expression> Dext;
	public Set<Expression> D;
	public Set<Expression> B;
	public Set<Expression> Pint;
	
	
	public component(Expression V, Expression Phi, Set<Expression> Pext){
		this.V = V;
		this.children = new HashSet<component>();
		this.D = new HashSet<Expression>();
		
		this.Pint = new HashSet<Expression>();
		this.Pint.add(Phi);

		this.Dext = new HashSet<Expression>();
		//Set<Expression> intersection = new HashSet<Expression>(Pext.getVariable); 
		//intersection.retainAll(S);
		
		this.B = new HashSet<Expression>();
		// this.B = Simplex(V)
	}
	
	
}
