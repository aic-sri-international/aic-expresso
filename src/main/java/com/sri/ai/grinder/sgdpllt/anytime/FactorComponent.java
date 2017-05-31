package com.sri.ai.grinder.sgdpllt.anytime;

import static com.sri.ai.expresso.helper.Expressions.apply;
import static com.sri.ai.grinder.sgdpllt.library.FunctorConstants.EQUAL;
import static com.sri.ai.grinder.sgdpllt.library.FunctorConstants.GREATER_THAN;
import static com.sri.ai.grinder.sgdpllt.library.FunctorConstants.IF_THEN_ELSE;

import java.lang.reflect.Array;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashSet;
import java.util.Random;
import java.util.Set;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.core.DefaultSymbol;

public class FactorComponent {

	public Model M;
	public Expression Phi;
	public Set<Expression> Parent;
	public ArrayList<VariableComponent> children;
	public Set<Expression> Dext;
	public Set<Expression> D;
	// public Bound B;
	public Set<Expression> Pint;

	public FactorComponent(Expression Phi, Expression Parent, Model M, Set<Expression> Pext) {

		
		this.Pint = new HashSet<Expression>();
		this.M = M;
		this.children = new ArrayList<VariableComponent>();
		this.Parent = new HashSet<Expression>();
		this.Parent.add(Parent);
		this.D = new HashSet<Expression>();
		this.Dext = new HashSet<Expression>();
		// this.B = new HashSet<Expression>();
		// this.B = Simplex(V)

		this.Phi = Phi;
		this.Pint.add(Phi);
		
		Set<Expression> intersection = new HashSet<Expression>();
		intersection.addAll(M.getNeighborsOfSet(M.getInitializedFactor()));
		Collection<Expression> S = M.getNeighbors(Phi);
		for (Expression e : this.Parent){
			S.remove(e);
		}
		S.retainAll(intersection);
		this.Dext.addAll(S);
		M.InitializeFComponent.add(this);

	}
	
	
	
	public void update(Set<Expression> Pext) {
		
		if (this.children.isEmpty()) {
			for (Expression e : this.M.getNeighbors(Phi)) {
				if (!this.Parent.contains(e)) {
					Set<Expression> union = new HashSet<Expression>(Pext);
					union.add(Phi);
					
					
					boolean test = false;
					
					
					for(VariableComponent c : M.InitializeVComponent){
						if (c.V.equals(e)){
							test = true;
							this.Parent.add(c.V);
						}
					}
					
					if (test == false){
						VariableComponent newV = new VariableComponent(e, Phi, M, union);
						this.children.add(newV);
						Set<Expression> intersection = new HashSet<Expression>(newV.Dext);
						intersection.retainAll(M.getNeighborsOfSet(Pext));
						Dext.addAll(intersection);
						
						
						D.addAll(newV.Dext);
					}
				}

				D.removeAll(Dext);
			}
		} else {
			int j = this.choose();
			Set<Expression> union = new HashSet<Expression>(Pext);
			for (int i = 0; i < this.children.size(); i++) {
				union.addAll(this.children.get(i).Pint);
			}
			this.children.get(j).update(union);

			Set<Expression> intersection = new HashSet<Expression>(this.children.get(j).Dext);
			intersection.retainAll(M.getNeighborsOfSet(Pext));
			Dext.addAll(intersection);
			
			
			D.addAll(this.children.get(j).Dext);
			D.removeAll(Dext);

			Pint.addAll(this.children.get(j).Pint);

			// B = 
		}
	}

	public int choose() {
		Random rn = new Random();
		return rn.nextInt(this.children.size());
		//return 0;
	}

	public void print(int tabs){
		String tab = new String();
		for (int i = 0; i<tabs; i++){
			tab += "\t";
		}
		System.out.println(tab + "Factor : " + Phi);
		System.out.println(tab + "Dext : " + Dext);
		System.out.println(tab + "D : " + D);
		
		for (VariableComponent c : this.children){
			c.print(tabs + 1);
		}

	}
	
	public static void main(String[] args) {

		/* 
		
		// First Model with 1 loop
		  
		Expression func = DefaultSymbol.createSymbol("f");
		
		Expression a = DefaultSymbol.createSymbol("A");
		Expression b = DefaultSymbol.createSymbol("B");
		Expression c = DefaultSymbol.createSymbol("C");
		Expression d = DefaultSymbol.createSymbol("D");
		Expression e = DefaultSymbol.createSymbol("E");
		Expression f = DefaultSymbol.createSymbol("F");
		Expression g = DefaultSymbol.createSymbol("G");
		Expression q = DefaultSymbol.createSymbol("Q");

		Expression f1 = apply(IF_THEN_ELSE, a, q, d);
		Expression f2 = apply(IF_THEN_ELSE, b, q, g);
		Expression f3 = apply(IF_THEN_ELSE, b, f, c);
		Expression f4 = apply(IF_THEN_ELSE, c, a, e);
		Expression res = apply(func, q);

		Set<Expression> Factor = new HashSet<Expression>();
		Factor.add(f1);
		Factor.add(f2);
		Factor.add(f3);
		Factor.add(f4);
		Factor.add(res);*/
		
		/*//Second model with 2 loops
	
		Expression func = DefaultSymbol.createSymbol("f");
		
		Expression a = DefaultSymbol.createSymbol("A");
		Expression b = DefaultSymbol.createSymbol("B");
		Expression c = DefaultSymbol.createSymbol("C");
		Expression d = DefaultSymbol.createSymbol("D");
		Expression e = DefaultSymbol.createSymbol("E");
		Expression f = DefaultSymbol.createSymbol("F");
		Expression q = DefaultSymbol.createSymbol("Q");

		Expression f1 = apply(IF_THEN_ELSE, a, q, b);
		Expression f2 = apply(IF_THEN_ELSE, b, a, e);
		Expression f3 = apply(IF_THEN_ELSE, a, d, c);
		Expression f4 = apply(IF_THEN_ELSE, d, e, f);
		Expression res = apply(func, q);

		Set<Expression> Factor = new HashSet<Expression>();
		Factor.add(f1);
		Factor.add(f2);
		Factor.add(f3);
		Factor.add(f4);
		Factor.add(res);
		
		*/
		
		// Third model with local loops
		
		Expression func = DefaultSymbol.createSymbol("f");
		
		Expression a = DefaultSymbol.createSymbol("A");
		Expression b = DefaultSymbol.createSymbol("B");
		Expression c = DefaultSymbol.createSymbol("C");
		Expression d = DefaultSymbol.createSymbol("D");
		Expression e = DefaultSymbol.createSymbol("E");
		Expression f = DefaultSymbol.createSymbol("F");
		Expression g = DefaultSymbol.createSymbol("G");
		Expression h = DefaultSymbol.createSymbol("H");
		Expression q = DefaultSymbol.createSymbol("Q");

		Expression f1 = apply(IF_THEN_ELSE, q, a, b);
		Expression f2 = apply(IF_THEN_ELSE, a, b, c);
		Expression f3 = apply(IF_THEN_ELSE, c, d, e);
		Expression f4 = apply(IF_THEN_ELSE, e, f, g);
		Expression f5 = apply(IF_THEN_ELSE, f, g, h);
		Expression res = apply(func, q);

		Set<Expression> Factor = new HashSet<Expression>();
		Factor.add(f1);
		Factor.add(f2);
		Factor.add(f3);
		Factor.add(f4);
		Factor.add(f5);
		Factor.add(res);

		Model m = new Model(Factor);

		VariableComponent ComponentResultat = new VariableComponent(q, res, m, new HashSet<Expression>() );
		
		int nbIter = 30;
		
		for (int i = 0; i<nbIter; i++){
			ComponentResultat.update(new HashSet<Expression>());
			System.out.println("Niveau " + i);
			System.out.println(" ");
			ComponentResultat.M.printInitialized();
			System.out.println(" ");
			System.out.println(" ");
		}
		
		ComponentResultat.print(0);
	}
}
