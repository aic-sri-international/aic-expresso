package anytimeExactBeliefPropagation.Model.Node;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.sgdpllt.library.bounds.Bound;

/**
 * A class that provides the basic nodes for a factor graph.
 * Nodes can be {@link VariableNode}s or {@link FactorNode}s.
 * Nodes have A value, which is an expression identifying the Variable/Factor and
 * a Bound, which is the message bound that it transmits.
 * 
 * @author ferreira
 *
 */

public abstract class Node {
	
	protected Expression value;
	protected Bound bound;	
	
	public Node(Expression value) {
		this.value = value;
	}
	
	public Expression getValue() {
		return value;
	}
	public void setValue(Expression value) {
		this.value = value;
	}
	
	public Bound getBound() {
		return bound;
	}
	public void setBound(Bound bound) {
		this.bound = bound;
	}
	
	@Override
	public String toString() {
		String type = "Unkown";
		if (this.isVariable()) {
			type = "Variable";
		}
		if (this.isFactor()) {
			type = "Factor";
		}
		String result = type +" Node: " + this.value;// + "\t Bound Extremes: " + bound;
		return result;
	}
		
	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		// result = prime * result + ((bound == null) ? 0 : bound.hashCode());
		result = prime * result + ((value == null) ? 0 : value.hashCode());
		return result;
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (obj == null)
			return false;
		if (getClass() != obj.getClass())
			return false;
		Node other = (Node) obj;
		// if (bound == null) {
		//	if (other.bound != null)
		//		return false;
		//} else if (!bound.equals(other.bound))
		//	return false;
		if (value == null) {
			if (other.value != null)
				return false;
		} else if (!value.equals(other.value))
			return false;
		return true;
	}

	public abstract boolean isVariable();
	public abstract boolean isFactor();
}
