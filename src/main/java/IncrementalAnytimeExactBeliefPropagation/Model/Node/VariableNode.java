package IncrementalAnytimeExactBeliefPropagation.Model.Node;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.api.Context;
import com.sri.ai.grinder.api.Theory;

/**
 * Special type of {@link Node} used to represent variables
 * @author ferreira
 *
 */
public class VariableNode extends  Node {
	
	public VariableNode(Expression value, boolean isExtensional, Theory theory, Context context) {
		super(value);
		// TODO right initialization
		// Initialiation for  Variable nodes is the simplex
		// this.bound = Bounds.makeSingleElementBound(makeSymbol(1), isExtensional);
		// this.bound = Bounds.simplex(arrayList(value), theory, context, isExtensional);
	}

	@Override
	public boolean isVariable() {
		return true;
	}

	@Override
	public boolean isFactor() {
		return false;
	}

}
