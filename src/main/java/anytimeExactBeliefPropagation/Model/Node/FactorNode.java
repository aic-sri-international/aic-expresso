package anytimeExactBeliefPropagation.Model.Node;

import static com.sri.ai.util.Util.arrayList;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.sgdpllt.api.Context;
import com.sri.ai.grinder.sgdpllt.api.Theory;
import com.sri.ai.grinder.sgdpllt.library.bounds.Bounds;

/**
 * Special type of {@link Node} used to represent factors
 * @author ferreira
 *
 */
public class FactorNode extends  Node {

	public FactorNode(Expression value, boolean isExtensional, Theory theory, Context context) {
		super(value);
		//TODO right initialization
		//Initialiation for  Variable nodes is the 
		this.bound = Bounds.makeSingleElementBound(value, isExtensional);
	}


	
	@Override
	public boolean isVariable() {
		return false;
	}

	@Override
	public boolean isFactor() {
		return true;
	}
}
