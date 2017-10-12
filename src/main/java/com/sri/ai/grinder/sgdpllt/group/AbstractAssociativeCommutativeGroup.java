package com.sri.ai.grinder.sgdpllt.group;

import com.sri.ai.util.Util;

/**
 * An abstract implementation specifying that toString returns the object's name as extracted from the class name.
 * 
 * @author braz
 *
 */
public abstract class AbstractAssociativeCommutativeGroup implements AssociativeCommutativeGroup {

	@Override
	public String toString() {
		return Util.camelCaseToSpacedString(getClass().getSimpleName());
	}
}