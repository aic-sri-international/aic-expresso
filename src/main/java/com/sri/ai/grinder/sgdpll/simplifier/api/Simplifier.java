package com.sri.ai.grinder.sgdpll.simplifier.api;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.sgdpll.api.Context;
import com.sri.ai.util.base.BinaryFunction;

/**
 * A Simplifier knows just enough about the symbols in a language to simplify it in a shallow way,
 * that is, to replace function applications by a simpler equivalent expression, if that expression is determined by their immediate arguments.
 * Shallow simplifications are required to take polynomial time in the size of expressions (preferably linear time).
 * <p>
 * Examples of shallow simplifications are <code>x + 0</code> to <code>x</code>, <code>x or true</code> to <code>true</code>, and <code>x + 1 + 3</code> to <code>x + 4</code>.
 * Simplifications that are <i>not</i> shallow include those requiring case analysis (inference), such as <code>(p and q) or (p and not q)</code>leading to <code>p</code>.
 * @author braz
 *
 */
public interface Simplifier extends BinaryFunction<Expression, Context, Expression> {
}