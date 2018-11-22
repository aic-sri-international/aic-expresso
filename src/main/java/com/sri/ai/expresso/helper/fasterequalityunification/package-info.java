/**
 * A package containing a simple, fast unification algorithm for between
 * a term f(c1,...,cn) with c_i constants,
 * and a term in which all occurrences of f have arguments that are either constants or variables.
 * <p>
 * The result is a conjunction of equalities and disequalities between variables and constants.
 * <p>
 * In spite of being inside expresso, this algorithm is not defined for <code>Expression</code>,
 * but in terms of an algorithm-specific interface.
 * This promotes encapsulation (it will not be broken by changes to classes and interfaces outside of it)
 * and allows us to define the simplest possible interface that will satisfy the algorithm's needs,
 * improving clarity and simplicity.
 * <p>
 * To apply it to <code>Expression</code>, one must define adapters from one implementation to another.
 */
package com.sri.ai.expresso.helper.fasterequalityunification;
