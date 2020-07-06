package com.sri.ai.grinder.interpreter

import com.sri.ai.expresso.api.Expression
import com.sri.ai.expresso.helper.SubExpressionsDepthFirstIterator
import com.sri.ai.util.Util

class CompilationIncrementalEvaluator(variables: List<Expression>): CompilationEvaluator(variables) {

    companion object {
        @JvmStatic
        fun makeWithVariablesFrom(expression: Expression) = CompilationIncrementalEvaluator(variablesInOrder(expression))


        @JvmStatic
        fun makeEvaluator(expression: Expression, variables: ArrayList<out Expression>): Evaluator? =
            makeEvaluator(::CompilationIncrementalEvaluator, expression, variables)
    }

    // In this extension, Java expressions for a sub-expression can be of two types:
    // a method invocation of the respective method, or the body of that method decomposing
    // the expression into an operation on the Java expression of its arguments (sub-expressions).
    // To distinguish those two senses, we call the former "java expression" and the latter "super Java expression"
    // (since the super class defines Java expressions as an operation on arguments).
    // Importantly, note that the "super Java expression" uses the method 'javaExpression' on an expression's arguments.
    // Because 'javaExpression' is overridden here, the "super Java expression" will be an operation on the
    // Java expressions of its arguments, which will be method invocations.

    private val subExpressionIDs = mutableMapOf<Expression, Int>()
    private val subExpressionStrings = mutableMapOf<Expression, String>()
    private var nextID: Int = 0
    private var subExpressionsMaxVariableIndex = mutableMapOf<Expression, Int>()

    private var subExpressionSuperTypedJavas = mutableMapOf<Expression, TypedJava>()

    private var alreadyGenerated = mutableSetOf<Expression>()

    override fun evaluatorClassDefinition(expression: Expression): String {
        collectInformationFromRootExpression(expression)
        return super.evaluatorClassDefinition(expression)
    }

    override fun supplementaryDefinitions(expression: Expression): String {
        return (
                """
                    ${cacheDefinitions()}
                    ${subExpressionMethodDefinitions(expression)}
                """
                )
    }

//    private fun joinCode(): String {
//        return ("""
//            private String join(int[] assignment) {
//                String result = "";
//                for (int i = 0; i != assignment.length; i++) {
//                    if (i == 0) {
//                        result = result + assignment[0];
//                    }
//                    else {
//                        result = result + ", " + assignment[i];
//                    }
//                }
//                return result;
//            }
//        """.trimIndent())
//    }

    private fun cacheDefinitions(): String {
        return (
                """
                    int intCache[] = new int[$nextID];
                    double doubleCache[] = new double[$nextID];
                    boolean booleanCache[] = new boolean[$nextID];
                    int indexOfMostSignificantVariableToChange;
                    boolean noAssignmentsHaveBeenComputedYet = true;
                """.trimIndent()
                )
    }

    private fun subExpressionMethodDefinitions(expression: Expression): String {
        return Util.`in`(SubExpressionsDepthFirstIterator(expression))
                .filterNot { isNonVariableSymbol(it) }
                .joinToString("\n\n") { subExpressionMethodDefinition(it) }
    }

    private fun subExpressionMethodDefinition(expression: Expression): String {

        if (expression in alreadyGenerated) {
            return ""
        }
        alreadyGenerated.add(expression)

        // We use super.javaExpressionAndType, which expands into, for example, javaExpressionAndType(arg0) + javaExpressionAndType(arg1),
        // which in turn will use *this* class' javaExpressionAndType which is just the expression's method invocation.
        var (javaExpression, type) = subExpressionSuperTypedJavas[expression] as TypedJava
        val expressionID = subExpressionIDs[expression]
        val expressionString = subExpressionStrings[expression]

        val computeMostSignificantVariableToChangeIfNeeded =
                if (expressionID == 0) {
                    """
                        // System.out.println("Starting assignment " + join(assignment));
                        
                        if (noAssignmentsHaveBeenComputedYet) {
                            indexOfMostSignificantVariableToChange = 0;
                        }
                        else {
                            for (indexOfMostSignificantVariableToChange = ${variables.size} - 1;
                                indexOfMostSignificantVariableToChange != 0 && assignment[indexOfMostSignificantVariableToChange - 1] == 0;
                                indexOfMostSignificantVariableToChange--);
                            if (indexOfMostSignificantVariableToChange != 0) {
                                indexOfMostSignificantVariableToChange--;
                            }
                        }
                    """.trimIndent()
                }
                else ""

        val setNoAssignmentsHaveBeenComputedYetIfNeeded =
                if (expressionID == 0) {
                    "noAssignmentsHaveBeenComputedYet = false;"
                }
                else ""

        return (
        """
            private $type ${methodName(expression)}(int[] assignment) {
                $computeMostSignificantVariableToChangeIfNeeded
                $type value;
                if (noAssignmentsHaveBeenComputedYet || ${subExpressionsMaxVariableIndex[expression]} >= indexOfMostSignificantVariableToChange) {
                    value = $javaExpression;
                    ${type}Cache[$expressionID] = value;
                    // System.out.println("Computed $expressionString on " + join(assignment) + ": " + value); 
                }
                else {
                    value = ${type}Cache[$expressionID];
                    // System.out.println("Used cache for $expressionString on " + join(assignment) + ": " + value); 
                }
                $setNoAssignmentsHaveBeenComputedYetIfNeeded
                return value;
            }
        """.trimIndent())
    }

    private fun collectInformationFromRootExpression(expression: Expression) {
        alreadyGenerated.clear()
        collectIDsAndMaxVariableIndicesFromRootExpression(expression)
        collectSuperTypedJavasFromRootExpression(expression)
    }

    private fun collectIDsAndMaxVariableIndicesFromRootExpression(expression: Expression) {
        nextID = 0
        subExpressionsMaxVariableIndex = mutableMapOf()
        collectIDsAndMaxVariableIndices(expression)
    }

    private fun collectIDsAndMaxVariableIndices(expression: Expression) {
        subExpressionIDs[expression] = nextID++
        subExpressionStrings[expression] = expression.toString();
        if (expression.syntacticFormType == "Symbol") {
            collectMaxVariableIndicesFromSymbol(expression)
        }
        else {
            collectMaxVariableIndicesFromCompoundExpression(expression)
        }
    }

    private fun collectMaxVariableIndicesFromSymbol(expression: Expression) {
        subExpressionsMaxVariableIndex[expression] = variableIndices[expression] ?: -1
    }

    private fun collectMaxVariableIndicesFromCompoundExpression(expression: Expression) {
        expression.arguments.forEach { collectIDsAndMaxVariableIndices(it) }
        val subMaxVariableIndices = expression.arguments.map { subExpressionsMaxVariableIndex[it] as Int }
        subExpressionsMaxVariableIndex[expression] = subMaxVariableIndices.max() as Int
    }

    private fun collectSuperTypedJavasFromRootExpression(expression: Expression) {
        subExpressionSuperTypedJavas = mutableMapOf()
        collectSuperTypedJavas(expression)
    }

    private fun collectSuperTypedJavas(expression: Expression) {
        subExpressionSuperTypedJavas[expression] = super.typedJava(expression)
        expression.arguments.forEach(::collectSuperTypedJavas)
    }

    override fun java(expression: Expression): String {
        return if (isNonVariableSymbol(expression)) {
            expression.toString()
        }
        else {
            "${methodName(expression)}(assignment)"
        }
    }

    private fun methodName(expression: Expression) = "expression${subExpressionIDs[expression]}"

    private fun isNonVariableSymbol(expression: Expression) =
            expression.syntacticFormType == "Symbol" && !isVariable(expression)
}