package com.sri.ai.grinder.interpreter

import com.sri.ai.expresso.api.Expression
import com.sri.ai.expresso.helper.SubExpressionsDepthFirstIterator
import com.sri.ai.util.Util

class CompilationIncrementalEvaluator(variables: List<Expression>): CompilationEvaluator(variables) {

    // In this extension, Java expressions for a sub-expression can be of two types:
    // a method invocation of the respective method, or the body of that method decomposing
    // the expression into an operation on the Java expression of its arguments (sub-expressions).
    // To distinguish those two senses, we call the former "java expression" and the latter "super Java expression"
    // (since the super class defines Java expressions as an operation on arguments).
    // Importantly, note that the "super Java expression" uses the method 'javaExpression' on an expression's arguments.
    // Because 'javaExpression' is overridden here, the "super Java expression" will be an operation on the
    // Java expressions of its arguments, which will be method invocations.

    private val subExpressionIDs = mutableMapOf<Expression, Int>()
    private var nextID: Int = 0
    private var subExpressionsMaxVariableIndex = mutableMapOf<Expression, Int>()

    private var subExpressionSuperJavaExpressionsAndTypes = mutableMapOf<Expression, Pair<String, String>>()

    override fun evaluatorClassDefinition(expression: Expression): String {
        collectInformationFromRootExpression(expression)
        return super.evaluatorClassDefinition(expression)
    }

    override fun supplementaryDefinitions(expression: Expression): String {
        return subExpressionMethodDefinitions(expression)
    }

    private fun subExpressionMethodDefinitions(expression: Expression): String {
        return Util.`in`(SubExpressionsDepthFirstIterator(expression))
                .filterNot { isNonVariableSymbol(it) }
                .joinToString("\n\n") { subExpressionMethodDefinition(it) }
    }

    private fun subExpressionMethodDefinition(expression: Expression): String {
        // We use super.javaExpressionAndType, which expands into, for example, javaExpressionAndType(arg0) + javaExpressionAndType(arg1),
        // which in turn will use *this* class' javaExpressionAndType which is just the expression's method invocation.
        val (javaExpression, type) = subExpressionSuperJavaExpressionsAndTypes[expression] as Pair<String, String>
        return (
        """
            private $type ${methodName(expression)}(int[] assignment) {
                return $javaExpression;
            }
        """.trimIndent())
    }

    private fun collectInformationFromRootExpression(expression: Expression) {
        collectIDsAndMaxVariableIndicesFromRootExpression(expression)
        collectSuperJavaExpressionsAndTypesFromRootExpression(expression)
    }

    private fun collectIDsAndMaxVariableIndicesFromRootExpression(expression: Expression) {
        nextID = 0
        subExpressionsMaxVariableIndex = mutableMapOf()
        collectIDsAndMaxVariableIndices(expression)
    }

    private fun collectIDsAndMaxVariableIndices(expression: Expression) {
        subExpressionIDs[expression] = nextID++
        if (expression.syntacticFormType == "Symbol") {
            collectMaxVariableIndicesFromSymbol(expression)
        }
        else {
            collectMaxVariableIndicesFromCompoundExpression(expression)
        }
    }

    private fun collectMaxVariableIndicesFromSymbol(expression: Expression) {
        subExpressionsMaxVariableIndex[expression] = variableIndices[expression] ?: variableIndices.size
    }

    private fun collectMaxVariableIndicesFromCompoundExpression(expression: Expression) {
        expression.arguments.forEach { collectIDsAndMaxVariableIndices(it) }
        val subMaxVariableIndices = expression.arguments.map { subExpressionsMaxVariableIndex[it] as Int }
        subExpressionsMaxVariableIndex[expression] = subMaxVariableIndices.max() as Int
    }

    private fun collectSuperJavaExpressionsAndTypesFromRootExpression(expression: Expression) {
        subExpressionSuperJavaExpressionsAndTypes = mutableMapOf()
        collectSuperJavaExpressionsAndTypes(expression)
    }

    private fun collectSuperJavaExpressionsAndTypes(expression: Expression) {
        subExpressionSuperJavaExpressionsAndTypes[expression] = super.javaExpressionAndType(expression)
        expression.arguments.forEach(::collectSuperJavaExpressionsAndTypes)
    }

    override fun javaExpression(expression: Expression): String {
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