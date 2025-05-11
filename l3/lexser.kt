import java.io.File

fun main() {
    val inputFiles = listOf("input_1.txt", "input_2.txt", "input_3.txt")
    val lexemeOrder = listOf(
        "LINE_NUMBER", "NUMBER", "KEYWORD", "OPERATOR", "SYMBOL",
        "Literal Strings", "IDENTIFIER", "COMMENT", "MATH_FUNCTION", "ARRAY", "UNKNOWN"
    )

    val parseTreesFile = File("parser_trees.txt")
    parseTreesFile.writeText("") 

    for (fileName in inputFiles) {
        val file = File(fileName)
        if (!file.exists()) {
            println("Error: File $fileName not found.")
            continue
        }

        val code = file.readLines()
        val tokens = tokenize(code)

        if (code.lastOrNull()?.trim()?.endsWith("END") != true) {
            println("\u001B[31m-------------------------------------------------")
            println("ERROR: The program must end with the keyword 'END'!")
            println("File: $fileName")
            println("-------------------------------------------------\u001B[0m")
        }

        println("-------------------------------------------------")
        println("Processing file: $fileName")
        println("Lexeme classes: ")
        println("-------------------------------------------------")

        val lexemeClasses = mutableMapOf<String, MutableList<String>>()
        for (token in tokens) {
            lexemeClasses.computeIfAbsent(token.type) { mutableListOf() }.add(token.value)
        }

        for (type in lexemeOrder) {
            val lexemes = lexemeClasses[type] ?: emptyList()
            val lexemeList = if (lexemes.isEmpty()) "No tokens" else lexemes.joinToString(", ")
            println("$type (${lexemes.size}): $lexemeList")
        }
        println()

        println("Table ")
        println("Item Number | Identifier | Information")
        println("-------------------------------------------------")

        tokens.forEachIndexed { index, token ->
            println("${index + 1} | ${token.value} | ${token.type}")
        }
        println("\n")

        val uniqueLexemeClasses = mutableMapOf<String, MutableSet<String>>()
        for (token in tokens) {
            uniqueLexemeClasses.computeIfAbsent(token.type) { mutableSetOf() }.add(token.value)
        }

        for (type in lexemeOrder) {
            val lexemes = uniqueLexemeClasses[type] ?: emptySet()
            val lexemeList = if (lexemes.isEmpty()) "No tokens" else lexemes.joinToString(", ")
            println("$type (${lexemes.size}): $lexemeList")
        }
        println("\n")

        val parseTree = generateParseTree(tokens)
        parseTreesFile.appendText("File: $fileName\n")
        parseTreesFile.appendText(parseTree)
        parseTreesFile.appendText("\n-------------------------------------------------\n\n")
    }
}

data class Token(val type: String, val value: String)

fun tokenize(codeLines: List<String>): List<Token> {
    val tokens = mutableListOf<Token>()
    val operators = setOf("=", "+", "-", "*", "/", "^", ":")
    val keywords = setOf("LET", "END", "FOR", "NEXT", "TYPE", "ACCEPT")
    val symbols = mapOf(
        "(" to "LEFT_PAREN", ")" to "RIGHT_PAREN", "{" to "LEFT_BRACE", "}" to "RIGHT_BRACE",
        ";" to "SEMICOLON", "\"" to "QUOTE", "," to "COMMA"
    )
    val mathFunctions = setOf("FSIN", "FCOS", "FATN", "FLOG", "FSQT", "FABS", "FITR", "ABSVAL", "FEXP", "FSGN", "EXPVAL", "FRAN")

    val numberRegex = Regex("[-]?\\d+(\\.\\d+)?")
    val lineNumberRegex = Regex("^(\\d+\\.\\d+|\\d+)\\s")
    val identifierRegex = Regex("^[A-Za-z_][A-Za-z0-9_]*$")
    val commentRegex = Regex("!.*")
    val functionRegex = Regex("([A-Z]+)\\((.*)\\)")
    val arrayRegex = Regex("([A-Za-z_][A-Za-z0-9_]*)\\(([^)]*)\\)")

    var lastLineNumber: Double? = null

    for (line in codeLines) {
        var remainingLine = line.trim()
        if (remainingLine.isEmpty()) continue

        val lineNumberMatch = lineNumberRegex.find(remainingLine)
        if (lineNumberMatch != null) {
            val currentLineNumber = lineNumberMatch.value.trim().toDouble()
            if (lastLineNumber != null && currentLineNumber <= lastLineNumber) {
                println("\u001B[31m-------------------------------------------------")
                println("ERROR: Line numbers are not in ascending order!")
                println("Error at line: $currentLineNumber")
                println("Previous line number: $lastLineNumber")
                println("-------------------------------------------------\u001B[0m")
            }
            lastLineNumber = currentLineNumber
            tokens.add(Token("LINE_NUMBER", lineNumberMatch.value.trim()))
            remainingLine = remainingLine.substring(lineNumberMatch.range.last + 1).trim()
        }

        val commentMatch = commentRegex.find(remainingLine)
        if (commentMatch != null) {
            tokens.add(Token("COMMENT", commentMatch.value))
            remainingLine = remainingLine.split("!").first().trim()
        }

        if (remainingLine.startsWith("TYPE")) {
            tokens.add(Token("KEYWORD", "TYPE"))
            remainingLine = remainingLine.substringAfter("TYPE").trim()
            val quoteCount = remainingLine.count { it == '"' }
            if (quoteCount % 2 != 0) {
                println("\u001B[31m-------------------------------------------------")
                println("ERROR: String literals after TYPE are not properly closed with double quotes!")
                println("Error at line: ${lineNumberMatch?.value?.trim() ?: "unknown"}")
                println("Line content: $remainingLine")
                println("-------------------------------------------------\u001B[0m")
            } else {
                val parts = remainingLine.split("\"")
                for (i in parts.indices step 2) {
                    if (i + 1 < parts.size) {
                        tokens.add(Token("SYMBOL", "\" (QUOTE)"))
                        tokens.add(Token("Literal Strings", parts[i + 1]))
                        tokens.add(Token("SYMBOL", "\" (QUOTE)"))
                    }
                }
                remainingLine = parts.filterIndexed { index, _ -> index % 2 == 0 }.joinToString("").trim()
            }
        }

        val words = mutableListOf<String>()
        var insideString = false
        val stringBuffer = StringBuilder()

        for (char in remainingLine) {
            when {
                char == '"' -> {
                    if (insideString) {
                        words.add(stringBuffer.toString())
                        stringBuffer.clear()
                        insideString = false
                    } else {
                        if (stringBuffer.isNotEmpty()) {
                            words.add(stringBuffer.toString())
                            stringBuffer.clear()
                        }
                        insideString = true
                    }
                }
                insideString -> stringBuffer.append(char)
                char.isWhitespace() -> if (stringBuffer.isNotEmpty()) {
                    words.add(stringBuffer.toString())
                    stringBuffer.clear()
                }
                else -> stringBuffer.append(char)
            }
        }
        if (stringBuffer.isNotEmpty()) words.add(stringBuffer.toString())

        val processedWords = mutableListOf<String>()
        for (word in words) {
            if (word.endsWith(",") && word.length > 1) {
                processedWords.add(word.dropLast(1))
                processedWords.add(",")
            } else {
                processedWords.add(word)
            }
        }

        for (word in processedWords) {
            when {
                word == "\"" -> tokens.add(Token("SYMBOL", "\" (QUOTE)"))
                numberRegex.matches(word) -> tokens.add(Token("NUMBER", word))
                word in keywords -> tokens.add(Token("KEYWORD", word))
                word in operators -> tokens.add(Token("OPERATOR", word))
                symbols.containsKey(word) -> tokens.add(Token("SYMBOL", "${word} (${symbols[word]})"))
                functionRegex.matches(word) -> {
                    val functionMatch = functionRegex.find(word)
                    if (functionMatch != null) {
                        val functionName = functionMatch.groupValues[1]
                        val arguments = functionMatch.groupValues[2]
                        if (functionName in mathFunctions) {
                            tokens.add(Token("MATH_FUNCTION", functionName))
                        } else {
                            tokens.add(Token("ARRAY", functionName))
                        }
                        tokens.add(Token("SYMBOL", "( (LEFT_PAREN)"))
                        val argumentTokens = tokenize(listOf(arguments))
                        tokens.addAll(argumentTokens)
                        tokens.add(Token("SYMBOL", ") (RIGHT_PAREN)"))
                    }
                }
                arrayRegex.matches(word) -> {
                    val arrayMatch = arrayRegex.find(word)
                    if (arrayMatch != null) {
                        val arrayName = arrayMatch.groupValues[1]
                        val index = arrayMatch.groupValues[2]
                        tokens.add(Token("ARRAY", arrayName))
                        tokens.add(Token("SYMBOL", "( (LEFT_PAREN)"))
                        val indexTokens = tokenize(listOf(index))
                        tokens.addAll(indexTokens)
                        tokens.add(Token("SYMBOL", ") (RIGHT_PAREN)"))
                    }
                }
                word.startsWith("(") || word.endsWith(")") -> {
                    if (word.startsWith("(")) {
                        tokens.add(Token("SYMBOL", "( (LEFT_PAREN)"))
                        val content = word.substring(1)
                        if (content.isNotEmpty()) {
                            val contentTokens = tokenize(listOf(content))
                            tokens.addAll(contentTokens)
                        }
                    }
                    if (word.endsWith(")")) {
                        val content = word.dropLast(1)
                        if (content.isNotEmpty()) {
                            val contentTokens = tokenize(listOf(content))
                            tokens.addAll(contentTokens)
                        }
                        tokens.add(Token("SYMBOL", ") (RIGHT_PAREN)"))
                    }
                }
                identifierRegex.matches(word) -> tokens.add(Token("IDENTIFIER", word))
                word.startsWith("-") && identifierRegex.matches(word.drop(1)) -> tokens.add(Token("IDENTIFIER", word))
                word == "," -> tokens.add(Token("SYMBOL", ", (COMMA)"))
                else -> tokens.add(Token("UNKNOWN", word))
            }
        }
    }

    return tokens
}

fun generateParseTree(tokens: List<Token>): String {
    val parser = EnhancedParser(tokens)
    return try {
        parser.parse()
    } catch (e: Exception) {
        "Error parsing expression: ${e.message}"
    }
}

class EnhancedParser(private val tokens: List<Token>) {
    private var index = 0
    private val tree = StringBuilder()
    private val indentStack = mutableListOf<String>()
    private val parentHasSibling = mutableListOf<Boolean>()

    fun parse(): String {
        while (index < tokens.size) {
            when (tokens[index].type) {
                "LINE_NUMBER" -> parseLineNumber()
                "KEYWORD" -> parseKeyword()
                "COMMENT" -> parseComment()
                else -> index++
            }
        }
        return tree.toString()
    }

    private fun parseLineNumber() {
        addNode("LINE_NUMBER: ${tokens[index].value}", isRoot = true)
        index++
    }

    private fun parseKeyword() {
        when (tokens[index].value) {
            "LET" -> parseAssignment()
            "TYPE" -> parseType()
            "ACCEPT" -> parseAccept()
            "END" -> parseEnd()
            else -> index++
        }
    }

    private fun parseAssignment() {
        startNode("LET")
        index++
        parseIdentifier()
        if (match("OPERATOR", "=")) {
            addNode("OPERATOR: =")
            index++
            parseExpression()
        }
        endNode()
    }

    private fun parseExpression() {
        startNode("Expression")
        parseTerm()
        while (match("OPERATOR", "+") || match("OPERATOR", "-")) {
            addOperatorNode()
            parseTerm()
        }
        endNode()
    }

    private fun parseTerm() {
        startNode("Term")
        parseFactor()
        while (match("OPERATOR", "*") || match("OPERATOR", "/") || match("OPERATOR", "^")) {
            addOperatorNode()
            parseFactor()
        }
        endNode()
    }

    private fun parseFactor() {
        startNode("Factor")
        when {
            match("IDENTIFIER") -> parseIdentifier()
            match("NUMBER") -> parseNumber()
            match("SYMBOL", "(") -> parseParentheses()
            else -> index++
        }
        endNode()
    }

    private fun parseIdentifier() {
        if (match("IDENTIFIER")) {
            addNode("IDENTIFIER: ${tokens[index].value}")
            index++
        }
    }

    private fun parseNumber() {
        if (match("NUMBER")) {
            addNode("NUMBER: ${tokens[index].value}")
            index++
        }
    }

    private fun parseType() {
        startNode("TYPE")
        index++
        while (index < tokens.size) {
            when {
                match("SYMBOL", "\"") -> {
                    addNode("SYMBOL: \"")
                    index++
                }
                match("Literal Strings") -> {
                    addNode("LITERAL: ${tokens[index].value}")
                    index++
                }
                match("IDENTIFIER") -> parseIdentifier()
                else -> break
            }
        }
        endNode()
    }

    private fun parseAccept() {
        startNode("ACCEPT")
        index++
        parseIdentifier()
        endNode()
    }

    private fun parseComment() {
        addNode("COMMENT: ${tokens[index].value}", isRoot = true)
        index++
    }

    private fun parseParentheses() {
        addNode("SYMBOL: (")
        index++
        parseExpression()
        if (match("SYMBOL", ")")) {
            addNode("SYMBOL: )")
            index++
        }
    }

    private fun parseEnd() {
        addNode("END", isRoot = true)
        index++
    }

    private fun startNode(label: String) {
        indentStack.add(label)
        parentHasSibling.add(false)
        tree.append("${currentIndent}+-- $label\n")
    }

    private fun endNode() {
        indentStack.removeLast()
        parentHasSibling.removeLastOrNull()
    }

    private fun addNode(text: String, isRoot: Boolean = false) {
        val indent = if (isRoot) "" else currentIndent
        tree.append("$indent+-- $text\n")
        if (!isRoot && indentStack.isNotEmpty()) {
            parentHasSibling[parentHasSibling.lastIndex] = true
        }
    }

    private val currentIndent: String
        get() = indentStack.indices.joinToString("") { 
            if (parentHasSibling.getOrNull(it) == true) "|   " else "    " 
        }

    private fun addOperatorNode() {
        if (index < tokens.size) {
            addNode("OPERATOR: ${tokens[index].value}")
            index++
        }
    }

    private fun match(type: String, value: String? = null): Boolean {
        return index < tokens.size && tokens[index].type == type &&
                (value == null || tokens[index].value == value)
    }
}