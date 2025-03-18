import java.io.File

fun main() {
    val inputFiles = listOf("input_1.txt", "input_2.txt", "input_3.txt")
    val lexemeOrder = listOf(
        "LINE_NUMBER", "NUMBER", "KEYWORD", "OPERATOR", "SYMBOL",
        "Literal Strings", "IDENTIFIER", "COMMENT", "MATH_FUNCTION", "ARRAY", "UNKNOWN"
    )

    for (fileName in inputFiles) {
        val file = File(fileName)
        if (!file.exists()) {
            println("Error: File $fileName not found.")
            continue
        }

        val code = file.readLines()
        val tokens = tokenize(code)

        // Проверка, что последняя строка содержит END
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

        // Группируем лексемы по классам (все элементы)
        val lexemeClasses = mutableMapOf<String, MutableList<String>>()
        for (token in tokens) {
            lexemeClasses.computeIfAbsent(token.type) { mutableListOf() }.add(token.value)
        }

        // Выводим лексемы по классам в порядке lexemeOrder (все элементы)
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

        // Третий вывод: уникальные лексемы по классам
        println("Unique lexemes by class: ")
        println("-------------------------------------------------")

        // Группируем лексемы по классам (уникальные элементы)
        val uniqueLexemeClasses = mutableMapOf<String, MutableSet<String>>()
        for (token in tokens) {
            uniqueLexemeClasses.computeIfAbsent(token.type) { mutableSetOf() }.add(token.value)
        }

        // Выводим уникальные лексемы по классам в порядке lexemeOrder
        for (type in lexemeOrder) {
            val lexemes = uniqueLexemeClasses[type] ?: emptySet()
            val lexemeList = if (lexemes.isEmpty()) "No tokens" else lexemes.joinToString(", ")
            println("$type (${lexemes.size}): $lexemeList")
        }
        println("\n")
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
    val lineNumberRegex = Regex("^(\\d+\\.\\d+|\\d+)\\s") // Номер строки должен заканчиваться пробелом
    val identifierRegex = Regex("^[A-Za-z_][A-Za-z0-9_]*$")
    val commentRegex = Regex("!.*")
    val functionRegex = Regex("([A-Z]+)\\((.*)\\)") // Регулярное выражение для математических функций
    val arrayRegex = Regex("([A-Za-z_][A-Za-z0-9_]*)\\(([^)]*)\\)") // Регулярное выражение для массивов

    var lastLineNumber: Double? = null // Переменная для хранения последнего номера строки

    for (line in codeLines) {
        var remainingLine = line.trim()
        if (remainingLine.isEmpty()) continue

        // Сначала обрабатываем номер строки
        val lineNumberMatch = lineNumberRegex.find(remainingLine)
        if (lineNumberMatch != null) {
            val currentLineNumber = lineNumberMatch.value.trim().toDouble()

            // Проверяем, что номер строки возрастает
            if (lastLineNumber != null && currentLineNumber <= lastLineNumber) {
                // Яркое оформление ошибки
                println("\u001B[31m-------------------------------------------------")
                println("ERROR: Line numbers are not in ascending order!")
                println("Error at line: $currentLineNumber")
                println("Previous line number: $lastLineNumber")
                println("-------------------------------------------------\u001B[0m")
            }

            lastLineNumber = currentLineNumber // Обновляем последний номер строки
            tokens.add(Token("LINE_NUMBER", lineNumberMatch.value.trim()))
            remainingLine = remainingLine.substring(lineNumberMatch.range.last + 1).trim()
        }

        // Затем обрабатываем комментарий
        val commentMatch = commentRegex.find(remainingLine)
        if (commentMatch != null) {
            tokens.add(Token("COMMENT", commentMatch.value))
            remainingLine = remainingLine.split("!").first().trim()
        }

        // Обрабатываем строки после TYPE
        if (remainingLine.startsWith("TYPE")) {
            // Добавляем TYPE как KEYWORD
            tokens.add(Token("KEYWORD", "TYPE"))
            remainingLine = remainingLine.substringAfter("TYPE").trim()

            // Проверяем, что все кавычки закрыты
            val quoteCount = remainingLine.count { it == '"' }
            if (quoteCount % 2 != 0) {
                // Яркое оформление ошибки
                println("\u001B[31m-------------------------------------------------")
                println("ERROR: String literals after TYPE are not properly closed with double quotes!")
                println("Error at line: ${lineNumberMatch?.value?.trim() ?: "unknown"}")
                println("Line content: $remainingLine")
                println("-------------------------------------------------\u001B[0m")
            } else {
                // Обрабатываем строку с несколькими строковыми литералами
                val parts = remainingLine.split("\"")
                for (i in parts.indices step 2) {
                    if (i + 1 < parts.size) {
                        tokens.add(Token("SYMBOL", "\" (QUOTE)"))
                        tokens.add(Token("Literal Strings", parts[i + 1]))
                        tokens.add(Token("SYMBOL", "\" (QUOTE)"))
                    }
                }
                // Убираем обработанные строковые литералы из оставшейся части строки
                remainingLine = parts.filterIndexed { index, _ -> index % 2 == 0 }.joinToString("").trim()
            }
        }

        // Обрабатываем оставшуюся часть строки
        val words = mutableListOf<String>()
        var insideString = false
        val stringBuffer = StringBuilder()

        for (char in remainingLine) {
            when {
                char == '"' -> {
                    if (insideString) {
                        // Если строка заканчивается, добавляем всё содержимое как одну лексему
                        words.add(stringBuffer.toString())
                        stringBuffer.clear()
                        insideString = false
                    } else {
                        // Если строка начинается, добавляем открывающую кавычку
                        if (stringBuffer.isNotEmpty()) {
                            words.add(stringBuffer.toString())
                            stringBuffer.clear()
                        }
                        insideString = true
                    }
                }
                insideString -> stringBuffer.append(char) // Внутри строки добавляем символы в буфер
                char.isWhitespace() -> if (stringBuffer.isNotEmpty()) {
                    words.add(stringBuffer.toString())
                    stringBuffer.clear()
                }
                else -> stringBuffer.append(char)
            }
        }
        if (stringBuffer.isNotEmpty()) words.add(stringBuffer.toString())

        // Разделяем идентификаторы и запятые, если они идут вместе без пробела
        val processedWords = mutableListOf<String>()
        for (word in words) {
            if (word.endsWith(",") && word.length > 1) {
                // Разделяем идентификатор и запятую
                processedWords.add(word.dropLast(1)) // Идентификатор
                processedWords.add(",") // Запятая
            } else {
                processedWords.add(word)
            }
        }

        // Обрабатываем оставшиеся слова
        for (word in processedWords) {
            when {
                word == "\"" -> tokens.add(Token("SYMBOL", "\" (QUOTE)"))
                numberRegex.matches(word) -> tokens.add(Token("NUMBER", word)) // Обрабатываем числа
                word in keywords -> tokens.add(Token("KEYWORD", word))
                word in operators -> tokens.add(Token("OPERATOR", word))
                symbols.containsKey(word) -> tokens.add(Token("SYMBOL", "${word} (${symbols[word]})"))
                functionRegex.matches(word) -> {
                    // Разбиваем математическую функцию на компоненты
                    val functionMatch = functionRegex.find(word)
                    if (functionMatch != null) {
                        val functionName = functionMatch.groupValues[1]
                        val arguments = functionMatch.groupValues[2]

                        // Добавляем имя функции как MATH_FUNCTION, если оно есть в списке
                        if (functionName in mathFunctions) {
                            tokens.add(Token("MATH_FUNCTION", functionName))
                        } else {
                            // Если имя функции не в списке, добавляем как ARRAY
                            tokens.add(Token("ARRAY", functionName))
                        }

                        // Добавляем открывающую скобку как SYMBOL
                        tokens.add(Token("SYMBOL", "( (LEFT_PAREN)"))

                        // Обрабатываем аргументы
                        val argumentTokens = tokenize(listOf(arguments)) // Рекурсивно обрабатываем аргументы
                        tokens.addAll(argumentTokens)

                        // Добавляем закрывающую скобку как SYMBOL
                        tokens.add(Token("SYMBOL", ") (RIGHT_PAREN)"))
                    }
                }
                arrayRegex.matches(word) -> {
                    // Обрабатываем массивы
                    val arrayMatch = arrayRegex.find(word)
                    if (arrayMatch != null) {
                        val arrayName = arrayMatch.groupValues[1]
                        val index = arrayMatch.groupValues[2]

                        // Добавляем имя массива как ARRAY
                        tokens.add(Token("ARRAY", arrayName))

                        // Добавляем открывающую скобку как SYMBOL
                        tokens.add(Token("SYMBOL", "( (LEFT_PAREN)"))

                        // Обрабатываем индекс
                        val indexTokens = tokenize(listOf(index)) // Рекурсивно обрабатываем индекс
                        tokens.addAll(indexTokens)

                        // Добавляем закрывающую скобку как SYMBOL
                        tokens.add(Token("SYMBOL", ") (RIGHT_PAREN)"))
                    }
                }
                word.startsWith("(") || word.endsWith(")") -> {
                    // Обрабатываем выражения в скобках
                    if (word.startsWith("(")) {
                        tokens.add(Token("SYMBOL", "( (LEFT_PAREN)"))
                        val content = word.substring(1)
                        if (content.isNotEmpty()) {
                            val contentTokens = tokenize(listOf(content)) // Рекурсивно обрабатываем содержимое
                            tokens.addAll(contentTokens)
                        }
                    }
                    if (word.endsWith(")")) {
                        val content = word.dropLast(1)
                        if (content.isNotEmpty()) {
                            val contentTokens = tokenize(listOf(content)) // Рекурсивно обрабатываем содержимое
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