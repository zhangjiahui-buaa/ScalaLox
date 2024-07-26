package com.craftinginterpreters.lox
import com.craftinginterpreters.lox.TokenType._
import scala.collection.mutable.ListBuffer
class Scanner(source: String){
    private var start: Int = 0
    private var current: Int = 0
    private var line: Int = 0
    private val tokens: ListBuffer[Token] = ListBuffer[Token]()
    private val reserved: Map[String, TokenType] = Map(
        "and" -> AND,
        "class" -> CLASS,
        "else" -> ELSE,
        "false" -> FALSE,
        "for" -> FOR,
        "fun" -> FUN,
        "if" -> IF,
        "nil" -> NIL,
        "or" -> OR,
        "print" -> PRINT,
        "return" -> RETURN,
        "super" -> SUPER,
        "this" -> THIS,
        "true" -> TRUE,
        "var" -> VAR,
        "while" -> WHILE
    )
    def scanTokens(): ListBuffer[Token] = {
        while(!isAtEnd()){
            start = current
            scanToken()
        }
        tokens += new Token(EOF, "", null, line)
        tokens
    }
    private def scanToken(): Unit = {
        val c: Char = advance()
        c match{
            // single char
            case '(' => addToken(LEFT_PAREN)
            case ')' => addToken(RIGHT_PAREN)
            case '{' => addToken(LEFT_BRACE)
            case '}' => addToken(RIGHT_BRACE)
            case ',' => addToken(COMMA)
            case '.' => addToken(DOT)
            case '-' => addToken(MINUS)
            case '+' => addToken(PLUS)
            case ';' => addToken(SEMICOLON)
            case '*' => addToken(STAR)
            // double or single char
            case '!' => if(probe('=')) addToken(BANG_EQUAL) else addToken(BANG)
            case '=' => if(probe('=')) addToken(EQUAL_EQUAL) else addToken(EQUAL)
            case '<' => if(probe('=')) addToken(LESS_EQUAL) else addToken(LESS)
            case '>' => if(probe('=')) addToken(GREATER_EQUAL) else addToken(GREATER)
            case '/' => if(probe('/')){
                while(peek() != '\n' && !isAtEnd()) advance()
            }else{
                addToken(SLASH)
            }
            // whitespace
            case ' ' => 
            case '\r' =>
            case '\t' =>
            case '\n' => line = line + 1
            // string lieral
            case '"' => string()
            // num literal
            case _ => if(isDigit(c)){
                number()
            } else if(isAlpha(c)){
                identifier()
            }else {
                Lox.error(line, "Unexpected character.")
            }
        }
    }

    private def identifier(): Unit = {
        while(isAlpha(peek()) || isDigit(peek())){
            advance()
        }
        val str: String = source.substring(start, current)
        if(reserved.contains(str)) addToken(reserved(str)) else addToken(IDENTIFIER)
    }

    private def isAlpha(c: Char): Boolean = {
        (c >= 'a' && c <='z') || (c >= 'A' && c <= 'Z') || c == '_'
    }

    private def number(): Unit = {

        //integer part
        while(isDigit(peek())){
            advance()
        }

        //decimal part
        if(peek() == '.' && isDigit(peekNext())){
            advance()
            while(isDigit(peek())){
                advance()
            }
        }

        val numString: String = source.substring(start, current)
        val num: Double = numString.toDouble
        addToken(NUMBER, num)

    }

    private def isDigit(c: Char): Boolean = {
        c >= '0' && c <= '9' 
    }

    private def string(): Unit = {
        while(peek() != '"' && !isAtEnd()){
            if(peek() == '\n'){
                line += 1
            }
            advance()
        }
        if(isAtEnd()) {
            Lox.error(line, "Unbounded string literal.")
            return
        }
        advance()

        val value = source.substring(start+1, current-1)
        addToken(STRING, value)
    }

    private def peek(): Char = {
        if(isAtEnd()) '\u0000'
        else source(current)
    }

    private def peekNext(): Char = {
        if(current + 1 >= source.length) '\u0000'
        else source(current+1)
    }
    private def probe(expected: Char): Boolean = {
        if(isAtEnd()) return false
        if(source(current) != expected) return false
        current += 1
        true
    }

    private def advance(): Char = {
        current += 1
        source(current - 1)
    }

    private def addToken(tokenType: TokenType): Unit = {
        addToken(tokenType, null)
    }

    private def addToken(tokenType: TokenType, literal: Any): Unit = {
        val text: String = source.substring(start, current)
        tokens += new Token(tokenType, text, literal, line)
    }

    private def isAtEnd(): Boolean = {
        current >= source.length
    }
}