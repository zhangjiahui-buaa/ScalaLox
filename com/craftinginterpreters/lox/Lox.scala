package com.craftinginterpreters.lox

import java.nio.file.Files
import java.nio.file.Paths
import scala.io.{Source, StdIn}
import scala.collection.mutable.ListBuffer
import com.craftinginterpreters.lox.{Scanner, Token}
object Lox{

    private var hadError: Boolean = false
    def main(args: Array[String]): Unit = {
        if(args.length > 1){
            println("Usage: jlox [script]")
            System.exit(64)
        }else if (args.length == 1){
            runFile(args(0))
        }else{
            runPrompt()
        }
    }

    private def runFile(path: String): Unit = {
        run(Source.fromFile(path).mkString)
        if(hadError) System.exit((65))
    }

    private def runPrompt(): Unit = {
        while(true){
            print("> ")
            val str = StdIn.readLine()
            if(str == null) return
            run(str)
            hadError = false
        }
    }

    private def run(source: String): Unit = {
        val scanner: Scanner = new Scanner(source)
        val tokens: ListBuffer[Token] = scanner.scanTokens()

        for(token <- tokens){
            println(token)
        }
    }

    def error(line: Int, message: String): Unit = {
        report(line, "", message)
    }

    private def report(line: Int, where: String, message: String): Unit = {
        System.err.println(s"[line $line] Error $where: $message")
    }
}