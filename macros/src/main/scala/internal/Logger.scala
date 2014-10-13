/*
Copyright 2014 sumito3478 <sumito3478@gmail.com>
Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
*/
package scarango
package internal

import scala.language.experimental.macros
import scala.reflect.macros._
import scala.collection.immutable._
import scala.reflect.macros._
import logging._

object Logger {
  object Macros {
    def log(c: Context)(_msg: c.Tree, _impl: c.Tree, level: String): c.Tree = {
      val r = new Reflect(c)
      import r.{ c => _, _ }
      import r.c.universe._
      val msg = _msg.asInstanceOf[r.c.Tree]
      val impl = _impl.asInstanceOf[r.c.Tree]
      checking(weakTypeOf[Unit]) {
        val enabledMethod = newTermName("is" ++ level.take(1).toUpperCase(java.util.Locale.ENGLISH) ++ level.drop(1) ++ "Enabled")
        val logMethod = newTermName(level)
        val pos = {
          val p = r.c.enclosingPosition
          s"${p.source.file.relativePath}:${p.line}:${p.column}"
        }
        q"""
        if ($impl.$enabledMethod)
          $impl.$logMethod($pos, $msg)
      """
      }.asInstanceOf[c.Tree]
    }
    def logThrowable(c: Context)(_msg: c.Tree, _e: c.Tree, _impl: c.Tree, level: String): c.Tree = {
      val r = new Reflect(c)
      import r.{ c => _, _ }
      import r.c.universe._
      val msg = _msg.asInstanceOf[r.c.Tree]
      val impl = _impl.asInstanceOf[r.c.Tree]
      val e = _e.asInstanceOf[r.c.Tree]
      checking(weakTypeOf[Unit]) {
        val enabledMethod = newTermName("is" ++ level.take(1).toUpperCase(java.util.Locale.ENGLISH) ++ level.drop(1) ++ "Enabled")
        val logMethod = newTermName(level ++ "Throwable")
        val pos = {
          val p = r.c.enclosingPosition
          s"${p.source.file.relativePath}:${p.line}:${p.column}"
        }
        q"""
        if ($impl.$enabledMethod)
          $impl.$logMethod($pos, $msg, $e)
      """
      }.asInstanceOf[c.Tree]
    }
    def error[L](c: Context)(msg: c.Expr[String])(impl: c.Expr[LoggerImpl[L]]): c.Expr[Unit] = c.Expr[Unit](log(c)(msg.tree, impl.tree, "error"))
    def errorThrowable[L](c: Context)(msg: c.Expr[String], e: c.Expr[Throwable])(impl: c.Expr[LoggerImpl[L]]): c.Expr[Unit] = c.Expr[Unit](logThrowable(c)(msg.tree, e.tree, impl.tree, "error"))
    def warn[L](c: Context)(msg: c.Expr[String])(impl: c.Expr[LoggerImpl[L]]): c.Expr[Unit] = c.Expr[Unit](log(c)(msg.tree, impl.tree, "warn"))
    def warnThrowable[L](c: Context)(msg: c.Expr[String], e: c.Expr[Throwable])(impl: c.Expr[LoggerImpl[L]]): c.Expr[Unit] = c.Expr[Unit](logThrowable(c)(msg.tree, e.tree, impl.tree, "warn"))
    def info[L](c: Context)(msg: c.Expr[String])(impl: c.Expr[LoggerImpl[L]]): c.Expr[Unit] = c.Expr[Unit](log(c)(msg.tree, impl.tree, "info"))
    def infoThrowable[L](c: Context)(msg: c.Expr[String], e: c.Expr[Throwable])(impl: c.Expr[LoggerImpl[L]]): c.Expr[Unit] = c.Expr[Unit](logThrowable(c)(msg.tree, e.tree, impl.tree, "info"))
    def debug[L](c: Context)(msg: c.Expr[String])(impl: c.Expr[LoggerImpl[L]]): c.Expr[Unit] = c.Expr[Unit](log(c)(msg.tree, impl.tree, "debug"))
    def debugThrowable[L](c: Context)(msg: c.Expr[String], e: c.Expr[Throwable])(impl: c.Expr[LoggerImpl[L]]): c.Expr[Unit] = c.Expr[Unit](logThrowable(c)(msg.tree, e.tree, impl.tree, "debug"))
    def trace[L](c: Context)(msg: c.Expr[String])(impl: c.Expr[LoggerImpl[L]]): c.Expr[Unit] = c.Expr[Unit](log(c)(msg.tree, impl.tree, "trace"))
    def traceThrowable[L](c: Context)(msg: c.Expr[String], e: c.Expr[Throwable])(impl: c.Expr[LoggerImpl[L]]): c.Expr[Unit] = c.Expr[Unit](logThrowable(c)(msg.tree, e.tree, impl.tree, "trace"))
  }
  def error[L: LoggerImpl](msg: String): Unit = macro Macros.error[L]
  def errorThrowable[L: LoggerImpl](msg: String, e: Throwable): Unit = macro Macros.errorThrowable[L]
  def warn[L: LoggerImpl](msg: String): Unit = macro Macros.warn[L]
  def warnThrowable[L: LoggerImpl](msg: String, e: Throwable): Unit = macro Macros.warnThrowable[L]
  def info[L: LoggerImpl](msg: String): Unit = macro Macros.info[L]
  def infoThrowable[L: LoggerImpl](msg: String, e: Throwable): Unit = macro Macros.infoThrowable[L]
  def debug[L: LoggerImpl](msg: String): Unit = macro Macros.debug[L]
  def debugThrowable[L: LoggerImpl](msg: String, e: Throwable): Unit = macro Macros.debugThrowable[L]
  def trace[L: LoggerImpl](msg: String): Unit = macro Macros.trace[L]
  def traceThrowable[L: LoggerImpl](msg: String, e: Throwable): Unit = macro Macros.traceThrowable[L]
}

