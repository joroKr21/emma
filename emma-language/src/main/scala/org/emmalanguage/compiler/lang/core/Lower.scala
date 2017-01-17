/*
 * Copyright © 2014 TU Berlin (emma@dima.tu-berlin.de)
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package org.emmalanguage
package compiler.lang.core

import compiler.Common
import compiler.ir.DSCFAnnotations._
import compiler.lang.source.Source
import util.Monoids._

import shapeless._

import scala.collection.breakOut
import scala.util.control.TailCalls.TailRec

/** Trampolining tail calls to avoid stack overflow. */
private[core] trait Lower extends Common {
  self: Core with Source =>

  /** Trampolining tail calls to avoid stack overflow. */
  private[core] object Lower {

    import UniverseImplicits._
    import Core.{Lang => core}
    import Source.{Lang => src}
    import u.internal.flags

    private val module = u.rootMirror.staticModule("scala.util.control.TailCalls")
    private val TailCalls = Some(core.Ref(module))
    private val done = module.info.member(api.TermName("done")).asMethod
    private val tailcall = module.info.member(api.TermName("tailcall")).asMethod
    private val result = api.Type[TailRec[Nothing]].typeConstructor
      .member(api.TermName("result")).asMethod

    /**
     * Wraps return values and tail calls in a trampoline, finally returning its result.
     *
     * == Preconditions ==
     * - The input tree is in DSCF (see [[Core.dscf]]).
     *
     * == Postconditions ==
     * - All return values and tail calls are wrapped in a trampoline.
     * - The ANF shape of the input is NOT preserved.
     */
    lazy val trampoline: u.Tree => u.Tree = api.BottomUp.unsafe
      .withAncestors.inherit { // Local method definitions.
        case core.Let(_, defs, _) =>
          (for (core.DefDef(method, tparams, paramss, _) <- defs) yield {
            val (own, nme, pos) = (method.owner, method.name, method.pos)
            val flg = flags(method)
            val pss = paramss.map(_.map(_.symbol.asTerm))
            val res = api.Type.kind1[TailRec](method.info.finalResultType)
            val ans = method.annotations
            method -> api.DefSym(own, nme, tparams, pss, res, flg, pos, ans)
          }).toMap
      } (overwrite).transformWith {
        // Return position in a method definition, wrap in trampoline.
        case Attr.inh(tree, local :: (_ :+ (_: u.DefDef) :+ core.Let(_, _, expr)) :: _)
          if tree == expr => wrap(expr, local)

        // Local method definition, returns a trampoline.
        case Attr.inh(core.DefDef(method, tparams, paramss, core.Let(vals, defs, expr)),
          local :: _) =>
          val pss = paramss.map(_.map(_.symbol.asTerm))
          core.DefDef(local(method), tparams, pss, core.Let(vals, defs, expr))

        // Local method call outside, retrieve the trampoline result.
        case Attr.inh(core.DefCall(None, cont, targs, argss), local :: ancestors :: _)
          if local.contains(cont) && ancestors.forall {
            case _: u.DefDef => false
            case _ => true
          } => core.DefCall(Some(core.DefCall(None, local(cont), targs, argss)), result)
      }.andThen(_.tree)

    lazy val imperative: u.Tree => u.Tree = api.BottomUp
      .withOwner.transformWith {
      // No control-flow
      case let @ core.Let(_, Seq(), _) => let
      // Branches
      case core.Let(vals, defs, core.Branch(cond, thn, els))
        if defs.exists(d => api.Sym.findAnn[branch](d.symbol).isDefined) =>
        val thnBody = defs.find(d => api.Sym.findAnn[thenBranch](d.symbol).isDefined)
        val elsBody = defs.find(d => api.Sym.findAnn[elseBranch](d.symbol).isDefined)
        val suf = defs.last
        val vars = suf.symbol.asMethod.paramLists.flatten
        els

      // While loop
      case Attr.inh(src.Block(stats :+
        core.DefDef(method, _, Seq(loopPars),
          src.Block(condStats :+
            core.DefDef(bodyMeth, _, Seq(bodyPars), src.Block(bodyStats, _)) :+
            core.DefDef(sufMeth, _, Seq(sufPars), sufBody),
            src.Branch(condExpr,
              core.DefCall(None, _, _, Seq(bodyArgs)),
              core.DefCall(None, _, _, Seq(sufArgs))))),
        src.DefCall(_, _, _, Seq(loopArgs))), owner :: _)
        if api.Sym.findAnn[whileLoop](method).isDefined =>

        val loopAliases: Map[u.TermSymbol, u.TermSymbol] = loopPars.map {
          case src.ParDef(par, _) => par -> api.VarSym(owner, par.name, par.info)
        } (breakOut)

        val bodyAliases = for ((src.ParDef(par, _), src.Ref(arg)) <- bodyPars zip bodyArgs)
          yield par -> loopAliases(arg)

        val sufAliases = for ((src.ParDef(par, _), src.Ref(arg)) <- sufPars zip sufArgs)
          yield par -> loopAliases(arg)



        api.Tree.rename(loopAliases.toSeq)(
          src.Block(stats :+ src.While(
            src.Block(condStats, condExpr),
            api.Tree.rename(bodyAliases)(src.Block(bodyStats))),
            sufBody))
    }._tree

    /** Wraps the return value / tail call of a method in a trampoline. */
    private def wrap(expr: u.Tree, local: Map[u.MethodSymbol, u.MethodSymbol]): u.Tree =
      expr match {
        // Wrap both branches.
        case core.Branch(cond, thn, els) =>
          core.Branch(cond, wrap(thn, local), wrap(els, local))

        // Wrap a tail call.
        case core.DefCall(None, cont, targs, argss) if local.contains(cont) =>
          val Res = cont.info.finalResultType
          core.DefCall(TailCalls, tailcall, Seq(Res),
            Seq(Seq(core.DefCall(None, local(cont), targs, argss))))

        // Wrap a return value.
        case _ =>
          core.DefCall(TailCalls, done, Seq(expr.tpe), Seq(Seq(expr)))
      }
  }
}
