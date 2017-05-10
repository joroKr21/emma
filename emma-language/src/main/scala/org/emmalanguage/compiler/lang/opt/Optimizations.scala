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
package compiler.lang.opt

import compiler.Common
import compiler.lang.cf.ControlFlow
import compiler.lang.core.Core

/** Language optimizations. */
trait Optimizations extends Common
  with FoldForestFusion
  with FoldGroupFusion
  with ComprehensionCompiler {
  this: Core with ControlFlow =>

  /** Static (compile-time) optimizations. */
  object Optimizations {

    /** Performs [[FoldForestFusion.foldForestFusion()]] followed by [[FoldGroupFusion.foldGroupFusion]]. */
    lazy val foldFusion: u.Tree => u.Tree =
      FoldForestFusion.foldForestFusion andThen FoldGroupFusion.foldGroupFusion

    /** Delegates to [[ComprehensionCompiler.apply()]]. */
    def comprehend[C, R](r: Reified[C, R])(c: C): R = ComprehensionCompiler(r)(c)
  }
}
