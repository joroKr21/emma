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
package compiler.lang.comprehension

import api._
import api.backend.LocalOps._
import compiler.BaseCompilerSpec
import compiler.ir.ComprehensionSyntax._
import test.schema.Marketing._

import shapeless.::

/** A spec for comprehension combination. */
class CombinationSpec extends BaseCompilerSpec {

  import compiler._
  import Combination._
  import universe.reify

  // ---------------------------------------------------------------------------
  // Transformation pipelines
  // ---------------------------------------------------------------------------

  val liftPipeline: u.Expr[Any] => u.Tree =
    pipeline(typeCheck = true)(
      Core.lnf
    ).compose(_.tree)

  val combine: u.Expr[Any] => u.Tree =
    pipeline(typeCheck = true)(
      Core.lnf,
      tree => time(Comprehension.combine(tree), "combine"),
      Core.unnest
    ).compose(_.tree)

  def applyOnce(rule: (u.Symbol, u.Tree) => Option[u.Tree]): u.Expr[Any] => u.Tree = {
    val transform = api.TopDown.withOwner().transformWith {
      case Attr.inh(tree, owner :: _) => rule(owner, tree).getOrElse(tree)
    }.andThen(_.tree)

    pipeline(typeCheck = true)(
      Core.lnf,
      tree => time(transform(tree), "match rule"),
      Core.unnest
    ).compose(_.tree)
  }

  // ---------------------------------------------------------------------------
  // Spec tests
  // ---------------------------------------------------------------------------

  "match once " - {
    "filter" in {
      val inp = reify(comprehension[User, DataBag] {
        val u = generator[User, DataBag] {
          users withFilter { _.id > 0 }
        }
        guard { u.id < 11 }
        head { u }
      })

      val exp = reify(comprehension[User, DataBag] {
        val u = generator[User, DataBag] {
          val filtered$1 = users withFilter { _.id > 0 }
          filtered$1 withFilter { _.id < 11 }
        }
        head { u }
      })

      applyOnce(MatchFilter)(inp) shouldBe alphaEqTo(liftPipeline(exp))
    }

    "flatMap" in {
      val inp = reify(comprehension[Long, DataBag] {
        val x = generator[User, DataBag] { users }
        val y = generator[Long, DataBag] {
          DataBag(Seq(x.id, x.id))
        }
        head { y }
      })

      val exp = reify(comprehension[Long, DataBag] {
        val y = generator[Long, DataBag] {
          users flatMap { x => DataBag(Seq(x.id, x.id)) }
        }
        head { y }
      })

      applyOnce(MatchFlatMap)(inp) shouldBe alphaEqTo(liftPipeline(exp))
    }

    "flatMap2" in {
      val inp = reify(comprehension[(User, Long, User), DataBag] {
        val x = generator[User, DataBag] { users }
        val y = generator[Long, DataBag] {
          DataBag(Seq(x.id, x.id))
        }
        guard { x.id != y }
        val z = generator[User, DataBag] {
          DataBag(Seq(x))
        }
        head { (x, y, z) }
      })

      val exp = reify(comprehension[(User, Long, User), DataBag] {
        val xy = generator[(User, Long), DataBag] {
          users flatMap { x =>
            DataBag(Seq(x.id, x.id)) map { (x, _) }
          }
        }
        guard {
          val x = xy._1
          val y = xy._2
          x.id != y
        }
        val z = generator[User, DataBag] {
          val x = xy._1
          DataBag(Seq(x))
        }
        head {
          val x = xy._1
          val y = xy._2
          (x, y, z)
        }
      })

      applyOnce(MatchFlatMap2)(inp) shouldBe alphaEqTo(liftPipeline(exp))
    }

    "cross (simple)" in {
      val inp = reify(comprehension[(User, User), DataBag] {
        val x = generator[User, DataBag] { users }
        val y = generator[User, DataBag] { users }
        head { (x, y) }
      })

      val exp = reify(comprehension[(User, User), DataBag] {
        val xy = generator[(User, User), DataBag] {
          cross(users, users)
        }
        head {
          val x = xy._1
          val y = xy._2
          (x, y)
        }
      })

      applyOnce(MatchCross)(inp) shouldBe alphaEqTo(liftPipeline(exp))
    }

    "cross (complex)" in {
      val inp = reify(comprehension[String, DataBag] {
        val u = generator[DataBag[User], DataBag] {
          DataBag(Seq(users, users))
        }
        val x = generator[User, DataBag] { u }
        val y = generator[Int, DataBag] {
          DataBag(Seq(1,2,3)) union DataBag(Seq(4,5,6))
        }
        val z = generator[Long, DataBag] {
          DataBag(Seq(x.id + x.id))
        }
        guard { y != 0 }
        head { (x.id + y + z).toString }
      })

      val exp = reify(comprehension[String, DataBag] {
        val u = generator[DataBag[User], DataBag] {
          DataBag(Seq(users, users))
        }
        val xy = generator[(User, Int), DataBag] {
          val union$1 = DataBag(Seq(1,2,3)) union DataBag(Seq(4,5,6))
          cross(u, union$1)
        }
        val z = generator[Long, DataBag] {
          val x = xy._1
          DataBag(Seq(x.id + x.id))
        }
        guard { xy._2 != 0 }
        head {
          val x = xy._1
          val y = xy._2
          (x.id + y + z).toString
        }
      })

      applyOnce(MatchCross)(inp) shouldBe alphaEqTo(liftPipeline(exp))
    }

    "equiJoin (simple)" in {
      val inp = reify(comprehension[(User, User), DataBag] {
        val x = generator[User, DataBag] { users }
        val y = generator[User, DataBag] { users }
        guard { x.id == y.id }
        head { (x, y) }
      })

      val exp = reify(comprehension[(User, User), DataBag] {
        val xy = generator[(User, User), DataBag] {
          val users$1: test.schema.Marketing.users.type = users
          val users$2: test.schema.Marketing.users.type = users
          val kx = (x: User) => x.id
          val ky = (y: User) => y.id
          equiJoin(kx, ky)(users$1, users$2)
        }
        head {
          val x = xy._1
          val y = xy._2
          (x, y)
        }
      })

      applyOnce(MatchEquiJoin)(inp) shouldBe alphaEqTo(liftPipeline(exp))
    }

    "equiJoin (reversed)" in {
      val inp = reify(comprehension[(User, User), DataBag] {
        val x = generator[User, DataBag] { users }
        val y = generator[User, DataBag] { users }
        // lhs of `==` refers to the second generator, rhs refers to the first
        guard { y.id == x.id }
        head { (x, y) }
      })

      val exp = reify(comprehension[(User, User), DataBag] {
        val xy = generator[(User, User), DataBag] {
          val users$1: test.schema.Marketing.users.type = users
          val users$2: test.schema.Marketing.users.type = users
          val kx = (x: User) => x.id
          val ky = (y: User) => y.id
          equiJoin(kx, ky)(users$1, users$2)
        }
        head {
          val x = xy._1
          val y = xy._2
          (x, y)
        }
      })

      applyOnce(MatchEquiJoin)(inp) shouldBe alphaEqTo(liftPipeline(exp))
    }

    "equiJoin (not applicable)" in {
      val inp = reify(comprehension[(User, User), DataBag] {
        val x = generator[User, DataBag] { users }
        val y = generator[User, DataBag] { users }
        // the lhs of `==` refers to both x and y, so this is not an equiJoin
        guard { x.id + y.id == y.id }
        head { (x, y) }
      })

      applyOnce(MatchEquiJoin)(inp) shouldBe alphaEqTo(liftPipeline(inp))
    }

    "equiJoin (complex)" in {
      val inp = reify(comprehension[Short, DataBag] {
        val w = generator[Int, DataBag] {
          DataBag(Seq(8))
        }
        val x = generator[Int, DataBag] {
          DataBag(Seq(1,2,3)) union DataBag(Seq(4,5,6))
        }
        val y = generator[User, DataBag] {
          users union users
        }
        guard { y.id * 37 == 27 + 2 * x }
        val z = generator[Long, DataBag] {
          DataBag(Seq(3L, 6L))
        }
        guard { z != x && z != y.id }
        head { (x + y.name.first.length - z * w).asInstanceOf[Short] }
      })

      val exp = reify(comprehension[Short, DataBag] {
        val w = generator[Int, DataBag] {
          DataBag[Int](Seq(8))
        }
        val xy = generator[(Int, User), DataBag] {
          val union$1 = DataBag(Seq(1,2,3)) union DataBag(Seq(4,5,6))
          val union$2 = users union users
          val kx = (x: Int) => (27 + 2 * x).asInstanceOf[Long]
          val ky = (y: User) => y.id * 37
          equiJoin[Int, User, Long](kx, ky)(union$1, union$2)
        }
        val z = generator[Long, DataBag] {
          DataBag(Seq(3L, 6L))
        }
        guard {
          val x = xy._1
          val y = xy._2
          z != x && z != y.id
        }
        head {
          val x = xy._1
          val y = xy._2
          (x + y.name.first.length - z * w).asInstanceOf[Short]
        }
      })

      applyOnce(MatchEquiJoin)(inp) shouldBe alphaEqTo(liftPipeline(exp))
    }

    "residual" in {
      val inp = reify(comprehension[User, DataBag] {
        val x = generator[User, DataBag] { users }
        head { x }
      })

      val exp = reify(users map { x => x })
      val lifted = liftPipeline(exp)
      applyOnce(MatchResidual)(inp) shouldBe alphaEqTo(lifted)
      combine(inp) shouldBe alphaEqTo(lifted)
    }
  }

  "combine" - {
    "three-way cross" in {
      val inp = reify(comprehension[(User, User, Int), DataBag] {
        val x = generator[User, DataBag] { users.distinct }
        val y = generator[User, DataBag] { users union users }
        val z = generator[Int, DataBag] {
          DataBag(Seq(1,2,3))
        }
        head { (x, y, z) }
      })

      val exp = reify {
        cross(
          cross(
            users.distinct,
            users union users),
          DataBag(Seq(1, 2, 3))
        ) map { xy =>
          val x = xy._1
          val y = xy._2
          val x1 = x._1
          val y1 = x._2
          (x1, y1, y)
        }
      }

      combine(inp) shouldBe alphaEqTo(liftPipeline(exp))
    }

    "cross then filter" in {
      val inp = reify(comprehension[(User, User), DataBag] {
        val x = generator[User, DataBag] { users }
        val y = generator[User, DataBag] { users }
        guard { x.id != y.id }
        head { (x, y) }
      })

      val exp = reify {
        cross(users, users) withFilter { xy =>
          val x = xy._1
          val y = xy._2
          x.id != y.id
        } map { xy =>
          val x = xy._1
          val y = xy._2
          (x, y)
        }
      }

      combine(inp) shouldBe alphaEqTo(liftPipeline(exp))
    }

    "filter twice then cross" in {
      val inp = reify(comprehension[(User, User), DataBag] {
        val x = generator[User, DataBag] { users }
        val y = generator[User, DataBag] { users }
        guard { x.id != 3 }
        guard { y.id != 5 }
        head { (x, y) }
      })

      val exp = reify {
        cross(
          users withFilter { _.id != 3 },
          users withFilter { _.id != 5 }
        ) map { xy =>
          val x = xy._1
          val y = xy._2
          (x, y)
        }
      }

      combine(inp) shouldBe alphaEqTo(liftPipeline(exp))
    }
  }

  "combine nested" - {
    "in head" in {
      val inp = reify(comprehension[DataBag[User], DataBag] {
        val x = generator[DataBag[User], DataBag] {
          DataBag(Seq(users,users))
        }
        head {
          comprehension[User, DataBag] {
            val y = generator[User, DataBag] { x }
            head { y }
          }
        }
      })

      val exp = reify {
        val nested$1 = DataBag(Seq(users, users))
        nested$1 map { _ map { x => x } }
      }

      combine(inp) shouldBe alphaEqTo(liftPipeline(exp))
    }

    "complex" in {
      val inp = reify(comprehension[DataBag[User], DataBag] {
        val x = generator[DataBag[User], DataBag] {
          DataBag(Seq(users,users))
        }
        val y = generator[Long, DataBag] {
          val nested = comprehension[Long, DataBag] {
            val w = generator[User, DataBag] { x union x }
            head { w.id }
          }
          nested
        }
        head {
          comprehension[User, DataBag] {
            val z = generator[User, DataBag] { x }
            guard { z.id != y }
            head { z }
          }
        }
      })

      val exp = reify {
        val nested$1 = DataBag(Seq(users, users))
        val fmapped$1 = nested$1 flatMap { xs$1 =>
          val union$1 = xs$1 union xs$1
          val mapped$1 = union$1 map { _.id }
          mapped$1 map { (xs$1, _) }
        }
        fmapped$1 map { xy =>
          val x = xy._1
          val y = xy._2
          val filtered$1 = x withFilter { _.id != y }
          filtered$1 map { x => x }
        }
      }

      combine(inp) shouldBe alphaEqTo(liftPipeline(exp))
    }
  }

  "match once with control flow" - {
    "filter" in {
      val inp = reify(comprehension[User, DataBag] {
        val u = generator[User, DataBag] {
          var us = users
          while (us.size < 100) us = us union us
          us
        }
        guard { u.id < 11 }
        head { u }
      })

      val exp = reify(comprehension[User, DataBag] {
        val u = generator[User, DataBag] {
          var us = users
          while (us.size < 100) us = us union us
          us.withFilter(_.id < 11)
        }
        head { u }
      })

      applyOnce(MatchFilter)(inp) shouldBe alphaEqTo(liftPipeline(exp))
    }

    "flatMap" in {
      val inp = reify(comprehension[Long, DataBag] {
        val x = generator[User, DataBag] {
          var us = users
          while (us.size < 100) us = us union us
          us
        }
        val y = generator[Long, DataBag] {
          DataBag(Seq(x.id, x.id))
        }
        head { y }
      })

      val exp = reify(comprehension[Long, DataBag] {
        val y = generator[Long, DataBag] {
          var us = users
          while (us.size < 100) us = us union us
          us flatMap { x => DataBag(Seq(x.id, x.id)) }
        }
        head { y }
      })

      applyOnce(MatchFlatMap)(inp) shouldBe alphaEqTo(liftPipeline(exp))
    }

    "flatMap2" in {
      val inp = reify(comprehension[(User, Long, User), DataBag] {
        val x = generator[User, DataBag] {
          var us = users
          while (us.size < 100) us = us union us
          us
        }
        val y = generator[Long, DataBag] {
          DataBag(Seq(x.id, x.id))
        }
        guard { x.id != y }
        val z = generator[User, DataBag] {
          DataBag(Seq(x))
        }
        head { (x, y, z) }
      })

      val exp = reify(comprehension[(User, Long, User), DataBag] {
        val xy = generator[(User, Long), DataBag] {
          var us = users
          while (us.size < 100) us = us union us
          us flatMap { x =>
            DataBag(Seq(x.id, x.id)) map { (x, _) }
          }
        }
        guard {
          val x = xy._1
          val y = xy._2
          x.id != y
        }
        val z = generator[User, DataBag] {
          val x = xy._1
          DataBag(Seq(x))
        }
        head {
          val x = xy._1
          val y = xy._2
          (x, y, z)
        }
      })

      applyOnce(MatchFlatMap2)(inp) shouldBe alphaEqTo(liftPipeline(exp))
    }

    "cross" in {
      val inp = reify(comprehension[(User, User), DataBag] {
        val x = generator[User, DataBag] {
          var us1 = users
          while (us1.size < 100) us1 = us1 union us1
          us1
        }
        val y = generator[User, DataBag] {
          var us2 = users
          do us2 = us2 union us2 while (us2.size < 100)
          us2
        }
        head { (x, y) }
      })

      val exp = reify(comprehension[(User, User), DataBag] {
        val xy = generator[(User, User), DataBag] {
          var us1 = users
          while (us1.size < 100) us1 = us1 union us1
          var us2 = users
          do us2 = us2 union us2 while (us2.size < 100)
          cross(us1, us2)
        }
        head {
          val x = xy._1
          val y = xy._2
          (x, y)
        }
      })

      applyOnce(MatchCross)(inp) shouldBe alphaEqTo(liftPipeline(exp))
    }

    "equiJoin" in {
      val inp = reify(comprehension[(User, User), DataBag] {
        val x = generator[User, DataBag] {
          var us1 = users
          while (us1.size < 100) us1 = us1 union us1
          us1
        }
        val y = generator[User, DataBag] {
          var us2 = users
          do us2 = us2 union us2 while (us2.size < 100)
          us2
        }
        guard { x.id == y.id }
        head { (x, y) }
      })

      val exp = reify(comprehension[(User, User), DataBag] {
        val xy = generator[(User, User), DataBag] {
          var us1 = users
          while (us1.size < 100) us1 = us1 union us1
          var us2 = users
          do us2 = us2 union us2 while (us2.size < 100)
          val kx = (x: User) => x.id
          val ky = (y: User) => y.id
          equiJoin(kx, ky)(us1, us2)
        }
        head {
          val x = xy._1
          val y = xy._2
          (x, y)
        }
      })

      applyOnce(MatchEquiJoin)(inp) shouldBe alphaEqTo(liftPipeline(exp))
    }

    "residual" in {
      val inp = reify(comprehension[User, DataBag] {
        val x = generator[User, DataBag] {
          var us = users
          while (us.size < 100) us = us union us
          us
        }
        head { x }
      })

      val exp = reify {
        var us = users
        while (us.size < 100) us = us union us
        us map { x => x }
      }

      val lifted = liftPipeline(exp)
      applyOnce(MatchResidual)(inp) shouldBe alphaEqTo(lifted)
      combine(inp) shouldBe alphaEqTo(lifted)
    }
  }
}
