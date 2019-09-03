/*
 * sbt IO
 *
 * Copyright 2011 - 2019, Lightbend, Inc.
 * Copyright 2008 - 2010, Mark Harrah
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 */

package sbt.nio.file

import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Prop._
import org.scalacheck._
import sbt.io.AllPassFilter

import scala.util.Try

object GlobOrderingSpecification extends Properties("GlobOrdering") {
  property("Glob.ordering consistency") = forAll { (globs: List[Glob]) =>
//    println(globs.size -> globs.headOption)
    val _ = globs.sorted
    true
  }

  implicit lazy val validPath: Arbitrary[java.nio.file.Path] = Arbitrary {
    implicit val chars = Arbitrary(Gen.oneOf(List('/', '.') ++ ('a' to 'z')))
    arbitrary[List[Char]]
      .filter(s => Try(new java.io.File(s.mkString).toPath).isSuccess)
      .map(s => new java.io.File(s.mkString).toPath)
  }

  implicit def relativeglobs: Arbitrary[RelativeGlob] = Arbitrary {
    for {
      path <- validPath.arbitrary
      recurse <- Gen.frequency[Option[RelativeGlob]](
        1 -> relativeglobs.arbitrary.map(Option.apply),
        1 -> Gen.const(None),
      )
      relativeGlob <- Gen.oneOf[RelativeGlob](
        RelativeGlob.NoPath,
        RelativeGlob.**,
        RelativeGlob.*,
      )
    } yield recurse.map(g => relativeGlob / g).getOrElse(relativeGlob / path.toString)
  }

  implicit def matcherglobs: Arbitrary[RelativeGlob.Matcher] = Arbitrary {
    for {
      path <- validPath.arbitrary
      left <- Gen.frequency(
        1 -> matcherglobs.arbitrary.map(Option.apply),
        10 -> Gen.const(None),
      )
      right <- Gen.frequency(
        1 -> matcherglobs.arbitrary.map(Option.apply),
        10 -> Gen.const(None),
      )
      glob <- (left, right) match {
        case (Some(left), Some(right)) =>
          Gen.oneOf(
            RelativeGlob.AndMatcher(left, right),
            RelativeGlob.OrMatcher(left, right),
          )
        case (Some(left), None)  => Gen.const(RelativeGlob.Matcher.not(left))
        case (None, Some(right)) => Gen.const(RelativeGlob.Matcher.not(right))
        case _ =>
          Gen.oneOf(
            AnyPath,
            RelativeGlob.NoPath,
          )
      }
    } yield glob
  }

  implicit def globs: Arbitrary[Glob] = Arbitrary {
    for {
      path <- validPath.arbitrary
      path2 <- validPath.arbitrary
      path3 <- validPath.arbitrary
      path4 <- validPath.arbitrary
      recursive <- Arbitrary.arbitrary[Boolean]
      relative <- Gen.frequency[Option[Glob]](
        1 -> globs.arbitrary.map(Option.apply),
        1 -> Gen.const(None),
      )
      relativeGlob <- relativeglobs.arbitrary
      matcher <- matcherglobs.arbitrary
      glob <- Gen.oneOf(
        Glob.FullFileGlob(path, recursive, AllPassFilter),
        AnyPath,
        Glob.Empty,
        Glob(path2),
        Glob.Pattern(path3, RelativeGlob("*")),
        relativeGlob,
        matcher,
      )
    } yield glob
  }
}
