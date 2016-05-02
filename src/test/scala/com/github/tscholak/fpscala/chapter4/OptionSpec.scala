package com.github.tscholak.fpscala.chapter4

import scala.{Nil => _, List => _, Some => _, None => _, Option => _, Either => _, _}
import org.scalatest.{Matchers, FlatSpec}
import com.github.tscholak.fpscala.chapter3._

class OptionSpec extends FlatSpec with Matchers {

  // exercise 4.2
  "An empty Sequence" should "have a variance of None" in {
    Option.variance(Seq.empty[Double]) should be(None)
  }
  "A Sequence of 1.0" should "have a variance of Some(0)" in {
    Option.variance(Seq(1.0)) should be(Some(0))
  }
  "A Sequence of -1.0,2.0,5.0" should "have a variance of Some(6.0)" in {
    Option.variance(Seq(-1.0,2.0,5.0)) should be(Some(6.0))
  }

  // exercise 4.4
  "A List of Some(1), Some(2)" should "become Some(List(1,2)) when combined" in {
    Option.sequence3(List(Some(1),Some(2))) should be(Some(List(1,2)))
  }
  "A List of Some(1), None" should "become None when combined" in {
    Option.sequence3(List(Some(1),None)) should be(None)
  }
  "A List of None, Some(2)" should "become None when combined" in {
    Option.sequence3(List(None,Some(2))) should be(None)
  }
  "Nil" should "become Some(Nil) when combined" in {
    Option.sequence3(Nil) should be(Some(Nil))
  }


}