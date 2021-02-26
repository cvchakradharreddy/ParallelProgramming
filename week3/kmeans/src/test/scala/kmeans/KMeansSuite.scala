package kmeans

import java.util.concurrent._
import scala.collection.{mutable, Map, Seq}
import scala.collection.parallel.{ParMap, ParSeq}
import scala.collection.parallel.CollectionConverters._
import scala.math._
import org.junit._
import org.junit.Assert.assertEquals

class KMeansSuite {
  object KM extends KMeans
  import KM._

  def checkClassify(points: Seq[Point], means: Seq[Point], expected: Map[Point, Seq[Point]]): Unit =
    assertEquals(expected, classify(points, means))

  @Test def `'classify should work for empty 'points' and empty 'means'`: Unit = {
    val points: Seq[Point] = IndexedSeq()
    val means: Seq[Point] = IndexedSeq()
    val expected = Map[Point, Seq[Point]]()
    checkClassify(points, means, expected)
  }

  @Test def `'classify' should work for empty 'points' and 'means' == Seq(Point(1,1,1))`: Unit = {
    val points: Seq[Point] = IndexedSeq()
    val mean = new Point(1, 1, 1)
    val means: Seq[Point] = IndexedSeq(mean)
    val expected = Map[Point, Seq[Point]]((mean, Seq()))
    checkClassify(points, means, expected)
  }

  @Test def `'classify' should work for 'points' == Seq((1, 1, 0), (1, -1, 0), (-1, 1, 0), (-1, -1, 0)) and 'means' == Seq((0, 0, 0))`: Unit = {
    val p1 = new Point(1, 1, 0)
    val p2 = new Point(1, -1, 0)
    val p3 = new Point(-1, 1, 0)
    val p4 = new Point(-1, -1, 0)
    val points: Seq[Point] = IndexedSeq(p1, p2, p3, p4)
    val mean = new Point(0, 0, 0)
    val means: Seq[Point] = IndexedSeq(mean)
    val expected = Map((mean, Seq(p1, p2, p3, p4)))
    checkClassify(points, means, expected)
  }

  @Test def `'classify' should work for 'points' == Seq((1, 1, 0), (1, -1, 0), (-1, 1, 0), (-1, -1, 0)) and 'means' == Seq((1, 0, 0), (-1, 0, 0))`: Unit = {
    val p1 = new Point(1, 1, 0)
    val p2 = new Point(1, -1, 0)
    val p3 = new Point(-1, 1, 0)
    val p4 = new Point(-1, -1, 0)
    val points: Seq[Point] = IndexedSeq(p1, p2, p3, p4)
    val mean1 = new Point(1, 0, 0)
    val mean2 = new Point(-1, 0, 0)
    val means: Seq[Point] = IndexedSeq(mean1, mean2)
    val expected = Map((mean1, Seq(p1, p2)), (mean2, Seq(p3, p4)))
    checkClassify(points, means, expected)
  }

  def checkParClassify(points: ParSeq[Point], means: ParSeq[Point], expected: ParMap[Point, ParSeq[Point]]): Unit = {
    assertEquals(s"classify($points, $means) should equal to $expected", expected, classify(points, means))
  }

  @Test def `'classify' with data parallelism should work for empty 'points' and empty 'means'`: Unit = {
    val points: ParSeq[Point] = IndexedSeq().par
    val means: ParSeq[Point] = IndexedSeq().par
    val expected = ParMap[Point, ParSeq[Point]]()
    checkParClassify(points, means, expected)
  }

}


