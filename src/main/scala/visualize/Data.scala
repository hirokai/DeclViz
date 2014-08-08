package visualize

import visualize.TypeDefs.{Array2D, Group2D}
import scala.reflect.ClassTag
import scala.Array
import org.scalajs.dom

// Generate elem from data point
abstract class Translate[A, B <: Elem] {
  def apply(dat: A): B
}

abstract class Elem extends Reify {
  val xmlns = "http://www.w3.org/2000/svg"
  def mkElem(n: String) = dom.document.createElementNS(xmlns,n)
}

case class Rect(x: Coord, y: Coord, width: Coord, height: Coord, fill: String) extends Elem {
  import dom.document
  def reify = {
    val rect = mkElem("rect")
    rect.setAttribute("x",x.v.toString)
    rect.setAttribute("y",y.v.toString)
    rect.setAttribute("width",width.v.toString)
    rect.setAttribute("height",height.v.toString)
    rect.setAttribute("style",s"fill: $fill")
    rect
  }
}

case class RectGen[A](x: A => Coord, y: A => Coord, w: A => Coord, h: A => Coord, fill: A => String) extends Translate[A, Rect] {
  def apply(v: A) = {
    Rect(x(v),y(v),w(v),h(v),fill(v))
  }
}

case class RectAndTextGen[A](x: A => Coord, y: A => Coord, w: A => Coord, h: A => Coord, fill: A => String, text: A => String) extends Translate[A, ArrayElem[Elem]] {
  def apply(v: A) = {
    ArrayElem(Rect(x(v),y(v),w(v),h(v),fill(v)),Text(text(v)))
  }
}

case class Group[A <: Elem](children: Array[A], props: Array[(String,String)]) extends Elem {
  def reify = {
    val g = mkElem("g")
    children.map(e => g.appendChild(e.reify))
    g
  }
}


// Group a = f a
// translate :: (Translate a) => a -> b
// fmap translate :: f a -> f b (Group a -> Group b)
// RectGen.apply :: a -> b

case class ArrayElem[A <: Elem](elems: A*) extends Elem {
  def reify = {  //FIXME: This should be not g.
    val g = mkElem("g")
    elems.map(e => g.appendChild(e.reify))
    g
  }
}

case class GroupGen[A:  ClassTag](props: Array[(String,  String)] = Array()) {
  def fmap(tr: Translate[A,Elem])(arr: Array[A]): Group[Elem] = {
    Group({
      val a: Array[Elem] = arr.map(tr(_))
      a
    },props)
  }
}

package object TypeDefs {
  type Group2D[A <: Elem] = Group[Group[A]]
  type Array2D[A] = Array[Array[A]]
}
case class Group2DGen[A,B <: Elem]() {
  def fmap(tr: Translate[A,B])(matrix: Array[Array[A]])(implicit ev: ClassTag[B]): Group2D[B] = {
    Group(matrix.map(vs => Group[B]({
      val es: Array[B] = vs.map(tr(_)).toArray
      es
    },Array())),Array())
  }
}

case class Coord(_v: Double){
  def v = _v
}

object Scale {
  type Const[A] = A => A
  type RealScaling = Double => Coord
  type Tup2[A] = (A,A)
  val const: Double => Any => Coord = v => _ => Coord(v)
  val const2: Any => Any => Any = v => _ => v
  val linear: (Tup2[Double], Tup2[Coord]) => RealScaling
    = (range: (Double,Double), dest: (Coord,Coord)) => {v =>
    val (x1,x2) = range
    val (Coord(y1),Coord(y2)) = dest
    Coord((v-x1)*(y2-y1)/(x2-x1))
  }
  def linear(x1: Double,x2: Double,y1: Double,y2:Double): RealScaling = Scale.linear((x1,x2),(Coord(y1),Coord(y2)))
}

trait Reify {
  import org.scalajs.dom
  def reify: dom.Node
}

object TestData {
  import Scale.linear


  val funcx: ((Int,Int)) => Coord = {case (i,j) => Scale.linear(0,20,100,300)(i.toDouble)}
  val funcy: ((Int,Int)) => Coord = {case (i,j) => Scale.linear(0,20,100,300)(j.toDouble)}
  val colorFunc: ((Int,Int)) => String = {case (i,j) => if ((i+j)%2==0) "pink" else "lightblue"}

  val gen: Translate[(Int,Int),Rect] = RectGen[(Int,Int)](funcx, funcy, Scale.const(8), Scale.const(8), colorFunc)
  val dat: Array2D[(Int,Int)] = Array.tabulate(10,10){(i,j) => (i*3,j*2)}

  //Instantiate with generator and data.
  val test2: Group2D[Rect] = Group2DGen[(Int,Int),Rect]().fmap(gen)(dat)

  val test: Rect =  RectGen[(Int,Int)](funcx, funcy, Scale.const(70), Scale.const(100), colorFunc)((1,2))
  val textfunc: ((Int,Int)) => String = (a: (Int,Int)) => s"${a._1},${a._2}"
  val gen3: Translate[(Int,Int),ArrayElem[Elem]] = RectAndTextGen(funcx, funcy, Scale.const(8), Scale.const(8), colorFunc, textfunc)
  val test3: Group2D[ArrayElem[Elem]] = Group2DGen[(Int,Int),ArrayElem[Elem]]().fmap(gen3)(dat)

}

case class Text(t: String) extends Elem {
  def reify = {
    val text = mkElem("text")
    text.innerHTML = t
    text
  }
}

object Main {
  def main() {
    println(TestData.test3.reify)
  }
}