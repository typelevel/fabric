/*
 * Copyright (c) 2021 Typelevel
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of
 * this software and associated documentation files (the "Software"), to deal in
 * the Software without restriction, including without limitation the rights to
 * use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of
 * the Software, and to permit persons to whom the Software is furnished to do so,
 * subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
 * FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
 * COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
 * IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
 * CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

package fabric.rw

import fabric.*
import fabric.define.*

import scala.deriving.*
import scala.compiletime.*
import scala.quoted._

import scala.collection.immutable.VectorMap

trait CompileRW {
  inline def gen[T <: Product](using Mirror.ProductOf[T]): RW[T] = new ClassRW[T] {
    override protected def t2Map(t: T): Map[String, Json] = toMap(t)
    override protected def map2T(map: Map[String, Json]): T = fromMap[T](map)
    override def definition: DefType = toDefinition[T]
  }

  inline def genR[T <: Product](using Mirror.ProductOf[T]): Reader[T] = new ClassR[T] {
    override protected def t2Map(t: T): Map[String, Json] = toMap(t)
  }

  inline def genW[T <: Product](using Mirror.ProductOf[T]): Writer[T] = new ClassW[T] {
    override protected def map2T(map: Map[String, Json]): T = fromMap[T](map)
  }

  inline def toDefinition[T <: Product](using p: Mirror.ProductOf[T]): DefType = {
    DefType.Obj(toDefinitionElems[T, p.MirroredElemTypes, p.MirroredElemLabels](0))
  }

  inline def toDefinitionElems[A <: Product, T <: Tuple, L <: Tuple](index: Int): Map[String, DefType] = {
    inline erasedValue[T] match {
      case _: (hd *: tl) => {
        inline erasedValue[L] match {
          case _: (hdLabel *: tlLabels) =>
            val hdLabelValue = constValue[hdLabel].asInstanceOf[String]
            val rw = summonInline[RW[hd]]
            Map(hdLabelValue -> rw.definition) ++ toDefinitionElems[A, tl, tlLabels](index + 1)
          case EmptyTuple => sys.error("Not possible")
        }
      }
      case EmptyTuple => Map.empty
    }
  }

  inline def toMap[T <: Product](t: T)(using p: Mirror.ProductOf[T]): Map[String, Json] = {
    toMapElems[T, p.MirroredElemTypes, p.MirroredElemLabels](t, 0)
  }

  inline def toMapElems[A <: Product, T <: Tuple, L <: Tuple](a: A, index: Int): Map[String, Json] = {
    inline erasedValue[T] match {
      case _: (hd *: tl) => {
        inline erasedValue[L] match {
          case _: (hdLabel *: tlLabels) =>
            import fabric.rw._
            val hdLabelValue = constValue[hdLabel].asInstanceOf[String]
            val hdValue = a.productElement(index).asInstanceOf[hd]
            val hdReader = summonInline[Reader[hd]]
            val value = hdReader.read(hdValue)
            toMapElems[A, tl, tlLabels](a, index + 1) ++ VectorMap(hdLabelValue -> value)
          case EmptyTuple => sys.error("Not possible")
        }
      }
      case EmptyTuple => Map.empty
    }
  }

  inline def fromMap[T <: Product](map: Map[String, Json])(using p: Mirror.ProductOf[T]): T = {
    inline val size = constValue[Tuple.Size[p.MirroredElemTypes]]
    val defaults = getDefaultParams[T]
    val arr = new Array[Any](size)
    fromMapElems[T, p.MirroredElemTypes, p.MirroredElemLabels](map, 0, arr, defaults)
    val product: Product = new Product {
      override def canEqual(that: Any): Boolean = true
      override def productArity: Int = arr.size
      override def productElement(n: Int): Any = arr(n)
    }
    p.fromProduct(product)
  }

  inline def fromMapElems[A <: Product, T <: Tuple, L <: Tuple](map: Map[String, Json], index: Int, arr: Array[Any], defaults: Map[String, Any]): Unit = {
    inline erasedValue[T] match {
      case _: (hd *: tl) =>
        inline erasedValue[L] match {
          case _: (hdLabel *: tlLabels) =>
            import fabric.rw._
            val hdLabelValue = constValue[hdLabel].asInstanceOf[String]
            val hdValueOption = map.get(hdLabelValue)
            val hdWritable = summonInline[Writer[hd]]
            val valueOption = hdValueOption.map(hdWritable.write)
            def defaultAlternative = inline erasedValue[hd] match {
              case _: Option[optHd] => None
              case _ => sys.error(s"Unable to find field ${getClassName[A]}.$hdLabelValue (and no defaults set) in ${Obj(map)}")
            }
            def default = defaults.getOrElse(hdLabelValue, defaultAlternative)
            val value = valueOption.getOrElse(default)
            arr(index) = value
            fromMapElems[A, tl, tlLabels](map, index + 1, arr, defaults)
          case EmptyTuple => sys.error("Not possible")
      }
      case EmptyTuple => // Finished
    }
  }

  inline def getDefaultParams[T]: Map[String, AnyRef] = ${ CompileRW.getDefaultParmasImpl[T] }

  inline def getClassName[T]: String = ${ CompileRW.getClassNameImpl[T] }
}

object CompileRW {
  def getDefaultParmasImpl[T](using Quotes, Type[T]): Expr[Map[String, AnyRef]] = {
    import quotes.reflect._
    val sym = TypeTree.of[T].symbol

    if (sym.isClassDef) {
      val comp = if (sym.isClassDef) sym.companionClass else sym
      val names =
        for p <- sym.caseFields if p.flags.is(Flags.HasDefault)
        yield p.name
      val namesExpr: Expr[List[String]] = Expr.ofList(names.map(Expr(_)))

      val body = comp.tree.asInstanceOf[ClassDef].body
      val idents: List[Ref] =
        for case deff @ DefDef(name, _, _, _) <- body
        if name.startsWith("$lessinit$greater$default")
        yield Ref(deff.symbol)
      val identsExpr: Expr[List[Any]] =
        Expr.ofList(idents.map(_.asExpr))

      '{ $namesExpr.zip($identsExpr.map(_.asInstanceOf[AnyRef])).toMap }
    } else {
      '{ Map.empty }
    }
  }

  def getClassNameImpl[T](using Quotes, Type[T]): Expr[String] = {
    import quotes.reflect._

    Expr(TypeTree.of[T].symbol.companionClass.fullName)
  }
}