package hierarchical.parse

import hierarchical._

import scala.xml.{Node, Elem}

object XML extends Parser {
  override def parse(content: String): Value = {
    def toJson(node: Node): Option[Value] = node match {
      case elem: Elem => {
        val attributes: List[(String, Value)] = elem.attributes.map(md => toJson(md.value.head).map(md.key -> _)).toList.flatten
        val children = elem.child.toList.collect {
          case child: Elem => toJson(child).map(child.label -> _)
        }.flatten
        val text = elem.text.trim
        if (attributes.isEmpty && children.isEmpty) {
          if (text.isEmpty) {
            None
          } else {
            Some(str(text))
          }
        } else {
          Some(obj(attributes ::: children: _*))
        }
      }
      case _ => None
    }

    val root = scala.xml.XML.loadString(content)
    obj(root.label -> toJson(root).getOrElse(Null))
  }
}
