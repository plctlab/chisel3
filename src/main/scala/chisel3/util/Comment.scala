package chisel3.util

import chisel3.experimental.{ChiselAnnotation, annotate}
import chisel3.internal.{InstanceId, NamedComponent}
import firrtl.{AttributeAnnotation, DocStringAnnotation}

/** An util to easily create [[DocStringAnnotation]] and [[AttributeAnnotation]] */
object Comment {
  implicit class AddDoc(x: InstanceId) {
    /** Add a [[DocStringAnnotation]] to Module or Component */
    def addDoc(doc: String): Unit = annotate(new ChiselAnnotation {
      override def toFirrtl = DocStringAnnotation(x.toTarget, doc)
    })
  }

  implicit class AddAttr(x: NamedComponent) {
    /** Add a [[AttributeAnnotation]] to Component */
    def addAttr(attr: String): Unit = annotate(new ChiselAnnotation {
      override def toFirrtl = AttributeAnnotation(x.toTarget, attr)
    })
  }
}
