package org.goldenport.record.v2.bag

import scalaz._, Scalaz._
import java.io.OutputStream
import java.nio.charset.Charset
import java.util.zip._
import org.goldenport.Platform
import org.goldenport.Strings
import org.goldenport.bag._

/*
 * @since   Dec. 18, 2014
 *  version Oct. 24, 2015
 *  version Jul. 25, 2017
 * @version Sep.  4, 2017
 * @author  ASAMI, Tomoharu
 */
class ZipBag(val tree: Tree[ZipBag.Node]) extends Bag {
  import ZipBag._

  override def filenameSuffix = Some("zip")
  override def mimetype: String = Strings.mimetype.application_zip
  def getChunkBag = None
  def createChunkBag: ChunkBag = {
    val buf = new BufferFileBag()
    for {
      out <- resource.managed(buf.openOutputStream())
      zout <- resource.managed(new ZipOutputStream(out))
      node <- tree.subForest
    } {
      _build(zout, node)
    }
    buf
  }

  private def _build(
    zout: ZipOutputStream,
    node: Tree[Node]
  ) {
    node.rootLabel match {
      case ContainerNode(name) =>
        node.subForest.foreach(_build(zout, _, name))
      case BagNode(name, bag) =>
        _add_entry(zout, name, bag)
      case RootNode => sys.error("no reach")
    }
  }

  private def _build(
    zout: ZipOutputStream,
    node: Tree[Node],
    parentpathname: String
  ) {
    node.rootLabel match {
      case ContainerNode(name) =>
        val pathname = parentpathname + "/" + name
        node.subForest.foreach(_build(zout, _, pathname))
      case BagNode(name, bag) =>
        val pathname = parentpathname + "/" + name
        _add_entry(zout, pathname, bag)
      case RootNode => sys.error("no reach")
    }
  }

  private def _add_entry(
    zout: ZipOutputStream,
    name: String,
    bag: Bag
  ) {
    val entry = new ZipEntry(name)
    zout.putNextEntry(entry)
    bag.copyTo(zout)
    zout.closeEntry()
  }
}

object ZipBag {
  sealed trait Node {
    def getName: Option[String]
    def isName(p: String) = getName.fold(false)(_ == p)
  }
  case class BagNode(name: String, bag: Bag) extends Node {
    def getName = Some(name)
  }
  case class ContainerNode(name: String) extends Node {
    def getName = Some(name)
  }
  case object RootNode extends Node {
    def getName = None
  }

  def fromCsvs(files: (String, CsvBag)*): ZipBag = {
    val bags: Seq[Tree[Node]] = files.map(x => 
      Tree.leaf[Node](BagNode(x._1, x._2.bag)))
    val tree = Tree.node(RootNode, bags.toStream)
    new ZipBag(tree)
  }

  def fromFilesAndImages(files: Seq[Bag], images: Seq[Bag]): ZipBag = {
    val a = files.map(x => Tree.leaf[Node](BagNode(x.filename, x)))
    val b = images.map(x => Tree.leaf[Node](BagNode(x.filename, x)))
    val c = Tree.node[Node](ContainerNode("images"), b.toStream)
    val d = (a :+ c).toStream
    new ZipBag(Tree.node(RootNode, d))
  }
}
