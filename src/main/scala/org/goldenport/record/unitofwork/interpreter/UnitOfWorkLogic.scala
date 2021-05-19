package org.goldenport.record.unitofwork.interpreter

import scalaz.{Store => _, _}, Scalaz._
import java.io.File
import java.net.URL
import java.net.URI
import java.nio.charset.Charset
import org.goldenport.RAISE
import org.goldenport.bag.{ChunkBag, BufferFileBag}
import org.goldenport.bag.ClobBag
import org.goldenport.io.IoUtils
import org.goldenport.record.v2._
import org.goldenport.record.http
import org.goldenport.record.unitofwork._
import org.goldenport.record.unitofwork.UnitOfWork._

/*
 * @since   Nov. 16, 2015
 *  version Dec.  2, 2015
 *  version Apr. 28, 2016
 *  version Sep. 17, 2018
 *  version Oct.  2, 2018
 *  version Jun. 24, 2019
 * @version Apr.  4, 2021
 * @author  ASAMI, Tomoharu
 */
trait UnitOfWorkLogic extends LogicBase {
  protected def http_Driver: http.Driver = http.StandardHttpDriver()
  lazy private val _http_driver = http_Driver

  def invokeService(req: ServiceRequest): ServiceResponse
  def httpService(req: http.Request): http.Response = _http_driver.invoke(req)
  def fileFetch(uri: URI): ChunkBag = RAISE.notImplementedYetDefect // TODO URN
  def fileFetch(url: URL): ChunkBag = BufferFileBag.fromUrl(url)
  def fileFetch(file: File): ChunkBag = BufferFileBag.fromFile(file)
  def fileFetch(name: String): ChunkBag = BufferFileBag.fromUri(name)
  def fileSave(uri: URI, o: String, charset: Charset): Unit = fileSave(uri.toURL, o, charset)
  def fileSave(uri: URI, o: ClobBag, charset: Charset): Unit = fileSave(uri.toURL, o, charset)
  def fileSave(uri: URI, o: ChunkBag): Unit = RAISE.notImplementedYetDefect // TODO URN
  def fileSave(url: URL, o: String, charset: Charset): Unit = RAISE.notImplementedYetDefect
  def fileSave(url: URL, o: ClobBag, charset: Charset): Unit = IoUtils.save(url, o, charset)
  def fileSave(url: URL, o: ChunkBag): Unit = RAISE.notImplementedYetDefect
  def fileSave(file: File, o: String, charset: Charset): Unit = RAISE.notImplementedYetDefect
  def fileSave(file: File, o: ClobBag, charset: Charset): Unit = IoUtils.save(file, o, charset)
  def fileSave(file: File, o: ChunkBag): Unit = RAISE.notImplementedYetDefect
  def fileSave(name: String, o: String, charset: Charset): Unit = RAISE.notImplementedYetDefect
  def fileSave(name: String, o: ClobBag, charset: Charset): Unit = RAISE.notImplementedYetDefect
  def fileSave(name: String, o: ChunkBag): Unit = RAISE.notImplementedYetDefect
  def executeShellCommand(cmd: String): String = RAISE.notImplementedYetDefect
  def executeShellCommand(cmd: Seq[String], env: Record, dir: File): String = RAISE.notImplementedYetDefect
  def extension[T](p: ExtensionUnitOfWork[_]): T =
    sys.error(s"Undefined ExtensionUnitOfWork: $p")
}

object UnitOfWorkLogic {
  val printer = new UnitOfWorkLogic {
    def invokeService(req: ServiceRequest): ServiceResponse = {
      println(s"invokeService: ")
      new ServiceResponse {}
    }

    def commit(): CommitResult = RAISE.noReachDefect
    def abort(message: String): Unit = RAISE.noReachDefect
    def abort(e: Throwable): Unit = RAISE.noReachDefect
  }
}
