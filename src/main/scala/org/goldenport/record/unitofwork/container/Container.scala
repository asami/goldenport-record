package org.goldenport.record.unitofwork.container

import org.goldenport.io.ResourceManager

/*
 * @since   Feb. 11, 2020
 * @version Feb. 11, 2020
 * @author  ASAMI, Tomoharu
 */
trait Container {
  def config: ConfigBase

  lazy val resourceManager = new ResourceManager()
}
