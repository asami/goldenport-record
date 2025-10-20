package org.goldenport.record.unitofwork.container

import org.goldenport.cli.{Config => CliConfig}
import org.goldenport.log.LogLevel
import org.goldenport.record.unitofwork.interpreter._

/*
 * @since   Feb. 11, 2020
 *  version Feb. 11, 2020
 * @version Sep. 12, 2025
 * @author  ASAMI, Tomoharu
 */
case class Config(
  cliConfig: CliConfig,
  serviceLogic: UnitOfWorkLogic,
  storeLogic: StoreOperationLogic
) extends ConfigBase {
  def withLogLevel(p: LogLevel) = copy(cliConfig.withLogLevel(p))
  def withServiceLogic(p: UnitOfWorkLogic) = copy(serviceLogic = p)
  def withStoreLogic(p: StoreOperationLogic) = copy(storeLogic = p)
}

object Config {
  private val _default_context = UnitOfWorkLogic.ExecutionContext.default

  val defaultServiceLogic = new StandardUnitOfWorkLogic(_default_context)
  val defaultStoreLogic = new StandardStoreOperationLogic()
  val default = Config(CliConfig.c, defaultServiceLogic, defaultStoreLogic)

  object log {
    val error = default.withLogLevel(LogLevel.Error)
    val warn = default.withLogLevel(LogLevel.Warn)
    val info = default.withLogLevel(LogLevel.Info)
    val debug = default.withLogLevel(LogLevel.Debug)
    val trace = default.withLogLevel(LogLevel.Trace)
  }
}
