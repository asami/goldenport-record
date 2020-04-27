package org.goldenport.record.unitofwork.container

import org.goldenport.cli.{Config => CliConfig}
import org.goldenport.log.LogLevel
import org.goldenport.record.unitofwork.interpreter._
import org.goldenport.record.query.QueryExpression
import org.goldenport.util.DateTimeUtils

/*
 * @since   Feb. 11, 2020
 * @version Feb. 11, 2020
 * @author  ASAMI, Tomoharu
 */
trait ConfigBase {
  def cliConfig: CliConfig
  def serviceLogic: UnitOfWorkLogic
  def storeLogic: StoreOperationLogic

  // TODO storeOperationLogic
  // lazy val sqlContext =
  //   if (true) // TODO configurable
  //     SqlContext.createEachTime(properties, createQueryContext())
  //   else if (false)
  //     SqlContext.createAutoCommit(properties, createQueryContext())
  //   else
  //     SqlContext.createConnectionPool(properties, createQueryContext())

  def properties = cliConfig.properties
  def i18nContext = cliConfig.i18n
  def createQueryContext() = QueryExpression.Context(
    DateTimeUtils.toDateTime(System.currentTimeMillis, i18nContext.datetimezone),
    i18nContext.datetimezone
  )
  def logConfig = cliConfig.log
  def charset = cliConfig.charset
  def newline = cliConfig.newline
  def homeDirectory = cliConfig.homeDirectory
  def getProjectDirectory = cliConfig.projectDirectory
  def workDirectory = cliConfig.workDirectory
  def logLevel: LogLevel = logConfig.level getOrElse LogLevel.Info
}
