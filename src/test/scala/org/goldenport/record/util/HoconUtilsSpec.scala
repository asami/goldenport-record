package org.goldenport.record.util

import java.net.URL
import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith
import org.scalatest._
import com.typesafe.config.{Config => Hocon, ConfigFactory}
import org.goldenport.RAISE
import org.goldenport.record.v3.Record

/*
 * @since   Sep.  7, 2022
 * @version Dec. 16, 2022
 * @author  ASAMI, Tomoharu
 */
@RunWith(classOf[JUnitRunner])
class HoconUtilsSpec extends WordSpec with Matchers {
  "toRecord" should {
    "toRecord" which {
      "toRecord" ignore {
        val s = """
organization={
  member=[{
    name="room1"
    title="水無月"
    resource_id="mycolorweb-mycolorweb-1654495057289-bz_resource-577fbd95-1483-4f29-a2a4-442134db05c9"
  },{
    name="room2"
    title="如月"
    resource_id="mycolorweb-mycolorweb-1654495068463-bz_resource-42401e8d-a79a-44b0-9596-00b0ace0dd2c"
  },{
    name="room3"
    title="弥生"
    resource_id="mycolorweb-mycolorweb-1654495076155-bz_resource-85dd10e0-543d-4dab-96cc-449057bb3b43"
  },{
    name="room4"
    title="卯月"
    resource_id="mycolorweb-mycolorweb-1654559064255-bz_resource-05515d85-f64c-4b8f-8d40-08e5ad362f94"
  },{
    name="room5"
    title="皐月"
    resource_id="mycolorweb-mycolorweb-1654559154850-bz_resource-35f94be5-d8e6-407f-a168-1216d3f603a1"
  }]
}"""
        val c = ConfigFactory.parseString(s)
        val r = HoconUtils.toRecord(c)
        val v = r.getRecord("organization")
        // println(s"r: $r")
        // println(s"v: $v")
        // v foreach {
        //   case m: Record =>
        //     val a = m.takeRecordList("member")
        //     println(s"m: $m")
        //     println(s"a: $a")
        //   case m => println(m)
        // }
        v.nonEmpty should be(true)
        v.get.takeRecordList("member").nonEmpty should be(true)
      }

      "tree" in {
        val s = """http.baseurl="http://localhost:9000/2.1/MyColorWeb"
http.header="Authorization: Bearer snkvnVcBKNCPVzVovwaTtwzQRgjdqvGS"
db.default.driver=com.mysql.jdbc.Driver
db.default.url="jdbc:mysql://develop-master.everforth.com/crossbird?zeroDateTimeBehavior=convertToNull&useUnicode=true&characterEncoding=utf8"
db.default.user="yujikosuga"
db.default.password="3qWb3r8wO56JVsL"
db.default.connectionTestStatement="SELECT 1"

appid="SDKSampleApp-API-4008"
accesstoken="JJnRJwHYUCbYhEQvnizArZDOqTsHhQcl"
productid=""
contactid=""
sessionid="aadd867516aff9877e6ff68c4021a4eff1868c4f20132"
userid="mycolorweb-mycolorweb-1647914068479-user-c7e420ad-cb74-4512-837e-cf032d1ff9e9"
payment.fee.rule {
  provider=[{
    name="np-postpay"
    interval="2022-09-01T00:00:00~"
    fee=187
    tax.kind=none
  },{
    name="np-postpay"
    fee=209
    tax.kind=none
  }]
}
"""
//         val s = """http.baseurl="http://localhost:9000/2.1/MyColorWeb"
// http.header="Authorization: Bearer snkvnVcBKNCPVzVovwaTtwzQRgjdqvGS"
// """
//         val s = """appid="SDKSampleApp-API-4008"
// """
        val c = ConfigFactory.parseString(s)
        val r = HoconUtils.toRecord(c)
        println(r.toJsonString)
        val c2 = c.getConfig("payment.fee.rule")
        val r2 = HoconUtils.toRecord(c2)
        println(r2.toJsonString)
      }
    }
  }
}
