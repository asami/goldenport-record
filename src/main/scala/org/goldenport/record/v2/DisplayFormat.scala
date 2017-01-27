package org.goldenport.record.v2

/*
 * @since   Oct. 25, 2015
 * @version Jan. 24, 2017
 * @author  ASAMI, Tomoharu
 */
case class DisplayFormat(
  orderBy: Option[SqlOrder],
  width: Option[DisplayFormat.Length],
  header: DisplayFormat.Format,
  body: DisplayFormat.Format,
  sequence: Option[Int],
  formatter: Option[Formatter]
) {
  def withFormatter(p: Formatter) = copy(formatter = Some(p))
}

object DisplayFormat {
  val empty = DisplayFormat(None, None, Format.empty, Format.empty, None, None)

  def HFBBFB(hf: Int, hb: Int, bf: Int, bb: Int) =
    empty.copy(header = Format.FB(hf, hb), body = Format.FB(bf, bb))

  def HBBB(hb: Int, bb: Int) =
    empty.copy(header = Format.B(hb), body = Format.B(bb))

  case class Format(
    font: Option[Font],
    align: Option[Align],
    vertical: Option[Vertical],
    foregroudColorRgb: Option[Int],
    backgroundColorRgb: Option[Int],
    height: Option[Length]
  )

  object Format {
    val empty = Format(None, None, None, None, None, None)

    def B(bg: Int) =
      Format(None, None, None, None, Some(bg), None)

    def FB(fg: Int, bg: Int) =
      Format(None, None, None, Some(fg), Some(fg), None)
  }

  sealed trait Align extends Powertype
  case object GeneralAlign extends Align { // (initial), ALIGN_GENERAL
    val name = "general"
    val value = 0x0
  }
  case object LeftAlign extends Align { // left, ALIGN_LEFT
    val name = "left"
    val value = 0x1
  }
  case object CenterAlign extends Align { // center, ALIGN_CENTER
    val name = "center"
    val value = 0x2
  }
  case object RightAlign extends Align { // right, ALIGN_RIGHT
    val name = "right"
    val value = 0x3
  }
  case object FillAlign extends Align { // ALIGN_FILL
    val name = "fill"
    val value = 0x4
  }
  case object JustifyAlign extends Align { // justify, ALIGN_JUSTIFY
    val name = "justtify"
    val value = 0x5
  }
  case object CenterSelectionAlign extends Align { // ALIGN_CENTER_SELECTION
    val name = "center-selection"
    val value = 0x6
  }
  object Align extends PowertypeClass {
    type T = Align
    val elements = Vector(
      GeneralAlign,
      LeftAlign,
      CenterAlign,
      RightAlign,
      FillAlign,
      JustifyAlign,
      CenterSelectionAlign
    )
  }

  sealed trait Vertical extends Powertype
  case object TopVertical extends Vertical { // initial, VERTICAL_TOP
    val name = "top"
    val value = 0x0
  }
  case object CenterVertical extends Vertical { // center, VERTICAL_CENTER
    val name = "center"
    val value = 0x1
  }
  case object BottomVertical extends Vertical { // bottom, VERTICAL_BOTTOM
    val name = "bottom"
    val value = 0x2
  }
  case object JustifyVertical extends Vertical { // justify, VERTICAL_JUSTIFY
    val name = "justtify"
    val value = 0x3
  }
  object Vertical extends PowertypeClass {
    type T = Vertical
    val elements = Vector(
      TopVertical,
      CenterVertical,
      BottomVertical,
      JustifyVertical
    )
  }

  case class Font(
    fontName: String,
    altFontNames: Seq[String] = Nil,
    heightInPoints: Option[Int] = None, // CSS: font-size
    colorRgb: Option[Int] = None, // CSS: color
    bold: Option[Boolean] = None, // CSS: font-style: normal= None, bold, bolder, lighter, [number]
                                 // Excel: BOLDWEIGHT_NORMAL, BOLDWEIGHT_BOLD
    italic: Option[Boolean] = None, // CSS: font-style: normal, italic, oblique
                                   // Excel: boolean
    underline: Option[Boolean] = None, // CSS: text-decoration: none, underline # overline, line-through
                                      // Excel: U_NONE, U_SINGLE, U_DOUBLE, U_SINGLE_ACCOUNTING, U_DOUBLE_ACCOUNTING
    strikeout: Option[Boolean] = None, // CSS: text-decoration: none, line-through
                                      // Excel: boolean
    typeOffsetting: Option[Int] = None // CSS: vertical-align: super, sub
                                       // Excel: SS_NONE, SS_SUPER, SS_SUB
  )

  sealed trait Length {
  }

  case class EmLength(v: Float) extends Length {
  }

  case class ExLength(v: Float) extends Length {
  }

  case class PercentLength(v: Float) extends Length {
  }

  case class PxLength(v: Int) extends Length {
  }

  case class PtLength(v: Float) extends Length {
  }

  case class InLength(v: Float) extends Length {
  }

  case class CmLength(v: Float) extends Length {
  }

  case class MmLength(v: Float) extends Length {
  }

  object Length {
    case class BySuffix(suffix: String, creator: String => Length)

    val bySuffix = Vector(
      BySuffix("em", s => EmLength(s.toFloat)),
      BySuffix("ex" , s => ExLength(s.toFloat)),
      BySuffix("%" , s => PercentLength(s.toFloat)),
      BySuffix("px" , s => PxLength(s.toInt)),
      BySuffix("pt" , s => PtLength(s.toFloat)),
      BySuffix("in" , s => InLength(s.toFloat)),
      BySuffix("cm" , s => CmLength(s.toFloat)),
      BySuffix("mm" , s => MmLength(s.toFloat))
    )

    def apply(s: String) = {
      val a = bySuffix.toStream map {
        case BySuffix(suffix, f) if s.endsWith(suffix) => f(s.substring(0, s.length - suffix.length))
      }
      a.headOption getOrElse {
        EmLength(s.toFloat)
      }
    }
  }
}
