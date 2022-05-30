#!/usr/bin/env -S scala -nc

import java.nio.file.{Paths, Files}
import java.nio.charset.StandardCharsets
import java.util.InputMismatchException
import scala.collection.mutable.ArrayBuffer
import scala.xml.{Elem, Node, NodeSeq, Text, XML}
import scala.xml.transform.{RewriteRule, RuleTransformer}

class Component(xml: Elem, width: Double, height: Double, x: Double, y: Double, rotation: Double) {
  def elem() = {
    var transforms = List[String]()
    if (rotation != 0) {
      val pixelsX = width / 25.4 * 96 / 2
      val pixelsY = height / 25.4 * 96 / 2
      transforms = s"translate(-${pixelsX}, -${pixelsY})" :: transforms
      transforms = s"rotate(${rotation})" :: transforms
      transforms = s"translate(${pixelsX}, ${pixelsY})" :: transforms
    }

    if (x != 0 || y != 0) {
      val pixelsX = x / 25.4 * 96
      val pixelsY = y / 25.4 * 96
      transforms = s"translate(${pixelsX}, ${pixelsY})" :: transforms
    }

    val transformString = transforms.mkString(" ")

    <g transform={transformString}>
      {xml}
    </g>
  }

  def translated(x: Double, y: Double) = {
    new Component(xml, width, height, this.x + x, this.y + y, rotation)
  }

  def rotated(angle: Double) = {
    new Component(xml, width, height, x, y, rotation + angle)
  }
}

object Component {
  private def parseLength(str: String): Double = {
    var multiplier = 0
    if (str.endsWith("mm")) {
      return str.substring(0, str.length - 2).toDouble
    } else if (str.endsWith("in")) {
      return str.substring(0, str.length - 2).toDouble * 25.4
    }
    throw new InputMismatchException()
  }

  def fromFile(path: String) = {
    val xml = XML.loadFile(path)
    val width = parseLength((xml \ "@width").toString)
    val height = parseLength((xml \ "@height").toString)
    new Component(xml, width, height, 0, 0, 0)
  }
}

sealed trait TagMatch {
  def matches(tagName: String, tags: Map[String, String]): Boolean
}

case class Required(value: String) extends TagMatch {
  override def matches(tagName: String, tags: Map[String, String]) = {
    tags.get(tagName) == Some(value)
  }
}

object Forbidden extends TagMatch {
  override def matches(tagName: String, tags: Map[String, String]) = {
    !tags.contains(tagName)
  }
}

case class Label(tags: Option[Map[String, TagMatch]], layers: Option[Seq[Int]]) {
}

object Label {
  def default() = {
    new Label(None, None)
  }

  def parse(label: String) = {
    var tags: Option[Map[String, TagMatch]] = None
    var layers: Option[Seq[Int]] = None

    for (entry <- label.split(";")) {
      if (entry.length != 0) {
        val pair = entry.split("=")
        if (pair.length == 2) {
          val key = pair(0)
          val value = pair(1)
          if (key == "layers") {
            if (layers.isDefined) {
              throw new InputMismatchException(s"Layers specified multiple times: ${entry}")
            }
            val parsedLayers = value.split(",").map(_.toInt).toSeq
            layers = Some(parsedLayers)
          } else if (key == "tags") {
            if (tags.isDefined) {
              throw new InputMismatchException(s"Tags specified multiple times: ${entry}")
            }
            val parsedTags = value.split(",").map(tagString => {
              if (tagString(0) == '!') {
                (tagString.substring(1), Forbidden)
              } else {
                val tagSplit = tagString.split(":")
                if (tagSplit.length != 2) {
                  throw new InputMismatchException(s"Invalid tag specifier: ${tagString}")
                }
                (tagSplit(0), new Required(tagSplit(1)))
              }
            }).toMap
            tags = Some(parsedTags)
          } else {
            Console.err.println(s"Unhandled label component: ${key} = ${value}")
          }
        } else {
          Console.err.println(s"Unhandled label: ${entry}")
        }
      }
    }

    new Label(tags, layers)
  }
}

object CaseGen extends App {
  val base = Component.fromFile("./components/nanuk_910.svg")
  val px4 = Component.fromFile("./components/px4_full/main.svg")
  val magazine = Component.fromFile("./components/px4_full/magazine.svg")
  val speedloader = Component.fromFile("./components/speedloader.svg")
  val ammo = Component.fromFile("./components/federal_50ct_small.svg")
  val laserRound = Component.fromFile("./components/9mm_laser_round.svg")
  val silica = Component.fromFile("./components/wiseorb_silica.svg")
  val cleaningRod = Component.fromFile("./components/px4_full/cleaning_rod.svg");
  val cleaningBrush = Component.fromFile("./components/px4_full/cleaning_brush.svg");

  val magazineRotationAngle = 0
  val magazineRotationRadians = magazineRotationAngle / 180.0 * Math.PI
  val magazineCoords = (112, 110)
  val magazineSep = 40

  var unfiltered =
    <svg version="1.1" xmlns="http://www.w3.org/2000/svg" xmlns:svg="http://www.w3.org/2000/svg">
      {base.elem}
      {px4.translated(136, 5).rotated(13.58).elem}
      {
        magazine
          .translated(magazineCoords._1, magazineCoords._2)
          .rotated(magazineRotationAngle + 180)
          .elem
      }
      {
        magazine
          .translated(
            magazineCoords._1 - magazineSep * Math.sin(magazineRotationRadians),
            magazineCoords._2 + magazineSep * Math.cos(magazineRotationRadians))
          .rotated(magazineRotationAngle)
          .elem
      }
      {silica.translated(170, 55).rotated(13.58).elem}
      {speedloader.translated(85, 195).elem}
      {ammo.translated(22.5, 12.5).elem}
      {cleaningRod.translated(12.5, 31.5).elem}
      {cleaningBrush.translated(33, 153).elem}
      {cleaningBrush.translated(50.5, 153).rotated(180).elem}
      {laserRound.translated(80, 153).elem}
    </svg>

  val tags = Map(
    "light" -> "gm-p13",
    "sight" -> "fastfire3"
  )

  val tagFilterRule = new RewriteRule {
    override def transform(node: Node): NodeSeq = {
      val labelString = node.attribute("http://www.inkscape.org/namespaces/inkscape", "label").asInstanceOf[Option[Text]]
      val label = Label.parse(labelString.map(_.data).getOrElse(""))
      label.tags match {
        case Some(tagRequirements) => {
          for ((tagName, tagRequirement) <- tagRequirements) {
            if (!tagRequirement.matches(tagName, tags)) {
              return NodeSeq.Empty
            }
          }
        }

        case None => {}
      }

      Seq(node)
    }
  }

  def layerFilterRule(layer: Int) = new RewriteRule {
    override def transform(node: Node): NodeSeq = {
      val labelString = node.attribute("http://www.inkscape.org/namespaces/inkscape", "label").asInstanceOf[Option[Text]]
      val label = Label.parse(labelString.map(_.data).getOrElse(""))
      label.layers match {
        case Some(layers) => {
          if (!layers.contains(layer)) {
            return NodeSeq.Empty
          }
        }

        case None => {}
      }

      Seq(node)
    }
  }

  val tagFiltered = new RuleTransformer(tagFilterRule).apply(unfiltered)
  for (i <- 0 until 4) {
    val result = new RuleTransformer(layerFilterRule(i)).apply(tagFiltered)
    Files.write(Paths.get(s"px4_${i}.svg"), result.toString().getBytes(StandardCharsets.UTF_8))
  }
}
