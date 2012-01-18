package org.intermine.client.example
import org.intermine.client.Service
import org.intermine.client.query.constraint.Constraint._
import org.intermine.client.query.Query._
import scalaz._
import Validation.Monad._

object GenesByPathway extends App {
  import Scalaz._

  override def main(args: Array[String]): Unit = {
    val opts = parseCmdLine(args)
    val s = new Service(opts('service).toString)
    ((s from "Gene") >>= (_.select("symbol", "proteins.primaryIdentifier", "proteins.length")) >>= (_.where("pathways.name" |==| opts('pathway))) >>= (_.rows))
      .fold(e => {println("ERROR" + e); exit(1)}, rows => {
        rows groupBy (_("symbol")) map {case (sym, rs) => {
          println(sym)
          rs map (r => println(r("proteins.primaryIdentifier") + " - " + r("proteins.length"))); println
        }}
        println("Average length: " + (rows map (_("proteins.length").toInt)).sum / rows.size)
      })
  }

  def usage = """USAGE: genes-in-pathway [--pathway PATHWAY-NAME] [--service SERVICE-ROOT-URI]
  """

  type OptionMap = Map[Symbol, Any]

  def processOpts(map: OptionMap, list: List[String]): OptionMap = list match {
    case Nil => map
    case "--pathway" :: value :: tail => processOpts(map ++ Map('pathway -> value), tail)
    case "--service" :: value :: tail => processOpts(map ++ Map('service -> value), tail)
    case option :: tail => println(GenesByPathway.usage)
                           println("Unknown option: " + option)
                           exit(1)
  }

  def parseCmdLine(args: Array[String]) = processOpts(Map('pathway -> "Pentose phosphate pathway", 'service -> "http://www.flymine.org/query/service"), args.toList)
}
