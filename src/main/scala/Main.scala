import java.io._
import play.api.libs.json._
import play.api.libs.functional.syntax._
import play.api.libs.json.OFormat.oFormatFromReadsAndOWrites
import play.api.libs.json.Format.GenericFormat
import scala.collection.mutable.Map
import scala.util.control._
import scala.collection.mutable.ListBuffer
import scala.io.StdIn._

object add {
  def addInt( a:Int, b:Int ) : Int = {
    var sum:Int = 0
    sum = a + b
    return sum
  }
}

case class Measurement(
    startAvenue: String,
    startStreet: String,
    transitTime: Double,
    endAvenue: String,
    endStreet: String
)



case class trafficMeasurements(
  measurementTime: Double,
  measurements: List[Measurement]
)

case class info(
  nodeName: String,
  transitTimes: List[Double],
  averageTime:Double
)

case class linkInfo(
  linkedNodes: List[info]
)

case class path(
  lastNode: String,
  transitTime: Double,
  visited: List[String]
)

implicit val MeasurementReads: Reads[Measurement] = (
  (JsPath \ "startAvenue").read[String] and
  (JsPath \ "startStreet").read[String] and
  (JsPath \ "transitTime").read[Double] and
  (JsPath \ "endAvenue").read[String] and
  (JsPath \ "endStreet").read[String]
)(Measurement.apply _)

implicit val trafficMeasurementsReads: Reads[trafficMeasurements] = (
  (JsPath \ "measurementTime").read[Double] and
  (JsPath \ "measurements").read[List[Measurement]]
)(trafficMeasurements.apply _)

var shortest_path:String = ""
var shortest_time:Double = 999999999
var mapInfo:Map[String,linkInfo] = Map()
var target:String = ""
var paths:List[path] = List(null)


// def find_path(start_point:String, transit_time:Double, path:String):Unit = {
//   // print(start_point)
//   println(path)
//   if(transit_time>shortest_time) return
//   if(start_point == target){
//     if(transit_time < shortest_time){
//       shortest_path = path
//       shortest_time = transit_time
//       println("current_short_path=")
//       println(shortest_path)
//       println(shortest_time)
//     }    
//     return
//   }
//   val linked = mapInfo(start_point)
//   if(linked == null) return
//   if(linked.linkedNodes == null) return
//   for(i <-0 to linked.linkedNodes.length-1){
//     var _target = linked.linkedNodes(i)
//     if(!path.contains(_target.nodeName)){
//       // var _average:Double = 0
//       // for(j <-0 to _target.transitTimes.length-1){
//       //   _average = _average + _target.transitTimes(j)
//       // }
//       // _average = _average / _target.transitTimes.length
//       val newPath:String = path + _target.nodeName+"->"// + "  ("+_average+" s)"
//       find_path(_target.nodeName, transit_time+_target.averageTime, newPath)   
//     }    
//   }
// }

def find_path(start_point:String, target_point:String):Unit = {
  paths = List(null)
  paths = paths.patch(0, Seq(new path(start_point, 0, List(start_point))), 1)
  var flag:Boolean = true
  while(flag == true){
    var shortest = paths(0)
    //remove first one
    paths = paths.filter(_ != shortest)
    //get linked nodes
    var linked = mapInfo(shortest.lastNode).linkedNodes
    var newpaths:List[path] = List()
    for ( i <- 0 to linked.length-1){
      if(!shortest.visited.contains(linked(i).nodeName)){
        var newpath = new path(linked(i).nodeName, shortest.transitTime + linked(i).averageTime, List.concat(shortest.visited, Seq(linked(i).nodeName)))
        print("searching... ")
        println(newpath)
        if (newpath.lastNode == target_point){
          println("\n")
          println("~~~ shortest path found! ~~~")
          print("shortest Time = ")
          println(newpath.transitTime)
          print("shortest Path = ")
          println(newpath.visited)
          return;
        }
        //insert proper positon in list
        val loop = new Breaks
        var added:Boolean = false
        loop.breakable {
          for( j <- 0 to paths.length -1){
            if(newpath.transitTime < paths(j).transitTime){
              paths = paths.patch(j, Seq(newpath), 0)
              added = true
              loop.break
            }
            
          }
        }
        if(paths.length == 0) {
          paths = paths.patch(0, Seq(newpath), 0)
          added = true
        }
        if(added == false){
          paths = paths.patch(paths.length, Seq(newpath), 0)
        }
      }
      
    }
    // flag = false
  }
}

@main def main: Unit = 
  // val writer = new PrintWriter(new File("test.txt" ))

  // writer.write("Hello Scala123")
  // writer.close()
  val input_file = "data.txt"
  val json_content = scala.io.Source.fromFile(input_file).mkString
  val json_data = Json.parse(json_content)
  val trafficmeasurements = (json_data \ "trafficMeasurements").validate[List[trafficMeasurements]].get
  for( index1 <- 0 to trafficmeasurements.length-1){
    for( index2 <- 0 to trafficmeasurements(index1).measurements.length-1){
      val measure = trafficmeasurements(index1).measurements(index2)
      val nodeName = measure.startAvenue + measure.startStreet
      val endnodeName = measure.endAvenue + measure.endStreet
      val transitTime = measure.transitTime
      if(!mapInfo.contains(nodeName)){
        val linkinfo = new linkInfo(null);
        mapInfo += (nodeName -> linkinfo)
      }else{
        val linkinfo = mapInfo(nodeName)
        if(linkinfo.linkedNodes == null){
          mapInfo(nodeName) = new linkInfo(List(new info(endnodeName, List(transitTime), transitTime)))
        }else{
          var isNewLink = true
          val loop = new Breaks;
          loop.breakable {
            val _oldList = linkinfo.linkedNodes
            for( i <- 0 to linkinfo.linkedNodes.length-1){
              val _linked = linkinfo.linkedNodes(i)
              if(_linked.nodeName == endnodeName){
                val newaverage = (_linked.averageTime * _linked.transitTimes.length + transitTime)/(_linked.transitTimes.length+1)
                val _newlinked = new info(_linked.nodeName, _linked.transitTimes.patch(0, Seq(transitTime), 0), newaverage)
                val _newList = _oldList.patch(i, Seq(_newlinked), 1)
                mapInfo(nodeName) = new linkInfo(_newList)
                isNewLink = false
                loop.break
              }
            }
            if(isNewLink){
              val _newList = _oldList.patch(0, Seq(new info(endnodeName, List(transitTime), transitTime)), 0)
              mapInfo(nodeName) = new linkInfo(_newList)
            }
          }
        }
      }
    }
  }
  // find_path("G11", "B1")
  var startP :String = ""
  var targetP: String =""
  while(true){
    while(startP == ""){
      println("Input start point (example: G11)")   
      startP = readLine()
      if(!mapInfo.contains(startP)){
        println("Invalid point, type again!")
        startP =""
      }
    }

    while(targetP == ""){
      println("Input target point (example: B1)")
      targetP = readLine()
      if(!mapInfo.contains(targetP)){
        println("Invalid point, type again!")
        targetP =""
      }
    }
    
    
    find_path(startP, targetP)
    startP =""
    targetP =""
  }
  

def msg = "I was compiled by Scala 3. "
