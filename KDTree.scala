import math._
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.SortedMap

case class Point(x: Double, y: Double) {
  def distance(other: Point): Double = {
    val deglen = 110.25
    deglen*sqrt(pow(x - other.x, 2) + pow((y - other.y)*cos(other.x), 2))
  }
}

case class Node[T <% Ordered[T]](var datax: T, var datay: T, var left: Node[T], var right: Node[T]) {
  override def toString = "[" + datax.toString + " , " + datay.toString + "] "
}

case class KDTree[T <% Ordered[T]](var root: Node[T]) {
  def search(datax: T, datay: T): Node[T] = {
    var node = root
    var a = 0; //axis var
    var res = +1;
    while (node != null) {
      if (a % 2 == 0) res = datax.compare(node.datax) //axis x level
      else res = datay.compare(node.datay) // axis y level

      if (res == 0) {
        if (a % 2 == 0) {
          if (datay.compare(node.datay) == 0) return node
          else res = +1 // idio x diaforetiko y
        } else {
          if (datax.compare(node.datax) == 0) return node
          else res = +1 // idio y diaforetiko x
        }

      }

      if (res < 0) node = node.left
      else node = node.right
      a += 1;
    }
    null
  }

  def insert(datax: T, datay: T) {
    if (root == null) {
      root = Node[T](datax, datay, null, null);
      return
    }

    var node = root
    var a = 0;
    var res = +1;
    while (node != null) {

      if (a % 2 == 0) res = datax.compare(node.datax) // x axis
      else res = datay.compare(node.datay) //y axis

      if (res == 0) {
        if (a % 2 == 0) {
          if (datay.compare(node.datay) == 0) return
          else res = +1
        } else {
          if (datax.compare(node.datax) == 0) return
          else res = +1
        }
      }
      if (res < 0) {
        if (node.left == null) {
          node.left = Node[T](datax, datay, null, null);
          return
        }
        else node = node.left
      } else if (res > 0) {
        if (node.right == null) {
          node.right = Node[T](datax, datay, null, null);
          return
        } else node = node.right
      }
      a += 1;
    }
  }

  def remove(datax: T, datay: T): Boolean = {
    val newroot = new Node[T](root.datax, root.datay, root, null)
    val res = remove(root, newroot, datax, datay, 0)
    root = newroot.left
    res
  }

  private def remove(node: Node[T], parent: Node[T], datax: T, datay: T, count: Int): Boolean = {
    if (node == null) return false
    var res = +1

    if (count % 2 == 0) res = datax.compare(node.datax)
    else res = datay.compare(node.datay)

    var modul = count + 1
    if (res == 0) {
      if (count % 2 == 0) {
        if (datay.compare(node.datay) == 0) {
          if (node.right != null) {
            var nodeX = findMinX(node.right, node.right)
            node.datax = nodeX.datax
            node.datay = nodeX.datay
            remove(node.right, node, node.datax, node.datay, modul)
          } else if (node.left != null) {
            var nodeX = findMinX(node.left, node.left)
            node.datax = nodeX.datax
            node.datay = nodeX.datay
            node.right = node.left
            node.left = null
            remove(node.right, node, node.datax, node.datay, modul)
          } else if (node == parent.left) {
            parent.left = if (node.left != null) node.left else node.right
          } else if (node == parent.right) {
            parent.right = if (node.left != null) node.left else node.right
          }
          return true
        } else res = +1
      } else {
        if (datax.compare(node.datax) == 0) {
          if (node.right != null) {
            var nodeX = findMinY(node.right, node.right)
            node.datax = nodeX.datax
            node.datay = nodeX.datay
            remove(node.right, node, node.datax, node.datay, modul)
          } else if (node.left != null) {
            var nodeX = findMinY(node.left, node.left)
            node.datax = nodeX.datax
            node.datay = nodeX.datay
            node.right = node.left
            node.left = null
            remove(node.right, node, node.datax, node.datay, modul)
          } else if (node == parent.left){
            parent.left = if (node.left != null) node.left else node.right
          }else if (node == parent.right){
            parent.right = if (node.left != null) node.left else node.right
          }
          return true
        } else res = +1
      }
    }
    if (res < 0) remove(node.left, node, datax, datay, modul)
    else remove(node.right, node, datax, datay, modul)
  }

  private def findMinX(node: Node[T], min: Node[T]): Node[T] = {
    val s = new ListBuffer[Node[T]]
    var min = node;
    s.append(node)
    while (s.nonEmpty) {
      val node = s.head
      if (min.datax > node.datax) min = node
      s.remove(0)
      if (node.left != null) s.append(node.left)
      if (node.right != null) s.append(node.right)
    }
    min
  }

  private def findMinY(node: Node[T], min: Node[T]): Node[T] = {
    val s = new ListBuffer[Node[T]]
    var min = node;
    s.append(node)
    while (s.nonEmpty) {
      val node = s.head
      if (min.datay > node.datay) min = node
      s.remove(0)
      if (node.left != null) s.append(node.left)
      if (node.right != null) s.append(node.right)
    }
    min
  }

  def findkNN(datax: Double, datay: Double, k: Int) = {
    val sortedMap = SortedMap.empty[Double,Node[T]]
    val point = Point(datax, datay)
    val pointR = makePoint(root)
    var min: Node[T] = null
    var rootDistance = point.distance(pointR)
    sortedMap += (rootDistance -> root  )
    println
    println("Root Node: " + root + " -> Distance from given " + point + ": " + rootDistance)
    println
    var s = "Left Path Tracking :"
    min = LR_Range(point, k, root.left,root.left,sortedMap,s)
    println
    s = "Right Path Tracking :"
    min =LR_Range(point, k, root.right,root.right,sortedMap,s)

    val knnList = ListBuffer(sortedMap.toSeq: _*).take(k)
    println
    println
    println("The " + k + " Nearest Neighbours of " + point + " are")
    println
    println("(Distance,Point): ")
    knnList.foreach(println)
    min = knnList(0)._2
    min
  }

  def LR_Range(point: Point, k: Int,nodeLR: Node[T], minLR: Node[T], sortedMap: SortedMap[Double, Node[T]], s: String): Node[T] = {
    var node = nodeLR
    var min = minLR
    var backTrack = false;
    var parent: Node[T] = null
    val pointR = makePoint(node)
    var minDistance = point.distance(pointR)
    sortedMap += (minDistance -> node)
    println
    println(s)
    println
    println("Start Node: " + node + " -> Distance from given " + point + ": " + minDistance)
    println
    println("Parent         | " + "Left Child     | " + "Right Child    | " + "Distance From Left | Distance From Right")

    while (node != null) {
      if (node.left != null && node.right != null) { // 2 childs
        val point1 = makePoint(node.left)
        val point2 = makePoint(node.right)
        val dist1 = point.distance(point1)
        val dist2 = point.distance(point2)
        parent = node
        println(parent + " | " + node.left + " | " + node.right + " | " + dist1 + " | " + dist2)
        if (dist1 > dist2) {
          if (minDistance > dist2) {
            minDistance = dist2
            min = node.right
          }
          sortedMap += (dist2 -> node.right )
          node = node.right
        }
        else {
          if (dist1 > minDistance ) {
            minDistance = dist1
            min = node.left
          }
          sortedMap += (dist1 -> node.left )
          node = node.left
        }
      } else if (node.left != null) {
        val point1 = makePoint(node.left)
        val dist1 = point.distance(point1)
        parent = node
        println(parent + " | " + node.left + " | " + "    ----       | " + dist1 + " |     ----        ")
        if (minDistance > dist1) {
          minDistance = dist1
          min = node.left
        }
        sortedMap += (dist1 -> node.left)
        node = node.left
      } else if (node.right != null) {
        val point2 = makePoint(node.right)
        val dist2 = point.distance(point2)
        parent = node
        println(parent + " | " + "    ----       | " + node.right + " | " + "       ----        | " + dist2)
        if (minDistance > dist2) {
          minDistance = dist2
          min = node.right
        }
        sortedMap += (dist2 -> node.right)
        node = node.right
      } else {
        if (!backTrack) {
          //Backtracking
          if (node == parent.left) {
            if (parent.right != null) {
              node = parent.right
              val pointB = makePoint(node)
              val distb = point.distance(pointB)
              println
              println("BackTracking :")
              println("Parent         | " + "Left Child     | " +
                "Right Child    | " +
                "Distance From Left | Distance From Right")

                println(parent + " | " +
                  "    Hide       | " + node + " | " +
                  "       Hide        | " + distb)
                  sortedMap += (distb -> node)
                }
              } else if (node == parent.right) {
                if (parent.left != null) {
                  node = parent.left
                  val pointB = makePoint(node)
                  val distb = point.distance(pointB)
                  println
                  println("BackTracking :")
                  println("Parent         | " + "Left Child     | " +
                    "Right Child    | " +
                    "Distance From Left | Distance From Right")

                    println(parent + " | " +
                      node + " | " + "    Hide       | " +
                      distb + " |        Hide          ")
                      sortedMap += (distb -> node)
                    }
                  }

                  backTrack = true //stop backtracking

                } else return min
              }
            }
            min
          }
          def makePoint(node: Node[T]): Point = {
            val x = node.datax.asInstanceOf[Double]
            val y = node.datay.asInstanceOf[Double]
            val point = Point(x, y)
            point
          }

          //DFS
          def dfsTrav(apply: Node[T] => Unit)  {
            val s = new ListBuffer[Node[T]]
            s.append(root)
            while (s.nonEmpty) {
              val node = s.last
              s.remove(s.length - 1)
              apply(node)
              if (node.right != null) s.append(node.right)
              if (node.left != null) s.append(node.left)
            }
          }

          //BFS
          def bfsTrav(apply: Node[T] => Unit)  {
            val s = new ListBuffer[Node[T]]
            s.append(root)
            while (s.nonEmpty) {
              val node = s.head
              s.remove(0)
              apply(node)
              if (node.left != null) s.append(node.left)
              if (node.right != null) s.append(node.right)
            }
          }
        }
