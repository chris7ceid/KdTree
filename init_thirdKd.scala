var tree = KDTree[Double](null)
val bufferedSource = io.Source.fromFile("Inputs/input_thirdKd.csv")
for (line <- bufferedSource.getLines) {
    val cols = line.split(",").map(_.trim)
    tree.insert(cols(0).toDouble,cols(1).toDouble);
}
bufferedSource.close
