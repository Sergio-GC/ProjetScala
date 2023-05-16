package demo

import com.github.tototoshi.csv._
import java.io.File

@main def hello() = {
    val reader = CSVReader.open(new File("data/07-ShelterDogs.csv"))
    val rows = reader.all()
    rows.foreach(row => println(row.mkString(", ")))
}