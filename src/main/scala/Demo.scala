package demo

import com.github.tototoshi.csv._
import java.io.File
import java.util.Date
import java.text.SimpleDateFormat

// Define traits or abstract classes for common properties
trait Animal {
  def id: Int
  def name: String
  def age: Double
}

trait Pet {
  def color: String
}

trait Dog {
  def breed: String
  def size: String
  def coat: String
}

// Create case classes representing the main data elements
case class Doggo(
    val id: Int,
    val name: String,
    val age: Double,
    val sex: String, //Maybe replace for a boolean
    val breed: String, //TODO replace by an external table to avoid human errors
    val dateFound: Date,
    val adoptableFrom: Date,
    val posted: Date, //TODO ask what can this be...
    val color: String,
    val coat: String, //TODO replace by external table to avoid human errors
    val size: String, //TODO same than above ^^
    val neutered: Boolean,
    val housbroken: Boolean,
    val likesPeople: Boolean,
    val likesChildren: Boolean,
    val getAlongMales: Boolean,
    val getAlongFemales: Boolean,
    val getAlongCats: Boolean,
    val keepIn: String //TODO external table
) extends Animal with Pet with Dog

object Doggo {
  def apply(row: Map[String, String], dateFormat: SimpleDateFormat): Doggo = {
    val id = row("ID").toInt
    val name = row("name")
    val age = row("age").toDouble
    val sex = row("sex")
    val breed = row("breed")
    val dateFound = dateFormat.parse(row("date_found"))
    val adoptableFrom = dateFormat.parse(row("adoptable_from"))
    val posted = dateFormat.parse(row("posted"))
    val color = row("color")
    val coat = row("coat")
    val size = row("size")
    val neutered = row("neutered").equals("yes")
    val housebroken = row("housebroken").equals("yes")
    val likesPeople = row("likes_people").equals("yes")
    val likesChildren = row("likes_children").equals("yes")
    val getAlongMales = row("get_along_males").equals("yes")
    val getAlongFemales = row("get_along_females").equals("yes")
    val getAlongCats = row("get_along_cats").equals("yes")
    val keepIn = row("keep_in")
    
    Doggo(
      id, name, age, sex, breed, dateFound, adoptableFrom, posted, color, coat, size,
      neutered, housebroken, likesPeople, likesChildren, getAlongMales, getAlongFemales, getAlongCats, keepIn
    )
  }
}

@main def loadData() = {
    val dateFormat = new SimpleDateFormat("yyyy-MM-dd")

    val reader = CSVReader.open(new java.io.File("data/07-ShelterDogs.csv"))
    val csvData: List[List[String]] = reader.all()
    reader.close()

    val header: List[String] = csvData.head
    val data: List[List[String]] = csvData.tail

    val doggos: List[Doggo] = data.map{row => 
        val dogValues: Map[String, String] = header.zip(row).toMap
        Doggo(dogValues, dateFormat)
    }

    // Filter and print doggos with breed "Labrador Retriever"
    val labradorDoggos: List[Doggo] = filterByBreed(doggos, "Labrador Retriever")
    printDoggos(labradorDoggos)

    val labradorCount: Int = labradorDoggos.size
    println(s"Number of Labrador Retrievers: $labradorCount")



    val doggosWithoutName = doggos.count(_.name.isEmpty)
    val totalDoggos = doggos.size

    println(s"Amount of dogs without name: $doggosWithoutName out of $totalDoggos")
}

def filterByBreed(doggos: List[Doggo], breed: String): List[Doggo] = {
  doggos.filter(_.breed == breed)
}

def printDoggos(doggos: List[Doggo]): Unit = {
  doggos.foreach { pet =>
    println(s"ID: ${pet.id}")
    println(s"Name: ${pet.name}")
    println(s"Age: ${pet.age}")
    println(s"Breed: ${pet.breed}")
    println(s"Date found: ${pet.dateFound}")
    println()
  }
}