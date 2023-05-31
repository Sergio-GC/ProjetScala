package demo

import com.github.tototoshi.csv._
import java.io.File
import java.util.Date
import java.text.SimpleDateFormat
import scala.collection.MapView

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

    // 1 - Filter and print doggos with breed "Labrador Retriever"
    val labradorDoggos: List[Doggo] = filterByBreed(doggos, "Labrador Retriever")
    printDoggos(labradorDoggos)

    val labradorCount: Int = labradorDoggos.size
    println(s"Number of Labrador Retrievers: $labradorCount \n")

    // 2 - Calculate the average age of all doggos
    val totalAge: Double = doggos.map(_.age).sum
    val averageAge: Double = totalAge / doggos.size

    println(s"Average age of all doggos: $averageAge \n")

    // 3 - Group doggos by breed and count the number of doggos in each breed
    val doggosByBreed: Map[String, List[Doggo]] = doggos.groupBy(_.breed)
    val doggoCountByBreed: MapView[String, Int] = doggosByBreed.mapValues(_.size)

    doggoCountByBreed.foreach { case (breed, count) =>
      println(s"Breed: $breed - Count: $count")
    }

    // 4 - Find the oldest doggo
    val oldestDoggo: Option[Doggo] = doggos.maxByOption(_.age)

    oldestDoggo.foreach { doggo =>
      println("\nDetails of the oldest doggo:")
      println(s"ID: ${doggo.id}")
      println(s"Name: ${doggo.name}")
      println(s"Age: ${doggo.age}")
      println(s"Breed: ${doggo.breed}\n")
    }

     // 5 - Extract the names of all doggos and print the count of doggos without name
    val doggoNames: List[String] = doggos.map(_.name)

    val doggosWithoutName = doggoNames.count(_.isEmpty)
    val totalDoggos = doggos.size

    println(s"Amount of dogs without name: $doggosWithoutName out of $totalDoggos")

    val goldenDoggos = findDoggosByColor(doggos = doggos, color = "golden")
    val nbGolden = goldenDoggos.size
    println(s"Golden doggos: ${nbGolden}")

    println(s"Is every dog neutered? : ${areAllDoggosNeutered(doggos = doggos)}")
    println(s"Average age of Labrador Retrievers : ${calculateAvgAgeByBreed(doggos = doggos, breed = "Labrador Retriever")} years")
}

/**
  * Find dogs of a given color.
  *
  * @param doggos List of dogs where it should look for the given color
  * @param color Color to look for in the list of dogs
  * @return List of dogs of the given color
  */
def findDoggosByColor(doggos: List[Doggo], color: String): List[Doggo] = {
  doggos.filter(_.color == color)
}

/**
  * Checks if every dog in a list is neutered
  *
  * @param doggos List of dogs
  * @return Boolean --> True if every dog in the list has been neutered, false otherwise
  */
def areAllDoggosNeutered(doggos: List[Doggo]): Boolean = {
  doggos.forall(_.neutered)
}

/**
  * Calculate average for a given breed
  *
  * @param doggos List of dogs
  * @param breed Breed to look for
  * @return Average age of every dog of the given breed
  */
def calculateAvgAgeByBreed(doggos: List[Doggo], breed: String): Double = {
  val totalAge = doggos
    .filter(_.breed == breed)
    .map(_.age)
    .sum

    totalAge / doggos.size
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