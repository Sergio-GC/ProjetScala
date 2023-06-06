package demo

import com.github.tototoshi.csv._
import java.io.File
import java.util.Date
import java.text.SimpleDateFormat
import scala.collection.MapView
import scala.util.Success
import scala.util.Failure
import scala.util.Try

/**
  * Animal Trait
  * 
  * Defines common properties of an animal
  */
trait Animal {
  def id: Int
  def name: String
  def age: Double
}

/**
  * Pet Trait
  *
  * Defines common properties of a pet.
  */
trait Pet {
  def color: String
}

/**
  * Dog Trait
  *
  * Defines common properties of a dog.
  */
trait Dog {
  def breed: String
  def size: String
  def coat: String
}

/**
  * Doggo Class
  *
  * Represents a shelter dog.
  *
  * @param id            Unique identifier of the dog
  * @param name          Name of the dog
  * @param age           Age of the dog
  * @param sex           Sex of the dog
  * @param breed         Breed of the dog
  * @param dateFound     Date when the dog was found
  * @param adoptableFrom Date when the dog becomes adoptable
  * @param posted        Date when the dog was posted
  * @param color         Color of the dog
  * @param coat          Coat type of the dog
  * @param size          Size of the dog
  * @param neutered      Indicates if the dog is neutered
  * @param housebroken   Indicates if the dog is housebroken
  * @param likesPeople   Indicates if the dog likes people
  * @param likesChildren Indicates if the dog likes children
  * @param getAlongMales Indicates if the dog gets along with males
  * @param getAlongFemales Indicates if the dog gets along with females
  * @param getAlongCats  Indicates if the dog gets along with cats
  * @param keepIn        Location where the dog should be kept
  */
case class Doggo(
    val id: Int,
    val name: String,
    val age: Double,
    val sex: String,
    val breed: String,
    val dateFound: Date,
    val adoptableFrom: Date,
    val posted: Date,
    val color: String,
    val coat: String,
    val size: String,
    val neutered: Boolean,
    val housbroken: Boolean,
    val likesPeople: Boolean,
    val likesChildren: Boolean,
    val getAlongMales: Boolean,
    val getAlongFemales: Boolean,
    val getAlongCats: Boolean,
    val keepIn: String
) extends Animal with Pet with Dog

/**
  * Doggo Companion Object
  *
  * Provides utility methods for creating Doggo instances.
  */
object Doggo {
    /**
      * Create a Doggo instance from a CSV row.
      *
      * @param row        Map containing the CSV row data
      * @param dateFormat Date format used in the CSV file
      * @return Doggo instance
      */
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

/**
  * Main Program
  *
  * Loads and processes shelter dog data.
  */
@main def loadData() = {
    // Retrieve the data from the CSV file
    val dateFormat = new SimpleDateFormat("yyyy-MM-dd")

    val result: Try[List[Doggo]] = Try {
        val reader = CSVReader.open(new java.io.File("data/07-ShelterDogs.csv"))
        val csvData: List[List[String]] = reader.all()
        reader.close()

        val header: List[String] = csvData.head
        val data: List[List[String]] = csvData.tail

        data.map{row => 
            val dogValues: Map[String, String] = header.zip(row).toMap
            Doggo(dogValues, dateFormat)
        }.toList
    }
    
    result match {
        case Success(doggos) =>
            // Process the loaded doggos

            // Filter and print doggos with breed "Labrador Retriever"
            val labradorDoggos: List[Doggo] = filterByBreed(doggos, "Labrador Retriever")
            printDoggos(labradorDoggos)

            val labradorCount: Int = labradorDoggos.size
            println(s"Number of Labrador Retrievers: $labradorCount \n")

            // Calculate the average age of all doggos
            val totalAge: Double = doggos.map(_.age).sum
            val averageAge: Double = totalAge / doggos.size

            println(s"Average age of all doggos: $averageAge \n")

            // Group doggos by breed and count the number of doggos in each breed
            val doggosByBreed: Map[String, List[Doggo]] = doggos.groupBy(_.breed)
            val doggoCountByBreed: MapView[String, Int] = doggosByBreed.mapValues(_.size)

            doggoCountByBreed.foreach { case (breed, count) =>
                println(s"Breed: $breed - Count: $count")
            }

            // Find the oldest doggo
            val oldestDoggo: Option[Doggo] = doggos.maxByOption(_.age)

            oldestDoggo.foreach { doggo =>
                println("\nDetails of the oldest doggo:")
                println(s"ID: ${doggo.id}")
                println(s"Name: ${doggo.name}")
                println(s"Age: ${doggo.age}")
                println(s"Breed: ${doggo.breed}\n")
            }

            // Extract the names of all doggos and print the count of doggos without name
            val doggoNames: List[String] = doggos.map(_.name)

            val doggosWithoutName = doggoNames.count(_.isEmpty)
            val totalDoggos = doggos.size

            println(s"Amount of dogs without name: $doggosWithoutName out of $totalDoggos")

            // Count all the doggos having a golden color
            val goldenDoggos = findDoggosByColor(doggos = doggos, color = "golden")
            val nbGolden = goldenDoggos.size
            println(s"\nAmount of golden doggos: ${nbGolden}")

            // Determine if all the dogs are neutered
            println(s"\nIs every doggo neutered? : ${areAllDoggosNeutered(doggos = doggos)}")

            // Calculate the average agor of Labrador Retrievers
            println(s"\nAverage age of Labrador Retrievers: ${calculateAvgAgeByBreed(doggos = doggos, breed = "Labrador Retriever")} years")

        case Failure(exception) =>
            println(s"An error occured while loading the data: ${exception.getMessage}")
    }
 
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
    val doggosOfBreed = filterByBreed(doggos, breed)
    val totalAge = doggosOfBreed.map(_.age).sum
    val count = doggosOfBreed.length

    totalAge / count
}

/**
  * Filters a list of doggos by breed.
  *
  * @param doggos The list of `Doggo` objects to be filtered.
  * @param breed  The breed to filter by.
  * @return A new list containing only the `Doggo` objects that match the specified breed.
  */
def filterByBreed(doggos: List[Doggo], breed: String): List[Doggo] = {
  doggos.filter(_.breed == breed)
}

/**
  * Prints the details of a list of doggos.
  *
  * @param doggos The list of `Doggo` objects to be printed.
  */
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