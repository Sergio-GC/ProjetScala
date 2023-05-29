package demo

import com.github.tototoshi.csv._
import java.io.File
import java.util.Date
import java.text.SimpleDateFormat

class Doggo(
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
)

@main def loadData() = {
    val dateFormat = new SimpleDateFormat("yyyy-MM-dd")

    val reader = CSVReader.open(new java.io.File("data/07-ShelterDogs.csv"))
    val csvData: List[List[String]] = reader.all()
    reader.close()

    val header: List[String] = csvData.head
    val data: List[List[String]] = csvData.tail

    val doggos: List[Doggo] = data.map{row => 
        val dogValues: Map[String, String] = header.zip(row).toMap
        val dateFound = dateFormat.parse(dogValues("date_found"))
        val adoptableFrom = dateFormat.parse(dogValues("adoptable_from"))
        val posted = dateFormat.parse(dogValues("posted"))

        val neutered = dogValues("neutered").equals("yes")
        val housebroken = dogValues("housebroken").equals("yes")
        val likes_people = dogValues("likes_people").equals("yes")
        val likes_children = dogValues("likes_children").equals("yes")
        val get_along_males = dogValues("get_along_males").equals("yes")
        val get_along_females = dogValues("get_along_females").equals("yes")
        val get_along_cats = dogValues("get_along_cats").equals("yes")

        Doggo(
            dogValues("ID").toInt,
            dogValues("name"),
            dogValues("age").toDouble,
            dogValues("sex"),
            dogValues("breed"),
            dateFound,
            adoptableFrom,
            posted,
            dogValues("color"),
            dogValues("coat"),
            dogValues("size"),
            neutered,
            housebroken,
            likes_people,
            likes_children,
            get_along_males,
            get_along_females,
            get_along_cats,
            dogValues("keep_in")
        )
    }

    doggos.foreach { pet =>
        println(s"ID: ${pet.id}")
        println(s"Name: ${pet.name}")
        println(s"Age: ${pet.age}")
        println(s"Date found: ${pet.dateFound}")
    }

    val doggosWithoutName = doggos.count(doggo => doggo.name.equals(""))
    val totalDoggos = doggos.size

    println(s"Amount of dogs without name: $doggosWithoutName out of $totalDoggos")
}