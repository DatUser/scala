package main

import scala.io
import main.Parse

object MainClass {

val runways = Parse.parse_runway()

val airports = Parse.parse_airport()

val countries = Parse.parse_country()

def print_line() : Unit = {
    println("-----------------------------------------------------------------")
}

def adapt_str(str : String, len : Int) : String = {
    str.length >= len match {
        case true => str
        case _ => adapt_str(str + " ", len)
    }
}

def print_runway(runway : Runway) : Unit = {
    println(adapt_str(runway.id_.toString, 8) + "|" +
            adapt_str(runway.airport_ref_.toString, 15) + "|" +
            adapt_str(runway.surface_, 29) + "|" +
            adapt_str(runway.le_ident_, 10))
}

def print_runways_rec_bis(airport : Airport, index : Int) : Unit = {
    val opt = airport.runways_.lift(index)
    opt match {
        case Some(x) => print_runway(x)
        case None =>
    }
    opt match {
        case Some(x) => print_runways_rec_bis(airport, index + 1)
        case None =>
    }
}

def print_runways_rec(country : Country, index : Int) : Unit = {
    val opt = country.airports_.lift(index)
    opt match {
        case Some(x) => print_runways_rec_bis(x, 0)
        case None =>
    }
    opt match {
        case Some(x) =>  print_runways_rec(country, index + 1)
        case None =>
    }
}

def print_runways(country : Country) : Unit = {
    println("   id   |  airport_ref  |           surface           | latitude")
    print_line()
    print_runways_rec(country, 0)
}

def get_country_info() : Unit = {
    println("Please enter a country name/code")
    Storage.find_country(scala.io.StdIn.readLine()) match {
        case Some(x) => print_runways(x)
        case _ => get_country_info()
    }
    println()
    get_input()
}

def print_lowest(list : List[Country], index : Int) : Unit = {
    val cond = (index < list.size) && (index < 10)
    cond match {
        case true => println(adapt_str(list(index).code_, 10) + "|" 
                                + adapt_str(list(index).name_, 30))
        case false =>
    }
    cond match {
        case true => print_lowest(list, index + 1)
        case false =>
    }
}

def print_highest(list : List[Country], index : Int) : Unit = {
    val cond = (index >= 0) && (index >= list.size - 10)
    cond match {
        case true => println(adapt_str(list(index).code_, 10) + "|" 
                                + adapt_str(list(index).name_, 30))
        case false =>
    }
    cond match {
        case true => print_highest(list, index - 1)
        case false =>
    }

}

def print_top_airport() : Unit = {
    val l = countries.sortBy(_.airports_.size)
    println("Countries with highest number of airports\n" +
            "   code   |             name")
    print_highest(l, l.size - 1)
    println("\nCountries with lowest number of airports\n" +
            "   code   |             name")
    print_lowest(l, 0)
}

def print_country_surface(country : Country, types : List[String],
                            index : Int) : Unit = {
    println(adapt_str(country.code_, 10) + "|" +
            adapt_str(country.name_, 30) + "|" +
            types(index))
    (index + 1) < types.size match {
        case true => print_country_surface(country, types, index + 1)
        case false =>
    }
}

def print_runway_types_rec(index : Int) : Unit = {
    val country = countries(index)
    val types = Storage.get_country_runway_types(country)
    (types.length > 0) match {
        case true => print_country_surface(country, types, 0)
        case false =>
    }
    (index + 1) < countries.size match {
        case true => print_runway_types_rec(index + 1)
        case false =>
    }
}

def print_runway_types() : Unit = {
    println("Countries and runway surfaces\n" +
            "   code   |             name             |    type")
    countries.size > 0 match {
        case true =>print_runway_types_rec(0)
        case false =>
    }
}

def print_top_latitude_rec(lats : List[String], index : Int) : Unit = {
    println(lats(index))

    (index + 1) < lats.size && (index + 1) < 10 match {
        case true => print_top_latitude_rec(lats, index + 1)
        case false =>
    }
}

def print_top_latitude() : Unit = {
    val lats = Storage.get_runway_latitudes()
    println("10 most common runway latitudes\n\n"
            + "latitude\n"
            + "--------")
    lats.size > 0 match {
        case true => print_top_latitude_rec(lats, 0)
        case false =>
    }
}

def get_report_type() : Unit = {
    println("A - 10 countries with highest number of airports and countries" +
            " with lowest number of airports\n" + 
            "B - Type of runways per countries\n" +
            "C - Top 10 most common runway latitude\n" +
            "Choose between 'A', 'B' and 'C'")
    scala.io.StdIn.readLine() match {
        case "A" => print_top_airport()
        case "B" => print_runway_types()
        case "C" => print_top_latitude()
        case _ => get_report_type()
    }
    println()
    get_input()
}

def get_input(): Unit = {
    println("Choose between 'Query', 'Reports' and 'Exit'")
    scala.io.StdIn.readLine() match {
        case "Query" => get_country_info()
        case "Reports" => get_report_type()
        case "Exit" =>
        case _ => get_input()
    }
}

def main(args: Array[String]): Unit = {
    get_input()
}

}
