package main

import scala.io
import main.Storage
import main.Country
import main.MainClass

object Parse {

val pattern = "((-)?[0-9\\.]+|\"[^\"]*\")?(,|$)".r

def str_to_airport(str : String) : Airport = {
    val l = pattern.findAllIn(str).toList
    val airport = Storage.to_id(l(0).replaceAll("[\",]", ""))
    Airport(airport, l(8).replaceAll("[\",]", ""), l(3).replaceAll("[\",]", ""),
            Storage.get_airport_runways(airport))
}

def str_to_country(str : String) : Country = {
    val l = pattern.findAllIn(str).toList
    val code = l(1).replaceAll("[\",]", "")
    Country(code, l(2).replaceAll("[\",]", ""),
                Storage.get_country_airports(code))
}

def str_to_runway(str : String) : Runway = {
    val l = pattern.findAllIn(str).toList
    Runway(Storage.to_id(l(0).replaceAll("[\",]", "")),
            Storage.to_id(l(1).replaceAll("[\",]", "")),
            l(5).replaceAll("[\",]", ""), l(8).replaceAll("[\",]", ""))
}

def str_to_lines(str : String) : List[String] = {
    str.split("\\n").map(_.trim).toList
}

def get_country_data(lines : List[String], index : Int,
                        countries : List[Country]) : List[Country] = {
    (lines.lift(index)) match {
        case Some(s) => get_country_data(lines, index + 1,
                                    str_to_country(s)::countries)
        case None => countries
    }
}

def get_airport_data(lines : List[String], index : Int,
                        airports : List[Airport]) : List[Airport] = {
    (lines.lift(index)) match {
        case Some(s) => get_airport_data(lines, index + 1,
                                            str_to_airport(s)::airports)
        case None => airports
    }
}

def get_runway_data(lines : List[String], index : Int,
                        runways : List[Runway]) : List[Runway] = {
    (lines.lift(index)) match {
        case Some(s) => get_runway_data(lines, index + 1,
                                            str_to_runway(s)::runways)
        case None => runways
    }
}

def extract_country(text : String) : List[Country] = {
    get_country_data(str_to_lines(text), 1, List.empty)
}

def extract_airport(text : String) : List[Airport] = {
    get_airport_data(str_to_lines(text), 1, List.empty)
}

def extract_runway(text : String) : List[Runway] = {
    get_runway_data(str_to_lines(text), 1, List.empty)
}

def print_countries(list : List[Country]) : Unit = {
    for (elt <- list) { 
        println(elt.name_ + "-" + elt.code_);
    }
}

def print_airports(list : List[Airport]) : Unit = {
    for (elt <- list) { 
        println(elt.id_ + "-" + elt.iso_country_);
    }
}

def parse_country() : List[Country] = {
    extract_country(scala.io.Source.fromFile("resources/countries.csv").mkString)
}

def parse_airport() : List[Airport] = {
    extract_airport(scala.io.Source.fromFile("resources/airports.csv").mkString)
}

def parse_runway() : List[Runway] = {
    extract_runway(scala.io.Source.fromFile("resources/runways.csv").mkString)
}

}
