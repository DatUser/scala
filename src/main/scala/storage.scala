package main

import main.Parse
import main.MainClass

case class Country(code_ : String, name_ : String, airports_ : List[Airport])

case class Airport(id_ : Int, iso_country_ : String, name_ : String,
                    runways_ : List[Runway])

case class Runway(id_ : Int, airport_ref_ : Int, surface_ : String,
                    le_ident_ : String)

object Storage {

def get_country_airports(country : String) : List[Airport] = {
    MainClass.airports.filter(x => x.iso_country_ == country)
}

def get_airport_runways(airport : Int) : List[Runway] = {
    MainClass.runways.filter(x => x.airport_ref_ == airport)
}

def find_country(str : String) : Option[Country] = {
     MainClass.countries
                .filter(x => (x.name_ == str) || (x.code_ == str)).lift(0)
}

def to_id(str : String) : Int = {
    str match {
        case "" => 0
        case _ => str.toInt
    }
}

def get_country_runway_types(country : Country) : List[String] = {
    country.airports_.map(x => x.runways_).flatten.map(x => x.surface_)
                        .foldLeft(List[String]())((acc, elt) =>
                        acc.contains(elt) match {
                            case true => acc
                            case false => elt::acc
                        })
}

def get_runway_latitudes_occ() : Map[String, Int] = {
    MainClass.runways.foldLeft(Map[String, Int]() withDefaultValue 0)(
        (acc, elt) => acc + (elt.le_ident_ -> (1 + acc(elt.le_ident_))))
}

def get_runway_latitudes() : List[String] = {
    val occs = get_runway_latitudes_occ()
    MainClass.runways.map(x => x.le_ident_)
                        .sortBy(occs(_))
                        .foldLeft(List[String]())((acc, elt) =>
                        acc.contains(elt) match {
                            case true => acc
                            case false => elt::acc
                        })
}

}
