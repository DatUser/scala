import main.MainClass
import main.Parse
import main.Storage

class ParseTest extends org.scalatest.FunSuite {

test("countryParsing"){
    assert(MainClass.countries.size == 247)
}

test("airportParsing"){
    assert(MainClass.airports.size == 46505)
}

test("runwayParsing"){
    assert(MainClass.runways.size == 39536)
}

test("airportTransfer"){
    assert(MainClass.countries.map(x => x.airports_).flatten.size == 46505)
}

test("runwayTransfer"){
    assert(MainClass.airports.map(x => x.runways_).flatten.size == 39536)
}

test("linesToList"){
    assert(Parse.str_to_lines("je\nmange\nhaha\n1\n2") ==
            List("je", "mange", "haha", "1", "2"))
    assert(Parse.str_to_lines("je\nmange\n\n1\n2") ==
            List("je", "mange", "", "1", "2"))
    assert(Parse.str_to_lines("\nmange\nhaha\n1\n2") ==
            List("", "mange", "haha", "1", "2"))
}

test("linesToCountry"){
    val c1 = Parse.str_to_country("302719,\"UA\",\"Ukraine\",\"EU\",\"http:"+
                                "//en.wikipedia.org/wiki/Ukraine\",")
    assert(c1.code_ == "UA")
    assert(c1.name_ == "Ukraine")
    val c2 = Parse.str_to_country("302770,\"MH\",\"Marshall Islands\",\"OC\","+
                        "\"http://en.wikipedia.org/wiki/Marshall_Islands\",")
    assert(c2.code_ == "MH")
    assert(c2.name_ == "Marshall Islands")
    val c3 = Parse.str_to_country("302559,\"BJ\",\"Benin\",\"AF\",\"http:"+
                        "//en.wikipedia.org/wiki/Benin\",")
    assert(c3.code_ == "BJ")
    assert(c3.name_ == "Benin")
    val c4 = Parse.str_to_country("302576,\"GN\",\"Guinea\",\"AF\",\"http:"+
                        "//en.wikipedia.org/wiki/Guinea\",")
    assert(c4.code_ == "GN")
    assert(c4.name_ == "Guinea")
    val c5 = Parse.str_to_country("302633,\"IL\",\"Israel\",\"AS\",\"http:"+
                        "//en.wikipedia.org/wiki/Israel\",")
    assert(c5.code_ == "IL")
    assert(c5.name_ == "Israel")
}

test("linesToAirport"){
    val a1 = Parse.str_to_airport("31701,\"ZYJL\",\"small_airport\","+
        "\"Jilin Airport\",44.002201080322266,126.39600372314453,,\"AS\","+
        "\"CN\",\"CN-22\",\"Jilin\",\"no\",\"ZYJL\",\"JIL\",,,,")
    assert(a1.id_ == 31701)
    assert(a1.iso_country_ == "CN")
    assert(a1.name_ == "Jilin Airport")
}

test("linesToRunways"){
    val r1 = Parse.str_to_runway("258958,27756,\"YMCO\",3372,,\"X\",0,0,"+
                                "\"18\",,,84,,,\"36\",,,76,,")
    assert(r1.id_ == 258958)
    assert(r1.airport_ref_ == 27756)
    assert(r1.surface_ == "X")
    assert(r1.le_ident_ == "18")
}


}
