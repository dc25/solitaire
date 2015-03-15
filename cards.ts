///<reference path="d3.d.ts" />
"use strict";

//Width and height
var w:number = 800;
var h:number = 800;

// Global scale to apply to all cards displayed
var cardScale:number = 1.4;

//Create SVG Element
var svg = d3.select("body")
    .append("svg")
    .attr("width", w)
    .attr("height", h);


function placeCard(name, x:number, y:number) {
    var card = document.getElementById(name);

    // queryString thanks to : http://stackoverflow.com/questions/23034283/is-it-possible-to-use-htmls-queryselector-to-select-by-xlink-attribute-in-an

    var queryString = 'use[*|href="#base"]';
    var base = d3.select(card).select(queryString);

    var xOffset:number = parseInt(base.attr("x"));
    var yOffset:number = parseInt(base.attr("y"));

    svg
        .append("g")
        .attr("transform", function(d, i){ 
        return ""
                + "scale (" + cardScale + ")"
                + "translate (" + 
                  (0      + x/cardScale - xOffset) + "," 
                + (235.27 + y/cardScale - yOffset) + ")" 
        ;
    })
    .each(function(d, i) { 
       this.appendChild(card.cloneNode(true)); 
    });
}

//Import the full deck of cards.
d3.xml("pretty-svg-cards.svg", "image/svg+xml", function(xml) {  

    d3.select("body")
      .append("div")
      .attr("style", "display: none; visibility: hidden")
      .each(function(d, i){ 
           var plane = this.appendChild(xml.documentElement.cloneNode(true)); 
      });

    placeCard("red_joker", 0, 0);
    placeCard("2_club", 150, 0);
    placeCard("king_heart", 300, 300);
    placeCard("black_joker", 450, 200);
    placeCard("queen_spade", 600, 0);
    placeCard("king_heart", 100, 300);
});



