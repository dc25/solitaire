///<reference path="d3.d.ts" />
"use strict";

// Global scale to apply to all cards displayed
var cardScale:number = 0.5

function placeCard(name:string, x:number, y:number) {
    var card = document.getElementById(name);

    // queryString thanks to : http://stackoverflow.com/questions/23034283/is-it-possible-to-use-htmls-queryselector-to-select-by-xlink-attribute-in-an

    var queryString = 'use[*|href="#base"]';
    var base = d3.select(card).select(queryString);

    var xOffset:number = parseInt(base.attr("x"));
    var yOffset:number = parseInt(base.attr("y"));

    d3.select("body svg")
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

var A:any;

function loadCards(cb) {
    //Import the full deck of cards.
    d3.xml("pretty-svg-cards.svg", "image/svg+xml", function(xml) {  

        d3.select("body")
          .append("div")
          .attr("style", "display: none; visibility: hidden")
          .each(function(d, i){ 
               this.appendChild(xml.documentElement.cloneNode(true)); 
          });

        A(cb, [0]);
    });
}

