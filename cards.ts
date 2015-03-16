///<reference path="d3.d.ts" />
"use strict";

// Global scale to apply to all cards displayed
var cardScale:number = 0.5
var drag = d3.behavior.drag()
    .on("dragstart", function () { d3.event.sourceEvent.stopPropagation(); })
    .on("drag", dragmove);

// Define drag beavior
function dragmove(d) {
    d.xtranslate += d3.event.dx/cardScale;
    d.ytranslate += d3.event.dy/cardScale;
    d3.select(this).attr("transform", 
                  "scale (" + cardScale + ")"
                + "translate (" +  d.xtranslate + "," +  d.ytranslate + ")" );
}

function placeCard(name:string, x:number, y:number) {
    var card = document.getElementById(name);

    // queryString thanks to : http://stackoverflow.com/questions/23034283/is-it-possible-to-use-htmls-queryselector-to-select-by-xlink-attribute-in-an

    var queryString = 'use[*|href="#base"]';
    var base = d3.select(card).select(queryString);

    var xOffset:number = parseInt(base.attr("x"));
    var yOffset:number = parseInt(base.attr("y"));

    d3.select("body svg")
        .append("g")
        .call(drag)
        .data([{xtranslate:(0      + x/cardScale - xOffset),
                ytranslate:(235.27 + y/cardScale - yOffset)
               }])
        .attr("transform", function(d, i){ 
        return ""
                + "scale (" + cardScale + ")"
                + "translate (" + d.xtranslate + "," +  d.ytranslate + ")" 
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

