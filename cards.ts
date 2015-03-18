///<reference path="d3.d.ts" />
"use strict";

// A & B are required by haste for callbacks.  See: 
// https://github.com/valderman/haste-compiler/blob/master/doc/js-externals.txt
// for details.
var A:any;
var B:any;

// For debugging.
function showAlert_ffi(msg:string) {
        alert(msg);
}

// Global scale to apply to all cards displayed
var cardScale:number = 0.5
var drag = d3.behavior.drag()
    .on("dragstart", function () { d3.event.sourceEvent.stopPropagation(); })
    .on("drag", dragmove)
    .on("dragend", dragend);

// Define drag beavior
function dragmove(d) {
    d.xtranslate += d3.event.dx/cardScale;
    d.ytranslate += d3.event.dy/cardScale;
    d3.select(this).attr("transform", 
                  "scale (" + cardScale + ")"
                + "translate (" +  d.xtranslate + "," +  d.ytranslate + ")" );
}

// Provide for callback into haskell when object stops being dragged.
var dragEndCallback;

// Called from haskell
function setDragEndCallback_ffi(cb) {
    dragEndCallback = cb;
}

// Define dragend behavior - just call back into haskell.
function dragend(d) {

    // additional select("g") because card is nested below dragged object
    var draggedId:string = d3.select(this).select("g").attr("id");

    var coordinates = d3.mouse(this.parentNode);
    var xCoord = coordinates[0];
    var yCoord = coordinates[1];
    
    B(A(dragEndCallback, [[0,draggedId], [0,xCoord], [0,yCoord], 0]));
}

function alignCard_ffi(name:string, classname:string, x:number, y:number) {
    var card = document.getElementById(name);

    // queryString thanks to : http://stackoverflow.com/questions/23034283/is-it-possible-to-use-htmls-queryselector-to-select-by-xlink-attribute-in-an

    var queryString = 'use[*|href="#base"]';
    var base = d3.select(card).select(queryString);

    var xOffset = parseInt(base.attr("x"));
    var yOffset = parseInt(base.attr("y"));

    d3.select('body svg g[data-name="' +name +'"]')
        .attr("class", function(d, i){ 
                   return classname; 
               }
             )
        .data([{xtranslate:(0      + x/cardScale - xOffset),
                ytranslate:(235.27 + y/cardScale - yOffset)
                }]
             )
        .transition()
        .attr("transform", function(d, i){ 
                 return "scale (" + cardScale + ")"
                      + "translate (" + d.xtranslate + "," + d.ytranslate + ")" ;
               }
             )
        ;
}

function placeCard_ffi(name:string, classname:string, x:number, y:number) {
    var card = document.getElementById(name);

    // queryString thanks to : http://stackoverflow.com/questions/23034283/is-it-possible-to-use-htmls-queryselector-to-select-by-xlink-attribute-in-an

    var queryString = 'use[*|href="#base"]';
    var base = d3.select(card).select(queryString);

    var xOffset:number = parseInt(base.attr("x"));
    var yOffset:number = parseInt(base.attr("y"));

    d3.select("body svg")
        .append("g")
        .each(function(d, i) { 
                   this.appendChild(card.cloneNode(true)); 
               }
             )
        .attr("data-name", function(d, i){ 
                   return name; 
               }
             )
        .attr("class", function(d, i){ 
                   return classname; 
               }
             )
        .data([{xtranslate:(0      + x/cardScale - xOffset),
                ytranslate:(235.27 + y/cardScale - yOffset)
                }]
             )
        .attr("transform", function(d, i){ 
                   return "scale (" + cardScale + ")"
                                    + "translate (" + d.xtranslate + "," 
                                                    +  d.ytranslate + ")" ;
               }
             );

    // There must be a better way of enabling drag 
    // for new cards in a visble column.
    if (classname.indexOf("visibleColumn") > -1)
    {
        var selectArg = "g[class=" + classname + "]";
        d3.select(selectArg).call(drag);
    }
}

function loadCards_ffi(cb) {
    //Import the full deck of cards.
    d3.xml("pretty-svg-cards.svg", "image/svg+xml", function(xml) {  

        d3.select("body")
          .append("div")
          .attr("style", "display: none; visibility: hidden")
          .each(function(d, i){ 
               this.appendChild(xml.documentElement.cloneNode(true)); 
          });

        // Call back to haskell when done.
        B(A(cb, [0])); 
    });
}
