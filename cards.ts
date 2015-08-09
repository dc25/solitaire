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

// For debugging.
function consoleLog_ffi(msg:string) {
    console.log(msg);
}

// Global scale to apply to all cards displayed
var cardScale:number = 0.5

var drag = d3.behavior.drag()
    .on("dragstart", dragstart)
    .on("drag", dragmove)
    .on("dragend", dragend);

function dragstart() { 
    d3.event.sourceEvent.stopPropagation(); 

    var selectArg;

    // Turn off mouseover across the board while dragging
    selectArg = 'g[class*="visible"]';
    d3.selectAll(selectArg).on("mouseover", null)

    selectArg = 'g[class="solitareDeck"]';
    d3.selectAll(selectArg).on("mouseover", null)

    selectArg = 'g[class="hiddenReserves"]';
    d3.selectAll(selectArg).on("mouseover", null)
}

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

    var selectArg;
    // Turn on mouseover for all visible objects when done dragging
    selectArg = 'g[class*="visible"]';
    d3.selectAll(selectArg).on("mouseover", mouseover)

    selectArg = 'g[class="solitareDeck"]';
    d3.selectAll(selectArg).on("mouseover", mouseover)

    selectArg = 'g[class="hiddenReserves"]';
    d3.selectAll(selectArg).on("mouseover", mouseover)

    var dragged = d3.select(this);

    var draggedClassName = dragged.attr("class");
    var draggedId = dragged.select("g").attr("id"); // select("g") because card is nested below dragged object

    var coordinates = d3.mouse(this.parentNode);
    var xCoord = coordinates[0];
    var yCoord = coordinates[1];
    
    dragEndCallback (draggedId, draggedClassName, xCoord, yCoord);
}

// Provide for callback into haskell when mouse passes over object
var mouseoverCallback;

// Called from haskell
function setMouseoverCallback_ffi(cb) {
    mouseoverCallback = cb;
}

// Define mouseover behavior - just call back into haskell.
function mouseover(d,i) {

    var moused = d3.select(this);

    var mousedClassName = moused.attr("class");
    var mousedId = moused.select("g").attr("id");

    var coordinates = d3.mouse(this.parentNode);
    var xCoord = coordinates[0];
    var yCoord = coordinates[1];

    mouseoverCallback(mousedId, mousedClassName, xCoord, yCoord);
}

function placeCard(id:string, name:string, classname:string, x:number, y:number) {

    // Thanks to : 
    // http://stackoverflow.com/questions/10337640/how-to-access-the-dom-element-that-correlates-to-a-d3-svg-object
    // for telling how to use node() to retrieve DOM element from selection.

    // Select to see if the card has already been displayed
    var cardSelect = d3.select('body svg g[data-name="' +name +'"]');

    var card;
    var alreadyDisplayed = ! cardSelect.empty();
    if (! alreadyDisplayed) {
        var documentElement = document.getElementById(id);
        cardSelect 
            = d3.select("body svg")
                .append("g")
                .each(function(d, i) { 
                           this.appendChild(documentElement.cloneNode(true)); 
                       }
                     )
                .attr("data-name", function(d, i){ 
                           return name; 
                       }
                     );
    }

    // queryString thanks to : http://stackoverflow.com/questions/23034283/is-it-possible-to-use-htmls-queryselector-to-select-by-xlink-attribute-in-an
    card = cardSelect.node();
    var base = d3.select(card).select('use[*|href="#base"]');

    var xOffset = parseInt(base.attr("x"));
    var yOffset = parseInt(base.attr("y"));

    cardSelect
        .data([{xtranslate:(0      + x/cardScale - xOffset),
                ytranslate:(235.27 + y/cardScale - yOffset)
                }]
             )
        .attr("class", function(d, i){ return classname; });

    var transformFunction = function(d, i){ 
        return "scale (" + cardScale + ")"
             + "translate (" + d.xtranslate + "," + d.ytranslate + ")" ;
    }

    if (alreadyDisplayed) {
        cardSelect.transition().attr("transform", transformFunction);
    } else {
        cardSelect.attr("transform", transformFunction).on("mouseover", mouseover);

        // There must be a better way of enabling drag 
        // for new cards in a visble column.
        if (    (classname.indexOf("visibleColumn") > -1)
             || (classname == "hiddenReserves")  
             || (classname == "solitareDeck")  )
        {
            var selectArg = "g[class=" + classname + "]";
            d3.selectAll(selectArg).call(drag);
        }
    }
}

function deleteByClass(cssSelection:string) {
    d3.selectAll("." + cssSelection).remove()
}

function loadCards(cb) {
    //Import the full deck of cards.
    d3.xml("pretty-svg-cards.svg", "image/svg+xml", function(xml) {  

        d3.select("body")
          .append("div")
          .attr("style", "display: none; visibility: hidden")
          .each(function(d, i){ 
               this.appendChild(xml.documentElement.cloneNode(true)); 
          });

        // Call back to haskell when done.
        cb();
    });
}
