function showLightbox() {
        document.getElementById('over').style.display='block';
        document.getElementById('fade').style.display='block';
}
function hideLightbox() {
        document.getElementById('over').style.display='none';
        document.getElementById('fade').style.display='none';
}
function showLightboxDos() {
        document.getElementById('overdos').style.display='block';
        document.getElementById('fadedos').style.display='block';
}
function hideLightboxDos() {
        document.getElementById('overdos').style.display='none';
        document.getElementById('fadedos').style.display='none';
}
function showLightboxTres() {
        document.getElementById('overtres').style.display='block';
        document.getElementById('fadetres').style.display='block';
}
function hideLightboxTres() {
        document.getElementById('overtres').style.display='none';
        document.getElementById('fadetres').style.display='none';
}
function showLightboxCuatro() {
        document.getElementById('overcuatro').style.display='block';
        document.getElementById('fadecuatro').style.display='block';
}
function hideLightboxCuatro() {
        document.getElementById('overcuatro').style.display='none';
        document.getElementById('fadecuatro').style.display='none';
}
function showLightboxCinco() {
        document.getElementById('overcinco').style.display='block';
        document.getElementById('fadecinco').style.display='block';
}
function hideLightboxCinco() {
        document.getElementById('overcinco').style.display='none';
        document.getElementById('fadecinco').style.display='none';
}
function showLightboxSeis() {
        document.getElementById('overseis').style.display='block';
        document.getElementById('fadeseis').style.display='block';
}
function hideLightboxSeis() {
        document.getElementById('overseis').style.display='none';
        document.getElementById('fadeseis').style.display='none';
}

function showQc(){
        document.getElementById('qcresults').style.display = 'block';
        document.getElementById('pngresults').style.display = 'none';   
        document.getElementById('description').style.display = 'none';
        document.getElementById('shownovelmirna').style.display = 'none';       
        
        document.getElementById('showqc').style.color = '#000000';
        document.getElementById('showqc').style.backgroundColor = '#d0dff5';
        
        document.getElementById('showtables').style.color = '#999999';
        document.getElementById('showtables').style.backgroundColor = '#e7e7e7';
        
        document.getElementById('shownovel').style.color = '#999999';   
        document.getElementById('shownovel').style.backgroundColor = '#e7e7e7';

        document.getElementById('showdescription').style.color = '#999999';
        document.getElementById('showdescription').style.backgroundColor = '#e7e7e7';
        
}
function showPngs(){
        document.getElementById('qcresults').style.display = 'none';
        document.getElementById('pngresults').style.display = 'block';  
        document.getElementById('description').style.display = 'none';
        document.getElementById('shownovelmirna').style.display = 'none';       
        
        document.getElementById('showtables').style.color = '#000000';
        document.getElementById('showtables').style.backgroundColor = '#d0dff5';
        
        document.getElementById('showqc').style.color = '#999999';
        document.getElementById('showqc').style.backgroundColor = '#e7e7e7';
        
        document.getElementById('shownovel').style.color = '#999999';
        document.getElementById('shownovel').style.backgroundColor = '#e7e7e7';

        document.getElementById('showdescription').style.color = '#999999';
        document.getElementById('showdescription').style.backgroundColor = '#e7e7e7';
        
}

function showDescription(){
        document.getElementById('qcresults').style.display = 'none';
        document.getElementById('pngresults').style.display = 'none';   
        document.getElementById('description').style.display = 'block'; 
        document.getElementById('shownovelmirna').style.display = 'none';
        
        document.getElementById('showdescription').style.color = '#000000';
        document.getElementById('showdescription').style.backgroundColor = '#d0dff5';
                
        document.getElementById('showqc').style.color = '#999999';
        document.getElementById('showqc').style.backgroundColor = '#e7e7e7';
        
        document.getElementById('showtables').style.color = '#999999';
        document.getElementById('showtables').style.backgroundColor = '#e7e7e7';
        
        document.getElementById('shownovel').style.color = '#999999';
        document.getElementById('shownovel').style.backgroundColor = '#e7e7e7';
}

function showNovelMirna(){
        document.getElementById('qcresults').style.display = 'none';
        document.getElementById('pngresults').style.display = 'none';   
    document.getElementById('description').style.display = 'none'; 
        document.getElementById('shownovelmirna').style.display = 'block';

        document.getElementById('shownovel').style.color = '#000000';
        document.getElementById('shownovel').style.backgroundColor = '#d0dff5';
        
        document.getElementById('showqc').style.color = '#999999';
        document.getElementById('showqc').style.backgroundColor = '#e7e7e7';
        
        document.getElementById('showdescription').style.color = '#999999';
        document.getElementById('showdescription').style.backgroundColor = '#e7e7e7';
        
    document.getElementById('showtables').style.color = '#999999';
    document.getElementById('showtables').style.backgroundColor = '#e7e7e7';
}

function show() {
document.getElementById("speech-bubble").style.display = 'block';
        document.getElementById("speech-bubble").style.marginTop = '-395px';
}
function out() {document.getElementById("speech-bubble").style.display = 'none';}


var pie_charts_svg_options = {
  "size": 300, // size of the contain

  "baseline-radius": 300 * 0.1,
  "arc-step-size": 300 * 0.04,

  "font-size": 12,

  "colors": [ //taken from http://www.somersault1824.com/tips-for-designing-scientific-figures-for-color-blind-readers/
              "rgb( 0, 109, 219 )", //darkish blue
              "rgb( 109, 182, 255 )", //middle bright blue
              "rgb( 182, 219, 255 )", //bright blue
              "rgb( 0, 146, 146 )", //teal
              "rgb( 0, 73, 73 )", //dark green
              "rgb( 73, 0, 146 )", //purple
              "rgb( 182, 109, 255 )", //lilac
              "rgb( 36, 255, 36 )", //greenish color
              "rgb( 219, 209, 0 )", //yellowish green
              "rgb( 255, 182, 119 )", //yellowish orange
              "rgb( 255, 255, 109 )", //yellow
              "rgb( 146, 0, 0 )", //brown
              "rgb( 146, 73, 0 )", //brown
              "rgb( 255, 109, 182 )", //reddish pink
              "rgb( 255, 255, 255 )", //white
              "rgb( 0, 0, 0 )", //black
  ]
}


/**
 * A function that transforms the data from test.json into a structure that is
 * sample-centric. i.e. the result of this function is a list where each entry
 * is a JSON object. Each of these JSON objects has a key "sample_id" which
 * (obviously) is the sample ID. The JSON objects also contain further
 * key-value pairs (see transformed variable in the code of this function).
 * The parameters correspond to the variables in the test.json with the same
 * names.
 *
 * The structure of the result value is:
 *
 * [ {"sample_id": sample_id, key1: value1, key2: value2 }... ]
 */
function extract_sample_information_from_test_json_file_data( trimming, counting, mapping ) {
	function transform( x, subset_key, key, value ) { //x = [ {"key": subset_key, "values": [ {key: extract_this, value: extract_this"} ] } ] where only the records will be processed that match the parameters to transform
		var result = {};
		var found = false;
		var subset = null;
		for( var i = 0; i < x.length; i++ ) {
			subset = x[ i ];
			if( subset.key == subset_key ) {
				found = true;
				break;
			}
		}
		if( !found ) {
			if( typeof console == "object" ) {
				console.log( "Did not find key " + subset_key );
			}
			return null;
		}
		var values = subset.values;
		for( var i = 0; i < values.length; i++ ) {
			var obj = values[ i ];
			result[ obj[ key ] ] = obj[ value ];
		}
		return result;
	}
	var result = []; //each element: {sample_id: x, property1: value1, property2: value2}

	var transformed = {
		"trimming_too_short": transform( trimming, "Too short reads", "label", "value" ),
		"trimming_too_long": transform( trimming, "Too long reads", "label", "value" ),
		"counting_mirna": transform( counting, "mirna", "label", "number" ),
		"counting_pirna": transform( counting, "pirna", "label", "number" ),
		"counting_snorna": transform( counting, "snorna", "label", "number" ),
		"counting_snrna": transform( counting, "snrna", "label", "number" ),
		"counting_rrna": transform( counting, "rrna", "label", "number" ),
		"initial_number_of_reads": transform( mapping, "Initial number of reads", "label", "value" ),
		//"mapping_length_filtering": transform( mapping, "Length filtering", "label", "value" ),
		"mapping_unique": transform( mapping, "Unique mapping", "label", "value" ),
	};

	for( sample_id in transformed.trimming_too_short ) {
		var sample = { "sample_id": sample_id }; //, "total_counts": "FIXME" }; //TODO think of a good general way to determine a value
		for( var prop in transformed ) {
			sample[ prop ] = transformed[ prop ][ sample_id ];
		}
		//sample.mapping_discarded_by_length = sample.mapping_initial - sample.mapping_length_filtering;
		//sample.mapping_non_unique = sample.mapping_initial - sample.mapping_unique;
		sample.trimming_alright = sample.initial_number_of_reads - sample.trimming_too_short - sample.trimming_too_long;
		sample.mapping_non_unique = sample.initial_number_of_reads - sample.mapping_unique;
		result[ result.length ] = sample;
	}
	return result;
}

// Mapped function
function makeManyMultilayerPieCharts(data, chartContainer, titleKey, layerKeys, svg) {
  var container = d3.select("#"+chartContainer) // get the container which will store all pie chart

  // CHANGE!
  // begin change: remove all old divs
  container.selectAll("div.multilayerPieDIV").remove()
  // end change:

  container.selectAll("div.multilayerPieDIV") // make an empty selection to generate divs for each data sample
  .data(data).enter().append("div") // bind data (assumed to be a list of data objects)
  .attr("class", "multilayerPieDIV") // give it class name for corresponding css file
  .attr("id", function(d, i) {return d[titleKey] + "-pie-container"}) // give it unique id
  .append("svg") // add svg environment
  .attr("width", svg.size).attr("height", svg.size) // square size
  .attr("perserveAspectRatio", "xMinYMid meet") // for preserved scaling
  .attr("viewBox", "0 0 " + svg.size + " " + svg.size)
  .each(function(d, i) { // make chart for each
    makeMultilayerPieChart(d, titleKey, layerKeys, svg)
  })
}

// Singular function
function makeMultilayerPieChart(data, titleKey, layerKeys, svg) {


  var x = svg.size / 2 // horizontal center of chart
  var y = svg.size / 2 // vertical center of chart

  // Add additional keys to data for positioning of tooltip
  data.x = x
  data.y = y
  data.layers = layerKeys.length
  data.baselineRadius = svg["baseline-radius"]
  data.arcStepSize = svg["arc-step-size"]

  // select specific pie chart container
  var pieContainer = d3.select("#" + data[titleKey] + "-pie-container")
  var svgElement = pieContainer.select("svg") // grab the svg
  var currentPie = svgElement.append("g").attr("class", "pie").attr("id", data[titleKey]) // add a group element
  currentPie.datum(data) // bind data to specific group

  var colorIndex = 0
  // end change

  // For each layer
  for (var indexOfPieLayer = 0; indexOfPieLayer < layerKeys.length; indexOfPieLayer++) {
    // Extract keys for current layer
    var currentLayerKeys = layerKeys[indexOfPieLayer]
    // Calculate the maximum number of elements in the layer
    var pieTotal = 0
    for (var i = 0; i < currentLayerKeys.length; i++) {
      var currentKey = currentLayerKeys[i]
      pieTotal += data[currentKey]
    }

    // Set the scaling factor
    var scalingFactor = 1 / pieTotal * 360

    // arcs start at 0
    var previousArcPoint = 0

    // for each key in the layer
    for (var indexOfKey = 0; indexOfKey < currentLayerKeys.length; indexOfKey++) {
      var currentKey = currentLayerKeys[indexOfKey]
      // scale current value in proportion to total
      var currentArcPoint = data[currentKey] * scalingFactor

      // add path for the arc
      var arcPath = currentPie.append("path")
        .attr("d", calculateArcPath(x, y, svg["baseline-radius"] + svg["arc-step-size"] * indexOfPieLayer, svg["arc-step-size"], previousArcPoint, previousArcPoint + currentArcPoint))
        .attr("fill", svg.colors[colorIndex]) // give it color
        .attr("stroke", "black") // stroke
        .on("mouseover", arcMouseover) // mouseover / mouseout events
        .on("mouseout", arcMouseout)
        .datum(currentKey) // bind the key the arc segment belongs to

      // update helper variables
      previousArcPoint += currentArcPoint
      colorIndex += 1
    }
  }

  currentPie.append("text")
  .text(data[titleKey])
  .attr("text-anchor", "middle")
  .attr("x", x)
  .attr("y", y + svg["baseline-radius"] + svg["arc-step-size"] * indexOfPieLayer + 2 * svg["font-size"])

  function arcMouseover(d, i) {
    d3.select(this).attr('stroke-width', "2")
    var pieData = d3.select(this.parentNode).datum()
    adjustedY = pieData.y - pieData.baselineRadius - pieData.arcStepSize * pieData.layers - svg["font-size"]

    var tooltipText = [pieData[titleKey], d + ": " + pieData[d]]


    // find layer of current arc
    var keyLayer = 0
    for (var layerIndex = 0; layerIndex < layerKeys.length; layerIndex++) {
      if (layerKeys[layerIndex].includes(d)) {
        keyLayer = layerIndex
      }
    }
    var currentLayerKeys = layerKeys[keyLayer]
    var pieTotal = 0
    for (var i = 0; i < currentLayerKeys.length; i++) {
      var currentKey = currentLayerKeys[i]
      pieTotal += data[currentKey]
    }
    tooltipText.push("Out of: " + pieTotal)

    makeTooltip(pieData.x, adjustedY, tooltipText, svg["font-size"], pieData[titleKey])
  }

  function arcMouseout(d, i) {
    d3.select(this).attr('stroke-width', "1")
    d3.select(this).attr('transform', "scale(1)")
    d3.selectAll("g.tooltipGroup").remove()
  }
}

function calculateArcPath(x, y, radius, spread, startAngle, endAngle){
    var innerStart = polarToCartesian(x, y, radius, endAngle);
  	var innerEnd = polarToCartesian(x, y, radius, startAngle);
    var outerStart = polarToCartesian(x, y, radius + spread, endAngle);
    var outerEnd = polarToCartesian(x, y, radius + spread, startAngle);
    var largeArcFlag = endAngle - startAngle <= 180 ? "0" : "1";

    var d = [
        "M", outerStart.x, outerStart.y,
        "A", radius + spread, radius + spread, 0, largeArcFlag, 0, outerEnd.x, outerEnd.y,
        "L", innerEnd.x, innerEnd.y,
        "A", radius, radius, 0, largeArcFlag, 1, innerStart.x, innerStart.y,
        "L", outerStart.x, outerStart.y, "Z"
    ].join(" ");

    return d;
}

function polarToCartesian(centerX, centerY, radius, angleInDegrees) {
  var angleInRadians = (angleInDegrees-90) * Math.PI / 180.0;
  return {
    x: centerX + (radius * Math.cos(angleInRadians)),
    y: centerY + (radius * Math.sin(angleInRadians))
  };
}

function getToolTipPath(x, y, stringLength, numberOfTextLines, fontsize, pointShift) {
    // Move to x / y (i.e. start at tip of upside down triangle)
    d = "M " + x.toString() + " " + y.toString() + " "

    neededLength = stringLength * fontsize / 2
        // pointShift = 0.05 * neededLength

    // go up and to the left
        // go left
    d += "l -" + (neededLength / 2 + pointShift) + " 0 "
        // curve left and up
    d += "q -" + pointShift + " 0 -" + pointShift + " -" + pointShift + " "
        // go up
    d += "l 0 -" + (numberOfTextLines * fontsize) + " "
        // curve up and right
    d += "q 0 -" + pointShift + " " + pointShift + " -" + pointShift + " "
        // go right
    d += "l " + (pointShift * 2 + neededLength) + " 0 "
        // curve right and down
    d += "q " + pointShift + " 0 " + pointShift + " " + pointShift + " "
        // go down
    d += "l 0 " + (numberOfTextLines * fontsize) + " "
        // curve down and left
    d += "q 0 " + pointShift + " -" + pointShift + " " + pointShift + " "
        /// go left
    d += "l -" + (neededLength / 2) + " 0 z"
    return d

}

function makeTooltip(x, y, textArray, fontsize, whereToAdd) {

    var longestString = d3.max(textArray, function(el) {
        return el.toString().length
    })

    pointShift = 10
    var path = getToolTipPath(x, y, longestString, textArray.length, fontsize, pointShift)

    d3.select("#" + whereToAdd)
        .append("g")
        .attr("class", "tooltipGroup")
        .attr("id", "tooltip")


    var tooltip = d3.select("g.tooltipGroup")
    tooltip.append("path")
        .attr("id", "tooltipPath")
        .attr("d", path)
        .attr("fill", "rgb(51, 51, 51)")
        .attr("stroke", "rgb(51, 51, 51)")
        .attr("opacity", .8)


    // textArray.reverse()
    var toolTipHeight = textArray.length * fontsize + 2 * pointShift + pointShift

    for (var i = 0; i < textArray.length; i++) {

        // -10 because that is hand picked point size
        // 2 * - 10 because one for point and one for bottom curve

        var text = tooltip.append("text")
            .text(textArray[i])
            .attr("font-size", fontsize)
            .attr("text-anchor", "middle")
            .attr("x", x)
            .attr("transform", "translate(0," +  (y - toolTipHeight  +  pointShift + (i+1) * fontsize + fontsize / 2) + ")")
            .attr("class", "tooltipText")
            .attr("fill", "white")
            .attr("font-family", "times")
            .attr("font-weight", "normal")
        if (i > 0) {
            text.attr("fill", "cyan")
                .attr("font-family", "times")
                .attr("font-weight", "normal")
        }
    }
}

var keys_for_rna_species_layers = [["counting_mirna", "counting_pirna", "counting_snorna", "counting_snrna", "counting_rrna"]]
var keys_for_rna_reads_layers = [["trimming_too_short", "trimming_too_long", "trimming_alright"],
		                 ["mapping_unique", "mapping_non_unique"]];

function createPieCharts( keys_for_layers ) { //callback for body-onLoad
	//input parameters to extract_sample_information_from_test_json_file_data come from test.json
	var samples_info = extract_sample_information_from_test_json_file_data( trimming, counting, mapping );

	makeManyMultilayerPieCharts( samples_info, "oasis-d3-multilayer-pie-charts-container", "sample_id", keys_for_layers, pie_charts_svg_options )
}
