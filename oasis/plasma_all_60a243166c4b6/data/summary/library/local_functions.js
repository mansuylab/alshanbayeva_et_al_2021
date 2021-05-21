//Set json object for plots
//var pca_total=JSON.parse(pca_total)
//var trimming=JSON.parse(trimming)
//var counting=JSON.parse(counting)
//var mapping=JSON.parse(mapping)

var margin = {top: 30, right: 20, bottom: 50, left: 150},
width = 1000 - margin.left - margin.right,
height = 500 - margin.top - margin.bottom;

// Create PCA plot
nv.addGraph(function() {
	var chart = nv.models.scatterChart()
	.x(function(d) { return d.x })
	.y(function(d) { return d.y })
	.showDistX(false)    //showDist, when true, will display those little distribution lines on the axis.
	.showDistY(false)
	.showLegend(false)
	.margin(margin)
	//.transitionDuration(350)
	.color(d3.scale.category10().range())
	.pointRange([70,70]) // increases the range for the circle inside the scatterplot	;
	
	//Configure how the tooltip looks.
	chart.tooltip.contentGenerator(function(key){
		return '<h3>' + key.series[0].key + '</h3>';
		});
	
	//Axis settings
		// Get labels and variance from json file
	var pc1 = pca_variance[0];
	var pc2 = pca_variance[1];
	chart.xAxis.axisLabel(pc1.key+ " : "+pc1.variance+"%").tickFormat(d3.format('.02f'));
	chart.yAxis.axisLabel(pc2.key+ " : "+pc2.variance+"%").tickFormat(d3.format('.02f'));
	
	
	d3.select('#pca_total svg')
	.datum(pca_total)
	//.transition()			// Allow for transition between point labels
	//.duration(500)		// How long the label is shown after it displays
	.call(chart)
	;
	
	nv.utils.windowResize(chart.update);
	return chart;
});

// Create number of reads removed after trimming (too short or too long)
nv.addGraph(function() {
	var newHeight=height*(trimming[0].values.length/20);
	if(newHeight<500){
		newHeight=500;
	}else{
			newHeight=height*(trimming[0].values.length/20);
	}
	
	var chart = nv.models.multiBarHorizontalChart()
	.x(function(d) { return d.label })
	.y(function(d) { return d.value })
	.showValues(false)
	.margin(margin)
	//.tooltips(true)
	.showControls(false)
	.height(newHeight)
	;
	
	//Configure how the tooltip looks.
	chart.tooltipContent(function(key) {
		tooltip_str = '<h3><center><b>'+key.data.key+'</b></center></h3>' + '<p>Sample name: ' + key.data.label + '</p><p>Number: ' + key.data.value  + '</p><p>Percentage: ' + key.data.percentage + '</p>';
		return tooltip_str;
    });
	
	chart.yAxis.tickFormat(function(d) { return d3.format(',2s')(d) ; });
	d3.select('#trimming svg')
	.datum(trimming)
	.attr("style", "height: ".concat(newHeight))
	.call(chart)
	// This select all the series (options and by default display only one, for which the value is true in visible)
	d3.select("g.nv-legendWrap").selectAll("g.nv-series").filter(function(d) { return d.visible == false; })
	.each(function(d) {
		this.dispatchEvent(new Event("click"));
	});
	
	
	nv.utils.windowResize(chart.update);
	return chart;
});

// Create pie chart of reads belonging to different sRNA species
nv.addGraph(function() {
	var newHeight=height*(trimming[0].values.length/20);
	if(newHeight<500){
		newHeight=500;
	}else{
		newHeight=height*(trimming[0].values.length/20);
	}
	
	var chart = nv.models.multiBarHorizontalChart()
	.x(function(d) { return d.label })
	.y(function(d) { return d.value })
	.showValues(false)          //Show bar value next to each bar
	//.tooltips(true)           //Show tooltips on hover
	//.transitionDuration(350)
	.showControls(false)
	.margin(margin)				//Allow user to switch between "Grouped" and "Stacked" mode
	.height(newHeight)
	;
	
	//Configure how the tooltip looks.
	chart.tooltipContent(function(key) {
		tooltip_str = '<h3><center><b>'+key.data.key+'</b></center></h3>' + '<p>Sample name: ' + key.data.label + '</p><p>Number: ' + key.data.number  + '</p><p>Percentage: ' + key.data.value + '</p>';
		return tooltip_str;
    });
	
	//chart.yAxis.tickFormat(d3.format(',.2f'));
	chart.yAxis.tickFormat(function(d) { return d3.format(',2s')(d) + '%'; });
	
	d3.select('#counting svg')
	.datum(counting)
	.attr("style", "height: ".concat(newHeight))
	.transition().duration(500)
	.call(chart)
	;
	
	// This select all the series (options and by default display only one, for which the value is true in visible)
	d3.select("g.nv-legendWrap").selectAll("g.nv-series")        .filter(function(d) { return d.visible == false; })
	.each(function(d) {
		this.dispatchEvent(new Event("click"));
	});
	
	//.attr("style", "height: ".concat(height*counting[0].values.length/10))
	
	nv.utils.windowResize(chart.update);
	
	return chart;
});

// Create bar graph of number of reads mapped to the genome in multiple samples
nv.addGraph(function() {
	var newHeight=height*(trimming[0].values.length/20);
	if(newHeight<500){
		newHeight=500;
	}else{
		newHeight=height*(trimming[0].values.length/20);
	}
	
	var chart = nv.models.multiBarHorizontalChart()
	.x(function(d) { return d.label })
	.y(function(d) { return d.value })
	.showValues(false)           //Show bar value next to each bar
	//.tooltips(true)             //Show tooltips on hover
	//.transitionDuration(350)
	.showControls(false)
	.height(newHeight)
	.margin(margin)
	;
	
	//Configure how the tooltip looks.
	chart.tooltipContent(function(key) {
		tooltip_str = '<h3><center><b>'+key.data.key+'</b></center></h3>' + '<p>Sample name: ' + key.data.label + '</p><p>Number: ' + key.data.value  + '</p><p>Percentage: ' + key.data.percentage + '</p>';
		return tooltip_str;
    });
	
	chart.yAxis.tickFormat(d3.format(',.3sM'));
	d3.select('#mapping svg')
	.datum(mapping)
	.attr("style", "height: ".concat(newHeight))
	.transition().duration(500)
	.call(chart)
	;
	
	// This select all the series (options and by default display only one, for which the value is true in visible)
	d3.selectAll(".nv-series")
	.filter(function(d) { return d.visible == false; })
	.each(function(d) {
		this.dispatchEvent(new Event("click"));
	});
	//.attr("style", "height: ".concat(height*counting[0].values.length/6))
	
	//var map = document.getElementById('mapping');
	//map.style.height = height;
	//map.style.width = width;
	
	nv.utils.windowResize(chart.update);
	
	return chart;
});
