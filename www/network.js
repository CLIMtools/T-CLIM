
if(!container_dimensions) {
	var container_dimensions = { width: 1000, height: 800},
	    margins = { top: 30, right: 25, bottom: 50, left: 0},
	    legend = 150,voffset = 15,hoffset = 15,
	    chart_dimensions = {
	            width: (container_dimensions.width)-margins.left-margins.right-legend,
	            height: (container_dimensions.height)-margins.top-margins.bottom
	    };
}




network = function(nodes, links, 
	nodeName, nodeInnerColour, nodeOuterColour, nodeSize, 
	linkValue, linkColour, linkWidth, linkDistance, 
	charge, parentElement){
	
	// force directed graph and svg parameters
	charge = charge || -300
	linkDistance = linkDistance || 100
	parentElement = parentElement || "body"
	
	// node parameters
	nodeSize = nodeSize || 8
	
	// link parameters
	//linkWidth = linkWidth || ""




// minimum force directed graph layout
	var svg = d3.select(parentElement).append("svg")
		.attr("width", container_dimensions.width)
		.attr("height", container_dimensions.height);
	
	var force = d3.layout.force()
	  .nodes(nodes)
	  .links(links)
	  .size([container_dimensions.width, container_dimensions.height])
	  .linkDistance(function(d) { return (Math.pow(1-Math.abs(d.value),2)+0.01) * linkDistance; }) // //
	  .charge(charge)
	  .start();
	  
	var drag = force.drag()
		.on("dragstart", dragstart);
	  
	// link build
	var link = svg.selectAll(".link")
      	.data(links)
    	.enter().append("g")
		.attr("class", "link");

	var link_line = link.append("line");

	var link_text = link.append("text")
			.attr("class","linktext")
			.attr("dy", ".35em");


	// node build
	var node = svg.selectAll(".node")
	  .data(nodes)
	.enter().append("g")
	  .attr("class", "node")
	  .on("mouseover", nodeMouseover)
	  .on("mouseout", nodeMouseout)
      .on("dblclick", dblclick)
	  .call(drag);
	
	var node_circle = node.append("circle")
	.attr("r", nodeSize);
	
	
	function nodeMouseover() {
		d3.select(this).select("circle").transition()
			.duration(750)
			.attr("r", nodeSize*2);
		d3.select(this).select("text").transition()
			.duration(750)
			.attr("x", nodeSize);
	}

	function nodeMouseout() {
		d3.select(this).select("circle").transition()
		  .duration(750)
		  .attr("r", nodeSize);
		d3.select(this).select("text").transition()
		  .duration(750)
		  .attr("x", 0);
	}


	force.on("tick", function() {
		
		/*link
		.attr("x1", function(d) { return d.source.x; })
		.attr("y1", function(d) { return d.source.y; })
		.attr("x2", function(d) { return d.target.x; })
		.attr("y2", function(d) { return d.target.y; });*/

		link_line
			.attr("x1", function(d) { return d.source.x; })
			.attr("y1", function(d) { return d.source.y; })
			.attr("x2", function(d) { return d.target.x; })
			.attr("y2", function(d) { return d.target.y; });
			
		link_text
			.attr("dx", function(d) { return (d.source.x+d.target.x)/2; })
			.attr("dy", function(d) { return (d.source.y+d.target.y)/2; })
		
		node.attr("transform", function(d) { return "translate(" + d.x + "," + d.y + ")"; });
	})
	
	
	function dblclick(d) {
	  d3.select(this).classed("fixed", d.fixed = false);
	}

	function dragstart(d) {
	  d3.select(this).classed("fixed", d.fixed = true);
	}

// optional annotation
	
	var y_offset = 50;
	if(nodeName){
		var node_text = node.append("svg:text")
			.attr("class","nodetext")
			.attr("dx", 1.5*nodeSize)
			.attr("dy", ".35em")
			.text(function(d){return d[nodeName]});
	}

	if(nodeInnerColour){	
		var innerNode_range = ["grey","dodgerblue","#ff7f0e","#9467bd","#2ca02c","#d62728","#8c564b","#e377c2","#7f7f7f","#bcbd22","#17becf","#1f77b4"];
		c_nodeInnerColour = colour_scale(nodes,nodeInnerColour,innerNode_range);
		node_circle.style('fill',function(d) { return c_nodeInnerColour(d[nodeInnerColour])} );
		y_offset = draw_legend(name="nodeInnerColour", col_scale=c_nodeInnerColour, title="Inner Node Colour", svg);
	}
	
	if(nodeOuterColour){
		var outerNode_range = ["white","black","#1f77b4","#bcbd22","#7f7f7f","#e377c2","#8c564b","#d62728","#2ca02c","#9467bd","#ff7f0e","#17becf"];
		c_nodeOuterColour = colour_scale(nodes,nodeOuterColour,outerNode_range);
		node_circle.style('stroke',function(d) { return c_nodeOuterColour(d[nodeOuterColour])} );
		y_offset = draw_legend(name="nodeOuterColour", col_scale=c_nodeOuterColour, title="Outer Node Colour", svg, y_offset);
	}
	
	if(linkValue){
		l_linkValue = two_dec_if_num(links,linkValue)
		link_text.text(function(d) { return l_linkValue(d) ; })
		if(linkWidth){
			link_line.style("stroke-width", function(d) {return linkWidth*(0.01+Math.pow(Math.abs(d[linkValue]),2));} ); // unflexible coding
		}
	}
	
	if(linkColour){
		c_linkColour = colour_scale(links,linkColour,["red","green"]);
		link_line.style('stroke',function(d) { return c_linkColour(Math.sign(d[linkColour]))} );
		//y_offset = draw_legend(name="linkColour", col_scale=c_linkColour, title="Link Colour", svg, y_offset);
	}
	

	//(Math.pow(1-Math.abs(d.value),2)+0.01) * linkDistance
}


function colour_scale(data,keyName, colour_range) {
	var col_extent = d3.extent(data,function(d) { return d[keyName] });
	if(typeof(col_extent[0])==typeof('') || typeof(col_extent[0])==typeof(true)) {
				var col_unique = d3.set(data.map(function(d) { return d[keyName] })).values().sort();
				var col_scale = d3.scale.ordinal()
						.domain(col_unique)
						.range(colour_range);
			} else {
				var col_scale = d3.scale.linear()
						.domain(col_extent)
						.range(colour_range)
						.interpolate(d3.interpolateLab);
			}
	return col_scale ;
}


function two_dec_if_num(data,keyName) {
	var val_extent = d3.extent(data,function(d) { return d[keyName] });
	if(typeof(val_extent[0])==typeof('') || typeof(val_extent[0])==typeof(true)) {
				var val_return = function(d) { return d[keyName]; } ;
			} else {
				var val_return = function(d) { return Math.round(100*d[keyName])/100; } ;
			}
	return val_return ;
}


function draw_legend(name, col_scale, title, svg, y_offset, x_offset){

	x_offset = x_offset || 50
	y_offset = y_offset || 50
	
	//y_offset = y_offset  + (legendRectSize + legendSpacing)*col_scale.domain().length / 2
	
	var legendRectSize = 18,
		legendSpacing = 4;
		
	y_offset = y_offset  + (legendRectSize + legendSpacing)*col_scale.domain().length / 2
	
	var Colour_legend = svg.append("g")
			.attr("class", name.concat('_legend'))
			.attr("transform","translate(" + x_offset + "," + y_offset + ")");
			
		Colour_legend.append("rect")
			.attr("class", "legend_box");
			
		Colour_legend.append("text")
			.attr("class", "legend_title")
			.attr("x", -2 * legendRectSize)
			.attr("y", -(5 + (legendRectSize + legendSpacing)*col_scale.domain().length / 2))
			.text(title) ;
			
		var legend_items = Colour_legend.selectAll(".legend_items")
			.data(col_scale.domain())
			.enter()
			.append("g")
			.attr("class", "legend_items").attr('transform', function(d, i) {
				var height = legendRectSize + legendSpacing;
				var offset =  height * col_scale.domain().length / 2;
				var horz = -2 * legendRectSize;
				var vert = i * height - offset;
				return 'translate(' + horz + ',' + vert + ')';
			}) ;
			
		legend_items.append('rect')
			.attr('width', legendRectSize)
			.attr('height', legendRectSize)
			.style('fill', col_scale);
			//.style('stroke', col_scale);
			
		legend_items.append('text')
			.attr('x', legendRectSize + legendSpacing)
			.attr('y', legendRectSize - legendSpacing)
			.text(function(d) { return d; });
			
		return y_offset + 30 + (legendRectSize + legendSpacing) * col_scale.domain().length/2 
  }

