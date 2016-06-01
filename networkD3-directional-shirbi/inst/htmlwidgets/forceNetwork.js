HTMLWidgets.widget({

  name: "forceNetwork",

  type: "output",

  initialize: function(el, width, height) {

    d3.select(el).append("svg")
        .attr("width", width)
        .attr("height", height);

    return d3.layout.force();
  },

  resize: function(el, width, height, force) {

    d3.select(el).select("svg")
        .attr("width", width)
        .attr("height", height);

    force.size([width, height]).resume();
  },

  renderValue: function(el, x, force) {

  // Compute the node radius  using the javascript math expression specified
    function nodeSize(d) {
            if (options.nodesize) {
                    return eval(options.radiusCalculation);

            } else {
                    return 6}

    }


    // alias options
    var options = x.options;

    // convert links and nodes data frames to d3 friendly format
    var links = HTMLWidgets.dataframeToD3(x.links);
    var nodes = HTMLWidgets.dataframeToD3(x.nodes);
    
    // get the width and height
    var width = el.offsetWidth;
    var height = el.offsetHeight;

    var color = eval(options.colourScale);

    // set this up even if zoom = F
    var zoom = d3.behavior.zoom();

    // create d3 force layout
    force
      .nodes(d3.values(nodes))
      .links(links)
      .size([width, height])
      .linkDistance(options.linkDistance)
      .charge(options.charge)
      .on("tick", tick)
      .start();
    // thanks http://plnkr.co/edit/cxLlvIlmo1Y6vJyPs6N9?p=preview
    //  http://stackoverflow.com/questions/22924253/adding-pan-zoom-to-d3js-force-directed
    //  var drag = force.drag()
    //    .on("dragstart", dragstart)
      // allow force drag to work with pan/zoom drag
    //  function dragstart(d) {
    //    d3.event.sourceEvent.preventDefault();
    //    d3.event.sourceEvent.stopPropagation();
    //  }
    var node_drag = d3.behavior.drag()
        .on("dragstart", dragstart)
        .on("drag", dragmove)
        .on("dragend", dragend);
    function dragstart(d, i) {
        force.stop() // stops the force auto positioning before you start dragging
    }
    function dragmove(d, i) {
        d.px += d3.event.dx;
        d.py += d3.event.dy;
        d.x += d3.event.dx;
        d.y += d3.event.dy;
    }
    function dragend(d, i) {
        d.fixed = true; // of course set the node to fixed so the force doesn't include the node in its auto positioning stuff
        force.resume();
    }
    function releasenode(d) {
        d.fixed = false; // of course set the node to fixed so the force doesn't include the node in its auto positioning stuff
        //force.resume();
    }
    
    // select the svg element and remove existing children
    var svg = d3.select(el).select("svg");
    svg.selectAll("*").remove();
    // add two g layers; the first will be zoom target if zoom = T
    //  fine to have two g layers even if zoom = F
    svg = svg
        .append("g").attr("class","zoom-layer")
        .append("g")

    // add zooming if requested
    if (options.zoom) {
      function redraw() {
        d3.select(el).select(".zoom-layer").attr("transform",
          "translate(" + d3.event.translate + ")"+
          " scale(" + d3.event.scale + ")");
      }
      zoom.on("zoom", redraw)

      d3.select(el).select("svg")
        .attr("pointer-events", "all")
        .call(zoom);

    } else {
      zoom.on("zoom", null);
    }

    if (options.arrows) {
    // draw links with arrows
    
    var link = svg.selectAll(".link")
        .data(force.links())
        .enter().append("line")
        .attr("class", "link")
        .attr("source", function(d) { return d.source.name; })
        .attr("value", function(d) { return d.value; })
        .style("stroke", function(d) { return d.colour ; })
        .style("opacity", 0.05)
        .style("stroke-width", eval("(" + options.linkWidth + ")"))
        .style("marker-end",  "url(#end)") // Modified line
        .on("mouseover", function(d) {
            d3.select(this)
              .style("opacity", 1);
        })
        .on("mouseout", function(d) {
            d3.select(this)
              .style("opacity", 0.05);
        });
        
    var linkText = svg.selectAll(".link")
        .append("text")
        .attr("class", "link-label")
        .attr("font-family", "Arial, Helvetica, sans-serif")
        .attr("fill", "Black")
        .style("font", "normal 12px Arial")
        .style("opacity", 1)
        .attr("dy", ".35em")
        .attr("text-anchor", "middle")
        .text(function(d) {
            return d.value;
     });
    }

    else {
      // draw links without arrows
      var link = svg.selectAll(".link")
        .data(force.links())
        .enter().append("line")
        .attr("class", "link")
        .attr("source", function(d) { return d.source.name;})
        .style("stroke", function(d) { return d.colour ; })
        //.style("stroke", options.linkColour)
        .style("opacity", 0.05)
        //.style("stroke-width", eval("(" + options.linkWidth + ")"))
        .style("stroke-width", options.linkWidth)

        .on("mouseover", function(d) {
            d3.select(this)
              .style("opacity", 1);
        })
        .on("mouseout", function(d) {
            d3.select(this)
              .style("opacity", 0.05)
        });
        
      link.append("svg:text")
      .attr("class", "linktext")
      .attr("dx", 12)
      .attr("dy", ".35em")
      .text(function(d) { console.log(d.value); return d.value })
      .style("font", options.fontSize + "px " + options.fontFamily)
      .style("opacity", options.opacityNoHover)
      .style("pointer-events", "none");
    }

    //var associated_links = svg.selectAll("line")
    //    filter(function(d) { return d.source.index == i })
    //    .style("opacity", 1)
    
    // draw nodes
    var node = svg.selectAll(".node")
      .data(force.nodes())
      .enter().append("g")
      .attr("class", "node")
      .attr("name", function(d) { return d.name;})
      .style("fill", function(d) { return color(d.group); })
      .style("opacity", options.opacity)
      .on("mouseover", mouseover)
      .on("mouseout", mouseout)
      .on('dblclick', releasenode)
      .call(node_drag); //Added 

    node.append("circle")
      .attr("r", function(d){return nodeSize(d);})
      .attr("name", function(d) { return d.name;})
      .style("stroke", "#fff")
      .style("opacity", 1)
      .style("stroke-width", "1.5px");

    node.append("svg:text")
      .attr("class", "nodetext")
      .attr("dx", 12)
      .attr("dy", ".35em")
      .text(function(d) { return d.name })
      .style("font", options.fontSize + "px " + options.fontFamily)
      .style("opacity", 1)
      .style("pointer-events", "none");

    function tick() {
      node.attr("transform", function(d) {
        if(options.bounded){ // adds bounding box
            d.x = Math.max(nodeSize(d), Math.min(width - nodeSize(d), d.x));
            d.y = Math.max(nodeSize(d), Math.min(height - nodeSize(d), d.y));
        }

        return "translate(" + d.x + "," + d.y + ")"});

      link
        .attr("x1", function(d) { return d.source.x; })
        .attr("y1", function(d) { return d.source.y; })
        .attr("x2", function(d) { return d.target.x; })
        .attr("y2", function(d) { return d.target.y; });
        
      linkText
        .attr("x", function(d) {
            return ((d.source.x + d.target.x)/2);
        })
        .attr("y", function(d) {
            return ((d.source.y + d.target.y)/2);
        });
    }
    
    function mouseover() {
      d3.select(this).select("circle").transition()
        .duration(750)
        .attr("r", function(d){return nodeSize(d)+5;})
      d3.select(this).select("text").transition()
        .duration(750)
        .attr("x", 13)
        .style("stroke-width", ".5px")
        .style("font", options.clickTextSize + "px ")
        .style("opacity", 1);
      var node_name = d3.select(this).attr("name");
      d3.selectAll("line").filter(function(d){ 
        //console.log(node_name);
        //console.log(d.source.name); 
        //console.log(d.source.name == node_name); 
        return (d.source.name == node_name);})//.each(function(p){ 
            //d3.select(this)
            .style("opacity", 1);
    }

    function mouseout() {
      d3.select(this).select("circle").transition()
        .duration(750)
        .attr("r", function(d){return nodeSize(d);});
      d3.select(this).select("text").transition()
        .duration(1250)
        .attr("x", 0)
        .style("font", options.fontSize + "px ")
        .style("opacity", options.opacityNoHover);
      var node_name = d3.select(this).attr("name");
      d3.selectAll("line").filter(function(d){ 
        return (d.source.name == node_name);})//.each(function(p){ 
            //d3.select(this)
            .style("opacity", 0.05);