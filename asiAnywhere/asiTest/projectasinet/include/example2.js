/* © 2009 ROBO Design
 * http://www.robodesign.ro
 */

// Keep everything in anonymous function, called on window load.
if(window.addEventListener) {
    window.addEventListener('load', function() {
        var canvas, context, tool;

        function init() {
            // Find the canvas element.
            canvas = document.getElementById('imageView');
            if (!canvas) {
                alert('Error: I cannot find the canvas element!');
                //return;
            }

            if (!canvas.getContext) {
                alert('Error: no canvas.getContext!');
                //return;
            }

            // Get the 2D canvas context.
            context = canvas.getContext('2d');
            if (!context) {
                alert('Error: failed to getContext!');
                //return;
            }

            var is_touch_device = 'ontouchstart' in document.documentElement;

            if (is_touch_device) {
                // create a drawer which tracks touch movements
                var drawer = {
                    isDrawing: false,
                    touchstart: function(coors) {
                        context.beginPath();
                        context.moveTo(coors.x, coors.y);
                        this.isDrawing = true;
                    },
                    touchmove: function(coors) {
                        if (this.isDrawing) {
                            context.lineTo(coors.x, coors.y);
                            context.stroke();
                        }
                    },
                    touchend: function(coors) {
                        if (this.isDrawing) {
                            this.touchmove(coors);
                            this.isDrawing = false;
                        }
                    }
                };

                // create a function to pass touch events and coordinates to drawer
                function draw(event) {

                    // get the touch coordinates.  Using the first touch in case of multi-touch
                    var coors = {
                        x: event.targetTouches[0].pageX,
                        y: event.targetTouches[0].pageY
                    };

                    // Now we need to get the offset of the canvas location
                    var obj = canvas;

                    if (obj.offsetParent) {
                        // Every time we find a new object, we add its offsetLeft and offsetTop to curleft and curtop.
                        do {
                            coors.x -= obj.offsetLeft;
                            coors.y -= obj.offsetTop;
                        }
                        // The while loop can be "while (obj = obj.offsetParent)" only, which does return null
                        // when null is passed back, but that creates a warning in some editors (i.e. VS2010).
                        while ((obj = obj.offsetParent) != null);
                    }

                    // pass the coordinates to the appropriate handler
                    drawer[event.type](coors);
                }


                // attach the touchstart, touchmove, touchend event listeners.
                canvas.addEventListener('touchstart', draw, false);
                canvas.addEventListener('touchmove', draw, false);
                canvas.addEventListener('touchend', draw, false);

                // prevent elastic scrolling
                canvas.addEventListener('touchmove', function(event) {
                    event.preventDefault();
                }, false);
            }
            else {
                
                // Pencil tool instance.
                tool = new tool_pencil();

                // Attach the mousedown, mousemove and mouseup event listeners.
                canvas.addEventListener('mousedown', ev_canvas, false);
                canvas.addEventListener('mousemove', ev_canvas, false);
                canvas.addEventListener('mouseup', ev_canvas, false);
            }


        }

        // This painting tool works like a drawing pencil which tracks the mouse 
        // movements.
        function tool_pencil() {
            var tool = this;
            this.started = false;

            // This is called when you start holding down the mouse button.
            // This starts the pencil drawing.
            this.mousedown = function(ev) {
                context.beginPath();
                context.moveTo(ev._x, ev._y);
                tool.started = true;                
            };

            // This function is called every time you move the mouse. Obviously, it only 
            // draws if the tool.started state is set to true (when you are holding down 
            // the mouse button).
            this.mousemove = function(ev) {
                if (tool.started) {
                    context.lineTo(ev._x, ev._y);
                    context.stroke();
                }
            };

            // This is called when you release the mouse button.
            this.mouseup = function(ev) {
                if (tool.started) {
                    tool.mousemove(ev);
                    tool.started = false;
                }
            };
        }

        // The general-purpose event handler. This function just determines the mouse 
        // position relative to the canvas element.
        function ev_canvas(ev) {
            if (ev.layerX || ev.layerX == 0) { // Firefox
                ev._x = ev.layerX;
                ev._y = ev.layerY;
            } else if (ev.offsetX || ev.offsetX == 0) { // Opera
                ev._x = ev.offsetX;
                ev._y = ev.offsetY;
            }

            // Call the event handler of the tool.
            var func = tool[ev.type];
            if (func) {
                func(ev);
            }
        }



        init();

    }, false); }

// vim:set spell spl=en fo=wan1croql tw=80 ts=2 sw=2 sts=2 sta et ai cin fenc=utf-8 ff=unix:

