<!-- 
	WebSignatureCapture (WEBSIGN) copyright © 2008 - 2009 www.websignaturecapture.com
	Contact: info@websignaturecapture.com
	This code is not a freeware. You are not authorized to distribute
	or use it if you have not purchased. Please visit
	http://www.websignaturecapture.com to buy it
-->
<html>
  <head>
    <title>Signature</title>
    <script language=javascript>
		if (document.layers)
			document.captureEvents(Event.MOUSEOVER | Event.MOUSEOUT | Event.MOUSEDOWN)
			
			document.oncontextmenu	=	new Function("return false");
			
			if (self == top)
			 { 
				 window.location.href = './';
			 }
</script>

<style>
	body{
		cursor : hand;
		padding:0;
		margin:0;
		width:100%;
		height:100%;
	}
	#pointer{
		position:absolute;
		background:#000;
		width:3px;
		height:3px;
		font-size:1px;
		z-index:32768;
	}
</style></head><body onselectstart="return false">
<form name="fm" method="post" action="Signature.aspx">
<input id="l_x" name="l_x" type="hidden">
<input id="l_y" name="l_y" type="hidden">
<input id="l_Width" name="l_Width" type="hidden">
<input id="l_Color" name="l_Color" type="hidden">
<input id="l_BGColor" name="l_BGColor" type="hidden">
<input id="l_File" name="l_File" type="hidden">
<input id="l_CanvasW" name="l_CanvasW" type="hidden">
<input id="l_CanvasH" name="l_CanvasH" type="hidden">
<input id="l_SavePath" name="l_SavePath" type="hidden">
</form>
<noscript>
	    <META http-equiv="refresh" content="1;URL=./"> 
</noscript>

<script language=javascript>

var noSign = "";
var IsValid = false;
document.getElementById("l_Width").value = getQueryVariable("PWidth");
document.getElementById("l_Color").value = getQueryVariable("PColor");
document.getElementById("l_BGColor").value = getQueryVariable("BGColor");
document.getElementById("l_File").value = getQueryVariable("SignFile");
document.getElementById("l_CanvasW").value = getQueryVariable("CanvasW");
document.getElementById("l_CanvasH").value = getQueryVariable("CanvasH");
noSign = removespace(getQueryVariable("NoSign"));
document.getElementById("l_SavePath").value = getQueryVariable("SSavePath");

document.bgColor = document.getElementById("l_BGColor").value;

function removespace(inputstr)
{
	posn = inputstr.indexOf("%20");                    //find the first %20 
	 while (posn > -1)                                  //while there is a %20 
     { inputstr = inputstr.substring(0,posn) + " " + inputstr.substring(posn+3); 
       posn = inputstr.indexOf("%20");              //find next %20 
     }; 	
     
     return inputstr;
}

function save(){
    
    var buf_x="";
	var buf_y="";
	with(JSD_CONTROL){
		for(i=0;i<canvas.line_number;i++){
			buf_x += LogX[i] + "|"
			buf_y += LogY[i] + "|"
		}
	}
	document.getElementById("l_x").value = buf_x;
	document.getElementById("l_y").value = buf_y;
	
	if(buf_x.length == 0 || buf_y.length == 0)
	{
	   alert(noSign);
	   IsValid = false;
	}
	else
	{	
	   IsValid = true;
	   document.fm.submit();
	}
}

function RefreshImage()
{
  alert('refreshing..');
}

function getQueryVariable(variable)
{
var query = window.location.search.substring(1);
var vars = query.split("&");
for (var i=0;i<vars.length;i++)
{
var pair = vars[i].split("=");
if (pair[0] == variable)
{
return pair[1];
}
}
} 

</script>

<script>

cache_obj = new Object();
recent_dot = 0;

nobasi_x = 0;
nobasi_y = 0;
dot_num = 0;
IE = (navigator.appName == "Microsoft Internet Explorer")?1:0;
if (window.opera){IE = 0}
var mpath   = '';

var KC = new Object();
KC = {
	Z : 90,
	X : 88,
	Q : 81,
	A : 65,
	plus  : 107,
	minus : 109
}
var USE_VML = 1;



function jsd_canvas(o){
	this.canvasObj = o.canvasObj || document.body;

	this.nowX = 0;
	this.nowY = 0;
	
	this.PlotX = new Array();
	this.PlotY = new Array();
	
	this.kitenX = 0;
	this.kitenY = 0;

	this.isIE = (navigator.appName == "Microsoft Internet Explorer")?1:0;
	this.line_number = 0;
	return this;
}

function jsd_brush(o){
	this.size  =  o.size || 3;
	this.color =  o.color || "#F00";
	return this;
}

function jsd_control(o){
	this.canvas = o.canvas;
	this.brush  = o.brush;
	this.nowX = 0;
	this.nowY = 0;
	this.mdown = 0;
	this.dot = document.createElement("span", "Dot");
	with(this.dot.style){
		fontSize = "0px";
		background = getQueryVariable('PColor');
		position = "absolute";
		zIndex = "10000";
	}
	
	this.LogX = new Array();
	this.LogY = new Array();
	return this;
}


jsd_control.prototype = {
	launch : function (){
	
		with (this){
			canvas.pointerObj = document.getElementById("pointer");
			/* js error setBrush(brush.size,brush.color); */
		}
	},
	mousedown : function(e){
		this.mdown = 1;
		this.LogX[this.canvas.line_number] = "";
		this.LogY[this.canvas.line_number] = "";
		this.PastX = e.pageX;
		this.PastY = e.pageY;
	},
	mouseup   : function(){
		this.mdown = 0;
		this.canvas.line_number++;
		this.set_next();
	},

	set_next : function (){
		with(this.canvas){
			var div = document.createElement("div");
			div.id = "l" + line_number;
		
			document.body.appendChild(div);
			cache_obj[line_number] = div;
			dot_num = 0;
			window.status = "n" + line_number + "";
		}
	},
	logging  : function (){
		
	},
	
	/* js error
	setBrush : function (Size,Color){
		with(this.canvas.pointerObj.style){
			width =  Size + "px";
			height = Size + "px";
			background = Color ;
		}
		with (this.dot.style){
			width =  Size + "px";
			height = Size + "px";
		}
	},
    */

	line     : function (X,Y){
		var xMove = X - this.PastX;
		var yMove = Y - this.PastY;
		var xDecrement = xMove < 0 ? 1 : -1;
		var yDecrement = yMove < 0 ? 1 : -1;
		var b_dot = 1;
		var count = 0;
		if (Math.abs(xMove) >= Math.abs(yMove)){
			for (var i = xMove; i != 0; i += xDecrement){
				count++;
				if(count % b_dot == 0){
					PlotX[PlotX.length] = X - i;
					PlotY[PlotY.length] = Y - Math.round(yMove * i / xMove);
				}
			}
		}else{
			for (var i = yMove; i != 0; i += yDecrement){
				count++;
				if(count % b_dot == 0){
					PlotX[PlotX.length] = X - Math.round(xMove * i / yMove);
					PlotY[PlotY.length] = Y - i;
				}
			}
		}
		for(var i=0;i<PlotX.length;i++){
			this.drawDot(PlotX[i],PlotY[i])
		}
		PlotX=new Array();
		PlotY=new Array();
		this.PastX = X; this.PastY = Y;
	},
	drawDot : function(x,y){
		
		line_number = JSD_CONTROL.canvas.line_number;
		if (recent_dot && this.kitenY == y && !nobasi_y){
			recent_dot.style.width = Math.abs(this.kitenX - x) + 1 + "px";
			
			if(this.kitenX > x){
				recent_dot.style.left = x + "px";
			}
			nobasi_x = 1;
			nobasi_y = 0;
		}else if(recent_dot && this.kitenX == x && !nobasi_x){
			recent_dot.style.height = Math.abs(this.kitenY - y) + 1 + "px";
		
			if(this.kitenY > y){
				recent_dot.style.top = y + "px";
			}
			nobasi_x = 0;
			nobasi_y = 1;
		}else{
			var dot = this.dot.cloneNode(true);
			with(dot.style){
				left = x + "px";
				top  = y + "px";
			}
			recent_dot = dot;
			cache_obj[line_number].appendChild(dot);

			this.kitenX = x;
			this.kitenY = y;
			nobasi_x = 0;
			nobasi_y = 0;
		}

        /* fix for safari */
        		
		if(recent_dot.style.width == "")
		    recent_dot.style.width = getQueryVariable('PWidth') + "px";
		    
		if(recent_dot.style.height == "")
		    recent_dot.style.height = getQueryVariable('PWidth') + "px"; 

	},
	mousemove : function (e,obj){
		with(obj){
			if(USE_VML){
				status = canvas.line_number + ":" + nowX + "," + nowY;
			}
			nowX = e.pageX;
			nowY = e.pageY;
			if(this.mdown){
				this.LogX[canvas.line_number] += nowX + ",";
				this.LogY[canvas.line_number] += nowY + ",";
				line(nowX,nowY);
			}
		}
		/* js error
		with(obj){
			pointerObj.style.left = nowX - (brush.size / 2) + "px";
			pointerObj.style.top  = nowY - (brush.size / 2) + "px";
		}
		*/
	},
	keydown : function (e,obj){
		var key = e.keyCode || e.which;
		var bold = function (){
			with(obj.brush){size++;obj.setBrush(size,color);}
		};
		var thin = function (){
			with(obj.brush){
				if(size > 1)size--;obj.setBrush(size,color);}
			};
		var back = function (){
				with(obj.canvas){
					if(line_number > 0){
					line_number--;
					var re = document.getElementById('l'+ (line_number));
					canvasObj.removeChild(re);
					line_number--;
					obj.mouseup();
					}
				}
			};
		switch(key){
			case KC.Z:back();break;
			case KC.Q:bold();break;
			case KC.plus:bold();break;
			case KC.A:thin();break;
			case KC.minus:thin();break;
		}
	},
	addEvent : function(obj, type, listener) {
		if (obj.addEventListener) // Std DOM Events
			obj.addEventListener(type, listener, false);
		else if (obj.attachEvent) // IE
			obj.attachEvent(
				'on' + type,
				function() { listener( {
					type            : window.event.type,
					keyCode         : window.event.keyCode,
					target          : window.event.srcElement,
					currentTarget   : obj,
					clientX         : window.event.clientX,
					clientY         : window.event.clientY,
					pageX           : document.body.scrollLeft+ window.event.clientX,
					pageY           : document.body.scrollTop + window.event.clientY,
					shiftKey        : window.event.shiftKey,
					stopPropagation : function() { window.event.cancelBubble = true }
				} ) }
			);
	}
};


if (IE && USE_VML){
	IE_draw = new Object();
	IE_draw.prototype = {
		set_next : function(){
			mpath = '';
			var w='<v:shape id="vml_line'
			+ this.canvas.line_number
			+ '" filled="false" strokecolor="' + this.brush.color + '"'
			+ ' strokeweight="' + this.brush.size + 'px" style="behavior:url(#default#VML);'
			+ ' visibility:visible;position:absolute;left:0;top:0;width:100;height:100;antialias:false;"'
			+ ' coordsize="100,100" coordorigin="0, 0" >'
			+ '<v:path v="m 0,0 l 100,100 200,50 x e"/></v:shape>';
			var sp = document.createElement("span");
			sp.innerHTML = w;
			sp.id = "l" + this.canvas.line_number;
			document.body.appendChild(sp);
			cache_obj[this.canvas.line_number] = document.getElementById('vml_line'+ this.canvas.line_number);
		},
		line : function (x,y){
			if(mpath){
				mpath += x + ',' + y + ' ';
			}else{
				mpath = x + ',' + y + ' l ';
			}
			var v = cache_obj[this.canvas.line_number];
			v.path = 'm ' + mpath + ' ';
			v.strokeweight = this.brush.size + "px";
		}
	};
	force_inherit(jsd_control,IE_draw);
}

var JSD_CONTROL = new jsd_control({
	canvas :jsd_canvas({
			
		}),
	brush  :jsd_brush({
			size:getQueryVariable('PWidth'),
			color:"'" + getQueryVariable('PColor') + "'" 
		})
});
window.onload = function(){
	JSD_CONTROL.set_next();
	with(JSD_CONTROL){
		addEvent(canvas.canvasObj,'mousedown',function(e){mousedown(e,JSD_CONTROL)})
		addEvent(document,'keydown',function(e){keydown(e,JSD_CONTROL)})
		addEvent(canvas.canvasObj,'mouseup',  function(e){mouseup(e,JSD_CONTROL)})
		//addEvent(canvas.canvasObj,'mouseout',  function(e){mouseup(e,JSD_CONTROL)})
		addEvent(canvas.canvasObj,'mousemove',function(e){mousemove(e,JSD_CONTROL)})
	}
}
JSD_CONTROL.launch();


function copy_properties(src, dest){
    for (var prop in src) {
        dest[prop] = src[prop];
    }
}

function force_inherit(subClass, superClass) {
    copy_properties(superClass.prototype, subClass.prototype);
}

</script>

<div id="l0"></div>
</body></html>
