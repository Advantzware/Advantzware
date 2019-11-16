jQuery.expr[':'].Contains = function(a, i, m) { 
  return jQuery(a).text().toUpperCase().indexOf(m[3].toUpperCase()) >= 0; 
};

(function($){
	$.fn.extend({
 	    SmartDataGrid:function(params){
 	      var conf = {};
		  $.extend(conf, params);
		  return $(this).each(function(){
			var oDatagrid = this;
			$(this).find(".Filter a").bind("click", function(){
					//IPad Darstellung fix
					if (navigator.userAgent.indexOf('iPad') > -1){
						$(this).next().addClass("iPad");
					}
					$(this).next().removeClass("hidden")
								  .addClass    ("show");
					$(this).next().find("select").focus();
			    });
			$(this).find(".FilterContent select").bind("blur", function(){
					$(this).parent().removeClass("show")
					  			  .addClass   ("hidden");
		    	});
			$(this).find(".FilterContent select").bind("mouseup", function(){
				$(this).parent().removeClass("show")
				  			  .addClass   ("hidden");
	    	});
		});
	  }
	});
})(jQuery);