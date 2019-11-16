jQuery.expr[':'].Contains = function(a, i, m) { 
  return jQuery(a).text().toUpperCase().indexOf(m[3].toUpperCase()) >= 0; 
};

(function($){
	$.fn.extend({
 	    SmartDialog:function(params){
 	      var conf = {};
		  $.extend(conf, params);
		  return $(this).each(function(){
			var oDialog = this
			$($(this).find(".Title")).draggit($(this).find(".Error"),150,200);
			$(this).find("Button").click(function(){
				switch($(this).attr("value")){
				case "ok":
					$(oDialog).hide();
					break;
				case "details":
					if ($(oDialog).find(".StackTrace").hasClass("hidden")){
						$(oDialog).find(".StackTrace").removeClass("hidden")
													  .addClass("show");
						$(oDialog).find(".DetailButton img").attr("src", "/SmartComponentsWeb/Images/MessageDialog/arrow_up_blue.png");
					}else{
						$(oDialog).find(".StackTrace").removeClass("show")
													  .addClass("hidden");
						$(oDialog).find(".DetailButton img").attr("src", "/SmartComponentsWeb/Images/MessageDialog/arrow_down_blue.png");
					}
					break;
				}
			});
			
		  });
	  }
	});
})(jQuery);