jQuery.expr[':'].Contains = function(a, i, m) { 
  return jQuery(a).text().toUpperCase().indexOf(m[3].toUpperCase()) >= 0; 
};

(function($){
	$.fn.extend({
 	    TaskBarMenu:function(params){
 	      var conf = {};
		  $.extend(conf, params);
		  return $(this).each(function(){
			var oMenubar = this;
			
			if ($(oMenubar).find("li").length == 0){
				$(oMenubar).hover (function (){
					$(oMenubar).removeClass ("TaskBarMenuContainerHover");
				});
			}else{
				$(oMenubar).hover (function (){
					$(oMenubar).addClass ("TaskBarMenuContainerHover");
				}, 
				function (){
					if ($(oMenubar).find(".TaskBarMenuLinks").hasClass ("hidden")){
						$(oMenubar).removeClass ("TaskBarMenuContainerHover");
					}
				});
			}
			
			$(this).bind("click", function(){

				if ($(oMenubar).find(".TaskBarMenuLinks").hasClass("hidden")){
					$(oMenubar).find(".TaskBarMenuLinks").removeClass("hidden");
					$(oMenubar).find(".TaskBarMenuLinks").addClass("show");
				}else{
					$(oMenubar).find(".TaskBarMenuLinks").removeClass("show");
					$(oMenubar).find(".TaskBarMenuLinks").addClass("hidden");
				}
			    
			});
		});
	  }
	});
})(jQuery);