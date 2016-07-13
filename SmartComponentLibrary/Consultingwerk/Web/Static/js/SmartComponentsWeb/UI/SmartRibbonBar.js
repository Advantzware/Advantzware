jQuery.expr[':'].Contains = function(a, i, m) { 
  return jQuery(a).text().toUpperCase().indexOf(m[3].toUpperCase()) >= 0; 
};

(function($){
	$.fn.extend({
 	    SmartRibbonBar:function(params){
 	      var conf = {};
		  $.extend(conf, params);
		  return $(this).each(function(){
			var oToolbar = this;
			//TabClick Event binden
			$(this).find(".SmartRibbonTabs li").bind("click", function(){
					
					//aktiven SmartRibbonTab setzen
			      	$(oToolbar).find(".SmartRibbonTabs li").removeClass("SmartRibbonTab_active");
			      	$(this).addClass("SmartRibbonTab_active");
			      	$(oToolbar).find(".SmartRibbonLinks").removeClass("show");
			      	$(oToolbar).find(".SmartRibbonLinks").addClass("hidden");
			      	
			      	//aktiven SmartRibbonLink setzen
			      	var activeSmartRibbonLink = "#" + $(this).attr("id") + "Links";
			      	$(oToolbar).find(activeSmartRibbonLink).addClass("show");
			      	$(oToolbar).find(activeSmartRibbonLink).removeClass("hidden");
			      	
			    });
			//ToolClick Event binden
			$(this).find(".SmartRibbonLinkNormal, .SmartRibbonLinkLarge").bind("click", function(){
				if ($(this).find("a").attr("href") == "#"){
					var strClickValue = "";
					var strActionForm = ""
					strClickValue = $(oToolbar).attr("id") + "." + $(this).attr("action");
					strActionForm = $(oToolbar).attr("actionForm");
					
					$(oToolbar).find(".ClickValue").attr("value", strClickValue);
					document.forms[strActionForm].submit();
				}
			});
		});
	  }
	});
})(jQuery);