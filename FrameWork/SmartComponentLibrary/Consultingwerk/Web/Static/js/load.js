jQuery.expr[':'].Contains = function(a, i, m) { 
  return jQuery(a).text().toUpperCase().indexOf(m[3].toUpperCase()) >= 0; 
};

function onLoad(){
	$(".SmartRibbonBar").SmartRibbonBar();
	$(".SmartDataGrid").SmartDataGrid();
	$(".SmartDialog").SmartDialog();
	$(".TaskBarMenuContainer").TaskBarMenu();
};