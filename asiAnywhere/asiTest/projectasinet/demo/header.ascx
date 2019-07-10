<%@ Control Language="c#" AutoEventWireup="false" Inherits="HeaderFooterDemo" Codebehind="header.ascx.cs" %>

<script type="text/javascript" src="include/treemenu.js"></script>
<link rel="stylesheet" type="text/css" href="include/tree.css" />
<link rel="stylesheet" href="include/dhtmlwindow.css" type="text/css" />
<script type="text/javascript" src="include/dhtmlwindow.js"></script>

<script type="text/javascript">
function showMenu(vMenu){ 
  if (vMenu == "" && ddtreemenu.getCookie("showmenu")!="") {
      vMenu = ddtreemenu.getCookie("showmenu");
  }
  
  if (document.getElementById) {
     if (vMenu == "1"){
        document.getElementById('menu').style.display = 'block';
        ddtreemenu.setCookie("showmenu", "1", 1);
     }
     if (vMenu == "2"){
       document.getElementById('menu').style.display = 'none';
       ddtreemenu.setCookie("showmenu", "2", 1);
     }
  }
  
}

function stateChecker() {
   var checkedButton = "";
   for (var i = 0; i < document.forms["frmList"].chDelete.length; i++) {      
      if (document.forms["frmList"].chDelete[i].checked) {
         checkedButton = document.forms["frmList"].chDelete[i].value;
      }
   }   
   return checkedButton;
}
</script>

<style>

.sitelogo{
margin-left:650px;
height:50px;
margin-top:10px
} 


.menu {
	background-color:#288EB1;
	padding:1px 15px 1px 10px;
	border-radius: 15px;
	width:250px;  }
	.sitelogo{
margin-left:5px;
height:40px;
margin-top:1px
} 
</style>

<table width="100%" align="left" valign="top" cellSpacing="0" cellPadding="0" border="0">
    <tr>
        <td valign="top" align="left" class="blackshade"  colspan="2">
            <img src="http://www.advantzware.com/wp-content/uploads/2016/09/advantzware-logo8.png" alt="Advantzware" class="sitelogo">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<b><font color=#666666>© 2001 - 2007 Advanced Software, Inc. All rights reserved.</font></b><br>
            <asp:hyperlink runat="server" id="ShowMenu" NavigateUrl="javascript:showMenu('1');"><img width="75px" src="include/menu.png" border="0" alt="Show" /> </asp:hyperlink>
            <asp:hyperlink runat="server" id="HideMenu" NavigateUrl="javascript:showMenu('2');"><img width="75px" src="include/hide.png" border="0" alt="Hide" /></asp:hyperlink>
         </td>
    </tr>
    <tr>
        <td align="left" valign="top" nowrap>
   <div id="menu" class="menu">             
	<ul id="asimenu" class="treeview">	    
	    <%
	         if (Session["User"] != null) 
	         {
		createMenu("1", "top");
	         }
	   %>                 
	</ul>              
	<script type="text/javascript">
		ddtreemenu.createTree("asimenu", true);
                            showMenu("");
	</script>
        </div>
        </td>
        <td valign="top">
