<%@ Page Language="C#" AutoEventWireup="true" Debug="false" Inherits="style_box_design" Title="Style LookUp" Codebehind="style_box_design.aspx.cs" %>

<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">

<html xmlns="http://www.w3.org/1999/xhtml" >
<head id="Head1" runat="server">
    <title>Style </title>
    <LINK href="include/style.css" type="text/css" rel="stylesheet"/>
<link rel="stylesheet" href="include/dhtmlwindow.css" type="text/css" />
</head>
<script type="text/javascript" src="include/dhtmlwindow.js">    
</script>
<script type="text/javascript">
    function stylebox() {
        var NewWindow = window.open("style_box_design.aspx", "StyleBoxDesignWindow", "width=450,height=350,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
    }
    
</script>
<body>
    <form id="form1" runat="server">
    
    <div>
    
     <table><tr bgcolor="gray"><td> <asp:LinkButton ID="lnk_Listcustomers" OnClick="style_bro_click" runat="server" ><img src="Images/browse style 0.jpg" border="0" alt="List Style " /></asp:LinkButton>
      <asp:LinkButton ID="lnk_boxdesign" runat="server" OnClick="lnk_boxdesign_Click" > <img src="Images/box design 1.jpg" border="0" alt="Box Design" /></asp:LinkButton>
      <asp:LinkButton ID="lnk_3boxdesign" runat="server" OnClick="lnk_3boxdesign_Click" ><img src="Images/box 3d image 0.jpg" border="0" alt="3D Box Design" /></asp:LinkButton>
            
      </td>
      </tr></table>
  
  </div>
    
    <div>
       
    
        <asp:Label ID="Label1" Font-Bold="true" ForeColor="red" runat="server" Text="Label"></asp:Label>
        <asp:ImageButton ID="ImageButton1" runat="server" />
    </form>
</body>
</html>
