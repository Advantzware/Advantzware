<%@ Page Language="c#" AutoEventWireup="true" Debug="false" Inherits="purgecontact" Codebehind="purgecontact.aspx.cs" %>
<%@ Register Src="footer.ascx" TagName="Footer" TagPrefix="ft" %>
<%@ Register Src="header.ascx" TagName="Header" TagPrefix="hd" %>
<html xmlns="http://www.w3.org/1999/xhtml" >
  <head id="Head1" runat="server">
    <title>Customers</title>
    <LINK href="include/style.css" type="text/css" rel="stylesheet"/>
    <script language = JavaScript>
    
    var bSelected=false;
    function ChSel()
    {
        var theForm = document.forms['frmList'];
        if (!theForm) theForm = document.frmList;
        bSelected = !bSelected; 
        var i;
        for (i=0;i<theForm.chDelete.length;++i) theForm.chDelete[i].checked=bSelected;
    } 
    
    function OnKeyDown()
    {
        e = window.event;
        if (e.keyCode == 13)
        {
            e.cancel = true;
            var theForm = document.forms['frmList'];
            if (!theForm) theForm = document.frmList;                
            theForm.btnSearch.click();              
        }
    }
    
    function contactcustomerlook(){ 
  var NewWindow = window.open("contact_customer_lookup.aspx","ContactCustomerLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");

}

function ContactCustomerLookup(ReturnObj1){ 
  document.forms[0].Company_TextBox.value = ReturnObj1;
  } 
  
  function contactcustomerlook2(){ 
  var NewWindow = window.open("contact_customer_lookup2.aspx","ContactCustomerLookupWindow2","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}

function ContactCustomerLookup2(ReturnObj1){ 
  document.forms[0].Companyto_TextBox.value = ReturnObj1;
  }

    </script> 
  </head>    
   <body>
    <form id="frmList" runat="server"  defaultfocus='firstnameTextBox'>   
        <hd:header id="Header1" runat="server"></hd:header>
      <div>
            
      <TABLE id="tblTop" cellSpacing="3" border="0">
        <TR>
          
          <TD align=center nowrap><font size=+0><b>Purge Contact&nbsp;</b></font></TD>
          <TD vAlign="middle">
            <asp:linkbutton id="hlkBackToMenu" runat="server" OnClick="hlkBackToMenu_Click">Back to menu</asp:linkbutton>
          </TD> 
          <TD vAlign="middle" align="center"><b>Users</b>&nbsp;&nbsp;&nbsp;Logged as&nbsp;
            <asp:label id="lblUser" runat="server" Font-Bold="True">&nbsp;</asp:label>&nbsp;&nbsp;&nbsp;
            <asp:linkbutton id="hlnkLogOut" runat="server" OnClick="hlnkLogOut_Click">Log out</asp:linkbutton>
            &nbsp;&nbsp;&nbsp;<asp:hyperlink id="hlnkChangePwd" runat="server" NavigateUrl="changepwd.aspx">Change password</asp:hyperlink>
          &nbsp;<b>Company:</b> &nbsp;<asp:label id="lblComp" runat="server" Font-Bold="True">&nbsp;</asp:label>
</TD>
          
          
          <TD vAlign="middle" width="20">&nbsp;</TD>
          
          <td width=30>&nbsp;</td>
        </TR>
      </TABLE>
      <asp:Label ID="Label1" runat="server" Text="" Font-Bold="true" ForeColor="red"></asp:Label>
      <table class="shade"><tr><td><b>From First Name:</b></td>
      <td>
          <asp:TextBox ID="firstnameTextBox" runat="server"></asp:TextBox></td>
          <td><b>To First Name:</b></td>
          <td><asp:TextBox ID="LastnameTextBox" runat="server"></asp:TextBox></td></tr>
      <tr><td><b>From Customer:</b></td>
      <td><asp:TextBox ID="Company_TextBox" runat="server"></asp:TextBox>
      <a href="#" onClick="contactcustomerlook(); return false"><asp:Image ID="CustomerLook" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
      </td>
      <td><b>To Customer:</b></td>
      <td><asp:TextBox ID="Companyto_TextBox" runat="server"></asp:TextBox>
      <a href="#" onClick="contactcustomerlook2(); return false"><asp:Image ID="Image1" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
      </td></tr>
      <tr><td>
          <asp:Button ID="DeleteButton" CssClass="button" runat="server" OnClick="DeleteButton_Click" OnClientClick="return confirm('Are you want to Sure to Delete?')"  Text="Delete" /></td></tr>
      </table>
          </div>
    <ft:footer id="Footer1" runat="server"></ft:footer>
    </form>
  </body>
</HTML>

