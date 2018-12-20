<%@ Page Language="C#" AutoEventWireup="true" Debug="true" Inherits="view_cross_reference" Codebehind="view_cross_reference.aspx.cs" %>
<%@ Register Src="footer.ascx" TagName="Footer" TagPrefix="ft" %>
<%@ Register Src="header.ascx" TagName="Header" TagPrefix="hd" %>
<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">

<html xmlns="http://www.w3.org/1999/xhtml" >
<head id="Head1" runat="server">
    <title>View Vendor Cross Reference</title>
    <LINK href="include/style2.css" type="text/css" rel="stylesheet"/>
     <link href="include/tree.css" rel="stylesheet" type="text/css" />
    <link href="include/tree.css" rel="stylesheet" type="text/css" />
    <link href="include/tree.css" rel="stylesheet" type="text/css" />
    <link href="include/tree.css" rel="stylesheet" type="text/css" />
    <link href="include/tree.css" rel="stylesheet" type="text/css" />
    <link href="include/tree.css" rel="stylesheet" type="text/css" />
    <link href="include/tree.css" rel="stylesheet" type="text/css" />
    <LINK REL="stylesheet" TYPE="text/css" HREF="include/CalendarControl.css" >
    <script language="javascript" src="include/date.js"></script>
    <script language="javascript" src="include/event.js"></script>
    <script language="javascript" src="include/insert.js"></script>
    <script language = "JavaScript" src="include/CalendarControl.js">    
    </script>
    
    <script>
     function contactcustomerlook(){ 
  var NewWindow = window.open("contact_customer_lookup.aspx","ContactCustomerLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}

function ContactCustomerLookup(ReturnObj1,ReturnObj2,ReturnObj3,ReturnObj4,ReturnObj5,ReturnObj6,ReturnObj7,ReturnObj8,ReturnObj9, ReturnObj10,ReturnObj11){ 
  document.forms[0].FormView1_vCustNoTextBox.value = ReturnObj1;
  
}
    
    </script>
    </head>
<body>
    <form id="form1" runat="server">
    <hd:header id="Header1" runat="server"></hd:header>
    <div>
        <asp:HiddenField ID="HiddenFGitem" runat="server" />
        <asp:HiddenField ID="HiddenVanderCode" runat="server" />
        <asp:HiddenField ID="HiddenDeptCode" runat="server" />
        <asp:HiddenField ID="HiddenLine" runat="server" />
        <asp:HiddenField ID="HiddenField5" runat="server" />
    <TABLE id="tblTop" cellSpacing="3" align="center" border="0" Width="100%">
        <TR>
          <TD width=30>&nbsp;</TD>
          <TD align=center nowrap><font size=+0><b>View Vendor Cross Reference&nbsp;<asp:Label ID="lbl_page" runat="server"></asp:Label></b></font></TD>
          <TD vAlign="middle">
            <asp:linkbutton id="LinkButton1" runat="server" OnClick="LinkButton1_Click">Back to menu</asp:linkbutton>
          </TD>
          <TD align="right"><font size=+0><b>&nbsp;&nbsp;</b></font></TD>
          <TD vAlign="middle" align="left">Logged as&nbsp;
            <asp:label id="lblUser" runat="server" Font-Bold="True">&nbsp;</asp:label>&nbsp;&nbsp;&nbsp;
            <asp:linkbutton id="hlnkLogOut" runat="server" OnClick="hlnkLogOut_Click">Log out</asp:linkbutton>
            
            &nbsp;&nbsp;&nbsp;<asp:hyperlink id="hlnkChangePwd" runat="server" NavigateUrl="changepwd.aspx">Change password</asp:hyperlink>
            &nbsp;&nbsp;&nbsp;
            <b>Company:</b>&nbsp;
            <asp:label id="lblComp" runat="server" Font-Bold="True">&nbsp;</asp:label>&nbsp;&nbsp;&nbsp;
          </TD>
          
                    
          <TD vAlign="middle" width="20">&nbsp;</TD>
          <td width=30>&nbsp;</td>
        </TR>
      </TABLE>  
  </div>
    
    <div>
    
    <table>
    <tr style="background-color:Gray">
    <td><div  id="navigation" style="width:100%">
		<ul nowrap><li  >
    <asp:LinkButton ID="lnk_list" runat="server" OnClick="lnk_browse_click">Browse Vendor</asp:LinkButton></li>
    <li class="selected" ><asp:LinkButton ID="lnk_view" runat="server" OnClick="lnk_view_click">View Vendor</asp:LinkButton></li></ul></div>
    </td>
    </tr></table>
    <br />
        <asp:Button ID="addnewbutton" CssClass = "button" OnClick="newbutton_click" runat="server" Text="Add" />
    <asp:FormView ID="FormView1" runat="server" DataSourceID="ObjectDataSource1" OnDataBound="FormView1_DataBound" OnPreRender="FormView1_PreRender">
            <EditItemTemplate>
            <table class="shade">
                <tr><td align="right" style="padding-right:5px"><b>Suppliers A/R Code:</b></td>
                <td><asp:TextBox ID="vCustNoTextBox" runat="server" Text='<%# Bind("vCustNo") %>'></asp:TextBox>
                <a href="#" onclick="contactcustomerlook(); return false"><asp:Image ID="Image9" runat="server" ImageUrl="images/lookup_icon.gif" /> </a>
                </td></tr>                
                <tr><td align="right" style="padding-right:5px"><b>Customers A/P Code</b></td>
                <td><asp:TextBox ID="vVendorCodeTextBox" runat="server" Text='<%# Bind("vVendorCode") %>'></asp:TextBox></td></tr>
                <tr><td align="right" style="padding-right:5px"><b>Customers A/P Description</b></td>
                <td><asp:TextBox ID="vCustNameTextBox" runat="server" Text='<%# Bind("vCustName") %>'></asp:TextBox></td></tr>
                <%--vCompany:
                <asp:TextBox ID="vCompanyTextBox" runat="server" Text='<%# Bind("vCompany") %>'>
                </asp:TextBox><br />--%>
                <tr><td>
                <asp:Button ID="UpdateButton" runat="server" CssClass="button" CausesValidation="True" OnClick="Update_Button_Click" Text="Save">
                </asp:Button>
                <asp:Button ID="UpdateCancelButton" runat="server" CssClass="button" CausesValidation="False" CommandName="Cancel" OnClick="Cancil_button_click" 
                    Text="Cancel">
                </asp:Button>
                </td></tr>
            </table>
            </EditItemTemplate>
            <InsertItemTemplate>
                <table class="shade">
                <tr><td align="right" style="padding-right:5px"><b>Suppliers A/R Code:</b></td>                
                <td><asp:TextBox ID="vCustNoTextBox" runat="server" Text='<%# Bind("vCustNo") %>'></asp:TextBox>
                <a href="#" onclick="contactcustomerlook(); return false"><asp:Image ID="Image9" runat="server" ImageUrl="images/lookup_icon.gif" /> </a>
                </td></tr>
                <tr><td align="right" style="padding-right:5px"><b>Customers A/P Code</b></td>
                <td><asp:TextBox ID="vVendorCodeTextBox" runat="server" Text='<%# Bind("vVendorCode") %>'></asp:TextBox></td></tr>
                <tr><td align="right" style="padding-right:5px"><b>Customers A/P Description</b></td>
                <td><asp:TextBox ID="vCustNameTextBox" runat="server" Text='<%# Bind("vCustName") %>'></asp:TextBox></td></tr>
                <%--vCompany:
                <asp:TextBox ID="vCompanyTextBox" runat="server" Text='<%# Bind("vCompany") %>'>
                </asp:TextBox><br />--%>
                <tr><td>
                <asp:Button ID="InsertButton" runat="server" CssClass="button"  CausesValidation="True" OnClick="Insert_Button_Click" Text="Save">
                </asp:Button>
                <asp:Button ID="InsertCancelButton" runat="server" CssClass="button" CausesValidation="False" CommandName="Cancel" OnClick="Cancil_button_click" 
                    Text="Cancel">
                </asp:Button>
                </td></tr>
            </table
            </InsertItemTemplate>
            <ItemTemplate>
                <table class="shade" width="400px">
                    <tr><td align="right" style="padding-right:5px"><b>Suppliers A/R Code:</b></td>  
                        <td><asp:Label ID="vCustNoLabel" runat="server" Width="120px" BackColor="turquoise" BorderColor="Black" BorderStyle="solid" BorderWidth="1px" Text='<%# Bind("vCustNo") %>'></asp:Label></td>
                    </tr>
                    <tr><td align="right" style="padding-right:5px"><b>Customers A/P Code:</b></td>                 
                        <td><asp:Label ID="vVendorCodeLabel" runat="server" Width="120px" BackColor="turquoise" BorderColor="Black" BorderStyle="solid" BorderWidth="1px"  Text='<%# Bind("vVendorCode") %>'></asp:Label></td>
                    </tr>    
                    <tr><td align="right" style="padding-right:5px"><b>Customers A/P Description:</b></td>   
                        <td><asp:Label ID="vCustNameLabel" runat="server" Width="120px" BackColor="turquoise" BorderColor="Black" BorderStyle="solid" BorderWidth="1px" Text='<%# Bind("vCustName") %>'></asp:Label></td>
                    </tr>
                    <tr><td colspan="3"> &nbsp;&nbsp; &nbsp;&nbsp; &nbsp;&nbsp; &nbsp;&nbsp; &nbsp;&nbsp;              
                        <asp:Button ID="AddButton" runat="server" CssClass="button" CausesValidation="False" CommandName="new" Text="Add">
                        </asp:Button>
                        <asp:Button ID="UpdateButton" runat="server" CssClass="button" CausesValidation="False" CommandName="edit"
                        Text="Update"></asp:Button>
                        <asp:Button ID="DeleteButton" runat="server" CssClass="button" CausesValidation="False" OnClick="DeleteButton_Click" OnClientClick="return confirm('Are you sure you want to delete this record')"
                        Text="Delete"></asp:Button>
                    </td></tr>  
                </table>
               <%-- vCompany:
                <asp:Label ID="vCompanyLabel" runat="server" Text='<%# Bind("vCompany") %>'></asp:Label><br />--%>
            </ItemTemplate>
        </asp:FormView>
        <asp:ObjectDataSource ID="ObjectDataSource1" runat="server" OldValuesParameterFormatString="original_{0}"
            SelectMethod="Selectvendorcross" TypeName="custitem">
            <SelectParameters>
                <asp:Parameter Name="prmUser" Type="String" />
                <asp:Parameter Name="prmAction" Type="String" DefaultValue="View" />
                <asp:SessionParameter DefaultValue="" Name="prmCustNo" SessionField="crossref_arcode_txt_view" Type="String" />
                <asp:Parameter Name="prmVendorCode" Type="String" />
                <asp:Parameter Name="prmCustName" Type="String" />
                <asp:Parameter Name="prmComp" Type="String" />
                <asp:Parameter Name="prmUpdateCustno" Type="String" />
            </SelectParameters>
        </asp:ObjectDataSource>
        
        <br />
        </div>
    <ft:footer id="Footer1" runat="server"></ft:footer>
    </form>
</body>
</html>