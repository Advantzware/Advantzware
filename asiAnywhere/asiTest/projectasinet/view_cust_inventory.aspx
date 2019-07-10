<%@ Page Language="c#" AutoEventWireup="true" Debug="false" Inherits="view_cust_inventory" Codebehind="view_cust_inventory.aspx.cs" %>
<%@ Register Src="footer.ascx" TagName="Footer" TagPrefix="ft" %>
<%@ Register Src="header.ascx" TagName="Header" TagPrefix="hd" %>
<html xmlns="http://www.w3.org/1999/xhtml" >
  <head id="Head1" runat="server">
    <title>View Inventory</title>
    
    <LINK href="include/style2.css" type="text/css" rel="stylesheet"/>
     
    <link href="include/tree.css" rel="stylesheet" type="text/css" />
    <link href="include/dhtmlwindow.css" rel="stylesheet" type="text/css" />
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

function ContactCustomerLookup(ReturnObj1,ReturnObj2,ReturnObj3,ReturnObj4,ReturnObj5,ReturnObj6,ReturnObj7,ReturnObj8,ReturnObj9, ReturnObj10,ReturnObj11){ 
  document.forms[0].FormView1$vCustnoTextBox.value = ReturnObj1;
  
  
    
}

function fglook(){ 
  var NewWindow = window.open("fgitem_lookup.aspx","FGLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}

function FGLookup(ReturnObj1){ 
  document.forms[0].FormView1$vItemTextBox.value = ReturnObj1;
  
}
function LocationLook(){ 
  var NewWindow = window.open("wharehouse_lookup.aspx","WhareHouseLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}
function WhareHouseLookup(ReturnObj1){ 
  document.forms[0].FormView1$vLocTextBox.value = ReturnObj1;
}    

function ShipTOLook2(){ 
var lookHidden = document.getElementById("FormView1_vCustnoTextBox").value;
  var NewWindow = window.open("ShipToCustLook.aspx?look="+ lookHidden +"","LocationLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}
function ShipTo2Lookup(ReturnObj1){ 
   document.forms[0].FormView1$vLocTextBox.value=ReturnObj1;
}   
function custchk()
{
var cust = document.getElementById("FormView1_vCustnoTextBox");
if (cust.value == "")
{
alert("First Customers A/R Code be Enter");
cust.focus();
}
}
    </script> 
  </head>    
   <body>
    <form id="frmList" runat="server"  >   
        <hd:header id="Header1" runat="server"></hd:header>
      <div>
            
      <TABLE id="tblTop" cellSpacing="3" border="0">
        <TR>
          
          <TD align=center nowrap><font size=+0><b>View Customer Inventory &nbsp;</b></font></TD>
          <td><asp:linkbutton id="LinkButton1" runat="server" OnClick="LinkButton1_Click">Back to menu</asp:linkbutton></td>

          <TD vAlign="middle" align="center"><b>Users</b>&nbsp;&nbsp;&nbsp;Logged as&nbsp;
            <asp:label id="lblUser" runat="server" Font-Bold="True">&nbsp;</asp:label>&nbsp;&nbsp;&nbsp;
            <asp:linkbutton id="hlnkLogOut" runat="server" OnClick="hlnkLogOut_Click">Log out</asp:linkbutton>
            &nbsp;&nbsp;&nbsp;<asp:hyperlink id="hlnkChangePwd" runat="server" NavigateUrl="changepwd.aspx">Change password</asp:hyperlink>
          &nbsp;<b>Company:</b> &nbsp;<asp:label id="lblComp" runat="server" Font-Bold="True">&nbsp;</asp:label></TD>
          
          <%--<TD vAlign="middle" align="center">
          <asp:label id="lblQuickJump" runat="server">Quick jump :&nbsp;</asp:label>&nbsp;
          <asp:dropdownlist id="ddlQuickJump" runat="server"  AutoPostBack="True" OnSelectedIndexChanged="ddlQuickJump_SelectedIndexChanged">                            
          </asp:dropdownlist>&nbsp;&nbsp;
          </TD>--%>
          
          <TD vAlign="middle" width="20">&nbsp;</TD>
          
          <td width=30>&nbsp;</td>
        </TR>
      </TABLE>
      <table>
    <tr bgcolor="gray">
    <td><div  id="navigation" style="width:100%">
		<ul nowrap><li  >
    <asp:LinkButton ID="listImageButton"  OnClick="list_cust_click"  runat="server" >List Inventory</asp:LinkButton></li>
      <li class="selected"><asp:LinkButton ID="viewImageButton" OnClick="view_cust_click"  runat="server" >View Inventory</asp:LinkButton> </li></ul></div> </td>
    </tr>
    </table>
    
    <asp:label id="lblMessage" runat="server" ForeColor="Red"></asp:label>&nbsp;
            
          <asp:FormView ID="FormView1" runat="server" OnPreRender="formview_prerender_click" OnDataBound="FormView1_DataBound" DataSourceID="ObjectDataSource1"  >
              <EditItemTemplate>
                     <table class="shade">
                     <tr>
                  <td align="right" style="padding-right:5px"><b>Customer#:</b></td>
                  <td><asp:TextBox ID="vCustnoTextBox" Width="120px"  runat="server" Text='<%# Bind("vCustno") %>'>
                  </asp:TextBox>
                    <a href="#" onClick="contactcustomerlook(); return false"><asp:Image ID="Image1" runat="server" ImageUrl="images/lookup_icon.gif" /></a>			 
                  </td>
                  <td align="right" style="padding-right:5px"><b>Ship-To:</b></td>
                  <td><asp:TextBox ID="vLocTextBox" Width="120px" runat="server"   Text='<%# Bind("vLoc") %>'>
                  </asp:TextBox>
                  <a href="#" onfocus="custchk()" onClick="ShipTOLook2(); return false"><asp:Image ID="Image3" runat="server" ImageUrl="images/lookup_icon.gif" /></a>			
                  </td></tr>
                  <tr><td align="right" style="padding-right:5px"><b>FG Item#:</b></td>
                  <td><asp:TextBox ID="vItemTextBox" Width="120px" runat="server"   Text='<%# Bind("vItem") %>'>
                  </asp:TextBox>
                  <a href="#" onClick="fglook(); return false"><asp:Image ID="Image2" runat="server" ImageUrl="images/lookup_icon.gif" /></a>			
                  </td>
                  <td align="right" style="padding-right:5px"><b>On-Hand Qty:</b></td>
                  <td><asp:TextBox ID="vQtyTextBox" Width="120px"  runat="server" Text='<%# Bind("vQty") %>'>
                  </asp:TextBox>
                  <asp:CompareValidator ID="CompareValidator2" runat="server" ErrorMessage="Only Numbers" ControlToValidate="vQtyTextBox" Display="dynamic" SetFocusOnError="true" Operator="datatypecheck" Type="integer"></asp:CompareValidator>
                  </td></tr>
                  <tr><td align="right" style="padding-right:5px"><b>Annual Consumption: </b></td>
                  <td><asp:TextBox ID="vConsumTextBox" Width="100px"  runat="server" Text='<%# Bind("vConsum") %>'>
                  </asp:TextBox>
                      <asp:CompareValidator ID="CompareValidator3" runat="server" ErrorMessage="Only Numbers" ControlToValidate="vConsumTextBox" Display="dynamic" SetFocusOnError="true" Operator="datatypecheck" Type="integer"></asp:CompareValidator>
                  </td></tr>
                  <tr><td colspan="2"><asp:Button ID="UpdateButton" CssClass="button" runat="server" CausesValidation="True" OnClick="update_click"
                      Text="Save">
                  </asp:Button>
                  <asp:Button ID="UpdateCancelButton" runat="server" CssClass="button" CausesValidation="False" CommandName="Cancel"
                      Text="Cancel">
                  </asp:Button></td> </tr></table>
              </EditItemTemplate>
              <InsertItemTemplate>
              <table  class="shade">
                     <tr>
                  <td align="right" style="padding-right:5px"><b>Customer#:</b></td>
                  <td nowrap><asp:TextBox ID="vCustnoTextBox" runat="server" Text='<%# Bind("vCustno") %>'>
                  </asp:TextBox>
                  <a href="#" onClick="contactcustomerlook(); return false"><asp:Image ID="Image1" runat="server" ImageUrl="images/lookup_icon.gif" /></a>			
                  <asp:RequiredFieldValidator ID="RequiredFieldValidator1" Display="Dynamic" SetFocusOnError="true" ControlToValidate="vCustnoTextBox" runat="server" ErrorMessage="Enter the Customer"></asp:RequiredFieldValidator>
                  </td>
                  <td align="right" style="padding-right:5px"><b>Ship-To:</b></td>
                  <td nowrap><asp:TextBox ID="vLocTextBox" runat="server" Text='<%# Bind("vLoc") %>'>
                  </asp:TextBox>
                   <a href="#" onfocus="custchk()" onClick="ShipTOLook2(); return false"><asp:Image ID="Image3" runat="server" ImageUrl="images/lookup_icon.gif" /></a>			
                  <asp:RequiredFieldValidator ID="RequiredFieldValidator2" Display="Dynamic" ControlToValidate="vLocTextBox" runat="server" ErrorMessage="Enter the WhareHouse"></asp:RequiredFieldValidator>
                  </td></tr>
                  <tr><td align="right" style="padding-right:5px"><b>FG Item#:</b></td>
                  <td nowrap><asp:TextBox ID="vItemTextBox" runat="server" Text='<%# Bind("vItem") %>'>
                  </asp:TextBox>
                  <a href="#" onClick="fglook(); return false"><asp:Image ID="Image2" runat="server" ImageUrl="images/lookup_icon.gif" /></a>			
                  <asp:RequiredFieldValidator ID="RequiredFieldValidator3" Display="Dynamic" ControlToValidate="vItemTextBox" runat="server" ErrorMessage="Enter the FG Item"></asp:RequiredFieldValidator>
                  </td>
                  <td align="right" style="padding-right:5px"><b>On-Hand Qty:</b></td>
                  <td nowrap><asp:TextBox ID="vQtyTextBox" Width="100px" runat="server" Text='<%# Bind("vQty") %>'>
                  </asp:TextBox>                  
                  <asp:CompareValidator ID="CompareValidator3" runat="server" ErrorMessage="Only Numbers" ControlToValidate="vQtyTextBox" Display="dynamic" SetFocusOnError="true" Operator="datatypecheck" Type="integer"></asp:CompareValidator>
                  </td></tr>
                  <tr><td align="right" style="padding-right:5px"><b>Annual Consumption:</b></td>
                  <td><asp:TextBox ID="vConsumTextBox" Width="100px" runat="server" Text='<%# Bind("vConsum") %>'>
                  </asp:TextBox>
                  <asp:CompareValidator ID="CompareValidator1" runat="server" ErrorMessage="Only Numbers" ControlToValidate="vConsumTextBox" Display="dynamic" SetFocusOnError="true" Operator="datatypecheck" Type="integer"></asp:CompareValidator>
                  </td></tr>
                  <tr><td colspan="2"><asp:Button ID="InsertButton" CssClass="button" OnClick="save_click" runat="server" 
                      Text="Save">
                  </asp:Button>
                  <asp:Button ID="InsertCancelButton" runat="server" CssClass="button" CausesValidation="False" CommandName="Cancel"
                      Text="Cancel">
                  </asp:Button></td></tr>
                  </table>
              </InsertItemTemplate>
              <ItemTemplate>
              <table class="shade"><tr>
                  <td align="right" style="padding-right:5px"><b>Customer#:</b></td>
                  <td><asp:Label ID="vCustnoLabel" Width="120px" BackColor="turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px"  runat="server" Text='<%# Bind("vCustno") %>'></asp:Label></td>
                   <td align="right" style="padding-right:5px"><b>Ship-To:</b></td>
                  <td><asp:Label ID="vLocLabel" Width="120px" runat="server" BackColor="turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px"  Text='<%# Bind("vLoc") %>'></asp:Label></td>
                  <%--<td><asp:Label ID="vdescLabel" Width="140px" runat="server" BackColor="turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px"  Text='<%# Bind("vDscr") %>'></asp:Label></td>--%>
                  </tr>
                  <tr>
                   <td align="right" style="padding-right:5px"><b>FG Item # :</b></td>
                  <td><asp:Label ID="vItemLabel" Width="120px" BackColor="turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px"  runat="server" Text='<%# Bind("vItem") %>'></asp:Label></td>
                  <td align="right" style="padding-right:5px"><b>On-Hand Qty:</b></td>
                  <td><asp:Label ID="vQtyLabel" Width="120px" runat="server" BackColor="turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px"  Text='<%# Bind("vQty") %>'></asp:Label></td></tr>
                  <tr><td align="right" style="padding-right:5px"><b>Annual Consumption:</b></td>
                  <td><asp:Label ID="vConsumLabel" Width="120px" runat="server" BackColor="turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px"  Text='<%# Bind("vConsum") %>'></asp:Label>
                  <asp:Label  ID="reckeyLabel" Visible="false" runat="server" Text='<%# Bind("vreckey") %>'></asp:Label>
                  </td></tr>
                  
                  <tr><td colspan="3"><asp:Button ID="addButton" CssClass="button" runat="server" CausesValidation="True" CommandName="new"
                      Text="Add">
                  </asp:Button>
                  <asp:Button ID="updateButton" runat="server" CssClass="button" CausesValidation="False" CommandName="edit"
                      Text="Update">
                  </asp:Button>
                  <asp:Button ID="deleteButton" runat="server" CssClass="button" CausesValidation="False" OnClick="delete_Click" OnClientClick="return confirm ('Are You Sure want to Delete this Record')"
                      Text="Delete">
                  </asp:Button></td></tr>
                  </table>
              </ItemTemplate>
          </asp:FormView>
          <asp:ObjectDataSource ID="ObjectDataSource1" runat="server" OldValuesParameterFormatString="original_{0}"
              SelectMethod="SelectCustitem" TypeName="custitem">
              <SelectParameters>
                  <asp:Parameter DefaultValue="" Name="prmUser" Type="String" />
                  <asp:Parameter  Name="prmActCitem" Type="String"  />
                  <asp:SessionParameter  Name="prmCust" SessionField="index_view_cust_inv_cust"  Type="String" />
                  <asp:SessionParameter Name="prmLoc" SessionField="index_view_cust_inv_loc" Type="String" />
                  <asp:SessionParameter Name="prmItem" SessionField="index_view_cust_inv_item" Type="String" />
                  <asp:Parameter Name="prmQty" Type="Decimal" />
                  <asp:Parameter Name="prmConsum" Type="Decimal" />
                  <asp:Parameter   Name="prmRecKey" Type="string" />
                  <asp:Parameter Name="prmUpdateCust" Type="string" />
                <asp:Parameter Name="prmUpdateloc" Type="string" />
                <asp:Parameter Name="prmUpdateItem" Type="string" />
              </SelectParameters>
          </asp:ObjectDataSource>
      
    </div>
    </form>
</body>
</html>
