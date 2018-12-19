<%@ Page Language="c#" AutoEventWireup="true" Debug="false" Inherits="view_cust_plant" Codebehind="view_cust_plant.aspx.cs" %>
<%@ Register Src="footer.ascx" TagName="Footer" TagPrefix="ft" %>
<%@ Register Src="header.ascx" TagName="Header" TagPrefix="hd" %>
<html xmlns="http://www.w3.org/1999/xhtml" >
  <head id="Head1" runat="server">
    <title>Plant File Maintenance</title>
    
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
   

function Cross_Look(){ 
  var NewWindow = window.open("crossref_lookup.aspx","CrossRefWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}

function CrossRefLookup(ReturnObj1,ReturnObj2){
    document.forms[0].FormView1_vCustTextBox.value = ReturnObj1;
    document.forms[0].FormView1_vVendorCodeLabel.value = ReturnObj2;
   //document.getElementById("FormView1_vVendorCodeLabel").innerText = ReturnObj2;
  
}

function ShipTOLook2(){ 
var lookHidden = document.getElementById("FormView1_vCustTextBox").value;
  var NewWindow = window.open("ShipToCustLook.aspx?look="+ lookHidden +"","LocationLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}
function ShipTo2Lookup(ReturnObj1){ 
   document.forms[0].FormView1_vShipidTextBox.value=ReturnObj1;
}


function LocationLook(){ 
  var NewWindow = window.open("wharehouse_lookup.aspx","LocationLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}
function WhareHouseLookup(ReturnObj1,ReturnObj2,ReturnObj3,ReturnObj4,ReturnObj5,ReturnObj6,ReturnObj7,ReturnObj8){ 
  document.forms[0].FormView1_vShipidTextBox.value = ReturnObj1;
  document.forms[0].FormView1_vSNameTextBox.value=ReturnObj2;
  document.forms[0].FormView1_vShipAddr1TextBox.value=ReturnObj3;
  document.forms[0].FormView1_vShipStateTextBox.value=ReturnObj4;
  document.forms[0].FormView1_vShipCityTextBox.value=ReturnObj5;
  document.forms[0].FormView1_vShipZipTextBox.value=ReturnObj6;
  
  document.forms[0].FormView1_vShipAddr2TextBox.value=ReturnObj7;
  document.forms[0].FormView1_vCustTextBox.value=ReturnObj8;
}    
    </script> 
  </head>    
   <body>
    <form id="frmList" runat="server"  >   
        <hd:header id="Header1" runat="server"></hd:header>
      <div>
            
      <TABLE id="tblTop" cellSpacing="3" border="0">
        <TR>
          
          <TD align=center nowrap><font size=+0><b>Plant File Maintenance &nbsp;</b></font></TD>
          <td><asp:linkbutton id="LinkButton1" runat="server" OnClick="LinkButton1_Click">Back to menu</asp:linkbutton></td>

          <TD vAlign="middle" align="center"><b></b>&nbsp;&nbsp;&nbsp;Logged as&nbsp;
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
    <asp:LinkButton ID="listImageButton"  OnClick="list_cust_click"  runat="server" > List WareHouse</asp:LinkButton></li>
    <li class="selected"><asp:LinkButton ID="viewImageButton" OnClick="view_cust_click"  runat="server" > View WareHouse</asp:LinkButton></li> </ul></div> </td>
    </tr>
    </table>
    
    <asp:label id="lblMessage" runat="server" ForeColor="Red"></asp:label>&nbsp;
            
          <asp:FormView ID="FormView1" runat="server" DataSourceID="ObjectDataSource1" OnDataBound="FormView1_DataBound" OnUnload="FormView1_Unload"  >
              <EditItemTemplate>
                
               <table class="shade">
                   <tr><td align="right" style="padding-right:5px"><b> Suppliers A/R Code:</b></td>
                   <td><asp:TextBox ID="vCustTextBox" runat="server" OnTextChanged="customer_txt_Click" AutoPostBack="true" Text='<%# Bind("vCust") %>'> </asp:TextBox>
                   <a href="#" onClick="Cross_Look(); return false"><asp:Image ID="Image6" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
                   </td></tr>
                   <tr><td align="right" style="padding-right:5px"><b>Suppliers Ship ID:</b></td>                   
                   <td><asp:TextBox ID="vShipidTextBox" runat="server" Text='<%# Bind("vShipid") %>'></asp:TextBox>
                   <a href="#" onClick="ShipTOLook2(); return false"><asp:Image ID="Image1" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
                   </td></tr>
                   <tr><td align="right" style="padding-right:5px"><b>Customers A/P Code:</b></td>
                   <td><asp:TextBox ID="vVendorCodeLabel"  Enabled="false" Width="120px" runat="server" Text='<%# Bind("vVendorCode") %>'> </asp:TextBox></td></tr>
                   <tr><td align="right" style="padding-right:5px"><b>Customers Plant ID:</b></td>
                   <td><asp:TextBox ID="vPlantidTextBox" runat="server" Text='<%# Bind("vPlantid") %>'></asp:TextBox></td></tr>
                   <tr><td align="right" style="padding-right:5px"><b>Customers  Dept Code:</b></td>
                   <td><asp:TextBox ID="vVenderDeptCodeTextBox" runat="server" Text='<%# Bind("vVenderDeptCode") %>'></asp:TextBox></td></tr>
                   <tr><td align="right" style="padding-right:5px"><b>Customers Plant Name:</b></td>
                   <td><asp:TextBox ID="vPlantNameTextBox" runat="server" Text='<%# Bind("vPlantName") %>'> </asp:TextBox></td></tr>
                   
                   <tr><td colspan="2">
                   <asp:Button ID="UpdateButton" CssClass="buttonM" runat="server" CausesValidation="True" OnClick="Update_Button_Click" 
                      Text="Save">
                  </asp:Button>
                  <asp:Button ID="UpdateCancelButton" CssClass="buttonM" runat="server" CausesValidation="False" CommandName="Cancel"
                      Text="Cancel">
                  </asp:Button>
                  </td></tr></table>
                   
              </EditItemTemplate>
              <InsertItemTemplate>
              
               <table class="shade">
                   <tr><td align="right" style="padding-right:5px"><b> Suppliers A/R Code:</b></td>
                   <td><asp:TextBox ID="vCustTextBox" runat="server" OnTextChanged="customer_txt_Click" AutoPostBack="true" Text='<%# Bind("vCust") %>'> </asp:TextBox>
                   <a href="#" onClick="Cross_Look(); return false"><asp:Image ID="Image6" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
                   </td></tr>
                   <tr><td align="right" style="padding-right:5px"><b>Suppliers Ship ID:</b></td>                   
                   <td><asp:TextBox ID="vShipidTextBox" runat="server" Text='<%# Bind("vShipid") %>'></asp:TextBox>
                   <a href="#" onClick="ShipTOLook2(); return false"><asp:Image ID="Image1" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
                   </td></tr>
                   <tr><td align="right" style="padding-right:5px"><b>Customers A/P Code:</b></td>
                   <td><asp:TextBox ID="vVendorCodeLabel"  Enabled="false" Width="120px" runat="server" Text='<%# Bind("vVendorCode") %>'> </asp:TextBox></td></tr>
                   <tr><td align="right" style="padding-right:5px"><b>Customers Plant ID:</b></td>
                   <td><asp:TextBox ID="vPlantidTextBox" runat="server" Text='<%# Bind("vPlantid") %>'></asp:TextBox></td></tr>
                   <tr><td align="right" style="padding-right:5px"><b>Customers  Dept Code:</b></td>
                   <td><asp:TextBox ID="vVenderDeptCodeTextBox" runat="server" Text='<%# Bind("vVenderDeptCode") %>'></asp:TextBox></td></tr>
                   <tr><td align="right" style="padding-right:5px"><b>Customers Plant Name:</b></td>
                   <td><asp:TextBox ID="vPlantNameTextBox" runat="server" Text='<%# Bind("vPlantName") %>'> </asp:TextBox></td></tr>
                   
                   <tr><td colspan="2">
                    <asp:Button ID="InsertButton" runat="server" CausesValidation="True" CssClass="buttonM" OnClick="Insert_Button_Click"
                      Text="Save">
                  </asp:Button>
                  <asp:Button ID="InsertCancelButton" CssClass="buttonM" runat="server" CausesValidation="False" CommandName="Cancel"
                      Text="Cancel">
                  </asp:Button>
                   </td></tr>
                   
                   </table>                 
                
                 
              </InsertItemTemplate>
              <ItemTemplate>
                   <table class="shade">
                   <tr><td align="right" style="padding-right:5px"><b> Suppliers A/R Code:</b></td>
                   <td><asp:Label ID="vCustLabel" BackColor="turquoise" Width="120px" runat="server" Text='<%# Bind("vCust") %>'></asp:Label></td></tr>
                   <tr><td align="right" style="padding-right:5px"><b>Suppliers Ship ID:</b></td>                   
                   <td><asp:Label ID="vShipidLabel" BackColor="turquoise" Width="120px" runat="server" Text='<%# Bind("vShipid") %>'></asp:Label></td></tr>
                   <tr><td align="right" style="padding-right:5px"><b>Customers A/P Code:</b></td>
                   <td><asp:Label ID="vVendorCodeLabel" BackColor="turquoise" Width="120px" runat="server" Text='<%# Bind("vVendorCode") %>'> </asp:Label></td></tr>
                   <tr><td align="right" style="padding-right:5px"><b>Customers Plant ID:</b></td>
                   <td><asp:Label ID="vPlantidLabel" BackColor="turquoise" Width="120px" runat="server" Text='<%# Bind("vPlantid") %>'></asp:Label></td></tr>
                   <tr><td align="right" style="padding-right:5px"><b>Customers  Dept Code:</b></td>
                   <td><asp:Label ID="vVenderDeptCodeLabel" BackColor="turquoise" Width="120px" runat="server" Text='<%# Bind("vVenderDeptCode") %>'> </asp:Label></td></tr>
                   <tr><td align="right" style="padding-right:5px"><b>Customers Plant Name:</b></td>
                   <td><asp:Label ID="vPlantNameLabel" BackColor="turquoise" Width="120px" runat="server" Text='<%# Bind("vPlantName") %>'> </asp:Label></td></tr>
                   <tr><td style="display:none">
                    <asp:Label ID="vReckeyLabel" runat="server" Text='<%# Bind("vReckey") %>'></asp:Label>
                   </td></tr>
                   </table>                 
                  <asp:Button ID="InsertButton" runat="server" CausesValidation="True" CssClass="buttonM" CommandName="New"
                      Text="Add">
                  </asp:Button>
                  <asp:Button ID="UpdateButton" runat="server"  CssClass="buttonM" CommandName="Edit"
                      Text="Update">
                  </asp:Button>
                  <asp:Button ID="DeleteButton" runat="server"  CssClass="buttonM" OnClick="DeleteButton_Click" OnClientClick="return confirm('Are you sure you want to delete this record')"
                      Text="Delete">
                  </asp:Button>
                  
              </ItemTemplate>
          </asp:FormView>
          
          <asp:ObjectDataSource ID="ObjectDataSource1" runat="server" OldValuesParameterFormatString="original_{0}"
              SelectMethod="SelectCustPlant" TypeName="custitem">
              <SelectParameters>
                  <asp:Parameter DefaultValue="" Name="prmUser" Type="String" />
                  <asp:SessionParameter Name="prmRecKey" SessionField="view_cust_plant_cust_reckey" Type="String" />
                  <asp:Parameter DefaultValue="view" Name="prmActPlant" Type="String" />
                  <asp:Parameter Name="prmCust" Type="String" />
                  <asp:Parameter DefaultValue="" Name="prmShipid" Type="String" />
                  <asp:Parameter Name="prmPlantid" Type="String" />
                  <asp:Parameter Name="prmDeptCode" Type="String" />
                  <asp:Parameter Name="prmPlantName" Type="String" />
                  <asp:Parameter Name="prmVanderCode" Type="String" />
              </SelectParameters>
          </asp:ObjectDataSource>
      
    </div>
    </form>
</body>
</html>
