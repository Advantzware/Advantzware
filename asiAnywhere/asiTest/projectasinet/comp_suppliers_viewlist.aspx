<%@ Page Language="c#" AutoEventWireup="true" Debug="false" Inherits="Ccomp_suppliers_viewlist" Codebehind="comp_suppliers_viewlist.aspx.cs" %>
<%@ Register Src="footer.ascx" TagName="Footer" TagPrefix="ft" %>
<%@ Register Src="header.ascx" TagName="Header" TagPrefix="hd" %>
<html xmlns="http://www.w3.org/1999/xhtml" >
  <head id="Head1" runat="server">
    <title>Competitive Suppliers</title>
    
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
    
function zipcodelook(){ 
var NewWindow = window.open("zipcode_lookup.aspx","ZipCodeLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}

function ZipCodeLookup(ReturnObj1, ReturnObj2, ReturnObj3){ 
  document.forms[0].FormView1_zipTextBox.value = ReturnObj1;
   document.forms[0].FormView1_cityTextBox.value = ReturnObj2;
   document.forms[0].FormView1_stateTextBox.value = ReturnObj3;
   document.forms[0].FormView1_zipTextBox.focus();
    
}
function statecodelook(){ 
  var NewWindow = window.open("statecode_lookup.aspx","StateCodeLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}

function StateCodeLookup(ReturnObj1){ 
  document.forms[0].FormView1_stateTextBox.value = ReturnObj1;
  document.forms[0].FormView1_stateTextBox.focus();
    
}
    
    </script> 
  </head>    
   <body>
    <form id="frmList" runat="server"  >   
        <hd:header id="Header1" runat="server"></hd:header>
      <div>
            
      <TABLE id="tblTop" cellSpacing="3" border="0">
        <TR>
          
          <TD align=center nowrap><font size=+0><b>Competitive Suppliers&nbsp;</b></font></TD>
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
		<ul nowrap> <li >
        <asp:LinkButton ID="lnk_listsupplier" runat="server" OnClick="lnk_listsupplier_Click" >List Supplier</asp:LinkButton></li>
        <li class="selected"><asp:LinkButton ID="lnk_viewsupplier" runat="server" OnClick="lnk_viewsupplier_Click">View Supplier</asp:LinkButton></li>
        <li><asp:LinkButton ID="lnk_notes" runat="server" OnClick="lnk_notes_click" >Notes</asp:LinkButton></li></ul></div>
    </td>
    </tr>
    </table>
    
    <asp:label id="lblMessage" runat="server" ForeColor="Red"></asp:label>
    
    <asp:sqldatasource id="Sqldatasource1"
                SelectCommand="select [comp_code],   [name],[address1],[address2],[city],[state],[zip],[reckey],[rec_key_value]   From [dbo].[comp_suppliers],[dbo].[contact_reckey] where [comp_code]=@comp_code ORDER BY [reckey] DESC, [comp_code] ASC"
                DeleteCommand="delete from [dbo].[comp_suppliers] where [comp_code]=@comp_code"
                                
                ConnectionString="<%$ ConnectionStrings:Project1ConnectionString%>"
                ProviderName="<%$ ConnectionStrings:Project1ConnectionString.providerName%>"
                 runat="server" OnInserted="Sqldatasource1_Inserted" >
        <SelectParameters>
            <%--<asp:QueryStringParameter  QueryStringField="code" Name="code" Type="String" DefaultValue="-1" />--%>
                <asp:SessionParameter SessionField="comp_supplier_code" Name="comp_code" Type="string" DefaultValue="1" />
        </SelectParameters> 
        <DeleteParameters>
        <asp:SessionParameter SessionField="comp_supplier_code" Name="comp_code" Type="string" DefaultValue="1" />
        </DeleteParameters>      
        <%--<InsertParameters>
            <asp:Parameter Name="comp_code" Type="String"/>
            <asp:Parameter Name="name" Type="String"/>
            <asp:Parameter Name="rec_key" Type="String"/>
            <asp:SessionParameter Name="rec_key_value" SessionField="view_list_rec_key" DefaultValue="1215" Type="string" />
        </InsertParameters>--%>
        <%--<UpdateParameters>
            <asp:Parameter Name="name" Type="String"/>

            <asp:SessionParameter  SessionField="comp_supplier_code" Name="comp_code" Type="String" />

        </UpdateParameters>--%>
            </asp:sqldatasource>
            
          <asp:FormView ID="FormView1" runat="server" DataKeyNames="comp_code" DataSourceID="Sqldatasource1"  OnDataBound="FormView1_DataBound" >
              <EditItemTemplate>
              <table class="shade">
              <tr>
              <td align="right" style="padding-right:5px;"><b>Code:</b></td>
              <td width="40px" ><b><asp:label    ID="codelabel" runat="server" Text='<%# Bind("comp_code") %>'>
                  </asp:label></b></td>
              <td   align="right" style="padding-right:5px;"><b>Name:</b></td>
              <td width="100px"><b><asp:TextBox ID="nameTextBox" MaxLength="30" Width="175px" runat="server" Text='<%# Bind("name") %>'>
                  </asp:TextBox></b></td>
              </tr>
              <tr><td></td><td></td>
              <td align="right" style="padding-right:5px;"><b>Address:</b></td>
              <td style="width:80px;"><b><asp:TextBox MaxLength="30" TabIndex="0"  Width="175px" ID="address1TextBox" runat="server" Text='<%# Bind("address1") %>'>
                  </asp:TextBox></b></td>
              </tr> <tr><td></td><td></td>   
              <td align="right" style="padding-right:5px;"><b>Address:</b></td>
              <td><b><asp:TextBox ID="address2TextBox" MaxLength="30" Width="175px" runat="server" Text='<%# Bind("address2") %>'>
                  </asp:TextBox></b></td>
              </tr>
              <tr><td></td><td></td>
              <td align="right" style="padding-right:5px;"><b>City:</b></td>
              <td  width="80px"><b><asp:TextBox MaxLength="15" TabIndex="0"  Width="100px" ID="cityTextBox" runat="server" Text='<%# Bind("city") %>'>
                  </asp:TextBox></b></td>
              <td align="right" style="padding-right:5px;"><b>State:</b></td>
              <td><b><asp:TextBox ID="stateTextBox" MaxLength="15" Width="100px" runat="server" Text='<%# Bind("state") %>'>
                  </asp:TextBox></b><a href="#" tabindex="1" onClick="statecodelook(); return false"><asp:Image ID="Image2" runat="server" ImageUrl="images/lookup_icon.gif" /></a></td>
                  <td align="right" style="padding-right:5px;"><b>Zip:</b></td>
              <td><b><asp:TextBox ID="zipTextBox" MaxLength="8" Width="100px" runat="server" Text='<%# Bind("zip") %>'>
                  </asp:TextBox></b><a href="#" tabindex="1" onClick="zipcodelook(); return false"><asp:Image ID="Image1" runat="server" ImageUrl="images/lookup_icon.gif" /></a></td>
              </tr>
             
              
            
              </table>
                  <asp:Button ID="UpdateButton" CssClass="buttonM" runat="server" CausesValidation="False"  OnClick="UpdateButton_Click"
                      Text="Save">
                  </asp:Button>
                  <asp:Button ID="UpdateCancelButton" CssClass="buttonM" runat="server" CausesValidation="False" CommandName="Cancel"
                      Text="Cancel">
                  </asp:Button>
              </EditItemTemplate>
              <InsertItemTemplate>
              <table class="shade">
              <tr>
              <td align="right" style="padding-right:5px;"><b>Code:</b></td>
              <td width="40px" ><b><asp:Textbox    ID="codeTextBox" MaxLength="8" Width="60px" runat="server" Text='<%# Bind("comp_code") %>'>
                  </asp:TextBox></b></td>
              <td   align="right" style="padding-right:5px;"><b>Name:</b></td>
              <td width="100px"><b><asp:TextBox ID="nameTextBox" MaxLength="30" Width="175px" runat="server" Text='<%# Bind("name") %>'>
                  </asp:TextBox></b></td>
              </tr>
              <tr><td></td><td></td>
              <td align="right" style="padding-right:5px;"><b>Address:</b></td>
              <td style="width:80px;"><b><asp:TextBox MaxLength="30" TabIndex="0"  Width="175px" ID="address1TextBox" runat="server" Text='<%# Bind("address1") %>'>
                  </asp:TextBox></b></td>
              </tr> <tr><td></td><td></td>   
              <td align="right" style="padding-right:5px;"><b>Address:</b></td>
              <td><b><asp:TextBox ID="address2TextBox" MaxLength="30" Width="175px" runat="server" Text='<%# Bind("address2") %>'>
                  </asp:TextBox></b></td>
              </tr>
              <tr><td></td><td></td>
              <td align="right" style="padding-right:5px;"><b>City:</b></td>
              <td  width="80px"><b><asp:TextBox MaxLength="15" TabIndex="0"  Width="100px" ID="cityTextBox" runat="server" Text='<%# Bind("city") %>'>
                  </asp:TextBox></b></td>
              <td align="right" style="padding-right:5px;"><b>State:</b></td>
              <td><b><asp:TextBox ID="stateTextBox" MaxLength="15" Width="100px" runat="server" Text='<%# Bind("state") %>'>
                  </asp:TextBox></b><a href="#" tabindex="1" onClick="statecodelook(); return false"><asp:Image ID="Image2" runat="server" ImageUrl="images/lookup_icon.gif" /></a></td>
                  <td align="right" style="padding-right:5px;"><b>Zip:</b></td>
              <td><b><asp:TextBox ID="zipTextBox" MaxLength="8" Width="100px" runat="server" Text='<%# Bind("zip") %>'>
                  </asp:TextBox></b><a href="#" tabindex="1" onClick="zipcodelook(); return false"><asp:Image ID="Image1" runat="server" ImageUrl="images/lookup_icon.gif" /></a></td>
              </tr>
             
              
            
              </table>
                    
                  
                  <asp:TextBox ID="TextBox2" Visible="false" MaxLength="8" Width="100px" runat="server" Text='<%# Bind("rec_key") %>'>
                  </asp:TextBox>
                  
                  <asp:TextBox ID="TextBox1" Visible="false" MaxLength="8" Width="100px" runat="server" Text='<%# Bind("rec_key_value") %>'>
                  </asp:TextBox>
                  
                  
                  <asp:Button ID="InsertButton" CssClass="buttonM" runat="server" CausesValidation="True" 
                      Text="Save" OnClick="InsertButton_Click">
                  </asp:Button>
                  <asp:Button ID="InsertCancelButton" CssClass="buttonM" runat="server" CausesValidation="False" CommandName="Cancel"
                      Text="Cancel">
                  </asp:Button>
              </InsertItemTemplate>
              <ItemTemplate>
                  <table class="shade">
              <tr>
              <td align="right" style="padding-right:5px;"><b>Code:</b></td>
              <td style="width:80px;"><b><asp:label  BackColor="Turquoise" BorderColor="Black" BorderStyle="solid" BorderWidth="1px" TabIndex="0"  Width="60px" ID="codeTextBox" runat="server" Text='<%# Bind("comp_code") %>'>
                  </asp:label></b></td>
              <td align="right" style="padding-right:5px;"><b>Name:</b></td>
              <td><b><asp:label ID="nameTextBox"  BackColor="Turquoise" BorderColor="Black" BorderStyle="solid" BorderWidth="1px" Width="175px" runat="server" Text='<%# Bind("name") %>'>
                  </asp:label></b></td>
              </tr>
              <tr><td></td><td></td>
              <td align="right" style="padding-right:5px;"><b>Address:</b></td>
              <td style="width:80px;"><b><asp:label TabIndex="0"  Width="175px" ID="address1TextBox"  BackColor="Turquoise" BorderColor="Black" BorderStyle="solid" BorderWidth="1px" runat="server" Text='<%# Bind("address1") %>'>
                  </asp:label></b></tr><tr>
              <td></td><td></td><td align="right" style="padding-right:5px;"><b>Address:</b></td>
              <td><b><asp:label ID="address2TextBox"  BackColor="Turquoise" BorderColor="Black" BorderStyle="solid" BorderWidth="1px" Width="175px" runat="server" Text='<%# Bind("address2") %>'>
                  </asp:label></b></tr>
              
              <tr><td></td><td></td>
              <td align="right" style="padding-right:5px;"><b>City:</b></td>
              <td style="width:80px;"><b><asp:label  TabIndex="0"  BackColor="Turquoise" BorderColor="Black" BorderStyle="solid" BorderWidth="1px"  Width="100px" ID="cityTextBox" runat="server" Text='<%# Bind("city") %>'>
                  </asp:label></b></td>
              <td align="right" style="padding-right:5px;"><b>State:</b></td>
              <td><b><asp:label ID="stateTextBox"  BackColor="Turquoise" BorderColor="Black" BorderStyle="solid" BorderWidth="1px" Width="100px" runat="server" Text='<%# Bind("state") %>'>
                  </asp:label></b></td>
                            <td align="right" style="padding-right:5px;"><b>Zip:</b></td>
              <td><b><asp:label ID="zipTextBox"  BackColor="Turquoise" BorderColor="Black" BorderStyle="solid" BorderWidth="1px" Width="100px" runat="server" Text='<%# Bind("zip") %>'>
                  </asp:label></b></td>
              </tr>
              </table>
              
              <asp:label ID="Label1" Visible="false"  BackColor="Turquoise" BorderColor="Black" BorderStyle="solid" BorderWidth="1px" Width="100px" runat="server" Text='<%# Bind("rec_key_value") %>'>
                  </asp:label>
                  
                  <asp:label ID="Label2" Visible="false"  BackColor="Turquoise" BorderColor="Black" BorderStyle="solid" BorderWidth="1px" Width="100px" runat="server" Text='<%# Bind("reckey") %>'>
                  </asp:label>
                  
              
                  <asp:Button ID="AddButton" runat="server" CssClass="buttonM" CausesValidation="False" CommandName="new"
                      Text="Add" OnClick="AddButton_Click">
                  </asp:Button>
                  <asp:Button ID="EditButton" runat="server" CssClass="buttonM" CausesValidation="False" CommandName="Edit"
                      Text="Update">
                  </asp:Button>
                  <asp:Button ID="DeleteButton" runat="server" OnClick="DeleteButton_Click" CssClass="buttonM" CausesValidation="False" CommandName="Delete" OnClientClick="return confirm('Are you sure you want to delete')"
                      Text="Delete">
                  </asp:Button>
              </ItemTemplate>
          </asp:FormView>
      
    </div>
    </form>
</body>
</html>
