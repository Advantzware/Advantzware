<%@ Page Language="c#" AutoEventWireup="true" Debug="false" Inherits="bank_reco_view" Codebehind="bank_reco_view.aspx.cs" %>
<%@ Register Src="footer.ascx" TagName="Footer" TagPrefix="ft" %>
<%@ Register Src="header.ascx" TagName="Header" TagPrefix="hd" %>
<html xmlns="http://www.w3.org/1999/xhtml" >
  <head id="Head1" runat="server">
    <title>Bank Reconciliation</title>
    <LINK href="include/style2.css" type="text/css" rel="stylesheet"/>
    <LINK REL="stylesheet" TYPE="text/css" HREF="include/CalendarControl.css" >
    <link href="include/tree.css" rel="stylesheet" type="text/css" />
    <link href="include/dhtmlwindow.css" rel="stylesheet" type="text/css" />
    <script language="javascript" src="include/date.js"></script>
    <script language="javascript" src="include/event.js"></script>
    <script language="javascript" src="include/insert.js"></script>
    <script language = "JavaScript" src="include/CalendarControl.js"></script>
    <script language ="javascript">
    
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
    
    
function vendorlook() {
     
    var NewWindow = window.open("corvend_lookup.aspx", "VendLookup", "width=500,height=400,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}
function VendLookup(ReturnObj1) {
    document.forms[0].vendor_TextBox.value = ReturnObj1;

}
function ordernotes() {
    var NewWindow = window.open("top_list_notes.aspx", "ListNotes", "width=600,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}



    </script> 
  </head>    
   <body>
    <form id="frmList" runat="server"  defaultfocus='bank_TextBox'>   
        <hd:header id="Header1" runat="server"></hd:header>
      
       <table width="100%"><tr><td><div>
        <table align="left" border="1" width="75%">
                <tr class="topheadcolor">
                        <td nowrap width="25px";>
                        <a href="#" onClick="ordernotes(); return false"><asp:Image ID="img_btn_notes" Width="35px" ToolTip="Notes" runat="server" ImageUrl="~/Images/edit.ico" /></a>                        
                        </td>
                        <td nowrap width="25px";>
                        <asp:ImageButton ID="img_btn_exit" runat="server" Width="35px" ImageUrl="~/Images/exit-au.bmp" ToolTip="LogOut" OnClick="img_btn_exit_click" />
                        </td>
                        
                        <td nowrap> &nbsp;</td>
                </tr>
      </table></div>
          </td>
      </tr>
      <tr>
      <td>
      
      <TABLE id="tblTop" cellSpacing="3" align="center" border="0" Width="100%">
        <TR>         
          <TD align=left nowrap><font size=+0><b>Bank Reconciliation&nbsp;</b></font></TD>
          <TD >
            <asp:linkbutton id="hlkBackToMenu" OnClick="Back_tomenu_Click" runat="server" >Back to menu</asp:linkbutton>
          
            &nbsp;&nbsp;Logged as&nbsp;
            <asp:label id="lblUser" runat="server" Font-Bold="True">&nbsp;</asp:label>&nbsp;&nbsp;&nbsp;
            <asp:linkbutton id="hlnkLogOut" runat="server" OnClick="hlnkLogOut_Click">Log out</asp:linkbutton>
            &nbsp;&nbsp;<asp:hyperlink id="hlnkChangePwd" runat="server" NavigateUrl="changepwd.aspx"></asp:hyperlink>
            &nbsp;<b>Company:</b>&nbsp;  <asp:Label ID="labelcompany" runat="server" Text="Label"></asp:Label></TD>
          
         
          <TD vAlign="middle" width="20">&nbsp;</TD>
          
          <td width=30>&nbsp;</td>
        </TR>
      </TABLE>
      <table><tr bgcolor="gray"><td><div  id="navigation" style="width:100%">
		<ul nowrap> <li >
      <asp:LinkButton ID="lnk_Listcustomers" OnClick="lnk_viewcustomers_Click" runat="server" >List Bank Reco</asp:LinkButton></li>
      <li class="selected"><asp:LinkButton ID="lnk_viewcustomers" runat="server"  > View Bank Reco</asp:LinkButton></li></ul></div>
            
      </td>
      </tr></table>
     
          <asp:FormView ID="FormView1" runat="server" DataSourceID="ObjectDataSource1">
              <EditItemTemplate>
                   <asp:Panel ID="editPanel" runat="server" DefaultButton="UpdateButton">
                  <fieldset class="shade">
                  <table>
                  <tr><td><b>Check/Journal#:</b></td>
                  <td><b>Trans Date:</b></td><td><b>Amount:</b></td>
                  <td><b>Bank Code:</b></td><td><b>Vendor#:</b></td>
                  <td><b>Name:</b></td><td><b>Cleared?:</b></td></tr>
                  
                  <tr><td><asp:Label ID="numberLabel" Width="100px"  BackColor="turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" runat="server" Text='<%# Bind("number") %>' /></td>
                  <td><asp:Label ID="vdateLabel" Width="100px"  BackColor="turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" runat="server" Text='<%# Bind("vdate") %>' /></td>
                  <td><asp:Label ID="amtLabel" Width="100px"  BackColor="turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" runat="server" Text='<%# Bind("amt") %>' /></td>
                  <td><asp:Label ID="bankLabel" Width="100px"  BackColor="turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" runat="server" Text='<%# Bind("bank") %>' /></td>
                  <td><asp:Label ID="vendLabel" Width="100px"  BackColor="turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" runat="server" Text='<%# Bind("vend") %>' /></td>
                  <td><asp:Label ID="vendnameLabel" Width="150px"  BackColor="turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" runat="server" Text='<%# Bind("vendname") %>' /></td>
                  <td nowrap> <asp:RadioButtonList ID="RadioButtonList1" runat="server" RepeatColumns="2" CellSpacing="2"  SelectedValue='<%# Bind("cleared") %>' runat="server">
                      <asp:ListItem Text="Yes" Value="yes"></asp:ListItem>
                      <asp:ListItem Text="No" Value="no"></asp:ListItem>
                      </asp:RadioButtonList>
                  </td></tr>
                  <tr><td colspan="4">
                  <asp:Label ID="reckeyLabel" runat="server" Visible="false" Text='<%# Bind("reckey") %>' />
                  <asp:Button ID="UpdateButton" runat="server" CausesValidation="True" CssClass="button" OnClick="UpdateButton_Click"
                       Text="Save" />
                  &nbsp;<asp:Button ID="UpdateCancelButton" runat="server" 
                     CssClass="button" CausesValidation="False" CommandName="Cancel" Text="Cancel" />
                     </td></tr></table></fieldset></asp:Panel>
              </EditItemTemplate>             
              <ItemTemplate>
                  <asp:Panel ID="itemPanel" runat="server" DefaultButton="UpdateItemButton">
                  <fieldset class="shade">
                  <table>
                  <tr><td><b>Check/Journal#:</b></td>
                  <td><b>Trans Date:</b></td><td><b>Amount:</b></td>
                  <td><b>Bank Code:</b></td><td><b>Vendor#:</b></td>
                  <td><b>Name:</b></td><td><b>Cleared?:</b></td></tr>
                  
                  <tr><td><asp:Label ID="numberLabel" Width="100px"  BackColor="turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" runat="server" Text='<%# Bind("number") %>' /></td>
                  <td><asp:Label ID="vdateLabel" Width="100px"  BackColor="turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" runat="server" Text='<%# Bind("vdate") %>' /></td>
                  <td><asp:Label ID="amtLabel" Width="100px"  BackColor="turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" runat="server" Text='<%# Bind("amt") %>' /></td>
                  <td><asp:Label ID="bankLabel" Width="100px"  BackColor="turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" runat="server" Text='<%# Bind("bank") %>' /></td>
                  <td><asp:Label ID="vendLabel" Width="100px"  BackColor="turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" runat="server" Text='<%# Bind("vend") %>' /></td>
                  <td><asp:Label ID="vendnameLabel" Width="150px"  BackColor="turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" runat="server" Text='<%# Bind("vendname") %>' /></td>
                  <td><asp:Label ID="clearedLabel" Width="50px"  BackColor="turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" runat="server" Text='<%# Bind("cleared") %>' /></td></tr>
                  <tr><td colspan="4">
                  <asp:Label ID="reckeyLabel" runat="server" Visible="false" Text='<%# Bind("reckey") %>' />
                  
                   <asp:Button ID="UpdateItemButton" runat="server" CssClass="button" CommandName="Edit" Text="Update" />
                  </td></tr>
                  </table>
                  
                  </fieldset>                  
                  </asp:Panel>
                 
                  
                  
              </ItemTemplate>
          </asp:FormView>
            
            <asp:ObjectDataSource ID="ObjectDataSource1" runat="server" 
                OldValuesParameterFormatString="original_{0}" SelectMethod="SelectBankReconciliation" 
                TypeName="voucherpay">
                <SelectParameters>
                   <asp:Parameter DefaultValue="View" Name="prmAction" Type="String" />                    
                   <asp:Parameter Name="prmComp"  Type="string" />
                   <asp:Parameter Name="prmUser" Type="string" />                     
                   <asp:Parameter Name="prmnumber" Type="String" />
                   <asp:Parameter Name="prmvdate" Type="String" />
                   <asp:Parameter Name="prmamt" Type="Decimal" />
                   <asp:Parameter Name="prmbank" Type="Int32" />
                   <asp:Parameter  Name="prmvend" Type="String" />
                   <asp:Parameter Name="prmvendname" Type="String" />
                   <asp:Parameter Name="prmcleared" Type="String" />
                   <asp:SessionParameter SessionField="bank_reco_reckey_rec" Name="prmReckey" Type="String" />
                </SelectParameters>
            </asp:ObjectDataSource></td></tr></TABLE></asp:Panel>
                     
          </td></tr></table>
      
     
    <ft:footer id="Footer1" runat="server"></ft:footer>
    </form>
  </body>
</html>

