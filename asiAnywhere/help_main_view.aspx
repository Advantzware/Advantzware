<%@ Page Language="c#" AutoEventWireup="true" Debug="false" Inherits="help_main_view" Codebehind="help_main_view.aspx.cs" %>
<%@ Register Src="footer.ascx" TagName="Footer" TagPrefix="ft" %>
<%@ Register Src="header.ascx" TagName="Header" TagPrefix="hd" %>
<html xmlns="http://www.w3.org/1999/xhtml" >
  <head id="Head1" runat="server">
    <title>Client Maintenance</title>
    
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
 
    
    </script> 
  </head>    
   <body>
    <form id="frmList" runat="server" >   
        <hd:header id="Header1" runat="server"></hd:header>
    <asp:HiddenField ID="HiddenField1" runat="server" />
      <div>
            
      <TABLE id="tblTop" cellSpacing="3" border="0">
        <TR>
          
          <TD align=center nowrap><font size=+0><b>Client Maintenance&nbsp;</b></font></TD>
          <TD vAlign="middle">
            <asp:linkbutton id="hlkBackToMenu" runat="server" OnClick="hlkBackToMenu_Click">Back to menu</asp:linkbutton>
          </TD>
          <TD vAlign="middle" align="center"><b>Users</b>&nbsp;&nbsp;&nbsp;Logged as&nbsp;
            <asp:label id="lblUser" runat="server" Font-Bold="True">&nbsp;</asp:label>&nbsp;&nbsp;&nbsp;
            <asp:linkbutton id="hlnkLogOut" runat="server" OnClick="hlnkLogOut_Click">Log out</asp:linkbutton>
            &nbsp;&nbsp;&nbsp;<asp:hyperlink id="hlnkChangePwd" runat="server" NavigateUrl="changepwd.aspx"></asp:hyperlink>
          &nbsp;<b>Company:</b> &nbsp;<asp:label id="lblComp" runat="server" Font-Bold="True">&nbsp;</asp:label>
</TD>
          
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
        <asp:LinkButton ID="lnk_listview_industry" runat="server"  OnClick="lnk_listview_industry_Click" >List Client</asp:LinkButton></li>
        <li class="selected"><asp:LinkButton ID="lnk_listview_sic" runat="server"  OnClick="lnk_listview_sic_Click">View Client</asp:LinkButton></li></ul></div>
        
    </td>
    </tr>
    </table> <asp:label id="lblMessage" runat="server" ForeColor="Red"></asp:label>
    
      <asp:sqldatasource id="Sqldatasource1"
                SelectCommand="SELECT * FROM [client_id] WHERE ([client_id] = @client_id) ORDER BY [client_id]"
                
                ConnectionString="<%$ ConnectionStrings:Project1ConnectionString %>"
                ProviderName="<%$ ConnectionStrings:Project1ConnectionString.providerName %>"
                OnSelected="help_mainSqlDataSource_Selected" 
           runat="server" OnDeleted="Sqldatasource1_Deleted" 
              OnInserted="Sqldatasource1_Inserted">
        <SelectParameters>
        <asp:SessionParameter SessionField="help_main_list_code" Name="client_id" Type="String"  />
        </SelectParameters>
        <%--<DeleteParameters>
        <asp:SessionParameter SessionField="industry_sic_list_code" Name="industry_sic_code" Type="String" />
        </DeleteParameters>--%>
            </asp:sqldatasource>     
      
          <asp:FormView ID="FormView1" runat="server" DataSourceID="Sqldatasource1" 
              OnDataBound="FormView1_DataBound" >
              <EditItemTemplate>
              <asp:Panel ID="Edit_panel" runat="server" DefaultButton="UpdateButton">
                  <table class="shade"><tr><td><b>Client Id:</b></td><td>
                  <asp:Label ID="client_idLabel" BackColor="paleturquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" Width="120px" runat="server"  Text='<%# Bind("client_id") %>' />
                                  
                  </td>
                  <td><b>Paid:</b></td>
                  <td><asp:Label ID="flagsLabel" Visible="false" Width="50px" BackColor="paleturquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" runat="server" Text='<%# Bind("flags") %>' />
                  <asp:CheckBox ID="CheckBox1"  Width="10px" runat="server" /> </td>
                  </tr>
                  <tr><td colspan="4"><br />
                  <asp:Button ID="UpdateButton" runat="server" CausesValidation="True"  CssClass="button" OnClick="UpdateButton_Click" Text="Save" />
                  &nbsp;<asp:Button ID="UpdateCancelButton" runat="server" CausesValidation="False" CssClass="button" CommandName="Cancel" Text="Cancel" />
                  </td></tr></table>
               </asp:Panel>   
              </EditItemTemplate>
              <InsertItemTemplate>
               <asp:Panel ID="Insert_panel" runat="server" DefaultButton="InsertButton">
                  <table class="shade"><tr><td><b>Client Id:</b></td><td>
                  <asp:TextBox ID="client_idTextBox" runat="server"  Text='<%# Bind("client_id") %>' />
                                  
                  </td>
                  <td><b>Paid:</b></td>
                  <td><asp:CheckBox ID="CheckBox1"  Width="10px" runat="server" /></td>
                  </tr>
                  <tr><td colspan="4">
                  <br />
                  <asp:Button ID="InsertButton" runat="server" CausesValidation="True"  CssClass="button" OnClick="InsertButton_Click" Text="Save" />
                  &nbsp;<asp:Button ID="InsertCancelButton" runat="server" CausesValidation="False" CssClass="button" CommandName="Cancel" Text="Cancel" />
                  </td></tr></table>
                </asp:Panel>
              </InsertItemTemplate>
              <ItemTemplate>
              
                  <table class="shade"><tr><td><b>Client Id:</b></td><td>
                  <asp:Label ID="client_idLabel" Width="120px" BackColor="paleturquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" runat="server" Text='<%# Bind("client_id") %>' />
                                 
                  </td>
                  <td><b>Paid:</b></td>
                  <td><asp:Label ID="flagsLabel" Visible="false" Width="50px" BackColor="paleturquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" runat="server" Text='<%# Bind("flags") %>' />
                  <asp:CheckBox ID="CheckBox1"  Enabled="false" Width="10px" runat="server" BackColor="paleturquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" /></td>
                  </tr>
                  <tr><td colspan="4"><br />
                  <asp:Button ID="AddButton" CssClass="button" runat="server" CausesValidation="False" CommandName="new" 
                    Text="Add" > </asp:Button>
                    <asp:Button ID="UpdateButton" CssClass="button" runat="server" CausesValidation="False" CommandName="edit"
                    Text="Update"> </asp:Button>
                    <%--<asp:Button ID="CopyButton" CssClass="button" runat="server" CausesValidation="False" CommandName="copy"
                    Text="Copy"> </asp:Button>--%>
                    <asp:Button ID="DeleteButton" CssClass="button" runat="server" CausesValidation="False" CommandName="Delete" OnClientClick="return confirm('Are you sure to delete ?')"
                    Text="Delete" OnClick="DeleteButton_Click"></asp:Button>
                       </td></tr>            
                    </table>
              </ItemTemplate>
          </asp:FormView>
        <asp:Button ID="newaddbutton" CssClass="button" runat="server" OnClick="newaddbutton_Click" Text="Add" ></asp:Button>
    </div>
    <ft:footer id="Footer1" runat="server"></ft:footer>
    </form>
  </body>
</HTML>

