<%@ Page Language="c#" AutoEventWireup="true" Debug="false" Inherits="Cindustry_sic_listview" Codebehind="industry_sic_listview.aspx.cs" %>
<%@ Register Src="footer.ascx" TagName="Footer" TagPrefix="ft" %>
<%@ Register Src="header.ascx" TagName="Header" TagPrefix="hd" %>
<html xmlns="http://www.w3.org/1999/xhtml" >
  <head id="Head1" runat="server">
    <title>Industry Sic Code</title>
    
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
      <div>
            
      <TABLE id="tblTop" cellSpacing="3" border="0">
        <TR>
          
          <TD align=center nowrap><font size=+0><b>Industry Sic Code&nbsp;</b></font></TD>
          <TD vAlign="middle">
            <asp:linkbutton id="hlkBackToMenu" runat="server" OnClick="hlkBackToMenu_Click">Back to menu</asp:linkbutton>
          </TD>
          <TD vAlign="middle" align="center"><b>Users</b>&nbsp;&nbsp;&nbsp;Logged as&nbsp;
            <asp:label id="lblUser" runat="server" Font-Bold="True">&nbsp;</asp:label>&nbsp;&nbsp;&nbsp;
            <asp:linkbutton id="hlnkLogOut" runat="server" OnClick="hlnkLogOut_Click">Log out</asp:linkbutton>
            &nbsp;&nbsp;&nbsp;<asp:hyperlink id="hlnkChangePwd" runat="server" NavigateUrl="changepwd.aspx">Change password</asp:hyperlink>
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
		<ul nowrap> <li  >
        <asp:LinkButton ID="lnk_listview_industry" runat="server"  OnClick="lnk_listview_industry_Click" >List Sic Code</asp:LinkButton></li>
        <li class="selected"><asp:LinkButton ID="lnk_listview_sic" runat="server"  OnClick="lnk_listview_sic_Click">View Sic Code</asp:LinkButton></li></ul></div>
        
    </td>
    </tr>
    </table> <asp:label id="lblMessage" runat="server" ForeColor="Red"></asp:label>
    
      <asp:sqldatasource id="Sqldatasource1"
                SelectCommand="select [industry_sic_code],   [description]   From [dbo].[industry_sic] where [industry_sic_code]=@industry_sic_code ORDER BY [industry_sic_code] ASC"
                 UpdateCommand="update [dbo].[industry_sic] set [description]=@description where  [industry_sic_code]=@industry_sic_code"
                  InsertCommand="insert into [dbo].[industry_sic] ([industry_sic_code], [description]) values (@industry_sic_code, @description) "
                
                ConnectionString="<%$ ConnectionStrings:Project1ConnectionString%>"
                ProviderName="<%$ ConnectionStrings:Project1ConnectionString.providerName%>"
                OnSelected="industry_sicSqlDataSource_Selected" 
           runat="server" OnDeleted="Sqldatasource1_Deleted" OnInserted="Sqldatasource1_Inserted">
        <SelectParameters>
        <asp:SessionParameter SessionField="industry_sic_list_code" Name="industry_sic_code" Type="string"  />
        </SelectParameters>
        <InsertParameters>
        <asp:Parameter Name="industry_sic_code" Type="string" />
        <asp:Parameter Name="description" Type="string" />
        </InsertParameters>
        <%--<DeleteParameters>
        <asp:SessionParameter SessionField="industry_sic_list_code" Name="industry_sic_code" Type="String" />
        </DeleteParameters>--%>
            </asp:sqldatasource>     
      
          <asp:FormView ID="FormView1" runat="server" DataSourceID="Sqldatasource1" OnDataBound="FormView1_DataBound" >
              <EditItemTemplate>
              <table class="shade">
              <tr>
              <td><b>Code:</b></td>
              <td style="width:80px;"><b><asp:Label ID="codeTextBox" runat="server" Text='<%# Bind("industry_sic_code") %>'>
                  </asp:Label></b></td>
              <td><b>Description:</b></td>
              <td><b><asp:TextBox ID="descriptionTextBox" MaxLength="30" Width="175px" runat="server" Text='<%# Bind("description") %>'>
                  </asp:TextBox></b></td>
              </tr>
              </table>
                 
                  <asp:Button ID="UpdateButton" CssClass="buttonM" runat="server" CausesValidation="True" CommandName="Update"
                      Text="Save">
                  </asp:Button>
                  <asp:Button ID="UpdateCancelButton" CssClass="buttonM" runat="server" CausesValidation="False" CommandName="Cancel"
                      Text="Cancel">
                  </asp:Button>
              </EditItemTemplate>
              <InsertItemTemplate>
              <table class="shade">
              <tr>
              <td><b>Code:</b></td>
              <td style="width:80px;"><b><asp:TextBox ID="codeTextBox" Width="40px"  MaxLength="5" runat="server" Text='<%# Bind("industry_sic_code") %>'>
                  </asp:TextBox></b></td>
              <td><b>Description:</b></td>
              <td><b><asp:TextBox ID="descriptionTextBox" Width="175px" MaxLength="30" runat="server" Text='<%# Bind("description") %>'>
                  </asp:TextBox></b></td>
              </tr>
              </table>
                  
                  <asp:Button ID="InsertButton" CssClass="buttonM" runat="server" CausesValidation="True"  CommandName="Insert"
                      Text="Save" OnClick="InsertButton_Click">
                  </asp:Button>
                  <asp:Button ID="InsertCancelButton" CssClass="buttonM" runat="server" CausesValidation="False" CommandName="Cancel"
                      Text="Cancel">
                  </asp:Button>
              </InsertItemTemplate>
              <ItemTemplate>
              <table class="shade" width="450px">
              
              <tr>
              <td align="left" style="padding-right:5px;"><b>Code:</b>
              <b><asp:Label ID="codeLabel"  BackColor="PaleTurquoise" BorderColor="White" Width="70px" BorderStyle="Solid" BorderWidth="1px" runat="server" Text='<%# Bind("industry_sic_code") %>'></asp:Label></b></td>
              <td><b>Description:</b></td>
              <td><b><asp:Label ID="descriptionLabel"  BackColor="PaleTurquoise" BorderColor="White" Width="183px" BorderStyle="Solid" BorderWidth="1px" runat="server" Text='<%# Bind("description") %>'>
                  </asp:Label></b></td>
              <tr><td></td></tr>
              <tr>
                <td>
                          <asp:Button ID="AddButton" CssClass="buttonM" runat="server"  CausesValidation="False" CommandName="New"
                      Text="Add">
                     
                  </asp:Button>
                  
                  <asp:Button ID="UpdateButton" CssClass="buttonM" runat="server" CausesValidation="False" CommandName="Edit"
                      Text="Update">
                  </asp:Button>
                  <asp:Button ID="DeleteButton" CssClass="buttonM" runat="server" CausesValidation="False" OnClientClick="return confirm('Are you sure you want to delete this record')"
                      Text="Delete" OnClick="DeleteButton_Click">
                  </asp:Button>
                 </td> </tr></table>
              </ItemTemplate>
          </asp:FormView>
          <asp:Button ID="newaddbutton" CssClass="buttonm" runat="server" OnClick="newaddbutton_Click" Text="Add" ></asp:Button>
    </div>
    <ft:footer id="Footer1" runat="server"></ft:footer>
    </form>
  </body>
</HTML>

