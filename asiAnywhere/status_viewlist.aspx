<%@ Page Language="c#" AutoEventWireup="true" Debug="false" Inherits="Cstatus_viewlist" Codebehind="status_viewlist.aspx.cs" %>
<%@ Register Src="footer.ascx" TagName="Footer" TagPrefix="ft" %>
<%@ Register Src="header.ascx" TagName="Header" TagPrefix="hd" %>
<html xmlns="http://www.w3.org/1999/xhtml" >
  <head id="Head1" runat="server">
    <title>Status</title>
    <link href="include/style2.css" type="text/css" rel="stylesheet"/>
     
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
    
    </script> 
</head>
<body>
    <form id="frmList" runat="server">
    <hd:header id="Header1" runat="server"></hd:header>    
    <div>
    <TABLE id="tblTop" cellSpacing="3" border="0">
        <TR>
          
          <TD align=center nowrap><font size=+0><b>Status&nbsp;</b></font></TD>
          <TD vAlign="middle">
            <asp:linkbutton id="hlkBackToMenu" runat="server" OnClick="hlkBackToMenu_Click">Back to menu </asp:linkbutton>
          </TD>
          <TD vAlign="middle" align="center"><b>Users</b>&nbsp;&nbsp;Logged as&nbsp;
            <asp:label id="lblUser" runat="server" Font-Bold="True">&nbsp;</asp:label>&nbsp;&nbsp;
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
      <table><tr bgcolor="gray">
      <td><div  id="navigation" style="width:100%">
		<ul nowrap> <li  >
      <asp:LinkButton ID="lnk_status_list" runat="server" OnClick="lnk_status_list_Click" >List Status</asp:LinkButton></li>
      <li class="selected"><asp:LinkButton ID="lnk_status_view" runat="server" OnClick="lnk_status_view_Click" >View Status</asp:LinkButton></li></ul></div>
      </td>
      
      </tr></table>
      <asp:label id="lblMessage" runat="server" ForeColor="Red"></asp:label>
      <asp:Button ID="AddNewButton" runat="server" Text="ADD" CssClass="buttonM" OnClick="AddNewButton_Click" />
      <asp:sqldatasource id="Sqldatasource1"
                SelectCommand="select [status_code],   [description]   From [dbo].[status] where [status_code]=@status_code ORDER BY [status_code] ASC"
               
                InsertCommand="insert into [dbo].[status] ([status_code], [description]) values (@status_code, @description) "
                UpdateCommand="update [dbo].[status] set [description]=@description where  [status_code]=@status_code"
                ConnectionString="<%$ ConnectionStrings:Project1ConnectionString%>"
                ProviderName="<%$ ConnectionStrings:Project1ConnectionString.providerName%>"
                
           runat="server" OnInserted="Sqldatasource1_Inserted" OnDeleted="Sqldatasource1_Deleted">
        <SelectParameters >
        <asp:SessionParameter SessionField="status_list_code" Name="status_code" Type="string" DefaultValue="1" />
        </SelectParameters>
         <%--<DeleteParameters>
           <asp:Parameter Name="status_code" Type="string"  />
           </DeleteParameters>--%>
         <%--<InsertParameters>
            <asp:Parameter Name="status_code" Type="String"/>
            <asp:Parameter Name="description" Type="String"/>
            
             </InsertParameters>--%>
             <UpdateParameters>
              <asp:Parameter Name="description" Type="String"/>

               <%--<asp:QueryStringParameter  QueryStringField="status_code" Name="status_code" Type="String" DefaultValue="-1" />--%>

        </UpdateParameters>
           
            </asp:sqldatasource>
          <asp:FormView ID="FormView1" runat="server" DataSourceID="Sqldatasource1"   OnDataBound="FormView1_DataBound">
              <EditItemTemplate>
              <table class="shade">
              <tr><td><b>Code:</b></td>
              <td style="width:80px;"><b> <asp:Label ID="codeTextBox"  runat="server" Text='<%# Bind("status_code") %>'>
                  </asp:Label></b></td>
              <td><b>Description:</b></td>
              <td><asp:TextBox ID="descriptionTextBox" MaxLength="30"  Width="175px" runat="server" Text='<%# Bind("description") %>'>
                  </asp:TextBox></td>
              </tr>
             
                  
                 
                 
             <tr><td>
                  <asp:Button ID="UpdateButton" runat="server" CssClass="buttonM" CausesValidation="True" CommandName="Update"
                      Text="Save">
                  </asp:Button></td><td>
                  <asp:Button ID="UpdateCancelButton" runat="server" CssClass="buttonM" CausesValidation="False" CommandName="Cancel"
                      Text="Cancel">
                  </asp:Button></td></tr> </table>
              </EditItemTemplate>
              <InsertItemTemplate>
              <table class="shade">
              <tr>
              <td><b>Code:</b></td>
              <td style="width:80px;"><asp:TextBox ID="codeTextBox" MaxLength="5"   Width="40px" runat="server" Text='<%# Bind("status_code") %>'>
                  </asp:TextBox></td>
              <td><b>Description:</b></td>
              <td><asp:TextBox ID="descriptionTextBox" MaxLength="30" Width="175px" runat="server" Text='<%# Bind("description") %>'>
                  </asp:TextBox></td>
              </tr><tr>
              <td><asp:Button ID="InsertButton" runat="server" CssClass="buttonM" CausesValidation="True" OnClick="InsertButton_Click" CommandName="Insert"
                      Text="Save" >
                  </asp:Button></td>
              <td><asp:Button ID="InsertCancelButton" runat="server" CssClass="buttonM" CausesValidation="False" CommandName="Cancel"
                      Text="Cancel">
                  </asp:Button></td>
              </tr>
              
              </table>
                  
                  
                  
              </InsertItemTemplate>
              <ItemTemplate>
                  <table class="shade"><tr><td><b>Code:</b>
                  <asp:Label ID="codeLabel" runat="server" BackColor="Turquoise" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" Text='<%# Bind("status_code") %>' Width="85px"></asp:Label></td>
                  <td><b>Description:</b></td>
                  <td><asp:Label ID="descriptionLabel" runat="server" BackColor="Turquoise" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" Text='<%# Bind("description") %>' Width="120px"></asp:Label></td></tr>
                  <tr><td><asp:Button ID="AddButton" CssClass="ButtonM" runat="server" CausesValidation="False" CommandName="new"
                      Text="Add "></asp:Button>
                      <asp:Button ID="EditButton" runat="server" CssClass="buttonM" CausesValidation="False" CommandName="edit"
                      Text="Update"></asp:Button>
                  <asp:Button ID="DeleteButton" runat="server" CssClass="buttonM" CausesValidation="False" OnClientClick="return confirm('Are you sure you want to delete this record')" 
                      Text="Delete" OnClick="DeleteButton_Click">
                  </asp:Button></td></tr></table>
              </ItemTemplate>
          </asp:FormView>
          
          
    
    </div>
    <ft:footer id="Footer1" runat="server"></ft:footer>
    </form>
</body>
</html>
