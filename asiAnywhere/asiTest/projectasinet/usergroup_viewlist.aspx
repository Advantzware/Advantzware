<%@ Page Language="c#" AutoEventWireup="true" Debug="false" Inherits="Cusergroup_viewlist" Codebehind="usergroup_viewlist.aspx.cs" %>
<%@ Register Src="footer.ascx" TagName="Footer" TagPrefix="ft" %>
<%@ Register Src="header.ascx" TagName="Header" TagPrefix="hd" %>

<html xmlns="http://www.w3.org/1999/xhtml" >
  <head id="Head1" runat="server">
    <title>User Group Maintenance</title>
    <LINK href="include/style2.css" type="text/css" rel="stylesheet"/>
    <script language = JavaScript>

  </script> 
  </head>    
   <body>
    <hd:Header ID="Header1" runat="server" />
    <form id="frmList" runat="server" >   

      <div align="left" style="width:850px">
            
      <TABLE id="tblTop" cellSpacing="3" align="center" border="0" Width="100%">
        <TR>
          <TD width=30>&nbsp;</TD>
          <TD align=center nowrap><font size=+0><b>User Group Maintenance&nbsp;</b></font></TD>
          <TD vAlign="middle">
            <asp:linkbutton id="hlkBackToMenu" runat="server" ></asp:linkbutton>
          </TD>
          <TD vAlign="middle" align="center">Logged as&nbsp;
            <asp:label id="lblUser" runat="server" Font-Bold="True">&nbsp;</asp:label>&nbsp;&nbsp;&nbsp;
            <asp:linkbutton id="hlnkLogOut" runat="server" OnClick="hlnkLogOut_Click">Log out</asp:linkbutton>
            
            &nbsp;&nbsp;&nbsp;<asp:hyperlink id="hlnkChangePwd" runat="server" NavigateUrl="changepwd.aspx">Change password</asp:hyperlink>
            
          </TD>
          
                    
          <TD vAlign="middle" width="20">&nbsp;</TD>
          
         
          <td width=30>&nbsp;</td>
        </TR>
      </TABLE>
      <table><tr bgcolor="gray"><td> <div  id="navigation" style="width:100%">
		<ul nowrap> <li >
      <asp:LinkButton ID="lnk_listgroup" runat="server" OnClick="lnk_listgroup_Click" >List Group</asp:LinkButton></li>
      <li class="selected"><asp:LinkButton ID="lnk_viewgroup" runat="server" OnClick="lnk_viewgroup_Click" >View Group</asp:LinkButton></li>
      <li><asp:LinkButton ID="lnk_groupuser" runat="server" OnClick="lnk_groupuser_Click">User Group</asp:LinkButton></li></ul></div>
      
      
      </td>
      </tr></table>
      <asp:label id="lblMessage" runat="server" ForeColor="Red"></asp:label>
         <asp:sqldatasource id="usergroupSqlDataSource"
                SelectCommand="select [groupID],   [group],[groupID2]=(select max(groupID) from [dbo].[usergroup])    From [dbo].[usergroup] where [groupID]=@groupID ORDER BY [groupID] ASC"
                InsertCommand="insert into [dbo].[usergroup] ([group]) values (@group) "
                 UpdateCommand="update [dbo].[usergroup] set [group]=@group where  [groupID]=@groupID"
                DeleteCommand="delete from [dbo].[usergroup] where [groupID]=@groupID"
                ConnectionString="<%$ ConnectionStrings:Project1ConnectionString%>"
                ProviderName="<%$ ConnectionStrings:Project1ConnectionString.providerName%>"
               
           runat="server">
              <SelectParameters>
              <asp:SessionParameter Name="groupID" SessionField="user_group_code" Type="string" DefaultValue="1" />
              </SelectParameters>
              <DeleteParameters>
              <asp:SessionParameter Name="groupID" SessionField="user_group_code" Type="string" DefaultValue="1" />
              </DeleteParameters>
           <InsertParameters>
            <asp:Parameter Name="group" Type="String"/>
           
            
        </InsertParameters>
        <UpdateParameters>
            <asp:Parameter Name="group" Type="String"/>

            <%--<asp:QueryStringParameter  QueryStringField="groupID" Name="groupID" Type="Int32" DefaultValue="-1" />--%>
            <asp:SessionParameter Name="groupID" SessionField="user_group_code" Type="string" />
        </UpdateParameters>
            </asp:sqldatasource>
          <asp:FormView ID="FormView1" runat="server" DataSourceID="usergroupSqlDataSource" OnDataBound="FormView1_DataBound">
              <EditItemTemplate>
                    <table class="shade">
                  <tr><td><b>GroupID:</b></td>
                  <td align="center"><asp:Label ID="groupIDLabel1" runat="server" Width="50px" Text='<%# Eval("groupID") %>'></asp:Label></td>
                  <td><b>Group:</b></td>
                 <td> <asp:TextBox ID="groupTextBox" runat="server" Text='<%# Bind("group") %>'>
                  </asp:TextBox></td></tr>
                  <td colspan="2"><asp:Button ID="UpdateButton" runat="server" CssClass="button" CausesValidation="True" CommandName="Update"
                      Text="Save">
                  </asp:Button>
                  <asp:Button ID="UpdateCancelButton" runat="server" CssClass="button" CausesValidation="False" CommandName="Cancel"
                      Text="Cancel">
                  </asp:Button></td></tr></table>
              </EditItemTemplate>
              <InsertItemTemplate>
                    <table class="shade">
                   <tr><td><b> GroupID:</b></td>
                    <td><asp:Label ID="groupIDLabel1" runat="server" Text='<%# Eval("groupID") %>'></asp:Label></td>
                  <td><b>Group:</b></td>
                  <td><asp:TextBox ID="groupTextBox" runat="server" Text='<%# Bind("group") %>'>
                  </asp:TextBox></td></tr>
                  <td colspan="2"><asp:Button ID="InsertButton" CssClass="button" runat="server" CausesValidation="True" CommandName="Insert"
                      Text="save">
                  </asp:Button>
                  <asp:Button ID="InsertCancelButton" CssClass="button" runat="server" CausesValidation="False" CommandName="Cancel"
                      Text="Cancel">
                  </asp:Button></td></tr></table>
              </InsertItemTemplate>
              <ItemTemplate>
                <table class="shade"> <tr><td><b>GroupID:</b></td>
                 <td><asp:Label ID="groupIDLabel" runat="server" Width="100px" BackColor="paleturquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" Text='<%# Eval("groupID") %>'></asp:Label></td>
                  <td><b>Group:</b></td>
                  <td><asp:Label ID="groupLabel" runat="server" Width="100px" BackColor="paleturquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" Text='<%# Bind("group") %>'></asp:Label></td></tr>
                  <asp:Label ID="groupID2Label" Visible="false" runat="server" Text='<%# Bind("groupID2") %>'></asp:Label>
                  <td colspan="3"><asp:Button ID="NewButton" runat="server" CssClass="button" CausesValidation="False" CommandName="New"
                      Text="Add" OnClick="AddButton_Click">
                  </asp:Button>
                  <asp:Button ID="EditButton" runat="server" CssClass="button" CausesValidation="False" CommandName="Edit"
                      Text="Update">
                  </asp:Button>
                  
                  
                  <asp:Button ID="DeleteButton" runat="server" CssClass="button" CausesValidation="False" CommandName="Delete"  OnClientClick="return confirm('Are you sure want to Delete?')"
                      Text="Delete">
                  </asp:Button></td></tr></table>
              </ItemTemplate>
          </asp:FormView>
    </div>
    </form>
</body>
</html>
