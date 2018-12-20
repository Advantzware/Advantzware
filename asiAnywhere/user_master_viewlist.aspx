<%@ Page Language="C#" AutoEventWireup="true" Inherits="user_master_viewlist" Codebehind="user_master_viewlist.aspx.cs" %>

<%@ Register Src="footer.ascx" TagName="Footer" TagPrefix="ft" %>
<%@ Register Src="header.ascx" TagName="Header" TagPrefix="hd" %>
<html xmlns="http://www.w3.org/1999/xhtml" >
  <head id="Head1" runat="server">
    <title>User Maintenance</title>
    <link href="include/style2.css" type="text/css" rel="stylesheet"/>
    <script language = JavaScript>
    function groupidlook(){ 
  var NewWindow = window.open("user_groupidlookup.aspx","groupidlookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}
function groupidlookup(ReturnObj1){ 
  document.forms[0].FormView1$GroupIDTextBox.value = ReturnObj1;
}
    </script>
</head>
<body>
    <form id="viewlist" runat="server">
    <hd:header id="Header1" runat="server"></hd:header>
    <div>
    <table id="tblTop" cellspacing="3" align="center" border="0" width="100%">
        <tr>
          
          <td align="center"><font size="+0"><b>&nbsp;User Maintenance&nbsp;</b></font></td>
          <td vAlign="middle">
            <asp:linkbutton id="hlkBackToMenu" runat="server" OnClick="hlkBackToMenu_Click"></asp:linkbutton>
          </td>
          <td vAlign="middle" align="center">Logged as&nbsp;
            <asp:label id="lblUser" runat="server" Font-Bold="True">&nbsp;</asp:label>&nbsp;&nbsp;&nbsp;
            <asp:linkbutton id="hlnkLogOut" runat="server" OnClick="hlnkLogOut_Click">Log out</asp:linkbutton>
            &nbsp;&nbsp;&nbsp;<asp:hyperlink id="hlnkChangePwd" runat="server" NavigateUrl="changepwd.aspx">Change password</asp:hyperlink>
          </td>
          
          <td valign="middle" width="20">&nbsp;</td>
          
          <td width="30">&nbsp;</td>
        </tr>
      </table>
    <table><tr bgcolor="gray"><td><div  id="navigation" style="width:100%">
		<ul nowrap> <li class="selected">
     <asp:LinkButton ID="lnk_Listview" runat="server" OnClick="lnk_Listview_Click" >List Users</asp:LinkButton></li>
     <li><asp:LinkButton ID="lnk_viewuser" runat="server" OnClick="lnk_viewuser_Click" >View Users</asp:LinkButton></li></ul></div>
      <%--<asp:LinkButton ID="lnk_company" runat="server" OnClick="lnk_company_Click"  ><img src="Images/company 0.jpg" border="0" alt="Company" /></asp:LinkButton>--%>
      
      </td>
      </tr></table>
    <asp:sqldatasource id="user_masterSqlDataSource"
                SelectCommand="select [UserID],   [Username],   [Password],   [email],   [GroupID],[name], [UserId2]=(select max(UserId) from [dbo].[user_master])   From [dbo].[user_master] where [UserID]=@UserID ORDER BY [Username] ASC"
                DeleteCommand="delete from [dbo].[user_master] where [UserID]=@UserID"
                 InsertCommand="insert into [dbo].[user_master] ([Username], [Password], [email], [GroupID],[name]) values (@Username, @Password, @email, @GroupID,@name)"
                 UpdateCommand="update [dbo].[user_master] set  [Password]=@Password, [email]=@email, [GroupID]=@GroupID, [name]=@name where  [UserID]=@UserID"
                ConnectionString="<%$ ConnectionStrings:Project1ConnectionString%>"
                ProviderName="<%$ ConnectionStrings:Project1ConnectionString.providerName%>"
                 
         runat="server">
         <SelectParameters>
        <asp:SessionParameter SessionField="user_master_id" Name="UserID" Type="string"  DefaultValue="1" />
        </SelectParameters>
        <DeleteParameters>
        <asp:SessionParameter SessionField="user_master_id" Name="UserID" Type="string"  DefaultValue="1" />
        </DeleteParameters>
        <InsertParameters>
            <asp:Parameter Name="Username" Type="String"/>
            <asp:Parameter Name="Password" Type="String"/>
            <asp:Parameter Name="email" Type="String"/>
            <asp:Parameter Name="GroupID" Type="String"/>
            <asp:Parameter Name="name" Type="String"/>
            
            
        </InsertParameters>
        <UpdateParameters>
             <asp:Parameter Name="name" Type="String"/>
            <asp:Parameter Name="Password" Type="String"/>
            <asp:Parameter Name="email" Type="String"/>
            <asp:Parameter Name="GroupID" Type="String"/>
           
            

            <asp:SessionParameter  SessionField="user_master_id" Name="UserID" Type="string" DefaultValue="1" />

        </UpdateParameters>
            </asp:sqldatasource>
        <asp:label id="lblMessage" runat="server" ForeColor="Red"></asp:label>
        <asp:FormView ID="FormView1" runat="server" DataSourceID="user_masterSqlDataSource" OnDataBound="FormView1_DataBound"  OnItemInserted="FormView1_ItemInserted" >
            <EditItemTemplate>
            <table class="shade"><tr>
                
                <td align="right" style="padding-right:5px;"><b>UserID:</b></td><td>
                <asp:Label ID="UsernameTextBox" runat="server" Text='<%# Eval("Username") %>'>
                </asp:Label></td>
                <td align="right" style="padding-right:5px;"><b>User Name:</b></td>
                <td><asp:TextBox ID="nameTextBox" runat="server" Text='<%# Bind("name") %>'>
                </asp:TextBox></td>
                </tr>
                
                <tr><td align="right" style="padding-right:5px;"><b>Password:</b></td>
                <td><asp:TextBox ID="PasswordTextBox" runat="server" Text='<%# Bind("Password") %>' >
                </asp:TextBox></td>
                <td align="right" style="padding-right:5px;"><b>Email:</b></td>
                <td><asp:TextBox ID="emailTextBox" runat="server" Text='<%# Bind("email") %>'>
                </asp:TextBox></td></tr>
                <tr><td align="right" style="padding-right:5px;"><b>GroupID:</b></td>
                <td><asp:TextBox ID="GroupIDTextBox" runat="server" Width="100px" Text='<%# Bind("GroupID") %>'>
                </asp:TextBox><a href="#" onClick="groupidlook(); return false"><asp:Image ID="groupidLookup" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
                </td>
                
                </tr>
                
               <%-- UserId2:
                <asp:TextBox ID="UserId2TextBox" runat="server" Text='<%# Bind("UserId2") %>'>
                </asp:TextBox><br />--%>
                
                <tr><td><asp:Button ID="UpdateButton" CssClass="button" runat="server" CausesValidation="True" OnClick="UpdateNew_Click" CommandName="Update"
                    Text="Save">
                </asp:Button></td>
                <td><asp:Button ID="UpdateCancelButton" CssClass="button" runat="server" CausesValidation="False" CommandName="Cancel"
                    Text="Cancel">
                </asp:Button></td></tr></table>
            </EditItemTemplate>
            <InsertItemTemplate>
                <table class="shade">
                <tr>                
                <td align="right" style="padding-right:5px;"><b>UserID:</b></td>
                <td><asp:TextBox ID="UsernameTextBox" runat="server" Text='<%# Bind("Username") %>'></asp:TextBox></td>
                 <td align="right" style="padding-right:5px;"><b>User Name:</b></td>
                <td><asp:TextBox ID="nameTextBox" runat="server" Text='<%# Bind("name") %>'>
                </asp:TextBox></td></tr>
                <tr><td align="right" style="padding-right:5px;"><b>Password:</b></td><td><asp:TextBox ID="PasswordTextBox" TextMode="Password" runat="server" Text='<%# Bind("Password") %>'></asp:TextBox></td>
                
                <td align="right" style="padding-right:5px;"><b> Email:</b></td>
                <td><asp:TextBox ID="emailTextBox" runat="server" Text='<%# Bind("email") %>'></asp:TextBox></td></tr>
                <tr><td align="right" style="padding-right:5px;"><b>GroupID:</b></td>
                <td><asp:TextBox ID="GroupIDTextBox" Width="100" runat="server" Text='<%# Bind("GroupID") %>'></asp:TextBox>
                <a href="#" onClick="groupidlook(); return false"><asp:Image ID="groupidLookup" runat="server" ImageUrl="images/lookup_icon.gif" /></a></td>
               
                </tr><tr>
                
                
                <%--<asp:TextBox ID="UserId2TextBox" runat="server" Text='<%# Bind("UserId2") %>'>
                </asp:TextBox><br />--%>
                
                <td><asp:Button ID="InsertButton" CssClass="button" runat="server" CausesValidation="True" CommandName="Insert"
                    Text="Save" OnClick = "Addnew_click"></asp:Button></td>
                <td> <asp:Button ID="InsertCancelButton" CssClass="button" runat="server" CausesValidation="False" CommandName="Cancel"
                    Text="Cancel">
                </asp:Button></td>
                </tr>
                </table>
                
                               
               
            </InsertItemTemplate>
            <ItemTemplate>
               <table class="shade"><tr>
               <td align="right" style="padding-right:5px;"><b>UserID:</b></td>
               <td><asp:Label ID="UsernameLabel" Width="150px" BackColor="paleturquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" runat="server" Text='<%# Bind("Username") %>'></asp:Label></td>
               <td align="right" style="padding-right:5px;"><b>Username:</b></td>
               <td><asp:Label ID="nameLabel" Width="150px" BackColor="paleturquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" runat="server" Text='<%# Bind("name") %>'></asp:Label></td>
               </tr>
               
                
                
                
                <tr><%--<td align="right" style="padding-right:5px;"><b>Password:</b></td>--%>
                
                <td align="right" style="padding-right:5px;"><b>Email:</b></td>
                <td><asp:Label ID="emailLabel" Width="150px" BackColor="paleturquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" runat="server" Text='<%# Bind("email") %>'></asp:Label></td>
                <td align="right" style="padding-right:5px;"><b>GroupID:</b></td>
                <td><asp:Label ID="GroupIDLabel" Width="150px" BackColor="paleturquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" runat="server" Text='<%# Bind("GroupID") %>'></asp:Label></td>
                </tr>
                  </table>
               
              <asp:Label ID="PasswordLabel"  Visible="false" Width="100px" BackColor="paleturquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" runat="server" Text='<%# Bind("Password") %>'></asp:Label>
                <asp:Label ID="UserId2Label" Visible="false" runat="server" Text='<%# Bind("UserId2") %>'></asp:Label>
               <table>
               <tr> <td><asp:Button ID="AddButton" CssClass="button" runat="server" CausesValidation="False" CommandName="new" 
                    Text="Add" OnClick="AddButton_Click"> </asp:Button></td>
                    <td><asp:Button ID="UpdateButton" CssClass="button" runat="server" CausesValidation="False" CommandName="edit"
                    Text="Update"> </asp:Button></td>
                    <%--<asp:Button ID="CopyButton" CssClass="button" runat="server" CausesValidation="False" CommandName="copy"
                    Text="Copy"> </asp:Button>--%>
                    <td><asp:Button ID="DeleteButton" CssClass="button" runat="server" CausesValidation="False" CommandName="Delete" OnClientClick="return confirm('Are you sure to delete ?')"
                    Text="Delete" OnClick="DeleteButton_Click">
                </asp:Button></td></tr> </table>
            </ItemTemplate>
        </asp:FormView>
    </div>
     <ft:footer id="Footer1" runat="server"></ft:footer>
    </form>
</body>
</html>
