<%@ Page Language="C#" AutoEventWireup="true" Debug="true" ValidateRequest="false" Inherits="banner_view" Codebehind="banner_view.aspx.cs" %>
<%@ Register Src="footer.ascx" TagName="Footer" TagPrefix="ft" %>
<%@ Register Src="header.ascx" TagName="Header" TagPrefix="hd" %>
<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">

<script runat="server">

</script>

<html xmlns="http://www.w3.org/1999/xhtml" >
<head id="Head1" runat="server">
    <title>View Banner</title>
    <LINK href="include/style.css" type="text/css" rel="stylesheet"/>
     <link href="include/tree.css" rel="stylesheet" type="text/css" />
</head>
<body>
    <form id="form1" runat="server">
     <hd:header id="Header1" runat="server"></hd:header>
    <div>
    <table id="tblTop" cellspacing="3" align="center" border="0" width="100%">
        <tr>
          
          <td align="center"><font size="+0"><b>&nbsp;Banner View&nbsp;</b></font></td>
          
          <td valign="middle" align="center"><b>Users</b>&nbsp;&nbsp;Logged as&nbsp;
            <asp:label id="lblUser" runat="server" Font-Bold="True">&nbsp;</asp:label>&nbsp;&nbsp;&nbsp;
            <asp:linkbutton id="hlnkLogOut" runat="server" OnClick="hlnkLogOut_Click">Log out</asp:linkbutton>
            &nbsp;&nbsp;&nbsp;<asp:hyperlink id="hlnkChangePwd" runat="server" NavigateUrl="changepwd.aspx">Change password</asp:hyperlink>
          </td>
          
          <td valign="middle" width="20">&nbsp;</td>
          
          <td width="30">&nbsp;</td>
        </tr>
      </table>
      
       <table>
    <tr bgcolor="gray">
    <td>
        <asp:LinkButton ID="lnk_bannerlist" runat="server" OnClick="lnk_bannerlist_click"><img src="Images/list banner 0.jpg" border="0" alt="List Contacts" /></asp:LinkButton>
        <asp:LinkButton ID="lnk_bannerview" runat="server"><img src="Images/view banner 1.jpg" border="0" alt="View Contacts" /></asp:LinkButton>
        
    </td>
    </tr>
    </table><asp:HiddenField ID="HiddenField1" runat="server" />
    <asp:Button ID="AddNewButton" runat="server" CssClass="buttonM" OnClick="AddNewButton_Click" Text="Add" />
        <asp:FormView ID="FormView1" runat="server" DataSourceID="SqlDataSource1" >
            <EditItemTemplate>
            <table class="shade">
            <tr>
            <td align="right" style="padding-right:5px;"><b>Web Address:</b></td>
            <td><b><asp:TextBox ID="url_addressTextBox" Width="200px" runat="server" Text='<%# Bind("url_address") %>'>
                </asp:TextBox></b>&nbsp &nbsp <b>Default:</b>
                    <asp:CheckBox ID="DefaultCheckBox" runat="server" Checked='<%# Bind("default_val") %>' /></td>
                </tr>
                <tr>
            <td valign="top" align="right" style="padding-right:5px;"><b>Banner:</b></td>
            <td><b><asp:TextBox ID="bannerTextBox" TextMode="MultiLine" Height="300px" Width="400px" runat="server" Text='<%# Bind("banner") %>'>
                </asp:TextBox></b></td>
            </tr>
            </table>
               
                <asp:Button ID="UpdateButton" CssClass="buttonM" runat="server" CausesValidation="True"
                    Text="Save" OnClick="UpdateButton_Click">
                </asp:Button>
                <asp:Button ID="UpdateCancelButton" CssClass="buttonM" runat="server" CausesValidation="False"
                    Text="Cancel" >
                </asp:Button>
            </EditItemTemplate>
            <InsertItemTemplate>
            <table class="shade">
            <tr>
            <td align="right" style="padding-right:5px;"><b>Web Address:</b></td>
            <td><b><asp:TextBox ID="url_addressTextBox" Width="200px" runat="server" Text='<%# Bind("url_address") %>'>
                </asp:TextBox></b>&nbsp &nbsp <b>Default:</b>
                    <asp:CheckBox ID="DefaultCheckBox" runat="server" Checked ='<%# Bind("default_val") %>' /></td>
                </tr>
                <tr>
            <td valign="top" align="right" style="padding-right:5px;"><b>Banner:</b></td>
            <td><b> <asp:TextBox ID="bannerTextBox" TextMode="multiline" Height="300px" Width="400px" runat="server" Text='<%# Bind("banner") %>'>
                </asp:TextBox></b></td>
            </tr>
            </table>
               
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
            <td width="80px" align="right" style="padding-right:5px;"><b>Web Address:</b></td>
            <td><b><asp:Label ID="url_addressLabel" BackColor="PaleTurquoise" Width="200px" BorderColor="White" BorderStyle="Solid" BorderWidth="1px" runat="server" Text='<%# Bind("url_address") %>'>
                </asp:Label></b> &nbsp &nbsp  <b>Default:</b>
                    <asp:CheckBox ID="DefaultCheckBox" BackColor="PaleTurquoise"  BorderColor="White" BorderStyle="Solid" BorderWidth="1px" runat="server" Checked='<%# Bind("default_val") %>' Enabled="false"  /></td>
                </tr>
                <tr>
            <td valign="top" align="right" style="padding-right:5px;"><b>Banner:</b></td>
            <td><b><asp:Label ID="bannerLabel" runat="server" Text='<%# Eval("banner") %>'></asp:Label></b></td>
            </tr>
            </table>
               <asp:Button ID="AddButton" runat="server" Text="Add" CssClass="buttonM" CommandName="New" />
               <asp:Button ID="UpdateButton" runat="server" Text="Update" CssClass="buttonM" CommandName="Edit" />
               <asp:Button ID="DeleteButton" runat="server" Text="Delete" CssClass="buttonM" OnClientClick="return confirm('Are you sure you want to delete this record')" OnClick="DeleteButton_Click" />
            </ItemTemplate>
        </asp:FormView>
        <asp:SqlDataSource ID="SqlDataSource1" runat="server" ConnectionString="<%$ ConnectionStrings:Project1ConnectionString %>"
            SelectCommand="SELECT [url_address], [banner],[default_val] FROM [setup] WHERE ([url_address] = @url_address)">
            <SelectParameters>
                <asp:SessionParameter Name="url_address" SessionField="banner_url_add" Type="String" />
            </SelectParameters>
        </asp:SqlDataSource>
    
    </div>
    <ft:footer id="Footer1" runat="server"></ft:footer>
    </form>
</body>
</html>
