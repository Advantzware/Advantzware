<%@ Page Language="C#" AutoEventWireup="true" Debug="true" Inherits="banner_list" Codebehind="banner_list.aspx.cs" %>
<%@ Register Src="footer.ascx" TagName="Footer" TagPrefix="ft" %>
<%@ Register Src="header.ascx" TagName="Header" TagPrefix="hd" %>
<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">

<script runat="server">

</script>

<html xmlns="http://www.w3.org/1999/xhtml" >
<head id="Head1" runat="server">
    <title>List Banner</title>
    <LINK href="include/style.css" type="text/css" rel="stylesheet"/>
     <link href="include/tree.css" rel="stylesheet" type="text/css" />
</head>
<body>
    <form id="form1" runat="server">
     <hd:header id="Header1" runat="server"></hd:header>
    <div>
     <table id="tblTop" cellspacing="3" align="center" border="0" width="100%">
        <tr>
          
          <td align="center"><font size="+0"><b>&nbsp;Banner List&nbsp;</b></font></td>
          
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
        <asp:LinkButton ID="lnk_bannerlist" runat="server"><img src="Images/list banner 1.jpg" border="0" alt="List Contacts" /></asp:LinkButton>
        <asp:LinkButton ID="lnk_bannerview" runat="server" OnClick="lnk_bannerview_click"><img src="Images/view banner 0.jpg" border="0" alt="View Contacts" /></asp:LinkButton>
        
    </td>
    </tr>
    </table>
    
            <asp:GridView id="GridView1" runat="server" DataKeyNames="url_address" CssClass="Grid" Width="250px" OnSelectedIndexChanged="GridView1_SelectedIndexChanged"
                                   
                  AutoGenerateColumns="False" AllowPaging="True" AllowSorting="True" BorderStyle="Dotted" EmptyDataText="No records found" DataSourceID="SqlDataSource1" >
        <SelectedRowStyle CssClass="GridSelected" BackColor="Yellow" />
        <AlternatingRowStyle CssClass="GridItemOdd" />
        <EmptyDataRowStyle BorderStyle="None" BorderWidth="0px" Font-Bold="True" HorizontalAlign="Center" VerticalAlign="Middle" />
        <RowStyle CssClass="shade" />
        <HeaderStyle BackColor="Teal" VerticalAlign="Middle"  HorizontalAlign="Center" Wrap="False" ForeColor="White"></HeaderStyle>
                <Columns>
                <asp:CommandField ShowSelectButton="True" ButtonType="Image" SelectText="" SelectImageUrl="images\sel.gif" >
                    <ItemStyle Width="10px" />
                    
                </asp:CommandField>
                    <asp:BoundField DataField="url_address" HeaderText="Web Address" SortExpression="url_address" />
                    <asp:BoundField DataField="default_val" HeaderText="Default" SortExpression="default_val" />
                    <asp:CommandField ShowEditButton="true" />
                </Columns>
        </asp:GridView>
        <asp:SqlDataSource ID="SqlDataSource1" runat="server" ConnectionString="<%$ ConnectionStrings:Project1ConnectionString %>"
            SelectCommand="SELECT [url_address],[default_val] FROM [setup]"
             UpdateCommand="update setup set default_val=@default_val where url_address=@url_address">
             <UpdateParameters>
             <%--<asp:Parameter Name="default_val" Type="boolean" />--%>
           <asp:SessionParameter SessionField="banner_url_add" Name="url_address" Type="string" />
             </UpdateParameters>
             </asp:SqlDataSource>
    </div>
    <ft:footer id="Footer1" runat="server"></ft:footer>
    </form>
</body>
</html>
