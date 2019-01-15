<%@ Page Language="C#" AutoEventWireup="true" Inherits="list_notes" Codebehind="list_notes.aspx.cs" %>
<%@ Register Src="footer.ascx" TagName="Footer" TagPrefix="ft" %>
<%@ Register Src="header.ascx" TagName="Header" TagPrefix="hd" %>
<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">

<script runat="server">

</script>

<html xmlns="http://www.w3.org/1999/xhtml" >
<head id="Head1" runat="server">
    <title>List Notes</title>
    <LINK href="include/style.css" type="text/css" rel="stylesheet"/>
     
    <link href="include/tree.css" rel="stylesheet" type="text/css" />
    <link href="include/dhtmlwindow.css" rel="stylesheet" type="text/css" />
</head>
<body>
    <form id="form1" runat="server">
    <hd:header id="Header1" runat="server"></hd:header>
    <div>
    <table id="tblTop" cellspacing="3" border="0">
        <tr>
          
          <td align="center"><font size="+0"><b>&nbsp;List Notes&nbsp;</b></font></td>
          <td><asp:linkbutton id="LinkButton1" runat="server" OnClick="LinkButton1_Click">Back to menu</asp:linkbutton></td>
          <td valign="middle" align="center"><b>Users</b>&nbsp;&nbsp;Logged as&nbsp;
            <asp:label id="lblUser" runat="server" Font-Bold="True">&nbsp;</asp:label>&nbsp;&nbsp;&nbsp;
            <asp:linkbutton id="hlnkLogOut" runat="server" OnClick="hlnkLogOut_Click">Log out</asp:linkbutton>
            &nbsp;&nbsp;&nbsp;<asp:hyperlink id="hlnkChangePwd" runat="server" NavigateUrl="changepwd.aspx"></asp:hyperlink>
            &nbsp;<b>Company:</b> &nbsp;<asp:label id="lblComp" runat="server" Font-Bold="True">&nbsp;</asp:label>

          </td>
          
          <td valign="middle" width="20">&nbsp;</td>
          
          <td width="30">&nbsp;</td>
        </tr>
      </table>
      
      <table>
    <tr bgcolor="gray">
    <td><div  id="navigation" style="width:100%">
		<ul nowrap> <li id="lilistcnd" runat="server">
          <asp:LinkButton ID="lnk_listcontact" runat="server" OnClick="lnk_listcontacts_click">List Contacts </asp:LinkButton></li>
        <li id="liviewcnd" runat="server"><asp:LinkButton ID="lnk_viewcontact" runat="server" OnClick="lnk_viewcontacts_click">View Contacts</asp:LinkButton></li>
        <li id="lilistsp" runat="server"><asp:LinkButton ID="lnk_listsupplier" runat="server" OnClick="lnk_listsupplier_Click" >List Supplier</asp:LinkButton></li>
        <li id="liviewsp" runat="server"><asp:LinkButton ID="lnk_viewsupplier" runat="server" OnClick="lnk_viewsupplier_Click">View Supplier</asp:LinkButton></li>
               
        <li class="selected"><asp:LinkButton ID="lnk_brwsnotes" runat="server">List Notes</asp:LinkButton></li>
        <li><asp:LinkButton ID="lnk_viewnotes" runat="server" OnClick="lnk_viewsnotes_click">View Notes</asp:LinkButton></li>
        
        <li id="limail" runat="server"><asp:LinkButton ID="lnk_MailList" runat="server" OnClick="lnk_MailList_click">Mail Label</asp:LinkButton></li>
        <li id="licalender" runat="server"><asp:LinkButton ID="lnk_calendar" runat="server" OnClick="lnk_calendar_click">Calendar</asp:LinkButton></li></ul></div>
    </td>
    </tr>
    </table>
   <%-- <a href="contact_list.aspx">Contact List</a>--%>
   
   <%--<asp:LinkButton ID="comp_supplier" runat="server" OnClick="comp_supplier_click">List Supplier</asp:LinkButton>
    <asp:LinkButton ID="contact_list" runat="server" OnClick="contact_list_click">Contact List</asp:LinkButton>
    --%>
    
    
    <asp:FormView ID="FormView1" Width="400px" runat="server" DataSourceID="SqlDataSource2" CellPadding="4" ForeColor="#333333">
            
            <ItemTemplate>
                <b>First Name:</b>
               <b> <asp:Label ID="first_nameLabel" BackColor="Turquoise" Width="120px" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" runat="server" Text='<%# Bind("first_name") %>'>
                </asp:Label></b>
                &nbsp;&nbsp;
                <b>Last Name:</b>
                <b><asp:Label ID="last_nameLabel" runat="server" BackColor="Turquoise" Width="120px" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" Text='<%# Bind("last_name") %>'></asp:Label></b>
            </ItemTemplate>
            <FooterStyle BackColor="#507CD1" Font-Bold="True" ForeColor="White" />
            <RowStyle BackColor="#EFF3FB" />
            <PagerStyle BackColor="#2461BF" ForeColor="White" HorizontalAlign="Center" />
            <HeaderStyle BackColor="#507CD1" Font-Bold="True" ForeColor="White" />
            <EditRowStyle BackColor="#2461BF" />
        </asp:FormView>
        <asp:SqlDataSource ID="SqlDataSource2" runat="server" ConnectionString="<%$ ConnectionStrings:Project1ConnectionString %>"
            SelectCommand="SELECT [first_name], [last_name] FROM [contact] WHERE (([first_name] = @first_name) AND ([last_name] = @last_name))">
            <SelectParameters>
                <asp:SessionParameter Name="first_name" SessionField="contact_list_first_name" Type="String" />
                <asp:SessionParameter Name="last_name" SessionField="contact_list_last_name" Type="String" />
            </SelectParameters>
        </asp:SqlDataSource>
    
    
    <asp:SqlDataSource id="SqlDataSource1" runat="server"
      SelectCommand="select [rec_key], [note_date],   [note_time],   [user_id],   [viewed],   [note_title],   [note_text]  from   [dbo].[notes] where [rec_key]=@rec_key"
       
        ConnectionString="<%$ ConnectionStrings:Project1ConnectionString %>"
        ProviderName="<%$ ConnectionStrings:Project1ConnectionString.providerName %>" 
    >
        <SelectParameters>
        <asp:SessionParameter Name="rec_key" SessionField="contact_rec_key"
                        Type="String" />
                    
        </SelectParameters>
        
        </asp:SqlDataSource>
        
        <asp:GridView ID="GridView1" CssClass="Grid" Width="400px" runat="server" AutoGenerateColumns="False" AllowPaging="True" AllowSorting="True" BorderStyle="Dotted" EmptyDataText="No records found" DataSourceID="SqlDataSource1" OnSelectedIndexChanged="GridView1_SelectedIndexChanged">
          <SelectedRowStyle CssClass="GridSelected" BackColor="Yellow" />
        <AlternatingRowStyle CssClass="GridItemOdd" />
        <EmptyDataRowStyle BorderStyle="None" BorderWidth="0px" Font-Bold="True"  />
        <RowStyle CssClass="shade" />
        <HeaderStyle  VerticalAlign="Middle"  HorizontalAlign="Center" Wrap="False" ForeColor="White" CssClass="headcolor"></HeaderStyle>
          
            <Columns>
            <asp:CommandField ShowSelectButton="True" ButtonType="Image" SelectText="" SelectImageUrl="images\sel.gif" >
                    <ItemStyle Width="10px" />
                </asp:CommandField>
                <asp:TemplateField HeaderText="Note Date" SortExpression="note_date">
                    <EditItemTemplate>
                        <asp:TextBox ID="TextBox1" runat="server" Text='<%# Bind("note_date") %>'></asp:TextBox>
                    </EditItemTemplate>
                    <ItemTemplate>
                        <asp:Label ID="Label1" runat="server" Text='<%# Bind("note_date","{0:MM/dd/yyyy}") %>'></asp:Label>
                    </ItemTemplate>
                    <ItemStyle  />
                </asp:TemplateField>
                <asp:BoundField DataField="note_time" HeaderText="Note Time" ItemStyle-HorizontalAlign="center" SortExpression="note_time" />
                <asp:BoundField DataField="note_title" HeaderText="Note Title" ItemStyle-HorizontalAlign="center" SortExpression="note_title" />
                <asp:BoundField DataField="user_id" HeaderText="UserId" ItemStyle-HorizontalAlign="center" SortExpression="user_id" />
                
            </Columns>
        </asp:GridView>
    </div>
     <ft:footer id="Footer1" runat="server"></ft:footer>
    </form>
    
</body>
</html>
