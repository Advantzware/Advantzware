<%@ Page Language="c#" AutoEventWireup="true" Debug="true" Inherits="Group_user_list" Codebehind="group_user.aspx.cs" %>
<%@ Register Src="footer.ascx" TagName="Footer" TagPrefix="ft" %>
<%@ Register Src="header.ascx" TagName="Header" TagPrefix="hd" %>
<html xmlns="http://www.w3.org/1999/xhtml" >
  <head id="Head1" runat="server">
    <title>User Maintenance</title>
    <LINK href="include/style2.css" type="text/css" rel="stylesheet"/>
  </head>    
   <body>
    
    <form id="frmList" runat="server"  defaultfocus='txt_userid'>   
        <hd:header id="Header1" runat="server"></hd:header>
      <div align="left" style="width:850px">
            
      <TABLE id="tblTop" cellSpacing="3" align="center" border="0" Width="100%">
        <TR>
          <TD width=30 style="height: 21px">&nbsp;</TD>
          <TD align=center nowrap style="height: 21px"><font size=+0><b>&nbsp;User Group Maintenance&nbsp;</b></font></TD>
          <TD vAlign="middle" style="height: 21px">
            <asp:linkbutton id="hlkBackToMenu" runat="server" OnClick="hlkBackToMenu_Click"></asp:linkbutton>
          </TD>
          <TD vAlign="middle" align="center" style="height: 21px">Logged as&nbsp;
            <asp:label id="lblUser" runat="server" Font-Bold="True">&nbsp;</asp:label>&nbsp;&nbsp;&nbsp;
            <asp:linkbutton id="hlnkLogOut" runat="server" OnClick="hlnkLogOut_Click">Log out</asp:linkbutton>
            
            &nbsp;&nbsp;&nbsp;<asp:hyperlink id="hlnkChangePwd" runat="server" NavigateUrl="changepwd.aspx">Change password</asp:hyperlink>
            
          </TD>                              
          <TD vAlign="middle" width="20" style="height: 21px">&nbsp;</TD>                              
          <td width=30 style="height: 21px">&nbsp;</td>
        </TR>
      </TABLE>
      
      <table>
        <tr>
            <td nowrap> <div  id="navigation" style="width:100%">
		<ul nowrap> <li>
            <asp:LinkButton ID="lnk_listgroup" runat="server" OnClick="lnk_listgroup_Click" >List Group</asp:LinkButton></li>
      <li><asp:LinkButton ID="lnk_viewgroup" runat="server" OnClick="lnk_viewgroup_Click" >View Group</asp:LinkButton></li>
      <li class="selected"><asp:LinkButton ID="lnk_groupuser" runat="server" OnClick="lnk_groupuser_Click">User Group</asp:LinkButton></li></ul></div>
      
            </td>
        </tr>
      </table>
      
      <TABLE  id="tblMain" cellSpacing="1" cellPadding="1"  width='95%' border="0">        
        <tr>
          <td>

            <asp:GridView id="dbGrid_user_master" runat="server" CssClass="Grid" Width="100%"
                   DataKeyNames="Username" EnableViewState="False"
                  OnPageIndexChanging="dbGrid_user_master_PageIndexChanging" OnSelectedIndexChanged="dbGrid_user_master_SelectedIndexChanged"
                    OnSorting="dbGrid_user_master_Sorting"
                                                   

                  AutoGenerateColumns="False" AllowPaging="True" AllowSorting="True" BorderStyle="Dotted" EmptyDataText="No records found" DataSourceID="SqlDataSource1" >
        <SelectedRowStyle BackColor="Yellow" CssClass="GridSelected" />
        <AlternatingRowStyle CssClass="GridItemOdd" />
        <EmptyDataRowStyle BorderStyle="None" BorderWidth="0px" Font-Bold="True" HorizontalAlign="Center" VerticalAlign="Middle" />
        <RowStyle HorizontalAlign="Center" CssClass="shade" />
        <HeaderStyle  BackColor="Teal" ForeColor="White" VerticalAlign="Middle"  HorizontalAlign="Center" Wrap="False"></HeaderStyle>
        
              <Columns>
                  <asp:BoundField DataField="Username" HeaderText="User Name" ReadOnly="True" SortExpression="Username" />
                  <asp:BoundField DataField="email" HeaderText="Email" SortExpression="email" />
                  <asp:BoundField DataField="GroupID" HeaderText="GroupID" SortExpression="GroupID" />
                  <asp:BoundField DataField="name" HeaderText="Name" SortExpression="name" />

              </Columns>
            </asp:GridView>
              <asp:SqlDataSource ID="SqlDataSource1" runat="server" ConnectionString="<%$ ConnectionStrings:Project1ConnectionString %>"
                  SelectCommand="SELECT [Username], [email], [GroupID], [name] FROM [user_master] WHERE ([GroupID] = @GroupID)">
                  <SelectParameters>
                      <asp:SessionParameter Name="GroupID" SessionField="group_user" Type="String" />
                  </SelectParameters>
              </asp:SqlDataSource>
          </TD>
        </TR>
      </TABLE>      
      
    </div>
    <ft:footer id="Footer1" runat="server"></ft:footer>
    </form>
    
  </body>
</HTML>

