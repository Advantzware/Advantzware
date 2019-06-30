<%@ Page Language="c#" AutoEventWireup="true" Inherits="Cmain_menu_print" Codebehind="main_menu_print.aspx.cs" %>
<html xmlns="http://www.w3.org/1999/xhtml" >
  <head id="Head1" runat="server">
    <title>Menu Maintenance - Printer-friendly version</title>
    <LINK href="include/style.css" type="text/css" rel="stylesheet"/>
  </head>    
  <body>
    <form id="frmList" runat="server">
      <div> 
    
    <table cellSpacing="1" cellPadding="1" width="95%" border="0" align="center">
      <tr>
        <td>
            <asp:label id="lblMessage" runat="server" ForeColor="Red"></asp:label>
      <font size=+0><b>Table:&nbsp;Menu Maintenance&nbsp;</b></font>
            <asp:sqldatasource id="main_menuSqlDataSource"
                SelectCommand="select [group_id],   [parent],   [menu_label],   [destination],   [security],   [description],   [menu_id],   [order_num],   [allowedidtypes],   [long_desc],   [target_page]   From [dbo].[main_menu] ORDER BY [order_num] ASC"
                ConnectionString="<%$ ConnectionStrings:Project1ConnectionString%>"
                ProviderName="<%$ ConnectionStrings:Project1ConnectionString.providerName%>"
                runat="server">
            </asp:sqldatasource>
            <asp:GridView id="dbGrid_main_menu" runat="server" CssClass="Grid" Width="100%"
                   datasourceid="main_menuSqlDataSource" DataKeyNames="menu_id"
                   AllowPaging=true AllowSorting=true
                  
                  OnRowDataBound="dbGrid_main_menu_RowDataBound" 

                  AutoGenerateColumns="False" BorderStyle="Dotted">
              <SelectedRowStyle CssClass="GridSelected"></SelectedRowStyle>
              <AlternatingRowStyle CssClass="GridItemOdd"></AlternatingRowStyle>
              <RowStyle CssClass="shade"></RowStyle>
              <HeaderStyle CssClass="blackshade" VerticalAlign="Middle"  HorizontalAlign="Center" Wrap="False"></HeaderStyle>
              
              <Columns> 

                <asp:BoundField DataField="description" ReadOnly="True" HeaderText="Description">
                  
                </asp:BoundField>

                <asp:BoundField DataField="menu_id" ReadOnly="True" HeaderText="Menu ID">
                  
                </asp:BoundField>

                <asp:BoundField DataField="group_id" ReadOnly="True" HeaderText="Group ID">
                  
                </asp:BoundField>

                <asp:BoundField DataField="menu_label" ReadOnly="True" HeaderText="Menu Label">
                  
                </asp:BoundField>

                <asp:BoundField DataField="long_desc" ReadOnly="True" HeaderText="Long Desc">
                  
                </asp:BoundField>

                <asp:BoundField DataField="order_num" ReadOnly="True" HeaderText="Order#">
                  
                </asp:BoundField>

                <asp:BoundField DataField="allowedidtypes" ReadOnly="True" HeaderText="Allowed ID Types">
                  
                </asp:BoundField>

                <asp:BoundField DataField="target_page" ReadOnly="True" HeaderText="Target Page">
                  
                </asp:BoundField>

                <asp:BoundField DataField="parent" ReadOnly="True" HeaderText="Parent">
                  
                </asp:BoundField>

                <asp:BoundField DataField="destination" ReadOnly="True" HeaderText="Destination">
                  
                </asp:BoundField>

                <asp:BoundField DataField="security" ReadOnly="True" HeaderText="Security Group">
                  
                </asp:BoundField>

              </Columns>
              <PagerSettings Visible="False" />
            </asp:GridView>
        </td>
      </tr>
    </table>      
      
    </div>
    </form>
  </body>
</HTML>