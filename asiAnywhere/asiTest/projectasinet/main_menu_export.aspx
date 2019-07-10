<%@ Page Language="c#" AutoEventWireup="true" Inherits="Cmain_menu_export" Codebehind="main_menu_export.aspx.cs" %>
<html xmlns="http://www.w3.org/1999/xhtml" >
  <head id="Head1" runat="server">
    <title>Menu Maintenance - Export</title>
    <LINK href="include/style.css" type="text/css" rel="stylesheet"/>
  </head>    
  <body>
    <form id="frmList" runat="server">
  <table cellSpacing="1" cellPadding="3" border="1">
                      <TR class="blackshade">
                        <TD align="center">Data range</TD>
                        <TD align="center">Output format</TD>
                      </TR>
                      <TR vAlign="top">
                        <TD width="200" bgColor="white">
                          <asp:RadioButton id="rbAll" runat="server" Text="All records" GroupName="rec" Checked="True"></asp:RadioButton><BR>
                          <asp:RadioButton id="rbCurr" runat="server" Text="Current page only" GroupName="rec"></asp:RadioButton></TD>
                        <TD bgColor="white">
                          <asp:RadioButton id="rbExel" runat="server" GroupName="type" Checked="True"></asp:RadioButton>&nbsp;<IMG src="images/excel.gif">
                          Excel<BR>
                          <asp:RadioButton id="rbWord" runat="server" GroupName="type"></asp:RadioButton>&nbsp;<IMG src="images/word.gif">
                          Word<BR>
                          <asp:RadioButton id="rbCSV" runat="server" GroupName="type"></asp:RadioButton>&nbsp;<IMG src="images/csv.gif">
                          CSV (comma separated values)<BR>
                          <asp:RadioButton id="rbXML" runat="server" GroupName="type"></asp:RadioButton>&nbsp;<IMG src="images/xml.gif">
                          XML<BR>
                          <BR>
                        </TD>
                      </TR>
                      <TR vAlign="middle" height="40">
                        <TD align="center" bgColor="white" colSpan="2">&nbsp;
                          <asp:Button id="btnExport" runat="server" Text="Export" OnClick="btnExport_Click"></asp:Button></TD>
                      </TR>
      </table>
            <asp:label id="lblMessage" runat="server" ForeColor="Red"></asp:label>         
            <asp:sqldatasource id="main_menuSqlDataSource"
                SelectCommand="select [group_id],   [parent],   [menu_label],   [destination],   [security],   [description],   [menu_id],   [order_num],   [allowedidtypes],   [long_desc],   [target_page]   From [dbo].[main_menu] ORDER BY [order_num] ASC"
                ConnectionString="<%$ ConnectionStrings:Project1ConnectionString%>"
                ProviderName="<%$ ConnectionStrings:Project1ConnectionString.providerName%>"
                runat="server">
            </asp:sqldatasource>
            <asp:GridView id="dbGrid_main_menu" runat="server" CssClass="Grid" Width="100%"
                   datasourceid="main_menuSqlDataSource" DataKeyNames="menu_id"
                   AllowPaging=true AllowSorting=true Visible=false                  
                
                  OnRowDataBound="dbGrid_main_menu_RowDataBound" 

                  AutoGenerateColumns="False" BorderStyle="Dotted">
              <SelectedRowStyle CssClass="GridSelected"></SelectedRowStyle>
              <AlternatingRowStyle CssClass="GridItemOdd"></AlternatingRowStyle>
              <RowStyle CssClass="GridItem"></RowStyle>
              <HeaderStyle CssClass="GridHead" VerticalAlign="Middle"  HorizontalAlign="Center" Wrap="False"></HeaderStyle>
              
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
      
    </form>
  </body>
</HTML>