<%@ Page Language="c#" AutoEventWireup="false" Inherits="Cmain_menu_list" Codebehind="main_menu_list.aspx.cs" %>
<%@ Register Src="footer.ascx" TagName="Footer" TagPrefix="ft" %>
<%@ Register Src="header.ascx" TagName="Header" TagPrefix="hd" %>
<html xmlns="http://www.w3.org/1999/xhtml" >
  <head id="Head1" runat="server">
    <title>Menu Maintenance</title>
    <LINK href="include/style.css" type="text/css" rel="stylesheet"/>
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
    <hd:Header ID="Header1" runat="server" />
    <form id="frmList" runat="server" defaultbutton='btnSearch' defaultfocus='txtSearchValue'>   

      <div>
            
      <TABLE id="tblTop" cellSpacing="3" align="center" border="0" Width="100%">
        <TR>
          <TD width=30>&nbsp;</TD>
          <TD align=center nowrap><font size=+0><b>Table:&nbsp;Menu Maintenance&nbsp;</b></font></TD>
          <TD vAlign="middle">
            <asp:linkbutton id="hlkBackToMenu" runat="server" OnClick="hlkBackToMenu_Click">Back to menu</asp:linkbutton>
          </TD>
          <TD vAlign="middle" align="center">Logged as&nbsp;
            <asp:label id="lblUser" runat="server" Font-Bold="True">&nbsp;</asp:label>&nbsp;&nbsp;&nbsp;
            <asp:linkbutton id="hlnkLogOut" runat="server" OnClick="hlnkLogOut_Click">Log out</asp:linkbutton>
            
            &nbsp;&nbsp;&nbsp;<asp:hyperlink id="hlnkChangePwd" runat="server" NavigateUrl="changepwd.aspx">Change password</asp:hyperlink>
            
          </TD>
          
                    
          <TD vAlign="middle" width="20">&nbsp;</TD>
          <TD vAlign="middle" align="center">
            <asp:hyperlink id="hlnkAdvSearch" runat="server" NavigateUrl="main_menu_Search.aspx">Advanced search</asp:hyperlink>
          </TD>
          <TD vAlign="middle" align="center">&nbsp;
            <asp:hyperlink id="hlnkExport" runat="server" NavigateUrl="main_menu_export.aspx" target=_blank>Export results</asp:hyperlink>
          </TD>
          <TD vAlign="middle" align="right">
            <asp:hyperlink id="hlnkPrintImg" runat="server" NavigateUrl="main_menu_print.aspx" target=_blank>
              <img src="images/printer.gif" border="0">
            </asp:hyperlink>            
          </TD>
          <TD vAlign="middle" align="left">&nbsp;
            <asp:hyperlink id="hlnkPrint" runat="server" NavigateUrl="main_menu_print.aspx"  target=_blank>Printer-friendly version</asp:hyperlink>
          </TD>
          <td width=30>&nbsp;</td>
        </TR>
      </TABLE>
      <TABLE id="tblMain" cellSpacing="1" cellPadding="1" align='center' width='95%' border="0">
        <TR>
          <TD>
            <TABLE id="tblSearch" cellSpacing="1" cellPadding="5" width="100%" border="0"  bgcolor=black>
              <TR>
                <TD class="shade" align="center" width="50"><nobr>
                  <asp:linkbutton id="btnAdd" runat="server" OnClick="btnAdd_Click">Add new</asp:linkbutton></nobr>
                </TD>               
                          
                <TD id="tdSearch" runat="server" class="shade" vAlign="middle" align="center" width="800">&nbsp;                
                <B>Search for:&nbsp; </B>&nbsp;&nbsp;&nbsp;
                  <asp:dropdownlist id="ddlSearchField" runat="server">
                    <asp:ListItem Value="Any Field">Any field</asp:ListItem>
            <asp:ListItem Value="menu_id">Menu ID</asp:ListItem>            <asp:ListItem Value="menu_label">Menu Label</asp:ListItem>            <asp:ListItem Value="target_page">Target Page</asp:ListItem>            <asp:ListItem Value="parent">Parent</asp:ListItem>            <asp:ListItem Value="security">Security Group</asp:ListItem>            <asp:ListItem Value="description">Description</asp:ListItem>
                  </asp:dropdownlist>&nbsp;&nbsp;
                  <asp:dropdownlist id="ddlSearchOperation" runat="server">
                    <asp:ListItem Value="Contains">Contains</asp:ListItem>
                    <asp:ListItem Value="Equals">Equals</asp:ListItem>
                    <asp:ListItem Value="Starts with ...">Starts with ...</asp:ListItem>
                    <asp:ListItem Value="More than ...">More than ...</asp:ListItem>
                    <asp:ListItem Value="Less than ...">Less than ...</asp:ListItem>
                    <asp:ListItem Value="Equal or more than ...">Equal or more than ...</asp:ListItem>
                    <asp:ListItem Value="Equal or less than ...">Equal or less than ...</asp:ListItem>
                    <asp:ListItem Value="IsNull">Empty</asp:ListItem>                   
                  </asp:dropdownlist>
                  <asp:textbox id="txtSearchValue" runat="server" Width="136px"></asp:textbox>
                  <asp:button id="btnSearch" runat="server" CssClass="button" Text="Search" OnClick="btnSearch_Click"></asp:button>
                  <asp:button id="btnShowAll" runat="server" CssClass="button" Text="Show all" OnClick="btnShowAll_Click"></asp:button>&nbsp;&nbsp;&nbsp;
                </TD>               
                
                <TD id="tdInfo" runat="server" class="shade" align="center" width="100">
                  <asp:label id="lblCount" runat="server" Height="3px">Details found:&nbsp;0</asp:label><BR>
                  <asp:label id="lblPage" runat="server">Page&nbsp;<%=

                 (dbGrid_main_menu.PageCount ==0)?0:dbGrid_main_menu.PageIndex + 1
                  %>&nbsp;of&nbsp;<%=dbGrid_main_menu.PageCount%></asp:label>
                </TD>
                <TD id="tdPageCount" runat="server" class="shade" align="left">
          <table><tr><td align="center">
            Records Per Page::<BR>
            <asp:dropdownlist id="ddlPagerCount" runat="server" AutoPostBack="True" OnSelectedIndexChanged="ddlPagerCount_SelectedIndexChanged">
              <asp:ListItem Value="10">10</asp:ListItem>
              <asp:ListItem Value="20">20</asp:ListItem>
              <asp:ListItem Value="30">30</asp:ListItem>
              <asp:ListItem Value="50">50</asp:ListItem>
              <asp:ListItem Value="100">100</asp:ListItem>
              <asp:ListItem Value="500">500</asp:ListItem>
            </asp:dropdownlist>
          </td></tr></table>  
                </TD>
              </TR>
            </TABLE>
            <asp:label id="lblMessage" runat="server" ForeColor="Red"></asp:label>
          </td>
        </tr>
        <tr>
          <td>

      <p>
      <a href=# onClick = "ChSel()">Select/Unselect all</a>
      <asp:linkbutton id="btnDelete" runat="server" OnClick="btnDelete_Click">Delete selected</asp:linkbutton>
      <asp:linkbutton id="btnTopMenu" onClick="btnTopMenu_Click"  runat="server">Top Level Menu</asp:linkbutton>
      </p>

            <asp:sqldatasource id="main_menuSqlDataSource"
                SelectCommand="select [group_id],   [parent],   [menu_label],   [destination],   [security],   [description],   [menu_id],   [order_num],   [allowedidtypes],   [long_desc],   [target_page]   From [dbo].[main_menu] ORDER BY [order_num] ASC"
                DeleteCommand="delete from [dbo].[main_menu] where [menu_id]=@menu_id"
                ConnectionString="<%$ ConnectionStrings:Project1ConnectionString%>"
                ProviderName="<%$ ConnectionStrings:Project1ConnectionString.providerName%>"
                OnSelected="main_menuSqlDataSource_Selected" 
        OnDeleting="main_menuSqlDataSource_Deleting"   runat="server">
            </asp:sqldatasource>
            <asp:GridView id="dbGrid_main_menu" runat="server" CssClass="Grid" Width="100%"
                  datasourceid="main_menuSqlDataSource" DataKeyNames="menu_id"
                  OnPageIndexChanged="dbGrid_main_menu_PageIndexChanged" OnRowCommand="dbGrid_main_menu_RowCommand" OnSorted="dbGrid_main_menu_Sorted"

                  OnRowCreated="dbGrid_main_menu_RowCreated"

                  OnRowDataBound="dbGrid_main_menu_RowDataBound" 

                  OnRowDeleted="dbGrid_main_menu_RowDeleted"                  

                  AutoGenerateColumns="False" AllowPaging="True" AllowSorting="True" BorderStyle="Dotted" EmptyDataText="No records found">
        <SelectedRowStyle CssClass="GridSelected" />
        <AlternatingRowStyle CssClass="GridItemOdd" />
        <EmptyDataRowStyle BorderStyle="None" BorderWidth="0px" Font-Bold="True" HorizontalAlign="Center" VerticalAlign="Middle" />
        <RowStyle CssClass="shade" />
        <HeaderStyle CssClass="blackshade" VerticalAlign="Middle"  HorizontalAlign="Center" Wrap="False"></HeaderStyle>
        
              <Columns>

                <asp:TemplateField HeaderImageUrl="images\icon_delete.gif">
              <ItemStyle HorizontalAlign=Center />
                  <ItemTemplate>          
          <input type="checkbox" name="chDelete" value='<%#DataBinder.Eval(Container,"RowIndex")%>'/>
                  </ItemTemplate>
                </asp:TemplateField>        

              <asp:ButtonField Text="Edit" CommandName="cmdEdit" HeaderImageUrl="images\icon_edit.gif"></asp:ButtonField>

	      <asp:TemplateField HeaderText="Submenu">                  
	    <ItemTemplate>
                    <asp:LinkButton id="SubmenuButton" runat="server" Text='<%#  GetText(Convert.ToString(DataBinder.Eval(Container, "DataItem.target_page"))) %>' CommandName='<%#  GetText(Convert.ToString(DataBinder.Eval(Container, "DataItem.target_page"))) %>' CausesValidation="false" CommandArgument='<%#  Convert.ToString(DataBinder.Eval(Container, "DataItem.menu_id")) %>'>  </asp:LinkButton>                
                  </ItemTemplate>              
                </asp:TemplateField>

	      

                <asp:BoundField DataField="menu_id" ReadOnly="True" HeaderText="Menu ID" SortExpression="menu_id">
                  
                </asp:BoundField>

                <asp:BoundField DataField="menu_label" ReadOnly="True" HeaderText="Menu Label" SortExpression="menu_label">
                  
                </asp:BoundField>

                <asp:BoundField DataField="order_num" ReadOnly="True" HeaderText="Order#" SortExpression="order_num">
                  
                </asp:BoundField>

                <asp:BoundField DataField="target_page" ReadOnly="True" HeaderText="Target Page" SortExpression="target_page">
                  
                </asp:BoundField>

                <asp:BoundField DataField="parent" ReadOnly="True" HeaderText="Parent" SortExpression="parent">
                  
                </asp:BoundField>

                <asp:BoundField DataField="security" ReadOnly="True" HeaderText="Security Group" SortExpression="security">
                  
                </asp:BoundField>

              </Columns>
            </asp:GridView>
          </TD>
        </TR>
      </TABLE>      
      
    </div>
    
    </form>
    <ft:Footer ID="Footer1" runat="server" />
  </body>
</HTML>

