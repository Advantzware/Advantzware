<%@ Page Language="c#" AutoEventWireup="true" Debug="false" Inherits="supplier_lookup" Codebehind="supplier_lookup.aspx.cs" %>

<html xmlns="http://www.w3.org/1999/xhtml" >
  <head id="Head1" runat="server">
    <title>Suppliers Lookup</title>
    
    <LINK href="include/style.css" type="text/css" rel="stylesheet"/>
     
    <link href="include/tree.css" rel="stylesheet" type="text/css" />
    <link href="include/dhtmlwindow.css" rel="stylesheet" type="text/css" />
    <script language = JavaScript>
    
      
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
    <form id="frmList" runat="server" defaultbutton='btnSearch' defaultfocus="txtSearchValue">   
        
      <div>
      
      <TABLE id="tblMain" cellSpacing="1" Width="450px" cellPadding="1" border="0">
        <TR>
          <TD style="width: 761px">
            <TABLE id="tblSearch" cellSpacing="1" Width="450px"  cellPadding="5" border="0"  bgcolor=black>
              <TR>
                <TD class="shade" align="center" width="50"><nobr>
                  <%--<asp:linkbutton id="btnAdd" runat="server" OnClick="btnAdd_Click">Add new</asp:linkbutton></nobr>--%>
                  <asp:button id="btnSearch" runat="server" Width="40px" CssClass="button" Text="Go" OnClick="btnSearch_Click"></asp:button>
                  <br />
                  <br />
                  <asp:button id="btnShowAll" runat="server" Width="40px" CssClass="button" Text="All" OnClick="btnShowAll_Click"></asp:button>
                </TD>               
                          
                <TD id="tdSearch" runat="server" class="shade" vAlign="middle" align="center">               
                <B>Search for:&nbsp; </B>&nbsp;&nbsp
                  <asp:dropdownlist Width="80px" id="ddlSearchField" runat="server">
                    <%--<asp:ListItem Value="Any Field">Any field</asp:ListItem>--%>
            <asp:ListItem Value="comp_code">Code</asp:ListItem>            <asp:ListItem Value="name">Compettive Supplier Name</asp:ListItem>
                  </asp:dropdownlist>&nbsp;&nbsp;&nbsp;
                  <asp:dropdownlist Width="80px" id="ddlSearchOperation" runat="server">
                    <%--<asp:ListItem Value="Contains">Contains</asp:ListItem>--%>
                    <asp:ListItem Value="Equals">Equals</asp:ListItem>
                    <asp:ListItem Value="Starts with ...">Begin</asp:ListItem>
                                     
                  </asp:dropdownlist>&nbsp;&nbsp;
                  <asp:textbox id="txtSearchValue" runat="server" Width="100px"></asp:textbox>&nbsp;
                  
                </TD>               
                
              </TR>
            </TABLE>
           
          </td>
        </tr>
        <tr>
          <td style="width: 400px">

            <asp:sqldatasource id="comp_suppliersSqlDataSource"
                SelectCommand="select [comp_code],   [name],[address1],[address2],[city],[state],[zip]   From [dbo].[comp_suppliers] ORDER BY [comp_code] ASC"
                DeleteCommand="delete from [dbo].[comp_suppliers] where [comp_code]=@comp_code"
                ConnectionString="<%$ ConnectionStrings:Project1ConnectionString%>"
                ProviderName="<%$ ConnectionStrings:Project1ConnectionString.providerName%>"
                OnSelected="comp_suppliersSqlDataSource_Selected" 
        OnDeleting="comp_suppliersSqlDataSource_Deleting"   runat="server">
            </asp:sqldatasource>
            <asp:GridView id="dbGrid_comp_suppliers" runat="server" CssClass="Grid" Width="450px"
                  datasourceid="comp_suppliersSqlDataSource" DataKeyNames="comp_code"
                  OnPageIndexChanged="dbGrid_comp_suppliers_PageIndexChanged" OnRowCommand="dbGrid_comp_suppliers_RowCommand" OnSorted="dbGrid_comp_suppliers_Sorted"

                  OnRowCreated="dbGrid_comp_suppliers_RowCreated"

                  OnRowDataBound="dbGrid_comp_suppliers_RowDataBound" 

                  OnRowDeleted="dbGrid_comp_suppliers_RowDeleted"                  

                  AutoGenerateColumns="False" AllowPaging="True" AllowSorting="True" BorderStyle="Dotted" EmptyDataText="No records found" OnSelectedIndexChanged="dbGrid_comp_suppliers_SelectedIndexChanged">
        <SelectedRowStyle BackColor="Yellow" CssClass="GridSelected" />
        <AlternatingRowStyle CssClass="GridItemOdd" />
        <EmptyDataRowStyle BorderStyle="None" BorderWidth="0px" Font-Bold="True" HorizontalAlign="Center" VerticalAlign="Middle" />
        <RowStyle CssClass="shade" />
        <HeaderStyle BackColor="Teal" ForeColor="White" VerticalAlign="Middle"  HorizontalAlign="Center" Wrap="False"></HeaderStyle>
        
              <Columns>

                <asp:TemplateField Visible="False" HeaderImageUrl="images\icon_delete.gif">
              <ItemStyle HorizontalAlign="Center" />
                  <ItemTemplate>          
          <input type="checkbox" name="chDelete" value='<%#DataBinder.Eval(Container,"RowIndex")%>'/>
                  </ItemTemplate>
                </asp:TemplateField>
                  
                  <asp:TemplateField>
                  <ItemStyle HorizontalAlign=Center />
                   <ItemTemplate>       
		    <a href="#" onClick="javascript:top.opener.window.SupplierLookup('<%#DataBinder.Eval(Container,"DataItem.comp_code")%>','<%#DataBinder.Eval(Container,"DataItem.name")%>');window.close();">Select</a>             	    
                   </ItemTemplate>
                </asp:TemplateField>

              <asp:ButtonField Text="Edit" Visible="False" CommandName="cmdEdit" HeaderImageUrl="images\icon_edit.gif"></asp:ButtonField>

                <asp:BoundField DataField="comp_code" ReadOnly="True" HeaderText="Code" SortExpression="comp_code">
                    <ItemStyle HorizontalAlign="Center" />
                  
                </asp:BoundField>

                <asp:BoundField DataField="name" ReadOnly="True" HeaderText="Competitive Supplier Name" SortExpression="name">
                    <ItemStyle HorizontalAlign="Center" />
                  
                </asp:BoundField>
                   


              </Columns>
            </asp:GridView>
          </TD>
        </TR>
      </TABLE>      
      <input type="button" name="close" class="buttonM" id="close" value="Close" onclick="javascript:window.close()" />
      
    </div>
    
    </form>
  </body>
</HTML>

