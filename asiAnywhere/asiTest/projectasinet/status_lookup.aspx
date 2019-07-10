<%@ Page Language="c#" AutoEventWireup="true" Debug="false" Inherits="status_lookup" Codebehind="status_lookup.aspx.cs" %>

<html xmlns="http://www.w3.org/1999/xhtml" >
  <head id="Head1" runat="server">
    <title>Industry Sic Lookup</title>
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
    <form id="frmList" runat="server" defaultbutton='btnSearch' defaultfocus="txtSearchValue" >   
        
      <div>
            
      
      <TABLE id="tblMain" cellSpacing="1" width="450px" cellPadding="1" border="0">
        <TR>
          <TD>
            <TABLE id="tblSearch" cellSpacing="1" width="450px" cellPadding="5" border="0"  bgcolor=black>
              <TR>
                <TD class="shade" align="center" width="45"><nobr>
                  <%--<asp:linkbutton id="btnAdd" runat="server" OnClick="btnAdd_Click">Add new</asp:linkbutton></nobr>--%>
                  <asp:button id="btnSearch" runat="server" CssClass="button" Width="40px" Text="Go" OnClick="btnSearch_Click"></asp:button>
                  <br />
                  <br />
                  <asp:button id="btnShowAll" runat="server" CssClass="button" Width="40px" Text="All" OnClick="btnShowAll_Click"></asp:button>
                </TD>               
                          
                 <TD id="tdSearch" runat="server" class="shade" vAlign="middle">&nbsp;                
                <B>Search for:&nbsp; </B>
                  <asp:dropdownlist Width="80px" id="ddlSearchField" runat="server">
                   
            <asp:ListItem Value="status_code">Status Code</asp:ListItem>            <asp:ListItem Value="description">Description</asp:ListItem>
                  </asp:dropdownlist>&nbsp;
                  <asp:dropdownlist Width="80px" id="ddlSearchOperation" runat="server">
                   
                    <asp:ListItem Value="Starts with ...">Begin</asp:ListItem>
                    <asp:ListItem Value="Equals">Equals</asp:ListItem>
                                    
                  </asp:dropdownlist>
                  <asp:textbox id="txtSearchValue" runat="server" Width="100px"></asp:textbox>
                  
                </TD>                
                
                
                
              </TR>
            </TABLE>
            <asp:label id="lblMessage" runat="server" ForeColor="Red"></asp:label>
          </td>
        </tr>
        <tr>
          <td>


             <asp:sqldatasource id="statusSqlDataSource"
                SelectCommand="select [status_code],   [description]   From [dbo].[status] ORDER BY [status_code] ASC"
                DeleteCommand="delete from [dbo].[status] where [status_code]=@status_code"
                ConnectionString="<%$ ConnectionStrings:Project1ConnectionString%>"
                ProviderName="<%$ ConnectionStrings:Project1ConnectionString.providerName%>"
                OnSelected="statusSqlDataSource_Selected" 
          runat="server" >
            </asp:sqldatasource>
           <asp:GridView id="dbGrid_status" runat="server" CssClass="Grid" Width="450px" 
                  datasourceid="statusSqlDataSource" DataKeyNames="status_code"
                  OnPageIndexChanged="dbGrid_status_PageIndexChanged" OnRowCommand="dbGrid_status_RowCommand" OnSorted="dbGrid_status_Sorted"

                  OnRowCreated="dbGrid_status_RowCreated"

                  OnRowDataBound="dbGrid_status_RowDataBound" 

                  OnRowDeleted="dbGrid_status_RowDeleted"                  

                  AutoGenerateColumns="False" AllowPaging="True" AllowSorting="True" BorderStyle="Dotted" EmptyDataText="No records found" OnSelectedIndexChanged="dbGrid_status_SelectedIndexChanged">
        <SelectedRowStyle BackColor="Yellow" CssClass="GridSelected" />
        <AlternatingRowStyle CssClass="GridItemOdd" />
        <EmptyDataRowStyle BorderStyle="None" BorderWidth="0px" Font-Bold="True" HorizontalAlign="Center" VerticalAlign="Middle" />
        <RowStyle HorizontalAlign="Center" CssClass="shade" />
        <HeaderStyle  BackColor="Teal" ForeColor="White" VerticalAlign="Middle"  HorizontalAlign="Center" Wrap="False"></HeaderStyle>
        
              <Columns>
              <asp:TemplateField Visible="false" HeaderImageUrl="images\icon_delete.gif">
              <ItemStyle  />
                  <ItemTemplate>          
          <input type="checkbox" name="chDelete" value='<%#DataBinder.Eval(Container,"RowIndex")%>'/>
                  </ItemTemplate>
                </asp:TemplateField>  
                <asp:TemplateField>
                  <ItemStyle HorizontalAlign=Center />
                   <ItemTemplate>       
		    <a href="#" onClick="javascript:top.opener.window.statusLookup('<%#DataBinder.Eval(Container,"DataItem.status_code")%>','<%#DataBinder.Eval(Container,"DataItem.description")%>');window.close();">Select</a>             	    
                   </ItemTemplate>
                </asp:TemplateField>
              <asp:ButtonField Text="Edit" Visible="False" CommandName="cmdEdit" HeaderImageUrl="images\icon_edit.gif"></asp:ButtonField>

                <asp:BoundField DataField="status_code" ReadOnly="True" HeaderText="Status Code" SortExpression="status_code">
                  
                </asp:BoundField>

                <asp:BoundField DataField="description" ReadOnly="True" HeaderText="Description" SortExpression="description">
                  
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

