<%@ Page Language="c#" AutoEventWireup="true" Debug="false" Inherits="user_groupidlookup" Codebehind="user_groupidlookup.aspx.cs" %>

<html xmlns="http://www.w3.org/1999/xhtml" >
  <head id="Head1" runat="server">
    <title>User Maintenance</title>
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
    
    <form id="frmList" runat="server" defaultbutton='btnSearch' defaultfocus='txtSearchValue'>   
        
      <div>
            
      
      
      <TABLE id="tblMain" cellSpacing="1" cellPadding="1"  width='450px' border="0">
        <TR>
          <TD>
            <TABLE id="tblSearch" cellSpacing="1" cellPadding="5" width="450px" border="0"  bgcolor=black>
              <TR>
                <TD class="shade" align="center" width="50">
                  <asp:button id="btnSearch" runat="server" Width="40px" CssClass="button" Text="Go" OnClick="btnSearch_Click"></asp:button><br /><br />
                  <asp:button id="btnShowAll" runat="server" Width="40px" CssClass="button" Text="All" OnClick="btnShowAll_Click"></asp:button>&nbsp;
                </TD>               
                          
                <TD id="tdSearch" runat="server" class="shade" vAlign="middle" align="center" width="800">&nbsp;                
                  <asp:dropdownlist id="ddlSearchField" runat="server">
                    <%--<asp:ListItem Value="Any Field">Any field</asp:ListItem>--%>
            <asp:ListItem Value="group">Group</asp:ListItem>   <asp:ListItem Value="GroupID">GroupID</asp:ListItem>
                  </asp:dropdownlist>&nbsp;
                  <asp:dropdownlist id="ddlSearchOperation" runat="server">
                   <%-- <asp:ListItem Value="Contains">Contains</asp:ListItem>--%>
                    <asp:ListItem Value="Equals">Equals</asp:ListItem>
                    <asp:ListItem Value="Starts with ...">Starts with ...</asp:ListItem>
                   <%-- <asp:ListItem Value="More than ...">More than ...</asp:ListItem>
                    <asp:ListItem Value="Less than ...">Less than ...</asp:ListItem>
                    <asp:ListItem Value="Equal or more than ...">Equal or more than ...</asp:ListItem>
                    <asp:ListItem Value="Equal or less than ...">Equal or less than ...</asp:ListItem>
                    <asp:ListItem Value="IsNull">Empty</asp:ListItem>   --%>                
                  </asp:dropdownlist>
                  <asp:textbox id="txtSearchValue" runat="server" Width="111px"></asp:textbox>
                  
                </TD>               
                
                
              </TR>
            </TABLE>
            <asp:label id="lblMessage" runat="server" ForeColor="Red"></asp:label>
          </td>
        </tr>
        <tr>
          <td>

     

            <asp:sqldatasource id="user_masterSqlDataSource"
                SelectCommand="select [groupID],   [group],   [groupID2]   From [dbo].[usergroup] ORDER BY [group] ASC"
                
                ConnectionString="<%$ ConnectionStrings:Project1ConnectionString%>"
                ProviderName="<%$ ConnectionStrings:Project1ConnectionString.providerName%>"
                   runat="server">
            </asp:sqldatasource>
            <asp:GridView id="dbGrid_user_master" runat="server" CssClass="Grid" Width="100%"
                  datasourceid="user_masterSqlDataSource" DataKeyNames="groupID"
                  OnPageIndexChanged="dbGrid_user_master_PageIndexChanged" OnRowCommand="dbGrid_user_master_RowCommand" OnSorted="dbGrid_user_master_Sorted"

                  OnRowCreated="dbGrid_user_master_RowCreated"

                  OnRowDataBound="dbGrid_user_master_RowDataBound" 

                  OnRowDeleted="dbGrid_user_master_RowDeleted"                  

                  AutoGenerateColumns="False" AllowPaging="True" AllowSorting="True" BorderStyle="Dotted" EmptyDataText="No records found" OnSelectedIndexChanged="dbGrid_user_master_SelectedIndexChanged">
        <SelectedRowStyle BackColor="yellow" CssClass="GridSelected" />
        <AlternatingRowStyle CssClass="GridItemOdd" />
        <EmptyDataRowStyle BorderStyle="None" BorderWidth="0px" Font-Bold="True" HorizontalAlign="Center" VerticalAlign="Middle" />
        <RowStyle  CssClass="shade" />
        <HeaderStyle  BackColor="teal" ForeColor="white" VerticalAlign="Middle"  HorizontalAlign="Center" Wrap="False"></HeaderStyle>
        
              <Columns>

                <asp:TemplateField Visible="False" HeaderImageUrl="images\icon_delete.gif">
              <ItemStyle  />
                  <ItemTemplate>          
         
                  </ItemTemplate>
                </asp:TemplateField>    
                
                <asp:TemplateField>
                  <ItemStyle  />
                   <ItemTemplate>       
		    <a href="#" onClick="javascript:top.opener.window.groupidlookup('<%#DataBinder.Eval(Container,"DataItem.group")%>');window.close();">Select</a>             	    
                   </ItemTemplate>
                </asp:TemplateField>  

              

                <asp:BoundField DataField="groupID" ReadOnly="True" HeaderText="groupID" SortExpression="groupID">
                  
                </asp:BoundField>

                <%--<asp:BoundField DataField="Username" ReadOnly="True" HeaderText="Username" SortExpression="Username">
                  
                </asp:BoundField>

                <asp:BoundField DataField="email" ReadOnly="True" HeaderText="email" SortExpression="email">
                  
                </asp:BoundField>--%>

                <asp:BoundField DataField="group" ReadOnly="True" HeaderText="Group" SortExpression="group">
                  
                </asp:BoundField>

              </Columns>
            </asp:GridView>
          </TD>
          
        </TR>
        <tr><td><input type="button" name="close" class="buttonM" id="close" value="Close" onclick="javascript:window.close()" /></td></tr>
      </TABLE>      
      
    </div>
    
    </form>
    
  </body>
</HTML>

