<%@ Page Language="c#" AutoEventWireup="true" Debug="false" Inherits="Cusergroup_list" Codebehind="usergroup_list.aspx.cs" %>
<%@ Register Src="footer.ascx" TagName="Footer" TagPrefix="ft" %>
<%@ Register Src="header.ascx" TagName="Header" TagPrefix="hd" %>

<html xmlns="http://www.w3.org/1999/xhtml" >
  <head id="Head1" runat="server">
    <title>User Group Maintenance</title>
    <LINK href="include/style2.css" type="text/css" rel="stylesheet"/>
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
    <form id="frmList" runat="server" defaultfocus='txtSearchValue'>   

      <div>
            
      <TABLE id="tblTop" cellSpacing="3" align="center" border="0" Width="100%">
        <TR>
          <TD width=30>&nbsp;</TD>
          <TD align=center nowrap><font size=+0><b>User Group Maintenance&nbsp;</b></font></TD>
          <TD vAlign="middle">
            <asp:linkbutton id="hlkBackToMenu" runat="server" OnClick="hlkBackToMenu_Click"></asp:linkbutton>
          </TD>
          <TD vAlign="middle" align="center">Logged as&nbsp;
            <asp:label id="lblUser" runat="server" Font-Bold="True">&nbsp;</asp:label>&nbsp;&nbsp;&nbsp;
            <asp:linkbutton id="hlnkLogOut" runat="server" OnClick="hlnkLogOut_Click">Log out</asp:linkbutton>
            
            &nbsp;&nbsp;&nbsp;<asp:hyperlink id="hlnkChangePwd" runat="server" NavigateUrl="changepwd.aspx">Change password</asp:hyperlink>
            
          </TD>
          
                    
          <TD vAlign="middle" width="20">&nbsp;</TD>
          
         
          <td width=30>&nbsp;</td>
        </TR>
       
      </TABLE>
      <table cellSpacing="1"   ><tr bgcolor="gray"><td> <div  id="navigation" style="width:100%">
		<ul nowrap> <li class="selected">
      <asp:LinkButton ID="lnk_listgroup" runat="server" OnClick="lnk_listgroup_Click" >List Group</asp:LinkButton></li>
      <li><asp:LinkButton ID="lnk_viewgroup" runat="server" OnClick="lnk_viewgroup_Click" >View Group</asp:LinkButton></li>
      <li><asp:LinkButton ID="lnk_groupuser" runat="server" OnClick="lnk_groupuser_Click">User Group</asp:LinkButton></li></ul></div>
      
      
      </td>
      </tr></table>
      
      <TABLE id="tblMain" cellSpacing="1" cellPadding="1"  width='95%' border="0">
        <TR>
          <TD>
            <TABLE id="tblSearch" cellSpacing="1" cellPadding="5" width="100%" border="0"  bgcolor=black>
              <TR>
                <TD class="shade" align="center" width="50"><nobr>
                  <asp:button id="btnSearch" runat="server" CssClass="button" Width="40px" Text="Go" OnClick="btnSearch_Click"></asp:button><br /><br />
                  <asp:button id="btnShowAll" runat="server" CssClass="button" Width="40px" Text="All" OnClick="btnShowAll_Click"></asp:button>
                </TD>               
                          
                <TD id="tdSearch" runat="server" class="shade" vAlign="middle" align="center" width="800">&nbsp;                
                <B>Search for:&nbsp; </B>&nbsp;&nbsp;&nbsp;
                  <asp:dropdownlist id="ddlSearchField" runat="server">
                    <%--<asp:ListItem Value="Any Field">Any field</asp:ListItem>--%>
            <asp:ListItem Value="groupID">groupID</asp:ListItem>            <asp:ListItem Value="group">group</asp:ListItem>
                  </asp:dropdownlist>&nbsp;&nbsp;
                  <asp:dropdownlist id="ddlSearchOperation" runat="server">
                    <%--<asp:ListItem Value="Contains">Contains</asp:ListItem>--%>
                    
                    <asp:ListItem Value="Starts with ...">Begin</asp:ListItem>
                    <asp:ListItem Value="Equals">Equals</asp:ListItem>
                   <%-- <asp:ListItem Value="More than ...">More than ...</asp:ListItem>
                    <asp:ListItem Value="Less than ...">Less than ...</asp:ListItem>
                    <asp:ListItem Value="Equal or more than ...">Equal or more than ...</asp:ListItem>
                    <asp:ListItem Value="Equal or less than ...">Equal or less than ...</asp:ListItem>
                    <asp:ListItem Value="IsNull">Empty</asp:ListItem> --%>                  
                  </asp:dropdownlist>
                  <asp:textbox id="txtSearchValue" runat="server" Width="136px"></asp:textbox>
                  
                </TD>               
                
                <TD id="tdInfo" runat="server" class="shade" align="center" width="100">
                  <asp:label id="lblCount" runat="server" Height="3px">Details found:&nbsp;0</asp:label><BR>
                  <asp:label id="lblPage" runat="server">Page&nbsp;<%=

                 (dbGrid_usergroup.PageCount ==0)?0:dbGrid_usergroup.PageIndex + 1
                  %>&nbsp;of&nbsp;<%=dbGrid_usergroup.PageCount%></asp:label>
                </TD>
                <TD id="tdPageCount" runat="server" class="shade" align="left">
          <table><tr><td align="center">
            Records Per Page:<br /><asp:FormView ID="FormView2" runat="server" DataSourceID="ObjectDataSource2">
                          <EditItemTemplate>
                              aLine:
                              <asp:TextBox ID="aLineTextBox" runat="server" Text='<%# Bind("aLine") %>'>
                              </asp:TextBox><br />
                              <asp:LinkButton ID="UpdateButton" runat="server" CausesValidation="True" CommandName="Update"
                                  Text="Update">
                              </asp:LinkButton>
                              <asp:LinkButton ID="UpdateCancelButton" runat="server" CausesValidation="False" CommandName="Cancel"
                                  Text="Cancel">
                              </asp:LinkButton>
                          </EditItemTemplate>
                          <InsertItemTemplate>
                              aLine:
                              <asp:TextBox ID="aLineTextBox" runat="server" Text='<%# Bind("aLine") %>'>
                              </asp:TextBox><br />
                              <asp:LinkButton ID="InsertButton" runat="server" CausesValidation="True" CommandName="Insert"
                                  Text="Insert">
                              </asp:LinkButton>
                              <asp:LinkButton ID="InsertCancelButton" runat="server" CausesValidation="False" CommandName="Cancel"
                                  Text="Cancel">
                              </asp:LinkButton>
                          </InsertItemTemplate>
                          <ItemTemplate>
                             
                              <asp:TextBox ID="aLineLabel" runat="server" Width="70px" OnTextChanged="ddl_display_TextChanged" Text='<%# Bind("aLine") %>'></asp:TextBox>
                              <asp:CompareValidator ID="CompareValidator1" runat="server" ErrorMessage="Only Numbers" ControlToValidate="aLineLabel" Display="dynamic" SetFocusOnError="true" Operator="datatypecheck" Type="integer"></asp:CompareValidator>
                              <%--<asp:Label ID="aLineLabel" runat="server" Text='<%# Bind("aLine") %>'></asp:Label>--%>
                          </ItemTemplate>
                      </asp:FormView>
                      <asp:ObjectDataSource ID="ObjectDataSource2" runat="server" OldValuesParameterFormatString="original_{0}"
                          SelectMethod="SelectRows" TypeName="Order">
                          <SelectParameters>
                              <asp:SessionParameter Name="prmUser" SessionField="Rowuser" Type="String" />
                              <asp:SessionParameter Name="vLine" Type="Int32" SessionField="gridsize" />
                          </SelectParameters>
                      </asp:ObjectDataSource>
                
          </td></tr></table>  
                </TD>
              </TR>
            </TABLE>
            <asp:label id="lblMessage" runat="server" ForeColor="Red"></asp:label>
          </td>
        </tr>
        <tr>
          <td>

      

            <asp:sqldatasource id="usergroupSqlDataSource"
                SelectCommand="select [groupID],   [group]   From [dbo].[usergroup] ORDER BY [groupID] ASC"
                DeleteCommand="delete from [dbo].[usergroup] where [groupID]=@groupID"
                ConnectionString="<%$ ConnectionStrings:Project1ConnectionString%>"
                ProviderName="<%$ ConnectionStrings:Project1ConnectionString.providerName%>"
                OnSelected="usergroupSqlDataSource_Selected" 
        OnDeleting="usergroupSqlDataSource_Deleting"   runat="server">
            </asp:sqldatasource>
            <asp:GridView id="dbGrid_usergroup" runat="server" CssClass="Grid" Width="100%"
                  datasourceid="usergroupSqlDataSource" DataKeyNames="groupID"
                  OnPageIndexChanged="dbGrid_usergroup_PageIndexChanged" OnRowCommand="dbGrid_usergroup_RowCommand" OnSorted="dbGrid_usergroup_Sorted"

                  OnRowCreated="dbGrid_usergroup_RowCreated"

                  OnRowDataBound="dbGrid_usergroup_RowDataBound" 

                  OnRowDeleted="dbGrid_usergroup_RowDeleted"                  

                  AutoGenerateColumns="False" AllowPaging="True" AllowSorting="True" BorderStyle="Dotted" EmptyDataText="No records found" OnSelectedIndexChanged="dbGrid_usergroup_SelectedIndexChanged">
        <SelectedRowStyle  BackColor="yellow" />
        <AlternatingRowStyle CssClass="GridItemOdd" />
        <EmptyDataRowStyle BorderStyle="None" BorderWidth="0px" Font-Bold="True" HorizontalAlign="Center" VerticalAlign="Middle" />
        <RowStyle CssClass="shade" />
        <HeaderStyle   ForeColor="White" CssClass="headcolor" VerticalAlign="Middle"  HorizontalAlign="Center" Wrap="False"></HeaderStyle>
        
              <Columns>

              <asp:TemplateField Visible="False" HeaderImageUrl="images\icon_delete.gif">
              <ItemStyle HorizontalAlign=Center />
                  <ItemTemplate>          
          <input type="checkbox" name="chDelete" value='<%#DataBinder.Eval(Container,"RowIndex")%>'/>
                  </ItemTemplate>
                </asp:TemplateField>    
                <asp:CommandField ShowSelectButton="True" ButtonType="Image" SelectImageUrl="~/Images/sel.gif" SelectText="" >
                    <ItemStyle Width="10px" />
                </asp:CommandField> 

              <asp:ButtonField Visible="False" Text="Edit" CommandName="cmdEdit" HeaderImageUrl="images\icon_edit.gif"></asp:ButtonField>

                <asp:BoundField DataField="groupID" ReadOnly="True" HeaderText="Group ID" SortExpression="groupID">
                  
                </asp:BoundField>

                <asp:BoundField DataField="group" ReadOnly="True" HeaderText="Group" SortExpression="group">
                  
                </asp:BoundField>

              </Columns>
            </asp:GridView>
          </TD>
        </TR>
      </TABLE>      
      
    </div>
    
    </form>
  </body>
  <ft:Footer ID="Footer1" runat="server" />
</HTML>

