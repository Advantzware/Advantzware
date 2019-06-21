<%@ Page Language="c#" AutoEventWireup="true" Debug="false" Inherits="Cstatus_list" Codebehind="status_list.aspx.cs" %>
<%@ Register Src="footer.ascx" TagName="Footer" TagPrefix="ft" %>
<%@ Register Src="header.ascx" TagName="Header" TagPrefix="hd" %>
<html xmlns="http://www.w3.org/1999/xhtml" >
  <head id="Head1" runat="server">
    <title>Status</title>
    <LINK href="include/style2.css" type="text/css" rel="stylesheet"/>
     
    <link href="include/tree.css" rel="stylesheet" type="text/css" />
    <link href="include/dhtmlwindow.css" rel="stylesheet" type="text/css" />
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
    <form id="frmList" runat="server"  defaultfocus='txt_statuscode'  >   
        <hd:header id="Header1" runat="server"></hd:header>
      <div>
            
      <TABLE id="tblTop" cellSpacing="3" border="0">
        <TR>
          
          <TD align=center nowrap><font size=+0><b>Status&nbsp;</b></font></TD>
          <TD vAlign="middle">
            <asp:linkbutton id="hlkBackToMenu" runat="server" OnClick="hlkBackToMenu_Click">Back to menu</asp:linkbutton>
          </TD>
          <TD vAlign="middle" align="center"><b>Users</b>&nbsp;&nbsp;Logged as&nbsp;
            <asp:label id="lblUser" runat="server" Font-Bold="True">&nbsp;</asp:label>&nbsp;&nbsp;
            <asp:linkbutton id="hlnkLogOut" runat="server" OnClick="hlnkLogOut_Click">Log out</asp:linkbutton>
            &nbsp;&nbsp;&nbsp;<asp:hyperlink id="hlnkChangePwd" runat="server" NavigateUrl="changepwd.aspx">Change password</asp:hyperlink>
          &nbsp;<b>Company:</b> &nbsp;<asp:label id="lblComp" runat="server" Font-Bold="True">&nbsp;</asp:label></TD>
          
         
          <TD vAlign="middle" width="20">&nbsp;</TD>
          
          <td width=30>&nbsp;</td>
        </TR>
      </TABLE>
      <table><tr bgcolor="gray">
      <td><div  id="navigation" style="width:100%">
		<ul nowrap> <li class="selected">
      <asp:LinkButton ID="lnk_status_list" runat="server" OnClick="lnk_status_list_Click" >List Status</asp:LinkButton></li>
      <li ><asp:LinkButton ID="lnk_status_view" runat="server" OnClick="lnk_status_view_Click" >View Status</asp:LinkButton></li></ul></div>  </td>
      
      </tr></table>
       <TABLE id="tblMain" cellSpacing="1" cellPadding="1" border="0" Width="510px">
        <TR>
          <TD>
            <TABLE id="tblSearch" cellSpacing="1" cellPadding="5" border="0"   bgcolor=black Width="510px">
              <TR>
                <TD class="shade" align="center" width="45">
                   <table cellspacing="2" cellpadding="1"  border="0" class="shade" bgcolor="gray">    		   
		            <tr><td  >
		   
                  <asp:button id="btnSearch" runat="server" CssClass="button" Width="40px" Text="Go" OnClick="btnSearch_Click"></asp:button>
                  <br />
                  
                  
                  <asp:button id="btnShowAll" runat="server" CssClass="button" Width="40px" Text="All" OnClick="btnShowAll_Click"></asp:button>
                </TD>               
                          
                               
                    <td class="shade"    align= "center" nowrap>
                  <b>Status Code</b><br />
                    <asp:TextBox ID="txt_statuscode" Width="100px" runat="server"></asp:TextBox>
                    </td>
                    <td class="shade"  align="center"  nowrap>
                   <b>Description</b><br />
                    <asp:textbox id="txt_description" runat="server" Width="100px"></asp:textbox>
                    </td>
              
                       
                
                <TD id="tdInfo" runat="server" class="shade" align="center">
                <table><tr><td align="center" nowrap>
            <b>Records/Page</b><BR>
          
          <asp:FormView ID="FormView2" runat="server" DataSourceID="ObjectDataSource2">
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
                              <asp:CompareValidator ID="CompareValidator4" runat="server" ControlToValidate="aLineLabel" SetFocusOnError="true" Display="dynamic" Operator="dataTypeCheck" Type="Integer" ErrorMessage="Invalid Number"></asp:CompareValidator>
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
                
                 </TR></table> 
              </TR>
            </TABLE>
            
            <asp:label id="lblMessage" runat="server" ForeColor="Red"></asp:label>
          </td>
        </tr>
        <tr>
          <td>

      <%--<p>
      <a href=# onClick = "ChSel()">Select/Unselect all</a>
      <asp:linkbutton id="btnDelete" runat="server" OnClick="btnDelete_Click">Delete selected</asp:linkbutton>
      </p>--%>

            <%--<asp:sqldatasource id="statusSqlDataSource"
                SelectCommand="select [status_code],   [description]   From [dbo].[status] ORDER BY [status_code] ASC"
                DeleteCommand="delete from [dbo].[status] where [status_code]=@status_code"
                ConnectionString="<%$ ConnectionStrings:Project1ConnectionString%>"
                ProviderName="<%$ ConnectionStrings:Project1ConnectionString.providerName%>"
                OnSelected="statusSqlDataSource_Selected" 
          runat="server" >
            </asp:sqldatasource>--%>
            <asp:GridView id="dbGrid_status" runat="server" CssClass="Grid" Width="510px" 
                   
                  OnPageIndexChanging="dbGrid_industry_sic_PageIndexChanging" 
                   OnSorted="dbGrid_status_Sorted"


                  OnRowDataBound="dbGrid_status_RowDataBound" OnSorting="GridView1_Sorting"

                  OnRowDeleted="dbGrid_status_RowDeleted"                  

                  AllowPaging="True" AllowSorting="True" BorderStyle="Dotted" EmptyDataText="No records found" OnSelectedIndexChanged="dbGrid_status_SelectedIndexChanged">
        <SelectedRowStyle BackColor="Yellow" CssClass="GridSelected" />
        <AlternatingRowStyle CssClass="GridItemOdd" />
        <EmptyDataRowStyle BorderStyle="None" BorderWidth="0px" Font-Bold="True" HorizontalAlign="Center" VerticalAlign="Middle" />
        <RowStyle  CssClass="shade" />
        <HeaderStyle  ForeColor="White" CssClass="headcolor" VerticalAlign="Middle"  HorizontalAlign="Center" Wrap="False"></HeaderStyle>
        
              <Columns>
                <asp:CommandField ShowSelectButton="True" ButtonType="Image" SelectImageUrl="~/Images/sel.gif" SelectText="" >
                    <ItemStyle Width="10px" />
                </asp:CommandField>
             

              </Columns>
            </asp:GridView>
          </TD>
        </TR>
        
        
         </TABLE>
          
          
    </div>
    <ft:footer id="Footer1" runat="server"></ft:footer>
    </form>
  </body>
</HTML>

