<%@ Page Language="c#" AutoEventWireup="true" EnableViewState="true" Inherits="Cuser_master_list" EnableEventValidation="false" Codebehind="user_master_list.aspx.cs" %>
<%@ Register Src="footer.ascx" TagName="Footer" TagPrefix="ft" %>
<%@ Register Src="header.ascx" TagName="Header" TagPrefix="hd" %>
<html xmlns="http://www.w3.org/1999/xhtml" >
  <head id="Head1" runat="server">
    <title>User Maintenance</title>
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
    
    <form id="frmList" runat="server"  defaultfocus='txt_userid'>   
        <hd:header id="Header1" runat="server"></hd:header>
      <div>
            
      <TABLE id="tblTop" cellSpacing="3" align="center" border="0" Width="100%">
        <TR>
          <TD width=30>&nbsp;</TD>
          <TD align=center nowrap><font size=+0><b>&nbsp;User Maintenance&nbsp;</b></font></TD>
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
      <table><tr bgcolor="gray"><td><div  id="navigation" style="width:100%">
		<ul nowrap> <li class="selected">
      <asp:LinkButton ID="lnk_Listview" runat="server" OnClick="lnk_Listview_Click">List Users</asp:LinkButton></li>
      <li><asp:LinkButton ID="lnk_viewuser" runat="server" OnClick="lnk_viewuser_Click">View Users</asp:LinkButton></li></ul></div>
      <%--<asp:LinkButton ID="lnk_company" runat="server" OnClick="lnk_company_Click" ><img src="Images/company 0.jpg"  border="0" alt="Company" /></asp:LinkButton>--%>
      
      
      </td>
      </tr></table>
      <TABLE  id="tblMain" cellSpacing="1" cellPadding="1"  width='95%' border="0">
        <TR>
          <TD>
            <TABLE class="shade" id="tblSearch" cellSpacing="0" cellPadding="0" width="100%" border="1"  bgcolor=black>
              <TR>
              <TD class="shade" align="left"><br>
                  <table cellspacing="2" cellpadding="1"  border="0" class="shade" bgcolor="gray">    		   
		            <tr>
                <TD nowrap>
                  <asp:button id="btnSearch" runat="server" Width="40px" CssClass="button" Text="Go" OnClick="btnSearch_Click"></asp:button><br /><br />
                  <asp:button id="btnShowAll" runat="server" Width="40px" CssClass="button" Text="All" OnClick="btnShowAll_Click"></asp:button>&nbsp;
                </TD>               
                          
                <td >
                  <b>User ID</b><br />
                    <asp:TextBox ID="txt_userid" Width="100px" runat="server"></asp:TextBox>
                    </td><td>
                   <b>User Name</b><br />
                    <asp:textbox id="txt_username" runat="server" Width="100px"></asp:textbox>
                    </td><td>
                    
                    <b>Email</b><br />
                    <asp:textbox id="txt_email" runat="server" Width="100px"></asp:textbox>
                        </td><td>
                    <b>Group</b><br />
                    <asp:textbox id="txt_group" runat="server" Width="80px"></asp:textbox>
                    
                </td>          
                
                
                <TD id="tdPageCount" runat="server" class="shade" align="left">
          <table><tr><td align="center">
            Records Per Page:<BR>
          
          <asp:FormView ID="FormView1" runat="server"  DataSourceID="ObjectDataSource2">
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
                             
                              <asp:TextBox ID="aLineLabel" runat="server" Width="40px" OnTextChanged="ddl_display_TextChanged" Text='<%# Bind("aLine") %>'></asp:TextBox>
                             <asp:CompareValidator ID="CompareValidator1" runat="server" ErrorMessage="Invalid Input" ControlToValidate="aLineLabel" Display="dynamic" SetFocusOnError="true" Operator="datatypecheck" Type="integer"></asp:CompareValidator>
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
            </TD></TR></TABLE>
          </td>
        </tr>
        <tr>
          <td>

            <asp:GridView id="dbGrid_user_master" runat="server" CssClass="Grid" Width="100%"
                   DataKeyNames="UserID" EnableViewState="false"
                  OnPageIndexChanging="dbGrid_user_master_PageIndexChanging" OnSelectedIndexChanged="dbGrid_user_master_SelectedIndexChanged"
                    OnSorting="dbGrid_user_master_Sorting"
                                                   

                  AutoGenerateColumns="False" AllowPaging="True" AllowSorting="True" BorderStyle="Dotted" EmptyDataText="No records found" >
        <SelectedRowStyle BackColor="Yellow" CssClass="GridSelected" />
        <AlternatingRowStyle CssClass="GridItemOdd" />
        <EmptyDataRowStyle BorderStyle="None" BorderWidth="0px" Font-Bold="True" HorizontalAlign="Center" VerticalAlign="Middle" />
        <RowStyle HorizontalAlign="Center" CssClass="shade" />
        <HeaderStyle  ForeColor="White" CssClass="headcolor" VerticalAlign="Middle"  HorizontalAlign="Center" Wrap="False"></HeaderStyle>
        
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
                  <asp:TemplateField HeaderText="UserID" Visible="false" SortExpression="UserID">
                      <EditItemTemplate>
                          <asp:Label ID="Label1edit" runat="server" Text='<%# Eval("UserID") %>'></asp:Label>
                      </EditItemTemplate>
                      <ItemTemplate>
                          <asp:Label ID="Label1" runat="server" Text='<%# Bind("UserID") %>'></asp:Label>
                      </ItemTemplate>
                  </asp:TemplateField>

                <asp:BoundField DataField="Username" ReadOnly="True" HeaderText="UserID" SortExpression="Username">
                    <ItemStyle HorizontalAlign="Left" />
                  
                </asp:BoundField>
                <asp:BoundField DataField="name" ReadOnly="True" HeaderText="User Name" SortExpression="name">
                    <ItemStyle HorizontalAlign="Left" />
                    
                  
                </asp:BoundField>
                

                <asp:BoundField DataField="email" ReadOnly="True" HeaderText="Email" SortExpression="email">
                    <ItemStyle HorizontalAlign="Left" />
                    
                  
                </asp:BoundField>
                

                <asp:BoundField DataField="GroupID" ReadOnly="True" HeaderText="GroupID" SortExpression="GroupID">
                    <ItemStyle HorizontalAlign="Left" />
                  
                </asp:BoundField>

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

