<%@ Page Language="c#" AutoEventWireup="true" Debug="false" Inherits="view_main_help" Codebehind="view_main_help.aspx.cs" %>
<%@ Register Src="footer.ascx" TagName="Footer" TagPrefix="ft" %>
<%@ Register Src="header.ascx" TagName="Header" TagPrefix="hd" %>
<html xmlns="http://www.w3.org/1999/xhtml" >
  <head id="Head1" runat="server">
    <title>Help Maintenance</title>
    
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
 
    
    </script> 
  </head>    
   <body>
    <form id="frmList" runat="server" >   
        <hd:header id="Header1" runat="server"></hd:header>
    <asp:HiddenField ID="HiddenField1" runat="server" />
      <div>
            
      <TABLE id="tblTop" cellSpacing="3" border="0">
        <TR>
          
          <TD align=center nowrap><font size=+0><b>Help Maintenance&nbsp;</b></font></TD>
          <TD vAlign="middle">
            <asp:linkbutton id="hlkBackToMenu" runat="server" OnClick="hlkBackToMenu_Click">Back to menu</asp:linkbutton>
          </TD>
          <TD vAlign="middle" align="center"><b>Users</b>&nbsp;&nbsp;&nbsp;Logged as&nbsp;
            <asp:label id="lblUser" runat="server" Font-Bold="True">&nbsp;</asp:label>&nbsp;&nbsp;&nbsp;
            <asp:linkbutton id="hlnkLogOut" runat="server" OnClick="hlnkLogOut_Click">Log out</asp:linkbutton>
            &nbsp;&nbsp;&nbsp;<asp:hyperlink id="hlnkChangePwd" runat="server" NavigateUrl="changepwd.aspx"></asp:hyperlink>
          &nbsp;<b>Company:</b> &nbsp;<asp:label id="lblComp" runat="server" Font-Bold="True">&nbsp;</asp:label>
</TD>
          
          <%--<TD vAlign="middle" align="center">
          <asp:label id="lblQuickJump" runat="server">Quick jump :&nbsp;</asp:label>&nbsp;
          <asp:dropdownlist id="ddlQuickJump" runat="server"  AutoPostBack="True" OnSelectedIndexChanged="ddlQuickJump_SelectedIndexChanged">                            
          </asp:dropdownlist>&nbsp;&nbsp;
          </TD>--%>
          
          <TD vAlign="middle" width="20">&nbsp;</TD>
          
          <td width=30>&nbsp;</td>
        </TR>
      </TABLE>
      <table>
    <tr bgcolor="gray">
    <td><div  id="navigation" style="width:100%">
		<ul nowrap> <li >
        <asp:LinkButton ID="lnk_listview_industry" runat="server"  OnClick="lnk_listview_industry_Click" >Browse Help</asp:LinkButton></li>
        <li class="selected"><asp:LinkButton ID="lnk_listview_sic" runat="server"  OnClick="lnk_listview_sic_Click">View Help</asp:LinkButton></li></ul></div>
        
    </td>
    </tr>
    </table> <asp:label id="lblMessage" runat="server" ForeColor="Red"></asp:label>
    
      <asp:sqldatasource id="Sqldatasource1"
                SelectCommand="SELECT * FROM [hlp_head] WHERE ([msg_num] = @msg_num)"
                
                ConnectionString="<%$ ConnectionStrings:Project1ConnectionString %>"
                ProviderName="<%$ ConnectionStrings:Project1ConnectionString.providerName %>"
                OnSelected="help_mainSqlDataSource_Selected" 
           runat="server" OnDeleted="Sqldatasource1_Deleted" 
              OnInserted="Sqldatasource1_Inserted">
        <SelectParameters>
        <asp:SessionParameter SessionField="help_main_list_code_help" Name="msg_num" Type="Int32"  />
        </SelectParameters>
        <%--<DeleteParameters>
        <asp:SessionParameter SessionField="industry_sic_list_code" Name="industry_sic_code" Type="String" />
        </DeleteParameters>--%>
            </asp:sqldatasource>     
      
          <asp:FormView ID="FormView1" runat="server" DataSourceID="Sqldatasource1" 
              OnDataBound="FormView1_DataBound" DataKeyNames="msg_num" >
              
              
              <ItemTemplate> 
              <table class="shade">
              <tr><td><b>Message#:</b></td><td><asp:Label ID="msg_numLabel" Width="100px" runat="server" BackColor="PaleTurquoise" BorderColor="White" BorderStyle="Solid" BorderWidth="1px" Text='<%# Eval("msg_num") %>' /></td>
              </tr>
              <tr><td><b>Field Name:</b></td><td><asp:Label ID="fld_nameLabel" Width="100px" runat="server" BackColor="PaleTurquoise" BorderColor="White" BorderStyle="Solid" BorderWidth="1px" Text='<%# Bind("fld_name") %>' /></td>
              <td><b>Title:</b></td><td><asp:Label ID="frm_titleLabel" Width="200px" runat="server" BackColor="PaleTurquoise" BorderColor="White" BorderStyle="Solid" BorderWidth="1px" Text='<%# Bind("frm_title") %>' /></td></tr>
              <tr><td><b>File Name:</b></td><td><asp:Label ID="fil_nameLabel" Width="100px" runat="server" BackColor="PaleTurquoise" BorderColor="White" BorderStyle="Solid" BorderWidth="1px" Text='<%# Bind("fil_name") %>' /></td>
              <td><b>Frame Name:</b></td><td><asp:Label ID="frm_nameLabel" Width="200px" runat="server" BackColor="PaleTurquoise" BorderColor="White" BorderStyle="Solid" BorderWidth="1px" Text='<%# Bind("frm_name") %>' /></td></tr>
              <tr><td valign="top"><b>Help Contents:</b></td><td colspan="3"><asp:TextBox ID="help_txtLabel" TextMode="MultiLine" Height="400px"  Width="600px" ReadOnly="true" BackColor="PaleTurquoise" BorderColor="White" BorderStyle="Solid" BorderWidth="1px" runat="server" Text='<%# Bind("help_txt") %>' /></td>
              </tr>
              </table>
                            
                  
              </ItemTemplate>
          </asp:FormView>
        
    </div>
    <ft:footer id="Footer1" runat="server"></ft:footer>
    </form>
  </body>
</HTML>

