<%@ Page Language="c#" AutoEventWireup="true" Debug="true" Inherits="vendor_cross_reference" Codebehind="vendor_cross_reference.aspx.cs" %>
<%@ Register Src="footer.ascx" TagName="Footer" TagPrefix="ft" %>
<%@ Register Src="header.ascx" TagName="Header" TagPrefix="hd" %>
<html xmlns="http://www.w3.org/1999/xhtml" >
  <head id="Head1" runat="server">
    <title>Vendor Cross Reference</title>
    
    <link href="include/style2.css" type="text/css" rel="stylesheet"/>
      <LINK REL="stylesheet" TYPE="text/css" HREF="include/CalendarControl.css" >
    <link href="include/tree.css" rel="stylesheet" type="text/css" />
    <link href="include/dhtmlwindow.css" rel="stylesheet" type="text/css" />
    <script language = "JavaScript" src="include/CalendarControl.js">       
    </script>
    <script language="javascript" src="include/date.js"></script>
    <script language="javascript" src="include/event.js"></script>
    <script language="javascript" src="include/insert.js"></script>
    <script language = "JavaScript" type="text/javascript">
    
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
    function setfocus() {

        var da = document.getElementById("txt_arcode");
        da.focus();

    }
   
    </script> 
  </head>    
   <body>
    <form id="frmList" runat="server"   defaultfocus ='txt_arcode' >   
        <hd:header id="Header1" runat="server"></hd:header>
      <div>
            
      <table id="tblTop" cellSpacing="3" border="0">
        <tr>
          
          <td align=center nowrap><font size=+0><b>Vendor Cross Reference&nbsp;</b></font></td>
          <td vAlign="middle">
            <asp:linkbutton id="hlkBackToMenu" runat="server" OnClick="hlkBackToMenu_Click"></asp:linkbutton>
          </td>
          <td vAlign="middle" align="center"><b></b>&nbsp;&nbsp;&nbsp;Logged as&nbsp;
            <asp:label id="lblUser" runat="server" Font-Bold="True">&nbsp;</asp:label>&nbsp;&nbsp;&nbsp;
            <asp:linkbutton id="hlnkLogOut" runat="server" OnClick="hlnkLogOut_Click">Log out</asp:linkbutton>
            &nbsp;&nbsp;&nbsp;<asp:hyperlink id="hlnkChangePwd" runat="server" NavigateUrl="changepwd.aspx">Change password</asp:hyperlink>
          </td>  <td>&nbsp;&nbsp;&nbsp;
            <b>Company:</b>&nbsp;
            <asp:label id="lblComp" runat="server" Font-Bold="True">&nbsp;</asp:label>&nbsp;&nbsp;&nbsp;
          </TD>        
         
          
          <td vAlign="middle" width="20">&nbsp;</td>
          
          <td width=30>&nbsp;</td>
        </tr>
      </table>
      <table>
    <tr style="background-color:Gray">
    <td><div  id="navigation" style="width:100%">
		<ul nowrap><li class="selected" >
    <asp:LinkButton ID="lnk_listitem" runat="server" OnClick="lnk_browse_click">Browse Vendor</asp:LinkButton></li>
    <li><asp:LinkButton ID="lnk_viewitem" runat="server" OnClick="lnk_view_click">View Vendor</asp:LinkButton></li></ul></div>
    </td>
    </tr></table>
     
      <TABLE id="tblMain" cellSpacing="1" Width="500px" cellPadding="1" border="0">
        <TR>
          <TD style="width: 761px">
            <TABLE id="tblSearch" cellSpacing="1" Width="500px"  cellPadding="5" border="0"  bgcolor=black>
              <TR>
                <TD class="shade" align="center" width="50">
                  <table cellspacing="2" cellpadding="1"  border="0" class="shade" bgcolor="gray">    		   
		            <tr><td>
                  <asp:button id="btnSearch" runat="server" Width="40px" CssClass="button" Text="Go" OnClick="btnSearch_Click"></asp:button>
                  <br />
                 
                  <asp:button id="btnShowAll" runat="server" Width="40px" CssClass="button" Text="All" OnClick="btnShowAll_Click"></asp:button>
                </TD>  
                         
                     <td class="shade" align="center" nowrap >
                  <b>Suppliers A/R Code</b> <br />
                    <asp:TextBox ID="txt_arcode" Width="70px" MaxLength="8" runat="server"></asp:TextBox>
                    </td>            
                    <td class="shade" align="center" nowrap >
                   <b>Customers A/P Code</b><br />
                    <asp:textbox id="txt_apcode" runat="server" Width="90px"></asp:textbox>                    
                    </td>            
                    <td class="shade" align="center" nowrap >
                      <b>Customers A/P Description</b> <br />
                      <asp:TextBox ID="txt_custapdesc" Width="140px" MaxLength="30" runat="server" ></asp:TextBox>
                    </td> 
                                   
                
                <td align="center" class="shade" nowrap>
           <b> Records/Page</b><BR>
                        
            <asp:FormView ID="FormView2" runat="server" DataSourceID="ObjectDataSource2">
                          
                          <ItemTemplate>                             
                              <asp:TextBox ID="aLineLabel" runat="server" Width="70px" onblur="setfocus()" OnTextChanged="ddl_display_TextChanged" Text='<%# Bind("aLine") %>'></asp:TextBox>
                              <asp:CompareValidator ID="CompareValidator4" runat="server" ControlToValidate="aLineLabel" SetFocusOnError="true" Display="dynamic" Operator="dataTypeCheck" Type="Integer" ErrorMessage="Invalid Input"></asp:CompareValidator>
                          </ItemTemplate>
                      </asp:FormView>
                      <asp:ObjectDataSource ID="ObjectDataSource2" runat="server" OldValuesParameterFormatString="original_{0}"
                          SelectMethod="SelectRows" TypeName="Order">
                          <SelectParameters>
                              <asp:SessionParameter Name="prmUser" SessionField="Rowuser" Type="String" />
                              <asp:SessionParameter Name="vLine" Type="Int32" SessionField="gridsize" />
                          </SelectParameters>
                      </asp:ObjectDataSource>
                </td>
                </TR></table>
               </TD>
              </TR>
            </TABLE>
           
          </td>
        </tr>
        <tr>
          <td style="width: 761px">

     <asp:GridView ID="GridView1" AutoGenerateColumns="False" runat="server" DataSourceID="ObjectDataSource1"
     AllowPaging="True" AllowSorting="True" 
            EmptyDataText="No Records Found" BorderStyle="Dotted" CssClass="Grid" OnSelectedIndexChanged="GridView1_SelectedIndexChanged" Width="100%">
            <SelectedRowStyle CssClass="GridSelected" BackColor="Yellow" />
            <AlternatingRowStyle CssClass="GridItemOdd" />
            <EmptyDataRowStyle BorderStyle="Dotted" BorderColor="Gray" BorderWidth="0px" Font-Bold="True" HorizontalAlign="Center" VerticalAlign="Middle" />
            <HeaderStyle  ForeColor="White" CssClass="headcolor" Height="40px"  VerticalAlign="Middle"  HorizontalAlign="Center" ></HeaderStyle>
        <RowStyle CssClass="shade"  />
         <Columns>
             <asp:CommandField ShowSelectButton="True" ButtonType="Image" SelectImageUrl="~/Images/sel.gif" SelectText="" > 
                <ItemStyle Width="10px" />
             </asp:CommandField>
             <asp:BoundField DataField="vCustNo" HeaderText="Suppliers A/R Code" SortExpression="vCustNo" />
             <asp:BoundField DataField="vVendorCode" HeaderText="Customers A/P Code" SortExpression="vVendorCode" />
             <asp:BoundField DataField="vCustName" HeaderText="Customers A/P Description" SortExpression="vCustName" />
             <asp:BoundField DataField="vCompany" HeaderText="vCompany" SortExpression="vCompany" Visible="false" />
         </Columns>
     </asp:GridView>
              <asp:ObjectDataSource ID="ObjectDataSource1" runat="server" OldValuesParameterFormatString="original_{0}"
                  SelectMethod="Selectvendorcross" TypeName="custitem">
                  <SelectParameters>
                      <asp:Parameter Name="prmUser" Type="String" />
                      <asp:Parameter Name="prmAction" Type="String" DefaultValue="Select" />
                      <asp:SessionParameter SessionField="crossref_arcode_txt" Name="prmCustNo" Type="String" />
                      <asp:SessionParameter SessionField="crossref_apcode_txt" Name="prmVendorCode" Type="String" />
                      <asp:Parameter Name="prmCustName" Type="String" />
                      <asp:Parameter Name="prmComp" Type="String" />
                      <asp:Parameter Name="prmUpdateCustno" Type="String" />
                  </SelectParameters>
              </asp:ObjectDataSource>           
           
          </TD>
        </TR>
      </TABLE>      
      
      
    </div>
    <ft:footer id="Footer1" runat="server"></ft:footer>
    </form>
  </body>
</html>

