<%@ Page Language="c#" AutoEventWireup="true" Debug="true" Inherits="warehouse_history" Codebehind="warehouse_history.aspx.cs" %>
<%@ Register Src="footer.ascx" TagName="Footer" TagPrefix="ft" %>
<%@ Register Src="header.ascx" TagName="Header" TagPrefix="hd" %>
<html xmlns="http://www.w3.org/1999/xhtml" >
  <head id="Head1" runat="server">
    <title>Warehouse History</title>
    
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

        var da = document.getElementById("txt_seq");
        da.focus();

    }
   
    </script> 
  </head>    
   <body>
    <form id="frmList" runat="server"   defaultfocus ='txt_seq' >   
        <hd:header id="Header1" runat="server"></hd:header>
      <div>
            
      <table id="tblTop" cellSpacing="3" border="0">
        <tr>
          
          <td align=center nowrap><font size=+0><b>Warehouse History&nbsp;</b></font></td>
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
    <asp:LinkButton ID="lnk_listitem" runat="server" OnClick="lnk_browse_hist_click">Browse History</asp:LinkButton></li>
    <li><asp:LinkButton ID="lnk_viewitem" runat="server" OnClick="lnk_view_hist_click">View History</asp:LinkButton></li></ul></div>
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
                    <b>Seq#</b> <br />
                    <asp:TextBox ID="txt_seq" Width="70px" runat="server"></asp:TextBox>
                    <asp:CompareValidator ID="CompareValidator1" runat="server" ErrorMessage="Only integer" ControlToValidate="txt_seq" Display="dynamic" SetFocusOnError="true" Operator="datatypecheck" Type="integer"></asp:CompareValidator>
                </td>            
                <td class="shade" align="center" nowrap >
                    <b>Trans Date</b><br />
                    <asp:textbox id="txt_transdate" runat="server" Width="90px"></asp:textbox>                    
                </td>            
                <td class="shade" align="center" nowrap >
                    <b>Cust PO#</b> <br />
                    <asp:TextBox ID="txt_custpo" Width="100px" MaxLength="15" runat="server" ></asp:TextBox>
                </td>                     
                <td class="shade" align="center" nowrap >
                    <b>Cust Part#</b> <br />
                    <asp:TextBox ID="txt_custpart" Width="100px" MaxLength="15" runat="server" ></asp:TextBox>
                </td>                     
                <td class="shade" align="center" nowrap >
                    <b>FG Item#</b> <br />
                    <asp:TextBox ID="txt_fgitem" Width="100px" MaxLength="15" runat="server" ></asp:TextBox>
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
            <HeaderStyle   ForeColor="White" CssClass="headcolor" Height="40px"  VerticalAlign="Middle"  HorizontalAlign="Center" ></HeaderStyle>
        <RowStyle CssClass="shade"  />
         <Columns>
             <asp:CommandField ShowSelectButton="True" ButtonType="Image" SelectImageUrl="~/Images/sel.gif" SelectText="" > 
                <ItemStyle Width="10px" />
             </asp:CommandField>
             <asp:BoundField DataField="vRNo" HeaderText="Seq#" SortExpression="vRNo" />
             <asp:BoundField DataField="vTransType" HeaderText="Trans Type" SortExpression="vTransType" />
             <asp:BoundField DataField="vTransDate" HeaderText="Trans Date" SortExpression="vTransDate" />
             <asp:BoundField DataField="vTransQty" HeaderText="Trans Quantity" SortExpression="vTransQty" />
             <asp:BoundField DataField="vItemPoNo" HeaderText="Customers PO#" SortExpression="vItemPoNo" />
             <asp:BoundField DataField="vItemLineNo" HeaderText="Line#" SortExpression="vItemLineNo" />
             <asp:BoundField DataField="vCustPartNo" HeaderText="Customers Part#" SortExpression="vCustPartNo" />
             <asp:BoundField DataField="vFgItemNo" HeaderText="Suppliers FG Item" SortExpression="vFgItemNo" />
             <asp:BoundField DataField="vVendorCode" HeaderText="Custmers A/P Code" SortExpression="vVendorCode" />
             <asp:BoundField DataField="vVendorPlantCode" HeaderText="Customers Plant ID" SortExpression="vVendorPlantCode" />
             <asp:BoundField DataField="vVendorDeptCode" HeaderText="Customers Dept Code" SortExpression="vVendorDeptCode" />
             <asp:BoundField DataField="vVendOrdNo" HeaderText="Suppliers Order#" SortExpression="vVendOrdNo" />
             <asp:BoundField DataField="vVendJobNo" HeaderText="Suppliers Job#" SortExpression="vVendJobNo" />
             <asp:BoundField DataField="vVendJobNo2" HeaderText="" SortExpression="vVendJobNo2" />
             <asp:BoundField DataField="vSellPrice" HeaderText="Suppliers Item Sell Price" SortExpression="vSellPrice" />
             <asp:BoundField DataField="vPlantTotOhQty" HeaderText="Cust. Plant On Hand Qty" SortExpression="vPlantTotOhQty" />
              <asp:TemplateField HeaderText="reckey"  Visible="false">
                <ItemTemplate>
                <asp:Label ID="reckeyLabel" runat="server"  Text='<%# Bind("vRec_key") %>'></asp:Label>
                </ItemTemplate>
                </asp:TemplateField>
         </Columns>
     </asp:GridView>
              <asp:ObjectDataSource ID="ObjectDataSource1" runat="server" OldValuesParameterFormatString="original_{0}"
                  SelectMethod="Selectwarehousehist" TypeName="custitem">
                  <SelectParameters>
                      <asp:Parameter Name="prmUser" Type="String" />
                      <asp:Parameter Name="prmAction" Type="String" DefaultValue="Select" />
                      <asp:SessionParameter SessionField="warehouse_hist_seq_txt" Name="prmSeqNo" Type="Int32" />
                      <asp:SessionParameter SessionField="warehouse_hist_transdate_txt" Name="prmTransDate" Type="String" />
                      <asp:SessionParameter SessionField="warehouse_hist_custpo_txt" Name="prmCustomersPoNo" Type="String" />
                      <asp:SessionParameter SessionField="warehouse_hist_custpart_txt" Name="prmCustPart" Type="String" />
                      <asp:SessionParameter SessionField="warehouse_hist_fgitem_txt" Name="prmFgItem" Type="String" />
                      <asp:Parameter Name="prmComp" Type="String" />
                         <asp:Parameter Name="prmReckey" Type="String" />
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

