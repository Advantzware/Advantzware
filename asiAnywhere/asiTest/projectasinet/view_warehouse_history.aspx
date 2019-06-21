<%@ Page Language="C#" AutoEventWireup="true" Debug="true" Inherits="view_warehouse_history" Codebehind="view_warehouse_history.aspx.cs" %>
<%@ Register Src="footer.ascx" TagName="Footer" TagPrefix="ft" %>
<%@ Register Src="header.ascx" TagName="Header" TagPrefix="hd" %>
<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">

<html xmlns="http://www.w3.org/1999/xhtml" >
<head id="Head1" runat="server">
    <title>View Warehouse History</title>
    <LINK href="include/style2.css" type="text/css" rel="stylesheet"/>
     <link href="include/tree.css" rel="stylesheet" type="text/css" />
    <link href="include/tree.css" rel="stylesheet" type="text/css" />
    <link href="include/tree.css" rel="stylesheet" type="text/css" />
    <link href="include/tree.css" rel="stylesheet" type="text/css" />
    <link href="include/tree.css" rel="stylesheet" type="text/css" />
    <link href="include/tree.css" rel="stylesheet" type="text/css" />
    <link href="include/tree.css" rel="stylesheet" type="text/css" />
    <LINK REL="stylesheet" TYPE="text/css" HREF="include/CalendarControl.css" >
    <script language="javascript" src="include/date.js"></script>
    <script language="javascript" src="include/event.js"></script>
    <script language="javascript" src="include/insert.js"></script>
    <script language = "JavaScript" src="include/CalendarControl.js">    
    </script>
    
    <script>
     function contactcustomerlook(){ 
  var NewWindow = window.open("contact_customer_lookup.aspx","ContactCustomerLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}

function ContactCustomerLookup(ReturnObj1,ReturnObj2,ReturnObj3,ReturnObj4,ReturnObj5,ReturnObj6,ReturnObj7,ReturnObj8,ReturnObj9, ReturnObj10,ReturnObj11){ 
  document.forms[0].FormView1_vCustNoTextBox.value = ReturnObj1;
  
}
    
    </script>
    </head>
<body>
    <form id="form1" runat="server">
    <hd:header id="Header1" runat="server"></hd:header>
    <div>
        <asp:HiddenField ID="HiddenFGitem" runat="server" />
        <asp:HiddenField ID="HiddenVanderCode" runat="server" />
        <asp:HiddenField ID="HiddenDeptCode" runat="server" />
        <asp:HiddenField ID="HiddenLine" runat="server" />
        <asp:HiddenField ID="HiddenField5" runat="server" />
    <TABLE id="tblTop" cellSpacing="3" align="center" border="0" Width="100%">
        <TR>
          <TD width=30>&nbsp;</TD>
          <TD align=center nowrap><font size=+0><b>View Warehouse History&nbsp;<asp:Label ID="lbl_page" runat="server"></asp:Label></b></font></TD>
          <TD vAlign="middle">
            <asp:linkbutton id="LinkButton1" runat="server" OnClick="LinkButton1_Click">Back to menu</asp:linkbutton>
          </TD>
          <TD align="right"><font size=+0><b>&nbsp;&nbsp;</b></font></TD>
          <TD vAlign="middle" align="left">Logged as&nbsp;
            <asp:label id="lblUser" runat="server" Font-Bold="True">&nbsp;</asp:label>&nbsp;&nbsp;&nbsp;
            <asp:linkbutton id="hlnkLogOut" runat="server" OnClick="hlnkLogOut_Click">Log out</asp:linkbutton>
            
            &nbsp;&nbsp;&nbsp;<asp:hyperlink id="hlnkChangePwd" runat="server" NavigateUrl="changepwd.aspx">Change password</asp:hyperlink>
            &nbsp;&nbsp;&nbsp;
            <b>Company:</b>&nbsp;
            <asp:label id="lblComp" runat="server" Font-Bold="True">&nbsp;</asp:label>&nbsp;&nbsp;&nbsp;
          </TD>
          
                    
          <TD vAlign="middle" width="20">&nbsp;</TD>
          <td width=30>&nbsp;</td>
        </TR>
      </TABLE>  
  </div>
    
    <div>
    
    <table>
    <tr style="background-color:Gray">
    <td><div  id="navigation" style="width:100%">
		<ul nowrap><li  >
    <asp:LinkButton ID="lnk_listitem" runat="server" OnClick="lnk_browse_hist_click">Browse History</asp:LinkButton></li>
    <li class="selected"><asp:LinkButton ID="lnk_viewitem" runat="server" OnClick="lnk_view_hist_click">View History</asp:LinkButton></li></ul></div>
    </td>
    </tr></table>
    <br />
        <asp:Button ID="addnewbutton" CssClass = "button" OnClick="newbutton_click" runat="server" Text="Add" />
    <asp:FormView ID="FormView1" runat="server" DataSourceID="ObjectDataSource1" OnDataBound="FormView1_DataBound" OnPreRender="FormView1_PreRender">                        
            <ItemTemplate>
                <table class="shade" width="400px">
                <tr><td align="right" style="padding-right:5px" nowrap><b>Seq. #:</b></td> 
                <td><asp:Label ID="vRNoLabel" runat="server" Width="120px" BackColor="turquoise" BorderColor="Black" BorderStyle="solid" BorderWidth="1px" Text='<%# Bind("vRNo") %>'></asp:Label></td>
                <td align="right" style="padding-right:5px" nowrap><b>Transaction Type:</b></td> 
                <td><asp:Label ID="vTransTypeLabel" runat="server" Width="120px" BackColor="turquoise" BorderColor="Black" BorderStyle="solid" BorderWidth="1px" Text='<%# Bind("vTransType") %>'></asp:Label></td></tr>
                <tr><td align="right" style="padding-right:5px" nowrap><b>Transaction Date:</b></td> 
                <td><asp:Label ID="vTransDateLabel" runat="server" Width="120px" BackColor="turquoise" BorderColor="Black" BorderStyle="solid" BorderWidth="1px" Text='<%# Bind("vTransDate") %>'></asp:Label></td>
                <td align="right" style="padding-right:5px" nowrap><b>Transaction Quantity:</b></td> 
                <td><asp:Label ID="vTransQtyLabel" runat="server" Width="120px" BackColor="turquoise" BorderColor="Black" BorderStyle="solid" BorderWidth="1px" Text='<%# Bind("vTransQty") %>'></asp:Label></td></tr>
                <tr><td align="right" style="padding-right:5px" nowrap><b>Customers PO#:</b></td> 
                <td><asp:Label ID="vItemPoNoLabel" runat="server" Width="120px" BackColor="turquoise" BorderColor="Black" BorderStyle="solid" BorderWidth="1px" Text='<%# Bind("vItemPoNo") %>'></asp:Label></td>
                <td align="right" style="padding-right:5px" nowrap><b>Customers PO Line#:</b></td> 
                <td><asp:Label ID="vItemLineNoLabel" runat="server" Width="120px" BackColor="turquoise" BorderColor="Black" BorderStyle="solid" BorderWidth="1px" Text='<%# Bind("vItemLineNo") %>'></asp:Label></td></tr>
                <tr><td align="right" style="padding-right:5px" nowrap><b>Customers Part#:</b></td> 
                <td><asp:Label ID="vCustPartNoLabel" runat="server" Width="120px" BackColor="turquoise" BorderColor="Black" BorderStyle="solid" BorderWidth="1px" Text='<%# Bind("vCustPartNo") %>'></asp:Label></td>
                <td align="right" style="padding-right:5px" nowrap><b>Suppliers FG Item:</b></td> 
                <td><asp:Label ID="vFgItemNoLabel" runat="server" Width="120px" BackColor="turquoise" BorderColor="Black" BorderStyle="solid" BorderWidth="1px" Text='<%# Bind("vFgItemNo") %>'></asp:Label></td></tr>
                <tr><td align="right" style="padding-right:5px" nowrap><b>Customers A/P Code:</b></td> 
                <td><asp:Label ID="vVendorCodeLabel" runat="server" Width="120px" BackColor="turquoise" BorderColor="Black" BorderStyle="solid" BorderWidth="1px" Text='<%# Bind("vVendorCode") %>'></asp:Label></td>
                <td align="right" style="padding-right:5px" nowrap><b>Customers Plant ID:</b></td> 
                <td><asp:Label ID="vVendorPlantCodeLabel" runat="server" Width="120px" BackColor="turquoise" BorderColor="Black" BorderStyle="solid" BorderWidth="1px" Text='<%# Bind("vVendorPlantCode") %>'></asp:Label></td></tr>
                <tr><td align="right" style="padding-right:5px" nowrap><b>Customers Dept Code:</b></td>
                <td><asp:Label ID="vVendorDeptCodeLabel" runat="server" Width="120px" BackColor="turquoise" BorderColor="Black" BorderStyle="solid" BorderWidth="1px" Text='<%# Bind("vVendorDeptCode") %>'></asp:Label></td>
                <td align="right" style="padding-right:5px" nowrap><b>Suppliers Order:</b></td>
                <td><asp:Label ID="vVendOrdNoLabel" runat="server" Width="120px" BackColor="turquoise" BorderColor="Black" BorderStyle="solid" BorderWidth="1px" Text='<%# Bind("vVendOrdNo") %>'></asp:Label></td></tr>
                <tr><td align="right" style="padding-right:5px" nowrap><b>Suppliers Job#:</b></td>
                <td><asp:Label ID="vVendJobNoLabel" runat="server" Width="120px" BackColor="turquoise" BorderColor="Black" BorderStyle="solid" BorderWidth="1px" Text='<%# Bind("vVendJobNo") %>'></asp:Label></td>
                <%--vVendJobNo2:
                <asp:Label ID="vVendJobNo2Label" runat="server" Text='<%# Bind("vVendJobNo2") %>'>
                </asp:Label><br />--%>                
                <td align="right" style="padding-right:5px" nowrap><b>Suppliers Item Sell Price:</b></td>
                <td><asp:Label ID="vSellPriceLabel" runat="server" Width="120px" BackColor="turquoise" BorderColor="Black" BorderStyle="solid" BorderWidth="1px" Text='<%# Bind("vSellPrice") %>'></asp:Label></td></tr>
                <tr><td align="right" style="padding-right:5px" nowrap><b>Cust. Plant On Hand Qty:</b></td>
                <td><asp:Label ID="vPlantTotOhQtyLabel" runat="server" Width="120px" BackColor="turquoise" BorderColor="Black" BorderStyle="solid" BorderWidth="1px" Text='<%# Bind("vPlantTotOhQty") %>'></asp:Label></td>
                <td align="right" style="padding-right:5px" nowrap><b>Created On:</b></td>
                <td><asp:Label ID="Label1" runat="server" Width="120px" BackColor="turquoise" BorderColor="Black" BorderStyle="solid" BorderWidth="1px" Text='<%# Bind("vCreateDate","{0:MM/dd/yyyy}") %>'></asp:Label></td></tr>
                <tr><td align="right" style="padding-right:5px" nowrap><b>Created By:</b></td>
                <td><asp:Label ID="Label2" runat="server" Width="120px" BackColor="turquoise" BorderColor="Black" BorderStyle="solid" BorderWidth="1px" Text='<%# Bind("vCreatedBy") %>'></asp:Label></td></tr>
            </ItemTemplate>
        </asp:FormView>
        <asp:ObjectDataSource ID="ObjectDataSource1" runat="server" OldValuesParameterFormatString="original_{0}"
            SelectMethod="Selectwarehousehist" TypeName="custitem">
            <SelectParameters>
                <asp:Parameter Name="prmUser" Type="String" />
                <asp:Parameter Name="prmAction" Type="String" DefaultValue="View" />
                <asp:SessionParameter Name="prmSeqNo" SessionField="warehousehist_seq_txt_view" Type="Int32" />
                <asp:Parameter Name="prmTransDate" Type="String" />
                <asp:Parameter Name="prmCustomersPoNo" Type="String" />
                <asp:Parameter Name="prmCustPart" Type="String" />
                <asp:Parameter Name="prmFgItem" Type="String" />
                <asp:Parameter Name="prmComp" Type="String" />
                  <asp:SessionParameter SessionField="warehousehist_seq_txt_view_reckey" Name="prmReckey" Type="String" />
            </SelectParameters>
        </asp:ObjectDataSource>
        
        <br />
        </div>
    <ft:footer id="Footer1" runat="server"></ft:footer>
    </form>
</body>
</html>