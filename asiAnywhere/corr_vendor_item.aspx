<%@ Page Language="C#" AutoEventWireup="true" Debug="true" Inherits="corr_vendor_item" Title="Vendor Cost" Codebehind="corr_vendor_item.aspx.cs" %>

<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">

<html xmlns="http://www.w3.org/1999/xhtml" >
<head id="Head1" runat="server">
    <title>Add New Vendor Cost</title>
    <LINK href="include/style.css" type="text/css" rel="stylesheet"/>
<link rel="stylesheet" href="include/dhtmlwindow.css" type="text/css" />
 <LINK REL="stylesheet" TYPE="text/css" HREF="include/CalendarControl.css" >
    
</head>
<script language = "JavaScript" src="include/CalendarControl.js"></script>
<script type="text/javascript" src="include/dhtmlwindow.js"></script>
 <script language="javascript" src="include/date.js"></script>
    <script language="javascript" src="include/event.js"></script>
    <script language="javascript" src="include/insert.js"></script>
<script type="text/javascript">
    
        function addcostclick() {
        var NewWindow = window.open("corr_ven_item.aspx", "AddVendorCostWindow", "width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
    }
    function uomlook() {
        var itemtext = document.getElementById("FormView1_vITEMTextBox").innerHTML;
        var NewWindow = window.open("coruom_lookup.aspx?item=" + itemtext + "", "UomLookup", "width=500,height=400,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
    }
    function UomLookup(ReturnObj1, ReturnObj2) {
        document.forms[0].FormView1_stdumTextBox.value = ReturnObj1;

    }
     function vendorlook() {
      
        var NewWindow = window.open("corvend_lookup.aspx", "UomLookup", "width=500,height=400,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
    }
    function VendLookup(ReturnObj1, ReturnObj2) {
        document.forms[0].FormView1_vend_noTextBox.value = ReturnObj1;
        document.getElementById("FormView1_vendnameLabel").innerHTML = ReturnObj2;

    }
       
</script>
<body>
    <form id="form1" runat="server">
   
       
    <div >
       
        <asp:FormView ID="FormView1" CssClass="shade" runat="server" OnDataBound="FormView1_DataBound" DataSourceID="ObjectDataSource1">
            <EditItemTemplate>
            <fieldset>
            <table class="shade"><tr><td align="right" style="padding-right:5px"><b>Item No:</b></td>
            <td><asp:Label ID="vITEMTextBox" Width="100px" BackColor="Turquoise" runat="server" Text='<%# Bind("vITEM") %>' /></td></tr>
            <tr><td align="right" style="padding-right:5px"><b>Purchased cost UOM:</b></td>
            <td><asp:TextBox ID="stdumTextBox" Width="100px" runat="server" Text='<%# Bind("stdum") %>' />
            <a href="#" onclick="uomlook(); return false"><asp:Image ID="Image2" runat="server" ImageUrl="images/lookup_icon.gif" /></a></td>            
            <td align="right" style="padding-right:5px"><b>Vendor Item No:</b></td>
            <td> <asp:TextBox ID="vend_itemTextBox" Width="80px" runat="server" Text='<%# Bind("[vend-item]") %>' /></td></tr>
            <tr><td align="right" style="padding-right:5px"><b>Vendor:</b></td>
            <td> <asp:TextBox ID="vend_noTextBox" Width="100px" runat="server" Text='<%# Bind("[vend-no]") %>' />
            <a href="#" onclick="vendorlook(); return false"><asp:Image ID="Image1" runat="server" ImageUrl="images/lookup_icon.gif" /></a></td>
            <td colspan="2">&nbsp;&nbsp;&nbsp;&nbsp;<asp:Label ID="vendnameLabel" BackColor="Turquoise" runat="server" Text='<%# Bind("[vend-name]") %>'></asp:Label></td></tr>
            <tr><td align="right" style="padding-right:5px"><b>Cost Update:</b></td>
            <td><asp:TextBox ID="updated_dateTextBox" Width="100px" onfocus="javascript:preEnter( this, 'yes' );" onblur="javascript:preLeave( this, 'date', '99/99/9999' );" runat="server" Text='<%# Bind("[updated-date]") %>' />
            <a href="#" onclick="showCalendarControl(FormView1_updated_dateTextBox); return false"><asp:Image ID="datelook" runat="server" ImageUrl="images/lookup_icon.gif" /> </a></td></tr>
            </table>
            <table class="shade">
            <tr><td>
            <table><tr><td><b>Qty to</b></td>
            <td><b>Cost Per</b></td>
            <td><b>Set</b></td></tr>
            <tr><td nowrap> <asp:TextBox ID="run_qty1TextBox" runat="server" Width="50px" Text='<%# Bind("[run-qty1]") %>' />
            <asp:CompareValidator ID="CompareValidator1" ControlToValidate="run_qty1TextBox" Display="Dynamic" SetFocusOnError="true" Type="Double" Operator="DataTypeCheck" runat="server" ErrorMessage="Invalid Entry"></asp:CompareValidator></td>
            <td nowrap> <asp:TextBox ID="run_cost1TextBox" runat="server" Width="50px" Text='<%# Bind("[run-cost1]") %>' />
            <asp:CompareValidator ID="CompareValidator2" ControlToValidate="run_cost1TextBox" Display="Dynamic" SetFocusOnError="true" Type="Double" Operator="DataTypeCheck" runat="server" ErrorMessage="Invalid Entry"></asp:CompareValidator></td>
            <td nowrap><asp:TextBox ID="setups1TextBox" runat="server" Width="50px" Text='<%# Bind("setups1") %>' />
            <asp:CompareValidator ID="CompareValidator3" ControlToValidate="setups1TextBox" Display="Dynamic" SetFocusOnError="true" Type="Double" Operator="DataTypeCheck" runat="server" ErrorMessage="Invalid Entry"></asp:CompareValidator></td></tr>
           
            <tr><td nowrap><asp:TextBox ID="run_qty2TextBox" runat="server" Width="50px" Text='<%# Bind("[run-qty2]") %>' />
            <asp:CompareValidator ID="CompareValidator4" ControlToValidate="run_qty2TextBox" Display="Dynamic" SetFocusOnError="true" Type="Double" Operator="DataTypeCheck" runat="server" ErrorMessage="Invalid Entry"></asp:CompareValidator></td>
            <td nowrap><asp:TextBox ID="run_cost2TextBox" runat="server" Width="50px"  Text='<%# Bind("[run-cost2]") %>' />
            <asp:CompareValidator ID="CompareValidator5" ControlToValidate="run_cost2TextBox" Display="Dynamic" SetFocusOnError="true" Type="Double" Operator="DataTypeCheck" runat="server" ErrorMessage="Invalid Entry"></asp:CompareValidator></td>
            <td nowrap><asp:TextBox ID="setups2TextBox" runat="server" Width="50px" Text='<%# Bind("setups2") %>' />
            <asp:CompareValidator ID="CompareValidator6" ControlToValidate="setups2TextBox" Display="Dynamic" SetFocusOnError="true" Type="Double" Operator="DataTypeCheck" runat="server" ErrorMessage="Invalid Entry"></asp:CompareValidator></td></tr>
            
            <tr><td nowrap><asp:TextBox ID="run_qty3TextBox" runat="server" Width="50px" Text='<%# Bind("[run-qty3]") %>' />
            <asp:CompareValidator ID="CompareValidator7" ControlToValidate="run_qty3TextBox" Display="Dynamic" SetFocusOnError="true" Type="Double" Operator="DataTypeCheck" runat="server" ErrorMessage="Invalid Entry"></asp:CompareValidator></td>
            <td nowrap><asp:TextBox ID="run_cost3TextBox" runat="server" Width="50px" Text='<%# Bind("[run-cost3]") %>' />
            <asp:CompareValidator ID="CompareValidator8" ControlToValidate="run_cost3TextBox" Display="Dynamic" SetFocusOnError="true" Type="Double" Operator="DataTypeCheck" runat="server" ErrorMessage="Invalid Entry"></asp:CompareValidator></td>
            <td nowrap><asp:TextBox ID="setups3TextBox" runat="server" Width="50px" Text='<%# Bind("setups3") %>' />
            <asp:CompareValidator ID="CompareValidator9" ControlToValidate="setups3TextBox" Display="Dynamic" SetFocusOnError="true" Type="Double" Operator="DataTypeCheck" runat="server" ErrorMessage="Invalid Entry"></asp:CompareValidator></td></tr>
            
            <tr><td nowrap><asp:TextBox ID="run_qty4TextBox" runat="server" Width="50px" Text='<%# Bind("[run-qty4]") %>' />
            <asp:CompareValidator ID="CompareValidator10" ControlToValidate="run_qty4TextBox" Display="Dynamic" SetFocusOnError="true" Type="Double" Operator="DataTypeCheck" runat="server" ErrorMessage="Invalid Entry"></asp:CompareValidator></td>
            <td nowrap><asp:TextBox ID="run_cost4TextBox" runat="server" Width="50px" Text='<%# Bind("[run-cost4]") %>' />
            <asp:CompareValidator ID="CompareValidator11" ControlToValidate="run_cost4TextBox" Display="Dynamic" SetFocusOnError="true" Type="Double" Operator="DataTypeCheck" runat="server" ErrorMessage="Invalid Entry"></asp:CompareValidator></td>
            <td nowrap><asp:TextBox ID="setups4TextBox" runat="server" Width="50px" Text='<%# Bind("setups4") %>' />
            <asp:CompareValidator ID="CompareValidator12" ControlToValidate="setups4TextBox" Display="Dynamic" SetFocusOnError="true" Type="Double" Operator="DataTypeCheck" runat="server" ErrorMessage="Invalid Entry"></asp:CompareValidator></td></tr>
            
            <tr><td nowrap><asp:TextBox ID="run_qty5TextBox" runat="server" Width="50px" Text='<%# Bind("[run-qty5]") %>' />
            <asp:CompareValidator ID="CompareValidator13" ControlToValidate="run_qty5TextBox" Display="Dynamic" SetFocusOnError="true" Type="Double" Operator="DataTypeCheck" runat="server" ErrorMessage="Invalid Entry"></asp:CompareValidator></td>
            <td nowrap> <asp:TextBox ID="run_cost5TextBox" runat="server" Width="50px" Text='<%# Bind("[run-cost5]") %>' />
            <asp:CompareValidator ID="CompareValidator14" ControlToValidate="run_cost5TextBox" Display="Dynamic" SetFocusOnError="true" Type="Double" Operator="DataTypeCheck" runat="server" ErrorMessage="Invalid Entry"></asp:CompareValidator></td>
            <td nowrap><asp:TextBox ID="setups5TextBox" runat="server" Width="50px" Text='<%# Bind("setups5") %>' />
            <asp:CompareValidator ID="CompareValidator15" ControlToValidate="setups5TextBox" Display="Dynamic" SetFocusOnError="true" Type="Double" Operator="DataTypeCheck" runat="server" ErrorMessage="Invalid Entry"></asp:CompareValidator></td></tr>
            
            <tr><td nowrap> <asp:TextBox ID="run_qty6TextBox" runat="server" Width="50px" Text='<%# Bind("[run-qty6]") %>' />
            <asp:CompareValidator ID="CompareValidator16" ControlToValidate="run_qty6TextBox" Display="Dynamic" SetFocusOnError="true" Type="Double" Operator="DataTypeCheck" runat="server" ErrorMessage="Invalid Entry"></asp:CompareValidator></td>
            <td nowrap><asp:TextBox ID="run_cost6TextBox" runat="server" Width="50px" Text='<%# Bind("[run-cost6]") %>' />
            <asp:CompareValidator ID="CompareValidator17" ControlToValidate="run_cost6TextBox" Display="Dynamic" SetFocusOnError="true" Type="Double" Operator="DataTypeCheck" runat="server" ErrorMessage="Invalid Entry"></asp:CompareValidator></td>
            <td nowrap><asp:TextBox ID="setups6TextBox" runat="server" Width="50px" Text='<%# Bind("setups6") %>' />
            <asp:CompareValidator ID="CompareValidator18" ControlToValidate="setups6TextBox" Display="Dynamic" SetFocusOnError="true" Type="Double" Operator="DataTypeCheck" runat="server" ErrorMessage="Invalid Entry"></asp:CompareValidator></td></tr>
            
            <tr><td nowrap><asp:TextBox ID="run_qty7TextBox" runat="server" Width="50px" Text='<%# Bind("[run-qty7]") %>' />
            <asp:CompareValidator ID="CompareValidator19" ControlToValidate="run_qty7TextBox" Display="Dynamic" SetFocusOnError="true" Type="Double" Operator="DataTypeCheck" runat="server" ErrorMessage="Invalid Entry"></asp:CompareValidator></td>
            <td nowrap><asp:TextBox ID="run_cost7TextBox" runat="server" Width="50px" Text='<%# Bind("[run-cost7]") %>' />
            <asp:CompareValidator ID="CompareValidator20" ControlToValidate="run_cost7TextBox" Display="Dynamic" SetFocusOnError="true" Type="Double" Operator="DataTypeCheck" runat="server" ErrorMessage="Invalid Entry"></asp:CompareValidator></td>
            <td nowrap><asp:TextBox ID="setups7TextBox" runat="server" Width="50px" Text='<%# Bind("setups7") %>' />
            <asp:CompareValidator ID="CompareValidator21" ControlToValidate="setups7TextBox" Display="Dynamic" SetFocusOnError="true" Type="Double" Operator="DataTypeCheck" runat="server" ErrorMessage="Invalid Entry"></asp:CompareValidator></td></tr>
            
            <tr><td nowrap><asp:TextBox ID="run_qty8TextBox" runat="server" Width="50px" Text='<%# Bind("[run-qty8]") %>' />
            <asp:CompareValidator ID="CompareValidator22" ControlToValidate="run_qty8TextBox" Display="Dynamic" SetFocusOnError="true" Type="Double" Operator="DataTypeCheck" runat="server" ErrorMessage="Invalid Entry"></asp:CompareValidator></td>
            <td nowrap><asp:TextBox ID="run_cost8TextBox" runat="server" Width="50px" Text='<%# Bind("[run-cost8]") %>' />
            <asp:CompareValidator ID="CompareValidator23" ControlToValidate="run_cost8TextBox" Display="Dynamic" SetFocusOnError="true" Type="Double" Operator="DataTypeCheck" runat="server" ErrorMessage="Invalid Entry"></asp:CompareValidator></td>
            <td nowrap><asp:TextBox ID="setups8TextBox" runat="server" Width="50px" Text='<%# Bind("setups8") %>' />
            <asp:CompareValidator ID="CompareValidator24" ControlToValidate="setups8TextBox" Display="Dynamic" SetFocusOnError="true" Type="Double" Operator="DataTypeCheck" runat="server" ErrorMessage="Invalid Entry"></asp:CompareValidator></td></tr>
            
            <tr><td nowrap><asp:TextBox ID="run_qty9TextBox" runat="server" Width="50px"  Text='<%# Bind("[run-qty9]") %>' />
            <asp:CompareValidator ID="CompareValidator25" ControlToValidate="run_qty9TextBox" Display="Dynamic" SetFocusOnError="true" Type="Double" Operator="DataTypeCheck" runat="server" ErrorMessage="Invalid Entry"></asp:CompareValidator></td>
            <td nowrap><asp:TextBox ID="run_cost9TextBox" runat="server" Width="50px" Text='<%# Bind("[run-cost9]") %>' />
            <asp:CompareValidator ID="CompareValidator26" ControlToValidate="run_cost9TextBox" Display="Dynamic" SetFocusOnError="true" Type="Double" Operator="DataTypeCheck" runat="server" ErrorMessage="Invalid Entry"></asp:CompareValidator></td>
            <td nowrap> <asp:TextBox ID="setups9TextBox" runat="server" Width="50px" Text='<%# Bind("setups9") %>' />
            <asp:CompareValidator ID="CompareValidator27" ControlToValidate="setups9TextBox" Display="Dynamic" SetFocusOnError="true" Type="Double" Operator="DataTypeCheck" runat="server" ErrorMessage="Invalid Entry"></asp:CompareValidator></td></tr>
            
            <tr><td nowrap><asp:TextBox ID="run_qty10TextBox" runat="server" Width="50px" Text='<%# Bind("[run-qty10]") %>' />
            <asp:CompareValidator ID="CompareValidator28" ControlToValidate="run_qty10TextBox" Display="Dynamic" SetFocusOnError="true" Type="Double" Operator="DataTypeCheck" runat="server" ErrorMessage="Invalid Entry"></asp:CompareValidator></td>
            <td nowrap><asp:TextBox ID="run_cost10TextBox" runat="server" Width="50px" Text='<%# Bind("[run-cost10]") %>' />
            <asp:CompareValidator ID="CompareValidator29" ControlToValidate="run_cost10TextBox" Display="Dynamic" SetFocusOnError="true" Type="Double" Operator="DataTypeCheck" runat="server" ErrorMessage="Invalid Entry"></asp:CompareValidator></td>
            <td nowrap><asp:TextBox ID="setups10TextBox" runat="server" Width="50px" Text='<%# Bind("setups10") %>' />
            <asp:CompareValidator ID="CompareValidator30" ControlToValidate="setups10TextBox" Display="Dynamic" SetFocusOnError="true" Type="Double" Operator="DataTypeCheck" runat="server" ErrorMessage="Invalid Entry"></asp:CompareValidator></td></tr>
            </table>
            </td>            
            <td><fieldset><legend></legend>
            <table><tr><td colspan="3"><b>Valid Estimate Roll Width</b></td></tr>
            <tr><td nowrap><asp:TextBox ID="roll_w1TextBox" runat="server" Width="50px" Text='<%# Bind("[roll-w1]") %>' />
            <asp:CompareValidator ID="CompareValidator31" ControlToValidate="roll_w1TextBox" Display="Dynamic" SetFocusOnError="true" Type="Double" Operator="DataTypeCheck" runat="server" ErrorMessage="Invalid Entry"></asp:CompareValidator></td>
            <td nowrap> <asp:TextBox ID="roll_w2TextBox" runat="server" Width="50px" Text='<%# Bind("[roll-w2]") %>' />
            <asp:CompareValidator ID="CompareValidator32" ControlToValidate="roll_w2TextBox" Display="Dynamic" SetFocusOnError="true" Type="Double" Operator="DataTypeCheck" runat="server" ErrorMessage="Invalid Entry"></asp:CompareValidator></td>
            <td nowrap><asp:TextBox ID="roll_w3TextBox" runat="server" Width="50px" Text='<%# Bind("[roll-w3]") %>' />
            <asp:CompareValidator ID="CompareValidator33" ControlToValidate="roll_w3TextBox" Display="Dynamic" SetFocusOnError="true" Type="Double" Operator="DataTypeCheck" runat="server" ErrorMessage="Invalid Entry"></asp:CompareValidator></td></tr>
            <tr><td nowrap><asp:TextBox ID="roll_w4TextBox" runat="server" Width="50px" Text='<%# Bind("[roll-w4]") %>' />
            <asp:CompareValidator ID="CompareValidator34" ControlToValidate="roll_w4TextBox" Display="Dynamic" SetFocusOnError="true" Type="Double" Operator="DataTypeCheck" runat="server" ErrorMessage="Invalid Entry"></asp:CompareValidator></td>
            <td nowrap><asp:TextBox ID="roll_w5TextBox" runat="server" Width="50px" Text='<%# Bind("[roll-w5]") %>' />
            <asp:CompareValidator ID="CompareValidator35" ControlToValidate="roll_w5TextBox" Display="Dynamic" SetFocusOnError="true" Type="Double" Operator="DataTypeCheck" runat="server" ErrorMessage="Invalid Entry"></asp:CompareValidator></td>
            <td nowrap><asp:TextBox ID="roll_w6TextBox" runat="server" Width="50px" Text='<%# Bind("[roll-w6]") %>' />
            <asp:CompareValidator ID="CompareValidator36" ControlToValidate="roll_w6TextBox" Display="Dynamic" SetFocusOnError="true" Type="Double" Operator="DataTypeCheck" runat="server" ErrorMessage="Invalid Entry"></asp:CompareValidator></td></tr>
            <tr><td nowrap><asp:TextBox ID="roll_w7TextBox" runat="server" Width="50px" Text='<%# Bind("[roll-w7]") %>' />
            <asp:CompareValidator ID="CompareValidator37" ControlToValidate="roll_w7TextBox" Display="Dynamic" SetFocusOnError="true" Type="Double" Operator="DataTypeCheck" runat="server" ErrorMessage="Invalid Entry"></asp:CompareValidator></td>
            <td nowrap><asp:TextBox ID="roll_w8TextBox" runat="server" Width="50px" Text='<%# Bind("[roll-w8]") %>' />
            <asp:CompareValidator ID="CompareValidator38" ControlToValidate="roll_w8TextBox" Display="Dynamic" SetFocusOnError="true" Type="Double" Operator="DataTypeCheck" runat="server" ErrorMessage="Invalid Entry"></asp:CompareValidator></td>
            <td nowrap> <asp:TextBox ID="roll_w9TextBox" runat="server" Width="50px" Text='<%# Bind("[roll-w9]") %>' />
            <asp:CompareValidator ID="CompareValidator39" ControlToValidate="roll_w9TextBox" Display="Dynamic" SetFocusOnError="true" Type="Double" Operator="DataTypeCheck" runat="server" ErrorMessage="Invalid Entry"></asp:CompareValidator></td></tr>
            <tr><td nowrap><asp:TextBox ID="roll_w10TextBox" runat="server" Width="50px" Text='<%# Bind("[roll-w10]") %>' />
            <asp:CompareValidator ID="CompareValidator40" ControlToValidate="roll_w10TextBox" Display="Dynamic" SetFocusOnError="true" Type="Double" Operator="DataTypeCheck" runat="server" ErrorMessage="Invalid Entry"></asp:CompareValidator></td>
            <td nowrap><asp:TextBox ID="roll_w11TextBox" runat="server" Width="50px" Text='<%# Bind("[roll-w11]") %>' />
            <asp:CompareValidator ID="CompareValidator41" ControlToValidate="roll_w11TextBox" Display="Dynamic" SetFocusOnError="true" Type="Double" Operator="DataTypeCheck" runat="server" ErrorMessage="Invalid Entry"></asp:CompareValidator></td>
            <td nowrap><asp:TextBox ID="roll_w12TextBox" runat="server" Width="50px" Text='<%# Bind("[roll-w12]") %>' />
            <asp:CompareValidator ID="CompareValidator42" ControlToValidate="roll_w12TextBox" Display="Dynamic" SetFocusOnError="true" Type="Double" Operator="DataTypeCheck" runat="server" ErrorMessage="Invalid Entry"></asp:CompareValidator>
               </td></tr>
            <tr><td nowrap><asp:TextBox ID="roll_w13TextBox" runat="server" Width="50px" Text='<%# Bind("[roll-w13]") %>' />
            <asp:CompareValidator ID="CompareValidator43" ControlToValidate="roll_w13TextBox" Display="Dynamic" SetFocusOnError="true" Type="Double" Operator="DataTypeCheck" runat="server" ErrorMessage="Invalid Entry"></asp:CompareValidator></td>
            <td nowrap><asp:TextBox ID="roll_w14TextBox" runat="server" Width="50px" Text='<%# Bind("[roll-w14]") %>' />
            <asp:CompareValidator ID="CompareValidator44" ControlToValidate="roll_w14TextBox" Display="Dynamic" SetFocusOnError="true" Type="Double" Operator="DataTypeCheck" runat="server" ErrorMessage="Invalid Entry"></asp:CompareValidator></td>
            <td nowrap><asp:TextBox ID="roll_w15TextBox" runat="server" Width="50px"  Text='<%# Bind("[roll-w15]") %>' />
            <asp:CompareValidator ID="CompareValidator45" ControlToValidate="roll_w15TextBox" Display="Dynamic" SetFocusOnError="true" Type="Double" Operator="DataTypeCheck" runat="server" ErrorMessage="Invalid Entry"></asp:CompareValidator></td></tr>
            <tr><td nowrap> <asp:TextBox ID="roll_w16TextBox" runat="server" Width="50px" Text='<%# Bind("[roll-w16]") %>' />
            <asp:CompareValidator ID="CompareValidator46" ControlToValidate="roll_w16TextBox" Display="Dynamic" SetFocusOnError="true" Type="Double" Operator="DataTypeCheck" runat="server" ErrorMessage="Invalid Entry"></asp:CompareValidator></td>
            <td nowrap><asp:TextBox ID="roll_w17TextBox" runat="server" Width="50px" Text='<%# Bind("[roll-w17]") %>' />
            <asp:CompareValidator ID="CompareValidator47" ControlToValidate="roll_w17TextBox" Display="Dynamic" SetFocusOnError="true" Type="Double" Operator="DataTypeCheck" runat="server" ErrorMessage="Invalid Entry"></asp:CompareValidator></td>
            <td nowrap> <asp:TextBox ID="roll_w18TextBox" runat="server" Width="50px" Text='<%# Bind("[roll-w18]") %>' />
            <asp:CompareValidator ID="CompareValidator48" ControlToValidate="roll_w18TextBox" Display="Dynamic" SetFocusOnError="true" Type="Double" Operator="DataTypeCheck" runat="server" ErrorMessage="Invalid Entry"></asp:CompareValidator></td></tr>
            <tr><td nowrap><asp:TextBox ID="roll_w19TextBox" runat="server" Width="50px" Text='<%# Bind("[roll-w19]") %>' />
            <asp:CompareValidator ID="CompareValidator49" ControlToValidate="roll_w19TextBox" Display="Dynamic" SetFocusOnError="true" Type="Double" Operator="DataTypeCheck" runat="server" ErrorMessage="Invalid Entry"></asp:CompareValidator></td>
            <td nowrap><asp:TextBox ID="roll_w20TextBox" runat="server" Width="50px"  Text='<%# Bind("[roll-w20]") %>' />
            <asp:CompareValidator ID="CompareValidator50" ControlToValidate="roll_w20TextBox" Display="Dynamic" SetFocusOnError="true" Type="Double" Operator="DataTypeCheck" runat="server" ErrorMessage="Invalid Entry"></asp:CompareValidator></td>
            <td nowrap><asp:TextBox ID="roll_w21TextBox" runat="server" Width="50px" Text='<%# Bind("[roll-w21]") %>' />
            <asp:CompareValidator ID="CompareValidator51" ControlToValidate="roll_w21TextBox" Display="Dynamic" SetFocusOnError="true" Type="Double" Operator="DataTypeCheck" runat="server" ErrorMessage="Invalid Entry"></asp:CompareValidator></td></tr>
            <tr><td nowrap> <asp:TextBox ID="roll_w22TextBox" runat="server" Width="50px" Text='<%# Bind("[roll-w22]") %>' />
            <asp:CompareValidator ID="CompareValidator52" ControlToValidate="roll_w22TextBox" Display="Dynamic" SetFocusOnError="true" Type="Double" Operator="DataTypeCheck" runat="server" ErrorMessage="Invalid Entry"></asp:CompareValidator></td>
            <td nowrap><asp:TextBox ID="roll_w23TextBox" runat="server" Width="50px" Text='<%# Bind("[roll-w23]") %>' />
            <asp:CompareValidator ID="CompareValidator53" ControlToValidate="roll_w23TextBox" Display="Dynamic" SetFocusOnError="true" Type="Double" Operator="DataTypeCheck" runat="server" ErrorMessage="Invalid Entry"></asp:CompareValidator></td>
            <td nowrap><asp:TextBox ID="roll_w24TextBox" runat="server" Width="50px" Text='<%# Bind("[roll-w24]") %>' />
            <asp:CompareValidator ID="CompareValidator54" ControlToValidate="roll_w24TextBox" Display="Dynamic" SetFocusOnError="true" Type="Double" Operator="DataTypeCheck" runat="server" ErrorMessage="Invalid Entry"></asp:CompareValidator></td></tr>
            <tr><td nowrap><asp:TextBox ID="roll_w25TextBox" runat="server" Width="50px" Text='<%# Bind("[roll-w25]") %>' />
            <asp:CompareValidator ID="CompareValidator55" ControlToValidate="roll_w25TextBox" Display="Dynamic" SetFocusOnError="true" Type="Double" Operator="DataTypeCheck" runat="server" ErrorMessage="Invalid Entry"></asp:CompareValidator></td><td></td>
            <td nowrap><asp:TextBox ID="roll_w26TextBox" runat="server"  Width="50px"   Text='<%# Bind("[roll-w26]") %>' />
            <asp:CompareValidator ID="CompareValidator56" ControlToValidate="roll_w26TextBox" Display="Dynamic" SetFocusOnError="true" Type="Double" Operator="DataTypeCheck" runat="server" ErrorMessage="Invalid Entry"></asp:CompareValidator></td></tr>
            </table>
            </fieldset>
            </td></tr>
            <tr>
            <td>
            <table id="showtable" runat="server"><tr><td><br /></td></tr>
               <tr><td align="right" style="padding-right:5px"><b>Under Width:</b></td>
              <td nowrap> <asp:TextBox ID="TextBoxLabel3" Width="45px" runat="server" Text='<%# Bind("[width-min]") %>' />
              <asp:CompareValidator ID="CompareValidator61" ControlToValidate="TextBoxLabel3" Display="Dynamic" SetFocusOnError="true" Type="Double" Operator="DataTypeCheck" runat="server" ErrorMessage="Invalid Entry"></asp:CompareValidator></td>
               <td> <b>$:</b></td>
              <td nowrap> <asp:TextBox ID="TextBoxLabel4" Width="45px"  runat="server" Text='<%# Bind("[width-cst]") %>' />
              <asp:CompareValidator ID="CompareValidator62" ControlToValidate="TextBoxLabel4" Display="Dynamic" SetFocusOnError="true" Type="Double" Operator="DataTypeCheck" runat="server" ErrorMessage="Invalid Entry"></asp:CompareValidator></td></tr>
              <tr><td align="right" style="padding-right:5px"><b>Length:</b></td>
              <td nowrap><asp:TextBox ID="TextBoxLabel5" Width="45px"  runat="server" Text='<%# Bind("[length-min]") %>' />
              <asp:CompareValidator ID="CompareValidator63" ControlToValidate="TextBoxLabel5" Display="Dynamic" SetFocusOnError="true" Type="Double" Operator="DataTypeCheck" runat="server" ErrorMessage="Invalid Entry"></asp:CompareValidator></td>
              <td> <b>$:</b></td>
              <td nowrap><asp:TextBox ID="TextBoxLabel6" Width="45px"  runat="server" Text='<%# Bind("[length-cst]") %>' />
              <asp:CompareValidator ID="CompareValidator64" ControlToValidate="TextBoxLabel6" Display="Dynamic" SetFocusOnError="true" Type="Double" Operator="DataTypeCheck" runat="server" ErrorMessage="Invalid Entry"></asp:CompareValidator></td></tr>
              </table>
            </td><td>
            <table><tr>
            <td></td><td><b>Min</b></td><td><b>Max</b></td>
            </tr>
            <tr><td align="right" style="padding-right:5px"><b>Sheet Width:</b></td>
            
            <td nowrap><asp:TextBox ID="roll_w27TextBox" runat="server" Width="50px"   Text='<%# Bind("[roll-w27]") %>' />
            
            <asp:CompareValidator ID="CompareValidator57" ControlToValidate="roll_w27TextBox" Display="Dynamic" SetFocusOnError="true" Type="Double" Operator="DataTypeCheck" runat="server" ErrorMessage="Invalid Entry"></asp:CompareValidator></td>
            <td nowrap><asp:TextBox ID="roll_w28TextBox"    runat="server" Width="50px" Text='<%# Bind("[roll-w28]") %>' />
            <asp:CompareValidator ID="CompareValidator58" ControlToValidate="roll_w28TextBox" Display="Dynamic" ControlToCompare="roll_w27TextBox" SetFocusOnError="true" Type="Double" Operator="GreaterThan" runat="server" ErrorMessage="Must be grater than Min Sheet Width"></asp:CompareValidator>
             <asp:RequiredFieldValidator ID="RequiredFieldValidator1" ControlToValidate="roll_w28TextBox" runat="server" Display="Dynamic" SetFocusOnError="true" ErrorMessage="Enter the Max Sheet Width"></asp:RequiredFieldValidator></td>
            </tr>
            <tr><td align="right" style="padding-right:5px"><b>Length:</b></td>
            <td nowrap><asp:TextBox ID="roll_w29TextBox" Width="50px" runat="server" Text='<%# Bind("[roll-w29]") %>' />
            <asp:CompareValidator ID="CompareValidator59" ControlToValidate="roll_w29TextBox" Display="Dynamic" SetFocusOnError="true" Type="Double" Operator="DataTypeCheck" runat="server" ErrorMessage="Invalid Entry"></asp:CompareValidator></td>
            <td nowrap> <asp:TextBox Width="50px" ID="roll_w30TextBox"  runat="server" Text='<%# Bind("[roll-w30]") %>' />
            <asp:CompareValidator ID="CompareValidator60" ControlToValidate="roll_w30TextBox" Display="Dynamic" ControlToCompare="roll_w29TextBox" SetFocusOnError="true" Type="Double" Operator="GreaterThan" runat="server" ErrorMessage="Must be grater than Min Length"></asp:CompareValidator></td>
                <asp:RequiredFieldValidator ID="RequiredFieldValidator2" runat="server" ControlToValidate="roll_w30TextBox" Display="Dynamic" SetFocusOnError="true" ErrorMessage="Enter the Max Length"></asp:RequiredFieldValidator></tr>
            </table>
            </td></tr>
            <tr><td colspan="2" align="left">
            <asp:Label ID="roid1Label" Visible="false"  runat="server" Text='<%# Bind("[rowid-eitem]") %>' ></asp:Label>
                <asp:Label ID="roid2Label" Visible="false"  runat="server" Text='<%# Bind("[rowid-vend]") %>' ></asp:Label>
                <asp:Label ID="showLabel" Visible="false" runat="server" Text='<%# Bind("[showitem]") %>' ></asp:Label>
               
                <asp:Button ID="UpdateButton" runat="server" CssClass="button" CausesValidation="True" OnClick="Save_ButtonClick"  Text="Save" />
                &nbsp;<asp:Button ID="UpdateCancelButton" CssClass="button" runat="server" 
                    CausesValidation="False" CommandName="Cancel" Text="Cancel" OnClick="UpdateButtonCencel_Click" />
            </td></tr>
            </table>
            </fieldset>
            </EditItemTemplate>
            
            <ItemTemplate>
            <fieldset>
              <table class="shade">
              <tr><td align="right" style="padding-right:5px"><b>Item No:</b></td>
              <td> <asp:Label ID="vITEMLabel" Width="100px" BackColor="Turquoise" runat="server" Text='<%# Bind("vITEM") %>' /></td></tr>
              <tr><td align="right" style="padding-right:5px"><b>Purchased Cost Uom:</b></td>
              <td><asp:Label ID="stdumLabel" Width="100px" BackColor="Turquoise" runat="server" Text='<%# Bind("stdum") %>' /></td>
              <td align="right" style="padding-right:5px"><b>Vendor Item No:</b></td>
              <td><asp:Label ID="vend_itemLabel" Width="100px" BackColor="Turquoise" runat="server" Text='<%# Bind("[vend-item]") %>' /></td></tr>
              <tr><td align="right" style="padding-right:5px"><b>Vendor:</b></td>
              <td><asp:Label ID="vend_noLabel" Width="100px" BackColor="Turquoise" runat="server" Text='<%# Bind("[vend-no]") %>' /></td>
              <td colspan="2"><asp:Label ID="vendnameLabel" BackColor="Turquoise" runat="server" Text='<%# Bind("[vend-name]") %>'></asp:Label> </td></tr>
              <tr><td align="right" style="padding-right:5px"><b>Cost Update:</b></td>
              <td><asp:Label ID="updated_dateLabel" Width="100px" BackColor="Turquoise" runat="server" Text='<%# Bind("[updated-date]") %>' /></td></tr></table>
              <table>
              <tr>
              <td>
              <table>
              <tr><td><b>Qty to</b></td><td><b>Cost Per</b></td><td><b>Setup</b></td></tr>
              <tr><td><asp:Label ID="run_qty1Label" Width="60px" BackColor="Turquoise" runat="server" Text='<%# Bind("[run-qty1]") %>' /></td>
              <td><asp:Label ID="run_cost1Label" Width="60px" BackColor="Turquoise" runat="server" Text='<%# Bind("[run-cost1]") %>' /></td>
              <td><asp:Label ID="setups1Label" Width="60px" BackColor="Turquoise" runat="server" Text='<%# Bind("setups1") %>' /></td></tr>
              <tr><td><asp:Label ID="run_qty2Label" Width="60px" BackColor="Turquoise" runat="server" Text='<%# Bind("[run-qty2]") %>' /></td>
              <td><asp:Label ID="run_cost2Label" Width="60px" BackColor="Turquoise" runat="server" Text='<%# Bind("[run-cost2]") %>' /></td>
              <td><asp:Label ID="setups2Label" Width="60px" BackColor="Turquoise" runat="server" Text='<%# Bind("setups2") %>' /></td></tr>
              <tr><td><asp:Label ID="run_qty3Label" Width="60px" BackColor="Turquoise" runat="server" Text='<%# Bind("[run-qty3]") %>' /></td>
              <td> <asp:Label ID="run_cost3Label" Width="60px" BackColor="Turquoise" runat="server" Text='<%# Bind("[run-cost3]") %>' /></td>
              <td><asp:Label ID="setups3Label" Width="60px" BackColor="Turquoise" runat="server" Text='<%# Bind("setups3") %>' /></td></tr>
              <tr><td><asp:Label ID="run_qty4Label" Width="60px" BackColor="Turquoise" runat="server" Text='<%# Bind("[run-qty4]") %>' /></td>
              <td> <asp:Label ID="run_cost4Label" Width="60px" BackColor="Turquoise" runat="server" Text='<%# Bind("[run-cost4]") %>' /></td>
              <td><asp:Label ID="setups4Label" Width="60px" BackColor="Turquoise" runat="server" Text='<%# Bind("setups4") %>' /></td></tr>
              <tr><td><asp:Label ID="run_qty5Label" Width="60px" BackColor="Turquoise" runat="server" Text='<%# Bind("[run-qty5]") %>' /></td>
              <td> <asp:Label ID="run_cost5Label" Width="60px" BackColor="Turquoise" runat="server" Text='<%# Bind("[run-cost5]") %>' /></td>
              <td> <asp:Label ID="setups5Label" Width="60px" BackColor="Turquoise" runat="server" Text='<%# Bind("setups5") %>' /></td></tr>
              <tr><td><asp:Label ID="run_qty6Label" Width="60px" BackColor="Turquoise" runat="server" Text='<%# Bind("[run-qty6]") %>' /></td>
              <td><asp:Label ID="run_cost6Label" Width="60px" BackColor="Turquoise" runat="server" Text='<%# Bind("[run-cost6]") %>' /></td>
              <td><asp:Label ID="setups6Label" Width="60px" BackColor="Turquoise" runat="server" Text='<%# Bind("setups6") %>' /></td></tr>
              <tr><td> <asp:Label ID="run_qty7Label" Width="60px" BackColor="Turquoise" runat="server" Text='<%# Bind("[run-qty7]") %>' /></td>
              <td><asp:Label ID="run_cost7Label" Width="60px" BackColor="Turquoise" runat="server" Text='<%# Bind("[run-cost7]") %>' /></td>
              <td> <asp:Label ID="setups7Label" Width="60px" BackColor="Turquoise" runat="server" Text='<%# Bind("setups7") %>' /></td></tr>
              <tr><td> <asp:Label ID="run_qty8Label" Width="60px" BackColor="Turquoise" runat="server" Text='<%# Bind("[run-qty8]") %>' /></td>
              <td> <asp:Label ID="run_cost8Label" Width="60px" BackColor="Turquoise" runat="server" Text='<%# Bind("[run-cost8]") %>' /></td>
              <td><asp:Label ID="setups8Label" Width="60px" BackColor="Turquoise" runat="server" Text='<%# Bind("setups8") %>' /></td></tr>
              <tr><td> <asp:Label ID="run_qty9Label" Width="60px" BackColor="Turquoise" runat="server" Text='<%# Bind("[run-qty9]") %>' /></td>
              <td><asp:Label ID="run_cost9Label" Width="60px" BackColor="Turquoise" runat="server" Text='<%# Bind("[run-cost9]") %>' /></td>
              <td><asp:Label ID="setups9Label" Width="60px" BackColor="Turquoise" runat="server" Text='<%# Bind("setups9") %>' /></td></tr>
              <tr><td><asp:Label ID="run_qty10Label" Width="60px" BackColor="Turquoise" runat="server" Text='<%# Bind("[run-qty10]") %>' /></td>
              <td><asp:Label ID="run_cost10Label" Width="60px" BackColor="Turquoise" runat="server" Text='<%# Bind("[run-cost10]") %>' /></td>
              <td> <asp:Label ID="setups10Label" Width="60px" BackColor="Turquoise" runat="server" Text='<%# Bind("setups10") %>' /></td></tr>
              </table>
              </td>
              <td>
              <fieldset><legend></legend>
              <table>
              <tr><td colspan="3"><b>Valid  Estimate  Roll  Width</b></td></tr>
              <tr><td><asp:Label ID="roll_w1Label" Width="60px" BackColor="Turquoise" runat="server" Text='<%# Bind("[roll-w1]") %>' /></td>
              <td><asp:Label ID="roll_w2Label" Width="60px" BackColor="Turquoise" runat="server" Text='<%# Bind("[roll-w2]") %>' /></td>
              <td> <asp:Label ID="roll_w3Label" Width="60px" BackColor="Turquoise" runat="server" Text='<%# Bind("[roll-w3]") %>' /></td></tr>
              <tr><td>  <asp:Label ID="roll_w4Label" Width="60px" BackColor="Turquoise" runat="server" Text='<%# Bind("[roll-w4]") %>' /></td>
              <td>   <asp:Label ID="roll_w5Label" Width="60px" BackColor="Turquoise" runat="server" Text='<%# Bind("[roll-w5]") %>' /></td>
              <td> <asp:Label ID="roll_w6Label" Width="60px" BackColor="Turquoise" runat="server" Text='<%# Bind("[roll-w6]") %>' /></td></tr>
              <tr><td><asp:Label ID="roll_w7Label" Width="60px" BackColor="Turquoise" runat="server" Text='<%# Bind("[roll-w7]") %>' /></td>
              <td> <asp:Label ID="roll_w8Label" Width="60px" BackColor="Turquoise" runat="server" Text='<%# Bind("[roll-w8]") %>' /></td>
              <td><asp:Label ID="roll_w9Label" Width="60px" BackColor="Turquoise" runat="server" Text='<%# Bind("[roll-w9]") %>' /></td></tr>
              <tr><td><asp:Label ID="roll_w10Label" Width="60px" BackColor="Turquoise" runat="server" Text='<%# Bind("[roll-w10]") %>' /></td>
              <td> <asp:Label ID="roll_w11Label" Width="60px" BackColor="Turquoise" runat="server" Text='<%# Bind("[roll-w11]") %>' /></td>
              <td><asp:Label ID="roll_w12Label" Width="60px" BackColor="Turquoise" runat="server" Text='<%# Bind("[roll-w12]") %>' /></td></tr>
              <tr><td><asp:Label ID="roll_w13Label" Width="60px" BackColor="Turquoise" runat="server" Text='<%# Bind("[roll-w13]") %>' /></td>
              <td><asp:Label ID="roll_w14Label" Width="60px" BackColor="Turquoise" runat="server" Text='<%# Bind("[roll-w14]") %>' /></td>
              <td> <asp:Label ID="roll_w15Label" Width="60px" BackColor="Turquoise" runat="server" Text='<%# Bind("[roll-w15]") %>' /></td></tr>
              <tr><td><asp:Label ID="roll_w16Label" Width="60px" BackColor="Turquoise" runat="server" Text='<%# Bind("[roll-w16]") %>' /></td>
              <td> <asp:Label ID="roll_w17Label" Width="60px" BackColor="Turquoise" runat="server" Text='<%# Bind("[roll-w17]") %>' /></td>
              <td><asp:Label ID="roll_w18Label" Width="60px" BackColor="Turquoise" runat="server" Text='<%# Bind("[roll-w18]") %>' /></td></tr>
              <tr><td><asp:Label ID="roll_w19Label" Width="60px" BackColor="Turquoise" runat="server" Text='<%# Bind("[roll-w19]") %>' /></td>
              <td> <asp:Label ID="roll_w20Label" Width="60px" BackColor="Turquoise" runat="server" Text='<%# Bind("[roll-w20]") %>' /></td>
              <td><asp:Label ID="roll_w21Label" Width="60px" BackColor="Turquoise" runat="server" Text='<%# Bind("[roll-w21]") %>' /></td></tr>
              <tr><td> <asp:Label ID="roll_w22Label" Width="60px" BackColor="Turquoise" runat="server" Text='<%# Bind("[roll-w22]") %>' /></td>
              <td> <asp:Label ID="roll_w23Label" Width="60px" BackColor="Turquoise" runat="server" Text='<%# Bind("[roll-w23]") %>' /></td>
              <td> <asp:Label ID="roll_w24Label" Width="60px" BackColor="Turquoise" runat="server" Text='<%# Bind("[roll-w24]") %>' /></td></tr>
              <tr><td> <asp:Label ID="roll_w25Label" Width="60px" BackColor="Turquoise" runat="server" Text='<%# Bind("[roll-w25]") %>' /></td>
              <td></td>
              <td><asp:Label ID="roll_w26Label" Width="60px" BackColor="Turquoise" runat="server" Text='<%# Bind("[roll-w26]") %>' /></td></tr>
              </table>
              </fieldset>
              </td>              
              </tr>
              <tr><td>
                <table id="showtable" runat="server"><tr><td><br /></td></tr>
               <tr><td align="right" style="padding-right:5px"><b>Under Width:</b></td>
              <td> <asp:Label ID="Label3" Width="50px" BackColor="Turquoise" runat="server" Text='<%# Bind("[width-min]") %>' /></td> <td> <b>$:</b></td>
              <td> <asp:Label ID="Label4" Width="50px" BackColor="Turquoise" runat="server" Text='<%# Bind("[width-cst]") %>' /></td></tr>
              <tr><td align="right" style="padding-right:5px"><b>Length:</b></td>
              <td><asp:Label ID="Label5" Width="50px" BackColor="Turquoise" runat="server" Text='<%# Bind("[length-min]") %>' /></td><td> <b>$:</b></td>
              <td><asp:Label ID="Label6" Width="50px" BackColor="Turquoise" runat="server" Text='<%# Bind("[length-cst]") %>' /></td></tr>
              </table>
              </td>
              <td>
              <table><tr><td></td><td><b>Min</b></td><td><b>Max</b></td></tr>
               <tr><td align="right" style="padding-right:5px"><b>Sheet Width:</b></td>
              <td> <asp:Label ID="roll_w27Label" Width="60px" BackColor="Turquoise" runat="server" Text='<%# Bind("[roll-w27]") %>' /></td>
              <td> <asp:Label ID="roll_w28Label" Width="60px" BackColor="Turquoise" runat="server" Text='<%# Bind("[roll-w28]") %>' /></td></tr>
              <tr><td align="right" style="padding-right:5px"><b>Length:</b></td>
              <td><asp:Label ID="roll_w29Label" Width="60px" BackColor="Turquoise" runat="server" Text='<%# Bind("[roll-w29]") %>' /></td>
              <td><asp:Label ID="roll_w30Label" Width="60px" BackColor="Turquoise" runat="server" Text='<%# Bind("[roll-w30]") %>' /></td></tr>
              </table>
              </td></tr>              
              <tr><td colspan="2" align="left">
               <asp:Label ID="Label1" Visible="false" runat="server" Text='<%# Bind("[rowid-eitem]") %>' />
                  <asp:Label ID="Label2" Visible="false" runat="server" Text='<%# Bind("[rowid-vend]") %>' />
                     <asp:Label ID="showLabel" Visible="false" runat="server" Text='<%# Bind("[showitem]") %>' ></asp:Label>
                 <asp:Button ID="InsertButton" runat="server" CssClass="button" CausesValidation="True" CommandName="edit" Text="Update" />
                &nbsp;<asp:Button ID="InsertCancelButton" runat="server" CssClass="button"  CausesValidation="False" OnClientClick="javascript:window.opener.location.href='corr_vendor_cost.aspx'; self.close()" Text="Close" />
              </td></tr>
              </table>
              </fieldset>
                
               
            </ItemTemplate>
        </asp:FormView>
        <asp:ObjectDataSource ID="ObjectDataSource1" runat="server" OldValuesParameterFormatString="original_{0}"
         SelectMethod="SelectVendorItem" TypeName="Corrugated">
            <SelectParameters>
                    
                <asp:Parameter DefaultValue="View" Name="prmAction" Type="String" />
            <asp:Parameter Name="prmUser" Type="String" />
                    
<asp:Parameter Name="prmEst" Type="String"></asp:Parameter>
                <asp:Parameter Name="prmFormNo" Type="Int32" />
                <asp:Parameter Name="prmBlanck" Type="Int32" />                 
                <asp:SessionParameter  Name="prmItem" 
                    SessionField="corr_vendor_cost_item_get" Type="String" />
                <asp:Parameter Name="prmStdum" Type="String" />
                <asp:Parameter Name="prmVendItem" Type="String" />
                <asp:Parameter Name="prmVendNO" Type="String" />
                <asp:Parameter Name="prmDate" Type="String" />
                <asp:Parameter Name="prmRunQty1" Type="Decimal" />
                <asp:Parameter Name="prmRunQty2" Type="Decimal" />
                <asp:Parameter Name="prmRunQty3" Type="Decimal" />
                <asp:Parameter Name="prmRunQty4" Type="Decimal" />
                <asp:Parameter Name="prmRunQty5" Type="Decimal" />
                <asp:Parameter Name="prmRunQty6" Type="Decimal" />
                <asp:Parameter Name="prmRunQty7" Type="Decimal" />
                <asp:Parameter Name="prmRunQty8" Type="Decimal" />
                <asp:Parameter Name="prmRunQty9" Type="Decimal" />
                <asp:Parameter Name="prmRunQty10" Type="Decimal" />
                <asp:Parameter Name="prmSetups1" Type="Decimal" />
                <asp:Parameter Name="prmSetups2" Type="Decimal" />
                <asp:Parameter Name="prmSetups3" Type="Decimal" />
                <asp:Parameter Name="prmSetups4" Type="Decimal" />
                <asp:Parameter Name="prmSetups5" Type="Decimal" />
                <asp:Parameter Name="prmSetups6" Type="Decimal" />
                <asp:Parameter Name="prmSetups7" Type="Decimal" />
                <asp:Parameter Name="prmSetups8" Type="Decimal" />
                <asp:Parameter Name="prmSetups9" Type="Decimal" />
                <asp:Parameter Name="prmSetups10" Type="Decimal" />
                <asp:Parameter Name="prmRunCost1" Type="Decimal" />
                <asp:Parameter Name="prmRunCost2" Type="Decimal" />
                <asp:Parameter Name="prmRunCost3" Type="Decimal" />
                <asp:Parameter Name="prmRunCost4" Type="Decimal" />
                <asp:Parameter Name="prmRunCost5" Type="Decimal" />
                <asp:Parameter Name="prmRunCost6" Type="Decimal" />
                <asp:Parameter Name="prmRunCost7" Type="Decimal" />
                <asp:Parameter Name="prmRunCost8" Type="Decimal" />
                <asp:Parameter Name="prmRunCost9" Type="Decimal" />
                <asp:Parameter Name="prmRunCost10" Type="Decimal" />
                <asp:Parameter Name="prmRollw1" Type="Decimal" />
                <asp:Parameter Name="prmRollw2" Type="Decimal" />
                <asp:Parameter Name="prmRollw3" Type="Decimal" />
                <asp:Parameter Name="prmRollw4" Type="Decimal" />
                <asp:Parameter Name="prmRollw5" Type="Decimal" />
                <asp:Parameter Name="prmRollw6" Type="Decimal" />
                <asp:Parameter Name="prmRollw7" Type="Decimal" />
                <asp:Parameter Name="prmRollw8" Type="Decimal" />
                <asp:Parameter Name="prmRollw9" Type="Decimal" />
                <asp:Parameter Name="prmRollw10" Type="Decimal" />
                <asp:Parameter Name="prmRollw11" Type="Decimal" />
                <asp:Parameter Name="prmRollw12" Type="Decimal" />
                <asp:Parameter Name="prmRollw13" Type="Decimal" />
                <asp:Parameter Name="prmRollw14" Type="Decimal" />
                <asp:Parameter Name="prmRollw15" Type="Decimal" />
                <asp:Parameter Name="prmRollw16" Type="Decimal" />
                <asp:Parameter Name="prmRollw17" Type="Decimal" />
                <asp:Parameter Name="prmRollw18" Type="Decimal" />
                <asp:Parameter Name="prmRollw19" Type="Decimal" />
                <asp:Parameter Name="prmRollw20" Type="Decimal" />
                <asp:Parameter Name="prmRollw21" Type="Decimal" />
                <asp:Parameter Name="prmRollw22" Type="Decimal" />
                <asp:Parameter Name="prmRollw23" Type="Decimal" />
                <asp:Parameter Name="prmRollw24" Type="Decimal" />
                <asp:Parameter Name="prmRollw25" Type="Decimal" />
                <asp:Parameter Name="prmRollw26" Type="Decimal" />
                <asp:Parameter Name="prmRollw27" Type="Decimal" />
                <asp:Parameter Name="prmRollw28" Type="Decimal" />
                <asp:Parameter Name="prmRollw29" Type="Decimal" />
                <asp:Parameter Name="prmRollw30" Type="Decimal" />
                <asp:Parameter Name="prmRowid1" Type="String" />
                <asp:Parameter Name="prmRowid2" Type="String" />
                <asp:Parameter Name="prmWidthmin" Type="Decimal" />
                <asp:Parameter Name="prmLengthmin" Type="Decimal" />
                <asp:Parameter Name="prmWidthcst" Type="Decimal" />
                <asp:Parameter Name="prmLengthcst" Type="Decimal" />
                
                 
            </SelectParameters>
        </asp:ObjectDataSource>
    </div>
    </form>
</body>
</html>
