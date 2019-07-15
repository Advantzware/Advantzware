<%@ Page Language="C#" AutoEventWireup="true" Debug="true" Inherits="corr_vend_fgitem" Title="Vendor Cost" Codebehind="corr_vend_fgitem.aspx.cs" %>

<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">

<html xmlns="http://www.w3.org/1999/xhtml" >
<head id="Head1" runat="server">
    <title>Add New Vendor Cost</title>
    <LINK href="include/style.css" type="text/css" rel="stylesheet"/>
<link rel="stylesheet" href="include/dhtmlwindow.css" type="text/css" />
</head>
<script type="text/javascript" src="include/dhtmlwindow.js"></script>
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
            <tr><td  align="right" style="padding-right:5px"><b>Purchased cost UOM:</b></td>
            <td nowrap><asp:TextBox ID="stdumTextBox" Width="50px" runat="server" Text='<%# Bind("vStdum") %>' />
            <a href="#" onclick="uomlook(); return false"><asp:Image ID="Image2" runat="server" ImageUrl="images/lookup_icon.gif" /></a></td>            
            <td nowrap align="right" style="padding-right:5px"><b>Vendor Item No:</b></td>
            <td nowrap> <asp:TextBox ID="vend_itemTextBox" Width="80px" runat="server" Text='<%# Bind("[vVend-item]") %>' /></td>
            <td align="right" style="padding-right:5px"><b>Cust:</b></td>
            <td nowrap><asp:TextBox ID="custTextBox" Width="80px" runat="server" Text='<%# Bind("[vCust]") %>' /></td></tr>
            <tr><td align="right" style="padding-right:5px"><b>Vendor:</b></td>
            <td nowrap colspan="4"> <asp:TextBox ID="vend_noTextBox" Width="100px" runat="server" Text='<%# Bind("[vend-no]") %>' />
            <a href="#" onclick="vendorlook(); return false"><asp:Image ID="Image1" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
            &nbsp;&nbsp;&nbsp;&nbsp;<asp:Label ID="vendnameLabel" BackColor="Turquoise" runat="server" Text='<%# Bind("[vend-name]") %>'></asp:Label></td></tr>
            <tr></tr>
            </table>
            <table align="center" class="shade">
            <tr><td>
            <table><tr><td><b>Qty to</b></td>
            <td><b>Cost Per</b></td>
            <td><b>Set</b></td></tr>
            <tr><td nowrap> <asp:TextBox ID="run_qty1TextBox" runat="server" Width="50px" Text='<%# Bind("[vRun-qty1]") %>' />
            <asp:CompareValidator ID="CompareValidator1" ControlToValidate="run_qty1TextBox" Display="Dynamic" SetFocusOnError="true" Type="Double" Operator="DataTypeCheck" runat="server" ErrorMessage="Invalid Entry"></asp:CompareValidator></td>
            <td nowrap> <asp:TextBox ID="run_cost1TextBox" runat="server" Width="50px" Text='<%# Bind("[vRun-cost1]") %>' />
            <asp:CompareValidator ID="CompareValidator2" ControlToValidate="run_cost1TextBox" Display="Dynamic" SetFocusOnError="true" Type="Double" Operator="DataTypeCheck" runat="server" ErrorMessage="Invalid Entry"></asp:CompareValidator></td>
            <td nowrap><asp:TextBox ID="setups1TextBox" runat="server" Width="50px" Text='<%# Bind("vSetups1") %>' />
            <asp:CompareValidator ID="CompareValidator3" ControlToValidate="setups1TextBox" Display="Dynamic" SetFocusOnError="true" Type="Double" Operator="DataTypeCheck" runat="server" ErrorMessage="Invalid Entry"></asp:CompareValidator></td></tr>
           
            <tr><td nowrap><asp:TextBox ID="run_qty2TextBox" runat="server" Width="50px" Text='<%# Bind("[vRun-qty2]") %>' />
            <asp:CompareValidator ID="CompareValidator4" ControlToValidate="run_qty2TextBox" Display="Dynamic" SetFocusOnError="true" Type="Double" Operator="DataTypeCheck" runat="server" ErrorMessage="Invalid Entry"></asp:CompareValidator></td>
            <td nowrap><asp:TextBox ID="run_cost2TextBox" runat="server" Width="50px"  Text='<%# Bind("[vRun-cost2]") %>' />
            <asp:CompareValidator ID="CompareValidator5" ControlToValidate="run_cost2TextBox" Display="Dynamic" SetFocusOnError="true" Type="Double" Operator="DataTypeCheck" runat="server" ErrorMessage="Invalid Entry"></asp:CompareValidator></td>
            <td nowrap><asp:TextBox ID="setups2TextBox" runat="server" Width="50px" Text='<%# Bind("vSetups2") %>' />
            <asp:CompareValidator ID="CompareValidator6" ControlToValidate="setups2TextBox" Display="Dynamic" SetFocusOnError="true" Type="Double" Operator="DataTypeCheck" runat="server" ErrorMessage="Invalid Entry"></asp:CompareValidator></td></tr>
            
            <tr><td nowrap><asp:TextBox ID="run_qty3TextBox" runat="server" Width="50px" Text='<%# Bind("[vRun-qty3]") %>' />
            <asp:CompareValidator ID="CompareValidator7" ControlToValidate="run_qty3TextBox" Display="Dynamic" SetFocusOnError="true" Type="Double" Operator="DataTypeCheck" runat="server" ErrorMessage="Invalid Entry"></asp:CompareValidator></td>
            <td nowrap><asp:TextBox ID="run_cost3TextBox" runat="server" Width="50px" Text='<%# Bind("[vRun-cost3]") %>' />
            <asp:CompareValidator ID="CompareValidator8" ControlToValidate="run_cost3TextBox" Display="Dynamic" SetFocusOnError="true" Type="Double" Operator="DataTypeCheck" runat="server" ErrorMessage="Invalid Entry"></asp:CompareValidator></td>
            <td nowrap><asp:TextBox ID="setups3TextBox" runat="server" Width="50px" Text='<%# Bind("vSetups3") %>' />
            <asp:CompareValidator ID="CompareValidator9" ControlToValidate="setups3TextBox" Display="Dynamic" SetFocusOnError="true" Type="Double" Operator="DataTypeCheck" runat="server" ErrorMessage="Invalid Entry"></asp:CompareValidator></td></tr>
            
            <tr><td nowrap><asp:TextBox ID="run_qty4TextBox" runat="server" Width="50px" Text='<%# Bind("[vRun-qty4]") %>' />
            <asp:CompareValidator ID="CompareValidator10" ControlToValidate="run_qty4TextBox" Display="Dynamic" SetFocusOnError="true" Type="Double" Operator="DataTypeCheck" runat="server" ErrorMessage="Invalid Entry"></asp:CompareValidator></td>
            <td nowrap><asp:TextBox ID="run_cost4TextBox" runat="server" Width="50px" Text='<%# Bind("[vRun-cost4]") %>' />
            <asp:CompareValidator ID="CompareValidator11" ControlToValidate="run_cost4TextBox" Display="Dynamic" SetFocusOnError="true" Type="Double" Operator="DataTypeCheck" runat="server" ErrorMessage="Invalid Entry"></asp:CompareValidator></td>
            <td nowrap><asp:TextBox ID="setups4TextBox" runat="server" Width="50px" Text='<%# Bind("vSetups4") %>' />
            <asp:CompareValidator ID="CompareValidator12" ControlToValidate="setups4TextBox" Display="Dynamic" SetFocusOnError="true" Type="Double" Operator="DataTypeCheck" runat="server" ErrorMessage="Invalid Entry"></asp:CompareValidator></td></tr>
            
            <tr><td nowrap><asp:TextBox ID="run_qty5TextBox" runat="server" Width="50px" Text='<%# Bind("[vRun-qty5]") %>' />
            <asp:CompareValidator ID="CompareValidator13" ControlToValidate="run_qty5TextBox" Display="Dynamic" SetFocusOnError="true" Type="Double" Operator="DataTypeCheck" runat="server" ErrorMessage="Invalid Entry"></asp:CompareValidator></td>
            <td nowrap> <asp:TextBox ID="run_cost5TextBox" runat="server" Width="50px" Text='<%# Bind("[vRun-cost5]") %>' />
            <asp:CompareValidator ID="CompareValidator14" ControlToValidate="run_cost5TextBox" Display="Dynamic" SetFocusOnError="true" Type="Double" Operator="DataTypeCheck" runat="server" ErrorMessage="Invalid Entry"></asp:CompareValidator></td>
            <td nowrap><asp:TextBox ID="setups5TextBox" runat="server" Width="50px" Text='<%# Bind("vSetups5") %>' />
            <asp:CompareValidator ID="CompareValidator15" ControlToValidate="setups5TextBox" Display="Dynamic" SetFocusOnError="true" Type="Double" Operator="DataTypeCheck" runat="server" ErrorMessage="Invalid Entry"></asp:CompareValidator></td></tr>
            
            <tr><td nowrap> <asp:TextBox ID="run_qty6TextBox" runat="server" Width="50px" Text='<%# Bind("[vRun-qty6]") %>' />
            <asp:CompareValidator ID="CompareValidator16" ControlToValidate="run_qty6TextBox" Display="Dynamic" SetFocusOnError="true" Type="Double" Operator="DataTypeCheck" runat="server" ErrorMessage="Invalid Entry"></asp:CompareValidator></td>
            <td nowrap><asp:TextBox ID="run_cost6TextBox" runat="server" Width="50px" Text='<%# Bind("[vRun-cost6]") %>' />
            <asp:CompareValidator ID="CompareValidator17" ControlToValidate="run_cost6TextBox" Display="Dynamic" SetFocusOnError="true" Type="Double" Operator="DataTypeCheck" runat="server" ErrorMessage="Invalid Entry"></asp:CompareValidator></td>
            <td nowrap><asp:TextBox ID="setups6TextBox" runat="server" Width="50px" Text='<%# Bind("vSetups6") %>' />
            <asp:CompareValidator ID="CompareValidator18" ControlToValidate="setups6TextBox" Display="Dynamic" SetFocusOnError="true" Type="Double" Operator="DataTypeCheck" runat="server" ErrorMessage="Invalid Entry"></asp:CompareValidator></td></tr>
            
            <tr><td nowrap><asp:TextBox ID="run_qty7TextBox" runat="server" Width="50px" Text='<%# Bind("[vRun-qty7]") %>' />
            <asp:CompareValidator ID="CompareValidator19" ControlToValidate="run_qty7TextBox" Display="Dynamic" SetFocusOnError="true" Type="Double" Operator="DataTypeCheck" runat="server" ErrorMessage="Invalid Entry"></asp:CompareValidator></td>
            <td nowrap><asp:TextBox ID="run_cost7TextBox" runat="server" Width="50px" Text='<%# Bind("[vRun-cost7]") %>' />
            <asp:CompareValidator ID="CompareValidator20" ControlToValidate="run_cost7TextBox" Display="Dynamic" SetFocusOnError="true" Type="Double" Operator="DataTypeCheck" runat="server" ErrorMessage="Invalid Entry"></asp:CompareValidator></td>
            <td nowrap><asp:TextBox ID="setups7TextBox" runat="server" Width="50px" Text='<%# Bind("vSetups7") %>' />
            <asp:CompareValidator ID="CompareValidator21" ControlToValidate="setups7TextBox" Display="Dynamic" SetFocusOnError="true" Type="Double" Operator="DataTypeCheck" runat="server" ErrorMessage="Invalid Entry"></asp:CompareValidator></td></tr>
            
            <tr><td nowrap><asp:TextBox ID="run_qty8TextBox" runat="server" Width="50px" Text='<%# Bind("[vRun-qty8]") %>' />
            <asp:CompareValidator ID="CompareValidator22" ControlToValidate="run_qty8TextBox" Display="Dynamic" SetFocusOnError="true" Type="Double" Operator="DataTypeCheck" runat="server" ErrorMessage="Invalid Entry"></asp:CompareValidator></td>
            <td nowrap><asp:TextBox ID="run_cost8TextBox" runat="server" Width="50px" Text='<%# Bind("[vRun-cost8]") %>' />
            <asp:CompareValidator ID="CompareValidator23" ControlToValidate="run_cost8TextBox" Display="Dynamic" SetFocusOnError="true" Type="Double" Operator="DataTypeCheck" runat="server" ErrorMessage="Invalid Entry"></asp:CompareValidator></td>
            <td nowrap><asp:TextBox ID="setups8TextBox" runat="server" Width="50px" Text='<%# Bind("vSetups8") %>' />
            <asp:CompareValidator ID="CompareValidator24" ControlToValidate="setups8TextBox" Display="Dynamic" SetFocusOnError="true" Type="Double" Operator="DataTypeCheck" runat="server" ErrorMessage="Invalid Entry"></asp:CompareValidator></td></tr>
            
            <tr><td nowrap><asp:TextBox ID="run_qty9TextBox" runat="server" Width="50px"  Text='<%# Bind("[vRun-qty9]") %>' />
            <asp:CompareValidator ID="CompareValidator25" ControlToValidate="run_qty9TextBox" Display="Dynamic" SetFocusOnError="true" Type="Double" Operator="DataTypeCheck" runat="server" ErrorMessage="Invalid Entry"></asp:CompareValidator></td>
            <td nowrap><asp:TextBox ID="run_cost9TextBox" runat="server" Width="50px" Text='<%# Bind("[vRun-cost9]") %>' />
            <asp:CompareValidator ID="CompareValidator26" ControlToValidate="run_cost9TextBox" Display="Dynamic" SetFocusOnError="true" Type="Double" Operator="DataTypeCheck" runat="server" ErrorMessage="Invalid Entry"></asp:CompareValidator></td>
            <td nowrap> <asp:TextBox ID="setups9TextBox" runat="server" Width="50px" Text='<%# Bind("vSetups9") %>' />
            <asp:CompareValidator ID="CompareValidator27" ControlToValidate="setups9TextBox" Display="Dynamic" SetFocusOnError="true" Type="Double" Operator="DataTypeCheck" runat="server" ErrorMessage="Invalid Entry"></asp:CompareValidator></td></tr>
            
            <tr><td nowrap><asp:TextBox ID="run_qty10TextBox" runat="server" Width="50px" Text='<%# Bind("[vRun-qty10]") %>' />
            <asp:CompareValidator ID="CompareValidator28" ControlToValidate="run_qty10TextBox" Display="Dynamic" SetFocusOnError="true" Type="Double" Operator="DataTypeCheck" runat="server" ErrorMessage="Invalid Entry"></asp:CompareValidator></td>
            <td nowrap><asp:TextBox ID="run_cost10TextBox" runat="server" Width="50px" Text='<%# Bind("[vRun-cost10]") %>' />
            <asp:CompareValidator ID="CompareValidator29" ControlToValidate="run_cost10TextBox" Display="Dynamic" SetFocusOnError="true" Type="Double" Operator="DataTypeCheck" runat="server" ErrorMessage="Invalid Entry"></asp:CompareValidator></td>
            <td nowrap><asp:TextBox ID="setups10TextBox" runat="server" Width="50px" Text='<%# Bind("vSetups10") %>' />
            <asp:CompareValidator ID="CompareValidator30" ControlToValidate="setups10TextBox" Display="Dynamic" SetFocusOnError="true" Type="Double" Operator="DataTypeCheck" runat="server" ErrorMessage="Invalid Entry"></asp:CompareValidator></td>
            <td><b>GS&A O/H Markup%:</b></td>
            <td><asp:TextBox ID="MarkupTextBox" runat="server" Width="50px" Text='<%# Bind("vMarkup") %>' /></td>
            </tr>
            </table>
            </td>            
            </tr>
            <tr>
            <td>
            <table><tr>
            <td></td><td><b>Min</b></td><td><b>Max</b></td><td></td><td><b>Min</b></td><td><b>Max</b></td>
            </tr>
            <tr><td align="right" style="padding-right:5px"><b>Sheet Width:</b></td>
            
            <td nowrap><asp:TextBox ID="roll_w27TextBox" runat="server" Width="50px"   Text='<%# Bind("[vRoll-w1]") %>' />
            
            <asp:CompareValidator ID="CompareValidator57" ControlToValidate="roll_w27TextBox" Display="Dynamic" SetFocusOnError="true" Type="Double" Operator="DataTypeCheck" runat="server" ErrorMessage="Invalid Entry"></asp:CompareValidator></td>
            <td nowrap><asp:TextBox ID="roll_w28TextBox"   runat="server" Width="50px" Text='<%# Bind("[vRoll-w2]") %>' />
            <asp:CompareValidator ID="CompareValidator58" ControlToValidate="roll_w28TextBox" ControlToCompare="roll_w27TextBox" Display="Dynamic" SetFocusOnError="true" Type="Double" Operator="GreaterThan" runat="server" ErrorMessage="Invalid Entry"></asp:CompareValidator>
             <asp:RequiredFieldValidator ID="RequiredFieldValidator1" ControlToValidate="roll_w28TextBox" runat="server" Display="Dynamic" SetFocusOnError="true" ErrorMessage="Enter the Max Sheet Width"></asp:RequiredFieldValidator></td>
            
            <td align="right" style="padding-right:5px"><b>Length:</b></td>
            <td nowrap><asp:TextBox ID="roll_w29TextBox" Width="50px" runat="server"  Text='<%# Bind("[vRoll-w3]") %>' />
            <asp:CompareValidator ID="CompareValidator59" ControlToValidate="roll_w29TextBox" Display="Dynamic" SetFocusOnError="true" Type="Double" Operator="DataTypeCheck" runat="server" ErrorMessage="Invalid Entry"></asp:CompareValidator></td>
            <td nowrap> <asp:TextBox Width="50px" ID="roll_w30TextBox" runat="server" Text='<%# Bind("[vRoll-w4]") %>' />
            <asp:CompareValidator ID="CompareValidator60" ControlToValidate="roll_w30TextBox" ControlToCompare="roll_w29TextBox" Display="Dynamic" SetFocusOnError="true" Type="Double" Operator="GreaterThan" runat="server" ErrorMessage="Invalid Entry"></asp:CompareValidator>
            <asp:RequiredFieldValidator ID="RequiredFieldValidator2" runat="server" ControlToValidate="roll_w30TextBox" Display="Dynamic" SetFocusOnError="true" ErrorMessage="Enter the Max Length"></asp:RequiredFieldValidator></td>
            </tr>
            </table>
            </td></tr>
            <tr><td colspan="2" align="left">
            <asp:Label ID="roid1Label"   runat="server" Text='<%# Bind("[vRowid-eitem]") %>' ></asp:Label>
                <asp:Label ID="roid2Label"   runat="server" Text='<%# Bind("[vRowid-vend]") %>' ></asp:Label>
                <asp:Label ID="showLabel"  runat="server" Text='<%# Bind("[vShowitem]") %>' ></asp:Label>
               
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
              <td><asp:Label ID="stdumLabel" Width="50px" BackColor="Turquoise" runat="server" Text='<%# Bind("vStdum") %>' /></td>
              <td nowrap align="right" style="padding-right:5px"><b>Vendor Item No:</b></td>
              <td><asp:Label ID="vend_itemLabel" Width="70px" BackColor="Turquoise" runat="server" Text='<%# Bind("[vVend-item]") %>' /></td>
              <td align="right" style="padding-right:5px"><b>Cust:</b></td>
              <td><asp:Label ID="CustLabel" Width="80px" BackColor="Turquoise" runat="server" Text='<%# Bind("[vCust]") %>' /></td></tr>
              <tr><td align="right" style="padding-right:5px"><b>Vendor:</b></td>
              <td colspan="6"><asp:Label ID="vend_noLabel" Width="100px" BackColor="Turquoise" runat="server" Text='<%# Bind("[vend-no]") %>' />
              <asp:Label ID="vendnameLabel" BackColor="Turquoise" runat="server" Text='<%# Bind("[vend-name]") %>'></asp:Label> </td></tr>
              <tr></tr></table>
              <table align="center">
              <tr>
              <td>
              <table>
              <tr><td><b>Qty to</b></td><td><b>Cost Per</b></td><td><b>Setup</b></td></tr>
              <tr><td><asp:Label ID="run_qty1Label" Width="60px" BackColor="Turquoise" runat="server" Text='<%# Bind("[vRun-qty1]") %>' /></td>
              <td><asp:Label ID="run_cost1Label" Width="60px" BackColor="Turquoise" runat="server" Text='<%# Bind("[vRun-cost1]") %>' /></td>
              <td><asp:Label ID="setups1Label" Width="60px" BackColor="Turquoise" runat="server" Text='<%# Bind("vSetups1") %>' /></td></tr>
              <tr><td><asp:Label ID="run_qty2Label" Width="60px" BackColor="Turquoise" runat="server" Text='<%# Bind("[vRun-qty2]") %>' /></td>
              <td><asp:Label ID="run_cost2Label" Width="60px" BackColor="Turquoise" runat="server" Text='<%# Bind("[vRun-cost2]") %>' /></td>
              <td><asp:Label ID="setups2Label" Width="60px" BackColor="Turquoise" runat="server" Text='<%# Bind("vSetups2") %>' /></td></tr>
              <tr><td><asp:Label ID="run_qty3Label" Width="60px" BackColor="Turquoise" runat="server" Text='<%# Bind("[vRun-qty3]") %>' /></td>
              <td> <asp:Label ID="run_cost3Label" Width="60px" BackColor="Turquoise" runat="server" Text='<%# Bind("[vRun-cost3]") %>' /></td>
              <td><asp:Label ID="setups3Label" Width="60px" BackColor="Turquoise" runat="server" Text='<%# Bind("vSetups3") %>' /></td></tr>
              <tr><td><asp:Label ID="run_qty4Label" Width="60px" BackColor="Turquoise" runat="server" Text='<%# Bind("[vRun-qty4]") %>' /></td>
              <td> <asp:Label ID="run_cost4Label" Width="60px" BackColor="Turquoise" runat="server" Text='<%# Bind("[vRun-cost4]") %>' /></td>
              <td><asp:Label ID="setups4Label" Width="60px" BackColor="Turquoise" runat="server" Text='<%# Bind("vSetups4") %>' /></td></tr>
              <tr><td><asp:Label ID="run_qty5Label" Width="60px" BackColor="Turquoise" runat="server" Text='<%# Bind("[vRun-qty5]") %>' /></td>
              <td> <asp:Label ID="run_cost5Label" Width="60px" BackColor="Turquoise" runat="server" Text='<%# Bind("[vRun-cost5]") %>' /></td>
              <td> <asp:Label ID="setups5Label" Width="60px" BackColor="Turquoise" runat="server" Text='<%# Bind("vSetups5") %>' /></td></tr>
              <tr><td><asp:Label ID="run_qty6Label" Width="60px" BackColor="Turquoise" runat="server" Text='<%# Bind("[vRun-qty6]") %>' /></td>
              <td><asp:Label ID="run_cost6Label" Width="60px" BackColor="Turquoise" runat="server" Text='<%# Bind("[vRun-cost6]") %>' /></td>
              <td><asp:Label ID="setups6Label" Width="60px" BackColor="Turquoise" runat="server" Text='<%# Bind("vSetups6") %>' /></td></tr>
              <tr><td> <asp:Label ID="run_qty7Label" Width="60px" BackColor="Turquoise" runat="server" Text='<%# Bind("[vRun-qty7]") %>' /></td>
              <td><asp:Label ID="run_cost7Label" Width="60px" BackColor="Turquoise" runat="server" Text='<%# Bind("[vRun-cost7]") %>' /></td>
              <td> <asp:Label ID="setups7Label" Width="60px" BackColor="Turquoise" runat="server" Text='<%# Bind("vSetups7") %>' /></td></tr>
              <tr><td> <asp:Label ID="run_qty8Label" Width="60px" BackColor="Turquoise" runat="server" Text='<%# Bind("[vRun-qty8]") %>' /></td>
              <td> <asp:Label ID="run_cost8Label" Width="60px" BackColor="Turquoise" runat="server" Text='<%# Bind("[vRun-cost8]") %>' /></td>
              <td><asp:Label ID="setups8Label" Width="60px" BackColor="Turquoise" runat="server" Text='<%# Bind("vSetups8") %>' /></td></tr>
              <tr><td> <asp:Label ID="run_qty9Label" Width="60px" BackColor="Turquoise" runat="server" Text='<%# Bind("[vRun-qty9]") %>' /></td>
              <td><asp:Label ID="run_cost9Label" Width="60px" BackColor="Turquoise" runat="server" Text='<%# Bind("[vRun-cost9]") %>' /></td>
              <td><asp:Label ID="setups9Label" Width="60px" BackColor="Turquoise" runat="server" Text='<%# Bind("vSetups9") %>' /></td></tr>
              <tr><td><asp:Label ID="run_qty10Label" Width="60px" BackColor="Turquoise" runat="server" Text='<%# Bind("[vRun-qty10]") %>' /></td>
              <td><asp:Label ID="run_cost10Label" Width="60px" BackColor="Turquoise" runat="server" Text='<%# Bind("[vRun-cost10]") %>' /></td>
              <td> <asp:Label ID="setups10Label" Width="60px" BackColor="Turquoise" runat="server" Text='<%# Bind("vSetups10") %>' /></td>
              <td><b>GS&A O/H Markup%:</b></td>
              <td><asp:Label ID="vMarkupLabel" Width="60px" BackColor="Turquoise" runat="server" Text='<%# Bind("vMarkup") %>' /></td>
              </tr>
              </table>
              </td>                          
              </tr>
              <tr>
              <td>
              <table><tr><td></td><td><b>Min</b></td><td><b>Max</b></td><td></td><td><b>Min</b></td><td><b>Max</b></td></tr>
               <tr><td align="right" style="padding-right:5px"><b>Sheet Width:</b></td>
              <td> <asp:Label ID="roll_w27Label" Width="60px" BackColor="Turquoise" runat="server" Text='<%# Bind("[vRoll-w1]") %>' /></td>
              <td> <asp:Label ID="roll_w28Label" Width="60px" BackColor="Turquoise" runat="server" Text='<%# Bind("[vRoll-w2]") %>' /></td>
              <td align="right" style="padding-right:5px"><b>Length:</b></td>
              <td><asp:Label ID="roll_w29Label" Width="60px" BackColor="Turquoise" runat="server" Text='<%# Bind("[vRoll-w3]") %>' /></td>
              <td><asp:Label ID="roll_w30Label" Width="60px" BackColor="Turquoise" runat="server" Text='<%# Bind("[vRoll-w4]") %>' /></td></tr>
              </table>
              </td></tr>              
              <tr><td colspan="2" align="left">
               <asp:Label ID="Label1" Visible="false" runat="server" Text='<%# Bind("[vRowid-eitem]") %>' />
                  <asp:Label ID="Label2" Visible="false" runat="server" Text='<%# Bind("[vRowid-vend]") %>' />
                     <asp:Label ID="showLabel" Visible="false" runat="server" Text='<%# Bind("[vShowitem]") %>' ></asp:Label>
                 <asp:Button ID="InsertButton" runat="server" CssClass="button" CausesValidation="True" CommandName="edit" Text="Update" />
                &nbsp;<asp:Button ID="InsertCancelButton" runat="server" CssClass="button"  CausesValidation="False" OnClientClick="javascript:window.opener.location.href='corr_vendor_cost.aspx'; self.close()" Text="Close" />
              </td></tr>
              </table>
              </fieldset>
                
               
            </ItemTemplate>
        </asp:FormView>
        <asp:ObjectDataSource ID="ObjectDataSource1" runat="server" OldValuesParameterFormatString="original_{0}"
         SelectMethod="SelectVendFGItem" TypeName="Corrugated">
            <SelectParameters>
                    
                <asp:Parameter DefaultValue="View" Name="prmAction" Type="String" />
            <asp:Parameter Name="prmUser" Type="String" />
                    
                <asp:SessionParameter SessionField="corr_vendor_cost_est" Name="prmEst" Type="String" />
                <asp:SessionParameter SessionField="corr_vendor_cost_form" Name="prmFormNo" Type="Int32" />
                <asp:SessionParameter SessionField="corr_vendor_cost_blank" Name="prmBlanck" Type="Int32" />                 
                <asp:SessionParameter  Name="prmItem" 
                    SessionField="corr_vendor_cost_item_get" Type="String" />
                <asp:Parameter Name="prmStdum" Type="String" />
                <asp:Parameter Name="prmVendItem" Type="String" />
                <asp:Parameter Name="prmVendNO" Type="String" />
                <asp:Parameter Name="prmCust" Type="String" />
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
                <asp:Parameter Name="prmRowid1" Type="String" />
                <asp:Parameter Name="prmRowid2" Type="String" />
                <asp:Parameter Name="prmMarkup" Type="Decimal" />
                
                <asp:Parameter Name="prmTbSel" Type="String" />
                <asp:Parameter Name="prmTbSel1" Type="String" />
                <asp:Parameter Name="prmTbSel2" Type="String" />
                <asp:Parameter Name="prmTbSel3" Type="String" />
                <asp:Parameter Name="prmTbSel4" Type="String" />
                <asp:Parameter Name="prmTbSel5" Type="String" />
                <asp:Parameter Name="prmTbSel6" Type="String" />
                <asp:Parameter Name="prmTbSel7" Type="String" />
                <asp:Parameter Name="prmTbSel8" Type="String" />
                <asp:Parameter Name="prmTbSel9" Type="String" />
                <asp:Parameter Name="prmTbSel10" Type="String" />
                              
                                
                 
            </SelectParameters>
        </asp:ObjectDataSource>
    </div>
    </form>
</body>
</html>
