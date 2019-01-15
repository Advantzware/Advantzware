<%@ Page Language="C#" AutoEventWireup="true" Debug="true" Inherits="view_item_popup" Codebehind="view_item_popup.aspx.cs" %>

<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">

<html xmlns="http://www.w3.org/1999/xhtml" >
<head runat="server">
    <title>View Item Entry</title>
    <LINK href="include/style.css" type="text/css" rel="stylesheet"/>  
    <LINK REL="stylesheet" TYPE="text/css" HREF="include/CalendarControl.css" >
    <script language="javascript"  src="include/CalendarControl.js" > </script>
    <script language="javascript" src="include/date.js"></script>
    <script language="javascript" src="include/event.js"></script>
    <script language="javascript" src="include/insert.js"></script>

    <script type="text/javascript" language="javascript">
        function focusval(obj) {
            obj.style.backgroundColor = 'blue';
            obj.style.color = 'white';
        }
        function blurval(obj) {
            obj.style.backgroundColor = 'Window';
            obj.style.color = 'WindowText';
        }

        function grater(obj) {
            obj.style.backgroundColor = 'Window';
            obj.style.color = 'WindowText';
            var caldis = 0;     
            
            var val = document.forms[0].FormView1_quantityTextBox;
            if (val.value <= 0) {
                alert("Quantity must be greater than 0");
                val.focus();
                return false;
            }
            else {                
                var discount = document.forms[0].FormView1_discountTextBox;
                var uom = document.forms[0].FormView1_TextBox1;
                var casunit = document.forms[0].FormView1_counterTextBox;
                var hidtot = document.forms[0].Hiddentotal;
                var price = document.forms[0].FormView1_priceTextBox;
                var total = document.forms[0].FormView1_extpriceTextBox;
                              
                if (document.forms[0].FormView1_TextBox1) {                                      
                    if (uom.value == "CS" || uom.value == "cs" && casunit.value != 0) {
                        total.value = price.value * val.value / casunit.value;
                        if (discount.value >= 1) {
                            caldis = total.value * discount.value / 100;
                            total.value = total.value - caldis;
                        }
                        hidtot.value = total.value;
                        uom.value = "CS";
                    }
                    else if (uom.value == "C" || uom.value == "c") {
                        total.value = price.value * val.value / 100;
                        if (discount.value >= 1) {
                            caldis = total.value * discount.value / 100;
                            total.value = total.value - caldis;
                        }
                        hidtot.value = total.value;
                        uom.value = "C";
                    }
                    else if (uom.value == "M" || uom.value == "m") {                   
                        total.value = price.value * val.value / 1000;
                        if (discount.value >= 1) {
                            caldis = total.value * discount.value / 100;
                            total.value = total.value - caldis;
                        }
                        hidtot.value = total.value;
                        uom.value = "M";
                    }
                    else {
                        total.value = price.value * val.value;
                        if (discount.value >= 1) {
                            caldis = total.value * discount.value / 100;
                            total.value = total.value - caldis;
                        }
                        hidtot.value = total.value;
                    }
                }
                else {
                    var hiduom = document.forms[0].HiddenTextBox1;
                    var hidcounter = document.forms[0].Hiddencounter;
                    if (hiduom.value == "CS" || hiduom.value == "cs" && hidcounter.value != 0) {
                        total.value = price.value * val.value / hidcounter.value;
                        if (discount.value >= 1) {
                            caldis = total.value * discount.value / 100;
                            total.value = total.value - caldis;
                        }
                        hidtot.value = total.value;
                    }
                    else if (hiduom.value == "C" || hiduom.value == "c") {
                        total.value = price.value * val.value / 100;
                        if (discount.value >= 1) {
                            caldis = total.value * discount.value / 100;
                            total.value = total.value - caldis;
                        }
                        hidtot.value = total.value;
                    }
                    else if (hiduom.value == "M" || hiduom.value == "m") {
                        total.value = price.value * val.value / 1000;
                        if (discount.value >= 1) {
                            caldis = total.value * discount.value / 100;
                            total.value = total.value - caldis;
                        }
                        hidtot.value = total.value;
                    }
                    else {
                        total.value = price.value * val.value;
                        if (discount.value >= 1) {
                            caldis = total.value * discount.value / 100;
                            total.value = total.value - caldis;
                        }
                        hidtot.value = total.value;
                    }
                }
                var dotpr = price.value;
                if (dotpr.indexOf(".") != -1) {
                    return;
                }
                else if (dotpr.length > 7 && dotpr.length < 9)
                    dotpr = dotpr + ".";
                document.forms[0].FormView1_priceTextBox.value = dotpr;
            }         
        }
        

        function validatelimit(obj, maxchar) {

            if (this.id) obj = this;

            var remaningChar = maxchar - obj.value.length; 

            if (remaningChar <= 0) {
                obj.value = obj.value.substring(maxchar, 0);
                alert('Character Limit exceeds!');
                return false;

            }
            else
            { return true; }
        }

        function PriceLook() {

            var item1 = document.getElementById("FormView1_Item1TextBox").value;
            var uom1 = document.getElementById("FormView1_TextBox1").value;
            var NewWindow = window.open("price_lookup.aspx?item=" + item1 + "&uom=" + uom1 + "", "typeordLookupWindow", "width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
        }
        function PriceLookup(ReturnObj1) {
            var qty = document.getElementById("FormView1_quantityTextBox");
            var discount = document.getElementById("FormView1_discountTextBox");
            var hidtot = document.getElementById("Hiddentotal");
            var total = document.getElementById("FormView1_extpriceTextBox");
            var uom = document.getElementById("FormView1_TextBox1");

            var countprice = 0;
            var caldis = 0;
            if (uom.value == "M") {

                countprice = ReturnObj1 * 1000;

                document.forms[0].FormView1_priceTextBox.value = countprice.toFixed(4);
                document.forms[0].Hiddenprice.value = countprice.toFixed(4);
                document.getElementById('FormView1_priceTextBox').focus();


                total.value = (countprice * qty.value / 1000).toFixed(4);
                if (discount.value >= 1) {
                    caldis = total.value * discount.value / 100;
                    total.value = total.value - caldis;
                }
                hidtot.value = total.value;

            }
            else if (uom.value == "C") {
                countprice = ReturnObj1 * 100;
                document.forms[0].FormView1_priceTextBox.value = countprice.toFixed(4);
                document.forms[0].Hiddenprice.value = countprice.toFixed(4);
                document.getElementById('FormView1_priceTextBox').focus();

                total.value = (countprice * qty.value / 100).toFixed(4);
                if (discount.value >= 1) {
                    caldis = total.value * discount.value / 100;
                    total.value = total.value - caldis;
                }
                hidtot.value = total.value;

            }
            else {
                document.forms[0].FormView1_priceTextBox.value = ReturnObj1;
                document.forms[0].Hiddenprice.value = ReturnObj1;
                document.getElementById('FormView1_priceTextBox').focus();

                total.value = (qty.value * ReturnObj1).toFixed(4);
                if (discount.value >= 1) {
                    caldis = total.value * discount.value / 100;
                    total.value = total.value - caldis;
                }
                hidtot.value = total.value;
            }



        }
        function QuantityLook() {
            if (document.getElementById("FormView1_vQnoTextBox")) {
                var est1 = document.getElementById("FormView1_vQnoTextBox").value;
            }
            else {
                var est1 = document.getElementById("FormView1_vQnoLabel").innerText;
            }
            var item1 = document.getElementById("FormView1_Item1TextBox").value;
            var custpart1 = document.getElementById("FormView1_CustPartTextBox").value;
            var NewWindow = window.open("item_quantity_lookup.aspx?est=" + est1 + "&item=" + item1 + "&custpart=" + custpart1 + "", "typeordLookupWindow", "width=500,height=300,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
        }
        function QuantityLookup(ReturnObj1, ReturnObj2, ReturnObj3) {
            document.forms[0].FormView1_quantityTextBox.value = ReturnObj1;
            document.forms[0].Hiddenquantity.value = ReturnObj1;
            document.forms[0].FormView1_priceTextBox.value = ReturnObj2;
            document.forms[0].Hiddenprice.value = ReturnObj2;
            document.forms[0].FormView1_TextBox1.value = ReturnObj3;
            document.forms[0].HiddenTextBox1.value = ReturnObj3;
            document.getElementById('FormView1_quantityTextBox').focus();

            var discount = document.getElementById("FormView1_discountTextBox");
            var hidcounter = document.getElementById("Hiddencounter");
            var hidtot = document.getElementById("Hiddentotal");
            var total = document.getElementById("FormView1_extpriceTextBox");
            var casunit = document.getElementById("FormView1_counterTextBox");

            if (ReturnObj3 == "CS" || ReturnObj3 == "cs" && casunit.value != 0) {
                total.value = ReturnObj2 * ReturnObj1 / casunit.value;
                if (discount.value >= 1) {
                    caldis = total.value * discount.value / 100;
                    total.value = total.value - caldis;
                }
                hidtot.value = total.value;
            }
            else if (ReturnObj3 == "C" || ReturnObj3 == "c") {
                total.value = ReturnObj2 * ReturnObj1 / 100;
                if (discount.value >= 1) {
                    caldis = total.value * discount.value / 100;
                    total.value = total.value - caldis;
                }
                hidtot.value = total.value;
            }
            else if (ReturnObj3 == "M" || ReturnObj3 == "m") {
                total.value = ReturnObj2 * ReturnObj1 / 1000;
                if (discount.value >= 1) {
                    caldis = total.value * discount.value / 100;
                    total.value = total.value - caldis;
                }
                hidtot.value = total.value;
            }
            else {
                total.value = ReturnObj2 * ReturnObj1;
                if (discount.value >= 1) {
                    caldis = total.value * discount.value / 100;
                    total.value = total.value - caldis;
                }
                hidtot.value = total.value;
            }
            if (document.getElementById("FormView1_Item1TextBox").value != "") {
                if (document.getElementById("FormView1_Item1TextBox").disabled != true) {
                    var fgitem = document.getElementById("FormView1_Item1TextBox");
                    fgitem.focus();
                }
                else if (document.getElementById("FormView1_quantityTextBox")) {
                    var qty = document.getElementById("FormView1_quantityTextBox");
                    qty.focus();
                }
            }
            else {
                if (document.getElementById("FormView1_Item1TextBox").disabled != true) {
                    var fgitem = document.getElementById("FormView1_Item1TextBox");
                    fgitem.focus();
                }
                else {
                    var custpart = document.getElementById("FormView1_CustPartTextBox");
                    custpart.focus();
                }
            }
        }
        function custpartlook()
 { 
    var fgitem = document.getElementById("FormView1_Item1TextBox").value;
    var NewWindow = window.open("custpart_lookup.aspx?item="+fgitem+"","CustPartLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
 }

 function CustPartLookup(ReturnObj1, ReturnObj2, ReturnObj3, ReturnObj4, ReturnObj5, ReturnObj6, ReturnObj7, ReturnObj8, ReturnObj9, ReturnObj10, ReturnObj11, ReturnObj12) {
     if (ReturnObj1.indexOf(":")) {
         var val = ReturnObj1;
         ReturnObj1 = val.replace(":", "\"");
     }

     document.forms[0].FormView1_CustPartTextBox.value = ReturnObj1;
     document.forms[0].Hiddencustpart.value = ReturnObj1;
     document.getElementById('FormView1_CustPartTextBox').focus();
 }
 function BoardPoLook() {
     var NewWindow = window.open("boardpo_lookup.aspx", "BoardPoLookupWindow", "width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
 }
 function BoardPoLookup(ReturnObj1, ReturnObj2) {
     document.forms[0].FormView1_vPonoTextBox.value = ReturnObj1;
     document.forms[0].FormView1_vBoardVenTextBox.value = ReturnObj2;
     document.getElementById('FormView1_vPonoTextBox').focus();
 }

 function salesreplook() {
     var NewWindow = window.open("salesrep_lookup.aspx", "SalesRepLookupWindow", "width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
 }
 function SalesRepLookup(ReturnObj1, ReturnObj2) {
     document.forms[0].FormView1_vSmanTextBox.value = ReturnObj1;
     document.forms[0].Hiddensman.value = ReturnObj1;
     document.forms[0].FormView1_vSnameTextBox.value = ReturnObj2;
     document.forms[0].Hiddensname.value = ReturnObj2;
     document.getElementById('FormView1_vSmanTextBox').focus();
 }

 function smancopylook1() {
     var NewWindow = window.open("sman_copylookup.aspx", "smancopyLookupWindow", "width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
 }
 function smancopyLookup(ReturnObj1, ReturnObj2) {
     document.forms[0].FormView1_vSman2TextBox.value = ReturnObj1;
     document.forms[0].Hiddensman2.value = ReturnObj1;
     document.forms[0].FormView1_vSname2TextBox.value = ReturnObj2;
     document.forms[0].Hiddensname2.value = ReturnObj2;
     document.getElementById('FormView1_vSman2TextBox').focus();
 }

 function salesmanlook() {
     var NewWindow = window.open("salesman_lookup.aspx", "SalesManLookupWindow", "width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
 }
 function smancopyLookup1(ReturnObj1, ReturnObj2) {
     document.forms[0].FormView1_vSman3TextBox.value = ReturnObj1;
     document.forms[0].Hiddensman3.value = ReturnObj1;
     document.forms[0].FormView1_vSname3TextBox.value = ReturnObj2;
     document.forms[0].Hiddensname3.value = ReturnObj2;
     document.getElementById('FormView1_vSman3TextBox').focus();
 }
 var val = "";

 function uomlook() {
     val = "p";
     var NewWindow = window.open("Uom_lookup.aspx", "UomLookupWindow", "width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
 }
 function uomqtylook() {
     val = "q";
     var NewWindow = window.open("Uom_lookup.aspx", "UomLookupWindow", "width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
 }
 function UomLookup(ReturnObj1, ReturnObj2) {
     if (val == "p") {
         document.forms[0].FormView1_TextBox1.value = ReturnObj1;
         document.forms[0].HiddenTextBox1.value = ReturnObj1;
         // document.forms[0].FormView1_vSname3TextBox.value = ReturnObj2;
         document.getElementById('FormView1_TextBox1').focus();
     }
     else if (val == "q") {
     document.forms[0].FormView1_vTypeTextBox.value = ReturnObj1;
     document.forms[0].FormView1_vTypeTextBox.focus();     
     }
 } 

 function getQty() {          
     if ((document.forms[0].FormView1_vTypeTextBox.value).toUpperCase() == "EA") {            
            document.forms[0].FormView1_quantityTextBox.value = parseInt(document.forms[0].FormView1_quantityTextBox.value) * 1; 
            document.forms[0].FormView1_vTypeTextBox.value = "EA";
        }
        if ((document.forms[0].FormView1_vTypeTextBox.value).toUpperCase() == "C") {
            document.forms[0].FormView1_quantityTextBox.value = parseInt(document.forms[0].FormView1_quantityTextBox.value) * 100;
            document.forms[0].FormView1_extpriceTextBox.value = parseInt(document.forms[0].FormView1_priceTextBox.value) * parseInt(document.forms[0].FormView1_quantityTextBox.value);
            document.forms[0].Hiddentotal.value = parseInt(document.forms[0].FormView1_priceTextBox.value) * parseInt(document.forms[0].FormView1_quantityTextBox.value);
            document.forms[0].FormView1_vTypeTextBox.value = "EA";
        }
        if ((document.forms[0].FormView1_vTypeTextBox.value).toUpperCase() == "M") {
            document.forms[0].FormView1_quantityTextBox.value = parseInt(document.forms[0].FormView1_quantityTextBox.value) * 1000;
            document.forms[0].FormView1_extpriceTextBox.value = parseInt(document.forms[0].FormView1_priceTextBox.value) * parseInt(document.forms[0].FormView1_quantityTextBox.value);
            document.forms[0].Hiddentotal.value = parseInt(document.forms[0].FormView1_priceTextBox.value) * parseInt(document.forms[0].FormView1_quantityTextBox.value);
            document.forms[0].FormView1_vTypeTextBox.value = "EA";
        }
        document.forms[0].FormView1_vTypeTextBox.style.color = "white";
    }
    function checkQty() {
        var val = document.forms[0].FormView1_quantityTextBox;
        if (val.value <= 0) {
            alert("Quantity must be greater than 0");
            val.focus();
            return false;
        }
    }

    </script>   
</head>
<body>
    <form id="form1" runat="server">
    
    <asp:HiddenField ID="HiddenField4" runat="server"></asp:HiddenField>
    <asp:HiddenField ID="HiddenField5" runat="server"></asp:HiddenField>
    <asp:HiddenField ID="HiddenFielditem" runat="server"></asp:HiddenField>
    <asp:HiddenField ID="HiddenFieldname" runat="server"></asp:HiddenField>
    <asp:HiddenField ID="Hiddencustpart" runat="server"></asp:HiddenField>
    <asp:HiddenField ID="Hiddencustdesc" runat="server"></asp:HiddenField>
    <asp:HiddenField ID="Hiddencustdesc2" runat="server"></asp:HiddenField>
    <asp:HiddenField ID="Hiddenprice" runat="server"></asp:HiddenField>
    <asp:HiddenField ID="HiddenTextBox1" runat="server"></asp:HiddenField>
    <asp:HiddenField ID="Hiddencounter" runat="server"></asp:HiddenField>
    <asp:HiddenField ID="Hiddentotal" runat="server"></asp:HiddenField>
    <asp:HiddenField ID="Hiddencost" runat="server"></asp:HiddenField>
    <asp:HiddenField ID="HiddenDropdown1" runat="server"></asp:HiddenField>
    <asp:HiddenField ID="Hiddenestno" runat="server"></asp:HiddenField>
    <asp:HiddenField ID="Hiddenjob" runat="server"></asp:HiddenField>
    <asp:HiddenField ID="Hiddenjob2" runat="server"></asp:HiddenField>
    <asp:HiddenField ID="Hiddenquantity" runat="server"></asp:HiddenField>
    <asp:HiddenField ID="Hiddencasunit" runat="server"></asp:HiddenField>
    <asp:HiddenField ID="Hiddenspct" runat="server"></asp:HiddenField>
    <asp:HiddenField ID="Hiddensman" runat="server"></asp:HiddenField>
    <asp:HiddenField ID="Hiddencomm" runat="server"></asp:HiddenField>
    <asp:HiddenField ID="Hiddensname" runat="server"></asp:HiddenField>
    <asp:HiddenField ID="Hiddendiscount" runat="server"></asp:HiddenField>
    <asp:HiddenField ID="Hiddenover" runat="server"></asp:HiddenField>
    <asp:HiddenField ID="Hiddenunder" runat="server"></asp:HiddenField>
    <asp:HiddenField ID="Hiddentax" runat="server"></asp:HiddenField>
    <asp:HiddenField ID="Hiddensman2" runat="server"></asp:HiddenField>
    <asp:HiddenField ID="Hiddensname2" runat="server"></asp:HiddenField>
    <asp:HiddenField ID="Hiddensman3" runat="server"></asp:HiddenField>
    <asp:HiddenField ID="Hiddensname3" runat="server"></asp:HiddenField>
    <asp:HiddenField ID="HiddenEstmate" runat="server"></asp:HiddenField>
    <asp:HiddenField ID="HiddenQuoteNum" runat="server"></asp:HiddenField>
    
    <div>
        <TABLE id="tblTop" cellSpacing="3" border="0" Width="100%">
            <TR>            
                <TD nowrap><font size=+0><b>View Item Entry &nbsp;</b></font></TD>                       
            </TR>
        </TABLE>
        <asp:HiddenField ID="HiddenField1" runat="server" />
        <asp:HiddenField ID="HiddenField2" runat="server" />
        <asp:HiddenField ID="HiddenField3" runat="server" />
        
            
    <asp:Label ID="lbl_error" runat="server" Font-Bold="true" ForeColor="red"></asp:Label>
       


<asp:FormView ID="FormView1" Font-Bold="true" runat="server" DataSourceID="ObjectDataSource1" OnDataBound="FormView1_DataBound" OnUnload="FormView1_Unload">
        <EditItemTemplate>
            <asp:Panel ID="update_panel" runat="server" DefaultButton="UpdateButton">
                <table width="800px">
                    <tr>
                        <td>
                            <table width="800px" class="shade">
                                <tr>
                                    <td nowrap align="right" style="padding-top:5px;">Order#:</td>
                                    <td nowrap>
                                        <asp:Label ID="ord_label" runat="server" BackColor="turquoise" Width="80px" BorderColor="white" BorderStyle="solid" BorderWidth="1px"></asp:Label>
                                    </td>
                                    <td nowrap align="right" style="padding-top:5px;" >Cust#:</td>
                                    <td nowrap>
                                        <asp:Label ID="cust_label" runat="server" Width="100px" BackColor="turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px"></asp:Label>
                                    </td>                                   
                                    <td align="right" style="padding-right:5px"><b>Quote:</b></td>
                                     <td>
                                       <asp:TextBox ID="vQnoTextBox" Width="70px" ReadOnly="true" BackColor="turquoise" runat="server" Text='<%# Bind("[vQno]") %>'></asp:TextBox>                                      
                                       
                                       </td>
                                    <td align="right" style="padding-right:5px"><b>Job Number:</b></td>
                                    <td><b>
                                        <asp:Label ID="job_noTextBox" Width="70px" runat="server" Text='<%# Bind("[job-no]") %>'></asp:Label>
                                        <asp:Label ID="Jobno2Label" Width="30px"  runat="server" Text='<%# Bind("[job-no2]") %>'></asp:Label>
                                     </b></td>
                                    <td></td>
                                    <td></td>
            
                                    <td align="right" style="padding-right:5px"><b>
                                        <asp:Label ID="Manlabel" runat="server" Text="Managed Inventory"></asp:Label></b></td><td><asp:CheckBox ID="vManagCheckBox" runat="server" Checked='<%# Bind("vManag") %>' />
                                    </td>
                                    <td align="right" style="padding-right:5px"><b><asp:Label ID="orlabel" runat="server"></asp:Label> </b></td>            
                            </tr>
                    </table>
                </td>
            </tr>            
            <tr>
                <td>
                    <table>
                        <tr>
                            <td>
                                <fieldset style="width:280px; border:solid 1px black; background-color:#EFF3FB; height:140px;">
                                    <table>
                                        <tr>
                                            <td align="right" style="padding-right:5px"><b>FG Item:</b></td>
                                            <td nowrap><b>
                                                <asp:TextBox ID="Item1TextBox" Enabled="false" MaxLength="30" Width="170px" onfocus= "javascript:focusval(this)" onblur="setfocus(this)"   runat="server" TabIndex="0" Text='<%# Bind("Item1") %>'></asp:TextBox></b>                                                
                                            </td>
                                        </tr>
                                        <tr>
                                            <td align="right" style="padding-right:5px"><b>Cust Part#:</b></td>
                                            <td nowrap><b>
                                                <asp:TextBox ID="CustPartTextBox" MaxLength="30" Width="170px" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" runat="server" AutoPostBack="true" OnTextChanged="custpart_Click" Text='<%# Bind("CustPart") %>'></asp:TextBox></b>
                                                <a href="#" tabindex="1" onClick="custpartlook(); return false"><asp:Image ID="CustPartLookup" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
                                             </td>
                                        </tr>
                                        <tr>
                                            <td align="right" style="padding-right:5px"><b>Item Name:</b></td>
                                            <td>
                                                <asp:TextBox ID="Name1TextBox" MaxLength="30" Width="170px" onfocus= "javascript:focusval(this)" onblur="javascript:inameblurval(this)" runat="server" Text='<%# Bind("Name1") %>'></asp:TextBox>
                                            </td>
                                            
                                        </tr>
                                        <tr>
                                            <td align="right" style="padding-right:5px"><b>Description:</b></td>
                                            <td>
                                                <asp:TextBox ID="DscrTextBox" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" MaxLength="30" Width="170px" runat="server" Text='<%# Bind("Dscr") %>'></asp:TextBox>
                                            </td>
                                        </tr>
                                        <tr>
                                            <td align="right" style="padding-right:5px"><b>Description:</b></td>
                                            <td>
                                                <asp:TextBox ID="Dscr2TextBox" MaxLength="30" Width="170px" onfocus= "javascript:focusval(this)" onblur="javascript:descblurval(this)" runat="server" Text='<%# Bind("Dscr2") %>'></asp:TextBox>
                                            </td>
                                        </tr>
                                    </table>
                                 </fieldset>
                               </td>
                               <td>
                                <fieldset style="width:260px; border:solid 1px black; background-color:#EFF3FB; height:140px;">
                                    <table>
                                        <tr>
                                            <td style="display:none">
                                                <asp:Label ID="Label_rec_key" runat="server" Visible="false" Text='<%# Bind("vReckey") %>'></asp:Label>                                            
                                                <asp:Label ID="EstNumLabel" runat="server" Text='<%# Eval("est-no") %>'></asp:Label>
                                                <asp:Label ID="vlineLabe" Visible="false" runat="server" Text='<%# Bind("vLine") %>'></asp:Label>
                                            </td>
                                            <td nowrap align="right"  style="padding-right:5px"><b>Quantity:</b></td>
                                            <td nowrap>
                                                <asp:TextBox ID="quantityTextBox" MaxLength="9" onfocus= "javascript:focusval(this)"  onblur="javascript:blurval(this)" Width="60px" runat="server" AutoPostBack="true" OnTextChanged="qtytextchange" Text='<%# Bind("quantity") %>'></asp:TextBox>
                                                <a href="#" tabindex="1" tabindex="1" onClick="QuantityLook(); return false"><asp:Image ID="Image11" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
                                                <asp:TextBox ID="vTypeTextBox" Width="20px" onfocus= "javascript:focusval(this); checkQty();" onblur="getQty();" runat="server" Text=''></asp:TextBox>
                                                <a href="#" tabindex="1" tabindex="1" onClick="uomqtylook(); return false"><asp:Image ID="Image8" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
                                                
                                                <asp:RequiredFieldValidator ID="RequiredFieldValidator1" ControlToValidate="quantityTextBox" Display="dynamic" runat="server" SetFocusOnError="true" ErrorMessage="Enter The Quantity"></asp:RequiredFieldValidator>
                                                <asp:RangeValidator ID="RangeValidator1" ControlToValidate="quantityTextBox" Display="Dynamic" Type="Integer" MinimumValue="1" MaximumValue="999999999" runat="server" ErrorMessage="Quantity must be greater than 0"></asp:RangeValidator>
                                            </td>
                                            
                                        </tr>                                      
                                        <tr>
                                            <td align="right" style="padding-right:5px"><b>Price:</b></td>
                                            <td nowrap>
                                                <asp:TextBox ID="priceTextBox" MaxLength="11" Width="100px" onkeyup="grater(this);" onfocus= "javascript:focusval(this); grater(this);" onblur="grater(this)" runat="server" Text='<%# Bind("price") %>'></asp:TextBox>
                                                <a href="#" tabindex="1" onClick="PriceLook(); return false"><asp:Image ID="Image12" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
                                                <%--<asp:TextBox ID="TextBox1" Width="20px" Enabled="false" onfocus= "javascript:focusval(this)" onblur="grater(this)" runat="server" Text='<%# Bind("uom") %>'></asp:TextBox>--%>
                                                <asp:TextBox ID="TextBox1" Width="20px" onfocus= "javascript:focusval(this); grater(this);" onblur="grater(this)" runat="server" Text='<%# Bind("uom") %>'></asp:TextBox>
                                                <a href="#" tabindex="1" onClick="uomlook(); return false"><asp:Image ID="Image5" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
                                                <asp:CompareValidator ID="CompareValidator2" runat="server" ErrorMessage="Only Numbers" ControlToValidate="priceTextBox" Display="dynamic" SetFocusOnError="true" Operator="datatypecheck" Type="double"></asp:CompareValidator>
                                            </td>
                                        </tr>
                                        <tr>
                                            <td align="right" style="padding-right:5px"><b>Discount:</b></td>
                                            <td>
                                                <asp:TextBox ID="discountTextBox" MaxLength="2" Width="30px" onfocus= "javascript:focusval(this)" onblur="grater(this)" runat="server" Text='<%# Bind("discount") %>'></asp:TextBox>
                                                <asp:CompareValidator ID="DiscountCompareValidator" runat="server" ErrorMessage="Only Numbers" ControlToValidate="discountTextBox" Display="dynamic" SetFocusOnError="true" Operator="datatypecheck" Type="double"></asp:CompareValidator>
                                            </td>
                                        </tr>
                                        <tr>
                                            <td align="right" style="padding-right:5px"><b>Total Price:</b></td>
                                            <td>
                                                <asp:TextBox ID="extpriceTextBox" Width="100px" Enabled="false" runat="server" Text='<%# Bind("extprice") %>'></asp:TextBox>
                                                <asp:CompareValidator ID="CompareValidator3" runat="server" ErrorMessage="Only Numbers" ControlToValidate="extpriceTextBox" Display="dynamic" SetFocusOnError="true" Operator="datatypecheck" Type="double"></asp:CompareValidator>
                                            </td>
                                        </tr>
                                        <tr>
                                            <td align="right" style="padding-right:5px"><b><asp:Label ID="cost1Label" runat="server" Text="Cost/M:"></asp:Label></b></td>
                                            <td>
                                                <asp:TextBox ID="CostTextBox" Width="100px" Enabled="false" runat="server" Text='<%# Bind("vCost") %>'></asp:TextBox>
                                                <asp:CompareValidator ID="CompareValidator20" runat="server" ErrorMessage="Only Numbers" ControlToValidate="CostTextBox" Display="dynamic" SetFocusOnError="true" Operator="datatypecheck" Type="double"></asp:CompareValidator>
                                            </td>
                                        </tr>
                                        
                                    </table>
                                 </fieldset>
                              </td>
                              <td>
                                <fieldset style="width:260px; border:solid 1px black; background-color:#EFF3FB; height:140px;">
                                    <table>
                                    <tr></tr>
                                        <tr>
                                            <td>&nbsp;</td>
                                            <td>
                                                <asp:DropDownList ID="DropDownList1" Width="100px" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" SelectedValue='<%# Bind("vType") %>' DataTextField='<%# Bind("vType") %>' runat="server">
                                                    <asp:ListItem Value="O">O- Original</asp:ListItem>
                                                    <asp:ListItem Value="C">C- Change</asp:ListItem>
                                                    <asp:ListItem Value="N">N- New</asp:ListItem>
                                                    <asp:ListItem Value="Q">Q- Quality/Re-work</asp:ListItem>
                                                    <asp:ListItem Value="R">R- Repeat</asp:ListItem>
                                                    <asp:ListItem Value="T">T- Transfer</asp:ListItem>
                                                    <asp:ListItem Value="X">X- Complete re-run</asp:ListItem>
                                                    <asp:ListItem Value=""></asp:ListItem>
                                                </asp:DropDownList>
                                            </td>
                                        </tr>
                                        <tr>                                            
                                            <td colspan="2"> &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;               
                                                <b>Tax:&nbsp;&nbsp;</b><asp:CheckBox ID="taxableCheckBox" runat="server" Checked='<%# Bind("taxable") %>' />
                                            </td>
                                        </tr>
                                        
                                        <tr>
                                            <td align="right" style="padding-right:5px"><b><asp:Label ID="qtyunitLabel"  runat="server" Text="Qty/Unit:"></asp:Label></b></td>
                                            <td>
                                                <asp:TextBox ID="counterTextBox" MaxLength="6" Width="100px" onfocus= "javascript:focusval(this)" onblur="grater(this)" runat="server" Text='<%# Bind("counter") %>'></asp:TextBox>
                                                <asp:CompareValidator ID="QtyCompareValidator" runat="server" ErrorMessage="Only Numbers" ControlToValidate="counterTextBox" Display="dynamic" SetFocusOnError="true" Operator="datatypecheck" Type="double"></asp:CompareValidator>
                                            </td>
                                        </tr>
                                        
                                        <tr>
                                            <td align="right" style="padding-right:5px"><b><asp:Label ID="partLabel" runat="server" Text="Partial:"></asp:Label></b></td>
                                            <td>
                                                <asp:TextBox ID="vPartialTextBox" MaxLength="7" Width="100px" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" runat="server" Text='<%# Bind("vPartial") %>'></asp:TextBox>
                                                <asp:CompareValidator ID="PartialCompareValidator" runat="server" ErrorMessage="Only Numbers" ControlToValidate="vPartialTextBox" Display="dynamic" SetFocusOnError="true" Operator="datatypecheck" Type="double"></asp:CompareValidator>
                                            </td>
                                        </tr>
                                        <tr>
                                            <td align="right" style="padding-right:5px"><b><asp:Label ID="unitpalletLabel" runat="server" Text="Units/Pallet:"></asp:Label></b></td>
                                            <td>
                                                <asp:TextBox ID="VcasUnitTextBox" MaxLength="4" Width="100px" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" runat="server" Text='<%# Bind("VcasUnit") %>'></asp:TextBox>
                                                <asp:CompareValidator ID="UnitsCompareValidator" runat="server" ErrorMessage="Only Numbers" ControlToValidate="VcasUnitTextBox" Display="dynamic" SetFocusOnError="true" Operator="datatypecheck" Type="double"></asp:CompareValidator>
                                            </td>
                                        </tr>                                       
                                     </table>
                                  </fieldset>
                               </td>
                             </tr>
                           </table>
                         </td>
                       </tr>
                       <tr>
                            <td>
                                <table>
                                    <tr>
                                        <td>
                                             <fieldset style="width:400px; border:solid 1px black; background-color:#EFF3FB; height:70px;">
                                                <table>
                                                    <tr>
                                                        <td align="right" style="padding-right:5px"><b>Cust Po#:</b></td>
                                                        <td nowrap>
                                                            <asp:TextBox ID="custpoTextBox" MaxLength="15" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)"  runat="server" Text='<%# Bind("custpo") %>'></asp:TextBox>
                                                        </td>
                                                        <td align="right" style="padding-right:5px"><b>Ln#:</b></td>
                                                        <td>
                                                            <asp:TextBox ID="vLineTextBox" Width="50px" MaxLength="3" onfocus= "javascript:focusval(this)" onblur="setfocus4(this)" runat="server" Text='<%# Bind("vEnum") %>'></asp:TextBox>
                                                            <asp:RegularExpressionValidator ID="RegularExpressionValidator1" runat="server" SetFocusOnError="true" ControlToValidate="vLineTextBox" ErrorMessage="Only Integer Number" Display="Dynamic" ValidationExpression="(^([0-9]*|\d*\d{1}?\d*)$)"></asp:RegularExpressionValidator> 
                                                        </td>
                                                     </tr>
                                                     <tr>
                                                        <td nowrap align="right" style="padding-right:5px"><b><asp:Label ID="bpolabel" runat="server" Text="Board PO#:"></asp:Label></b></td>
                                                        <td>
                                                            <asp:TextBox ID="vPonoTextBox" MaxLength="6" Width="80px" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" runat="server" Text='<%# Bind("vPono") %>'></asp:TextBox>
                                                            <a href="#" tabindex="1" onClick="BoardPoLook(); return false"><asp:Image ID="Image3" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
                                                            <asp:RegularExpressionValidator ID="RegularExpressionValidator2" runat="server" SetFocusOnError="true" ControlToValidate="vPonoTextBox" ErrorMessage="Only  Number" Display="Dynamic" ValidationExpression="(^([0-9]*|\d*\d{1}?\d*)$)"></asp:RegularExpressionValidator>
                                                        </td>
                                                        <td nowrap align="right" style="padding-right:5px"><b><asp:Label ID="bvendlabel" runat="server" Text="Board Vendor#:"></asp:Label></b></td>
                                                        <td>
                                                            <asp:TextBox ID="vBoardVenTextBox"  Width="80px" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" runat="server" Text='<%# Bind("vBoardVen") %>'></asp:TextBox>
                                                        </td>
                                                     </tr>
                                                  </table>
                                               </fieldset>
                                             </td>
                                             <td>
                                                <fieldset style="width:400px; border:solid 1px black; background-color:#EFF3FB; height:70px;">
                                                    <table>
                                                        <tr>
                                                            <td nowrap align="right" style="padding-right:5px"><b>Priority:</b>
                                                                <asp:DropDownList ID="DropDownList2" Width="100px" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" SelectedValue='<%# Bind("promised") %>' DataTextField='<%# Bind("promised") %>' runat="server">
                                                                    <asp:ListItem Value="ASAP">ASAP- As Soon As Possible</asp:ListItem>
                                                                    <asp:ListItem Value="NB4">NB4- Not Before</asp:ListItem>
                                                                    <asp:ListItem Value="MUST"></asp:ListItem>
                                                                    <asp:ListItem Value="HOT"></asp:ListItem>
                                                                    <asp:ListItem Value="RUSH"></asp:ListItem>
                                                                    <asp:ListItem Value="WO"></asp:ListItem>
                                                                    <asp:ListItem Value="HOLD"></asp:ListItem>
                                                                    <asp:ListItem Value="CR"></asp:ListItem>
                                                                    <asp:ListItem Value="BY"></asp:ListItem>
                                                                    <asp:ListItem Value="ON">ON</asp:ListItem>
                                                                    <asp:ListItem Value="NH"></asp:ListItem>
                                                                    <asp:ListItem Value="$$$">$$$- Credit Hold</asp:ListItem>
                                                                    <asp:ListItem Value="AM">AM- AM Delivery</asp:ListItem>
                                                                    <asp:ListItem Value="INK">INK- Waiting for Ink Info</asp:ListItem>
                                                                    <asp:ListItem Value=""></asp:ListItem>
                                                                </asp:DropDownList>
                                                            </td>
                                                            <td align="right" style="padding-right:5px"><b>Due Date:</b>
                                                                <asp:TextBox ID="requestdateTextBox" onfocus="javascript:preEnter( this, 'yes' );" onblur="javascript:preLeave( this, 'date', '99/99/9999' );" MaxLength="10" Width="80px" ToolTip="MM/DD/YYYY" runat="server" Text='<%# Bind("requestdate","{0:MM/dd/yyyy}") %>'></asp:TextBox>
                                                                <a href="#" onblur="FormView1_requestdateTextBox.focus()" tabindex="1" onClick="showCalendarControl(FormView1_requestdateTextBox); return false"><asp:Image ID="Image6" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
                                                                
                                                            </td>
                                                        </tr>
                                                        <tr>
                                                            <td nowrap align="right" style="padding-right:5px"><b>Priority:</b>
                                                                <asp:DropDownList ID="DropDownList3" Width="100px" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" SelectedValue='<%# Bind("requested") %>' DataTextField='<%# Bind("requested") %>' runat="server">
                                                                <asp:ListItem Value="ASAP">ASAP- As Soon As Possible</asp:ListItem>
                                                                 <asp:ListItem Value="NB4">NB4- Not Before</asp:ListItem>
                                                                 <asp:ListItem Value="MUST"></asp:ListItem>
                                                                 <asp:ListItem Value="HOT"></asp:ListItem>
                                                                 <asp:ListItem Value="RUSH"></asp:ListItem>
                                                                 <asp:ListItem Value="WO"></asp:ListItem>
                                                                 <asp:ListItem Value="HOLD"></asp:ListItem>
                                                                 <asp:ListItem Value="CR"></asp:ListItem>
                                                                 <asp:ListItem Value="BY"></asp:ListItem>
                                                                 <asp:ListItem Value="ON">ON</asp:ListItem>
                                                                 <asp:ListItem Value="NH"></asp:ListItem>
                                                                 <asp:ListItem Value="$$$">$$$- Credit Hold</asp:ListItem>
                                                                 <asp:ListItem Value="AM">AM- AM Delivery</asp:ListItem>
                                                                 <asp:ListItem Value="INK">INK- Waiting for Ink Info</asp:ListItem>
                                                                 <asp:ListItem Value=""></asp:ListItem>
                                                              </asp:DropDownList>
                                                           </td>
                                                           <td nowrap align="right" style="padding-right:5px"><b>Promise Date:</b>
                                                                <asp:TextBox ID="promisdateTextBox" onfocus="javascript:preEnter( this, 'yes' );" onblur="javascript:preLeave( this, 'date', '99/99/9999' );" ToolTip="MM/DD/YYYY"  Width="80px" runat="server" Text='<%# Bind("promisdate","{0:MM/dd/yyyy}") %>'></asp:TextBox>
                                                                <a href="#" onblur="FormView1_promisdateTextBox.focus()" tabindex="1" onClick="showCalendarControl(FormView1_promisdateTextBox); return false"><asp:Image ID="Image1" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
                                                                
                                                           </td>
                                                          </tr>
                                                        </table>
                                                     </fieldset>
                                                </td>
                                             </tr>
                                          </table>
                                       </td>
                                    </tr>
                                    <tr>
                                        <td>
                                            <table>
                                                <tr>
                                                    <td>
                                                        <fieldset style="width:400px; border:solid 1px black; background-color:#EFF3FB; height:110px;">
                                                            <table>
                                                                <tr>
                                                                    <td><b>Sales Rep </b></td>
                                                                    <td><b>Sales Rep Name</b></td>                                                                   
                                                                </tr>
                                                                <tr>
                                                                    <td>
                                                                        <asp:TextBox ID="vSmanTextBox" MaxLength="3" Width="50px" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" runat="server" Text='<%# Bind("vSman") %>'></asp:TextBox>
                                                                        <a href="#" tabindex="1" onClick="salesreplook(); return false"><asp:Image ID="Image2" runat="server" ImageUrl="images/lookup_icon.gif" />
                                                                    </td>
                                                                    <td>
                                                                        <asp:TextBox ID="vSnameTextBox" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" runat="server" Text='<%# Bind("vSname") %>'></asp:TextBox>
                                                                    </td>                                                                   
                                                                </tr>
                                                                <tr>
                                                                <td>
                                                                     <asp:TextBox ID="vSman2TextBox" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" MaxLength="3" Width="50px" runat="server" Text='<%# Bind("vSman2") %>'></asp:TextBox>
                                                                     <a href="#" tabindex="1" onClick="smancopylook1(); return false"><asp:Image ID="Image7" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
                                                                 </td>
                                                                 <td>
                                                                    <asp:TextBox ID="vSname2TextBox" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" runat="server" Text='<%# Bind("vSname2") %>'></asp:TextBox>
                                                                 </td>                                                                
                                                            </tr>
                                                            <tr>
                                                                <td>
                                                                    <asp:TextBox ID="vSman3TextBox" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" MaxLength="3" Width="50px" runat="server" Text='<%# Bind("vSman3") %>'></asp:TextBox>
                                                                        <a href="#" tabindex="1" onClick="salesmanlook(); return false"><asp:Image ID="Image4" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
                                                                </td>
                                                                <td>
                                                                    <asp:TextBox ID="vSname3TextBox" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" runat="server" Text='<%# Bind("vSname3") %>'></asp:TextBox>
                                                                </td>                               
                                                            </tr>
                                                        </table>
                                                    </fieldset>
                                                  </td>
                                                  <td>
                                                    <fieldset style="width:400px; border:solid 1px black; background-color:#EFF3FB; height:110px;">
                                                      <table>
                                                        <tr>
                                                            <td align="right" style="padding-right:5px"><b>Overrun%:</b></td>
                                                            <td>
                                                                <asp:TextBox ID="vOverTextBox" Width="50px" MaxLength="5" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" onkeyup="valoverrun()" runat="server"  Text='<%# Bind("vOver") %>'></asp:TextBox>
                                                                <asp:CompareValidator ID="CompareValidator7" runat="server"  ErrorMessage="Only Numbers" ControlToValidate="vOverTextBox"  Display="dynamic" Operator="datatypecheck" Type="double"></asp:CompareValidator>
                                                            </td>
                                                            <td align="right" style="padding-right:5px"><b>Underrun%:</b></td>
                                                            <td>
                                                                <asp:TextBox ID="vUnderTextBox" Width="50px" onfocus= "javascript:focusval(this)" onblur="javascript:underblurval(this)" MaxLength="5" onkeyup="valunderrun()" runat="server" Text='<%# Bind("vUnder") %>'></asp:TextBox>
                                                                <asp:CompareValidator ID="CompareValidator6" runat="server" ErrorMessage="Only Numbers" ControlToValidate="vUnderTextBox"  Display="dynamic" Operator="datatypecheck" Type="double"></asp:CompareValidator>
                                                            </td>
                                                        </tr>
                                                    </table>
                                                  </fieldset>
                                                </td>
                                             </tr>
                                             <tr>
                                                <td colspan="2">
                                                     <asp:Button ID="UpdateButton" CssClass="button" runat="server"  CausesValidation="True" Text="Save" OnClick="UpdateButton_Click"></asp:Button>
                                                     
                                                </td>
                                     </tr>
                               </table>            
                     </asp:Panel>
        </EditItemTemplate>                
</asp:FormView>
    
    <asp:ObjectDataSource ID="ObjectDataSource1" runat="server" OldValuesParameterFormatString="original_{0}" SelectMethod="SelectViewItemEstimate" TypeName="orderentry">
        <SelectParameters>
            <asp:Parameter Name="prmUser" Type="String" />
            <asp:Parameter DefaultValue="Select" Name="prmAction"  Type="String" />
            <asp:SessionParameter DefaultValue="" Name="prmOrderNum" SessionField="order_est" Type="Int32" />                      
            <asp:QueryStringParameter Name="prmLine" QueryStringField="line" Type="Int32" />                        
            <asp:Parameter Name="prmEstimate" Type="String" />
            <asp:Parameter Name="prmItemNum" Type="String" />
            <asp:Parameter Name="prmPartNum" Type="String" />
            <asp:Parameter Name="prmQty" Type="Decimal" />
            <asp:Parameter Name="prmItemName" Type="String" />
            <asp:Parameter Name="prmPartdscr" Type="String" />
            <asp:Parameter Name="prmPartdscr1" Type="String" />
            <asp:Parameter Name="prmPartdscr2" Type="String" />
            <asp:Parameter Name="prmPrice" Type="Decimal" />
            <asp:Parameter Name="prmUom" Type="String" />
            <asp:Parameter Name="prmTax" Type="String" />
            <asp:Parameter Name="prmPoNum" Type="String" />
            <asp:Parameter Name="prmJob" Type="String" />
            <asp:Parameter Name="prmJob2" Type="Int32" />
            <asp:Parameter Name="prmDiscount" Type="Decimal" />
            <asp:Parameter Name="prmCode" Type="String" />
            <asp:Parameter Name="prmReqDate" Type="DateTime" />
            <asp:Parameter Name="prmTPrice" Type="Decimal" />
            <asp:Parameter Name="prmPromCode" Type="String" />
            <asp:Parameter Name="prmPromDate" Type="DateTime" />
            <asp:Parameter Name="prmShip" Type="Decimal" />
            <asp:Parameter Name="prmCas" Type="Int32" />
            <asp:Parameter Name="prmPartial" Type="Decimal" />
            <asp:Parameter Name="prmUnit" Type="Int32" />
            <asp:Parameter Name="prmEnum" Type="Int32" />
            <asp:Parameter Name="prmPrevOrder" Type="Int32" />
            <asp:Parameter Name="prmSman" Type="String" />
            <asp:Parameter Name="prmSman2" Type="String" />
            <asp:Parameter Name="prmSman3" Type="String" />
            <asp:Parameter Name="prmType" Type="String" />
            <asp:Parameter Name="prmOver" Type="Decimal" />
            <asp:Parameter Name="prmUnder" Type="Decimal" />
            <asp:Parameter Name="prmVend" Type="String" />
            <asp:Parameter Name="prmManag" Type="String" />
            <asp:Parameter Name="prmLn" Type="String" />
            <asp:Parameter Name="prmSname" Type="String" />
            <asp:Parameter Name="prmSname2" Type="String" />
            <asp:Parameter Name="prmSname3" Type="String" />
            <asp:Parameter Name="prmSpct" Type="Decimal" />
            <asp:Parameter Name="prmSpct2" Type="Decimal" />
            <asp:Parameter Name="prmSpct3" Type="Decimal" />
            <asp:Parameter Name="prmComm" Type="Decimal" />
            <asp:Parameter Name="prmComm2" Type="Decimal" />
            <asp:Parameter Name="prmComm3" Type="Decimal" />
            <asp:Parameter Name="prmCost" Type="Decimal" />
            <asp:Parameter Name="prmQno" Type="int32" />
            <asp:Parameter Name="prmNewItemCreated" Type="String" />
        </SelectParameters>     
    </asp:ObjectDataSource>
    &nbsp;&nbsp; 
                 
    </div>
    </form>
</body>
</html>
