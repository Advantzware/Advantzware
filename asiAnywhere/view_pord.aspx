<%@ Page Language="C#" MasterPageFile="MasterPagepo.master" Debug="true" AutoEventWireup="true" Inherits="view_pord" Title="Purchase Order" Codebehind="view_pord.aspx.cs" %>

<asp:Content ID="Content1" ContentPlaceHolderID="ContentPlaceHolder1"  runat="server">
<LINK REL="stylesheet" TYPE="text/css" HREF="include/CalendarControl.css" >
    <script language = "JavaScript" src="include/CalendarControl.js"></script>
    <script language="javascript" src="include/date.js"></script>
    <script language="javascript" src="include/event.js"></script>
    <script language="javascript" src="include/insert.js"></script>
<script type="text/javascript" language="javascript">
  

function carrierlook() {
    var NewWindow = window.open("Carrier_lookup.aspx", "CarrierlookupWindow", "width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}
function Carrierlookup(ReturnObj1) {
    document.forms[0].ctl00_ContentPlaceHolder1_FormView1_vCarrierTextBox.value = ReturnObj1;
    document.forms[0].ctl00_ContentPlaceHolder1_FormView1_vCarrierTextBox.focus();
}

function taxcodelook() {
    var NewWindow = window.open("tax_lookup.aspx", "TaxLookupWindow", "width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}
function TaxLookup(ReturnObj1) {
    document.forms[0].ctl00_ContentPlaceHolder1_FormView1_vTaxGrTextBox.value = ReturnObj1;
    document.forms[0].ctl00_ContentPlaceHolder1_FormView1_vTaxGrTextBox.focus();
}
function termslook() {
    var NewWindow = window.open("terms_lookup.aspx", "termsLookupWindow", "width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}

function termsLookup(ReturnObj1) {
    document.forms[0].ctl00_ContentPlaceHolder1_FormView1_vTermsTextBox.value = ReturnObj1;
    document.forms[0].ctl00_ContentPlaceHolder1_FormView1_vTermsTextBox.focus();
}

function vendorlook() {

    var NewWindow = window.open("corvend_lookup.aspx", "UomLookup", "width=500,height=400,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}
function VendLookup(ReturnObj1, ReturnObj2, ReturnObj3, ReturnObj4, ReturnObj5, ReturnObj6) {
    document.forms[0].ctl00_ContentPlaceHolder1_FormView1_vVendNoTextBox.value = ReturnObj1;
    document.forms[0].ctl00_ContentPlaceHolder1_FormView1_vVendNameTextBox.value = ReturnObj2;
    document.forms[0].ctl00_ContentPlaceHolder1_FormView1_vVendAdd1TextBox.value = ReturnObj3;
    //document.forms[0].ctl00_ContentPlaceHolder1_FormView1_vVendAdd2TextBox.value = ReturnObj4;
    document.forms[0].ctl00_ContentPlaceHolder1_FormView1_vVendCityTextBox.value = ReturnObj4;
    document.forms[0].ctl00_ContentPlaceHolder1_FormView1_vVendStateTextBox.value = ReturnObj5;
    document.forms[0].ctl00_ContentPlaceHolder1_FormView1_vVendZipTextBox.value = ReturnObj6;
   //  document.forms[0].ctl00_ContentPlaceHolder1_FormView1_vVendAreaCodeTextBox.value = ReturnObj8;
    // document.forms[0].ctl00_ContentPlaceHolder1_FormView1_vVendPhoneTextBox.value = ReturnObj9;
//    document.getElementById("ctl00_ContentPlaceHolder1_FormView1_vVendNameTextBox").innerHTML = ReturnObj2;
    document.forms[0].ctl00_ContentPlaceHolder1_FormView1_vVendNoTextBox.onchange();
}

function buyerlookup() {

    var NewWindow = window.open("buyerlook.aspx", "UomLookup", "width=500,height=400,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}
function buyerlook(ReturnObj1) {
    document.forms[0].ctl00_ContentPlaceHolder1_FormView1_vBuyerTextBox.value = ReturnObj1;
    document.forms[0].ctl00_ContentPlaceHolder1_FormView1_vBuyerTextBox.focus();
}

function rfqslook() {
    var custval = document.forms[0].ctl00$ContentPlaceHolder1$txt_customer.value;
    var NewWindow = window.open("rfqs_lookup.aspx?customer=" + custval, "RfqsLookupWindow", "width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}
function rfqLookup(ReturnObj1){
    document.forms[0].ctl00$ContentPlaceHolder1$txt_rfq.value = ReturnObj1;
    document.forms[0].ctl00$ContentPlaceHolder1$txt_rfq.focus();
}

function customerlook(){ 
  var NewWindow = window.open("customer_lookup.aspx","CustomerLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}

function CustomerLookup(ReturnObj1){
    document.forms[0].ctl00$ContentPlaceHolder1$txt_customer.value = ReturnObj1;
    document.forms[0].ctl00$ContentPlaceHolder1$txt_customer.focus();
}

function custpartlook() {
    var custval = document.forms[0].ctl00$ContentPlaceHolder1$txt_customer.value;
    var NewWindow = window.open("customerpart_lookup.aspx?customer=" + custval, "CustPartLookupWindow", "width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}

function CustPartLookup(ReturnObj1){
    document.forms[0].ctl00$ContentPlaceHolder1$txt_custpart.value = ReturnObj1;
    document.forms[0].ctl00$ContentPlaceHolder1$txt_custpart.focus();
}

function customerpolook() {
    var cust = "";
    //    if(cust=="")
    //    {
    //        var cust=document.getElementById("ctl00_ContentPlaceHolder1_HiddenField1").value;    
    //    }
    var NewWindow = window.open("customerpo_entry_lookup.aspx?customer=" + cust + "", "EstimateLookupWindow", "width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}

function CustomerPOLookup(ReturnObj1) {
    document.forms[0].ctl00$ContentPlaceHolder1$txt_po.value = ReturnObj1;
    document.forms[0].ctl00$ContentPlaceHolder1$txt_po.focus();
}



function setestdate() {

    var da = document.getElementById("ctl00_ContentPlaceHolder1_FormView1_vPoDateTextBox");
    da.focus();

}

function setestdate2() {

    var dar = document.getElementById("ctl00_ContentPlaceHolder1_FormView1_vDueDateTextBox");
    dar.focus();

}

function setestdate3() {

    var dal = document.getElementById("ctl00_ContentPlaceHolder1_FormView1_vLastShipDateTextBox");
    dal.focus();

}

function focusval(obj) {
    obj.style.backgroundColor = 'blue';
    obj.style.color = 'white';
}
function blurval(obj) {
    obj.style.backgroundColor = 'Window';
    obj.style.color = 'WindowText';
}
function setfocus(obj) {
    obj.style.backgroundColor = 'Window';
    obj.style.color = 'WindowText';

}
function preEnter(fieldObj, canEdit) {
    fieldObj.style.backgroundColor = 'blue';
    fieldObj.style.color = 'white';
    if (canEdit == "no") {
        fieldObj.blur();
        leaveField(fieldObj);
    }

    enterField(fieldObj);
    return;
}
function preLeave(fieldObj, fieldType, fieldFormat) {
    fieldObj.style.backgroundColor = 'Window';
    fieldObj.style.color = 'WindowText';

    fieldType = fieldType.toLowerCase();

    if ((fieldType == "") || (fieldType == "text")) {
        leaveField(fieldObj);
    }
    if (fieldType == "date") {
        if (fieldFormat == "") {
            var dateFormat = "99/99/9999";
        } else { var dateFormat = fieldFormat; }
        checkDate(dateFormat, fieldObj, '01/01/1950', '12/31/3000', 0);
    }

    if (fieldType == "number") {
        if (fieldFormat == "") {
            var numFormat = "(>>>>9)";
        } else { var numFormat = fieldFormat; }
        checkNum(numFormat, fieldObj, '?', '?', 0);
    }   
}


function clickButton(e, ctl00$ContentPlaceHolder1$btnSearch){ 

      var evt = e ? e : window.event;

      var bt = document.getElementById(ctl00$ContentPlaceHolder1$btnSearch);

      if (bt){ 

          if (evt.keyCode == 13){ 

                bt.click(); 

                return false; 

          } 
      } 
  }

</script>

<div>
    <asp:FormView
     ID="FormView1" runat="server" OnDataBound="FormView1_DataBound" OnUnload="FormView1_Unload" DataSourceID="ObjectDataSource1">
        <EditItemTemplate>
        
        <asp:Panel ID="Edit_Panel" runat="server" CssClass="shade" DefaultButton="UpdateButton"> <fieldset >  
            <table class="shade"><tr><td>
            <table class="shade">
            <tr><td nowrap align="right" style="padding-right:5px;width:93px"><b>Po#:</b></td>
            <td style="width:100px"><asp:Label ID="vPoNoTextBox" Width="100" runat="server" Text='<%# Bind("vPoNo") %>' /></td>
            <td nowrap align="left" style="padding-right:5px;"><b>Po Date:</b></td>
            <td style="width:120px"><asp:TextBox ID="vPoDateTextBox" Width="70" onfocus="javascript:preEnter( this, 'yes' );" onblur="javascript:preLeave( this, 'date', '99/99/9999' );"  runat="server"  Text='<%# Bind("vPoDate") %>' />      
			<a href="#" onblur="setestdate()" tabindex="1" onClick="showCalendarControl(ctl00_ContentPlaceHolder1_FormView1_vPoDateTextBox); return false"><asp:Image ID="Image3" runat="server" ImageUrl="images/lookup_icon.gif" /></a> </td>
             <td nowrap align="left" style="padding-right:5px;"><b>Type:</b></td>
             <td style="width:120px"><asp:TextBox ID="vTypeTextBox" Width="25" onfocus="javascript:focusval(this)" onblur="setfocus(this)" runat="server" Text='<%# Bind("vType") %>' /></td>
             <td nowrap align="left" style="padding-right:5px;"><b>Status:</b></td>
             <td><asp:Label ID="vStatTextBox" Width="50" runat="server" Text='<%# Bind("vStat") %>' /></td>
            </tr>
            </table ></td></tr>
            <tr><td><table  class="shade">
            <tr><td nowrap align="right" style="padding-right:5px;width:93px"><b>Vendor#:</b></td>
            <td><asp:TextBox ID="vVendNoTextBox" OnTextChanged="vender_TextChange" AutoPostBack="true" onfocus= "javascript:focusval(this)" onblur="setfocus(this)" MaxLength="8" Width="100" runat="server" Text='<%# Bind("vVendNo") %>' />
                <a href="#" tabindex="1" onclick="vendorlook(); return false"><asp:Image ID="Image4" runat="server" ImageUrl="images/lookup_icon.gif" /></a></td>
            <td nowrap align="right" style="padding-right:5px;width:100px"><b>Ship TO:</b></td>
            <td> <asp:TextBox ID="vShipIdTextBox" MaxLength="8" Width="100" Enabled="false" runat="server" Text='<%# Bind("vShipId") %>' /></td></tr>
            <tr><td></td>
            <td><asp:TextBox ID="vVendNameTextBox" Enabled="false" Width="170" runat="server"  Text='<%# Bind("vVendName") %>' /></td>
            <td></td>
            <td><asp:TextBox ID="vShipNameTextBox" Enabled="false" Width="170" runat="server"         Text='<%# Bind("vShipName") %>' /></td></tr>
            <tr><td></td>
            <td><asp:TextBox ID="vVendAdd1TextBox" Enabled="false" Width="170" runat="server" Text='<%# Bind("vVendAdd1") %>' /></td>
            <td></td><td><asp:TextBox ID="vShipAddr1TextBox" Enabled="false" Width="170" runat="server" Text='<%# Bind("vShipAddr1") %>' /></td></tr>
            <tr><td></td>
            <td><asp:TextBox ID="vVendAdd2TextBox" Enabled="false" Width="170" runat="server" Text='<%# Bind("vVendAdd2") %>' /></td>
            <td></td><td><asp:TextBox ID="vShipAddr2TextBox" Width="170" Enabled="false" runat="server" Text='<%# Bind("vShipAddr2") %>' /></td></tr>
            <tr><td></td>
            <td><asp:TextBox ID="vVendCityTextBox" Width="80" Enabled="false" runat="server" Text='<%# Bind("vVendCity") %>' />
             <asp:TextBox ID="vVendStateTextBox" Width="25" Enabled="false" runat="server" Text='<%# Bind("vVendState") %>' />
             <asp:TextBox ID="vVendZipTextBox" Width="47" Enabled="false" runat="server" Text='<%# Bind("vVendZip") %>' /></td>
            <td></td>
            <td><asp:TextBox ID="vShipCityTextBox" Width="80" Enabled="false" runat="server" Text='<%# Bind("vShipCity") %>' />
            <asp:TextBox ID="vShipStateTextBox" Width="25" Enabled="false" runat="server" Text='<%# Bind("vShipState") %>' />
             <asp:TextBox ID="vShipZipTextBox" Width="47" Enabled="false" runat="server"  Text='<%# Bind("vShipZip") %>' /></td></tr>
            <tr><td></td>
            <td><asp:TextBox ID="vVendAreaCodeTextBox" Width="45" Enabled="false" runat="server" Text='<%# Bind("vVendAreaCode") %>' />
            <asp:TextBox ID="vVendPhoneTextBox" Width="115" Enabled="false" runat="server" Text='<%# Bind("vVendPhone") %>' /> </td>
            <td></td>
            <td><asp:TextBox ID="vShipAreaCodeTextBox" Enabled="false" Width="45" runat="server" Text='<%# Bind("vShipAreaCode") %>' />
            <asp:TextBox ID="vShipPhoneTextBox" Width="115" Enabled="false" runat="server" Text='<%# Bind("vShipPhone") %>' /></td></tr>
            </table></td></tr>
            <tr><td><table class="shade">
            
            <tr><td nowrap align="right" style="padding-right:5px;"><b>Buyer:</b></td><td><asp:TextBox ID="vBuyerTextBox" onfocus= "javascript:focusval(this)" onblur="setfocus(this)" Width="90" runat="server" Text='<%# Bind("vBuyer") %>' />
                <a href="#" tabindex="1" onClick="buyerlookup(); return false"><asp:Image ID="Image5" runat="server" ImageUrl="images/lookup_icon.gif" /></a></td>
            <td nowrap align="right" style="padding-right:5px;width:120px"><b>Shipping Carrier:</b></td><td><asp:TextBox ID="vCarrierTextBox" MaxLength="6" Width="70" onfocus= "javascript:focusval(this)" onblur="setfocus(this)" runat="server" Text='<%# Bind("vCarrier") %>' />
                <a href="#" tabindex="1" onClick="carrierlook(); return false"><asp:Image ID="Image11" runat="server" ImageUrl="images/lookup_icon.gif" /></a></td>
            <td nowrap align="right" style="padding-right:5px;width:120px"><b>Total Freight:</b></td><td><asp:TextBox ID="vTFreightTextBox" MaxLength="8" Width="80" onfocus= "javascript:focusval(this)" onblur="setfocus(this)" runat="server" Text='<%# Bind("vTFreight") %>' />
            <asp:CompareValidator ID="CompareValidator5" ControlToValidate="vTFreightTextBox" Display="Dynamic" Operator="DataTypeCheck" Type="Double" runat="server" ErrorMessage="Enter Valid Value"></asp:CompareValidator></td></tr>
            
            <tr><td nowrap align="right" style="padding-right:5px;"><b>Contact:</b></td><td><asp:TextBox ID="vContactTextBox" runat="server" onfocus= "javascript:focusval(this)" onblur="setfocus(this)" Text='<%# Bind("vContact") %>' /></td>
            <td nowrap align="right" style="padding-right:5px;"><b>Tax Code:</b></td><td><asp:TextBox ID="vTaxGrTextBox" MaxLength="4" Width="50" onfocus= "javascript:focusval(this)" onblur="setfocus(this)" runat="server" Text='<%# Bind("vTaxGr") %>' />
               <a href="#" tabindex="1" onClick="taxcodelook(); return false"><asp:Image ID="Image14" runat="server" ImageUrl="images/lookup_icon.gif" /></a></td>
            <td nowrap align="right" style="padding-right:5px;"><b>Tax:</b></td><td><asp:TextBox ID="vTaxTextBox" Enabled="false" Width="80" runat="server" Text='<%# Bind("vTax") %>' /></td></tr>
            
            <tr><td nowrap align="right" style="padding-right:5px;"><b>Required Date:</b></td><td><asp:TextBox ID="vDueDateTextBox" Width="70" onfocus="javascript:preEnter( this, 'yes' );" onblur="javascript:preLeave( this, 'date', '99/99/9999' );" runat="server" Text='<%# Bind("vDueDate") %>' />
              <a href="#" onblur="setestdate2()" tabindex="1" onClick="showCalendarControl(ctl00_ContentPlaceHolder1_FormView1_vDueDateTextBox); return false"><asp:Image ID="Image1" runat="server" ImageUrl="images/lookup_icon.gif" /></a></td>
            <td nowrap align="right" style="padding-right:5px;"><b>Payment Terms:</b></td><td><asp:TextBox ID="vTermsTextBox" MaxLength="6" Width="70" onfocus= "javascript:focusval(this)" onblur="setfocus(this)" runat="server" Text='<%# Bind("vTerms") %>' />
              <a href="#" tabindex="1" onClick="termslook(); return false"><asp:Image ID="Image7" runat="server" ImageUrl="images/lookup_icon.gif" /></a></td>
            <td nowrap align="right" style="padding-right:5px;"><b>Total Cost:</b></td><td><asp:TextBox ID="vTCostTextBox" Enabled="false" Width="80" runat="server" Text='<%# Bind("vTCost") %>' /></td></tr>
            
            <tr><td nowrap align="right" style="padding-right:5px;"><b>Last Ship Date:</b></td><td><asp:TextBox ID="vLastShipDateTextBox" Width="70" onfocus="javascript:preEnter( this, 'yes' );" onblur="javascript:preLeave( this, 'date', '99/99/9999' );" runat="server" Text='<%# Bind("vLastShipDate") %>' />
              <a href="#" onblur="setestdate3()" tabindex="1" onClick="showCalendarControl(ctl00_ContentPlaceHolder1_FormView1_vLastShipDateTextBox); return false"><asp:Image ID="Image2" runat="server" ImageUrl="images/lookup_icon.gif" /></a></td>
            <td nowrap align="right" style="padding-right:5px;"> <b>Freight Payment:</b></td>
            <td  colspan="3"><asp:RadioButtonList ID="FreightPaymentRadioButtonList" onfocus= "javascript:focusval(this)" onblur="setfocus(this)"  RepeatLayout="Flow"  CellSpacing="1" RepeatColumns="3"     SelectedValue ='<%# Bind("vFrtPay") %>' Font-Bold="true" Enabled="true"    runat="server">
                                                         <asp:ListItem value="B"  Text="Bill        "   />
                                                         <asp:ListItem value="C" Text="Collect        " />
                                                         <asp:ListItem value="P" Text="Perpaid" />                                                         
                                                     </asp:RadioButtonList></td></tr>
            
            <tr><td nowrap align="right" style="padding-right:5px;"><b>Under/Overrun%:</b></td>
            <td><asp:TextBox ID="vUnderPctTextBox" onfocus= "javascript:focusval(this)" onblur="setfocus(this)" Width="50" runat="server" Text='<%# Bind("vUnderPct") %>' />
            <asp:CompareValidator ID="CompareValidator3" ControlToValidate="vUnderPctTextBox" Display="Dynamic" Operator="DataTypeCheck" Type="Double" runat="server" ErrorMessage="Enter Valid Value"></asp:CompareValidator>
            <asp:TextBox ID="vOverPctTextBox" Width="50" runat="server" onfocus= "javascript:focusval(this)" onblur="setfocus(this)" Text='<%# Bind("vOverPct") %>' />
            <asp:CompareValidator ID="CompareValidator4" ControlToValidate="vOverPctTextBox" Display="Dynamic" Operator="DataTypeCheck" Type="Double" runat="server" ErrorMessage="Enter Valid Value"></asp:CompareValidator></td>
            <td align="right" style="padding-right:5px;"><b>FOB:</b></td>
            <td colspan="3"><asp:RadioButtonList ID="FOBRadioButtonList1"   RepeatLayout="Flow"  CellSpacing="1" RepeatColumns="2"   SelectedValue = '<%# Bind("vFobCode") %>' Enabled="true" Font-Bold="true"   runat="server">
                                                    <asp:ListItem value="Dest"  Text="Destination        "   />
                                                    <asp:ListItem  value="Orig" Text="Origin" />
                                                </asp:RadioButtonList></td></tr>
           <tr><td colspan="6">
            <asp:TextBox ID="vRecKeyTextBox" Visible="false" runat="server" Text='<%# Bind("vRecKey") %>' />
            <br />
            <asp:Button ID="UpdateButton" runat="server" class="buttonM" CausesValidation="True" 
                OnClick="UpdateButton_Click" Text="Save" />
            &nbsp;<asp:Button ID="UpdateCancelButton" class="buttonM" runat="server" 
                CausesValidation="False" CommandName="Cancel" Text="Cancel" /></td></tr>
            </table>
            </td></tr></table></fieldset>
            </asp:Panel>
            
        </EditItemTemplate>
        <InsertItemTemplate>
            <asp:Panel ID="Insert_Panel" runat="server" CssClass="shade" DefaultButton="InsertButton">
            <fieldset>
            <table class="shade"><tr><td>
            <table class="shade">
            <tr><td nowrap align="right" style="padding-right:5px;width:93px"><b>Po#:</b></td>
            <td style="width:100px"><asp:Label ID="vPoNoTextBox" Width="100" runat="server" Text='<%# Bind("vPoNo") %>' /></td>
            <td nowrap align="left" style="padding-right:5px;"><b>Po Date:</b></td>
            <td style="width:120px"><asp:TextBox ID="vPoDateTextBox" Width="70" onfocus="javascript:preEnter( this, 'yes' );" onblur="javascript:preLeave( this, 'date', '99/99/9999' );"  runat="server"  Text='<%# Bind("vPoDate") %>' />      
			   <a href="#" onblur="setestdate()" tabindex="1" onClick="showCalendarControl(ctl00_ContentPlaceHolder1_FormView1_vPoDateTextBox); return false"><asp:Image ID="Image3" runat="server" ImageUrl="images/lookup_icon.gif" /></a> </td>
             <td nowrap align="left" style="padding-right:5px;"><b>Type:</b></td>
             <td style="width:120px"><asp:TextBox ID="vTypeTextBox" Width="25" onfocus= "javascript:focusval(this)" onblur="setfocus(this)" runat="server" Text='<%# Bind("vType") %>' /></td>
             <td nowrap align="left" style="padding-right:5px;"><b>Status:</b></td>
             <td><asp:Label ID="vStatTextBox" Width="25" runat="server" onfocus= "javascript:focusval(this)" onblur="setfocus(this)" Text='<%# Bind("vStat") %>' /></td>
            </tr>
            </table ></td></tr>
            <tr><td><table  class="shade">
            <tr><td nowrap align="right" style="padding-right:5px;width:93px"><b>Vendor#:</b></td>
            <td><asp:TextBox ID="vVendNoTextBox" OnTextChanged="vender_TextChange" AutoPostBack="true" onfocus= "javascript:focusval(this)" onblur="setfocus(this)" MaxLength="8" Width="100" runat="server" Text='<%# Bind("vVendNo") %>' />
                <a href="#" tabindex="1" onclick="vendorlook(); return false"><asp:Image ID="Image4" runat="server" ImageUrl="images/lookup_icon.gif" /></a></td>
            <td nowrap align="right" style="padding-right:5px;width:100px"><b>Ship TO:</b></td>
            <td> <asp:TextBox ID="vShipIdTextBox" MaxLength="8" Width="100" runat="server" onfocus= "javascript:focusval(this)" onblur="setfocus(this)" Text='<%# Bind("vShipId") %>' /></td></tr>
            <tr><td></td>
            <td><asp:TextBox ID="vVendNameTextBox" Enabled="false" Width="170" runat="server"  Text='<%# Bind("vVendName") %>' /></td>
            <td></td>
            <td><asp:TextBox ID="vShipNameTextBox" Enabled="false" Width="170" runat="server"         Text='<%# Bind("vShipName") %>' /></td></tr>
            <tr><td></td>
            <td><asp:TextBox ID="vVendAdd1TextBox" Enabled="false" Width="170" runat="server" Text='<%# Bind("vVendAdd1") %>' /></td>
            <td></td><td><asp:TextBox ID="vShipAddr1TextBox" Enabled="false" Width="170" runat="server" Text='<%# Bind("vShipAddr1") %>' /></td></tr>
            <tr><td></td>
            <td><asp:TextBox ID="vVendAdd2TextBox" Enabled="false" Width="170" runat="server" Text='<%# Bind("vVendAdd2") %>' /></td>
            <td></td><td><asp:TextBox ID="vShipAddr2TextBox" Width="170" Enabled="false" runat="server" Text='<%# Bind("vShipAddr2") %>' /></td></tr>
            <tr><td></td>
            <td><asp:TextBox ID="vVendCityTextBox" Width="80" Enabled="false" runat="server" Text='<%# Bind("vVendCity") %>' />
             <asp:TextBox ID="vVendStateTextBox" Width="25" Enabled="false" runat="server" Text='<%# Bind("vVendState") %>' />
             <asp:TextBox ID="vVendZipTextBox" Width="47" Enabled="false" runat="server" Text='<%# Bind("vVendZip") %>' /></td>
            <td></td>
            <td><asp:TextBox ID="vShipCityTextBox" Width="80" Enabled="false" runat="server" Text='<%# Bind("vShipCity") %>' />
            <asp:TextBox ID="vShipStateTextBox" Width="25" Enabled="false" runat="server" Text='<%# Bind("vShipState") %>' />
             <asp:TextBox ID="vShipZipTextBox" Width="47" Enabled="false" runat="server"  Text='<%# Bind("vShipZip") %>' /></td></tr>
            <tr><td></td>
            <td><asp:TextBox ID="vVendAreaCodeTextBox" Width="45" Enabled="false" runat="server" Text='<%# Bind("vVendAreaCode") %>' />
            <asp:TextBox ID="vVendPhoneTextBox" Width="115" Enabled="false" runat="server" Text='<%# Bind("vVendPhone") %>' /> </td>
            <td></td>
            <td><asp:TextBox ID="vShipAreaCodeTextBox" Enabled="false" Width="45" runat="server" Text='<%# Bind("vShipAreaCode") %>' />
            <asp:TextBox ID="vShipPhoneTextBox" Width="115" Enabled="false" runat="server" Text='<%# Bind("vShipPhone") %>' /></td></tr>
            </table></td></tr>
            <tr><td><table class="shade">
            
            <tr><td nowrap align="right" style="padding-right:5px;"><b>Buyer:</b></td>
            <td><asp:TextBox ID="vBuyerTextBox" Width="90" runat="server" onfocus= "javascript:focusval(this)" onblur="setfocus(this)" Text='<%# Bind("vBuyer") %>' /></td>
            <td nowrap align="right" style="padding-right:5px;width:120px"><b>Shipping Carrier:</b></td>
            <td><asp:TextBox ID="vCarrierTextBox" MaxLength="6" Width="70" runat="server" onfocus= "javascript:focusval(this)" onblur="setfocus(this)" Text='<%# Bind("vCarrier") %>' />
                <a href="#" tabindex="1" onClick="carrierlook(); return false"><asp:Image ID="Image11" runat="server" ImageUrl="images/lookup_icon.gif" /></a></td>
            <td nowrap align="right" style="padding-right:5px;width:120px"><b>Total Freight:</b></td>
            <td><asp:TextBox ID="vTFreightTextBox" MaxLength="8" Width="80" onfocus= "javascript:focusval(this)" onblur="setfocus(this)" runat="server" Text='<%# Bind("vTFreight") %>' />
            <asp:CompareValidator ID="CompareValidator3" ControlToValidate="vTFreightTextBox" Display="Dynamic" Operator="DataTypeCheck" Type="Double" runat="server" ErrorMessage="Enter Valid Value"></asp:CompareValidator></td></tr>
            
            <tr><td nowrap align="right" style="padding-right:5px;"><b>Contact:</b></td>
            <td><asp:TextBox ID="vContactTextBox" runat="server" onfocus= "javascript:focusval(this)" onblur="setfocus(this)" Text='<%# Bind("vContact") %>' /></td>
            <td nowrap align="right" style="padding-right:5px;"><b>Tax Code:</b></td>
            <td><asp:TextBox ID="vTaxGrTextBox" MaxLength="4" Width="50" onfocus= "javascript:focusval(this)" onblur="setfocus(this)" runat="server" Text='<%# Bind("vTaxGr") %>' />
               <a href="#" tabindex="1" onClick="taxcodelook(); return false"><asp:Image ID="Image14" runat="server" ImageUrl="images/lookup_icon.gif" /></a></td>
            <td nowrap align="right" style="padding-right:5px;"><b>Tax:</b></td>
            <td><asp:TextBox ID="vTaxTextBox" Enabled="false" Width="80" runat="server" Text='<%# Bind("vTax") %>' /></td></tr>
            
            <tr><td nowrap align="right" style="padding-right:5px;"><b>Required Date:</b></td>
            <td><asp:TextBox ID="vDueDateTextBox" Width="70" onfocus="javascript:preEnter( this, 'yes' );" onblur="javascript:preLeave( this, 'date', '99/99/9999' );" runat="server" Text='<%# Bind("vDueDate") %>' />
              <a href="#" onblur="setestdate2()" tabindex="1" onClick="showCalendarControl(ctl00_ContentPlaceHolder1_FormView1_vDueDateTextBox); return false"><asp:Image ID="Image1" runat="server" ImageUrl="images/lookup_icon.gif" /></a></td>
            <td nowrap align="right" style="padding-right:5px;"><b>Payment Terms:</b></td>
            <td><asp:TextBox ID="vTermsTextBox" MaxLength="6" Width="70" onfocus= "javascript:focusval(this)" onblur="setfocus(this)" runat="server" Text='<%# Bind("vTerms") %>' />
              <a href="#" tabindex="1" onClick="termslook(); return false"><asp:Image ID="Image7" runat="server" ImageUrl="images/lookup_icon.gif" /></a></td>
            <td nowrap align="right" style="padding-right:5px;"><b>Total Cost:</b></td>
            <td><asp:TextBox ID="vTCostTextBox" Enabled="false" Width="80" runat="server" Text='<%# Bind("vTCost") %>' /></td></tr>
            
            <tr><td nowrap align="right" style="padding-right:5px;"><b>Last Ship Date:</b></td>
            <td><asp:TextBox ID="vLastShipDateTextBox" Width="70" onfocus="javascript:preEnter( this, 'yes' );" onblur="javascript:preLeave( this, 'date', '99/99/9999' );" runat="server" Text='<%# Bind("vLastShipDate") %>' />
              <a href="#" onblur="setestdate3()" tabindex="1" onClick="showCalendarControl(ctl00_ContentPlaceHolder1_FormView1_vLastShipDateTextBox); return false"><asp:Image ID="Image2" runat="server" ImageUrl="images/lookup_icon.gif" /></a></td>
            <td nowrap align="right" style="padding-right:5px;"> <b>Freight Payment:</b></td>
            <td  colspan="3"><asp:RadioButtonList ID="FreightPaymentRadioButtonList"  RepeatLayout="Flow"  CellSpacing="1" RepeatColumns="3"     SelectedValue ='<%# Bind("vFrtPay") %>' Font-Bold="true" Enabled="true"    runat="server">
                                                         <asp:ListItem value="B"  Text="Bill          "   />
                                                         <asp:ListItem value="C" Text="Collect          " />
                                                         <asp:ListItem value="P" Text="Perpaid" />                                                         
                                                     </asp:RadioButtonList></td></tr>
            
            <tr><td nowrap align="right" style="padding-right:5px;"><b>Under/Overrun%:</b></td>
            <td><asp:TextBox ID="vUnderPctTextBox" Width="50" onfocus= "javascript:focusval(this)" onblur="setfocus(this)" runat="server" Text='<%# Bind("vUnderPct") %>' />
                <asp:CompareValidator ID="CompareValidator1" ControlToValidate="vUnderPctTextBox" Display="Dynamic" Operator="DataTypeCheck" Type="Double" runat="server" ErrorMessage="Enter Valid Value"></asp:CompareValidator>
            <asp:TextBox ID="vOverPctTextBox" Width="50" runat="server" onfocus= "javascript:focusval(this)" onblur="setfocus(this)" Text='<%# Bind("vOverPct") %>' />
            <asp:CompareValidator ID="CompareValidator2" ControlToValidate="vOverPctTextBox" Display="Dynamic" Operator="DataTypeCheck" Type="Double" runat="server" ErrorMessage="Enter Valid Value"></asp:CompareValidator></td>
            <td align="right" style="padding-right:5px;"><b>FOB:</b></td>
            <td colspan="3"><asp:RadioButtonList ID="FOBRadioButtonList1"  RepeatLayout="Flow"  CellSpacing="1" RepeatColumns="2"   SelectedValue = '<%# Bind("vFobCode") %>' Enabled="true" Font-Bold="true"   runat="server">
                                                    <asp:ListItem value="Dest"  Text="Destination          "   />
                                                    <asp:ListItem  value="Orig" Text="Origin" />
                                                </asp:RadioButtonList></td></tr>
           <tr><td colspan="6">
            <asp:TextBox ID="vRecKeyTextBox" Visible="false" runat="server" Text='<%# Bind("vRecKey") %>' />
            <br />
           <asp:Button ID="InsertButton" runat="server" CausesValidation="True" class="buttonM" OnClick="InsertButton_Click"  Text="Save" />
            &nbsp;<asp:Button ID="InsertCancelButton" runat="server" 
                CausesValidation="False" CommandName="Cancel" class="buttonM" OnClick="InsertCancelButton_Click" Text="Cancel" /></td></tr>
            </table>
            </td></tr></table></fieldset>
            </asp:Panel>
        </InsertItemTemplate>
        <ItemTemplate>
        <fieldset style="background-color:#EFF3FB;">
            <table class="shade"><tr><td>
            <table class="shade">
            <tr><td nowrap align="right" style="padding-right:5px;width:93px"><b>Po#:</b></td>
            <td style="width:150px"><asp:Label ID="vPoNolabel" Width="100" BackColor="turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" runat="server" Text='<%# Bind("vPoNo") %>' /></td>
            <td nowrap align="left" style="padding-right:5px;"><b>Po Date:</b></td>
            <td style="width:150px"><asp:label ID="vPoDatelabel" Width="100" BackColor="Turquoise" BorderColor="white" BorderStyle="Solid" BorderWidth="1px" runat="server" Text='<%# Bind("vPoDate") %>' /></td>
             <td nowrap align="left" style="padding-right:5px;"><b>Type:</b></td>
             <td style="width:150px"><asp:label ID="vTypelabel" Width="25" BackColor="Turquoise" BorderColor="white" BorderStyle="Solid" BorderWidth="1px" runat="server" Text='<%# Bind("vType") %>' /></td>
             <td nowrap align="left" style="padding-right:5px;"><b>Status:</b></td>
             <td><asp:Label ID="vStatlabel" Width="25" BackColor="Turquoise" BorderColor="white" BorderStyle="Solid" BorderWidth="1px" runat="server" Text='<%# Bind("vStat") %>' /></td>
            </tr>
            </table ></td></tr>
            <tr><td><table  class="shade">
            <tr><td nowrap align="right" style="padding-right:5px;width:93px"><b>Vendor#:</b></td>
            <td><asp:label ID="vVendNolabel" Width="100" BackColor="Turquoise" BorderColor="white" BorderStyle="Solid" BorderWidth="1px" runat="server" Text='<%# Bind("vVendNo") %>' /></td>
            <td nowrap align="right" style="padding-right:5px;width:100px"><b>Ship TO:</b></td>
            <td> <asp:label ID="vShipIdlabel" Width="100" BackColor="Turquoise" BorderColor="white" BorderStyle="Solid" BorderWidth="1px" runat="server" Text='<%# Bind("vShipId") %>' /></td></tr>
            <tr><td></td>
            <td><asp:label ID="vVendNamelabel" Width="170" BackColor="Turquoise" BorderColor="white" BorderStyle="Solid" BorderWidth="1px" runat="server"  Text='<%# Bind("vVendName") %>' /></td>
            <td></td>
            <td><asp:label ID="vShipNamelabel" Width="170" BackColor="Turquoise" BorderColor="white" BorderStyle="Solid" BorderWidth="1px" runat="server"         Text='<%# Bind("vShipName") %>' /></td></tr>
            <tr><td></td>
            <td><asp:label ID="vVendAdd1label" Width="170" BackColor="Turquoise" BorderColor="white" BorderStyle="Solid" BorderWidth="1px" runat="server" Text='<%# Bind("vVendAdd1") %>' /></td>
            <td></td><td><asp:label ID="vShipAddr1label" Width="170" BackColor="Turquoise" BorderColor="white" BorderStyle="Solid" BorderWidth="1px" runat="server" Text='<%# Bind("vShipAddr1") %>' /></td></tr>
            <tr><td></td>
            <td><asp:label ID="vVendAdd2label" Width="170" BackColor="Turquoise" BorderColor="white" BorderStyle="Solid" BorderWidth="1px" runat="server" Text='<%# Bind("vVendAdd2") %>' /></td>
            <td></td><td><asp:label ID="vShipAddr2label" Width="170" BackColor="Turquoise" BorderColor="white" BorderStyle="Solid" BorderWidth="1px" runat="server" Text='<%# Bind("vShipAddr2") %>' /></td></tr>
            <tr><td></td>
            <td><asp:label ID="vVendCitylabel" Width="80" BackColor="Turquoise" BorderColor="white" BorderStyle="Solid" BorderWidth="1px" runat="server" Text='<%# Bind("vVendCity") %>' />
             <asp:label ID="vVendStatelabel" Width="25" BackColor="Turquoise" BorderColor="white" BorderStyle="Solid" BorderWidth="1px" runat="server" Text='<%# Bind("vVendState") %>' />
             <asp:label ID="vVendZiplabel" Width="52" BackColor="Turquoise" BorderColor="white" BorderStyle="Solid" BorderWidth="1px" runat="server" Text='<%# Bind("vVendZip") %>' /></td>
            <td></td>
            <td><asp:label ID="vShipCitylabel" Width="80" BackColor="Turquoise" BorderColor="white" BorderStyle="Solid" BorderWidth="1px" runat="server" Text='<%# Bind("vShipCity") %>' />
            <asp:label ID="vShipStatelabel" Width="25" BackColor="Turquoise" BorderColor="white" BorderStyle="Solid" BorderWidth="1px" runat="server" Text='<%# Bind("vShipState") %>' />
             <asp:label ID="vShipZiplabel" Width="52" BackColor="Turquoise" BorderColor="white" BorderStyle="Solid" BorderWidth="1px" runat="server"  Text='<%# Bind("vShipZip") %>' /></td></tr>
            <tr><td></td>
            <td><asp:label ID="vVendAreaCodelabel" Width="45" BackColor="Turquoise" BorderColor="white" BorderStyle="Solid" BorderWidth="1px" runat="server" Text='<%# Bind("vVendAreaCode") %>' />
            <asp:label ID="vVendPhonelabel" Width="115" BackColor="Turquoise" BorderColor="white" BorderStyle="Solid" BorderWidth="1px" runat="server" Text='<%# Bind("vVendPhone") %>' /> </td>
            <td></td>
            <td><asp:label ID="vShipAreaCodelabel" Width="45" BackColor="Turquoise" BorderColor="white" BorderStyle="Solid" BorderWidth="1px" runat="server" Text='<%# Bind("vShipAreaCode") %>' />
            <asp:label ID="vShipPhonelabel" Width="115" BackColor="Turquoise" BorderColor="white" BorderStyle="Solid" BorderWidth="1px" runat="server" Text='<%# Bind("vShipPhone") %>' /></td></tr>
            </table></td></tr>
            <tr><td><table class="shade">
            
            <tr><td nowrap align="right" style="padding-right:5px;"><b>Buyer:</b></td><td><asp:label ID="vBuyerlabel" Width="100" BackColor="Turquoise" BorderColor="white" BorderStyle="Solid" BorderWidth="1px" runat="server" Text='<%# Bind("vBuyer") %>' /></td>
            <td nowrap align="right" style="padding-right:5px;width:120px"><b>Shipping Carrier:</b></td><td><asp:label ID="vCarrierlabel" Width="70" BackColor="Turquoise" BorderColor="white" BorderStyle="Solid" BorderWidth="1px" runat="server" Text='<%# Bind("vCarrier") %>' /></td>
            <td nowrap align="right" style="padding-right:5px;width:120px"><b>Total Freight:</b></td><td><asp:label ID="vTFreightlabel" Width="80" BackColor="Turquoise" BorderColor="white" BorderStyle="Solid" BorderWidth="1px" runat="server" Text='<%# Bind("vTFreight") %>' /></td></tr>
            
            <tr><td nowrap align="right" style="padding-right:5px;"><b>Contact:</b></td><td><asp:label ID="vContactlabel" Width="100" BackColor="Turquoise" BorderColor="white" BorderStyle="Solid" BorderWidth="1px" runat="server" Text='<%# Bind("vContact") %>' /></td>
            <td nowrap align="right" style="padding-right:5px;"><b>Tax Code:</b></td><td><asp:label ID="vTaxGrlabel" Width="50" BackColor="Turquoise" BorderColor="white" BorderStyle="Solid" BorderWidth="1px" runat="server" Text='<%# Bind("vTaxGr") %>' /></td>
            <td nowrap align="right" style="padding-right:5px;"><b>Tax:</b></td><td><asp:label ID="vTaxlabel" Width="80" BackColor="Turquoise" BorderColor="white" BorderStyle="Solid" BorderWidth="1px" runat="server" Text='<%# Bind("vTax") %>' /></td></tr>
            
            <tr><td nowrap align="right" style="padding-right:5px;"><b>Required Date:</b></td><td><asp:label ID="vDueDatelabel" Width="100" BackColor="Turquoise" BorderColor="white" BorderStyle="Solid" BorderWidth="1px" runat="server" Text='<%# Bind("vDueDate") %>' /></td>
            <td nowrap align="right" style="padding-right:5px;"><b>Payment Terms:</b></td><td><asp:label ID="vTermslabel" Width="70" BackColor="Turquoise" BorderColor="white" BorderStyle="Solid" BorderWidth="1px" runat="server" Text='<%# Bind("vTerms") %>' /></td>
            <td nowrap align="right" style="padding-right:5px;"><b>Total Cost:</b></td><td><asp:label ID="vTCostlabel" Width="80" BackColor="Turquoise" BorderColor="white" BorderStyle="Solid" BorderWidth="1px" runat="server" Text='<%# Bind("vTCost") %>' /></td></tr>
            
            <tr><td nowrap align="right" style="padding-right:5px;"><b>Last Ship Date:</b></td><td><asp:label ID="vLastShipDatelabel" Width="100" BackColor="Turquoise" BorderColor="white" BorderStyle="Solid" BorderWidth="1px" runat="server" Text='<%# Bind("vLastShipDate") %>' /></td>
            <td nowrap align="right" style="padding-right:5px;"> <b>Freight Payment:</b></td>
            <td  colspan="3"><asp:RadioButtonList ID="FreightPaymentRadioButtonList"  RepeatLayout="Flow" BackColor="Turquoise" BorderColor="white" BorderStyle="Solid" BorderWidth="1px"  CellSpacing="1" RepeatColumns="3"  Width="200px"   SelectedValue ='<%# Bind("vFrtPay") %>' Enabled="false" Font-Bold="true"   runat="server">
                                                         <asp:ListItem value="B"  Text="Bill"   />
                                                         <asp:ListItem value="C" Text="Collect" />
                                                         <asp:ListItem value="P" Text="Perpaid" />                                                         
                                                     </asp:RadioButtonList></td></tr>
            
            <tr><td nowrap align="right" style="padding-right:5px;"><b>Under/Overrun%:</b></td><td><asp:label ID="vUnderPctlabel" Width="48" BackColor="Turquoise" BorderColor="white" BorderStyle="Solid" BorderWidth="1px" runat="server" Text='<%# Bind("vUnderPct") %>' />
            <asp:label ID="vOverPctlabel" Width="48" BackColor="Turquoise" BorderColor="white" BorderStyle="Solid" BorderWidth="1px" runat="server" Text='<%# Bind("vOverPct") %>' /></td>
            <td align="right" style="padding-right:5px;"><b>FOB:</b></td>
            <td colspan="3"><asp:RadioButtonList ID="FOBRadioButtonList"  RepeatLayout="Flow" BackColor="Turquoise"  BorderColor="white" BorderStyle="Solid" BorderWidth="1px" CellSpacing="1" RepeatColumns="2" Width="150px"   SelectedValue = '<%# Bind("vFobCode") %>' Enabled="false" Font-Bold="true"   runat="server">
                                                    <asp:ListItem value="Dest"  Text="Destination"   />
                                                    <asp:ListItem  value="Orig" Text="Origin" />
                                                </asp:RadioButtonList></td></tr>
           <tr><td colspan="6">
            <asp:label ID="vRecKeylabel" Visible="false" runat="server" Text='<%# Bind("vRecKey") %>' />
            <br />
            
            <asp:Button ID="NewButton" runat="server" CausesValidation="False" CommandName="New" CssClass="buttonM" 
                   Text="Add">
                </asp:Button> 
             <asp:Button ID="EditButton" runat="server" CausesValidation="False" CommandName="Edit" CssClass="buttonM" 
                   Text="Update">
                </asp:Button>
                <asp:Button ID="DeleteButton" runat="server"  CssClass="buttonM" OnClick="DeleteButton_Click" OnClientClick="return confirm('Are you sure you want to delete this record')"
                    Text="Delete">
                </asp:Button>                                
                
                
                
         </table>
            </td></tr></table></fieldset>       
        </ItemTemplate>
    </asp:FormView>
    <br />
    
    <asp:ObjectDataSource ID="ObjectDataSource1" runat="server" OldValuesParameterFormatString="original_{0}"
        SelectMethod="SelectViewPo" TypeName="browspo">
        <SelectParameters>            
            <asp:Parameter Name="prmUser"  Type="String" />            
            <asp:Parameter Name="prmAction" DefaultValue="View"  Type="String"  />
            <asp:SessionParameter Name="prmPoNo" SessionField="pur_ord_po" Type="Int32"   />
            <asp:Parameter Name="prmPoDate"  Type="String" />
            <asp:Parameter Name="prmType"  Type="String" />
            <asp:Parameter Name="prmStat"  Type="String" />
            <asp:Parameter Name="prmVendNo"  Type="String" />
            <asp:Parameter Name="prmVendName"  Type="String" />
            <asp:Parameter Name="prmVendAdd1" Type="String"></asp:Parameter>
            <asp:Parameter Name="prmVendAdd2" Type="String" />
            <asp:Parameter Name="prmVendCity" Type="String" />
            <asp:Parameter Name="prmVendState" Type="String" />
            <asp:Parameter Name="prmVendZip" Type="String" />
            <asp:Parameter Name="prmVendAreaCode" Type="String" />
            <asp:Parameter Name="prmVendPhone" Type="String" />
            <asp:Parameter Name="prmShipId" Type="String" />
            <asp:Parameter Name="prmShipName" Type="String" />
            <asp:Parameter Name="prmShipAddr" Type="String" />
            <asp:Parameter Name="prmShipCity" Type="String" />
            <asp:Parameter Name="prmShipState" Type="String" />
            <asp:Parameter Name="prmShipZip" Type="String" />
            <asp:Parameter Name="prmShipAreaCode" Type="String" />
            <asp:Parameter Name="prmShipPhone" Type="String" />
            <asp:Parameter Name="prmBuyer" Type="String" />
            <asp:Parameter Name="prmContact" Type="String" />
            <asp:Parameter Name="prmDueDate" Type="String" />
            <asp:Parameter Name="prmLastShipDate" Type="String" />
            <asp:Parameter Name="prmUnderPct" Type="Int32" />
            <asp:Parameter Name="prmOverPct" Type="Int32" />
            <asp:Parameter Name="prmCarrier" Type="String" />
            <asp:Parameter Name="prmTaxGr" Type="String" />
            <asp:Parameter Name="prmTerms" Type="String" />
            <asp:Parameter Name="prmFrtPay" Type="String" />
            <asp:Parameter Name="prmFobCode" Type="String" />
            <asp:Parameter Name="prmTFreight" Type="Int32" />
            <asp:Parameter Name="prmTax" Type="Int32" />
            <asp:Parameter Name="prmTCost" Type="Int32" />
            <asp:Parameter Name="prmRecKey" Type="String" />
        </SelectParameters>
    </asp:ObjectDataSource>
</div>

</asp:Content>
