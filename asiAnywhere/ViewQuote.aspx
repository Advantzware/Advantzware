<%@ Page Language="C#" MasterPageFile="~/MasterPage6.master" Debug="true" AutoEventWireup="true" Inherits="ViewQuote" Title="View Quote" Codebehind="ViewQuote.aspx.cs" %>
<asp:Content ID="Content1" ContentPlaceHolderID="ContentPlaceHolder1" Runat="Server">
<script src="include/CalendarControl.js"></script>
<script language="javascript" src="include/date.js"></script>
<script language="javascript" src="include/event.js"></script>
<script language="javascript" src="include/insert.js"></script>


<script>
    function contactcustomerlook(){ 
  var NewWindow = window.open("custshiplook.aspx","ContactCustomerLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}

function CustShipLook(ReturnObj1, ReturnObj2, ReturnObj3, ReturnObj4, ReturnObj5, ReturnObj6, ReturnObj7, ReturnObj8, ReturnObj9, ReturnObj10, ReturnObj11, ReturnObj12, ReturnObj13, ReturnObj14, ReturnObj15, ReturnObj16, ReturnObj17, ReturnObj18, ReturnObj19, ReturnObj20,ReturnObj21, ReturnObj22, ReturnObj23, ReturnObj24, ReturnObj25, ReturnObj26, ReturnObj27, ReturnObj28, ReturnObj29, ReturnObj30) { 
  document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vCustTextBox.value = ReturnObj1;
  document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vBillTextBox.value = ReturnObj2;
  document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vBill2TextBox.value = ReturnObj3;
  document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vBill3TextBox.value = ReturnObj4;
  document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vBill4TextBox.value = ReturnObj5 + "," + ReturnObj6 + ", " + ReturnObj7;
  document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vCarrierTextBox.value = ReturnObj8;
  document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vCarrdscrTextBox.value = ReturnObj9;
  document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vSmanTextBox.value = ReturnObj10;
  document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vSnameTextBox.value = ReturnObj11;
  document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vTermsTextBox.value = ReturnObj12;
  document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vTermDscrTextBox.value = ReturnObj13;
  document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vDelZoneTextBox.value = ReturnObj14;
  document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vZondescTextBox.value = ReturnObj15;


  document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vShipidTextBox.value = ReturnObj16; 
  document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vShipTextBox.value = ReturnObj17;
  document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vShip2TextBox.value = ReturnObj18;
  document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vShip3TextBox.value = ReturnObj19;
  document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vShip4TextBox.value = ReturnObj20 + ", " + ReturnObj21 + ", " + ReturnObj22;
  document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vSoldIdTextBox.value = ReturnObj23;
  document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vSoldTextBox.value = ReturnObj24;
  document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vSold2TextBox.value = ReturnObj25;
  document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vSold3TextBox.value = ReturnObj26;
  document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vSold4TextBox.value = ReturnObj27 + ", " + ReturnObj28 + ", " + ReturnObj29;
  document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vContactTextBox.value = ReturnObj30;
 
 
  
}
function salesreplook() {
    var NewWindow = window.open("salesrep_lookup.aspx", "SalesRepLookupWindow", "width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}

function SalesRepLookup(ReturnObj1, ReturnObj2) {
    document.forms[0].ctl00_ContentPlaceHolder1_FormView1_vSmanTextBox.value = ReturnObj1;
    document.forms[0].ctl00_ContentPlaceHolder1_FormView1_vSnameTextBox.value = ReturnObj2;
}
function deliveryzonelook() {
    var carrier = document.getElementById("ctl00_ContentPlaceHolder1_FormView1_vCarrierTextBox").value;
    var NewWindow = window.open("zone_lookup.aspx?zone=" + carrier + "", "DelZoneLookupWindow", "width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}
function zoneLookup(ReturnObj1, ReturnObj2) {
    document.forms[0].ctl00_ContentPlaceHolder1_FormView1_vDelZoneTextBox.value = ReturnObj1;
    document.forms[0].ctl00_ContentPlaceHolder1_FormView1_vZondescTextBox.value = ReturnObj2;
}
function ShipTOLook() {
    var lookHidden = document.getElementById("ctl00_ContentPlaceHolder1_FormView1_vCustTextBox").value;
    var NewWindow = window.open("ShipIdCustLook.aspx?look=" + lookHidden + "", "ShipToLookupWindow", "width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}
function ShipToLookup(ReturnObj1,ReturnObj2,ReturnObj3,ReturnObj4,ReturnObj5,ReturnObj6,ReturnObj7,ReturnObj8) {
    document.forms[0].ctl00_ContentPlaceHolder1_FormView1_vShipidTextBox.value = ReturnObj1;
    document.forms[0].ctl00_ContentPlaceHolder1_FormView1_vShipTextBox.value = ReturnObj3;
    document.forms[0].ctl00_ContentPlaceHolder1_FormView1_vShip2TextBox.value = ReturnObj4;
    document.forms[0].ctl00_ContentPlaceHolder1_FormView1_vShip3TextBox.value = ReturnObj5;
    document.forms[0].ctl00_ContentPlaceHolder1_FormView1_vShip4TextBox.value = ReturnObj6 + "," + ReturnObj7 + "," + ReturnObj8;

}

function SoldLook() {
    var lookHidden = document.getElementById("ctl00_ContentPlaceHolder1_FormView1_vCustTextBox").value;
    var NewWindow = window.open("soldto_lookup.aspx?look=" + lookHidden + "", "SoldToLookupWindow", "width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}
function SoldToLookup(ReturnObj1, ReturnObj2, ReturnObj3, ReturnObj4, ReturnObj5, ReturnObj6, ReturnObj7) {
    document.forms[0].ctl00_ContentPlaceHolder1_FormView1_vSoldIdTextBox.value = ReturnObj1;
    document.forms[0].ctl00_ContentPlaceHolder1_FormView1_vSoldTextBox.value = ReturnObj2;
    document.forms[0].ctl00_ContentPlaceHolder1_FormView1_vSold2TextBox.value = ReturnObj3;
    document.forms[0].ctl00_ContentPlaceHolder1_FormView1_vSold3TextBox.value = ReturnObj4;
    document.forms[0].ctl00_ContentPlaceHolder1_FormView1_vSold4TextBox.value = ReturnObj5 + "," + ReturnObj6 + "," + ReturnObj7;
 }

 function Custpartlook() {
     var NewWindow = window.open("lcpart_lookup.aspx", "CustPartLookupWindow", "width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
 }
 function lcpartlook(ReturnObj1, ReturnObj2, ReturnObj3, ReturnObj4, ReturnObj5, ReturnObj6, ReturnObj7) {
     document.forms[0].ctl00_ContentPlaceHolder1_FormView2_vPartNoTextBox.value = ReturnObj1;
     document.forms[0].ctl00_ContentPlaceHolder1_FormView2_vInoTextBox.value = ReturnObj2; 
     document.forms[0].ctl00_ContentPlaceHolder1_HiddenField1.value = ReturnObj2;
     document.forms[0].ctl00_ContentPlaceHolder1_FormView2_vPartDscr1TextBox.value = ReturnObj3;
    
     document.forms[0].ctl00_ContentPlaceHolder1_FormView2_vStyleTextBox.value = ReturnObj4;
     document.forms[0].ctl00_ContentPlaceHolder1_FormView2_vPriceTextBox.value = ReturnObj5;
     document.forms[0].ctl00_ContentPlaceHolder1_FormView2_vUomTextBox.value = ReturnObj6;
     document.forms[0].ctl00_ContentPlaceHolder1_FormView2_vSizeTextBox.value = ReturnObj7 ;
     
     document.forms[0].ctl00_ContentPlaceHolder1_FormView2_vPartDscr1TextBox.focus();
     
     
 }
</script>

<div>
    <asp:HiddenField ID="HiddenField1" runat="server" />
<asp:HyperLink ID="Hyperlink1" runat="server" NavigateUrl="list_rfqs.aspx">Back to list</asp:HyperLink>
    <asp:FormView ID="FormView1" runat="server" DataSourceID="ObjectDataSource1" OnDataBound="FormView1_ondatabound">
        <EditItemTemplate>
         <asp:Panel ID="Panel_Edit1" runat="server"  DefaultButton="Button1" >
            <table class="shade"><tr>
            <td> <b>Quote#:</b>
            <asp:Label ID="vQuoteTextBox" Width="60px" BackColor="Turquoise" runat="server" Text='<%# Bind("vQuote") %>'>
            </asp:Label></td>
            <td align="right" style="padding-right:5px"><b>Quote Date:</b>
            <asp:TextBox ID="vDateTextBox" onfocus="javascript:preEnter( this, 'yes' );" onblur="javascript:preLeave( this, 'date', '99/99/9999' );" runat="server" Text='<%# Bind("vDate","{0:MM/dd/yyyy}") %>'>
            </asp:TextBox><a href="#" tabindex="1" onClick="showCalendarControl(ctl00_ContentPlaceHolder1_FormView1_vDateTextBox); return false"><asp:Image ID="Image5" runat="server" ImageUrl="images/lookup_icon.gif" /></a> </td>
            <td colspan="5"><b>Estimate:</b>
            <asp:TextBox ID="vEstimateTextBox" runat="server" Text='<%# Bind("vEstimate") %>'>
            </asp:TextBox>
            <b>RFQ:</b>
            <asp:TextBox ID="vRfqTextBox" runat="server" Text='<%# Bind("vRfq") %>'>
            </asp:TextBox>
            <b>Status:</b>
            <asp:Label ID="vStatTextBox" runat="server" BackColor="Turquoise" Text='<%# Bind("vStat") %>'>
            </asp:Label></td>
            </tr><tr><td></td>
            <td align="right" style="padding-right:5px"> <b>Delivery Date:</b>
            <asp:TextBox ID="vDelDateTextBox" onfocus="javascript:preEnter( this, 'yes' );" onblur="javascript:preLeave( this, 'date', '99/99/9999' );" runat="server" Text='<%# Bind("vDelDate","{0:MM/dd/yyyy}") %>'>
            </asp:TextBox><a href="#" tabindex="1" onClick="showCalendarControl(ctl00_ContentPlaceHolder1_FormView1_vDelDateTextBox); return false"><asp:Image ID="Image6" runat="server" ImageUrl="images/lookup_icon.gif" /></a> </td>
            <td colspan="4" ><b>Contact:</b>
            <asp:TextBox ID="vContactTextBox" Width="250px" runat="server" Text='<%# Bind("vContact") %>'>
            </asp:TextBox></td></tr>
            <tr><td></td><td><b>Bill To</b></td><td><b>Ship To</b></td><td><b>Sold To</b></td></tr>
            <tr><td></td><td><asp:TextBox ID="vCustTextBox" TabIndex="1" ReadOnly="true" BackColor="Turquoise" Width="90px" runat="server" Text='<%# Bind("vCust") %>'>
            </asp:TextBox><a href="#" tabindex="1" visible="false" onClick="contactcustomerlook(); return false"><asp:Image ID="CustomerLook" visible="false" runat="server" ImageUrl="images/lookup_icon.gif" /></a></td>
            <td> <asp:TextBox ID="vShipidTextBox" runat="server"  Width="90px" Text='<%# Bind("vShipid") %>'>
            </asp:TextBox><a href="#" tabindex="1" onClick="ShipTOLook(); return false"><asp:Image ID="Image3" runat="server" ImageUrl="images/lookup_icon.gif" /></a></td>
            <td><asp:TextBox ID="vSoldIdTextBox" runat="server"  Width="90px" Text='<%# Bind("vSoldId") %>'>
            </asp:TextBox><a href="#" tabindex="1" onClick="SoldLook(); return false"><asp:Image ID="Image4" runat="server" ImageUrl="images/lookup_icon.gif" /></a></td></tr>
            <tr><td></td><td> <asp:TextBox TabIndex="1" ReadOnly="true" BackColor="Turquoise" ID="vBillTextBox" runat="server" Text='<%# Bind("vBill") %>'>
            </asp:TextBox></td>
            <td><asp:TextBox ID="vShipTextBox" TabIndex="1" ReadOnly="true" BackColor="Turquoise" runat="server" Text='<%# Bind("vShip") %>'>
            </asp:TextBox></td>
            <td><asp:TextBox ID="vSoldTextBox" TabIndex="1" ReadOnly="true" BackColor="Turquoise" runat="server" Text='<%# Bind("vSold") %>'>
            </asp:TextBox></td></tr>
            <tr><td></td><td><asp:TextBox ID="vBill2TextBox" TabIndex="1" ReadOnly="true" BackColor="Turquoise" runat="server" Text='<%# Bind("vBill2") %>'>
            </asp:TextBox></td>
            <td><asp:TextBox ID="vShip2TextBox" TabIndex="1" ReadOnly="true" BackColor="Turquoise" runat="server" Text='<%# Bind("vShip2") %>'>
            </asp:TextBox></td>
            <td><asp:TextBox ID="vSold2TextBox" TabIndex="1" ReadOnly="true" BackColor="Turquoise" runat="server" Text='<%# Bind("vSold2") %>'>
            </asp:TextBox></td></tr>
            <tr><td></td><td><asp:TextBox ID="vBill3TextBox" TabIndex="1" ReadOnly="true" BackColor="Turquoise" runat="server" Text='<%# Bind("vBill3") %>'>
            </asp:TextBox></td>
            <td><asp:TextBox ID="vShip3TextBox" TabIndex="1" ReadOnly="true" BackColor="Turquoise" runat="server" Text='<%# Bind("vShip3") %>'>
            </asp:TextBox></td>
            <td><asp:TextBox ID="vSold3TextBox" TabIndex="1" ReadOnly="true" BackColor="Turquoise" runat="server" Text='<%# Bind("vSold3") %>'>
            </asp:TextBox></td></tr>
            <tr><td></td><td> <asp:TextBox ID="vBill4TextBox" TabIndex="1" ReadOnly="true" BackColor="Turquoise" runat="server" Text='<%# Bind("vBill4") %>'>
            </asp:TextBox></td>
            <td><asp:TextBox ID="vShip4TextBox" TabIndex="1" ReadOnly="true" BackColor="Turquoise" runat="server" Text='<%# Bind("vShip4") %>'>
            </asp:TextBox></td>
            <td><asp:TextBox ID="vSold4TextBox" TabIndex="1" ReadOnly="true" BackColor="Turquoise" runat="server" Text='<%# Bind("vSold4") %>'>
            </asp:TextBox></td></tr>
            <tr><td></td><td align="right" style="padding-right:5px"><b>Salesman:</b>
            <asp:TextBox ID="vSmanTextBox" Width="100px" runat="server" Text='<%# Bind("vSman") %>'>
            </asp:TextBox><a href="#" tabindex="1" onClick="salesreplook(); return false"><asp:Image ID="Image1" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
            </td>
            <td> <asp:TextBox ID="vSnameTextBox" TabIndex="1" ReadOnly="true" BackColor="Turquoise" runat="server" Text='<%# Bind("vSname") %>'>
            </asp:TextBox></td>
            <td align="right" style="padding-right:5px"><b>Carrier:</b>
            <asp:TextBox ID="vCarrierTextBox" TabIndex="1" ReadOnly="true" BackColor="Turquoise" runat="server" Text='<%# Bind("vCarrier") %>'>
            </asp:TextBox></td>
            <td><asp:TextBox ID="vCarrdscrTextBox" TabIndex="1" ReadOnly="true" BackColor="Turquoise" runat="server" Text='<%# Bind("vCarrdscr") %>'>
            </asp:TextBox></td></tr>
            <tr><td></td><td align="right" style="padding-right:5px"><b>Terms:</b>
            <asp:TextBox ID="vTermsTextBox" runat="server" Text='<%# Bind("vTerms") %>'>
            </asp:TextBox></td>
            <td><asp:TextBox ID="vTermDscrTextBox" TabIndex="1" ReadOnly="true" BackColor="Turquoise" runat="server" Text='<%# Bind("vTermDscr") %>'>
            </asp:TextBox></td>
            <td align="right" style="padding-right:5px"> <b>Zone:</b>
            <asp:TextBox ID="vDelZoneTextBox" Width="100px" onblur="document.getElementById('ctl00_ContentPlaceHolder1_FormView1_vDateTextBox').focus();" runat="server" Text='<%# Bind("vDelZone") %>'>
            </asp:TextBox><a href="#" tabindex="1" onClick="deliveryzonelook(); return false"><asp:Image ID="Image2" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
            </td>
            <td> 
            <asp:TextBox ID="vZondescTextBox" TabIndex="1" ReadOnly="true" BackColor="Turquoise" runat="server" Text='<%# Bind("vZondesc") %>'>
            </asp:TextBox></td></tr>
                      
           
            <tr><td>
             <asp:Button ID="Button1" runat="server" Font-Bold="true" CssClass="buttonM" OnClick="UpdateButton_click"
                Text="Save">
            </asp:Button>
            <asp:Button ID="Button2" Font-Bold="true" CssClass="buttonM" runat="server" CausesValidation="False" CommandName="Cancel" 
                Text="Cancel">
            </asp:Button></td></tr></table></asp:Panel>
        </EditItemTemplate>
        <InsertItemTemplate>
        <asp:Panel ID="Panel_Edit1" runat="server"  DefaultButton="InsertButton" >
            <table class="shade"><tr>
            <%--<td> <b>Quote#:</b>
            <asp:TextBox ID="vQuoteTextBox" Width="60px" runat="server" Text='<%# Bind("vQuote") %>'>
            </asp:TextBox></td>--%>
            <td align="right" style="padding-right:5px"><b>Quote Date:</b>
            <asp:TextBox ID="vDateTextBox" Width="70px" runat="server" onfocus="javascript:preEnter( this, 'yes' );" onblur="javascript:preLeave( this, 'date', '99/99/9999' );" Text='<%# Bind("vDate","{0:MM/dd/yyyy}") %>'>
            </asp:TextBox><a href="#" tabindex="1" onClick="showCalendarControl(ctl00_ContentPlaceHolder1_FormView1_vDateTextBox); return false"><asp:Image ID="Image6" runat="server" ImageUrl="images/lookup_icon.gif" /></a> </td>
            <td colspan="5"><b>Estimate:</b>
            <asp:TextBox ID="vEstimateTextBox" Width="100px" runat="server" Text='<%# Bind("vEstimate") %>'>
            </asp:TextBox>
            <b>RFQ:</b>
            <asp:TextBox ID="vRfqTextBox" Width="100px" runat="server" Text='<%# Bind("vRfq") %>'>
            </asp:TextBox>
            <b>Status:</b>
            <asp:TextBox ID="vStatTextBox" Width="100px" runat="server" Text='<%# Bind("vStat") %>'>
            </asp:TextBox></td>
            </tr><tr>
            <td align="right" style="padding-right:5px"> <b>Delivery Date:</b>
            <asp:TextBox ID="vDelDateTextBox" Width="70px" runat="server" onfocus="javascript:preEnter( this, 'yes' );" onblur="javascript:preLeave( this, 'date', '99/99/9999' );" Text='<%# Bind("vDelDate","{0:MM/dd/yyyy}") %>'>
            </asp:TextBox><a href="#" tabindex="1" onClick="showCalendarControl(ctl00_ContentPlaceHolder1_FormView1_vDelDateTextBox); return false"><asp:Image ID="Image7" runat="server" ImageUrl="images/lookup_icon.gif" /></a> </td>
            <td colspan="4" ><b>Contact:</b>
            <asp:TextBox ID="vContactTextBox" Width="250px" runat="server" Text='<%# Bind("vContact") %>'>
            </asp:TextBox></td></tr>
            <tr><td></td><td><b>Bill To</b></td><td><b>Ship To</b></td><td><b>Sold To</b></td></tr>
            <tr><td></td><td><asp:TextBox ID="vCustTextBox" Width="100px" runat="server" Text='<%# Bind("vCust") %>'>
            </asp:TextBox><a href="#" tabindex="1" onClick="contactcustomerlook(); return false"><asp:Image ID="CustomerLook"  runat="server" ImageUrl="images/lookup_icon.gif" /></a></td>
            <td> <asp:TextBox ID="vShipidTextBox"  runat="server"  Width="100px" Text='<%# Bind("vShipid") %>'>
            </asp:TextBox><a href="#" tabindex="1" onClick="ShipTOLook(); return false"><asp:Image ID="Image3" runat="server" ImageUrl="images/lookup_icon.gif" /></a></td>
            <td><asp:TextBox ID="vSoldIdTextBox" runat="server"  Width="100px" Text='<%# Bind("vSoldId") %>'>
            </asp:TextBox><a href="#" tabindex="1" onClick="SoldLook(); return false"><asp:Image ID="Image4" runat="server" ImageUrl="images/lookup_icon.gif" /></a></td></tr>
            <tr><td></td><td> <asp:TextBox ID="vBillTextBox"   runat="server" Text='<%# Bind("vBill") %>'>
            </asp:TextBox></td>
            <td><asp:TextBox ID="vShipTextBox"     runat="server" Text='<%# Bind("vShip") %>'>
            </asp:TextBox></td>
            <td><asp:TextBox ID="vSoldTextBox"   runat="server" Text='<%# Bind("vSold") %>'>
            </asp:TextBox></td></tr>
            <tr><td></td><td><asp:TextBox ID="vBill2TextBox"   runat="server" Text='<%# Bind("vBill2") %>'>
            </asp:TextBox></td>
            <td><asp:TextBox ID="vShip2TextBox"  runat="server"   Text='<%# Bind("vShip2") %>'>
            </asp:TextBox></td>
            <td><asp:TextBox ID="vSold2TextBox"  runat="server"    Text='<%# Bind("vSold2") %>'>
            </asp:TextBox></td></tr>
            <tr><td></td><td><asp:TextBox ID="vBill3TextBox"    runat="server" Text='<%# Bind("vBill3") %>'>
            </asp:TextBox></td>
            <td><asp:TextBox ID="vShip3TextBox"  runat="server"    Text='<%# Bind("vShip3") %>'>
            </asp:TextBox></td>
            <td><asp:TextBox ID="vSold3TextBox"  runat="server"    Text='<%# Bind("vSold3") %>'>
            </asp:TextBox></td></tr>
            <tr><td></td><td> <asp:TextBox ID="vBill4TextBox"  runat="server" TabIndex="10"  Text='<%# Bind("vBill4") %>'>
            </asp:TextBox></td>
            <td><asp:TextBox ID="vShip4TextBox"  runat="server"    Text='<%# Bind("vShip4") %>'>
            </asp:TextBox></td>
            <td><asp:TextBox ID="vSold4TextBox"  runat="server"   Text='<%# Bind("vSold4") %>'>
            </asp:TextBox></td></tr>
            <tr><td></td><td align="right" style="padding-right:5px"><b>Salesman:</b>
            <asp:TextBox ID="vSmanTextBox" Width="60px" runat="server" Text='<%# Bind("vSman") %>'>
            </asp:TextBox><a href="#" tabindex="1" onClick="salesreplook(); return false"><asp:Image ID="Image1" runat="server" ImageUrl="images/lookup_icon.gif" /></a></td>
            <td> <asp:TextBox ID="vSnameTextBox" runat="server"   Text='<%# Bind("vSname") %>'>
            </asp:TextBox></td>
            <td align="right" style="padding-right:5px"><b>Carrier:</b>
            <asp:TextBox ID="vCarrierTextBox" Width="80px" runat="server" Text='<%# Bind("vCarrier") %>'>
            </asp:TextBox></td>
            <td><asp:TextBox ID="vCarrdscrTextBox" runat="server"    Text='<%# Bind("vCarrdscr") %>'>
            </asp:TextBox></td></tr>
            <tr><td></td><td align="right" style="padding-right:5px"><b>Terms:</b>
            <asp:TextBox ID="vTermsTextBox" Width="80px" runat="server" Text='<%# Bind("vTerms") %>'>
            </asp:TextBox></td>
            <td><asp:TextBox ID="vTermDscrTextBox" runat="server"   Text='<%# Bind("vTermDscr") %>'>
            </asp:TextBox></td>
            <td align="right" style="padding-right:5px"> <b>Zone:</b>
            <asp:TextBox ID="vDelZoneTextBox"  Width="60px" runat="server" Text='<%# Bind("vDelZone") %>'>
            </asp:TextBox><a href="#" tabindex="1" onClick="deliveryzonelook(); return false"><asp:Image ID="Image2" runat="server" ImageUrl="images/lookup_icon.gif" /></a></td>
            <td> 
            <asp:TextBox ID="vZondescTextBox" runat="server" onblur="document.getElementById('ctl00_ContentPlaceHolder1_FormView1_vDateTextBox').focus();"  Text='<%# Bind("vZondesc") %>'>
            </asp:TextBox></td></tr>
                              
           
            <tr><td><asp:Button ID="InsertButton" runat="server" CssClass="buttonM" OnClick="Add_viewquote"
                Text="Save">
            </asp:Button>
            <asp:Button ID="InsertCancelButton" runat="server" CssClass="buttonM" CausesValidation="False" OnClick="formview1_cancel_click"  CommandName="Cancel"
                Text="Cancel">
            </asp:Button></td></tr></table></asp:Panel>
        </InsertItemTemplate>
        <ItemTemplate>
            <table class="shade"><tr>
            <td><b>Quote#:</b>
            <asp:Label ID="vQuoteLabel" Width="50px" BackColor="turquoise" BorderColor="black" BorderStyle="solid" BorderWidth="1px" runat="server" Text='<%# Bind("vQuote") %>'></asp:Label></td>
            <td><b>Quote Date:</b>
            <asp:Label ID="vDateLabel" Width="100px" BackColor="turquoise" BorderColor="black" BorderStyle="solid" BorderWidth="1px" runat="server" Text='<%# Bind("vDate","{0:MM/dd/yyyy}") %>'></asp:Label></td>
            <td colspan="5"> <b>Estimate#:</b>
            <asp:Label ID="vEstimateLabel" Width="100px" BackColor="turquoise" BorderColor="black" BorderStyle="solid" BorderWidth="1px" runat="server" Text='<%# Bind("vEstimate") %>'></asp:Label>
            <b>RFQ:</b>
            <asp:Label ID="vRfqLabel" Width="100px" BackColor="turquoise" BorderColor="black" BorderStyle="solid" BorderWidth="1px" runat="server" Text='<%# Bind("vRfq") %>'></asp:Label>
            <b>Status</b>
            <asp:Label ID="vStatLabel" Width="100px" BackColor="turquoise" BorderColor="black" BorderStyle="solid" BorderWidth="1px" runat="server" Text='<%# Bind("vStat") %>'></asp:Label></td>
            </tr><tr>
            <td></td><td align="left" style="padding-right:5px"> <b>Delivery Date:</b>
            <asp:Label ID="vDelDateLabel" Width="100px" BackColor="turquoise" BorderColor="black" BorderStyle="solid" BorderWidth="1px" runat="server" Text='<%# Bind("vDelDate","{0:MM/dd/yyyy}") %>'></asp:Label></td>
            <td colspan="4"><b>Contact:</b>
            <asp:Label ID="vContactLabel" Width="200px" BackColor="turquoise" BorderColor="black" BorderStyle="solid" BorderWidth="1px" runat="server" Text='<%# Bind("vContact") %>'></asp:Label></td></tr>
            <tr><td></td><td><b>Bill To</b></td>
            <td><b>Ship To</b></td><td><b>Sold To</b></td></tr>
            <tr><td></td><td><asp:Label ID="vCustLabel" Width="100px" BackColor="turquoise" BorderColor="black" BorderStyle="solid" BorderWidth="1px" runat="server" Text='<%# Bind("vCust") %>'></asp:Label></td>
            <td><asp:Label ID="vShipidLabel" runat="server" Width="100px" BackColor="turquoise" BorderColor="black" BorderStyle="solid" BorderWidth="1px" Text='<%# Bind("vShipid") %>'></asp:Label></td>
            <td><asp:Label ID="vSoldIdLabel" runat="server" Width="100px" BackColor="turquoise" BorderColor="black" BorderStyle="solid" BorderWidth="1px" Text='<%# Bind("vSoldId") %>'></asp:Label></td></tr>
            <tr><td></td><td><asp:Label ID="vBillLabel" runat="server" Width="150px" BackColor="turquoise" BorderColor="black" BorderStyle="solid" BorderWidth="1px" Text='<%# Bind("vBill") %>'></asp:Label></td>
            <td><asp:Label ID="vShipLabel" runat="server" Width="150px" BackColor="turquoise" BorderColor="black" BorderStyle="solid" BorderWidth="1px" Text='<%# Bind("vShip") %>'></asp:Label></td>
            <td><asp:Label ID="vSoldLabel" runat="server" Width="150px" BackColor="turquoise" BorderColor="black" BorderStyle="solid" BorderWidth="1px" Text='<%# Bind("vSold") %>'></asp:Label></td></tr>
            <tr><td></td><td><asp:Label ID="vBill2Label" runat="server" Width="150px" BackColor="turquoise" BorderColor="black" BorderStyle="solid" BorderWidth="1px" Text='<%# Bind("vBill2") %>'></asp:Label></td>
            <td><asp:Label ID="vShip2Label" runat="server" Width="150px" BackColor="turquoise" BorderColor="black" BorderStyle="solid" BorderWidth="1px" Text='<%# Bind("vShip2") %>'></asp:Label></td>
            <td><asp:Label ID="vSold2Label" runat="server" Width="150px" BackColor="turquoise" BorderColor="black" BorderStyle="solid" BorderWidth="1px" Text='<%# Bind("vSold2") %>'></asp:Label></td></tr>
            <tr><td></td><td><asp:Label ID="vBill3Label" runat="server" Width="150px" BackColor="turquoise" BorderColor="black" BorderStyle="solid" BorderWidth="1px" Text='<%# Bind("vBill3") %>'></asp:Label></td>
            <td><asp:Label ID="vShip3Label" runat="server" Width="150px" BackColor="turquoise" BorderColor="black" BorderStyle="solid" BorderWidth="1px" Text='<%# Bind("vShip3") %>'></asp:Label></td>
            <td><asp:Label ID="vSold3Label" runat="server" Width="150px" BackColor="turquoise" BorderColor="black" BorderStyle="solid" BorderWidth="1px" Text='<%# Bind("vSold3") %>'></asp:Label></td></tr>
            <tr><td></td><td><asp:Label ID="vBill4Label" runat="server" Width="150px" BackColor="turquoise" BorderColor="black" BorderStyle="solid" BorderWidth="1px" Text='<%# Bind("vBill4") %>'></asp:Label></td>
            <td><asp:Label ID="vShip4Label" runat="server" Width="150px" BackColor="turquoise" BorderColor="black" BorderStyle="solid" BorderWidth="1px" Text='<%# Bind("vShip4") %>'></asp:Label></td>
            <td><asp:Label ID="vSold4Label" runat="server" Width="150px" BackColor="turquoise" BorderColor="black" BorderStyle="solid" BorderWidth="1px" Text='<%# Bind("vSold4") %>'></asp:Label></td></tr>
            <tr><td></td><td align="left" style="padding-right:5px"> <b>Salesman:</b>
            <asp:Label ID="vSmanLabel" runat="server" Width="90px" BackColor="turquoise" BorderColor="black" BorderStyle="solid" BorderWidth="1px" Text='<%# Bind("vSman") %>'></asp:Label></td>
            <td><asp:Label ID="vSnameLabel" runat="server" Width="150px" BackColor="turquoise" BorderColor="black" BorderStyle="solid" BorderWidth="1px" Text='<%# Bind("vSname") %>'></asp:Label></td>
            <td align="left" style="padding-right:5px"> <b>Carrier:</b>
            <asp:Label ID="vCarrierLabel" runat="server" Width="105px" BackColor="turquoise" BorderColor="black" BorderStyle="solid" BorderWidth="1px" Text='<%# Bind("vCarrier") %>'></asp:Label></td>
            <td> <asp:Label ID="vCarrdscrLabel" runat="server" Width="100px" BackColor="turquoise" BorderColor="black" BorderStyle="solid" BorderWidth="1px" Text='<%# Bind("vCarrdscr") %>'></asp:Label></td></tr>
            <tr><td></td><td align="left" style="padding-right:5px" ><b>Terms:</b>
            <asp:Label ID="vTermsLabel" runat="server" Width="108px" BackColor="turquoise" BorderColor="black" BorderStyle="solid" BorderWidth="1px" Text='<%# Bind("vTerms") %>'></asp:Label></td>
            <td><asp:Label ID="vTermDscrLabel" runat="server" Width="150px" BackColor="turquoise" BorderColor="black" BorderStyle="solid" BorderWidth="1px" Text='<%# Bind("vTermDscr") %>'></asp:Label></td>
            <td align="left" style="padding-right:5px"> <b>Zone:</b>
            <asp:Label ID="vDelZoneLabel" runat="server" Width="115px" BackColor="turquoise" BorderColor="black" BorderStyle="solid" BorderWidth="1px" Text='<%# Bind("vDelZone") %>'></asp:Label>
            </td>
            <td><asp:Label ID="vZondescLabel" runat="server" Width="100px" BackColor="turquoise" BorderColor="black" BorderStyle="solid" BorderWidth="1px" Text='<%# Bind("vZondesc") %>'></asp:Label></td></tr>
            <tr><td colspan="4">
            
                       
            <asp:Button ID="AddButton" runat="server"  CausesValidation="False" CssClass="buttonM" CommandName="new"
                 Text="Add">
                </asp:Button> 
            <asp:Button ID="UpdateButton" runat="server"  CssClass="button" CausesValidation="False" CommandName="edit"
                Text="Update">
            </asp:Button>
            <asp:Button ID="DeleteButton" runat="server"  CssClass="button" CausesValidation="False" OnClick="delete_button_Click" OnClientClick="return confirm('Delete Currently Selected Record?')"     Text="Delete">
            </asp:Button>
            
            <asp:Button ID="PrintButton" runat="server" CssClass="buttonM" OnClick="PrintButtonClick" Text="Print" />            
            <asp:Button ID="EmailQuoteButton" runat="server" CssClass="buttonM" OnClick="EmailButtonClick" Text="Email Quotes" />
            </td></tr>
            
           
            </table>
        </ItemTemplate>
    </asp:FormView>
    
    
    <br />
    <asp:GridView ID="GridView1" runat="server" AllowPaging="True" AllowSorting="True" OnSelectedIndexChanged="GridView1_SelectedIndex"
        AutoGenerateColumns="False" CssClass="Grid" DataSourceID="ObjectDataSource2" DataKeyNames="vLine"
        EmptyDataText="No Record Found"  Width="730px">
        <EmptyDataRowStyle BorderColor="Gray" BorderStyle="Dotted" BorderWidth="0px" Font-Bold="True"
            HorizontalAlign="Center" VerticalAlign="Middle" />
        <RowStyle CssClass="shade" />
        <Columns>
            <asp:CommandField ButtonType="Image" SelectImageUrl="~/Images/sel.gif" SelectText="" ShowSelectButton="True">
                <ItemStyle Width="10px" />
            </asp:CommandField>
            <asp:BoundField DataField="vPartNo" HeaderText="Cust Part#" 
                SortExpression="vPartNo" />
            <asp:BoundField DataField="vIno" HeaderText="Item No" SortExpression="vIno" />
            <asp:BoundField DataField="vPartDscr1" HeaderText="Item Description" 
                SortExpression="vPartDscr1" />
            <asp:BoundField DataField="vPartDscr2" HeaderText="Item Description2" 
                SortExpression="vPartDscr2" />
            <asp:BoundField DataField="vStyle" HeaderText="Style" 
                SortExpression="vStyle" />
            <asp:BoundField DataField="vQty" HeaderText="Qty" SortExpression="vQty" />
            <asp:BoundField DataField="vPrice" HeaderText="Price" 
                SortExpression="vPrice" />
            <asp:BoundField DataField="vUom" HeaderText="Uom" SortExpression="vUom" />
            <asp:BoundField DataField="vSize" HeaderText="Dimensions" SortExpression="vSize" />
            <asp:BoundField DataField="vIdscr" HeaderText="Board" 
                SortExpression="vIdscr" />
            <asp:BoundField DataField="vIcoldscr" HeaderText="Color" 
                SortExpression="vIcoldscr" />
                <asp:BoundField Visible="false" DataField="vLine" HeaderText="vLine" 
                SortExpression="vLine" />
        </Columns>
        <SelectedRowStyle CssClass="GridSelected" BackColor="Yellow" />
        <HeaderStyle BackColor="Teal" CssClass="gridrowhdr" ForeColor="White" HorizontalAlign="Center"
            VerticalAlign="Middle" Wrap="False" />
        <AlternatingRowStyle CssClass="GridItemOdd" />
    </asp:GridView>
    
    <br />
     <asp:Button ID="AddNewButton" runat="server" Text="Add" CssClass="button" OnClick="AddNewButton_Click" />
    <asp:FormView ID="FormView2" runat="server" OnDataBound="Formview2_onbatabound"    DataSourceID="ObjectDataSource_item">
        <EditItemTemplate>
        <asp:Panel ID="Panel_Edit" runat="server"  DefaultButton="UpdateButton" >
        <table class="shade">
        <tr>
        <td><b>Cust Part#:</b></td> <td><b>Item No:</b></td><td><b>Item Description:</b></td>
        <td><b>Item Description2:</b></td><td><b>Style:</b></td><td><b>Qty:</b></td><td><b>Price:</b></td>
        <td><b>Uom:</b></td><td><b>Dimensions:</b></td><td><b>Board:</b></td><td><b>Color:</b></td>
        </tr>
        <tr>
        <td><asp:TextBox ID="vPartNoTextBox" Width="120px" Enabled="false" runat="server" Text='<%# Bind("vPartNo") %>' /></td>
        <td><asp:TextBox ID="vInoTextBox" Width="120px" Enabled="false" runat="server" Text='<%# Bind("vIno") %>' /></td>
        <td><asp:TextBox ID="vPartDscr1TextBox" runat="server" Text='<%# Bind("vPartDscr1") %>' /></td>
        <td> <asp:TextBox ID="vPartDscr2TextBox" runat="server" Text='<%# Bind("vPartDscr2") %>' /></td>
        <td><asp:TextBox ID="vStyleTextBox" Enabled="false" Width="80px" runat="server" Text='<%# Bind("vStyle") %>' /></td>
        <td> <asp:TextBox ID="vQtyTextBox" Enabled="false" Width="60px" runat="server" Text='<%# Bind("vQty") %>' /></td>
        <td><asp:TextBox ID="vPriceTextBox" Enabled="false" Width="60px" runat="server" Text='<%# Bind("vPrice") %>' /></td>
        <td><asp:TextBox ID="vUomTextBox" Enabled="false" Width="30px" runat="server" Text='<%# Bind("vUom") %>' /></td>
        <td> <asp:TextBox ID="vSizeTextBox" Width="100px" runat="server" Text='<%# Bind("vSize") %>' /></td>
        <td> <asp:TextBox ID="vIdscrTextBox" Width="100px" runat="server" Text='<%# Bind("vIdscr") %>' /></td>
        <td><asp:TextBox ID="vIcoldscrTextBox" Width="60px" onblur="document.getElementById('ctl00_ContentPlaceHolder1_FormView2_vPartDscr1TextBox').focus();" runat="server" Text='<%# Bind("vIcoldscr") %>' /></td>
        <td style="display:none"> <asp:TextBox ID="vLineTextBox" runat="server" Text='<%# Bind("vLine") %>' /></td>
        </tr>
        <tr>
        <td colspan="6">
        <asp:Button ID="UpdateButton" runat="server" CausesValidation="True" CssClass="button" OnClick="Formview2_update_button_click" Text="Save" />
            &nbsp;<asp:Button ID="UpdateCancelButton" runat="server" CssClass="button" CausesValidation="False" CommandName="Cancel" Text="Cancel" />
        </td>
        </tr>
        </table>         
          </asp:Panel>  
        </EditItemTemplate>
        <InsertItemTemplate>
         <asp:Panel ID="Panel_Edit" runat="server"  DefaultButton="InsertButton" >
            <table class="shade">
        <tr>
        <td><b>Cust Part#:</b></td> <td><b>Item No:</b></td><td><b>Item Description:</b></td>
        <td><b>Item Description2:</b></td><td><b>Style:</b></td><td><b>Qty:</b></td><td><b>Price:</b></td>
        <td><b>Uom:</b></td><td><b>Dimensions:</b></td><td><b>Board:</b></td><td><b>Color:</b></td>
        </tr>
        <tr>
        <td nowrap><asp:TextBox ID="vPartNoTextBox" Width="120px" runat="server" Text='<%# Bind("vPartNo") %>' />
        <a href="#" tabindex="1" onClick="Custpartlook(); return false"><asp:Image ID="Image1" runat="server" ImageUrl="images/lookup_icon.gif" /></a></td>
        <td><asp:TextBox ID="vInoTextBox" Width="120px" Enabled="false" runat="server" Text='<%# Bind("vIno") %>' /></td>
        <td><asp:TextBox ID="vPartDscr1TextBox" runat="server" onfocus="this.select()" Text='<%# Bind("vPartDscr1") %>' /></td>
        <td> <asp:TextBox ID="vPartDscr2TextBox" runat="server" Text='<%# Bind("vPartDscr2") %>' /></td>
        <td><asp:TextBox ID="vStyleTextBox" Width="80px" runat="server" Text='<%# Bind("vStyle") %>' /></td>
        <td nowrap> <asp:TextBox ID="vQtyTextBox" Width="60px" runat="server" Text='<%# Bind("vQty") %>' />
            <asp:CompareValidator ID="CompareValidator1" runat="server" ControlToValidate="vQtyTextBox" Operator="DataTypeCheck" Type="Integer" Display="Dynamic" SetFocusOnError="true" ErrorMessage="Invalid Entry"></asp:CompareValidator>
        </td>        
        <td nowrap><asp:TextBox ID="vPriceTextBox" Width="60px" runat="server" Text='<%# Bind("vPrice") %>' />
        <asp:CompareValidator ID="CompareValidator2" runat="server" ControlToValidate="vPriceTextBox" Operator="DataTypeCheck" Type="Double" Display="Dynamic" SetFocusOnError="true" ErrorMessage="Invalid Entry"></asp:CompareValidator>
        </td>
        <td><asp:TextBox ID="vUomTextBox" Width="30px" runat="server" Text='<%# Bind("vUom") %>' /></td>
        <td> <asp:TextBox ID="vSizeTextBox" Width="100px" runat="server" Text='<%# Bind("vSize") %>' /></td>
        <td> <asp:TextBox ID="vIdscrTextBox" Width="100px" runat="server" Text='<%# Bind("vIdscr") %>' /></td>
        <td><asp:TextBox ID="vIcoldscrTextBox" Width="60px" onblur="document.getElementById('ctl00_ContentPlaceHolder1_FormView2_vPartNoTextBox').focus();" runat="server" Text='<%# Bind("vIcoldscr") %>' /></td>
        <td style="display:none"> <asp:TextBox ID="vLineTextBox" runat="server" Text='<%# Bind("vLine") %>' /></td>
        </tr>
        <tr>
        <td colspan="6">
            <asp:Button ID="InsertButton" runat="server" CausesValidation="True" OnClick="Formview2_insert_button_Click" CssClass="button" Text="Save" />
            &nbsp;<asp:Button ID="InsertCancelButton" runat="server" CssClass="button" CausesValidation="False" CommandName="Cancel" Text="Cancel" />
                </td></tr></table></asp:Panel>
        </InsertItemTemplate>
        <ItemTemplate>
            <table class="shade">
            <tr><td style="display:none">        
            vPartNo:
            <asp:Label ID="vPartNoLabel" runat="server" Text='<%# Bind("vPartNo") %>' />           
            vIno:
            <asp:Label ID="vInoLabel" runat="server" Text='<%# Bind("vIno") %>' />            
            <asp:Label ID="vPartDscr1Label" runat="server" Text='<%# Bind("vPartDscr1") %>' />            
            <asp:Label ID="vPartDscr2Label" runat="server" Text='<%# Bind("vPartDscr2") %>' />           
            <asp:Label ID="vStyleLabel" runat="server" Text='<%# Bind("vStyle") %>' />            
            <asp:Label ID="vQtyLabel" runat="server" Text='<%# Bind("vQty") %>' />            
            <asp:Label ID="vPriceLabel" runat="server" Text='<%# Bind("vPrice") %>' />           
            <asp:Label ID="vUomLabel" runat="server" Text='<%# Bind("vUom") %>' />            
            <asp:Label ID="vSizeLabel" runat="server" Text='<%# Bind("vSize") %>' />           
            <asp:Label ID="vIdscrLabel" runat="server" Text='<%# Bind("vIdscr") %>' />           
            <asp:Label ID="vIcoldscrLabel" runat="server" Text='<%# Bind("vIcoldscr") %>' />           
            <asp:Label ID="vLineLabel" runat="server" Text='<%# Bind("vLine") %>' />            
            </td>
            </tr><tr><td>
            <asp:Button ID="AddButton" runat="server"  CausesValidation="False" CssClass="buttonM" CommandName="new"
                 Text="Add">
                </asp:Button> 
            <asp:Button ID="UpdateButton" runat="server"  CssClass="button" CausesValidation="False" CommandName="edit"
                Text="Update">
            </asp:Button>
            <asp:Button ID="DeleteButton" runat="server"  CssClass="button" CausesValidation="False" OnClick="Formview2_deletebutton_Click"  OnClientClick="return confirm('Delete Currently Selected Record?')"     Text="Delete">
            </asp:Button>
            </td></tr>           
            
             </table>
            
        </ItemTemplate>
    </asp:FormView>
    
    <asp:ObjectDataSource ID="ObjectDataSource_item" runat="server" 
        OldValuesParameterFormatString="original_{0}" SelectMethod="SelectQuoteItems" 
        TypeName="QuoteDetail">
        <SelectParameters>
            <asp:Parameter Name="prmUser" Type="String" />
            <asp:Parameter DefaultValue="View" Name="prmAction" Type="String" />
            <asp:SessionParameter DefaultValue="" Name="prmQuote" SessionField="quote_no" Type="Int32" />
            <asp:Parameter Name="prmPartNo" Type="String" />
            <asp:Parameter Name="prmItemNo" Type="String" />
            <asp:Parameter Name="prmPartDscr1" Type="String" />
            <asp:Parameter Name="prmPartDscr2" Type="String" />
            <asp:Parameter Name="prmQty" Type="Int32" />
            <asp:Parameter Name="prmPrice" Type="Decimal" />
            <asp:Parameter Name="prmUom" Type="String" />
            <asp:Parameter Name="prmDimensions" Type="String" />
            <asp:Parameter Name="prmBoard" Type="String" />
            <asp:Parameter Name="prmColor" Type="String" />
            <asp:Parameter Name="prmStyle" Type="String" />
          <asp:ControlParameter ControlID="GridView1" Name="prmLine" 
                PropertyName="SelectedValue" Type="Int32" />
        </SelectParameters>
    </asp:ObjectDataSource>
    
    <asp:ObjectDataSource ID="ObjectDataSource2" runat="server" 
        OldValuesParameterFormatString="original_{0}" SelectMethod="SelectQuoteItems" 
        TypeName="QuoteDetail">
        <SelectParameters>
            <asp:Parameter Name="prmUser" Type="String" />
            <asp:Parameter Name="prmAction" DefaultValue="Select" Type="String" />
            <asp:SessionParameter DefaultValue="" Name="prmQuote" SessionField="quote_no" Type="Int32" />
            <asp:Parameter Name="prmPartNo" Type="String" />
            <asp:Parameter Name="prmItemNo" Type="String" />
            <asp:Parameter Name="prmPartDscr1" Type="String" />
            <asp:Parameter Name="prmPartDscr2" Type="String" />
            <asp:Parameter Name="prmQty" Type="Int32" />
            <asp:Parameter Name="prmPrice" Type="Decimal" />
            <asp:Parameter Name="prmUom" Type="String" />
            <asp:Parameter Name="prmDimensions" Type="String" />
            <asp:Parameter Name="prmBoard" Type="String" />
            <asp:Parameter Name="prmColor" Type="String" />
            <asp:Parameter Name="prmLine" Type="String" />
              <asp:Parameter Name="prmStyle" Type="String" />
        </SelectParameters>
    </asp:ObjectDataSource>
    
    
    
    <asp:ObjectDataSource ID="ObjectDataSource1" runat="server" OldValuesParameterFormatString="original_{0}"
        SelectMethod="SelectViewQuotes" TypeName="browsquote">
        <SelectParameters>
            <asp:Parameter Name="prmUser" Type="String" />
            <asp:Parameter DefaultValue="select" Name="prmAction" Type="String" />
            <asp:SessionParameter DefaultValue="" Name="prmQuote" SessionField="quote_no" Type="Int32" />
            <asp:Parameter Name="PrmDate" Type="DateTime" />
            <asp:Parameter Name="prmCust" Type="String" />
            <asp:Parameter Name="prmEst" Type="String" />
            <asp:Parameter Name="prmRfq" Type="String" />
            <asp:Parameter Name="prmDel" Type="DateTime" />
            <asp:Parameter Name="prmBill" Type="String" />
            <asp:Parameter Name="prmBill2" Type="String" />
            <asp:Parameter Name="prmBill3" Type="String" />
            <asp:Parameter Name="prmBill4" Type="String" />
            <asp:Parameter Name="prmShipid" Type="String" />
            <asp:Parameter Name="prmShip" Type="String" />
            <asp:Parameter Name="prmShip2" Type="String" />
            <asp:Parameter Name="prmShip3" Type="String" />
            <asp:Parameter Name="prmShip4" Type="String" />
            <asp:Parameter Name="PrmContact" Type="String" />
            <asp:Parameter Name="prmSoldid" Type="String" />
            <asp:Parameter Name="prmSold" Type="String" />
            <asp:Parameter Name="prmSold2" Type="String" />
            <asp:Parameter Name="prmSold3" Type="String" />
            <asp:Parameter Name="prmSold4" Type="String" />
            <asp:Parameter Name="prmSman" Type="String" />
            <asp:Parameter Name="prmTerms" Type="String" />
            <asp:Parameter Name="prmCarr" Type="String" />
            <asp:Parameter Name="prmZone" Type="String" />
            <asp:Parameter Name="prmStat" Type="String" />
            <asp:Parameter Name="prmPart" Type="String" />
            <asp:Parameter Name="prmCarrdscr" Type="String" />
            <asp:Parameter Name="prmTermDscr" Type="String" />
            <asp:Parameter Name="prmSName" Type="String" />
            <asp:Parameter Name="prmZondesc" Type="String" />
        </SelectParameters>
    </asp:ObjectDataSource>
   
   
</div>


</asp:Content>

