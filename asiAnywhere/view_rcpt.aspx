<%@ Page Language="C#" MasterPageFile="MasterPageRcpt.master" Debug="true" AutoEventWireup="true" Inherits="view_rcpt" Title="Warehouse Transactions Receipts" Codebehind="view_rcpt.aspx.cs" %>

<asp:Content ID="Content1" ContentPlaceHolderID="ContentPlaceHolder1"  runat="server">
<LINK REL="stylesheet" TYPE="text/css" HREF="include/CalendarControl.css" >
    <script language = "JavaScript" src="include/CalendarControl.js"></script>
    <script language="javascript" src="include/date.js"></script>
    <script language="javascript" src="include/event.js"></script>
    <script language="javascript" src="include/insert.js"></script>
<script type="text/javascript" language="javascript">

    function unitcount() {    
    window.scroll(800, 900);
}
function fglotfocus() {
    window.scroll(10, 900);
}
function coltotqty() {
    var unit = document.getElementById("ctl00_ContentPlaceHolder1_FormView1_vCasesTextBox").value;
    var count = document.getElementById("ctl00_ContentPlaceHolder1_FormView1_vQtyCasTextBox").value;
    var parsial = document.getElementById("ctl00_ContentPlaceHolder1_FormView1_vPartialTextBox").value;
    var totqty = document.getElementById("ctl00_ContentPlaceHolder1_FormView1_vT_QtyTextBox");

    totqty.value = (parseInt(unit * count)) + parseInt(parsial);
}
function locationlook() {
    var NewWindow = window.open("location_lookup.aspx", "LocationLookUpWindow", "width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}
function LocationLookUp(ReturnObj1, ReturnObj2) {
    document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vLocTextBox.value = ReturnObj1;
    document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vLocTextBox.focus();
}

function binlook() {
    var loc1 = document.forms[0].ctl00_ContentPlaceHolder1_FormView1_vLocTextBox.value;
    var NewWindow = window.open("custbin_lookup.aspx?binloc=" + loc1 + "", "BinLookUpWindow", "width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}
function CustBinLookup(ReturnObj1, ReturnObj2) {
    document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vLocBinTextBox.value = ReturnObj1;
    document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vLocBinTextBox.focus();
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
   
   var NewWindow = window.open("lpofglook.aspx", "CustomerpoWindow", "width=650,height=400,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}

function lpofglLookup(ReturnObj1, ReturnObj2, ReturnObj3, ReturnObj4, ReturnObj5, ReturnObj6, ReturnObj7, ReturnObj8, ReturnObj9, ReturnObj10, ReturnObj11, ReturnObj12) {
    document.forms[0].ctl00_ContentPlaceHolder1_FormView1_vPo_noTextBox.value = ReturnObj1;
    document.forms[0].ctl00_ContentPlaceHolder1_FormView1_vItemTextBox.value = ReturnObj2;
    document.forms[0].ctl00_ContentPlaceHolder1_FormView1_vItemNameTextBox.value = ReturnObj3;
    document.forms[0].ctl00_ContentPlaceHolder1_FormView1_vJob_noTextBox.value = ReturnObj4;
    document.forms[0].ctl00_ContentPlaceHolder1_FormView1_vJob_no2TextBox.value = ReturnObj5;
    //document.forms[0].ctl00_ContentPlaceHolder1_FormView1_vJob_noTextBox.value = ReturnObj6;
    //document.forms[0].ctl00_ContentPlaceHolder1_FormView1_vJob_no2TextBox.value = ReturnObj7;

    document.forms[0].ctl00_ContentPlaceHolder1_FormView1_vCostUomTextBox.value = ReturnObj8;
    document.forms[0].ctl00_ContentPlaceHolder1_FormView1_vStdCostTextBox.value = ReturnObj9;
    document.forms[0].ctl00_ContentPlaceHolder1_FormView1_vLocTextBox.value = ReturnObj10;
    document.forms[0].ctl00_ContentPlaceHolder1_FormView1_vLocBinTextBox.value = ReturnObj11;
    document.forms[0].ctl00_ContentPlaceHolder1_FormView1_vQtyCasTextBox.value = ReturnObj12;
    
    document.getElementById("ctl00_ContentPlaceHolder1_FormView1_vPo_noTextBox").onchange();
    
    
    
}

function fglook() {
   
    var NewWindow = window.open("lfglook.aspx", "FGLookupWindow", "width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}

function fglLookup(ReturnObj1,ReturnObj2,ReturnObj3,ReturnObj4,ReturnObj5,ReturnObj6,ReturnObj7) {
    document.forms[0].ctl00_ContentPlaceHolder1_FormView1_vItemTextBox.value = ReturnObj1;
    document.forms[0].ctl00_ContentPlaceHolder1_FormView1_vItemNameTextBox.value = ReturnObj2;
    document.forms[0].ctl00_ContentPlaceHolder1_FormView1_vLocTextBox.value = ReturnObj3;
    document.forms[0].ctl00_ContentPlaceHolder1_FormView1_vLocBinTextBox.value = ReturnObj4;
    document.forms[0].ctl00_ContentPlaceHolder1_FormView1_vStdCostTextBox.value = ReturnObj5;
    document.forms[0].ctl00_ContentPlaceHolder1_FormView1_vQtyCasTextBox.value = ReturnObj7;
    document.forms[0].ctl00_ContentPlaceHolder1_FormView1_vCasUnitTextBox.value = "1"
    document.forms[0].ctl00_ContentPlaceHolder1_FormView1_vItemTextBox.onchange();
}

function job1look() {
    var NewWindow = window.open("jobRep_lookup.aspx", "JobRepLookupWindow", "width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}
function JobRepLookup(ReturnObj1, ReturnObj2, ReturnObj3, ReturnObj4, ReturnObj5, ReturnObj6, ReturnObj7, ReturnObj8, ReturnObj9) {
    document.forms[0].ctl00_ContentPlaceHolder1_FormView1_vJob_noTextBox.value = ReturnObj1;
    document.forms[0].ctl00_ContentPlaceHolder1_FormView1_vJob_no2TextBox.value = ReturnObj2;    
    document.forms[0].ctl00_ContentPlaceHolder1_FormView1_vItemTextBox.value = ReturnObj3;
    document.forms[0].ctl00_ContentPlaceHolder1_FormView1_vItemNameTextBox.value = ReturnObj4;
    document.forms[0].ctl00_ContentPlaceHolder1_FormView1_vStdCostTextBox.value = ReturnObj5;
    document.forms[0].ctl00_ContentPlaceHolder1_FormView1_vCostUomTextBox.value = ReturnObj6;
    document.forms[0].ctl00_ContentPlaceHolder1_FormView1_vLocTextBox.value = ReturnObj7;
    document.forms[0].ctl00_ContentPlaceHolder1_FormView1_vLocBinTextBox.value = ReturnObj8;
    document.forms[0].ctl00_ContentPlaceHolder1_FormView1_vQtyCasTextBox.value = ReturnObj9;
    document.forms[0].ctl00_ContentPlaceHolder1_FormView1_vJob_noTextBox.onchange();
  
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
    <asp:FormView ID="FormView1" runat="server" OnDataBound="FormView1_DataBound" OnUnload="FormView1_Unload" DataSourceID="ObjectDataSource1">
        <EditItemTemplate>
        <asp:Panel ID="Edit_panel" runat="server" DefaultButton="UpdateButton">
        <br />
        <fieldset style="background-color:#EFF3FB">
        <table class="shade">
        <tr>
        <td nowrap align="left" style="padding-right:5px;"><b>
            Seq#: </b><br>
            <asp:Label ID="vRnoLabel" Width="55px" BackColor="PaleTurquoise" BorderColor="White" BorderStyle="Solid" BorderWidth="1px" runat="server" Text='<%# Bind("vRno") %>' />
            <br />                        
        </td>
        <td nowrap align="left" style="padding-right:5px;"><b> 
        Receipt Date: </b><br>
            <asp:TextBox ID="vDateTextBox" Width="65px" runat="server" onfocus="javascript:preEnter( this, 'yes' );" onblur="javascript:preLeave( this, 'date', '99/99/9999' );" Text='<%# Bind("vDate") %>' />
            <a href="#" tabindex="1" onblur="ctl00_ContentPlaceHolder1_FormView1_vDateTextBox.focus()" onClick="showCalendarControl(ctl00_ContentPlaceHolder1_FormView1_vDateTextBox); return false"><asp:Image ID="Image3" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
            <br />
        </td>
        <td nowrap align="left" style="padding-right:5px;"><b> 
        Receipt Time: </b><br>
            <asp:Label ID="vTransTimeLabel" Width="70px" BackColor="PaleTurquoise" BorderColor="White" BorderStyle="Solid" BorderWidth="1px" runat="server" 
                Text='<%# Bind("vTransTime") %>' />
                <br />
        </td>
        <td nowrap align="left" style="padding-right:5px;"><b> 
        Tag#: </b><br>
            <asp:TextBox ID="vTagTextBox" MaxLength="20" Width="118px" runat="server" Text='<%# Bind("vTag") %>' />
            <br />
        </td>    
                 
         <td nowrap align="left" style="padding-right:5px;"><b>   
            Po#: </b><br>
            <asp:TextBox ID="vPo_noTextBox" MaxLength="9" Width="58px" OnTextChanged="pono_TextChange" AutoPostBack="true" runat="server" Text='<%# Bind("vPo_no") %>' />
            
            <a href="#" tabindex="1" tabindex="1" onClick="customerpolook(); return false"><asp:Image ID="pofglLookup" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
            <br />
            </td>
            <td nowrap align="left" style="padding-right:5px;"><b>
            Job#: </b><br>
            <asp:TextBox ID="vJob_noTextBox" MaxLength="6" Width="40px" OnTextChanged="jobno_TextChange" AutoPostBack="true" runat="server" Text='<%# Bind("vJob_no") %>' />
            <br />
            </td>
            <td nowrap align="left" style="padding-right:5px;">
             <br>
            <asp:TextBox ID="vJob_no2TextBox" MaxLength="2" Width="15px" runat="server" 
                Text='<%# Bind("vJob_no2") %>' />
            <asp:CompareValidator ID="CompareValidator5" runat="server" ControlToValidate="vJob_no2TextBox" SetFocusOnError="true" Display="dynamic" Operator="dataTypeCheck" Type="Integer" ErrorMessage="Number Only"></asp:CompareValidator>    
            <a href="#" tabindex="1" tabindex="1" onClick="job1look(); return false"><asp:Image ID="Job1Lookup" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
            </br></td>
            <td nowrap align="left" style="padding-right:5px;"><b> 
            Item: </b><br>
            <asp:TextBox ID="vItemTextBox" MaxLength="15" OnTextChanged="ItemTextBox_Change" AutoPostBack="true" Width="95px" runat="server" Text='<%# Bind("vItem") %>' />
            <a href="#" tabindex="1" tabindex="1" onClick="fglook(); return false"><asp:Image ID="FGLookup" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
            <br />
            </td>
            <td nowrap align="left" style="padding-right:5px;"><b> 
            Name/Desc: </b><br>
            <asp:TextBox ID="vItemNameTextBox" MaxLength="30" Width="110px" runat="server" 
                Text='<%# Bind("vItemName") %>' />
            <br />
            </td>
            <td nowrap align="left" style="padding-right:5px;"><b> 
            Whse: </b><br>
            <asp:TextBox ID="vLocTextBox" MaxLength="5" Width="35px" runat="server" Text='<%# Bind("vLoc") %>' />
             <a href="#" tabindex="1" tabindex="1" onClick="locationlook(); return false"><asp:Image ID="Image1" runat="server" ImageUrl="images/lookup_icon.gif" /></a><br />
            </td>
            <td nowrap align="left" style="padding-right:5px;"><b> 
            Bin: </b><br>
            <asp:TextBox ID="vLocBinTextBox" MaxLength="8" Width="55px" runat="server" Text='<%# Bind("vLocBin") %>' />
            <a href="#" tabindex="1" tabindex="1" onClick="binlook(); return false"><asp:Image ID="Image2" runat="server" ImageUrl="images/lookup_icon.gif" /></a><br />
            </td>
            <td nowrap align="left" style="padding-right:5px;"><b> 
            Units: </b><br>
            <asp:TextBox ID="vCasesTextBox" MaxLength="6" Width="45px" onkeyup="coltotqty()" runat="server" Text='<%# Bind("vCases") %>' />
            <asp:CompareValidator ID="CompareValidator1" runat="server" ControlToValidate="vCasesTextBox" SetFocusOnError="true" Display="dynamic" Operator="dataTypeCheck" Type="Integer" ErrorMessage="Number Only"></asp:CompareValidator>
            <br />
            </td>
            <td nowrap align="left" style="padding-right:5px;"><b> 
            Units Count: </b><br>            
            <asp:TextBox ID="vQtyCasTextBox" MaxLength="6" Width="65px" onkeyup="coltotqty()" onblur="unitcount()" runat="server" Text='<%# Bind("vQtyCas") %>' />
            <asp:CompareValidator ID="CompareValidator2" runat="server" ControlToValidate="vQtyCasTextBox" SetFocusOnError="true" Display="dynamic" Operator="dataTypeCheck" Type="Integer" ErrorMessage="Number Only"></asp:CompareValidator>
            <br />
            </td>
            <td nowrap align="left" style="padding-right:5px;"><b> 
            Unit Per Pallet: </b><br>
            <asp:TextBox ID="vCasUnitTextBox" MaxLength="3" Width="75px" runat="server" 
                Text='<%# Bind("vCasUnit") %>' />
                <asp:CompareValidator ID="CompareValidator3" runat="server" ControlToValidate="vCasUnitTextBox" SetFocusOnError="true" Display="dynamic" Operator="dataTypeCheck" Type="Integer" ErrorMessage="Number Only"></asp:CompareValidator>
            <br />
            </td>
            <td nowrap align="left" style="padding-right:5px;"><b> 
            Partial: </b><br>
            <asp:TextBox ID="vPartialTextBox" MaxLength="6" onkeyup="coltotqty()" Width="45px" runat="server" 
                Text='<%# Bind("vPartial") %>' />
                <asp:CompareValidator ID="CompareValidator4" runat="server" ControlToValidate="vPartialTextBox" SetFocusOnError="true" Display="dynamic" Operator="dataTypeCheck" Type="Integer" ErrorMessage="Number Only"></asp:CompareValidator>
            <br />
            </td>
            <td nowrap align="left" style="padding-right:5px;"><b> 
            Cost/UOM: </b><br>
            <asp:TextBox ID="vStdCostTextBox"  Width="65px" runat="server" 
                Text='<%# Bind("vStdCost") %>' />
                <asp:CompareValidator ID="CompareValidator6" runat="server" ControlToValidate="vStdCostTextBox" SetFocusOnError="true" Display="dynamic" Operator="dataTypeCheck" Type="Double" ErrorMessage="Number Only"></asp:CompareValidator>
            <br />
            </td>
            <td nowrap align="left" style="padding-right:5px;"><b> 
            UOM: </b><br>
            <asp:TextBox ID="vCostUomTextBox"  MaxLength="3" Width="20px" runat="server" 
                Text='<%# Bind("vCostUom") %>' />
            <br />
            </td>
            <td nowrap align="left" style="padding-right:5px;"><b> 
            Total Qty: </b><br>
            <asp:TextBox ID="vT_QtyTextBox" Width="65px" Enabled="false" runat="server" Text='<%# Bind("vT_Qty") %>' />
            <asp:CompareValidator ID="CompareValidator7" runat="server" ControlToValidate="vT_QtyTextBox" SetFocusOnError="true" Display="dynamic" Operator="dataTypeCheck" Type="Double" ErrorMessage="Number Only"></asp:CompareValidator>
            <br />
            </td>
            <td nowrap align="left" style="padding-right:5px;"><b> 
            Freight Cost: </b><br>
            <asp:TextBox ID="vFrtCostTextBox"  Width="65px" runat="server" 
                Text='<%# Bind("vFrtCost") %>' />
                <asp:CompareValidator ID="CompareValidator8" runat="server" ControlToValidate="vFrtCostTextBox" SetFocusOnError="true" Display="dynamic" Operator="dataTypeCheck" Type="Double" ErrorMessage="Number Only"></asp:CompareValidator>
            <br />
            </td>
            <td nowrap align="left" style="padding-right:5px;"><b> 
            Extended Cost: </b><br>
            <asp:TextBox ID="vExtCostTextBox" Enabled="false" Width="77px" runat="server" 
                Text='<%# Bind("vExtCost") %>' />
                <asp:CompareValidator ID="CompareValidator9" runat="server" ControlToValidate="vExtCostTextBox" SetFocusOnError="true" Display="dynamic" Operator="dataTypeCheck" Type="Double" ErrorMessage="Number Only"></asp:CompareValidator>
            <br />
            </td>
            <td nowrap align="left" style="padding-right:5px;"><b> 
            FG Lot#: </b><br>
            <asp:TextBox ID="vStackCodeTextBox" onblur="fglotfocus()" Width="65px" runat="server" 
                Text='<%# Bind("vStackCode") %>' />
            <br />
            </td>
            <td nowrap align="left" style="padding-right:5px;"><b> 
            Created By: </b><br>
            <asp:Label ID="vCreatedByLabel" Width="65px" BackColor="PaleTurquoise" BorderColor="White" BorderStyle="Solid" BorderWidth="1px" runat="server" 
                Text='<%# Bind("vCreatedBy") %>' />
            <br />
            </td>
            <td nowrap align="left" style="padding-right:5px;"><b> 
            Last Update By: </b><br>
            <asp:Label ID="vCreate2Label" Width="78px" BackColor="PaleTurquoise" BorderColor="White" BorderStyle="Solid" BorderWidth="1px" runat="server" 
                Text='<%# Bind("vCreate2") %>' />
            <br />
            </td>
            <td nowrap align="left" style="padding-right:5px;"><b> 
            Total Weight: </b><br>
            <asp:TextBox ID="vTot_WtTextBox" Width="65px" Enabled="false" runat="server" Text='<%# Bind("vTot_Wt") %>' />
            <asp:CompareValidator ID="CompareValidator10" runat="server" ControlToValidate="vTot_WtTextBox" SetFocusOnError="true" Display="dynamic" Operator="dataTypeCheck" Type="Double" ErrorMessage="Number Only"></asp:CompareValidator>
            <br />
            </td>
            <%--<td><b> 
            vRecKey: <br>
            <asp:TextBox ID="vRecKeyTextBox" Visible="false" Width="65px" runat="server" Text='<%# Bind("vRecKey") %>' />
            <br />
            </td>--%>
            </tr>
            
            <tr>
            <td colspan="2">
            <br />
            <asp:Button ID="UpdateButton" CssClass="buttonM" OnClick="UpdateButton_Click" runat="server" CausesValidation="True" 
                CommandName="Save" Text="Save" />
            &nbsp;<asp:Button ID="UpdateCancelButton" CssClass="buttonM" runat="server" 
                CausesValidation="False" CommandName="Cancel" Text="Cancel" />
                </td>
            </tr>
        </table>
        </fieldset>
        </asp:Panel>
        </EditItemTemplate>
        <InsertItemTemplate>
         <asp:Panel ID="Insert_panel" runat="server" DefaultButton="InsertButton">
            <br />
        <fieldset style="background-color:#EFF3FB">
        <table class="shade">
        <tr>
        <td nowrap align="left" style="padding-right:5px;"><b>
            Seq#: </b><br>
            <asp:Label ID="vRnoTextBox" Width="55px" runat="server" Text='<%# Bind("vRno") %>' />
            <br />                        
        </td>
        <td nowrap align="left" style="padding-right:5px;"><b> 
        Receipt Date: </b><br>
            <asp:TextBox ID="vDateTextBox" Width="65px" runat="server" onfocus="javascript:preEnter( this, 'yes' );" onblur="javascript:preLeave( this, 'date', '99/99/9999' );" Text='<%# Bind("vDate") %>' />
            <a href="#" tabindex="1" onblur="ctl00_ContentPlaceHolder1_FormView1_vDateTextBox.focus()" onClick="showCalendarControl(ctl00_ContentPlaceHolder1_FormView1_vDateTextBox); return false"><asp:Image ID="Image3" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
            <br />
        </td>
        <td nowrap align="left" style="padding-right:5px;"><b> 
        Receipt Time: </b><br>
            <asp:Label ID="vTransTimeLabel" Width="70px" Height="17px" BackColor="PaleTurquoise" BorderColor="White" BorderStyle="Solid" BorderWidth="1px" runat="server" 
                Text='<%# Bind("vTransTime") %>' />
                <br />
        </td>
        <td nowrap align="left" style="padding-right:5px;"><b> 
        Tag#: </b><br>
            <asp:TextBox ID="vTagTextBox" MaxLength="20" Width="118px" runat="server" Text='<%# Bind("vTag") %>' />
            <br />
        </td>    
                 
         <td nowrap align="left" style="padding-right:5px;"><b>   
            Po#: </b><br>
            <asp:TextBox ID="vPo_noTextBox" MaxLength="9" Width="58px" OnTextChanged="pono_TextChange" AutoPostBack="true" runat="server" Text='<%# Bind("vPo_no") %>' />
            
            <a href="#" tabindex="1" onClick="customerpolook(); return false"><asp:Image ID="pofglLookup" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
            <br />
            </td>
            <td nowrap align="left" style="padding-right:5px;"><b>
            Job#: </b><br>
            <asp:TextBox ID="vJob_noTextBox" MaxLength="6" Width="40px" OnTextChanged="jobno_TextChange" AutoPostBack="true" runat="server" Text='<%# Bind("vJob_no") %>' />
            
            <br />
            </td>
            <td nowrap align="left" style="padding-right:5px;">
             <br>
            <asp:TextBox ID="vJob_no2TextBox" MaxLength="2" Width="15px" runat="server" 
                Text='<%# Bind("vJob_no2") %>' />
            <asp:CompareValidator ID="CompareValidator5" runat="server" ControlToValidate="vJob_no2TextBox" SetFocusOnError="true" Display="dynamic" Operator="dataTypeCheck" Type="Integer" ErrorMessage="Number Only"></asp:CompareValidator>        
            <a href="#" tabindex="1" onClick="job1look(); return false"><asp:Image ID="Job1Lookup" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
            </br></td>
            <td nowrap align="left" style="padding-right:5px;"><b> 
            Item: </b><br>
            <asp:TextBox ID="vItemTextBox" MaxLength="15" Width="95px" OnTextChanged="ItemTextBox_Change" AutoPostBack="true" runat="server" Text='<%# Bind("vItem") %>' />
            <a href="#" tabindex="1" onClick="fglook(); return false"><asp:Image ID="FGLookup" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
            <br />
            </td>
            <td nowrap align="left" style="padding-right:5px;"><b> 
            Name/Desc: </b><br>
            <asp:TextBox ID="vItemNameTextBox" MaxLength="30" Width="110px" runat="server" 
                Text='<%# Bind("vItemName") %>' />
            <br />
            </td>
            <td nowrap align="left" style="padding-right:5px;"><b> 
            Whse: </b><br>
            <asp:TextBox ID="vLocTextBox" MaxLength="5" Width="35px" runat="server" Text='<%# Bind("vLoc") %>' />
            <a href="#" tabindex="1" tabindex="1" onClick="locationlook(); return false"><asp:Image ID="Image1" runat="server" ImageUrl="images/lookup_icon.gif" /></a><br />
            </td>
            <td nowrap align="left" style="padding-right:5px;"><b> 
            Bin: </b><br>
            <asp:TextBox ID="vLocBinTextBox" MaxLength="8" Width="55px" runat="server" Text='<%# Bind("vLocBin") %>' />
            <a href="#" tabindex="1" tabindex="1" onClick="binlook(); return false"><asp:Image ID="Image4" runat="server" ImageUrl="images/lookup_icon.gif" /></a><br />
            </td>
            <td nowrap align="left" style="padding-right:5px;"><b> 
            Units: </b><br>
            <asp:TextBox ID="vCasesTextBox" MaxLength="6" Width="45px" onkeyup="coltotqty()" runat="server" Text='<%# Bind("vCases") %>' />
            <asp:CompareValidator ID="CompareValidator1" runat="server" ControlToValidate="vCasesTextBox" SetFocusOnError="true" Display="dynamic" Operator="dataTypeCheck" Type="Integer" ErrorMessage="Number Only"></asp:CompareValidator>
            <br />
            </td>
            <td nowrap align="left" style="padding-right:5px;"><b> 
            Units Count: </b><br>
            <asp:TextBox ID="vQtyCasTextBox" MaxLength="6" Width="65px" onkeyup="coltotqty()" onblur="unitcount()" runat="server" Text='<%# Bind("vQtyCas") %>' />
            <asp:CompareValidator ID="CompareValidator11" runat="server" ControlToValidate="vQtyCasTextBox" SetFocusOnError="true" Display="dynamic" Operator="dataTypeCheck" Type="Integer" ErrorMessage="Number Only"></asp:CompareValidator>
            <br />
            </td>
            <td nowrap align="left" style="padding-right:5px;"><b> 
            Unit Per Pallet: </b><br>
            <asp:TextBox ID="vCasUnitTextBox" MaxLength="3" Width="75px" runat="server" 
                Text='<%# Bind("vCasUnit") %>' />
            <asp:CompareValidator ID="CompareValidator12" runat="server" ControlToValidate="vCasUnitTextBox" SetFocusOnError="true" Display="dynamic" Operator="dataTypeCheck" Type="Integer" ErrorMessage="Number Only"></asp:CompareValidator>    
            <br />
            </td>
            <td nowrap align="left" style="padding-right:5px;"><b> 
            Partial: </b><br>
            <asp:TextBox ID="vPartialTextBox" MaxLength="6" onkeyup="coltotqty()" Width="45px" runat="server" 
                Text='<%# Bind("vPartial") %>' />
            <asp:CompareValidator ID="CompareValidator13" runat="server" ControlToValidate="vPartialTextBox" SetFocusOnError="true" Display="dynamic" Operator="dataTypeCheck" Type="Integer" ErrorMessage="Number Only"></asp:CompareValidator>    
            <br />
            </td>
            <td nowrap align="left" style="padding-right:5px;"><b> 
            Cost/UOM: </b><br>
            <asp:TextBox ID="vStdCostTextBox"  Width="65px" runat="server" 
                Text='<%# Bind("vStdCost") %>' />
            <asp:CompareValidator ID="CompareValidator6" runat="server" ControlToValidate="vStdCostTextBox" SetFocusOnError="true" Display="dynamic" Operator="dataTypeCheck" Type="Double" ErrorMessage="Number Only"></asp:CompareValidator>    
            <br />
            </td>
            <td nowrap align="left" style="padding-right:5px;"><b> 
            UOM: </b><br>
            <asp:TextBox ID="vCostUomTextBox"  MaxLength="3" Width="20px" runat="server" 
                Text='<%# Bind("vCostUom") %>' />
            <br />
            </td>
            <td nowrap align="left" style="padding-right:5px;"><b> 
            Total Qty: </b><br>
            <asp:TextBox ID="vT_QtyTextBox" Width="65px" Enabled="false" runat="server" Text='<%# Bind("vT_Qty") %>' />
            <asp:CompareValidator ID="CompareValidator14" runat="server" ControlToValidate="vT_QtyTextBox" SetFocusOnError="true" Display="dynamic" Operator="dataTypeCheck" Type="Double" ErrorMessage="Number Only"></asp:CompareValidator>
            <br />
            </td>
            <td nowrap align="left" style="padding-right:5px;"><b> 
            Freight Cost: </b><br>
            <asp:TextBox ID="vFrtCostTextBox" Width="65px"  runat="server" 
                Text='<%# Bind("vFrtCost") %>' />
            <asp:CompareValidator ID="CompareValidator15" runat="server" ControlToValidate="vFrtCostTextBox" SetFocusOnError="true" Display="dynamic" Operator="dataTypeCheck" Type="Double" ErrorMessage="Number Only"></asp:CompareValidator>    
            <br />
            </td>
            <td nowrap align="left" style="padding-right:5px;"><b> 
            Extended Cost: </b><br>
            <asp:TextBox ID="vExtCostTextBox" Enabled="false" Width="77px" runat="server" 
                Text='<%# Bind("vExtCost") %>' />
            <asp:CompareValidator ID="CompareValidator16" runat="server" ControlToValidate="vExtCostTextBox" SetFocusOnError="true" Display="dynamic" Operator="dataTypeCheck" Type="Double" ErrorMessage="Number Only"></asp:CompareValidator>    
            <br />
            </td>
            <td nowrap align="left" style="padding-right:5px;"><b> 
            FG Lot#: </b><br>
            <asp:TextBox ID="vStackCodeTextBox" onblur="fglotfocus()" Width="65px" runat="server" 
                Text='<%# Bind("vStackCode") %>' />
            <br />
            </td>
            <td nowrap align="left" style="padding-right:5px;"><b> 
            Created By: </b><br>
            <asp:Label ID="vCreatedByLabel" Height="17px" Width="65px" BackColor="PaleTurquoise" BorderColor="White" BorderStyle="Solid" BorderWidth="1px" runat="server" 
                Text='<%# Bind("vCreatedBy") %>' />
            <br />
            </td>
            <td nowrap align="left" style="padding-right:5px;"><b> 
            Last Update By: </b><br>
            <asp:Label ID="vCreate2Label" Width="78px" Height="17px" BackColor="PaleTurquoise" BorderColor="White" BorderStyle="Solid" BorderWidth="1px" runat="server" 
                Text='<%# Bind("vCreate2") %>' />
            <br />
            </td>
            <td nowrap align="left" style="padding-right:5px;"><b> 
            Total Weight: </b><br>
            <asp:TextBox ID="vTot_WtTextBox" Width="65px" Enabled="false" runat="server" Text='<%# Bind("vTot_Wt") %>' />
            <asp:CompareValidator ID="CompareValidator17" runat="server" ControlToValidate="vTot_WtTextBox" SetFocusOnError="true" Display="dynamic" Operator="dataTypeCheck" Type="Double" ErrorMessage="Number Only"></asp:CompareValidator>
            <br />
            </td>
            <%--<td><b> 
            vRecKey: <br>
            <asp:TextBox ID="vRecKeyTextBox" Visible="false" Width="65px" runat="server" Text='<%# Bind("vRecKey") %>' />
            <br />
            </td>--%>
            </tr>
            <tr>
            <td colspan="4">
            <br />
            <asp:Button ID="InsertButton" CssClass="buttonM" OnClick="InsertButton_Click" runat="server" CausesValidation="True" 
                CommandName="Save" Text="Save" />
            &nbsp;<asp:Button ID="InsertCancelButton" CssClass="buttonM" OnClick="InserCancelButton_Click" runat="server" 
                CausesValidation="False" CommandName="Cancel" Text="Cancel" />
                </td>
             </tr>
             </table>   
             </fieldset>
             </asp:Panel>
        </InsertItemTemplate>
        
        
        <ItemTemplate>
            <br />
        <fieldset style="background-color:#EFF3FB">
        <table class="shade">
        <tr>
        <td nowrap align="left" style="padding-right:5px;"><b>
            Seq#: </b><br>
            <asp:Label ID="vRnoLabel" Width="55px" BackColor="PaleTurquoise" BorderColor="White" BorderStyle="Solid" BorderWidth="1px" runat="server" Text='<%# Bind("vRno") %>' />
            <br />                        
        </td>
        <td nowrap align="left" style="padding-right:5px;"><b> 
        Receipt Date: </b><br>
            <asp:Label ID="vDateLabel" Width="65px" BackColor="PaleTurquoise" BorderColor="White" BorderStyle="Solid" BorderWidth="1px" runat="server" Text='<%# Bind("vDate") %>' />
            <br />
        </td>
        <td nowrap align="left" style="padding-right:5px;"><b> 
        Receipt Time: </b><br>
            <asp:Label ID="vTransTimeLabel" Width="70px" BackColor="PaleTurquoise" BorderColor="White" BorderStyle="Solid" BorderWidth="1px" runat="server" 
                Text='<%# Bind("vTransTime") %>' />
                <br />
        </td>
        <td nowrap align="left" style="padding-right:5px;"><b> 
        Tag#: </b><br>
            <asp:Label ID="vTagLabel" MaxLength="20" Width="118px" BackColor="PaleTurquoise" BorderColor="White" BorderStyle="Solid" BorderWidth="1px" runat="server" Text='<%# Bind("vTag") %>' />
            <br />
        </td>    
                 
         <td nowrap align="left" style="padding-right:5px;"><b>   
            Po#: </b><br>
            <asp:Label ID="vPo_noLabel" MaxLength="9" Width="58px" BackColor="PaleTurquoise" BorderColor="White" BorderStyle="Solid" BorderWidth="1px" runat="server" Text='<%# Bind("vPo_no") %>' />
            <br />
            </td>
            <td nowrap align="left" style="padding-right:5px;"><b>
            Job#: </b><br>
            <asp:Label ID="vJob_noLabel" MaxLength="6" Width="40px" BackColor="PaleTurquoise" BorderColor="White" BorderStyle="Solid" BorderWidth="1px" runat="server" Text='<%# Bind("vJob_no") %>' />
            <br />
            </td>
            <td nowrap align="left" style="padding-right:5px;">
             <br>
            <asp:Label ID="vJob_no2Label" MaxLength="2" Width="15px" BackColor="PaleTurquoise" BorderColor="White" BorderStyle="Solid" BorderWidth="1px" runat="server" 
                Text='<%# Bind("vJob_no2") %>' />
            
            </br></td>
            <td nowrap align="left" style="padding-right:5px;"><b> 
            Item: </b><br>
            <asp:Label ID="vItemLabel" MaxLength="15" Width="95px" BackColor="PaleTurquoise" BorderColor="White" BorderStyle="Solid" BorderWidth="1px" runat="server" Text='<%# Bind("vItem") %>' />
            <br />
            </td>
            <td nowrap align="left" style="padding-right:5px;"><b> 
            Name/Desc: </b><br>
            <asp:Label ID="vItemNameLabel" MaxLength="30" Width="140px" BackColor="PaleTurquoise" BorderColor="White" BorderStyle="Solid" BorderWidth="1px" runat="server" 
                Text='<%# Bind("vItemName") %>' />
            <br />
            </td>
            <td nowrap align="left" style="padding-right:5px;"><b> 
            Whse: </b><br>
            <asp:Label ID="vLocLabel" MaxLength="5" Width="35px" BackColor="PaleTurquoise" BorderColor="White" BorderStyle="Solid" BorderWidth="1px" runat="server" Text='<%# Bind("vLoc") %>' />
            <br />
            </td>
            <td nowrap align="left" style="padding-right:5px;"><b> 
            Bin: </b><br>
            <asp:Label ID="vLocBinLabel" MaxLength="8" Width="55px" BackColor="PaleTurquoise" BorderColor="White" BorderStyle="Solid" BorderWidth="1px" runat="server" Text='<%# Bind("vLocBin") %>' />
            <br />
            </td>
            <td nowrap align="left" style="padding-right:5px;"><b> 
            Units: </b><br>
            <asp:Label ID="vCasesLabel" MaxLength="6" Width="45px" BackColor="PaleTurquoise" BorderColor="White" BorderStyle="Solid" BorderWidth="1px" runat="server" Text='<%# Bind("vCases") %>' />
            <br />
            </td>
            <td nowrap align="left" style="padding-right:5px;"><b> 
            Units Count: </b><br>
            <asp:Label ID="vQtyCasLabel" MaxLength="6" Width="65px" BackColor="PaleTurquoise" BorderColor="White" BorderStyle="Solid" BorderWidth="1px" runat="server" Text='<%# Bind("vQtyCas") %>' />
            <br />
            </td>
            <td nowrap align="left" style="padding-right:5px;"><b> 
            Unit Per Pallet: </b><br>
            <asp:Label ID="vCasUnitLabel" MaxLength="3" Width="75px" BackColor="PaleTurquoise" BorderColor="White" BorderStyle="Solid" BorderWidth="1px" runat="server" 
                Text='<%# Bind("vCasUnit") %>' />
            <br />
            </td>
            <td nowrap align="left" style="padding-right:5px;"><b> 
            Partial: </b><br>
            <asp:Label ID="vPartialLabel" MaxLength="6" Width="45px" BackColor="PaleTurquoise" BorderColor="White" BorderStyle="Solid" BorderWidth="1px" runat="server" 
                Text='<%# Bind("vPartial") %>' />
            <br />
            </td>
            <td nowrap align="left" style="padding-right:5px;"><b> 
            Cost/UOM: </b><br>
            <asp:Label ID="vStdCostLabel" Width="65px" BackColor="PaleTurquoise" BorderColor="White" BorderStyle="Solid" BorderWidth="1px" runat="server" 
                Text='<%# Bind("vStdCost") %>' />
            <br />
            </td>
            <td nowrap align="left" style="padding-right:5px;"><b> 
            UOM: </b><br>
            <asp:Label ID="vCostUomLabel" MaxLength="3" Width="20px" BackColor="PaleTurquoise" BorderColor="White" BorderStyle="Solid" BorderWidth="1px" runat="server" 
                Text='<%# Bind("vCostUom") %>' />
            <br />
            </td>
            <td nowrap align="left" style="padding-right:5px;"><b> 
            Total Qty: </b><br>
            <asp:Label ID="vT_QtyLabel" Width="65px" BackColor="PaleTurquoise" BorderColor="White" BorderStyle="Solid" BorderWidth="1px" runat="server" Text='<%# Bind("vT_Qty") %>' />
            <br />
            </td>
            <td nowrap align="left" style="padding-right:5px;"><b> 
            Freight Cost: </b><br>
            <asp:Label ID="vFrtCostLabel" Width="65px" BackColor="PaleTurquoise" BorderColor="White" BorderStyle="Solid" BorderWidth="1px" runat="server" 
                Text='<%# Bind("vFrtCost") %>' />
            <br />
            </td>
            <td nowrap align="left" style="padding-right:5px;"><b> 
            Extended Cost: </b><br>
            <asp:Label ID="vExtCostLabel" Width="77px" BackColor="PaleTurquoise" BorderColor="White" BorderStyle="Solid" BorderWidth="1px" runat="server" 
                Text='<%# Bind("vExtCost") %>' />
            <br />
            </td>
            <td nowrap align="left" style="padding-right:5px;"><b> 
            FG Lot#: </b><br>
            <asp:Label ID="vStackCodeLabel" Width="65px" BackColor="PaleTurquoise" BorderColor="White" BorderStyle="Solid" BorderWidth="1px" runat="server" 
                Text='<%# Bind("vStackCode") %>' />
            <br />
            </td>
            <td nowrap align="left" style="padding-right:5px;"><b> 
            Created By: </b><br>
            <asp:Label ID="vCreatedByLabel" Width="65px" BackColor="PaleTurquoise" BorderColor="White" BorderStyle="Solid" BorderWidth="1px" runat="server" 
                Text='<%# Bind("vCreatedBy") %>' />
            <br />
            </td>
            <td nowrap align="left" style="padding-right:5px;"><b> 
            Last Update By: </b><br>
            <asp:Label ID="vCreate2Label" Width="78px" BackColor="PaleTurquoise" BorderColor="White" BorderStyle="Solid" BorderWidth="1px" runat="server" 
                Text='<%# Bind("vCreate2") %>' />
            <br />
            </td>
            <td nowrap align="left" style="padding-right:5px;"><b> 
            Total Weight: </b><br>
            <asp:Label ID="vTot_WtLabel" Width="65px" BackColor="PaleTurquoise" BorderColor="White" BorderStyle="Solid" BorderWidth="1px" runat="server" Text='<%# Bind("vTot_Wt") %>' />
            <br />
            </td>
            <%--<td><b> 
            vRecKey: <br>
            <asp:TextBox ID="vRecKeyTextBox" Visible="false" Width="65px" runat="server" Text='<%# Bind("vRecKey") %>' />
            <br />
            </td>--%>
            </tr>
            <tr>

    <td colspan="6">
    <br />
        <asp:Button ID="addButton" runat="server" CommandName="new"  CssClass="button" Text="Add"></asp:Button>
        <asp:Button ID="UpdatButton"  runat="server" CommandName="Edit" CssClass="buttonM"  Text="Update" />
        <%--<asp:Button ID="copybutton" runat="server" CommandName="Edit" OnClick="CopyButton_click" CssClass="buttonM" Text="Copy"  OnClick="DeleteButton_Click" />--%>
        <asp:Button ID="deleteButton" runat="server" CssClass="button" CausesValidation="False" Text="Delete" OnClick="DeleteButton_Click"  OnClientClick="return confirm('Are you sure you want to delete this record')"></asp:Button>
    </td>
  </tr>
  </table>
  </fieldset>
        </ItemTemplate>
    </asp:FormView>
    <br />
    
    <asp:ObjectDataSource ID="ObjectDataSource1" runat="server" OldValuesParameterFormatString="original_{0}"
        SelectMethod="ViewRcpt" TypeName="itemhistory">
        <SelectParameters>            
            <asp:Parameter Name="prmUser"  Type="String" />            
            <asp:Parameter Name="prmAction" DefaultValue="Select"  Type="String"  />
            <asp:Parameter Name="prmFgItem"  Type="String" />
            <asp:Parameter Name="prmJobno"  Type="String" />
            <asp:Parameter Name="prmPono"  Type="String" />
            <asp:SessionParameter Name="prmSeqno" SessionField="seqno" Type="String" />
            <asp:Parameter Name="prmRcptDate"  Type="String" />
            <asp:Parameter Name="prmTagno"  Type="String" />
            <asp:Parameter Name="prmTransTime" Type="String"></asp:Parameter>
            <asp:Parameter Name="prmJob_no2" Type="String" />
            <asp:Parameter Name="prmName" Type="String" />
            <asp:Parameter Name="prmLoc" Type="String" />
            <asp:Parameter Name="prmLocBin" Type="String" />
            <asp:Parameter Name="prmCases" Type="String" />
            <asp:Parameter Name="prmQty_Cas" Type="String" />
            <asp:Parameter Name="prmCasUnit" Type="String" />
            <asp:Parameter Name="prmPartial" Type="String" />
            <asp:Parameter Name="prmStdCost" Type="String" />
            <asp:Parameter Name="prmCost_Uom" Type="String" />
            <asp:Parameter Name="prmTQty" Type="String" />
            <asp:Parameter Name="prmFrtCost" Type="String" />
            <asp:Parameter Name="prmExtCost" Type="String" />
            <asp:Parameter Name="prmStackCode" Type="String" />
            <asp:Parameter Name="prmCreatedBy" Type="String" />
            <asp:Parameter Name="prmCreate2" Type="String" />
            <asp:Parameter Name="prmTotWt" Type="String" />
            <asp:Parameter Name="prmRecKey" Type="String" />
        </SelectParameters>
    </asp:ObjectDataSource>
</div>

</asp:Content>
