<%@ Page Language="C#" MasterPageFile="MasterPage5.master" Debug="true" AutoEventWireup="true" Inherits="rfqitem" Title="Request for Quote" Codebehind="rfq_item.aspx.cs" %>

<asp:Content ID="Content1" ContentPlaceHolderID="ContentPlaceHolder1" runat="server">

<script language="javascript" src="include/CalendarControl.js"></script>
<script language="javascript" src="include/date.js"></script>
<script language="javascript" src="include/event.js"></script>
<script language="javascript" src="include/insert.js"></script>

<script type="text/javascript">
function showQty()
{
    var show=document.getElementById("Qtyhelp");
    show.style.display='inline';
    var originalqty=document.getElementById("ctl00_ContentPlaceHolder1_FormView1_RfqQtyTextBox");
    var qty=document.getElementById("ctl00_ContentPlaceHolder1_FormView1_TextBox1");
    var style = document.getElementById("ctl00_ContentPlaceHolder1_FormView1_RfqstyleTextBox");

    if (originalqty.value <= 0 || isNaN(originalqty.value))
    {
        alert("Qty must be greater than 0");                
        originalqty.focus();
    }
    else {               
            qty.value = originalqty.value;
            document.forms[0].ctl00$ContentPlaceHolder1$FormView1$lv_delivery_1.value = "1";
            document.forms[0].ctl00$ContentPlaceHolder1$FormView1$lv_uom_1.value = "M";           
            qty.focus();
        }        
}
function openstyle() {
    var NewWindow = window.open("style_bro.aspx", "StyleBroWindow", "width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}

function defaultVal(val) {
    var str = "ctl00$ContentPlaceHolder1$FormView1$lv_qty_" + val;    
    qtyval = eval("document.forms[0]." + str + ".value");

    if (parseFloat(qtyval) > 0) {
        eval("document.forms[0].ctl00$ContentPlaceHolder1$FormView1$lv_delivery_" + val + ".value=1");
        var uomval = eval("document.forms[0].ctl00$ContentPlaceHolder1$FormView1$lv_uom_" + val);
        uomval.value = "M";
    }
    else if (parseFloat(qtyval) <= 0) {
        eval("document.forms[0].ctl00$ContentPlaceHolder1$FormView1$lv_delivery_" + val + ".value=");
        var uomval = eval("document.forms[0].ctl00$ContentPlaceHolder1$FormView1$lv_uom_" + val);
        uomval.value = "";
    }  
}

function reverseqty()
{
  
  var originalqty=document.getElementById("ctl00_ContentPlaceHolder1_FormView1_RfqQtyTextBox");
  var qty=document.getElementById("ctl00_ContentPlaceHolder1_FormView1_TextBox1");
  originalqty.value=qty.value;
}
 
 function QPartlook()
  { 
  var NewWindow = window.open("QFgItemLook.aspx?from=custpart","FgItemWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
  }

function QPartLookup(ReturnObj1)
{
    document.forms[0].ctl00$ContentPlaceHolder1$FormView1$RfqPartnoTextBox.value = ReturnObj1;
    document.getElementById("ContentPlaceHolder1_FormView1_RfqPartnoTextBox").focus();
 
}
  function QFgItemlook()
  { 
  var NewWindow = window.open("QFgItemLook.aspx?from=fgitm","FgItemWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
  }

function QFgItemLookUp(ReturnObj1,ReturnObj2,ReturnObj3,ReturnObj4,ReturnObj5,ReturnObj6,ReturnObj7,ReturnObj8,ReturnObj9,ReturnObj10,ReturnObj11,ReturnObj12,ReturnObj13)
{ 
  document.forms[0].ctl00$ContentPlaceHolder1$FormView1$RfqStockTextBox.value = ReturnObj1;
  document.forms[0].ctl00$ContentPlaceHolder1$FormView1$RfqNameTextBox.value=ReturnObj2;
  document.forms[0].ctl00$ContentPlaceHolder1$FormView1$RfqPartnoTextBox.value = ReturnObj3;
  document.forms[0].ctl00$ContentPlaceHolder1$FormView1$RfqstyleTextBox.value = ReturnObj4;
  document.forms[0].ctl00$ContentPlaceHolder1$FormView1$RfqProcatTextBox .value=ReturnObj5;
  document.forms[0].ctl00$ContentPlaceHolder1$FormView1$RfqColTextBox.value = ReturnObj6;
  document.forms[0].ctl00$ContentPlaceHolder1$FormView1$RfqCoatTextBox.value = ReturnObj7;
  document.forms[0].ctl00$ContentPlaceHolder1$FormView1$RfqLengthTextBox.value=ReturnObj8;
  document.forms[0].ctl00$ContentPlaceHolder1$FormView1$RfqWidthTextBox.value = ReturnObj9;
  document.forms[0].ctl00$ContentPlaceHolder1$FormView1$RfqDepthTextBox.value = ReturnObj10;
  
 document.forms[0].ctl00$ContentPlaceHolder1$FormView1$HiddenField1.value=ReturnObj11;
 document.forms[0].ctl00$ContentPlaceHolder1$FormView1$RfqBoardTextBox.value=ReturnObj12;
 document.forms[0].ctl00$ContentPlaceHolder1$FormView1$RfqCalTextBox.value = ReturnObj13;
 document.getElementById("ctl00_ContentPlaceHolder1_FormView1_RfqPartnoTextBox").focus();
  
}
function stylelook(){ 
  var NewWindow = window.open("StyleLookup.aspx","StyleWindow","width=600,height=650,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}

function styleLookUp(ReturnObj1){
    document.forms[0].ctl00$ContentPlaceHolder1$FormView1$RfqstyleTextBox.value = ReturnObj1;
    document.getElementById("ctl00_ContentPlaceHolder1_FormView1_RfqstyleTextBox").focus();
  
}
function Boardlook1(){ 
  var NewWindow = window.open("BoardLook1.aspx","BoardWindow","width=500,height=300,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}

function BoardLookUp1(ReturnObj1, ReturnObj2){ 
  document.forms[0].ctl00$ContentPlaceHolder1$FormView1$RfqBoardTextBox.value = ReturnObj1;
  document.forms[0].ctl00$ContentPlaceHolder1$FormView1$RfqCalTextBox.value = ReturnObj2;
  document.getElementById("ctl00_ContentPlaceHolder1_FormView1_RfqBoardTextBox").focus();
}
 function categorylookup(){ 
  var NewWindow =window.open("CategoryLookup.aspx","CategoryWindow","width=600,height=420,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}

function categoryLookUp(ReturnObj1){
    document.forms[0].ctl00$ContentPlaceHolder1$FormView1$RfqProcatTextBox.value = ReturnObj1;
    document.getElementById("ctl00_ContentPlaceHolder1_FormView1_RfqProcatTextBox").focus();  
}
 
 function validate(evt)
 {
   var charcode = (evt.which) ? evt.which : event.keyCode
   if(charcode > 31 && (charcode < 48 || charcode > 57) )
   return false;
   
   return true;


}
var prevval = "";
function getprevval(obj) {
    prevval = obj.value;
}

function checkNumVal(obj, tval) {
    if (isNaN(obj.value)) {
        if (tval == "q") {
            alert("Please Enter Valid Quantity");
            //obj.value = prevval;
            obj.focus();
            return;
        }
        if (tval == "d") {
            alert("Please Enter Valid Delivery");
            obj.value = prevval;
        }
        if (tval == "p") {
            alert("Please Enter Valid Price");
            obj.value = prevval;
        }
    }   

   
}

function displayqty(obj,tval) {
    
    if (parseInt(obj.value) <= 0 || obj.value == "") {
        
            var show = document.getElementById("Qtyhelp");
            var desc = document.getElementById("ctl00_ContentPlaceHolder1_FormView1_RfqNameTextBox");
            var fgitem = document.getElementById("ctl00_ContentPlaceHolder1_FormView1_RfqStockTextBox");
            show.style.display = 'none';

            if (tval == "1") {
                desc.focus();
            }
            else fgitem.focus();
        
    }
}

function checkUom(obj) {
    var uomval = (obj.value).toUpperCase();    

    if (uomval != "" && uomval != "M" && uomval != "EA" && uomval != "L" && uomval != "CS" && uomval != "C" && uomval != "LB" && uomval != "DRM" && uomval != "ROL" && uomval != "PKG" && uomval != "SET" && uomval != "DOZ" && uomval != "BDL") {
        alert("Invalid UOM");
        obj.value = prevval;
        return;
    }
}
 
 function checkNumeric(objName,comma,period)
{
	var numberfield = objName;
	if (chkNumeric(objName,comma,period) == false)
	{
		numberfield.select();
		numberfield.focus();
		return false;
	}
	else
	{
		return true;
	}
}

function chkNumeric(objName,comma,period)
{
var checkOK = "0123456789" + comma + period ;
var checkStr = objName;
var allValid = true;
var decPoints = 0;
var allNum = "";

for (i = 0;  i < checkStr.value.length;  i++)
{
ch = checkStr.value.charAt(i);
for (j = 0;  j < checkOK.length;  j++)
if (ch == checkOK.charAt(j))
break;
if (j == checkOK.length)
{
allValid = false;
break;
}
if (ch != ",")
allNum += ch;
}
if (!allValid)
{	
alertsay = "Please enter only these values \""
alertsay = alertsay + checkOK + "\" in the \"" + checkStr.name + "\" field."
alert(alertsay);
return (false);
}
}
//if(checkStr.value<=0)
//{
//document.getElementById("ctl00_ContentPlaceHolder1_FormView1_Label7").innerHTML="Must have some value";

//checkStr.focus();
//}
//else
//{
//document.getElementById("ctl00_ContentPlaceHolder1_FormView1_Label7").innerHTML="";
//}
//if(checkStr.value<=0)
//{
//document.getElementById("ctl00_ContentPlaceHolder1_FormView1_Label8").innerHTML="Must have some value";
//checkStr.focus();
//}
//else
//{
//document.getElementById("ctl00_ContentPlaceHolder1_FormView1_Label8").innerHTML="";
//}

var val1 = "";

function uomlook(value) {
    val1 = value;
    var NewWindow = window.open("Uom_lookup.aspx", "UomLookupWindow", "width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}
function UomLookup(ReturnObj1, ReturnObj2) {
    var uomval = eval("document.forms[0].ctl00$ContentPlaceHolder1$FormView1$lv_uom_" + val1);
    uomval.value = ReturnObj1;
    document.getElementById("ctl00_ContentPlaceHolder1_FormView1_lv_uom_" + val1).focus();
} 


function decimalval()
{

var positionOfC = document.forms[0].ctl00$ContentPlaceHolder1$FormView1$RfqLengthTextBox.value;
var positionOfC1 = document.forms[0].ctl00$ContentPlaceHolder1$FormView1$RfqWidthTextBox.value;
var positionOfC2 = document.forms[0].ctl00$ContentPlaceHolder1$FormView1$RfqDepthTextBox.value;

if(positionOfC>1)
{

var val=positionOfC.indexOf(".");
if(val==-1)
{
if(positionOfC.length>1 && positionOfC.length<3)
document.forms[0].ctl00$ContentPlaceHolder1$FormView1$RfqLengthTextBox.value=positionOfC + ".";
}
}

if(positionOfC1>1)
{
var val=positionOfC1.indexOf(".");
if(val==-1)
{
if(positionOfC1.length>1 && positionOfC1.length<3)
document.forms[0].ctl00$ContentPlaceHolder1$FormView1$RfqWidthTextBox.value=positionOfC1 + ".";
}
}

if(positionOfC2>1)
{
var val=positionOfC2.indexOf(".");
if(val==-1)
{
if(positionOfC2.length>1 && positionOfC2.length<3)
document.forms[0].ctl00$ContentPlaceHolder1$FormView1$RfqDepthTextBox.value=positionOfC2 + ".";
}
}
}
</script>

<div>
<fieldset style="background-color:#EFF3FB; width:700px;">
<legend>Reference Information</legend>
    <asp:FormView ID="FormView2" runat="server" DataSourceID="ObjectDataSource3" Width="700px">
        
        <ItemTemplate>
        
           <b>RFQ#:</b>
            <asp:Label ID="aRfqNoLabel" runat="server" BackColor="Turquoise" Width="143px" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" Text='<%# Bind("aRfqNo") %>'></asp:Label>
            &nbsp;&nbsp;&nbsp;&nbsp;
           <b> Customer#:</b>
            <asp:Label ID="aCustNoLabel" runat="server" BackColor="Turquoise" Width="163px" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" Text='<%# Bind("aCustNo") %>'></asp:Label>
            &nbsp;&nbsp;&nbsp;&nbsp;
           <b> Requested Date:</b>
            <asp:Label ID="vRfqDtLabel" runat="server" BackColor="Turquoise" Width="143px" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" Text='<%# Bind("vRfqDt","{0:MM/dd/yyyy}") %>'></asp:Label>
        </ItemTemplate>
    </asp:FormView>
    
</fieldset>
    <asp:ObjectDataSource ID="ObjectDataSource3" runat="server" OldValuesParameterFormatString="original_{0}"
        SelectMethod="RfqItemDscr" TypeName="rfqs">
        <SelectParameters>
            <asp:Parameter Name="prmUser" Type="String" />
            <asp:SessionParameter Name="prmRfqNo" SessionField="Rfqseq" Type="Int32" />
            <asp:SessionParameter SessionField="list_rfq_cust_part_no" Name="prmPartNo" Type="string" />
        </SelectParameters>
    </asp:ObjectDataSource>
</div>
<div>
    <br />
     
     
     <asp:Button ID="AddNewButton" runat="server" Text="Add" CssClass="buttonM" OnClick="AddNewButton_Click" />
    
    
    <asp:FormView ID="FormView1" runat="server" DataSourceID="ObjectDataSource2" OnDataBound="FormView1_DataBound" OnPreRender="FormView1_PreRender" OnUnload="FormView1_Unload">
      <EditItemTemplate>
       <asp:Panel ID="Panel1" runat="server" DefaultButton="Button1">
        <asp:HiddenField ID="HiddenField1" runat="server" />        
        <fieldset style="background-color:#EFF3FB">  
        <table>
            <tr><td>     
            
        <table class="shade" style="width: 700px">
        <tr>                                
            <td nowrap align="left" style="width: 92px; padding-right:5px;"><b>Seq No:</b><br /><b><asp:Label ID="Label1" runat="server" Text='<%# Bind("RfqSeqNo") %>'></asp:Label></b></td>
                           
            <td nowrap align="left" style="padding-right:5px;"><b>Qty:</b><br /><b><asp:TextBox ID="RfqQtyTextBox" Width="50px" MaxLength="8" onkeypress="return validate(event)" onblur="showQty()" runat="server" Text='<%# Bind("RfqQty","{0:#########}") %>'>
                </asp:TextBox>
                <%--<asp:CompareValidator ID="CompareValidator8" runat="server" ErrorMessage="Invalid Entry" ControlToValidate="RfqQtyTextBox" Display="dynamic" SetFocusOnError="true" Operator="DataTypeCheck" Type="Integer"></asp:CompareValidator>--%>
                <%--<asp:RequiredFieldValidator ID="RequiredFieldValidator3" runat="server" ErrorMessage="Cannot be empty" ControlToValidate="RfqQtyTextBox" Display="dynamic" SetFocusOnError="true"></asp:RequiredFieldValidator>--%>
                </b></td>
                   
         <td nowrap  align="left" style="padding-right:5px;"><b>FG Item#:</b><br /><b><asp:TextBox Enabled="false" ID="RfqStockTextBox" Width="90px" onkeyup="QFgItemlook()" MaxLength="15" runat="server" Text='<%# Bind("RfqStock") %>'>
        
            </asp:TextBox>
            </td>
        <td nowrap align="left" style="padding-right:5px;"><b>Item Name:</b><br /><b><asp:TextBox ID="RfqNameTextBox" MaxLength="30" Width="160px" runat="server" Text='<%# Bind("RfqName") %>'>
            </asp:TextBox>   <asp:RequiredFieldValidator ID="RequiredFieldValidator3" runat="server" ErrorMessage="Cannot be empty" ControlToValidate="RfqNameTextBox" Display="dynamic" SetFocusOnError="true"></asp:RequiredFieldValidator>         
            </b></td>
         <td nowrap align="left" style="padding-right:5px;"><b>Customer Part#:</b><br /><b><asp:TextBox MaxLength="15" Width="90px"  ID="RfqPartnoTextBox" runat="server" Text='<%# Bind("RfqPartno") %>'>
            </asp:TextBox><a href="#" tabindex="1" onClick="QPartlook(); return false" ><asp:Image ID="Image2" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
            <asp:RequiredFieldValidator ID="RequiredFieldValidator5" runat="server" ErrorMessage="Cannot be empty" ControlToValidate="RfqPartnoTextBox" Display="dynamic" SetFocusOnError="true"></asp:RequiredFieldValidator>
            </b></td>
            
        <td nowrap align="left" style="padding-right:5px;"><b>Category:</b><br /><b><asp:TextBox Width="50px" MaxLength="6" ID="RfqProcatTextBox" AutoPostBack="true" OnTextChanged="CategoryTextChanged" runat="server" Text='<%# Bind("RfqProcat") %>'>
            </asp:TextBox>
            <a href="#" tabindex="1" onClick="categorylookup(); return false" ><asp:Image ID="Image5" runat="server" ImageUrl="images/lookup_icon.gif" />
            <asp:RequiredFieldValidator ID="RequiredFieldValidator7" runat="server" ErrorMessage="Cannot be empty" ControlToValidate="RfqProcatTextBox" Display="dynamic" SetFocusOnError="true"></asp:RequiredFieldValidator>
            </b></td>
            
            <td style="display:none">
                <asp:HiddenField ID="HiddenField2" Value='<%# Bind("RfqProcat") %>' runat="server" />
            </td>
          <td nowrap align="left" style="padding-right:5px;"><b>Color:</b><br /><b><asp:TextBox ID="RfqColTextBox" Width="30px" MaxLength="2" onkeypress="return validate(event)" runat="server" Text='<%# Bind("RfqCol") %>'></asp:TextBox></b>
            <asp:CompareValidator ID="CompareValidator5" runat="server" ControlToValidate="RfqColTextBox" SetFocusOnError="true" Display="dynamic" Operator="dataTypeCheck" Type="Integer" ErrorMessage="Invalid Color"></asp:CompareValidator>
        </td>            
       
        <td nowrap align="left" style="padding-right:5px;"><b>Coating:</b><br /><b><asp:TextBox MaxLength="2" Width="20px" onkeypress="return validate(event)" ID="RfqCoatTextBox" runat="server" Text='<%# Bind("RfqCoat") %>'></asp:TextBox></b>        
            <asp:CompareValidator ID="CompareValidator6" runat="server" ControlToValidate="RfqCoatTextBox" SetFocusOnError="true" Display="dynamic" Operator="dataTypeCheck" Type="Integer" ErrorMessage="Invalid Coating"></asp:CompareValidator>  
        </td>
        
        <td nowrap align="left" style="padding-right:5px;"><b>Style:</b><br /><b><asp:TextBox ID="RfqstyleTextBox" Width="40px" MaxLength="6" AutoPostBack="true" OnTextChanged="StyleTextChanged" runat="server" Text='<%# Bind("Rfqstyle") %>'>
            </asp:TextBox>
            <a href="#" tabindex="1" onClick="stylelook(); return false" ><asp:Image ID="Image4" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
            <input id="Button3"  type="button" onclick="openstyle()" value="Style Image" />
            <asp:RequiredFieldValidator ID="RequiredFieldValidator6" runat="server" ErrorMessage="Cannot be empty" ControlToValidate="RfqstyleTextBox" Display="dynamic" SetFocusOnError="true"></asp:RequiredFieldValidator>
            </b></td>
         <td nowrap align="left" style="padding-right:5px;"><b>Length:</b><br /><b><asp:TextBox ID="RfqLengthTextBox" Width="50px" onBlur="checkNumeric(this,',','.');" onkeyup="javascript:decimalval();" MaxLength="8" runat="server" Text='<%# Bind("RfqLength","{0:###,##0.00}") %>'>
            </asp:TextBox>
            <asp:RequiredFieldValidator ID="RequiredFieldValidator1" runat="server" ErrorMessage="Cannot be empty" ControlToValidate="RfqLengthTextBox" Display="dynamic" SetFocusOnError="true"></asp:RequiredFieldValidator>
            <asp:RangeValidator ID="RangeValidator2" runat="server" ControlToValidate="RfqLengthTextBox" SetFocusOnError="true" Display="dynamic" MinimumValue="00000.01" MaximumValue="99999.99" ErrorMessage="Must have some value"></asp:RangeValidator>
            <asp:CompareValidator ID="CompareValidator1" runat="server" ErrorMessage="Only single decimal" ControlToValidate="RfqLengthTextBox" Display="dynamic" SetFocusOnError="true" Operator="DataTypeCheck" Type="double"></asp:CompareValidator>
            </b></td> 
            <td nowrap align="left" style="padding-right:5px;"><b>Width:</b><br /><b><asp:TextBox ID="RfqWidthTextBox" Width="50px" onBlur="checkNumeric(this,',','.');" onkeyup="javascript:decimalval();" MaxLength="8" runat="server" Text='<%# Bind("RfqWidth","{0:###,##0.00}") %>'>
            </asp:TextBox>
            <asp:RequiredFieldValidator ID="RequiredFieldValidator2" runat="server" ErrorMessage="Cannot be empty" ControlToValidate="RfqWidthTextBox" Display="dynamic" SetFocusOnError="true"></asp:RequiredFieldValidator>
            <asp:RangeValidator ID="RangeValidator1" runat="server" ControlToValidate="RfqWidthTextBox" SetFocusOnError="true" Display="dynamic" MinimumValue="00000.01" MaximumValue="99999.99" ErrorMessage="Must have some value"></asp:RangeValidator>
            <asp:CompareValidator ID="CompareValidator2" runat="server" ErrorMessage="Only single decimal" ControlToValidate="RfqWidthTextBox" Display="dynamic" SetFocusOnError="true" Operator="DataTypeCheck" Type="double"></asp:CompareValidator>
            </b></td>
            
        <td nowrap align="left" style="padding-right:5px;"><b>Depth:</b><br /><b><asp:TextBox ID="RfqDepthTextBox" Width="50px" MaxLength="8" onkeyup="javascript:decimalval();" onBlur="checkNumeric(this,',','.');" runat="server" Text='<%# Bind("RfqDepth","{0:###,##0.00}") %>'>
            </asp:TextBox>
            <asp:CompareValidator ID="CompareValidator3" runat="server" ErrorMessage="Only single decimal" ControlToValidate="RfqDepthTextBox" Display="dynamic" SetFocusOnError="true" Operator="DataTypeCheck" Type="double"></asp:CompareValidator>
            </b></td>    
        
        <td nowrap align="left" style=" padding-right:5px;"><b>Board:</b><br /><b><asp:TextBox ID="RfqBoardTextBox" Width="65px" MaxLength="10" AutoPostBack="true" OnTextChanged="BoardTextChanged" runat="server" Text='<%# Bind("RfqBoard") %>'>
            </asp:TextBox><a href="#" tabindex="1" onClick="Boardlook1(); return false"><asp:Image ID="Image3" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
            <asp:RequiredFieldValidator ID="RequiredFieldValidator8" runat="server" ErrorMessage="Cannot be empty" ControlToValidate="RfqBoardTextBox" Display="dynamic" SetFocusOnError="true"></asp:RequiredFieldValidator>
            </b></td>
        <td nowrap align="left" style="padding-right:5px;"><b>Caliper:</b><br /><b><asp:TextBox ID="RfqCalTextBox" Width="50px" Enabled="false" MaxLength="1" runat="server" Text='<%# Bind("RfqCal","{0:###,##0.00000}") %>'>
            </asp:TextBox></b></td>
            
                  
       
         <td nowrap align="left" style="padding-right:5px"><b>Qty/Set:</b><br /><b><asp:Label ID="qtysetLabel" runat="server" BackColor="PaleTurquoise" Width="60px" BorderColor="White" BorderStyle="Solid" BorderWidth="1px" Text='<%# Bind("qtyset") %>'></asp:Label></b></td>
              
        </tr>
              
        </table>    
            
            <table id="Qtyhelp" align="left" width="400px" style="display:none;">
            <tr>
            <td><b>Quantity</b></td>
            <td><b>Deliveries</b></td>
            <td><b>Price</b></td>
            <td><b>UOM</b></td>
            <td><b>Date</b></td>
            </tr>
            <tr>
            <td Width="70px"><b><asp:TextBox ID="TextBox1" onfocus="getprevval(this)" onblur="reverseqty();checkNumVal(this, 'q')" MaxLength="8" Width="50px" runat="server"></asp:TextBox></b></td>
            <td Width="30px"><b><asp:TextBox ID="lv_delivery_1" MaxLength="3" Width="30px" onfocus="getprevval(this)" onblur="checkNumVal(this, 'd')" Text='<%# Bind("lv_delivery_1") %>' runat="server"></asp:TextBox></b></td>
            <td Width="70px"><b><asp:TextBox ID="lv_price_1" MaxLength="5" Width="40px" onfocus="getprevval(this)" onblur="checkNumVal(this, 'p')" Text='<%# Bind("lv_price_1") %>' runat="server"></asp:TextBox></b></td>
            <td Width="70px"><b><asp:TextBox ID="lv_uom_1" MaxLength="3" Width="35px" onfocus="getprevval(this)" onblur="checkUom(this)" Text='<%# Bind("lv_uom_1") %>' runat="server"></asp:TextBox></b>
                <a href="#" tabindex="1" onClick="uomlook('1'); return false"><asp:Image ID="Image34" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
            </td>
            <td Width="100px" nowrap><b><asp:TextBox ID="lv_date_1" onfocus="javascript:preEnter( this, 'yes' );" onblur="javascript:preLeave( this, 'date', '99/99/9999' );" MaxLength="10" Width="70px" Text='<%# Bind("lv_date_1","{0:MM/dd/yyyy}") %>' runat="server"></asp:TextBox></b>
                <a href="#" tabindex="1" onblur="ctl00$ContentPlaceHolder1$FormView1$lv_date_1.focus()" onClick="showCalendarControl(ctl00$ContentPlaceHolder1$FormView1$lv_date_1); return false"><asp:Image ID="Image25" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
            </td>
            </tr>
            <tr>
            <td Width="70px"><b><asp:TextBox ID="lv_qty_2" MaxLength="8" Width="50px" onfocus="getprevval(this)" onblur="checkNumVal(this, 'q');displayqty(this, '1')" Text='<%# Bind("lv_qty_2") %>' runat="server"></asp:TextBox></b></td>
            <td Width="30px" ><b><asp:TextBox ID="lv_delivery_2" MaxLength="3" Width="30px" onfocus="getprevval(this)" onblur="checkNumVal(this, 'd')" Text='<%# Bind("lv_delivery_2") %>' runat="server"></asp:TextBox></b></td>
            <td Width="70px"><b><asp:TextBox ID="lv_price_2" MaxLength="5" Width="40px" onfocus="getprevval(this)" onblur="checkNumVal(this, 'p')" Text='<%# Bind("lv_price_2") %>' runat="server"></asp:TextBox></b></td>
            <td Width="70px"><b><asp:TextBox ID="lv_uom_2" MaxLength="3" Width="35px" onfocus="getprevval(this)" onblur="checkUom(this)" Text='<%# Bind("lv_uom_2") %>' runat="server"></asp:TextBox></b>
                <a href="#" tabindex="1" onClick="uomlook('2'); return false"><asp:Image ID="Image33" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
            </td>
            <td Width="100px" nowrap><b><asp:TextBox ID="lv_date_2" onfocus="javascript:preEnter( this, 'yes' );" onblur="javascript:preLeave( this, 'date', '99/99/9999' );" MaxLength="10" Width="70px" Text='<%# Bind("lv_date_2","{0:MM/dd/yyyy}") %>' runat="server"></asp:TextBox></b>
                <a href="#" tabindex="1" onblur="ctl00$ContentPlaceHolder1$FormView1$lv_date_2.focus()" onClick="showCalendarControl(ctl00$ContentPlaceHolder1$FormView1$lv_date_2); return false"><asp:Image ID="Image24" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
            </td>
            </tr>
            <tr>
            <td Width="70px"><b><asp:TextBox ID="lv_qty_3"  MaxLength="8" Width="50px" onfocus="getprevval(this)" onblur="checkNumVal(this, 'q');displayqty(this, '1')" Text='<%# Bind("lv_qty_3") %>' runat="server"></asp:TextBox></b></td>
            <td Width="30px"><b><asp:TextBox ID="lv_delivery_3" MaxLength="3" Width="30px" onfocus="getprevval(this)" onblur="checkNumVal(this, 'd')" Text='<%# Bind("lv_delivery_3") %>' runat="server"></asp:TextBox></b></td>
            <td Width="70px"><b><asp:TextBox ID="lv_price_3" MaxLength="5" Width="40px" onfocus="getprevval(this)" onblur="checkNumVal(this, 'p')" Text='<%# Bind("lv_price_3") %>' runat="server"></asp:TextBox></b></td>
            <td Width="70px"><b><asp:TextBox ID="lv_uom_3" MaxLength="3" Width="35px" onfocus="getprevval(this)" onblur="checkUom(this)" Text='<%# Bind("lv_uom_3") %>' runat="server"></asp:TextBox></b>
                <a href="#" tabindex="1" onClick="uomlook('3'); return false"><asp:Image ID="Image32" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
            </td>
            <td Width="100px" nowrap><b><asp:TextBox ID="lv_date_3" onfocus="javascript:preEnter( this, 'yes' );" onblur="javascript:preLeave( this, 'date', '99/99/9999' );" MaxLength="10" Width="70px" Text='<%# Bind("lv_date_3","{0:MM/dd/yyyy}") %>' runat="server"></asp:TextBox></b>
                <a href="#" tabindex="1" onblur="ctl00$ContentPlaceHolder1$FormView1$lv_date_3.focus()" onClick="showCalendarControl(ctl00$ContentPlaceHolder1$FormView1$lv_date_3); return false"><asp:Image ID="Image23" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
            </td>
            </tr>
            <tr>
            <td Width="70px"><b><asp:TextBox ID="lv_qty_4" MaxLength="8" Width="50px" onfocus="getprevval(this)" onblur="checkNumVal(this, 'q');displayqty(this, '1')" Text='<%# Bind("lv_qty_4") %>' runat="server"></asp:TextBox></b></td>
            <td Width="30px"><b><asp:TextBox ID="lv_delivery_4" MaxLength="3" Width="30px" onfocus="getprevval(this)" onblur="checkNumVal(this, 'd')" Text='<%# Bind("lv_delivery_4") %>' runat="server"></asp:TextBox></b></td>
            <td Width="70px"><b><asp:TextBox ID="lv_price_4" MaxLength="5" Width="40px" onfocus="getprevval(this)" onblur="checkNumVal(this, 'p')" Text='<%# Bind("lv_price_4") %>' runat="server"></asp:TextBox></b></td>
            <td Width="70px"><b><asp:TextBox ID="lv_uom_4" MaxLength="3" Width="35px" onfocus="getprevval(this)" onblur="checkUom(this)" Text='<%# Bind("lv_uom_4") %>' runat="server"></asp:TextBox></b>
                <a href="#" tabindex="1" onClick="uomlook('4'); return false"><asp:Image ID="Image31" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
            </td>
            <td Width="100px" nowrap><b><asp:TextBox ID="lv_date_4" onfocus="javascript:preEnter( this, 'yes' );" onblur="javascript:preLeave( this, 'date', '99/99/9999' );" MaxLength="10" Width="70px" Text='<%# Bind("lv_date_4","{0:MM/dd/yyyy}") %>' runat="server"></asp:TextBox></b>
                <a href="#" tabindex="1" onblur="ctl00$ContentPlaceHolder1$FormView1$lv_date_4.focus()" onClick="showCalendarControl(ctl00$ContentPlaceHolder1$FormView1$lv_date_4); return false"><asp:Image ID="Image22" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
            </td>
            </tr>
            <tr>
            <td Width="70px"><b><asp:TextBox ID="lv_qty_5" MaxLength="8" Width="50px" onfocus="getprevval(this)" onblur="checkNumVal(this, 'q');displayqty(this, '1')" Text='<%# Bind("lv_qty_5") %>' runat="server"></asp:TextBox></b></td>
            <td Width="30px"><b><asp:TextBox ID="lv_delivery_5" Width="30px" MaxLength="3" onfocus="getprevval(this)" onblur="checkNumVal(this, 'd')" Text='<%# Bind("lv_delivery_5") %>' runat="server"></asp:TextBox></b></td>
            <td Width="70px"><b><asp:TextBox ID="lv_price_5" MaxLength="5" Width="40px" onfocus="getprevval(this)" onblur="checkNumVal(this, 'p')" Text='<%# Bind("lv_price_5") %>' runat="server"></asp:TextBox></b></td>
            <td Width="70px"><b><asp:TextBox ID="lv_uom_5" MaxLength="3" Width="35px" onfocus="getprevval(this)" onblur="checkUom(this)" Text='<%# Bind("lv_uom_5") %>' runat="server"></asp:TextBox></b>
                <a href="#" tabindex="1" onClick="uomlook('5'); return false"><asp:Image ID="Image30" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
            </td>
            <td Width="100px" nowrap><b><asp:TextBox ID="lv_date_5" MaxLength="10" Width="70px" Text='<%# Bind("lv_date_5","{0:MM/dd/yyyy}") %>' runat="server"></asp:TextBox></b>
                <a href="#" tabindex="1" onblur="ctl00$ContentPlaceHolder1$FormView1$lv_date_5.focus()" onClick="showCalendarControl(ctl00$ContentPlaceHolder1$FormView1$lv_date_5); return false"><asp:Image ID="Image21" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
            </td>
            </tr>
            <tr>
            <td Width="70px"><b><asp:TextBox ID="lv_qty_6" MaxLength="8" Width="50px" onfocus="getprevval(this)" onblur="checkNumVal(this, 'q');displayqty(this, '1')" Text='<%# Bind("lv_qty_6") %>' runat="server"></asp:TextBox></b></td>
            <td Width="30px"><b><asp:TextBox ID="lv_delivery_6" MaxLength="3" Width="30px" onfocus="getprevval(this)" onblur="checkNumVal(this, 'd')" Text='<%# Bind("lv_delivery_6") %>' runat="server"></asp:TextBox></b></td>
            <td Width="70px"><b><asp:TextBox ID="lv_price_6" MaxLength="5" Width="40px" onfocus="getprevval(this)" onblur="checkNumVal(this, 'p')" Text='<%# Bind("lv_price_6") %>' runat="server"></asp:TextBox></b></td>
            <td Width="70px"><b><asp:TextBox ID="lv_uom_6" MaxLength="3" Width="35px" onfocus="getprevval(this)" onblur="checkUom(this)" Text='<%# Bind("lv_uom_6") %>' runat="server"></asp:TextBox></b>
                <a href="#" tabindex="1" onClick="uomlook('6'); return false"><asp:Image ID="Image29" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
            </td>
            <td Width="100px" nowrap><b><asp:TextBox ID="lv_date_6" onfocus="javascript:preEnter( this, 'yes' );" onblur="javascript:preLeave( this, 'date', '99/99/9999' );" MaxLength="10" Width="70px" Text='<%# Bind("lv_date_6","{0:MM/dd/yyyy}") %>' runat="server"></asp:TextBox></b>
                <a href="#" tabindex="1" onblur="ctl00$ContentPlaceHolder1$FormView1$lv_date_6.focus()" onClick="showCalendarControl(ctl00$ContentPlaceHolder1$FormView1$lv_date_6); return false"><asp:Image ID="Image20" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
            </td>
            </tr>
            <tr>
            <td Width="70px"><b><asp:TextBox ID="lv_qty_7" MaxLength="8" Width="50px" onfocus="getprevval(this)" onblur="checkNumVal(this, 'q');displayqty(this, '1')" Text='<%# Bind("lv_qty_7") %>' runat="server"></asp:TextBox></b></td>
            <td Width="30px"><b><asp:TextBox ID="lv_delivery_7" MaxLength="3" Width="30px" onfocus="getprevval(this)" onblur="checkNumVal(this, 'd')" Text='<%# Bind("lv_delivery_7") %>' runat="server"></asp:TextBox></b></td>
            <td Width="70px"><b><asp:TextBox ID="lv_price_7" MaxLength="5" Width="40px" onfocus="getprevval(this)" onblur="checkNumVal(this, 'p')" Text='<%# Bind("lv_price_7") %>' runat="server"></asp:TextBox></b></td>
            <td Width="70px"><b><asp:TextBox ID="lv_uom_7" MaxLength="3" Width="35px" onfocus="getprevval(this)" onblur="checkUom(this)" Text='<%# Bind("lv_uom_7") %>' runat="server"></asp:TextBox></b>
                <a href="#" tabindex="1"  onClick="uomlook('7'); return false"><asp:Image ID="Image28" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
            </td>
            <td Width="100px" nowrap><b><asp:TextBox ID="lv_date_7" onfocus="javascript:preEnter( this, 'yes' );" onblur="javascript:preLeave( this, 'date', '99/99/9999' );" MaxLength="10" Width="70px" Text='<%# Bind("lv_date_7","{0:MM/dd/yyyy}") %>' runat="server"></asp:TextBox></b>
                <a href="#" tabindex="1" onblur="ctl00$ContentPlaceHolder1$FormView1$lv_date_7.focus()" onClick="showCalendarControl(ctl00$ContentPlaceHolder1$FormView1$lv_date_7); return false"><asp:Image ID="Image19" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
            </td>
            </tr>
            <tr>
            <td Width="70px"><b><asp:TextBox ID="lv_qty_8" MaxLength="8" Width="50px" onfocus="getprevval(this)" onblur="checkNumVal(this, 'q');displayqty(this, '1')" Text='<%# Bind("lv_qty_8") %>' runat="server"></asp:TextBox></b></td>
            <td Width="30px"><b><asp:TextBox ID="lv_delivery_8" MaxLength="3" Width="30px" onfocus="getprevval(this)" onblur="checkNumVal(this, 'd')" Text='<%# Bind("lv_delivery_8") %>' runat="server"></asp:TextBox></b></td>
            <td Width="70px"><b><asp:TextBox ID="lv_price_8" MaxLength="5" Width="40px" onfocus="getprevval(this)" onblur="checkNumVal(this, 'p')" Text='<%# Bind("lv_price_8") %>' runat="server"></asp:TextBox></b></td>
            <td Width="70px"><b><asp:TextBox ID="lv_uom_8" MaxLength="3" Width="35px" onfocus="getprevval(this)" onblur="checkUom(this)" Text='<%# Bind("lv_uom_8") %>' runat="server"></asp:TextBox></b>
                <a href="#" tabindex="1" onClick="uomlook('8'); return false"><asp:Image ID="Image27" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
            </td>
            <td Width="100px" nowrap><b><asp:TextBox ID="lv_date_8" onfocus="javascript:preEnter( this, 'yes' );" onblur="javascript:preLeave( this, 'date', '99/99/9999' );" MaxLength="10" Width="70px" Text='<%# Bind("lv_date_8","{0:MM/dd/yyyy}") %>' runat="server"></asp:TextBox></b>
                <a href="#" tabindex="1" onblur="ctl00$ContentPlaceHolder1$FormView1$lv_date_8.focus()" onClick="showCalendarControl(ctl00$ContentPlaceHolder1$FormView1$lv_date_8); return false"><asp:Image ID="Image18" runat="server" ImageUrl="images/lookup_icon.gif" /></a>                
            </td>
            </tr>
            <tr>
            <td Width="70px"><b><asp:TextBox ID="lv_qty_9" MaxLength="8" Width="50px" onfocus="getprevval(this)" onblur="checkNumVal(this, 'q');displayqty(this, '1')" Text='<%# Bind("lv_qty_9") %>' runat="server"></asp:TextBox></b></td>
            <td Width="30px"><b><asp:TextBox ID="lv_delivery_9" MaxLength="3" Width="30px" onfocus="getprevval(this)" onblur="checkNumVal(this, 'd')" Text='<%# Bind("lv_delivery_9") %>' runat="server"></asp:TextBox></b></td>
            <td Width="70px"><b><asp:TextBox ID="lv_price_9" MaxLength="5" Width="40px" onfocus="getprevval(this)" onblur="checkNumVal(this, 'p')" Text='<%# Bind("lv_price_9") %>' runat="server"></asp:TextBox></b></td>
            <td Width="70px"><b><asp:TextBox ID="lv_uom_9" MaxLength="3" Width="35px" onfocus="getprevval(this)" onblur="checkUom(this)" Text='<%# Bind("lv_uom_9") %>' runat="server"></asp:TextBox></b>
                <a href="#" tabindex="1" onClick="uomlook('9'); return false"><asp:Image ID="Image26" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
            </td>
            <td Width="100px" nowrap><b><asp:TextBox ID="lv_date_9" onfocus="javascript:preEnter( this, 'yes' );" onblur="javascript:preLeave( this, 'date', '99/99/9999' );" MaxLength="10" Width="70px" Text='<%# Bind("lv_date_9","{0:MM/dd/yyyy}") %>' runat="server"></asp:TextBox></b>
                <a href="#" tabindex="1" onblur="ctl00$ContentPlaceHolder1$FormView1$lv_date_9.focus()" onClick="showCalendarControl(ctl00$ContentPlaceHolder1$FormView1$lv_date_9); return false"><asp:Image ID="Image17" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
            </td>
            </tr>
            <tr>
            <td Width="70px"><b><asp:TextBox ID="lv_qty_10" MaxLength="8" Width="50px" onfocus="getprevval(this)" onblur="checkNumVal(this, 'q');displayqty(this, '1')" Text='<%# Bind("lv_qty_10") %>' runat="server"></asp:TextBox></b></td>
            <td Width="30px"><b><asp:TextBox ID="lv_delivery_10" MaxLength="3" Width="30px" onfocus="getprevval(this)" onblur="checkNumVal(this, 'd')" Text='<%# Bind("lv_delivery_10") %>' runat="server"></asp:TextBox></b></td>
            <td Width="70px"><b><asp:TextBox ID="lv_price_10" Width="40px" MaxLength="5" onfocus="getprevval(this)" onblur="checkNumVal(this, 'p')" Text='<%# Bind("lv_price_10") %>' runat="server"></asp:TextBox></b></td>
            <td Width="70px"><b><asp:TextBox ID="lv_uom_10" MaxLength="3" Width="35px" onfocus="getprevval(this)" onblur="checkUom(this)" Text='<%# Bind("lv_uom_10") %>' runat="server"></asp:TextBox></b>
                <a href="#" tabindex="1" onClick="uomlook('10'); return false"><asp:Image ID="Image15" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
            </td>
            <td Width="100px" nowrap><b><asp:TextBox ID="lv_date_10" onfocus="javascript:preEnter( this, 'yes' );" onblur="javascript:preLeave( this, 'date', '99/99/9999' );" MaxLength="10" Width="70px" Text='<%# Bind("lv_date_10","{0:MM/dd/yyyy}") %>' runat="server"></asp:TextBox></b>
                <a href="#" tabindex="1" onblur="ctl00$ContentPlaceHolder1$FormView1$lv_date_10.focus()" onClick="showCalendarControl(ctl00$ContentPlaceHolder1$FormView1$lv_date_10); return false"><asp:Image ID="Image16" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
            </td>
            </tr>
            
            <%--</table>
            
            <br />--%>
            <tr><td colspan="2">
            <input type="hidden"  name="HiddenRowid" value='<%# Server.UrlEncode(Convert.ToString(DataBinder.Eval(Container,"DataItem.RfqRowid"))) %>' />
            <input type="hidden"  name="ARowid" value='<%# Server.UrlEncode(Convert.ToString(DataBinder.Eval(Container,"DataItem.ARowid"))) %>' />
            <br />           
            
            </td></tr></table>
            </td></tr>
            <tr><td>
            <asp:Button ID="Button1" runat="server"   CssClass="buttonM" onclick="updateRfqitem"
                Text="Save" >
            </asp:Button>
            <asp:Button ID="Button2" runat="server" CausesValidation="False" CommandName="Cancel" CssClass="buttonM"
                Text="Cancel">
            </asp:Button>
            </td></tr>
            </table>
            </fieldset>         
           </asp:Panel>
        </EditItemTemplate>
        
        
        <InsertItemTemplate>
        <asp:Panel ID="Panel1" runat="server" DefaultButton="Button1">
         <asp:HiddenField ID="HiddenField1" runat="server" />
        <fieldset  style="background-color:#EFF3FB">        
        <table><tr><td>
        <table class="shade" style="width: 700px">
            <tr>       
            <td visible="false" align="right" padding-right:5px;"><b></b></td>
        <td visible="false"><b><asp:Label ID="Label1" runat="server" Text='<%# Bind("RfqSeqNo") %>'>
            </asp:Label></b></td>  
            
                <td nowrap align="left" style="padding-right:5px;"><b>Qty:</b><br /><b><asp:TextBox ID="RfqQtyTextBox" Width="50px" MaxLength="8" onkeypress="return validate(event)" onblur="showQty()" runat="server" Text='<%# Bind("RfqQty","{0:#########}") %>'>
                    </asp:TextBox>
                   <%-- <asp:CompareValidator ID="CompareValidator8" runat="server" ErrorMessage="Invalid Entry" ControlToValidate="RfqQtyTextBox"  Display="dynamic" SetFocusOnError="true" Operator="DataTypeCheck" Type="Integer"></asp:CompareValidator>--%>
                    <%--<asp:RequiredFieldValidator ID="RequiredFieldValidator3" runat="server" ErrorMessage="Cannot be empty" ControlToValidate="RfqQtyTextBox" Display="dynamic" SetFocusOnError="true"></asp:RequiredFieldValidator>--%>
                </b></td>
                        
       
        <td nowrap align="left"  style="padding-right:5px;"><b>FG Item#:</b><br /><b><asp:TextBox ID="RfqStockTextBox" Width="90px" MaxLength="15" runat="server" Text='<%# Bind("RfqStock") %>'>        
            </asp:TextBox><a href="#" tabindex="1" onClick="QFgItemlook(); return false" ><asp:Image ID="Image1" runat="server" ImageUrl="images/lookup_icon.gif" /></a><br /></b></td>
            <td nowrap align="left" style="padding-right:5px;"><b>Item Name:</b><br /><b><asp:TextBox ID="RfqNameTextBox" MaxLength="30" Width="160px" runat="server" Text='<%# Bind("RfqName") %>'>
            </asp:TextBox>  <asp:RequiredFieldValidator ID="RequiredFieldValidator3" runat="server" ErrorMessage="Cannot be empty" ControlToValidate="RfqNameTextBox" Display="dynamic" SetFocusOnError="true"></asp:RequiredFieldValidator>         
            </b></td>
         <td nowrap align="left"  style="padding-right:5px;"><b>Customer Part#:</b><br /><b><asp:TextBox MaxLength="15" Width="90px"  ID="RfqPartnoTextBox" runat="server" Text='<%# Bind("RfqPartno") %>'>
            </asp:TextBox><a href="#" tabindex="1" onClick="QPartlook(); return false" ><asp:Image ID="Image2" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
            <asp:RequiredFieldValidator ID="RequiredFieldValidator5" runat="server" ErrorMessage="Cannot be empty" ControlToValidate="RfqPartnoTextBox" Display="dynamic" SetFocusOnError="true"></asp:RequiredFieldValidator>
            </b></td>
       <td nowrap align="left" style="padding-right:5px;"><b>Category:</b><br /><b><asp:TextBox Width="50px" MaxLength="6" ID="RfqProcatTextBox" AutoPostBack="true" OnTextChanged="CategoryTextChanged" runat="server" Text='<%# Bind("RfqProcat") %>'>
            </asp:TextBox>
            <a href="#" tabindex="1" onClick="categorylookup(); return false" ><asp:Image ID="Image5" runat="server" ImageUrl="images/lookup_icon.gif" />
            <asp:RequiredFieldValidator ID="RequiredFieldValidator7" runat="server" ErrorMessage="Cannot be empty" ControlToValidate="RfqProcatTextBox" Display="dynamic" SetFocusOnError="true"></asp:RequiredFieldValidator>
            </b></td>
          <td nowrap align="left" style="padding-right:5px;"><b>Color:</b><br /><b><asp:TextBox ID="RfqColTextBox" Width="30px" MaxLength="2" onkeypress="return validate(event)" runat="server" Text='<%# Bind("RfqCol") %>'></asp:TextBox></b>
            <asp:CompareValidator ID="CompareValidator7" runat="server" ControlToValidate="RfqColTextBox" SetFocusOnError="true" Display="dynamic" Operator="dataTypeCheck" Type="Integer" ErrorMessage="Invalid Color"></asp:CompareValidator>    
        </td>         
      
       <td nowrap align="right"  style="padding-right:5px;"><b>Coating:</b><br /><b><asp:TextBox MaxLength="2" Width="20px" onkeypress="return validate(event)" ID="RfqCoatTextBox" runat="server" Text='<%# Bind("RfqCoat") %>'></asp:TextBox></b>
            <asp:CompareValidator ID="CompareValidator6" runat="server" ControlToValidate="RfqCoatTextBox" SetFocusOnError="true" Display="dynamic" Operator="dataTypeCheck" Type="Integer" ErrorMessage="Invalid Coating"></asp:CompareValidator>  
        </td>
        
       <td nowrap align="left" style="padding-right:5px;"><b>Style:</b><br /><b><asp:TextBox ID="RfqstyleTextBox" Width="40px" MaxLength="6" AutoPostBack="true" OnTextChanged="StyleTextChanged" runat="server" Text='<%# Bind("Rfqstyle") %>'>
            </asp:TextBox>
            <a href="#" tabindex="1" onClick="stylelook(); return false" ><asp:Image ID="Image4" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
            <input id="Button3" type="button" onclick="openstyle()" value="Style Image" />
            <asp:RequiredFieldValidator ID="RequiredFieldValidator6" runat="server" ErrorMessage="Cannot be empty" ControlToValidate="RfqstyleTextBox" Display="dynamic" SetFocusOnError="true"></asp:RequiredFieldValidator>
            </b></td>
            
            <td nowrap align="left" style="padding-right:5px;"><b>Length:</b><br /><b><asp:TextBox ID="RfqLengthTextBox" Width="50px" onBlur="checkNumeric(this,',','.');" onkeyup="javascript:decimalval();" MaxLength="8" runat="server" Text='<%# Bind("RfqLength","{0:###,##0.00}") %>'>
            </asp:TextBox>
            <asp:RequiredFieldValidator ID="RequiredFieldValidator1" runat="server" ErrorMessage="Cannot be empty" ControlToValidate="RfqLengthTextBox" Display="dynamic" SetFocusOnError="true"></asp:RequiredFieldValidator>
            <asp:RangeValidator ID="RangeValidator2" runat="server" ControlToValidate="RfqLengthTextBox" SetFocusOnError="true" Display="dynamic" MinimumValue="00000.01" MaximumValue="99999.99" ErrorMessage="Must have some value"></asp:RangeValidator>
            <asp:CompareValidator ID="CompareValidator5" runat="server" ErrorMessage="Only single decimal" ControlToValidate="RfqLengthTextBox" Display="dynamic" SetFocusOnError="true" Operator="DataTypeCheck" Type="double"></asp:CompareValidator>
            </b></td>
            <td nowrap align="left"  padding-right:5px;"><b>Width:</b><br /><b><asp:TextBox ID="RfqWidthTextBox" Width="50px" onBlur="checkNumeric(this,',','.');" onkeyup="javascript:decimalval();" MaxLength="8" runat="server" Text='<%# Bind("RfqWidth","{0:###,##0.00}") %>'>
            </asp:TextBox>
            <asp:RequiredFieldValidator ID="RequiredFieldValidator2" runat="server" ErrorMessage="Cannot be empty" ControlToValidate="RfqWidthTextBox" Display="dynamic" SetFocusOnError="true"></asp:RequiredFieldValidator>
            <asp:RangeValidator ID="RangeValidator1" runat="server" ControlToValidate="RfqWidthTextBox" SetFocusOnError="true" Display="dynamic" MinimumValue="00000.01" MaximumValue="99999.99" ErrorMessage="Must have some value"></asp:RangeValidator>
            <asp:CompareValidator ID="CompareValidator4" runat="server" ErrorMessage="Only single decimal" ControlToValidate="RfqWidthTextBox" Display="dynamic" SetFocusOnError="true" Operator="DataTypeCheck" Type="double"></asp:CompareValidator>
            </b></td>
            
        <td nowrap align="left" style="padding-right:5px;"><b>Depth:</b><br /><b><asp:TextBox ID="RfqDepthTextBox" Width="50px" MaxLength="8" onkeyup="javascript:decimalval();" onBlur="checkNumeric(this,',','.');" runat="server" Text='<%# Bind("RfqDepth","{0:###,##0.00}") %>'>
            </asp:TextBox>
            <asp:CompareValidator ID="CompareValidator3" runat="server" ErrorMessage="Only single decimal" ControlToValidate="RfqDepthTextBox" Display="dynamic" SetFocusOnError="true" Operator="DataTypeCheck" Type="double"></asp:CompareValidator>
            </b></td>  
         
        <td nowrap align="left"  style="padding-right:5px;"><b>Board:</b><br /><b><asp:TextBox ID="RfqBoardTextBox" Width="65px" MaxLength="10" AutoPostBack="true" OnTextChanged="BoardTextChanged" runat="server" Text='<%# Bind("RfqBoard") %>'>
            </asp:TextBox><a href="#" tabindex="1" onClick="Boardlook1(); return false"><asp:Image ID="Image3" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
            <asp:RequiredFieldValidator ID="RequiredFieldValidator8" runat="server" ErrorMessage="Cannot be empty" ControlToValidate="RfqBoardTextBox" Display="dynamic" SetFocusOnError="true"></asp:RequiredFieldValidator>
            </b></td>
        <td nowrap align="left" style="padding-right:5px;"><b>Caliper:</b><br /><b><asp:TextBox ID="RfqCalTextBox" Enabled="false" Width="50px" MaxLength="1" runat="server" Text='<%# Bind("RfqCal","{0:###,##0.00000}") %>'>
            </asp:TextBox></b></td>                                 
       
         <td nowrap align="left" style="padding-right:5px"><b>Qty/Set:</b><br /><b><asp:Label ID="qtysetLabel" runat="server" BackColor="PaleTurquoise" Width="60px" BorderColor="White" BorderStyle="Solid" BorderWidth="1px" Text='<%# Bind("qtyset") %>'></asp:Label></b></td>
              
        </tr>
       
        </table>
            
            <table id="Qtyhelp" align="left" width="400px" style="display:none;">
            <tr>
            <td><b>Quantity</b></td>
            <td><b>Deliveries</b></td>
            <td><b>Price</b></td>
            <td><b>UOM</b></td>
            <td><b>Date</b></td>
            </tr>
            <tr>
            <td Width="70px"><b><asp:TextBox ID="TextBox1" onfocus="getprevval(this)" onblur="reverseqty(); checkNumVal(this, 'q')" MaxLength="8" Width="50px" runat="server"></asp:TextBox></b></td>
            <td Width="30px"><b><asp:TextBox ID="lv_delivery_1" MaxLength="3" Width="30px" onfocus="getprevval(this)" onblur="checkNumVal(this, 'd')" Text='<%# Bind("lv_delivery_1") %>' runat="server"></asp:TextBox></b></td>
            <td Width="70px"><b><asp:TextBox ID="lv_price_1" MaxLength="5" Width="40px" onfocus="getprevval(this)" onblur="checkNumVal(this, 'p')" Text='<%# Bind("lv_price_1") %>' runat="server"></asp:TextBox></b></td>
            <td Width="100px" nowrap><b><asp:TextBox ID="lv_uom_1" MaxLength="3" Width="35px" onfocus="getprevval(this)" onblur="checkUom(this)" Text='<%# Bind("lv_uom_1") %>' runat="server"></asp:TextBox></b>
                <a href="#" tabindex="1"  onClick="uomlook('1'); return false"><asp:Image ID="Image15" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
            </td>
            <td Width="100px" nowrap><b><asp:TextBox ID="lv_date_1" onfocus="javascript:preEnter( this, 'yes' );" onblur="javascript:preLeave( this, 'date', '99/99/9999' );" MaxLength="10" Width="70px" Text='<%# Bind("lv_date_1","{0:MM/dd/yyyy}") %>' runat="server"></asp:TextBox></b>
                <a href="#" tabindex="1" onblur="ctl00$ContentPlaceHolder1$FormView1$lv_date_1.focus()" onClick="showCalendarControl(ctl00$ContentPlaceHolder1$FormView1$lv_date_1); return false"><asp:Image ID="Image25" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
            </td>
            </tr>
            <tr>
            <td Width="70px"><b><asp:TextBox ID="lv_qty_2" MaxLength="8" Width="50px" onfocus="getprevval(this)" onblur="defaultVal('2');checkNumVal(this, 'q');displayqty(this, '2')" Text='<%# Bind("lv_qty_2") %>' runat="server"></asp:TextBox></b></td>
            <td Width="30px" ><b><asp:TextBox ID="lv_delivery_2" MaxLength="3" Width="30px" onfocus="getprevval(this)" onblur="checkNumVal(this, 'd')" Text='<%# Bind("lv_delivery_2") %>' runat="server"></asp:TextBox></b></td>
            <td Width="70px"><b><asp:TextBox ID="lv_price_2" MaxLength="5" Width="40px" onfocus="getprevval(this)" onblur="checkNumVal(this, 'p')" Text='<%# Bind("lv_price_2") %>' runat="server"></asp:TextBox></b></td>
            <td Width="70px"><b><asp:TextBox ID="lv_uom_2" MaxLength="3" Width="35px" onfocus="getprevval(this)" onblur="checkUom(this)" Text='<%# Bind("lv_uom_2") %>' runat="server"></asp:TextBox></b>
                <a href="#" tabindex="1" onClick="uomlook('2'); return false"><asp:Image ID="Image14" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
            </td>
            <td Width="100px" nowrap><b><asp:TextBox ID="lv_date_2" onfocus="javascript:preEnter( this, 'yes' );" onblur="javascript:preLeave( this, 'date', '99/99/9999' );" MaxLength="10" Width="70px" Text='<%# Bind("lv_date_2","{0:MM/dd/yyyy}") %>' runat="server"></asp:TextBox></b>
                <a href="#" tabindex="1" onblur="ctl00$ContentPlaceHolder1$FormView1$lv_date_2.focus()" onClick="showCalendarControl(ctl00$ContentPlaceHolder1$FormView1$lv_date_2); return false"><asp:Image ID="Image24" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
            </td>
            </tr>
            <tr>
            <td Width="70px"><b><asp:TextBox ID="lv_qty_3"  MaxLength="8" Width="50px" onfocus="getprevval(this)" onblur="defaultVal('3');checkNumVal(this, 'q');displayqty(this, '2')" Text='<%# Bind("lv_qty_3") %>' runat="server"></asp:TextBox></b></td>
            <td Width="30px"><b><asp:TextBox ID="lv_delivery_3" MaxLength="3" Width="30px" onfocus="getprevval(this)" onblur="checkNumVal(this, 'd')" Text='<%# Bind("lv_delivery_3") %>' runat="server"></asp:TextBox></b></td>
            <td Width="70px"><b><asp:TextBox ID="lv_price_3" MaxLength="5" Width="40px" onfocus="getprevval(this)" onblur="checkNumVal(this, 'p')" Text='<%# Bind("lv_price_3") %>' runat="server"></asp:TextBox></b></td>
            <td Width="70px"><b><asp:TextBox ID="lv_uom_3" MaxLength="3" Width="35px" onfocus="getprevval(this)" onblur="checkUom(this)" Text='<%# Bind("lv_uom_3") %>' runat="server"></asp:TextBox></b>
                <a href="#" tabindex="1" onClick="uomlook('3'); return false"><asp:Image ID="Image13" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
            </td>
            <td Width="100px" nowrap><b><asp:TextBox ID="lv_date_3" onfocus="javascript:preEnter( this, 'yes' );" onblur="javascript:preLeave( this, 'date', '99/99/9999' );" MaxLength="10" Width="70px" Text='<%# Bind("lv_date_3","{0:MM/dd/yyyy}") %>' runat="server"></asp:TextBox></b>
                <a href="#" tabindex="1" onblur="ctl00$ContentPlaceHolder1$FormView1$lv_date_3.focus()" onClick="showCalendarControl(ctl00$ContentPlaceHolder1$FormView1$lv_date_3); return false"><asp:Image ID="Image23" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
            </td>
            </tr>
            <tr>
            <td Width="70px"><b><asp:TextBox ID="lv_qty_4" MaxLength="8" Width="50px" onfocus="getprevval(this)" onblur="defaultVal('4');checkNumVal(this, 'q');displayqty(this, '2')" Text='<%# Bind("lv_qty_4") %>' runat="server"></asp:TextBox></b></td>
            <td Width="30px"><b><asp:TextBox ID="lv_delivery_4" MaxLength="3" Width="30px" onfocus="getprevval(this)" onblur="checkNumVal(this, 'd')" Text='<%# Bind("lv_delivery_4") %>' runat="server"></asp:TextBox></b></td>
            <td Width="70px"><b><asp:TextBox ID="lv_price_4" MaxLength="5" Width="40px" onfocus="getprevval(this)" onblur="checkNumVal(this, 'p')" Text='<%# Bind("lv_price_4") %>' runat="server"></asp:TextBox></b></td>
            <td Width="70px"><b><asp:TextBox ID="lv_uom_4" MaxLength="3" Width="35px" onfocus="getprevval(this)" onblur="checkUom(this)" Text='<%# Bind("lv_uom_4") %>' runat="server"></asp:TextBox></b>
                <a href="#" tabindex="1" onClick="uomlook('4'); return false"><asp:Image ID="Image12" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
            </td>
            <td Width="100px" nowrap><b><asp:TextBox ID="lv_date_4" onfocus="javascript:preEnter( this, 'yes' );" onblur="javascript:preLeave( this, 'date', '99/99/9999' );" MaxLength="10" Width="70px" Text='<%# Bind("lv_date_4","{0:MM/dd/yyyy}") %>' runat="server"></asp:TextBox></b>
                <a href="#" tabindex="1" onblur="ctl00$ContentPlaceHolder1$FormView1$lv_date_4.focus()" onClick="showCalendarControl(ctl00$ContentPlaceHolder1$FormView1$lv_date_4); return false"><asp:Image ID="Image22" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
            </td>
            </tr>
            <tr>
            <td Width="70px"><b><asp:TextBox ID="lv_qty_5" MaxLength="8" Width="50px" onfocus="getprevval(this)" onblur="defaultVal('5');checkNumVal(this, 'q');displayqty(this, '2')" Text='<%# Bind("lv_qty_5") %>' runat="server"></asp:TextBox></b></td>
            <td Width="30px"><b><asp:TextBox ID="lv_delivery_5" Width="30px" MaxLength="3" onfocus="getprevval(this)" onblur="checkNumVal(this, 'd')" Text='<%# Bind("lv_delivery_5") %>' runat="server"></asp:TextBox></b></td>
            <td Width="70px"><b><asp:TextBox ID="lv_price_5" MaxLength="5" Width="40px" onfocus="getprevval(this)" onblur="checkNumVal(this, 'p')" Text='<%# Bind("lv_price_5") %>' runat="server"></asp:TextBox></b></td>
            <td Width="70px"><b><asp:TextBox ID="lv_uom_5" MaxLength="3" Width="35px" onfocus="getprevval(this)" onblur="checkUom(this)" Text='<%# Bind("lv_uom_5") %>' runat="server"></asp:TextBox></b>
                <a href="#" tabindex="1" onClick="uomlook('5'); return false"><asp:Image ID="Image11" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
            </td>
            <td Width="100px" nowrap><b><asp:TextBox ID="lv_date_5" onfocus="javascript:preEnter( this, 'yes' );" onblur="javascript:preLeave( this, 'date', '99/99/9999' );" MaxLength="10" Width="70px" Text='<%# Bind("lv_date_5","{0:MM/dd/yyyy}") %>' runat="server"></asp:TextBox></b>
                <a href="#" tabindex="1" onblur="ctl00$ContentPlaceHolder1$FormView1$lv_date_5.focus()" onClick="showCalendarControl(ctl00$ContentPlaceHolder1$FormView1$lv_date_5); return false"><asp:Image ID="Image21" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
            </td>
            </tr>
            <tr>
            <td Width="70px"><b><asp:TextBox ID="lv_qty_6" MaxLength="8" Width="50px" onfocus="getprevval(this)" onblur="defaultVal('6');checkNumVal(this, 'q');displayqty(this, '2')" Text='<%# Bind("lv_qty_6") %>' runat="server"></asp:TextBox></b></td>
            <td Width="30px"><b><asp:TextBox ID="lv_delivery_6" MaxLength="3" Width="30px" onfocus="getprevval(this)" onblur="checkNumVal(this, 'd')" Text='<%# Bind("lv_delivery_6") %>' runat="server"></asp:TextBox></b></td>
            <td Width="70px"><b><asp:TextBox ID="lv_price_6" MaxLength="5" Width="40px" onfocus="getprevval(this)" onblur="checkNumVal(this, 'p')" Text='<%# Bind("lv_price_6") %>' runat="server"></asp:TextBox></b></td>
            <td Width="70px"><b><asp:TextBox ID="lv_uom_6" MaxLength="3" Width="35px" onfocus="getprevval(this)" onblur="checkUom(this)" Text='<%# Bind("lv_uom_6") %>' runat="server"></asp:TextBox></b>
                <a href="#" tabindex="1" onClick="uomlook('6'); return false"><asp:Image ID="Image10" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
            </td>
            <td Width="100px" nowrap><b><asp:TextBox ID="lv_date_6" onfocus="javascript:preEnter( this, 'yes' );" onblur="javascript:preLeave( this, 'date', '99/99/9999' );" MaxLength="10" Width="70px" Text='<%# Bind("lv_date_6","{0:MM/dd/yyyy}") %>' runat="server"></asp:TextBox></b>
                <a href="#" tabindex="1" onblur="ctl00$ContentPlaceHolder1$FormView1$lv_date_6.focus()" onClick="showCalendarControl(ctl00$ContentPlaceHolder1$FormView1$lv_date_6); return false"><asp:Image ID="Image20" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
            </td>
            </tr>
            <tr>
            <td Width="70px"><b><asp:TextBox ID="lv_qty_7" MaxLength="8" Width="50px" onfocus="getprevval(this)" onblur="defaultVal('7');checkNumVal(this, 'q');displayqty(this, '2')" Text='<%# Bind("lv_qty_7") %>' runat="server"></asp:TextBox></b></td>
            <td Width="30px"><b><asp:TextBox ID="lv_delivery_7" MaxLength="3" Width="30px" onfocus="getprevval(this)" onblur="checkNumVal(this, 'd')" Text='<%# Bind("lv_delivery_7") %>' runat="server"></asp:TextBox></b></td>
            <td Width="70px"><b><asp:TextBox ID="lv_price_7" MaxLength="5" Width="40px" onfocus="getprevval(this)" onblur="checkNumVal(this, 'p')" Text='<%# Bind("lv_price_7") %>' runat="server"></asp:TextBox></b></td>
            <td Width="70px"><b><asp:TextBox ID="lv_uom_7" MaxLength="3" Width="35px" onfocus="getprevval(this)" onblur="checkUom(this)" Text='<%# Bind("lv_uom_7") %>' runat="server"></asp:TextBox></b>
                <a href="#" tabindex="1" onClick="uomlook('7'); return false"><asp:Image ID="Image9" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
            </td>
            <td Width="100px" nowrap><b><asp:TextBox ID="lv_date_7" onfocus="javascript:preEnter( this, 'yes' );" onblur="javascript:preLeave( this, 'date', '99/99/9999' );" MaxLength="10" Width="70px" Text='<%# Bind("lv_date_7","{0:MM/dd/yyyy}") %>' runat="server"></asp:TextBox></b>
                <a href="#" tabindex="1" onblur="ctl00$ContentPlaceHolder1$FormView1$lv_date_7.focus()" onClick="showCalendarControl(ctl00$ContentPlaceHolder1$FormView1$lv_date_7); return false"><asp:Image ID="Image19" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
            </td>
            </tr>
            <tr>
            <td Width="70px"><b><asp:TextBox ID="lv_qty_8" MaxLength="8" Width="50px" onfocus="getprevval(this)" onblur="defaultVal('8');checkNumVal(this, 'q');displayqty(this, '2')"  Text='<%# Bind("lv_qty_8") %>' runat="server"></asp:TextBox></b></td>
            <td Width="30px"><b><asp:TextBox ID="lv_delivery_8" MaxLength="3" Width="30px" onfocus="getprevval(this)" onblur="checkNumVal(this, 'd')" Text='<%# Bind("lv_delivery_8") %>' runat="server"></asp:TextBox></b></td>
            <td Width="70px"><b><asp:TextBox ID="lv_price_8" MaxLength="5" Width="40px" onfocus="getprevval(this)" onblur="checkNumVal(this, 'p')" Text='<%# Bind("lv_price_8") %>' runat="server"></asp:TextBox></b></td>
            <td Width="70px"><b><asp:TextBox ID="lv_uom_8" MaxLength="3" Width="35px" onfocus="getprevval(this)" onblur="checkUom(this)" Text='<%# Bind("lv_uom_8") %>' runat="server"></asp:TextBox></b>
                <a href="#" tabindex="1"  onClick="uomlook('8'); return false"><asp:Image ID="Image8" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
            </td>
            <td Width="100px" nowrap><b><asp:TextBox ID="lv_date_8" onfocus="javascript:preEnter( this, 'yes' );" onblur="javascript:preLeave( this, 'date', '99/99/9999' );" MaxLength="10" Width="70px" Text='<%# Bind("lv_date_8","{0:MM/dd/yyyy}") %>' runat="server"></asp:TextBox></b>
                <a href="#" tabindex="1" onblur="ctl00$ContentPlaceHolder1$FormView1$lv_date_8.focus()" onClick="showCalendarControl(ctl00$ContentPlaceHolder1$FormView1$lv_date_8); return false"><asp:Image ID="Image18" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
            </td>
            </tr>
            <tr>
            <td Width="70px"><b><asp:TextBox ID="lv_qty_9" MaxLength="8" Width="50px" onfocus="getprevval(this)" onblur="defaultVal('9');checkNumVal(this, 'q');displayqty(this, '2')" Text='<%# Bind("lv_qty_9") %>' runat="server"></asp:TextBox></b></td>
            <td Width="30px"><b><asp:TextBox ID="lv_delivery_9" MaxLength="3" Width="30px" onfocus="getprevval(this)" onblur="checkNumVal(this, 'd')" Text='<%# Bind("lv_delivery_9") %>' runat="server"></asp:TextBox></b></td>
            <td Width="70px"><b><asp:TextBox ID="lv_price_9" MaxLength="5" Width="40px" onfocus="getprevval(this)" onblur="checkNumVal(this, 'p')" Text='<%# Bind("lv_price_9") %>' runat="server"></asp:TextBox></b></td>
            <td Width="70px"><b><asp:TextBox ID="lv_uom_9" MaxLength="3" Width="35px" onfocus="getprevval(this)" onblur="checkUom(this)" Text='<%# Bind("lv_uom_9") %>' runat="server"></asp:TextBox></b>
                <a href="#" tabindex="1" onClick="uomlook('9'); return false"><asp:Image ID="Image7" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
            </td>
            <td Width="100px" nowrap><b><asp:TextBox ID="lv_date_9" onfocus="javascript:preEnter( this, 'yes' );" onblur="javascript:preLeave( this, 'date', '99/99/9999' );" MaxLength="10" Width="70px" Text='<%# Bind("lv_date_9","{0:MM/dd/yyyy}") %>' runat="server"></asp:TextBox></b>
                <a href="#" tabindex="1" onblur="ctl00$ContentPlaceHolder1$FormView1$lv_date_9.focus()" onClick="showCalendarControl(ctl00$ContentPlaceHolder1$FormView1$lv_date_9); return false"><asp:Image ID="Image17" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
            </td>
            </tr>
            <tr>
            <td Width="70px"><b><asp:TextBox ID="lv_qty_10" MaxLength="8" Width="50px" onfocus="getprevval(this)" onblur="defaultVal('10');checkNumVal(this, 'q');displayqty(this, '2')" Text='<%# Bind("lv_qty_10") %>' runat="server"></asp:TextBox></b></td>
            <td Width="30px"><b><asp:TextBox ID="lv_delivery_10" MaxLength="3" Width="30px" onfocus="getprevval(this)" onblur="checkNumVal(this, 'd')" Text='<%# Bind("lv_delivery_10") %>' runat="server"></asp:TextBox></b></td>
            <td Width="70px"><b><asp:TextBox ID="lv_price_10" Width="40px" MaxLength="5" onfocus="getprevval(this)" onblur="checkNumVal(this, 'p')" Text='<%# Bind("lv_price_10") %>' runat="server"></asp:TextBox></b></td>
            <td Width="70px"><b><asp:TextBox ID="lv_uom_10" MaxLength="3" Width="35px" onfocus="getprevval(this)" onblur="checkUom(this)" Text='<%# Bind("lv_uom_10") %>' runat="server"></asp:TextBox></b>
                <a href="#" tabindex="1" onClick="uomlook('10'); return false"><asp:Image ID="Image6" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
            </td>
            <td Width="100px" nowrap><b><asp:TextBox ID="lv_date_10" onfocus="javascript:preEnter( this, 'yes' );" onblur="javascript:preLeave( this, 'date', '99/99/9999' );" MaxLength="10" Width="70px" Text='<%# Bind("lv_date_10","{0:MM/dd/yyyy}") %>' runat="server"></asp:TextBox></b>
                <a href="#" tabindex="1" onblur="ctl00$ContentPlaceHolder1$FormView1$lv_date_10.focus()" onClick="showCalendarControl(ctl00$ContentPlaceHolder1$FormView1$lv_date_10); return false"><asp:Image ID="Image16" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
            </td>
            </tr>
            
            <%--</table>--%>
            <tr><td colspan="2">
            
            <%--<br />--%>
            <input type="hidden"  name="HiddenRowid" value='<%# Server.UrlEncode(Convert.ToString(DataBinder.Eval(Container,"DataItem.RfqRowid"))) %>' />
            <input type="hidden"  name="ARowid" value='<%# Server.UrlEncode(Convert.ToString(DataBinder.Eval(Container,"DataItem.ARowid"))) %>' />
            <br />
            <%--<table><tr><td>--%>
           
            <%--<asp:Button ID="Button2" runat="server" CausesValidation="False" OnClick="cancelbutton_click" CssClass="buttonM" Text="Cancel" />--%>                       
            </td></tr></table>
            
           </td></tr>
           <tr><td>
            <asp:Button ID="Button1" runat="server"   CssClass="buttonM" onclick="Insertbutton_Click" Text="Save" >
            </asp:Button>
            <asp:Button ID="Button2" runat="server" CausesValidation="False" CommandName="Cancel" CssClass="buttonM" Text="Cancel">
            </asp:Button>
           </td></tr>
           </table>
           
            </fieldset>
          </asp:Panel>  
        </InsertItemTemplate>
        <ItemTemplate>
         <fieldset style="background-color:#EFF3FB">       
        
        <table class="shade">            
        <tr>                 
        <td nowrap align="left" style="padding-right:5px"><b>Seq No:</b>              
        <br /><b><asp:Label ID="RfqSeqNoLabel" BackColor="PaleTurquoise" Width="50px" BorderColor="White" BorderStyle="Solid" BorderWidth="1px" runat="server" Text='<%# Bind("RfqSeqNo") %>'></asp:Label></b></td>            
        <td nowrap align="left" style="padding-right:5px"><b>Qty:</b><br /><b><asp:Label ID="RfqQtyLabel" BackColor="PaleTurquoise" Width="60px" BorderColor="White" BorderStyle="Solid" BorderWidth="1px" runat="server" Text='<%# Bind("RfqQty") %>'></asp:Label></b></td>        
        <td nowrap align="left" style="padding-right:5px"><b>Fg Item#:</b><br /><b><asp:Label ID="RfqStockLabel" runat="server" BackColor="PaleTurquoise" Width="120px" BorderColor="White" BorderStyle="Solid" BorderWidth="1px" Text='<%# Bind("RfqStock") %>'></asp:Label></b></td>                    
        <td nowrap align="left" style="padding-right:5px"><b>Item Name:</b><br /><b><asp:Label ID="RfqNameLabel" runat="server" BackColor="PaleTurquoise" Width="120px" BorderColor="White" BorderStyle="Solid" BorderWidth="1px" Text='<%# Bind("RfqName") %>'></asp:Label></b></td>
        <td nowrap  align="left" style="padding-right:5px"><b>Customer Part#:</b><br /><b><asp:Label ID="RfqPartnoLabel" runat="server" BackColor="PaleTurquoise" Width="120px" BorderColor="White" BorderStyle="Solid" BorderWidth="1px" Text='<%# Bind("RfqPartno") %>'></asp:Label></b></td>               
        <td nowrap align="left" style="padding-right:5px"><b>Category:</b><br /><b><asp:Label ID="RfqProcatLabel" runat="server" BackColor="PaleTurquoise" Width="120px" BorderColor="White" BorderStyle="Solid" BorderWidth="1px" Text='<%# Bind("RfqProcat") %>'></asp:Label></b></td>
        <td nowrap  align="left" style="padding-right:5px"><b>Color:</b><br /><b><asp:Label ID="RfqColLabel" runat="server" BackColor="PaleTurquoise" Width="60px" BorderColor="White" BorderStyle="Solid" BorderWidth="1px" Text='<%# Bind("RfqCol") %>'></asp:Label></b></td>
        <td nowrap align="left" style="padding-right:5px"><b>Coating:</b><br /><b><asp:Label ID="RfqCoatLabel" runat="server" BackColor="PaleTurquoise" Width="60px" BorderColor="White" BorderStyle="Solid" BorderWidth="1px" Text='<%# Bind("RfqCoat") %>'></asp:Label></b></td>
        <td nowrap align="left" style="padding-right:5px"><b>Style:</b><br /><b><asp:Label ID="RfqstyleLabel" runat="server" BackColor="PaleTurquoise" Width="120px" BorderColor="White" BorderStyle="Solid" BorderWidth="1px" Text='<%# Bind("Rfqstyle") %>'></asp:Label></b></td>
        <td nowrap align="left" style="padding-right:5px"><b>Length:</b><br /><b><asp:Label ID="RfqLengthLabel" runat="server" BackColor="PaleTurquoise" Width="60px" BorderColor="White" BorderStyle="Solid" BorderWidth="1px" Text='<%# Bind("RfqLength") %>'></asp:Label></b></td>
        <td nowrap align="left" style="padding-right:5px"><b>Width:</b><br /><b><asp:Label ID="RfqWidthLabel" runat="server" BackColor="PaleTurquoise" Width="60px" BorderColor="White" BorderStyle="Solid" BorderWidth="1px" Text='<%# Bind("RfqWidth") %>'></asp:Label></b></td>
        <td nowrap align="left" style="padding-right:5px"><b>Depth:</b><br /><b><asp:Label ID="RfqDepthLabel" runat="server" BackColor="PaleTurquoise" Width="60px" BorderColor="White" BorderStyle="Solid" BorderWidth="1px" Text='<%# Bind("RfqDepth") %>'></asp:Label></b></td>
        <td nowrap align="left" style="padding-right:5px"><b>Board:</b><br /><b><asp:Label ID="RfqBoardLabel" runat="server" BackColor="PaleTurquoise" Width="120px" BorderColor="White" BorderStyle="Solid" BorderWidth="1px" Text='<%# Bind("RfqBoard") %>'></asp:Label></b></td>
        <td nowrap align="left" style="padding-right:5px"><b>Caliper:</b><br /><b><asp:Label ID="RfqCalLabel" runat="server" BackColor="PaleTurquoise" Width="60px" BorderColor="White" BorderStyle="Solid" BorderWidth="1px" Text='<%# Bind("RfqCal") %>'></asp:Label></b></td>        
        <td nowrap align="left" style="padding-right:5px"><b>Qty/Set:</b><br /><b><asp:Label ID="qtysetLabel" runat="server" BackColor="PaleTurquoise" Width="60px" BorderColor="White" BorderStyle="Solid" BorderWidth="1px" Text='<%# Bind("qtyset") %>'></asp:Label></b></td>              
        </tr>
        
        </table>
           
           
           <br />
                <asp:Button ID="EditButton" runat="server" CausesValidation="False" CommandName="Edit" CssClass="buttonM" 
                   Text="Update">
                </asp:Button>
                <asp:Button ID="DeleteButton" runat="server"  CssClass="buttonM" OnClick="Delete_RfqItem" OnClientClick="return confirm('Are you sure you want to delete this record')"
                    Text="Delete">
                </asp:Button>                                
                
                <asp:Button ID="NewButton" runat="server" CausesValidation="False" CommandName="New" CssClass="buttonM" 
                   Text="Add">
                </asp:Button> 
                <%--<asp:Button ID="CopyButton" runat="server" CausesValidation="False" CommandName="Edit" CssClass="buttonM" 
                  OnClick="Copy_Rfqitem"  Text="Copy">
                </asp:Button>
                
                <asp:Button ID="EstimateButton" runat="server" CausesValidation="False"  CssClass="buttonM" 
               OnClick = "rfq_estimate"     Text="Submit Quote">
                </asp:Button>--%>
                </fieldset> 
        </ItemTemplate>
    </asp:FormView>
    <asp:ObjectDataSource ID="ObjectDataSource2" runat="server" OldValuesParameterFormatString="original_{0}"
        SelectMethod="Rfqitem" TypeName="rfqs">
        <SelectParameters>
            <asp:Parameter Name="prmComp" Type="String" />
            <asp:Parameter Name="prmUser" Type="String" />
            <asp:Parameter Name="prmAction" DefaultValue="select" Type="String" />
            <asp:SessionParameter Name="prmRfqNo" SessionField="Rfqseq" Type="Int32" />
            <asp:SessionParameter SessionField="rfqcustpart" Name="RfqSeqNo" Type="Int32" />
            <asp:Parameter Name="RfqQty" Type="Int32" />
            <asp:Parameter Name="RfqStock" Type="String" />
            <asp:Parameter Name="RfqName" Type="String" />
            <asp:Parameter Name="RfqPartno" Type="String" />
            <asp:Parameter Name="Rfqstyle" Type="String" />
            <asp:Parameter Name="RfqProcat" Type="String" />
            <asp:Parameter Name="RfqCol" Type="Int32" />
            <asp:Parameter Name="RfqCoat" Type="Int32" />
            <asp:Parameter Name="RfqLength" Type="Decimal" />
            <asp:Parameter Name="RfqWidth" Type="Decimal" />
            <asp:Parameter Name="RfqDepth" Type="Decimal" />
            <asp:Parameter Name="RfqBoard" Type="String" />
            <asp:Parameter Name="RfqCal" Type="Decimal" />
            <asp:Parameter Name="RfqQuantity" Type="Int32" />
            <asp:Parameter Name="RfqRowid" Type="Int64" />
            
            <asp:Parameter Name="lv_qty2" Type="Int32" />
            <asp:Parameter Name="lv_qty3" Type="Int32" />
            <asp:Parameter Name="lv_qty4" Type="Int32" />
            <asp:Parameter Name="lv_qty5" Type="Int32" />
            <asp:Parameter Name="lv_qty6" Type="Int32" />
            <asp:Parameter Name="lv_qty7" Type="Int32" />
            <asp:Parameter Name="lv_qty8" Type="Int32" />
            <asp:Parameter Name="lv_qty9" Type="Int32" />
            <asp:Parameter Name="lv_qty10" Type="Int32" />
            <asp:Parameter Name="lv_price_1" Type="Decimal" />
            <asp:Parameter Name="lv_price_2" Type="Decimal" />
            <asp:Parameter Name="lv_price_3" Type="Decimal" />
            <asp:Parameter Name="lv_price_4" Type="Decimal" />
            <asp:Parameter Name="lv_price_5" Type="Decimal" />
            <asp:Parameter Name="lv_price_6" Type="Decimal" />
            <asp:Parameter Name="lv_price_7" Type="Decimal" />
            <asp:Parameter Name="lv_price_8" Type="Decimal" />
            <asp:Parameter Name="lv_price_9" Type="Decimal" />
            <asp:Parameter Name="lv_price_10" Type="Decimal" />
            <asp:Parameter Name="lv_uom_1" Type="String" />
            <asp:Parameter Name="lv_uom_2" Type="String" />
            <asp:Parameter Name="lv_uom_3" Type="String" />
            <asp:Parameter Name="lv_uom_4" Type="String" />
            <asp:Parameter Name="lv_uom_5" Type="String" />
            <asp:Parameter Name="lv_uom_6" Type="String" />
            <asp:Parameter Name="lv_uom_7" Type="String" />
            <asp:Parameter Name="lv_uom_8" Type="String" />
            <asp:Parameter Name="lv_uom_9" Type="String" />
            <asp:Parameter Name="lv_uom_10" Type="String" />
            <asp:Parameter Name="lv_date_1" Type="DateTime" />
            <asp:Parameter Name="lv_date_2" Type="DateTime" />
            <asp:Parameter Name="lv_date_3" Type="DateTime" />
            <asp:Parameter Name="lv_date_4" Type="DateTime" />
            <asp:Parameter Name="lv_date_5" Type="DateTime" />
            <asp:Parameter Name="lv_date_6" Type="DateTime" />
            <asp:Parameter Name="lv_date_7" Type="DateTime" />
            <asp:Parameter Name="lv_date_8" Type="DateTime" />
            <asp:Parameter Name="lv_date_9" Type="DateTime" />
            <asp:Parameter Name="lv_date_10" Type="DateTime" />
            <asp:Parameter Name="lv_delivery_1" Type="Int32" />
            <asp:Parameter Name="lv_delivery_2" Type="Int32" />
            <asp:Parameter Name="lv_delivery_3" Type="Int32" />
            <asp:Parameter Name="lv_delivery_4" Type="Int32" />
            <asp:Parameter Name="lv_delivery_5" Type="Int32" />
            <asp:Parameter Name="lv_delivery_6" Type="Int32" />
            <asp:Parameter Name="lv_delivery_7" Type="Int32" />
            <asp:Parameter Name="lv_delivery_8" Type="Int32" />
            <asp:Parameter Name="lv_delivery_9" Type="Int32" />
            <asp:Parameter Name="lv_delivery_10" Type="Int32" />
            <asp:Parameter Name="RfqEstNo" Type="string" />
            </SelectParameters>
    </asp:ObjectDataSource>
</div>
</asp:Content>
