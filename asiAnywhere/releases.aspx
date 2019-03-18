<%@ Page Language="C#" MasterPageFile="~/MasterPage.master" Debug="false" AutoEventWireup="true" Inherits="releases" Title="Releases" Codebehind="releases.aspx.cs" %>
<asp:Content ID="Content1" ContentPlaceHolderID="ContentPlaceHolder1" Runat="Server">


<script>

function Datevalidate()
{
    var duedate=document.getElementById("ctl00_ContentPlaceHolder1_FormView1_MyDate").value;
    
    if(duedate.length>1 && duedate.length<3 && duedate.indexOf('/')!=1)
    {
        document.getElementById("ctl00_ContentPlaceHolder1_FormView1_MyDate").value = duedate + "/";
    }
    if(duedate.length>4 && duedate.length<6 && duedate.indexOf('/')!=3)
    {
        document.getElementById("ctl00_ContentPlaceHolder1_FormView1_MyDate").value = duedate + "/";
    }
}

function ShipIdlook(){ 
  var NewWindow = window.open("ShipIdLook.aspx","ShipToLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}

function ShipIdLookup(ReturnObj1){ 
  document.forms[0].ctl00_ContentPlaceHolder1_FormView1_ShipToTextBox.value = ReturnObj1;
  //document.forms[0].ctl00$ContentPlaceHolder1$FormView1$VShipnameTextBox.value = ReturnObj2;
}
function Carrierlook(){ 
  var NewWindow = window.open("Carrier_lookup.aspx","CarrierLookupWindow","width=600,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}

function Carrierlookup(ReturnObj1){ 
  document.forms[0].ctl00$ContentPlaceHolder1$FormView1$ViaTextBox.value = ReturnObj1;
}


function Datelook(){ 
  var NewWindow = window.open("date_lookup.aspx","DateLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
  }
function Datelookup(obj)
{
  document.forms[0].ctl00$ContentPlaceHolder1$FormView1$MyDate.value=obj;
}


function Datelook1()
{
  document.forms[0].ctl00$ContentPlaceHolder1$FormView1$MyDate.value="";
  Datelook();
}
//function validate2()
//{
//var old=document.forms[0].ctl00$ContentPlaceHolder1$FormView1$MyDate.value;
// var ys=confirm("You Cannot Modify Date Manually. Want To Use Calender?");
// if(ys)
// {
//   Datelook();
// }
// else
// {
//  document.forms[0].ctl00$ContentPlaceHolder1$FormView1$MyDate.value="";
// }
//}

function showFeature(vchangedate)
{


var a=document.getElementById("vdate");
var b=document.getElementById("po");
a.style.display='inline';
b.style.display='none';
}
function showFeature2(vpo)
{

var a=document.getElementById("vdate");
var b=document.getElementById("po");
b.style.display='inline';
a.style.display='none';
}

function checkfrtpay(obj) {
    var frpy = (obj.value).toUpperCase();

    if (frpy != "" && frpy != "P" && frpy != "C" && frpy != "B" && frpy != "T") {
        alert("Invalid Freight Pay Code, Pls enter P,C,B Or T");
        obj.focus();
        return;
    }
}

function checkfob(obj) {
    var fb = (obj.value).toUpperCase();

    if (fb != "" && fb != "D" && fb != "O") {
        alert("Invalid FOB Code, Pls enter (D)est Or (O)rig");
        obj.focus();
        return;
    }
}

//function showFeature(vchangedate){


//if ((vchangedate == "YES"))
//{
//document.all["vdate"].style.display = "block";
//}

//document.all["vdate"].style.display = "none";
//}
//function showFeature2(vpo)
//{

//if ((vpo == "YES")){
//document.all["po"].style.display = "block";
//}

//document.all["po"].style.display = "none";
//}







var isNav4 = false, isNav5 = false, isIE4 = false
var strSeperator = "/"; 

var vDateType = 3; 
var vYearType = 4; 
var vYearLength = 2; 
var err = 0; 
if(navigator.appName == "Netscape") {
if (navigator.appVersion < "5") {
isNav4 = true;
isNav5 = false;
}
else
if (navigator.appVersion > "4") {
isNav4 = false;
isNav5 = true;
   }
}
else {
isIE4 = true;
}
function DateFormat(vDateName, vDateValue, e, dateCheck, dateType) {
vDateType = dateType;

if (vDateValue == "~") {
alert("AppVersion = "+navigator.appVersion+" \nNav. 4 Version = "+isNav4+" \nNav. 5 Version = "+isNav5+" \nIE Version = "+isIE4+" \nYear Type = "+vYearType+" \nDate Type = "+vDateType+" \nSeparator = "+strSeperator);
vDateName.value = "";
vDateName.focus();
return true;
}
var whichCode = (window.Event) ? e.which : e.keyCode;

if (vDateValue.length > 8 && isNav4) {
if ((vDateValue.indexOf("-") >= 1) || (vDateValue.indexOf("/") >= 1))
return true;
}

var alphaCheck = " abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ/-";
if (alphaCheck.indexOf(vDateValue) >= 1) {
if (isNav4) {
vDateName.value = "";
vDateName.focus();
vDateName.select();
return false;
}
else {
vDateName.value = vDateName.value.substr(0, (vDateValue.length-1));
return false;
   }
}
if (whichCode == 8) 
return false;
else {

var strCheck = '47,48,49,50,51,52,53,54,55,56,57,58,59,95,96,97,98,99,100,101,102,103,104,105';
if (strCheck.indexOf(whichCode) != -1) {
if (isNav4) {
if (((vDateValue.length < 6 && dateCheck) || (vDateValue.length == 7 && dateCheck)) && (vDateValue.length >=1)) {
alert("Invalid Date. The Format is mm/dd/yyyy.\nPlease Re-Enter");
vDateName.value = "";
vDateName.focus();
vDateName.select();
return false;
}
if (vDateValue.length == 6 && dateCheck) {
var mDay = vDateName.value.substr(2,2);
var mMonth = vDateName.value.substr(0,2);
var mYear = vDateName.value.substr(4,4)

if (mYear.length == 2 && vYearType == 4) {
var mToday = new Date();

var checkYear = mToday.getFullYear() + 30; 
var mCheckYear = '20' + mYear;
if (mCheckYear >= checkYear)
mYear = '19' + mYear;
else
mYear = '20' + mYear;
}
var vDateValueCheck = mMonth+strSeperator+mDay+strSeperator+mYear;
if (!dateValid(vDateValueCheck)) {
alert("Invalid Date. The Format is mm/dd/yyyy.\nPlease Re-Enter");
vDateName.value = "";
vDateName.focus();
vDateName.select();
return false;
}
return true;
}
else {

if (vDateValue.length >= 8  && dateCheck) {
if (vDateType == 1) 
{
var mDay = vDateName.value.substr(2,2);
var mMonth = vDateName.value.substr(0,2);
var mYear = vDateName.value.substr(4,4)
vDateName.value = mMonth+strSeperator+mDay+strSeperator+mYear;
}
if (vDateType == 2) 
{
var mYear = vDateName.value.substr(0,4)
var mMonth = vDateName.value.substr(4,2);
var mDay = vDateName.value.substr(6,2);
vDateName.value = mYear+strSeperator+mMonth+strSeperator+mDay;
}
if (vDateType == 3)
{
var mMonth = vDateName.value.substr(0,2);
var mDay = vDateName.value.substr(0,2);
var mYear = vDateName.value.substr(4,4)
vDateName.value = mDay+strSeperator+mMonth+strSeperator+mYear;
}

var vDateTypeTemp = vDateType;
vDateType = 1;
var vDateValueCheck = mMonth+strSeperator+mDay+strSeperator+mYear;
if (!dateValid(vDateValueCheck)) {
alert("Invalid Date. The Format is mm/dd/yyyy.\nPlease Re-Enter");
vDateType = vDateTypeTemp;
vDateName.value = "";
vDateName.focus();
vDateName.select();
return false;
}
vDateType = vDateTypeTemp;
return true;
}
else {
if (((vDateValue.length < 6 && dateCheck) || (vDateValue.length == 9 && dateCheck)) && (vDateValue.length >=1)) {
alert("Invalid Date. The Format  is mm/dd/yyyy.\nPlease Re-Enter");
vDateName.value = "";
vDateName.focus();
vDateName.select();
return false;
         }
      }
   }
}
else {

if (((vDateValue.length < 6 && dateCheck) || (vDateValue.length == 9 && dateCheck)) && (vDateValue.length >=1)) {
alert("Invalid Date. The Format e is mm/dd/yyyy.\nPlease Re-Enter");
vDateName.value = "";
vDateName.focus();
return true;
}

if (vDateValue.length >= 8 && dateCheck) {

if (vDateType == 1) 
{
var mMonth = vDateName.value.substr(0,2);
var mDay = vDateName.value.substr(3,2);
var mYear = vDateName.value.substr(6,4)
}
if (vDateType == 2) 
{
var mYear = vDateName.value.substr(0,4)
var mMonth = vDateName.value.substr(5,2);
var mDay = vDateName.value.substr(8,2);
}
if (vDateType == 3) 
{
var mDay = vDateName.value.substr(0,2);
var mMonth = vDateName.value.substr(3,2);
var mYear = vDateName.value.substr(6,4)
}
if (vYearLength == 4) {
if (mYear.length < 4) {
alert("Invalid Date. The Format is mm/dd/yyyy.\nPlease Re-Enter");
vDateName.value = "";
vDateName.focus();
return true;
   }
}

var vDateTypeTemp = vDateType;

vDateType = 1;

var vDateValueCheck = mMonth+strSeperator+mDay+strSeperator+mYear;
if (mYear.length == 2 && vYearType == 4 && dateCheck) {

var mToday = new Date();

var checkYear = mToday.getFullYear() + 30; 
var mCheckYear = '20' + mYear;
if (mCheckYear >= checkYear)
mYear = '19' + mYear;
else
mYear = '20' + mYear;
vDateValueCheck = mMonth+strSeperator+mDay+strSeperator+mYear;

if (vDateTypeTemp == 1) // mm/dd/yyyy
vDateName.value = mMonth+strSeperator+mDay+strSeperator+mYear;
if (vDateTypeTemp == 3) // dd/mm/yyyy
vDateName.value = mDay+strSeperator+mMonth+strSeperator+mYear;
} 
if (!dateValid(vDateValueCheck)) {
alert("Invalid Date. The Format is mm/dd/yyyy.\nPlease Re-Enter");
vDateType = vDateTypeTemp;
vDateName.value = "";
vDateName.focus();
return true;
}
vDateType = vDateTypeTemp;
return true;
}
else {
if (vDateType == 1) {
if (vDateValue.length == 2) {
vDateName.value = vDateValue+strSeperator;
}
if (vDateValue.length == 5) {
vDateName.value = vDateValue+strSeperator;
   }
}
if (vDateType == 2) {
if (vDateValue.length == 4) {
vDateName.value = vDateValue+strSeperator;
}
if (vDateValue.length == 7) {
vDateName.value = vDateValue+strSeperator;
   }
} 
if (vDateType == 3) {
if (vDateValue.length == 2) {
vDateName.value = vDateValue+strSeperator;
}
if (vDateValue.length == 5) {
vDateName.value = vDateValue+strSeperator;
   }
}
return true;
   }
}
if (vDateValue.length == 10&& dateCheck) {
if (!dateValid(vDateName)) {
 
alert("Invalid Date. The Format is mm/dd/yyyy.\nPlease Re-Enter");
vDateName.focus();
vDateName.select();
   }
}
return false;
}
else {

if (isNav4) {
vDateName.value = "";
vDateName.focus();
vDateName.select();
return false;
}
else
{
vDateName.value = vDateName.value.substr(0, (vDateValue.length-1));
return false;
         }
      }
   }
}
function dateValid(objName) {
var strDate;
var strDateArray;
var strDay;
var strMonth;
var strYear;
var intday;
var intMonth;
var intYear;
var booFound = false;
var datefield = objName;
var strSeparatorArray = new Array("-"," ","/",".");
var intElementNr;
// var err = 0;
var strMonthArray = new Array(12);
strMonthArray[0] = "Jan";
strMonthArray[1] = "Feb";
strMonthArray[2] = "Mar";
strMonthArray[3] = "Apr";
strMonthArray[4] = "May";
strMonthArray[5] = "Jun";
strMonthArray[6] = "Jul";
strMonthArray[7] = "Aug";
strMonthArray[8] = "Sep";
strMonthArray[9] = "Oct";
strMonthArray[10] = "Nov";
strMonthArray[11] = "Dec";
//strDate = datefield.value;
strDate = objName;
if (strDate.length < 1) {
return true;
}
for (intElementNr = 0; intElementNr < strSeparatorArray.length; intElementNr++) {
if (strDate.indexOf(strSeparatorArray[intElementNr]) != -1) {
strDateArray = strDate.split(strSeparatorArray[intElementNr]);
if (strDateArray.length != 3) {
err = 1;
return false;
}
else {
strDay = strDateArray[0];
strMonth = strDateArray[1];
strYear = strDateArray[2];
}
booFound = true;
   }
}
if (booFound == false) {
if (strDate.length>5) {
strDay = strDate.substr(0, 2);
strMonth = strDate.substr(2, 2);
strYear = strDate.substr(4);
   }
}
//Adjustment for short years entered
if (strYear.length == 2) {
strYear = '20' + strYear;
}
strTemp = strDay;
strDay = strMonth;
strMonth = strTemp;
intday = parseInt(strDay, 10);
if (isNaN(intday)) {
err = 2;
return false;
}
intMonth = parseInt(strMonth, 10);
if (isNaN(intMonth)) {
for (i = 0;i<12;i++) {
if (strMonth.toUpperCase() == strMonthArray[i].toUpperCase()) {
intMonth = i+1;
strMonth = strMonthArray[i];
i = 12;
   }
}
if (isNaN(intMonth)) {
err = 3;
return false;
   }
}
intYear = parseInt(strYear, 10);
if (isNaN(intYear)) {
err = 4;
return false;
}
if (intMonth>12 || intMonth<1) {
err = 5;
return false;
}
if ((intMonth == 1 || intMonth == 3 || intMonth == 5 || intMonth == 7 || intMonth == 8 || intMonth == 10 || intMonth == 12) && (intday > 31 || intday < 1)) {
err = 6;
return false;
}
if ((intMonth == 4 || intMonth == 6 || intMonth == 9 || intMonth == 11) && (intday > 30 || intday < 1)) {
err = 7;
return false;
}
if (intMonth == 2) {
if (intday < 1) {
err = 8;
return false;
}
if (LeapYear(intYear) == true) {
if (intday > 29) {
err = 9;
return false;
   }
}
else {
if (intday > 28) {
err = 10;
return false;
      }
   }
}
return true;
}
function LeapYear(intYear) {
if (intYear % 100 == 0) {
if (intYear % 400 == 0) { return true; }
}
else {
if ((intYear % 4) == 0) { return true; }
}
return false;
}


function confirmmessage()
{
alert("Shipto already exists.");
}
</script>
<div>
    
    
    
        <asp:FormView ID="FormView2" runat="server" CellPadding="4" DataSourceID="ObjectDataSource3" ForeColor="#333333" Width="940px">
            <FooterStyle BackColor="#507CD1" Font-Bold="True" ForeColor="White" />
            <EditRowStyle BackColor="#2461BF" />
            
            <ItemTemplate>
            <fieldset style="background-color:#EFF3FB;width:940px">
            <table>
            <tr>
            <td style="width: 33px"><b>Item#:</b></td>
            <td style="width: 85px"><b><asp:Label ID="Item1Label" runat="server" Text='<%# Bind("Item1") %>' BackColor="Turquoise" Width="120px" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px"></asp:Label></b></td>
            <td><b></b></td>
            <td style="width: 165px"><b><asp:Label ID="NameLabel" runat="server" Text='<%# Bind("Name") %>' BackColor="Turquoise" Width="120px" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px"></asp:Label></b></td>
            <td style="width: 40px"><b>Order#:</b></td>
            <td style="width: 147px"><b><asp:Label ID="OrderLabel" runat="server" Text='<%# Bind("Order") %>' BackColor="Turquoise" Width="120px" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px"></asp:Label></b></td>
            <td><b>Estimate#:</b></td>
            <td style="width: 148px"><b><asp:Label ID="EstimateLabel" runat="server" Text='<%# Bind("Estimate") %>' BackColor="Turquoise" Width="120px" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px"></asp:Label></b></td>
            </tr>
            </table>
            <fieldset style="width: 918px">
            <table>
            <tr>
            <td style="width: 79px; height: 56px;"><b>FG Qtys:</b> </td>
            <td style="width: 758px; height: 56px;">
            <table>
            <tr>
            <td style="width: 127px"><b>On Hand</b></td>
            <td style="width: 127px"><b>On Order</b></td>
            <td style="width: 130px"><b>Allocated</b></td>
            <td style="width: 130px"><b>Backorder</b></td>
            <td style="width: 129px"><b>Available</b></td>
            <td style="width: 137px"><b>Reorder</b></td>
            </tr>
            <tr>
            <td style="width: 127px"><b><asp:Label ID="q_onhLabel" runat="server" Text='<%# Bind("[q-onh]") %>' BackColor="Turquoise" Width="120px" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px"></asp:Label></b></td>
            <td style="width: 127px"><b><asp:Label ID="q_onoLabel" runat="server" Text='<%# Bind("[q-ono]") %>' BackColor="Turquoise" Width="120px" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px"></asp:Label></b></td>
            <td style="width: 130px"><b><asp:Label ID="q_allocLabel" runat="server" Text='<%# Bind("[q-alloc]") %>' BackColor="Turquoise" Width="120px" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px"></asp:Label></b></td>
            <td style="width: 130px"><b><asp:Label ID="q_backLabel" runat="server" Text='<%# Bind("[q-back]") %>' BackColor="Turquoise" Width="120px" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px"></asp:Label></b></td>
            <td style="width: 129px"><b><asp:Label ID="q_availLabel" runat="server" Text='<%# Bind("[q-avail]") %>' BackColor="Turquoise" Width="120px" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px"></asp:Label></b></td>
            <td style="width: 137px"><b> <asp:Label ID="ord_levelLabel" runat="server" Text='<%# Bind("[ord-level]") %>' BackColor="Turquoise" Width="120px" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px"></asp:Label></b></td>
            </tr>
            </table>
            </td>
            </tr>
            </table>
            </fieldset>
            </fieldset>
                
                
                
                
            </ItemTemplate>
            <HeaderStyle BackColor="#507CD1" Font-Bold="True" ForeColor="White" />
            <RowStyle BackColor="#EFF3FB" />
            <PagerStyle BackColor="#2461BF" ForeColor="White" HorizontalAlign="Center" />
            
        </asp:FormView>
         
   
        <asp:ObjectDataSource ID="ObjectDataSource3" runat="server" OldValuesParameterFormatString="original_{0}"
            SelectMethod="SelectviewSel" TypeName="Order" >
        <SelectParameters>
            <asp:Parameter Name="prmUser" Type="String" />
            <asp:SessionParameter Name="prmOrderNum" SessionField="releases" Type="String" />
            <asp:Parameter DefaultValue="Select" Name="prmAction" Type="String" />
            <asp:SessionParameter DefaultValue="" Name="prmItemNum" SessionField="line" Type="String" />
        </SelectParameters>
    </asp:ObjectDataSource>
    <br />
    
    
    
   
   
   
    
    <asp:GridView ID="GridView1" runat="server" AutoGenerateColumns="False" DataSourceID="ObjectDataSource1" AllowPaging="True" AllowSorting="True" BorderStyle="Dotted" EmptyDataText="No Record Found" Width="950px">
        <Columns>
        
        <asp:TemplateField HeaderText="Select" >
               <ItemStyle HorizontalAlign="Center" />
               <ItemTemplate>
                   
                   <input type="radio" checked="checked" name="Relradio" value='<%# Server.UrlEncode(Convert.ToString(DataBinder.Eval(Container,"DataItem.vRowid"))) %>,<%# Server.UrlEncode(Convert.ToString(DataBinder.Eval(Container,"DataItem.Vstatus"))) %>' />
               </ItemTemplate>
                   <ControlStyle Width="28px" />
                   <HeaderStyle CssClass="headcolor" ForeColor="White" />
           
               </asp:TemplateField>
               
            <asp:BoundField DataField="print" HeaderText="Prt" SortExpression="print" >
                <ItemStyle Wrap="False" />
            </asp:BoundField>
                  
            <asp:BoundField DataField="SI" HeaderText="S/I" SortExpression="SI" >
                <ItemStyle Wrap="False" />
            </asp:BoundField>
            <asp:BoundField DataField="ShipTo" HeaderText="Ship To" SortExpression="ShipTo" >
                <ItemStyle Wrap="False" />
            </asp:BoundField>
           
            <asp:BoundField DataField="Vstatus" HeaderText="S" SortExpression="Vstatus" >
                 
                <ItemStyle Wrap="False" />
            </asp:BoundField>
            <asp:BoundField DataField="Via" HeaderText="Via" SortExpression="Via" >
                <ItemStyle Wrap="False" />
            </asp:BoundField>
            <asp:TemplateField HeaderText="Sched Qty" SortExpression="SQty">
               
                <ItemTemplate>
                    <asp:Label ID="Label1" runat="server" Text='<%# Bind("SQty","{0:###,##0}") %>'></asp:Label>
                </ItemTemplate>
                <ItemStyle HorizontalAlign="Right" Wrap="False" />
            </asp:TemplateField>
            <asp:TemplateField HeaderText="Actual Qty" SortExpression="qty">
                
                <ItemTemplate>
                    <asp:Label ID="Label2" runat="server" Text='<%# Bind("qty","{0:###,##0}") %>'></asp:Label>
                </ItemTemplate>
                <ItemStyle HorizontalAlign="Right" Wrap="False" />
            </asp:TemplateField>
            <asp:BoundField DataField="po-no" HeaderText="Customer PO#" SortExpression="po-no" >
                <ItemStyle Wrap="False" />
            </asp:BoundField>
            <asp:BoundField DataField="lot-no" HeaderText="Customer Lot#" SortExpression="lot-no" >
                <ItemStyle Wrap="False" />
            </asp:BoundField>
            <asp:BoundField DataField="Vdate" HeaderText="Date" SortExpression="Vdate" DataFormatString="{0:MM/dd/yyyy}" >
                <ItemStyle Wrap="False" />
            </asp:BoundField>
            <asp:BoundField DataField="addr" HeaderText="Ship To Address" SortExpression="addr" >
                <ItemStyle Wrap="False" />
            </asp:BoundField>
            <asp:BoundField DataField="city" HeaderText="City" SortExpression="city" >
                <ItemStyle Wrap="False" />
            </asp:BoundField>
            
            <asp:BoundField DataField="state" HeaderText="State" SortExpression="state" >
                <ItemStyle Wrap="False" />
            </asp:BoundField>
            <asp:TemplateField HeaderText="Sell Price" SortExpression="sell-price">
                
                <ItemTemplate>
                    <asp:Label ID="Label3" runat="server" Text='<%# Bind("[sell-price]","{0:###,##0.00}") %>'></asp:Label>
                </ItemTemplate>
                <ItemStyle HorizontalAlign="Right" Wrap="False" />
            </asp:TemplateField>
            <asp:TemplateField HeaderText="Discount" SortExpression="disc">
                
                <ItemTemplate>
                    <asp:Label ID="Label4" runat="server" Text='<%# Bind("disc","{0:##0.00}") %>'></asp:Label>
                </ItemTemplate>
                <ItemStyle HorizontalAlign="Right" Wrap="False" />
            </asp:TemplateField>          
                      
            <asp:TemplateField HeaderText="Ext Price" SortExpression="price">
            
                
                <ItemTemplate>
                    <asp:Label ID="Label5" runat="server" Text='<%# Bind("price","{0:##0.00}") %>'></asp:Label>
                </ItemTemplate>
                <ItemStyle HorizontalAlign="Right" Wrap="False" />
            </asp:TemplateField>
            <asp:TemplateField HeaderText="Frt Pay" SortExpression="frtpay">
                
                <ItemTemplate>
                    <asp:Label ID="frtpayLabel" runat="server" Text='<%# Bind("frtpay") %>'></asp:Label>
                </ItemTemplate>
                <ItemStyle HorizontalAlign="Right" Wrap="False" />
            </asp:TemplateField>
            
            <asp:TemplateField HeaderText="FOB" SortExpression="fob">
                
                <ItemTemplate>
                    <asp:Label ID="fobLabel" runat="server" Text='<%# Bind("fob") %>'></asp:Label>
                </ItemTemplate>
                <ItemStyle HorizontalAlign="Right" Wrap="False" />
            </asp:TemplateField>
            
             <%-- <asp:BoundField DataField="lvmailto"  HeaderText="lv-mailto" SortExpression="lvmailto" />
            <asp:BoundField DataField="lvmailsubject"  HeaderText="lv-mailsubject" SortExpression="lvmailsubject" />
            <asp:BoundField DataField="lvmailbody"  HeaderText="lv-mailbody" SortExpression="lvmailbody" />--%>  
        </Columns>
        <EmptyDataRowStyle BorderColor="Gray" BorderStyle="Dotted" BorderWidth="0px" Font-Bold="True"
            HorizontalAlign="Center" VerticalAlign="Middle" />
        <RowStyle CssClass="shade" />
        <SelectedRowStyle CssClass="GridSelected" BackColor="Yellow" />
        <HeaderStyle CssClass="headcolor" ForeColor="White" HorizontalAlign="Center"
            VerticalAlign="Middle" Wrap="False" />
        <AlternatingRowStyle CssClass="GridItemOdd" />
    </asp:GridView>
    <asp:FormView ID="FormView1" OnDataBound="FormView1_DataBound" runat="server" DataSourceID="ObjectDataSource2">
        <EditItemTemplate >
        
        <fieldset style="background-color:#EFF3FB;width:940px">
        
       
       <table><tr><td style="width:60%;">
        
        <table >
        
                <tr><td><b>Prt:</b></td>
        <td  style="width: 200px" ><b>
        <%--<asp:TextBox ID="printLabel" runat="server" onBlur="disableallcontrol()" Text='<%# Bind("print") %>'></asp:TextBox>--%>
        <asp:Label ID="printLabel" runat="server" Text='<%# Bind("print") %>'></asp:Label><br /></b></td>
        <td><b>PO#:</b></td>
        <td><b><asp:TextBox MaxLength="15" Width="100px" ID="po_noTextBox" onBlur="showFeature2( 'YES' );" runat="server" Text='<%# Bind("[po-no]") %>'>
            </asp:TextBox><br /></b></td></tr>
        
           <tr><td><b> S/I:</b></td>
           <td><b><%--<asp:TextBox Width="15px" Visible="false" ID="SITextBox" MaxLength=1 runat="server" Text='<%# Bind("SI") %>'>
            </asp:TextBox>--%>
               <asp:DropDownList ID="SITextBox" runat="server" SelectedValue='<%# Bind("SI") %>'>
                   <asp:ListItem Value="B">B  (Both Ship & Invoice)</asp:ListItem>
                   <asp:ListItem Value="I">I  (Invoice Only)</asp:ListItem>
                   <asp:ListItem Value="S">S  (Ship Only)</asp:ListItem>
                   <asp:ListItem Value="T">T  (Transfer)</asp:ListItem>
               </asp:DropDownList></b></td>
           <td><b> Customer Lot#:</b></td>
           <td><b><asp:TextBox MaxLength="15" Width="100px" ID="lot_noTextBox" runat="server" Text='<%# Bind("[lot-no]") %>'>
            </asp:TextBox><br /></b></td></tr> 
          <tr><td><b>ShipTo:</b></td>
          <td><b> <asp:TextBox MaxLength="7" Width="50px" ID="ShipToTextBox" onkeyup="ShipIdlook()" runat="server" Text='<%# Bind("ShipTo") %>'>
            </asp:TextBox><a href="#" onClick="ShipIdlook(); return false" ><asp:Image ID="Image1" runat="server" ImageUrl="images/lookup_icon.gif" /></a><br /></b></td>
          <td><b>Date:</b></td>
          <td><b><asp:TextBox ID="MyDate"  Width="60px"  runat="server" Text='<%# Bind("Vdate","{0:d}") %>' onBlur="showFeature( 'YES' );"  onKeyUp="Datevalidate()" ></asp:TextBox>
          <asp:CompareValidator ID="CompareValidator6" runat="server" ControlToValidate="MyDate" Display="dynamic" Operator="DataTypeCheck" Type="Date" SetFocusOnError="true" ErrorMessage="Invalid Date. Format is MM/DD/YYYY"></asp:CompareValidator> 
          <a href="#" onClick="Datelook(); return false"><asp:Image ID="Image3" runat="server" ImageUrl="images/lookup_icon.gif" /></a><br /></b></td>
          </tr>
           <tr><td><b>S:</b></td>
           <td><b> <asp:Label ID="VstatusTextBox" runat="server" Text='<%# Bind("Vstatus") %>'>
            </asp:Label><br /></b></td>
           <td><b> Ship To Address:</b></td>
           <td><b> <asp:Label ID="addrLabel" runat="server" Text='<%# Bind("addr") %>'></asp:Label><br /></b></td></tr>
            <tr><td><b> Via:</b></td>
            <td><b><asp:TextBox MaxLength="5" Width="40px" ID="ViaTextBox" runat="server" Text='<%# Bind("Via") %>'>
            </asp:TextBox><a href="#" onClick="Carrierlook(); return false" ><asp:Image ID="Image2" runat="server" ImageUrl="images/lookup_icon.gif" /></a><br /></b></td>
            <td><b>City:</b></td>
            <td><b><asp:Label ID="cityLabel" runat="server" Text='<%# Bind("city") %>'></asp:Label></b><br /></td>
            <td><b>State:</b></td>
            <td><b><asp:Label ID="stateLabel" runat="server" Text='<%# Bind("state") %>'></asp:Label><br /></b></td>
            </tr>
            
            <tr><td><b> Sched Qty:</b></td>
            <td><b></b><asp:TextBox MaxLength="8" Width="60px" ID="SQtyTextBox" runat="server" Text='<%# Bind("SQty","{0:##,###,##0}") %>'>
            </asp:TextBox><%--<asp:RegularExpressionValidator ID="vldNumber" ControlToValidate="SQtyTextBox" Display="Dynamic" ErrorMessage="Only Numbers Without Comma's" ValidationExpression="(^([0-9]*|\d*\d{1}?\d*)$)" Runat="server"> 
                          </asp:RegularExpressionValidator>--%><br /></td>
            <td><b>Ext Price:</b></td>
           <td><b><asp:Label ID="priceLabel" runat="server" Text='<%# Bind("price") %>'></asp:Label><br /></b></td>
           <%--<td><b>Sell Price </b></td>
            <td><b><asp:TextBox Width="65px" MaxLength="11" ID="sellPrice" runat="server" Text='<%# Bind("[sell-price]","{0:###,###,##0.00}") %>'> </asp:TextBox><br /></b></td>--%></tr>
                   
           <tr><td><b>Actual Qty:</b></td>
            <td><b><asp:Label ID="qtyLabel" runat="server" Text='<%# Bind("qty") %>'></asp:Label><br /></b></td>
           <%--<td><b>Discount:</b></td>
           <td><b><asp:Label ID="discLabel" runat="server" Text='<%# Bind("disc") %>'></asp:Label><br /></b></td>--%>
           <td><b>Frt Pay:</b></td>
           <td><b><asp:TextBox ID="frtpayLabel" MaxLength="8" Width="60px" onblur="checkfrtpay(this)" runat="server" Text='<%# Bind("frtpay") %>'></asp:TextBox><br /></b></td></tr>
           <tr></tr><td><b>FOB:</b></td>
           <td><b><asp:TextBox ID="fobLabel" MaxLength="1" Width="20px" onblur="checkfob(this)" runat="server" Text='<%# Bind("fob") %>'></asp:TextBox><br /></b></td></tr>
           <br />
           
           </table>
           
           </td>
           <td>
           
            <SPAN ID="vdate" style="display:none">
            <table width="300px">
            <tr><td>
                   <asp:CheckBox ID="CheckBox5" Text="&nbsp;&nbsp; Change Date for All Items with Same Date?" Font-Bold="true" runat="server" /></td></tr>
               <tr><td>
        <asp:CheckBox ID="CheckBox1" runat="server" Text="&nbsp;&nbsp; Change View Order Date?" Font-Bold="True" Width="220px" /></td></tr>
      <%--<tr>  <td><asp:CheckBox ID="CheckBox2" runat="server" Text="&nbsp;&nbsp;  Header Last Ship Date?" Font-Bold="True" Width="220px" /></td></tr>--%>
        <tr><td><asp:CheckBox ID="CheckBox3" runat="server" Text="&nbsp;&nbsp;  Change View Item Date for All Items?" Font-Bold="True" Width="250px" /></td></tr>
        <%--<tr><td><asp:CheckBox ID="CheckBox4" runat="server" Text="&nbsp;&nbsp;  Line Item Last Ship Date?" Font-Bold="True" Width="220px" /></td>--%>
        </table></SPAN>
        
        </td>
        <td style="width:20%;">
        <SPAN ID="po" style="display:none">
           <table width="300px">
          
           
                <tr><td>
                    <asp:RadioButton ID="CheckBox6" GroupName="selectone" Text="&nbsp; Change Item PO# to All Items & All Dates?" Font-Bold="true" runat="server" />
                   <%--<asp:CheckBox ID="CheckBox6" Text="&nbsp;&nbsp; Change Item PO# to All Items & All Dates?" Font-Bold="true" runat="server" />--%></td></tr>
           
           
                  <tr><td>
                  <asp:RadioButton ID="CheckBox7" GroupName="selectone" Text="&nbsp; Change Item PO# for All Items with Same Date?" Font-Bold="true" runat="server" />
                   <%--<asp:CheckBox ID="CheckBox7" Text="&nbsp;&nbsp; Change Item PO# for All Items with Same Date?" Font-Bold="true" runat="server" />--%></td></tr>
           
           
           </table>
           </SPAN>
       
           
           </td></tr></table>
           
           
           
        
            
            <br />
            <input type="hidden"  name="HiddenRowid" value='<%# Server.UrlEncode(Convert.ToString(DataBinder.Eval(Container,"DataItem.vRowid"))) %>' />
            <input type="hidden"  name="ARowid" value='<%# Server.UrlEncode(Convert.ToString(DataBinder.Eval(Container,"DataItem.ARowid"))) %>' />
            <br />
         
            <asp:Button ID="UpdateButton" runat="server"   CssClass="buttonM" onclick="updateRelease"
                Text="Save" >
            </asp:Button>
            <asp:Button ID="UpdateCancelButton" runat="server" CausesValidation="False" CommandName="Cancel" CssClass="buttonM" onclick="Cancel_release"
                Text="Cancel">
            </asp:Button>
            
              
              </fieldset>
                       
        </EditItemTemplate>
        <InsertItemTemplate>
        <table width = 600px class="shade"><tr><td><b>Part:</b></td>
        <td><b><asp:TextBox ID="PartTextBox" runat="server" Text='<%# Bind("Part") %>'>
            </asp:TextBox><br /></b></td>
        <td><b> print:</b></td>
        <td><b><asp:TextBox ID="printTextBox" runat="server" Text='<%# Bind("print") %>'></asp:TextBox><br /></b></td></tr>
           <tr><td><b> ord-no:</b></td>
           <td><b> <asp:TextBox ID="ord_noTextBox" runat="server" Text='<%# Bind("[ord-no]") %>'>
            </asp:TextBox><br /></b></td>
           <td><b>line:</b></td>
           <td><b><asp:TextBox ID="lineTextBox" runat="server" Text='<%# Bind("line") %>'>
            </asp:TextBox><br /></b></td>
           </tr> 
            <tr><td><b> SI:</b></td>
            <td><b><asp:TextBox ID="SITextBox" MaxLength=1 runat="server" Text='<%# Bind("SI") %>'>
            </asp:TextBox><br /></b></td>
            <td><b> ShipTo:</b></td>
            <td><b><asp:TextBox ID="ShipToTextBox" runat="server" Text='<%# Bind("ShipTo") %>'>
            </asp:TextBox><br /></b></td></tr>
           <tr><td><b>  Vstatus:</b></td>
           <td><b><asp:TextBox ID="VstatusTextBox" runat="server" Text='<%# Bind("Vstatus") %>'>
            </asp:TextBox><br /></b></td>
           <td><b> Via:</b></td>
           <td><b><asp:TextBox ID="ViaTextBox" runat="server" Text='<%# Bind("Via") %>'>
            </asp:TextBox><br /></b></td></tr>
            <tr><td><b> qty:</b></td>
            <td><b><asp:TextBox ID="qtyTextBox" runat="server" Text='<%# Bind("qty") %>'>
            </asp:TextBox><br /></b></td>
            <td><b>SQty:</b></td>
            <td><b> <asp:TextBox ID="SQtyTextBox" runat="server" Text='<%# Bind("SQty") %>'>
            </asp:TextBox><br /></b></td></tr>
           <tr><td><b> po-no:</b></td>
           <td><b><asp:TextBox ID="po_noTextBox" runat="server" Text='<%# Bind("[po-no]") %>'>
            </asp:TextBox><br /></b></td>
           <td><b> Vdate:</b></td>
           <td><b> <asp:TextBox ID="VdateTextBox" runat="server" Text='<%# Bind("Vdate") %>'>
            </asp:TextBox><br /></b></td></tr>
            <tr><td><b>addr:</b></td>
            <td><b><asp:TextBox ID="addrTextBox" runat="server" Text='<%# Bind("addr") %>'>
            </asp:TextBox><br /></b></td>
            <td><b> city:</b></td>
            <td><b>
            <asp:TextBox ID="cityTextBox" runat="server" Text='<%# Bind("city") %>'>
            </asp:TextBox><br /></b></td></tr> 
           
           <tr><td><b> disc:</b></td>
           <td><b><asp:TextBox ID="TextBox1" runat="server" Text='<%# Bind("disc") %>'>
            </asp:TextBox><br /></b></td>
           <td><b>state:</b></td>
           <td><b><asp:TextBox ID="stateTextBox" runat="server" Text='<%# Bind("state") %>'>
            </asp:TextBox><br /></b></td></tr>
            
             <tr><td><b>price:</b></td>
           <td><b><asp:TextBox ID="priceTextBox" runat="server" Text='<%# Bind("price") %>'>
            </asp:TextBox><br /></b></td>
            <tr><td><b>frtpay:</b></td>
           <td><b><asp:TextBox ID="frtpayTextBox" runat="server" Text='<%# Bind("frtpay") %>'>
            </asp:TextBox><br /></b></td>
            <tr><td><b>fob:</b></td>
           <td><b><asp:TextBox ID="fobTextBox" runat="server" Text='<%# Bind("fob") %>'>
            </asp:TextBox><br /></b></td>
           <td><b>lot-no:</b></td>
           <td><b><asp:TextBox ID="lot_noTextBox" runat="server" Text='<%# Bind("[lot-no]") %>'>
            </asp:TextBox><br /></b></td></tr>
                        
            </table>
            <asp:Button ID="InsertButton" runat="server" CausesValidation="True" CommandName="Insert" CssClass="buttonM" 
                Text="Save">
            </asp:Button>
            <asp:Button ID="InsertCancelButton" runat="server" CausesValidation="False" CommandName="Cancel" CssClass="buttonM" 
                Text="Cancel">
            </asp:Button>
        </InsertItemTemplate>
        <ItemTemplate>
            
            <asp:Button ID="EditButton" runat="server" Visible="false" CausesValidation="False" CommandName="Edit" CssClass="buttonM" OnClick="updaterel"
                    Text="Update">
                </asp:Button>
                <asp:Button ID="DeleteButton" runat="server" Visible="false" CssClass="buttonM" OnClick="Delete_release"
                    Text="Delete">
                </asp:Button>
                <asp:Button ID="NewButton" runat="server" Visible="false" CausesValidation="False" CommandName="Edit" CssClass="buttonM" 
                    Text="Add" OnClick="AddRelease">
                </asp:Button>
                <asp:Button ID="ReleaseButton" runat="server" Visible="false"  CssClass="buttonM" OnClick = "Releaseitem"
                    Text="Release" >
                </asp:Button>
                <asp:Button ID="BolInvButton" runat="server" Visible="false"  CssClass="buttonM" OnClientClick="return confirm('Are you sure you want to create the BOL')" OnClick = "BolInv_Click"
                    Text="BOL/Invoice" >
                </asp:Button>
                
                <%--<asp:Button ID="BolButton" runat="server"  CssClass="buttonM" 
                    Text="BOL">
                </asp:Button>
                <asp:Button ID="jobButton" runat="server"  CssClass="buttonM" 
                    Text="Add Job">
                </asp:Button>
                <asp:Button ID="unpostButton" runat="server"  CssClass="buttonM" OnClick="Unpost_release"
                    Text="Unpost Actual">
                </asp:Button>--%>
                
                
        </ItemTemplate>
    </asp:FormView>
    &nbsp;
    
    <br />
    <asp:ObjectDataSource ID="ObjectDataSource1" runat="server" OldValuesParameterFormatString="original_{0}"
        SelectMethod="SelectRel" TypeName="Order">
        <SelectParameters>
            <asp:Parameter Name="prmUser" Type="String" />
            <asp:SessionParameter Name="prmOrderNum" SessionField="releases" Type="String" />
            <asp:SessionParameter Name="prmItemNum" SessionField="line" Type="String" />
            <asp:Parameter Name="prmAction" Type="String" /> 
            <asp:Parameter Name="vRowid" Type="Int64" />
            <asp:Parameter Name="Asi" Type="String" />
            <asp:Parameter Name="AShipTo" Type="String" />
            <asp:Parameter Name="AVia" Type="String" />
            <asp:Parameter Name="ASqty" Type="String" />
            <asp:Parameter Name="ApoNo" Type="String" />
            <asp:Parameter Name="AlotNo" Type="String" />
            <asp:Parameter Name="ADate" Type="DateTime" />
            <asp:Parameter Name="sellPrice" Type="String" />
            <asp:Parameter Name="HeaderDueDate" Type="String" />
            <asp:Parameter Name="HeaderLastShipDate" Type="String" />
            <asp:Parameter Name="LineItemDueDate" Type="String" />
            <asp:Parameter Name="LineItemLastShipDate" Type="String" />
            <asp:Parameter Name="RelAllDt" Type="String" />
            <asp:Parameter Name="AllPo" Type="String" />
            <asp:Parameter Name="ShipAllDate" Type="String" />
            <asp:Parameter Name="Afrtpay" Type="String" />
            <asp:Parameter Name="Afob" Type="String" />
        </SelectParameters>
    </asp:ObjectDataSource>
 <asp:ObjectDataSource ID="ObjectDataSource2" runat="server" OldValuesParameterFormatString="original_{0}"
        SelectMethod="SelectRel" TypeName="Order">
        <SelectParameters>
            <asp:Parameter Name="prmUser" Type="String" />
            <asp:SessionParameter Name="prmOrderNum" SessionField="releases" Type="String" />
            <asp:SessionParameter DefaultValue="" Name="prmItemNum" SessionField="line" Type="String" />
            <asp:Parameter Name="prmAction" Type="String" DefaultValue="Select" /> 
            <asp:Parameter Name="vRowid" Type="Int64" />
            <asp:Parameter Name="Asi" Type="String" /> 
            <asp:Parameter Name="AShipTo" Type="String" /> 
            <asp:Parameter Name="AVia" Type="String" /> 
            <asp:Parameter Name="ASqty" Type="String" /> 
            <asp:Parameter Name="ApoNo" Type="String" /> 
            <asp:Parameter Name="AlotNo" Type="String" /> 
            <asp:Parameter Name="ADate" Type= "DateTime" /> 
            <asp:Parameter Name="sellPrice" Type="String" /> 
            <asp:Parameter Name="HeaderDueDate" Type="String" />
            <asp:Parameter Name="HeaderLastShipDate" Type="String" />
            <asp:Parameter Name="LineItemDueDate" Type="String" />
            <asp:Parameter Name="LineItemLastShipDate" Type="String" />
            <asp:Parameter Name="RelAllDt" Type="String" />
            <asp:Parameter Name="AllPo" Type="String" />
            <asp:Parameter Name="ShipAllDate" Type="String" />
            <asp:Parameter Name="Afrtpay" Type="String" />
            <asp:Parameter Name="Afob" Type="String" />
        </SelectParameters>
    </asp:ObjectDataSource>
 
</div>
</asp:Content>

