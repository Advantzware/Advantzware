<%@ Page Language="C#" MasterPageFile="~/MasterPage3.master" EnableEventValidation="false" Inherits="itemhistoryfile" Title="Item History" Codebehind="itemhistory.aspx.cs" %>
<asp:Content ID="Content1" ContentPlaceHolderID="ContentPlaceHolder1" Runat="Server">
    <LINK href="include/style.css" type="text/css" rel="stylesheet"/>
    <LINK href="order_inquiry.css" rel="stylesheet" type="text/css" />
    <LINK href="include/style.css" type="text/css" rel="stylesheet"/>
    <LINK REL="stylesheet" TYPE="text/css" HREF="include/CalendarControl.css" >
    
    <script language = "JavaScript" src="include/CalendarControl.js"></script>
    <script language="javascript" src="include/date.js"></script>
    <script language="javascript" src="include/event.js"></script>
    <script language="javascript" src="include/insert.js"></script>
     <script type="text/javascript">

         function locationlook() {
             var NewWindow = window.open("location_lookup.aspx", "LocationLookUpWindow", "width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
         }
         function LocationLookUp(ReturnObj1, ReturnObj2) {
             document.forms[0].ctl00$ContentPlaceHolder1$txt_ware.value = ReturnObj1;
             document.forms[0].ctl00$ContentPlaceHolder1$txt_ware.focus();
         }
function job1look(){ 
  var NewWindow = window.open("job1_lookup.aspx","Job1LookupWindow","width=450,height=300,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}
function Job1Lookup(ReturnObj1){ 
  document.forms[0].ctl00$ContentPlaceHolder1$txt_job1.value = ReturnObj1;
}

function Datelook(){ 
  var NewWindow = window.open("history_datelookup.aspx","DateLookupWindow","width=320,height=260,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
  }
function Datelookup(obj)
{
  document.forms[0].ctl00$ContentPlaceHolder1$txt_date.value=obj;
}

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
alert("Invalid Date\nPlease Re-Enter");
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
alert("Invalid Date\nPlease Re-Enter");
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
var mMonth = vDateName.value.substr(2,2);
var mDay = vDateName.value.substr(0,2);
var mYear = vDateName.value.substr(4,4)
vDateName.value = mDay+strSeperator+mMonth+strSeperator+mYear;
}

var vDateTypeTemp = vDateType;
vDateType = 1;
var vDateValueCheck = mMonth+strSeperator+mDay+strSeperator+mYear;
if (!dateValid(vDateValueCheck)) {
alert("Invalid Date\nPlease Re-Enter");
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
if (((vDateValue.length < 8 && dateCheck) || (vDateValue.length == 9 && dateCheck)) && (vDateValue.length >=1)) {
alert("Invalid Date\nPlease Re-Enter");
vDateName.value = "";
vDateName.focus();
vDateName.select();
return false;
         }
      }
   }
}
else {

if (((vDateValue.length < 8 && dateCheck) || (vDateValue.length == 9 && dateCheck)) && (vDateValue.length >=1)) {
alert("Invalid Date\nPlease Re-Enter");
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
alert("Invalid Date\nPlease Re-Enter");
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
alert("Invalid Date\nPlease Re-Enter");
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
 
alert("Invalid Date\nPlease Re-Enter");
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
</script>
    <br />
    <asp:LinkButton OnClick="LinkButton_Click" ID="LinkButton1" runat="server">List Order</asp:LinkButton>
    <asp:LinkButton OnClick="LinkButton2_Click" ID="LinkButton2" runat="server">List Order</asp:LinkButton>
     <asp:LinkButton OnClick="LinkButton3_Click" ID="LinkButton3" runat="server">List Order</asp:LinkButton>
    <br />
   
        <div>
        <asp:Panel ID="searchpanel" runat="server" DefaultButton="btn_go">
        <fieldset style="background-color:#EFF3FB; width: 823px;">
        <table>
        <tr>
            <td style="width: 42px">
            <asp:Button ID="btn_go" runat="server" Text="GO" Width="65px" OnClick="btn_go_Click" style="position: relative" /></td>
        <td><b>Fg Item#:</b></td>
        <td style="width: 75px"><b>
            <asp:TextBox ID="txt_fgitem" ReadOnly =true runat="server"  Width="110px"></asp:TextBox></b></td>
        <td><b>Job#:</b></td>
        <td>
            <b><asp:TextBox ID="txt_job1" runat="server" Width="53px"></asp:TextBox><a href="#" onClick="job1look(); return false"><asp:Image ID="Job1Lookup" runat="server" ImageUrl="images/lookup_icon.gif" /></a></b></td>
        <td style="width: 2px"><b>
        </b><asp:TextBox ID="txt_job2" runat="server" Width="40px"></asp:TextBox></td>
        <td><b>Trans Code:</b></td>
        <%--<td style="width: 66px"><b id="B1" runat="server">&nbsp;<asp:TextBox ID="txt_trans" runat="server" Width="56px"></asp:TextBox></b></td>--%>
        <%--<td style="width: 66px"><b id="B1" runat="server">&nbsp;<asp:TextBox ID="RCodeTextBox" runat="server" Text='<%# Bind("RCode") %>'>--%>
                    
                  <td><b>   <asp:DropDownList ID="RCodeTextBox"  runat="server" SelectedValue='<%# Bind("RCode") %>'>
                    <asp:ListItem Value="B" Selected="True"> ANY </asp:ListItem>
                   <asp:ListItem Value="R" >R  (Receipt)</asp:ListItem>
                   <asp:ListItem Value="S">S  (Shipment)</asp:ListItem>
                   <asp:ListItem Value="T">T  (Transfer)</asp:ListItem>
                   <asp:ListItem Value="A">A  (Adjustment)</asp:ListItem>
                   <asp:ListItem Value="C">C  (Cycle Count)</asp:ListItem>
                   <asp:ListItem Value="E">E  (Return)</asp:ListItem>
               </asp:DropDownList></b></td>
        <td><b>From Date:</b></td>
        <td><b><asp:TextBox ID="txt_date"  Width="60px"  runat="server" Text='<%# Bind("Vdate","{0:d}") %>' onfocus="javascript:preEnter( this, 'yes' );" onblur="javascript:preLeave( this, 'date', '99/99/9999' );">
            </asp:TextBox><a href="#" onClick="showCalendarControl(ctl00_ContentPlaceHolder1_txt_date); return false"><asp:Image ID="Image3" runat="server" ImageUrl="images/lookup_icon.gif" /></a><br /></b></td>

            <td nowrap>Rows/Page<br>
                  
                      <asp:FormView ID="FormView2" runat="server" DataSourceID="ObjectDataSource3">
                          <EditItemTemplate>
                              aLine:
                              <asp:TextBox ID="aLineTextBox" runat="server" Text='<%# Bind("aLine") %>'>
                              </asp:TextBox><br />
                              <asp:LinkButton ID="UpdateButton" runat="server" CausesValidation="True" CommandName="Update"
                                  Text="Update">
                              </asp:LinkButton>
                              <asp:LinkButton ID="UpdateCancelButton" runat="server" CausesValidation="False" CommandName="Cancel"
                                  Text="Cancel">
                              </asp:LinkButton>
                          </EditItemTemplate>
                          <InsertItemTemplate>
                              aLine:
                              <asp:TextBox ID="aLineTextBox" runat="server" Text='<%# Bind("aLine") %>'>
                              </asp:TextBox><br />
                              <asp:LinkButton ID="InsertButton" runat="server" CausesValidation="True" CommandName="Insert"
                                  Text="Insert">
                              </asp:LinkButton>
                              <asp:LinkButton ID="InsertCancelButton" runat="server" CausesValidation="False" CommandName="Cancel"
                                  Text="Cancel">
                              </asp:LinkButton>
                          </InsertItemTemplate>
                          <ItemTemplate>
                             
                              <asp:TextBox ID="aLineLabel" runat="server" Width="40px" OnTextChanged="ddl_display_TextChanged" Text='<%# Bind("aLine") %>'></asp:TextBox>
                              <asp:CompareValidator ID="CompareValidator1" runat="server" ErrorMessage="Invalid Input" ControlToValidate="aLineLabel" Display="dynamic" SetFocusOnError="true" Operator="datatypecheck" Type="integer"></asp:CompareValidator>
                              <%--<asp:Label ID="aLineLabel" runat="server" Text='<%# Bind("aLine") %>'></asp:Label>--%>
                          </ItemTemplate>
                      </asp:FormView>
                      <asp:ObjectDataSource ID="ObjectDataSource3" runat="server" OldValuesParameterFormatString="original_{0}"
                          SelectMethod="SelectRows" TypeName="Order">
                          <SelectParameters>
                              <asp:SessionParameter Name="prmUser" SessionField="Rowuser" Type="String" />
                              <asp:SessionParameter Name="vLine" Type="Int32" SessionField="gridsize" />
                          </SelectParameters>
                      </asp:ObjectDataSource>
                  
                 </td>
        </tr>
        </table>
        <table class="shade" style="width: 750px">
        
        <tr>
            <td style="width: 58px">
            <asp:Button ID="btn_showall" runat="server" Text="Show All" OnClick="btn_showall_Click" style="position: relative; left: 0px; top: 0px;" /></td>
        <td style="width: 1059px"><b>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Tag#:</b> <asp:TextBox ID="txt_tag" runat="server" Width="72px"></asp:TextBox>&nbsp
            <asp:Button ID="btn_fgitem" runat="server" Text="<== Prefix FG Item#" OnClick="btn_fgitem_Click" />&nbsp;
            <b>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Warehouse#:</b> <asp:TextBox ID="txt_ware" runat="server" Width="72px"></asp:TextBox>
            <a href="#" tabindex="1" onClick ="locationlook(); return false"><asp:Image ID="Image10" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
            <b>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; PO#:</b> <asp:TextBox ID="txt_pono" runat="server" Width="72px"></asp:TextBox>&nbsp
        </td> 
            
        </tr>
            <tr>
                <td style="width: 58px">
                </td>
                <td style="width: 1059px">
                </td>
            </tr>
        <tr>
            <td colspan="2" style="height: 6px">
            <asp:FormView ID="FormView1" runat="server" DataSourceID="ObjectDataSource2">
                
                <ItemTemplate>
                    <table>
                    <tr>
                    <td style="width: 74px; height: 18px;">
                        <strong>Item Name:</strong></td>
                    <td style="width: 125px; height: 18px;" align="left"><b><asp:Label ID="i_nameLabel" runat="server" Text='<%# Bind("[i-name]") %>' BackColor="Turquoise" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" Width="120px"></asp:Label></b></td>
                    <td style="width: 84px; height: 18px;" align="left"><b>Qty On Hand:</b></td>
                    <td style="width: 81px; height: 18px;" align="left"><b><asp:Label ID="q_onhLabel" runat="server" Text='<%# Bind("[q-onh]") %>' BackColor="Turquoise" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" Width="80px"></asp:Label></b></td>
                    <td style="width: 60px; height: 18px;" align="left"><b>Qty Avail:</b></td>
                    <td style="width: 199px; height: 18px;" align="left"><b><asp:Label ID="q_availLabel" runat="server" Text='<%# Bind("[q-avail]") %>' BackColor="Turquoise" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" Width="80px"></asp:Label></b></td>
                    </tr>
                    </table>
                    
                
                    
                </ItemTemplate>
            </asp:FormView>
            </td>
        </tr>
        </table>
        </fieldset></asp:Panel>
        </div>
        <br />
        <div>
            <asp:GridView ID="GridView1" runat="server" AutoGenerateColumns="true" OnRowCreated="GridView1_RowCreated" AllowPaging="true" OnPageIndexChanging="GridView1_PageIndexChanging" 
            AllowSorting="True" OnSorting="GridView1_Sorting"  OnRowDataBound="GridView1_RowDataBound"
                BorderStyle="Dotted" CssClass="Grid" EmptyDataText="No Record Found" Width="85%" >
                <EmptyDataRowStyle BorderColor="Gray" BorderStyle="Dotted" BorderWidth="0px" Font-Bold="True"
                    HorizontalAlign="Center" VerticalAlign="Middle" />
                <RowStyle CssClass="shade" />
                <SelectedRowStyle CssClass="GridSelected" BackColor="Yellow" />
                <HeaderStyle ForeColor="White" CssClass="headcolor" HorizontalAlign="Center"
                    VerticalAlign="Middle" Wrap="False" />
                <AlternatingRowStyle CssClass="GridItemOdd" />
                <Columns>
                    <asp:TemplateField HeaderText="TR Date"  Visible="False">
                        
                        <ItemStyle Wrap="False" />
                        <ItemTemplate>
                            <asp:Label ID="Label1" runat="server" Text='<%# Bind("[TR Date]") %>' ></asp:Label>
                        </ItemTemplate>
                    </asp:TemplateField>
                  
                </Columns>
            </asp:GridView>
            <asp:ObjectDataSource ID="ObjectDataSource1" runat="server" OldValuesParameterFormatString="original_{0}"
                SelectMethod="SelectHistory" TypeName="itemhistory">
                <SelectParameters>
                    <asp:Parameter Name="prmUser" Type="String" />
                    <asp:Parameter Name="prmAction" Type="String" DefaultValue="Select" />
                     <asp:Parameter Name="prmOrderNum"  Type="String" />
                    <asp:SessionParameter Name="prmItemNum" SessionField="item_list_item" Type="String" />
                    <asp:Parameter Name="prmJob" Type="String" />
                <asp:Parameter Name="prmJob2" Type="String" />
                <asp:Parameter Name="prmCode" Type="String" />
                <asp:Parameter Name="prmDate" Type="String" />
                <asp:Parameter Name="prmTag" Type="String" />     
                 <asp:Parameter Name="prmWareHouse" Type="String" />
                <asp:Parameter Name="prmPoNo" Type="String" />             
                </SelectParameters>
            </asp:ObjectDataSource>
            <asp:ObjectDataSource ID="ObjectDataSource2" runat="server" OldValuesParameterFormatString="original_{0}"
                SelectMethod="SelectHistory" TypeName="itemhistory">
                <SelectParameters>
                    <asp:Parameter Name="prmUser" Type="String" />
                    <asp:Parameter DefaultValue="Select" Name="prmAction" Type="String" />
                    <asp:Parameter  Name="prmOrderNum"  Type="String" />
                    <asp:SessionParameter Name="prmItemNum" SessionField="item_list_item" Type="String" />
                    <asp:Parameter Name="prmJob" Type="String" />
                <asp:Parameter Name="prmJob2" Type="String" />
                <asp:Parameter Name="prmCode" Type="String" />
                <asp:Parameter Name="prmDate" Type="String" />
                <asp:Parameter Name="prmTag" Type="String" />
                <asp:Parameter Name="prmWareHouse" Type="String" />
                <asp:Parameter Name="prmPoNo" Type="String" />   
                </SelectParameters>
            </asp:ObjectDataSource>
            <br />
        </div></asp:Content>

