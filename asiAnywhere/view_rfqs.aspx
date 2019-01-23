<%@ Page Language="C#" MasterPageFile="MasterPage5.master" Debug="false" AutoEventWireup="true" Inherits="viewrfqs" Title="View Rfq" Codebehind="view_rfqs.aspx.cs" %>

<asp:Content ID="Content1" ContentPlaceHolderID="ContentPlaceHolder1" runat="server">
<script language="javascript" type="text/javascript" for="window" event="onunload">
<!--
return window_onunload()
// -->
</script>

<script language="javascript" src="include/CalendarControl.js"></script>
<script language="javascript" src="include/date.js"></script>
<script language="javascript" src="include/event.js"></script>
<script language="javascript" src="include/insert.js"></script>
<asp:ScriptManager ID="ScriptManager1" runat="server">
            </asp:ScriptManager>

<script type="text/javascript">

//function duedateval()
//{
//    var duedate=document.getElementById("ctl00_ContentPlaceHolder1_FormView1_due_dateTextBox").value;
//    
//    if(duedate.length>1 && duedate.length<3 && duedate.indexOf('/')!=1)
//    {
//        document.getElementById("ctl00_ContentPlaceHolder1_FormView1_due_dateTextBox").value = duedate + "/";
//    }
//    if(duedate.length>4 && duedate.length<6 && duedate.indexOf('/')!=3)
//    {
//        document.getElementById("ctl00_ContentPlaceHolder1_FormView1_due_dateTextBox").value = duedate + "/";
//    }
//}

//function reqdateval()
//{
//    var duedate=document.getElementById("ctl00_ContentPlaceHolder1_FormView1_req_dateTextBox").value;
//    
//    if(duedate.length>1 && duedate.length<3 && duedate.indexOf('/')!=1)
//    {
//        document.getElementById("ctl00_ContentPlaceHolder1_FormView1_req_dateTextBox").value = duedate + "/";
//    }
//    if(duedate.length>4 && duedate.length<6 && duedate.indexOf('/')!=3)
//    {
//        document.getElementById("ctl00_ContentPlaceHolder1_FormView1_req_dateTextBox").value = duedate + "/";
//    }
//}
    var check = "";
    function customerlook(obj) {
        check = obj;
  var NewWindow = window.open("customer_lookup.aspx","CustomerLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}

function CustomerLookup(ReturnObj1,ReturnObj2,ReturnObj3,ReturnObj4,ReturnObj5,ReturnObj6,ReturnObj7,ReturnObj8,ReturnObj9 ,ReturnObj10,ReturnObj11){ 
  document.forms[0].ctl00$ContentPlaceHolder1$FormView1$cust_noTextBox.value = ReturnObj1;
  document.forms[0].ctl00$ContentPlaceHolder1$FormView1$ship_nameTextBox.value = ReturnObj2;
  //document.forms[0].ctl00$ContentPlaceHolder1$FormView1$fob_codeTextBox.value = ReturnObj3;
  document.forms[0].ctl00$ContentPlaceHolder1$FormView1$smanNameTextBox.value = ReturnObj4;
  document.forms[0].ctl00$ContentPlaceHolder1$FormView1$ship_cityTextBox.value = ReturnObj5;
  document.forms[0].ctl00$ContentPlaceHolder1$FormView1$ship_stateTextBox.value = ReturnObj6;
  document.forms[0].ctl00$ContentPlaceHolder1$FormView1$ship_zipTextBox.value = ReturnObj7;
  document.forms[0].ctl00$ContentPlaceHolder1$FormView1$smanTextBox.value = ReturnObj8;
  document.forms[0].ctl00$ContentPlaceHolder1$FormView1$commTextBox.value = ReturnObj9;
  document.forms[0].ctl00$ContentPlaceHolder1$HiddenFieldComm.value = ReturnObj9;
  document.forms[0].ctl00$ContentPlaceHolder1$FormView1$shipAddrTextBox.value = ReturnObj10;
  document.forms[0].ctl00$ContentPlaceHolder1$FormView1$shipAddr2TextBox.value = ReturnObj11;
  //document.forms[0].ctl00$ContentPlaceHolder1$FormView1$fob_codeTextBox.value = ReturnObj12;
  
  if (check == "2") {
      if (ReturnObj3 == "DEST") {
          document.forms[0].ctl00_ContentPlaceHolder1_FormView1_RD5.checked = true;
      }
      else {
          document.forms[0].ctl00_ContentPlaceHolder1_FormView1_RD6.checked = true;
      }
  }
  else {
      /*if (ReturnObj3 == "DEST") sss{
          document.forms[0].ctl00_ContentPlaceHolder1_FormView1_RadioButtonList1.SelectedValue = "D";
      }
      else {
          document.forms[0].ctl00_ContentPlaceHolder1_FormView1_RadioButtonList1.SelectedValue = "O";
      }*/
  }
document.forms[0].ctl00$ContentPlaceHolder1$FormView1$cust_noTextBox.onchange();
  
}



function customer2look(){ 
  var NewWindow = window.open("customer2_lookup.aspx","CustomerLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}

function customer2look(){ 
  var NewWindow = window.open("customer2_lookup.aspx","CustomerLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}

function Customer2Lookup(ReturnObj1,ReturnObj2,ReturnObj3,ReturnObj4,ReturnObj5,ReturnObj6,ReturnObj7,ReturnObj8,ReturnObj9 ,ReturnObj10)
{ 
if(document.forms[0].ctl00$ContentPlaceHolder1$FormView2$cust_noTextBox)
{
  document.forms[0].ctl00$ContentPlaceHolder1$FormView2$cust_noTextBox.value = ReturnObj1;
} 
else
{
document.forms[0].ctl00$ContentPlaceHolder1$FormView1$cust_noTextBox.value = ReturnObj1;
}
if(document.forms[0].ctl00$ContentPlaceHolder1$FormView2$ship_nameTextBox)
{
 document.forms[0].ctl00$ContentPlaceHolder1$FormView2$ship_nameTextBox.value = ReturnObj2;
} 
else
{
 document.forms[0].ctl00$ContentPlaceHolder1$FormView1$ship_nameTextBox.value = ReturnObj2;
}
 //document.forms[0].ctl00$ContentPlaceHolder1$FormView1$fob_codeTextBox.value = ReturnObj3;
if(document.forms[0].ctl00$ContentPlaceHolder1$FormView2$smanNameTextBox)
{  
document.forms[0].ctl00$ContentPlaceHolder1$FormView2$smanNameTextBox.value = ReturnObj4;
}
else
{
document.forms[0].ctl00$ContentPlaceHolder1$FormView1$smanNameTextBox.value = ReturnObj4;
}
if(document.forms[0].ctl00$ContentPlaceHolder1$FormView2$ship_cityTextBox) 
{ 
document.forms[0].ctl00$ContentPlaceHolder1$FormView2$ship_cityTextBox.value = ReturnObj5;
}
else
{
document.forms[0].ctl00$ContentPlaceHolder1$FormView1$ship_cityTextBox.value = ReturnObj5;
}

if(document.forms[0].ctl00$ContentPlaceHolder1$FormView2$ship_stateTextBox)
{
  document.forms[0].ctl00$ContentPlaceHolder1$FormView2$ship_stateTextBox.value = ReturnObj6;
}
else
{
document.forms[0].ctl00$ContentPlaceHolder1$FormView1$ship_stateTextBox.value = ReturnObj6;
}

if(document.forms[0].ctl00$ContentPlaceHolder1$FormView2$ship_zipTextBox)
{
  document.forms[0].ctl00$ContentPlaceHolder1$FormView2$ship_zipTextBox.value = ReturnObj7;
}
else
{
document.forms[0].ctl00$ContentPlaceHolder1$FormView1$ship_zipTextBox.value = ReturnObj7;
}

if(document.forms[0].ctl00$ContentPlaceHolder1$FormView2$smanTextBox)
{
  document.forms[0].ctl00$ContentPlaceHolder1$FormView2$smanTextBox.value = ReturnObj8;
}
else
{
document.forms[0].ctl00$ContentPlaceHolder1$FormView1$smanTextBox.value = ReturnObj8;
}
  
if(document.forms[0].ctl00$ContentPlaceHolder1$FormView2$shipAddrTextBox)
{
  document.forms[0].ctl00$ContentPlaceHolder1$FormView2$shipAddrTextBox.value = ReturnObj9;
}
else
{
document.forms[0].ctl00$ContentPlaceHolder1$FormView1$shipAddrTextBox.value = ReturnObj9;
}

if(document.forms[0].ctl00$ContentPlaceHolder1$FormView2$shipAddr2TextBox)
{
  document.forms[0].ctl00$ContentPlaceHolder1$FormView2$shipAddr2TextBox.value = ReturnObj10;
}
else
{
document.forms[0].ctl00$ContentPlaceHolder1$FormView1$shipAddr2TextBox.value = ReturnObj10;
}

   
   if(ReturnObj3=="DEST")
  {
  document.forms[0].ctl00_ContentPlaceHolder1_FormView1_RD5.checked=true;
  }
  else
  {
   document.forms[0].ctl00_ContentPlaceHolder1_FormView1_RD6.checked=true;
  }
}


function customer3look(){ 
  var NewWindow = window.open("customer3_lookup.aspx","CustomerLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}

function Customer3Lookup(ReturnObj1,ReturnObj2,ReturnObj3,ReturnObj4,ReturnObj5,ReturnObj6,ReturnObj7,ReturnObj8,ReturnObj9 ,ReturnObj10)
{ 
  document.forms[0].ctl00$ContentPlaceHolder1$FormView2$cust_noTextBox.value = ReturnObj1;
  document.forms[0].ctl00$ContentPlaceHolder1$FormView2$ship_nameTextBox.value = ReturnObj2;
  //document.forms[0].ctl00$ContentPlaceHolder1$HiddenField1.value = ReturnObj3;
  document.forms[0].ctl00$ContentPlaceHolder1$FormView2$smanNameTextBox.value = ReturnObj4;
  document.forms[0].ctl00$ContentPlaceHolder1$FormView2$ship_cityTextBox.value = ReturnObj5;
  document.forms[0].ctl00$ContentPlaceHolder1$FormView2$ship_stateTextBox.value = ReturnObj6;
  document.forms[0].ctl00$ContentPlaceHolder1$FormView2$ship_zipTextBox.value = ReturnObj7;
  document.forms[0].ctl00$ContentPlaceHolder1$FormView2$smanTextBox.value = ReturnObj8;  
  document.forms[0].ctl00$ContentPlaceHolder1$FormView2$shipAddrTextBox.value = ReturnObj9;
  document.forms[0].ctl00$ContentPlaceHolder1$FormView2$shipAddr2TextBox.value = ReturnObj10;
  document.forms[0].ctl00$ContentPlaceHolder1$FormView2$cust_noTextBox.onchange();
   
}


function smanlook(){ 
  var NewWindow = window.open("sman_lookup.aspx","SalesmanLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}

function SalesManLookup(ReturnObj1, ReturnObj2, ReturnObj3){ 
document.forms[0].ctl00$ContentPlaceHolder1$FormView1$smanTextBox.value = ReturnObj1;
document.forms[0].ctl00$ContentPlaceHolder1$FormView1$smanNameTextBox.value = ReturnObj2;
if (document.forms[0].ctl00$ContentPlaceHolder1$FormView1$commTextBox) 
{
    document.forms[0].ctl00$ContentPlaceHolder1$FormView1$commTextBox.value = ReturnObj3;
    document.forms[0].ctl00$ContentPlaceHolder1$HiddenFieldComm.value = ReturnObj3;
}
document.getElementById("ctl00_ContentPlaceHolder1_FormView1_smanTextBox").focus();
}

function smanlook2(){ 
  var NewWindow = window.open("sman_lookup2.aspx","SalesmanLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}

function SalesManLookup2(ReturnObj1, ReturnObj2, ReturnObj3){ 
document.forms[0].ctl00$ContentPlaceHolder1$FormView2$smanTextBox.value = ReturnObj1;
document.forms[0].ctl00$ContentPlaceHolder1$FormView2$smanNameTextBox.value = ReturnObj2;
  
}

function Datelook(){ 
  var NewWindow = window.open("date_lookup.aspx","DateLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
  }
function Datelookup(obj)
{
  document.forms[0].ctl00_ContentPlaceHolder1_FormView1_req_dateTextBox.value=obj;
}

function Datelook2(){ 
  var NewWindow = window.open("date_lookup2.aspx","DateLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
  }
function Datelookup2(obj)
{
  document.forms[0].ctl00_ContentPlaceHolder1_FormView1_due_dateTextBox.value=obj;
}

function setestdate() {

    var da = document.getElementById("ctl00_ContentPlaceHolder1_FormView1_req_dateTextBox");
    da.focus();

}

function setestdate2() {

    var da = document.getElementById("ctl00_ContentPlaceHolder1_FormView1_due_dateTextBox");
    da.focus();

}

function Datelook4(){ 
  var NewWindow = window.open("date_lookup4.aspx","DateLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
  }
function Datelookup4(obj)
{
  document.forms[0].ctl00_ContentPlaceHolder1_FormView2_req_dateTextBox.value=obj;
}

function Datelook5(){ 
  var NewWindow = window.open("date_lookup5.aspx","DateLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
  }
function Datelookup5(obj)
{
  document.forms[0].ctl00_ContentPlaceHolder1_FormView2_due_dateTextBox.value=obj;
}

function Datelook1()
{
  document.forms[0].ctl00_ContentPlaceHolder1_FormView1_req_dateTextBox.value="";
  Datelook();
}
function Datelook3()
{
  document.forms[0].ctl00_ContentPlaceHolder1_FormView1_due_dateTextBox.value="";
  Datelook2();
}

function Datelook6()
{
  document.forms[0].ctl00_ContentPlaceHolder1_FormView2_req_dateTextBox.value="";
  Datelook4();
}
function Datelook7()
{
  document.forms[0].ctl00_ContentPlaceHolder1_FormView2_due_dateTextBox.value="";
  Datelook5();
}







</script>
<div>
    <asp:Button ID="newButton" runat="server" CssClass="button" OnClick="newbutton_Click" Text="Add" />
    <asp:HiddenField ID="HiddenFieldComm" runat="server" />
    <asp:HiddenField ID="HiddenFieldsman" runat="server" />
    <asp:HiddenField ID="HiddenFieldsname" runat="server" />                
      
    
    <asp:ObjectDataSource ID="ObjectDataSource2" runat="server" OldValuesParameterFormatString="original_{0}"
        SelectMethod="ViewRfq" TypeName="rfqs">
        <SelectParameters>
        <asp:Parameter Name="prmUser" Type="String" />
        <asp:Parameter Name="prmAction" DefaultValue="Select" Type="String" />
        <asp:Parameter Name="prmExt" Type="string" />
            <asp:SessionParameter Name="prmRfqNo" SessionField="Rfqview" Type="Int32" />
            <asp:Parameter Name="prmReqdate" Type="DateTime" />
            <asp:Parameter Name="prmDuedate" Type="DateTime" />
            <asp:Parameter Name="prmCustno" Type="String" />
            <asp:Parameter Name="prmShipname" Type="String" />
            <asp:Parameter Name="prmShipAddr" Type="String" />
            <asp:Parameter Name="prmShipAddr2" Type="String" />
            <asp:Parameter Name="prmShipcity" Type="String" />
            <asp:Parameter Name="prmShipstate" Type="String" />
            <asp:Parameter Name="prmShipzip" Type="String" />
            <asp:Parameter Name="prmSman" Type="String" />
            <asp:Parameter Name="prmSmanName" Type="String" />
            <asp:Parameter Name="prmComm" Type="Decimal" />
            <asp:Parameter Name="prmFobcode" Type="String" />
            <asp:Parameter Name="prmChgmethod" Type="String" />
            
            <asp:Parameter Name="prmWhmonth" Type="Int32" />
            <asp:Parameter Name="prmInst" Type="String" />
            <asp:Parameter Name="VRowid" Type="Int64" />
            
        </SelectParameters>
    </asp:ObjectDataSource>

    <asp:FormView ID="FormView1" runat="server" DataSourceID="ObjectDataSource1" OnDataBound="FormView1_DataBound" EmptyDataText="No Record Found" OnUnload="FormView1_Unload" OnPreRender="FormView1_PreRender">
        <ItemTemplate>
         <asp:Panel ID="Edit_Panel" runat="server" DefaultButton="UpdatButton">
        <fieldset style="background-color:#EFF3FB;">
        <table>
        <tr>
        <td align="right" style="padding-right:5px;"><b>RFQ#:</b></td>
        <td style="width: 178px"><b><asp:Label ID="Label1" BackColor="Turquoise" Width="170px" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" runat="server" Text='<%# Bind("rfq_no") %>'></asp:Label></b></td>
        <td align="right" style="width: 115px;padding-right:5px;"><b>Requested Date:</b></td>
        <td><b><asp:Label ID="Label2" runat="server" BackColor="Turquoise" Width="80px" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" Text='<%# Bind("req_date","{0:MM/dd/yyyy}") %>'></asp:Label></b></td>
        <td align="right" style="padding-right:5px;"><b>Due Date:</b></td>
        <td><b><asp:Label ID="Label3" runat="server" BackColor="Turquoise" Width="80px" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" Text='<%# Bind("due_date","{0:MM/dd/yyyy}") %>'></asp:Label></b></td>
        </tr>
        <tr>
        <td align="right" style="padding-right:5px;" ><b>Cust#:</b></td>
        <td style="width: 178px;"><b><asp:Label ID="Label4" BackColor="Turquoise" Width="170px" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" runat="server" Text='<%# Bind("cust_no") %>'></asp:Label></b></td>
        <td style="width: 115px;"><b></b></td>
        <td style="height: 23px"><b></b></td>
        <td align="right" style="padding-right:5px;"><b>FOB:</b></td>
        <td style="height: 23px"><b><asp:RadioButton ID="RD5" Enabled="false" runat="server" />Destination
            <asp:Label ID="Label5" Visible="false" runat="server" Text='<%# Bind("fob_code") %>'></asp:Label>
            <asp:RadioButton ID="RD6" Enabled="false" runat="server" />Origin</b></td>       
        </tr>
        <tr>
        <td align="right" style="padding-right:5px;"><b>Cust Name:</b></td>
        <td style="width: 178px"><b><asp:Label ID="Label6" BackColor="Turquoise" Width="170px" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" runat="server" Text='<%# Bind("ship_name") %>'></asp:Label>
         </b></td>
        <td style="width: 115px"><b></b></td>
        <td><b></b></td>
        <td align="right" style="padding-right:5px;"><b>Freight Charge:</b></td>
        <td><b><asp:Label ID="Label7" BackColor="Turquoise" Width="140px" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" runat="server" Text='<%# Bind("chg_method") %>'></asp:Label></b></td>
        </tr>
        <tr>
        <td align="right" style="padding-right:5px;"><b>Address:</b></td>
        <td style="width: 178px"><b><asp:Label ID="Label8" BackColor="Turquoise" Width="170px" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" runat="server" Text='<%# Bind("shipAddr") %>'></asp:Label></b></td>
        <td style="width: 115px"><b></b></td>
        <td><b></b></td>
        <td align="right" style="padding-right:5px;"><b>Warehouse Month:</b></td>
        <td><b><asp:Label ID="Label9" runat="server" BackColor="Turquoise" Width="140px" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" Text='<%# Bind("wh_month") %>'></asp:Label></b></td>
        </tr>
        <tr>
        <td><b></b></td>
        <td style="width: 178px"><b><asp:Label ID="Label10" runat="server" BackColor="Turquoise" Width="170px" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" Text='<%# Bind("shipAddr2") %>'></asp:Label></b></td>
        <td style="width: 115px"><b></b></td>
        <td><b></b></td>
        <td><b></b></td>
        <td><b></b></td>
        </tr>
        <tr>
        <td nowrap  align="right" style="padding-right:5px;"><b>City:</b></td>
        <td style="width: 178px"><b><asp:Label ID="Label11" runat="server" BackColor="Turquoise" Width="70px" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" Text='<%# Bind("ship_city") %>'></asp:Label>
         <asp:Label ID="Label12" BackColor="Turquoise" Width="55px" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" runat="server" Text='<%# Bind("ship_state") %>'>
            </asp:Label>
           <asp:Label ID="Label13" runat="server" BackColor="Turquoise" Width="35px" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" Text='<%# Bind("ship_zip") %>'></asp:Label> 
        </b></td>
        <td style="width: 115px"><b></b></td>
        <td><b></b></td>
        <td><b></b></td>
        <td><b></b></td>
        </tr>
        <tr>
        <td align="right" style="padding-right:5px; height: 44px;"><b>Sales Rep:</b></td>
        <td style="width: 178px; height: 44px;"><b><asp:Label ID="Label14" BackColor="Turquoise" Width="115px" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" runat="server" Text='<%# Bind("sman") %>'></asp:Label>
        <asp:Label ID="Label17" BackColor="Turquoise" Width="115px" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" runat="server" Text='<%# Bind("smanName") %>'></asp:Label>
        </b></td>
        <td style="width: 115px; height: 44px;"><b></b></td>
        <td style="height: 44px"><b></b></td>
        <td align="right" style="padding-right:5px; height: 44px;"><b><asp:Label ID="commlabel" runat="server" Text="Comm%:"></asp:Label></b></td>
        <td style="height: 44px"><b><asp:Label ID="Label15" runat="server" BackColor="Turquoise" Width="140px" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" Text='<%# Bind("comm","{0:###,###,##0.00}") %>'></asp:Label></b></td>
        </tr></table>
        <table>
        <tr>
        <td style="width: 338px"><b>Special Instructions:</b></td></tr>
        <tr>
        <td><b><asp:TextBox ID="Label16" Font-Bold="true" Height="50px" runat="server" TextMode="MultiLine" ReadOnly="true" BackColor="Turquoise" Width="550px" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" Text='<%# Bind("inst") %>'></asp:TextBox></b></td>
        </tr>
        </table>
        <br />
        <br />
        &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
        &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
        <asp:Button ID="UpdatButton" runat="server" OnClick="F1UpdateButton_Click" CommandName="Edit" CssClass="buttonM" Text="Update" />       
        <asp:Button ID="NewButton" runat="server" CausesValidation="False" CssClass="buttonM" CommandName="New" Text="Add">
        </asp:Button> 
                <asp:Button ID="DeleteButton" runat="server"  CssClass="buttonM" OnClick="Delete_Rfqview"
                    Text="Delete" OnClientClick="return confirm('Are you sure you want to delete this record')"> </asp:Button> 
        <%--<asp:Button ID="CopyButton" runat="server" CommandName="Edit" CssClass="buttonM" Text="Copy" OnClick="Copy_Rfqview"/>--%>
        </fieldset>
        
        </asp:Panel>
          
        </ItemTemplate>
        <EditItemTemplate>
         <asp:Panel ID="Edit_Panel" runat="server" DefaultButton="UpdateButton">
        <fieldset style="background-color:#EFF3FB">
        <table>
        <tr>
        <td><b>RFQ#:</b></td>
        <td width="180px"><b><asp:Label ID="rfq_noTextBox" runat="server" Text='<%# Bind("rfq_no") %>'>
            </asp:Label></b></td>
        <td><b>Requested Date:</b></td>
        <td width="140px"><b><asp:TextBox ID="req_dateTextBox"  onfocus="javascript:preEnter( this, 'yes' );" onblur="javascript:preLeave( this, 'date', '99/99/9999' );"  MaxLength="10" Width="70px" runat="server" Text='<%# Bind("req_date","{0:MM/dd/yyyy}") %>'>
            </asp:TextBox>
            <a href="#" onblur="setestdate()" tabindex="1" onClick="showCalendarControl(ctl00_ContentPlaceHolder1_FormView1_req_dateTextBox); return false"><asp:Image ID="Image3" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
            
            </b>
            </td>
        <td><b>Due Date:</b></td>
        <td><b><asp:TextBox ID="due_dateTextBox" runat="server" onfocus="javascript:preEnter( this, 'yes' );" onblur="javascript:preLeave( this, 'date', '99/99/9999' );"  MaxLength="10" Width="70px" Text='<%# Bind("due_date","{0:MM/dd/yyyy}") %>'>
            </asp:TextBox></b>
            <a href="#" onblur="setestdate2()" tabindex="1" onClick="showCalendarControl(ctl00_ContentPlaceHolder1_FormView1_due_dateTextBox); return false"><asp:Image ID="Image1" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
            
            </td>
        </tr>
        <tr>
        <td><b>Cust#:</b></td>       
        <td nowrap><b><asp:TextBox ID="cust_noTextBox" AutoPostBack="true" OnTextChanged="cust_change_textbox" MaxLength="8" runat="server" Text='<%# Bind("cust_no") %>'>
            </asp:TextBox>
            <a href="#" tabindex="1" onClick="customerlook('2'); return false"><asp:Image ID="CustomerLook" runat="server" ImageUrl="images/lookup_icon.gif" /></a></b></td>            
        <td><b></b></td>
        <td><b></b></td>
        <td><b>FOB:</b></td>
        <td><b><asp:TextBox ID="fob_codeTextBox" Visible="false" runat="server" Text='<%# Bind("fob_code") %>'></asp:TextBox>
           <asp:RadioButton ID="RD5" GroupName="editstatus" runat="server" />Destination
           <asp:RadioButton ID="RD6" GroupName="editstatus" runat="server" />Origin</b></td>
        
        </tr>
        <tr>
        <td><b>Cust Name:</b></td>
        <td><b><asp:TextBox ID="ship_nameTextBox" runat="server" Text='<%# Bind("ship_name") %>'>
            </asp:TextBox></b></td>
        <td><b></b></td>
        <td><b></b></td>
        <td><b>Freight Charge:</b></td>
        <td><b><asp:DropDownList ID="chg_methodTextBox" runat="server" SelectedValue='<%# Bind("chg_method") %>' DataTextField='<%# Bind("chg_method") %>'>
         <asp:ListItem>Prepaid</asp:ListItem>
        <asp:ListItem>Collect</asp:ListItem>
        <asp:ListItem>Bill</asp:ListItem>
        <asp:ListItem>Third Party</asp:ListItem>
            </asp:DropDownList></b></td>
        </tr>
        <tr>
        <td><b>Address:</b></td>
        <td><b><asp:TextBox ID="shipAddrTextBox" runat="server" Text='<%# Bind("shipAddr") %>'>
            </asp:TextBox></b></td>
        <td><b></b></td>
        <td><b></b></td>
        <td><b>Warehouse Month:</b></td>
        <td><b><asp:DropDownList ID="wh_monthTextBox" runat="server" SelectedValue='<%# Bind("wh_month") %>' DataTextField='<%# Bind("wh_month") %>'>
        <asp:ListItem>1</asp:ListItem>
        <asp:ListItem>2</asp:ListItem>
        <asp:ListItem>3</asp:ListItem>
        <asp:ListItem>4</asp:ListItem>
        <asp:ListItem>5</asp:ListItem>
        <asp:ListItem>6</asp:ListItem>
        <asp:ListItem>7</asp:ListItem>
        <asp:ListItem>8</asp:ListItem>
        <asp:ListItem>9</asp:ListItem>
        <asp:ListItem>10</asp:ListItem>
        <asp:ListItem>11</asp:ListItem>
        <asp:ListItem>12</asp:ListItem>
        <asp:ListItem>13</asp:ListItem>
        <asp:ListItem>14</asp:ListItem>
        <asp:ListItem>15</asp:ListItem>
        <asp:ListItem>16</asp:ListItem>
        <asp:ListItem>17</asp:ListItem>
        <asp:ListItem>18</asp:ListItem>
        <asp:ListItem>19</asp:ListItem>
        <asp:ListItem>20</asp:ListItem>
        <asp:ListItem>21</asp:ListItem>
        <asp:ListItem>22</asp:ListItem>
        <asp:ListItem>23</asp:ListItem>
        <asp:ListItem>24</asp:ListItem>
            </asp:DropDownList></b></td>
        </tr>
        <tr>
        <td><b></b></td>
        
        <td><b><asp:TextBox ID="shipAddr2TextBox" runat="server" Text='<%# Bind("shipAddr2") %>'>
            </asp:TextBox></b></td>
            <td><b></b></td>
        <td><b></b></td>
        <td><b></b></td>
        <td><b></b></td>
        </tr>
        <tr>
        <td><b>City:</b></td>
        <td nowrap><b> <asp:TextBox ID="ship_cityTextBox" Width="100px" runat="server" Text='<%# Bind("ship_city") %>'>
            </asp:TextBox>
           <asp:TextBox ID="ship_stateTextBox" runat="server" Width="50px" Text='<%# Bind("ship_state") %>'>
            </asp:TextBox>
            <asp:TextBox ID="ship_zipTextBox" runat="server" Width="30px" Text='<%# Bind("ship_zip") %>'>
            </asp:TextBox></b></td>
        <td><b></b></td>
        <td><b> </b></td>
        <td><b></b></td>
        <td><b></b></td>
        </tr>
        <tr>
        <td><b>Sales Rep:</b></td>
        <td><b> <asp:TextBox ID="smanTextBox" MaxLength="3" Width="40px" runat="server" Text='<%# Bind("sman") %>'>
            </asp:TextBox><a href="#" tabindex="1" onClick="smanlook(); return false"><asp:Image ID="SalesRep" runat="server" ImageUrl="images/lookup_icon.gif" /></a></b>
           <asp:TextBox ID="smanNameTextBox" Width="110px" runat="server" Text='<%# Bind("smanName") %>'>
            </asp:TextBox> </b></td>
        <td><b></b></td>
        <td><b></b></td>
        <td><b><asp:Label ID="commlabel" runat="server" Text="Comm%:"></asp:Label></b></td>
        <td><b><asp:TextBox ID="commTextBox" MaxLength="6" Width="50px" runat="server" Text='<%# Bind("comm") %>'>
            </asp:TextBox></b>
            <asp:CompareValidator ID="CompareValidator1" runat="server" ErrorMessage="Only decimal values" ControlToValidate="commTextBox" SetFocusOnError="true" Display="dynamic" Operator="DataTypeCheck" Type="double"></asp:CompareValidator>
            </td>
        </tr>
       
        </table>
        <table>
        <tr>
        <td><b>Special Instruction:</b></td>
        <td><b></b></td>
        <td><b></b></td>
        <td><b></b></td>
        <td><b></b></td>
        <td><b></b></td>
        </tr>
        <tr>
        <td><b><asp:TextBox ID="instTextBox" Font-Bold="true" Height="50px" Width="550px" TextMode="multiline" runat="server" Text='<%# Bind("inst") %>'>
            </asp:TextBox></b></td>
        </tr>
        </table>
        </fieldset>
<input type="hidden"  name="VRowid" value='<%# Server.UrlEncode(Convert.ToString(DataBinder.Eval(Container,"DataItem.VRowid"))) %>' />
            
            <asp:Button ID="UpdateButton" runat="server" Font-Bold="true" CssClass="buttonM" OnClick="UpdateButon_click"
                Text="Save">
            </asp:Button>
            <asp:Button ID="UpdateCancelButton" Font-Bold="true" CssClass="buttonM" runat="server"  CausesValidation="False" CommandName="Cancel"
                Text="Cancel" OnClick="UpdateButton_Cancel_Click">
            </asp:Button>
             </asp:Panel>
        </EditItemTemplate>
        <InsertItemTemplate>
        <asp:Panel ID="Insert_Panel" runat="server" DefaultButton="InsertButton">
        <fieldset style="background-color:#EFF3FB;">
        <table>
        <tr>
        <td ><b>RFQ:</b></td>
        <td><b><asp:Label ID="rfq_noTextBox" BackColor="PaleTurquoise" Width="50px" BorderColor="White" BorderStyle="Solid" BorderWidth="1px"  runat="server" Text='<%# Bind("rfq_no") %>'>
            </asp:Label></b></td>
        
        <td><b>Requested Date:</b></td>
        <td width="180px"><b><asp:TextBox ID="req_dateTextBox" Width="80px" onfocus="javascript:preEnter( this, 'yes' );" onblur="javascript:preLeave( this, 'date', '99/99/9999' );"  MaxLength="10" runat="server" Text='<%# Bind("req_date") %>'>
            </asp:TextBox></b>
            <a href="#" onblur="setestdate()" tabindex="1" onClick="showCalendarControl(ctl00_ContentPlaceHolder1_FormView1_req_dateTextBox); return false"><asp:Image ID="Image3" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
            
            </td>
        <td><b>Due Date:</b></td>
        <td nowrap><b><asp:TextBox ID="due_dateTextBox" Width="80px" onfocus="javascript:preEnter( this, 'yes' );" onblur="javascript:preLeave( this, 'date', '99/99/9999' );"  MaxLength="10" runat="server" Text='<%# Bind("due_date") %>'>
            </asp:TextBox></b>
            <a href="#" onblur="setestdate2()" tabindex="1" onClick="showCalendarControl(ctl00_ContentPlaceHolder1_FormView1_due_dateTextBox); return false"><asp:Image ID="Image2" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
            
            </td>
        </tr>
        <tr>
        <td><b>Cust#:</b></td>
        <td><b><asp:TextBox ID="cust_noTextBox" AutoPostBack="true" OnTextChanged="cust_change_textbox" runat="server" Text='<%# Bind("cust_no") %>'>
            </asp:TextBox></b><a href="#" tabindex="1" onClick="customerlook('1'); return false"><asp:Image ID="CustomerLook" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
            <a href="#" tabindex="1" onClick="customer2look(); return false"><asp:Image ID="Image4" runat="server" ImageUrl="images/lookup_icon.gif" /></a></td>
        <td><b></b></td>
        <td><b></b></td>
        <td><b>Fob:</b></td>
        <td><b>
            <asp:TextBox ID="fob_codeTextBox" Visible="false" runat="server" Text='<%# Bind("fob_code") %>'></asp:TextBox>
           <asp:RadioButtonList ID="RadioButtonList1"   RepeatLayout="Flow" CellSpacing="1" RepeatColumns="2" SelectedValue='<%# Bind("fob_code") %>' runat="server">
                <asp:ListItem Value="D" Text="Destination"></asp:ListItem>
                <asp:ListItem Value="O" Text="Origin"></asp:ListItem>                             
                </asp:RadioButtonList>
           
            </b></td>
        
        </tr>
        <tr>
        <td><b>Cust Name:</b></td>
        <td><b><asp:TextBox ID="ship_nameTextBox" runat="server" Text='<%# Bind("ship_name") %>'>
            </asp:TextBox></b></td>
        <td><b></b></td>
        <td><b></b></td>
        <td><b>Freight Charge:</b></td>
        <td><b>
            <asp:DropDownList ID="chg_methodTextBox" runat="server" SelectedValue='<%# Bind("chg_method") %>' DataTextField='<%# Bind("chg_method") %>'>
         <asp:ListItem>Prepaid</asp:ListItem>
        <asp:ListItem>Collect</asp:ListItem>
        <asp:ListItem>Bill</asp:ListItem>
        <asp:ListItem>Third Party</asp:ListItem>
            </asp:DropDownList>
            </b></td>
        </tr>
        <tr>
        <td><b>Address:</b></td>
        <td><b><asp:TextBox ID="shipAddrTextBox" runat="server" Text='<%# Bind("shipAddr") %>'>
            </asp:TextBox></b></td>
        <td><b></b></td>
        <td><b></b></td>
        <td><b>Warehouse Month:</b></td>
        <td><b>
            <asp:DropDownList ID="wh_monthTextBox" runat="server" SelectedValue='<%# Bind("wh_month") %>' DataTextField='<%# Bind("wh_month") %>'>
        <asp:ListItem>1</asp:ListItem>
        <asp:ListItem>2</asp:ListItem>
        <asp:ListItem>3</asp:ListItem>
        <asp:ListItem>4</asp:ListItem>
        <asp:ListItem>5</asp:ListItem>
        <asp:ListItem>6</asp:ListItem>
        <asp:ListItem>7</asp:ListItem>
        <asp:ListItem>8</asp:ListItem>
        <asp:ListItem>9</asp:ListItem>
        <asp:ListItem>10</asp:ListItem>
        <asp:ListItem>11</asp:ListItem>
        <asp:ListItem>12</asp:ListItem>
        <asp:ListItem>13</asp:ListItem>
        <asp:ListItem>14</asp:ListItem>
        <asp:ListItem>15</asp:ListItem>
        <asp:ListItem>16</asp:ListItem>
        <asp:ListItem>17</asp:ListItem>
        <asp:ListItem>18</asp:ListItem>
        <asp:ListItem>19</asp:ListItem>
        <asp:ListItem>20</asp:ListItem>
        <asp:ListItem>21</asp:ListItem>
        <asp:ListItem>22</asp:ListItem>
        <asp:ListItem>23</asp:ListItem>
        <asp:ListItem>24</asp:ListItem>
            </asp:DropDownList>
            </b></td>
        </tr>
        <tr>
        <td><b></b></td>
        <td><b><asp:TextBox ID="shipAddr2TextBox" runat="server" Text='<%# Bind("shipAddr2") %>'>
            </asp:TextBox></b></td>
        <td><b></b></td>
        <td><b></b></td>
        <td><b></b></td>
        <td><b></b></td>
        </tr>
        <tr>
        <td><b>City:</b></td>
        <td colspan="3"><b><asp:TextBox ID="ship_cityTextBox" Width="100px" runat="server" Text='<%# Bind("ship_city") %>'>
            </asp:TextBox>
            <asp:TextBox ID="ship_stateTextBox" Width="50px" runat="server" Text='<%# Bind("ship_state") %>'>
            </asp:TextBox>
            <asp:TextBox ID="ship_zipTextBox" Width="30px" runat="server" Text='<%# Bind("ship_zip") %>'>
            </asp:TextBox></b></td>
        
        
        <td><b></b></td>
        <td><b></b></td>
        </tr>
        <tr>
        <td><b>Sales Rep:</b></td>
        <td colspan="2"><b><asp:TextBox ID="smanTextBox" Width="70px" runat="server" Text='<%# Bind("sman") %>'>
            </asp:TextBox><a href="#" tabindex="1" onClick="smanlook(); return false"><asp:Image ID="SalesRep" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
            <asp:TextBox ID="smanNameTextBox" Width="110px" runat="server" Text='<%# Bind("smanName") %>'>
            </asp:TextBox></b></td>
        
        <td><b></b></td>
        <td><b><asp:Label ID="commlabel" runat="server" Text="Comm%:"></asp:Label></b></td>
        <td><b><asp:TextBox ID="commTextBox" Width="50px" runat="server" Text='<%# Bind("comm") %>'>
            </asp:TextBox></b>
            <asp:CompareValidator ID="CompareValidator1" runat="server" ErrorMessage="Only decimal values" ControlToValidate="commTextBox" SetFocusOnError="true" Display="dynamic" Operator="DataTypeCheck" Type="double"></asp:CompareValidator>
            </td>
        </tr></table>
        <table>
        <tr>
        <td><b>Special Instruction:</b></td>
        
        </tr>
        <tr>
        <td><b><asp:TextBox ID="instTextBox" TextMode="multiline" Height="30px" Width="370px" runat="server" Text='<%# Bind("inst") %>'>
            </asp:TextBox></b></td>
        </tr>
        </table>
        </fieldset>
            
            <asp:Button ID="InsertButton" runat="server" CssClass="buttonM"
                Text="Save" OnClick="InsertButton_click">
            </asp:Button>
            <asp:Button ID="InsertCancelButton" runat="server" CssClass="buttonM" CausesValidation="False"  CommandName="Cancel"
                Text="Cancel" >
            </asp:Button>
            </asp:Panel>
        </InsertItemTemplate>
        <EmptyDataRowStyle Font-Bold="True" />
    </asp:FormView>
    
    <asp:ObjectDataSource ID="ObjectDataSource1" runat="server" OldValuesParameterFormatString="original_{0}"
        SelectMethod="ViewRfq" TypeName="rfqs">
        <SelectParameters>
        <asp:Parameter Name="prmUser" Type="string" />
        <asp:Parameter Name="prmAction" DefaultValue="Select" Type="string" />
        <asp:Parameter Name="prmExt" Type="string" />
            <asp:SessionParameter Name="prmRfqNo" SessionField="Rfqview" Type="Int32" />
            <asp:Parameter Name="prmReqdate" Type="DateTime" />
            <asp:Parameter Name="prmDuedate" Type="DateTime" />
            <asp:Parameter Name="prmCustno" Type="string" />
            <asp:Parameter Name="prmShipname" Type="string" />
            <asp:Parameter Name="prmShipAddr" Type="string" />
            <asp:Parameter Name="prmShipAddr2" Type="string" />
            <asp:Parameter Name="prmShipcity" Type="string" />
            <asp:Parameter Name="prmShipstate" Type="string" />
            <asp:Parameter Name="prmShipzip" Type="string" />
            <asp:Parameter Name="prmSman" Type="string" />
            <asp:Parameter Name="prmSmanName" Type="string" />
            <asp:Parameter Name="prmComm" Type="decimal" />
            <asp:Parameter Name="prmFobcode" Type="string" />
            <asp:Parameter Name="prmChgmethod" Type="string" />
            
            <asp:Parameter Name="prmWhmonth" Type="Int32" />
            <asp:Parameter Name="prmInst" Type="string" />
            <asp:Parameter Name="VRowid" Type="Int64" />
            
        </SelectParameters>
    </asp:ObjectDataSource>
    &nbsp; 
    
    
</div>
</asp:Content>
