<%@ Page Language="c#" AutoEventWireup="true" Debug="false" Inherits="scheduled_job_report_list" Codebehind="~/scheduled_releases_job_report.aspx.cs" %>
<%@ Register Src="footer.ascx" TagName="Footer" TagPrefix="ft" %>
<%@ Register Src="header.ascx" TagName="Header" TagPrefix="hd" %>
<html xmlns="http://www.w3.org/1999/xhtml" >
  <head id="Head1" runat="server">
    <title>Scheduled Releases</title>
    <LINK href="include/style.css" type="text/css" rel="stylesheet"/>
    <LINK REL="stylesheet" TYPE="text/css" HREF="include/CalendarControl.css" >
    <script language = "JavaScript" src="include/CalendarControl.js"></script>
    <script language="javascript" src="include/date.js"></script>
    <script language="javascript" src="include/event.js"></script>
    <script language="javascript" src="include/insert.js"></script>
    
    <script language = JavaScript>    
    var bSelected=false;
    function ChSel()
    {
        var theForm = document.forms['frmList'];
        if (!theForm) theForm = document.frmList;
        bSelected = !bSelected; 
        var i;
        for (i=0;i<theForm.chDelete.length;++i) theForm.chDelete[i].checked=bSelected;
    } 
    
    function OnKeyDown()
    {
        e = window.event;
        if (e.keyCode == 13)
        {
            e.cancel = true;
            var theForm = document.forms['frmList'];
            if (!theForm) theForm = document.frmList;                
            theForm.btnSearch.click();              
        }
    }
    
 function samevalue()
    {
    var beginc=document.getElementById("TextBox1");
    var endc=document.getElementById("TextBox2");
    endc.value=beginc.value;
    }
    
    function samevalue2()
    {
    var beginc=document.getElementById("TextBox1");
    var endc=document.getElementById("TextBox2");
    if(endc.value!=beginc.value)
    {
    /*alert("Begin and End Customer Value must be same");
    endc.value=beginc.value;
    endc.focus();
    */
    }
    }

    var cval = "";
    function contactcustomerlook(val) {
        cval = val;
  var NewWindow = window.open("contact_customer_lookup.aspx","ContactCustomerLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}

function ContactCustomerLookup(ReturnObj1, ReturnObj2, ReturnObj3, ReturnObj4, ReturnObj5, ReturnObj6, ReturnObj7, ReturnObj8, ReturnObj9, ReturnObj10, ReturnObj11) {
    if (cval == 1) {
        document.forms[0].TextBox1.value = ReturnObj1;
        document.forms[0].TextBox2.value = ReturnObj1;
    }
    else if (cval == 2) {
        document.forms[0].TextBox2.value = ReturnObj1;
    }    
}

function salesreplook(){ 
  var NewWindow = window.open("salesrep_lookup.aspx","SalesRepLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}
function SalesRepLookup(ReturnObj1,ReturnObj2){ 
  document.forms[0].TextBox9.value = ReturnObj1;
  }

function smancopylook1(){ 
  var NewWindow = window.open("sman_copylookup.aspx","smancopyLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}
function smancopyLookup(ReturnObj1,ReturnObj2){ 
  document.forms[0].TextBox10.value = ReturnObj1;
 }
 
 function beproduct(){ 
  var NewWindow = window.open("jobcat_lookup.aspx","smancopyLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}
function jobcat(ReturnObj1){ 
  document.forms[0].beproTextBox.value = ReturnObj1;
 }
 function orderlook()
{
  var NewWindow = window.open("order_lookup.aspx","OrderLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}
function OrderLookup(Ret){
document.forms[0].TextBox3.value = Ret;
}
function order2look()
{
  var NewWindow = window.open("order2_lookup.aspx","OrderLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}
function Order2Lookup(Ret){
document.forms[0].TextBox4.value = Ret;
}
function Relook(){ 
  var NewWindow = window.open("reorder_item_lookup.aspx","ReItemLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}

function ReLookup(ReturnObj1){ 
  document.forms[0].TextBox5.value = ReturnObj1;
}
function Relook2(){ 
  var NewWindow = window.open("reorder_item_lookup2.aspx","ReItemLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}

function ReLookup2(ReturnObj1){ 
  document.forms[0].TextBox6.value = ReturnObj1;
}
function LocationLook(){ 
  var NewWindow = window.open("location_lookup.aspx","LocationLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}
function LocationLookUp(ReturnObj1){ 
  document.forms[0].TextBox7.value = ReturnObj1;
}
function Location2Look(){ 
  var NewWindow = window.open("location_lookup2.aspx","LocationLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}
function Location2LookUp(ReturnObj1){ 
  document.forms[0].TextBox8.value = ReturnObj1;
}
function carrierlook() {
    var NewWindow = window.open("Carrier_lookup.aspx", "CarrierLookupWindow", "width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}
function Carrierlookup(Ret) {
    document.forms[0].TextBox13.value = Ret;
}
function carrierlool2() {
    var NewWindow = window.open("Carrier_lookupcopy.aspx", "CarrierLookupWindow", "width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}
function Carrierlookupcopy(ReturnObj1) {
    document.forms[0].TextBox14.value = ReturnObj1;
}


 

function Datelook(){ 
  var NewWindow = window.open("date_lookup.aspx","DateLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
  }
function Datelookup(obj)
{
  document.forms[0].TextBox11.value=obj;
}

function Datelook1()
{
  document.forms[0].TextBox11.value="";
  Datelook();
}
function Date2look(){ 
  var NewWindow = window.open("date_lookup2.aspx","DateLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
  }
function Datelookup2(obj)
{
  document.forms[0].TextBox12.value=obj;
}

function Datelook2()
{
  document.forms[0].TextBox12.value="";
  Date2look();
}
function procatlook() {
    var NewWindow = window.open("procat_lookup.aspx", "ProcatLookupWindow", "width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}
function procatLookup(ReturnObj1) {
    document.forms[0].TextBox15.value = ReturnObj1;
}
function procat2look() {
    var NewWindow = window.open("procat_lookup2.aspx", "ProcatLookupWindow", "width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}
function procat2Lookup(ReturnObj1) {
    document.forms[0].TextBox16.value = ReturnObj1;
}

function custselect() {
    var chk = document.getElementById("CheckBox11");

    if (chk.checked == true) {
        document.forms[0].RadioButtonList1[0].checked = true;
    }
}
function disbcust(obj) {
    if (obj.checked) {
        var chk = document.getElementById("CheckBox11");
        chk.checked = false;
    }
}

 </script> 

      <link href="include/tree.css" rel="stylesheet" type="text/css" />
  </head>    
   <body>
    <form id="frmList" runat="server"  defaultfocus='TextBox1'>   
        <hd:header id="Header1" runat="server"></hd:header>
      <div>
         <asp:HiddenField ID="HiddenField1" runat="server" />
         <asp:HiddenField ID="HiddenField2" runat="server" />
         <asp:HiddenField ID="HiddenField3" runat="server" />
         <asp:HiddenField ID="HiddenField4" runat="server" />
         <asp:HiddenField ID="HiddenField5" runat="server" />
         <asp:HiddenField ID="HiddenField6" runat="server" />
         <asp:HiddenField ID="HiddenField7" runat="server" />
         <asp:HiddenField ID="HiddenField8" runat="server" />
         <asp:HiddenField ID="HiddenField9" runat="server" />
         <asp:HiddenField ID="HiddenField10" runat="server" />
         <asp:HiddenField ID="HiddenField11" runat="server" />
         <asp:HiddenField ID="HiddenField12" runat="server" />         
         <asp:HiddenField ID="HiddenField13" runat="server" />         
         <asp:HiddenField ID="HiddenField14" runat="server" />         
         <asp:HiddenField ID="HiddenField15" runat="server" />         
         <asp:HiddenField ID="HiddenField16" runat="server" /> 
         <asp:HiddenField ID="HiddenField17" runat="server" /> 
                 
          <asp:Label ID="Label1" runat="server" ForeColor="red" Font-Bold="true"></asp:Label>
          
           <asp:FormView ID="FormView2" runat="server" DataSourceID="ObjectDataSource2">
                            
              <ItemTemplate>
                  
                  <asp:Label ID="CustLabel" Visible="false" runat="server" Text='<%# Bind("Cust") %>'></asp:Label><br />
              </ItemTemplate>
          </asp:FormView>
          <asp:ObjectDataSource ID="ObjectDataSource2" runat="server" OldValuesParameterFormatString="original_{0}"
              SelectMethod="FillAlphabeticList" TypeName="reports">
              <SelectParameters>
                  <asp:Parameter Name="prmUser" Type="String" />
                  <asp:Parameter Name="prmComp" Type="String" />
              </SelectParameters>
          </asp:ObjectDataSource>
         
            
      <TABLE id="tblTop" cellSpacing="3" align="center" border="0" Width="100%">
        <TR>
            
          <TD align=center nowrap><font size=+0><b>Scheduled Releases Report &nbsp;</b></font></TD>
          <TD vAlign="middle" nowrap >
            <asp:linkbutton id="hlkBackToMenu" OnClick="Back_tomenu_Click" runat="server" >Back to menu</asp:linkbutton>
          </TD>
          <TD vAlign="middle" align="center" nowrap >Logged as&nbsp;
            <asp:label id="lblUser" runat="server" Font-Bold="True">&nbsp;</asp:label>&nbsp;&nbsp;&nbsp;
            <asp:linkbutton id="hlnkLogOut" runat="server" OnClick="hlnkLogOut_Click">Log out</asp:linkbutton>
            &nbsp;&nbsp;<asp:hyperlink id="hlnkChangePwd" runat="server" NavigateUrl="changepwd.aspx">Change password</asp:hyperlink>
          &nbsp;<b>Company:</b>&nbsp;<asp:Label ID="labelcompany" runat="server" Text="Label"></asp:Label></TD>
          
         
          <TD vAlign="middle" width="20">&nbsp;</TD>
          
          <td width=30>&nbsp;</td>
        </TR>
      </TABLE>
      <table class="shade" width="650px">
       
      <tr><td align="right" style="padding-right: 5px"><b>Begining Customer#:</b></td><td>
          <asp:TextBox ID="TextBox1"  onkeyup="samevalue()"  width="100px" runat="server"></asp:TextBox>
          <a href="#" tabindex="1" onClick="contactcustomerlook(1); return false"><asp:Image ID="CustomerLook" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
          </td>
      <td align="right" style="padding-right: 5px"><b>Ending Customer#:</b></td>
      <td><asp:TextBox ID="TextBox2"  width="100px" runat="server"></asp:TextBox>
        <a href="#" tabindex="1" onClick="contactcustomerlook(2); return false"><asp:Image ID="Image14" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
      </td>
        </tr>
        <tr><td align="right" style="padding-right:5px"><b>Begining Order#</b></td>
          <td>
              <asp:TextBox ID="TextBox3" Width="100px" runat="server"></asp:TextBox>
               <a href="#" tabindex="1" onClick="orderlook(); return false"><asp:Image ID="Image7" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
          <td align="right" style="padding-right:5px"><b>Ending Order#</b></td>
          <td>
              <asp:TextBox ID="TextBox4" Width="100px" runat="server"></asp:TextBox>
              <a href="#" tabindex="1" onClick="order2look(); return false"><asp:Image ID="Image8" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
              </td></tr>
              <tr><td align="right" style="padding-right:5px"><b>Begining Item#</b></td>
          <td>
              <asp:TextBox ID="TextBox5" Width="100px" runat="server"></asp:TextBox>
              <a href="#" tabindex="1" onClick="Relook(); return false"><asp:Image ID="Image5" runat="server" ImageUrl="images/lookup_icon.gif" /></a></td>
          <td align="right" style="padding-right:5px"><b>Ending Item#</b></td>
          <td>
              <asp:TextBox ID="TextBox6" Width="100px" runat="server"></asp:TextBox>
              <a href="#" tabindex="1" onClick="Relook2(); return false"><asp:Image ID="Image9" runat="server" ImageUrl="images/lookup_icon.gif" /></a></td></tr>
              <tr><td align="right" style="padding-right:5px"><b>Beginning WareHouse</b></td>
          <td>
              <asp:TextBox ID="TextBox7" Width="100px" runat="server"></asp:TextBox>
              <a href="#" tabindex="1" onClick="LocationLook(); return false"><asp:Image ID="Image6" runat="server" ImageUrl="images/lookup_icon.gif" /></a></td>
          <td align="right" style="padding-right:5px"><b>Ending WareHouse#</b></td>
          <td>
              <asp:TextBox ID="TextBox8" Width="100px" runat="server"></asp:TextBox>
              <a href="#" tabindex="1" onClick="Location2Look(); return false"><asp:Image ID="Image10" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
              </td></tr>
              <tr><td align="right" style="padding-right: 5px"><b>Begining Sales Rep#:</b></td>
          <td nowrap><asp:TextBox ID="TextBox9" Width="100px" runat="server"></asp:TextBox>
          <a href="#" tabindex="1" onClick="salesreplook(); return false"><asp:Image ID="FGLookup" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
          </td>
        <td align="right" style="padding-right: 5px"><b>Ending Sales Rep#:</b></td>
          <td nowrap><asp:TextBox ID="TextBox10"  Width="100px" runat="server"></asp:TextBox>
          <a href="#" tabindex="1" onClick="smancopylook1(); return false"><asp:Image ID="Image1" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
          </td>
          </tr>
      
        <tr><td align="right" style="padding-right: 5px"><b>Begining Date:</b></td>
          <td><asp:TextBox ID="TextBox11" onfocus="javascript:preEnter( this, 'yes' );" onblur="javascript:preLeave( this, 'date', '99/99/9999' );"  Width="100px" runat="server"></asp:TextBox>
          <a href="#" tabindex="1" onblur="document.getElementById('TextBox11').focus()" onClick="showCalendarControl(TextBox11); return false"><asp:Image ID="Image2" runat="server" ImageUrl="images/lookup_icon.gif" /></a></td>
        <td align="right" style="padding-right: 5px"><b>Ending Date:</b></td>
          <td><asp:TextBox ID="TextBox12" onfocus="javascript:preEnter( this, 'yes' );" onblur="javascript:preLeave( this, 'date', '99/99/9999' );"  Width="100px" runat="server"></asp:TextBox>
          <a href="#" tabindex="1" onblur="document.getElementById('TextBox12').focus()" onClick="showCalendarControl(TextBox12); return false"><asp:Image ID="Image3" onblur="document.getElementById('TextBox12').focus()" runat="server" ImageUrl="images/lookup_icon.gif" /></a></td></tr>
          <tr><td align="right" style="padding-right:5px; "><b>Begining Carrier#</b></td>
          <td >
              <asp:TextBox ID="TextBox13" Width="100px" runat="server"></asp:TextBox>
              <a href="#" tabindex="1" onClick="carrierlook(); return false"><asp:Image ID="Image4" runat="server" ImageUrl="images/lookup_icon.gif" /></a></td>
          <td align="right" style="padding-right:5px; "><b>Ending Carrier#</b></td>
          <td >
              <asp:TextBox ID="TextBox14" Width="100px" runat="server"></asp:TextBox>
              <a href="#" tabindex="1" onClick="carrierlool2(); return false"><asp:Image ID="Image11" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
              </td></tr>
              
            <tr><td align="right" style="padding-right:5px; "><b>Begining Prod. Category</b></td>
          <td >
              <asp:TextBox ID="TextBox15" Width="100px" runat="server"></asp:TextBox>
              <a href="#" tabindex="1" onClick="procatlook(); return false"><asp:Image ID="Image12" runat="server" ImageUrl="images/lookup_icon.gif" /></a></td>
          <td align="right" style="padding-right:5px; "><b>Ending Prod. Category</b></td>
          <td>
              <asp:TextBox ID="TextBox16" Width="100px" runat="server"></asp:TextBox>
              <a href="#" tabindex="1" onClick="procat2look(); return false"><asp:Image ID="Image13" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
              </td></tr>          
        
        </table>
          
        
        <table class="shade" style="width: 650px">                    
         <tr><td valign="top">          
        <fieldset>
        <table class="shade" style="height:230px">         
         <tr><td align="left" valign="top" style="padding-left:10px" ><b>Release Types:</b></td></tr>
         <tr><td><b><asp:CheckBox ID="CheckBox1" Text="Scheduled" runat="server"></asp:CheckBox></b></td></tr>
         <tr><td><b><asp:CheckBox ID="CheckBox2" Text="Late" runat="server"></asp:CheckBox></b></td></tr>
         <tr><td nowrap><b><asp:CheckBox ID="CheckBox3" Text="Past Last Ship Date" runat="server"  ></asp:CheckBox></b></td></tr>
         <tr><td> <b><asp:CheckBox ID="CheckBox4" Text="Actual" runat="server"></asp:CheckBox></b>  </td></tr> 
         <tr><td><b><asp:CheckBox ID="CheckBox5" Text="Backorder" runat="server"></asp:CheckBox></b></td></tr>
         <tr><td > <b><asp:CheckBox ID="CheckBox6" Text="Posted" runat="server"></asp:CheckBox></b></td></tr>
         <tr><td ><b><asp:CheckBox ID="CheckBox7" Text="invoice" runat="server"></asp:CheckBox></b></td></tr>
         <tr><td ><b><asp:CheckBox ID="CheckBox8" Text="Completed" runat="server"></asp:CheckBox></b></td></tr>
         <tr><td style="height:90px"><b>&nbsp;</b></td></tr>
         </table></fieldset></td>
         <td valign="top">
         <fieldset> <table style="height:230px">
          <tr><td nowrap align="Left" valign="top" style="padding-left:10px"  ><b>Sort Options:</b></td></tr>
                 <tr><td nowrap><b><asp:CheckBox ID="CheckBox11" Text="Subtotal by Customer#" onClick="custselect()" runat="server"></asp:CheckBox></b></td></tr>
             <tr><td valign="top"><asp:RadioButtonList ID="RadioButtonList1"  Font-Bold ="true" runat="server" >                              
                  <asp:ListItem  Value="Customer#"    Text="Customer#" />
                  <asp:ListItem  Value="Release Date"   Text="Releases Date" onclick="disbcust(this);" />
                  <asp:ListItem  Value="Item#"     Text="Item#" onclick="disbcust(this);" />
                  <asp:ListItem  Value="Item Name"    Text="Item Name" onclick="disbcust(this);" />
                  <asp:ListItem  Value="Territory"    Text="Territory" onclick="disbcust(this);" />
                  <asp:ListItem  Value="Carrier"    Text="Carrier" onclick="disbcust(this);" />                                                 
         </asp:RadioButtonList> &nbsp; &nbsp;</td></tr> 
         
            <tr><td style="height:93px"><b>&nbsp;</b></td></tr>
         </table></fieldset></td>
         
        
         <td valign="top">         
            <fieldset> <table style="height:230px">
                <tr><td nowrap align="Left" valign="top" style="padding-left:10px"  ><b>Print Options:</b></td></tr>
                <tr><td nowrap><b><asp:CheckBox ID="CheckBox9" Text="Print Components?" runat="server"></asp:CheckBox></b></td></tr>
                <tr><td nowrap><b><asp:CheckBox ID="CheckBox10" Text="Print Qty On-Hand?" runat="server"></asp:CheckBox></b></td></tr>
                <tr><td nowrap><b><asp:CheckBox ID="CheckBox12" Text="Print Last Ship Date?" runat="server"></asp:CheckBox></b></td></tr>
                
                <tr><td valign="top">
                    <fieldset><table cellpadding="0" cellspacing="0"><tr><td>
                        <asp:RadioButtonList ID="RadioButtonList2"  Font-Bold ="true" runat="server" >                              
                            <asp:ListItem  Value="MSF/Style"    Text="MSF/Style" />
                            <asp:ListItem   Value="Pallet Qty"   Text="Pallet Qty" />                                                                
                            </asp:RadioButtonList></td></tr></table></fieldset>
                            </td>
                    <td valign="top"> <b><asp:CheckBox ID="CheckBox13" Text="Print Due Alert?" runat="server"></asp:CheckBox></b></td>
                </tr>
                
                <tr><td valign="top">
                    <fieldset><table><tr><td>
                        <asp:RadioButtonList ID="RadioButtonList3"  Font-Bold ="true" runat="server" >                              
                            <asp:ListItem  Value="Print Item#"    Text="Print Item#" />
                            <asp:ListItem   Value="Print Item Name"   Text="Print Item Name" />                                                                
                            </asp:RadioButtonList></td></tr></table></fieldset>
                            </td>                   
                </tr>
                
                <tr><td valign="top">
                    <fieldset><table><tr><td>
                        <asp:RadioButtonList ID="RadioButtonList4"  Font-Bold ="true" runat="server" >                              
                            <asp:ListItem  Value="FG Item#"    Text="FG Item#" />
                            <asp:ListItem   Value="Expand Desc to 30"   Text="Expand Desc to 30" />                                                                
                            </asp:RadioButtonList></td></tr></table></fieldset>
                            &nbsp; &nbsp;</td>                   
                </tr>
                
                
                 
         </table></fieldset>
      
         </td>

          </tr>
          <tr><td colspan="2" align="left" style="padding-left:10px">
         &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
         <b>Output to?  <asp:RadioButtonList ID="RadioButtonList_out"  RepeatLayout="Flow"  CellSpacing="1" RepeatColumns="5" Font-Bold ="true" runat="server">
             
                 
                 <asp:ListItem   Value="No"   Text="Text File" />
                 <asp:ListItem  Value="Yes"  Text="Excel" />
                 
         </asp:RadioButtonList></b></td></tr>
         
         <tr><td  colspan="3">
             &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; <asp:Button ID="submitbutton" OnClick="submitbutton_click" runat="server" class="buttonM" Text="Submit" />
             &nbsp;&nbsp;&nbsp;&nbsp;
          <asp:Label ID="OutPutFile" runat="server" Text="Output File:" Font-Bold="True" Font-Size="Larger" ForeColor="Blue"></asp:Label>
              <asp:HyperLink ID="HyperLink1" runat="server" Target="_blank" Font-Bold="True" Font-Size="Larger" ForeColor="Red"></asp:HyperLink>
             </td>
          </tr>   
         </table>     
          
         
          <asp:FormView ID="FormView1" Visible="false"  runat="server" DataSourceID="ObjectDataSource1">
                           
              <ItemTemplate>
                  schedrelfile:
                  <asp:Label ID="schedrelfileLabel" runat="server" Text='<%# Bind("schedrelfile") %>'>
                  </asp:Label><br />
              </ItemTemplate>
              
              
          </asp:FormView>   
          <asp:ObjectDataSource ID="ObjectDataSource1" runat="server" OldValuesParameterFormatString="original_{0}"
              SelectMethod="dsSchedRelWoShip" TypeName="reports">
              <SelectParameters>
                  <asp:Parameter Name="prmUser" Type="String" />
                  <asp:Parameter Name="SchedRel" Type="String" />
                  <asp:Parameter Name="vBeginCust" Type="String" />
                  <asp:Parameter Name="vEndCust" Type="String" />
                  <asp:Parameter Name="vBeginOrder" Type="Int32" />
                  <asp:Parameter Name="vEndOrder" Type="Int32" />
                  <asp:Parameter Name="vBeginItem" Type="String" />
                  <asp:Parameter Name="vEndItem" Type="String" />
                  <asp:Parameter Name="vBeginLoc" Type="String" />
                  <asp:Parameter Name="vEndLoc" Type="String" />
                  <asp:Parameter Name="vBeginSalesMan" Type="String" />
                  <asp:Parameter Name="vEndSalesMan" Type="String" />
                  <asp:Parameter Name="vBeginDate" Type="DateTime" />
                  <asp:Parameter Name="vEndDate" Type="DateTime" />
                  <asp:Parameter Name="vBeginCarrier" Type="String" />
                  <asp:Parameter Name="vEndCarrier" Type="String" />
                  <asp:Parameter Name="vScheduled" Type="String" />
                  <asp:Parameter Name="vActual" Type="String" />
                  <asp:Parameter Name="vLate" Type="String" />
                  <asp:Parameter Name="vBackOrder" Type="String" />
                  <asp:Parameter Name="vPastLastShip" Type="String" />
                  <asp:Parameter Name="vPosted" Type="String" />
                  <asp:Parameter Name="vCompleted" Type="String" />
                  <asp:Parameter Name="vInvoice" Type="String" />
                  <asp:Parameter Name="vPrintComp" Type="String" />
                  <asp:Parameter Name="vPrintQtyOnHand" Type="String" />
                  <asp:Parameter Name="vSubTotal" Type="String" />
                  <asp:Parameter Name="vPrintLastShipDate" Type="String" />
                  <asp:Parameter Name="vSort" Type="String" />
                  <asp:Parameter Name="vPrintMsf" Type="String" />
                  <asp:Parameter Name="vPrint" Type="String" />
                  <asp:Parameter Name="vRdPrint3" Type="String" />                  
                  <asp:Parameter Name="vBeginProdCat" Type="String" />                  
                  <asp:Parameter Name="vEndProdCat" Type="String" />                  
                  <asp:Parameter Name="vPrintDue" Type="String" />                  
                  <asp:Parameter Name="prmOut" Type="String" />                  
              </SelectParameters>
          </asp:ObjectDataSource>
     
    </div>
    <ft:footer id="Footer1" runat="server"></ft:footer>
    </form>
  </body>
</HTML>

