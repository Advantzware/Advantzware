<%@ Page Language="c#" AutoEventWireup="true" Debug="true" Inherits="vend_viewlist" Codebehind="vend_viewlist.aspx.cs" %>

<%@ Register Src="footer.ascx" TagName="Footer" TagPrefix="ft" %>
<%@ Register Src="header.ascx" TagName="Header" TagPrefix="hd" %>
<html xmlns="http://www.w3.org/1999/xhtml" >
  <head id="Head1" runat="server">
    <title>Vendor</title>
    <LINK href="include/style2.css" type="text/css" rel="stylesheet"/>
   <LINK REL="stylesheet" TYPE="text/css" HREF="include/CalendarControl.css" >
    <script language="javascript" src="include/date.js"></script>
    <script language="javascript" src="include/event.js"></script>
    <script language="javascript" src="include/insert.js"></script>
    <script language = "JavaScript" src="include/CalendarControl.js">
    </script>
  
    
<script language="javascript" >


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
   
window.onload=setfocus;
function setfocus()
{
    if(document.getElementById("FormView1_vcustnoTextBox"))
    {
        var cust=document.getElementById("FormView1_vcustnoTextBox");
        cust.focus();
    }
    else if(document.getElementById("FormView1_vcustnameTextBox"))
        {
            var name=document.getElementById("FormView1_vcustnameTextBox");
            name.focus();
        }  
}

function preLeave( fieldObj, fieldType, fieldFormat ){
fieldObj.style.backgroundColor='Window';
    fieldObj.style.color='WindowText';
  fieldType = fieldType.toLowerCase();
 
  if((fieldType == "") || (fieldType == "text")){
     leaveField( fieldObj );
  }

if(fieldType == "date"){
     if(fieldFormat == ""){ var dateFormat = "99/99/9999";
     }else{ var dateFormat = fieldFormat; }
     checkDate(dateFormat,fieldObj,'01/01/1950','12/31/3000',0);
  } 

if(fieldType == "number"){
     if(fieldFormat == ""){ var numFormat = "(>>>>9)";
     }else{ var numFormat = fieldFormat; }
     checkNum(numFormat,fieldObj,'?','?',0);
  }      
}

function preEnter( fieldObj, canEdit ){
fieldObj.style.backgroundColor='blue';
    fieldObj.style.color = 'white';
  if(canEdit == "no"){
     fieldObj.blur();
     leaveField( fieldObj );      
  }
 
  enterField( fieldObj );
  return;
}

function focusval(obj) {
    obj.style.backgroundColor = 'blue';
    obj.style.color = 'white';
}
function blurval(obj) {
    obj.style.backgroundColor = 'Window';
    obj.style.color = 'WindowText';
}
    
function contactcustomerlook()
{ 
  var NewWindow = window.open("contact_customer_lookup.aspx","ContactCustomerLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}

function ContactCustomerLookup(ReturnObj1,ReturnObj2,ReturnObj3,ReturnObj4,ReturnObj5,ReturnObj6,ReturnObj7,ReturnObj8,ReturnObj9, ReturnObj10,ReturnObj11)
{ 
  document.forms[0].FormView1_vcustnoTextBox.value = ReturnObj1;
}


  
function zipcodelook()
{ 
  var NewWindow = window.open("zipcode_lookup.aspx","ZipCodeLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}

function ZipCodeLookup(ReturnObj1, ReturnObj2, ReturnObj3)
{
    document.forms[0].FormView1_vzipTextBox.value = ReturnObj1;
    document.forms[0].FormView1_vcityTextBox.value = ReturnObj2;
    document.forms[0].FormView1_vstateTextBox.value = ReturnObj3;
   
}
function citylook()
{ 
  var NewWindow = window.open("city_lookup.aspx","CityCodeLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}

function CityCodeLookup(ReturnObj1)
{
    document.forms[0].FormView1_vcityTextBox.value = ReturnObj1;
}
var vstate = "";
function statecodelook(var1) {
    vstate = var1;
  var NewWindow = window.open("statecode_lookup.aspx","StateCodeLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}

function StateCodeLookup(ReturnObj1) {
    if (vstate == 1) {
        document.forms[0].FormView1_vstateTextBox.value = ReturnObj1;
    }
    else {
        document.forms[0].FormView1_vrstateTextBox.value = ReturnObj1;
    }
}

function terrlook()
{ 
  var NewWindow = window.open("terr_lookup.aspx","TerrCodeLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}

function TerrCodeLookup(ReturnObj1,ReturnObj2)
{ 
   document.forms[0].FormView1_vterrTextBox.value = ReturnObj1;
   document.forms[0].FormView1_vdesterrTextBox.value= ReturnObj2;
}

function salesreplook()
{ 
  var NewWindow = window.open("salesrep_lookup.aspx","SalesRepLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}

function SalesRepLookup(ReturnObj1,ReturnObj2)
{ 
  document.forms[0].FormView1_vsmanTextBox.value = ReturnObj1;
  document.forms[0].FormView1_vdescsmanTextBox.value = ReturnObj2;
}

function ontypedscr() {
    var ty = document.getElementById("FormView1_vnameTextBox");
    ty.focus();
}
function onbuydscr() {
    var by = document.getElementById("FormView1_vadd2TextBox");
    by.focus();
}
function ontermdscr() {
    var ty = document.getElementById("FormView1_vdiscTextBox");
    ty.focus();
}
function oncarrierdscr() {
    var cy = document.getElementById("FormView1_vendorTextBox");
    cy.focus();
}
function onactdscr() {
    var at = document.getElementById("FormView1_vremitTextBox");
    at.focus();
}
function oncurdscr() {
    var cu = document.getElementById("FormView1_vtaxgrTextBox");
    cu.focus();
}


function buyerlookup() {

    var NewWindow = window.open("buyerlook.aspx", "UomLookup", "width=500,height=400,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}
function buyerlook(ReturnObj1, ReturnObj2) {
    document.forms[0].FormView1_vbuyerTextBox.value = ReturnObj1;
    document.forms[0].FormView1_vbuyerdscrTextBox.value = ReturnObj2;
}
//function datelook(){ 
//  var NewWindow = window.open("date_lookup.aspx","DatelookupWindow","width=260,height=260,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
//}

//function Datelookup(ReturnObj1)
//{ 
//  document.forms[0].FormView1_vdate1TextBox.value = ReturnObj1;
// 
//  
//
//}

function Syspramcharlook() {
    var NewWindow = window.open("SysPramCharValue_lookup.aspx", "SysPramCharLookupWindow", "width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}

function syspramchrlook(ReturnObj1) {
    document.forms[0].FormView1_vpoexportTextBox.value = ReturnObj1;
    
}

function termslook()
{ 
  var NewWindow = window.open("terms_lookup.aspx","termsLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}

function termsLookup(ReturnObj1,ReturnObj2)
{
    document.forms[0].FormView1_vtermsTextBox.value = ReturnObj1;
    document.forms[0].FormView1_vtermsdscrTextBox.value = ReturnObj2;
}
  
function vendortypelook()
{ 
  var NewWindow = window.open("vendtype_lookup.aspx","vendtypeLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}
function VendTypeLookup(ReturnObj1, ReturnObj2)
{
    document.forms[0].FormView1_vtypeTextBox.value = ReturnObj1;
    document.forms[0].FormView1_vtypedscrTextBox.value = ReturnObj2;
 }
 
 function locationlook()
 { 
  var NewWindow = window.open("location_lookup.aspx","LocationLookUpWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}
function LocationLookUp(ReturnObj1, ReturnObj2)
{ 
  document.forms[0].FormView1_vlocTextBox.value = ReturnObj1;
  document.forms[0].FormView1_vdesclocTextBox.value = ReturnObj2;
 }
function carrierlook()
{ 
  var NewWindow = window.open("Carrier_lookup.aspx","CarrierlookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}
function Carrierlookup(ReturnObj1, ReturnObj2)
{
    document.forms[0].FormView1_vcarrierTextBox.value = ReturnObj1;
    document.forms[0].FormView1_vcarrierdscrTextBox.value = ReturnObj2;
}

function AccountLook() {

    var NewWindow = window.open("accountlook.aspx", "AccountLookupWindow", "width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}

function AccountLookup(ReturnObj1, ReturnObj2) {
    document.forms[0].FormView1_vactnumTextBox.value = ReturnObj1;
    document.forms[0].FormView1_vactdscrTextBox.value = ReturnObj2;   
}
 
 
function currencylook()
{ 
  var NewWindow = window.open("currency_lookup.aspx","CurrencyLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}
function CurrencyLookup(ReturnObj1, ReturnObj2)
{
    document.forms[0].FormView1_vcurrcodeTextBox.value = ReturnObj1;
    document.forms[0].FormView1_vcurrdscrTextBox.value = ReturnObj2;
}
 
 function deliveryzonelook()
 {
  var carrier=document.getElementById("FormView1_vcarrierTextBox").value; 
  var NewWindow = window.open("zone_lookup.aspx?zone="+carrier+"","DelZoneLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
 }
function zoneLookup(ReturnObj1, ReturnObj2)
{ 
  document.forms[0].FormView1_vdelzoneTextBox.value = ReturnObj1;
  document.forms[0].FormView1_vdeszoneTextBox.value = ReturnObj2;  
 }
 
 function taxcodelook()
 { 
  var NewWindow = window.open("tax_lookup.aspx","TaxLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
 }
function TaxLookup(ReturnObj1)
{
    document.forms[0].FormView1_vtaxgrTextBox.value = ReturnObj1;
  
}
 
function focusval(obj)
{
    obj.style.backgroundColor='blue';
    obj.style.color = 'white';
}
function blurval(obj)
{
    obj.style.backgroundColor='Window';
    obj.style.color='WindowText';
}
function expblurval(obj)
{
    obj.style.backgroundColor='Window';
    obj.style.color='WindowText'; 
    
    if(document.getElementById("FormView1_vcustnoTextBox"))
    {
        var cust=document.getElementById("FormView1_vcustnoTextBox");
        cust.focus();
    }
    else if(document.getElementById("FormView1_vcustnameTextBox"))
    {
        var name=document.getElementById("FormView1_vcustnameTextBox");
        name.focus();
    }
}
function expdate()
{
    var expdate=document.getElementById("FormView1_vdatefield2TextBox").value;
    if(expdate.length>1 && expdate.length<3 && expdate.indexOf('/')!=1)
    {
        document.getElementById("FormView1_vdatefield2TextBox").value = expdate + "/";
    }
    if(expdate.length>4 && expdate.length<6 && expdate.indexOf('/')!=3)
    {
        document.getElementById("FormView1_vdatefield2TextBox").value = expdate + "/";
    }
}
function adddate()
{    
    var dateadd=document.getElementById("FormView1_vdate1TextBox").value;
    if(dateadd.length>1 && dateadd.length<3 && dateadd.indexOf('/')!=1)
    {
        document.getElementById("FormView1_vdate1TextBox").value = dateadd + "/";
    }
    if(dateadd.length>4 && dateadd.length<6 && dateadd.indexOf('/')!=3)
    {
        document.getElementById("FormView1_vdate1TextBox").value = dateadd + "/";
    }
}

function Datelook()
{ 
  var NewWindow = window.open("date_lookup.aspx","DateLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}
function Datelookup(obj)
{
  document.forms[0].FormView1_vdate1TextBox.value=obj;
}
function Datelook2()
{ 
  var NewWindow = window.open("date_lookup2.aspx","DateLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}
function Datelookup2(obj)
{
  document.forms[0].FormView1_vdatefield2TextBox.value=obj;
}
function flatcomm()
{
    var comm=document.getElementById("FormView1_vflatcommTextBox").value;
        if(comm.indexOf(".") != -1)
            {        
                return;
            } 
        else if(comm.length > 4 && comm.length < 6)
        comm=comm + ".";
        document.getElementById("FormView1_vflatcommTextBox").value = comm;
}
function empday()
{
    var emp=document.getElementById("FormView1_vcrholdinvdueTextBox").value;
        if(emp.indexOf(".") != -1)
            {        
                return;
            } 
        else if(emp.length > 6 && emp.length < 8)
        emp=emp + ".";
        document.getElementById("FormView1_vcrholdinvdueTextBox").value = emp;
}
function orlim()
{
    var orlim=document.getElementById("FormView1_vcrlimTextBox").value;
        if(orlim.indexOf(".") != -1)
            {        
                return;
            } 
        else if(orlim.length > 7 && orlim.length < 9)
        orlim=orlim + ".";
        document.getElementById("FormView1_vcrlimTextBox").value = orlim;   
}
function crlimit()
{
     var crlimt=document.getElementById("FormView1_vcrlimtTextBox").value;
        if(crlimt.indexOf(".") != -1)
            {        
                return;
            } 
        else if(crlimt.length > 8 && crlimt.length < 10)
        crlimt=crlimt + ".";
        document.getElementById("FormView1_vcrlimtTextBox").value = crlimt;
}
function discount()
{
    var disc=document.getElementById("FormView1_vdiscTextBox").value;
        if(disc.indexOf(".") != -1)
            {        
                return;
            } 
        else if(disc.length > 2 && disc.length < 4)
        disc=disc + ".";
        document.getElementById("FormView1_vdiscTextBox").value = disc;   
}
function underrun()
{
    var under=document.getElementById("FormView1_vunderpctTextBox").value;
        if(under.indexOf(".") != -1)
            {        
                return;
            } 
        else if(under.length > 1 && under.length < 3)
        under=under + ".";
        document.getElementById("FormView1_vunderpctTextBox").value = under;
}
function overrun()
{
    var over=document.getElementById("FormView1_voverpctTextBox").value;
        if(over.indexOf(".") != -1)
            {        
                return;
            } 
        else if(over.length > 1 && over.length < 3)
        over=over + ".";
        document.getElementById("FormView1_voverpctTextBox").value = over;
}
function markup()
{
    var mark=document.getElementById("FormView1_vmarkupTextBox").value;
        if(mark.indexOf(".") != -1)
            {        
                return;
            } 
        else if(mark.length > 1 && mark.length < 3)
        mark=mark + ".";
        document.getElementById("FormView1_vmarkupTextBox").value = mark;
}
function datevalcalender()
{
    var adddate = document.getElementById("FormView1_vdate1TextBox");
    //obj.style.backgroundColor='blue';
    //obj.style.color = 'white';
    adddate.style.backgroundColor = 'blue';
    adddate.style.color= 'white';
    showCalendarControl(adddate);
}
function espdatecal()
{
    var adddate = document.getElementById("FormView1_vdatefield2TextBox");   
    adddate.style.backgroundColor = 'blue';
    adddate.style.color= 'white';
    showCalendarControl(adddate);

}

   </script>

 </head>    

   <body>
        <form id="frmList" runat="server"  defaultfocus='cust_TextBox'>   
            <hd:header id="Header1" runat="server"></hd:header>
                <table width="100%"><tr><td><div>
        <table align="left" border="1" width="75%">
                <tr class="topheadcolor">                                      
                 
                        <td nowrap width="25px";>
                        <asp:ImageButton ID="img_btn_add" runat="server" Width="35px" ImageUrl="~/Images/add.bmp" ToolTip="Add" OnClick="img_btn_add_click" />
                        </td>                       
                        <td nowrap width="25px";>
                        <asp:ImageButton ID="img_btn_exit" runat="server" Width="35px" ImageUrl="~/Images/exit-au.bmp" ToolTip="LogOut" OnClick="hlnkLogOut_Click" />
                        </td>
                        <td nowrap> &nbsp;</td>
                </tr>
      </table></div>
        </td>
      </tr>
      <tr>
      <td>
                <div>            
                    <TABLE id="tblTop" cellSpacing="3" align="center" border="0" Width="100%">
                        <TR>
                             <TD width=30>&nbsp;</TD>
                            <TD align=center nowrap><font size=+0><b>Vendors&nbsp;</b></font></TD>
                            <td nowrap>
                                <asp:LinkButton ID="backtomenuLinkButton" OnClick ="Back_tomenu_Click" runat="server">Back to menu</asp:LinkButton>
                            </td>          
                            <TD  align="left" nowrap>Logged as&nbsp;
                                <asp:label id="lblUser" runat="server" Font-Bold="True">&nbsp;</asp:label>&nbsp;&nbsp;&nbsp;            
                                <asp:linkbutton id="hlnkLogOut" runat="server" OnClick="hlnkLogOut_Click">Log out</asp:linkbutton>
                                &nbsp;&nbsp;<asp:hyperlink id="hlnkChangePwd" runat="server" NavigateUrl="changepwd.aspx"></asp:hyperlink>
                                &nbsp;<b>Company: &nbsp;</b><asp:label id="labelcompany"   runat="server" Font-Bold="True">&nbsp;</asp:label>
                            </TD>
          
                            <TD vAlign="middle" width="20">&nbsp;</TD>          
                            <td width=30>&nbsp;</td>
                        </TR>
                    </TABLE>
                    <table>
                        <tr bgcolor="gray">
                            <td nowrap><div  id="navigation" style="width:100%">
		                        <ul nowrap> <li >
                                <asp:LinkButton ID="lnk_Listvend" runat="server" OnClick="lnk_Listvend_Click" >Brows Vendor</asp:LinkButton></li>
                                <li class="selected"><asp:LinkButton ID="lnk_viewvend" runat="server"  OnClick="lnk_viewvend_Click"  > View Vendor</asp:LinkButton></li>
                                <li><asp:LinkButton ID="lnk_listtot" runat="server" OnClick="lnk_listtot_Click" >Totals</asp:LinkButton></li></ul></div>
                                
                            </td>      
                        </tr>
                   </table>
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
            
                    <asp:FormView ID="FormView1" runat="server" OnDataBound="FormView1_DataBound" DataSourceID="ObjectDataSource1">
                        <EditItemTemplate>
                            <asp:Panel ID="updatePanel" runat="server" DefaultButton="UpdateButton">
                            <fieldset class="shade">
                            <table><tr><td colspan="2">
                            <table>
                            <tr><td align="right" style="padding-right:5px"><b>Vendor:</b></td>
                            <td nowrap="nowrap"> <asp:TextBox ID="vendorTextBox" BackColor="turquoise" ReadOnly="true" Width="100px" runat="server" Text='<%# Bind("vendor") %>' />
                             <b>Status:</b><b>&nbsp;<asp:RadioButton ID="RD5" GroupName="acco" runat="server" /><b>Active</b>
                                                        <asp:Label ID="activeLabel" Visible="false" runat="server" Text='<%# Bind("vactive") %>'></asp:Label>
                                                        <asp:RadioButton ID="RD6"  GroupName="acco" runat="server" />Inactive</b></td>
                            <td align="right" style="padding-right:5px"><b>Type:</b></td>
                            <td><asp:TextBox ID="vtypeTextBox" Width="70px" MaxLength="9" runat="server" Text='<%# Bind("vtype") %>' />
                            <a href="#" tabindex="1" onClick="vendortypelook(); return false"><asp:Image ID="vendtype" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
                            <asp:TextBox ID="vtypedscrTextBox" ForeColor="#ACA899" onfocus="ontypedscr()" Width="160px" runat="server"   Text='<%# Bind("vtypedscr") %>' /></td></tr>
                            <tr><td align="right" style="padding-right:5px"><b>Name:</b></td>
                            <td><asp:TextBox ID="vnameTextBox" Width="250px" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" runat="server" Text='<%# Bind("vname") %>' /></td>
                            <td align="right" style="padding-right:5px"><b>Contact:</b></td>
                            <td><asp:TextBox ID="vcontactTextBox" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" runat="server" Width="230px"  Text='<%# Bind("vcontact") %>' /> </td></tr>
                            <tr><td align="right" style="padding-right:5px"><b>Address:</b></td>
                            <td><asp:TextBox ID="vadd1TextBox" runat="server" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" Width="150px" Text='<%# Bind("vadd1") %>' /></td>
                            <td align="right" style="padding-right:5px"><b>Buyer:</b></td>
                            <td><asp:TextBox ID="vbuyerTextBox" Width="70px" MaxLength="3" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" runat="server" Text='<%# Bind("vbuyer") %>' />
                            <a href="#" tabindex="1" onClick="buyerlookup(); return false"><asp:Image ID="Image5" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
                            <asp:TextBox ID="vbuyerdscrTextBox" runat="server" ForeColor="#ACA899" onfocus="onbuydscr()" Width="160px"  Text='<%# Bind("vbuyerdscr") %>' /></td></tr>
                            <tr><td></td><td><asp:TextBox ID="vadd2TextBox" Width="150px" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" runat="server" Text='<%# Bind("vadd2") %>' /></td>
                            <td align="right" style="padding-right:5px"><b>Phone:</b></td>
                            <td><asp:TextBox ID="vareacodeTextBox" Width="30px" MaxLength="3" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" runat="server" Text='<%# Bind("vareacode") %>' />
                            <asp:TextBox ID="vphoneTextBox" MaxLength="8" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" runat="server"  Width="80px" Text='<%# Bind("vphone") %>' />
                            &nbsp;<b>Fax:</b>&nbsp; <asp:TextBox ID="vfaxareaTextBox" Width="30px" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" MaxLength="3" runat="server" Text='<%# Bind("vfaxarea") %>' />
                            <asp:TextBox ID="vfaxTextBox" Width="80px" MaxLength="8" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" runat="server" Text='<%# Bind("vfax") %>' /></td></tr>
                            <tr><td align="right" style="padding-right:5px"><b>City:</b></td>
                            <td><asp:TextBox ID="vcityTextBox" Width="80px" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" runat="server" Text='<%# Bind("vcity") %>' />
                            <a href="#" tabindex="1" onClick="citylook(); return false"><asp:Image ID="Image9" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
                            <asp:TextBox ID="vstateTextBox" Width="30px" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" runat="server" Text='<%# Bind("vstate") %>' />
                            <a href="#" tabindex="1" onClick="statecodelook(1); return false"><asp:Image ID="Image4" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
                            <asp:TextBox ID="vzipTextBox" Width="80px" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" runat="server" Text='<%# Bind("vzip") %>' />
                            <a href="#" tabindex="1" onClick="zipcodelook(); return false"><asp:Image ID="Image1" runat="server" ImageUrl="images/lookup_icon.gif" /></a></td>
                            <td align="right" style="padding-right:5px"><b>Fax Prefix:</b></td>
                            <td><asp:TextBox ID="vfaxprefixTextBox" Width="30px" MaxLength="3" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" runat="server" Text='<%# Bind("vfaxprefix") %>' /> 
                            &nbsp; <b>Fax Country:</b>  <asp:TextBox ID="vfaxcountryTextBox" MaxLength="8" Width="100px" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" runat="server" Text='<%# Bind("vfaxcountry") %>' /></td></tr>
                            <tr><td><b>Country:</b></td>
                            <td><asp:TextBox ID="vcountryTextBox" Width="100px" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" runat="server" Text='<%# Bind("vcountry") %>' />
                            <asp:TextBox ID="vPostalTextBox" Width="100px" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" runat="server" Text='<%# Bind("vPostal") %>' /></td>
                            <td colspan="2"> <b>Overrun Percentage: &nbsp;<asp:TextBox ID="voverpctTextBox" MaxLength="8" Width="70px" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" runat="server" Text='<%# Bind("voverpct") %>' />
                            <asp:CompareValidator ID="CompareValidator7" ControlToValidate="voverpctTextBox" SetFocusOnError="true" Display="Dynamic" Operator="DataTypeCheck" Type="Double" runat="server" ErrorMessage="Enter Valid Value"></asp:CompareValidator>
                            Underrun Percentage: &nbsp; </b><asp:TextBox ID="vunderpctTextBox" Width="70px" MaxLength="8" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" runat="server" Text='<%# Bind("vunderpct") %>' />
                            <asp:CompareValidator ID="CompareValidator8" ControlToValidate="vunderpctTextBox" SetFocusOnError="true" Display="Dynamic" Operator="DataTypeCheck" Type="Double" runat="server" ErrorMessage="Enter Valid Value"></asp:CompareValidator></td></tr>
                            <tr><td align="right" style="padding-right:5px"><b>Tax ID#:</b></td>
                            <td><asp:TextBox ID="vtaxidTextBox" runat="server" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" Width="150px" Text='<%# Bind("vtaxid") %>' /></td>
                            <td align="right" style="padding-right:5px"><b>Default G/L#:</b></td>
                            <td><asp:TextBox ID="vactnumTextBox" Width="100px" MaxLength="25" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" runat="server" Text='<%# Bind("vactnum") %>' />
                            <a href="#" tabindex="1" onClick="AccountLook(); return false"><asp:Image ID="Image13" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
                            <asp:TextBox ID="vactdscrTextBox" runat="server" onfocus="onactdscr()" ForeColor="#ACA899"  Width="150px" Text='<%# Bind("vactdscr") %>' /></td></tr>
                            </table>
                            </td></tr>
                            <tr><td>
                            <fieldset><table><tr ><td style="height:40px" colspan="2" ><b><asp:Label ID="remlabel" runat="server" Text="Remit to Address" ForeColor="Blue"></asp:Label></b></td></tr>
                            
                            <tr><td align="right" style="padding-right:5px"><b>Remit To:</b></td>
                            <td> <asp:TextBox ID="vremitTextBox" Width="180px" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" runat="server" Text='<%# Bind("vremit") %>' /></td></tr>
                            <tr><td align="right" style="padding-right:5px"><b>Address:</b></td>
                            <td><asp:TextBox ID="vradd1TextBox" runat="server" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" Width="150px" Text='<%# Bind("vradd1") %>' /></td></tr>
                            <tr><td></td><td><asp:TextBox ID="vradd2TextBox" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" Width="150px" runat="server" Text='<%# Bind("vradd2") %>' /></td></tr>
                            <tr><td align="right" style="padding-right:5px"><b>City:</b></td>
                            <td><asp:TextBox ID="vrcityTextBox" Width="80px" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" runat="server" Text='<%# Bind("vrcity") %>' />                            
                            <asp:TextBox ID="vrstateTextBox" Width="30px" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" runat="server" Text='<%# Bind("vrstate") %>' />
                            <a href="#" tabindex="1" onClick="statecodelook(2); return false"><asp:Image ID="Image10" runat="server" ImageUrl="images/lookup_icon.gif" /></a>                            
                            <asp:TextBox ID="vrzipTextBox" Width="80px" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" runat="server" Text='<%# Bind("vrzip") %>' /></td></tr>
                            <tr><td align="right" style="padding-right:5px"><b>Country:</b></td>
                            <td><asp:TextBox ID="vrcountryTextBox" Width="80px" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" runat="server"  Text='<%# Bind("vrcountry") %>' />
                            &nbsp; <b>Postal Code:</b>&nbsp;<asp:TextBox ID="vrpostalTextBox" Width="80px" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" runat="server" Text='<%# Bind("vrpostal") %>' /></td></tr>
                            <tr><td colspan="2"><b>Check Memo:</b>
                            <asp:TextBox ID="vcheckmemoTextBox" runat="server" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" Width="150px" Text='<%# Bind("vcheckmemo") %>' /></td></tr></table>
                            
                            </fieldset>
                            </td>
                            <td>
                            <fieldset><table>
                            <tr><td colspan="2"><b>Currency Code:</b> &nbsp; 
                            <asp:TextBox ID="vcurrcodeTextBox" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" runat="server" MaxLength="3" Width="100"  Text='<%# Bind("vcurrcode") %>' />
                            <a href="#" tabindex="1" onClick="currencylook(); return false"><asp:Image ID="Image12" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
                            <asp:TextBox ID="vcurrdscrTextBox" runat="server" onfocus="oncurdscr()" Width="150" ForeColor="#ACA899"  Text='<%# Bind("vcurrdscr") %>' /></td></tr>
                            <tr><td colspan="2"><b>Tax:</b>&nbsp; <asp:TextBox ID="vtaxgrTextBox" MaxLength="4" Width="50px" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" runat="server" Text='<%# Bind("vtaxgr") %>' />
                            <a href="#" tabindex="1" onClick="taxcodelook(); return false"><asp:Image ID="Image14" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
                            &nbsp;<b>1099 Code:&nbsp;</b><asp:TextBox ID="vcode1099TextBox" MaxLength="1" Width="50px" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" runat="server" Text='<%# Bind("vcode1099") %>' />
                            <asp:TextBox ID="vanedivendTextBox" Width="100px" Visible="false" runat="server" Text='<%# Bind("vanedivend") %>' />
                            <asp:CheckBox ID="CheckBox1" Text="EDI" runat="server" /></td></tr>
                            <tr><td align="right" style="padding-right:5px"><b>Terms:</b></td>
                            <td><asp:TextBox ID="vtermsTextBox" runat="server" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" MaxLength="5" Text='<%# Bind("vterms") %>' />
                            <a href="#" tabindex="1" onClick="termslook(); return false"><asp:Image ID="Image7" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
                            <asp:TextBox ID="vtermsdscrTextBox" runat="server" onfocus="ontermdscr()" ForeColor="#ACA899" Text='<%# Bind("vtermsdscr") %>' /></td></tr>
                            <tr><td align="right" style="padding-right:5px"><b>Discount%:</b></td>
                            <td><asp:TextBox ID="vdiscTextBox" runat="server" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" Text='<%# Bind("vdisc") %>' />
                            <asp:CompareValidator ID="CompareValidator2" ControlToValidate="vdiscTextBox" SetFocusOnError="true" Display="Dynamic" Operator="DataTypeCheck" Type="Double" runat="server" ErrorMessage="Enter Valid Value"></asp:CompareValidator>
                            &nbsp;<b>PoExport:</b>&nbsp;<asp:TextBox ID="vpoexportTextBox" runat="server" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" Text='<%# Bind("vpoexport") %>' />
                            <a href="#" tabindex="1" onClick="Syspramcharlook(); return false"><asp:Image ID="syspramimage" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
                            </td></tr>
                            </table></fieldset>
                            <fieldset>
                            <table><tr><td><b> <asp:Label ID="Label1" runat="server" Text="Shipping Information" ForeColor="Blue"/>    MaxPoCost: &nbsp;&nbsp;&nbsp;</b>
                            <asp:TextBox ID="vrebateTextBox" Width="100px" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" runat="server" Text='<%# Bind("vrebate") %>' />
                            <asp:CompareValidator ID="CompareValidator6" ControlToValidate="vrebateTextBox" SetFocusOnError="true" Display="Dynamic" Operator="DataTypeCheck" Type="Double" runat="server" ErrorMessage="Enter Valid Value"></asp:CompareValidator></td>
                            <td rowspan="3"> <b>FOB:</b>
                                <asp:RadioButtonList ID="RadioButtonList1" DataTextField='<%# Bind("vfobcode") %>' runat="server" SelectedValue='<%# Bind("vfobcode") %>'>
                                <asp:ListItem Value="Dest" Text="Dest"></asp:ListItem>
                                <asp:ListItem Value="Orig" Text="Orig"></asp:ListItem>
                                <asp:ListItem Value="" Text="None"></asp:ListItem>
                                </asp:RadioButtonList>
                            </td></tr>
                            <tr><td><b>Freight Pay:&nbsp</b><asp:TextBox Width="70px" ID="vfrtpayTextBox" MaxLength="1" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" runat="server" Text='<%# Bind("vfrtpay") %>' />
                            &nbsp; <b>Lead Time Day:&nbsp;</b><asp:TextBox ID="vdiscdaysTextBox" Width="30px" MaxLength="2" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" runat="server" Text='<%# Bind("vdiscdays") %>' />
                            <asp:CompareValidator ID="CompareValidator9" ControlToValidate="vdiscdaysTextBox" SetFocusOnError="true" Display="Dynamic" Operator="DataTypeCheck" Type="Integer" runat="server" ErrorMessage="Enter Valid Value"></asp:CompareValidator></td></tr>
                            <tr><td><b>Carrier:&nbsp;</b><asp:TextBox ID="vcarrierTextBox" MaxLength="5" runat="server" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" Width="70px" Text='<%# Bind("vcarrier") %>' />
                            <a href="#" tabindex="1" onClick="carrierlook(); return false"><asp:Image ID="Image11" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
                            <asp:TextBox ID="vcarrierdscrTextBox" runat="server" onfocus="oncarrierdscr()" ForeColor="#ACA899" Width="180px" Text='<%# Bind("vcarrierdscr") %>' /></td></tr>
                            </table>
                            </fieldset>
                            </td></tr>
                            </table>
                            </fieldset>
                           
                                                                                                           
                            <asp:TextBox ID="vreckeyTextBox" runat="server" Visible="false" Text='<%# Bind("vreckey") %>' />
                            <br />
                            <asp:Button ID="UpdateButton" runat="server" CausesValidation="True" OnClick="UpdateButton_Click" CssClass="button" Text="Save" />
                            &nbsp;<asp:Button ID="UpdateCancelButton" runat="server" CssClass="button"
                                CausesValidation="False" CommandName="Cancel" Text="Cancel" />
                                </asp:Panel>
                        </EditItemTemplate>
                        <InsertItemTemplate>
                            <asp:Panel ID="InsertPanel" runat="server" DefaultButton="InsertButton">
                            <fieldset class="shade">
                            <table><tr><td colspan="2">
                            <table>
                            <tr><td align="right" style="padding-right:5px"><b>Vendor:</b></td>
                            <td nowrap="nowrap"> <asp:TextBox ID="vendorTextBox" Width="100px" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" runat="server" Text='<%# Bind("vendor") %>' />
                             <b>Status:</b><b>&nbsp;<asp:RadioButton ID="RD5" GroupName="acco" runat="server" /><b>Active</b>
                                                        <asp:Label ID="activeLabel" Visible="false" runat="server" Text='<%# Bind("vactive") %>'></asp:Label>
                                                        <asp:RadioButton ID="RD6"  GroupName="acco" runat="server" />Inactive</b></td>
                            <td align="right" style="padding-right:5px"><b>Type:</b></td>
                            <td><asp:TextBox ID="vtypeTextBox" Width="70px" MaxLength="9" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" runat="server" Text='<%# Bind("vtype") %>' />
                            <a href="#" tabindex="1" onClick="vendortypelook(); return false"><asp:Image ID="vendtype" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
                            <asp:TextBox ID="vtypedscrTextBox" Width="160px" ForeColor="#ACA899" onfocus="ontypedscr()" runat="server"   Text='<%# Bind("vtypedscr") %>' /></td></tr>
                            <tr><td align="right" style="padding-right:5px"><b>Name:</b></td>
                            <td><asp:TextBox ID="vnameTextBox" Width="250px" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" runat="server" Text='<%# Bind("vname") %>' /></td>
                            <td align="right" style="padding-right:5px"><b>Contact:</b></td>
                            <td><asp:TextBox ID="vcontactTextBox" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" runat="server" Width="230px"  Text='<%# Bind("vcontact") %>' /> </td></tr>
                            <tr><td align="right" style="padding-right:5px"><b>Address:</b></td>
                            <td><asp:TextBox ID="vadd1TextBox" runat="server" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" Width="150px" Text='<%# Bind("vadd1") %>' /></td>
                            <td align="right" style="padding-right:5px"><b>Buyer:</b></td>
                            <td><asp:TextBox ID="vbuyerTextBox" Width="70px" MaxLength="3" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" runat="server" Text='<%# Bind("vbuyer") %>' />
                            <a href="#" tabindex="1" onClick="buyerlookup(); return false"><asp:Image ID="Image5" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
                            <asp:TextBox ID="vbuyerdscrTextBox" runat="server" onfocus="onbuydscr()" ForeColor="#ACA899" Width="160px"  Text='<%# Bind("vbuyerdscr") %>' /></td></tr>
                            <tr><td></td><td><asp:TextBox ID="vadd2TextBox" Width="150px" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" runat="server" Text='<%# Bind("vadd2") %>' /></td>
                            <td align="right" style="padding-right:5px"><b>Phone:</b></td>
                            <td><asp:TextBox ID="vareacodeTextBox" Width="30px" MaxLength="3" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" runat="server" Text='<%# Bind("vareacode") %>' />
                            <asp:TextBox ID="vphoneTextBox" runat="server" MaxLength="8" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" Width="80px" Text='<%# Bind("vphone") %>' />
                            &nbsp;<b>Fax:</b>&nbsp; <asp:TextBox ID="vfaxareaTextBox" Width="30px" MaxLength="3" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" runat="server" Text='<%# Bind("vfaxarea") %>' />
                            <asp:TextBox ID="vfaxTextBox" Width="80px" MaxLength="8" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" runat="server" Text='<%# Bind("vfax") %>' /></td></tr>
                            <tr><td align="right" style="padding-right:5px"><b>City:</b></td>
                            <td><asp:TextBox ID="vcityTextBox" Width="80px" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" runat="server" Text='<%# Bind("vcity") %>' />
                            <a href="#" tabindex="1" onClick="citylook(); return false"><asp:Image ID="Image2" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
                            <asp:TextBox ID="vstateTextBox" Width="30px" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" runat="server" Text='<%# Bind("vstate") %>' />
                            <a href="#" tabindex="1" onClick="statecodelook(1); return false"><asp:Image ID="Image3" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
                            <asp:TextBox ID="vzipTextBox" Width="80px" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" runat="server" Text='<%# Bind("vzip") %>' />
                            <a href="#" tabindex="1" onClick="zipcodelook(); return false"><asp:Image ID="Image6" runat="server" ImageUrl="images/lookup_icon.gif" /></a></td>
                            <td align="right" style="padding-right:5px"><b>Fax Prefix:</b></td>
                            <td><asp:TextBox ID="vfaxprefixTextBox" Width="30px" MaxLength="3" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" runat="server" Text='<%# Bind("vfaxprefix") %>' /> 
                            &nbsp; <b>Fax Country:</b>  <asp:TextBox ID="vfaxcountryTextBox" MaxLength="8" Width="100px" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" runat="server" Text='<%# Bind("vfaxcountry") %>' /></td></tr>
                            <tr><td><b>Country:</b></td>
                            <td><asp:TextBox ID="vcountryTextBox" Width="100px" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" runat="server" Text='<%# Bind("vcountry") %>' />
                            <asp:TextBox ID="vPostalTextBox" Width="100px" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" runat="server" Text='<%# Bind("vPostal") %>' /></td>
                            <td colspan="2"> <b>Overrun Percentage: &nbsp;<asp:TextBox ID="voverpctTextBox" MaxLength="8" Width="70px" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" runat="server" Text='<%# Bind("voverpct") %>' />
                            <asp:CompareValidator ID="CompareValidator5" ControlToValidate="voverpctTextBox" SetFocusOnError="true" Display="Dynamic" Operator="DataTypeCheck" Type="Double" runat="server" ErrorMessage="Enter Valid Value"></asp:CompareValidator>
                            Underrun Percentage: &nbsp; </b><asp:TextBox ID="vunderpctTextBox" Width="70px" MaxLength="8" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" runat="server" Text='<%# Bind("vunderpct") %>' />
                            <asp:CompareValidator ID="CompareValidator1" ControlToValidate="vunderpctTextBox" SetFocusOnError="true" Display="Dynamic" Operator="DataTypeCheck" Type="Double" runat="server" ErrorMessage="Enter Valid Value"></asp:CompareValidator></td></tr>
                            <tr><td align="right" style="padding-right:5px"><b>Tax ID#:</b></td>
                            <td><asp:TextBox ID="vtaxidTextBox" runat="server" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" Width="150px" Text='<%# Bind("vtaxid") %>' /></td>
                            <td align="right" style="padding-right:5px"><b>Default G/L#:</b></td>
                            <td><asp:TextBox ID="vactnumTextBox" Width="100px" MaxLength="25" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" runat="server" Text='<%# Bind("vactnum") %>' />
                            <a href="#" tabindex="1" onClick="AccountLook(); return false"><asp:Image ID="Image13" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
                            <asp:TextBox ID="vactdscrTextBox" runat="server" onfocus="onactdscr()" ForeColor="#ACA899" Width="150px" Text='<%# Bind("vactdscr") %>' /></td></tr>
                            </table>
                            </td></tr>
                            <tr><td>
                            <fieldset><table><tr ><td style="height:40px" colspan="2" ><b><asp:Label ID="remlabel" runat="server" Text="Remit to Address" ForeColor="Blue"></asp:Label></b></td></tr>
                            
                            <tr><td align="right" style="padding-right:5px"><b>Remit To:</b></td>
                            <td> <asp:TextBox ID="vremitTextBox" Width="180px" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" runat="server" Text='<%# Bind("vremit") %>' /></td></tr>
                            <tr><td align="right" style="padding-right:5px"><b>Address:</b></td>
                            <td><asp:TextBox ID="vradd1TextBox" runat="server" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" Width="150px" Text='<%# Bind("vradd1") %>' /></td></tr>
                            <tr><td></td><td><asp:TextBox ID="vradd2TextBox" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" Width="150px" runat="server" Text='<%# Bind("vradd2") %>' /></td></tr>
                            <tr><td align="right" style="padding-right:5px"><b>City:</b></td>
                            <td><asp:TextBox ID="vrcityTextBox" Width="80px" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" runat="server" Text='<%# Bind("vrcity") %>' />
                            <asp:TextBox ID="vrstateTextBox" Width="30px" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" runat="server" Text='<%# Bind("vrstate") %>' />
                            <a href="#" tabindex="1" onClick="statecodelook(2); return false"><asp:Image ID="Image10" runat="server" ImageUrl="images/lookup_icon.gif" /></a> 
                            <asp:TextBox ID="vrzipTextBox" Width="80px" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" runat="server" Text='<%# Bind("vrzip") %>' /></td></tr>
                            <tr><td align="right" style="padding-right:5px"><b>Country:</b></td>
                            <td><asp:TextBox ID="vrcountryTextBox" Width="80px" runat="server" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)"  Text='<%# Bind("vrcountry") %>' />
                            &nbsp; <b>Postal Code:</b>&nbsp;<asp:TextBox ID="vrpostalTextBox" Width="80px" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" runat="server" Text='<%# Bind("vrpostal") %>' /></td></tr>
                            <tr><td colspan="2"><b>Check Memo:</b>
                            <asp:TextBox ID="vcheckmemoTextBox" runat="server" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" Width="150px" Text='<%# Bind("vcheckmemo") %>' /></td></tr></table>
                            
                            </fieldset>
                            </td>
                            <td>
                            <fieldset><table>
                            <tr><td colspan="2"><b>Currency Code:</b> &nbsp; 
                            <asp:TextBox ID="vcurrcodeTextBox" runat="server" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" MaxLength="3" Width="100"  Text='<%# Bind("vcurrcode") %>' />
                            <a href="#" tabindex="1" onClick="currencylook(); return false"><asp:Image ID="Image12" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
                            <asp:TextBox ID="vcurrdscrTextBox" ForeColor="#ACA899" onfocus="oncurdscr()" Width="150" runat="server"  Text='<%# Bind("vcurrdscr") %>' /></td></tr>
                            <tr><td colspan="2"><b>Tax:</b>&nbsp; <asp:TextBox ID="vtaxgrTextBox" Width="50px" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" MaxLength="4" runat="server" Text='<%# Bind("vtaxgr") %>' />
                            <a href="#" tabindex="1" onClick="taxcodelook(); return false"><asp:Image ID="Image14" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
                            &nbsp;<b>1099 Code:&nbsp;</b><asp:TextBox ID="vcode1099TextBox" Width="50px" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" MaxLength="1" runat="server" Text='<%# Bind("vcode1099") %>' />
                            <asp:TextBox ID="vanedivendTextBox" Width="100px" Visible="false" runat="server" Text='<%# Bind("vanedivend") %>' />
                            <asp:CheckBox ID="CheckBox1" Text="EDI" runat="server" /></td></tr>
                            <tr><td align="right" style="padding-right:5px"><b>Terms:</b></td>
                            <td><asp:TextBox ID="vtermsTextBox" runat="server" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" MaxLength="5" Text='<%# Bind("vterms") %>' />
                            <a href="#" tabindex="1" onClick="termslook(); return false"><asp:Image ID="Image7" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
                            <asp:TextBox ID="vtermsdscrTextBox" runat="server" onfocus="ontermdscr()" ForeColor="#ACA899"  Text='<%# Bind("vtermsdscr") %>' /></td></tr>
                            <tr><td align="right" style="padding-right:5px"><b>Discount%:</b></td>
                            <td><asp:TextBox ID="vdiscTextBox" runat="server" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" Text='<%# Bind("vdisc") %>' />
                            <asp:CompareValidator ID="CompareValidator2" ControlToValidate="vdiscTextBox" SetFocusOnError="true" Display="Dynamic" Operator="DataTypeCheck" Type="Double" runat="server" ErrorMessage="Enter Valid Value"></asp:CompareValidator>
                            &nbsp;<b>PoExport:</b>&nbsp;<asp:TextBox ID="vpoexportTextBox" runat="server" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" Text='<%# Bind("vpoexport") %>' />
                            <a href="#" tabindex="1" onClick="Syspramcharlook(); return false"><asp:Image ID="syspramimage" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
                            </td></tr>
                            </table></fieldset>
                            <fieldset>
                            <table><tr><td><b> <asp:Label ID="Label1" runat="server" Text="Shipping Information" ForeColor="Blue"/>    MaxPoCost: &nbsp;&nbsp;&nbsp;</b>
                            <asp:TextBox ID="vrebateTextBox" Width="100px" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" runat="server" Text='<%# Bind("vrebate") %>' />
                            <asp:CompareValidator ID="CompareValidator3" ControlToValidate="vrebateTextBox" Display="Dynamic" SetFocusOnError="true" Operator="DataTypeCheck" Type="Double" runat="server" ErrorMessage="Enter Valid Value"></asp:CompareValidator></td>
                            <td rowspan="3"> <b>FOB:</b>
                                <asp:RadioButtonList ID="RadioButtonList1" DataTextField='<%# Bind("vfobcode") %>' runat="server" SelectedValue='<%# Bind("vfobcode") %>'>
                                <asp:ListItem Value="Dest" Text="Dest"></asp:ListItem>
                                <asp:ListItem Value="Orig" Text="Orig"></asp:ListItem>
                                <asp:ListItem Value="" Text="None"></asp:ListItem>
                                </asp:RadioButtonList>
                            </td></tr>
                            <tr><td><b>Freight Pay:&nbsp</b><asp:TextBox Width="70px" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" MaxLength="1" ID="vfrtpayTextBox" runat="server" Text='<%# Bind("vfrtpay") %>' />
                            &nbsp; <b>Lead Time Day:&nbsp;</b><asp:TextBox ID="vdiscdaysTextBox" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" Width="30px" MaxLength="2" runat="server" Text='<%# Bind("vdiscdays") %>' />
                            <asp:CompareValidator ID="CompareValidator4" ControlToValidate="vdiscdaysTextBox" Display="Dynamic" SetFocusOnError="true" Operator="DataTypeCheck" Type="Integer" runat="server" ErrorMessage="Enter Valid Value"></asp:CompareValidator></td></tr>
                            <tr><td><b>Carrier:&nbsp;</b><asp:TextBox ID="vcarrierTextBox" MaxLength="5" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" runat="server" Width="70px" Text='<%# Bind("vcarrier") %>' />
                            <a href="#" tabindex="1" onClick="carrierlook(); return false"><asp:Image ID="Image11" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
                            <asp:TextBox ID="vcarrierdscrTextBox" runat="server" onfocus="oncarrierdscr()" ForeColor="#ACA899" Width="180px" Text='<%# Bind("vcarrierdscr") %>' /></td></tr>
                            </table>
                            </fieldset>
                            </td></tr>
                            </table>
                            </fieldset>
                             <asp:TextBox ID="vreckeyTextBox" runat="server" Visible="false" Text='<%# Bind("vreckey") %>' />
                            <br />
                            <asp:Button ID="InsertButton" runat="server" CausesValidation="True" OnClick="addButton_Click"
                               CssClass="button"  Text="Save" />
                            &nbsp;<asp:Button ID="InsertCancelButton" runat="server" CssClass="button"
                                CausesValidation="False" CommandName="Cancel" Text="Cancel" />
                           </asp:Panel>
                        </InsertItemTemplate>
                        <ItemTemplate>
                            <asp:Panel ID="itemPanel" runat="server" DefaultButton="UpdateButton">
                            <fieldset class="shade">
                            <table><tr><td colspan="2">
                            <table>
                            <tr><td align="right" style="padding-right:5px"><b>Vendor:</b></td>
                            <td nowrap="nowrap"> <asp:Label ID="vendorLabel" BackColor="turquoise" Width="100px" runat="server" Text='<%# Bind("vendor") %>' />
                             <b>Status:</b><b>&nbsp;<asp:RadioButton ID="RD5" GroupName="acco" BackColor="turquoise" Enabled="false" runat="server" /><b>Active</b>
                                                        <asp:Label ID="activeLabel" Visible="false" runat="server" Enabled="false" Text='<%# Bind("vactive") %>'></asp:Label>
                                                        <asp:RadioButton ID="RD6"  GroupName="acco" BackColor="turquoise" Enabled="false" runat="server" />Inactive</b></td>
                            <td align="right" style="padding-right:5px"><b>Type:</b></td>
                            <td><asp:Label ID="vtypeLabel" BackColor="turquoise" Width="70px" MaxLength="9" runat="server" Text='<%# Bind("vtype") %>' />
                            <asp:Label ID="vtypedscrLabel" BackColor="turquoise" Width="160px" runat="server"   Text='<%# Bind("vtypedscr") %>' /></td></tr>
                            <tr><td align="right" style="padding-right:5px"><b>Name:</b></td>
                            <td><asp:Label ID="vnameLabel" Width="250px" BackColor="turquoise" runat="server" Text='<%# Bind("vname") %>' /></td>
                            <td align="right" style="padding-right:5px"><b>Contact:</b></td>
                            <td><asp:Label ID="vcontactLabel" runat="server" BackColor="turquoise" Width="230px"  Text='<%# Bind("vcontact") %>' /> </td></tr>
                            <tr><td align="right" style="padding-right:5px"><b>Address:</b></td>
                            <td><asp:Label ID="vadd1Label" runat="server" BackColor="turquoise" Width="150px" Text='<%# Bind("vadd1") %>' /></td>
                            <td align="right" style="padding-right:5px"><b>Buyer:</b></td>
                            <td><asp:Label ID="vbuyerLabel" Width="70px" BackColor="turquoise" MaxLength="3" runat="server" Text='<%# Bind("vbuyer") %>' />
                            <asp:Label ID="vbuyerdscrLabel" runat="server" Width="160px" BackColor="turquoise"  Text='<%# Bind("vbuyerdscr") %>' /></td></tr>
                            <tr><td></td><td><asp:Label ID="vadd2Label" Width="150px" BackColor="turquoise" runat="server" Text='<%# Bind("vadd2") %>' /></td>
                            <td align="right" style="padding-right:5px"><b>Phone:</b></td>
                            <td><asp:Label ID="vareacodeLabel" Width="30px" runat="server" BackColor="turquoise" Text='<%# Bind("vareacode") %>' />
                            <asp:Label ID="vphoneLabel" runat="server" BackColor="turquoise"  Width="80px" Text='<%# Bind("vphone") %>' />
                            &nbsp;<b>Fax:</b>&nbsp; <asp:Label ID="vfaxareaLabel" BackColor="turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" Width="30px" runat="server" Text='<%# Bind("vfaxarea") %>' />
                            <asp:Label ID="vfaxLabel" Width="80px" BackColor="turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" runat="server" Text='<%# Bind("vfax") %>' /></td></tr>
                            <tr><td align="right" style="padding-right:5px"><b>City:</b></td>
                            <td><asp:Label ID="vcityLabel" Width="80px" BackColor="turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" runat="server" Text='<%# Bind("vcity") %>' />
                            <asp:Label ID="vstateLabel" Width="30px" BackColor="turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" runat="server" Text='<%# Bind("vstate") %>' />
                            <asp:Label ID="vzipLabel" Width="80px" BackColor="turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" runat="server" Text='<%# Bind("vzip") %>' /></td>
                            <td align="right" style="padding-right:5px"><b>Fax Prefix:</b></td>
                            <td><asp:Label ID="vfaxprefixLabel" Width="30px" BackColor="turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" MaxLength="3" runat="server" Text='<%# Bind("vfaxprefix") %>' /> 
                            &nbsp; <b>Fax Country:</b>  <asp:Label ID="vfaxcountryLabel" BackColor="turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px"  Width="100px" runat="server" Text='<%# Bind("vfaxcountry") %>' /></td></tr>
                            <tr><td><b>Country:</b></td>
                            <td><asp:Label ID="vcountryLabel" Width="100px" BackColor="turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" runat="server" Text='<%# Bind("vcountry") %>' />
                            <asp:Label ID="vPostalLabel" Width="100px" runat="server" BackColor="turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" Text='<%# Bind("vPostal") %>' /></td>
                            <td colspan="2"> <b>Overrun Percentage: </b>&nbsp;<asp:Label ID="voverpctLabel" BackColor="turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" Width="70px" runat="server" Text='<%# Bind("voverpct") %>' />
                            <b>Underrun Percentage: &nbsp; </b><asp:Label ID="vunderpctLabel" Width="70px" BackColor="turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" runat="server" Text='<%# Bind("vunderpct") %>' /> </td></tr>
                            <tr><td align="right" style="padding-right:5px"><b>Tax ID#:</b></td>
                            <td><asp:Label ID="vtaxidLabel" runat="server" Width="150px" BackColor="turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" Text='<%# Bind("vtaxid") %>' /></td>
                            <td align="right" style="padding-right:5px"><b>Default G/L#:</b></td>
                            <td><asp:Label ID="vactnumLabel" Width="100px" BackColor="turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" runat="server" Text='<%# Bind("vactnum") %>' />
                            <asp:Label ID="vactdscrLabel" runat="server" BackColor="turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" Width="150px" Text='<%# Bind("vactdscr") %>' /></td></tr>
                            </table>
                            </td></tr>
                            <tr><td>
                            <fieldset><table><tr ><td style="height:40px" colspan="2" ><b>
                            <asp:Label ID="remlabel" runat="server" Text="Remit to Address" ForeColor="Blue"></asp:Label> </b></td></tr>
                            
                            <tr><td align="right" style="padding-right:5px"><b>Remit To:</b></td>
                            <td> <asp:Label ID="vremitLabel" Width="180px" BackColor="turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" runat="server" Text='<%# Bind("vremit") %>' /></td></tr>
                            <tr><td align="right" style="padding-right:5px"><b>Address:</b></td>
                            <td><asp:Label ID="vradd1Label" runat="server" BackColor="turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" Width="150px" Text='<%# Bind("vradd1") %>' /></td></tr>
                            <tr><td></td><td><asp:Label ID="vradd2Label" BackColor="turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" Width="150px" runat="server" Text='<%# Bind("vradd2") %>' /></td></tr>
                            <tr><td align="right" style="padding-right:5px"><b>City:</b></td>
                            <td><asp:Label ID="vrcityLabel" Width="80px" BackColor="turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px"  runat="server" Text='<%# Bind("vrcity") %>' />
                            <asp:Label ID="vrstateLabel" Width="30px" BackColor="turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" runat="server" Text='<%# Bind("vrstate") %>' />
                            <asp:Label ID="vrzipLabel" Width="80px" BackColor="turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" runat="server" Text='<%# Bind("vrzip") %>' /></td></tr>
                            <tr><td align="right" style="padding-right:5px"><b>Country:</b></td>
                            <td><asp:Label ID="vrcountryLabel" Width="80px" BackColor="turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" runat="server"  Text='<%# Bind("vrcountry") %>' />
                            &nbsp; <b>Postal Code:</b>&nbsp;<asp:Label ID="vrpostalLabel" Width="80px" BackColor="turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" runat="server" Text='<%# Bind("vrpostal") %>' /></td></tr>
                            <tr><td colspan="2"><br /><b>Check Memo:</b>
                            <asp:Label ID="vcheckmemoLabel" runat="server" Width="150px" BackColor="turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" Text='<%# Bind("vcheckmemo") %>' /></td></tr></table>
                            
                            </fieldset>
                            </td>
                            <td>
                            <fieldset><table>
                            <tr><td colspan="2"><b>Currency Code:</b> &nbsp; 
                            <asp:Label ID="vcurrcodeLabel" runat="server" Width="100px" BackColor="turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px"  Text='<%# Bind("vcurrcode") %>' />
                            <asp:Label ID="vcurrdscrLabel" runat="server" Width="120px" BackColor="turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" Text='<%# Bind("vcurrdscr") %>' /></td></tr>
                            <tr><td colspan="2"><b>Tax:</b>&nbsp; <asp:Label ID="vtaxgrLabel" Width="50px" runat="server" BackColor="turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" Text='<%# Bind("vtaxgr") %>' />
                            &nbsp;<b>1099 Code:</b>&nbsp;<asp:Label ID="vcode1099Label" Width="30px" BackColor="turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" runat="server" Text='<%# Bind("vcode1099") %>' />
                            <asp:Label ID="vanedivendLabel" Visible="false" BackColor="turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" Width="100px" runat="server" Text='<%# Bind("vanedivend") %>' />
                            <asp:CheckBox ID="CheckBox1" Enabled="false" BackColor="turquoise" Text="EDI" runat="server" /></td></tr>
                            <tr><td align="right" style="padding-right:5px"><b>Terms:</b></td>
                            <td><asp:Label ID="vtermsLabel" Width="100px" BackColor="turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" runat="server" Text='<%# Bind("vterms") %>' />
                            <asp:Label ID="vtermsdscrLabel" Width="120px" BackColor="turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" runat="server" Text='<%# Bind("vtermsdscr") %>' /></td></tr>
                            <tr><td align="right" style="padding-right:5px"><b>Discount%:</b></td>
                            <td><asp:Label ID="vdiscLabel" Width="100px" BackColor="turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" runat="server" Text='<%# Bind("vdisc") %>' />
                            &nbsp;<b>PoExport:</b>&nbsp;<asp:Label ID="vpoexportLabel" Width="120px" BackColor="turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" runat="server" Text='<%# Bind("vpoexport") %>' /></td></tr>
                            </table></fieldset>
                            <fieldset>
                            <table><tr><td><b> <asp:Label ID="Label1" runat="server" Text="Shipping Information" ForeColor="Blue"/>     MaxPoCost: &nbsp;&nbsp;&nbsp;</b>
                            <asp:Label ID="vrebateLabel" Width="100px" BackColor="turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px"  runat="server" Text='<%# Bind("vrebate") %>' />
                            </td>
                            <td rowspan="3"> <b>FOB:</b>
                                <asp:RadioButtonList ID="RadioButtonList1" DataTextField='<%# Bind("vfobcode") %>' Enabled="false" BackColor="turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" runat="server" SelectedValue='<%# Bind("vfobcode") %>'>
                                <asp:ListItem Value="Dest" Text="Dest"></asp:ListItem>
                                <asp:ListItem Value="Orig" Text="Orig"></asp:ListItem>
                                <asp:ListItem Value="" Text="None"></asp:ListItem>
                                </asp:RadioButtonList>
                            </td></tr>
                            <tr><td><b>Freight Pay:&nbsp</b><asp:Label Width="70px" BackColor="turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" ID="vfrtpayLabel" runat="server" Text='<%# Bind("vfrtpay") %>' />
                            &nbsp; <b>Lead Time Day:&nbsp;</b><asp:Label ID="vdiscdaysLabel" BackColor="turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" Width="30px" runat="server" Text='<%# Bind("vdiscdays") %>' /></td></tr>
                            <tr><td><b>Carrier:&nbsp;</b><asp:Label ID="vcarrierLabel"  runat="server" BackColor="turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" Width="70px" Text='<%# Bind("vcarrier") %>' />
                            <asp:Label ID="vcarrierdscrLabel" runat="server" BackColor="turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" Width="180px" Text='<%# Bind("vcarrierdscr") %>' /></td></tr>
                            </table>
                            </fieldset>
                            </td></tr>
                            </table>
                            </fieldset>
                            <asp:Label ID="vreckeyLabel" BackColor="turquoise" Visible="false" BorderColor="white" BorderStyle="solid" BorderWidth="1px" runat="server" Text='<%# Bind("vreckey") %>' />
                            <br />
                            
                    <asp:Button ID="AddButton" runat="server" CssClass="button"  CommandName="new" Text="Add" />
                    <asp:Button ID="UpdateButton" runat="server" CssClass="button" CommandName="edit" Text="Update" />
                   &nbsp;<asp:Button ID="DeleteButton" runat="server" CssClass="button" OnClientClick="return confirm('Are you sure you want to delete this record')" OnClick="Deletebutton_Click"   Text="Delete" />
                   </asp:Panel>
                        </ItemTemplate>
                    </asp:FormView>
  
          <asp:ObjectDataSource ID="ObjectDataSource1" runat="server" OldValuesParameterFormatString="original_{0}"
              SelectMethod="SelectVendorList" TypeName="voucherpay">
              <SelectParameters>
                  <asp:Parameter DefaultValue="View" Name="prmAction" Type="String" />
                  <asp:Parameter Name="prmUser" Type="String"  DefaultValue="" />
                  <asp:SessionParameter SessionField="vendor_list_vend_reckey" Name="prmReckey" Type="String" />
                  <asp:Parameter Name="prmactive" Type="String" />
                  <asp:Parameter Name="prmVendor" Type="String" />                  
                  <asp:Parameter Name="prmName" Type="String" />
                  <asp:Parameter Name="prmAdd1" Type="String" />
                  <asp:Parameter Name="prmAdd2" Type="String" />
                  <asp:Parameter Name="prmcity" Type="String" />
                  <asp:Parameter Name="prmstate" Type="String" />
                  <asp:Parameter Name="prmzip" Type="String" />
                  <asp:Parameter Name="prmCountry" Type="String" />
                  <asp:Parameter Name="prmPostal" Type="String" />
                  <asp:Parameter Name="prmtaxid" Type="String" />
                  <asp:Parameter Name="prmRemit" Type="String" />
                  <asp:Parameter Name="prmRadd1" Type="String" />
                  <asp:Parameter Name="prmRadd2" Type="String" />                 
                  <asp:Parameter Name="prmRcity" Type="String" />
                  <asp:Parameter Name="prmRstate" Type="String" />
                  <asp:Parameter Name="prmRzip" Type="String" />
                  <asp:Parameter Name="prmRcountry" Type="String" />
                  <asp:Parameter Name="prmRpostal" Type="String" />
                  <asp:Parameter Name="prmCheckmemo" Type="String" />
                  <asp:Parameter Name="prmtype" Type="String" />
                  <asp:Parameter Name="prmcontact" Type="String" />
                  <asp:Parameter Name="prmBuyer" Type="String" />
                  <asp:Parameter Name="prmareacode" Type="String" />
                  <asp:Parameter Name="prmphone" Type="String" />
                  <asp:Parameter Name="prmFaxarea" Type="String" />
                  <asp:Parameter Name="prmfax" Type="String" />
                  <asp:Parameter Name="prmfaxprefix" Type="String" />
                  <asp:Parameter Name="prmfaxcountry" Type="String" />
                  <asp:Parameter Name="prmoverpct" Type="Decimal" />
                  <asp:Parameter Name="prmunderpct" Type="Decimal" />
                  <asp:Parameter Name="prmActnum" Type="String" />
                  <asp:Parameter Name="prmcurrcode" Type="String" />
                  <asp:Parameter Name="prmtaxgr" Type="String" />
                  <asp:Parameter Name="prmCode1099" Type="String" />
                  <asp:Parameter Name="prmAnedivend" Type="String" />
                  <asp:Parameter Name="prmterms" Type="String" />
                  <asp:Parameter Name="prmdisc" Type="Decimal" />
                  <asp:Parameter Name="prmRebate" Type="Decimal" />
                  <asp:Parameter Name="prmfrtpay" Type="String" />
                  <asp:Parameter Name="prmDiscdays" Type="Int32" />
                  <asp:Parameter Name="prmcarrier" Type="String" />
                  <asp:Parameter Name="prmfobcode" Type="String" />
                  <asp:Parameter Name="prmTtypedscr" Type="String" />
                  <asp:Parameter Name="prmBuyerdscr" Type="String" />
                  <asp:Parameter Name="prmTermsdscr" Type="String" />
                  <asp:Parameter Name="prmCarrierdscr" Type="String" />
                  <asp:Parameter Name="prmCurrdscr" Type="String" />
                  <asp:Parameter Name="prmActdscr" Type="String" />
                  <asp:Parameter Name="prmPoexport" Type="String" />
              </SelectParameters>
          </asp:ObjectDataSource>
                                      
        
    </div></td></tr></table>
    <ft:footer id="Footer1" runat="server"></ft:footer>
    </form>
  </body>
</HTML>

