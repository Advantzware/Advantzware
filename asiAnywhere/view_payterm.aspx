<%@ Page Language="c#" AutoEventWireup="true" Debug="true" Inherits="view_payterm" Codebehind="view_payterm.aspx.cs" %>

<%@ Register Src="footer.ascx" TagName="Footer" TagPrefix="ft" %>
<%@ Register Src="header.ascx" TagName="Header" TagPrefix="hd" %>
<html xmlns="http://www.w3.org/1999/xhtml" >
  <head id="Head1" runat="server">
    <title>Terms</title>
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
    
function contactcustomerlook()
{ 
  var NewWindow = window.open("contact_customer_lookup.aspx","ContactCustomerLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}

function ContactCustomerLookup(ReturnObj1,ReturnObj2,ReturnObj3,ReturnObj4,ReturnObj5,ReturnObj6,ReturnObj7,ReturnObj8,ReturnObj9, ReturnObj10,ReturnObj11)
{ 
  document.forms[0].FormView1_vcustnoTextBox.value = ReturnObj1;
}

function focusval(obj) {
    obj.style.backgroundColor = 'blue';
    obj.style.color = 'white';
}
function blurval(obj) {
    obj.style.backgroundColor = 'Window';
    obj.style.color = 'WindowText';
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
    var comm = document.getElementById("FormView1_disc_rateTextBox").value;
        if(comm.indexOf(".") != -1)
            {        
                return;
            } 
        else if(comm.length > 1 && comm.length < 2)
        comm=comm + ".";
    document.getElementById("FormView1_disc_rateTextBox").value = comm;
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
                            <TD align=center nowrap><font size=+0><b>Terms&nbsp;</b></font></TD>
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
                                <asp:LinkButton ID="lnk_Listvend" runat="server" OnClick="lnk_Listvend_Click" >Browse Terms</asp:LinkButton></li>
                                <li class="selected"><asp:LinkButton ID="lnk_viewvend" runat="server"  OnClick="lnk_viewvend_Click"  > View Terms</asp:LinkButton></li></ul></div>
                                
                                
                            </td>      
                        </tr>
                   </table>
            <asp:HiddenField ID="HiddenField1" runat="server" />
            <asp:HiddenField ID="HiddenField2" runat="server" />
            
            
                    <asp:FormView ID="FormView1" runat="server" OnDataBound="FormView1_DataBound" OnUnload="formview1_onunload" DataSourceID="ObjectDataSource1">
                        <EditItemTemplate>
                            <asp:Panel ID="editpanel" runat="server" CssClass="shade" DefaultButton="UpdateButton">
                            <fieldset><table>
                            <tr><td align="right" style="padding-right:5px"><b>Terms:&nbsp;</b><asp:TextBox ID="tcodeTextBox" BackColor="turquoise" ReadOnly="true" Width="100px" MaxLength="5" runat="server" Text='<%# Bind("tcode") %>' /> </td>
                            <td align="right" style="padding-right:5px"><b>Description:</b></td>
                            <td colspan="4"><asp:TextBox ID="dscrTextBox" Width="220px" MaxLength="30" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" runat="server" Text='<%# Bind("dscr") %>' /></td></tr>
                            <tr><td ></td><td align="right" style="padding-right:5px"><b>Disc%:</b></td>
                            <td><asp:TextBox ID="disc_rateTextBox" Width="40px" MaxLength="4" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" onkeyup="flatcomm()" runat="server" Text='<%# Bind("disc_rate") %>' />
                            <asp:CompareValidator ID="CompareValidator4" ControlToValidate="disc_rateTextBox" Display="Dynamic" SetFocusOnError="true" Operator="DataTypeCheck" Type="Double" runat="server" ErrorMessage="Enter Valid Value"></asp:CompareValidator></td>
                            <td align="right" style="padding-right:5px"><b>Day:</b></td>
                            <td><asp:TextBox ID="disc_dysTextBox" runat="server" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" Width="40px" MaxLength="2" Text='<%# Bind("disc_dys") %>' />
                            <asp:CompareValidator ID="CompareValidator1" ControlToValidate="disc_dysTextBox" Display="Dynamic" SetFocusOnError="true" Operator="DataTypeCheck" Type="Integer" runat="server" ErrorMessage="Enter Valid Value"></asp:CompareValidator></td>
                            <td align="right" style="padding-right:5px"><b>Net: &nbsp;</b> <asp:TextBox ID="net_dysTextBox" Width="40px" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" MaxLength="2" runat="server" Text='<%# Bind("net_dys") %>' /> 
                            <asp:CompareValidator ID="CompareValidator2" ControlToValidate="net_dysTextBox" Display="Dynamic" SetFocusOnError="true" Operator="DataTypeCheck" Type="Integer" runat="server" ErrorMessage="Enter Valid Value"></asp:CompareValidator></td></tr>
                            <tr><td></td><td align="right" style="padding-right:5px"><b>Cutoff</b></td>
                            <td><asp:TextBox ID="CUT_dateTextBox" runat="server" Width="40px" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" MaxLength="3" Text='<%# Bind("CUT_date") %>' />
                            <asp:CompareValidator ID="CompareValidator3" ControlToValidate="CUT_dateTextBox" Display="Dynamic" SetFocusOnError="true" Operator="DataTypeCheck" Type="Integer" runat="server" ErrorMessage="Enter Valid Value"></asp:CompareValidator></td>
                            <td align="right" style="padding-right:5px"><b>Type:</b></td>
                            <td><asp:TextBox ID="vtypeTextBox" Width="40px" MaxLength="1" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" ToolTip="The valid Types are (P)roximo or (S)tandard" runat="server" Text='<%# Bind("vtype") %>' /></td>
                            <td align="center"><asp:TextBox ID="codTextBox" runat="server" Visible="false" Width="20px"  Text='<%# Bind("cod") %>' />
                            <asp:CheckBox ID="CheckBox1" Text="COD" runat="server" /> 
                            </td></tr>
                            </table><br /></fieldset>
                            
                            <asp:TextBox ID="vreckeyTextBox" Visible="false"  runat="server" Text='<%# Bind("vreckey") %>' />
                            
                            <asp:Button ID="UpdateButton" runat="server" CausesValidation="True" CssClass="button" OnClick="UpdateButton_Click"
                                Text="Save" />
                            &nbsp;<asp:Button ID="UpdateCancelButton" runat="server" CssClass="button"
                                CausesValidation="False" CommandName="Cancel" Text="Cancel" />
                                </asp:Panel>
                        </EditItemTemplate>
                        <InsertItemTemplate>
                            <asp:Panel ID="editpanel" runat="server" CssClass="shade" DefaultButton="InsertButton">
                            <fieldset><table>
                            <tr><td align="right" style="padding-right:5px"><b>Terms:&nbsp;</b><asp:TextBox ID="tcodeTextBox" Width="100px" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" MaxLength="5" runat="server" Text='<%# Bind("tcode") %>' /> </td>
                            <td align="right" style="padding-right:5px"><b>Description:</b></td>
                            <td colspan="4"><asp:TextBox ID="dscrTextBox" Width="220px" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" MaxLength="30" runat="server" Text='<%# Bind("dscr") %>' /></td></tr>
                            <tr><td ></td><td align="right" style="padding-right:5px"><b>Disc%:</b></td>
                            <td><asp:TextBox ID="disc_rateTextBox" Width="40px" MaxLength="4" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" onkeyup="flatcomm()" runat="server" Text='<%# Bind("disc_rate") %>' />
                            <asp:CompareValidator ID="CompareValidator4" ControlToValidate="disc_rateTextBox" Display="Dynamic" SetFocusOnError="true" Operator="DataTypeCheck" Type="Double" runat="server" ErrorMessage="Enter Valid Value"></asp:CompareValidator></td>
                            <td align="right" style="padding-right:5px"><b>Day:</b></td>
                            <td><asp:TextBox ID="disc_dysTextBox" runat="server" Width="40px" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" MaxLength="2" Text='<%# Bind("disc_dys") %>' />
                            <asp:CompareValidator ID="CompareValidator1" ControlToValidate="disc_dysTextBox" Display="Dynamic" SetFocusOnError="true" Operator="DataTypeCheck" Type="Integer" runat="server" ErrorMessage="Enter Valid Value"></asp:CompareValidator></td>
                            <td align="right" style="padding-right:5px"><b>Net: &nbsp;</b> <asp:TextBox ID="net_dysTextBox" Width="40px" MaxLength="2" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" runat="server" Text='<%# Bind("net_dys") %>' /> 
                            <asp:CompareValidator ID="CompareValidator2" ControlToValidate="net_dysTextBox" Display="Dynamic" SetFocusOnError="true" Operator="DataTypeCheck" Type="Integer" runat="server" ErrorMessage="Enter Valid Value"></asp:CompareValidator></td></tr>
                            <tr><td></td><td align="right" style="padding-right:5px"><b>Cutoff</b></td>
                            <td><asp:TextBox ID="CUT_dateTextBox" runat="server" Width="40px" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" MaxLength="3" Text='<%# Bind("CUT_date") %>' />
                            <asp:CompareValidator ID="CompareValidator3" ControlToValidate="CUT_dateTextBox" Display="Dynamic" SetFocusOnError="true" Operator="DataTypeCheck" Type="Integer" runat="server" ErrorMessage="Enter Valid Value"></asp:CompareValidator></td>
                            <td align="right" style="padding-right:5px"><b>Type:</b></td>
                            <td><asp:TextBox ID="vtypeTextBox" Width="40px" MaxLength="1" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" ToolTip="The valid Types are (P)roximo or (S)tandard" runat="server" Text='<%# Bind("vtype") %>' /></td>
                            <td align="center"><asp:TextBox ID="codTextBox" runat="server"  Visible="false" Width="20px"  Text='<%# Bind("cod") %>' />
                            <asp:CheckBox ID="CheckBox1" Text="COD" runat="server" /></td></tr>
                            </table><br /></fieldset>
                            
                            <asp:TextBox ID="vreckeyTextBox" Visible="false"  runat="server" Text='<%# Bind("vreckey") %>' />
                            
                            <asp:Button ID="InsertButton" runat="server" CausesValidation="True" CssClass="button"
                              OnClick="addButton_Click"  Text="Save" />
                            &nbsp;<asp:Button ID="InsertCancelButton" runat="server" CssClass="button"
                                CausesValidation="False" CommandName="Cancel" Text="Cancel" />
                               </asp:Panel>
                        </InsertItemTemplate>
                        <ItemTemplate>
                            <asp:Panel ID="editpanel" runat="server" CssClass="shade" DefaultButton="UpdateButton">
                            <fieldset><table>
                            <tr><td ><b>Terms:&nbsp;</b><asp:Label ID="tcodeLabel" Width="100px" BackColor="turquoise" runat="server" Text='<%# Bind("tcode") %>' /> </td>
                            <td align="right" style="padding-right:5px"><b>Description:</b></td>
                            <td colspan="4"><asp:Label ID="dscrLabel" Width="200px" BackColor="turquoise" runat="server" Text='<%# Bind("dscr") %>' /></td></tr>
                            <tr><td></td><td align="right" style="padding-right:5px"><b>Disc%:</b></td>
                            <td><asp:Label ID="disc_rateLabel" runat="server" Width="40px" BackColor="turquoise" Text='<%# Bind("disc_rate") %>' /></td>
                            <td align="right" style="padding-right:5px"><b>Day:</b></td>
                            <td><asp:Label ID="disc_dysLabel" runat="server" Width="40px" BackColor="turquoise" Text='<%# Bind("disc_dys") %>' /></td>
                            <td ><b>Net: &nbsp;</b> <asp:Label ID="net_dysLabel" Width="40px" BackColor="turquoise" runat="server" Text='<%# Bind("net_dys") %>' /> </td></tr>
                            <tr><td></td><td align="right" style="padding-right:5px"><b>Cutoff</b></td>
                            <td><asp:Label ID="CUT_dateLabel" runat="server" Width="40px" BackColor="turquoise" Text='<%# Bind("CUT_date") %>' /></td>
                            <td align="right" style="padding-right:5px"><b>Type:</b></td>
                            <td><asp:Label ID="vtypeLabel" runat="server" Width="40px" BackColor="turquoise" Text='<%# Bind("vtype") %>' /></td>
                            <td align="center"><asp:Label ID="codLabel" runat="server" Width="40px" Visible="false" BackColor="turquoise" Text='<%# Bind("cod") %>' />
                            <asp:CheckBox ID="CheckBox1" Text="COD" Enabled="false" BackColor="turquoise" runat="server" /> </td></tr>
                            </table><br /></fieldset>
                            <asp:Label ID="vreckeyLabel" runat="server" Visible="false" BackColor="turquoise" Text='<%# Bind("vreckey") %>' />
                           
                             <asp:Button ID="AddButton" runat="server" CssClass="button"  CommandName="new" Text="Add" />
                    <asp:Button ID="UpdateButton" runat="server" CssClass="button" CommandName="edit" Text="Update" />
                   &nbsp;<asp:Button ID="DeleteButton" runat="server" CssClass="button" OnClientClick="return confirm('Are you sure you want to delete this record')" OnClick="Deletebutton_Click"   Text="Delete" />
                            </asp:Panel>
                        </ItemTemplate>
                    </asp:FormView>
  
          <asp:ObjectDataSource ID="ObjectDataSource1" runat="server" OldValuesParameterFormatString="original_{0}"
              SelectMethod="SelectPaymentTerms" TypeName="voucherpay">
              <SelectParameters>
                  <asp:Parameter DefaultValue="View" Name="prmAction" Type="String" />
                  <asp:Parameter Name="prmUser" Type="String"  DefaultValue="" />
                  <asp:SessionParameter SessionField="payterms_list_vend_reckey" Name="prmReckey" 
                      Type="String" />
                  <asp:Parameter Name="prmtcode" Type="String" />
                  <asp:Parameter Name="prmdscr" Type="String" />                  
                  <asp:Parameter Name="prmdisc_rate" Type="Decimal" />
                  <asp:Parameter Name="prmdisc_dys" Type="Int32" />
                  <asp:Parameter Name="prmnet_dys" Type="Int32" />
                  <asp:Parameter Name="prmCUT_date" Type="Int32" />
                  <asp:Parameter Name="prmvtype" Type="String" />
                  <asp:Parameter Name="prmcod" Type="String" />
              </SelectParameters>
          </asp:ObjectDataSource>
                                      
        
    </div></td></tr></table>
    <ft:footer id="Footer1" runat="server"></ft:footer>
    </form>
  </body>
</HTML>

