<%@ Page Language="c#" AutoEventWireup="true" Debug="true" Inherits="customers_viewlist" Codebehind="cust_viewlist.aspx.cs" %>

<%@ Register Src="footer.ascx" TagName="Footer" TagPrefix="ft" %>
<%@ Register Src="header.ascx" TagName="Header" TagPrefix="hd" %>
<html xmlns="http://www.w3.org/1999/xhtml" >
  <head id="Head1" runat="server">
    <title>Customers</title>
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
  
function zipcodelook()
{ 
  var NewWindow = window.open("zipcode_lookup.aspx","ZipCodeLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}

function ZipCodeLookup(ReturnObj1, ReturnObj2, ReturnObj3)
{ 
   document.forms[0].FormView1_vcityTextBox.value = ReturnObj2;
   document.forms[0].FormView1_vzipTextBox.value = ReturnObj1;
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
function statecodelook()
{ 
  var NewWindow = window.open("statecode_lookup.aspx","StateCodeLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}

function StateCodeLookup(ReturnObj1)
{ 
  document.forms[0].FormView1_vstateTextBox.value = ReturnObj1;
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

function termslook()
{ 
  var NewWindow = window.open("terms_lookup.aspx","termsLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}

function termsLookup(ReturnObj1,ReturnObj2)
{ 
  document.forms[0].FormView1_vtermsTextBox.value = ReturnObj1;
  document.forms[0].FormView1_vdesctermsTextBox.value = ReturnObj2;
}
  
function typelook()
{ 
  var NewWindow = window.open("type_lookup.aspx","typeLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}
function TypeLookup(ReturnObj1, ReturnObj2)
{ 
  document.forms[0].FormView1_vtypeTextBox.value = ReturnObj1;
  document.forms[0].FormView1_vdesctypeTextBox.value = ReturnObj2;
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
  document.forms[0].FormView1_vdescarrierTextBox.value = ReturnObj2;
 }
 
function currencylook()
{ 
  var NewWindow = window.open("currency_lookup.aspx","CurrencyLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}
function CurrencyLookup(ReturnObj1, ReturnObj2)
{ 
  document.forms[0].FormView1_vcurrcodeTextBox.value = ReturnObj1;  
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
function TaxLookup(ReturnObj1, ReturnObj2)
{ 
  document.forms[0].FormView1_vtaxgrTextBox.value = ReturnObj1;
  document.forms[0].FormView1_vfobcodeTextBox.value = ReturnObj2;
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
                <div>
            
                    <TABLE id="tblTop" cellSpacing="3" align="center" border="0" Width="100%">
                        <TR>
                             <TD width=30>&nbsp;</TD>
                            <TD align=center nowrap><font size=+0><b>Customers&nbsp;</b></font></TD>
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
		                        <ul nowrap> <li  >
                                <asp:LinkButton ID="lnk_Listcustomers" runat="server" OnClick="lnk_Listcustomers_Click" >List Customers</asp:LinkButton></li>
                                <li class="selected"><asp:LinkButton ID="lnk_viewcustomers" runat="server"  OnClick="lnk_viewcustomers_Click"  >View Customers</asp:LinkButton></li>
                                <li><asp:LinkButton ID="lnk_listship" runat="server" OnClick="lnk_listship_Click" >List Ship To</asp:LinkButton></li>
                                <li><asp:LinkButton ID="lnk_viewship" runat="server"  OnClick="lnk_viewship_Click"  >View Ship To</asp:LinkButton></li>
                                <li><asp:LinkButton ID="lnk_listsold" runat="server" OnClick="lnk_listsold_Click" >List Sold To</asp:LinkButton></li>
                                <li><asp:LinkButton ID="lnk_viewsold" runat="server" OnClick="lnk_viewsold_Click" >View Sold TO</asp:LinkButton></li></ul></div>
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
            
            <asp:FormView ID="FormView1" runat="server" DataSourceID="ObjectDataSource1" OnDataBound="FormView1_DataBound" >
                <EditItemTemplate>
                  <asp:Panel ID="update_panel" runat="server" DefaultButton="UpdateButton">
                   <table width="950px" class="shade" >               
                        <tr>
                            <td>
                                <table  class="shade"  style="border:solid 1px black; height:165px;" width="460px">
                                    <tr>
                                        <td align="right" style="padding-right:5px" ><b>Customer:</b></td>
                                        <td><asp:Label ID="vcustnoLabel" BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" runat="server" Text='<%# Bind("vcustno") %>'>  </asp:Label>
                                        &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
                                        <b>Status:</b> 
                                            <asp:DropDownList ID="DropDownList2" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" Height="1px"  Width="75px" runat="server" SelectedValue = '<%# Bind("vactive") %>' >
                                                <asp:ListItem Value="A">Active </asp:ListItem>
                                                <asp:ListItem Value="I">Inactive</asp:ListItem>
                                                <asp:ListItem Value="X">Inhouse</asp:ListItem>
                                                <asp:ListItem Value="S">Statement</asp:ListItem>
                                                <asp:ListItem Value="E">Service</asp:ListItem>                   
                                            </asp:DropDownList>
                                        </td>
                                    </tr> 
                                    <tr>
                                        <td align="right" style="padding-right:5px"> <b>Name:</b></td>
                                        <td>
                                            <asp:TextBox ID="vcustnameTextBox" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" runat="server" Text='<%# Bind("vcustname") %>'></asp:TextBox>
                                        </td>
                                    <tr>
                                        <td align="right" style="padding-right:5px" > <b>Address:</b></td>
                                        <td>
                                            <asp:TextBox ID="vaddr1TextBox" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" runat="server" Text='<%# Bind("vaddr1") %>'></asp:TextBox>
                                        </td>
                                    <tr>
                                        <td align="right" style="padding-right:5px" > <b>Address:</b></td>
                                        <td>
                                            <asp:TextBox ID="vaddr2TextBox" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" runat="server" Text='<%# Bind("vaddr2") %>'></asp:TextBox>
                                        </td>
                                    <tr>
                                        <td align="right" style="padding-right:5px" ><b>City:</b></td>
                                        <td>
                                            <asp:TextBox ID="vcityTextBox" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" Width="70px" runat="server" Text='<%# Bind("vcity") %>'></asp:TextBox>
                                            <a href="#" tabindex="1" onClick="citylook(); return false"><asp:Image ID="Image9" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
                                             &nbsp; State:
                                            <asp:TextBox ID="vstateTextBox" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" width="50px" runat="server" Text='<%# Bind("vstate") %>'></asp:TextBox>
                                            <a href="#" tabindex="1" onClick="statecodelook(); return false"><asp:Image ID="Image4" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
                                            &nbsp; Zip:
                                            <asp:TextBox ID="vzipTextBox" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" width="70px" runat="server" Text='<%# Bind("vzip") %>'></asp:TextBox>
                                            <a href="#" tabindex="1" onClick="zipcodelook(); return false"><asp:Image ID="Image1" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
                                        </td>
                                    </tr>
                                    <tr>
                                        <td align="right" style="padding-right:5px"><b>Email:</b></td>
                                        <td>
                                            <asp:TextBox ID="vemailTextBox" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" MaxLength="50" Width="250px" runat="server" Text='<%# Bind("vemail") %>'></asp:TextBox>
                                            <asp:RegularExpressionValidator ID="RegularExpressionValidator23"  ControlToValidate="vemailTextBox" runat="server"
                                                    ErrorMessage="You must enter an email address" ValidationExpression="\w+([-+.']\w+)*@\w+([-.]\w+)*\.\w+([-.]\w+)*"> </asp:RegularExpressionValidator>
                                        </td>
                                    </tr>
                                </table>
                            </td>
                            <td>
                                <table class="shade"  style="border:solid 1px black; height:165px;" width="550px">
                                    <tr>
                                        <td align="right" style="padding-right:5px"><b>Date Added:</b></td>
                                        <td colspan="2" nowrap>
                                            <asp:TextBox ID="vdate1TextBox" Width="60px" MaxLength="10" ToolTip="MM/DD/YYYY" onfocus="javascript:preEnter( this, 'yes' );" onblur="javascript:preLeave( this, 'date', '99/99/9999' );" runat="server" Text='<%# Bind("vdate1","{0:MM/dd/yyyy}") %>'></asp:TextBox>
                                            <a href="#" tabindex="1" onClick="showCalendarControl(FormView1_vdate1TextBox); return false"><asp:Image ID="Image15" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
                                            
                                        </td>
                                    </tr> 
                                    <tr>
                                        <td  align="right" style="padding-right:5px" ><b>Type:</b></td>
                                        <td colspan="2">
                                            <asp:TextBox ID="vtypeTextBox" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" width="80px" runat="server" Text='<%# Bind("vtype") %>'></asp:TextBox>
                                            <a href="#" tabindex="1" onClick="typelook(); return false"><asp:Image ID="Image8" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
                                            <asp:TextBox ID="vdesctypeTextBox" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" width="100px" runat="server" Text='<%# Bind("vdesctype") %>'></asp:TextBox>
                                        </td>
                                    </tr>              
                                    <tr>
                                        <td align="right" style="padding-right:5px" ><b>Contact:</b></td>
                                        <td colspan="2">
                                            <asp:TextBox ID="vcontactTextBox" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" runat="server" Text='<%# Bind("vcontact") %>'></asp:TextBox>
                                        </td>
                                    </tr>
                                    <tr>
                                        <td align="right"  style="padding-right:5px">  <b>Sales Rep:</b></td>
                                        <td colspan="2">
                                            <asp:TextBox ID="vsmanTextBox" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" width="50px" runat="server" Text='<%# Bind("vsman") %>'></asp:TextBox>
                                            <a href="#" tabindex="1" onClick="salesreplook(); return false"><asp:Image ID="Image6" runat="server" ImageUrl="images/lookup_icon.gif" /></a> 
                                            <asp:TextBox ID="vdescsmanTextBox" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" width="80px" runat="server" Text='<%# Bind("vdescsman") %>'></asp:TextBox>
                                        &nbsp;&nbsp;&nbsp;&nbsp;
                                        <b>Flat Comm:</b>
                                            <asp:TextBox ID="vflatcommTextBox" MaxLength="8" onkeyup="flatcomm()" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" width="50px" runat="server" Text='<%# Bind("vflatcomm") %>'></asp:TextBox>
                                            <asp:RegularExpressionValidator ID="RegularExpressionValidator8" runat="server" SetFocusOnError="true" ControlToValidate="vflatcommTextBox" ErrorMessage="Invalid Flat Comm." Display="Dynamic" ValidationExpression="(^([0-9]*|\d*\d{1}.*?\d*)$)"></asp:RegularExpressionValidator>
                                        </td>
                                    </tr>
                                    <tr>
                                        <td align="right" style="padding-right:5px"><b>Phone:</b></td>
                                        <td colspan="2"> 
                                            <asp:TextBox ID="vareacodeTextBox" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" width="50px" MaxLength ="3" runat="server" Text='<%# Bind("vareacode") %>'></asp:TextBox>
                                            <%--<asp:RegularExpressionValidator ID="RegularExpressionValidator2" runat="server" SetFocusOnError="true" ControlToValidate="vareacodeTextBox" ErrorMessage="Invalid Phone Code." Display="Dynamic" ValidationExpression="(^([0-9]*|\d*\d{1}?\d*)$)"></asp:RegularExpressionValidator>               --%>
                                            <asp:TextBox ID="vphoneTextBox" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" width="80px" MaxLength="8" runat="server" Text='<%# Bind("vphone") %>'></asp:TextBox>
                                            <%--<asp:RegularExpressionValidator ID="RegularExpressionValidator1" runat="server" SetFocusOnError="true" ControlToValidate="vphoneTextBox" ErrorMessage="Invalid Phone No." Display="Dynamic" ValidationExpression="(^([0-9]*|\d*\d{1}?\d*)$)"></asp:RegularExpressionValidator>--%>
                                        </td>
                                    </tr>
                                    <tr>
                                        <td align="right" style="padding-right:5px"><b>Fax:</b></td>
                                        <td colspan="2"> 
                                            <asp:TextBox ID="vfaxcodeTextBox" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" width="50px" MaxLength="3" runat="server" Text='<%# Bind("vfaxcode") %>'></asp:TextBox>
                                            <asp:RegularExpressionValidator ID="RegularExpressionValidator3" runat="server" SetFocusOnError="true" ControlToValidate="vfaxcodeTextBox" ErrorMessage="Invalid Fax Code." Display="Dynamic" ValidationExpression="(^([0-9]*|\d*\d{1}?\d*)$)"></asp:RegularExpressionValidator>
                                            <asp:TextBox ID="vfaxTextBox" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" width="80px" MaxLength="8" runat="server" Text='<%# Bind("vfax") %>'></asp:TextBox>
                                            <asp:RegularExpressionValidator ID="RegularExpressionValidator4" runat="server" SetFocusOnError="true" ControlToValidate="vfaxTextBox" ErrorMessage="Invalid Fax No." Display="Dynamic" ValidationExpression="(^([0-9]*|\d*\d{1}?\d*)$)"></asp:RegularExpressionValidator>
                                       &nbsp;&nbsp;&nbsp;
                                       <b>Prefix:</b>
                                            <asp:TextBox ID="vfaxprefixTextBox" MaxLength="3" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" width="50px"  runat="server" Text='<%# Bind("vfaxprefix") %>'></asp:TextBox>
                                          &nbsp;  <b>Country:</b>
                                            <asp:TextBox ID="vfaxcountryTextBox" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" width="80px" runat="server" Text='<%# Bind("vfaxcountry") %>'></asp:TextBox>
                                       </td>
                                   </tr>
                            </table>
                        </td>
                    </tr>              
                    <tr>
                        <td>
                            <table class="shade" width="400px" style="border:solid 1px black; height:200px;" > 
                                <legend style="color:Blue">Credit Information</legend>
                                    <tr>
                                        <td align="right" style="padding-right:5px" > <b>Terms:</b></td>
                                        <td nowrap>
                                            <asp:TextBox ID="vtermsTextBox" MaxLength="5" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" Width="80px" runat="server" Text='<%# Bind("vterms") %>'></asp:TextBox>
                                            <a href="#" tabindex="1" onClick="termslook(); return false"><asp:Image ID="Image7" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
                                        </td>
                                        <td>
                                            <asp:TextBox ID="vdesctermsTextBox" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" Width="130px" runat="server" Text='<%# Bind("vdescterms") %>'></asp:TextBox>
                                        </td>
                                    </tr>
                                    <tr>
                                        <td align="right" style="padding-right:5px" ><b>Cr.Acct #:</b></td>
                                        <td>
                                            <asp:TextBox ID="vcruseTextBox" MaxLength="10" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" Width="80px" runat="server" Text='<%# Bind("vcruse") %>'></asp:TextBox>
                                        </td>
                                        <td nowrap ><b>Grace Days:</b>
                                            <asp:TextBox ID="vcrholdinvdaysTextBox" MaxLength="3" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" Width="40px" runat="server" Text='<%# Bind("vcrholdinvdays") %>'></asp:TextBox>
                                            <asp:RegularExpressionValidator ID="RegularExpressionValidator21" runat="server" SetFocusOnError="true" ControlToValidate="vcrholdinvdaysTextBox" ErrorMessage="Invalid grace days." Display="Dynamic" ValidationExpression="(^([0-9]*|\d*\d{1}?\d*)$)"></asp:RegularExpressionValidator>
                                        </td>
                                        <td nowrap><b>Dollars:</b>
                                            <asp:TextBox ID="vcrholdinvdueTextBox" MaxLength="10" onkeyup="empday()" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" ToolTip="Enter the integer or decimal value"  Width="90px" runat="server" Text='<%# Bind("vcrholdinvdue") %>'></asp:TextBox>
                                            <asp:RegularExpressionValidator ID="RegularExpressionValidator16" runat="server" SetFocusOnError="true" ControlToValidate="vcrholdinvdueTextBox" ErrorMessage="Invalid value." Display="Dynamic" ValidationExpression="(^([0-9]*|\d*\d{1}.*?\d*)$)"></asp:RegularExpressionValidator>
                                        </td>
                                     </tr>
                                    <tr>
                                        <td align="right" style="padding-right:5px" nowrap ><b>Credit Rating:</b></td>
                                        <td>
                                            <asp:TextBox ID="vcrratingTextBox" MaxLength="3" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" Width="40px" runat="server" Text='<%# Bind("vcrrating") %>'></asp:TextBox>
                                        </td>
                                        <td nowrap>
                                            <b>Price Level:</b>
                                            <asp:TextBox ID="vcustlevelTextBox" MaxLength="3" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" Width="40px" runat="server" Text='<%# Bind("vcustlevel") %>'></asp:TextBox>
                                            <asp:RegularExpressionValidator ID="RegularExpressionValidator19" runat="server" SetFocusOnError="true" ControlToValidate="vcustlevelTextBox" ErrorMessage="Invalid Price Level." Display="Dynamic" ValidationExpression="(^([0-9]*|\d*\d{1}?\d*)$)"></asp:RegularExpressionValidator>
                                        </td>
                                        <td nowrap >
                                            <b>Credit Hold:</b>
                                                <asp:CheckBox ID="vcrholdCheckBox" Width="10px" runat="server" Checked='<%# Bind("vcrhold") %>' />
                                        </td>
                                    </tr>
                                     <tr>
                                        <td align="right" style="padding-right:5px" nowrap ><b>Credit Limit:</b></td>
                                        <td>
                                            <asp:TextBox ID="vcrlimtTextBox" MaxLength="12" Width="80px" onkeyup="crlimit()" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" runat="server" Text='<%# Bind("vcrlim") %>'></asp:TextBox>
                                            <asp:RegularExpressionValidator ID="RegularExpressionValidator24" runat="server" SetFocusOnError="true" ControlToValidate="vcrlimtTextBox" ErrorMessage="Invalid Order Limit." Display="Dynamic" ValidationExpression="(^([0-9]*|\d*\d{1}.*?\d*)$)"></asp:RegularExpressionValidator>
                                        </td>
                                        <td nowrap><b>Finance Charges:</b>
                                            <asp:CheckBox ID="vfinchgCheckBox" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" Width="10px" runat="server" Checked='<%# Bind("vfinchg") %>' />
                                        </td>
                                        <td nowrap><b>Auto Reprice:</b>
                                            <asp:CheckBox ID="vautorepriceCheckBox" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" Width="10px" runat="server" Checked='<%# Bind("vautoreprice") %>' />
                                        </td>
                                    </tr>
                                    <tr>
                                        <td align="right" style="padding-right:5px" nowrap ><b>Order Limit:</b></td>
                                        <td>
                                            <asp:TextBox ID="vcrlimTextBox" MaxLength="11" onkeyup="orlim()" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" Width="80px" runat="server" Text='<%# Bind("vordlim") %>'></asp:TextBox>
                                            <asp:RegularExpressionValidator ID="RegularExpressionValidator14" runat="server" SetFocusOnError="true" ControlToValidate="vcrlimTextBox" ErrorMessage="Invalid Order Limit." Display="Dynamic" ValidationExpression="(^([0-9]*|\d*\d{1}.*?\d*)$)"></asp:RegularExpressionValidator>
                                        </td>
                                        <td nowrap><b>EDI:</b>
                                            <asp:CheckBox ID="vanedicustCheckBox" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" Width="10px" runat="server" Checked='<%# Bind("vanedicust") %>' />
                                        </td>
                                        <td nowrap>
                                            <b>Factored:</b>
                                            <asp:CheckBox ID="vfactoredCheckBox" Width="10px" runat="server" Checked='<%# Bind("vfactored") %>' />
                                        </td>
                                    </tr>
                                    <tr>
                                        <td align="right" style="padding-right:5px" ><b>Discount:</b></td>
                                        <td>
                                            <asp:TextBox ID="vdiscTextBox" MaxLength="6" onkeyup="discount()" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" Width="80px" runat="server" Text='<%# Bind("vdisc") %>'></asp:TextBox>
                                            <asp:RegularExpressionValidator ID="RegularExpressionValidator15" runat="server" SetFocusOnError="true" ControlToValidate="vdiscTextBox" ErrorMessage="Invalid Discount." Display="Dynamic" ValidationExpression="(^([0-9]*|\d*\d{1}.*?\d*)$)"></asp:RegularExpressionValidator>
                                        </td>
                                        
                                    </tr>
                                    <tr>
                                        <td align="right" style="padding-right:5px">  
                                            <b>Currency:</b></td>
                                        <td nowrap>
                                            <asp:TextBox ID="vcurrcodeTextBox" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" Width="80px" runat="server" Text='<%# Bind("vcurrcode") %>'></asp:TextBox>
                                            <a href="#" tabindex="1" onClick="currencylook(); return false"><asp:Image ID="Image12" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
                                        </td>
                                        <%--<td nowrap colspan="2"><b>Invoice Per:</b>
                                            <asp:RadioButtonList ID="invoiceperRadioButtonList1" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" RepeatLayout="Flow"  CellSpacing="2" RepeatColumns="3"  DataValueField='<%# Bind("vinvmeth") %>'  SelectedValue = '<%# Bind("vinvmeth") %>' Enabled="true" Font-Bold="true" runat="server">
                                                <asp:ListItem   value="False" Text="BOL" />
                                                <asp:ListItem  value="True" Text="PO" /> 
                                                <asp:ListItem  value="T" Text="Group By Date" />
                                            </asp:RadioButtonList>
                                        &nbsp;&nbsp;&nbsp;
                                        
                                        </td>--%>
                                    </tr>
                                                                        
                                </table> 
                            </td>
                            <td>
                                <table class="shade" style="border:solid 1px black; height:200px;" width="550px" > 
                                    <legend style="color:Blue">Shipping Information</legend> 
                                        <tr>
                                            <td colspan="7"  >
                                                <b>Freight Payment:</b>
                                                    <asp:RadioButtonList ID="FreightPaymentRadioButtonList" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" RepeatLayout="Flow"  CellSpacing="1" RepeatColumns="4"     SelectedValue ='<%# Bind("vfrtpay") %>' Font-Bold="true" Enabled="true"    runat="server">
                                                         <asp:ListItem value="B"  Text="Bill"   />
                                                         <asp:ListItem value="C" Text="Collect" />
                                                         <asp:ListItem value="P" Text="Perpaid" />
                                                         <asp:ListItem  Value="T" Text="3rd Party" /> 
                                                     </asp:RadioButtonList>
                                                    <%--<asp:TextBox ID="TextBox4" Text='<%# Bind("vfrtpay") %>' runat="server"></asp:TextBox>--%>
                                            &nbsp;&nbsp;&nbsp;&nbsp;
                                                <b>FOB:</b>
                                            
                                                <asp:RadioButtonList ID="FOBRadioButtonList1" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" RepeatLayout="Flow"  CellSpacing="1" RepeatColumns="2"   SelectedValue = '<%# Bind("vfobcode") %>' Enabled="true" Font-Bold="true"   runat="server">
                                                    <asp:ListItem value="DEST"  Text="Destination"   />
                                                    <asp:ListItem  value="ORIG" Text="Origin" />
                                                </asp:RadioButtonList>
                                            </td>
                                            
                                        </tr>
                                        <tr>
                                            <td align="right" style="padding-right:5px"><b>Location:</b></td>
                                            <td nowrap>
                                                <asp:TextBox ID="vlocTextBox" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" Width="80px"  runat="server" Text='<%# Bind("vloc") %>'></asp:TextBox>
                                                <a href="#" tabindex="1" onClick="locationlook(); return false"><asp:Image ID="Image10" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
                                            </td>
                                            <td>
                                                <asp:TextBox ID="vdesclocTextBox" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" Width="130px" runat="server" Text='<%# Bind("vdescloc") %>'></asp:TextBox>
                                            </td>
                                            <td nowrap><b>Partial Ship:</b>
                                                <asp:CheckBox ID="vshippartCheckBox" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" Width="1px" runat="server" Checked='<%# Bind("vshippart") %>' />
                                             </td>
                                        </tr>
                                        <tr>
                                            <td align="right" style="padding-right:5px"><b>Carrier:</b></td>
                                            <td nowrap>
                                                 <asp:TextBox ID="vcarrierTextBox" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" Width="80px"   runat="server" Text='<%# Bind("vcarrier") %>'></asp:TextBox><a href="#" onClick="carrierlook(); return false"><asp:Image ID="Image11" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
                                            </td>
                                            <td>
                                                <asp:TextBox ID="vdescarrierTextBox" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" Width="130px" runat="server" Text='<%# Bind("vdescarrier") %>'></asp:TextBox>
                                            </td>
                                            <td nowrap align="right" style="padding-right:5px"><b>Po # Mandatory:</b></td>
                                            <td>
                                                <asp:CheckBox ID="vmandatoryCheckBox" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" Width="1px" runat="server" Checked='<%# Bind("vmandatory") %>'  />
                                                
                                            </td>
                                        </tr>
                                        <tr>
                                            <td nowrap align="right" style="padding-right:5px"> <b>Delivery Zone:</b></td>
                                            <td nowrap>
                                                <asp:TextBox ID="vdelzoneTextBox" MaxLength="5" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" Width="80px"   runat="server" Text='<%# Bind("vdelzone") %>'></asp:TextBox>
                                                <a href="#" tabindex="1" onClick="deliveryzonelook(); return false"><asp:Image ID="Image13" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
                                            </td>
                                            <td>
                                                <asp:TextBox ID="vdeszoneTextBox" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" Width="130px" runat="server" Text='<%# Bind("vdeszone") %>'></asp:TextBox>
                                            </td>
                                            <td align="right" style="padding-right:5px" > <b>Pallet:</b></td>
                                            <td>
                                                <asp:TextBox ID="vpalletTextBox" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" Width="80px"  runat="server" Text='<%# Bind("vpallet") %>'></asp:TextBox>
                                            </td>
                                        </tr>
                                        <tr>
                                            <td nowrap align="right" style="padding-right:5px"> <b>Territory:</b></td>
                                            <td nowrap>
                                                <asp:TextBox ID="vterrTextBox" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" Width="80px"  runat="server" Text='<%# Bind("vterr") %>'></asp:TextBox>
                                                <a href="#" tabindex="1" onClick="terrlook(); return false"><asp:Image ID="Image2" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
                                            </td>
                                            <td> 
                                                <asp:TextBox ID="vdesterrTextBox" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" Width="130px"  runat="server" Text='<%# Bind("vdesterr") %>'></asp:TextBox>
                                            </td>
                                            <td nowrap align="right" style="padding-right:5px" ><b>Case Bundle:</b></td>
                                            <td>
                                                <asp:TextBox ID="vcasebundleTextBox" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" Width="80px"   runat="server" Text='<%# Bind("vcasebundle") %>'></asp:TextBox>
                                            </td>
                                        </tr>
                                        <tr>
                                            
                                            <td align="right" style="padding-right:5px"><b>Overrun%:</b></td>
                                            <td nowrap>
                                                <asp:TextBox ID="voverpctTextBox" MaxLength="5" onkeyup="overrun()" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" Width="50px"  runat="server" Text='<%# Bind("voverpct") %>'></asp:TextBox>
                                                <asp:RegularExpressionValidator ID="RegularExpressionValidator10" runat="server" SetFocusOnError="true" ControlToValidate="voverpctTextBox" ErrorMessage="Invalid Overrun%:." Display="Dynamic" ValidationExpression="(^([0-9]*|\d*\d{1}.*?\d*)$)"></asp:RegularExpressionValidator>
                                            </td>
                                            <td align="right" style="padding-right:5px"><b>Underrun%:</b></td>
                                            <td nowrap>
                                                <asp:TextBox ID="vunderpctTextBox" MaxLength="5" onkeyup="underrun()" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" Width="50px"  runat="server" Text='<%# Bind("vunderpct") %>'></asp:TextBox>
                                                <asp:RegularExpressionValidator ID="RegularExpressionValidator9" runat="server" SetFocusOnError="true" ControlToValidate="vunderpctTextBox" ErrorMessage="Invalid Underrun%." Display="Dynamic" ValidationExpression="(^([0-9]*|\d*\d{1}.*?\d*)$)"></asp:RegularExpressionValidator>
                                            </td>
                                        </tr>
                                        <tr>
                                            <td align="right" style="padding-right:5px"><b>Markup:</b></td>
                                            <td nowrap>
                                                <asp:TextBox ID="vmarkupTextBox" MaxLength="5" onkeyup="markup()" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" Width="50px"  runat="server" Text='<%# Bind("vmarkup") %>'></asp:TextBox>
                                                <asp:RegularExpressionValidator ID="RegularExpressionValidator17" runat="server" SetFocusOnError="true" ControlToValidate="vmarkupTextBox" ErrorMessage="Invalid Markup." Display="Dynamic" ValidationExpression="(^([0-9]*|\d*\d{1}.*?\d*)$)"></asp:RegularExpressionValidator>
                                            </td>
                                            <td align="right" style="padding-right:5px">  <b>Load Tags:</b></td>
                                            <td> 
                                                <asp:TextBox ID="vintfield1TextBox" MaxLength="7" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" Width="70px"  runat="server" Text='<%# Bind("vintfield1") %>'></asp:TextBox>
                                                <asp:RegularExpressionValidator ID="RegularExpressionValidator20" runat="server" SetFocusOnError="true" ControlToValidate="vintfield1TextBox" ErrorMessage="Invalid load tags" Display="Dynamic" ValidationExpression="(^([0-9]*|\d*\d{1}?\d*)$)"></asp:RegularExpressionValidator>
                                            </td>
                                            <td align="right" style="padding-right:5px" nowrap><b>Whse Days:</b>
                                                <asp:TextBox ID="vshipdaysTextBox" MaxLength="3" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" Width="50px"  runat="server" Text='<%# Bind("vshipdays") %>'></asp:TextBox>
                                                <asp:RegularExpressionValidator ID="RegularExpressionValidator18" runat="server" SetFocusOnError="true" ControlToValidate="vshipdaysTextBox" ErrorMessage="Invalid Where Days." Display="Dynamic" ValidationExpression="(^([0-9]*|\d*\d{1}?\d*)$)"></asp:RegularExpressionValidator>
                                            </td>    
                                            
                                            
                                        </tr>
                                                                               
                                      </tr>
                                   </table>
                                        <tr>
                                            <td colspan="2">
                                                <table class="shade" style="border:solid 1px black;"> 
                                                    <legend style="color:Blue">Tax Information</legend>
                                                        <tr>
                                                            <td align="right" style="padding-right:5px">
                                                                <b>Taxable:</b></td>
                                                             <td>
                                                                <asp:RadioButtonList onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" ID="taxableRadioButtonList1" RepeatLayout="Flow"  CellSpacing="1" RepeatColumns="2"   SelectedValue = '<%# Bind("vsort") %>' Enabled="True" Font-Bold="true" runat="server">
                                                                    <asp:ListItem   value="Y" Text="Yes" />
                                                                    <asp:ListItem   value="N" Text="No" />
                                                                </asp:RadioButtonList>
                                                            </td>
                                                        </tr>
                                                        <tr>
                                                            <td align="right" style="padding-right:5px" ><b>Tax Code:</b> </td>
                                                            <td>
                                                                <asp:TextBox ID="vtaxgrTextBox" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" Width="80px" runat="server" Text='<%# Bind("vtaxgr") %>'></asp:TextBox>
                                                                <a href="#" tabindex="1" onClick="taxcodelook(); return false"><asp:Image ID="Image14" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
                                                            </td>
                                                            <td>
                                                                <asp:TextBox ID="vfobcodeTextBox" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" Text='<%# Bind("vtaxdscr") %>' width="80px" runat="server" ></asp:TextBox>
                                                            </td>
                                                        </tr>
                                                        <tr>
                                                            <td align="right" style="padding-right:5px" ><b>Tax Resale#:</b></td>
                                                            <td>
                                                                <asp:TextBox ID="vtaxidTextBox" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" Width="80px" runat="server" Text='<%# Bind("vtaxid") %>'></asp:TextBox>
                                                            </td>
                                                            <td align="right" style="padding-right:5px" nowrap><b>Exp. Date:</b>
                                                                <asp:TextBox ID="vdatefield2TextBox" ToolTip="MM/DD/YYYY" MaxLength="10" onfocus="javascript:preEnter( this, 'yes' );" onblur="javascript:preLeave( this, 'date', '99/99/9999' );" Width="60px" runat="server" Text='<%# Bind("vdatefield2","{0:MM/dd/yyyy}") %>'></asp:TextBox>
                                                                <a href="#" tabindex="1" onClick="showCalendarControl(FormView1_vdatefield2TextBox); return false"><asp:Image ID="Image16" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
                                                                
                                                            </td>
                                                        </tr>
                                                  </table>
                                              </td>
                                            </tr>
                                            <tr>
                                                <td>
                                                    <asp:Button ID="UpdateButton" CssClass="button" runat="server" CausesValidation="True"  OnClick="UpdateButton_Click" Text="Save"></asp:Button>
                                                    <asp:Button ID="UpdateCancelButton" runat="server" CssClass="button" CausesValidation="False" CommandName="Cancel" Text="Cancel"></asp:Button>
                                                </td>
                                            </tr>
                                        </table>
                                     </asp:Panel>
                            </EditItemTemplate>
              
               <InsertItemTemplate>
                    <asp:Panel ID="insert_panel" runat="server" DefaultButton="InsertButton">
                        <table style="border: 1px solid " width="960px" class="shade">
                           <tr>
                                <td>
                                    <table  class="shade"  style="border:solid 1px black; height:175px;" width="465px">
                                        <tr>
                                            <td align="right" style="padding-right:5px" ><b>Customer:</b></td>
                                            <td>
                                                <asp:TextBox ID="vcustnoTextBox" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)"  Width="100px" runat="server" Text='<%# Bind("vcustno") %>'></asp:TextBox>
                                            </td>
                                                <asp:RequiredFieldValidator ID="RequiredFieldValidator1" ControlToValidate="vcustnoTextBox" Display="dynamic" SetFocusOnError="true" runat="server" ErrorMessage="This field cannot be blank"></asp:RequiredFieldValidator>
                                            <td>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<b>Status:</b></td>
                                            <td>
                                                <asp:DropDownList ID="DropDownList2" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" Height="1px"  Width="75px" runat="server" SelectedValue = '<%# Bind("vactive") %>' >
                                                    <asp:ListItem Value="A">Active</asp:ListItem>
                                                    <asp:ListItem Value="I">Inactive</asp:ListItem>
                                                    <asp:ListItem Value="X">Inhouse</asp:ListItem>
                                                    <asp:ListItem Value="S">Statement</asp:ListItem>
                                                    <asp:ListItem Value="E">Service</asp:ListItem>                   
                                                </asp:DropDownList>
                                            </td>
                                        </tr> 
                                        <tr>
                                            <td align="right" style="padding-right:5px" > <b>Name:</b></td>
                                            <td>
                                                <asp:TextBox ID="vcustnameTextBox" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" runat="server" Text='<%# Bind("vcustname") %>'></asp:TextBox>
                                            </td>
                                        <tr>
                                            <td align="right" style="padding-right:5px" > <b>Address:</b></td>
                                            <td>
                                                <asp:TextBox ID="vaddr1TextBox" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" runat="server" Text='<%# Bind("vaddr1") %>'></asp:TextBox>
                                            </td>
                                        <tr>
                                            <td align="right" style="padding-right:5px" > <b>Address:</b></td>
                                            <td>
                                                <asp:TextBox ID="vaddr2TextBox" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" runat="server" Text='<%# Bind("vaddr2") %>'></asp:TextBox>
                                            </td>
                                        <tr>
                                            <td align="right" style="padding-right:5px" ><b>City:</b></td>
                                            <td nowrap colspan="3">
                                                <asp:TextBox ID="vcityTextBox" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" Width="70px" runat="server" Text='<%# Bind("vcity") %>'></asp:TextBox>
                                                <a href="#" tabindex="1" onClick="citylook(); return false"><asp:Image ID="Image9" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
                                             State:
                                                <asp:TextBox ID="vstateTextBox" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" width="50px" runat="server" Text='<%# Bind("vstate") %>'></asp:TextBox>
                                                <a href="#" onClick="statecodelook(); return false"><asp:Image ID="Image2" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
                                             Zip:
                                                <asp:TextBox ID="vzipTextBox" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" width="70px" runat="server" Text='<%# Bind("vzip") %>'></asp:TextBox>
                                                <a href="#" tabindex="1" onClick="zipcodelook(); return false"><asp:Image ID="Image3" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
                                            </td>
                                        </tr>
                                        <tr>
                                            <td align="right" style="padding-right:5px"><b>Email:</b></td>
                                            <td>
                                                <asp:TextBox ID="vemailTextBox" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" MaxLength="50" Width="250px" runat="server" Text='<%# Bind("vemail") %>'></asp:TextBox>
                                                <asp:RegularExpressionValidator ID="RegularExpressionValidator1"  ControlToValidate="vemailTextBox" runat="server"
                                                    ErrorMessage="You must enter an email address" ValidationExpression="\w+([-+.']\w+)*@\w+([-.]\w+)*\.\w+([-.]\w+)*"> </asp:RegularExpressionValidator>
                                            </td>
                                        </tr>
                                    </table>
                                </td>
                                <td>
                                    <table class="shade"  style="border:solid 1px black; height:175px;" width="550px" >
                                        <tr>
                                            <td align="right" style="padding-right:5px"><b>Date Added:</b></td>
                                            <td colspan="2" nowrap>
                                                <asp:TextBox ID="vdate1TextBox" ToolTip="MM/DD/YYYY" Width="60px" onfocus="javascript:preEnter( this, 'yes' );" onblur="javascript:preLeave( this, 'date', '99/99/9999' );"  MaxLength="10" runat="server" Text='<%# Bind("vdate1","{0:MM/dd/yyyy}") %>'></asp:TextBox>
                                                <a href="#" tabindex="1" onClick="showCalendarControl(FormView1_vdate1TextBox); return false"><asp:Image ID="Image17" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
                                                
                                            </td>
                                        </tr> 
                                        <tr>
                                            <td  align="right" style="padding-right:5px" ><b>Type:</b></td>
                                            <td colspan="2" nowrap>
                                                <asp:TextBox ID="vtypeTextBox" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" width="80px" runat="server" Text='<%# Bind("vtype") %>'></asp:TextBox>
                                                <a href="#" tabindex="1" onClick="typelook(); return false"><asp:Image ID="Image8" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
                                                <asp:TextBox ID="vdesctypeTextBox" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" width="100px" runat="server" Text='<%# Bind("vdesctype") %>'></asp:TextBox>
                                            </td>
                                        </tr>
                                        <tr>
                                            <td align="right" style="padding-right:5px" ><b>Contact:</b></td>
                                            <td colspan="2">
                                                <asp:TextBox ID="vcontactTextBox" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" runat="server" Text='<%# Bind("vcontact") %>'></asp:TextBox>
                                            </td>
                                        </tr>
                                        <tr>
                                            <td align="right"  style="padding-right:5px">  <b>Sales Rep:</b></td>
                                            <td colspan="2" nowrap>
                                                <asp:TextBox ID="vsmanTextBox" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" width="50px" runat="server" Text='<%# Bind("vsman") %>'></asp:TextBox>
                                                <a href="#" tabindex="1" onClick="salesreplook(); return false"><asp:Image ID="Image6" runat="server" ImageUrl="images/lookup_icon.gif" /></a> 
                                                <asp:TextBox ID="vdescsmanTextBox" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" width="80px" runat="server" Text='<%# Bind("vdescsman") %>'></asp:TextBox>
                                            &nbsp;&nbsp;&nbsp;&nbsp;
                                            <b>Flat Comm:</b>
                                                <asp:TextBox ID="vflatcommTextBox" MaxLength="8" onkeyup="flatcomm()" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" width="50px" runat="server" Text='<%# Bind("vflatcomm") %>'></asp:TextBox>
                                                <asp:RegularExpressionValidator ID="RegularExpressionValidator8" runat="server" SetFocusOnError="true" ControlToValidate="vflatcommTextBox" ErrorMessage="Invalid Flat Comm." Display="Dynamic" ValidationExpression="(^([0-9]*|\d*\d{1}.*?\d*)$)"></asp:RegularExpressionValidator>
                                            </td>
                                        </tr>
                                        <tr>
                                            <td align="right" style="padding-right:5px"><b>Phone:</b></td>
                                            <td colspan="2" nowrap> 
                                                <asp:TextBox ID="vareacodeTextBox" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" width="50px" MaxLength="3" runat="server" Text='<%# Bind("vareacode") %>'></asp:TextBox>
                                                <%--<asp:RegularExpressionValidator ID="RegularExpressionValidator2" runat="server" SetFocusOnError="true" ControlToValidate="vareacodeTextBox" ErrorMessage="Invalid Phone Code." Display="Dynamic" ValidationExpression="(^([0-9]*|\d*\d{1}?\d*)$)"></asp:RegularExpressionValidator>--%>
                                                <asp:TextBox ID="vphoneTextBox" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" width="80px" MaxLength="8" runat="server" Text='<%# Bind("vphone") %>'></asp:TextBox>
                                                <%--<asp:RegularExpressionValidator ID="RegularExpressionValidator5" runat="server" SetFocusOnError="true" ControlToValidate="vphoneTextBox" ErrorMessage="Invalid Phone No." Display="Dynamic" ValidationExpression="(^([0-9]*|\d*\d{1}?\d*)$)"></asp:RegularExpressionValidator>--%>
                                            </td>
                                        </tr>
                                        <tr>
                                            <td align="right" style="padding-right:5px"><b>Fax:</b></td>
                                            <td colspan="2" nowrap> 
                                                <asp:TextBox ID="vfaxcodeTextBox" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" width="50px" MaxLength="3" runat="server" Text='<%# Bind("vfaxcode") %>'></asp:TextBox>
                                                <asp:RegularExpressionValidator ID="RegularExpressionValidator6" runat="server" SetFocusOnError="true" ControlToValidate="vfaxcodeTextBox" ErrorMessage="Invalid Fax Code." Display="Dynamic" ValidationExpression="(^([0-9]*|\d*\d{1}?\d*)$)"></asp:RegularExpressionValidator>
                                                <asp:TextBox ID="vfaxTextBox" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" width="80px" MaxLength="8" runat="server" Text='<%# Bind("vfax") %>'></asp:TextBox>
                                                <asp:RegularExpressionValidator ID="RegularExpressionValidator7" runat="server" SetFocusOnError="true" ControlToValidate="vfaxTextBox" ErrorMessage="Invalid Fax No." Display="Dynamic" ValidationExpression="(^([0-9]*|\d*\d{1}?\d*)$)"></asp:RegularExpressionValidator>
                                            &nbsp;&nbsp;&nbsp;&nbsp;
                                            <b>Prefix:</b>
                                                <asp:TextBox ID="vfaxprefixTextBox" MaxLength="3" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" width="50px" runat="server" Text='<%# Bind("vfaxprefix") %>'></asp:TextBox>
                                              &nbsp;  <b>Country:</b>
                                                    <asp:TextBox ID="vfaxcountryTextBox" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" width="80px" runat="server" Text='<%# Bind("vfaxcountry") %>'></asp:TextBox>
                                            </td>
                                        </tr>
                                    </table>
                                </td>
                            </tr>              
                            <tr>
                                <td>
                                    <table class="shade" width="400px" style="border:solid 1px black;"> 
                                    <legend style="color:Blue">Credit Information </legend>
                                        <tr>
                                            <td align="right" style="padding-right:5px" > <b>Terms:</b></td>
                                            <td nowrap>
                                                <asp:TextBox ID="vtermsTextBox" MaxLength="5" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" Width="80px" runat="server" Text='<%# Bind("vterms") %>'></asp:TextBox>
                                                <a href="#" tabindex="1" onClick="termslook(); return false"><asp:Image ID="Image7" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
                                            </td>
                                            <td>
                                                <asp:TextBox ID="vdesctermsTextBox" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" Width="130px" runat="server" Text='<%# Bind("vdescterms") %>'></asp:TextBox>
                                            </td>
                                        </tr>
                                        <tr>
                                            <td align="right" style="padding-right:5px" ><b>Cr.Acct #:</b></td>
                                            <td>
                                                <asp:TextBox ID="vcruseTextBox" MaxLength="10" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" Width="80px" runat="server" Text='<%# Bind("vcruse") %>'></asp:TextBox>
                                            </td>
                                            <td nowrap ><b>Grace Days:</b>
                                                <asp:TextBox ID="vcrholdinvdaysTextBox" MaxLength="3" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" Width="40px" runat="server" Text='<%# Bind("vcrholdinvdays") %>'></asp:TextBox>
                                                <asp:RegularExpressionValidator ID="RegularExpressionValidator21" runat="server" SetFocusOnError="true" ControlToValidate="vcrholdinvdaysTextBox" ErrorMessage="Invalid grace days." Display="Dynamic" ValidationExpression="(^([0-9]*|\d*\d{1}?\d*)$)"></asp:RegularExpressionValidator>
                                            </td>
                                            <td nowrap><b>Dollars:</b>
                                                <asp:TextBox ID="vcrholdinvdueTextBox" MaxLength="10" onkeyup="empday()" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" Width="90px" ToolTip="Enter the integer or decimal value" runat="server" Text='<%# Bind("vcrholdinvdue") %>'></asp:TextBox>
                                                <asp:RegularExpressionValidator ID="RegularExpressionValidator15" runat="server" SetFocusOnError="true" ControlToValidate="vcrholdinvdueTextBox" ErrorMessage="Invalid value." Display="Dynamic" ValidationExpression="(^([0-9]*|\d*\d{1}.*?\d*)$)"></asp:RegularExpressionValidator>
                                            </td>
                                        </tr>
                                        <tr>
                                            <td align="right" style="padding-right:5px" nowrap ><b>Credit Rating:</b></td>
                                            <td>
                                                <asp:TextBox ID="vcrratingTextBox" MaxLength="3" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" Width="40px" runat="server" Text='<%# Bind("vcrrating") %>'></asp:TextBox>
                                            </td>
                                            <td nowrap><b>Price Level:</b>
                                                <asp:TextBox ID="vcustlevelTextBox" MaxLength="2" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" Width="40px" runat="server" Text='<%# Bind("vcustlevel") %>'></asp:TextBox>
                                                <asp:RegularExpressionValidator ID="RegularExpressionValidator19" runat="server" SetFocusOnError="true" ControlToValidate="vcustlevelTextBox" ErrorMessage="Invalid Price Level." Display="Dynamic"  ValidationExpression="(^([0-9]*|\d*\d{1}?\d*)$)"></asp:RegularExpressionValidator>
                                            </td>
                                            <td nowrap><b>Credit Hold:</b>
                                                <asp:CheckBox ID="vcrholdCheckBox" Width="10px" runat="server" Checked='<%# Bind("vcrhold") %>' />
                                            </td>
                                        </tr>
                                        <tr>
                                            <td align="right" style="padding-right:5px" nowrap ><b>Credit Limit:</b></td>
                                            <td>
                                                <asp:TextBox ID="vcrlimtTextBox" MaxLength="12" Width="80px" onkeyup="crlimit()" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" runat="server" Text='<%# Bind("vcrlim") %>'></asp:TextBox>
                                                <asp:RegularExpressionValidator ID="RegularExpressionValidator24" runat="server" SetFocusOnError="true" ControlToValidate="vcrlimtTextBox" ErrorMessage="Invalid Order Limit." Display="Dynamic" ValidationExpression="(^([0-9]*|\d*\d{1}.*?\d*)$)"></asp:RegularExpressionValidator>
                                            </td>
                                            <td nowrap><b>Finance Charges:</b>
                                                <asp:CheckBox ID="vfinchgCheckBox" Width="10px" runat="server" Checked='<%# Bind("vfinchg") %>' />
                                            </td>
                                            <td nowrap><b>Auto Reprice:</b>
                                                <asp:CheckBox ID="vautorepriceCheckBox" Width="10px" runat="server" Checked='<%# Bind("vautoreprice") %>' />
                                            </td>
                                        </tr>
                                        <tr>
                                            <td align="right" style="padding-right:5px" nowrap ><b>Order Limit:</b></td>
                                            <td>
                                                <asp:TextBox ID="vcrlimTextBox" MaxLength="11" onkeyup="orlim()" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" Width="80px" runat="server" Text='<%# Bind("vordlim") %>'></asp:TextBox>
                                                <asp:RegularExpressionValidator ID="RegularExpressionValidator12" runat="server" SetFocusOnError="true" ControlToValidate="vcrlimTextBox" ErrorMessage="Invalid Order Limit." Display="Dynamic" ValidationExpression="(^([0-9]*|\d*\d{1}.*?\d*)$)"></asp:RegularExpressionValidator>
                                            </td>
                                            <td nowrap><b>EDI:</b>
                                                <asp:CheckBox ID="vanedicustCheckBox" Width="10px" runat="server" Checked='<%# Bind("vanedicust") %>' />
                                            </td>
                                            <td nowrap><b>Factored:</b>
                                                <asp:CheckBox ID="vfactoredCheckBox" Width="10px" runat="server" Checked='<%# Bind("vfactored") %>' />
                                            </td>
                                        </tr>
                                        <tr>
                                            <td align="right" style="padding-right:5px" ><b>Discount:</b></td>
                                            <td nowrap>
                                                <asp:TextBox ID="vdiscTextBox" MaxLength="6" onkeyup="discount()" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" Width="50px" runat="server" Text='<%# Bind("vdisc") %>'></asp:TextBox>
                                                <asp:RegularExpressionValidator ID="RegularExpressionValidator13" runat="server" SetFocusOnError="true" ControlToValidate="vdiscTextBox" ErrorMessage="Invalid Discount." Display="Dynamic" ValidationExpression="(^([0-9]*|\d*\d{1}.*?\d*)$)"></asp:RegularExpressionValidator>
                                            </td>
                                            
                                        </tr>
                                        <tr>
                                            <td align="right" style="padding-right:5px">  
                                                <b>Currency:</b></td>
                                            <td nowrap>
                                                <asp:TextBox ID="vcurrcodeTextBox" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)"  Width="80px" runat="server" Text='<%# Bind("vcurrcode") %>'></asp:TextBox>
                                                <a href="#" tabindex="1" onClick="currencylook(); return false"><asp:Image ID="Image12" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
                                            </td>
                                           <%-- <td nowrap colspan="2"><b>Invoice Per:</b>
                                                <asp:RadioButtonList ID="invoiceRadioButtonList" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" RepeatLayout="Flow"  CellSpacing="2" RepeatColumns="3"   SelectedValue = '<%# Bind("vinvmeth") %>' Enabled="true" Font-Bold="true" runat="server">
                                                    <asp:ListItem   value="False" Text="BOL" />
                                                    <asp:ListItem value="True" Text="PO" /> 
                                                    <asp:ListItem value="T" Text="Group By Date" />
                                                </asp:RadioButtonList>
                                                
                                            </td>--%>
                                        </tr>                                       
                                        
                                        
                                </table> 
                            </td>
                            <td>
                                <table class="shade" style="border:solid 1px black;" width="550px">
                                <legend style="color:Blue">Shipping Information</legend>
                                    <tr>
                                        <td colspan="7" nowrap>
                                            <b>Freight Payment:</b>
                                                <asp:RadioButtonList ID="FreightPaymentRadioButtonList" RepeatLayout="Flow"  CellSpacing="1" RepeatColumns="4"     SelectedValue ='<%# Bind("vfrtpay") %>' Font-Bold="true" Enabled="true" runat="server">
                                                     <asp:ListItem value="B"  Text="Bill"   />
                                                     <asp:ListItem value="C" Text="Collect" />
                                                     <asp:ListItem value="P" Text="Perpaid" />
                                                     <asp:ListItem  Value="T" Text="3rd Party" />
                                                 </asp:RadioButtonList>
                                            <%--<asp:TextBox ID="TextBox1" runat="server" Text ='<%# Bind("vfrtpay") %>'></asp:TextBox>--%>
                                        &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
                                            <b>FOB:</b>
                                            <asp:RadioButtonList ID="FOBRadioButtonList1" RepeatLayout="Flow"  CellSpacing="1" RepeatColumns="2"   SelectedValue = '<%# Bind("vfobcode") %>' Enabled="true" Font-Bold="true"   runat="server">
                                                <asp:ListItem value="DEST"  Text="Destination"   />
                                                <asp:ListItem  value="ORIG" Text="Origin" />
                                            </asp:RadioButtonList>
                                       </td>
                                    </tr>
                                   
                                    <tr>
                                        <td align="right" style="padding-right:5px"><b>Location:</b></td>
                                        <td nowrap>
                                            <asp:TextBox ID="vlocTextBox" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" Width="80px"  runat="server" Text='<%# Bind("vloc") %>'></asp:TextBox>
                                            <a href="#" tabindex="1" onClick="locationlook(); return false"><asp:Image ID="Image10" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
                                        </td>
                                        <td>
                                            <asp:TextBox ID="vdesclocTextBox" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" Width="130px" runat="server" Text='<%# Bind("vdescloc") %>'></asp:TextBox>
                                        </td>
                                        <td nowrap><b>Partial Ship:</b>
                                            <asp:CheckBox ID="vshippartCheckBox" Width="1px" runat="server" Checked='<%# Bind("vshippart") %>' />
                                      </td>
                                    </tr>
                                    <tr>
                                        <td align="right" style="padding-right:5px"><b>Carrier:</b></td>
                                        <td nowrap>
                                            <asp:TextBox ID="vcarrierTextBox" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" Width="80px"   runat="server" Text='<%# Bind("vcarrier") %>'></asp:TextBox>
                                            <a href="#" tabindex="1" onClick="carrierlook(); return false"><asp:Image ID="Image11" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
                                        </td>
                                        <td>
                                            <asp:TextBox ID="vdescarrierTextBox" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" Width="130px" runat="server" Text='<%# Bind("vdescarrier") %>'></asp:TextBox>
                                        </td>
                                        <td nowrap><b>Po # Mandatory:</b></td>
                                        <td> <asp:CheckBox ID="vmandatoryCheckBox" Width="1px" Checked='<%# Bind("vmandatory") %>' runat="server" /></td>
                                    </tr>
                                    <tr>
                                        <td align="right" style="padding-right:5px" nowrap> <b>Delivery Zone:</b></td>
                                        <td nowrap>
                                            <asp:TextBox ID="vdelzoneTextBox" MaxLength="5" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" Width="80px"   runat="server" Text='<%# Bind("vdelzone") %>'></asp:TextBox>
                                            <a href="#" tabindex="1" onClick="deliveryzonelook(); return false"><asp:Image ID="Image13" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
                                        </td>
                                        <td>
                                            <asp:TextBox ID="vdeszoneTextBox" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" Width="130px" runat="server" Text='<%# Bind("vdeszone") %>'></asp:TextBox>
                                        </td>
                                        <td align="right" style="padding-right:5px" > <b>Pallet:</b></td>
                                        <td>
                                            <asp:TextBox ID="vpalletTextBox" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" Width="80px"  runat="server" Text='<%# Bind("vpallet") %>'></asp:TextBox>
                                        </td>
                                    </tr>
                                    <tr>
                                        <td align="right" style="padding-right:5px"> <b>Territory:</b></td>
                                        <td nowrap>
                                            <asp:TextBox ID="vterrTextBox" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" Width="80px"  runat="server" Text='<%# Bind("vterr") %>'></asp:TextBox>
                                            <a href="#" tabindex="1" onClick ="terrlook(); return false"><asp:Image ID="Image5" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
                                        </td>
                                        <td> 
                                            <asp:TextBox ID="vdesterrTextBox" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" Width="130px"  runat="server" Text='<%# Bind("vdesterr") %>'></asp:TextBox>
                                        </td>
                                        <td align="right" style="padding-right:5px" nowrap><b>Case Bundle:</b></td>
                                        <td>
                                            <asp:TextBox ID="vcasebundleTextBox" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" Width="80px"   runat="server" Text='<%# Bind("vcasebundle") %>'></asp:TextBox>
                                        </td>
                                    </tr>
                                    <tr>
                                        <td align="right" style="padding-right:5px"><b>Overrun%:</b></td>
                                        <td nowrap>
                                            <asp:TextBox ID="voverpctTextBox" MaxLength="5" onkeyup="overrun()" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" Width="60px"  runat="server" Text='<%# Bind("voverpct") %>'></asp:TextBox>
                                            <asp:RegularExpressionValidator ID="RegularExpressionValidator11" runat="server" SetFocusOnError="true" ControlToValidate="voverpctTextBox" ErrorMessage="Invalid Overrun%." Display="Dynamic" ValidationExpression="(^([0-9]*|\d*\d{1}.*?\d*)$)"></asp:RegularExpressionValidator>
                                        </td>
                                        <td align="right" style="padding-right:5px"><b>Underrun%:</b></td>
                                        <td nowrap>
                                            <asp:TextBox ID="vunderpctTextBox" MaxLength="5" onkeyup="underrun()" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" Width="50px"  runat="server" Text='<%# Bind("vunderpct") %>'></asp:TextBox>
                                            <asp:RegularExpressionValidator ID="RegularExpressionValidator10" runat="server" SetFocusOnError="true" ControlToValidate="vunderpctTextBox" ErrorMessage="Invalid Underrun%." Display="Dynamic" ValidationExpression="(^([0-9]*|\d*\d{1}.*?\d*)$)"></asp:RegularExpressionValidator>
                                        </td>
                                        
                                    </tr>
                                    
                                    <tr>
                                        <td align="right" style="padding-right:5px"><b>Markup:</b></td>
                                        <td nowrap>
                                            <asp:TextBox ID="vmarkupTextBox" MaxLength="5" onkeyup="markup()" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" Width="60px"  runat="server" Text='<%# Bind("vmarkup") %>'></asp:TextBox>
                                            <asp:RegularExpressionValidator ID="RegularExpressionValidator17" runat="server" SetFocusOnError="true" ControlToValidate="vmarkupTextBox" ErrorMessage="Invalid Markup." Display="Dynamic" ValidationExpression="(^([0-9]*|\d*\d{1}.*?\d*)$)"></asp:RegularExpressionValidator>
                                        </td>
                                        <td align="right" style="padding-right:5px">  <b>Load Tags:</b></td>
                                        <td> 
                                            <asp:TextBox ID="vintfield1TextBox" MaxLength="7" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" Width="60px"  runat="server" Text='<%# Bind("vintfield1") %>'></asp:TextBox>
                                            <asp:RegularExpressionValidator ID="RegularExpressionValidator22" runat="server" SetFocusOnError="true" ControlToValidate="vintfield1TextBox" ErrorMessage="Invalid load tags" Display="Dynamic" ValidationExpression="(^([0-9]*|\d*\d{1}?\d*)$)"></asp:RegularExpressionValidator>
                                        </td>
                                        <td nowrap><b>Whse Days:</b>
                                            <asp:TextBox ID="vshipdaysTextBox" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" Width="50px"  runat="server" Text='<%# Bind("vshipdays") %>'></asp:TextBox>
                                            <asp:RegularExpressionValidator ID="RegularExpressionValidator18" runat="server" SetFocusOnError="true" ControlToValidate="vshipdaysTextBox" ErrorMessage="Invalid where Days." Display="Dynamic" ValidationExpression="(^([0-9]*|\d*\d{1}?\d*)$)"></asp:RegularExpressionValidator>
                                        </td>    
                                    </tr>
                                    
                               </tr>
                            </table>
                                <tr>
                                     <td colspan="2">
                                            <table class="shade" style="border:solid 1px black;">
                                            <legend style="color:Blue">Tax Information </legend> 
                                                <tr>
                                                    <td align="right" style="padding-right:5px" > <b>Taxable:</b></td>
                                                    <td>
                                                        <asp:RadioButtonList ID="taxableRadioButtonList1" RepeatLayout="Flow"  CellSpacing="1" RepeatColumns="2"   SelectedValue = '<%# Bind("vsort") %>' Enabled="True" Font-Bold="true" runat="server">
                                                            <asp:ListItem   value="Y" Text="Yes" />
                                                            <asp:ListItem value="N" Text="No" />
                                                        </asp:RadioButtonList>
                                                    </td>
                                                </tr>
                                                <tr>
                                                    <td align="right" style="padding-right:5px" ><b>Tax Code:</b> </td>
                                                    <td>
                                                        <asp:TextBox ID="vtaxgrTextBox" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" Width="80px" runat="server" Text='<%# Bind("vtaxgr") %>'></asp:TextBox><a href="#" tabindex="1" onClick="taxcodelook(); return false"><asp:Image ID="Image14" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
                                                    </td>
                                                    <td>
                                                        <asp:TextBox ID="vfobcodeTextBox" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" Width="80px" runat="server" ></asp:TextBox>
                                                    </td>
                                                </tr>
                                                <tr>
                                                    <td align="right" style="padding-right:5px" ><b>Tax Resale#:</b></td>
                                                    <td>
                                                        <asp:TextBox ID="vtaxidTextBox" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" Width="80px" runat="server" Text='<%# Bind("vtaxid") %>'></asp:TextBox>
                                                    </td>
                                                    <td align="right" style="padding-right:5px" nowrap><b>Exp. Date:</b>
                                                                <asp:TextBox ID="vdatefield2TextBox" ToolTip="MM/DD/YYYY" MaxLength="10" onfocus="javascript:preEnter( this, 'yes' );" onblur="javascript:preLeave( this, 'date', '99/99/9999' );" Width="60px" runat="server" Text='<%# Bind("vdatefield2","{0:MM/dd/yyyy}") %>'></asp:TextBox>
                                                                <a href="#" tabindex="1" onClick="showCalendarControl(FormView1_vdatefield2TextBox); return false"><asp:Image ID="Image16" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
                                                                
                                                            </td>
                                                </tr>
                                            </table>
                                        </td>
                                    </tr>
                                        <%--vordlim:
                                            <asp:TextBox ID="vordlimTextBox" runat="server" Text='<%# Bind("vordlim") %>'>
                                            </asp:TextBox>--%>
                                   <tr>
                                        <td>
                                            <asp:Button ID="InsertButton" CssClass="button" runat="server" CausesValidation="True" OnClick="addButton_Click" Text="Save"></asp:Button>
                                            <asp:Button ID="InsertCancelButton" CssClass="button" runat="server" CausesValidation="False" CommandName="Cancel" Text="Cancel"></asp:Button>
                                        </td>
                                    </tr>
                                </table>
                            </asp:Panel>
                </InsertItemTemplate>
              
   <ItemTemplate>
        <table style="border: 1px solid " class="shade">
            <tr>
                <td>
                    <table  class="shade"  style="border:solid 1px black;" width="430px">
                        <tr>
                            <td align="right" style="padding-right:5px" ><b>Customer:</b></td>
                            <td>
                                <asp:Label ID="vcustnoLabel" Width="150px" runat="server"  BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px"  Text='<%# Bind("vcustno") %>'></asp:Label>
                            </td>
                            <td><b>Status:</b>
                                <asp:DropDownList ID="DropDownList2" Height="1px"  Width="75px" BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" Enabled="false" runat="server" SelectedValue='<%# Bind("vactive") %>'>
                                    <asp:ListItem Value="A">Active</asp:ListItem>
                                    <asp:ListItem Value="I">Inactive</asp:ListItem>
                                    <asp:ListItem Value="X">Inhouse</asp:ListItem>
                                    <asp:ListItem Value="S">Statement</asp:ListItem>
                                    <asp:ListItem Value="E">Service</asp:ListItem>                   
                                </asp:DropDownList>             
                            </td>
                        </tr>
                        <tr>
                            <td align="right" style="padding-right:5px" > <b>Name:</b></td>
                            <td>
                                <asp:Label ID="vcustnameLabel" runat="server" Width="150px" BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px"  Text='<%# Bind("vcustname") %>'></asp:Label>
                            </td>
                        </tr> 
                        <tr>
                            <td align="right" style="padding-right:5px"><b>Address:</b></td>
                            <td>
                                <asp:Label ID="vaddr1Label" runat="server" Width="150px" BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px"  Text='<%# Bind("vaddr1") %>'></asp:Label>
                            </td>
                        </tr> 
                        <tr>
                            <td align="right" style="padding-right:5px"><b>Address:</b></td>
                            <td>
                                <asp:Label ID="vaddr2Label" runat="server" Width="150px" BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px"  Text='<%# Bind("vaddr2") %>'></asp:Label>
                            </td>
                        </tr> 
                        <tr>
                            <td align="right" style="padding-right:5px" ><b>City:</b></td>
                            <td>
                                <asp:Label ID="vcityLabel" runat="server" Width="75px" BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px"  Text='<%# Bind("vcity") %>'></asp:Label>
                                State:
                                <asp:Label ID="vstateLabel" Width="30px"  runat="server" BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px"  Text='<%# Bind("vstate") %>'></asp:Label>
                                Zip:
                                <asp:Label ID="vzipLabel" width="40px" runat="server" BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" Text='<%# Bind("vzip") %>'></asp:Label>
                            </td>
                        </tr>
                        <tr>
                            <td align="right" style="padding-right:5px"><b>Email:</b></td>
                            <td>
                                <asp:Label ID="vemailLabel"  Width="200px" runat="server" BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" Text='<%# Bind("vemail") %>'></asp:Label>
                            </td>
                        </tr>
                    </td>
                </table>             
              </td>
              <td>
                    <table class="shade"  style="border:solid 1px black;" width="550px" >
                        <tr>
                            <td align="right" style="padding-right:5px"><b>Date Added:</b></td>
                            <td colspan="2">
                                <asp:Label ID="vdate1Label" Width="150px" BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" runat="server" Text='<%# Bind("vdate1","{0:MM/dd/yyyy}") %>'></asp:Label>
                            </td>
                        </tr> 
                        <tr>
                            <td  align="right" style="padding-right:5px" ><b>Type:</b></td>
                            <td colspan="2" >
                                <asp:Label ID="vtypeLabel" Width="80px" BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px"  runat="server" Text='<%# Bind("vtype") %>'></asp:Label>
                                <asp:Label ID="vdesctypeLabel" Width="80px" BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px"  runat="server" Text='<%# Bind("vdesctype") %>'></asp:Label>
                            </td>
                        </tr>
                        <tr>
                            <td align="right" style="padding-right:5px" ><b>Contact:</b></td>
                            <td colspan="2">
                                <asp:Label ID="vcontactLabel" runat="server" Width="165px" BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" Text='<%# Bind("vcontact") %>'></asp:Label>
                             </td>
                        </tr>
                        <tr>
                            <td align="right" style="padding-right:5px">  <b>Sales Rep:</b></td>
                            <td colspan="2">
                                <asp:Label ID="vsmanLabel" runat="server" Width="50px" BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" Text='<%# Bind("vsman") %>'></asp:Label>
                                <asp:Label ID="vdescsmanLabel" runat="server" Width="110px" BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" Text='<%# Bind("vdescsman") %>'></asp:Label>
                            &nbsp;&nbsp;&nbsp;&nbsp;
                            <b>Flat Comm:</b>
                                <asp:Label ID="vflatcommLabel" runat="server" Width="50px" BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" Text='<%# Bind("vflatcomm") %>'></asp:Label>
                            </td>
                        </tr>
                        <tr>
                            <td align="right" style="padding-right:5px"><b>Phone:</b></td>
                            <td colspan="2">
                                <asp:Label ID="vareacodeLabel" Width="50px" BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" runat="server" Text='<%# Bind("vareacode") %>'></asp:Label>
                                 <asp:Label ID="vphoneLabel" Width="110px" BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" runat="server" Text='<%# Bind("vphone") %>'></asp:Label>
                            </td>
                        </tr>
                        <tr>
                            <td align="right" style="padding-right:5px"><b>Fax:</b></td>
                            <td colspan="2">
                                <asp:Label ID="vfaxcodeLabel"  Width="50px" BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" runat="server" Text='<%# Bind("vfaxcode") %>'></asp:Label>
                                <asp:Label ID="vfaxLabel" runat="server" Width="115px" BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" Text='<%# Bind("vfax") %>'></asp:Label>
                            &nbsp;&nbsp;&nbsp;&nbsp;
                            <b>Prefix:</b>
                                <asp:Label ID="vfaxprefixLabel" runat="server" Width="80px" BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" Text='<%# Bind("vfaxprefix") %>'>
                                </asp:Label>
                               &nbsp;&nbsp;&nbsp;
                                <b>Country:</b>
                            
                                <asp:Label ID="vfaxcountryLabel" runat="server" Width="80px" BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" Text='<%# Bind("vfaxcountry") %>'>
                                </asp:Label>
                            </td>
                       </tr>
                    </table>
                 </td> 
              </tr>
              <tr>
              <td>
                <table class="shade" style="border:solid 1px black;"  width="430px">
                    <legend style="color:Blue">Credit Information </legend>  
                        <tr>
                            <td align="right" style="padding-right:5px" > <b>Terms:</b></td>
                            <td>
                                <asp:Label ID="vtermsLabel" Width="80px" BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" runat="server" Text='<%# Bind("vterms") %>'></asp:Label>
                            </td>
                            <td>
                                <asp:Label ID="vdesctermsLabel" Width="130px" BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" runat="server" Text='<%# Bind("vdescterms") %>'></asp:Label>
                            </td>
                        </tr>
                        <tr>
                            <td align="right" style="padding-right:5px" ><b>Cr.Acct #:</b></td>
                            <td>
                                <asp:Label ID="vcruseLabel" Width="80px" BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" runat="server" Text='<%# Bind("vcruse") %>'></asp:Label>
                            </td>
                            <td><b>Grace Days:</b>
                                <asp:Label ID="vcrholdinvdaysLabel" Width="50px" BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" runat="server" Text='<%# Bind("vcrholdinvdays") %>'>
                                </asp:Label>
                            </td>
                            <td><b>Dollars:</b> &nbsp;&nbsp;
                                <asp:Label ID="vcrholdinvdueLabel" Width="50px" runat="server" BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" Text='<%# Bind("vcrholdinvdue") %>'>
                                </asp:Label>
                            </td>
                        </tr>
                        <tr>
                            <td align="right" style="padding-right:5px" nowrap ><b>Credit Rating:</b></td>
                            <td>
                                <asp:Label ID="vcrratingLabel" Width="80px" runat="server" BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" Text='<%# Bind("vcrrating") %>'></asp:Label>
                            </td>
                            <td><b>Price Level:</b>
                                <asp:Label ID="vcustlevelLabel" Width="50px" runat="server" BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" Text='<%# Bind("vcustlevel") %>'>
                                </asp:Label>
                            </td>
                            <td nowrap><b>Credit Hold:</b>
                                <asp:CheckBox ID="vcrholdCheckBox"  Width="10px" BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" runat="server" Checked='<%# Bind("vcrhold") %>'
                                    Enabled="false" />
                            </td>
                        </tr>
                        <tr>
                            <td align="right" style="padding-right:5px" nowrap ><b>Credit Limit:</b></td>
                            <td>
                                <asp:Label ID="Label2" Width="80px" runat="server" BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" Text='<%# Bind("vcrlim","{0:###,###,##0.00}") %>'></asp:Label>
                            </td>
                            
                            <td nowrap><b>Finance Charges:</b>
                                <asp:CheckBox ID="vfinchgCheckBox" width="10px" BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" runat="server" Checked='<%# Bind("vfinchg") %>'
                                    Enabled="false" />
                            </td>
                            <td nowrap><b>Auto Reprice:</b>
                                <asp:CheckBox ID="vautorepriceCheckBox" Width="10px" BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" runat="server" Checked='<%# Bind("vautoreprice") %>'
                                    Enabled="false" />
                            </td>
                        </tr>
                        <tr>
                            <td align="right" style="padding-right:5px" nowrap ><b>Order Limit:</b></td>
                            <td>
                                <asp:Label ID="vcrlimLabel" Width="80px" runat="server" BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" Text='<%# Bind("vordlim","{0:###,###,##0.00}") %>'></asp:Label>
                            </td>
                            <td nowrap><b>EDI:</b>
                                <asp:CheckBox ID="vanedicustCheckBox" width="10px" BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" runat="server" Checked='<%# Bind("vanedicust") %>'
                                    Enabled="false" />
                            </td>
                            <td nowrap>
                                <b>Factored:</b>
                                <asp:CheckBox ID="vfactoredCheckBox" Width="10px" BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" runat="server" Checked='<%# Bind("vfactored") %>'
                                    Enabled="false" />
                            </td>
                        </tr>
                        <tr>
                            <td align="right" style="padding-right:5px" ><b>Discount:</b></td>
                            <td>
                                <asp:Label ID="vdiscLabel" width="80px" BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" runat="server" Text='<%# Bind("vdisc") %>'></asp:Label>
                            </td>
                            
                        </tr>
                        <tr>
                            <td align="right" style="padding-right:5px">  
                                <b>Currency:</b>
                            </td>
                            <td>
                                <asp:Label ID="vcurrcodeLabel" Width="80px" BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" runat="server" Text='<%# Bind("vcurrcode") %>'></asp:Label>
                            </td>
                            <td nowrap colspan="2"><b>Invoice Per:</b>
                            
                                <asp:RadioButtonList ID="invoiceRadioButtonList" RepeatLayout="Flow" Enabled="false"  CellSpacing="2" RepeatColumns="3"   SelectedValue = '<%# Bind("vinvmeth") %>'  Font-Bold="true" runat="server">
                                    <asp:ListItem   value="False" Text="BOL" />
                                    <asp:ListItem value="True" Text="PO" /> 
                                    <asp:ListItem value="" Text="Group By Date" />
                                </asp:RadioButtonList>
                                <%--<asp:TextBox ID="TextBox2" Text='<%# Bind("vinvmeth") %>' runat="server"></asp:TextBox>--%>
                            &nbsp;&nbsp;&nbsp;
                            
                            </td>
                        </tr>
                       
                        
                  </table>              
              </td>
                            
              <td>
                    <table class="shade" style="border:solid 1px black;" width="550px" >
                        <legend style="color:Blue">Shipping Information </legend> 
                            <tr nowrap>
                                <td colspan="7">
                                    <b>Freight Payment:</b>
                                        <asp:RadioButtonList ID="FreightRadioButtonList" RepeatLayout="Flow"  Enabled="false" CellSpacing="1" RepeatColumns="4"     SelectedValue ='<%# Bind("vfrtpay") %>' Font-Bold="true"  runat="server">
                                            <asp:ListItem value="B"  Text="Bill"   />
                                            <asp:ListItem value="C" Text="Collect" />
                                            <asp:ListItem value="P" Text="Perpaid" />
                                            <asp:ListItem  Value="T" Text="3rd Party" />  </asp:RadioButtonList>
                                        <%--<asp:TextBox ID="TextBox1" runat="server" Text ='<%# Bind("vfrtpay") %>'></asp:TextBox>--%>
                                    &nbsp;&nbsp;&nbsp;
                                    <b>FOB:</b>
                                        <asp:RadioButtonList ID="FOBRadioButtonList" RepeatLayout="Flow"  CellSpacing="1" RepeatColumns="2"   SelectedValue = '<%# Bind("vfobcode") %>' Enabled="false" Font-Bold="true"   runat="server">
                                            <asp:ListItem value="DEST"  Text="Destination"   />
                                            <asp:ListItem  value="ORIG" Text="Origin" />
                                        </asp:RadioButtonList>
                                </td>
                            </tr>                            
                            <tr>
                                <td align="right" style="padding-right:5px"><b>Location:</b></td>
                                <td>
                                    <asp:Label ID="vlocLabel" Width="80px" BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" runat="server" Text='<%# Bind("vloc") %>'></asp:Label>
                                </td>
                                <td>
                                    <asp:Label ID="vdesclocLabel" BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" runat="server" Text='<%# Bind("vdescloc") %>'></asp:Label>
                                </td>
                                <td><b>Partial Ship:</b></td>
                                <td>
                                    <asp:CheckBox ID="vshippartCheckBox" Width="10px" BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" runat="server" Checked='<%# Bind("vshippart") %>'
                                        Enabled="false" />
                                </td>
                            </tr>
                            <tr>
                                <td align="right" style="padding-right:5px"><b>Carrier:</b></td>
                                <td>
                                    <asp:Label ID="vcarrierLabel" Width="80px"  BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" runat="server" Text='<%# Bind("vcarrier") %>'></asp:Label>
                                </td>
                                <td>    
                                    <asp:Label ID="vdescarrierLabel" BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" runat="server" Text='<%# Bind("vdescarrier") %>'>
                                    </asp:Label>
                                </td>
                                <td nowrap><b>Po # Mandatory:</b></td>
                                <td>
                                    <asp:CheckBox ID="vmandatoryCheckBox" Width="10px" BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" runat="server"  
                                       Checked='<%# Bind("vmandatory") %>' Enabled="false" />
                                </td>
                            </tr>
                            <tr>
                                <td align="right" style="padding-right:5px" nowrap> <b>Delivery Zone:</b></td>
                                <td>
                                    <asp:Label ID="vdelzoneLabel"  Width="80px" BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" runat="server" Text='<%# Bind("vdelzone") %>'></asp:Label>
                                </td>
                                <td>
                                    <asp:Label ID="vdeszoneLabel" BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" runat="server" Text='<%# Bind("vdeszone") %>'></asp:Label>
                                </td>
                                <td align="right" style="padding-right:5px" > <b>Pallet:</b></td>
                                <td>
                                    <asp:Label ID="vpalletLabel" Width="80px" BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" runat="server" Text='<%# Bind("vpallet") %>'></asp:Label>
                                </td>
                            </tr>
                            <tr>
                                <td align="right" style="padding-right:5px"> <b>Territory:</b></td>
                                <td>
                                    <asp:Label ID="vterrLabel" Width="80px"  BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" runat="server" Text='<%# Bind("vterr") %>'></asp:Label>
                                </td>
                                <td> 
                                    <asp:Label ID="vdesterrLabel" BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" runat="server" Text='<%# Bind("vdesterr") %>'></asp:Label>
                                </td>
                                <td align="right" style="padding-right:5px" ><b>Case Bundle:</b></td>
                                <td>
                                    <asp:Label ID="vcasebundleLabel" Width="80px"  BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" runat="server" Text='<%# Bind("vcasebundle") %>'>
                                    </asp:Label>
                                </td>
                            </tr>
                            <tr>
                            <td align="right" style="padding-right:5px"><b>Overrun%:</b></td>
                                <td>
                                    <asp:Label ID="voverpctLabel" Width="80px"  BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" runat="server" Text='<%# Bind("voverpct","{0:##0.00}") %>'></asp:Label>
                                </td>
                                <td align="right" style="padding-right:5px"><b>Underrun%:</b></td>
                                <td>
                                    <asp:Label ID="vunderpctLabel" Width="80px"  BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" runat="server" Text='<%# Bind("vunderpct","{0:##0.00}") %>'></asp:Label>
                                </td>
                                
                            </tr>
                           
                            <tr>
                                <td align="right" style="padding-right:5px"><b>Markup:</b></td>
                                <td>
                                    <asp:Label ID="vmarkupLabel" Width="80px"  BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" runat="server"  Text='<%# Bind("vmarkup") %>'></asp:Label>
                                </td>
                                <td align="right" style="padding-right:5px">  <b>Load Tags:</b></td>
                                <td>
                                    <asp:Label ID="vintfield1Label" Width="80px"  BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" runat="server" Text='<%# Bind("vintfield1") %>'>
                                    </asp:Label>
                                </td>
                                <td align="right" style="padding-right:5px" ><b>Whse Days:</b></td>
                                <td>
                                    <asp:Label ID="vshipdaysLabel" Width="80px"  BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" runat="server" Text='<%# Bind("vshipdays") %>'></asp:Label>
                                </td>    
                            </tr>
                            
                      </table>
                    </td>
                </tr>
                <tr>
                    <td colspan="2">
                        <table class="shade"  style="border:solid 1px black;" width="430px" >
                            <legend style="color:Blue">Tax Information </legend>  
                                <tr>
                                    <td align="right" style="padding-right:5px" ><b>Taxable:</b></td>
                                    <td>
                                        <asp:RadioButtonList ID="RadioButtonList1" RepeatLayout="Flow"  CellSpacing="1" RepeatColumns="2"   SelectedValue = '<%# Bind("vsort") %>' Enabled="false" Font-Bold="true" runat="server">
                                            <asp:ListItem  value="Y" Text="Yes" />
                                            <asp:ListItem value="N" Text="No" />
                                        </asp:RadioButtonList>
                                    </td>
                                </tr>
                                <tr>
                                    <td align="right" style="padding-right:5px" nowrap><b>Tax Code:</b> </td>
                                    <td>
                                        <asp:Label ID="vtaxgrLabel" Width="80px" BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" runat="server" Text='<%# Bind("vtaxgr") %>'></asp:Label>
                                    </td>
                                    <td>
                                        <asp:Label ID="Label1" Width="120px" BackColor="Turquoise" BorderColor="white" BorderStyle="solid" Text='<%# Bind("vtaxdscr") %>' BorderWidth="1px" runat="server" ></asp:Label>
                                    </td>
                                </tr>
                                <tr>
                                    <td align="right" style="padding-right:5px" nowrap><b>Tax Resale#:</b></td>
                                    <td>
                                        <asp:Label ID="vtaxidLabel" Width="80px" BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" runat="server" Text='<%# Bind("vtaxid") %>'></asp:Label>
                                    </td>
                                    <td align="right" style="padding-right:5px" nowrap><b>Exp. Date:</b></td>
                                    <td>
                                        <asp:Label ID="vdatefield2Label" Width="80px" BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" runat="server" Text='<%# Bind("vdatefield2","{0:MM/dd/yyyy}") %>'>
                                        </asp:Label>
                                    </td>
                                </tr>
                            </table>
                        </td>
                    </tr>
                    <tr>
                        <td colspan="3">
                            <asp:Button ID="AddButton" CssClass="button" runat="server" CausesValidation="False" CommandName="new" Text="Add"></asp:Button>
                            <asp:Button ID="UpdateButton" runat="server" CssClass="button" CausesValidation="False" CommandName="edit" Text="Update"></asp:Button>
                            <asp:Button ID="DeleteButton" runat="server" CssClass="button" CausesValidation="False"   OnClick="Deletebutton_Click" OnClientClick="return confirm('Are you sure to want to Delete?')" Text="Delete"></asp:Button>
                        </td>
                    </tr>
                            <%--<fieldset  style="background-color:#EFF3FB  ;  " > <legend style="color:Blue "> Customer Information</legend> --%>
                            <%--   vordlim:
                                    <asp:Label ID="vordlimLabel" runat="server" Text='<%# Bind("vordlim") %>'></asp:Label>
                            --%>                
                  
          </table>
      </ItemTemplate>
  </asp:FormView>
  
          <asp:ObjectDataSource ID="ObjectDataSource1" runat="server" OldValuesParameterFormatString="original_{0}"
              SelectMethod="SelectCustomer" TypeName="contact">
              <SelectParameters>
                  <asp:Parameter DefaultValue="View" Name="prmAction" Type="String" />
                  <asp:Parameter Name="prmComp" Type="String" />                  
                  <asp:Parameter Name="prmUser" Type="String"  DefaultValue="" />
                  <asp:SessionParameter Name="prmcustno" SessionField="customer1_list_cust" Type="String" />
                  <asp:Parameter Name="prmcustname" Type="String" />
                  <asp:Parameter Name="prmcity" Type="String" />
                  <asp:Parameter Name="prmstate" Type="String" />
                  <asp:Parameter Name="prmzip" Type="String" />
                  <asp:Parameter Name="prmtype" Type="String" />
                  <asp:Parameter Name="prmsman" Type="String" />
                  <asp:Parameter Name="prmterr" Type="String" />
                  <asp:Parameter Name="prmactive" Type="String" />
                  <asp:Parameter Name="prmdate1" Type="DateTime" />
                  <asp:Parameter Name="prmaddr1" Type="String" />
                  <asp:Parameter Name="prmaddr2" Type="String" />
                  <asp:Parameter Name="prmemail" Type="String" />
                  <asp:Parameter Name="prmterms" Type="String" />
                  <asp:Parameter Name="prmcruse" Type="String" />
                  <asp:Parameter Name="prmcrrating" Type="String" />                 
                  <asp:Parameter Name="prmordlim" Type="Decimal" />
                  <asp:Parameter Name="prmdisc" Type="Decimal" />
                  <asp:Parameter Name="prmcurrcode" Type="String" />
                  <asp:Parameter Name="prmcrholdinvdays" Type="Int32" />
                  <asp:Parameter Name="prmcrholdinvdue" Type="Decimal" />
                  <asp:Parameter Name="prmcustlevel" Type="Int32" />
                  <asp:Parameter Name="prmcrhold" Type="String" />
                  <asp:Parameter Name="prmfinchg" Type="String" />
                  <asp:Parameter Name="prmautoreprice" Type="String" />
                  <asp:Parameter Name="prmanedicust" Type="String" />
                  <asp:Parameter Name="prmfactored" Type="String" />
                  <asp:Parameter Name="prmsort" Type="String" />
                  <asp:Parameter Name="prmtaxgr" Type="String" />
                  <asp:Parameter Name="prmtaxid" Type="String" />
                  <asp:Parameter Name="prmdatefield2" Type="DateTime" />
                  <asp:Parameter Name="prmcontact" Type="String" />
                  <asp:Parameter Name="prmareacode" Type="String" />
                  <asp:Parameter Name="prmphone" Type="String" />
                  <asp:Parameter Name="prmfaxprefix" Type="String" />
                  <asp:Parameter Name="prmfaxcountry" Type="String" />
                  <asp:Parameter Name="prmfrtpay" Type="String" />
                  <asp:Parameter Name="prmfobcode" Type="String" />
                  <asp:Parameter Name="prmshippart" Type="String" />
                  <asp:Parameter Name="prmloc" Type="String" />
                  <asp:Parameter Name="prmcarrier" Type="String" />
                  <asp:Parameter Name="prmdelzone" Type="String" />
                  <asp:Parameter Name="prmunderpct" Type="Decimal" />
                  <asp:Parameter Name="prmoverpct" Type="Decimal" />
                  <asp:Parameter Name="prmmarkup" Type="Decimal" />
                  <asp:Parameter Name="prmshipdays" Type="Int32" />
                  <asp:Parameter Name="prmpallet" Type="String" />
                  <asp:Parameter Name="prmcasebundle" Type="String" />
                  <asp:Parameter Name="prmintfield1" Type="Int32" />
                  <asp:Parameter Name="prmdescsman" Type="String" />
                  <asp:Parameter Name="prmdesctype" Type="String" />
                  <asp:Parameter Name="prmdescterms" Type="String" />
                  <asp:Parameter Name="prmdescloc" Type="String" />
                  <asp:Parameter Name="prmdescarrier" Type="String" />
                  <asp:Parameter Name="prmdesterr" Type="String" />
                  <asp:Parameter Name="prmdeszone" Type="String" />
                  <asp:Parameter Name="prmflatcomm" Type="Decimal" />
                  <asp:Parameter Name="prminvmeth" Type="String" />
                  <asp:Parameter Name="prmmandatory" Type="String" />
                  <asp:Parameter Name="prmfax" Type="String" />
                  <asp:Parameter Name="prmfaxcode" Type="String" />
                  <asp:Parameter Name="prmcrlim" Type="decimal" />
              </SelectParameters>
          </asp:ObjectDataSource>
          <asp:Button ID="newaddButton" runat="server" CssClass="button"  OnClick="newaddButton_Click" Text="Add" />
          
          
          
        <asp:FormView ID="FormView2" Visible="false" runat="server" DataSourceID="ObjectDataSource2">
            <EditItemTemplate>
                vActive:
                <asp:TextBox ID="vActiveTextBox" runat="server" Text='<%# Bind("vActive") %>'>
                </asp:TextBox><br />
                vTerms:
                <asp:TextBox ID="vTermsTextBox" runat="server" Text='<%# Bind("vTerms") %>'>
                </asp:TextBox><br />
                vLoc:
                <asp:TextBox ID="vLocTextBox" runat="server" Text='<%# Bind("vLoc") %>'>
                </asp:TextBox><br />
                vCarrier:
                <asp:TextBox ID="vCarrierTextBox" runat="server" Text='<%# Bind("vCarrier") %>'>
                </asp:TextBox><br />
                vDelzone:
                <asp:TextBox ID="vDelzoneTextBox" runat="server" Text='<%# Bind("vDelzone") %>'>
                </asp:TextBox><br />
                vUnderpct:
                <asp:TextBox ID="vUnderpctTextBox" runat="server" Text='<%# Bind("vUnderpct") %>'>
                </asp:TextBox><br />
                vOverpct:
                <asp:TextBox ID="vOverpctTextBox" runat="server" Text='<%# Bind("vOverpct") %>'>
                </asp:TextBox><br />
                vMarkup:
                <asp:TextBox ID="vMarkupTextBox" runat="server" Text='<%# Bind("vMarkup") %>'>
                </asp:TextBox><br />
                vShipdays:
                <asp:TextBox ID="vShipdaysTextBox" runat="server" Text='<%# Bind("vShipdays") %>'>
                </asp:TextBox><br />
                vPallet:
                <asp:TextBox ID="vPalletTextBox" runat="server" Text='<%# Bind("vPallet") %>'>
                </asp:TextBox><br />
                vCasebundle:
                <asp:TextBox ID="vCasebundleTextBox" runat="server" Text='<%# Bind("vCasebundle") %>'>
                </asp:TextBox><br />
                vIntfield:
                <asp:TextBox ID="vIntfieldTextBox" runat="server" Text='<%# Bind("vIntfield") %>'>
                </asp:TextBox><br />
                vInvmeth:
                <%--<asp:CheckBox ID="vInvmethCheckBox" runat="server" Checked='<%# Bind("vInvmeth") %>' /><br />--%>
                vFrtpay:
                <asp:TextBox ID="vFrtpayTextBox" runat="server" Text='<%# Bind("vFrtpay") %>'>
                </asp:TextBox><br />
                vFobcode:
                <asp:TextBox ID="vFobcodeTextBox" runat="server" Text='<%# Bind("vFobcode") %>'>
                </asp:TextBox><br />
                vTaxgr:
                <asp:TextBox ID="vTaxgrTextBox" runat="server" Text='<%# Bind("vTaxgr") %>'>
                </asp:TextBox><br />
                vTaxid:
                <asp:TextBox ID="vTaxidTextBox" runat="server" Text='<%# Bind("vTaxid") %>'>
                </asp:TextBox><br />
                vCurrcode:
                <asp:TextBox ID="vCurrcodeTextBox" runat="server" Text='<%# Bind("vCurrcode") %>'>
                </asp:TextBox><br />
                vOrdlim:
                <asp:TextBox ID="vOrdlimTextBox" runat="server" Text='<%# Bind("vOrdlim") %>'>
                </asp:TextBox><br />
                vCrlim:
                <asp:TextBox ID="vCrlimTextBox" runat="server" Text='<%# Bind("vCrlim") %>'>
                </asp:TextBox><br />
                vType:
                <asp:TextBox ID="vTypeTextBox" runat="server" Text='<%# Bind("vType") %>'>
                </asp:TextBox><br />
                vSman:
                <asp:TextBox ID="vSmanTextBox" runat="server" Text='<%# Bind("vSman") %>'>
                </asp:TextBox><br />
                vdescsman:
                <asp:TextBox ID="vdescsmanTextBox" runat="server" Text='<%# Bind("vdescsman") %>'>
                </asp:TextBox><br />
                vdesctype:
                <asp:TextBox ID="vdesctypeTextBox" runat="server" Text='<%# Bind("vdesctype") %>'>
                </asp:TextBox><br />
                vdescterms:
                <asp:TextBox ID="vdesctermsTextBox" runat="server" Text='<%# Bind("vdescterms") %>'>
                </asp:TextBox><br />
                vdescloc:
                <asp:TextBox ID="vdesclocTextBox" runat="server" Text='<%# Bind("vdescloc") %>'>
                </asp:TextBox><br />
                vdescarrier:
                <asp:TextBox ID="vdescarrierTextBox" runat="server" Text='<%# Bind("vdescarrier") %>'>
                </asp:TextBox><br />
                vdesterr:
                <asp:TextBox ID="vdesterrTextBox" runat="server" Text='<%# Bind("vdesterr") %>'>
                </asp:TextBox><br />
                vdeszone:
                <asp:TextBox ID="vdeszoneTextBox" runat="server" Text='<%# Bind("vdeszone") %>'>
                </asp:TextBox><br />
                
                vterr:
                <asp:TextBox ID="vterrTextBox" runat="server" Text='<%# Bind("vterr") %>'>
                </asp:TextBox><br />
                vtaxcodedesc:
                <asp:TextBox ID="vTaxCodeDescTextbox" runat="server" Text='<%# Bind("vtexcode") %>'>
                </asp:TextBox><br />
                <asp:Button ID="UpdateButton" runat="server" CausesValidation="True" CommandName="Update"
                    Text="Save">
                </asp:Button>
                <asp:Button ID="UpdateCancelButton" runat="server" CausesValidation="False" CommandName="Cancel"
                    Text="Cancel">
                </asp:Button>
            </EditItemTemplate>
            
            <ItemTemplate>
                vActive:
                <asp:Label ID="vActiveLabel" runat="server" Text='<%# Bind("vActive") %>'></asp:Label><br />
                vTerms:
                <asp:Label ID="vTermsLabel" runat="server" Text='<%# Bind("vTerms") %>'></asp:Label><br />
                vLoc:
                <asp:Label ID="vLocLabel" runat="server" Text='<%# Bind("vLoc") %>'></asp:Label><br />
                vCarrier:
                <asp:Label ID="vCarrierLabel" runat="server" Text='<%# Bind("vCarrier") %>'></asp:Label><br />
                vDelzone:
                <asp:Label ID="vDelzoneLabel" runat="server" Text='<%# Bind("vDelzone") %>'></asp:Label><br />
                vUnderpct:
                <asp:Label ID="vUnderpctLabel" runat="server" Text='<%# Bind("vUnderpct") %>'></asp:Label><br />
                vOverpct:
                <asp:Label ID="vOverpctLabel" runat="server" Text='<%# Bind("vOverpct") %>'></asp:Label><br />
                <asp:LinkButton ID="lnk1" runat="server" CommandName="edit">edit</asp:LinkButton>
            </ItemTemplate>
        </asp:FormView>  
                    <asp:ObjectDataSource ID="ObjectDataSource2" runat="server" OldValuesParameterFormatString="original_{0}"
                        SelectMethod="SelectCustStock" TypeName="contact">
                        <SelectParameters>
                            <asp:Parameter Name="prmAction" DefaultValue="CustStock" Type="String" />
                            <asp:SessionParameter SessionField="Customers_Company" Name="prmComp" Type="String" />
                            <asp:SessionParameter SessionField="customer_user_id" Name="prmUser" Type="String" />
                        </SelectParameters>
                    </asp:ObjectDataSource>
    </div>
    <ft:footer id="Footer1" runat="server"></ft:footer>
    </form>
  </body>
</HTML>

