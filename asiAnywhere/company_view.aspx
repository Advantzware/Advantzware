<%@ Page Language="c#" AutoEventWireup="true" Debug="true" Inherits="company_view" Codebehind="company_view.aspx.cs" %>

<%@ Register Src="footer.ascx" TagName="Footer" TagPrefix="ft" %>
<%@ Register Src="header.ascx" TagName="Header" TagPrefix="hd" %>
<html xmlns="http://www.w3.org/1999/xhtml" >
  <head id="Head1" runat="server">
    <title>Company</title>
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


 
 
function currencylook()
{ 
  var NewWindow = window.open("currency_lookup.aspx","CurrencyLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}
function CurrencyLookup(ReturnObj1, ReturnObj2)
{
    document.forms[0].FormView1_curr_codeTextBox.value = ReturnObj1;
    document.forms[0].FormView1_cdescTextBox.value = ReturnObj2;
    document.forms[0].FormView1_curr_codeTextBox.focus();
      
}
 
 
 
 function Companylook()
 { 
  var NewWindow = window.open("company_lookup.aspx","CompanyLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
 }
 function CompanyLookup(ReturnObj1)
{
    document.forms[0].FormView1_co_accTextBox.value = ReturnObj1;
    document.forms[0].FormView1_co_accTextBox.focus();
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
            <table><tr><td><div>
            <table align="left" border="1" width="95%">
                <tr class="topheadcolor">                                      
                 
                        <td nowrap width="25px";>
                        <asp:ImageButton ID="img_btn_add" runat="server" Width="35px" ImageUrl="~/Images/add.bmp" ToolTip="Add" OnClick="img_btn_add_click" />
                        </td>                       
                        <td nowrap width="25px";>
                        <asp:ImageButton ID="img_btn_exit" runat="server" Width="35px" ImageUrl="~/Images/exit-au.bmp" ToolTip="LogOut" OnClick="hlnkLogOut_Click" />
                        </td>
                        <td nowrap> &nbsp;</td>
                </tr>
            </table></div></td></tr>
            <tr><td>
            
                <div>
            
                    <TABLE id="tblTop" cellSpacing="3" align="center" border="0" Width="100%">
                        <TR>
                             <TD width=30>&nbsp;</TD>
                            <TD align=center nowrap><font size=+0><b>Company&nbsp;</b></font></TD>
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
                                <asp:LinkButton ID="lnk_Listcompany" runat="server" OnClick="lnk_Listcompany_Click" >Brws Company</asp:LinkButton></li>
                                <li class="selected"><asp:LinkButton ID="lnk_viewcompany" runat="server"  OnClick="lnk_viewcompany_Click"  > View Company</asp:LinkButton></li>
                                <li><asp:LinkButton ID="lnk_listperiod" runat="server" OnClick="lnk_listperiod_Click" >Open periods</asp:LinkButton></li>
                                <li><asp:LinkButton ID="lnk_viewperiod" runat="server"  OnClick="lnk_viewperiod_Click"  > View period</asp:LinkButton></li></ul></div>
                                
                            </td>      
                        </tr>
                   </table>
            
            <asp:FormView ID="FormView1" runat="server" DataSourceID="ObjectDataSource1" OnDataBound="FormView1_DataBound" >
                <EditItemTemplate>
                    <asp:Panel ID="editpanel" Width="500px" CssClass="shade" runat="server" DefaultButton="UpdateButton">
                    <fieldset>
                    <table class="shade"> <tr><td align="right" style="padding-right:5px;"><b>Company:</b></td>
                    <td><asp:Label ID="companyLabel" Width="100px" BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" runat="server" Text='<%# Bind("company") %>' /></td>
                    <td align="right" style="padding-right:5px;"><b>Federal ID:</b></td>
                    <td><asp:TextBox ID="fidTextBox" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)"  runat="server" Text='<%# Bind("fid") %>' /></td></tr> 
                    <tr><td align="right" style="padding-right:5px;"><b>Company Name:</b></td>
                    <td><asp:TextBox ID="vnameTextBox" Width="180px" runat="server" Text='<%# Bind("vname") %>' /></td>
                    <td align="right" style="padding-right:5px;"><b>State ID:</b></td>
                    <td><asp:TextBox ID="sidTextBox" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" runat="server" Text='<%# Bind("sid") %>' /></td></tr>
                    
                    <tr><td align="right" style="padding-right:5px;"><b>Address:</b></td>
                    <td><asp:TextBox ID="addr1TextBox" Width="180px" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" runat="server" Text='<%# Bind("addr1") %>' /></td>
                    <td></td><td></td></tr>
                    <tr><td align="right" style="padding-right:5px;"><b>Address:</b></td>
                    <td><asp:TextBox ID="addr2TextBox" Width="180px" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" runat="server" Text='<%# Bind("addr2") %>' /></td><td></td><td></td></tr>
                    <tr><td align="right" style="padding-right:5px;"><b>City:</b></td>
                    <td colspan="2">
                    <asp:TextBox ID="cityTextBox" Width="90px" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" runat="server" Text='<%# Bind("city") %>' />                    
                    <asp:TextBox ID="stateTextBox" Width="40px" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" runat="server" Text='<%# Bind("state") %>' />                    
                    <asp:TextBox ID="zipTextBox" Width="90px" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" runat="server" Text='<%# Bind("zip") %>' />
                    </td></tr>
                    </table>
                    <table class="shade">
                    <tr><td align="right" style="padding-right:5px;"><b>Account Company:</b></td>
                    <td><asp:TextBox ID="co_accTextBox" runat="server" onfocus="document.getElementById('FormView1_num_perTextBox').focus()" Width="50px" ForeColor="#ACA899" Text='<%# Bind("[co-acc]") %>' />
                    </td>
                    <td align="right" style="padding-right:5px;"><b>Number of periods:</b></td>
                    <td><asp:TextBox ID="num_perTextBox" runat="server" MaxLength="2" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" Width="50px" Text='<%# Bind("[num-per]") %>' />
                    <asp:CompareValidator ID="CompareValidator1" runat="server" ControlToValidate="num_perTextBox" Operator="DataTypeCheck" Type="Integer" Display="Dynamic" SetFocusOnError="true" ErrorMessage="Enter Integer Value"></asp:CompareValidator>
                    </td></tr>
                    <tr><td align="right" style="padding-right:5px;"><b>G/L Account - # of Levels</b></td>
                    <td><asp:TextBox ID="acc_levelTextBox" MaxLength="1" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" Width="50px" runat="server" Text='<%# Bind("[acc-level]") %>' />
                    <asp:CompareValidator ID="CompareValidator9" runat="server" ControlToValidate="acc_levelTextBox" Operator="DataTypeCheck" Type="Integer" Display="Dynamic" SetFocusOnError="true" ErrorMessage="Enter Integer Value"></asp:CompareValidator>
                    </td>
                    <td colspan="2"><b>Digits:</b>
                    <asp:TextBox ID="acc_dig1TextBox" Width="20px" MaxLength="1" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" runat="server" Text='<%# Bind("[acc-dig1]") %>' />
                    <asp:CompareValidator ID="CompareValidator10" runat="server" ControlToValidate="acc_dig1TextBox" Operator="DataTypeCheck" Type="Integer" Display="Dynamic" SetFocusOnError="true" ErrorMessage="Enter Integer Value"></asp:CompareValidator>
                    <asp:TextBox ID="acc_dig2TextBox" Width="20px" MaxLength="1" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" runat="server" Text='<%# Bind("[acc-dig2]") %>' />
                    <asp:CompareValidator ID="CompareValidator11" runat="server" ControlToValidate="acc_dig2TextBox" Operator="DataTypeCheck" Type="Integer" Display="Dynamic" SetFocusOnError="true" ErrorMessage="Enter Integer Value"></asp:CompareValidator>
                    <asp:TextBox ID="acc_dig3TextBox" Width="20px" MaxLength="1" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" runat="server" Text='<%# Bind("[acc-dig3]") %>' />
                    <asp:CompareValidator ID="CompareValidator12" runat="server" ControlToValidate="acc_dig3TextBox" Operator="DataTypeCheck" Type="Integer" Display="Dynamic" SetFocusOnError="true" ErrorMessage="Enter Integer Value"></asp:CompareValidator>
                    <asp:TextBox ID="acc_dig4TextBox" Width="20px" MaxLength="1" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" runat="server" Text='<%# Bind("[acc-dig4]") %>' />
                    <asp:CompareValidator ID="CompareValidator13" runat="server" ControlToValidate="acc_dig4TextBox" Operator="DataTypeCheck" Type="Integer" Display="Dynamic" SetFocusOnError="true" ErrorMessage="Enter Integer Value"></asp:CompareValidator>
                    <asp:TextBox ID="acc_dig5TextBox" Width="20px" MaxLength="1" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" runat="server" Text='<%# Bind("[acc-dig5]") %>' />
                    <asp:CompareValidator ID="CompareValidator14" runat="server" ControlToValidate="acc_dig5TextBox" Operator="DataTypeCheck" Type="Integer" Display="Dynamic" SetFocusOnError="true" ErrorMessage="Enter Integer Value"></asp:CompareValidator>
                    </td></tr>
                    <tr><td align="right" style="padding-right:5px;"><b>Calendar Month of year End:</b></td>
                    <td><asp:TextBox ID="yend_offTextBox" Width="50px" MaxLength="2" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" runat="server" Text='<%# Bind("[yend-off]") %>' />
                    <asp:CompareValidator ID="CompareValidator15" runat="server" ControlToValidate="yend_offTextBox" Operator="DataTypeCheck" Type="Integer" Display="Dynamic" SetFocusOnError="true" ErrorMessage="Enter Integer Value"></asp:CompareValidator>
                    </td>
                    <td></td><td></td></tr>
                    <tr><td align="right" style="padding-right:5px;"><b>First Open Year:</b></td>
                    <td><asp:TextBox ID="firstyearTextBox" Width="50px" onfocus="document.getElementById('FormView1_curr_codeTextBox').focus()" runat="server" ForeColor="#ACA899" Text='<%# Bind("firstyear") %>' /></td><td></td><td></td></tr>
                    <tr><td align="right" style="padding-right:5px;"><b>Previous Year Closed?:</b></td>
                    <td><asp:TextBox ID="yend_perTextBox" Width="50px" ForeColor="#ACA899" onfocus="document.getElementById('FormView1_curr_codeTextBox').focus()" runat="server"  Text='<%# Bind("[yend-per]") %>' /></td><td></td><td></td></tr>
                    <tr><td align="right" style="padding-right:5px;"><b>First Open Period:</b></td>
                    <td><asp:TextBox ID="prdnumTextBox" Width="50px" onfocus="document.getElementById('FormView1_curr_codeTextBox').focus()" ForeColor="#ACA899" runat="server" Text='<%# Bind("prdnum") %>' /></td>
                    <td colspan="2"><asp:TextBox ID="prddt1TextBox" Width="70px" onfocus="document.getElementById('FormView1_curr_codeTextBox').focus()" ForeColor="#ACA899" runat="server" Text='<%# Bind("prddt1") %>' />&nbsp; <b>To</b> &nbsp;
                    <asp:TextBox ID="prddt2TextBox" Width="70px" runat="server" onfocus="document.getElementById('FormView1_curr_codeTextBox').focus()" ForeColor="#ACA899" Text='<%# Bind("prddt2") %>' /> </td></tr>
                    <tr><td align="right" style="padding-right:5px;"><b>Currency Code:</b></td>
                    <td><asp:TextBox ID="curr_codeTextBox" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" Width="50px" runat="server" Text='<%# Bind("[curr-code]") %>' />
                    <a href="#" tabindex="1" onClick="currencylook(); return false"><asp:Image ID="Image1" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
                    </td>
                    <td><asp:TextBox ID="cdescTextBox" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this);document.getElementById('FormView1_fidTextBox').focus()" runat="server" Text='<%# Bind("cdesc") %>' /></td><td></td></tr>
                    </table>
                    
                    <asp:TextBox ID="reckeyTextBox" Visible="false" runat="server" Text='<%# Bind("reckey") %>' />
                    
                    <asp:Button ID="UpdateButton" runat="server" CausesValidation="True" OnClick="UpdateButton_Click"
                        CssClass="button" Text="Save" />
                    &nbsp;<asp:Button ID="UpdateCancelButton" runat="server" CssClass="button"
                        CausesValidation="False" CommandName="Cancel" Text="Cancel" />
                        </fieldset></asp:Panel>
                            </EditItemTemplate>
                     
               <InsertItemTemplate>
                     <asp:Panel ID="insertpanel" Width="500px" CssClass="shade" runat="server"  DefaultButton="InsertButton">
                    <fieldset>
                    <table class="shade"> <tr><td align="right" style="padding-right:5px;"><b>Company:</b></td>
                    <td><asp:TextBox ID="companyTextBox" MaxLength="3" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" Width="100px" runat="server" Text='<%# Bind("company") %>' /></td>
                    <td align="right" style="padding-right:5px;"><b>Federal ID:</b></td>
                    <td><asp:TextBox ID="fidTextBox" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)"  runat="server" Text='<%# Bind("fid") %>' /></td></tr> 
                    <tr><td align="right" style="padding-right:5px;"><b>Company Name:</b></td>
                    <td><asp:TextBox ID="vnameTextBox" Width="180px" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" runat="server" Text='<%# Bind("vname") %>' /></td>
                    <td align="right" style="padding-right:5px;"><b>State ID:</b></td>
                    <td><asp:TextBox ID="sidTextBox" runat="server" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" Text='<%# Bind("sid") %>' /></td></tr>
                    
                    <tr><td align="right" style="padding-right:5px;"><b>Address:</b></td>
                    <td><asp:TextBox ID="addr1TextBox" Width="180px" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" runat="server" Text='<%# Bind("addr1") %>' /></td>
                    <td></td><td></td></tr>
                    <tr><td align="right" style="padding-right:5px;"><b>Address:</b></td>
                    <td><asp:TextBox ID="addr2TextBox" Width="180px" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" runat="server" Text='<%# Bind("addr2") %>' /></td><td></td><td></td></tr>
                    <tr><td align="right" style="padding-right:5px;"><b>City:</b></td>
                    <td colspan="2">
                    <asp:TextBox ID="cityTextBox" Width="90px" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" runat="server" Text='<%# Bind("city") %>' />                    
                    <asp:TextBox ID="stateTextBox" Width="40px" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" runat="server" Text='<%# Bind("state") %>' />                    
                    <asp:TextBox ID="zipTextBox" Width="90px" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" runat="server" Text='<%# Bind("zip") %>' />
                    </td></tr>
                    </table>
                    <table class="shade">
                    <tr><td align="right" style="padding-right:5px;"><b>Account Company:</b></td>
                    <td><asp:TextBox ID="co_accTextBox" runat="server" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" Width="50px" Text='<%# Bind("[co-acc]") %>' />
                    <a href="#" tabindex="1" onClick="companylook(); return false"><asp:Image ID="usersman" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
                    </td>
                    <td align="right" style="padding-right:5px;"><b>Number of periods:</b></td>
                    <td><asp:TextBox ID="num_perTextBox" MaxLength="2" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" runat="server" Width="50px" Text='<%# Bind("[num-per]") %>' />
                        <asp:CompareValidator ID="CompareValidator1" runat="server" ControlToValidate="num_perTextBox" Operator="DataTypeCheck" Type="Integer" Display="Dynamic" SetFocusOnError="true" ErrorMessage="Enter Integer Value"></asp:CompareValidator>
                    </td></tr>
                    <tr><td align="right" style="padding-right:5px;"><b>G/L Account - # of Levels</b></td>
                    <td><asp:TextBox ID="acc_levelTextBox" Width="50px" MaxLength="1" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" runat="server" Text='<%# Bind("[acc-level]") %>' />
                    <asp:CompareValidator ID="CompareValidator8" runat="server" ControlToValidate="acc_levelTextBox" Operator="DataTypeCheck" Type="Integer" Display="Dynamic" SetFocusOnError="true" ErrorMessage="Enter Integer Value"></asp:CompareValidator></td>
                    <td colspan="2"><b>Digits:</b>
                    <asp:TextBox ID="acc_dig1TextBox" Width="20px" MaxLength="1" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" runat="server" Text='<%# Bind("[acc-dig1]") %>' />
                    <asp:CompareValidator ID="CompareValidator2" runat="server" ControlToValidate="acc_dig1TextBox" Operator="DataTypeCheck" Type="Integer" Display="Dynamic" SetFocusOnError="true" ErrorMessage="Enter Integer Value"></asp:CompareValidator>
                    <asp:TextBox ID="acc_dig2TextBox" Width="20px" MaxLength="1" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" runat="server" Text='<%# Bind("[acc-dig2]") %>' />
                    <asp:CompareValidator ID="CompareValidator3" runat="server" ControlToValidate="acc_dig2TextBox" Operator="DataTypeCheck" Type="Integer" Display="Dynamic" SetFocusOnError="true" ErrorMessage="Enter Integer Value"></asp:CompareValidator>
                    <asp:TextBox ID="acc_dig3TextBox" Width="20px" MaxLength="1" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" runat="server" Text='<%# Bind("[acc-dig3]") %>' />
                    <asp:CompareValidator ID="CompareValidator4" runat="server" ControlToValidate="acc_dig3TextBox" Operator="DataTypeCheck" Type="Integer" Display="Dynamic" SetFocusOnError="true" ErrorMessage="Enter Integer Value"></asp:CompareValidator>
                    <asp:TextBox ID="acc_dig4TextBox" Width="20px" MaxLength="1" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" runat="server" Text='<%# Bind("[acc-dig4]") %>' />
                    <asp:CompareValidator ID="CompareValidator5" runat="server" ControlToValidate="acc_dig4TextBox" Operator="DataTypeCheck" Type="Integer" Display="Dynamic" SetFocusOnError="true" ErrorMessage="Enter Integer Value"></asp:CompareValidator>
                    <asp:TextBox ID="acc_dig5TextBox" Width="20px" MaxLength="1" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" runat="server" Text='<%# Bind("[acc-dig5]") %>' />
                    <asp:CompareValidator ID="CompareValidator6" runat="server" ControlToValidate="acc_dig5TextBox" Operator="DataTypeCheck" Type="Integer" Display="Dynamic" SetFocusOnError="true" ErrorMessage="Enter Integer Value"></asp:CompareValidator>
                    </td></tr>
                    <tr><td align="right" style="padding-right:5px;"><b>Calendar Month of year End:</b></td>
                    <td><asp:TextBox ID="yend_offTextBox" Width="50px" MaxLength="2" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" runat="server" Text='<%# Bind("[yend-off]") %>' />
                    <asp:CompareValidator ID="CompareValidator7" runat="server" ControlToValidate="yend_offTextBox" Operator="DataTypeCheck" Type="Integer" Display="Dynamic" SetFocusOnError="true" ErrorMessage="Enter Integer Value"></asp:CompareValidator></td>
                    <td></td><td></td></tr>
                    <tr><td align="right" style="padding-right:5px;"><b>First Open Year:</b></td>
                    <td><asp:TextBox ID="firstyearTextBox" Width="50px" Enabled="false" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" runat="server" Text='<%# Bind("firstyear") %>' /></td><td></td><td></td></tr>
                    <tr><td align="right" style="padding-right:5px;"><b>Previous Year Closed?:</b></td>
                    <td><asp:TextBox ID="yend_perTextBox" Width="50px" Enabled="false" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" runat="server"  Text='<%# Bind("[yend-per]") %>' /></td><td></td><td></td></tr>
                    <tr><td align="right" style="padding-right:5px;"><b>First Open Period:</b></td>
                    <td><asp:TextBox ID="prdnumTextBox" Width="50px" Enabled="false" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" runat="server" Text='<%# Bind("prdnum") %>' /></td>
                    <td colspan="2"><asp:TextBox ID="prddt1TextBox" Enabled="false" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" Width="70px" runat="server" Text='<%# Bind("prddt1") %>' />
                    <asp:TextBox ID="prddt2TextBox" Enabled="false" Width="70px" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" runat="server" Text='<%# Bind("prddt2") %>' /> </td></tr>
                    <tr><td align="right" style="padding-right:5px;"><b>Currency Code:</b></td>
                    <td><asp:TextBox ID="curr_codeTextBox" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" Width="50px" runat="server" Text='<%# Bind("[curr-code]") %>' />
                    <a href="#" tabindex="1" onClick="currencylook(); return false"><asp:Image ID="Image1" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
                    </td>
                    <td><asp:TextBox ID="cdescTextBox" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" runat="server" Text='<%# Bind("cdesc") %>' /></td><td></td></tr>
                    </table>
                    
                    <asp:TextBox ID="reckeyTextBox" Visible="false" runat="server" Text='<%# Bind("reckey") %>' />
                 
                    <asp:Button ID="InsertButton" runat="server" CausesValidation="True"  OnClick="addButton_Click"
                        CssClass="button" Text="Save" />
                    &nbsp;<asp:Button ID="InsertCancelButton" runat="server" CssClass="button"
                        CausesValidation="False" CommandName="Cancel" Text="Cancel" /></fieldset></asp:Panel>
                </InsertItemTemplate>
              
   <ItemTemplate>
        <asp:Panel ID="exitpanel" CssClass="shade" width="500px" runat="server" DefaultButton="">
        <fieldset class="shade">
                    <table class="shade" > <tr><td align="right" style="padding-right:5px;"><b>Company:</b></td>
                    <td><asp:Label ID="companyLabel" Width="100px" BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" runat="server" Text='<%# Bind("company") %>' /></td>
                    <td align="right" style="padding-right:5px;"><b>Federal ID:</b></td>
                    <td><asp:Label ID="fidLabel" BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" runat="server" Text='<%# Bind("fid") %>' /></td></tr> 
                    <tr><td align="right" style="padding-right:5px;"><b>Company Name:</b></td>
                    <td><asp:Label ID="vnameLabel" BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" Width="180px" runat="server" Text='<%# Bind("vname") %>' /></td>
                    <td align="right" style="padding-right:5px;"><b>State ID:</b></td>
                    <td><asp:Label ID="sidLabel" BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" runat="server" Text='<%# Bind("sid") %>' /></td></tr>
                    
                    <tr><td align="right" style="padding-right:5px;"><b>Address:</b></td>
                    <td><asp:Label ID="addr1Label" Width="180px" BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" runat="server" Text='<%# Bind("addr1") %>' /></td>
                    <td></td><td></td></tr>
                    <tr><td align="right" style="padding-right:5px;"><b>Address:</b></td>
                    <td><asp:Label ID="addr2Label" Width="180px" BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" runat="server" Text='<%# Bind("addr2") %>' /></td><td></td><td></td></tr>
                    <tr><td align="right" style="padding-right:5px;"><b>City:</b></td>
                    <td colspan="2">
                    <asp:Label ID="cityLabel" Width="90px" BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" runat="server" Text='<%# Bind("city") %>' />                    
                    <asp:Label ID="stateLabel" Width="40px" BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" runat="server" Text='<%# Bind("state") %>' />                    
                    <asp:Label ID="zipLabel" Width="90px" BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" runat="server" Text='<%# Bind("zip") %>' />
                    </td></tr>
                    </table>
                    <table class="shade">
                    <tr><td align="right" style="padding-right:5px;"><b>Account Company:</b></td>
                    <td><asp:Label ID="co_accLabel" runat="server" BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" Width="50px" Text='<%# Bind("[co-acc]") %>' /></td>
                    <td align="right" style="padding-right:5px;"><b>Number of periods:</b></td>
                    <td><asp:Label ID="num_perLabel" runat="server" BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" Width="50px" Text='<%# Bind("[num-per]") %>' /></td></tr>
                    <tr><td align="right" style="padding-right:5px;"><b>G/L Account - # of Levels</b></td>
                    <td><asp:Label ID="acc_levelLabel" Width="50px" BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" runat="server" Text='<%# Bind("[acc-level]") %>' /></td>
                    <td colspan="2"><b>Digits:</b>
                    <asp:Label ID="acc_dig1Label" Width="15px" BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" runat="server" Text='<%# Bind("[acc-dig1]") %>' />
                    <asp:Label ID="acc_dig2Label" Width="15px" BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" runat="server" Text='<%# Bind("[acc-dig2]") %>' />
                    <asp:Label ID="acc_dig3Label" Width="15px" BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" runat="server" Text='<%# Bind("[acc-dig3]") %>' />
                    <asp:Label ID="acc_dig4Label" Width="15px" BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" runat="server" Text='<%# Bind("[acc-dig4]") %>' />
                    <asp:Label ID="acc_dig5Label" Width="15px" BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" runat="server" Text='<%# Bind("[acc-dig5]") %>' />
                    </td></tr>
                    <tr><td align="right" style="padding-right:5px;"><b>Calendar Month of year End:</b></td>
                    <td><asp:Label ID="yend_offLabel" Width="50px" BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" runat="server" Text='<%# Bind("[yend-off]") %>' /></td>
                    <td></td><td></td></tr>
                    <tr><td align="right" style="padding-right:5px;"><b>First Open Year:</b></td>
                    <td><asp:Label ID="firstyearLabel" Width="50px" BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" runat="server" Text='<%# Bind("firstyear") %>' /></td><td></td><td></td></tr>
                    <tr><td align="right" style="padding-right:5px;"><b>Previous Year Closed?:</b></td>
                    <td><asp:Label ID="yend_perLabel" Width="50px" BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" runat="server"  Text='<%# Bind("[yend-per]") %>' /></td><td></td><td></td></tr>
                    <tr><td align="right" style="padding-right:5px;"><b>First Open Period:</b></td>
                    <td><asp:Label ID="prdnumLabel" Width="50px" BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" runat="server" Text='<%# Bind("prdnum") %>' /></td>
                    <td colspan="2"><asp:Label ID="prddt1Label" BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" Width="70px" runat="server" Text='<%# Bind("prddt1") %>' />
                    <asp:Label ID="prddt2Label" Width="70px" BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" runat="server" Text='<%# Bind("prddt2") %>' /> </td></tr>
                    <tr><td align="right" style="padding-right:5px;"><b>Currency Code:</b></td>
                    <td><asp:Label ID="curr_codeLabel" Width="50px" BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" runat="server" Text='<%# Bind("[curr-code]") %>' /></td>
                    <td><asp:Label ID="cdescLabel" runat="server" BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" Text='<%# Bind("cdesc") %>' /></td><td></td></tr>
                    </table>
                    <asp:Label ID="reckeyLabel" Visible="false" runat="server" Text='<%# Bind("reckey") %>' />
                    <asp:Button ID="AddButton" runat="server" CssClass="buttonM" CausesValidation="False" CommandName="new"
                      Text="Add" >
                    </asp:Button>
                    <asp:Button ID="EditButton" runat="server" CssClass="buttonM" CausesValidation="False" CommandName="Edit"
                      Text="Update">
                    </asp:Button>
                   <asp:Button ID="DeleteButton" runat="server" OnClick="Delete_Click" CssClass="buttonM" CausesValidation="False" OnClientClick="return confirm('Are you sure you want to delete')"
                      Text="Delete"></asp:Button>
                   </fieldset></asp:Panel>
      </ItemTemplate>
  </asp:FormView>
  
          <asp:ObjectDataSource ID="ObjectDataSource1" runat="server" OldValuesParameterFormatString="original_{0}"
              SelectMethod="CompanyList" TypeName="ledger">
              <SelectParameters>
                  <asp:Parameter DefaultValue="View" Name="prmAction" Type="String" />
                  <asp:Parameter Name="prmComp" Type="String" />                  
                  <asp:Parameter Name="prmUser" Type="String"  DefaultValue="" />
                  <asp:Parameter Name="prmCompany" Type="String" />
                  <asp:Parameter Name="prmfid" Type="String" />
                  <asp:Parameter Name="prmvname" Type="String" />                 
                  <asp:Parameter Name="prmsid" Type="String" />
                  <asp:Parameter Name="prmaddr1" Type="String" />
                  <asp:Parameter Name="prmaddr2" Type="String" />
                  <asp:Parameter Name="prmcity" Type="String" />
                  <asp:Parameter Name="prmstate" Type="String" />
                  <asp:Parameter Name="prmzip" Type="String" />
                  <asp:Parameter Name="prmcoacc" Type="String" />
                  <asp:Parameter Name="prmnumper" Type="String" />
                  <asp:Parameter Name="prmacclevel" Type="String" />
                  <asp:Parameter Name="prmaccdig1" Type="String" />
                  <asp:Parameter Name="prmaccdig2" Type="String" />
                  <asp:Parameter Name="prmaccdig3" Type="String" />
                  <asp:Parameter Name="prmaccdig4" Type="String" />
                  <asp:Parameter Name="prmaccdig5" Type="String" />
                  <asp:Parameter Name="prmyendoff" Type="String" />
                  <asp:Parameter Name="prmcurrcode" Type="String" />
                  <asp:Parameter Name="prmyendper" Type="String" />
                  <asp:Parameter Name="prmfirstyear" Type="String" />
                  <asp:Parameter Name="prmprdnum" Type="String" />
                  <asp:Parameter Name="prmprddt1" Type="String" />
                  <asp:Parameter Name="prmprddt2" Type="String" />
                  <asp:Parameter Name="prmcdesc" Type="String" />
                  <asp:SessionParameter SessionField="company_list_reckey_name" Name="prmReckey" Type="String" />
              </SelectParameters>
          </asp:ObjectDataSource>
          
          
          
       
    </div></td></tr></table>
    <ft:footer id="Footer1" runat="server"></ft:footer>
    </form>
  </body>
</HTML>

