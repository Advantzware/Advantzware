<%@ Page Language="c#" AutoEventWireup="true" Debug="true" Inherits="period_view" Codebehind="period_view.aspx.cs" %>

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
    <script language = "JavaScript" src="include/CalendarControl.js"></script>
    
    <script language="VBScript">
    Function makeMsgBox(title,message,icon,buttons,defButton,mode)
        butVal = icon + buttons + defButton + mode
        makeMsgBox = MsgBox(message,butVal,title)
    End Function

</script>
<script language="javascript">
    function confirmAdd() {
        var retVal = makeMsgBox("Confirmation", "Update Customer & Vendor Totals?", 48, 4, 256, 4096);
        if (retVal == 6) {
            document.forms[0].HiddenField1.value = "Yes" ;
        }
        else {
            document.forms[0].HiddenField1.value = "No";
        }
    }
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
        <asp:HiddenField ID="HiddenField1" runat="server" />
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
                            <TD align=center nowrap><font size=+0><b>Period&nbsp;</b></font></TD>
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
                                 <li ><asp:LinkButton ID="lnk_viewcompany" runat="server"  OnClick="lnk_viewcompany_Click"  > View Company</asp:LinkButton></li>
                                 <li ><asp:LinkButton ID="lnk_listperiod" runat="server" OnClick="lnk_listperiod_Click" >Open Periods</asp:LinkButton></li>
                                 <li class="selected" ><asp:LinkButton ID="lnk_viewperiod" runat="server"  OnClick="lnk_viewperiod_Click"  >View Period</asp:LinkButton></li></ul></div>
                                
                            </td>      
                        </tr>
                   </table>
                    <asp:FormView ID="FormView2" runat="server" DataSourceID="ObjectDataSource2" >
                    <ItemTemplate>
                    <fieldset style="widows:300px" class="shade"><legend>Reference Information</legend>
                    <table><tr><td>
                    <b>Company:</b></td>
                    <td><asp:Label ID="companyTextBox" Width="70px" BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" runat="server" Text='<%# Bind("company") %>' /></td>
                    <td><b>Name:</b></td>
                    <td><asp:Label ID="cnameTextBox" Width="180px" BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" runat="server" Text='<%# Bind("cname") %>' /></td></tr></table>
                    </fieldset>
                    </ItemTemplate>
                    </asp:FormView>
            <asp:FormView ID="FormView1" runat="server" DataSourceID="ObjectDataSource1" OnDataBound="FormView1_DataBound" >
                <EditItemTemplate>
                    <asp:Panel CssClass="shade" ID="editpanel" runat="server" >
                    <fieldset style="width:360px">
                    <table><tr><td align="right" style="padding-right:5px;"><b>Year:</b></td>
                    <td><asp:TextBox ID="peryrTextBox" ReadOnly="true" BackColor="#008284" runat="server" Width="70px" Text='<%# Bind("peryr") %>' /></td></tr>
                    <tr><td align="right" style="padding-right:5px;"><b>Period:</b></td>
                    <td><asp:TextBox ID="perpnumTextBox" ReadOnly="true" BackColor="#008284" Width="60px"  runat="server" Text='<%# Bind("perpnum") %>' /></td></tr>
                    <tr><td align="right" style="padding-right:5px;"><b>Period Start Date:</b></td>
                    <td><asp:TextBox ID="perpstTextBox" Width="100px" BackColor="#008284" onfocus="javascript:preEnter( this, 'yes' );" onblur="javascript:preLeave( this, 'date', '99/99/9999' );" runat="server" Text='<%# Bind("perpst") %>' />
                    <a href="#" tabindex="1" onblur="document.getElementById('FormView1_perpstTextBox').focus()" onClick="showCalendarControl(FormView1_perpstTextBox); return false"><asp:Image ID="Image1" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
                    </td></tr>
                    <tr><td align="right" style="padding-right:5px;"><b>Period End Date:</b></td>
                    <td><asp:TextBox ID="perpendTextBox" Width="100px" onfocus="javascript:preEnter( this, 'yes' );" onblur="javascript:preLeave( this, 'date', '99/99/9999' );" runat="server" Text='<%# Bind("perpend") %>' />
                    <a href="#" tabindex="1" onblur="document.getElementById('FormView1_perpendTextBox').focus()" onClick="showCalendarControl(FormView1_perpendTextBox); return false"><asp:Image ID="Image2" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
                    </td></tr>
                    <tr><td align="right" style="padding-right:5px;"><b>Status:</b></td>
                    <td><asp:TextBox ID="perpstatTextBox" Visible="false" runat="server" Text='<%# Bind("perpstat") %>' />
                     <asp:RadioButtonList ID="RadioButtonList1"  runat="server" Enabled="false" RepeatColumns="2" RepeatLayout="Flow" BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px">
                        <asp:ListItem Value="yes" Text="Open"> </asp:ListItem>
                        <asp:ListItem Value="no" Text="Closed"></asp:ListItem>
                        </asp:RadioButtonList></td></tr>
                    </table>
                    <asp:TextBox ID="preckeyTextBox" Visible="false" runat="server" Text='<%# Bind("preckey") %>' />
                    <br />
                    <asp:Button ID="UpdateButton" runat="server" CausesValidation="True" CssClass="button"
                        OnClick="UpdateButton_Click" Text="Save" />
                    &nbsp;<asp:Button ID="UpdateCancelButton" runat="server" CssClass="button"
                        CausesValidation="False" CommandName="Cancel" Text="Cancel" />
                        </fieldset>
                    
                     </asp:Panel>                
                            </EditItemTemplate> 
               <InsertItemTemplate>
                     <asp:Panel CssClass="shade" ID="editpanel" runat="server" >
                    <fieldset style="width:360px">
                    <table><tr><td align="right" style="padding-right:5px;"><b>Year:</b></td>
                    <td><asp:TextBox ID="peryrTextBox" BackColor="#008284" ForeColor="White" runat="server" Width="70px" Text='<%# Bind("peryr") %>' />
                        <asp:CompareValidator ID="CompareValidator1" ControlToValidate="peryrTextBox" Operator="DataTypeCheck" Type="Integer" Display="Dynamic"  SetFocusOnError="true" runat="server" ErrorMessage="Enter Integer Value"></asp:CompareValidator>
                    </td></tr>
                    <tr><td align="right" style="padding-right:5px;"><b>Period:</b></td>
                    <td><asp:TextBox ID="perpnumTextBox" Width="60px" BackColor="#008284" Enabled="false" runat="server" Text='<%# Bind("perpnum") %>' /></td></tr>
                    <tr><td align="right" style="padding-right:5px;"><b>Period Start Date:</b></td>
                    <td><asp:TextBox ID="perpstTextBox" Width="100px" BackColor="#008284" Enabled="false"  runat="server" Text='<%# Bind("perpst") %>' /></td></tr>
                    <tr><td align="right" style="padding-right:5px;"><b>Period End Date:</b></td>
                    <td><asp:TextBox ID="perpendTextBox" Width="100px" Enabled="false" runat="server" Text='<%# Bind("perpend") %>' /></td></tr>
                    <tr><td align="right" style="padding-right:5px;"><b>Status:</b></td>
                    <td><asp:TextBox ID="perpstatTextBox" Visible="false" runat="server" Text='<%# Bind("perpstat") %>' />
                     <asp:RadioButtonList ID="RadioButtonList1"  runat="server" RepeatColumns="2" RepeatLayout="Flow">
                        <asp:ListItem Value="yes" Text="Open"> </asp:ListItem>
                        <asp:ListItem Value="no" Text="Closed"></asp:ListItem>
                        </asp:RadioButtonList></td></tr>
                    </table>
                     <asp:TextBox ID="preckeyTextBox" Visible="false" runat="server" Text='<%# Bind("preckey") %>' />
                     <br />
                     <asp:Button ID="InsertButton" runat="server" CausesValidation="True"  CssClass="button" OnClientClick="confirmAdd()"
                         OnClick="addButton_Click" Text="Save" />
                     &nbsp;<asp:Button ID="InsertCancelButton" runat="server" CssClass="button"
                         CausesValidation="False" CommandName="Cancel" Text="Cancel" /></fieldset></asp:Panel>
                </InsertItemTemplate>
              
   <ItemTemplate>
        <fieldset class="shade" style="width:360px">
        <table class="shade"><tr><td align="right" style="padding-right:5px;"><b>Year:</b></td>
                    <td><asp:Label ID="peryrLabel" Width="100px" BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" runat="server" Text='<%# Bind("peryr") %>' /></td></tr>
                    <tr><td align="right" style="padding-right:5px;"><b>Period:</b></td>
                    <td><asp:Label ID="perpnumLabel" Width="50px" BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" runat="server" Text='<%# Bind("perpnum") %>' /></td></tr>
                    <tr><td align="right" style="padding-right:5px;"><b>Period Start Date:</b></td>
                    <td><asp:Label ID="perpstLabel" Width="150px" BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" runat="server" Text='<%# Bind("perpst") %>' /></td></tr>
                    <tr><td align="right" style="padding-right:5px;"><b>Period End Date:</b></td>
                    <td><asp:Label ID="perpendLabel" Width="150px" BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" runat="server" Text='<%# Bind("perpend") %>' /></td></tr>
                    <tr><td align="right" style="padding-right:5px;"><b>Status:</b></td>
                    <td><asp:Label ID="perpstatLabel" Visible="false" Width="50px"  runat="server" Text='<%# Bind("perpstat") %>' />
                        <asp:RadioButtonList ID="RadioButtonList1" Enabled="false" RepeatColumns="2" RepeatLayout="Flow" BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px"  runat="server">
                        <asp:ListItem Value="yes" Text="Open"> </asp:ListItem>
                        <asp:ListItem Value="no" Text="Closed"></asp:ListItem>
                        </asp:RadioButtonList>
                    </td></tr>
                    </table>
                    <asp:Label ID="preckeyLabel" Visible="false" runat="server" Text='<%# Bind("preckey") %>' />
                    <br />
                    <asp:Button ID="AddButton" runat="server" CssClass="buttonM" CausesValidation="False" CommandName="new"
                      Text="Add" >
                    </asp:Button>
                    <asp:Button ID="EditButton" runat="server" CssClass="buttonM" CausesValidation="False" CommandName="Edit"
                      Text="Update">
                    </asp:Button>
                   <asp:Button ID="DeleteButton" runat="server" OnClick="Delete_Click" CssClass="buttonM" CausesValidation="False" OnClientClick="return confirm('Are you sure you want to delete')"
                      Text="Delete"></asp:Button></fieldset>
      </ItemTemplate>
  </asp:FormView>
  
          <asp:ObjectDataSource ID="ObjectDataSource1" runat="server" OldValuesParameterFormatString="original_{0}"
              SelectMethod="PeriodList" TypeName="ledger">
              <SelectParameters>
                  <asp:Parameter DefaultValue="View" Name="prmAction" Type="String" />
                  <asp:Parameter Name="prmComp" Type="String" />                  
                  <asp:Parameter Name="prmUser" Type="String"  DefaultValue="" />
                  <asp:SessionParameter SessionField="company_list_company" Name="prmCompany" Type="String" />
                  <asp:SessionParameter SessionField="period_list_reckey_year" Name="prmyr" Type="String" />
                  <asp:Parameter Name="prmpnum" Type="String" />                 
                  <asp:Parameter Name="prmpst" Type="String" />
                  <asp:Parameter Name="prmpend" Type="String" />
                  <asp:Parameter Name="prmpstat" Type="String" />
                  <asp:SessionParameter SessionField="period_list_reckey_name" Name="prmReckey" 
                      Type="String" />
              </SelectParameters>
          </asp:ObjectDataSource>
          
          <asp:ObjectDataSource ID="ObjectDataSource2" runat="server" OldValuesParameterFormatString="original_{0}"
              SelectMethod="PeriodList" TypeName="ledger">
              <SelectParameters>
                  <asp:Parameter DefaultValue="CompanyDetail" Name="prmAction" Type="String" />
                  <asp:Parameter Name="prmComp" Type="String" />                  
                  <asp:Parameter Name="prmUser" Type="String"  DefaultValue="" />
                  <asp:SessionParameter SessionField="company_list_company" Name="prmCompany" Type="String" />
                  <asp:Parameter Name="prmyr" Type="String" />
                  <asp:Parameter Name="prmpnum" Type="String" />                 
                  <asp:Parameter Name="prmpst" Type="String" />
                  <asp:Parameter Name="prmpend" Type="String" />
                  <asp:Parameter Name="prmpstat" Type="String" />
                  <asp:SessionParameter SessionField="period_list_reckey_name" Name="prmReckey" 
                      Type="String" />
              </SelectParameters>
          </asp:ObjectDataSource>
          
          
          
       
    </div></td></tr></table>
    <ft:footer id="Footer1" runat="server"></ft:footer>
    </form>
  </body>
</HTML>

