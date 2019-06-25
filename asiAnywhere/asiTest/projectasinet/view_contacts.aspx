<%@ Page Language="C#" AutoEventWireup="true" Debug="true" Inherits="view_contact" Codebehind="view_contacts.aspx.cs" %>
<%@ Register Src="footer.ascx" TagName="Footer" TagPrefix="ft" %>
<%@ Register Src="header.ascx" TagName="Header" TagPrefix="hd" %>
<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">

<script runat="server">

</script>

<html xmlns="http://www.w3.org/1999/xhtml" >
<head id="Head1" runat="server">
    <title>View Contacts</title>
    <LINK href="include/style.css" type="text/css" rel="stylesheet"/>
     <link href="include/tree.css" rel="stylesheet" type="text/css" />
    <link href="include/tree.css" rel="stylesheet" type="text/css" />
    <link href="include/tree.css" rel="stylesheet" type="text/css" />
    <link href="include/tree.css" rel="stylesheet" type="text/css" />
    <link href="include/tree.css" rel="stylesheet" type="text/css" />
    <link href="include/dhtmlwindow.css" rel="stylesheet" type="text/css" />
    <link href="include/tree.css" rel="stylesheet" type="text/css" />
    <link href="include/style.css" type="text/css" rel="stylesheet"/>
    <script>
    function contactcustomerlook(){ 
  var NewWindow = window.open("contact_customer_lookup.aspx","ContactCustomerLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}

function ContactCustomerLookup(ReturnObj1,ReturnObj2,ReturnObj3,ReturnObj4,ReturnObj5,ReturnObj6,ReturnObj7,ReturnObj8,ReturnObj9, ReturnObj10,ReturnObj11){ 
  document.forms[0].FormView1_cust_noTextBox.value = ReturnObj1;
  document.forms[0].FormView1_cust_nameTextBox.value = ReturnObj2;
  document.forms[0].FormView1_addr1TextBox.value = ReturnObj3;
  document.forms[0].FormView1_addr2TextBox.value = ReturnObj4;
  document.forms[0].FormView1_cityTextBox.value = ReturnObj5;
  document.forms[0].FormView1_stateTextBox.value = ReturnObj6;
  document.forms[0].FormView1_zipTextBox.value = ReturnObj7;
  document.forms[0].FormView1_TextBox2.value = ReturnObj8;
  document.forms[0].FormView1_countryTextBox.value = ReturnObj9;
  document.forms[0].FormView1_countyTextBox.value = ReturnObj10;
  document.forms[0].FormView1_territoryTextBox.value = ReturnObj11;
  document.forms[0].FormView1_cust_noTextBox.focus();
}
 function zipcodelook(){ 
  var NewWindow = window.open("zipcode_lookup.aspx","ZipCodeLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}

function ZipCodeLookup(ReturnObj1, ReturnObj2, ReturnObj3){ 
  document.forms[0].FormView1_zipTextBox.value = ReturnObj1;
   document.forms[0].FormView1_cityTextBox.value = ReturnObj2;
    document.forms[0].FormView1_stateTextBox.value = ReturnObj3;
    document.forms[0].FormView1_zipTextBox.focus();
}
function citylook(){ 
  var NewWindow = window.open("city_lookup.aspx","CityCodeLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}

function CityCodeLookup(ReturnObj1){ 
  
   document.forms[0].FormView1_cityTextBox.value = ReturnObj1;
   document.forms[0].FormView1_cityTextBox.focus();
    
}
function statecodelook(){ 
  var NewWindow = window.open("statecode_lookup.aspx","StateCodeLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}

function StateCodeLookup(ReturnObj1){ 
  document.forms[0].FormView1_stateTextBox.value = ReturnObj1;
  document.forms[0].FormView1_stateTextBox.focus();
    
}
function terrlook(){ 
  var NewWindow = window.open("terr_lookup.aspx","TerrCodeLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}

function TerrCodeLookup(ReturnObj1){ 
  document.forms[0].FormView1_territoryTextBox.value = ReturnObj1;
  document.forms[0].FormView1_territoryTextBox.focus();
}
    function supplierlook(){ 
  var NewWindow = window.open("supplier_lookup.aspx","SupplierLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}
function SupplierLookup(ReturnObj1,ReturnObj2){ 
  document.forms[0].FormView1$comp_codeTextBox.value = ReturnObj1;
  document.forms[0].FormView1$comp_descTextBox.value = ReturnObj2;
  document.forms[0].FormView1$comp_codeTextBox.focus();
}
      function siclook(){ 
  var NewWindow = window.open("sic_lookup.aspx","sicLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}
function sicLookup(ReturnObj1,ReturnObj2){ 
  document.forms[0].FormView1$sic_codeTextBox.value = ReturnObj1;
  document.forms[0].FormView1$sic_descTextBox.value = ReturnObj2;
  document.forms[0].FormView1$sic_codeTextBox.focus();
}

function statuslook(){ 
  var NewWindow = window.open("status_lookup.aspx","statusLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}
function statusLookup(ReturnObj1,ReturnObj2){ 
  document.forms[0].FormView1$status_codeTextBox.value = ReturnObj1;
  document.forms[0].FormView1$status_descTextBox.value = ReturnObj2;
  document.forms[0].FormView1$status_codeTextBox.focus();
}
function titlelook(){ 
  var NewWindow = window.open("title_lookup.aspx","TitleLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}
function TitleLookup(ReturnObj1){ 
  document.forms[0].FormView1$contact_titleTextBox.value = ReturnObj1;
  document.forms[0].FormView1$contact_titleTextBox.focus();
}
 function typelook(){ 
  var NewWindow = window.open("type_lookup.aspx","typeLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}
function TypeLookup(ReturnObj1){
    document.forms[0].FormView1$typeTextBox.value = ReturnObj1;
    document.forms[0].FormView1$typeTextBox.focus();
 }

 function salesreplook(){ 
  var NewWindow = window.open("salesrep_lookup.aspx","SalesRepLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}

function SalesRepLookup(ReturnObj1){ 
  document.forms[0].FormView1_TextBox2.value = ReturnObj1;
  document.forms[0].FormView1_TextBox2.focus();
  
}


 function contactcustomercopylook(){ 
  var NewWindow = window.open("contact_customer_copylookup.aspx","ContactCustomerLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}

function ContactCustomerCopyLookup(ReturnObj1,ReturnObj2,ReturnObj3,ReturnObj4,ReturnObj5,ReturnObj6,ReturnObj7,ReturnObj8,ReturnObj9, ReturnObj10,ReturnObj11){ 
  document.forms[0].FormView2_cust_noTextBox.value = ReturnObj1;
  document.forms[0].FormView2_cust_nameTextBox.value = ReturnObj2;
  document.forms[0].FormView2_addr1TextBox.value = ReturnObj3;
  document.forms[0].FormView2_addr2TextBox.value = ReturnObj4;
  document.forms[0].FormView2_cityTextBox.value = ReturnObj5;
  document.forms[0].FormView2_stateTextBox.value = ReturnObj6;
  document.forms[0].FormView2_zipTextBox.value = ReturnObj7;
//  document.forms[0].FormView2_TextBox2.value = ReturnObj8;
  document.forms[0].FormView2_countryTextBox.value = ReturnObj9;
  document.forms[0].FormView2_countyTextBox.value = ReturnObj10;
  document.forms[0].FormView2_territoryTextBox.value = ReturnObj11;
  document.forms[0].FormView2_cust_noTextBox.focus();
}
function statecodecopylook(){ 
  var NewWindow = window.open("statecode_copylookup.aspx","StateCodeLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}

function StateCodeCopyLookup(ReturnObj1){ 
  document.forms[0].FormView2_stateTextBox.value = ReturnObj1;
  document.forms[0].FormView2_stateTextBox.focus();  
}
function terrcopylook(){ 
  var NewWindow = window.open("terr_copylookup.aspx","TerrCodeLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}

function TerrCodeCopyLookup(ReturnObj1){ 
  document.forms[0].FormView2$territoryTextBox.value = ReturnObj1;
  document.forms[0].FormView2$territoryTextBox.focus();
}
function zipcodecopylook(){ 
  var NewWindow = window.open("zipcode_copylookup.aspx","ZipCodeLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}

function ZipCodeCopyLookup(ReturnObj1, ReturnObj2, ReturnObj3){ 
  document.forms[0].FormView2$zipTextBox.value = ReturnObj1;
   document.forms[0].FormView2$cityTextBox.value = ReturnObj2;
    document.forms[0].FormView2$stateTextBox.value = ReturnObj3;
    document.forms[0].FormView2$zipTextBox.focus();
}
 function suppliercopylook(){ 
  var NewWindow = window.open("supplier_copylookup.aspx","SupplierLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}
function SupplierCopyLookup(ReturnObj1,ReturnObj2){ 
  document.forms[0].FormView2$comp_codeTextBox.value = ReturnObj1;
  document.forms[0].FormView2$comp_descTextBox.value = ReturnObj2;
  document.forms[0].FormView2$comp_codeTextBox.focus();
}
      function siccopylook(){ 
  var NewWindow = window.open("sic_copylookup.aspx","sicLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}
function sicCopyLookup(ReturnObj1,ReturnObj2){ 
  document.forms[0].FormView2$sic_codeTextBox.value = ReturnObj1;
  document.forms[0].FormView2$sic_descTextBox.value = ReturnObj2;
  document.forms[0].FormView2$sic_codeTextBox.focus();
}

function statuscopylook(){ 
  var NewWindow = window.open("status_copylookup.aspx","statusLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}
function statusCopyLookup(ReturnObj1,ReturnObj2){ 
  document.forms[0].FormView2$status_codeTextBox.value = ReturnObj1;
  document.forms[0].FormView2$status_descTextBox.value = ReturnObj2;
  document.forms[0].FormView2$status_codeTextBox.focus();
}
function titlecopylook(){ 
  var NewWindow = window.open("title_copylookup.aspx","TitlecopyLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}
function TitlecopyLookup(ReturnObj1){ 
  document.forms[0].FormView2$contact_titleTextBox.value = ReturnObj1;
  document.forms[0].FormView2$contact_titleTextBox.focus();
}
 function typecopylook(){ 
  var NewWindow = window.open("type_copylookup.aspx","typecopyLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}
function TypecopyLookup(ReturnObj1){
    document.forms[0].FormView2$typeTextBox.value = ReturnObj1;
    document.forms[0].FormView2$typeTextBox.focus();
 }
 function citycopylook(){ 
  var NewWindow = window.open("city_copylookup.aspx","CityCodecopyLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}

function CityCodecopyLookup(ReturnObj1){ 
  
   document.forms[0].FormView2_cityTextBox.value = ReturnObj1;
   document.forms[0].FormView2_cityTextBox.focus();
    
}
function smancopyLook(){ 
  var NewWindow = window.open("sman_copylookup.aspx","smancopyLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}

function smancopyLookup(ReturnObj1){ 
  document.forms[0].FormView2_smanTextBox.value = ReturnObj1;
  document.forms[0].FormView2_smanTextBox.focus();
  
}

function opencust()
{
    document.forms[0].FormView1$cust_noTextBox.value="";
    contactcustomerlook();
}
function opentype()
{
document.forms[0].FormView1$typeTextBox.value="";
typelook();
}
function opensales()
{
document.forms[0].FormView1$TextBox2.value="";
salesreplook();
}

function opencity()
{
document.forms[0].FormView1$cityTextBox.value="";
citylook();
}


function openstate()
{
document.forms[0].FormView1$stateTextBox.value="";
statecodelook();
}
function openzip()
{
document.forms[0].FormView1$zipTextBox.value="";
zipcodelook();
}
function openterr()
{
document.forms[0].FormView1$territoryTextBox.value="";
terrlook();
}
function opensupcode()
{
document.forms[0].FormView1$comp_codeTextBox.value="";
supplierlook();
}
function opensiccode()
{
document.forms[0].FormView1$sic_codeTextBox.value="";
siclook();
}
function openstatcode()
{
document.forms[0].FormView1$status_codeTextBox.value="";
statuslook();
}
function opentitle()
{
document.forms[0].FormView1$contact_titleTextBox.value="";
titlelook();
}





function opentitle2()
{
document.forms[0].FormView2$contact_titleTextBox.value="";
titlelook();
}

function opencust2()
{
    document.forms[0].FormView2$cust_noTextBox.value="";
    contactcustomercopylook();
}
function opentype2()
{
document.forms[0].FormView2$typeTextBox.value="";
typecopylook();
}
function opensales2()
{
document.forms[0].FormView2$TextBox2.value="";
salesreplook();
}

function opencity2()
{
document.forms[0].FormView2$cityTextBox.value="";
citycopylook();
}


function openstate2()
{
document.forms[0].FormView2$stateTextBox.value="";
statecodecopylook();
}
function openzip2()
{
document.forms[0].FormView2$zipTextBox.value="";
zipcodecopylook();
}
function openterr2()
{
document.forms[0].FormView2$territoryTextBox.value="";
terrcopylook();
}
function opensupcode2()
{
document.forms[0].FormView2$comp_codeTextBox.value="";
suppliercopylook();
}
function opensiccode2()
{
document.forms[0].FormView2$sic_codeTextBox.value="";
siccopylook();
}
function openstatcode2()
{
document.forms[0].FormView2$status_codeTextBox.value="";
statuscopylook();
}

  </script>
</head>
<body>
    <form id="form1" runat="server">
     <hd:header id="Header1" runat="server"></hd:header>
    <div>
    
    
    <table id="tblTop" cellspacing="3" align="center" border="0" width="100%">
        <tr>
          
          <td align="center"><font size="+0"><b>&nbsp;View Contacts&nbsp;</b></font></td>
          <td><asp:linkbutton id="LinkButton1" runat="server" OnClick="LinkButton1_Click">Back to menu</asp:linkbutton></td>

          <td valign="left" align="left"><b>Users</b>&nbsp;&nbsp;Logged as&nbsp;
            <asp:label id="lblUser" runat="server" Font-Bold="True">&nbsp;</asp:label>&nbsp;&nbsp;&nbsp;
            <asp:linkbutton id="hlnkLogOut" runat="server" OnClick="hlnkLogOut_Click">Log out</asp:linkbutton>
            &nbsp;&nbsp;&nbsp;<asp:hyperlink id="hlnkChangePwd" runat="server" NavigateUrl="changepwd.aspx"></asp:hyperlink>
            &nbsp;<b>Company:</b> &nbsp;<asp:label id="lblComp" runat="server" Font-Bold="True">&nbsp;</asp:label>
          </td>
          
          <td valign="middle" width="20">&nbsp;</td>
          
          <td width="30">&nbsp;</td>
        </tr>
      </table>
      
      <table>
        <tr bgcolor="gray">
        <td><div  id="navigation" style="width:100%">
		<ul nowrap> <li>
            <asp:LinkButton ID="lnk_listcontact" runat="server" OnClick="lnk_listcontact_click">List Contacts</asp:LinkButton></li>
            <li class="selected"><asp:LinkButton ID="lnk_viewcontact" runat="server">View Contacts</asp:LinkButton></li>
            <li><asp:LinkButton ID="lnk_notes" runat="server" OnClick="lnk_notes_click">Notes</asp:LinkButton></li>
            <li><asp:LinkButton ID="lnk_MailList" runat="server" OnClick="lnk_MailList_click">Mail Label</asp:LinkButton></li>
            <li><asp:LinkButton ID="lnk_calendar" runat="server" OnClick="lnk_calendar_click">Calendar</asp:LinkButton></li></ul></div>
        </td>
    </tr>
    </table>
        <asp:HiddenField ID="HiddenField1" runat="server" />
    
    <asp:SqlDataSource id="SqlDataSource1" runat="server"
      SelectCommand="select [comp_code],[sic_code],[status_code], [company],   [cust_no],   [ship_id],   [sman],   [first_name],   [last_name],   [middle_initial],   [sirname],   [contact_title],   [maillist],   [type],   [contact_loc],   [cust_name],   [addr1],   [addr2],   [city],   [state],   [zip],   [country],   [county],   [territory],   [access_code],   [phone],   [cell_phone],   [fax],   [extension],   [email],   [website],   [rec_key],[comp_des],[status_des],[sic_des],   [list_no],   [list_name],[reckey],[rec_key_value] from   [dbo].[contact],[dbo].[contact_reckey] where [rec_key]=@rec_key  ORDER BY [reckey] DESC, [last_name] DESC "

        
         
          DeleteCommand="delete from [dbo].[contact] where [rec_key]=@rec_key "

    OnInserted="contactSqlDataSource_Inserted" OnUpdated="contactSqlDataSource_Updated"
    OnInserting="contactSqlDataSource_Inserting"
        ConnectionString="<%$ ConnectionStrings:Project1ConnectionString %>"
        ProviderName="<%$ ConnectionStrings:Project1ConnectionString.providerName %>" OnDeleted="SqlDataSource1_Deleted"
    >
        <SelectParameters>
       
        <asp:SessionParameter Name="rec_key" SessionField="contact_rec_key"
                        Type="String" DefaultValue="" />                        
        </SelectParameters>
       
        
    </asp:SqlDataSource>
    
     <asp:label id="lblMessage" runat="server" ForeColor="Red"></asp:label>
     <asp:Button ID="AddNewButton" Text="Add" runat="server" CssClass="buttonM" OnClick="AddNewButton_Click" />
     
          <asp:FormView ID="FormView1" runat="server" OnDataBound="FormView1_DataBound" DataSourceID="SqlDataSource1" Width="850px" >
              <EditItemTemplate>
              <fieldset style="background-color:#EFF3FB; width:950px;">
              <table class="shade">
              <tr>
              <td align="right" style="padding-right:5px;"><b>Sirname:</b></td>
              <td><b>
                  <asp:DropDownList ID="DropDownList1" Width="45px" runat="server" SelectedValue='<%# Bind("sirname") %>'>
                   <asp:ListItem Value=""></asp:ListItem>
                   <asp:ListItem Value="Mr.">Mr.</asp:ListItem>
                   <asp:ListItem Value="Mrs.">Mrs.</asp:ListItem>
                   <asp:ListItem Value="Ms.">Ms.</asp:ListItem>
                   <asp:ListItem Value="Dr.">Dr.</asp:ListItem>                   
               </asp:DropDownList>
                  </b></td>
                  <td align="right" style="padding-right:5px;"><b>Contact:</b>&nbsp;</td>
                 <td style="width: 140px"><asp:TextBox ID="first_nameTextBox" MaxLength="30"  runat="server" Text='<%# Bind("first_name") %>'>
                  </asp:TextBox>
                     <asp:RequiredFieldValidator ID="RequiredFieldValidator2" ControlToValidate="first_nameTextBox" SetFocusOnError="true" Display="dynamic" runat="server" ErrorMessage="Required Field"></asp:RequiredFieldValidator>
                  </td>
                  <td align="center" style="padding-right:5px;"><asp:TextBox ID="middle_initialTextBox" Visible="false" runat="server" Text='<%# Bind("middle_initial") %>' MaxLength="1" Width="20px"></asp:TextBox></td>
                 
                  <td><asp:TextBox ID="last_nameTextBox" Width="150px" MaxLength="30" runat="server" Text='<%# Bind("last_name") %>'>
                  </asp:TextBox>
                  <asp:RequiredFieldValidator ID="RequiredFieldValidator3" ControlToValidate="last_nameTextBox" SetFocusOnError="true" Display="dynamic" runat="server" ErrorMessage="Required Field"></asp:RequiredFieldValidator>
                  </td>
                  <td align="right" style="padding-right:5px;"><b>Title:</b>
                  </td>
                  <td nowrap>
                      <asp:TextBox ID="contact_titleTextBox"  runat="server" Text='<%# Bind("contact_title") %>' MaxLength="50" Width="120px"></asp:TextBox>
                        <a href="#" tabindex="1" onClick="titlelook(); return false"><asp:Image ID="Image7" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
                  </td>
                  
                  <td align="right" style="padding-right:5px;" nowrap> <b>Customer Code:</b>
                
                  </td>
                  <td style="width: 140px" nowrap>
                  <asp:TextBox ID="cust_noTextBox"  MaxLength="8" Width="70px" runat="server" Text='<%# Bind("cust_no") %>'>
                  </asp:TextBox>
                   <a href="#" tabindex="1" onClick="contactcustomerlook(); return false"><asp:Image ID="CustomerLook" runat="server" ImageUrl="images/lookup_icon.gif" /></a></td>
                  <asp:RequiredFieldValidator ID="RequiredFieldValidator1" ControlToValidate="cust_noTextBox" Display="dynamic" SetFocusOnError="true" runat="server" ErrorMessage="This field cannot be blank"></asp:RequiredFieldValidator></tr>
              </table>
             <table>
             <tr>
             <td align="right" style="padding-right:5px;"><b>Acess Code:</b>
                  </td>
                  <td style="width: 140px"><asp:TextBox ID="access_codeTextBox" MaxLength="20"  runat="server" Text='<%# Bind("access_code") %>'>
                  </asp:TextBox>
                  </td>
              <td align="right" style="padding-right:5px;"><b>Phone:</b>
                  </td>
                  <td style="width: 140px"><asp:TextBox ID="phoneTextBox" MaxLength="12" Width="80px" runat="server" Text='<%# Bind("phone") %>'>
                  </asp:TextBox>
                  <asp:RegularExpressionValidator ID="RegularExpressionValidator2" runat="server" SetFocusOnError="true" ControlToValidate="phoneTextBox" ErrorMessage="Invalid Phone No." Display="Dynamic" ValidationExpression="((\(\d{3}\) ?)|(\d{3}-))?\d{3}-\d{4}"></asp:RegularExpressionValidator>
                  </td>
                  <td align="right" style="padding-right:5px;"><b>Extension:</b>
                  </td>
                  <td style="width: 100px"><asp:TextBox ID="extensionTextBox" MaxLength="5" Width="40px" runat="server" Text='<%# Bind("extension") %>'>
                  </asp:TextBox>
                  </td>
                   <td align="right" style="padding-right:5px;"><b>Type:</b>
                  </td>
                  <td style="width: 100px"> <asp:TextBox ID="typeTextBox"  MaxLength="8" Width="70px" runat="server" Text='<%# Bind("type") %>'>
                  </asp:TextBox><a href="#" tabindex="1" onClick="typelook(); return false"><asp:Image ID="Image8" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
                  </td>
             </tr>
             <tr>
             <td align="right" style="padding-right:5px;"><b>Cell Phone:</b>
                  </td>
                  <td style="width: 140px"><asp:TextBox ID="cell_phoneTextBox" MaxLength="12" Width="80px" runat="server" Text='<%# Bind("cell_phone") %>'>
                  </asp:TextBox>
                  <asp:RegularExpressionValidator ID="RegularExpressionValidator3" runat="server" SetFocusOnError="true" ControlToValidate="cell_phoneTextBox" ErrorMessage="Invalid Cell No" Display="Dynamic" ValidationExpression="((\(\d{3}\) ?)|(\d{3}-))?\d{3}-\d{4}"></asp:RegularExpressionValidator>
                  </td>
                  <td align="right" style="padding-right:5px;"><b>Fax</b>
                  </td>
                  <td style="width: 100px"><asp:TextBox ID="faxTextBox" MaxLength="12" Width="80px" runat="server" Text='<%# Bind("fax") %>'>
                  </asp:TextBox>
                  <asp:RegularExpressionValidator ID="RegularExpressionValidator4" runat="server" SetFocusOnError="true" ControlToValidate="faxTextBox" ErrorMessage="Invalid Fax" Display="Dynamic" ValidationExpression="((\(\d{3}\) ?)|(\d{3}-))?\d{3}-\d{4}"></asp:RegularExpressionValidator>
                  </td>
                  <td></td>
                  <td></td>
                  <td align="right" style="padding-right:5px;"><b>Sales Rep:</b>
                  </td>
                  <td style="width: 100px"><asp:TextBox ID="TextBox2"  Width="60px"  runat="server" Text='<%# Bind("sman") %>'>
                  </asp:TextBox><a href="#" tabindex="1" onClick="salesreplook(); return false"><asp:Image ID="SalesRep" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
                  </td>
             </tr>
             <tr>
             <td align="right" style="padding-right:5px;"><b>Location:</b>
                  </td>
                  <td style="width: 100px">
                  
                  <asp:DropDownList ID="SITextBox" Width="45px" runat="server" SelectedValue='<%# Bind("contact_loc") %>'>
                   <asp:ListItem Value=""></asp:ListItem>
                   <asp:ListItem Value="C">C</asp:ListItem>
                   <asp:ListItem Value="S">S</asp:ListItem>
                   
               </asp:DropDownList>
                  </td>
                 <td align="right" style="padding-right:5px;"><b>ShipId:</b>
                  </td>
                  <td style="width: 140px"><asp:TextBox ID="ship_idTextBox" MaxLength="8" Width="70px" runat="server" Text='<%# Bind("ship_id") %>'>
                  </asp:TextBox>
                  </td>
                  <td></td>
                  <td></td>
                  <td align="right" style="padding-right:px;"><b>Mail List:</b>
                  </td>
                  <td style="width: 100px">
                      <asp:CheckBox ID="maillistTextBox" runat="server"  Checked='<%# Bind("maillist") %>' />
                      
                  
                  </td> 
             </tr>
             </table>
             <table>
              <tr>
               <td align="right" style="padding-right:5px;"><b>Company:</b>
                  </td>
                  <td style="width: 140px"><asp:TextBox ID="cust_nameTextBox"  runat="server" Text='<%# Bind("cust_name") %>'>
                  </asp:TextBox>
                  </td>
                  <td align="right" style="padding-right:5px;"><b>Website:</b>
                  </td>
                  <td style="width: 270px"><asp:TextBox ID="websiteTextBox" MaxLength="50" Width="100px" runat="server" Text='<%# Bind("website") %>'>
                  </asp:TextBox>
                  </td>
                   <td align="right" style="padding-right:5px;"><b>Email:</b>
                  </td>
                  <td style="width: 270px"><asp:TextBox ID="emailTextBox" MaxLength="50" Width="140px" runat="server" Text='<%# Bind("email") %>'>
                  </asp:TextBox>
                   <asp:RegularExpressionValidator ID="RegularExpressionValidator1" runat="server" SetFocusOnError="true" ControlToValidate="emailTextBox" ErrorMessage="Invalid Email" Display="Dynamic" ValidationExpression="\w+([-+.']\w+)*@\w+([-.]\w+)*\.\w+([-.]\w+)*"></asp:RegularExpressionValidator>
                  </td>
                  
                  
              </tr>
              <tr>
                  <td align="right" style="padding-right:5px;"><b>Address:</b>
                  </td>
                  <td style="width: 140px"><asp:TextBox ID="addr1TextBox"  runat="server" Text='<%# Bind("addr1") %>'>
                  </asp:TextBox>
                  </td>
              </tr>
              <tr>
                  <td align="right" style="padding-right:5px;"><b>Address:</b>
                  </td>
                  <td style="width: 140px"><asp:TextBox ID="addr2TextBox"  runat="server" Text='<%# Bind("addr2") %>'>
                  </asp:TextBox>
                  </td>
              </tr>
              <tr>
                  <td align="right" style="padding-right:5px;"><b>City:</b>
                  </td>
                  
                  <td nowrap><asp:TextBox ID="cityTextBox"   runat="server" Text='<%# Bind("city") %>'>
                  </asp:TextBox> <a href="#" tabindex="1" onClick="citylook(); return false"><asp:Image ID="Image9" runat="server" ImageUrl="images/lookup_icon.gif" /></a></td>
                  <td align="right" style="padding-right:5px;"><b>State:</b></td>
                  <td ><asp:TextBox ID="stateTextBox"  runat="server" Width="70px" Text='<%# Bind("state") %>' ></asp:TextBox>
                  <a href="#" tabindex="1" onClick="statecodelook(); return false"><asp:Image ID="Image4" runat="server" ImageUrl="images/lookup_icon.gif" /></a></td>
                  <td align="right" style="padding-right:5px;"><b>Zip:</b></td>
                  <td > <asp:TextBox ID="zipTextBox"  Width="70px" runat="server" Text='<%# Bind("zip") %>' ></asp:TextBox>
                   <a href="#" tabindex="1" onClick="zipcodelook(); return false"><asp:Image ID="Image1" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
                  </td>
              </tr>
              
              <tr>
              <td align="right" style="padding-right:5px;"><b>Country:</b>
                  </td>
                  <td style="width: 140px"><asp:TextBox ID="countryTextBox" runat="server" Text='<%# Bind("country") %>'>
                  </asp:TextBox>
                  </td>
                  <td align="right" style="padding-right:5px;"><b>County:</b>
                  </td>
                  <td style="width: 140px"><asp:TextBox ID="countyTextBox" runat="server" Text='<%# Bind("county") %>'>
                  </asp:TextBox>
                  </td>
                  <td align="right" style="padding-right:5px;"><b>Territory:</b>
                  </td>
                  <td nowrap ><asp:TextBox ID="territoryTextBox"  Width="70px"  runat="server" Text='<%# Bind("territory") %>'>
                  </asp:TextBox>
                  <a href="#" tabindex="1" onClick="terrlook(); return false"><asp:Image ID="Image2" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
                  </td>
              </tr>
              
               
              <tr>
              <td align="right" style="padding-right:5px;"><b>Supplier Code:</b></td>
              <td><b><asp:TextBox ID="comp_codeTextBox" OnTextChanged="supp_textchanged" MaxLength="8" Width="70px" runat="server" Text='<%# Bind("comp_code") %>'>
                  </asp:TextBox></b>
                  <a href="#" tabindex="1" onClick="supplierlook(); return false"><asp:Image ID="SupplierLookup" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
                  <asp:CustomValidator ID="CustomValidator1" runat="server" ErrorMessage="Invalid Supp." ControlToValidate="comp_codeTextBox" Display="dynamic"
                   SetFocusOnError="true" OnServerValidate="Comp_Supp_Validate"></asp:CustomValidator>
                  </td>
                  <td align="right" style="padding-right:5px;"><b>Supplier Desc:</b></td>
                 <td><b>
                  <asp:TextBox ID="comp_descTextBox" runat="server" Text='<%# Bind("comp_des") %>'>
                  </asp:TextBox></b></td>
                  </tr>
                  <tr>
               <td align="right" style="padding-right:5px;"><b>Sic Code:</b></td>
                  <td><b>
                  <asp:TextBox ID="sic_codeTextBox"  MaxLength="5" OnTextChanged="sic_textchanged" Width="40px" runat="server" Text='<%# Bind("sic_code") %>'>
                  </asp:TextBox></b><a href="#" tabindex="1" onClick="siclook(); return false"><asp:Image ID="Siclookup" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
                  <asp:CustomValidator ID="CustomValidator2" runat="server" ErrorMessage="Invalid Sic" ControlToValidate="sic_codeTextBox" Display="dynamic"
                   SetFocusOnError="true" OnServerValidate="Comp_Sic_Validate"></asp:CustomValidator>
                  </td>
                  <td align="right" style="padding-right:5px;"><b>Sic Desc:</b></td>
                  <td><b><asp:TextBox ID="sic_descTextBox"   runat="server" Text='<%# Bind("sic_des") %>'>
                  </asp:TextBox></b></td>
                  </tr>
                  <tr>
                  <td align="right" style="padding-right:5px;"><b>Status Code:</b></td>
                  <td><b><asp:TextBox ID="status_codeTextBox" OnTextChanged="status_textchanged"  MaxLength="5" Width="40px" runat="server" Text='<%# Bind("status_code") %>'>
                  </asp:TextBox></b><a href="#" tabindex="1" onClick="statuslook(); return false"><asp:Image ID="statuslookup" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
                  <asp:CustomValidator ID="CustomValidator3" runat="server" ErrorMessage="Invalid Status" ControlToValidate="status_codeTextBox" Display="dynamic"
                   SetFocusOnError="true" OnServerValidate="Comp_Status_Validate"></asp:CustomValidator>
                  </td>
               <td align="right" style="padding-right:5px;"><b>Status Desc:</b></td>
                  <td><b><asp:TextBox ID="status_descTextBox"   runat="server" Text='<%# Bind("status_des") %>'>
                  </asp:TextBox></b></td>
              </tr>
              
                         
              
              <tr>
                 <td> <asp:TextBox ID="rec_keyTextBox" visible="false" runat="server" Text='<%# Bind("reckey") %>'>
                  </asp:TextBox></td>
                 <td> </td>
                 <td> <asp:TextBox ID="TextBox1" visible="false" runat="server" Text='<%# Bind("rec_key_value") %>'>
                  </asp:TextBox>
              </td></tr>
              
              <tr></tr>
              <tr></tr>
              <tr><td align="center">
             <asp:Button ID="UpdateButton" runat="server" CausesValidation="True" CssClass="buttonM"
                      Text="Save" OnClick="UpdateButton_Click" >
                  </asp:Button>
                  </td>
                  <td>
                  <asp:Button ID="UpdateCancelButton" runat="server" CausesValidation="False" CommandName="Cancel" CssClass="buttonM"
                      Text="Cancel">
                  </asp:Button>
                  </td></tr>
          </table>
          </fieldset>
               
              </EditItemTemplate>
              <InsertItemTemplate>
              <fieldset style="background-color:#EFF3FB; width:1050px;">
               <table class="shade">
               <tr>
               <td align="right" style="padding-right:5px;"><b>Sirname:</b></td>
              <td><b>
                  <asp:DropDownList ID="DropDownList2" Width="45px" runat="server" SelectedValue='<%# Bind("sirname") %>'>
                   <asp:ListItem Value=""></asp:ListItem>
                   <asp:ListItem Value="Mr.">Mr.</asp:ListItem>
                   <asp:ListItem Value="Mrs.">Mrs.</asp:ListItem>
                   <asp:ListItem Value="Ms.">Ms.</asp:ListItem>
                   <asp:ListItem Value="Dr.">Dr.</asp:ListItem>                   
               </asp:DropDownList>
                  </b></td>
                   <td align="right" style="padding-right:5px;"><b>Contact</b>&nbsp;</td>
                 <td style="width: 140px"><asp:TextBox ID="first_nameTextBox" MaxLength="30" Width="150px" runat="server" Text='<%# Bind("first_name") %>'>
                  </asp:TextBox>
                  <asp:RequiredFieldValidator ID="RequiredFieldValidator2" ControlToValidate="first_nameTextBox" SetFocusOnError="true" Display="dynamic" runat="server" ErrorMessage="Required Field"></asp:RequiredFieldValidator>
                  </td>
                   <td align="center"><asp:TextBox ID="middle_initialTextBox" Visible="false" runat="server" Text='<%# Bind("middle_initial") %>' MaxLength="1" Width="50px"></asp:TextBox></td>
                 
                  <td><asp:TextBox ID="last_nameTextBox" MaxLength="30" Width="150px" runat="server" Text='<%# Bind("last_name") %>'>
                  </asp:TextBox>
                  <asp:RequiredFieldValidator ID="RequiredFieldValidator4" ControlToValidate="last_nameTextBox" SetFocusOnError="true" Display="dynamic" runat="server" ErrorMessage="Required Field"></asp:RequiredFieldValidator>
                  </td>
                  <td align="right" style="padding-right:5px;" nowrap><b>Title</b>
                  </td>
                  <td nowrap>
                      <asp:TextBox ID="contact_titleTextBox" runat="server"  Text='<%# Bind("contact_title") %>' MaxLength="50" Width="120px"></asp:TextBox>
                        <a href="#" tabindex="1" onClick="titlelook(); return false"><asp:Image ID="Image7" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
                  </td>
                   <td align="right" style="padding-right:5px;" nowrap> <b>Customer Code</b>
                  </td>
                  <td nowrap style="width: 140px"><asp:TextBox ID="cust_noTextBox"  MaxLength="8" Width="70px" runat="server" Text='<%# Bind("cust_no") %>'>
                  </asp:TextBox>
                  <asp:RequiredFieldValidator ID="RequiredFieldValidator1" ControlToValidate="cust_noTextBox" Display="dynamic" SetFocusOnError="true" runat="server" ErrorMessage="This field cannot be blank"></asp:RequiredFieldValidator>
                  <a href="#" tabindex="1" onClick="contactcustomerlook(); return false"><asp:Image ID="CustomerLook" runat="server" ImageUrl="images/lookup_icon.gif" /></a></b>
                  </td>
               </tr>
               </table>
               
               <table>
              <tr>
              <td align="right" style="padding-right:5px;"><b>Access Code</b>
                  </td>
                  <td style="width: 140px"><asp:TextBox ID="access_codeTextBox" MaxLength="20" Width="140px" runat="server" Text='<%# Bind("access_code") %>'>
                  </asp:TextBox>
                  </td>
                  <td align="right" style="padding-right:5px;"><b>Phone</b>
                  </td>
                  <td style="width: 140px"><asp:TextBox ID="phoneTextBox" MaxLength="12" Width="70px" runat="server" Text='<%# Bind("phone") %>'>
                  </asp:TextBox>
                  <asp:RegularExpressionValidator ID="RegularExpressionValidator4" runat="server" SetFocusOnError="true" ControlToValidate="phoneTextBox" ErrorMessage="Invalid Pnone No" Display="Dynamic" ValidationExpression="((\(\d{3}\) ?)|(\d{3}-))?\d{3}-\d{4}"></asp:RegularExpressionValidator>
                  </td>
                  <td align="right" style="padding-right:5px;"><b>Extension</b>
                  </td>
                  <td style="width: 100px"><asp:TextBox ID="extensionTextBox" MaxLength="5" Width="40px" runat="server" Text='<%# Bind("extension") %>'>
                  </asp:TextBox>
                  </td>
                  <td align="right" style="padding-right:5px;"><b>Type</b>
                  </td>
                  <td style="width: 100px"> <asp:TextBox ID="typeTextBox"  MaxLength="8" Width="70px" runat="server" Text='<%# Bind("type") %>'>
                  </asp:TextBox><a href="#" tabindex="1" onClick="typelook(); return false"><asp:Image ID="Image8" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
                  </td>
              </tr>
              <tr>
              <td align="right" style="padding-right:5px;"><b>Cell Phone</b>
                  </td>
                  <td style="width: 140px"><asp:TextBox ID="cell_phoneTextBox" MaxLength="12" Width="80px" runat="server" Text='<%# Bind("cell_phone") %>'>
                  </asp:TextBox>
                  <asp:RegularExpressionValidator ID="RegularExpressionValidator5" runat="server" SetFocusOnError="true" ControlToValidate="cell_phoneTextBox" ErrorMessage="Invalid Cell No" Display="Dynamic" ValidationExpression="((\(\d{3}\) ?)|(\d{3}-))?\d{3}-\d{4}"></asp:RegularExpressionValidator>
                  </td>
                   <td align="right" style="padding-right:5px;"><b>Fax</b>
                  </td>
                  <td style="width: 100px"><asp:TextBox ID="faxTextBox" MaxLength="12" Width="80px" runat="server" Text='<%# Bind("fax") %>'>
                  </asp:TextBox>
                  <asp:RegularExpressionValidator ID="RegularExpressionValidator6" runat="server" SetFocusOnError="true" ControlToValidate="faxTextBox" ErrorMessage="Invalid Fax" Display="Dynamic" ValidationExpression="((\(\d{3}\) ?)|(\d{3}-))?\d{3}-\d{4}"></asp:RegularExpressionValidator>
                  </td>
                  <td></td>
                  <td></td>
                  <td align="right" style="padding-right:5px;"><b>Sales Rep</b>
                  </td>
                  <td  nowrap><asp:TextBox ID="TextBox2" runat="server"   Width="70px" Text='<%# Bind("sman") %>'>
                  </asp:TextBox><a href="#" tabindex="1" onClick="salesreplook(); return false"><asp:Image ID="SalesRep" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
                  </td> 
              </tr>
              <tr>
               <td align="right" style="padding-right:px;"><b>Location</b>
                  </td>
                  <td style="width: 100px">
                  
                  <asp:DropDownList ID="SITextBox" Width="45px" runat="server">
                   <asp:ListItem Value=""></asp:ListItem>
                   <asp:ListItem Value="C">C</asp:ListItem>
                   <asp:ListItem Value="S">S</asp:ListItem>
                   
               </asp:DropDownList>
                  </td>
              <td align="right" style="padding-right:5px;"><b>ShipId</b>
                  </td>
                  <td style="width: 140px"><asp:TextBox ID="ship_idTextBox" MaxLength="8" Width="70px" runat="server" Text='<%# Bind("ship_id") %>'>
                  </asp:TextBox>
                  </td>
                  <td></td>
                  <td></td>
                  <td align="right" style="padding-right:5px;"><b>Mail List</b>
                  </td>
                  <td style="width: 100px">
                      <asp:CheckBox ID="maillistTextBox" runat="server" Checked='<%# Bind("maillist") %>' />
                  
                  
                  </td>
              </tr>
              </table>
              <table>
              <tr>
                 
                  
                  <td align="right" style="padding-right:5px;"><b>Company</b>
                  </td>
                  <td style="width: 140px"><asp:TextBox ID="cust_nameTextBox" runat="server" Text='<%# Bind("cust_name") %>'>
                  </asp:TextBox>
                  </td>
                  
                  <td align="right" style="padding-right:5px;"><b>Website</b>
                  </td>
                  <td style="width: 270px"><asp:TextBox ID="websiteTextBox" MaxLength="50" Width="150px" runat="server" Text='<%# Bind("website") %>'>
                  </asp:TextBox>
                  </td>
                  <td align="right" style="padding-right:5px;"><b>Email</b>
                  </td>
                  <td style="width: 270px"><asp:TextBox ID="emailTextBox" MaxLength="50" Width="150px" runat="server" Text='<%# Bind("email") %>'>
                  </asp:TextBox>
                  <asp:RegularExpressionValidator ID="RegularExpressionValidator1" runat="server" SetFocusOnError="true" ControlToValidate="emailTextBox" ErrorMessage="Invalid Email" Display="Dynamic" ValidationExpression="\w+([-+.']\w+)*@\w+([-.]\w+)*\.\w+([-.]\w+)*"></asp:RegularExpressionValidator>
                  </td>                  
                  
              </tr>
              <tr>
              
               <td align="right" style="padding-right:5px;"><b>Address</b>
                  </td>
                  <td style="width: 140px"><asp:TextBox ID="addr1TextBox" runat="server" Text='<%# Bind("addr1") %>'>
                  </asp:TextBox>
                  </td>
                                   
                  
              </tr>
              
              <tr>
              <td align="right" style="padding-right:5px;"><b>Address</b>
                  </td>
                  <td style="width: 140px"><asp:TextBox ID="addr2TextBox" runat="server" Text='<%# Bind("addr2") %>'>
                  </asp:TextBox>
                  </td>
              
              
                
                  
              </tr>
              
               <tr>
                  <td align="right" style="padding-right:5px;" nowrap><b>City:</b>
                  
                  <td nowrap ><asp:TextBox ID="cityTextBox" runat="server"   Width="70px" Text='<%# Bind("city") %>'>
                  </asp:TextBox><a href="#" tabindex="1" onClick="citylook(); return false"><asp:Image ID="Image9" runat="server" ImageUrl="images/lookup_icon.gif" /></a></b></td></td>
                  </td>
                  <td align="right" style="padding-right:5px;"><b>State:</b></td>
                  <td nowrap><asp:TextBox ID="stateTextBox" runat="server"   Width="70px" Text='<%# Bind("state") %>' ></asp:TextBox>
                  <a href="#" tabindex="1" onClick="statecodelook(); return false"><asp:Image ID="Image3" runat="server" ImageUrl="images/lookup_icon.gif" /></a></b></td>
                  <td align="right" style="padding-right:5px;"><b>Zip:</b></td>
                  <td nowrap> <asp:TextBox ID="zipTextBox" Width="70px"   runat="server" Text='<%# Bind("zip") %>' ></asp:TextBox>
                  <a href="#" tabindex="1" onClick="zipcodelook(); return false"><asp:Image ID="Image1" runat="server" ImageUrl="images/lookup_icon.gif" /></a></b></td>
              </tr>
               <tr>
              <td align="right" style="padding-right:5px;"><b>Country</b>
                  </td>
                  <td style="width: 140px"><asp:TextBox ID="countryTextBox" runat="server" Text='<%# Bind("country") %>'>
                  </asp:TextBox>
                  </td>
                  <td align="right" style="padding-right:5px;"><b>County</b>
                  </td>
                  <td style="width: 140px"><asp:TextBox ID="countyTextBox" runat="server" Text='<%# Bind("county") %>'>
                  </asp:TextBox>
                  </td>
                  <td align="right" style="padding-right:5px;"><b>Territory</b>
                  </td>
                  <td nowrap><asp:TextBox ID="territoryTextBox" Width="70px"   runat="server" Text='<%# Bind("territory") %>'>
                  </asp:TextBox>
                  <a href="#" tabindex="1" onClick="terrlook(); return false"><asp:Image ID="Image2" runat="server" ImageUrl="images/lookup_icon.gif" /></a></b>
                  </td>
              </tr>
             
             
              
              <tr>
                <td align="right" style="padding-right:5px;"><b>Supplier Code:</b></td>
                 <td><b>
                  <asp:TextBox ID="comp_codeTextBox" MaxLength="8" OnTextChanged="supp_textchanged"  Width="70px" runat="server" Text='<%# Bind("comp_code") %>'>
                  </asp:TextBox></b><a href="#" tabindex="1" onClick="supplierlook(); return false"><asp:Image ID="SupplierLookup" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
                  <asp:CustomValidator ID="CustomValidator1" runat="server" ErrorMessage="Invalid Supp." ControlToValidate="comp_codeTextBox" Display="dynamic"
                   SetFocusOnError="true" OnServerValidate="Comp_Supp_Validate"></asp:CustomValidator>
                  </td>
                  <td align="right" style="padding-right:5px;"><b>Supplier Desc:</b></td>
                 <td><b>
                  <asp:TextBox ID="comp_descTextBox"   runat="server" Text='<%# Bind("comp_des") %>'>
                  </asp:TextBox></b></td>
                  </tr>
                  <tr>
                  <td align="right" style="padding-right:5px;"><b>Sic Code:</b></td>
                  <td><b><asp:TextBox ID="sic_codeTextBox" MaxLength="5" OnTextChanged="sic_textchanged" Width="40px" runat="server" Text='<%# Bind("sic_code") %>'>
                  </asp:TextBox></b><a href="#" tabindex="1" onClick="siclook(); return false"><asp:Image ID="Siclookup" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
                  <asp:CustomValidator ID="CustomValidator2" runat="server" ErrorMessage="Invalid Sic" ControlToValidate="sic_codeTextBox" Display="dynamic"
                   SetFocusOnError="true" OnServerValidate="Comp_Sic_Validate"></asp:CustomValidator>
                  </td>
                  <td align="right" style="padding-right:5px;"><b>Sic Desc:</b></td>
                  <td><b><asp:TextBox ID="sic_descTextBox"   runat="server" Text='<%# Bind("sic_des") %>'>
                  </asp:TextBox></b></td>
                  </tr>
                  <tr>
                  <td align="right" style="padding-right:5px;"><b>Status Code:</b></td>
                  <td><b><asp:TextBox ID="status_codeTextBox" MaxLength="5" OnTextChanged="status_textchanged"  Width="40px" runat="server" Text='<%# Bind("status_code") %>'>
                  </asp:TextBox></b><a href="#" tabindex="1" onClick="statuslook(); return false"><asp:Image ID="statuslookup" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
                  <asp:CustomValidator ID="CustomValidator3" runat="server" ErrorMessage="Invalid Status" ControlToValidate="status_codeTextBox" Display="dynamic"
                   SetFocusOnError="true" OnServerValidate="Comp_Status_Validate"></asp:CustomValidator>
                  </td>
                   <td align="right" style="padding-right:5px;"><b>Status Desc:</b></td>
                  <td><b><asp:TextBox ID="status_descTextBox" runat="server" Text='<%# Bind("status_des") %>'>
                  </asp:TextBox></b></td>
              </tr>
              
              
             
              <tr><td visible="false">
              </td>
                  <td><asp:TextBox ID="rec_keyTextBox" Visible="false" runat="server" Text='<%# Bind("reckey") %>'>
                  </asp:TextBox></td>
                 <td></td>
                 <td> <asp:TextBox ID="TextBox3" Visible="false" runat="server" Text='<%# Bind("rec_key_value") %>'>
                  </asp:TextBox></td></tr>
                  
                  <asp:TextBox ID="TextBox4" Visible="false" runat="server" Text='<%# Bind("rec_key") %>'>
                  </asp:TextBox>
                  
                  
             <tr><td><br /></td></tr>
             <tr><td align="center">
             <asp:Button ID="InsertButton" runat="server" CssClass="buttonM" CausesValidation="True"
                      Text="Save" OnClick="InsertButton_Click" >
                  </asp:Button></td><td>
                  <asp:Button ID="InsertCancelButton" CssClass="buttonM" runat="server" CausesValidation="False" CommandName="Cancel"
                      Text="Cancel">
                  </asp:Button></td></tr>
          </table>
          </fieldset>
                 
              </InsertItemTemplate>
              <ItemTemplate>
              <fieldset style="background-color:#EFF3FB; width:1000px;">
              
            <table class="shade">
              <tr>
              <td align="right" style="padding-right:5px;"><b>Sir Name:</b></td>
              <td><b>
              
                  <asp:DropDownList ID="DropDownList2" Enabled="false" BackColor="turquoise" Width="45px" runat="server" SelectedValue='<%# Bind("sirname") %>'>
                   <asp:ListItem Value=""></asp:ListItem>
                   <asp:ListItem Value="Mr.">Mr.</asp:ListItem>
                   <asp:ListItem Value="Mrs.">Mrs.</asp:ListItem>
                   <asp:ListItem Value="Ms.">Ms.</asp:ListItem>
                   <asp:ListItem Value="Dr.">Dr.</asp:ListItem>                   
               </asp:DropDownList>
              </b></td>
              <td width="80px" align="right" style="padding-right:5px;"><b>Contact:</b></td>
              <td style="width:90px;"><b><asp:Label ID="first_nameLabel" BackColor="Turquoise" BorderColor="Black" Width="150px" BorderStyle="Solid" BorderWidth="1px" runat="server" Text='<%# Bind("first_name") %>'>
                  </asp:Label></b></td>
              <td><b><asp:Label ID="middle_initialLabel" Visible="false" BackColor="Turquoise" BorderColor="Black" Width="80px" BorderStyle="Solid" BorderWidth="1px" runat="server" Text='<%# Bind("middle_initial") %>'>
                  </asp:Label></b></td>
              <td style="width:90px;"><b><asp:Label ID="last_nameLabel" runat="server" BackColor="Turquoise" BorderColor="Black" Width="170px" BorderStyle="Solid" BorderWidth="1px" Text='<%# Bind("last_name") %>'></asp:Label></b></td>
              <td align="right" style="padding-right:5px;"><b>Title:</b></td>
              <td><b><asp:Label ID="contact_titleLabel" BackColor="Turquoise" BorderColor="Black" Width="150px" BorderStyle="Solid" BorderWidth="1px" runat="server" Text='<%# Bind("contact_title") %>'>
                  </asp:Label></b></td>
              <td align="right" style="padding-right:5px;" nowrap><b>Customer Code:</b></td>
              <td><b><asp:Label ID="cust_noLabel" BackColor="Turquoise" BorderColor="Black" Width="120px" BorderStyle="Solid" BorderWidth="1px" runat="server" Text='<%# Bind("cust_no") %>'></asp:Label>
              </b></td>
              </tr>
              </table>
             <table>
             <tr>
             <td align="right" style="padding-right:5px;"><b>Access Code:</b></td>
              <td><b><asp:Label ID="access_codeLabel" runat="server" BackColor="Turquoise" BorderColor="Black" Width="120px" BorderStyle="Solid" BorderWidth="1px" Text='<%# Bind("access_code") %>'>
                  </asp:Label></b></td>
              <td align="right" style="padding-right:5px;"><b>Phone:</b></td>
              <td><b><asp:Label ID="phoneLabel" runat="server" BackColor="Turquoise" BorderColor="Black" Width="120px" BorderStyle="Solid" BorderWidth="1px" Text='<%# Bind("phone") %>'></asp:Label></b></td>
              <td align="right" style="padding-right:5px;"><b>Extension:</b></td>
              <td><b><asp:Label ID="extensionLabel" runat="server" BackColor="Turquoise" BorderColor="Black" Width="120px" BorderStyle="Solid" BorderWidth="1px" Text='<%# Bind("extension") %>'></asp:Label></b></td>
             <td align="right" style="padding-right:5px;"><b>Type:</b></td>
              <td><b><asp:Label ID="typeLabel" BackColor="Turquoise" BorderColor="Black" Width="120px" BorderStyle="Solid" BorderWidth="1px" runat="server" Text='<%# Bind("type") %>'></asp:Label></b></td>
               
             </tr>
             <tr>
             <td align="right" style="padding-right:5px;"><b>Cell Phone:</b></td>
              <td><b><asp:Label ID="cell_phoneLabel" runat="server" BackColor="Turquoise" BorderColor="Black" Width="120px" BorderStyle="Solid" BorderWidth="1px" Text='<%# Bind("cell_phone") %>'>
                  </asp:Label></b></td>
                  <td align="right" style="padding-right:5px;"><b>Fax:</b></td>
              <td><b><asp:Label ID="faxLabel" runat="server" BackColor="Turquoise" BorderColor="Black" Width="120px" BorderStyle="Solid" BorderWidth="1px" Text='<%# Bind("fax") %>'></asp:Label></b></td>
              
                  <td></td>
                  <td></td>
                   <td align="right" style="padding-right:5px;"><b>Sales Rep:</b></td>
              <td><b><asp:Label ID="smanLabel" BackColor="Turquoise" BorderColor="Black" Width="150px" BorderStyle="Solid" BorderWidth="1px" runat="server" Text='<%# Bind("sman") %>'></asp:Label>
               </td>
             </tr>
             <tr>
             <td align="right" style="padding-right:px;"><b>Location:</b></td>
              <td><b>
                  <asp:DropDownList ID="SITextBox" Enabled="false" BackColor="turquoise" Width="45px" runat="server" SelectedValue='<%# Bind("contact_loc") %>'>
                   <asp:ListItem Value=""></asp:ListItem>
                   <asp:ListItem Value="C">C</asp:ListItem>
                   <asp:ListItem Value="S">S</asp:ListItem>
                   
               </asp:DropDownList>
                  </b></td>
             <td align="right" style="padding-right:5px;"><b>Shipid:</b></td>
              <td><b><asp:Label ID="ship_idLabel" BackColor="Turquoise" BorderColor="Black" Width="120px" BorderStyle="Solid" BorderWidth="1px" runat="server" Text='<%# Bind("ship_id") %>'></asp:Label></b></td>
              <td></td>
              <td></td>
              <td align="right" style="padding-right:5px;"><b>Mail List:</b></td>
              <td><b>
              <asp:Label ID="maillistLabel2" Visible="false" BackColor="Turquoise" BorderColor="Black" Width="120px" BorderStyle="Solid" BorderWidth="1px" runat="server" Text='<%# Bind("maillist") %>'></asp:Label></b>
                  <asp:CheckBox ID="maillistLabel" BackColor="turquoise" BorderColor="black" BorderStyle="solid" Checked='<%# Bind("maillist") %>' Enabled="false" runat="server" />
              </td>
             </tr>
             </table>
              
              <table>
                           
              
              <tr>
              
              <td align="right" style="padding-right:5px;"><b>Company:</b></td>
              <td><b><asp:Label ID="cust_nameLabel" BackColor="Turquoise" BorderColor="Black" Width="150px" BorderStyle="Solid" BorderWidth="1px" runat="server" Text='<%# Bind("cust_name") %>'></asp:Label></b></td>
              <td align="right" style="padding-right:5px;"><b>Website:</b></td>
              <td><b><asp:Label ID="websiteLabel" runat="server" BackColor="Turquoise" BorderColor="Black" Width="170px" BorderStyle="Solid" BorderWidth="1px" Text='<%# Bind("website") %>'></asp:Label></b></td>
              <td align="right" style="padding-right:5px;"><b>Email:</b></td>
              <td><b><asp:Label ID="emailLabel" BackColor="Turquoise" BorderColor="Black" Width="170px" BorderStyle="Solid" BorderWidth="1px" runat="server" Text='<%# Bind("email") %>'></asp:Label></b></td>
              
              </tr>
              
              <tr>
              
              <td align="right" style="padding-right:5px;"><b>Address1:</b></td>
              <td><b><asp:Label ID="addr1Label" runat="server" BackColor="Turquoise" BorderColor="Black" Width="150px" BorderStyle="Solid" BorderWidth="1px" Text='<%# Bind("addr1") %>'></asp:Label></b></td>
              
              </tr>
              
              <tr>
              
              <td align="right" style="padding-right:5px;"><b>Address2:</b></td>
              <td><b><asp:Label ID="addr2Label" runat="server" BackColor="Turquoise" BorderColor="Black" Width="150px" BorderStyle="Solid" BorderWidth="1px" Text='<%# Bind("addr2") %>'></asp:Label></b></td>
              
              
              </tr>
              
              <tr>
              <td align="right" style="padding-right:5px;"><b>City:</b></td>
              <td><b><asp:Label ID="cityLabel" runat="server" BackColor="Turquoise" BorderColor="Black" Width="120px" BorderStyle="Solid" BorderWidth="1px" Text='<%# Bind("city") %>'></asp:Label></b></td>
              <td align="right" style="padding-right:5px;"><b>State:</b></td>
              <td><b><asp:Label ID="stateLabel" runat="server" BackColor="Turquoise" BorderColor="Black" Width="120px" BorderStyle="Solid" BorderWidth="1px" Text='<%# Bind("state") %>'></asp:Label></b></td>
              <td align="right" style="padding-right:5px;"><b>Zip:</b></td>
              <td><b><asp:Label ID="zipLabel" runat="server" BackColor="Turquoise" BorderColor="Black" Width="120px" BorderStyle="Solid" BorderWidth="1px" Text='<%# Bind("zip") %>'></asp:Label></b></td>
              </tr>
              <tr>
              <td align="right" style="padding-right:5px;"><b>Country:</b></td>
              <td><b><asp:Label ID="countryLabel" runat="server" BackColor="Turquoise" BorderColor="Black" Width="120px" BorderStyle="Solid" BorderWidth="1px" Text='<%# Bind("country") %>'></asp:Label></b></td>
              <td align="right" style="padding-right:5px;"><b>County:</b></td>
              <td><b><asp:Label ID="countyLabel" runat="server" BackColor="Turquoise" BorderColor="Black" Width="120px" BorderStyle="Solid" BorderWidth="1px" Text='<%# Bind("county") %>'></asp:Label></b></td>
              <td align="right" style="padding-right:5px;"><b>Territory:</b></td>
              <td><b><asp:Label ID="territoryLabel" runat="server" BackColor="Turquoise" BorderColor="Black" Width="120px" BorderStyle="Solid" BorderWidth="1px" Text='<%# Bind("territory") %>'></asp:Label></b></td>
              </tr>
              
             
              
               <tr>
              <td align="right" style="padding-right:5px;"><b>Supplier Code:</b></td>
               <td><b>
                  <asp:Label ID="comp_codeLabel" runat="server" BackColor="Turquoise" BorderColor="Black" Width="120px" BorderStyle="Solid" BorderWidth="1px" Text='<%# Bind("comp_code") %>'></asp:Label></b></td>
                  <td align="right" style="padding-right:5px;"><b>Supplier Desc:</b></td>
               <td><b>
                  <asp:Label ID="comp_descLabel" runat="server" BackColor="Turquoise" BorderColor="Black" Width="170px" BorderStyle="Solid" BorderWidth="1px" Text='<%# Bind("comp_des") %>'></asp:Label></b></td>
                 
                  </tr>
                  <tr>
                  <td align="right" style="padding-right:5px;"><b>Sic Code:</b></td>
                  <td><b>
                  <asp:Label ID="sic_codeLabel" runat="server" BackColor="Turquoise" BorderColor="Black" Width="120px" BorderStyle="Solid" BorderWidth="1px" Text='<%# Bind("sic_code") %>'></asp:Label></b></td>
                  <td align="right" style="padding-right:5px;"><b>Sic Desc:</b></td>
                  <td><b>
                  <asp:Label ID="sic_descLabel" runat="server" BackColor="Turquoise" BorderColor="Black" Width="170px" BorderStyle="Solid" BorderWidth="1px" Text='<%# Bind("sic_des") %>'></asp:Label></b></td>
                  
                  </tr>
                  <tr>
                  <td align="right" style="padding-right:5px;"><b>Status Code:</b></td>
                 <td><b> <asp:Label ID="status_codeLabel" BackColor="Turquoise" BorderColor="Black" Width="120px" BorderStyle="Solid" BorderWidth="1px" runat="server" Text='<%# Bind("status_code") %>'>
                  </asp:Label></b></td>
              <td align="right" style="padding-right:5px;"><b>Status Desc:</b></td>
                 <td><b> <asp:Label ID="status_descLabel" BackColor="Turquoise" BorderColor="Black" Width="170px" BorderStyle="Solid" BorderWidth="1px" runat="server" Text='<%# Bind("status_des") %>'>
                  </asp:Label></b></td>
              </tr>
              
              
              </table>
              <table>
              <tr>
              <td></td>
                  <td><asp:Label ID="rec_keyLabel" Visible="false"  runat="server" Text='<%# Bind("reckey") %>'></asp:Label>
                  </td>
                 <td></td>  
                  <td><asp:Label ID="Label1" Visible="false" runat="server" Text='<%# Bind("rec_key_value") %>'></asp:Label>
                  </td></tr>
                  
                  
              </td>
              </tr>
              </table>
                  
                 
                  <table>
                  <tr>
                  <td>
                  
     <asp:Button ID="EditButton" runat="server" Text="Update" CssClass="buttonM" CommandName="Edit" />
     <asp:Button ID="AddnewButton" runat="server" Text="Add" CssClass="buttonM" OnClick="AddnewButton_click" CommandName="New" />
     <asp:Button ID="CopyButton" runat="server" Text="Copy" CssClass="buttonM" OnClick="CopyButton_Click" />
     <asp:Button ID="DeleteButton" runat="server" OnClientClick="return confirm('Are you sure you want to delete this record')" CssClass="buttonM" CommandName="Delete" OnClick="DeleteButton_Click" Text="Delete" />
                  </td>
                  </tr>
                  </table>
                  </fieldset>
              </ItemTemplate>
          </asp:FormView>
          
          
          
          <asp:SqlDataSource id="SqlDataSource2" runat="server" 
            SelectCommand="select [comp_code],[sic_code],[status_code], [company],   [cust_no],   [ship_id],   [sman],   [first_name],   [last_name],   [middle_initial],   [sirname],   [contact_title],   [maillist],   [type],   [contact_loc],   [cust_name],   [addr1],   [addr2],   [city],   [state],   [zip],   [country],   [county],   [territory],   [access_code],   [phone],   [cell_phone],   [fax],   [extension],   [email],   [website],   [rec_key],[comp_des],[status_des],[sic_des],   [list_no],   [list_name],[reckey],[rec_key_value] from   [dbo].[contact],[dbo].[contact_reckey] where [rec_key]=@rec_key  ORDER BY [reckey] DESC, [last_name] DESC "

                        
            OnInserted="contactSqlDataSource_Inserted"
            OnInserting="contactSqlDataSource_Inserting"    
            ConnectionString="<%$ ConnectionStrings:Project1ConnectionString%>"
            ProviderName="<%$ ConnectionStrings:Project1ConnectionString.providerName%>"
        >
        <SelectParameters>
       
        <asp:SessionParameter Name="rec_key" SessionField="contact_rec_key"
                        Type="String" DefaultValue="" />                        
        </SelectParameters>       
       
    </asp:SqlDataSource>
    
    
          <asp:FormView ID="FormView2" runat="server" DataSourceID="SqlDataSource2" OnDataBound="FormView2_DataBound">
              <EditItemTemplate>
              
                
              </EditItemTemplate>
              <InsertItemTemplate>
              <fieldset style="background-color:#EFF3FB">
              <table class="shade">
              <tr>
              <td align="right" style="padding-right:5px;"><b>Sir Name:</b></td>
              <td><b>
                   <asp:DropDownList ID="DropDownList2" Width="45px" runat="server" SelectedValue='<%# Bind("sirname") %>'>
                   <asp:ListItem Value=""></asp:ListItem>
                   <asp:ListItem Value="Mr.">Mr.</asp:ListItem>
                   <asp:ListItem Value="Mrs.">Mrs.</asp:ListItem>
                   <asp:ListItem Value="Ms.">Ms.</asp:ListItem>
                   <asp:ListItem Value="Dr.">Dr.</asp:ListItem>                   
               </asp:DropDownList>
                  </b></td>
              <td align="right" style="padding-right:5px;"><b>Contact:</b></td>
              <td><b><asp:TextBox ID="first_nameTextBox" MaxLength="30" Width="150px" runat="server" Text='<%# Bind("first_name") %>'>
                  </asp:TextBox></b>
                  <asp:RequiredFieldValidator ID="RequiredFieldValidator5" ControlToValidate="first_nameTextBox" SetFocusOnError="true" Display="dynamic" runat="server" ErrorMessage="Required Field"></asp:RequiredFieldValidator>
                  </td>
              <td><b><asp:TextBox ID="middle_initialTextBox" Visible="false" MaxLength="1" Width="30px" runat="server" Text='<%# Bind("middle_initial") %>'>
                  </asp:TextBox></b></td>
              <td><b><asp:TextBox ID="last_nameTextBox" MaxLength="30" Width="150px" runat="server" Text='<%# Bind("last_name") %>'>
                  </asp:TextBox></b>
                  <asp:RequiredFieldValidator ID="RequiredFieldValidator4" ControlToValidate="last_nameTextBox" SetFocusOnError="true" Display="dynamic" runat="server" ErrorMessage="Required Field"></asp:RequiredFieldValidator>
                  </td>
                  <td align="right" style="padding-right:5px;"><b>Title:</b></td>
              <td><b><asp:TextBox ID="contact_titleTextBox" MaxLength="50"  Width="150px" runat="server" Text='<%# Bind("contact_title") %>'>
                  </asp:TextBox></b><a href="#" tabindex="1" onClick="titlecopylook(); return false"><asp:Image ID="Image11" runat="server" ImageUrl="images/lookup_icon.gif" /></a></td>
             
              <td align="right" style="padding-right:5px;" nowrap><b>Customer Code:</b></td>
              <td nowrap><b><asp:TextBox ID="cust_noTextBox" Width="70px"  MaxLength="8" runat="server" Text='<%# Bind("cust_no") %>'>
                  </asp:TextBox>
                  <asp:RequiredFieldValidator ID="RequiredFieldValidator1"   ControlToValidate="cust_noTextBox" Display="dynamic" SetFocusOnError="true" runat="server" ErrorMessage="This field cannot be blank"></asp:RequiredFieldValidator>
                  <a href="#" tabindex="1" onClick="contactcustomercopylook(); return false"><asp:Image ID="CustomerLook" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
                  </b></td>
              </tr>
              </table>
              <table>
              <tr>
              <td align="right" style="padding-right:5px;"><b>Access Code:</b></td>
              <td><b><asp:TextBox ID="access_codeTextBox" MaxLength="20" Width="140px" runat="server" Text='<%# Bind("access_code") %>'>
                  </asp:TextBox></b></td>
              <td align="right" style="padding-right:5px;"><b>Phone:</b></td>
              <td><b><asp:TextBox ID="phoneTextBox" MaxLength="12" Width="70px" runat="server" Text='<%# Bind("phone") %>'>
                  </asp:TextBox></b>
                  <asp:RegularExpressionValidator ID="RegularExpressionValidator6" runat="server" SetFocusOnError="true" ControlToValidate="phoneTextBox" ErrorMessage="Invalid Phone No" Display="Dynamic" ValidationExpression="((\(\d{3}\) ?)|(\d{3}-))?\d{3}-\d{4}"></asp:RegularExpressionValidator>
                  </td>
              <td align="right" style="padding-right:5px;"><b>Extension:</b></td>
              <td><b><asp:TextBox ID="extensionTextBox" MaxLength="5" Width="40px" runat="server" Text='<%# Bind("extension") %>'>
                  </asp:TextBox></b></td>
              <td align="right" style="padding-right:5px;"><b>Type:</b></td>
              <td><b><asp:TextBox ID="typeTextBox" MaxLength="8" Width="70px"   runat="server" Text='<%# Bind("type") %>'>
                  </asp:TextBox></b><a href="#" tabindex="1" onClick="typecopylook(); return false"><asp:Image ID="Image10" runat="server" ImageUrl="images/lookup_icon.gif" /></a></td>
              
              </tr>
              <tr>
               <td align="right" style="padding-right:5px;"><b>Cell Phone:</b></td>
              <td><b><asp:TextBox ID="cell_phoneTextBox" MaxLength="12" Width="70px" runat="server" Text='<%# Bind("cell_phone") %>'>
                  </asp:TextBox></b>
                  <asp:RegularExpressionValidator ID="RegularExpressionValidator7" runat="server" SetFocusOnError="true" ControlToValidate="cell_phoneTextBox" ErrorMessage="Invalid Cell No" Display="Dynamic" ValidationExpression="((\(\d{3}\) ?)|(\d{3}-))?\d{3}-\d{4}"></asp:RegularExpressionValidator>
                  </td>
                  <td align="right" style="padding-right:5px;"><b>Fax:</b></td>
              <td><b><asp:TextBox ID="faxTextBox" MaxLength="12" Width="80px" runat="server" Text='<%# Bind("fax") %>'>
                  </asp:TextBox></b>
                  <asp:RegularExpressionValidator ID="RegularExpressionValidator8" runat="server" SetFocusOnError="true" ControlToValidate="faxTextBox" ErrorMessage="Invalid Fax" Display="Dynamic" ValidationExpression="((\(\d{3}\) ?)|(\d{3}-))?\d{3}-\d{4}"></asp:RegularExpressionValidator>
                  </td>
                  <td></td>
                  <td></td>
              <td align="right" style="padding-right:5px;"><b>Sales Rep:</b></td>
              <td><b><asp:TextBox ID="smanTextBox" runat="server"   Text='<%# Bind("sman") %>'>
                  </asp:TextBox> <a href="#" tabindex="1" onClick="smancopyLook(); return false"><asp:Image ID="SalesRep" runat="server" ImageUrl="images/lookup_icon.gif" /></a></b></td>
              
              </tr>
              <tr>
              <td align="right" style="padding-right:5px;"><b>Location:</b></td>
              <td><b><%--<asp:TextBox ID="contact_locTextBox" runat="server" Text='<%# Bind("contact_loc") %>'>
                  </asp:TextBox>--%>
                  <asp:DropDownList ID="SITextBox" Width="45px" runat="server">
                   <asp:ListItem Value=""></asp:ListItem>
                   <asp:ListItem Value="C">C</asp:ListItem>
                   <asp:ListItem Value="S">S</asp:ListItem>
                   
               </asp:DropDownList>
                  </b></td>
              <td align="right" style="padding-right:5px;"><b>Shipid:</b></td>
              <td><b><asp:TextBox ID="ship_idTextBox" MaxLength="8" Width="70px" runat="server" Text='<%# Bind("ship_id") %>'>
                  </asp:TextBox></b></td>
                  <td></td>
                  <td></td>
              <td align="right" style="padding-right:px;"><b>Mail List:</b></td>
              <td><b>
             <%-- <asp:TextBox ID="maillistTextBox" MaxLength="3" Width="40px" runat="server" >
                  </asp:TextBox>--%>
                  
                  <asp:CheckBox ID="maillistTextBox" Checked='<%# Bind("maillist") %>' runat="server" />
                  </b></td>
              </tr>
              </table>
              <table>
              
              
              <tr>
               
              <td align="right" style="padding-right:5px;"><b>Company:</b></td>
              <td><b><asp:TextBox ID="cust_nameTextBox" runat="server" Text='<%# Bind("cust_name") %>'>
                  </asp:TextBox></b></td>
                  
                  <td align="right" style="padding-right:5px;"><b>Website:</b></td>
              <td><b><asp:TextBox ID="websiteTextBox" MaxLength="50" Width="150px" runat="server" Text='<%# Bind("website") %>'>
                  </asp:TextBox></b></td>
                  <td align="right" style="padding-right:5px;"><b>Email:</b></td>
              <td><b><asp:TextBox ID="emailTextBox" MaxLength="50" Width="150px" runat="server" Text='<%# Bind("email") %>'>
                  </asp:TextBox></b>
                  <asp:RegularExpressionValidator ID="RegularExpressionValidator1" runat="server" SetFocusOnError="true" ControlToValidate="emailTextBox" ErrorMessage="Invalid Email" Display="Dynamic" ValidationExpression="\w+([-+.']\w+)*@\w+([-.]\w+)*\.\w+([-.]\w+)*"></asp:RegularExpressionValidator>
                  </td> 
              
              </tr>
              <tr>
              
              <td align="right" style="padding-right:5px;"><b>Address1:</b></td>
              <td><b><asp:TextBox ID="addr1TextBox" runat="server" Text='<%# Bind("addr1") %>'>
                  </asp:TextBox></b></td>
              
              </tr>
              <tr>
              <td align="right" style="padding-right:5px;"><b>Address2:</b></td>
              <td><b><asp:TextBox ID="addr2TextBox" runat="server" Text='<%# Bind("addr2") %>'>
                  </asp:TextBox></b></td>
              
              
              </tr>
              
              <tr>
              <td align="right" style="padding-right:5px;"><b>City:</b></td>
              <td><b><asp:TextBox ID="cityTextBox" runat="server"  Text='<%# Bind("city") %>'>
                  </asp:TextBox><a href="#" tabindex="1" onClick="citycopylook(); return false"><asp:Image ID="Image12" runat="server" ImageUrl="images/lookup_icon.gif" /></a></b></td>
              <td align="right" style="padding-right:5px;"><b>State:</b></td>
              <td><b><asp:TextBox ID="stateTextBox" Width="70"   runat="server" Text='<%# Bind("state") %>'>
                  </asp:TextBox>
                  <a href="#" tabindex="1" onClick="statecodecopylook(); return false"><asp:Image ID="Image4" runat="server" ImageUrl="images/lookup_icon.gif" /></a></b></td>
              <td align="right" style="padding-right:5px;"><b>Zip:</b></td>
              <td nowrap><b><asp:TextBox ID="zipTextBox" Width="70px"   runat="server" Text='<%# Bind("zip") %>'>
                  </asp:TextBox>
                  <a href="#" tabindex="1" onClick="zipcodecopylook(); return false"><asp:Image ID="Image1" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
                  </b></td>
              </tr>
              <tr>
              <td align="right" style="padding-right:5px;"><b>Country:</b></td>
              <td><b><asp:TextBox ID="countryTextBox" runat="server" Text='<%# Bind("country") %>'>
                  </asp:TextBox></b></td>
              <td align="right" style="padding-right:5px;"><b>County:</b></td>
              <td><b><asp:TextBox ID="countyTextBox" runat="server" Text='<%# Bind("county") %>'>
                  </asp:TextBox></b></td>
              <td align="right" style="padding-right:5px;"><b>Territory:</b></td>
              <td nowrap><b><asp:TextBox ID="territoryTextBox"   Width="70px" runat="server" Text='<%# Bind("territory") %>'>
                  </asp:TextBox>
                  <a href="#" tabindex="1" onClick="terrcopylook(); return false"><asp:Image ID="Image2" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
                  </b></td>
              </tr>
              
             
              
              <tr>
              <td align="right" style="padding-right:5px;"><b>Supplier Code:</b></td>
                 <td><b>
                  <asp:TextBox ID="comp_codeTextBox" runat="server" OnTextChanged="supp2_textchanged" MaxLength="8" Width="70px" Text='<%# Bind("comp_code") %>'>
                  </asp:TextBox>                 
                  </b>
                  <a href="#" tabindex="1" onClick="suppliercopylook(); return false"><asp:Image ID="SupplierLookup" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
                  <asp:CustomValidator ID="CustomValidator1" runat="server" ErrorMessage="Invalid Supp." ControlToValidate="comp_codeTextBox" Display="dynamic"
                   SetFocusOnError="true" OnServerValidate="Comp_Supp2_Validate"></asp:CustomValidator>
                  </td>
                  <td align="right" style="padding-right:5px;"><b>Supplier Desc:</b></td>
                 <td><b>
                  <asp:TextBox ID="comp_descTextBox"   runat="server" Text='<%# Bind("comp_des") %>'>
                  </asp:TextBox></b></td>
                  </tr>
                  <tr>
                  <td align="right" style="padding-right:5px;"><b>Sic Code:</b></td>
                  <td><b><asp:TextBox ID="sic_codeTextBox" OnTextChanged="sic2_textchanged"  MaxLength="5" Width="40px" runat="server" Text='<%# Bind("sic_code") %>'>
                  </asp:TextBox></b>
                  <a href="#" tabindex="1" onClick="siccopylook(); return false"><asp:Image ID="Image5" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
                  <asp:CustomValidator ID="CustomValidator2" runat="server" ErrorMessage="Invalid Sic" ControlToValidate="sic_codeTextBox" Display="dynamic"
                   SetFocusOnError="true" OnServerValidate="Comp_Sic2_Validate"></asp:CustomValidator>
                  </td>
                  <td align="right" style="padding-right:5px;"><b>Sic Desc:</b></td>
                  <td><b><asp:TextBox ID="sic_descTextBox"   runat="server" Text='<%# Bind("sic_des") %>'>
                  </asp:TextBox></b></td>
                 </tr>
                 <tr>
                 <td align="right" style="padding-right:5px;"><b>Status Code:</b></td>
                  <td><b><asp:TextBox ID="status_codeTextBox" OnTextChanged="status2_textchanged"  MaxLength="5" Width="40px" runat="server" Text='<%# Bind("status_code") %>'>
                  </asp:TextBox></b>
                  <a href="#" tabindex="1" onClick="statuscopylook(); return false"><asp:Image ID="Image6" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
                  <asp:CustomValidator ID="CustomValidator3" runat="server" ErrorMessage="Invalid Status" ControlToValidate="status_codeTextBox" Display="dynamic"
                   SetFocusOnError="true" OnServerValidate="Comp_Status2_Validate"></asp:CustomValidator>
                  </td>
               <td align="right" style="padding-right:5px;"><b>Status Desc:</b></td>
                  <td><b><asp:TextBox ID="status_descTextBox" runat="server" Text='<%# Bind("status_des") %>'>
                  </asp:TextBox></b></td>
               </tr>
              
               
              <tr><td visible="false">
              </td>
                  <td><asp:TextBox ID="rec_keyTextBox" Visible="false" runat="server" Text='<%# Bind("reckey") %>'>
                  </asp:TextBox></td>
                 <td></td>
                 <td> <asp:TextBox ID="TextBox3" Visible="false" runat="server" Text='<%# Bind("rec_key_value") %>'>
                  </asp:TextBox></td></tr>
                  
                  <asp:TextBox ID="TextBox4" Visible="false" runat="server" Text='<%# Bind("rec_key") %>'>
                  </asp:TextBox>
              <tr><td></td></tr>
              <tr><td></td></tr>
              </table>
              
                  
                  <asp:Button ID="InsertButton" CssClass="buttonM" runat="server" CausesValidation="True"
                      Text="Save" OnClick="InsertButton2_Click">
                  </asp:Button>
                  <asp:Button ID="InsertCancelButton" CssClass="buttonM" runat="server" CausesValidation="False" CommandName="Cancel"
                      Text="Cancel" OnClick="InsertCancelButton_Click23">
                  </asp:Button>
                  
                  </fieldset>
              </InsertItemTemplate>
              <ItemTemplate>
              
                 
              </ItemTemplate>
          </asp:FormView>
               
        
    </div>
    <ft:footer id="Footer1" runat="server"></ft:footer>
    </form>
</body>
</html>
