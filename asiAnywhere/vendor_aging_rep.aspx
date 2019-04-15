<%@ Page Language="c#" AutoEventWireup="true" Debug="false" Inherits="vendor_aging_rep" Codebehind="~/vendor_aging_rep.aspx.cs" %>
<%@ Register Src="footer.ascx" TagName="Footer" TagPrefix="ft" %>
<%@ Register Src="header.ascx" TagName="Header" TagPrefix="hd" %>
<html xmlns="http://www.w3.org/1999/xhtml" >
  <head id="Head1" runat="server">
    <title>Vendor Aging</title>  
         
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
    alert("Begin and End Customer Value must be same");
    endc.value=beginc.value;
    endc.focus();
    }
    }
    
    function contactcustomerlook(){ 
  var NewWindow = window.open("contact_customer_lookup.aspx","ContactCustomerLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}

function ContactCustomerLookup(ReturnObj1,ReturnObj2,ReturnObj3,ReturnObj4,ReturnObj5,ReturnObj6,ReturnObj7,ReturnObj8,ReturnObj9, ReturnObj10,ReturnObj11){ 
  document.forms[0].TextBox1.value = ReturnObj1;
  //document.forms[0].TextBox2.value = ReturnObj1;
  
    
}

function contactcustomerlook2(){ 
  var NewWindow = window.open("contact_customer_copylookup.aspx","ContactCustomerLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}

function ContactCustomerCopyLookup(ReturnObj1,ReturnObj2,ReturnObj3,ReturnObj4,ReturnObj5,ReturnObj6,ReturnObj7,ReturnObj8,ReturnObj9, ReturnObj10,ReturnObj11){ 
  //document.forms[0].TextBox1.value = ReturnObj1;
  document.forms[0].TextBox2.value = ReturnObj1;
  
    
}

function salesreplook(){ 
  var NewWindow = window.open("salesrep_lookup.aspx","SalesRepLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}
function SalesRepLookup(ReturnObj1,ReturnObj2){ 
  document.forms[0].besmanTextBox.value = ReturnObj1;
  }


function smancopylook1(){ 
  var NewWindow = window.open("sman_copylookup.aspx","smancopyLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}
function smancopyLookup(ReturnObj1,ReturnObj2){ 
  document.forms[0].endsmanTextBox.value = ReturnObj1;
 }

 function Relook() {
     var item1 = document.getElementById("TextBox1").value;
  var NewWindow = window.open("reorder_item_lookup.aspx?item="+ item1 +"","ReItemLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}

function ReLookup(ReturnObj1){ 
  document.forms[0].beitemTextBox.value = ReturnObj1;
}
function Relook2() {
    var item2 = document.getElementById("TextBox2").value;
  var NewWindow = window.open("reorder_item_lookup2.aspx?item1="+ item2 +"","ReItemLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}

function ReLookup2(ReturnObj1){ 
  document.forms[0].enditTextBox.value = ReturnObj1;
}

function Datelook(){ 
  var NewWindow = window.open("date_lookup.aspx","DateLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
  }
function Datelookup(obj)
{
  document.forms[0].bedateTextBox.value=obj;
}

var vtyp;
function vendortypelook(obj4) {
    vtyp = obj4;
    var NewWindow = window.open("vendtype_lookup.aspx", "vendtypeLookupWindow", "width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}
function VendTypeLookup(ReturnObj1) {
    if (vtyp == "1") {
        document.forms[0].begtypeTextBox.value = ReturnObj1;
    }
    else {
        document.forms[0].endtypeTextBox.value = ReturnObj1;
    }
}

function datevalidate()
{
    var date=document.getElementById("bedateTextBox").value;
    
    if(date.length>1 && date.length<3 && date.indexOf('/')!=1)
    {
        document.getElementById("bedateTextBox").value = date + "/";
    }
    if(date.length>4 && date.length<6 && date.indexOf('/')!=3)
    {
        document.getElementById("bedateTextBox").value = date + "/";
    }
    var date2=document.getElementById("enddateTextBox").value;
    
    if(date2.length>1 && date2.length<3 && date2.indexOf('/')!=1)
    {
        document.getElementById("enddateTextBox").value = date2 + "/";
    }
    if(date2.length>4 && date2.length<6 && date2.indexOf('/')!=3)
    {
        document.getElementById("enddateTextBox").value = date2 + "/";
    }
}
function lnk_pdf_Click() {
    var pdfile = document.getElementById("fdgfd").innerText;
    var NewWindow = window.open("print_download_list.aspx", "OrderListNotes", "width=600,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}
var newvend = "";
function vendorlook(obj1) {
    newvend = obj1;
    var NewWindow = window.open("corvend_lookup.aspx", "VendLookup", "width=500,height=400,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}

function VendLookup(ReturnObj1) {
    if (newvend == "1")
        document.forms[0].begvendTextBox.value = ReturnObj1;
    else
        document.forms[0].endvendTextBox.value = ReturnObj1;

}
    </script> 
  </head>    
   <body>
    <form id="frmList" runat="server"  defaultfocus='begcompTextBox'>   
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
         
          <asp:Label ID="Label1" runat="server" ForeColor="red" Font-Bold="true"></asp:Label>
          
      <TABLE id="tblTop" cellSpacing="3" align="center" border="0" Width="100%">
        <TR>
            
          <TD align=center nowrap><font size=+0><b>Vendor Aging &nbsp;</b></font></TD>
          <TD vAlign="middle" nowrap >
            <asp:linkbutton id="hlkBackToMenu" OnClick="Back_tomenu_Click" runat="server" >Back to menu</asp:linkbutton>
          </TD>
          <TD vAlign="middle" align="center" nowrap >Logged as&nbsp;
            <asp:label id="lblUser" runat="server" Font-Bold="True">&nbsp;</asp:label>&nbsp;&nbsp;&nbsp;
            <asp:linkbutton id="hlnkLogOut" runat="server" OnClick="hlnkLogOut_Click">Log out</asp:linkbutton>
            &nbsp;&nbsp;<asp:hyperlink id="hlnkChangePwd" runat="server" NavigateUrl="changepwd.aspx"></asp:hyperlink>
          &nbsp;<b>Company:</b>&nbsp;<asp:Label ID="labelcompany" runat="server" Text="Label"></asp:Label></TD>
          
         
          <TD vAlign="middle" width="20">&nbsp;</TD>
          
          <td width=30>&nbsp;</td>
        </TR>
      </TABLE>
      
      <table class="shade" width="630px">
       
      <tr><td align="right" style="padding-right: 5px"><b>Beginning Company#:</b></td><td>
          <asp:TextBox ID="begcompTextBox"  width="100px" runat="server"></asp:TextBox>
         
          </td>
      <td align="right" style="padding-right: 5px"><b>Ending Company#:</b></td>
      <td nowrap>
        <asp:TextBox ID="endcompTextBox" width="100px" runat="server"></asp:TextBox>
        
      </td>
        </tr>
        <tr><td align="right" style="padding-right: 5px"><b>Beginning Vendor#:</b></td>
          <td nowrap><asp:TextBox ID="begvendTextBox" Width="100px" runat="server"></asp:TextBox>
          <a href="#" tabindex="1" onclick="vendorlook(1); return false"><asp:Image ID="Image3" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
          </td>
        <td align="right" style="padding-right: 5px"><b>Ending Vendor#:</b></td>
          <td nowrap><asp:TextBox ID="endvendTextBox"  Width="100px" runat="server"></asp:TextBox>
          <a href="#" tabindex="1" onclick="vendorlook(2); return false"><asp:Image ID="Image1" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
          </td>
          </tr>
          <tr><td align="right" style="padding-right:5px"><b>Beginning Currency:</b></td>
          <td>
              <asp:TextBox ID="begcurrTextBox" Width="100px" runat="server"></asp:TextBox>
              </td>
          <td align="right" style="padding-right:5px"><b>Ending Currency:</b></td>
          <td>
              <asp:TextBox ID="endcurrTextBox" Width="100px" runat="server"></asp:TextBox></td></tr>
        <tr><td align="right" style="padding-right: 5px"><b>Beginning Vendor Type:</b></td>
          <td nowrap><asp:TextBox ID="begtypeTextBox" Width="100px" runat="server"></asp:TextBox>
          <a href="#" tabindex="1" onClick="vendortypelook(1); return false"><asp:Image ID="Image5" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
          </td>
        <td align="right" style="padding-right: 5px"><b>Ending Vendor Type:</b></td>
          <td nowrap><asp:TextBox ID="endtypeTextBox" Width="100px" runat="server"></asp:TextBox>
          <a href="#" tabindex="1" onClick="vendortypelook(2); return false"><asp:Image ID="Image6" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
          </td>
          </tr>
       
          
              <tr> <td align="right" style="padding-right: 5px"><b>As of:</b></td>
              <td><asp:TextBox ID="asofTextBox" Width="100px" onfocus="javascript:preEnter( this, 'yes' );" onblur="javascript:preLeave( this, 'date', '99/99/9999' );" runat="server"></asp:TextBox>
              <a href="#" onblur="document.getElementById('asofTextBox').focus()" tabindex="1" onClick="showCalendarControl(asofTextBox); return false"><asp:Image ID="Image2" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
       
        </td>
        <td align="right" style="padding-right: 5px"><b>Which Date?:</b></td>
        <td><b><asp:RadioButtonList ID="RadioButtonList1" RepeatLayout="Flow"  CellSpacing="1" RepeatColumns="5" Font-Bold ="true" runat="server">
             
                 
                 <asp:ListItem  Value="Invoice"    Text="Invoice" />
                  <asp:ListItem Value="Posting"    Text="Posting" />
                 
         </asp:RadioButtonList></b>
        
        </td></tr>
       <tr> <td align="right" style="padding-right: 5px"><b>period Days1:</b></td>
              <td colspan="3"><asp:TextBox ID="day1TextBox" Width="40px" runat="server"></asp:TextBox>
              <asp:CompareValidator ID="CompareValidator1" runat="server" ControlToValidate="day1TextBox" Display="Dynamic" Operator="DataTypeCheck" Type="Integer" ErrorMessage="Only Integer Value"></asp:CompareValidator>
              <b>2:</b><asp:TextBox ID="day2TextBox" Width="40px" runat="server"></asp:TextBox>
              <asp:CompareValidator ID="CompareValidator2" runat="server" ControlToValidate="day2TextBox" Display="Dynamic" Operator="DataTypeCheck" Type="Integer" ErrorMessage="Only Integer Value"></asp:CompareValidator>
              <b>2:</b><asp:TextBox ID="day3TextBox" Width="40px" runat="server"></asp:TextBox>
              <asp:CompareValidator ID="CompareValidator3" runat="server" ControlToValidate="day3TextBox" Display="Dynamic" Operator="DataTypeCheck" Type="Integer" ErrorMessage="Only Integer Value"></asp:CompareValidator>
              <b>2:</b><asp:TextBox ID="day4TextBox" Width="40px" runat="server"></asp:TextBox>
              <asp:CompareValidator ID="CompareValidator4" runat="server" ControlToValidate="day4TextBox" Display="Dynamic" Operator="DataTypeCheck" Type="Integer" ErrorMessage="Only Integer Value"></asp:CompareValidator>
        </td>
        </tr>
        <tr><td></td><td colspan="2"><b>Sort By?:</b>
        <b><asp:RadioButtonList ID="RadioButtonList2" RepeatLayout="Flow"  CellSpacing="1" RepeatColumns="5" Font-Bold ="true" runat="server">
             
                 
                 <asp:ListItem   Value="Code"   Text="Vendor Code" />
                  <asp:ListItem  Value="Name"   Text="Vendor Name" />
                 
         </asp:RadioButtonList></b>
        </td></tr>
        <tr><td></td><td><b><asp:CheckBox ID="CheckBox1" Text="Detailed?" runat="server"></asp:CheckBox></b></td></tr>
        <tr><td colspan="2"><b> &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; 
        &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; <b>OutPut To?:</b> <asp:RadioButtonList ID="RadioButtonList3" RepeatLayout="Flow"  CellSpacing="1" RepeatColumns="5" Font-Bold ="true" runat="server">
             
                 
                 <asp:ListItem   Value="no"   Text="Text File" />
                  <asp:ListItem  Value="yes"   Text="Excel" />
                 
         </asp:RadioButtonList></b></td></tr>
         <tr><td colspan="2">
             &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; <asp:Button ID="submitbutton" OnClick="submitbutton_click" runat="server" class="buttonM" Text="Submit" />
             &nbsp;&nbsp;&nbsp;&nbsp;
          <asp:Label ID="OutPutFile" runat="server" Text="Print:" Font-Bold="True" Font-Size="Larger" ForeColor="Blue"></asp:Label>
              <asp:HyperLink ID="HyperLink1" runat="server" Target="_blank" Font-Bold="True" Font-Size="Larger" ForeColor="Red"></asp:HyperLink>              
             </td>
          </tr>   
        
        </table>
          
        
        
         
           
          <asp:FormView ID="FormView1" Visible="False"   runat="server" 
              DataSourceID="ObjectDataSource1">
             
             
             
              
              <ItemTemplate>
                  vendoraging:
                  <asp:Label ID="vendoragingLabel" runat="server" 
                      Text='<%# Bind("vendoraging") %>'></asp:Label><br />
                  a:
                  <asp:Label ID="aLabel" runat="server" Text='<%# Bind("a") %>'></asp:Label><br />
              </ItemTemplate>
          </asp:FormView>
          <asp:ObjectDataSource ID="ObjectDataSource1" runat="server" OldValuesParameterFormatString="original_{0}"
               SelectMethod="RepVendorAging" TypeName="voucherpay">
              <SelectParameters>
                  <asp:Parameter Name="prmUser" Type="String" />
                  <asp:Parameter Name="prmAction" Type="String" />
                   <asp:Parameter Name="prmOut" Type="String" />
                  <asp:Parameter Name="prmBeginCompany" Type="String" />
                  <asp:Parameter Name="prmEndCompany" Type="String" />
                  <asp:Parameter Name="prmBeginVendor" Type="String" />
                  <asp:Parameter Name="prmEndVendor" Type="String" />
                  <asp:Parameter Name="prmBeginCurr" Type="String" />
                  <asp:Parameter Name="prmEndCurr" Type="String" />
                  <asp:Parameter Name="prmBeginType" Type="String" />
                  <asp:Parameter Name="prmEndType" Type="String" />
                  <asp:Parameter Name="prmAsof" Type="String" />
                  <asp:Parameter Name="prmInvPost" Type="String" />
                  <asp:Parameter Name="prmDay1" Type="String" />
                  <asp:Parameter Name="prmDay2" Type="String" />
                  <asp:Parameter Name="prmDay3" Type="String" />
                  <asp:Parameter Name="prmDay4" Type="String" />
                  <asp:Parameter Name="prmSort" Type="String" />
                  <asp:Parameter Name="prmDetail" Type="String" />
              </SelectParameters>
          </asp:ObjectDataSource>
          
     
    </div>
    <ft:footer id="Footer1" runat="server"></ft:footer>
    </form>
  </body>
</HTML>

