<%@ Page Language="c#" AutoEventWireup="true" Debug="false" Inherits="apchk_reg" Codebehind="~/apchk_reg.aspx.cs" %>
<%@ Register Src="footer.ascx" TagName="Footer" TagPrefix="ft" %>
<%@ Register Src="header.ascx" TagName="Header" TagPrefix="hd" %>
<html xmlns="http://www.w3.org/1999/xhtml" >
  <head id="Head1" runat="server">
    <title>AP Check Register</title>  
         
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

function Datelook1()
{
  document.forms[0].bedateTextBox.value="";
  Datelook();
}
function Date2look(){ 
  var NewWindow = window.open("date_lookup2.aspx","DateLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
  }
function Datelookup2(obj)
{
  document.forms[0].enddateTextBox.value=obj;
}

function Datelook2()
{
  document.forms[0].enddateTextBox.value="";
  Date2look();
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
var bnk;
function banklookup(obj6) {
    bnk = obj6;
    var NewWindow = window.open("bank_lookup.aspx", "banklookup", "width=500,height=400,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}
function banklook(ReturnObj1) {
    if (bnk == "1") {
        document.forms[0].begbankTextBox.value = ReturnObj1;
    }
    else {
        document.forms[0].endbankTextBox.value = ReturnObj1;
    }
}
    </script> 
  </head>    
   <body>
    <form id="frmList" runat="server"  defaultfocus='begdateTextBox'>   
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
            
          <TD align=center nowrap><font size=+0><b>AP Check Register &nbsp;</b></font></TD>
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
       
      <tr><td align="right" style="padding-right: 5px"><b>Beginning Check Date:</b></td><td>
          <asp:TextBox ID="begdateTextBox"  width="100px" onfocus="javascript:preEnter( this, 'yes' );" onblur="javascript:preLeave( this, 'date', '99/99/9999' );" runat="server"></asp:TextBox>
          <a href="#" onblur="document.getElementById('begdateTextBox').focus()" tabindex="1" onClick="showCalendarControl(begdateTextBox); return false"><asp:Image ID="Image2" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
          </td>
      <td align="right" style="padding-right: 5px"><b>Ending Check Date:</b></td>
      <td nowrap>
        <asp:TextBox ID="enddateTextBox" width="100px" onfocus="javascript:preEnter( this, 'yes' );" onblur="javascript:preLeave( this, 'date', '99/99/9999' );" runat="server"></asp:TextBox>
        <a href="#" onblur="document.getElementById('enddateTextBox').focus()" tabindex="1" onClick="showCalendarControl(enddateTextBox); return false"><asp:Image ID="Image4" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
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
          <tr><td align="right" style="padding-right:5px"><b>Beginning Check#:</b></td>
          <td>
              <asp:TextBox ID="begchkTextBox" Width="100px" runat="server"></asp:TextBox>
              <asp:CompareValidator ID="CompareValidator1" runat="server" ControlToValidate="begchkTextBox" Display="Dynamic" Operator="DataTypeCheck" Type="Integer" ErrorMessage="Only Integer Value"></asp:CompareValidator>
              </td>
          <td align="right" style="padding-right:5px"><b>Ending Check#:</b></td>
          <td>
              <asp:TextBox ID="endchkTextBox" Width="100px" runat="server"></asp:TextBox>
              <asp:CompareValidator ID="CompareValidator2" runat="server" ControlToValidate="endchkTextBox" Display="Dynamic" Operator="DataTypeCheck" Type="Integer" ErrorMessage="Only Integer Value"></asp:CompareValidator></td></tr>
        <tr><td align="right" style="padding-right: 5px"><b>Beginning Bank:</b></td>
          <td nowrap><asp:TextBox ID="begbankTextBox" Width="100px" runat="server"></asp:TextBox>
          <a href="#" tabindex="1" onclick="banklookup(1); return false"><asp:Image ID="banklookimg" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
          </td>
        <td align="right" style="padding-right: 5px"><b>Ending Bank:</b></td>
          <td nowrap><asp:TextBox ID="endbankTextBox" Width="100px" runat="server"></asp:TextBox>
          <a href="#" tabindex="1" onclick="banklookup(2); return false"><asp:Image ID="Image5" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
          </td>
          </tr>
         <tr><td></td><td><b><asp:CheckBox ID="CheckBox1" Text="Print Invoice GL Account Detail?" runat="server"></asp:CheckBox></b></td></tr>
         <tr><td></td><td><b><asp:CheckBox ID="CheckBox2" Text="Run By Post Date?" runat="server"></asp:CheckBox></b></td></tr>
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
                  apchk:
                  <asp:Label ID="apchkLabel" runat="server" 
                      Text='<%# Bind("apchk") %>'></asp:Label><br />
                  abc:
                  <asp:Label ID="abcLabel" runat="server" Text='<%# Bind("abc") %>'></asp:Label><br />
              </ItemTemplate>
          </asp:FormView>
          <asp:ObjectDataSource ID="ObjectDataSource1" runat="server" OldValuesParameterFormatString="original_{0}"
               SelectMethod="APCheckRegister" TypeName="voucherpay">
              <SelectParameters>
                  <asp:Parameter Name="prmUser" Type="String" />
                  <asp:Parameter Name="prmapchk" Type="String" />
                  <asp:Parameter Name="prmbegvend" Type="String" />
                  <asp:Parameter Name="prmbegdt" Type="String" />
                  <asp:Parameter Name="prmbegchk" Type="Int32" />
                  <asp:Parameter Name="prmbegbnk" Type="String" />
                  <asp:Parameter Name="prmendvend" Type="String" />
                  <asp:Parameter Name="prmenddt" Type="String" />
                  <asp:Parameter Name="prmendchk" Type="Int32" />
                  <asp:Parameter Name="prmendbnk" Type="String" />
                  <asp:Parameter Name="prmglact" Type="String" />
                  <asp:Parameter Name="prmrunpst" Type="String" />
                   <asp:Parameter Name="prmOut" Type="String" />
              </SelectParameters>
          </asp:ObjectDataSource>
          
     
    </div>
    <ft:footer id="Footer1" runat="server"></ft:footer>
    </form>
  </body>
</HTML>

