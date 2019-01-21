<%@ Page Language="c#" AutoEventWireup="true" Debug="false" Inherits="open_order_report" Codebehind="open_order_report.aspx.cs" %>
<%@ Register Src="footer.ascx" TagName="Footer" TagPrefix="ft" %>
<%@ Register Src="header.ascx" TagName="Header" TagPrefix="hd" %>
<html xmlns="http://www.w3.org/1999/xhtml" >
  <head id="Head1" runat="server">
    <title>Open Order Report</title>
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
  
}
function contactcustomerlook2(){ 
  var NewWindow = window.open("contact_customer_copylookup.aspx","ContactCustomerLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}

function ContactCustomerCopyLookup(ReturnObj1,ReturnObj2,ReturnObj3,ReturnObj4,ReturnObj5,ReturnObj6,ReturnObj7,ReturnObj8,ReturnObj9, ReturnObj10,ReturnObj11){ 
  
  document.forms[0].TextBox2.value = ReturnObj1;
  }

  function Relook() {
      var item1 = document.getElementById("TextBox1").value;
  var NewWindow = window.open("reorder_item_lookup.aspx?item="+ item1 +"","ReItemLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}

function ReLookup(ReturnObj1){ 
  document.forms[0].TextBox3.value = ReturnObj1;
}
function Relook2() {
    var item2 = document.getElementById("TextBox2").value;
  var NewWindow = window.open("reorder_item_lookup2.aspx?item1="+ item2 +"","ReItemLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}

function ReLookup2(ReturnObj1){ 
  document.forms[0].TextBox4.value = ReturnObj1;
}
function Datelook(){ 
  var NewWindow = window.open("date_lookup.aspx","DateLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
  }
function Datelookup(obj)
{
  document.forms[0].BeginorderTextBox.value=obj;
}

function Datelook1()
{
  document.forms[0].BeginorderTextBox.value="";
  Datelook();
}
function Date2look(){ 
  var NewWindow = window.open("date_lookup2.aspx","DateLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
  }
function Datelookup2(obj)
{
  document.forms[0].endingorderTextBox.value=obj;
}

function Datelook2()
{
  document.forms[0].endingorderTextBox.value="";
  Date2look();
}
function Date3look(){ 
  var NewWindow = window.open("date3_lookup.aspx","DateLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
  }
function Datelookup3(obj)
{
  document.forms[0].beDueTextBox.value=obj;
}

function Datelook3()
{
  document.forms[0].beDueTextBox.value="";
  Date3look();
}
function Date4look(){ 
  var NewWindow = window.open("date4_lookup.aspx","DateLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
  }
function Datelookup4(obj)
{
  document.forms[0].enddueTextBox.value=obj;
}

function Datelook4()
{
  document.forms[0].enddueTextBox.value="";
  Date4look();
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
  function job1look(){ 
  var NewWindow = window.open("job1_lookup.aspx","JobLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}
function Job1Lookup(ReturnObj1){ 
  document.forms[0].bejobTextBox.value = ReturnObj1;
} 
 function jobReplook(){ 
  var NewWindow = window.open("jobRep_lookup.aspx","JobLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}
function JobRepLookup(ReturnObj1){ 
  document.forms[0].endjobTextBox.value = ReturnObj1;
}

 function custpolook(){ 
  var NewWindow = window.open("cust_po_lookup.aspx","CustPoLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}
function CustPoLookup(ReturnObj1){ 
  document.forms[0].becustpoTextBox.value = ReturnObj1;
}  
function custpo2look(){ 
  var NewWindow = window.open("cust_po_lookup2.aspx","CustPoLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}
function CustPoLookup2(ReturnObj1){ 
  document.forms[0].endcustpoTextBox.value = ReturnObj1;
}  
function userlook(){ 
  var NewWindow = window.open("user_lookup.aspx","UserLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}
function UserLookup(ReturnObj1){ 
  document.forms[0].beuserTextBox.value = ReturnObj1;
}  
function user2look(){ 
  var NewWindow = window.open("user2_lookup.aspx","UserLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}
function User2Lookup(ReturnObj1){ 
  document.forms[0].enduserTextBox.value = ReturnObj1;
}  
function datevalidate()
{
    var date=document.getElementById("BeginorderTextBox").value;
    
    if(date.length>1 && date.length<3 && date.indexOf('/')!=1)
    {
        document.getElementById("BeginorderTextBox").value = date + "/";
    }
    if(date.length>4 && date.length<6 && date.indexOf('/')!=3)
    {
        document.getElementById("BeginorderTextBox").value = date + "/";
    }
    var date2=document.getElementById("endingorderTextBox").value;
    
    if(date2.length>1 && date2.length<3 && date2.indexOf('/')!=1)
    {
        document.getElementById("endingorderTextBox").value = date2 + "/";
    }
    if(date2.length>4 && date2.length<6 && date2.indexOf('/')!=3)
    {
        document.getElementById("endingorderTextBox").value = date2 + "/";
    }
    
    var date3=document.getElementById("beDueTextBox").value;
    
    if(date3.length>1 && date3.length<3 && date3.indexOf('/')!=1)
    {
        document.getElementById("beDueTextBox").value = date3 + "/";
    }
    if(date3.length>4 && date3.length<6 && date3.indexOf('/')!=3)
    {
        document.getElementById("beDueTextBox").value = date3 + "/";
    }
    var date4=document.getElementById("enddueTextBox").value;
    
    if(date4.length>1 && date4.length<3 && date4.indexOf('/')!=1)
    {
        document.getElementById("enddueTextBox").value = date4 + "/";
    }
    if(date4.length>4 && date4.length<6 && date4.indexOf('/')!=3)
    {
        document.getElementById("enddueTextBox").value = date4 + "/";
    }
}
    </script> 
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
          
          <asp:Label ID="Label1" ForeColor="red" Font-Bold="true" runat="server" ></asp:Label>
          
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
            
          <TD align=center nowrap><font size=+0><b>Open Order Report&nbsp;</b></font></TD>
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
      <table class="shade" width="680px">
     
      <tr><td align="right" style="padding-right: 5px"><b>Begining Customer#:</b></td><td>
          <asp:TextBox ID="TextBox1" OnTextChanged="TextBox1_textchanged" onkeyup="samevalue()"  width="100px" runat="server"></asp:TextBox>
          <a href="#" tabindex="1" onClick="contactcustomerlook(); return false"><asp:Image ID="CustomerLook" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
          </td>
      <td align="right" style="padding-right: 5px"><b>Ending Customer#:</b></td>
      <td>
            <asp:TextBox ID="TextBox2" width="100px" runat="server"></asp:TextBox>
            <a href="#" tabindex="1" onClick="contactcustomerlook2(); return false"><asp:Image ID="Image12" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
      </td>
        </tr>
        <tr> <td align="right" style="padding-right: 5px"><b>Beginning Order Date:</b></td>
        <td>
            <asp:TextBox ID="BeginorderTextBox" onfocus="javascript:preEnter( this, 'yes' );" onblur="javascript:preLeave( this, 'date', '99/99/9999' );" MaxLength="12" Width="100px" runat="server"></asp:TextBox>
            <a href="#" tabindex="1" onclick="showCalendarControl(BeginorderTextBox); return false"><asp:Image ID="datelook" runat="server" ImageUrl="images/lookup_icon.gif" /> </a>
            
            </td>
        <td align="right" style="padding-right: 5px"><b>Ending Order Date:</b></td>
        <td><asp:TextBox ID="endingorderTextBox" onfocus="javascript:preEnter( this, 'yes' );" onblur="javascript:preLeave( this, 'date', '99/99/9999' );" MaxLength="12" Width="100px" runat="server"></asp:TextBox>
        <a href="#" tabindex="1" onclick="showCalendarControl(endingorderTextBox); return false"><asp:Image ID="Image2" runat="server" ImageUrl="images/lookup_icon.gif" /> </a>
        
        </td></tr>
        <tr> <td align="right" style="padding-right: 5px"><b>Beginning Customer Po#:</b></td><td><asp:TextBox ID="becustpoTextBox" Width="100px" runat="server"></asp:TextBox>
        <a href="#" tabindex="1" onclick="custpolook(); return false"><asp:Image ID="Image8" runat="server" ImageUrl="images/lookup_icon.gif" /> </a></td>
        <td align="right" style="padding-right: 5px"><b>Ending Customer Po#:</b></td><td><asp:TextBox ID="endcustpoTextBox" Width="100px" runat="server"></asp:TextBox>
        <a href="#" tabindex="1" onclick="custpo2look(); return false"><asp:Image ID="Image9" runat="server" ImageUrl="images/lookup_icon.gif" /> </a>
        </td></tr>
        <tr> <td align="right" style="padding-right: 5px"><b>Beginning Job#:</b></td><td><asp:TextBox ID="bejobTextBox" Width="60px" runat="server"></asp:TextBox>
        <a href="#" tabindex="1" onclick="job1look(); return false"><asp:Image ID="Image6" runat="server" ImageUrl="images/lookup_icon.gif" /> </a>
        <asp:TextBox ID="bejob2TextBox" Width="40px" runat="server"></asp:TextBox></td>
        <td align="right" style="padding-right: 5px"><b>Ending Job#: </b></td><td><asp:TextBox ID="endjobTextBox" Width="60px" runat="server"></asp:TextBox>
        <a href="#" tabindex="1" onclick="jobReplook(); return false"><asp:Image ID="Image7" runat="server" ImageUrl="images/lookup_icon.gif" /> </a>
        <asp:TextBox ID="endjob2TextBox" onblur="job2()" Width="40px" runat="server"></asp:TextBox></td></tr>
         <tr><td align="right" style="padding-right: 5px"><b>Beginning Item#:</b></td>
          <td nowrap><asp:TextBox ID="TextBox3" Width="100px" runat="server"></asp:TextBox>
          <a href="#" tabindex="1" onClick="Relook(); return false"><asp:Image ID="FGLookup" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
          </td>
        <td align="right" style="padding-right: 5px"><b>Ending Item#:</b></td>
          <td nowrap><asp:TextBox ID="TextBox4" Width="100px" runat="server"></asp:TextBox>
          <a href="#" tabindex="1" onClick="Relook2(); return false"><asp:Image ID="Image1" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
          </td>
          </tr>
        <tr> <td align="right" style="padding-right: 5px"><b>Beginning CAD#:</b></td><td><asp:TextBox ID="BeCADTextBox" runat="server"></asp:TextBox></td>
        <td align="right" style="padding-right: 5px"><b>Ending CAD#:</b></td><td><asp:TextBox ID="endCADTextBox" runat="server"></asp:TextBox></td></tr>
        <tr> <td align="right" style="padding-right: 5px"><b>Beginning Due Date:</b></td>
        <td><asp:TextBox ID="beDueTextBox" onfocus="javascript:preEnter( this, 'yes' );" onblur="javascript:preLeave( this, 'date', '99/99/9999' );" MaxLength="12" Width="100px" runat="server"></asp:TextBox>
        <a href="#" tabindex="1" onblur="document.getElementById('beDueTextBox').focus()" onclick="showCalendarControl(beDueTextBox); return false"><asp:Image ID="dueimage" runat="server" ImageUrl="images/lookup_icon.gif" /> </a>
            
        </td>
        <td align="right" style="padding-right: 5px"><b>Ending Due Date:</b></td>
        <td><asp:TextBox ID="enddueTextBox" onfocus="javascript:preEnter( this, 'yes' );" onblur="javascript:preLeave( this, 'date', '99/99/9999' );" MaxLength="12" Width="100px" runat="server"></asp:TextBox>
        <a href="#" tabindex="1" onblur="document.getElementById('enddueTextBox').focus()" onclick="showCalendarControl(enddueTextBox); return false"><asp:Image ID="Image3" runat="server" ImageUrl="images/lookup_icon.gif" /> </a>
            
        </td></tr>
        <tr> <td align="right" style="padding-right: 5px"><b>Beginning User Id:</b></td><td><asp:TextBox ID="beuserTextBox" Width="100px" runat="server"></asp:TextBox>
        <a href="#" tabindex="1" onclick="userlook(); return false"><asp:Image ID="Image10" runat="server" ImageUrl="images/lookup_icon.gif" /> </a>
        </td>
        <td align="right" style="padding-right: 5px"><b>Ending User Id:</b></td><td><asp:TextBox Width="100px" ID="enduserTextBox" runat="server"></asp:TextBox>
        <a href="#" tabindex="1" onclick="user2look(); return false"><asp:Image ID="Image11" runat="server" ImageUrl="images/lookup_icon.gif" /> </a>
        </td></tr>
        <tr> <td align="right" style="padding-right: 5px"><b>Beginning Sales Rep:</b></td><td><asp:TextBox ID="besmanTextBox" Width="100px" runat="server"></asp:TextBox>
        <a href="#" tabindex="1" onClick="salesreplook(); return false"><asp:Image ID="Image4" runat="server" ImageUrl="images/lookup_icon.gif" /></a></td>
        <td align="right" style="padding-right: 5px"><b>Ending Sales Rep:</b></td><td><asp:TextBox ID="endsmanTextBox" Width="100px" runat="server"></asp:TextBox>
        <a href="#" tabindex="1" onClick="smancopylook1(); return false"><asp:Image ID="Image5" runat="server" ImageUrl="images/lookup_icon.gif" /></a></td></tr>
        
        </table><fieldset style="background-color:#EFF3FB;width:680px;">
        <legend>Sort</legend>
        <table class="shade" width="680px">
        <tr><td align="right" style="padding-right: 5px"><b>Sort By?: </b></td>
        <td><asp:RadioButtonList ID="RadioButtonList2" RepeatLayout="Flow"  CellSpacing="1" RepeatColumns="5" Font-Bold ="true" runat="server">
             
                 <asp:ListItem      Text="Customer#" />
                  <asp:ListItem     Text="Due Date" />
                 <asp:ListItem      Text="Sales Rep" />                  
         </asp:RadioButtonList> </td></tr>
         <tr><td align="right" style="padding-right: 5px"><b>Secondary Sort:</b></td>
         <td><asp:RadioButtonList ID="RadioButtonList3" RepeatLayout="Flow"  CellSpacing="1" RepeatColumns="7" Font-Bold ="true" runat="server">
                 <asp:ListItem      Text="Po#" />
                 <asp:ListItem      Text="Item" />
                  <asp:ListItem     Text="Cust Part#" />
                 <asp:ListItem      Text="FG Item Name" />
                 <asp:ListItem      Text="Order#" />
                  <asp:ListItem     Text="Due Date" />
                 <asp:ListItem      Text="CAD#" />                     
         </asp:RadioButtonList></td></tr>
        </table></fieldset>
        <fieldset style="background-color:#EFF3FB;width:680px;">
        <legend>Print Selection</legend>
          <table class="shade" width="680px">
          <tr>
          <td align="right" style="padding-right: 5px"><b>Job Status?:</b></td>
          <td><b> <asp:RadioButtonList ID="RadioButtonList4" RepeatLayout="Flow"  CellSpacing="1" RepeatColumns="5" Font-Bold ="true" runat="server">
             
                 
                 <asp:ListItem      Text="Open" />
                  <asp:ListItem     Text="Close" />
                 <asp:ListItem      Text="All" />
                 </asp:RadioButtonList></b></td>             
         <td><b> <asp:CheckBox ID="CheckBox2" Text="Print Job Qty Details" runat="server" /></b></td>
         <td><b><asp:CheckBox ID="CheckBox3" Text="Include 0 Order Balance Items?" runat="server" /></b></td>
          </tr>
          <tr><td align="right" style="padding-right: 5px"><b>Order Status?:</b></td>
          <td><b> <asp:RadioButtonList ID="RadioButtonList5" RepeatLayout="Flow"  CellSpacing="1" RepeatColumns="5" Font-Bold ="true" runat="server">
             
                 
                 <asp:ListItem      Text="Open" />
                  <asp:ListItem     Text="Close" />
                 <asp:ListItem      Text="All" />
                 </asp:RadioButtonList></b></td>
          <td> <b><asp:CheckBox ID="CheckBox4" Text="Drop Order Underrun%" runat="server" /></b></td>
          <td> <b><asp:CheckBox ID="CheckBox1" Text="Include 0 Qty Wip Items" runat="server" /></b></td>
         
          </tr>
          
           <tr><td align="right" style="padding-right: 5px"><b>Due Date?:</b></td>
           <td colspan="2"><b> <asp:RadioButtonList ID="RadioButtonList6" RepeatLayout="Flow"  CellSpacing="1" RepeatColumns="5" Font-Bold ="true" runat="server">
                  <asp:ListItem      Text="Line" />
                  <asp:ListItem     Text="Release" />
                 </asp:RadioButtonList></b></td>
           <td><b> <asp:CheckBox ID="CheckBox5" Text="Include 0 Qty/Act Release Qty=0" runat="server" /></b></td></tr>
        <tr><td align="right" style="padding-right: 5px"><b>Wip Qty?:</b></td>
        <td colspan="2"><b><asp:RadioButtonList ID="RadioButtonList7" RepeatLayout="Flow"  CellSpacing="1" RepeatColumns="5" Font-Bold ="true" runat="server">
                  <asp:ListItem      Text="Order Qty-Oh-Ship" />
                  <asp:ListItem     Text="Job Qty-Rcpt" />
                 </asp:RadioButtonList></b></td>
           <td><b><asp:CheckBox ID="CheckBox6" Text="Include Jobs W/Q Oh?" runat="server" /></b></td></tr>
           <tr><td align="right" style="padding-right: 5px">
           <b>Output To:</b></td><td><b><asp:RadioButtonList ID="RadioButtonList8" RepeatLayout="Flow"  CellSpacing="1" RepeatColumns="5" Font-Bold ="true" runat="server">
                  <asp:ListItem      Text="Excel" />
                  
                 </asp:RadioButtonList></b>
           </td></tr>
           <tr><td colspan="3">
              <asp:Button ID="submitbutton" OnClick="submitbutton_click" runat="server" class="buttonM" Text="Submit" />
              &nbsp;&nbsp;&nbsp;&nbsp;
              <asp:Label ID="OutputLabel" runat="server" Text="Print:" Font-Bold="True" Font-Size="Larger" ForeColor="Blue"></asp:Label>
              &nbsp;
              <asp:HyperLink ID="HyperLink1" runat="server"  Target="_blank" Font-Bold="True" Font-Size="Larger" ForeColor="Red"></asp:HyperLink>
              </td>
          </tr>
           </table></fieldset>
                   
          <asp:FormView ID="FormView1" Visible="false" runat="server" DataSourceID="ObjectDataSource1">
              
              <ItemTemplate>
                  vExFile:
                  <asp:Label ID="vExFileLabel" runat="server" Text='<%# Bind("vExFile") %>'></asp:Label><br />
                  vFile:
                  <asp:Label ID="vFileLabel" runat="server" Text='<%# Bind("vFile") %>'></asp:Label><br />
              </ItemTemplate>
          </asp:FormView>
          <asp:ObjectDataSource ID="ObjectDataSource1" runat="server" OldValuesParameterFormatString="original_{0}"
              SelectMethod="ReOpenOrder" TypeName="reports">
              <SelectParameters>
                  <asp:Parameter Name="prmUser" Type="String" />
                  <asp:Parameter Name="prmBeginCust" Type="String" />
                  <asp:Parameter Name="prmEndCust" Type="String" />
                  <asp:Parameter Name="prmBegOrdate" Type="DateTime" />
                  <asp:Parameter Name="prmEndOrdate" Type="DateTime" />
                  <asp:Parameter Name="prmBegPo" Type="String" />
                  <asp:Parameter Name="prmEndPo" Type="String" />
                  <asp:Parameter Name="prmBegJob" Type="String" />
                  <asp:Parameter Name="prmEndJob" Type="String" />
                  <asp:Parameter Name="prmBegJob2" Type="String" />
                  <asp:Parameter Name="prmEndJob2" Type="String" />
                  <asp:Parameter Name="prmBegItem" Type="String" />
                  <asp:Parameter Name="prmEndItem" Type="String" />
                  <asp:Parameter Name="prmBegCad" Type="String" />
                  <asp:Parameter Name="prmEndCad" Type="String" />
                  <asp:Parameter Name="prmBegDue" Type="DateTime" />
                  <asp:Parameter Name="prmEndDue" Type="DateTime" />
                  <asp:Parameter Name="prmBegUser" Type="String" />
                  <asp:Parameter Name="prmEndUser" Type="String" />
                  <asp:Parameter Name="prmBegSman" Type="String" />
                  <asp:Parameter Name="prmEndSman" Type="String" />
                  <asp:Parameter Name="prmSortcust" Type="String" />
                  <asp:Parameter Name="prmSecondary" Type="String" />
                  <asp:Parameter Name="prmJobStat" Type="String" />
                  <asp:Parameter Name="prmOrdStat" Type="String" />
                  <asp:Parameter Name="prmDuedate" Type="String" />
                  <asp:Parameter Name="prmWipqty" Type="String" />
                  <asp:Parameter Name="prmPrintjob" Type="String" />
                  <asp:Parameter Name="prmDroporder" Type="String" />
                  <asp:Parameter Name="prmInorder" Type="String" />
                  <asp:Parameter Name="prmInqty" Type="String" />
                  <asp:Parameter Name="prmInact" Type="String" />
                  <asp:Parameter Name="prminjob" Type="String" />
                  <asp:Parameter Name="prmOutexcel" Type="String" />
              </SelectParameters>
          </asp:ObjectDataSource>
     
    </div>
    <ft:footer id="Footer1" runat="server"></ft:footer>
    </form>
  </body>
</HTML>

