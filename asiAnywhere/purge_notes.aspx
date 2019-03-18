<%@ Page Language="c#" AutoEventWireup="true" Debug="false" Inherits="purge_notes_list" Codebehind="purge_notes.aspx.cs" %>
<%@ Register Src="footer.ascx" TagName="Footer" TagPrefix="ft" %>
<%@ Register Src="header.ascx" TagName="Header" TagPrefix="hd" %>
<html xmlns="http://www.w3.org/1999/xhtml" >
  <head id="Head1" runat="server">
    <title>purge Notes</title>
    <LINK href="include/style.css" type="text/css" rel="stylesheet"/>
   <LINK REL="stylesheet" TYPE="text/css" HREF="include/CalendarControl.css" >
    <script language="javascript" src="include/date.js"></script>
<script language="javascript" src="include/event.js"></script>
<script language="javascript" src="include/insert.js"></script>
    <script language = "JavaScript" src="include/CalendarControl.js">
    
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
    
    
   

    </script>
    <script>
    function contactcustomerlook(){ 
  var NewWindow = window.open("contact_customer_lookup.aspx","ContactCustomerLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");

}

function ContactCustomerLookup(ReturnObj1){ 
  document.forms[0].FromcustomerTextBox.value = ReturnObj1;
  } 
  
  function contactcustomerlook2(){ 
  var NewWindow = window.open("contact_customer_lookup2.aspx","ContactCustomerLookupWindow2","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}

function ContactCustomerLookup2(ReturnObj1){ 
  document.forms[0].TocustomerTextBox.value = ReturnObj1;
  } 
  
  function Datelook(){ 
  var NewWindow = window.open("date_lookup.aspx","DateLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
  }
function Datelookup(obj)
{
  document.forms[0].DateTextBox.value=obj;
}

function Datelook2(){ 
  var NewWindow = window.open("date_lookup2.aspx","DateLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
  }
function Datelookup2(obj)
{
  document.forms[0].ToDateTextBox.value=obj;
}
function Datelook1()
{
  document.forms[0].DateTextBox.value="";
  Datelook();
}
function Datelook3()
{
  document.forms[0].ToDateTextBox.value="";
  Datelook2();
}
  function datevalidate()
  {
    var date1=document.getElementById("DateTextBox").value;
    var date2=document.getElementById("ToDateTextBox").value;
    
    if(date1.length>1 && date1.length<3 && date1.indexOf('/')!=1)
    {
        document.getElementById("DateTextBox").value = date1 + "/";
    }
    if(date1.length>4 && date1.length<6 && date1.indexOf('/')!=3)
    {
        document.getElementById("DateTextBox").value = date1 + "/";
    } 
    
    if(date2.length>1 && date2.length<3 && date2.indexOf('/')!=1)
    {
        document.getElementById("ToDateTextBox").value = date2 + "/";
    }
    if(date2.length>4 && date2.length<6 && date2.indexOf('/')!=3)
    {
        document.getElementById("ToDateTextBox").value = date2 + "/";
    }   
  }
    </script> 
  </head>    
   <body>
    <form id="frmList" runat="server"  defaultfocus='ContactTextBox'>   
        <hd:header id="Header1" runat="server"></hd:header>
      <div>
            
      <TABLE id="tblTop" cellSpacing="3"  border="0" >
        <TR>
          
          <TD  nowrap><font size=+0><b>Purge Notes&nbsp;</b></font></TD>
          <TD vAlign="middle">
            <asp:linkbutton id="hlkBackToMenu" runat="server" OnClick="hlkBackToMenu_Click">Back to menu</asp:linkbutton>
          </TD> 
          <TD vAlign="middle" align="center"><b>Users</b>&nbsp;Logged as&nbsp;
            <asp:label id="lblUser" runat="server" Font-Bold="True">&nbsp;</asp:label>&nbsp;&nbsp;
            <asp:linkbutton id="hlnkLogOut" runat="server" OnClick="hlnkLogOut_Click">Log out</asp:linkbutton>
            &nbsp;&nbsp;<asp:hyperlink id="hlnkChangePwd" runat="server" NavigateUrl="changepwd.aspx">Change password</asp:hyperlink>
          &nbsp;<b>Company:</b> &nbsp;<asp:label id="lblComp" runat="server" Font-Bold="True">&nbsp;</asp:label></TD>
          
         
          <TD vAlign="middle" width="20">&nbsp;</TD>
          
          <td width=30>&nbsp;</td>
        </TR>
      </TABLE>
     
      <table class="shade"><tr><td><b>From First Name:</b></td>
      <td><asp:TextBox ID="ContactTextBox" runat="server"></asp:TextBox></td>
      <td><b>To  First Name:</b></td>
      <td><asp:TextBox ID="ToContactTextBox" runat="server"></asp:TextBox></td>
      </tr>
      <tr><td><b>From Date:</b></td><td><asp:TextBox MaxLength="10" Width="80px" ID="DateTextBox" onfocus="javascript:preEnter( this, 'yes' );" onblur="javascript:preLeave( this, 'date', '99/99/9999' );"  ToolTip="MM/DD/YYYY" runat="server"></asp:TextBox>
      <a href="#" onClick="showCalendarControl(DateTextBox); return false"><asp:Image ID="Image3" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
      
      </td>
      <td><b>To Date:</b></td><td><asp:TextBox MaxLength="10" Width="80px" ID="ToDateTextBox" onfocus="javascript:preEnter( this, 'yes' );" onblur="javascript:preLeave( this, 'date', '99/99/9999' );"   ToolTip="MM/DD/YYYY" runat="server"></asp:TextBox>
      <a href="#" onClick="showCalendarControl(ToDateTextBox); return false"><asp:Image ID="Image2" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
      
      </td></tr>
      <tr>
      <td><b>From Customer:</b></td>
      <td><asp:TextBox ID="FromcustomerTextBox" runat="server"></asp:TextBox> 
      <a href="#" onClick="contactcustomerlook(); return false"><asp:Image ID="CustomerLook" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
      </td>
      <td><b>To Customer:</b></td>
      <td><asp:TextBox ID="TocustomerTextBox" runat="server"></asp:TextBox>
      <a href="#" onClick="contactcustomerlook2(); return false"><asp:Image ID="Image1" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
      </td>
      </tr>
      <tr>
          <asp:Label ID="Label1" runat="server" Text="" Font-Bold="true" ForeColor="red"></asp:Label>
      <td> <asp:Button ID="DeleteButton" runat="server" CssClass="button"  OnClick="DeleteButton_Click" Text="Delete" OnClientClick="return confirm('Are you sure you want to Delete?')" /></td></tr>
       </table>
    </div>
    <ft:footer id="Footer1" runat="server"></ft:footer>
    </form>
  </body>
</HTML>

