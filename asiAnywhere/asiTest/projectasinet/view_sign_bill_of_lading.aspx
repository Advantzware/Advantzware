<%@ Page Language="c#" AutoEventWireup="true" Debug="true" Inherits="view_sign_bill_of_lading" Codebehind="view_sign_bill_of_lading.aspx.cs" %>
<%@ Register Src="footer.ascx" TagName="Footer" TagPrefix="ft" %>
<%@ Register Src="header.ascx" TagName="Header" TagPrefix="hd" %>

<html xmlns="http://www.w3.org/1999/xhtml" >
  <head id="Head1" runat="server">
    <title>View Signed BOL</title>
    <LINK href="include/style.css" type="text/css" rel="stylesheet"/>
    <LINK REL="stylesheet" TYPE="text/css" HREF="include/CalendarControl.css" >
    <link href="include/tree.css" rel="stylesheet" type="text/css" />
    <script language = "JavaScript" src="include/CalendarControl.js"></script>
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
    
     
 
    
    function contactcustomerlook(){ 
  var NewWindow = window.open("contact_customer_lookup.aspx","ContactCustomerLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}

function ContactCustomerLookup(ReturnObj1,ReturnObj2,ReturnObj3,ReturnObj4,ReturnObj5,ReturnObj6,ReturnObj7,ReturnObj8,ReturnObj9, ReturnObj10,ReturnObj11){ 
  document.forms[0].TextBox1.value = ReturnObj1;
  document.forms[0].TextBox1.focus();    
}

function bolnumlook(){ 
  var NewWindow = window.open("bolnum_rep_lookup.aspx?post="+ "Yes" +"","BolNumLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}

function bolnumreplook(ReturnObj1){
    document.forms[0].TextBox3.value = ReturnObj1;
    document.forms[0].TextBox3.focus();
}

function Datelook(){ 
  var NewWindow = window.open("date_lookup.aspx","DateLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
  }
function Datelookup(obj)
{
  document.forms[0].TextBox9.value=obj;
}
function Datelook1()
{
  document.forms[0].TextBox9.value="";
  Datelook();
}
    </script> 
        
  </head>    
   <body>
    <form id="frmList" runat="server" defaultfocus="TextBox1" >   
        <hd:header id="Header1" runat="server"></hd:header>
      <div>       
            
                       
      <TABLE id="tblTop" cellSpacing="3" align="center" border="0" Width="100%">
        <TR>
            
          <TD  nowrap><font size=+0><b>View Signed BOL&nbsp;</b></font></TD>
          <TD  nowrap >
            <asp:linkbutton id="hlkBackToMenu" OnClick="Back_tomenu_Click" runat="server" ></asp:linkbutton>
          </TD>
          <TD vAlign="middle"  nowrap >Logged as&nbsp;
            <asp:label id="lblUser" runat="server" Font-Bold="True">&nbsp;</asp:label>&nbsp;&nbsp;&nbsp;
            <asp:linkbutton id="hlnkLogOut" runat="server" OnClick="hlnkLogOut_Click">Log out</asp:linkbutton>
            &nbsp;&nbsp;<asp:hyperlink id="hlnkChangePwd" runat="server" NavigateUrl="changepwd.aspx"></asp:hyperlink>
          &nbsp;<b>Company:</b>&nbsp;<asp:Label ID="labelcompany" runat="server" Text="Label"></asp:Label></TD>
          
         
          <TD vAlign="middle" width="20">&nbsp;</TD>
          
          <td width=30>&nbsp;</td>
        </TR>
      </TABLE>
      <asp:HiddenField ID="HiddenField1" runat="server" />
         <asp:HiddenField ID="HiddenField2" runat="server" />
         <asp:HiddenField ID="HiddenField3" runat="server" />
         <asp:HiddenField ID="HiddenField4" runat="server" />
         
      
         
          <asp:Label ID="Label1" Font-Bold="true" ForeColor="red" runat="server" ></asp:Label>
          
                    
      <table class="shade" width="520px">
     
      <tr><td align="right" style="padding-right: 5px"><b>Customer#:</b></td><td>
          <asp:TextBox ID="TextBox1" MaxLength="8"  width="100px" runat="server"></asp:TextBox>
          <a href="#" tabindex="1" onClick="contactcustomerlook(); return false"><asp:Image ID="CustomerLook" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
          </td>
        <td align="right" style="padding-right: 5px"><b> BOL#:</b></td>
          <td nowrap><asp:TextBox  ID="TextBox3" runat="server" MaxLength="8" Width="100px" ></asp:TextBox>          
          <a href="#" tabindex="1" onClick="bolnumlook(); return false"><asp:Image ID="BolLookup" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
            &nbsp;&nbsp;&nbsp;<asp:Label ID="Label2" runat="server" ForeColor="Red"></asp:Label>
              
          </td>
        </tr>
         <tr><td colspan="4">
              &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
              <asp:Button ID="submitbutton" OnClick="submitbutton_click" runat="server" class="buttonM" Text="Submit" /></td>
          </tr> 
       
        </table> 
        
          
      
          
    </div>
    <ft:footer id="Footer1" runat="server"></ft:footer>
         
    </form>
  </body>
</HTML>


