<%@ Page Language="c#" AutoEventWireup="true" Inherits="Remind" Codebehind="remind.aspx.cs" %>
<html xmlns="http://www.w3.org/1999/xhtml" >
<head id="Head1" runat="server">
    <title>Password reminder</title>
    <link href="include/style.css" type="text/css" rel="stylesheet"/>
       <script language="JavaScript">
  function UpdateValidators()
  {
      if (document.frmRemind.rbUserName.checked)
      {
        ValidatorEnable(revEmail, false);
        ValidatorEnable(rfvEmail, false);
        ValidatorEnable(rfvUserName, true);
      }
      else
      {
        ValidatorEnable(revEmail, true);
        ValidatorEnable(rfvEmail, true);
        ValidatorEnable(rfvUserName, false);

      }   
      return Page_IsValid;    
  }
    
  function UpdateControls()
  {
    ValidatorEnable(revEmail, false);
    ValidatorEnable(rfvEmail, false);
    ValidatorEnable(rfvUserName, false);

    if (document.frmRemind.rbUserName.checked)
    {
    document.frmRemind.txtUserName.style.backgroundColor='white';
    document.frmRemind.txtEmail.style.backgroundColor='gainsboro';
    document.frmRemind.txtUserName.disabled=false; 
    document.frmRemind.txtEmail.disabled=true;  
    document.frmRemind.txtEmail.value = ""; 
    document.frmRemind.txtUserName.focus();
    }
    else
    {
    document.frmRemind.txtUserName.style.backgroundColor='gainsboro';
    document.frmRemind.txtEmail.style.backgroundColor='white';
    document.frmRemind.txtUserName.disabled=true; 
    document.frmRemind.txtEmail.disabled=false;
    document.frmRemind.txtUserName.value = "";
    document.frmRemind.txtEmail.focus();  
    }

  }
    function OnKeyDown()
    {
      e = window.event;
      if (e.keyCode == 13)
      {
        e.cancel = true;
        document.forms[0].submit();
      } 
    }
  </script> 
</head>
<body onload="javascript: ValidatorEnable(rfvUserName, false);">  
    <form id="frmRemind" method="post" runat="server">  
    <table height="100%" width="100%" border="0">
    <tr><td vAlign="middle" align="center">
    <TABLE class="shade" height="75" cellSpacing="1" cellPadding="1" width="300" align="center">
      <asp:Panel Runat="server" id="pnlChange" width="300">
        <TR>
          <TD class="blackshade" align="center" colSpan="4"><b><font size="+1">&nbsp;<!--StartFragment -->
                <B><FONT size="+1">Password reminder</FONT></B></font></b></TD>
        </TR>
        <TR>
          <TD width="50">&nbsp;</TD>
          <TD align="center" colSpan="2"><br>
            <br>
            Please enter your username or email address  and click on submit to receive your password by email
            <br>
          </TD>
          <TD width="50">&nbsp;</TD>
        </TR>
        <TR>
          <TD>&nbsp;</TD>
          <TD width="35%" valign=top align="left">
              <nobr><asp:radiobutton id="rbEmail"  CssClass="checkbox" onclick="UpdateControls()" runat="server" Text="Email:" GroupName="Reminder"
              Checked="True"></asp:radiobutton></nobr>
          </TD>
          <TD>
            <asp:textbox id="txtEmail" runat="server" CssClass="control"></asp:textbox><br>
            <asp:regularexpressionvalidator id="revEmail" runat="server" Display="Dynamic" ControlToValidate="txtEmail" Enabled="False" ErrorMessage="Please enter valid email address."
              ValidationExpression="\w+([-+.]\w+)*@\w+([-.]\w+)*\.\w+([-.]\w+)*"></asp:regularexpressionvalidator>
            <asp:requiredfieldvalidator id="rfvEmail" runat="server" Display="Dynamic" ControlToValidate="txtEmail" Enabled="False" ErrorMessage="Please enter valid email address.">
            </asp:requiredfieldvalidator>
          </TD>
          <TD>&nbsp;</TD>
        </TR>
        <TR>
          <TD>&nbsp;</TD>
          <TD valign=top align="left"><nobr><asp:radiobutton id="rbUserName"  CssClass="checkbox" onclick="UpdateControls()" runat="server" Text="Username:" GroupName="Reminder"></asp:radiobutton></nobr></TD>
          <TD>
            <asp:textbox id="txtUserName" runat="server" Enabled="False" BackColor="gainsboro" CssClass="control"></asp:textbox><br>
            <asp:requiredfieldvalidator id="rfvUserName" runat="server" Display="Dynamic" ControlToValidate="txtUserName" Enabled="False"
              ErrorMessage="Username can not be empty."></asp:requiredfieldvalidator></TD>
          <TD>&nbsp;</TD>
        </TR>
        <TR>
          <TD>&nbsp;</TD>
          <TD align="center" colSpan="2"><br>
            <asp:button id="btnSubmit" runat="server" onClick="btnSubmit_Click" Text="Submit" CssClass="buttonM"></asp:button></TD>
          <TD>&nbsp;</TD>
        </TR>
        </asp:Panel>
        <TR>
          <TD>&nbsp;</TD>
          <TD align="center" colSpan="2"><br>
            <asp:label id="lblMessage" runat="server" ForeColor="Red"></asp:label><BR>
            <asp:hyperlink id="hlBack" runat="server"   NavigateUrl="./">Back to login page</asp:hyperlink><br><br>
          </TD>
          <TD>&nbsp;</TD>
        </TR>
      </TABLE>    
    </TD></TR></TABLE>  
    </form> 
  </body>
</HTML>