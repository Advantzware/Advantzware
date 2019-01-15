<%@ Page Language="c#" AutoEventWireup="true" Inherits="Menu" Codebehind="Menu.aspx.cs" %>
<%@ Register Src="footer.ascx" TagName="Footer" TagPrefix="ft" %>
<%@ Register Src="header.ascx" TagName="Header" TagPrefix="hd" %>
<HTML>
  <HEAD>
    <link REL="stylesheet" href="include/style.css" type="text/css">
  </HEAD>
  <body>
    <hd:Header ID="Header1" runat="server" />
    <form id="Form1" method="post" runat="server">
      
        <TABLE>
          <TR>
            <TD align="center">Logged as&nbsp;
              <asp:label id="lblUser" runat="server" Font-Bold="True">User</asp:label>&nbsp;&nbsp;&nbsp;
              <asp:linkbutton id="hlnkLogOut" runat="server" OnClick="hlnkLogOut_Click">Log out</asp:linkbutton></TD>
            <TD width="20">&nbsp;</TD>
          </TR>
        </TABLE>
      <hr size="1" noshade>
       
      
      
    </form>
    <ft:Footer ID="Footer1" runat="server" />
  </body>
</HTML>
