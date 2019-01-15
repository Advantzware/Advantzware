<%@ Page Language="c#" AutoEventWireup="true" Inherits="Login" Codebehind="Login.aspx.cs" %>
<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.0 Transitional//EN">
<HTML>
  <HEAD>
    <title>Login</title>
    <%--<LINK href="include/style.css" type="text/css" rel="stylesheet"/>--%>
    <script language="javascript">
        function loadjscssfile(filename, filetype) {
            if (filetype == "js") { //if filename is a external JavaScript file
                var fileref = document.createElement('script')
                fileref.setAttribute("type", "text/javascript")
                fileref.setAttribute("src", filename)
            }
            else if (filetype == "css") { //if filename is an external CSS file
                var fileref = document.createElement("link")
                fileref.setAttribute("rel", "stylesheet")
                fileref.setAttribute("type", "text/css")
                fileref.setAttribute("href", filename)
            }
            if (typeof fileref != "undefined")
                document.getElementsByTagName("head")[0].appendChild(fileref)
        }


        loadjscssfile("include/style.css", "css") ////dynamically load and add this .css file
    </script>
  </HEAD>
  <body>
    <form id="Form1" method="post" runat="server">
      <table height="100%" width="100%" border="0">
        <tr>
          <td vAlign="middle" align="center"><asp:panel id="Panel1" runat="server" Enabled="true" width="330" HorizontalAlign="Center" CssClass=shade>
              <TABLE class="loginForm" cellSpacing="0" cellPadding="4" width="330" border="0">
                <TR>
                  <TD colSpan="2" class="blackshade" align="center"><STRONG><FONT size=+1>Login</FONT></STRONG></TD>
                </TR>
                <TR>
                  <TD colSpan="2">&nbsp;</TD>
                </TR>
                <TR>
                  <TD vAlign="top" align="right" width="130">Username:&nbsp;
                  </TD>
                  <TD align="left">
                    <asp:textbox id="txtUserName" tabIndex="1" runat="server"  Width="150" CssClass="control"></asp:textbox>
                    <asp:RequiredFieldValidator id="rfvUserName" runat="server" ErrorMessage="Username can not be empty."
                      ControlToValidate="txtUserName" Display="Dynamic"></asp:RequiredFieldValidator></TD>
                </TR>
                <TR>
                  <TD vAlign="top" align="right">Password:&nbsp;</TD>
                  <TD align="left">
                    <asp:textbox id="txtPassword" tabIndex="2" runat="server" Width="150" CssClass="control"
                      TextMode="Password"></asp:textbox>
                    <asp:RequiredFieldValidator id="rfvPassword" runat="server" ErrorMessage="Password is required"
                      ControlToValidate="txtPassword" Display="Dynamic"></asp:RequiredFieldValidator></TD>
                </TR>
                
                <TR>
                  <TD align="center" colSpan="2">
                    <asp:CheckBox id="chbSavePassword" runat="server" TextAlign="Left" CssClass="checkbox" Text="Remember Password"></asp:CheckBox></TD>
                </TR>
                <TR>
                  <TD align="center" colSpan="2">&nbsp;
                    <asp:Button id="cmdLogin" runat="server" CssClass="buttonM" OnClick="cmdLogin_Click" Text="Submit"></asp:Button>&nbsp;</TD>
                </TR>
                
                <TR>
                  <TD align=middle valign=bottom  colspan=2><br>
                       <%-- <a href="register.aspx">Register</a>--%>
           
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<a href="remind.aspx">Forgot password?</a>

                   </td>
                </TR>
                
              </TABLE>
              <P>
                <asp:Label id="lblMessage" runat="server" CssClass="errormsg" ForeColor="Red"></asp:Label></P>
            </asp:panel></td>
        </tr>
      </table>
    </form>
    <P></P>
  </body>
</HTML>
