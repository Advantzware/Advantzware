<%@ Page Language="C#" AutoEventWireup="true" Inherits="exrate_lookup" Codebehind="exrate_lookup.aspx.cs" %>

<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">

<html xmlns="http://www.w3.org/1999/xhtml" >
<head id="Head1" runat="server">
    <title>Exchange Rate</title>
    <LINK href="include/style.css" type="text/css" rel="stylesheet"/>
<link rel="stylesheet" href="include/dhtmlwindow.css" type="text/css" />
</head>
<body>
    &nbsp;&nbsp;
    <form id="form1" runat="server" >
    <div><br /><br /><fieldset class="shade">
    <table   style="width:300px">
    <tr><td><b>Currency Code:</b></td>
    <td> <asp:Label ID="CurrLabel" runat="server" Width="100px" BackColor="Turquoise" ></asp:Label></td></tr>
    <tr><td><br /><b>Exchange Rate:</b></td>
    <td> <br /><asp:Label ID="RateLabel" runat="server" Width ="100px" BackColor="Turquoise" ></asp:Label></td></tr>
    <tr><td colspan="2"><br />&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
        <input id="Button1" type="button" class="button" onclick="window.close()" value="Ok" /> &nbsp;&nbsp;&nbsp; 
        <input id="Button2" class="button" type="button" onclick="window.close()" value="Cancel" /></td></tr>
    </table></fieldset>
    </div>
    
    </form>
</body>
</html>


