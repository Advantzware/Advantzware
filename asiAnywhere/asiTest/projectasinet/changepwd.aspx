<%@ Page Language="c#" AutoEventWireup="true" Inherits="changepwd" Codebehind="changepwd.aspx.cs" %>
<html xmlns="http://www.w3.org/1999/xhtml" >
<head runat="server">
    <title>Change password</title>
    <link href="include/style.css" type="text/css" rel="stylesheet"/>
</head>
<body>
    <form id="form1" runat="server">
    <table width="100%" height=100%><tr valign="middle" align="center"><td>
        <asp:ChangePassword ID="ChangePassword1" runat="server" CssClass="shade" BorderStyle="Solid" BorderWidth="1px" Height="150px" Width="350px"
            ChangePasswordTitleText="Change password" 
            PasswordLabelText="Old password" 
            PasswordRequiredErrorMessage="Field Old password can&#146;t be blank" 
            ConfirmNewPasswordLabelText="Confirm password" 
            ConfirmPasswordRequiredErrorMessage="Field New password can&#146;t be blank"         
            NewPasswordLabelText="New password" 
            NewPasswordRequiredErrorMessage="Field Confirm password can&#146;t be blank" 
            CancelButtonText="Back" 
            ChangePasswordButtonText="Submit" 
            NewPasswordRegularExpressionErrorMessage="Passwords do not match. Re-enter password" 
            SuccessText="Password was changed"
            OnCancelButtonClick="ChangePassword1_CancelButtonClick" 
            OnChangingPassword="ChangePassword1_ChangingPassword"
            >
			<TitleTextStyle CssClass="blackshade" Font-Bold="true" Font-Size=Medium />
            <CancelButtonStyle CssClass="button" />
			<ChangePasswordButtonStyle CssClass="button" />
        </asp:ChangePassword>
        <asp:label id="lblMessage" runat="server" ForeColor="Red"></asp:label><BR>
	</td></tr></table>		
    </form>
</body>
</html>
