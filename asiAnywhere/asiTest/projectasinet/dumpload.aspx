<%@ Page Language="C#" AutoEventWireup="true" Inherits="dumpload_main" Codebehind="~/dumpload.aspx.cs" %>
<%@ Register Src="footer.ascx" TagName="Footer" TagPrefix="ft" %>
<%@ Register Src="header.ascx" TagName="Header" TagPrefix="hd" %>
<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">

<script runat="server">

</script>

<html xmlns="http://www.w3.org/1999/xhtml" >
<head id="Head1" runat="server">
    <title>DumpLoad</title>
    <LINK href="include/style.css" type="text/css" rel="stylesheet"/>
     <link href="include/tree.css" rel="stylesheet" type="text/css" />
    <link href="include/tree.css" rel="stylesheet" type="text/css" />
</head>
<body>
    <form id="form1" runat="server">
     <hd:header id="Header1" runat="server"></hd:header>
    <div>
    <asp:Label ID="ErrLabel" runat="server" Font-Bold="true" ForeColor="red"></asp:Label>
    <br />
    <fieldset style="border:solid 1px black; background-color:#EFF3FB; height:400px; width:500px;">
    <legend style="padding-left:170px;"><b>Dump and Load</b></legend>
    <br />
    <br />
    <table style="width: 490px">
    <tr>
    <td>
    <fieldset style="width:482px;">
    <table align="center">
    <tr>
    <td>
    <b><asp:RadioButton ID="DumpRadio" runat="server" AutoPostBack="true" Text="Dump" GroupName="selectone" />
    &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
    <asp:RadioButton ID="LoadRadio" runat="server" AutoPostBack="true" Text="Load" GroupName="selectone" />
    </b>
    </td>
    </tr>
    </table>
    </fieldset>
    </td>
    </tr>
    <tr><td>
    &nbsp;
    </td></tr>
    <tr>
    <td>
    <fieldset style="width:482px;">
    <table>
    <tr>
    <td align="center">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
    &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
    &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
    <asp:Label ID="SqlDataBase" runat="server" Font-Bold="true" ForeColor="red" Text="Sql DataBase"></asp:Label>
    </td>
    
    </tr>
    <tr><td>&nbsp;</td></tr>
    <tr><td>&nbsp;</td></tr>
    <tr>    
    <td>
    <asp:RadioButton ID="MenuRadio" runat="server" GroupName="select" Text="Menu" Font-Bold="true" />
        &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp;
        &nbsp; &nbsp; &nbsp; 
         <asp:Label ID="menupath" runat="server" Text="Path" Font-Bold="true"></asp:Label>
        <asp:FileUpload ID="menuFileUpload" runat="server" /></td>
    
    </tr>
    <tr>
    <td>
    <asp:RadioButton ID="SqlUserRadio" Checked="true" runat="server" GroupName="select" Text="Users" Font-Bold="true" />
    &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
    &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
    <asp:Label ID="SqlUsersPath" runat="server" Text="Path" Font-Bold="true"></asp:Label>
        <asp:FileUpload ID="FileUpload1" runat="server" />
    </td>
    </tr>
    </table>
    </fieldset>
    <fieldset style="width:482px;">
    <table>
    <tr>
    <td align="center"><asp:Label ID="ProgressDataBase" runat="server" Font-Bold="true" ForeColor="red" Text="Progress DataBase"></asp:Label>
    </td>
    </tr>
    <tr><td>&nbsp;</td></tr>
    <tr><td>&nbsp;</td></tr>
    <tr>
    <td>
    <asp:RadioButton ID="SecurityRadio"  GroupName="select" runat="server" Text="Program Security" Font-Bold="true" />
    <asp:RadioButton ID="LoadSecurityRadio" GroupName="select" runat="server" Text="Program Security" Font-Bold="true" />
    
    &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
    <b><asp:Label ID="SecurityRelativePath" Text="Path:" runat="server"></asp:Label></b>
    <asp:TextBox ID="SecurityPath" runat="server" Width="200px"></asp:TextBox>
    </td>
    </tr>
    <tr>
    <td>
    <asp:RadioButton ID="UserRadio" GroupName="select" runat="server" Text="Users" Font-Bold="true" />
    <asp:RadioButton ID="LoadUserRadio" GroupName="select" runat="server" Text="Users" Font-Bold="true" />
    &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
     &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
    <b><asp:Label ID="UserRelativePath" Text="Path:" runat="server"></asp:Label></b>
    <asp:TextBox ID="UserPath" runat="server" Width="200px"></asp:TextBox>
    </td>
    </tr>
    </table>
    </fieldset>
    </td></tr>
    
    <tr><td>&nbsp;</td></tr>
    <tr><td>&nbsp;</td></tr>
    <tr>
    <td>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
    <asp:Button ID="DumpButton" runat="server" Text="Dump" CssClass="buttonM" OnClick="DumpButton_Click" />
   &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; 
    <asp:Button ID="LoadButton" runat="server" Text="Load" CssClass="buttonM" OnClick="LoadButton_Click" />
    </td>
    </tr>
    </table> 
   
    </fieldset>
    
    <asp:ObjectDataSource ID="ObjectDataSource1" runat="server" OldValuesParameterFormatString="original_{0}" SelectMethod="SelectDumpLoad" TypeName="dumpload">
        <SelectParameters>
            <asp:Parameter Name="prmAct1" Type="String" />
            <asp:Parameter Name="prmAct2" Type="String" />
            <asp:Parameter Name="PrmPath" Type="String" />
        </SelectParameters>
    </asp:ObjectDataSource>
    </div>
    
     <asp:FormView ID="FormView1" runat="server" DataKeyNames="prgmname" DataSourceID="ObjectDataSource1">
            
            <ItemTemplate>
                
                <asp:Label ID="prgmnameLabel" Visible="false" runat="server" Text='<%# Eval("prgmname") %>'></asp:Label><br />
                
                
                
            </ItemTemplate>
        </asp:FormView>
    <ft:footer id="Footer1" runat="server"></ft:footer>
    </form>
</body>
</html>
