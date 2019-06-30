<%@ Page Language="C#" AutoEventWireup="true" Inherits="dumpload_column_maint" Codebehind="~/dumpload_column_maintenance.aspx.cs" %>
<%@ Register Src="footer.ascx" TagName="Footer" TagPrefix="ft" %>
<%@ Register Src="header.ascx" TagName="Header" TagPrefix="hd" %>
<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">

<script runat="server">

</script>

<html xmlns="http://www.w3.org/1999/xhtml" >
<head id="Head1" runat="server">
    <title>DumpLoad Column Maintenance</title>
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
    <legend style="padding-left:170px;"><b>Dump and Load Column Maintenance</b></legend>
    <br />
    <br />
    <table style="width: 490px">
    
    
    <%--<tr>
    <td>
    <fieldset style="width:482px;">
    <table>        
    <tr>    
        <td><asp:RadioButton ID="ColumnMaintRadio" runat="server" Checked="true" GroupName="select" Text="Column Maintenance" Font-Bold="true" /></td>
         <td>
            <asp:Label ID="columnmaintpath" runat="server" Text="Path" Font-Bold="true"></asp:Label>
            <asp:FileUpload ID="columnmaintFileUpload" runat="server" />
        </td>    
    </tr>
       
    
    </table>
    </fieldset>        
    </td></tr>--%>    
    
    <%--<tr>
    <td>
        <fieldset style="width:482px;">
            <table align="left">
                <tr>
                    <td>     
                        <asp:DropDownList ID="ddl_main_program" AutoPostBack="true" runat="server" DataSourceID="SqlDataSource_select_program" DataTextField="main_program" DataValueField="main_program_val">                    
                        </asp:DropDownList>
                        <asp:SqlDataSource ID="SqlDataSource_select_program" runat="server"
                            ConnectionString="<%$ ConnectionStrings:Project1ConnectionString %>" SelectCommand="SELECT distinct [main_program],[main_program_val] FROM [program_master]">
                        </asp:SqlDataSource>
                    </td>
               
                    <td>                    
                        <asp:DropDownList ID="ddl_sub_program" AutoPostBack="true" runat="server" DataSourceID="SqlDataSource_sub_program" DataTextField="sub_program" DataValueField="sub_program_val">                   
                        </asp:DropDownList><asp:SqlDataSource ID="SqlDataSource_sub_program" runat="server"
                        ConnectionString="<%$ ConnectionStrings:Project1ConnectionString %>" SelectCommand="SELECT [sub_program], [sub_program_val] FROM [program_master] WHERE ([main_program_val] = @main_program_val)">
                        <SelectParameters>
                          <asp:ControlParameter ControlID="ddl_main_program" Name="main_program_val" PropertyName="SelectedValue"
                              Type="String" />
                        </SelectParameters>
                        </asp:SqlDataSource>
                     </td> 
 
    </tr>
    </table>
    </fieldset>
    </td>
    </tr>--%>
    
    <tr>
    <td>
        <fieldset style="width:482px;">
            <table align="left">
                <tr>
                    <td>     
                        <asp:DropDownList ID="ddl_program_name" AutoPostBack="true" runat="server" >   
                        <asp:ListItem Selected="True" Value="1">Column Maintenance</asp:ListItem>
                        <asp:ListItem Value="2">Module Maintenance</asp:ListItem>                 
                        </asp:DropDownList>                        
                    </td>
               
                </tr>
            </table>
        </fieldset>
    </td>
    </tr>
    
    
    <tr>
    <td>
        <fieldset style="width:482px;">
            <table align="left">
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
    
    
    <tr>
    <td>
        <fieldset style="width:482px;">
            <table align="left">
                <tr>                                                  
                   <td>
                        <asp:Label ID="columnmaintpath" runat="server" Text="Path" Font-Bold="true"></asp:Label>
                        <asp:FileUpload ID="columnmaintFileUpload" runat="server" />
                    </td> 
                </tr>
            </table>
        </fieldset>
    </td>
    </tr>
         
    
    <tr><td>&nbsp;</td></tr>
    <tr><td>&nbsp;</td></tr>
    <tr>
    <td>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
    <asp:Button ID="DumpButton" runat="server" Text="Dump" CssClass="buttonM" OnClick="DumpButton_Click" />
   &nbsp;&nbsp;&nbsp;&nbsp;
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
