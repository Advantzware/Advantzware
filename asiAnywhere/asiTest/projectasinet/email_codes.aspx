<%@ Page Language="C#" AutoEventWireup="true" Debug="true" Inherits="email_codes" Codebehind="email_codes.aspx.cs" %>
<%@ Register Src="footer.ascx" TagName="Footer" TagPrefix="ft" %>
<%@ Register Src="header.ascx" TagName="Header" TagPrefix="hd" %>

<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">

<html xmlns="http://www.w3.org/1999/xhtml" >
<head runat="server">
    <title>Email Codes</title>
    <LINK href="include/style.css" type="text/css" rel="stylesheet"/>  
    <script type="text/javascript" language="javascript">

        function validatelimit(obj, maxchar) {

            if (this.id) obj = this;

            var remaningChar = maxchar - obj.value.length; 

            if (remaningChar <= 0) {
                obj.value = obj.value.substring(maxchar, 0);
                alert('Character Limit exceeds!');
                return false;

            }
            else
            { return true; }
        }

    </script>   
</head>
<body>
    <form id="form1" runat="server">
   <hd:header id="Header1" runat="server"></hd:header>
    <div>
        <TABLE id="tblTop" cellSpacing="3" border="0" Width="100%">
            <TR>            
                <TD nowrap><font size=+0><b>Email Codes &nbsp;</b></font></TD>          
                <TD vAlign="middle" align="center" nowrap >Logged as&nbsp;
                    <asp:label id="lblUser" runat="server" Font-Bold="True">&nbsp;</asp:label>&nbsp;&nbsp;&nbsp;
                    <asp:linkbutton id="hlnkLogOut" runat="server" OnClick="hlnkLogOut_Click">Log out</asp:linkbutton>
                    &nbsp;<b>Company:</b>&nbsp;<asp:Label ID="labelcompany" runat="server" ></asp:Label></TD>                   
                <TD vAlign="middle" width="20">&nbsp;</TD>          
                <td width=350>&nbsp;</td>
            </TR>
        </TABLE>
        <asp:HiddenField ID="HiddenField1" runat="server" />
        <asp:HiddenField ID="HiddenField2" runat="server" />
        <asp:HiddenField ID="HiddenField3" runat="server" />
        
            
    <asp:Label ID="lbl_error" runat="server" Font-Bold="true" ForeColor="red"></asp:Label>
    
          
        <fieldset class="shade"  style="width:75%;">
        <table>
        <tr><td>              
            <asp:GridView ID="grid_view1" runat="server" AutoGenerateColumns="False" 
                OnSelectedIndexChanged="GridView1_SelectedIndexChanged" 
                OnRowCreated="GridView1_RowCreated" OnPageIndexChanging="GridView1_PageIndexChanging"
             EmptyDataText="No Records Found" BorderStyle="Dotted" CssClass="Grid" 
                OnPreRender="grid_view1_PreRender" DataKeyNames="program_name" DataSourceID="SqlDataSource1">
            <SelectedRowStyle CssClass="GridSelected" BackColor="Yellow" />
            <AlternatingRowStyle CssClass="GridItemOdd" />
            <EmptyDataRowStyle BorderStyle="Dotted" BorderColor="Gray" BorderWidth="0px" Font-Bold="True" HorizontalAlign="Center" VerticalAlign="Middle" />
            <HeaderStyle  BackColor="Teal" ForeColor="White" Height="40px"  VerticalAlign="Middle"  HorizontalAlign="Center" Wrap="False"></HeaderStyle>
            <RowStyle CssClass="shade"  />
           <Columns >
                <asp:CommandField ShowSelectButton="True" ButtonType="Image" SelectImageUrl="~/Images/sel.gif" SelectText="" > 
                    <ItemStyle Width="10px" />
                </asp:CommandField>
                <asp:BoundField DataField="program_name" HeaderText="Program Name" 
                    SortExpression="program_name" />
                <asp:BoundField DataField="email_from" HeaderText="Email From" 
                    SortExpression="email_from" />
                <asp:BoundField DataField="subject" HeaderText="Subject" 
                    SortExpression="subject" />
            
            </Columns>
        </asp:GridView>
            <asp:SqlDataSource ID="SqlDataSource1" runat="server" 
                ConnectionString="<%$ ConnectionStrings:Project1ConnectionString %>" 
                SelectCommand="SELECT * FROM [email_alerts]"></asp:SqlDataSource>
                
        </td></tr>
        </table><br>
      <table>
        <tr><td>                       
           
        <asp:FormView ID="FormView1" runat="server" DataSourceID="SqlDataSource_view_main" OnDataBound="form_view_databound" >
            <EditItemTemplate>             
                <table class="shade" >            
                <tr>
                    <td align="center" style="padding-right:5px" nowrap><b>Program Name:</b></td>
                    <td><asp:Label ID="program_nameLabel" Width="230px" runat="server" BackColor="turquoise" BorderColor="Black" BorderStyle="solid" BorderWidth="1px" Text='<%# Bind("program_name") %>' /></td>                    
                </tr>     
                <tr>
                    <td align="center" style="padding-right:5px" nowrap><b>Email From:</b></td>
                    <td><asp:TextBox ID="email_fromTextBox" runat="server" Width="230px" Text='<%# Bind("email_from") %>' /></td> 
                    <td nowrap><asp:RequiredFieldValidator ID="RequiredFieldValidator2" ControlToValidate="email_fromTextBox" runat="server" ErrorMessage="Email From is mandatory!"></asp:RequiredFieldValidator>
                    <asp:RegularExpressionValidator ID="rfvUserEmailValidate" runat="server" ValidationExpression="\w+([-+.]\w+)*@\w+([-.]\w+)*\.\w+([-.]\w+)*" ControlToValidate="email_fromTextBox" ErrorMessage="Invalid Email Format"></asp:RegularExpressionValidator></td>                   
                </tr>   
                <tr>
                    <td align="center" style="padding-right:5px" nowrap><b>Subject:</b></td>
                    <td><asp:TextBox ID="subjectTextBox" runat="server" TextMode="MultiLine" onkeyup="validatelimit(this,200)" MaxLength="200" Width="400px" Height="40px" Text='<%# Bind("subject") %>' /></td>                    
                    <td nowrap><asp:RequiredFieldValidator ID="RequiredFieldValidator3" ControlToValidate="subjectTextBox" runat="server" ErrorMessage="Subject is mandatory!"></asp:RequiredFieldValidator></td>
                </tr>  
                <tr>
                    <td colspan="2"> 
                        <asp:Button ID="UpdateButton" runat="server" CausesValidation="True" CssClass="button" OnClick="Update_Button_Click" Text="Save" />
                        &nbsp;<asp:Button ID="UpdateCancelButton" runat="server" CausesValidation="False" CssClass="button" CommandName="Cancel" Text="Cancel" />
                    </td>
                </tr></table>
            </EditItemTemplate>
            <InsertItemTemplate>
            <table class="shade" >            
                <tr>
                    <td align="center" style="padding-right:5px" nowrap><b>Program Name:</b></td>
                    <td><asp:TextBox ID="program_nameTextBox" Width="230px" runat="server" Text='<%# Bind("program_name") %>' /></td>
                    <td nowrap><asp:RequiredFieldValidator ID="RequiredFieldValidator1" ControlToValidate="program_nameTextBox" runat="server" ErrorMessage="Program Name is mandatory!"></asp:RequiredFieldValidator></td>
                </tr>     
                <tr>
                    <td align="center" style="padding-right:5px" nowrap><b>Email From:</b></td>
                    <td><asp:TextBox ID="email_fromTextBox" runat="server" Width="230px" Text='<%# Bind("email_from") %>' /></td>   
                    <td nowrap><asp:RequiredFieldValidator ID="RequiredFieldValidator2" ControlToValidate="email_fromTextBox" runat="server" ErrorMessage="Email From is mandatory!"></asp:RequiredFieldValidator>
                    <asp:RegularExpressionValidator ID="rfvUserEmailValidate" runat="server" ValidationExpression="\w+([-+.]\w+)*@\w+([-.]\w+)*\.\w+([-.]\w+)*" ControlToValidate="email_fromTextBox" ErrorMessage="Invalid Email Format"></asp:RegularExpressionValidator></td>                                     
                </tr>   
                <tr>
                    <td align="center" style="padding-right:5px" nowrap><b>Subject:</b></td>
                    <td><asp:TextBox ID="subjectTextBox" runat="server" MaxLength="10" TextMode="MultiLine" Width="400px" Height="40px" onkeyup="validatelimit(this,200)" Text='<%# Bind("subject") %>' /> </td>                    
                    <td nowrap><asp:RequiredFieldValidator ID="RequiredFieldValidator3" ControlToValidate="subjectTextBox" runat="server" ErrorMessage="Subject is mandatory!"></asp:RequiredFieldValidator>                         
                    </td>
                    
                </tr>                
                <tr>
                    <td colspan="2">         
                        <asp:Button ID="InsertButton" runat="server" CausesValidation="True" CssClass="button" Text="Save" OnClick="Insert_Button_Click" />
                        &nbsp;<asp:Button ID="InsertCancelButton" runat="server" CssClass="button" CausesValidation="False" CommandName="Cancel" Text="Cancel" />
                    </td>
                </tr></table>
                
            </InsertItemTemplate>
            <ItemTemplate>
                <table align="left" class="shade" width="250px">
                <tr>
                    <td align="right" style="padding-right:5px" nowrap><b>Program Name:</b></td>
                    <td><asp:Label ID="program_nameLabel" runat="server" Width="230px" BackColor="turquoise" BorderColor="Black" BorderStyle="solid" BorderWidth="1px" Text='<%# Bind("program_name") %>' /></td>                    
                </tr>                  
                <tr>
                    <td align="right" style="padding-right:5px" nowrap><b>Email From:</b></td>
                    <td><asp:Label ID="email_fromLabel" runat="server" Width="230px" BackColor="turquoise" BorderColor="Black" BorderStyle="solid" BorderWidth="1px" Text='<%# Bind("email_from") %>' /></td>                    
                </tr> 
                <tr>
                    <td align="right" style="padding-right:5px" nowrap><b>Subject:</b></td>
                    <td><asp:Label ID="subjectLabel" runat="server" TextMode="MultiLine" Width="400px" Height="40px" BackColor="turquoise" BorderColor="Black" BorderStyle="solid" BorderWidth="1px" Text='<%# Bind("subject") %>' /></td>                    
                </tr> 
                </table>                       
      </ItemTemplate>
   </asp:FormView>
   
         <asp:Button ID="add_button" runat="server" CssClass="button" Text="Add" OnClick="add_button_click" />
         <asp:Button ID="Button1" runat="server" CssClass="button" Text="Update" OnClick="update_button_click1" />
         <asp:Button ID="delete_button" runat="server" CssClass="button" Text="Delete" OnClick="delete_button_click1" />
         &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
         
        <asp:Label ID="UErrorLabel" ForeColor="red" runat="server" Text=""></asp:Label>
         
         
         <asp:SqlDataSource ID="SqlDataSource_view_main" runat="server" 
                ConnectionString="<%$ ConnectionStrings:Project1ConnectionString %>" 
                SelectCommand="SELECT * FROM [email_alerts] WHERE ([program_name] = @program_name)">
        <SelectParameters>            
            
            <asp:ControlParameter ControlID="grid_view1" Name="program_name" 
                PropertyName="SelectedValue" Type="String" />
            </SelectParameters>
        </asp:SqlDataSource>
         </td></tr></table> 
         
         </fieldset>
    </div>
    </form>
</body>
</html>
