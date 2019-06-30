<%@ Page Language="C#" AutoEventWireup="true" Debug="true" Inherits="main_show_hide_ord_entry" Codebehind="main_sequance_entry.aspx.cs" %>
<%@ Register Src="footer.ascx" TagName="Footer" TagPrefix="ft" %>
<%@ Register Src="header.ascx" TagName="Header" TagPrefix="hd" %>

<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">

<html xmlns="http://www.w3.org/1999/xhtml" >
<head runat="server">
    <title>Columns Maintenance</title>
    <LINK href="include/style.css" type="text/css" rel="stylesheet"/>    
</head>
<body>
    <form id="form1" runat="server">
   <hd:header id="Header1" runat="server"></hd:header>
    <div>
    
         <TABLE id="tblTop" cellSpacing="3" align="center" border="0" Width="100%">
        <TR>
            
          <TD align=center nowrap><font size=+0><b>Columns Maintenance &nbsp;</b></font></TD>
          
          <TD vAlign="middle" align="center" nowrap >Logged as&nbsp;
            <asp:label id="lblUser" runat="server" Font-Bold="True">&nbsp;</asp:label>&nbsp;&nbsp;&nbsp;
            <asp:linkbutton id="hlnkLogOut" runat="server" OnClick="hlnkLogOut_Click">Log out</asp:linkbutton>
            
          &nbsp;<b>Company:</b>&nbsp;<asp:Label ID="labelcompany" runat="server" ></asp:Label></TD>
          
         
          <TD vAlign="middle" width="20">&nbsp;</TD>
          
          <td width=30>&nbsp;</td>
        </TR>
      </TABLE>
        <asp:HiddenField ID="HiddenField1" runat="server" />
        <asp:HiddenField ID="HiddenField2" runat="server" />
        <asp:HiddenField ID="HiddenField3" runat="server" />
        
            
    <asp:Label ID="lbl_error" runat="server" Font-Bold="true" ForeColor="red"></asp:Label>
    
           <fieldset class="shade" style="width:45%;">
            <table>
        <tr>            
               <td>     
                    <asp:DropDownList ID="ddl_main_program" OnSelectedIndexChanged="ddl_main_program_selectedindexchanged" AutoPostBack="true" runat="server" DataSourceID="SqlDataSource_select_program" DataTextField="main_program" DataValueField="main_program_val">                    
                    </asp:DropDownList>
                    <asp:SqlDataSource ID="SqlDataSource_select_program" runat="server"
                        ConnectionString="<%$ ConnectionStrings:Project1ConnectionString %>" SelectCommand="SELECT distinct [main_program],[main_program_val] FROM [program_master]">
                    </asp:SqlDataSource>
               </td>
               <td>
                    
                  <asp:DropDownList ID="ddl_sub_program" OnSelectedIndexChanged="ddl_sub_program_selectedindexchanged" AutoPostBack="true" runat="server" DataSourceID="SqlDataSource_sub_program" DataTextField="sub_program" DataValueField="sub_program_val">                   
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
        <br />
        <fieldset class="shade"  style="width:45%;">
        <table align="center">
        <tr><td>
              
        <asp:GridView ID="grid_view1" runat="server" AutoGenerateColumns="false" OnSelectedIndexChanged="GridView1_SelectedIndexChanged" OnRowCreated="GridView1_RowCreated" OnPageIndexChanging="GridView1_PageIndexChanging"
             EmptyDataText="No Records Found" BorderStyle="Dotted" CssClass="Grid" OnPreRender="grid_view1_PreRender">
            <SelectedRowStyle CssClass="GridSelected" BackColor="Yellow" />
            <AlternatingRowStyle CssClass="GridItemOdd" />
            <EmptyDataRowStyle BorderStyle="Dotted" BorderColor="Gray" BorderWidth="0px" Font-Bold="True" HorizontalAlign="Center" VerticalAlign="Middle" />
            <HeaderStyle  BackColor="Teal" ForeColor="White" Height="40px"  VerticalAlign="Middle"  HorizontalAlign="Center" Wrap="False"></HeaderStyle>
            <RowStyle CssClass="shade"  />
           <Columns >
                <asp:CommandField ShowSelectButton="True" ButtonType="Image" SelectImageUrl="~/Images/sel.gif" SelectText="" > 
                    <ItemStyle Width="10px" />
                </asp:CommandField>
                <asp:TemplateField HeaderText="Seq Num" Visible="false" SortExpression="seq">
                    <ItemTemplate>
                        <asp:Label ID="seqLabel" runat="server" Text='<%# Bind("[seq_no]") %>'></asp:Label>                                    
                    </ItemTemplate>                                
                    <ItemStyle Wrap="false" />
                </asp:TemplateField>
                <asp:TemplateField HeaderText="Column Name" SortExpression="col_name">
                    <ItemTemplate>
                        <asp:Label ID="col_nameLabel" runat="server" Text='<%# Bind("[col_name]") %>'></asp:Label>
                    </ItemTemplate>
                    
                    <ItemStyle Wrap="false" />
                </asp:TemplateField>
                
                <asp:TemplateField HeaderText="Column Value" SortExpression="col_val">
                    <ItemTemplate>
                        <asp:Label ID="col_valLabel" runat="server" Text='<%# Bind("[col_val]") %>'></asp:Label>
                    </ItemTemplate>
                    
                    <ItemStyle Wrap="false" />
                </asp:TemplateField>
                
                <asp:TemplateField HeaderText="Main Program" Visible="false" SortExpression="main_program">
                    <ItemTemplate>
                        <asp:Label ID="main_programLabel" runat="server" Text='<%# Bind("[main_program]") %>'></asp:Label>
                    </ItemTemplate>
                    
                    <ItemStyle Wrap="false" />
                </asp:TemplateField>
                
                <asp:TemplateField HeaderText="Sub Program" Visible="false" SortExpression="sub_program">
                    <ItemTemplate>
                        <asp:Label ID="sub_programLabel" runat="server" Text='<%# Bind("[sub_program]") %>'></asp:Label>
                    </ItemTemplate>
                    
                    <ItemStyle Wrap="false" />
                </asp:TemplateField>
                                                           
            <asp:TemplateField HeaderText="field" Visible="false">
                <ItemTemplate>
                    <asp:Label ID="field_label" runat="server" Text=<%# Bind("col_val") %> ></asp:Label>
                </ItemTemplate>
            </asp:TemplateField>
            
        </Columns>
    </asp:GridView>
 </td>
 
         </tr>
      </table>
      <table>
        <tr><td>                       
           
        <asp:FormView ID="FormView1" runat="server" DataSourceID="SqlDataSource_view_main" OnDataBound="form_view_databound" >
            <EditItemTemplate> 
            
            <table class="shade">   
            <tr>
                        <td colspan="2" style="display:none">
                            <asp:Label ID="seq_noLabel" runat="server" Text='<%# Bind("seq_no") %>'></asp:Label>
                        </td>
                        </tr>
                                     
                    <tr>
                    <%--<td align="right" style="padding-right:5px"><b>Main Program:</b></td>--%>
                        <td style="display:none">
                            <asp:Label ID="main_programLabel" runat="server" Text='<%# Bind("main_program") %>'></asp:Label>
                        </td>
                        </tr>
                       <tr> 
                        <%--<td align="right" style="padding-right:5px"><b>Sub Program:</b></td>--%>
                        <td style="display:none"><asp:Label ID="sub_programLabel" runat="server" Text='<%# Bind("sub_program") %>'></asp:Label>
                        </td>
                    </tr>
                    
                    <tr><td align="right" style="padding-right:5px"><b>Column Name:</b></td>
                        <td><asp:TextBox ID="col_nameTextBox" runat="server" Text='<%# Bind("col_name") %>'></asp:TextBox>
                        </td>
                        </tr>
                       <tr> 
                        <td align="right" style="padding-right:5px"><b>Column Value:</b></td>
                        <td><asp:TextBox ID="col_valTextBox" runat="server" Text='<%# Bind("col_val") %>'></asp:TextBox></td>
                    </tr>
                <tr>
                <td>
                <asp:Button ID="UpdateButton" runat="server" CssClass="button" OnClick="Update_Button_Click" CausesValidation="True" 
                    Text="Save">
                </asp:Button>
                <asp:Button ID="UpdateCancelButton" runat="server" CssClass="button" CausesValidation="False" CommandName="Cancel"
                    Text="Cancel">
                </asp:Button>
                </td></tr>                  
                </table> 
                
            </EditItemTemplate>
            <InsertItemTemplate>
                <table class="shade" >                                   
                    
                    <tr><td align="center" style="padding-right:5px" nowrap><b>Column Name:</b></td>
                        <td><asp:TextBox ID="col_nameTextBox" runat="server" Text='<%# Bind("col_name") %>'></asp:TextBox>                            
                        </td>
                        <td nowrap><asp:RequiredFieldValidator ID="RequiredFieldValidator1" ControlToValidate="col_nameTextBox" runat="server" ErrorMessage="Column Name is mandatory!"></asp:RequiredFieldValidator></td>
                        </tr>
                       <tr> 
                        <td align="center" style="padding-right:5px" nowrap><b>Column Value:</b></td>
                        <td><asp:TextBox ID="col_valTextBox" runat="server" Text='<%# Bind("col_val") %>'></asp:TextBox>                            
                        </td>
                        <td nowrap><asp:RequiredFieldValidator ID="RequiredFieldValidator2" ControlToValidate="col_valTextBox" runat="server" ErrorMessage="Column Value is mandatory!"></asp:RequiredFieldValidator></td>
                    </tr>
                    <tr>
                    <td colspan="2">
                <asp:Button ID="InsertButton" runat="server" CssClass="button" CausesValidation="True" 
                    Text="Save" OnClick="Insert_Button_Click">
                </asp:Button>
                <asp:Button ID="InsertCancelButton" runat="server" CssClass="button" CausesValidation="False" CommandName="Cancel"
                    Text="Cancel">
                </asp:Button>
                </td></tr></table>
            </InsertItemTemplate>
            <ItemTemplate>
            <table align="left" class="shade" width="250px" style="display:none">
            
            <tr>
                    <%--<td align="right" style="padding-right:5px"><b>Main Program:</b></td>--%>
                    <td style="display:none"><asp:Label ID="main_programLabel" runat="server" Width="120px" BackColor="turquoise" BorderColor="Black" BorderStyle="solid" BorderWidth="1px" Text='<%# Bind("main_program") %>'></asp:Label></td>
                    
                </tr> 
                <tr>
                    <%--<td align="right" style="padding-right:5px"><b>Sub Program:</b></td>--%>
                    <td style="display:none"><asp:Label ID="sub_programLabel" runat="server" Width="120px" BackColor="turquoise" BorderColor="Black" BorderStyle="solid" BorderWidth="1px" Text='<%# Bind("sub_program") %>'></asp:Label></td>
                </tr>
                <tr>
                    <td style="display:none"><asp:Label ID="seq_noLabel" runat="server" Width="120px" BackColor="turquoise" BorderColor="Black" BorderStyle="solid" BorderWidth="1px" Text='<%# Bind("seq_no") %>'></asp:Label></td>
                </tr>
             <tr>
                    <td align="right" style="padding-right:5px"><b>Column Name:</b></td>
                    <td><asp:Label ID="col_nameLabel" runat="server" Width="120px" BackColor="turquoise" BorderColor="Black" BorderStyle="solid" BorderWidth="1px" Text='<%# Bind("col_name") %>'></asp:Label></td>
                    
                </tr>   
                <tr>
                    <td align="right" style="padding-right:5px"><b>Column Value:</b></td>
                    <td><asp:Label ID="col_valLabel" runat="server" Width="120px" BackColor="turquoise" BorderColor="Black" BorderStyle="solid" BorderWidth="1px" Text='<%# Bind("col_val") %>'></asp:Label></td>
                </tr> 
                
                
                <tr><td colspan="2"> &nbsp;&nbsp; &nbsp;&nbsp; &nbsp;&nbsp; &nbsp;&nbsp; &nbsp;&nbsp;              
                <asp:Button ID="AddButton" runat="server" CssClass="button" CausesValidation="False" CommandName="new" Text="Add">
                </asp:Button>
                <asp:Button ID="UpdateButton" runat="server" CssClass="button" CausesValidation="False" CommandName="edit"
                Text="Update"></asp:Button>                
                <%--<asp:Button ID="DeleteButton" runat="server" CssClass="button" CausesValidation="False" Text="Delete"></asp:Button>--%>               
            </td></tr> 
             </table>                 
              
      </ItemTemplate>
   </asp:FormView>
   
         <asp:Button ID="add_button" runat="server" CssClass="button" Text="Add" OnClick="add_button_click" />
         <asp:Button ID="Button1" runat="server" CssClass="button" Text="Update" OnClick="update_button_click1" />
         <asp:Button ID="delete_button" runat="server" CssClass="button" Text="Delete" OnClick="delete_button_click1" />
         &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
         
        <asp:Label ID="UErrorLabel" ForeColor="red" runat="server" Text=""></asp:Label>
         
         
         <asp:SqlDataSource ID="SqlDataSource_view_main" runat="server" ConnectionString="<%$ ConnectionStrings:Project1ConnectionString %>" SelectCommand="SELECT [seq_no], [main_program], [sub_program], [col_name], [col_val] FROM [column_maintenance] WHERE (([sub_program] = @sub_program) AND ([main_program] = @main_program) AND ([seq_no] = @seq_no))">
        <SelectParameters>            
            
            <asp:SessionParameter Name="main_program" SessionField="main_ord_entry_main_program_1" Type="String" />    
            <asp:SessionParameter Name="sub_program" SessionField="main_ord_entry_sub_program_1" Type="String" />                 
            <asp:SessionParameter Name="seq_no" SessionField="main_ord_entry_seqnum_1" Type="Int32" />
            </SelectParameters>
        </asp:SqlDataSource>
         </td></tr></table> </fieldset>
    </div>
    </form>
</body>
</html>
