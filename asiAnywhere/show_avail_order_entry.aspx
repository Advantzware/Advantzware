<%@ Page Language="C#" AutoEventWireup="true" Debug="true" Inherits="show_avail_order_entry" Codebehind="show_avail_order_entry.aspx.cs" %>

<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">

<html xmlns="http://www.w3.org/1999/xhtml" >
<head runat="server">
    <title>Show Columns</title>
    <LINK href="include/style.css" type="text/css" rel="stylesheet"/>
    <script>
        
        
        
    </script>
</head>
<body>
    <form id="form1" runat="server">
    <div>
                    
    <asp:Label ID="lbl_error" runat="server" Font-Bold="true" ForeColor="red"></asp:Label>
         
       <table>
        <tr>
                <td nowrap>Module Name:</td>
                <td nowrap>
                    <asp:Label ID="mod_name_label" runat="server" BackColor="Turquoise"></asp:Label>
                </td>
                <td nowrap>User Name:</td>
                <td nowrap>
                    <asp:Label ID="user_name_label" runat="server" BackColor="Turquoise"></asp:Label>
                </td>
             </tr>  
       </table>
       <br />
        <table>
                     
            <tr>
               <td valign="top">
                <fieldset align="top"><legend> Available Columns</legend>
                    <asp:GridView ID="GridView1" OnPreRender="GridView1_PreRender" Width="100%" EmptyDataText="No Record Found" AutoGenerateColumns="false" runat="server" OnSelectedIndexChanged="GridView1_SelectedIndexChanged" >
                        <SelectedRowStyle CssClass="GridSelected" BackColor="Yellow"  />
                        <AlternatingRowStyle CssClass="GridItemOdd" />
                        <EmptyDataRowStyle BorderStyle="Dotted" BorderColor="Gray" BorderWidth="0px" Font-Bold="True" HorizontalAlign="Center" VerticalAlign="Middle" />
                        <HeaderStyle  BackColor="Teal" ForeColor="White" Height="40px"  VerticalAlign="Middle"  HorizontalAlign="Center" Wrap="False"></HeaderStyle>
                        <RowStyle CssClass="shade"  />
                        <Columns> 
                            <asp:CommandField ShowSelectButton="True" ButtonType="Image" SelectImageUrl="~/Images/sel.gif" SelectText="" > 
                                <ItemStyle Width="10px" />
                            </asp:CommandField> 
                    
                            <asp:TemplateField Visible="false">
                                <ItemTemplate>
                                    <asp:Label ID="seq_label" runat="Server" Text='<%# Eval("[seq_no]") %>'></asp:Label>
                                </ItemTemplate>
                            </asp:TemplateField>
                            
                            <asp:TemplateField HeaderText="Column Name">
                                <ItemTemplate>
                                    <asp:Label ID="col_name_label" runat="Server" Text='<%# Eval("[col_name]") %>'></asp:Label>
                                </ItemTemplate>
                            </asp:TemplateField>                          
                           
                                           
                        </Columns>
                        </asp:GridView>
                        </fieldset>               
                    </td>
                    <td><asp:ImageButton ID="btn_save" runat="server" ImageUrl="~/Images/arrow right.jpg" CssClass="buttonM" OnClick="add_button_click"  /><br />
                    <asp:ImageButton ID="ImageButton1" runat="server" ImageUrl="~/Images/arrow left.jpg" CssClass="buttonM" OnClick="delete_button_click"  /></td>
                    <td valign="top">
                    <fieldset align="top" ><legend> Show</legend>
                    <asp:GridView ID="GridView2" OnPreRender="GridView2_PreRender" Width="100%" EmptyDataText="No Record Found" AutoGenerateColumns="false" runat="server" OnSelectedIndexChanged="GridView2_SelectedIndexChanged" >
                        <SelectedRowStyle CssClass="GridSelected" BackColor="Yellow" />
                        <AlternatingRowStyle CssClass="GridItemOdd" />
                        <EmptyDataRowStyle BorderStyle="Dotted" BorderColor="Gray" BorderWidth="0px" Font-Bold="True" HorizontalAlign="Center" VerticalAlign="Middle" />
                        <HeaderStyle  BackColor="Teal" ForeColor="White" Height="40px"  VerticalAlign="Middle"  HorizontalAlign="Center" Wrap="False"></HeaderStyle>
                        <RowStyle CssClass="shade"  />
                        <Columns>                            
                                   <asp:CommandField ShowSelectButton="True" ButtonType="Image" SelectImageUrl="~/Images/sel.gif" SelectText="" > 
                                        <ItemStyle Width="10px" />
                                    </asp:CommandField>                    
                                    
                                    <asp:TemplateField  Visible="false">
                                        <ItemTemplate>
                                            <asp:Label ID="seq_label" runat="Server" Text='<%# Eval("[seq_no]") %>'></asp:Label>
                                        </ItemTemplate>
                                    </asp:TemplateField>
                            
                                    <asp:TemplateField HeaderText="Column Name">
                                        <ItemTemplate>
                                            <asp:Label ID="col_name_label" runat="Server" Text='<%# Eval("[col_name]") %>'></asp:Label>
                                        </ItemTemplate>
                                    </asp:TemplateField>
                                    
                                    <asp:TemplateField Visible="false" >
                                        <ItemTemplate>
                                            <asp:Label ID="col_seq_label" runat="Server" Text='<%# Eval("[col_seq]") %>'></asp:Label>
                                        </ItemTemplate>
                                    </asp:TemplateField>
                            
                                    
                        </Columns>
                        </asp:GridView>
                        </fieldset>
                      </td>
                      <td>
                        <asp:ImageButton ID="ImageButton2" runat="server" ImageUrl="~/Images/arrow up.jpg" CssClass="buttonM" OnClick="up_button_click"  /><br />
                        <asp:ImageButton ID="ImageButton3" runat="server" ImageUrl="~/Images/arrow down.jpg" CssClass="buttonM" OnClick="down_button_click"  />
                      </td>
                                    
            </tr>
            
        </table>
        
    </div>
    </form>
</body>
</html>
