<%@ Page Language="C#" AutoEventWireup="true" MasterPageFile="~/MasterPage4.master" Inherits="viewItem" Title="View Item" Codebehind="~/viewItem.aspx.cs" %>

<script runat="server">

    
</script>

<asp:Content ID="Content1" ContentPlaceHolderID="ContentPlaceHolder1" Runat="Server">
<script language="javascript" type="text/javascript">
function history()
{
var NewWindow = window.open("item_history.aspx","OrderHelpWindow","width=600,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}
function stat()
{
var NewWindow = window.open("item_stat_crditlist.aspx","OrderStatWindow","width=600,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}
function Price() {
    var NewWindow = window.open("item_price_level.aspx", "PriceLevelWindow", "width=500,height=150,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}
</script>
    <asp:HiddenField ID="HiddenField1" runat="server" />
    <table>
        <tr>
            <td align=left style="width: 916px">
                
                <asp:FormView ID="FormView4" runat="server" DataKeyNames="ord-no" DataSourceID="ObjectDataSource2" CellPadding="4" ForeColor="#333333" Width="883px" Font-Bold="True" Height="40px">
                        
                    <ItemTemplate>
                    
                        &nbsp; &nbsp; &nbsp; &nbsp;&nbsp;
                        Order#:
                        <asp:Label ID="ord_noLabel" runat="server" Text='<%# Eval("[ord-no]") %>' BackColor="Turquoise" Width="80px" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px"></asp:Label>
                        &nbsp; &nbsp; &nbsp; 
                        Date:
                        <asp:Label ID="ord_dateLabel" runat="server" Text='<%# Bind("[ord-date]","{0:d}") %>' BackColor="Turquoise" Width="80px" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px"></asp:Label>
                        &nbsp; &nbsp; &nbsp; &nbsp;Type:
                        <asp:Label ID="typeLabel" runat="server" Text='<%# Bind("type") %>' BackColor="Turquoise" Width="80px" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px"></asp:Label>
                        &nbsp; &nbsp; &nbsp; Status:
                        <asp:Label ID="statLabel" runat="server" Text='<%# Bind("stat") %>' BackColor="Turquoise" Width="80px" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px"></asp:Label><br />
                        <br />
                        
                        
                    </ItemTemplate>
                    <FooterStyle BackColor="#507CD1" Font-Bold="True" ForeColor="White" />
                    <EditRowStyle BackColor="#2461BF" />
                    <RowStyle BackColor="#EFF3FB" />
                    <PagerStyle BackColor="#2461BF" ForeColor="White" HorizontalAlign="Center" />
                    <HeaderStyle BackColor="#507CD1" Font-Bold="True" ForeColor="White" />
                </asp:FormView>
                <asp:ObjectDataSource ID="ObjectDataSource2" runat="server" OldValuesParameterFormatString="original_{0}"
                    SelectMethod="Selectvieworder" TypeName="Order">
                    <SelectParameters>
                        <asp:Parameter Name="prmUser" Type="String" />
                        <asp:SessionParameter Name="prmOrderNum" SessionField="viewitem" Type="String" />
                    </SelectParameters>
                </asp:ObjectDataSource><asp:ObjectDataSource ID="ObjectDataSource1" OldValuesParameterFormatString="original_{0}" runat="server" SelectMethod="Selectview" TypeName="Order" >
                    <SelectParameters>
                        <asp:Parameter Name="prmUser" Type="String" />
                        <asp:Parameter DefaultValue="Select" Name="prmAction" Type="String" />
                        <asp:SessionParameter DefaultValue="" Name="prmOrderNum" SessionField="viewitem"
                            Type="String" />
                        <asp:SessionParameter DefaultValue="" Name="prmItemNum" SessionField="item" Type="String" />
                    </SelectParameters>
                </asp:ObjectDataSource>
               
            </td>
        </tr>
    </table>
    
    
            
                &nbsp;&nbsp;<asp:GridView ID="GridView1" runat="server" AllowPaging="True" AllowSorting="True"
                    AutoGenerateColumns="False" BorderStyle="Dotted" CssClass="Grid"
                    DataSourceID="ObjectDataSource1" EmptyDataText="No Record Found" Width="90%" OnSelectedIndexChanged="GridView1_SelectedIndexChanged" DataKeyNames="OrdNo,vLine,CustPart,Item1">
                    <EmptyDataRowStyle BorderColor="Gray" BorderStyle="Dotted" BorderWidth="0px" Font-Bold="True"
                        HorizontalAlign="Center" VerticalAlign="Middle" />
                    <Columns>
                        <asp:CommandField ButtonType="Image" HeaderText="Select" SelectImageUrl="~/Images/sel.gif"
                            SelectText="" ShowSelectButton="True">
                            <HeaderStyle ForeColor="White" />
                            <ItemStyle ForeColor="White" />
                        </asp:CommandField>
                        <asp:BoundField DataField="vLine" HeaderText="L#" SortExpression="vLine" >
                            <ItemStyle HorizontalAlign="Center" />
                        </asp:BoundField>
                        <asp:TemplateField HeaderText="Qty" SortExpression="quantity">
                            <EditItemTemplate>
                                <asp:TextBox ID="TextBox7" runat="server" Text='<%# Bind("quantity") %>'></asp:TextBox>
                            </EditItemTemplate>
                            <ItemTemplate>
                                <asp:Label ID="Label7" runat="server" Text='<%# Bind("quantity","{0:###,##0}") %>'></asp:Label>
                            </ItemTemplate>
                            <ItemStyle HorizontalAlign="Right" />
                        </asp:TemplateField>
                        <asp:TemplateField HeaderText="Price / UOM" SortExpression="price">
                            <EditItemTemplate>
                                <asp:TextBox ID="TextBox5" runat="server" Text='<%# Bind("price") %>'></asp:TextBox>
                            </EditItemTemplate>
                            <ItemTemplate>
                                <asp:Label ID="Label5" runat="server" Text='<%# Bind("price","{0:###,##0.00}") %>'></asp:Label>
                                <br />
                                <asp:Label ID="Label6" runat="server" Text='<%# Bind("uom") %>'></asp:Label>
                            </ItemTemplate>
                            <ItemStyle HorizontalAlign="Right" Wrap="False" Width="60px" />
                            <HeaderStyle Wrap="False" />
                            <FooterStyle Wrap="False" />
                        </asp:TemplateField>
                        <asp:TemplateField HeaderText="Item # /  Part #" SortExpression="Item1">
                            <EditItemTemplate>
                                <asp:TextBox ID="TextBox1" runat="server" Text='<%# Bind("Item1") %>'></asp:TextBox>
                            </EditItemTemplate>
                            <ItemTemplate>
                                <asp:Label ID="Label2" runat="server" Text='<%# Bind("Item1") %>'></asp:Label>
                                <br /> 
                              <asp:Label ID="Label11" runat="server" Text='<%# Bind("CustPart") %>'></asp:Label>  
                            </ItemTemplate>
                            <ItemStyle Wrap="False" />
                            <HeaderStyle Wrap="False" />
                            <FooterStyle Wrap="False" />
                        </asp:TemplateField>
                        <asp:TemplateField HeaderText="Cust Part#" SortExpression="CustPart">
                               <ItemTemplate>                               
                              <asp:Label ID="Labelcustpart" runat="server" Text='<%# Bind("CustPart") %>'></asp:Label>  
                            </ItemTemplate>
                            <ItemStyle Wrap="False" />
                            <HeaderStyle Wrap="False" />
                            <FooterStyle Wrap="False" />
                        </asp:TemplateField>
                        <asp:TemplateField HeaderText="Item Name#" SortExpression="Name1">
                               <ItemTemplate>                               
                              <asp:Label ID="Label3" runat="server" Text='<%# Bind("Name1") %>'></asp:Label> 
                            </ItemTemplate>
                            <ItemStyle Wrap="False" />
                            <HeaderStyle Wrap="False" />
                            <FooterStyle Wrap="False" />
                        </asp:TemplateField>
                                              
                        <asp:TemplateField HeaderText="Part Description" SortExpression="Dscr">
                            <ItemTemplate>                                
                                <asp:Label ID="Label4" runat="server" Text='<%# Bind("Dscr") %>'></asp:Label>
                            </ItemTemplate>
                            <ItemStyle Wrap="False" />
                            <HeaderStyle Wrap="False" />
                            <FooterStyle Wrap="False" />
                        </asp:TemplateField>
                         <asp:BoundField DataField="custpo" HeaderText="Cust Po #" SortExpression="custpo" />
                        <asp:TemplateField HeaderText="Due Date" SortExpression="requestdate">
                            <EditItemTemplate>
                                <asp:TextBox ID="TextBox2" runat="server" Text='<%# Bind("requestdate") %>'></asp:TextBox>
                            </EditItemTemplate>
                            <ItemTemplate>
                                <asp:Label ID="Label8" runat="server" Text='<%# Bind("requestdate","{0:d}") %>'></asp:Label>
                            </ItemTemplate>
                            <ItemStyle Width="60px" />
                        </asp:TemplateField>
                        <asp:BoundField DataField="est-no" HeaderText="Est #" SortExpression="est-no" />
                        <asp:BoundField DataField="job-no" HeaderText="Job #" SortExpression="job-no" />
                        <asp:TemplateField HeaderText="Disc" SortExpression="discount">
                            <EditItemTemplate>
                                <asp:TextBox ID="TextBox6" runat="server" Text='<%# Bind("discount") %>'></asp:TextBox>
                            </EditItemTemplate>
                            <ItemStyle HorizontalAlign="Right" />
                            <ItemTemplate>
                                <asp:Label ID="Label10" runat="server" Text='<%# Bind("discount","{0:###,##0.00}") %>'></asp:Label>
                            </ItemTemplate>
                        </asp:TemplateField>
                        <asp:TemplateField HeaderText="Total Price" SortExpression="extprice">
                            <EditItemTemplate>
                                <asp:TextBox ID="TextBox4" runat="server" Text='<%# Bind("extprice") %>'></asp:TextBox>
                            </EditItemTemplate>
                            <ItemStyle HorizontalAlign="Right" Width="65px" />
                            <ItemTemplate>
                                <asp:Label ID="Label9" runat="server" Text='<%# Bind("extprice","{0:###,##0.00}") %>'></asp:Label>
                            </ItemTemplate>
                        </asp:TemplateField>
                        <asp:CheckBoxField DataField="taxable" HeaderText="Tax" SortExpression="taxable" />
                        
                        <asp:TemplateField HeaderText="reckey" Visible="false">
                            
                            <ItemTemplate>
                                <asp:Label ID="Label_vReckey" runat="server" Text='<%# Bind("vReckey") %>'></asp:Label>
                            </ItemTemplate>
                        </asp:TemplateField>
                       
                    </Columns>
                    <RowStyle CssClass="shade" />
                    <SelectedRowStyle CssClass="GridSelected" BackColor="Yellow" />
                    <HeaderStyle  CssClass="headcolor" ForeColor="White" HorizontalAlign="Center"
                        VerticalAlign="Middle" Wrap="False" />
                    <AlternatingRowStyle CssClass="GridItemOdd" />
                </asp:GridView>
                <br />
                <br />
                <br />
            
    <table class="shade"><tr><td style="width: 884px">
    <table>
    <tr>
    <td>
    <table>
    <tr>
   <td align="right" style="padding-right:4px;"><b>
       <asp:Button ID="btnonhand" OnClick="lnkonhand_Click" runat="server" Text="On Hand" Height="18px" />:</b></td>
    
    
    </tr>
    <tr><td align="right" style="padding-right:4px;"><b>Backordered:</b></td></tr>
    </table>
    </td>
    
    
    
    <td style="width: 149px">
    <asp:FormView ID="FormView1" runat="server" DataSourceID="ObjectDataSource3" CellPadding="4" ForeColor="#333333" Width="155px">
       
         <ItemTemplate>
             <table><tr><td style="width: 130px"><b><asp:Label ID="q_onhLabel" runat="server" Text='<%# Bind("[q-onh]") %>' BackColor="Turquoise" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" Width="137px"></asp:Label></b></td></tr>
             <tr><td style="width: 130px"><b> <asp:Label ID="q_backLabel" runat="server" Text='<%# Bind("[q-back]") %>' BackColor="Turquoise" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" Width="137px"></asp:Label></b></td></tr>
             </table>
           
           
        </ItemTemplate>
        <FooterStyle BackColor="#507CD1" Font-Bold="True" ForeColor="White" />
        <EditRowStyle BackColor="#2461BF" />
        <RowStyle BackColor="#EFF3FB" />
        <PagerStyle BackColor="#2461BF" ForeColor="White" HorizontalAlign="Center" />
        <HeaderStyle BackColor="#507CD1" Font-Bold="True" ForeColor="White" />
        
        
        
        
    </asp:FormView>
    
    
    </td>
   
    <td style="width: 151px">
    <table>
    <tr>
    <td align="right" style="padding-right:4px;width:155px;"><b>
        <asp:Button ID="btnonorder" runat="server" OnClick="lnkonorder_Click" Text="On Order" Height="18px" />
        <asp:Button ID="btnbrowsorder" runat="server" OnClick="lnkbrowsorder_Click" Text="BrowsPO" Height="18px" />
        :</b></td>
    </tr>
    <tr>
    <td align="right" style="padding-right:4px;"><b>Available:</b></td>
    </tr>
    </table>
    </td>
    <td style="width: 74px">
        <asp:FormView ID="FormView2" runat="server" DataSourceID="ObjectDataSource3" CellPadding="4" ForeColor="#333333" Width="155px">
       
         <ItemTemplate>
            <table><tr><td style="width: 132px"><b><asp:Label ID="q_onoLabel" runat="server" Text='<%# Bind("[q-ono]") %>' BackColor="Turquoise" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" Width="137px"></asp:Label></b></td></tr>
            <tr><td style="width: 132px"><b> <asp:Label ID="q_availLabel" runat="server" Text='<%# Bind("[q-avail]") %>' BackColor="Turquoise" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" Width="137px"></asp:Label></b></td></tr>
            </table> 
        
           
        </ItemTemplate>
        <FooterStyle BackColor="#507CD1" Font-Bold="True" ForeColor="White" />
        <EditRowStyle BackColor="#2461BF" />
        <RowStyle BackColor="#EFF3FB" />
        <PagerStyle BackColor="#2461BF" ForeColor="White" HorizontalAlign="Center" />
        <HeaderStyle BackColor="#507CD1" Font-Bold="True" ForeColor="White" />
            
      
    </asp:FormView>
    </td>
    <td>
    <table>
    <tr>
    <td align="right" style="padding-right:4px;"><b>
        <asp:Button ID="btnallocated" runat="server" OnClick="lnk_allocated_Click" Text="Allocated" Height="18px" />
        :</b></td>
    </tr>
    <tr>
    <td align="right" style="padding-right:4px;"><b>Re Order:</b></td>
    </tr>
    </table>
    </td>
    <td style="width: 154px">
    <asp:FormView ID="FormView3" runat="server" DataSourceID="ObjectDataSource3" Width="160px">
        
        <ItemTemplate>
            
          <table><tr><td style="width: 138px"><b> <asp:Label ID="q_allocLabel" runat="server" Text='<%# Bind("[q-alloc]") %>' BackColor="Turquoise" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" Width="137px"></asp:Label></b></td></tr>
          <tr><td style="width: 138px"><b> <asp:Label ID="ord_levelLabel" runat="server" Text='<%# Bind("[ord-level]") %>' BackColor="Turquoise" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" Width="137px"></asp:Label></b></td></tr>
          </table> 

        </ItemTemplate>
       
        
        </asp:FormView>
    </td>
    </tr>
    </table>
    
    </td>
    </tr>
    <tr>
        <td>&nbsp;</td>
    </tr>
    <tr>
        <td>
            <asp:Button ID="Btn_View" runat="server" Text="View" CssClass="buttonM" OnClick="Btn_View_Click" />
            <asp:Button ID="Btn_Add" runat="server" Text="Add" CssClass="buttonM" OnClick="Btn_Add_Click" />
            <asp:Button ID="Btn_Update" runat="server" Text="Update" CssClass="buttonM" OnClick="Btn_Update_Click" />            
            <asp:Button ID="Btn_Delete" runat="server" Text="Delete" CssClass="buttonM" OnClick="Btn_Delete_Click" OnClientClick="return confirm('Are you sure you want to delete this record!')" />
            <input type="button" id="price_button"  runat="server" value="Price" class="buttonM" OnClick="Price()" />            
            <input type="button" id="History_Button" value="History" runat="server" class="buttonM" onClick="history()" />
            <input type="button" id="Button_Stat" value="Stat" runat="server" class="buttonM" onClick="stat()" />
            
        </td>
    </tr>
    </table> 
    
    <asp:ObjectDataSource ID="ObjectDataSource3" runat="server" OldValuesParameterFormatString="original_{0}"
        SelectMethod="SelectviewSel" TypeName="Order"  >
        <SelectParameters>
            <asp:Parameter Name="prmUser" Type="String" />
            <asp:Parameter DefaultValue="Select" Name="prmAction" Type="String" />
        <asp:SessionParameter Name="prmOrderNum" SessionField="viewitem" Type="String" />
            <asp:SessionParameter  Name="prmItemNum" SessionField="line" Type="String" />
        </SelectParameters>
    </asp:ObjectDataSource>
    
    
    <br />
    
    
    

</asp:Content>