<%@ Page Language="C#" MasterPageFile="~/MasterPage.master" AutoEventWireup="true" Inherits="mischarge" Title="Misc Charge" Codebehind="mischarge.aspx.cs" %>
<asp:Content ID="Content1" ContentPlaceHolderID="ContentPlaceHolder1" Runat="Server">
    
    
    <table>
        <tr>
            <td style="width: 575px; height: 64px;">
                <asp:FormView ID="FormView1" runat="server" DataKeyNames="company,ord-no" DataSourceID="ObjectDataSource2" CellPadding="4" ForeColor="#333333" Font-Bold="True" Width="800px">
                    
                    <ItemTemplate>
                        Order#:
                        <asp:Label ID="ord_noLabel" runat="server" Text='<%# Eval("[ord-no]") %>' BorderColor="Black" ForeColor="DarkGreen" BackColor="Turquoise" Width="80px" BorderStyle="Solid" BorderWidth="1px"></asp:Label>
                        &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; 
                        Date:
                        <asp:Label ID="ord_dateLabel" runat="server" Text='<%# Bind("[ord-date]","{0:MM/dd/yyyy}") %>' ForeColor="DarkGreen" BackColor="Turquoise" Width="80px" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px"></asp:Label>
                        &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp;&nbsp; 
                        Type:
                        <asp:Label ID="typeLabel" runat="server" Text='<%# Bind("type") %>' ForeColor="DarkGreen" BackColor="Turquoise" Width="80px" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px"></asp:Label>
                        &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp;&nbsp; Status:
                        <asp:Label ID="statLabel" runat="server" Text='<%# Bind("stat") %>' ForeColor="DarkGreen" BackColor="Turquoise" Width="80px" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px"></asp:Label>
                        &nbsp; &nbsp; &nbsp;
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
                        <asp:SessionParameter Name="prmOrderNum" SessionField="charge" Type="String" />
                    </SelectParameters>
                </asp:ObjectDataSource>
            </td>
        </tr>
    </table>
    <asp:GridView ID="GridView1" runat="server" AllowSorting="True" AutoGenerateColumns="False"
    EmptyDataText="No Records Found" Width="88%" BorderStyle="Dotted" CssClass="Grid" DataSourceID="ObjectDataSource1" AllowPaging="True" style="position: relative" >
        <Columns>
            <asp:BoundField DataField="vItem" HeaderText="Fg Item" SortExpression="vItem" >
                <ItemStyle HorizontalAlign="Center" VerticalAlign="Middle" Wrap="False" />
                <HeaderStyle Wrap="False" />
                <FooterStyle Wrap="False" />
            </asp:BoundField>
            <asp:BoundField DataField="Charge" HeaderText="Charge" SortExpression="Charge" >
                <ItemStyle Wrap="False" />
                <HeaderStyle Wrap="False" />
                <FooterStyle Wrap="False" />
            </asp:BoundField>
            <asp:TemplateField HeaderText="Sell Price" SortExpression="Sell">
                <EditItemTemplate>
                    <asp:TextBox ID="TextBox1" runat="server" Text='<%# Bind("Sell") %>'></asp:TextBox>
                </EditItemTemplate>
                <ItemTemplate>
                    <asp:Label ID="Label1" runat="server" Text='<%# Bind("Sell", "{0:###,##0.00}") %>'></asp:Label>
                </ItemTemplate>
                <ItemStyle HorizontalAlign="Center" VerticalAlign="Middle" Width="50px" Wrap="False" />
                <HeaderStyle Wrap="False" />
            </asp:TemplateField>
            <asp:BoundField DataField="Dscr" HeaderText="Description" SortExpression="Dscr" >
                <ItemStyle Wrap="False" />
                <HeaderStyle Wrap="False" />
                <FooterStyle Wrap="False" />
            </asp:BoundField>
            <asp:BoundField DataField="Po" HeaderText="Customer Po#" SortExpression="Po" >
                <ItemStyle HorizontalAlign="Center" VerticalAlign="Middle" Width="130px" Wrap="False" />
                <HeaderStyle Wrap="False" />
            </asp:BoundField>
            <asp:BoundField DataField="Job" HeaderText="Job#" SortExpression="Job" >
                <ItemStyle HorizontalAlign="Center" VerticalAlign="Middle" Width="30px" Wrap="False" />
                <HeaderStyle Wrap="False" />
            </asp:BoundField>
            <asp:TemplateField SortExpression="Job2">
                <EditItemTemplate>
                    <asp:TextBox ID="TextBox2" runat="server" Text='<%# Bind("Job2") %>'></asp:TextBox>
                </EditItemTemplate>
                <ItemTemplate>
                    <asp:Label ID="Label2" runat="server" Text='<%# Bind("Job2","{0:00}") %>'></asp:Label>
                </ItemTemplate>
                <ItemStyle HorizontalAlign="Center" VerticalAlign="Middle" Width="20px" Wrap="False" />
                <HeaderStyle Wrap="False" />
            </asp:TemplateField>
            <asp:BoundField DataField="Tax" HeaderText="Tax" SortExpression="Tax" >
                <ItemStyle HorizontalAlign="Center" VerticalAlign="Middle" Width="10px" Wrap="False" />
                <HeaderStyle Wrap="False" />
            </asp:BoundField>
            <asp:BoundField DataField="Bill" HeaderText="Bill" SortExpression="Bill" >
                <ItemStyle HorizontalAlign="Center" VerticalAlign="Middle" Width="10px" Wrap="False" />
                <HeaderStyle Wrap="False" />
            </asp:BoundField>
        </Columns>
    <EmptyDataRowStyle BorderColor="Gray" BorderStyle="Dotted" BorderWidth="0px" Font-Bold="True"
            HorizontalAlign="Center" VerticalAlign="Middle" />
        <RowStyle CssClass="shade" />
        <SelectedRowStyle CssClass="GridSelected" />
        <HeaderStyle CssClass="headcolor" ForeColor="White" HorizontalAlign="Center"
            VerticalAlign="Middle" Wrap="False" />
        <AlternatingRowStyle CssClass="GridItemOdd" />
    </asp:GridView>
    <asp:ObjectDataSource ID="ObjectDataSource1" runat="server" OldValuesParameterFormatString="original_{0}"
        SelectMethod="MiscCharge" TypeName="Order">
        <SelectParameters>
            <asp:Parameter Name="prmUser" Type="String" />
            <asp:Parameter DefaultValue="Select" Name="prmAction" Type="String" />
            <asp:SessionParameter Name="prmOrderNum" SessionField="charge" Type="String" />
            <asp:SessionParameter Name="vLine" SessionField="line" Type="String" />
        </SelectParameters>
    </asp:ObjectDataSource>
    <br />
    <br />
        <asp:ObjectDataSource ID="ObjectDataSource4" runat="server" OldValuesParameterFormatString="original_{0}"
        SelectMethod="OrderTot" TypeName="Order">
        <SelectParameters>
            <asp:Parameter Name="prmUser" Type="String" />
            <asp:Parameter Name="prmAction" Type="String" DefaultValue="Select" />
            <asp:SessionParameter Name="prmOrderNum" SessionField="charge" Type="String" />
            <asp:Parameter Name="prmWeight" Type="Decimal" />            
            <asp:Parameter Name="prmTax" Type="Decimal" />
            <asp:Parameter Name="prmFreight" Type="Decimal" />
            <asp:Parameter Name="prmFbill" Type="String" />
            <asp:Parameter Name="prmRevenue" Type="Decimal" />
            <asp:Parameter Name="prmCost" Type="Decimal" />
            <asp:Parameter Name="prmComm" Type="Decimal" />  
        </SelectParameters>
    </asp:ObjectDataSource>
    <br />
    <br />
    <asp:FormView ID="FormView2" runat="server" OnDataBound="formview2_ondatabound" CellPadding="4" DataSourceID="ObjectDataSource4"
        ForeColor="#333333" Width="1034px">
        <FooterStyle BackColor="#507CD1" Font-Bold="True" ForeColor="White" />
        <EditRowStyle BackColor="#2461BF" />
        
        <ItemTemplate>
            
            <table>
            <tr>
            <td style="width: 61px" align="right"><b>Weight: </b></td>
            <td style="width: 87px"><b><asp:Label ID="WeightLabel" runat="server" Text='<%# Bind("Weight1") %>' BackColor="Turquoise" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" Width="80px"></asp:Label> </b></td>
            <td style="width: 67px" align="right"><b> </b></td>
            <td style="width: 124px"><b> </b></td>
            <td style="width: 66px" align="right"><b>Tax1: </b></td>
            <td style="width: 97px"><b> <asp:Label ID="Tax1Label" runat="server" Text='<%# Bind("Taxes") %>' BackColor="Turquoise" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" Width="80px"></asp:Label></b></td>
            </tr>
            <tr>
            <td style="width: 61px" align="right"><b>Freight: </b></td>
            <td style="width: 87px"><b><asp:Label ID="FreightLabel" runat="server" Text='<%# Bind("Freig") %>' BackColor="Turquoise" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" Width="80px"></asp:Label> </b></td>
            <td style="width: 67px" align="right"><b>Bill Freight: </b></td>
            <td style="width: 124px"><b> <asp:CheckBox ID="BillFreCheckBox" runat="server" Checked='<%# Bind("BillF") %>'
                Enabled="false" /></b></td>
            <td style="width: 66px" align="right"><b>Order Total: </b></td>
            <td style="width: 97px"><b><asp:Label ID="TotalLabel" runat="server" Text='<%# Bind("Tot") %>' BackColor="Turquoise" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" Width="80px"></asp:Label> </b></td>
            
            </tr>
            <tr>
            <td colspan="3"><b></b></td>
            <td style="width: 66px"><b> </b></td>
           <td style="width: 66px" align="right"><b><asp:Label id="Ordercostlabel" runat="server" Text="Order Cost:"></asp:Label></b></td>
            <td style="width: 97px"><b><asp:Label ID="OrderCosLabel" runat="server" Text='<%# Bind("Cos") %>' BackColor="Turquoise" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" Width="80px"></asp:Label> </b></td>
            
            </tr>
            
            </table>
           
        </ItemTemplate>
        <HeaderStyle BackColor="#507CD1" Font-Bold="True" ForeColor="White" />
        <RowStyle BackColor="#EFF3FB" />
        <PagerStyle BackColor="#2461BF" ForeColor="White" HorizontalAlign="Center" />
        
    </asp:FormView>
    <%--<asp:FormView ID="FormView2" runat="server" DataSourceID="ObjectDataSource1" CellPadding="4" ForeColor="#333333" Width="800px" >
        
        <ItemTemplate>
        <table>
        <tr>
        <td style="width: 81px" align="right"><b> Weight:</b></td>
        <td style="width: 239px"><b><asp:Label ID="WeightLabel" runat="server" Text='<%# Bind("Weight","{0:###,##0.00}") %>' BackColor="Turquoise" Width="80px" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px"></asp:Label> </b></td>
        <td style="width: 150px"><b> </b></td>
        <td style="width: 143px"><b> </b></td>
        <td style="width: 80px" align="right"><b>Tax: </b></td>
        <td style="width: 226px"><b><asp:Label ID="Tax1Label" runat="server" Text='<%# Bind("Tax1","{0:###,##0.00}") %>' BackColor="Turquoise" Width="80px" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px"></asp:Label>  </b></td>
        </tr>
        <tr>
        <td style="width: 81px" align="right"><b>Freight: </b></td>
        <td style="width: 239px"><b> <asp:Label ID="FreightLabel" runat="server" Text='<%# Bind("Freight","{0:###,##0.00}") %>' BackColor="Turquoise" Width="80px" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px"></asp:Label></b></td>
        <td style="width: 150px"><b><asp:CheckBox ID="BillFreCheckBox" runat="server" Checked='<%# Bind("BillFre") %>'
                Enabled="false" />Bill Freight? </b></td>
        <td style="width: 143px"><b> </b></td>
        <td style="width: 80px" align="right"><b>Order Total: </b></td>
        <td style="width: 226px"><b><asp:Label ID="TotalLabel" runat="server" Text='<%# Bind("Total","{0:###,##0.00}") %>' BackColor="Turquoise" Width="80px" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px"></asp:Label> </b></td>
        </tr>
        </table>
        
        </ItemTemplate>
        <FooterStyle BackColor="#507CD1" Font-Bold="True" ForeColor="White" />
        <EditRowStyle BackColor="#2461BF" />
        <RowStyle BackColor="#EFF3FB" />
        <PagerStyle BackColor="#2461BF" ForeColor="White" HorizontalAlign="Center" />
        <HeaderStyle BackColor="#507CD1" Font-Bold="True" ForeColor="White" />
        <EditItemTemplate>
            Weight:
            <asp:TextBox ID="WeightTextBox" runat="server" Text='<%# Bind("Weight") %>'>
            </asp:TextBox><br />
            Tax1:
            <asp:TextBox ID="Tax1TextBox" runat="server" Text='<%# Bind("Tax1") %>'>
            </asp:TextBox><br />
            Freight:
            <asp:TextBox ID="FreightTextBox" runat="server" Text='<%# Bind("Freight") %>'>
            </asp:TextBox><br />
            BillFre:
            <asp:CheckBox ID="BillFreCheckBox" runat="server" Checked='<%# Bind("BillFre") %>' /><br />
            Total:
            <asp:TextBox ID="TotalTextBox" runat="server" Text='<%# Bind("Total") %>'>
            </asp:TextBox><br />
            Cost:
            <asp:TextBox ID="CostTextBox" runat="server" Text='<%# Bind("Cost") %>'>
            </asp:TextBox><br />
            <asp:LinkButton ID="UpdateButton" runat="server" CausesValidation="True" CommandName="Update"
                Text="Update">
            </asp:LinkButton>
            <asp:LinkButton ID="UpdateCancelButton" runat="server" CausesValidation="False" CommandName="Cancel"
                Text="Cancel">
            </asp:LinkButton>
        </EditItemTemplate>
        <InsertItemTemplate>
            Weight:
            <asp:TextBox ID="WeightTextBox" runat="server" Text='<%# Bind("Weight") %>'>
            </asp:TextBox><br />
            Tax1:
            <asp:TextBox ID="Tax1TextBox" runat="server" Text='<%# Bind("Tax1") %>'>
            </asp:TextBox><br />
            Freight:
            <asp:TextBox ID="FreightTextBox" runat="server" Text='<%# Bind("Freight") %>'>
            </asp:TextBox><br />
            BillFre:
            <asp:CheckBox ID="BillFreCheckBox" runat="server" Checked='<%# Bind("BillFre") %>' /><br />
            Total:
            <asp:TextBox ID="TotalTextBox" runat="server" Text='<%# Bind("Total") %>'>
            </asp:TextBox><br />
            Cost:
            <asp:TextBox ID="CostTextBox" runat="server" Text='<%# Bind("Cost") %>'>
            </asp:TextBox><br />
            <asp:LinkButton ID="InsertButton" runat="server" CausesValidation="True" CommandName="Insert"
                Text="Insert">
            </asp:LinkButton>
            <asp:LinkButton ID="InsertCancelButton" runat="server" CausesValidation="False" CommandName="Cancel"
                Text="Cancel">
            </asp:LinkButton>
        </InsertItemTemplate>
    </asp:FormView>--%>
    <asp:ObjectDataSource ID="ObjectDataSource3" runat="server" OldValuesParameterFormatString="original_{0}"
         SelectMethod="MiscCharge1" TypeName="Order">
        <SelectParameters>
            <asp:Parameter Name="prmUser" Type="String" />
            <asp:Parameter DefaultValue="Select" Name="prmAction" Type="String" />
            <asp:Parameter Name="prmSell" Type="String" />
        </SelectParameters>
    </asp:ObjectDataSource>
</asp:Content>

