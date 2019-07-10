<%@ Page Language="C#" MasterPageFile="~/MasterPage3.master" AutoEventWireup="true" Inherits="binjobs" Title="BinJobs" Codebehind="binjobs.aspx.cs" %>
<asp:Content ID="Content1" ContentPlaceHolderID="ContentPlaceHolder1" Runat="Server">
    <br />
    <asp:LinkButton OnClick="LinkButton_Click" ID="LinkButton1" runat="server">List Order</asp:LinkButton>
    <asp:LinkButton OnClick="LinkButton2_Click" ID="LinkButton2" runat="server">List Order</asp:LinkButton>
    <asp:LinkButton OnClick="LinkButton3_Click" ID="LinkButton3" runat="server">List Order</asp:LinkButton>
    <br />
      
   
    <asp:FormView ID="FormView2" runat="server" DataSourceID="ObjectDataSource2" Width="1024px" CellPadding="4" ForeColor="#333333">
       
        <ItemTemplate>
          <Table style="width: 948px">
              <tr>
                  <td align="left" style="width: 71px">
                      <strong>Customer:</strong></td>
                  <td style="width: 88px">
                      <asp:Label ID="custLabel" runat="server" Text='<%# Bind("cust") %>' BackColor="Turquoise" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" Width="80px" style="position: relative"></asp:Label></td>
              </tr>
          
          <tr><td style="width: 71px; height: 3px" align="left"><b>
              <br />
              Item#:</b></td>
          <td style="width: 88px; height: 3px"><b><br /><asp:Label ID="i_noLabel" runat="server" Text='<%# Bind("[i-no]") %>' BackColor="Turquoise" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" Width="100px" style="position: relative"></asp:Label></b></td>
          <td colspan="2" style="height: 3px"><b><br /></b><b><asp:Label ID="i_nameLabel" runat="server" Text='<%# Bind("[i-name]") %>' BackColor="Turquoise" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" Width="125px" style="position: relative"></asp:Label></td>
          </tr>
          
           <tr><td style="width: 71px"><b>On Hand:</b></td>
           <td style="width: 88px"><b>Job/Po:</b></td>
           <td><b>Allocated:</b></td>
           <td><b>BackOrdered:</b></td>
           <td><b>Available:</b></td>
           <td><b> ReOrderLev:</b></td>
           <td><b>ReOrderMin:</b></td>
           <td><b> ReOrderMax:</b></td></tr> 
           
           <tr><td style="width: 71px"><b> <asp:Label ID="q_onhLabel" runat="server" Text='<%# Bind("[q-onh]","{0:###,##0}") %>' BackColor="Turquoise" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" Width="80px"></asp:Label><br /></b></td>
           <td style="width: 88px"><b><asp:Label ID="q_onoLabel" runat="server" Text='<%# Bind("[q-ono]","{0:###,##0}") %>' BackColor="Turquoise" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" Width="80px"></asp:Label><br /></b></td>
           
           <td><b><asp:Label ID="q_allocLabel" runat="server" Text='<%# Bind("[q-alloc]","{0:###,##0}") %>' BackColor="Turquoise" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" Width="80px"></asp:Label><br /></b></td>
           <td><b><asp:Label ID="q_backLabel" runat="server" Text='<%# Bind("[q-back]","{0:###,##0}") %>' BackColor="Turquoise" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" Width="80px"></asp:Label><br /></b></td>
           <td><b> <asp:Label ID="q_availLabel" runat="server" Text='<%# Bind("[q-avail]","{0:###,##0}") %>' BackColor="Turquoise" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" Width="80px"></asp:Label><br /></b></td>
           <td><b><asp:Label ID="ord_levelLabel" runat="server" Text='<%# Bind("[ord-level]","{0:###,##0}") %>' BackColor="Turquoise" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" Width="80px"></asp:Label><br />
            </b></td>
           <td><b><asp:Label ID="ord_minLabel" runat="server" Text='<%# Bind("[ord-min]","{0:###,##0}") %>' BackColor="Turquoise" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" Width="80px"></asp:Label><br /></b></td>
           <td><b> <asp:Label ID="ord_maxLabel" runat="server" Text='<%# Bind("[ord-max]","{0:###,##0}") %>' BackColor="Turquoise" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" Width="80px"></asp:Label><br /></b></td></tr>
            
              
        </Table>
        </ItemTemplate>
        <FooterStyle BackColor="#507CD1" Font-Bold="True" ForeColor="White" />
        <EditRowStyle BackColor="#2461BF" />
        <RowStyle BackColor="#EFF3FB" />
        <PagerStyle BackColor="#2461BF" ForeColor="White" HorizontalAlign="Center" />
        <HeaderStyle BackColor="#507CD1" Font-Bold="True" ForeColor="White" />
      
    </asp:FormView>
    <asp:ObjectDataSource ID="ObjectDataSource2" runat="server" OldValuesParameterFormatString="original_{0}" SelectMethod="SelectBinJobUp" TypeName="Binitem">
        <SelectParameters>
            <asp:Parameter Name="prmUser" Type="String" /> 
            <asp:Parameter DefaultValue="Select" Name="prmAction" Type="String" />
            <asp:Parameter DefaultValue="" Name="prmOrderNum"  Type="String" />
            <asp:SessionParameter  Name="prmItemNum" SessionField="item_list_item" Type="String" />
        </SelectParameters>
    </asp:ObjectDataSource>
   
   <br />
    <asp:GridView ID="GridView1" runat="server" AllowPaging="True" AllowSorting="True"
        AutoGenerateColumns="False" CssClass="Grid" DataSourceID="ObjectDataSource1"
        EmptyDataText="No Record Found" Width="98%">
        <EmptyDataRowStyle BorderColor="Gray" BorderStyle="Dotted" BorderWidth="0px" Font-Bold="True"
            HorizontalAlign="Center" VerticalAlign="Middle" />
        <Columns>
            <asp:BoundField DataField="job-no-disp" HeaderText="Job#" SortExpression="job-no-disp" >
                <ItemStyle Wrap="False" />
            </asp:BoundField>
            <%--<asp:BoundField DataField="job-no2" HeaderText="" SortExpression="job-no" >
                <ItemStyle Wrap="False" />
            </asp:BoundField>--%>
            <asp:BoundField DataField="i-no" HeaderText="Item#" SortExpression="i-no" >
                <ItemStyle Wrap="False" />
            </asp:BoundField>
            <asp:BoundField DataField="tag" HeaderText="Tag" SortExpression="tag" >
                <ItemStyle Wrap="False" />
                <HeaderStyle Wrap="False" />
                <FooterStyle Wrap="False" />
            </asp:BoundField>
            <asp:BoundField DataField="loc" HeaderText="Whse" SortExpression="loc" >
                <ItemStyle Wrap="False" />
            </asp:BoundField>
            <asp:BoundField DataField="loc-bin" HeaderText="Bin" SortExpression="loc-bin" >
                <ItemStyle Wrap="False" />
            </asp:BoundField>
            
            <asp:BoundField DataField="cases" HeaderText="Units" SortExpression="cases" >
                <ItemStyle Wrap="False" />
            </asp:BoundField>
            <asp:BoundField DataField="case-count" HeaderText="Unit Count" SortExpression="case-count" >
                <ItemStyle Wrap="False" />
            </asp:BoundField>
            <asp:BoundField DataField="cases-unit" HeaderText="Unit/Pallet" SortExpression="cases-unit" >
                <ItemStyle Wrap="False" />
            </asp:BoundField>
            <asp:BoundField DataField="partial-count" HeaderText="Partial" SortExpression="partial-count" >
                <ItemStyle Wrap="False" />
            </asp:BoundField>
            <asp:TemplateField HeaderText="Total Qty" SortExpression="qty">
                <EditItemTemplate>
                    <asp:TextBox ID="TextBox6" runat="server" Text='<%# Bind("qty") %>'></asp:TextBox>
                </EditItemTemplate>
                <ItemTemplate>
                    <asp:Label ID="Label6" runat="server" Text='<%# Bind("qty","{0:###,##0}") %>'></asp:Label>
                </ItemTemplate>
                <ItemStyle Wrap="False" />
            </asp:TemplateField>
            <asp:BoundField DataField="rel-qty" HeaderText="Releases" SortExpression="rel-qty" >
                <ItemStyle Wrap="False" />
            </asp:BoundField>
            <asp:BoundField DataField="bol-qty" HeaderText="BOL Qty" SortExpression="bol-qty" >
                <ItemStyle Wrap="False" />
            </asp:BoundField>
            <asp:TemplateField HeaderText="Avail To Release" SortExpression="avl-qty">
                <EditItemTemplate>
                    <asp:TextBox ID="TextBox7" runat="server" Text='<%# Bind("[avl-qty]") %>'></asp:TextBox>
                </EditItemTemplate>
                <ItemTemplate>
                    <asp:Label ID="Label7" runat="server" Text='<%# Bind("[avl-qty]","{0:###,##0}") %>'></asp:Label>
                </ItemTemplate>
                <ItemStyle Wrap="False" />
            </asp:TemplateField>
            
            
            <asp:BoundField DataField="cust-no" HeaderText="Cust#" SortExpression="cust-no" >
                <ItemStyle Wrap="False" />
            </asp:BoundField>
        </Columns>
        <RowStyle CssClass="shade" />
        <SelectedRowStyle CssClass="GridSelected" BackColor="Yellow" />
        <HeaderStyle ForeColor="White" CssClass="headcolor" HorizontalAlign="Center"
            VerticalAlign="Middle" Wrap="False" />
        <AlternatingRowStyle CssClass="GridItemOdd" />
    </asp:GridView>
    <asp:ObjectDataSource ID="ObjectDataSource1" runat="server" OldValuesParameterFormatString="original_{0}"
        SelectMethod="SelectBin" TypeName="Binitem">
        <SelectParameters>
            <asp:Parameter Name="prmUser" Type="String" />
            <asp:Parameter DefaultValue="Select" Name="prmAction" Type="String" />
            <asp:Parameter DefaultValue="" Name="prmOrderNum"  Type="String" />
            <asp:SessionParameter  Name="prmItemNum" SessionField="item_list_item" Type="String" />
        </SelectParameters>
    </asp:ObjectDataSource>

</asp:Content>

