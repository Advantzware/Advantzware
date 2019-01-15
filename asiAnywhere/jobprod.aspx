<%@ Page Language="C#" MasterPageFile="~/MasterPage2.master" AutoEventWireup="true" Inherits="jobprodaspx" Title="Job Prod" Codebehind="jobprod.aspx.cs" %>
<asp:Content ID="Content1" ContentPlaceHolderID="ContentPlaceHolder1" Runat="Server">
<div>
    <br />
    <asp:LinkButton OnClick="LinkButton_Click" ID="LinkButton1" runat="server">List Order</asp:LinkButton>
    <asp:LinkButton OnClick="LinkButton2_Click" ID="LinkButton2" runat="server">List Order</asp:LinkButton>
    
    <asp:LinkButton OnClick="LinkButton3_Click" ID="LinkButton3" runat="server">List Order</asp:LinkButton>
    <br />
    <asp:FormView ID="FormView1" runat="server" DataSourceID="ObjectDataSource2" Width="140px" CellPadding="4" ForeColor="#333333" Height="84px">
        <ItemTemplate>
          <table style="width: 900px">
        <tr><td align="right"><b>Job:</b></td>
        <td style="width: 87px"><b><asp:Label ID="JobLabel" runat="server" Text='<%# Bind("Job") %>' BackColor="Turquoise" Width="80px" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px"></asp:Label></b></td>
        <td style="width: 111px"><b><asp:Label ID="Job2Label" runat="server" Text='<%# Bind("Job2") %>' BackColor="Turquoise" Width="80px" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px"></asp:Label></b></td>
        <td><b></b></td>
        
        <td style="width: 31px" align="right"><b>Est:</b></td>
        <td style="width: 94px"><b><asp:Label ID="estLabel" runat="server" Text='<%# Bind("est") %>' BackColor="Turquoise" Width="80px" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px"></asp:Label></b></td>
        <td style="width: 71px" align="right"><b>Status:</b></td>
        <td style="width: 94px"><b><asp:Label ID="StatsLabel" runat="server" Text='<%# Bind("Stats") %>' BackColor="Turquoise" Width="80px" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px"></asp:Label></b></td>
        
        <td style="width: 147px" align="right"><b>Start :</b></td>
        <td style="width: 123px"><b><asp:Label ID="StartdateLabel" runat="server" Text='<%# Bind("Startdate","{0:MM/dd/yyyy}") %>' BackColor="Turquoise" Width="80px" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px"></asp:Label></b></td>
            <td style="width: 123px">
            </td>
        </tr>
            <tr><td align="right"><b></b></td>
            <td style="width: 87px"><b></b></td>
            <td style="width: 111px"><b></b></td>
            <td><b></b></td>
            <td style="width: 31px" align="right"><b></b></td>
            <td style="width: 94px"><b></b></td>
            <td style="width: 71px" align="right"><b>User Id:</b></td>
            <td style="width: 94px"><b><asp:Label ID="Label1" runat="server" Text='<%# Bind("vUser") %>' BackColor="Turquoise" Width="80px" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px"></asp:Label></b></td>
         <td style="width: 147px" align="right"><b>Close :</b></td> 
         <td style="width: 123px"><b><asp:Label ID="EnddateLabel" runat="server" Text='<%# Bind("Enddate","{0:MM/dd/yyyy}") %>' BackColor="Turquoise" Width="80px" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" ></asp:Label></b></td>  
                <td style="width: 123px">
                </td>
            </tr>
            <tr><td align="right"><b></b></td>
            <td style="width: 87px"><b></b></td>
            <td style="width: 111px"><b></b></td>
            <td><b></b></td>
            <td style="width: 31px" align="right"><b></b></td>
            <td style="width: 94px"><b></b></td>
            <td style="width: 71px" align="right"><b></b></td>
            <td style="width: 94px"><b></b></td>
            <td style="width: 147px" align="right"><b>Due Date:</b></td>
            <td style="height: 18px; width: 123px;"><b><asp:Label ID="DuedateLabel" runat="server" Text='<%# Bind("Duedate"," {0:MM/dd/yyyy}") %>' BackColor="Turquoise" Width="80px" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px"></asp:Label></b></td>
                <td style="width: 123px; height: 18px">
                </td>
            </tr>
            
                    
            
           
        </table>
        </ItemTemplate>
        <FooterStyle BackColor="#507CD1" Font-Bold="True" ForeColor="White" />
        <EditRowStyle BackColor="#2461BF" />
        <RowStyle BackColor="#EFF3FB" />
        <PagerStyle BackColor="#2461BF" ForeColor="White" HorizontalAlign="Center" />
        <HeaderStyle BackColor="#507CD1" Font-Bold="True" ForeColor="White" />
             
       
    </asp:FormView>
   <br />
    <table class="shade" align="left" style="width: 908px">
    <tr>
    <td align="center" style="width: 774px; height: 18px"><b><asp:Label ID="Label2" runat="server" Text="Job Line Items" ></asp:Label></b></td>
    </tr>
    </table>
    <br />
    <br />
    
    <asp:GridView ID="GridView1" runat="server" AutoGenerateColumns="False" DataKeyNames="Order,Item" DataSourceID="ObjectDataSource1" AllowPaging="True" AllowSorting="True" BorderStyle="Dotted" CssClass="Grid" EmptyDataText="No Record Found" Width="75%">
        <Columns>
            <asp:BoundField DataField="Sheet" HeaderText="Sheet" SortExpression="Sheet" >
                <ItemStyle Wrap="False" />
            </asp:BoundField>
            <asp:BoundField DataField="BlankNum" HeaderText="BlankNum" SortExpression="BlankNum" >
                <ItemStyle Wrap="False" />
            </asp:BoundField>
            <asp:BoundField DataField="Customer" HeaderText="Customer" SortExpression="Customer" >
                <ItemStyle Wrap="False" />
            </asp:BoundField>
            <asp:BoundField DataField="Item" HeaderText="Item" SortExpression="Item" >
                <ItemStyle Wrap="False" />
            </asp:BoundField>
            <asp:TemplateField HeaderText="Quantity" SortExpression="Qty">
                
                <ItemTemplate>
                    <asp:Label ID="Label1" runat="server" Text='<%# Bind("Qty","{0:###,###.##}") %>'></asp:Label>
                </ItemTemplate>
                <ItemStyle Wrap="False" />
            </asp:TemplateField>
            <asp:TemplateField HeaderText="SqInch%" SortExpression="Inch">
                
                <ItemTemplate>
                    <asp:Label ID="Label2" runat="server" Text='<%# Bind("Inch","{0:####.00}") %>'></asp:Label>
                </ItemTemplate>
                <ItemStyle Wrap="False" />
            </asp:TemplateField>
            <asp:BoundField DataField="Order" HeaderText="Order" SortExpression="Order" >
                <ItemStyle Wrap="False" />
            </asp:BoundField>
            </Columns>
        <EmptyDataRowStyle BorderColor="Gray" BorderStyle="Dotted" BorderWidth="0px" Font-Bold="True"
            HorizontalAlign="Center" VerticalAlign="Middle" />
        <RowStyle CssClass="shade" />
        <SelectedRowStyle CssClass="GridSelected" />
        <HeaderStyle  ForeColor="White" CssClass="headcolor" HorizontalAlign="Center"
            VerticalAlign="Middle" Wrap="False" />
        <AlternatingRowStyle CssClass="GridItemOdd" />
    </asp:GridView>
    <asp:ObjectDataSource ID="ObjectDataSource1" runat="server" OldValuesParameterFormatString="original_{0}"
        SelectMethod="jobhdr" TypeName="jobprod">
        <SelectParameters>
            <asp:Parameter Name="prmUser" Type="String" />
            <asp:SessionParameter Name="prmOrderNum" SessionField="Material" Type="String" />
            <asp:SessionParameter Name="vLine" SessionField="line" Type="String" />
        </SelectParameters>
    </asp:ObjectDataSource>
    <asp:ObjectDataSource ID="ObjectDataSource2" runat="server" OldValuesParameterFormatString="original_{0}"
        SelectMethod="job" TypeName="jobprod">
        <SelectParameters>
            <asp:Parameter Name="prmUser" Type="String" />
            <asp:SessionParameter Name="prmOrderNum" SessionField="Material" Type="String" />
            <asp:SessionParameter Name="vLine" SessionField="line" Type="String" />
        </SelectParameters>
    </asp:ObjectDataSource>

</div>
</asp:Content>

