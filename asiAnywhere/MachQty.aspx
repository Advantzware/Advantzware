<%@ Page Language="C#" MasterPageFile="~/MasterPage2.master" AutoEventWireup="true" Inherits="MachQty" Title="Jobs For Order Line Item" Codebehind="MachQty.aspx.cs" %>
<asp:Content ID="Content1" ContentPlaceHolderID="ContentPlaceHolder1" Runat="Server">
    <div>
    <br />
  
    <asp:LinkButton OnClick="LinkButton_Click" ID="LinkButton1" runat="server">List Order</asp:LinkButton>
    <asp:LinkButton OnClick="LinkButton2_Click" ID="LinkButton2" runat="server">List Order</asp:LinkButton>
    <asp:LinkButton OnClick="LinkButton3_Click" ID="LinkButton3" runat="server">List Order</asp:LinkButton>
    <br />
    <asp:FormView ID="FormView1" runat="server" DataSourceID="ObjectDataSource2" Width="810px" CellPadding="4" ForeColor="#333333" Height="98px">
        <ItemTemplate>
          <table style="width: 1057px">
        <tr><td align="right" style="width: 19px"><b>Job:</b></td>
        <td style="width: 91px"><b><asp:Label ID="JobLabel" runat="server" Text='<%# Bind("Job") %>' BackColor="Turquoise" Width="80px" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px"></asp:Label></b></td>
        <td style="width: 97px"><b><asp:Label ID="Job2Label" runat="server" Text='<%# Bind("Job2") %>' BackColor="Turquoise" Width="80px" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px"></asp:Label></b></td>
        <td style="width: 2px"><b></b></td>
        
        <td style="width: 27px" align="right"><b>Est:</b></td>
        <td style="width: 89px"><b><asp:Label ID="estLabel" runat="server" Text='<%# Bind("est") %>' BackColor="Turquoise" Width="80px" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px"></asp:Label></b></td>
        <td style="width: 44px" align="right"><b>Status:</b></td>
        <td style="width: 92px"><b><asp:Label ID="StatsLabel" runat="server" Text='<%# Bind("Stats") %>' BackColor="Turquoise" Width="80px" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px"></asp:Label></b></td>
        
        <td style="width: 52px" align="right"><b>Start :</b></td>
        <td style="width: 416px"><b><asp:Label ID="StartdateLabel" runat="server" Text='<%# Bind("Startdate", "{0:MM/dd/yyyy}") %>' BackColor="Turquoise" Width="80px" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px"></asp:Label></b></td></tr>
            <tr><td align="right" style="width: 19px"><b></b></td>
            <td style="width: 91px"><b></b></td>
            <td style="width: 97px"><b></b></td>
            <td style="width: 2px"><b></b></td>
            <td style="width: 27px" align="right"><b></b></td>
            <td style="width: 89px"><b></b></td>
            <td style="width: 44px" align="right"><b>User Id:</b></td>
            <td style="width: 92px"><b><asp:Label ID="Label1" runat="server" Text='<%# Bind("vUser") %>' BackColor="Turquoise" Width="80px" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px"></asp:Label></b></td>
         <td style="width: 52px" align="right"><b>Close :</b></td> 
         <td style="width: 416px"><b><asp:Label ID="EnddateLabel" runat="server" Text='<%# Bind("Enddate") %>' BackColor="Turquoise" Width="80px" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" ></asp:Label></b></td>  
            </tr>
            <tr><td align="right" style="width: 19px"><b></b></td>
            <td style="width: 91px"><b></b></td>
            <td style="width: 97px"><b></b></td>
            <td style="width: 2px"><b></b></td>
            <td style="width: 27px" align="right"><b></b></td>
            <td style="width: 89px"><b></b></td>
            <td style="width: 44px" align="right"><b></b></td>
            <td style="width: 92px"><b></b></td>
            <td style="width: 52px" align="right"><b>Due Date:</b></td>
            <td style="height: 18px; width: 416px;"><b><asp:Label ID="DuedateLabel" runat="server" Text='<%# Bind("Duedate", "{0:MM/dd/yyyy}") %>' BackColor="Turquoise" Width="80px" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px"></asp:Label></b></td></tr>
            
                    
            
           
        </table>
        </ItemTemplate>
        <FooterStyle BackColor="#507CD1" Font-Bold="True" ForeColor="White" />
        <EditRowStyle BackColor="#2461BF" />
        <RowStyle BackColor="#EFF3FB" />
        <PagerStyle BackColor="#2461BF" ForeColor="White" HorizontalAlign="Center" />
        <HeaderStyle BackColor="#507CD1" Font-Bold="True" ForeColor="White" />
             
       
    </asp:FormView>
   <br />
    <table class="shade" align="center" width="100%">
    <tr>
    <td align="center" style="height: 18px"><b>Machine Quantity Variance</b></td>
    </tr>
    </table>
        &nbsp; &nbsp;&nbsp;&nbsp;&nbsp; &nbsp; &nbsp; &nbsp;&nbsp; &nbsp;&nbsp;
        <asp:GridView ID="GridView1" runat="server" AllowPaging="True" AutoGenerateColumns="False"
            DataSourceID="ObjectDataSource1" AllowSorting="True" BorderStyle="Dotted" CssClass="Grid" Width="100%">
            <Columns>
               
                <asp:BoundField DataField="v-form-no" HeaderText="Sheet" SortExpression="v-form-no" >
                    <ItemStyle Wrap="False" />
                </asp:BoundField>
                <asp:BoundField DataField="v-blank-no" HeaderText="Blank" SortExpression="v-blank-no" >
                    <ItemStyle Wrap="False" />
                </asp:BoundField>
                <asp:BoundField DataField="v-m-code" HeaderText="Machine" SortExpression="v-m-code" >
                    <ItemStyle Wrap="False" />
                </asp:BoundField>
                <asp:BoundField DataField="v-i-no" HeaderText="FG Item#" SortExpression="v-i-no" >
                    <ItemStyle Wrap="False" />
                </asp:BoundField>
                <asp:BoundField DataField="v-run-std" HeaderText="Std Run Qty" SortExpression="v-run-std" >
                    <ItemStyle Wrap="False" />
                </asp:BoundField>
                <asp:BoundField DataField="v-run-act" HeaderText="Act Run Qty" SortExpression="v-run-act" >
                    <ItemStyle Wrap="False" />
                </asp:BoundField>
                <asp:BoundField DataField="v-run-var" HeaderText="Run Qty Var" SortExpression="v-run-var" >
                    <ItemStyle Wrap="False" />
                </asp:BoundField>
                <asp:BoundField DataField="v-mr-std" HeaderText="Std MR Qty" SortExpression="v-mr-std" >
                    <ItemStyle Wrap="False" />
                </asp:BoundField>
                <asp:BoundField DataField="v-mr-act" HeaderText="Act MR Qty" SortExpression="v-mr-act" >
                    <ItemStyle Wrap="False" />
                </asp:BoundField>
                <asp:BoundField DataField="v-mr-var" HeaderText="MR Qty Var" SortExpression="v-mr-var" >
                    <ItemStyle Wrap="False" />
                </asp:BoundField>
                
            </Columns>
        
        <EmptyDataRowStyle BorderColor="Gray" BorderStyle="Dotted" BorderWidth="0px" Font-Bold="True"
            HorizontalAlign="Center" VerticalAlign="Middle" />
        <RowStyle CssClass="shade" />
        <SelectedRowStyle CssClass="GridSelected" />
        <HeaderStyle   ForeColor="White" CssClass="headcolor" HorizontalAlign="Center"
            VerticalAlign="Middle" Wrap="False" />
        <AlternatingRowStyle CssClass="GridItemOdd" />
    </asp:GridView>
        <asp:ObjectDataSource ID="ObjectDataSource1" runat="server" OldValuesParameterFormatString="original_{0}"
            SelectMethod="MachQty" TypeName="jobprod">
            <SelectParameters>
            <asp:Parameter Name="prmUser" Type="String" />
            <asp:SessionParameter Name="prmOrderNum" SessionField="MachQty" Type="String" />
            <asp:SessionParameter Name="vLine" SessionField="line" Type="String" />

            </SelectParameters>
        </asp:ObjectDataSource>
        &nbsp; &nbsp;
    <asp:ObjectDataSource ID="ObjectDataSource2" runat="server" OldValuesParameterFormatString="original_{0}"
        SelectMethod="job" TypeName="jobprod">
        <SelectParameters>
            <asp:Parameter Name="prmUser" Type="String" />
            <asp:SessionParameter Name="prmOrderNum" SessionField="MachQty" Type="String" />
            <asp:SessionParameter Name="vLine" SessionField="line" Type="String" />
        </SelectParameters>
    </asp:ObjectDataSource>
    </div>

</asp:Content>

