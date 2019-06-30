<%@ Page Language="C#" MasterPageFile="~/MasterPage2.master" AutoEventWireup="true" Inherits="MachWaste" Title="Jobs For Order Line Item" Codebehind="MachWaste.aspx.cs" %>
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
        <tr><td style="width: 45px; height: 18px" align="right">
            <strong>Job:</strong></td>
        <td style="width: 105px; height: 18px"><b><asp:Label ID="JobLabel" runat="server" Text='<%# Bind("Job") %>' BackColor="Turquoise" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" Width="80px"></asp:Label></b></td>
        <td style="width: 6px; height: 18px" align="right">
            <strong>Job2:</strong></td>
        <td style="width: 110px; height: 18px"><b><asp:Label ID="Job2Label" runat="server" Text='<%# Bind("Job2") %>' BackColor="Turquoise" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" Width="80px"></asp:Label></b></td>
        <td style="width: 7px; height: 18px" align="right">
            <strong>Est:</strong></td>
        <td style="width: 103px; height: 18px"><b><asp:Label ID="estLabel" runat="server" Text='<%# Bind("est") %>' BackColor="Turquoise" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" Width="80px"></asp:Label></b></td>
        <td style="width: 50px; height: 18px" align="right">
            <strong>User Id:</strong></td>
        <td style="width: 95px; height: 18px"><b><asp:Label ID="Label1" runat="server" Text='<%# Bind("vUser") %>' BackColor="Turquoise" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" Width="80px"></asp:Label></b></td>
        
        <td style="width: 94px; height: 18px" align="right">
            <strong>Stats:</strong></td>
        <td style="height: 18px"><b><asp:Label ID="StatsLabel" runat="server" Text='<%# Bind("Stats") %>' BackColor="Turquoise" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" Width="80px"></asp:Label></b></td></tr>
            <tr><td style="width: 45px; height: 18px"><b></b></td>
            <td style="width: 105px; height: 18px"><b></b></td>
            <td style="width: 6px; height: 18px"><b></b></td>
            <td style="width: 110px; height: 18px"><b></b></td>
            <td style="width: 7px; height: 18px"><b></b></td>
            <td style="width: 103px; height: 18px"><b></b></td>
            <td style="width: 50px; height: 18px"><b></b></td>
            <td style="width: 95px; height: 18px"><b></b></td>
         <td style="width: 94px; height: 18px" align="right">
             <strong>Start Date:</strong></td> 
         <td style="height: 18px"><b><asp:Label ID="StartdateLabel" runat="server" Text='<%# Bind("Startdate", "{0:MM/dd/yyyy}") %>' BackColor="Turquoise" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" Width="80px"></asp:Label></b></td>  
            </tr>
            <tr><td style="width: 45px; height: 17px"><b></b></td>
            <td style="width: 105px; height: 17px"><b></b></td>
            <td style="width: 6px; height: 17px"><b></b></td>
            <td style="width: 110px; height: 17px"><b></b></td>
            <td style="width: 7px; height: 17px"><b></b></td>
            <td style="width: 103px; height: 17px"><b></b></td>
            <td style="width: 50px; height: 17px"><b></b></td>
            <td style="width: 95px; height: 17px"><b></b></td>
            <td style="width: 94px; height: 17px" align="right">
                <strong>End Date:</strong></td>
            <td style="height: 17px"><b><asp:Label ID="EnddateLabel" runat="server" Text='<%# Bind("Enddate") %>' BackColor="Turquoise" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" Width="80px"></asp:Label></b></td></tr>
            
           <tr><td style="height: 18px; width: 45px;"><b></b></td>
           <td style="height: 18px; width: 105px;"><b></b></td>
           <td style="height: 18px; width: 6px;"><b></b></td>
           <td style="height: 18px; width: 110px;"><b></b></td>
           <td style="height: 18px; width: 7px;"><b></b></td>
           <td style="height: 18px; width: 103px;"><b></b></td>
           <td style="height: 18px; width: 50px;"><b></b></td>
           <td style="height: 18px; width: 95px;"><b></b></td>
           <td style="height: 18px; width: 94px;" align="right"><b>Due Date:</b></td>
           <td style="height: 18px"><b><asp:Label ID="DuedateLabel" runat="server" Text='<%# Bind("Duedate", "{0:MM/dd/yyyy}") %>' BackColor="Turquoise" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" Width="80px"></asp:Label></b></td></tr>
            
           
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
    <td align="center" style="height: 18px"><b>Machine Waste Variance</b></td>
    </tr>
    </table>
        &nbsp; &nbsp;&nbsp;&nbsp;&nbsp; &nbsp; &nbsp; &nbsp;&nbsp; &nbsp;&nbsp;
        <asp:GridView ID="GridView1" runat="server" AllowPaging="True" AutoGenerateColumns="False"
            DataSourceID="ObjectDataSource1" AllowSorting="True" BorderStyle="Dotted" CssClass="Grid" Width="100%">
            <Columns>
               
                <asp:BoundField DataField="w-form-no" HeaderText="Sheet" SortExpression="w-form-no" >
                    <ItemStyle Wrap="False" />
                </asp:BoundField>
                <asp:BoundField DataField="w-blank-no" HeaderText="Blank" SortExpression="w-blank-no" >
                    <ItemStyle Wrap="False" />
                </asp:BoundField>
                <asp:BoundField DataField="w-m-code" HeaderText="Machine" SortExpression="w-m-code" >
                    <ItemStyle Wrap="False" />
                </asp:BoundField>
                <asp:BoundField DataField="w-i-no" HeaderText="FG Item#" SortExpression="w-i-no" >
                    <ItemStyle Wrap="False" />
                </asp:BoundField>
                <asp:BoundField DataField="w-run-std" HeaderText="Std Run Waste" SortExpression="w-run-std" >
                    <ItemStyle Wrap="False" />
                </asp:BoundField>
                <asp:BoundField DataField="w-run-act" HeaderText="Act Run Waste" SortExpression="w-run-act" >
                    <ItemStyle Wrap="False" />
                </asp:BoundField>
                <asp:BoundField DataField="w-run-var" HeaderText="Run Waste Var" SortExpression="w-run-var" >
                    <ItemStyle Wrap="False" />
                </asp:BoundField>
                <asp:BoundField DataField="w-mr-std" HeaderText="Std MR Waste" SortExpression="w-mr-std" >
                    <ItemStyle Wrap="False" />
                </asp:BoundField>
                <asp:BoundField DataField="w-mr-act" HeaderText="Act MR Waste" SortExpression="w-mr-act" >
                    <ItemStyle Wrap="False" />
                </asp:BoundField>
                <asp:BoundField DataField="w-mr-var" HeaderText="MR Waste Var" SortExpression="w-mr-var" >
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
            SelectMethod="MachWaste" TypeName="jobprod">
            <SelectParameters>
            <asp:Parameter Name="prmUser" Type="String" />
            <asp:SessionParameter Name="prmOrderNum" SessionField="MachWaste" Type="String" />
            <asp:SessionParameter Name="vLine" SessionField="line" Type="String" />

            </SelectParameters>
        </asp:ObjectDataSource>
        &nbsp; &nbsp;
    <asp:ObjectDataSource ID="ObjectDataSource2" runat="server" OldValuesParameterFormatString="original_{0}"
        SelectMethod="job" TypeName="jobprod">
        <SelectParameters>
            <asp:Parameter Name="prmUser" Type="String" />
            <asp:SessionParameter Name="prmOrderNum" SessionField="MachWaste" Type="String" />
            <asp:SessionParameter Name="vLine" SessionField="line" Type="String" />
        </SelectParameters>
    </asp:ObjectDataSource>
    </div>

</asp:Content>

