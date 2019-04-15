<%@ Page Language="C#" MasterPageFile="~/MasterPage2.master" AutoEventWireup="true" Inherits="materialinfo" Title="Jobs For Order Line Item" Codebehind="materialinfo.aspx.cs" %>
<asp:Content ID="Content1" ContentPlaceHolderID="ContentPlaceHolder1" Runat="Server">
    <br />
    <asp:LinkButton OnClick="LinkButton_Click" ID="LinkButton1" runat="server">List Order</asp:LinkButton>
    <asp:LinkButton OnClick="LinkButton2_Click" ID="LinkButton2" runat="server">List Order</asp:LinkButton>
    <asp:LinkButton OnClick="LinkButton3_Click" ID="LinkButton3" runat="server">List Order</asp:LinkButton>
    <br />
    <br />
    <table class="shade" align="left" width="875">
    <tr>
    <td align="center" style="height: 19px; width: 863px;"><b> MatInfo</b></td>
    </tr>
    </table>
    <br />
    <br />
    <asp:FormView ID="FormView1" runat="server" DataSourceID="ObjectDataSource2" Width="875px" CellPadding="4" ForeColor="#333333" Height="79px">
        <ItemTemplate>
        <table style="width: 850px">
        <tr><td align="right" style="width: 43px"><b>Job:</b></td>
        <td style="width: 87px"><b><asp:Label ID="JobLabel" runat="server" Text='<%# Bind("Job") %>' BackColor="Turquoise" Width="80px" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px"></asp:Label></b></td>
        <td style="width: 105px"><b><asp:Label ID="Job2Label" runat="server" Text='<%# Bind("Job2") %>' BackColor="Turquoise" Width="80px" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px"></asp:Label></b></td>
        <td style="width: 14px"><b></b></td>
        
        <td align="right" style="width: 24px"><b>Est:</b></td>
        <td style="width: 109px"><b><asp:Label ID="estLabel" runat="server" Text='<%# Bind("est") %>' BackColor="Turquoise" Width="80px" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px"></asp:Label></b></td>
        <td align="right" style="width: 64px"><b>Status:</b></td>
        <td style="width: 98px"><b><asp:Label ID="StatsLabel" runat="server" Text='<%# Bind("Stats") %>' BackColor="Turquoise" Width="80px" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px"></asp:Label></b></td>
        
        <td align="right" style="width: 65px"><b>Start:</b></td>
        <td style="width: 158px"><b><asp:Label ID="StartdateLabel" runat="server" Text='<%# Bind("Startdate"," {0:MM/dd/yyyy}") %>' BackColor="Turquoise" Width="80px" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px"></asp:Label></b></td>
            <td style="width: 48px">
            </td>
        </tr>
            <tr><td align="right" style="width: 43px"><b></b></td>
            <td style="width: 87px"><b></b></td>
            <td style="width: 105px"><b></b></td>
            <td style="width: 14px"><b></b></td>
            <td align="right" style="width: 24px"><b></b></td>
            <td style="width: 109px"><b></b></td>
            <td align="right" style="width: 64px"><b>User Id:</b></td>
            <td style="width: 98px"><b><asp:Label ID="Label1" runat="server" Text='<%# Bind("vUser") %>' BackColor="Turquoise" Width="80px" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px"></asp:Label></b></td>
         <td align="right" style="width: 65px"><b>Close:</b></td> 
         <td style="width: 158px"><b><asp:Label ID="EnddateLabel" runat="server" Text='<%# Bind("Enddate") %>' BackColor="Turquoise" Width="80px" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" ></asp:Label></b></td>  
                <td style="width: 48px">
                </td>
            </tr>
            <tr><td align="right" style="width: 43px"><b></b></td>
            <td style="width: 87px"><b></b></td>
            <td style="width: 105px"><b></b></td>
            <td style="width: 14px"><b></b></td>
            <td align="right" style="width: 24px"><b></b></td>
            <td style="width: 109px"><b></b></td>
            <td style="width: 64px"><b></b></td>
            <td style="width: 98px"><b></b></td>
            <td align="right" style="width: 65px"><b>Due Date:</b></td>
            <td style="height: 18px; width: 158px;"><b><asp:Label ID="DuedateLabel" runat="server" Text='<%# Bind("Duedate"," {0:MM/dd/yyyy}") %>' BackColor="Turquoise" Width="80px" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px"></asp:Label></b></td>
                <td style="width: 48px; height: 18px">
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
    <table class="shade" align="left" width="875">
    <tr>
    <td align="center" style="width: 863px; height: 19px;"><b><asp:Label ID="Label2" runat="server" Text="Material Information" ></asp:Label></b></td>
    </tr>
    </table>
    <br />
    <br />
    
    <asp:GridView ID="GridView1" runat="server" AutoGenerateColumns="False" DataSourceID="MatInfoDataSource" EmptyDataText="No Record Found" AllowPaging="True" AllowSorting="True" BorderStyle="Dotted" CssClass="Grid" Width="75%">
        <Columns>
            <asp:BoundField DataField="rm-i-no" HeaderText="RM Item#" SortExpression="rm-i-no"  >
                <ControlStyle Width="50px" />
                <ItemStyle Wrap="False" />
            </asp:BoundField>
            <asp:BoundField DataField="i-name" HeaderText="Description" SortExpression="i-name" HtmlEncode="False" DataFormatString= "{0:5}" >
                <ItemStyle Wrap="False" />
            </asp:BoundField>
            <asp:BoundField DataField="q-onh" HeaderText="On-Hand" SortExpression="q-onh" >
                <ItemStyle Wrap="False" />
            </asp:BoundField>
            <asp:BoundField DataField="q-comm" HeaderText="Committed" SortExpression="q-comm" >
                <ItemStyle Wrap="False" />
            </asp:BoundField>
            <asp:BoundField DataField="q-avail" HeaderText="Available" SortExpression="q-avail" >
                <ItemStyle Wrap="False" />
            </asp:BoundField>
            <asp:BoundField DataField="qty-all" HeaderText="Job Commit" SortExpression="qty-all" >
                <ItemStyle Wrap="False" />
            </asp:BoundField>
            <asp:BoundField DataField="due-date" HeaderText="Due Date" SortExpression="due-date"  DataFormatString="{0:MM/dd/yyyy}"  >
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
    <asp:ObjectDataSource ID="MatInfoDataSource" runat="server" OldValuesParameterFormatString="original_{0}"
        SelectMethod="MatInfo" TypeName="jobprod">
        <SelectParameters>
            <asp:Parameter Name="prmUser" Type="String" />
            <asp:SessionParameter Name="prmOrderNum" SessionField="matinfo" Type="String" />
            <asp:SessionParameter Name="vLine" SessionField="line" Type="String" />
        </SelectParameters>
    </asp:ObjectDataSource>
    <asp:ObjectDataSource ID="ObjectDataSource2" runat="server" OldValuesParameterFormatString="original_{0}"
        SelectMethod="job" TypeName="jobprod">
        <SelectParameters>
            <asp:Parameter Name="prmUser" Type="String" />
            <asp:SessionParameter Name="prmOrderNum" SessionField="matinfo" Type="String" />
            <asp:SessionParameter Name="vLine" SessionField="line" Type="String" />
        </SelectParameters>
    </asp:ObjectDataSource>
</asp:Content>

