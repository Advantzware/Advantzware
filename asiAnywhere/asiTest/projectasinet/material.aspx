<%@ Page Language="C#" MasterPageFile="~/MasterPage2.master" AutoEventWireup="true" Inherits="Material" Title="Material" EnableEventValidation="false" Codebehind="material.aspx.cs" %>
<asp:Content ID="Content1" ContentPlaceHolderID="ContentPlaceHolder1" Runat="Server">
<script language="javascript" type="text/javascript">
<!--



// -->
</script>

    <div>
    <br />
  
    <asp:LinkButton OnClick="LinkButton_Click" ID="LinkButton1" runat="server">List Order</asp:LinkButton>
    <asp:LinkButton OnClick="LinkButton2_Click" ID="LinkButton2" runat="server">List Order</asp:LinkButton>
    <asp:LinkButton OnClick="LinkButton3_Click" ID="LinkButton3" runat="server">List Order</asp:LinkButton>
    <br />
        <table class="shade" align="left" width="809">
    <tr>
    <td align="center" style="height: 18px; width: 806px;"><b> Material</b></td>
    </tr>
    </table>
    <br />
    <br />
    <asp:FormView ID="FormView1" runat="server" DataSourceID="ObjectDataSource2" Width="600px" CellPadding="4" ForeColor="#333333" Height="98px">
        <ItemTemplate>
          <table style="width: 800px">
        <tr><td align="right"><b>Job:</b></td>
        <td style="width: 103px"><b><asp:Label ID="JobLabel" runat="server" Text='<%# Bind("Job") %>' BackColor="Turquoise" Width="82px" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px"></asp:Label></b></td>
        <td style="width: 89px"><b><asp:Label ID="Job2Label" runat="server" Text='<%# Bind("Job2") %>' BackColor="Turquoise" Width="77px" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px"></asp:Label></b></td>
        <td><b></b></td>
        
        <td align="right"><b>Est:</b></td>
        <td style="width: 98px"><b><asp:Label ID="estLabel" runat="server" Text='<%# Bind("est") %>' BackColor="Turquoise" Width="85px" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px"></asp:Label></b></td>
        <td align="right"><b>Status:</b></td>
        <td><b><asp:Label ID="StatsLabel" runat="server" Text='<%# Bind("Stats") %>' BackColor="Turquoise" Width="70px" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px"></asp:Label></b></td>
        
        <td align="right"><b>Start :</b></td>
        <td><b><asp:Label ID="StartdateLabel" runat="server" Text='<%# Bind("Startdate","{0:MM/dd/yyyy}") %>' BackColor="Turquoise" Width="90px" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px"></asp:Label></b></td></tr>
            <tr><td align="right"><b></b></td>
            <td style="width: 103px"><b></b></td>
            <td style="width: 89px"><b></b></td>
            <td><b></b></td>
            <td align="right"><b></b></td>
            <td style="width: 98px"><b></b></td>
            <td align="right"><b>User Id:</b></td>
            <td><b><asp:Label ID="Label1" runat="server" Text='<%# Bind("vUser") %>' BackColor="Turquoise" Width="70px" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px"></asp:Label></b></td>
         <td align="right"><b>Close :</b></td> 
         <td><b><asp:Label ID="EnddateLabel" runat="server" Text='<%# Bind("Enddate","{0:MM/dd/yyyy}") %>' BackColor="Turquoise" Width="90px" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" ></asp:Label></b></td>  
            </tr>
            <tr><td align="right"><b></b></td>
            <td style="width: 103px"><b></b></td>
            <td style="width: 89px"><b></b></td>
            <td><b></b></td>
            <td align="right"><b></b></td>
            <td style="width: 98px"><b></b></td>
            <td align="right"><b></b></td>
            <td><b></b></td>
            <td align="right"><b>Due Date:</b></td>
            <td style="height: 18px"><b><asp:Label ID="DuedateLabel" runat="server" Text='<%# Bind("Duedate","{0:MM/dd/yyyy}") %>' BackColor="Turquoise" Width="90px" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px"></asp:Label></b></td></tr>
            
                    
            
           
        </table>
        </ItemTemplate>
        <FooterStyle BackColor="#507CD1" Font-Bold="True" ForeColor="White" />
        <EditRowStyle BackColor="#2461BF" />
        <RowStyle BackColor="#EFF3FB" />
        <PagerStyle BackColor="#2461BF" ForeColor="White" HorizontalAlign="Center" />
        <HeaderStyle BackColor="#507CD1" Font-Bold="True" ForeColor="White" />
             
       
    </asp:FormView>
        &nbsp;<br />
        &nbsp;&nbsp; &nbsp; &nbsp; &nbsp;&nbsp; &nbsp;&nbsp; &nbsp;&nbsp;&nbsp;<%--<asp:LinkButton ID="LinkButton1" runat="server" OnClick="LinkButton1_Click">Sub Item</asp:LinkButton>--%>
        <asp:GridView ID="GridView1" runat="server" AllowPaging="True" AutoGenerateColumns="False" EmptyDataText="No records found"
            DataSourceID="ObjectDataSource1" AllowSorting="True" BorderStyle="Dotted" CssClass="Grid" Width="70%" OnSelectedIndexChanged="GridView1_SelectedIndexChanged">
            <Columns>
                <asp:CommandField ButtonType="Image" HeaderText="Select" SelectImageUrl="~/Images/sel.gif"
                    SelectText="" ShowSelectButton="True">
                    <HeaderStyle ForeColor="White" />
                    <ItemStyle Width="10px" />
                </asp:CommandField>
               
                <asp:BoundField DataField="form-no" HeaderText="Sheet" SortExpression="form-no" >
                    <ItemStyle Wrap="False" />
                </asp:BoundField>
                <asp:BoundField DataField="blank-no" HeaderText="Blank" SortExpression="blank-no" >
                    <ItemStyle Wrap="False" />
                </asp:BoundField>
                <asp:BoundField DataField="rm-i-no" HeaderText="RM Item#" SortExpression="rm-i-no" >
                    <ItemStyle Wrap="False" />
                </asp:BoundField>
                <asp:BoundField DataField="qty-std" HeaderText="Standard Qty" SortExpression="qty-std" >
                    <ItemStyle Wrap="False" />
                </asp:BoundField>
                <asp:BoundField DataField="qty-act" HeaderText="Actual Qty" SortExpression="qty-act" >
                    <ItemStyle Wrap="False" />
                </asp:BoundField>
                <asp:BoundField DataField="qty-var" HeaderText="Qty Variance" SortExpression="qty-var" >
                    <ItemStyle Wrap="False"  />
                   
                    
                </asp:BoundField>
                
            </Columns>
        
        <EmptyDataRowStyle BorderColor="Gray" BorderStyle="Dotted" BorderWidth="0px" Font-Bold="True"
            HorizontalAlign="Center" VerticalAlign="Middle" />
        <RowStyle CssClass="shade" />
        <SelectedRowStyle CssClass="GridSelected" BackColor="Yellow" />
        <HeaderStyle  ForeColor="White" CssClass="headcolor" HorizontalAlign="Center"
            VerticalAlign="Middle" Wrap="False" />
        <AlternatingRowStyle CssClass="GridItemOdd" />
    </asp:GridView>
        &nbsp; &nbsp;
        
        <br />
        
        
       
    
        <asp:FormView ID="FormView2" runat="server" DataSourceID="ObjectDataSource4" CellPadding="4" ForeColor="#333333" Width="1064px">
            <ItemTemplate>
                Job #:
                <asp:Label ID="job_noLabel" runat="server" Text='<%# Bind("[job-no]") %>'></asp:Label>
                -
                <asp:Label ID="job_no2Label" runat="server" Text='<%# Bind("[job-no2]") %>'></asp:Label>
                &nbsp;&nbsp;
                Machine:
                <asp:Label ID="machine_codeLabel" runat="server" Text='<%# Bind("[machine-code]") %>'>
                </asp:Label>
                &nbsp;&nbsp;
                Sheet:
                <asp:Label ID="sheet_noLabel" runat="server" Text='<%# Bind("[sheet-no]") %>'></asp:Label>
                &nbsp;&nbsp;
                Blank:
                <asp:Label ID="blank_noLabel" runat="server" Text='<%# Bind("[blank-no]") %>'></asp:Label>
            </ItemTemplate>
            <FooterStyle BackColor="#507CD1" Font-Bold="True" ForeColor="White" />
            <EditRowStyle BackColor="#2461BF" />
            <RowStyle BackColor="#EFF3FB" />
            <PagerStyle BackColor="#2461BF" ForeColor="White" HorizontalAlign="Center" />
            <HeaderStyle BackColor="#507CD1" Font-Bold="True" ForeColor="White" />
        </asp:FormView>
        
        <asp:GridView ID="GridView2" runat="server" AllowPaging="True" AutoGenerateColumns="False" EmptyDataText="No records found"
         DataSourceID="ObjectDataSource3" AllowSorting="True" BorderStyle="Dotted" CssClass="Grid" Width="100%">
            <Columns>
                <asp:BoundField DataField="i-no" HeaderText="RG Item #" SortExpression="i-no" >
                    <ItemStyle Width="80px" />
                </asp:BoundField>
                <asp:BoundField DataField="tran-date" HeaderText="Date" SortExpression="tran-date" DataFormatString="{0:dmm/dd/yyyy}" >
                    <ItemStyle Width="120px" />
                </asp:BoundField>
                <asp:BoundField DataField="job-no" HeaderText="Job #" SortExpression="job-no" >
                    <ItemStyle Width="80px" />
                </asp:BoundField>
                <asp:BoundField DataField="job-no2" HeaderText="job-no2" SortExpression="job-no2" >
                    <ItemStyle Width="80px" />
                </asp:BoundField>
                <asp:BoundField DataField="sheet-no" HeaderText="Sheet" SortExpression="sheet-no" >
                    <ItemStyle Width="80px" />
                </asp:BoundField>
                <asp:BoundField DataField="blank-no" HeaderText="Blank" SortExpression="blank-no" >
                    <ItemStyle Width="80px" />
                </asp:BoundField>
                <asp:BoundField DataField="tag" HeaderText="Tag" SortExpression="tag" >
                    <ItemStyle Width="80px" />
                </asp:BoundField>
                <asp:BoundField DataField="qty-posted" HeaderText="Qty. Posted" SortExpression="qty-posted" >
                    <ItemStyle Width="80px" />
                </asp:BoundField>
                <asp:BoundField DataField="qty-uom" HeaderText="UOM" SortExpression="qty-uom" >
                    <ItemStyle Width="80px" />
                </asp:BoundField>
                               
                
                
            </Columns>
        <EmptyDataRowStyle BorderColor="Gray" BorderStyle="Dotted" BorderWidth="0px" Font-Bold="True"
            HorizontalAlign="Center" VerticalAlign="Middle" />
        <RowStyle CssClass="shade" />
        <SelectedRowStyle CssClass="GridSelected" />
        <HeaderStyle BackColor="Teal" CssClass="gridrowhdr" ForeColor="White" HorizontalAlign="Center"
            VerticalAlign="Middle" Wrap="False" />
        <AlternatingRowStyle CssClass="GridItemOdd" />
    </asp:GridView>
        <br />
        <asp:ObjectDataSource ID="ObjectDataSource1" runat="server" OldValuesParameterFormatString="original_{0}"
            SelectMethod="Material" TypeName="jobprod">
            <SelectParameters>
               <asp:Parameter Name="prmUser" Type="String" />
               <asp:SessionParameter Name="prmOrderNum" SessionField="Material" Type="String" />
               <asp:SessionParameter Name="vLine" SessionField="line" Type="String" />
                           

            </SelectParameters>
        </asp:ObjectDataSource>
        &nbsp; &nbsp;
        <asp:ObjectDataSource ID="ObjectDataSource3" runat="server" OldValuesParameterFormatString="original_{0}"
            SelectMethod="MaterialInt" TypeName="jobprod" >
            <SelectParameters>
                <asp:Parameter Name="prmUser" Type="String" />
                <asp:SessionParameter Name="prmOrderNum" SessionField="Material" Type="String" />
                <asp:SessionParameter Name="vLine" SessionField="line" Type="String" />
                <asp:Parameter DefaultValue="Select" Name="prmAction" Type="String" />
                <asp:SessionParameter DefaultValue="" Name="vFormNo" SessionField="vFormNo" Type="String" />
                <asp:SessionParameter DefaultValue="" Name="vBlankNo" SessionField="vBlankNo" Type="String" />
                <asp:SessionParameter DefaultValue="" Name="prmItem" SessionField="prmItem" Type="String" />

            </SelectParameters>
        </asp:ObjectDataSource>
        <asp:ObjectDataSource ID="ObjectDataSource4" runat="server" OldValuesParameterFormatString="original_{0}"
            SelectMethod="MachHrHdrInt" TypeName="jobprod" >
            <SelectParameters>
                <asp:Parameter Name="prmUser" Type="String" />
                <asp:SessionParameter Name="prmOrderNum" SessionField="Material" Type="String" />
                <asp:SessionParameter Name="vLine" SessionField="line" Type="String" />
                <asp:Parameter DefaultValue="Select" Name="prmAction" Type="String" />
                <asp:SessionParameter DefaultValue="" Name="vFormNo" SessionField="vFormNo" Type="String" />
                <asp:SessionParameter DefaultValue="" Name="vBlankNo" SessionField="vBlankNo" Type="String" />
                <asp:SessionParameter DefaultValue="" Name="prmItem" SessionField="prmItem" Type="String" />

            </SelectParameters>
        </asp:ObjectDataSource>
        &nbsp; &nbsp;
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

