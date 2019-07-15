<%@ Page Language="C#" MasterPageFile="~/MasterPage2.master" AutoEventWireup="true" Inherits="machhrs" Title="Jobs For Order Line Item" Codebehind="machhrs.aspx.cs" %>
<asp:Content ID="Content1" ContentPlaceHolderID="ContentPlaceHolder1" Runat="Server">
    <div>
    <br />
  
    <asp:LinkButton OnClick="LinkButton_Click" ID="LinkButton1" runat="server">List Order</asp:LinkButton>
    <asp:LinkButton OnClick="LinkButton2_Click" ID="LinkButton2" runat="server">List Order</asp:LinkButton>
    <asp:LinkButton OnClick="LinkButton3_Click" ID="LinkButton3" runat="server">List Order</asp:LinkButton>
    <br />
    <asp:FormView ID="FormView1" runat="server" DataSourceID="ObjectDataSource2" Width="799px" CellPadding="4" ForeColor="#333333" Height="98px">
        <ItemTemplate>
          <table style="width: 900px">
        <tr><td align="right" style="height: 18px; width: 20px;">
            <strong>Job:</strong></td>
        <td style="width: 60px; height: 18px;"><b><asp:Label ID="JobLabel" runat="server" Text='<%# Bind("Job") %>' BackColor="Turquoise" Width="80px" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px"></asp:Label></b></td>
        <td style="width: 64px; height: 18px;"><b><asp:Label ID="Job2Label" runat="server" Text='<%# Bind("Job2") %>' BackColor="Turquoise" Width="80px" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px"></asp:Label></b></td>
        <td style="height: 18px; width: 5px;"><b></b></td>
        
        <td align="right" style="height: 18px; width: 17px;">
            <strong>Est:</strong></td>
        <td style="width: 57px; height: 18px;"><b><asp:Label ID="estLabel" runat="server" Text='<%# Bind("est") %>' BackColor="Turquoise" Width="80px" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px"></asp:Label></b></td>
        <td style="width: 44px; height: 18px;" align="right"><b>Status:</b></td>
        <td style="width: 85px; height: 18px;"><b><asp:Label ID="StatsLabel" runat="server" Text='<%# Bind("Stats") %>' BackColor="Turquoise" Width="80px" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px"></asp:Label></b></td>
        
        <td align="right" style="width: 57px; height: 18px;">
            <strong>&nbsp;Start :</strong></td>
        <td style="width: 212px; height: 18px"><b><asp:Label ID="StartdateLabel" runat="server" Text='<%# Bind("Startdate"," {0:MM/dd/yyyy}") %>' BackColor="Turquoise" Width="80px" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px"></asp:Label></b></td></tr>
            <tr><td align="right" style="width: 20px"><b></b></td>
            <td style="width: 60px"><b></b></td>
            <td style="width: 64px"><b></b></td>
            <td style="width: 5px"><b></b></td>
            <td align="right" style="width: 17px"><b></b></td>
            <td style="width: 57px"><b></b></td>
            <td style="width: 44px" align="right">
                <strong>User Id:</strong></td>
            <td style="width: 85px"><b><asp:Label ID="Label1" runat="server" Text='<%# Bind("vUser") %>' BackColor="Turquoise" Width="80px" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px"></asp:Label></b></td>
         <td align="right" style="width: 57px">
             <strong>Close :</strong></td> 
         <td style="width: 212px"><b><asp:Label ID="EnddateLabel" runat="server" Text='<%# Bind("Enddate") %>' BackColor="Turquoise" Width="80px" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" ></asp:Label></b></td>  
            </tr>
            <tr><td align="right" style="width: 20px"><b></b></td>
            <td style="width: 60px"><b></b></td>
            <td style="width: 64px"><b></b></td>
            <td style="width: 5px"><b></b></td>
            <td align="right" style="width: 17px"><b></b></td>
            <td style="width: 57px"><b></b></td>
            <td style="width: 44px" align="right"><b></b></td>
            <td style="width: 85px"><b></b></td>
            <td align="right" style="width: 57px">
                <strong>Due Date:</strong></td>
            <td style="height: 18px; width: 212px;"><b><asp:Label ID="DuedateLabel" runat="server" Text='<%# Bind("Duedate", "{0:MM/dd/yyyy}") %>' BackColor="Turquoise" Width="80px" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px"></asp:Label></b></td></tr>
            
                    
            
           
        </table>
        </ItemTemplate>
        <FooterStyle BackColor="#507CD1" Font-Bold="True" ForeColor="White" />
        <EditRowStyle BackColor="#2461BF" />
        <RowStyle BackColor="#EFF3FB" />
        <PagerStyle BackColor="#2461BF" ForeColor="White" HorizontalAlign="Center" />
        <HeaderStyle BackColor="#507CD1" Font-Bold="True" ForeColor="White" />
             
       
    </asp:FormView>
    <br />
        <table class="shade" align="left" style="width: 909px; height: 24px">
    <tr>
    <td align="center" style="height: 18px; width: 903px;"><b>Machine Hours Variance</b></td>
    </tr>
    </table>
    
        &nbsp;<br />
        <br />
        &nbsp;&nbsp;&nbsp;<%--<asp:LinkButton ID="LinkButton1" runat="server" OnClick="LinkButton1_Click" Visible="False">Sub Item</asp:LinkButton>--%>
        <asp:GridView ID="GridView1" runat="server" 
            AutoGenerateColumns="False" DataSourceID="ObjectDataSource1" AllowSorting="True" BorderStyle="Dotted" CssClass="Grid" Width="75%" OnSelectedIndexChanged="GridView1_SelectedIndexChanged" >
            <Columns>
                <asp:CommandField ButtonType="Image" HeaderText="Select" SelectImageUrl="~/Images/sel.gif"
                    SelectText="" ShowSelectButton="True">
                    <HeaderStyle ForeColor="White" />
                    <ItemStyle Width="10px" />
                </asp:CommandField>
               
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
                <asp:BoundField DataField="v-run-hr" HeaderText="Std Run Hrs" SortExpression="v-run-hr" >
                    <ItemStyle Wrap="False" />
                </asp:BoundField>
                <asp:BoundField DataField="v-run-act" HeaderText="Act Run Hrs" SortExpression="v-run-act" >
                    <ItemStyle Wrap="False" />
                </asp:BoundField>
                <asp:BoundField DataField="v-run-var" HeaderText="Run Hrs Var" SortExpression="v-run-var" >
                    <ItemStyle Wrap="False" />
                </asp:BoundField>
                <asp:BoundField DataField="v-mr-hr" HeaderText="Std MR Hrs" SortExpression="v-mr-hr" >
                    <ItemStyle Wrap="False" />
                </asp:BoundField>
                <asp:BoundField DataField="v-mr-act" HeaderText="Act MR Hrs" SortExpression="v-mr-act" >
                    <ItemStyle Wrap="False" />
                </asp:BoundField>
                <asp:BoundField DataField="v-mr-var" HeaderText="MR Hrs Var" SortExpression="v-mr-var" >
                    <ItemStyle Wrap="False" />
                </asp:BoundField>
            </Columns>
        <EmptyDataRowStyle BorderColor="Gray" BorderStyle="Dotted" BorderWidth="0px" Font-Bold="True"
            HorizontalAlign="Center" VerticalAlign="Middle" />
        <RowStyle CssClass="shade" />
        <SelectedRowStyle CssClass="GridSelected" BackColor="Yellow" />
        <HeaderStyle   ForeColor="White" CssClass="headcolor" HorizontalAlign="Center"
            VerticalAlign="Middle" Wrap="False" />
        <AlternatingRowStyle CssClass="GridItemOdd" />
    </asp:GridView>
        <br />
        
        
       
    
        <asp:FormView ID="FormView2" runat="server" DataSourceID="ObjectDataSource4" CellPadding="4" ForeColor="#333333" Width="944px">
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
        <br />
        <asp:GridView ID="GridView2" runat="server" AllowPaging="True" AutoGenerateColumns="False" EmptyDataText="No records found"
         DataSourceID="ObjectDataSource3" AllowSorting="True" BorderStyle="Dotted" CssClass="Grid" Width="75%" >
        
            <Columns>
                <asp:BoundField DataField="tran-type" HeaderText="Type" SortExpression="tran-type" >
                    <ItemStyle Wrap="False" />
                </asp:BoundField>
                <asp:BoundField DataField="tran-date" HeaderText="Date" SortExpression="tran-date" >
                    <ItemStyle Wrap="False" />
                </asp:BoundField>
                <asp:BoundField DataField="job-no" HeaderText="Job #" SortExpression="job-no" >
                    <ItemStyle Wrap="False" />
                </asp:BoundField>
                <asp:BoundField DataField="job-no2" SortExpression="job-no2" >
                    <ItemStyle Wrap="False" />
                </asp:BoundField>
                <asp:BoundField DataField="machine-code" HeaderText="Machine" SortExpression="machine-code" >
                    <ItemStyle Wrap="False" />
                </asp:BoundField>
                <asp:BoundField DataField="sheet-no" HeaderText="Sheet" SortExpression="sheet-no" >
                    <ItemStyle Wrap="False" />
                </asp:BoundField>
                <asp:BoundField DataField="blank-no" HeaderText="Blank" SortExpression="blank-no" >
                    <ItemStyle Wrap="False" />
                </asp:BoundField>
                <asp:BoundField DataField="i-no" HeaderText="FG Item #" SortExpression="i-no" >
                    <ItemStyle Wrap="False" />
                </asp:BoundField>
                <asp:BoundField DataField="charge-code" HeaderText="Charge" SortExpression="charge-code" >
                    <ItemStyle Wrap="False" />
                </asp:BoundField>
                <asp:BoundField DataField="start-time" HeaderText="Start Time" SortExpression="start-time" >
                    <ItemStyle Wrap="False" />
                </asp:BoundField>
                <asp:BoundField DataField="start-am" HeaderText="start-am" SortExpression="start-am" >
                    <ItemStyle Wrap="False" />
                </asp:BoundField>
                <asp:BoundField DataField="end-time" HeaderText="End Time" SortExpression="end-time" >
                    <ItemStyle Wrap="False" />
                </asp:BoundField>
                <asp:BoundField DataField="end-am" HeaderText="end-am" SortExpression="end-am" >
                    <ItemStyle Wrap="False" />
                </asp:BoundField>
                <asp:BoundField DataField="total-hours" HeaderText="Total Hours" SortExpression="total-hours" >
                    <ItemStyle Wrap="False" />
                </asp:BoundField>
                <asp:BoundField DataField="qty-posted" HeaderText="Qty. Posted" SortExpression="qty-posted" >
                    <ItemStyle Wrap="False" />
                </asp:BoundField>
                <asp:BoundField DataField="qty-waste" HeaderText="Qty. Wasted" SortExpression="qty-waste" >
                    <ItemStyle Wrap="False" />
                </asp:BoundField>
                <asp:CheckBoxField DataField="complete" HeaderText="Completed" SortExpression="complete" />
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
            SelectMethod="MachHr" TypeName="jobprod" >
            <SelectParameters>
                <asp:Parameter Name="prmUser" Type="String" />
                <asp:SessionParameter Name="prmOrderNum" SessionField="machhr" Type="String" />
                <asp:SessionParameter Name="vLine" SessionField="line" Type="String" />
            </SelectParameters>
        </asp:ObjectDataSource>
        &nbsp;
    <asp:ObjectDataSource ID="ObjectDataSource2" runat="server" OldValuesParameterFormatString="original_{0}"
        SelectMethod="job" TypeName="jobprod">
        <SelectParameters>
            <asp:Parameter Name="prmUser" Type="String" />
            <asp:SessionParameter Name="prmOrderNum" SessionField="machhr" Type="String" />
            <asp:SessionParameter Name="vLine" SessionField="line" Type="String" />
        </SelectParameters>
    </asp:ObjectDataSource>
     <asp:ObjectDataSource ID="ObjectDataSource3" runat="server" OldValuesParameterFormatString="original_{0}"
            SelectMethod="MachHrInt" TypeName="jobprod" >
            <SelectParameters>
                <asp:Parameter Name="prmUser" Type="String" />
                <asp:SessionParameter Name="prmOrderNum" SessionField="machhr" Type="String" />
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
                <asp:SessionParameter Name="prmOrderNum" SessionField="machhr" Type="String" />
                <asp:SessionParameter Name="vLine" SessionField="line" Type="String" />
                <asp:Parameter DefaultValue="Select" Name="prmAction" Type="String" />
                <asp:SessionParameter DefaultValue="" Name="vFormNo" SessionField="vFormNo" Type="String" />
                <asp:SessionParameter DefaultValue="" Name="vBlankNo" SessionField="vBlankNo" Type="String" />
                <asp:SessionParameter DefaultValue="" Name="prmItem" SessionField="prmItem" Type="String" />

            </SelectParameters>
        </asp:ObjectDataSource>
        &nbsp; &nbsp;
    </div>

</asp:Content>

