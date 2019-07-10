<%@ Page Language="C#" MasterPageFile="MasterPage5.master" Debug="true" AutoEventWireup="true" Inherits="rfq_list_item" Title="Request for Quote" Codebehind="rfq_list_item.aspx.cs" %>

<asp:Content ID="Content1" ContentPlaceHolderID="ContentPlaceHolder1" runat="server">



<div>
<fieldset style="background-color:#EFF3FB; width:700px;">
<legend>Reference Information</legend>
    <asp:FormView ID="FormView2" runat="server" DataSourceID="ObjectDataSource3" Width="700px">
       
        <ItemTemplate>
        
           <b>RFQ#:</b>
            <asp:Label ID="aRfqNoLabel" runat="server" BackColor="Turquoise" Width="143px" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" Text='<%# Bind("aRfqNo") %>'></asp:Label>
            &nbsp;&nbsp;&nbsp;&nbsp;
           <b> Customer#:</b>
            <asp:Label ID="aCustNoLabel" runat="server" BackColor="Turquoise" Width="163px" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" Text='<%# Bind("aCustNo") %>'></asp:Label>
            &nbsp;&nbsp;&nbsp;&nbsp;
           <b> Requested Date:</b>
            <asp:Label ID="vRfqDtLabel" runat="server" BackColor="Turquoise" Width="143px" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" Text='<%# Bind("vRfqDt","{0:MM/dd/yyyy}") %>'></asp:Label>
        </ItemTemplate>
    </asp:FormView>
    
</fieldset>
    <asp:ObjectDataSource ID="ObjectDataSource3" runat="server" OldValuesParameterFormatString="original_{0}"
        SelectMethod="RfqItemDscr" TypeName="rfqs">
        <SelectParameters>
            <asp:Parameter Name="prmUser" Type="String" />
            <asp:SessionParameter Name="prmRfqNo" SessionField="Rfqseq" Type="Int32" />
            <asp:SessionParameter SessionField="list_rfq_cust_part_no" Name="prmPartNo" Type="string" />
        </SelectParameters>
    </asp:ObjectDataSource>
</div>
<div>
    <br />
    <asp:GridView ID="GridView1" Width="700px" runat="server" AutoGenerateColumns="False" DataSourceID="ObjectDataSource1" AllowPaging="True" AllowSorting="True" BorderStyle="Dotted" CssClass="Grid" EmptyDataText="No Record Found" OnSelectedIndexChanged="GridView1_SelectedIndexChanged">
        <Columns>
         <asp:CommandField ShowSelectButton="True" ButtonType="Image" SelectImageUrl="~/Images/sel.gif" SelectText="" >
                    <ItemStyle Width="10px" />
                </asp:CommandField>
            
            <asp:BoundField DataField="RfqSeqNo" HeaderText="#" SortExpression="RfqSeqNo" />
            <asp:BoundField DataField="RfqQty" HeaderText="Qty" SortExpression="RfqQty" />
            <asp:BoundField DataField="RfqStock" HeaderText="FgItem#" SortExpression="RfqStock" />
            <asp:BoundField DataField="RfqName" HeaderText="Item Name" SortExpression="RfqName" />
            <asp:BoundField DataField="RfqPartno" HeaderText="Part#" SortExpression="RfqPartno" />
            <asp:BoundField DataField="Rfqstyle" HeaderText="Style" SortExpression="Rfqstyle" />
            <asp:BoundField DataField="RfqProcat" HeaderText="Category" SortExpression="RfqProcat" />
            <asp:BoundField DataField="RfqCol" HeaderText="Color" SortExpression="RfqCol" />
            <asp:BoundField DataField="RfqCoat" HeaderText="Coating" SortExpression="RfqCoat" />
            <asp:BoundField DataField="RfqLength" HeaderText="Length" SortExpression="RfqLength" />
            <asp:BoundField DataField="RfqWidth" HeaderText="Width" SortExpression="RfqWidth" />
            <asp:BoundField DataField="RfqDepth" HeaderText="Depth" SortExpression="RfqDepth" />
            <asp:BoundField DataField="RfqBoard" HeaderText="Board" SortExpression="RfqBoard" />
            <asp:BoundField DataField="RfqCal" HeaderText="Caliper" SortExpression="RfqCal" />
            <asp:BoundField DataField="RfqQuantity" HeaderText="Qty/Set" SortExpression="RfqQuantity" />
            <asp:TemplateField Visible="false" HeaderText="Type" SortExpression="RfqType">
                <EditItemTemplate>
                    <asp:TextBox ID="TextBox1" runat="server" Text='<%# Bind("RfqType") %>'></asp:TextBox>
                </EditItemTemplate>
                <ItemTemplate>
                    <asp:Label ID="Label1" runat="server" Text='<%# Bind("RfqType") %>'></asp:Label>
                </ItemTemplate>
            </asp:TemplateField>
            
        </Columns>
        <EmptyDataRowStyle BorderColor="Gray" BorderStyle="Dotted" Width="100%" BorderWidth="0px" Font-Bold="True"
            HorizontalAlign="Center" VerticalAlign="Middle" Wrap="False" />
            <SelectedRowStyle CssClass="GridSelected" BackColor="Yellow" />
        <HeaderStyle   ForeColor="White" CssClass="headcolor" HorizontalAlign="Center"
            VerticalAlign="Middle" Wrap="False" />
        <RowStyle CssClass="shade" />
        <AlternatingRowStyle CssClass="GridItemOdd" />
    </asp:GridView>
    <asp:ObjectDataSource ID="ObjectDataSource1" runat="server" OldValuesParameterFormatString="original_{0}"
        SelectMethod="SelectRfqitem" TypeName="rfqs">
        <SelectParameters>
            <asp:Parameter Name="prmComp" Type="String" />
            <asp:Parameter Name="prmUser" Type="String" />
            <asp:Parameter Name="prmAction" DefaultValue="Select" Type="String" />
            <asp:SessionParameter Name="prmRfqNo" SessionField="Rfqseq" Type="Int32" />
            <asp:Parameter Name="RfqSeqNo" Type="Int32" />
            
        </SelectParameters>
    </asp:ObjectDataSource>
    
    <asp:Button ID="EstimateButton" runat="server" CausesValidation="False"  CssClass="buttonM" 
               OnClick = "rfq_estimate"     Text="Submit Quote">
                </asp:Button>
</div>
</asp:Content>
