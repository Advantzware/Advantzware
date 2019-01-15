<%@ Page Language="C#" MasterPageFile="~/MasterPageFolding.master" Debug="true" AutoEventWireup="true" Inherits="fold_print" Title="Print" Codebehind="fold_print.aspx.cs" %>
<asp:Content ID="Content1" ContentPlaceHolderID="ContentPlaceHolder1" Runat="Server">

<asp:ScriptManager ID="ScriptManager1" runat="server" EnablePageMethods="true">
</asp:ScriptManager>   
<script>

window.onload = setfocus;
function setfocus()
{
    if(document.getElementById("ctl00_ContentPlaceHolder1_Print_FormView_vMarginTextBox"))
    {
        var margin = document.getElementById("ctl00_ContentPlaceHolder1_Print_FormView_vMarginTextBox");
        margin.focus();
    }
}
function margin() {
    var hidd = document.getElementById("ctl00_ContentPlaceHolder1_HiddenField1");
    hidd.value = "M";
        var mar=document.getElementById("ctl00_ContentPlaceHolder1_Print_FormView_vMarginTextBox").value;
        if (mar.indexOf(".") != -1) {
            return;
        }
        else if (mar.length > 2 && mar.length < 4) {
            mar = mar + ".";
            document.getElementById("ctl00_ContentPlaceHolder1_Print_FormView_vMarginTextBox").value = mar;
        }
        if (parseFloat(mar) > 100) {
            alert(" must be less than 100");
            return;
        }
    }
    function gross() {
        var hidd = document.getElementById("ctl00_ContentPlaceHolder1_HiddenField1");
        hidd.value = "G";
        var mar = document.getElementById("ctl00_ContentPlaceHolder1_Print_FormView_vGrossTextBox").value;
        if (mar.indexOf(".") != -1) {
            return;
        }
        else if (mar.length > 2 && mar.length < 4) {
            mar = mar + ".";
            document.getElementById("ctl00_ContentPlaceHolder1_Print_FormView_vGrossTextBox").value = mar;
        }
        if (parseFloat(mar) > 100) {
            alert(" must be less than 100");
            return;
        }
    }
    
    function sellprice() {
        var hidd = document.getElementById("ctl00_ContentPlaceHolder1_HiddenField1");
        hidd.value = "S";
        var sellp=document.getElementById("ctl00_ContentPlaceHolder1_Print_FormView_vSellPriceTextBox").value;
        if(sellp.indexOf(".") != -1)
            {        
                return;
            } 
        else if(sellp.length > 5 && sellp.length < 7)
        sellp=sellp + ".";
        document.getElementById("ctl00_ContentPlaceHolder1_Print_FormView_vSellPriceTextBox").value = sellp;
    }
    function callHardCopy() 
    {
        window.open("FoldEstimatePrint.aspx", "ContactCustomerLookupWindow", "width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
    }
    function Btn_calc_Click() {

        var type = document.getElementById("ctl00_ContentPlaceHolder1_FormView_MiscSub_typeLabel").innerHTML;
       
        var rowcnt = "no";
        var grid = window["<%= Print_GridView.ClientID %>"];
        var totalrowcount = grid.rows.length;
        if (totalrowcount > 1) {
            rowcnt = "yes";
        }
        
        if (type == "1") {
            window.open("fold_analysis.aspx?rowcnt=" + rowcnt + "", "foldingAnalysisWindow", "width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
        }
        if (type == "2") {
            window.open("fold_analysis_set.aspx?rowcnt=" + rowcnt + "", "foldingAnalysisWindow", "width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
        }
        if (type == "4") {
            window.open("fold_analysis1.aspx?rowcnt=" + rowcnt + "", "foldingAnalysisWindow", "width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
        }
    }

    function Btn_item_Click() {
        var type = document.getElementById("ctl00_ContentPlaceHolder1_FormView_MiscSub_typeLabel").innerHTML;

        if (type != "1")
            window.open("fold_estimate_probeit.aspx", "EstimateProbeitWindow", "width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
    }
</script>
     <asp:HiddenField ID="HiddenField1" runat="server" />
    <asp:FormView ID="FormView_MiscSub" runat="server" OnDataBound="FormView_MiscSub_ondataBound" DataSourceID="ObjectDataSource_top">
        <ItemTemplate>
            <fieldset class="shade">
                <legend>Reference Information:</legend>
                    <table>
                        <tr>
                            <td nowrap align="right" style="padding-right:5px;"><b>Estimate:</b></td>
                            <td nowrap><b><asp:Label ID="vEstimateLabel" runat="server" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" Width="80px" Text='<%# Bind("vEstimate") %>'></asp:Label></b></td>
                            <td nowrap align="right" style="padding-right:5px;"><b>Est Date:</b></td>
                            <td nowrap><b><asp:Label ID="vEstDateLabel" runat="server" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" Width="80px" Text='<%# Bind("vEstDate","{0:MM/dd/yyyy}") %>'></asp:Label></b></td>
                            <td nowrap align="right" style="padding-right:5px;"><b>Frm:</b></td>
                            <td nowrap><b>
                                    <asp:Label ID="vFormLabel" runat="server" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" Width="40px" Text='<%# Bind("vForm") %>'></asp:Label>
                                    of
                                    <asp:Label ID="vFormQtyLabel" runat="server" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" Width="40px" Text='<%# Bind("vFormQty") %>'></asp:Label>
                            </b></td>
                            <td nowrap align="right" style="padding-right:5px;"><b>Blk</b></td>
                            <td nowrap><b>
                                  <asp:Label ID="vBlkLabel" runat="server" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" Width="40px" Text='<%# Bind("vBlk") %>'></asp:Label>
                                  of
                                  <asp:Label ID="vBlkQtyLabel" runat="server" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" Width="40px" Text='<%# Bind("vBlkQty") %>'></asp:Label>  
                            </b></td>
                            <td nowrap align="right" style="padding-right:5px;"><b>Cust Part:</b></td>
                            <td nowrap><b><asp:Label ID="vCustPartLabel" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" Width="120px" runat="server" Text='<%# Bind("vCustPart") %>'></asp:Label></b></td>
                            <td style="display:none"><b><asp:Label ID="typeLabel" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" Width="80px" runat="server" Text='<%# Bind("vType") %>'></asp:Label></b>
                            <asp:Label ID="LogicLabel" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" Width="80px" runat="server" Text='<%# Bind("vLogic") %>'></asp:Label></td>
                        </tr>
                    </table>
            </fieldset>
         </ItemTemplate>
        
    </asp:FormView>
    <asp:GridView ID="Print_GridView" runat="server" AutoGenerateColumns="False"  DataSourceID="Fold_Print_ObjectDataSource" OnSelectedIndexChanged="Print_GridView_SelectedIndexChanged"
     AllowPaging="True" AllowSorting="True" EmptyDataText="No Records Found" OnRowDataBound="GridView_RowDataBound"  Width="100%" BorderStyle="Dotted" CssClass="Grid" PageSize="50">
        <SelectedRowStyle CssClass="GridSelected" BackColor="Yellow" />
        <AlternatingRowStyle CssClass="GridItemOdd" />
        <EmptyDataRowStyle BorderStyle="Dotted" BorderColor="Gray" BorderWidth="0px" Font-Bold="True" HorizontalAlign="Center" VerticalAlign="Middle" />
        <RowStyle CssClass="shade"  />
        <HeaderStyle CssClass="gridrowhdr" HorizontalAlign="Center" VerticalAlign="Middle" Wrap="False" BackColor="Teal" ForeColor="White" Height="40px" />
        <Columns>
            <asp:CommandField ShowSelectButton="True" ButtonType="Image" SelectImageUrl="~/Images/sel.gif" SelectText="" > 
                <ItemStyle Width="10px" />
            </asp:CommandField>
            
            <asp:TemplateField HeaderText="Line" Visible="false">
                <ItemTemplate>
                    <asp:Label ID="Label1" runat="server"  Text='<%# Bind("vLine") %>'></asp:Label>
                </ItemTemplate>
            </asp:TemplateField>
            
            <asp:BoundField DataField="vQty" HeaderText="Qty" SortExpression="vQty" />
            <asp:BoundField DataField="vTotalFactCost" HeaderText="TotalFactCost" SortExpression="vTotalFactCost" DataFormatString="{0:###,##0.00}" />
            <asp:BoundField DataField="vFullCost" HeaderText="FullCost" SortExpression="vFullCost" DataFormatString="{0:###,##0.00}" />
            <asp:BoundField DataField="vMargin" HeaderText="Margin%" SortExpression="vMargin" DataFormatString="{0:###,##0.00}" />
            
            <%--<asp:TemplateField HeaderText="Comm%" SortExpression="vComm">
            <ItemTemplate>
            <asp:Label ID="vCommLabel" runat="server" Text='<%# Bind("[vComm]","{0:###,##0.00}") %>'></asp:Label>
            </ItemTemplate>
            </asp:TemplateField>
            <asp:TemplateField HeaderText="Gross" SortExpression="vGross">
            <ItemTemplate>
            <asp:Label ID="vGrossLabel" runat="server" Text='<%# Bind("[vGross]","{0:###,##0.00}") %>'></asp:Label>
            </ItemTemplate>
            </asp:TemplateField>--%>
             <asp:BoundField DataField="vComm" HeaderText="Comm%" SortExpression="vComm" DataFormatString="{0:###,##0.00}" />
              <asp:BoundField DataField="vGross" HeaderText="Gross%" SortExpression="vGross" DataFormatString="{0:###,##0.00}" />
            <asp:BoundField DataField="vNet" HeaderText="Net%" SortExpression="vNet" DataFormatString="{0:###,##0.00}" />
            <asp:BoundField DataField="vSellPrice" HeaderText="SellPrice" SortExpression="vSellPrice" DataFormatString="{0:###,##0.00}" />
            <asp:BoundField DataField="vTotalSheet" HeaderText="TotalSheet" SortExpression="vTotalSheet" />
            <asp:BoundField DataField="vQty2" HeaderText="Q" SortExpression="vQty2" />
            <asp:BoundField DataField="vPriceBsf" HeaderText="PriceBsf" SortExpression="vPriceBsf" DataFormatString="{0:###,##0.00}" />
            <asp:BoundField DataField="vProbeDate" HeaderText="ProbeDate" SortExpression="vProbeDate" HtmlEncode="False" DataFormatString="{0:MM/dd/yyyy}" />
            <asp:BoundField DataField="vProbeBy" HeaderText="ProbeBy" SortExpression="vProbeBy" />
            <asp:BoundField DataField="vShipWeight" HeaderText="ShipWeight" SortExpression="vShipWeight" />
            <asp:BoundField DataField="vTotalMsf" HeaderText="TotalMsf" SortExpression="vTotalMsf" DataFormatString="{0:###,##0.00}" />
            <asp:BoundField DataField="vTime" HeaderText="Time" SortExpression="vTime" />
           
        </Columns>
    </asp:GridView>
    
    <asp:FormView ID="Print_FormView" runat="server" OnDataBound="print_formview_databound" DataSourceID="Fold_Print_ObjectDataSource2">
        <EditItemTemplate>
            <asp:Panel ID="Panel_Edit" runat="server" DefaultButton="UpdateButton">
               <asp:UpdatePanel ID="LengUpdate" runat="server">
                <ContentTemplate>
                <div >
                <asp:UpdateProgress ID="UpdateProgress1" runat="server" 
                    AssociatedUpdatePanelID="LengUpdate"
                    DisplayAfter="100" DynamicLayout="true">                    
                    <ProgressTemplate>                       
                        <asp:Label ID="lblProgress" runat="server" ></asp:Label>               
                    Please wait ...             
                    </ProgressTemplate>                    
                </asp:UpdateProgress>
                </div>
              
              <fieldset class="shade">
                    <table>
                    <tr>
                    <td align="right" style="padding-right:5px;"><b>Qty:</b></td>
                    <td> <asp:Label ID="vQtyLabel" runat="server" Width="100px" BackColor="Turquoise" Text='<%# Bind("vQty") %>'></asp:Label></td>
                    <td align="right" style="padding-right:5px;"><b>Tot Fact cost:</b></td>
                    <td> <asp:Label ID="vTotalFactCostLabel" runat="server" Width="100px" BackColor="Turquoise" Text='<%# Bind("vTotalFactCost","{0:######0.00}") %>'></asp:Label></td>
                    </tr>
                    <tr>
                    <td align="right" style="padding-right:5px;"><b>Full cost:</b></td>
                    <td><asp:Label ID="vFullCostLabel" runat="server" Width="100px" BackColor="Turquoise" Text='<%# Bind("vFullCost") %>'></asp:Label></td>
                    <td align="right" style="padding-right:5px;"><b>Margin%:</b></td>
                    <td><asp:TextBox ID="vMarginTextBox" MaxLength="6" onkeyup="margin()" OnTextChanged="cal_text_change" AutoPostBack="true" Width="95px"  runat="server" Text='<%# Bind("vMargin","{0:######0.00}") %>'></asp:TextBox>
                                <asp:CompareValidator ID="CompareValidator1" ControlToValidate="vMarginTextBox" Operator="dataTypeCheck" Type="double" Display="dynamic" runat="server" ErrorMessage="Only Decimal Value"></asp:CompareValidator> </td>
                    </tr>
                    <tr>
                    <td align="right" style="padding-right:5px;"><b><asp:Label ID="commnamelabel" runat="server" Text="Comm%"></asp:Label>
                    <asp:Label ID="grossnameLabel" runat="server" Text="Gross"></asp:Label></b></td>
                    <td><asp:Label ID="vCommLabel" runat="server" Width="100px" BackColor="Turquoise" Text='<%# Bind("vComm") %>'></asp:Label>
                    <asp:TextBox ID="vGrossTextBox"  onkeyup="gross()" OnTextChanged="cal_text_change" AutoPostBack="true" Width="95px"  runat="server" Text='<%# Bind("vGross","{0:######0.00}") %>'></asp:TextBox></td>
                    <td align="right" style="padding-right:5px;"><b>Net%</b></td>
                    <td> <asp:Label ID="vNetLabel" runat="server" Width="100px" BackColor="Turquoise" Text='<%# Bind("vNet") %>'></asp:Label></td>
                    </tr>
                    <tr>
                    <td align="right" style="padding-right:5px;"><b>Selling Price:</b></td>
                    <td><asp:TextBox ID="vSellPriceTextBox" MaxLength="9" onkeyup="sellprice()" OnTextChanged="cal_text_change" AutoPostBack="true" Width="95px"  runat="server" Text='<%# Bind("vSellPrice","{0:######0.00}") %>'></asp:TextBox>
                                    <asp:CompareValidator ID="CompareValidator2" ControlToValidate="vSellPriceTextBox" Operator="dataTypeCheck" Type="double" Display="dynamic" runat="server" ErrorMessage="Only Decimal Value"></asp:CompareValidator> </td>
                    <td align="right" style="padding-right:5px;"><b>Total Sheets:</b></td> 
                    <td><asp:Label ID="vTotalSheetLabel" runat="server" Width="100px" BackColor="Turquoise" Text='<%# Bind("vTotalSheet") %>'> </asp:Label></td>                                   
                    </tr>
                    <tr>
                    <td nowrap align="right" style="padding-right:5px;"><b>Q:</b></td>
                            <td nowrap><asp:DropDownList ID="vQty2TextBox" runat="server" onblur="document.getElementById('ctl00_ContentPlaceHolder1_Print_FormView_vMarginTextBox').focus()" SelectedValue='<%# Bind("vQty2") %>'>
                                        <asp:ListItem  Value="Y">Y</asp:ListItem>
                                        <asp:ListItem Value="N">N</asp:ListItem>
                            </asp:DropDownList></td>
                    <td align="right" style="padding-right:5px;"><b>Price Bsf:</b></td>                            
                    <td><asp:Label ID="vPriceBsfLabel" runat="server" Width="100px" BackColor="Turquoise" Text='<%# Bind("vPriceBsf") %>'></asp:Label></td>
                    </tr>
                    <tr>
                    <td align="right" style="padding-right:5px;"><b>Probe Date:</b></td>
                    <td> <asp:Label ID="vProbeDateLabel" runat="server" Width="100px" BackColor="Turquoise" Text='<%# Bind("vProbeDate","{0:MM/dd/yyyy}") %>'> </asp:Label></td>
                    <td align="right" style="padding-right:5px;"><b>Probe By:</b></td>
                    <td><asp:Label ID="vProbeByLabel" runat="server" Width="100px" BackColor="Turquoise" Text='<%# Bind("vProbeBy") %>'></asp:Label></td>
                    </tr>
                    <tr>
                    <td align="right" style="padding-right:5px;"><b>Shipping Width:</b></td>
                    <td><asp:Label ID="vShipWeightLabel" runat="server" Width="100px" BackColor="Turquoise" Text='<%# Bind("vShipWeight") %>'></asp:Label></td>
                    <td align="right" style="padding-right:5px;"><b>Total MSF:</b></td>
                    <td><asp:Label ID="vTotalMsfLabel" runat="server" Width="100px" BackColor="Turquoise" Text='<%# Bind("vTotalMsf") %>'></asp:Label></td>
                    </tr>
                                            
                    </table>
              </fieldset>          
                 </ContentTemplate></asp:UpdatePanel>       
           
            <asp:Button ID="UpdateButton" runat="server" CausesValidation="True" CssClass="buttonM" OnClick="Btn_Update_Click"
                Text="Save">
            </asp:Button>
            <asp:Button ID="UpdateCancelButton" runat="server" CssClass="buttonM" CausesValidation="False" CommandName="Cancel"
                Text="Cancel">
            </asp:Button>
            </asp:Panel>
        </EditItemTemplate>
        
        <ItemTemplate>
           
            <asp:Button ID="Update_Button" runat="server" Text="WhatIf" CommandName="Edit" CssClass="buttonM" />
            <asp:Button ID="Delete_Button" runat="server" Text="Delete" CssClass="buttonM" OnClientClick="return confirm('Are you sure you want to delete this record')" OnClick="Delete_Button_Click" />
            <input id="ItemButton" runat="server" class="button"  type="button" OnClick="Btn_item_Click()" value="Item" />
            <asp:Button ID="Btn_view" runat="server" Text="View" CssClass="buttonM" OnClick="Btn_view_Click" />
            <input id="Btn_calc_formview" runat="server" class="button"  type="button" OnClick="Btn_calc_Click()" value="Calculate" />                        
            <input type="button" value="Hard Copy" id="Btn_HardCopy" class="buttonM" onclick="callHardCopy()" />
             <asp:Button ID="QuoteButton" runat="server" Text="Quote" CssClass="buttonM" OnClick="Quote_Button_Click" />
            
           <%-- <asp:Button ID="ImpPricButton" runat="server" Text="Import Price" CssClass="buttonM" OnClick="Btn_importprice_Click" />--%>            
        </ItemTemplate>
    </asp:FormView>
    <input id="Btn_calc" runat="server" class="button" OnClick="Btn_calc_Click()" type="button" value="Calculate" />
    <asp:ObjectDataSource ID="Fold_Print_ObjectDataSource" runat="server" OldValuesParameterFormatString="original_{0}"
        SelectMethod="SelectFoldPrint" TypeName="Corrugated">
        <SelectParameters>
            <asp:Parameter Name="prmUser" Type="String" />
            <asp:Parameter Name="prmAction" Type="String" />
            <asp:Parameter Name="prmType" Type="String" />
            <asp:Parameter Name="prmComp" Type="String" />
            <asp:SessionParameter Name="prmEstimate" SessionField="order_folding_est" Type="String" />
            <asp:Parameter Name="prmEstDate" Type="DateTime" />
            <asp:SessionParameter Name="prmForm" SessionField="order_folding_formno" Type="Int32" />
            <asp:Parameter Name="prmFormQty" Type="Int32" />
            <asp:Parameter Name="prmBlk" Type="Int32" />
            <asp:Parameter Name="prmBlkQty" Type="Int32" />
            <asp:Parameter Name="prmCustPart" Type="String" />
            <asp:Parameter Name="prmQty" Type="Int32" />
            <asp:Parameter Name="prmTotalFactCost" Type="Decimal" />
            <asp:Parameter Name="prmFullCost" Type="Decimal" />
            <asp:Parameter Name="prmMargin" Type="Decimal" />
            <asp:Parameter Name="prmComm" Type="Decimal" />
            <asp:Parameter Name="prmNet" Type="Decimal" />
            <asp:Parameter Name="prmSellPrice" Type="Decimal" />
            <asp:Parameter Name="prmTotalSheet" Type="Int32" />
            <asp:Parameter Name="prmQty2" Type="String" />
            <asp:Parameter Name="prmPriceBsf" Type="Decimal" />
            <asp:Parameter Name="prmProbeDate" Type="DateTime" />                        
            <asp:Parameter Name="prmProbeBy" Type="String" />
            <asp:Parameter Name="prmShipWeight" Type="Decimal" />
            <asp:Parameter Name="prmTotalMsf" Type="Decimal" />
            <asp:Parameter Name="prmTime" Type="String" />
            <asp:Parameter Name="prmLine" Type="Int32" />
            <asp:Parameter Name="prmGross" Type="Decimal" />
             <asp:SessionParameter Name="prmBlank" SessionField="order_folding_blankno" Type="int32" />
        </SelectParameters>
    </asp:ObjectDataSource>
    
    <asp:ObjectDataSource ID="Fold_Print_ObjectDataSource2" runat="server" OldValuesParameterFormatString="original_{0}"
        SelectMethod="SelectFoldPrint" TypeName="Corrugated">
        <SelectParameters>
            <asp:Parameter Name="prmUser" Type="String" />
            <asp:Parameter Name="prmAction" Type="String" />
            <asp:Parameter Name="prmType" Type="String" />
            <asp:Parameter Name="prmComp" Type="String" />
            <asp:SessionParameter Name="prmEstimate" SessionField="order_folding_est" Type="String" />
            <asp:Parameter Name="prmEstDate" Type="DateTime" />
            <asp:SessionParameter Name="prmForm" SessionField="order_folding_formno" Type="Int32" />
            <asp:Parameter Name="prmFormQty" Type="Int32" />
            <asp:Parameter Name="prmBlk" Type="Int32" />
            <asp:Parameter Name="prmBlkQty" Type="Int32" />
            <asp:Parameter Name="prmCustPart" Type="String" />
            <asp:Parameter Name="prmQty" Type="Int32" />
            <asp:Parameter Name="prmTotalFactCost" Type="Decimal" />
            <asp:Parameter Name="prmFullCost" Type="Decimal" />
            <asp:Parameter Name="prmMargin" Type="Decimal" />
            <asp:Parameter Name="prmComm" Type="Decimal" />
            <asp:Parameter Name="prmNet" Type="Decimal" />
            <asp:Parameter Name="prmSellPrice" Type="Decimal" />
            <asp:Parameter Name="prmTotalSheet" Type="Int32" />
            <asp:Parameter Name="prmQty2" Type="String" />
            <asp:Parameter Name="prmPriceBsf" Type="Decimal" />
            <asp:Parameter Name="prmProbeDate" Type="DateTime" />                        
            <asp:Parameter Name="prmProbeBy" Type="String" />
            <asp:Parameter Name="prmShipWeight" Type="Decimal" />
            <asp:Parameter Name="prmTotalMsf" Type="Decimal" />
            <asp:Parameter Name="prmTime" Type="String" />
            <asp:SessionParameter SessionField="fold_est_line" Name="prmLine" Type="Int32" />
            <asp:Parameter Name="prmGross" Type="Decimal" />
            <asp:SessionParameter Name="prmBlank" SessionField="order_folding_blankno" Type="int32" />
        </SelectParameters>
    </asp:ObjectDataSource>
    
    
    <asp:FormView ID="FormView1" Visible="false" runat="server" DataSourceID="ObjectDataSource2">
        
       
        <ItemTemplate>
           
            <asp:Label ID="vCorrEstimateFileLabel" runat="server" Text='<%# Bind("vCorrEstimateFile") %>' />
            
        </ItemTemplate>
    </asp:FormView>
    <asp:ObjectDataSource ID="ObjectDataSource2" runat="server" 
        OldValuesParameterFormatString="original_{0}" 
        SelectMethod="CorrugatedEstimatePrint" TypeName="Corrugated">
        <SelectParameters>
            <asp:Parameter Name="prmUser" Type="String" />
            <asp:SessionParameter Name="prmAction" SessionField="order_folding_est" Type="String" />
            <asp:Parameter Name="prmOut" Type="String" />
            <asp:Parameter Name="vFromDept" Type="String" />
            <asp:Parameter Name="vToDept" Type="String" />
            <asp:Parameter Name="vTbPrtBox" Type="String" />
            <asp:Parameter Name="vTbPrtNote" Type="String" />
            <asp:Parameter Name="prmEstimate" Type="String" />
            <asp:SessionParameter Name="prmFormNo" SessionField="order_folding_formno"  Type="Int32" />
            <asp:SessionParameter Name="prmBlankNo" Type="Int32" />
            <asp:SessionParameter  Name="prmLine" SessionField="fold_est_line"  Type="Int32" />
        </SelectParameters>
    </asp:ObjectDataSource>
    
    <asp:ObjectDataSource ID="ObjectDataSource_top" runat="server" OldValuesParameterFormatString="original_{0}"
        SelectMethod="SelectFoldPrint" TypeName="Corrugated">
        <SelectParameters>
            <asp:Parameter Name="prmUser" Type="String" />
            <asp:Parameter DefaultValue="topshow" Name="prmAction" Type="String" />
            <asp:Parameter Name="prmType" Type="String" />
            <asp:Parameter Name="prmComp" Type="String" />
            <asp:SessionParameter Name="prmEstimate" SessionField="order_folding_est" Type="String" />
            <asp:Parameter Name="prmEstDate" Type="DateTime" />
            <asp:SessionParameter Name="prmForm" SessionField="order_folding_formno" Type="Int32" />
            <asp:Parameter Name="prmFormQty" Type="Int32" />
            <asp:Parameter Name="prmBlk" Type="Int32" />
            <asp:Parameter Name="prmBlkQty" Type="Int32" />
            <asp:Parameter Name="prmCustPart" Type="String" />
            <asp:Parameter Name="prmQty" Type="Int32" />
            <asp:Parameter Name="prmTotalFactCost" Type="Decimal" />
            <asp:Parameter Name="prmFullCost" Type="Decimal" />
            <asp:Parameter Name="prmMargin" Type="Decimal" />
            <asp:Parameter Name="prmComm" Type="Decimal" />
            <asp:Parameter Name="prmNet" Type="Decimal" />
            <asp:Parameter Name="prmSellPrice" Type="Decimal" />
            <asp:Parameter Name="prmTotalSheet" Type="Int32" />
            <asp:Parameter Name="prmQty2" Type="String" />
            <asp:Parameter Name="prmPriceBsf" Type="Decimal" />
            <asp:Parameter Name="prmProbeDate" Type="DateTime" />                        
            <asp:Parameter Name="prmProbeBy" Type="String" />
            <asp:Parameter Name="prmShipWeight" Type="Decimal" />
            <asp:Parameter Name="prmTotalMsf" Type="Decimal" />
            <asp:Parameter Name="prmTime" Type="String" />
            <asp:Parameter Name="prmLine" Type="Int32" />
            <asp:Parameter Name="prmGross" Type="Decimal" />
            <asp:SessionParameter Name="prmBlank" SessionField="order_folding_blankno" Type="int32" />
        </SelectParameters>
    </asp:ObjectDataSource>
    
    
</asp:Content>

