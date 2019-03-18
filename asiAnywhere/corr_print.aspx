<%@ Page Language="C#" MasterPageFile="~/MasterPageCorrugated.master" Debug="false" AutoEventWireup="true" Inherits="corr_print" Title="Print" Codebehind="corr_print.aspx.cs" %>
<asp:Content ID="Content1" ContentPlaceHolderID="ContentPlaceHolder1" Runat="Server">
<script>

window.onload=setfocus;
function setfocus()
{
    if(document.getElementById("ctl00_ContentPlaceHolder1_Print_FormView_vGrossTextBox"))
    {
        var gross=document.getElementById("ctl00_ContentPlaceHolder1_Print_FormView_vGrossTextBox");
        //gross.focus();
    }
}


function gross() {
    var hidden = document.getElementById("ctl00_ContentPlaceHolder1_HiddenField1");
    hidden.value = "G";
        var gros=document.getElementById("ctl00_ContentPlaceHolder1_Print_FormView_vGrossTextBox").value;
        if (gros.indexOf(".") != -1) {
            return;
        }
        else if (gros.length > 2 && gros.length < 4) {
            gros = gros + ".";
            document.getElementById("ctl00_ContentPlaceHolder1_Print_FormView_vGrossTextBox").value = gros;
        }
    }
    function net() {
        var hidden = document.getElementById("ctl00_ContentPlaceHolder1_HiddenField1");
        hidden.value = "N";
        var nett=document.getElementById("ctl00_ContentPlaceHolder1_Print_FormView_vNetTextBox").value;
        if (nett.indexOf(".") != -1) {
            return;
        }
        else if (nett.length > 2 && nett.length < 4) {
            nett = nett + ".";
            document.getElementById("ctl00_ContentPlaceHolder1_Print_FormView_vNetTextBox").value = nett;
        }
    }
    function selprice() {
        var hidden = document.getElementById("ctl00_ContentPlaceHolder1_HiddenField1");
        hidden.value = "S";
        var sellp=document.getElementById("ctl00_ContentPlaceHolder1_Print_FormView_vTextSellPrice").value;
        if (sellp.indexOf(".") != -1) {
            return;
        }
        else if (sellp.length > 6 && sellp.length < 8) {
            sellp = sellp + ".";
            document.getElementById("ctl00_ContentPlaceHolder1_Print_FormView_vTextSellPrice").value = sellp;
        }
    }
    function board() {
        var hidden = document.getElementById("ctl00_ContentPlaceHolder1_HiddenField1");
        hidden.value = "B";
        var brd=document.getElementById("ctl00_ContentPlaceHolder1_Print_FormView_vTextBoardPer").value;
        if (brd.indexOf(".") != -1) {
            return;
        }
        else if (brd.length > 2 && brd.length < 4) {
            brd = brd + ".";
            document.getElementById("ctl00_ContentPlaceHolder1_Print_FormView_vTextBoardPer").value = brd;
        }
    }
    function boardcontm() {
        var hidden = document.getElementById("ctl00_ContentPlaceHolder1_HiddenField1");
        hidden.value = "BCM";
        var boardcm=document.getElementById("ctl00_ContentPlaceHolder1_Print_FormView_vTextBoardContM").value;
        if (boardcm.indexOf(".") != -1) {
            return;
        }
        else if (boardcm.length > 6 && boardcm.length < 8) {
            boardcm = boardcm + ".";
            document.getElementById("ctl00_ContentPlaceHolder1_Print_FormView_vTextBoardContM").value = boardcm;
        }
    }
    function boardcont() {
        var hidden = document.getElementById("ctl00_ContentPlaceHolder1_HiddenField1");
        hidden.value = "BC$";
        var boardc=document.getElementById("ctl00_ContentPlaceHolder1_Print_FormView_vTextBoardCont").value;
        if (boardc.indexOf(".") != -1) {
            return;
        }
        else if (boardc.length > 6 && boardc.length < 8) {
            boardc = boardc + ".";
            document.getElementById("ctl00_ContentPlaceHolder1_Print_FormView_vTextBoardCont").value = boardc;
        }
    }
    function callHardCopy()
    {
        window.open("CorrEstimatePrint.aspx","ContactCustomerLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
    }
    function Btn_calc_Click() {
        var type = document.getElementById("ctl00_ContentPlaceHolder1_FormView_MiscSub_typeLabel").innerHTML;

        var rowcnt = "no";
        var grid = window["<%= Print_GridView.ClientID %>"];
        var totalrowcount = grid.rows.length;
        if (totalrowcount > 1) {
            rowcnt = "yes";
        }

        if (type == "5") {            
            window.open("estimate_analysis.aspx?rowcnt="+ rowcnt +"", "EstimateAnalysisWindow", "width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
        }
        if (type == "6") {
            window.open("estimate_analysis2.aspx?rowcnt=" + rowcnt + "", "EstimateAnalysisWindow", "width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
        }
        if (type == "8") {
            window.open("estimate_analysis1.aspx?rowcnt=" + rowcnt + "", "EstimateAnalysisWindow", "width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
        }
    }

    function Btn_item_Click() {
        var type = document.getElementById("ctl00_ContentPlaceHolder1_FormView_MiscSub_typeLabel").innerHTML;
        
        if (type != "5") 
            window.open("estimate_probeit.aspx", "EstimateProbeitWindow", "width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
    }
</script>

    <asp:HiddenField ID="HiddenField1" runat="server" />
    <asp:FormView ID="FormView_MiscSub" runat="server" DataSourceID="ObjectDataSource1">
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
                            <td nowrap><b><asp:Label ID="vCustPartLabel" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" Width="180px" runat="server" Text='<%# Bind("vCustPart") %>'></asp:Label></b></td>
                            <td style="display:none"><b><asp:Label ID="typeLabel" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" Width="180px" runat="server" Text='<%# Bind("vType") %>'></asp:Label></b></td>
                            
                        </tr>
                    </table>
            </fieldset>
         </ItemTemplate>
    </asp:FormView>
    <asp:GridView ID="Print_GridView" runat="server" AutoGenerateColumns="False" DataSourceID="Print_ObjectDataSource" OnSelectedIndexChanged="Print_GridView_SelectedIndexChanged"
     AllowPaging="True" AllowSorting="True" EmptyDataText="No Records Found" Width="100%" BorderStyle="Dotted" CssClass="Grid" PageSize="50">
        <SelectedRowStyle CssClass="GridSelected" BackColor="Yellow" />
        <AlternatingRowStyle CssClass="GridItemOdd" />
        <EmptyDataRowStyle BorderStyle="Dotted" BorderColor="Gray" BorderWidth="0px" Font-Bold="True" HorizontalAlign="Center" VerticalAlign="Middle" />
        <RowStyle CssClass="shade"  />
        <HeaderStyle  HorizontalAlign="Center" VerticalAlign="Middle" Wrap="False" ForeColor="White" CssClass="headcolor" Height="40px" />
        <Columns>
            <asp:CommandField ShowSelectButton="True" ButtonType="Image" SelectImageUrl="~/Images/sel.gif" SelectText="" > 
                <ItemStyle Width="10px" />
            </asp:CommandField>                        
            
            <asp:BoundField DataField="vQty" HeaderText="Qty" SortExpression="vQty" />
            <asp:BoundField DataField="vTotalFactCost" HeaderText="Tot. Fact Cost" SortExpression="vTotalFactCost" DataFormatString="{0:###,##0.00}" />
            <asp:BoundField DataField="vFullCost" HeaderText="Full Cost" SortExpression="vFullCost" DataFormatString="{0:###,##0.00}" />
            <asp:BoundField DataField="vMargin" HeaderText="Margin%" SortExpression="vMargin" DataFormatString="{0:###,##0.00}" />
            <asp:BoundField DataField="vGross" HeaderText="Gross%" SortExpression="vGross" DataFormatString="{0:###,##0.00}" />
            <asp:BoundField DataField="vNet" HeaderText="Net%" SortExpression="vNet" DataFormatString="{0:###,##0.00}" />
            <asp:BoundField DataField="vSellPrice" HeaderText="Selling Price" SortExpression="vSellPrice" DataFormatString="{0:###,##0.00}" />
            <asp:BoundField DataField="vTotalSheet" HeaderText="Total Sheet" SortExpression="vTotalSheet" />
            <asp:BoundField DataField="vQty2" HeaderText="Q" SortExpression="vQty2" />
            <asp:BoundField DataField="vPriceBsf" HeaderText="Price/Bsf" SortExpression="vPriceBsf" />
            <asp:BoundField DataField="vProbeDate" HeaderText="Probe Date" SortExpression="vProbeDate" HtmlEncode="False" DataFormatString="{0:MM/dd/yyyy}" />
            <asp:BoundField DataField="vBoardM" HeaderText="Board/M" SortExpression="vBoardM" DataFormatString="{0:###,##0.00}" />
            <asp:BoundField DataField="vBoard" HeaderText="Board%" SortExpression="vBoard" DataFormatString="{0:###,##0.00}" />
            <asp:BoundField DataField="vBoardContM" HeaderText="Board Contrib/M" SortExpression="vBoardContM" DataFormatString="{0:###,##0.00}" />
            <asp:BoundField DataField="vBoardCont" HeaderText="Board Contrib$" SortExpression="vBoardCont" DataFormatString="{0:###,##0.00}" />
            <asp:BoundField DataField="vProbeBy" HeaderText="Probe By" SortExpression="vProbeBy" />
            <asp:BoundField DataField="vTotalMsf" HeaderText="TotalMsf" SortExpression="vTotalMsf" DataFormatString="{0:###,##0.00}" />
            <asp:BoundField DataField="vTime" HeaderText="Time" SortExpression="vTime" />
            <asp:TemplateField HeaderText="Line" Visible="false" SortExpression="vLine">
                <EditItemTemplate>
                    <asp:TextBox ID="TextBox1" runat="server" Text='<%# Bind("vLine") %>'></asp:TextBox>
                </EditItemTemplate>
                <ItemTemplate>
                    <asp:Label ID="Label1" runat="server" Text='<%# Bind("vLine") %>'></asp:Label>
                </ItemTemplate>
            </asp:TemplateField>
        </Columns>
    </asp:GridView>
    
    <asp:FormView ID="Print_FormView" runat="server" OnDataBound="print_formview_databound" DataSourceID="Print_ObjectDataSource2">
        <EditItemTemplate>
        <asp:Panel ID="Panel_Edit" runat="server" DefaultButton="UpdateButton">
            <table class="shade">
            <tr><td align="right" style="padding-right:5px"><b>Qty:</b></td>
            <td><asp:Label runat="server" ID="qtyLabel" BackColor="Turquoise" Width="85px" Text='<%# Bind("vQty") %>'></asp:Label></td>
            <td align="right" style="padding-right:5px"><b>Tot. Fact Cost:</b></td>
            <td><asp:Label runat="server" ID="Label2" BackColor="Turquoise" Width="85px" Text='<%# Bind("vTotalFactCost") %>'></asp:Label></td>
            </tr>
            <tr>
            <td align="right" style="padding-right:5px"><b>Full Cost:</b></td>
            <td><asp:Label runat="server" ID="Label3" BackColor="Turquoise" Width="85px" Text='<%# Bind("vFullCost") %>'></asp:Label></td>
            <td align="right" style="padding-right:5px"><b>Margin:</b></td>
            <td> <asp:Label runat="server" ID="Label4" BackColor="Turquoise" Width="85px" Text='<%# Bind("vMargin") %>'></asp:Label></td>
            </tr>
            
                <tr>
                    <td nowrap align="right" style="padding-right:5px;"><b>Gross%:</b></td>
                    <td nowrap><asp:TextBox ID="vGrossTextBox" OnTextChanged="cal_text_change" AutoPostBack="true" onkeyup="gross()" MaxLength="6" Width="80px" runat="server" Text='<%# Bind("vGross","{0:##0.00}") %>'></asp:TextBox>
                        <asp:CompareValidator ID="CompareValidator1" ControlToValidate="vGrossTextBox" Operator="dataTypeCheck" Type="double" Display="dynamic" runat="server" ErrorMessage="Only Decimal Value"></asp:CompareValidator> 
                    </td>
                    <td nowrap align="right" style="padding-right:5px;"><b>Net%:</b></td>
                    <td nowrap><asp:TextBox ID="vNetTextBox" onkeyup="net()" OnTextChanged="cal_text_change" AutoPostBack="true" MaxLength="6" Width="80px" runat="server" Text='<%# Bind("vNet","{0:##0.00}") %>'></asp:TextBox>
                                <asp:CompareValidator ID="CompareValidator2" ControlToValidate="vNetTextBox" Operator="dataTypeCheck" Type="double" Display="dynamic" runat="server" ErrorMessage="Only Decimal Value"></asp:CompareValidator> 
                    </td>
                </tr>
                <tr>
                    <td nowrap align="right" style="padding-right:5px;"><b>SellPrice:</b></td>
                    <td nowrap><asp:TextBox ID="vTextSellPrice" onkeyup="selprice()" OnTextChanged="cal_text_change" AutoPostBack="true" Width="80px" MaxLength="10" runat="server" Text='<%# Bind("vSellPrice","{0:######0.00}") %>'></asp:TextBox>
                                <asp:CompareValidator ID="CompareValidator3" ControlToValidate="vTextSellPrice" Operator="dataTypeCheck" Type="double" Display="dynamic" runat="server" ErrorMessage="Only Decimal Value"></asp:CompareValidator> 
                    </td>
                    <td nowrap align="right" style="padding-right:5px;"><b>Total Sheet:</b></td>
                    <td nowrap> <asp:Label runat="server" ID="Label5" BackColor="Turquoise" Width="85px" Text='<%# Bind("vTotalSheet") %>'></asp:Label></td>
                </tr>
                <tr>
                <td  align="right" style="padding-right:5px"><b>Q:</b></td>
                <td><asp:DropDownList ID="vDropDownQ" runat="server" SelectedValue='<%# Bind("vQty2") %>'>
                                    <asp:ListItem>Y</asp:ListItem>
                                    <asp:ListItem>N</asp:ListItem>
                                </asp:DropDownList></td>
                <td  align="right" style="padding-right:5px"><b>Price/Bsf:</b></td>
                <td> <asp:Label runat="server"  ID="Label6" BackColor="Turquoise" Width="85px" Text='<%# Bind("vPriceBsf") %>'></asp:Label></td>
                </tr>
                <tr>
                <td  align="right" style="padding-right:5px"><b>Probe Date:</b></td>
                <td> <asp:Label runat="server" ID="Label7" BackColor="Turquoise" Width="85px" Text='<%# Bind("vProbeDate","{0:MM/dd/yyyy}") %>'></asp:Label></td>
                <td  align="right" style="padding-right:5px"><b>Board/M</b></td>
                <td><asp:Label runat="server" ID="Label8" BackColor="Turquoise" Width="85px" Text='<%# Bind("vBoardM") %>'></asp:Label></td>
                </tr>                
                <tr>
                    <td nowrap align="right" style="padding-right:5px;"><b>Board%:</b></td>
                    <td nowrap><asp:TextBox ID="vTextBoardPer" onkeyup="board()"  OnTextChanged="cal_text_change" AutoPostBack="true" Width="80px" MaxLength="6" runat="server" Text='<%# Bind("vBoard","{0:##0.00}") %>'></asp:TextBox>
                                <asp:CompareValidator ID="CompareValidator4" ControlToValidate="vTextBoardPer" Operator="dataTypeCheck" Type="double" Display="dynamic" runat="server" ErrorMessage="Only Decimal Value"></asp:CompareValidator> 
                    </td>
                    <td nowrap align="right" style="padding-right:5px;"><b>BoardContM:</b></td>
                    <td nowrap><asp:TextBox ID="vTextBoardContM" Width="80px" onkeyup="boardcontm()" OnTextChanged="cal_text_change" AutoPostBack="true" MaxLength="10" runat="server" Text='<%# Bind("vBoardContM","{0:######0.00}") %>'></asp:TextBox>
                                <asp:CompareValidator ID="CompareValidator5" ControlToValidate="vTextBoardContM" Operator="dataTypeCheck" Type="double" Display="dynamic" runat="server" ErrorMessage="Only Decimal Value"></asp:CompareValidator> 
                    </td>
                </tr>
                <tr>
                    <td nowrap align="right" style="padding-right:5px;"><b>BoardCont$:</b></td>
                    <td nowrap><asp:TextBox ID="vTextBoardCont" Width="80px" onkeyup="boardcont()" OnTextChanged="cal_text_change" AutoPostBack="true" MaxLength="10" runat="server" Text='<%# Bind("vBoardCont","{0:######0.00}") %>'></asp:TextBox>
                                <asp:CompareValidator ID="CompareValidator6" ControlToValidate="vTextBoardCont" Operator="dataTypeCheck" Type="double" Display="dynamic" runat="server" ErrorMessage="Only Decimal Value"></asp:CompareValidator> 
                    </td>
                    <td  align="right" style="padding-right:5px"><b>Probe By:</b></td>
                    <td> <asp:Label runat="server" ID="Label9" Width="85px"  BackColor="Turquoise" Text='<%# Bind("vProbeBy") %>'></asp:Label></td>                    
                </tr>
                <tr>
                <td  align="right" style="padding-right:5px"><b>TotalMsf:</b></td>
                <td> <asp:Label runat="server" ID="Label10" BackColor="Turquoise" Width="85px" Text='<%# Bind("vTotalMsf") %>'></asp:Label></td>
                <td align="right" style="padding-right:5px"><b>Time:</b></td>
                <td><asp:Label runat="server" ID="Label11" BackColor="Turquoise" Width="85px" Text='<%# Bind("vTime") %>'></asp:Label></td>
                </tr>
            </table>         
              
             
              
            <asp:Button ID="UpdateButton" CssClass="buttonM" runat="server" CausesValidation="True" OnClick="Btn_Update_Click"
                Text="Save">
            </asp:Button>
            <asp:Button ID="UpdateCancelButton" CssClass="buttonM" runat="server" CausesValidation="False" CommandName="Cancel"
                Text="Cancel">
            </asp:Button>
            </asp:Panel>
        </EditItemTemplate>
        <ItemTemplate>
           <%-- vEstimate:
            <asp:Label ID="vEstimateLabel" runat="server" Text='<%# Bind("vEstimate") %>'></asp:Label><br />
            vEstDate:
            <asp:Label ID="vEstDateLabel" runat="server" Text='<%# Bind("vEstDate") %>'></asp:Label><br />
            vForm:
            <asp:Label ID="vFormLabel" runat="server" Text='<%# Bind("vForm") %>'></asp:Label><br />
            vFormQty:
            <asp:Label ID="vFormQtyLabel" runat="server" Text='<%# Bind("vFormQty") %>'></asp:Label><br />
            vBlk:
            <asp:Label ID="vBlkLabel" runat="server" Text='<%# Bind("vBlk") %>'></asp:Label><br />
            vBlkQty:
            <asp:Label ID="vBlkQtyLabel" runat="server" Text='<%# Bind("vBlkQty") %>'></asp:Label><br />
            vCustPart:
            <asp:Label ID="vCustPartLabel" runat="server" Text='<%# Bind("vCustPart") %>'></asp:Label><br />
            vQty:
            <asp:Label ID="vQtyLabel" runat="server" Text='<%# Bind("vQty") %>'></asp:Label><br />
            vTotalFactCost:
            <asp:Label ID="vTotalFactCostLabel" runat="server" Text='<%# Bind("vTotalFactCost") %>'>
            </asp:Label><br />
            vFullCost:
            <asp:Label ID="vFullCostLabel" runat="server" Text='<%# Bind("vFullCost") %>'></asp:Label><br />
            vMargin:
            <asp:Label ID="vMarginLabel" runat="server" Text='<%# Bind("vMargin") %>'></asp:Label><br />
            vGross:
            <asp:Label ID="vGrossLabel" runat="server" Text='<%# Bind("vGross") %>'></asp:Label><br />
            vNet:
            <asp:Label ID="vNetLabel" runat="server" Text='<%# Bind("vNet") %>'></asp:Label><br />
            vSellPrice:
            <asp:Label ID="vSellPriceLabel" runat="server" Text='<%# Bind("vSellPrice") %>'>
            </asp:Label><br />
            vTotalSheet:
            <asp:Label ID="vTotalSheetLabel" runat="server" Text='<%# Bind("vTotalSheet") %>'>
            </asp:Label><br />
            vQty2:
            <asp:Label ID="vQty2Label" runat="server" Text='<%# Bind("vQty2") %>'></asp:Label><br />
            vPriceBsf:
            <asp:Label ID="vPriceBsfLabel" runat="server" Text='<%# Bind("vPriceBsf") %>'></asp:Label><br />
            vProbeDate:
            <asp:Label ID="vProbeDateLabel" runat="server" Text='<%# Bind("vProbeDate") %>'>
            </asp:Label><br />
            vBoardM:
            <asp:Label ID="vBoardMLabel" runat="server" Text='<%# Bind("vBoardM") %>'></asp:Label><br />
            vBoard:
            <asp:Label ID="vBoardLabel" runat="server" Text='<%# Bind("vBoard") %>'></asp:Label><br />
            vBoardContM:
            <asp:Label ID="vBoardContMLabel" runat="server" Text='<%# Bind("vBoardContM") %>'>
            </asp:Label><br />
            vBoardCont:
            <asp:Label ID="vBoardContLabel" runat="server" Text='<%# Bind("vBoardCont") %>'>
            </asp:Label><br />
            vProbeBy:
            <asp:Label ID="vProbeByLabel" runat="server" Text='<%# Bind("vProbeBy") %>'></asp:Label><br />
            vTotalMsf:
            <asp:Label ID="vTotalMsfLabel" runat="server" Text='<%# Bind("vTotalMsf") %>'></asp:Label><br />
            vTime:--%>
            <asp:Label ID="vFileLabel" Visible="false" runat="server" Text='<%# Bind("vFile") %>'></asp:Label><br />
            <asp:Button ID="Btn_Update" runat="server" Text="WhatIf" CssClass="buttonM" CommandName="Edit" />
            <asp:Button ID="Btn_Delete" runat="server" Text="Delete" CssClass="buttonM" OnClientClick="return confirm('Are you sure you want to delete this record')" OnClick="Btn_Delete_Click" />            
            <input id="ItemButton" runat="server" class="button"  type="button" OnClick="Btn_item_Click()" value="Item" />            
            <asp:Button ID="Btn_view" runat="server" Text="View" CssClass="buttonM" OnClick="Btn_view_Click" />
            <input type="button" value="Hard Copy" id="Btn_HardCopy" class="buttonM" onclick="callHardCopy()" />
            <asp:Button ID="QuoteButton" runat="server" Text="Quote" CssClass="buttonM" OnClick="Quote_Button_Click" />
            <input id="Btn_calc_formview" runat="server" class="button"  type="button" OnClick="Btn_calc_Click()" value="Calculate" />
            <asp:Button ID="PrintBoxButton" runat="server" Text="Print Box" CssClass="buttonM" OnClick="Btn_printbox_Click" />
        </ItemTemplate>
    </asp:FormView>    
    <input id="Btn_calc" runat="server" class="button" OnClick="Btn_calc_Click()" type="button" value="Calculate" />
    <asp:Button ID="PrintBox" runat="server" Text="Print Box" CssClass="buttonM" OnClick="Btn_printbox_Click" />
    <asp:ObjectDataSource ID="Print_ObjectDataSource" runat="server" OldValuesParameterFormatString="original_{0}"
        SelectMethod="SelectCorrPrint" TypeName="Corrugated">
        <SelectParameters>
            <asp:Parameter Name="prmUser" Type="String" />
            <asp:Parameter Name="prmAction" Type="String" />
            <asp:Parameter Name="prmType" Type="String" />
            <asp:Parameter Name="prmComp" Type="String" />
            <asp:SessionParameter Name="prmEstimate" SessionField="order_corrugated_est" Type="String" />
            <asp:Parameter Name="prmEstDate" Type="DateTime" />
            <asp:SessionParameter Name="prmForm" SessionField="order_corrugated_formno" Type="Int32" />
            <asp:Parameter Name="prmFormQty" Type="Int32" />
            <asp:Parameter Name="prmBlk" Type="Int32" />
            <asp:Parameter Name="prmBlkQty" Type="Int32" />
            <asp:Parameter Name="prmCustPart" Type="String" />
            <asp:Parameter Name="prmQty" Type="Int32" />
            <asp:Parameter Name="prmTotalFactCost" Type="Decimal" />
            <asp:Parameter Name="prmFullCost" Type="Decimal" />
            <asp:Parameter Name="prmMargin" Type="Decimal" />
            <asp:Parameter Name="prmGross" Type="Decimal" />
            <asp:Parameter Name="prmNet" Type="Decimal" />
            <asp:Parameter Name="prmSellPrice" Type="Decimal" />
            <asp:Parameter Name="prmTotalSheet" Type="Int32" />
            <asp:Parameter Name="prmQty2" Type="String" />
            <asp:Parameter Name="prmPriceBsf" Type="Decimal" />
            <asp:Parameter Name="prmProbeDate" Type="DateTime" />
            <asp:Parameter Name="prmBoardM" Type="Decimal" />
            <asp:Parameter Name="prmBoard" Type="Decimal" />
            <asp:Parameter Name="prmBoardContM" Type="Decimal" />
            <asp:Parameter Name="prmBoardCont" Type="Decimal" />
            <asp:Parameter Name="prmProbeBy" Type="String" />
            <asp:Parameter Name="prmTotalMsf" Type="Decimal" />
            <asp:Parameter Name="prmTime" Type="String" />
            <asp:Parameter Name="prmLine" Type="Int32" />
             <asp:SessionParameter Name="prmBlank" SessionField="order_corrugated_blankno" Type="int32" />
        </SelectParameters>
    </asp:ObjectDataSource>
    
    <asp:ObjectDataSource ID="Print_ObjectDataSource2" runat="server" OldValuesParameterFormatString="original_{0}"
        SelectMethod="SelectCorrPrint" TypeName="Corrugated">
        <SelectParameters>
            <asp:Parameter Name="prmUser" Type="String" />
            <asp:Parameter Name="prmAction" Type="String" />
            <asp:Parameter Name="prmType" Type="String" />
            <asp:Parameter Name="prmComp" Type="String" />
            <asp:SessionParameter Name="prmEstimate" SessionField="order_corrugated_est" Type="String" />
            <asp:Parameter Name="prmEstDate" Type="DateTime" />
            <asp:SessionParameter Name="prmForm" SessionField="order_corrugated_formno" Type="Int32" />
            <asp:Parameter Name="prmFormQty" Type="Int32" />
            <asp:Parameter Name="prmBlk" Type="Int32" />
            <asp:Parameter Name="prmBlkQty" Type="Int32" />
            <asp:Parameter Name="prmCustPart" Type="String" />
            <asp:Parameter Name="prmQty" Type="Int32" />
            <asp:Parameter Name="prmTotalFactCost" Type="Decimal" />
            <asp:Parameter Name="prmFullCost" Type="Decimal" />
            <asp:Parameter Name="prmMargin" Type="Decimal" />
            <asp:Parameter Name="prmGross" Type="Decimal" />
            <asp:Parameter Name="prmNet" Type="Decimal" />
            <asp:Parameter Name="prmSellPrice" Type="Decimal" />
            <asp:Parameter Name="prmTotalSheet" Type="Int32" />
            <asp:Parameter Name="prmQty2" Type="String" />
            <asp:Parameter Name="prmPriceBsf" Type="Decimal" />
            <asp:Parameter Name="prmProbeDate" Type="DateTime" />
            <asp:Parameter Name="prmBoardM" Type="Decimal" />
            <asp:Parameter Name="prmBoard" Type="Decimal" />
            <asp:Parameter Name="prmBoardContM" Type="Decimal" />
            <asp:Parameter Name="prmBoardCont" Type="Decimal" />
            <asp:Parameter Name="prmProbeBy" Type="String" />
            <asp:Parameter Name="prmTotalMsf" Type="Decimal" />
            <asp:Parameter Name="prmTime" Type="String" />
            <asp:SessionParameter SessionField="corr_print_line" Name="prmLine" Type="Int32" />
             <asp:SessionParameter Name="prmBlank" SessionField="order_corrugated_blankno" Type="int32" />
        </SelectParameters>
    </asp:ObjectDataSource>
    <asp:ObjectDataSource ID="ObjectDataSource1" runat="server" OldValuesParameterFormatString="original_{0}"
        SelectMethod="SelectCorrPrint" TypeName="Corrugated">
        <SelectParameters>
            <asp:Parameter Name="prmUser" Type="String" />
            <asp:Parameter Name="prmAction" Type="String" DefaultValue="PrintTop" />
            <asp:Parameter Name="prmType" Type="String" />
            <asp:Parameter Name="prmComp" Type="String" />
            <asp:SessionParameter Name="prmEstimate" SessionField="order_corrugated_est" Type="String" />
            <asp:Parameter Name="prmEstDate" Type="DateTime" />
            <asp:SessionParameter Name="prmForm" SessionField="order_corrugated_formno" Type="Int32" />
            <asp:Parameter Name="prmFormQty" Type="Int32" />
            <asp:Parameter Name="prmBlk" Type="Int32" />
            <asp:Parameter Name="prmBlkQty" Type="Int32" />
            <asp:Parameter Name="prmCustPart" Type="String" />
            <asp:Parameter Name="prmQty" Type="Int32" />
            <asp:Parameter Name="prmTotalFactCost" Type="Decimal" />
            <asp:Parameter Name="prmFullCost" Type="Decimal" />
            <asp:Parameter Name="prmMargin" Type="Decimal" />
            <asp:Parameter Name="prmGross" Type="Decimal" />
            <asp:Parameter Name="prmNet" Type="Decimal" />
            <asp:Parameter Name="prmSellPrice" Type="Decimal" />
            <asp:Parameter Name="prmTotalSheet" Type="Int32" />
            <asp:Parameter Name="prmQty2" Type="String" />
            <asp:Parameter Name="prmPriceBsf" Type="Decimal" />
            <asp:Parameter Name="prmProbeDate" Type="DateTime" />
            <asp:Parameter Name="prmBoardM" Type="Decimal" />
            <asp:Parameter Name="prmBoard" Type="Decimal" />
            <asp:Parameter Name="prmBoardContM" Type="Decimal" />
            <asp:Parameter Name="prmBoardCont" Type="Decimal" />
            <asp:Parameter Name="prmProbeBy" Type="String" />
            <asp:Parameter Name="prmTotalMsf" Type="Decimal" />
            <asp:Parameter Name="prmTime" Type="String" />
            <asp:Parameter Name="prmLine" Type="Int32" />
            <asp:SessionParameter Name="prmBlank" SessionField="order_corrugated_blankno" Type="int32" />
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
            <asp:SessionParameter Name="prmAction" SessionField="order_corrugated_est" Type="String" />
            <asp:Parameter Name="prmOut" Type="String" />
            <asp:Parameter Name="vFromDept" Type="String" />
            <asp:Parameter Name="vToDept" Type="String" />
            <asp:Parameter Name="vTbPrtBox" Type="String" />
            <asp:Parameter Name="vTbPrtNote" Type="String" />
            <asp:Parameter Name="prmEstimate" Type="String" />
            <asp:SessionParameter Name="prmFormNo" SessionField="order_corrugated_formno"  Type="Int32" />
            <asp:SessionParameter Name="prmBlankNo" Type="Int32" />
            <asp:SessionParameter  Name="prmLine" SessionField="corr_print_line"  Type="Int32" />
        </SelectParameters>
    </asp:ObjectDataSource>
    
    <asp:FormView ID="FormView_PrintBox" Visible="false" runat="server" DataSourceID="ObjectDataSource3">
              
        <ItemTemplate>
           
            <asp:Label ID="vCorrEstimateBoxFileLabel" runat="server" Text='<%# Bind("vCorrEstimateBoxFile") %>' />
            
        </ItemTemplate>
    </asp:FormView>
    <asp:ObjectDataSource ID="ObjectDataSource3" runat="server" 
        OldValuesParameterFormatString="original_{0}" 
        SelectMethod="CorrugatedEstimatePrintBox" TypeName="Corrugated">
        <SelectParameters>
            <asp:Parameter Name="prmUser" Type="String" />
            <asp:SessionParameter Name="prmAction" SessionField="order_corrugated_est" Type="String" />
            <asp:Parameter Name="prmOut" Type="String" />          
            <asp:Parameter Name="prmEstimate" Type="String" />
            <asp:SessionParameter Name="prmFormNo" SessionField="order_corrugated_formno"  Type="Int32" />                    
            <asp:SessionParameter Name="prmBlankNo" SessionField="order_corrugated_blankno" Type="int32" />
            <asp:SessionParameter  Name="prmLine" SessionField="corr_print_line"  Type="Int32" />
        </SelectParameters>
    </asp:ObjectDataSource>
</asp:Content>

