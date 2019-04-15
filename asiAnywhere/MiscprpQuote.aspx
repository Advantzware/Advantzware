<%@ Page Language="C#" MasterPageFile="~/MasterPage6.master" Debug="true" AutoEventWireup="true" Inherits="MiscprpQuote" Title="Prep/Misc Chg" Codebehind="MiscprpQuote.aspx.cs" %>
<asp:Content ID="Content1" ContentPlaceHolderID="ContentPlaceHolder1" Runat="Server">
<script src="include/CalendarControl.js"></script>
<script language="javascript" src="include/date.js"></script>
<script language="javascript" src="include/event.js"></script>
<script language="javascript" src="include/insert.js"></script>
<script language="javascript" src="include/validate2.js"></script>


<script>

function calcamt()
{
var amt = document.getElementById("ctl00_ContentPlaceHolder1_FormView2_qtqty_amtTextBox");
var prpqty = document.getElementById("ctl00_ContentPlaceHolder1_FormView2_qtqty_prepqtyTextBox");
var cost = document.getElementById("ctl00_ContentPlaceHolder1_FormView2_qtqty_costTextBox");
var mkup = document.getElementById("ctl00_ContentPlaceHolder1_FormView2_qtqty_mkupTextBox");
var amtz = document.getElementById("ctl00_ContentPlaceHolder1_FormView2_qtqty_amtzTextBox");

if (amtz.value == "")
    var varamtz = 1;
else
    var varamtz = amtz.value;

var amount = parseFloat((prpqty.value) * (cost.value) / (1 - ((mkup.value) / 100)) * ((varamtz) / 100));

amt.value = amount.toFixed(2);

}

function preplook() {
    var NewWindow = window.open("prep_lookup.aspx", "CarrierLookup", "width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}
function PrepLookup(ReturnObj1, ReturnObj2, ReturnObj3, ReturnObj4, ReturnObj5, ReturnObj6, ReturnObj7) {
    //document.forms[0].ctl00_ContentPlaceHolder1_FormView2_vCodeTextBox.value = ReturnObj1;
    document.forms[0].ctl00_ContentPlaceHolder1_FormView2_qtqty_chargeTextBox.value = ReturnObj2;
    document.getElementById("ctl00_ContentPlaceHolder1_FormView2_qtqty_prepqtyTextBox").select();
}
function getdecimal(obj, obj2) {
    if (obj.value.indexOf(".") != -1) {
        return;
    }
    else if (obj.value.length == obj2) {        
        obj.value = obj.value + ".";
    }

}

   
</script>

<div>
<asp:HyperLink ID="Hyperlink1" runat="server" NavigateUrl="list_rfqs.aspx">Back to list</asp:HyperLink>
    
    <asp:FormView ID="FormView1" runat="server" DataSourceID="ObjectDataSource1">
        
        
     <ItemTemplate>
            <fieldset><legend>Reference Information</legend>
            <table width="715px"><tr><td><b>Quote#</b>  &nbsp;&nbsp;
            <asp:Label ID="qtqty_quoteLabel" runat="server" Width="100px" BackColor="Turquoise"  Text='<%# Bind("[qtqty-quote]") %>' />
            </td>
            <td> <b>Cust Part#</b> &nbsp;&nbsp;
            <asp:Label ID="qtqty_partnoLabel" Width="100px" BackColor="Turquoise"  runat="server" Text='<%# Bind("[qtqty-partno]") %>' />
           
            &nbsp;&nbsp;&nbsp;&nbsp;
            <asp:Label ID="qtqty_dscrLabel"  BackColor="Turquoise"  runat="server"  Text='<%# Bind("[qtqty-dscr]") %>' />
           </td>
           <td><b>Qty</b>&nbsp;&nbsp;
            <asp:Label ID="qtqty_qtyLabel" Width="100px" BackColor="Turquoise"  runat="server" Text='<%# Bind("[qtqty-qty]") %>' />
            </td>
          </tr></table> </fieldset>
        </ItemTemplate>
    </asp:FormView>
    <asp:ObjectDataSource ID="ObjectDataSource1" runat="server" 
        OldValuesParameterFormatString="original_{0}" SelectMethod="SelectQuoteCharge" 
        TypeName="QuoteDetail">
        <SelectParameters>
            <asp:Parameter Name="prmUser" Type="String" />
            <asp:Parameter DefaultValue="TopInfo" Name="prmAction" Type="String" />
            <asp:SessionParameter DefaultValue="" Name="prmQuote" SessionField="quote_no" 
                Type="Int32" />
            <asp:SessionParameter Name="prmQty" SessionField="MiscprpQuote_main_qty" Type="Int32" />
            <asp:SessionParameter Name="prmLine" DefaultValue="0" SessionField="viewquote_qty_lineno" Type="Int32" />
            <asp:Parameter Name="prmSnum" Type="Int32" />
            <asp:Parameter Name="prmBnum" Type="Int32" />
            <asp:Parameter Name="prmBill" Type="String" />
            <asp:Parameter Name="prmCharge" Type="String" />
            <asp:Parameter Name="prmPrepQty" Type="Decimal" />
            <asp:Parameter Name="prmCost" Type="Decimal" />
            <asp:Parameter Name="prmMkup" Type="Decimal" />
            <asp:Parameter Name="prmAmtz" Type="Decimal" />
            <asp:Parameter Name="prmAmt" Type="Decimal" />
            <asp:Parameter Name="prmMatF" Type="Decimal" />
            <asp:Parameter Name="prmMatM" Type="Decimal" />
            <asp:Parameter Name="prmLabF" Type="Decimal" />
            <asp:Parameter Name="prmLabM" Type="Decimal" />
            <asp:Parameter Name="prmSimon" Type="String" />
            <asp:Parameter Name="prmReckey" Type="String" />
        </SelectParameters>
    </asp:ObjectDataSource>

      <asp:GridView ID="GridView1" runat="server" AllowPaging="True"
        AllowSorting="True" OnSelectedIndexChanged="GridView1_SelectedIndex" DataKeyNames="qtqty-reckey"
        AutoGenerateColumns="False" CssClass="Grid" DataSourceID="ObjectDataSource2"
        EmptyDataText="No Record Found"  Width="730px">
        <EmptyDataRowStyle BorderColor="Gray" BorderStyle="Dotted" BorderWidth="0px" Font-Bold="True"
            HorizontalAlign="Center" VerticalAlign="Middle" />
        <RowStyle CssClass="shade" />
        <Columns>
            <asp:CommandField ButtonType="Image" SelectImageUrl="~/Images/sel.gif" SelectText="" ShowSelectButton="True">
                <ItemStyle Width="10px" />
            </asp:CommandField>
            
            <asp:BoundField DataField="qtqty-snum" HeaderText="S" SortExpression="qtqty-snum" />
            <asp:BoundField DataField="qtqty-bnum" HeaderText="B" SortExpression="qtqty-bnum" />
            <asp:BoundField DataField="qtqty-bill" HeaderText="Bill" SortExpression="qtqty-bill" />
            <asp:BoundField DataField="qtqty-charge" HeaderText="Charge" SortExpression="qtqty-charge" />
            <asp:BoundField DataField="qtqty-prepqty" HeaderText="Qty" SortExpression="qtqty-prepqty" />
            <asp:BoundField DataField="qtqty-cost" HeaderText="Cost" SortExpression="qtqty-cost" />
            <asp:BoundField DataField="qtqty-mkup" HeaderText="Mkup" SortExpression="qtqty-mkup" />
            <asp:BoundField DataField="qtqty-amtz" HeaderText="Amtz" SortExpression="qtqty-amtz" />
            <asp:BoundField DataField="qtqty-amt" HeaderText="Amount" SortExpression="qtqty-amt" />
            <asp:BoundField DataField="qtqty-matf" HeaderText="Mat'l SU" SortExpression="qtqty-matf" />
            <asp:BoundField DataField="qtqty-matm" HeaderText="Mat'l/M" SortExpression="qtqty-matm" />
            <asp:BoundField DataField="qtqty-labf" HeaderText="Labor SU" SortExpression="qtqty-labf" />
            <asp:BoundField DataField="qtqty-labm" HeaderText="Labor/M" SortExpression="qtqty-labm" />
            <asp:BoundField DataField="qtqty-simon" HeaderText="SIMON" SortExpression="qtqty-simon" />
            <asp:BoundField Visible="false" DataField="qtqty-reckey" HeaderText="qtqty-reckey"  SortExpression="qtqty-reckey" />
            
        </Columns>
        <SelectedRowStyle CssClass="GridSelected" BackColor="Yellow" />
        <HeaderStyle BackColor="Teal" CssClass="gridrowhdr" ForeColor="White" HorizontalAlign="Center"
            VerticalAlign="Middle" Wrap="False" />
        <AlternatingRowStyle CssClass="GridItemOdd" />
    </asp:GridView>
    
    <br />
    <asp:Button ID="AddNewButton" runat="server" Text="Add" CssClass="button" OnClick="AddNewButton_Click" />
    <asp:FormView ID="FormView2" runat="server" OnDataBound="Formview2_onbatabound"    DataSourceID="ObjectDataSource_item">
        <EditItemTemplate>
            <asp:Panel ID="editpanel" runat="server" DefaultButton="UpdateButton">
            <table class="shade">
            <tr><td><b>S:</b></td><td><b>B:</b></td><td><b>Bill:</b></td><td><b>Charge:</b></td><td><b>Qty:</b></td>
            <td><b>Cost:</b></td><td><b>Mkup:</b></td><td><b>Amtz:</b></td><td><b>Amunt:</b></td><td><b>Mat'l SU:</b></td>
            <td><b>Mat'l/M:</b></td><td><b>Labor SU:</b></td><td><b>Labor/M:</b></td><td><b>SIMON:</b></td></tr>
            <tr><td nowrap>
            <asp:TextBox ID="qtqty_snumTextBox" Width="30px" MaxLength="2" runat="server" Text='<%# Bind("[qtqty-snum]") %>' />
                <asp:CompareValidator ID="CompareValidator1" runat="server" ControlToValidate="qtqty_snumTextBox" SetFocusOnError="true" Operator="DataTypeCheck" Type="Integer" Display="Dynamic" ErrorMessage="Invalid Entry"></asp:CompareValidator>
            </td><td nowrap>
            <asp:TextBox ID="qtqty_bnumTextBox" Width="30px" MaxLength="2" runat="server" Text='<%# Bind("[qtqty-bnum]") %>' />
            <asp:CompareValidator ID="CompareValidator2" runat="server" ControlToValidate="qtqty_bnumTextBox" SetFocusOnError="true" Operator="DataTypeCheck" Type="Integer" Display="Dynamic" ErrorMessage="Invalid Entry"></asp:CompareValidator>
            </td><td nowrap>
            <asp:TextBox ID="qtqty_billTextBox" Visible="false" Width="80px" runat="server" Text='<%# Bind("[qtqty-bill]") %>' />
             <asp:DropDownList ID="DropDownList2"  runat="server">
               <asp:ListItem Value="L" Text="Labor"></asp:ListItem>
                <asp:ListItem Value="M" Text="Materials"></asp:ListItem>
                 <asp:ListItem Value="N" Text="No Charge"></asp:ListItem>
                  <asp:ListItem Value="T" Text="Time"></asp:ListItem>
                   <asp:ListItem Value="W" Text="Will Advise"></asp:ListItem>
               </asp:DropDownList>
            </td><td nowrap>
            <asp:TextBox ID="qtqty_chargeTextBox" Width="80px" runat="server" Text='<%# Bind("[qtqty-charge]") %>' />
            <a href="#" onclick="preplook(); return false"><asp:Image ID="Image2" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
            </td><td nowrap>
            <asp:TextBox ID="qtqty_prepqtyTextBox" onkeyup="calcamt()"  Width="80px" runat="server" Text='<%# Bind("[qtqty-prepqty]") %>' />
            <asp:CompareValidator ID="CompareValidator3" runat="server" ControlToValidate="qtqty_prepqtyTextBox" SetFocusOnError="true" Operator="DataTypeCheck" Type="Double" Display="Dynamic" ErrorMessage="Invalid Entry"></asp:CompareValidator>
            </td><td nowrap>
            <asp:TextBox ID="qtqty_costTextBox" onkeyup="getdecimal(this,5);extractNumber(this,2,true);calcamt();"   onkeypress="return blockNonNumbers(this, event, true, true);"  Width="80px" runat="server" Text='<%# Bind("[qtqty-cost]") %>' />
            <asp:CompareValidator ID="CompareValidator4" runat="server" ControlToValidate="qtqty_costTextBox" SetFocusOnError="true" Operator="DataTypeCheck" Type="Double" Display="Dynamic" ErrorMessage="Invalid Entry"></asp:CompareValidator>
           </td><td nowrap>
            <asp:TextBox ID="qtqty_mkupTextBox"  onkeyup="getdecimal(this,3);extractNumber(this,2,true);calcamt();"   onkeypress="return blockNonNumbers(this, event, true, true);" Width="40px" MaxLength="6" runat="server" Text='<%# Bind("[qtqty-mkup]") %>' />
            <asp:CompareValidator ID="CompareValidator5" runat="server" ControlToValidate="qtqty_mkupTextBox" SetFocusOnError="true" Operator="DataTypeCheck" Type="Double" Display="Dynamic" ErrorMessage="Invalid Entry"></asp:CompareValidator>
            </td><td nowrap>
            <asp:TextBox ID="qtqty_amtzTextBox"  onkeyup="getdecimal(this,3);extractNumber(this,2,true);calcamt();"   onkeypress="return blockNonNumbers(this, event, true, true);" Width="40px" MaxLength="6" runat="server" Text='<%# Bind("[qtqty-amtz]") %>' />
            <asp:CompareValidator ID="CompareValidator6" runat="server" ControlToValidate="qtqty_amtzTextBox" SetFocusOnError="true" Operator="DataTypeCheck" Type="Double" Display="Dynamic" ErrorMessage="Invalid Entry"></asp:CompareValidator>
            </td><td nowrap>
            <asp:TextBox ID="qtqty_amtTextBox" Width="60px"   MaxLength="8" runat="server" Text='<%# Bind("[qtqty-amt]") %>' />
            <asp:CompareValidator ID="CompareValidator7" runat="server" ControlToValidate="qtqty_amtTextBox" SetFocusOnError="true" Operator="DataTypeCheck" Type="Double" Display="Dynamic" ErrorMessage="Invalid Entry"></asp:CompareValidator>
            </td><td nowrap>
            <asp:TextBox ID="qtqty_matfTextBox" Width="60px" onkeyup="getdecimal(this,5);extractNumber(this,2,true);"   onkeypress="return blockNonNumbers(this, event, true, true);" MaxLength="8" runat="server" Text='<%# Bind("[qtqty-matf]") %>' />
            <asp:CompareValidator ID="CompareValidator8" runat="server" ControlToValidate="qtqty_matfTextBox" SetFocusOnError="true" Operator="DataTypeCheck" Type="Double" Display="Dynamic" ErrorMessage="Invalid Entry"></asp:CompareValidator>
            </td><td nowrap>
            <asp:TextBox ID="qtqty_matmTextBox" Width="60px" onkeyup="getdecimal(this,5);extractNumber(this,2,true);"   onkeypress="return blockNonNumbers(this, event, true, true);" MaxLength="8" runat="server" Text='<%# Bind("[qtqty-matm]") %>' />
            <asp:CompareValidator ID="CompareValidator9" runat="server" ControlToValidate="qtqty_matmTextBox" SetFocusOnError="true" Operator="DataTypeCheck" Type="Double" Display="Dynamic" ErrorMessage="Invalid Entry"></asp:CompareValidator>
            </td><td nowrap>
            <asp:TextBox ID="qtqty_labfTextBox" Width="60px" MaxLength="8" onkeyup="getdecimal(this,5);extractNumber(this,2,true);"   onkeypress="return blockNonNumbers(this, event, true, true);" runat="server" Text='<%# Bind("[qtqty-labf]") %>' />
            <asp:CompareValidator ID="CompareValidator10" runat="server" ControlToValidate="qtqty_labfTextBox" SetFocusOnError="true" Operator="DataTypeCheck" Type="Double" Display="Dynamic" ErrorMessage="Invalid Entry"></asp:CompareValidator>
           </td><td nowrap>
            <asp:TextBox ID="qtqty_labmTextBox" Width="60px" MaxLength="8" onkeyup="getdecimal(this,5);extractNumber(this,2,true);"   onkeypress="return blockNonNumbers(this, event, true, true);" runat="server" Text='<%# Bind("[qtqty-labm]") %>' />
            <asp:CompareValidator ID="CompareValidator11" runat="server" ControlToValidate="qtqty_labmTextBox" SetFocusOnError="true" Operator="DataTypeCheck" Type="Double" Display="Dynamic" ErrorMessage="Invalid Entry"></asp:CompareValidator>
           </td><td nowrap>
            <asp:TextBox ID="qtqty_simonTextBox" Visible="false" Width="70px" runat="server" Text='<%# Bind("[qtqty-simon]") %>' />
               <asp:DropDownList ID="DropDownList1"  runat="server" onblur="document.getElementById('ctl00_ContentPlaceHolder1_FormView2_qtqty_snumTextBox').focus();">
               <asp:ListItem Value="S" Text="Separate"></asp:ListItem>
                <asp:ListItem Value="I" Text="Integrate"></asp:ListItem>
                 <asp:ListItem Value="M" Text="Maintenance"></asp:ListItem>
                  <asp:ListItem Value="O" Text="Other"></asp:ListItem>
                   <asp:ListItem Value="N" Text="No charge"></asp:ListItem>
               </asp:DropDownList>
            </td><td style="display:none">
            <asp:TextBox ID="qtqty_reckeyTextBox" runat="server" Text='<%# Bind("[qtqty-reckey]") %>' />
            </td>
            </tr> <tr><td colspan="8">
           <asp:Button ID="UpdateButton" runat="server" CausesValidation="True" CssClass="button" OnClick="Formview2_update_button_click" Text="Save" />
            &nbsp;<asp:Button ID="UpdateCancelButton" runat="server" CssClass="button" CausesValidation="False" CommandName="Cancel" Text="Cancel" />
            </td></tr>  </table></asp:Panel>
        </EditItemTemplate>
        <InsertItemTemplate>
           <asp:Panel ID="insertpanel" runat="server" DefaultButton="InsertButton">
            <table class="shade">
            <tr><td><b>S:</b></td><td><b>B:</b></td><td><b>Bill:</b></td><td><b>Charge:</b></td><td><b>Qty:</b></td>
            <td><b>Cost:</b></td><td><b>Mkup:</b></td><td><b>Amtz:</b></td><td><b>Amunt:</b></td><td><b>Mat'l SU:</b></td>
            <td><b>Mat'l/M:</b></td><td><b>Labor SU:</b></td><td><b>Labor/M:</b></td><td><b>SIMON:</b></td></tr>
            <tr><td nowrap>
            <asp:TextBox ID="qtqty_snumTextBox" Width="30px" MaxLength="2" runat="server" Text='<%# Bind("[qtqty-snum]") %>' />
                <asp:CompareValidator ID="CompareValidator1" runat="server" ControlToValidate="qtqty_snumTextBox" SetFocusOnError="true" Operator="DataTypeCheck" Type="Integer" Display="Dynamic" ErrorMessage="Invalid Entry"></asp:CompareValidator>
            </td><td nowrap>
            <asp:TextBox ID="qtqty_bnumTextBox" Width="30px" MaxLength="2" runat="server" Text='<%# Bind("[qtqty-bnum]") %>' />
            <asp:CompareValidator ID="CompareValidator2" runat="server" ControlToValidate="qtqty_bnumTextBox" SetFocusOnError="true" Operator="DataTypeCheck" Type="Integer" Display="Dynamic" ErrorMessage="Invalid Entry"></asp:CompareValidator>
            </td><td nowrap>
            <asp:TextBox ID="qtqty_billTextBox" Visible="false" Width="80px" runat="server" Text='<%# Bind("[qtqty-bill]") %>' />
             <asp:DropDownList ID="DropDownList2"  runat="server">
               <asp:ListItem Value="L" Text="Labor"></asp:ListItem>
                <asp:ListItem Value="M" Text="Materials"></asp:ListItem>
                 <asp:ListItem Value="N" Text="No Charge"></asp:ListItem>
                  <asp:ListItem Value="T" Text="Time"></asp:ListItem>
                   <asp:ListItem Value="W" Text="Will Advise"></asp:ListItem>
               </asp:DropDownList>
            </td><td nowrap>
            <asp:TextBox ID="qtqty_chargeTextBox" Width="80px" runat="server" Text='<%# Bind("[qtqty-charge]") %>' />
            <a href="#" onclick="preplook(); return false"><asp:Image ID="Image2" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
            </td><td nowrap>
            <asp:TextBox ID="qtqty_prepqtyTextBox" onkeyup="calcamt()" Width="80px" runat="server" Text='<%# Bind("[qtqty-prepqty]") %>' />
            <asp:CompareValidator ID="CompareValidator3" runat="server" ControlToValidate="qtqty_prepqtyTextBox" SetFocusOnError="true" Operator="DataTypeCheck" Type="Double" Display="Dynamic" ErrorMessage="Invalid Entry"></asp:CompareValidator>
            </td><td nowrap>
            <asp:TextBox ID="qtqty_costTextBox" Width="80px" onkeyup="getdecimal(this,5);extractNumber(this,2,true);calcamt();"   onkeypress="return blockNonNumbers(this, event, true, true);" runat="server" Text='<%# Bind("[qtqty-cost]") %>' />
            <asp:CompareValidator ID="CompareValidator4" runat="server" ControlToValidate="qtqty_costTextBox" SetFocusOnError="true" Operator="DataTypeCheck" Type="Double" Display="Dynamic" ErrorMessage="Invalid Entry"></asp:CompareValidator>
           </td><td nowrap>
            <asp:TextBox ID="qtqty_mkupTextBox" Width="40px" onkeyup="getdecimal(this,3);extractNumber(this,2,true);calcamt();"   onkeypress="return blockNonNumbers(this, event, true, true);" MaxLength="6" runat="server" Text='<%# Bind("[qtqty-mkup]") %>' />
            <asp:CompareValidator ID="CompareValidator5" runat="server" ControlToValidate="qtqty_mkupTextBox" SetFocusOnError="true" Operator="DataTypeCheck" Type="Double" Display="Dynamic" ErrorMessage="Invalid Entry"></asp:CompareValidator>
            </td><td nowrap>
            <asp:TextBox ID="qtqty_amtzTextBox" Width="40px" onkeyup="getdecimal(this,3);extractNumber(this,2,true);calcamt();"   onkeypress="return blockNonNumbers(this, event, true, true);" MaxLength="6" runat="server" Text='<%# Bind("[qtqty-amtz]") %>' />
            <asp:CompareValidator ID="CompareValidator6" runat="server" ControlToValidate="qtqty_amtzTextBox" SetFocusOnError="true" Operator="DataTypeCheck" Type="Double" Display="Dynamic" ErrorMessage="Invalid Entry"></asp:CompareValidator>
            </td><td nowrap>
            <asp:TextBox ID="qtqty_amtTextBox" Width="60px" MaxLength="8"   runat="server" Text='<%# Bind("[qtqty-amt]") %>' />
            <asp:CompareValidator ID="CompareValidator7" runat="server" ControlToValidate="qtqty_amtTextBox" SetFocusOnError="true" Operator="DataTypeCheck" Type="Double" Display="Dynamic" ErrorMessage="Invalid Entry"></asp:CompareValidator>
            </td><td nowrap>
            <asp:TextBox ID="qtqty_matfTextBox" Width="60px" onkeyup="getdecimal(this,5);extractNumber(this,2,true);"   onkeypress="return blockNonNumbers(this, event, true, true);" MaxLength="8" runat="server" Text='<%# Bind("[qtqty-matf]") %>' />
            <asp:CompareValidator ID="CompareValidator8" runat="server" ControlToValidate="qtqty_matfTextBox" SetFocusOnError="true" Operator="DataTypeCheck" Type="Double" Display="Dynamic" ErrorMessage="Invalid Entry"></asp:CompareValidator>
            </td><td nowrap>
            <asp:TextBox ID="qtqty_matmTextBox" Width="60px" MaxLength="8" onkeyup="getdecimal(this,5);extractNumber(this,2,true);"   onkeypress="return blockNonNumbers(this, event, true, true);" runat="server" Text='<%# Bind("[qtqty-matm]") %>' />
            <asp:CompareValidator ID="CompareValidator9" runat="server" ControlToValidate="qtqty_matmTextBox" SetFocusOnError="true" Operator="DataTypeCheck" Type="Double" Display="Dynamic" ErrorMessage="Invalid Entry"></asp:CompareValidator>
            </td><td nowrap>
            <asp:TextBox ID="qtqty_labfTextBox" Width="60px" MaxLength="8" onkeyup="getdecimal(this,5);extractNumber(this,2,true);"   onkeypress="return blockNonNumbers(this, event, true, true);" runat="server" Text='<%# Bind("[qtqty-labf]") %>' />
            <asp:CompareValidator ID="CompareValidator10" runat="server" ControlToValidate="qtqty_labfTextBox" SetFocusOnError="true" Operator="DataTypeCheck" Type="Double" Display="Dynamic" ErrorMessage="Invalid Entry"></asp:CompareValidator>
           </td><td nowrap>
            <asp:TextBox ID="qtqty_labmTextBox" Width="60px" MaxLength="8" onkeyup="getdecimal(this,5);extractNumber(this,2,true);"   onkeypress="return blockNonNumbers(this, event, true, true);" runat="server" Text='<%# Bind("[qtqty-labm]") %>' />
            <asp:CompareValidator ID="CompareValidator11" runat="server" ControlToValidate="qtqty_labmTextBox" SetFocusOnError="true" Operator="DataTypeCheck" Type="Double" Display="Dynamic" ErrorMessage="Invalid Entry"></asp:CompareValidator>
           </td><td nowrap>
            <asp:TextBox ID="qtqty_simonTextBox" Visible="false" Width="70px" runat="server" Text='<%# Bind("[qtqty-simon]") %>' />
               <asp:DropDownList ID="DropDownList1"  runat="server" onblur="document.getElementById('ctl00_ContentPlaceHolder1_FormView2_qtqty_snumTextBox').focus();">
               <asp:ListItem Value="S" Text="Separate"></asp:ListItem>
                <asp:ListItem Value="I" Text="Integrate"></asp:ListItem>
                 <asp:ListItem Value="M" Text="Maintenance"></asp:ListItem>
                  <asp:ListItem Value="O" Text="Other"></asp:ListItem>
                   <asp:ListItem Value="N" Text="No charge"></asp:ListItem>
               </asp:DropDownList>
            </td><td style="display:none">
            <asp:TextBox ID="qtqty_reckeyTextBox" runat="server" Text='<%# Bind("[qtqty-reckey]") %>' />
            </td>
            </tr> <tr><td colspan="8">
            
           <asp:Button ID="InsertButton" runat="server" CausesValidation="True" OnClick="Formview2_insert_button_Click" CssClass="button" Text="Save" />
            &nbsp;<asp:Button ID="InsertCancelButton" runat="server" CssClass="button" CausesValidation="False" CommandName="Cancel" Text="Cancel" />
            </td></tr></table></asp:Panel>
        </InsertItemTemplate>
        <ItemTemplate>
            <table><tr><td style="display:none">
            qtqty-snum:
            <asp:Label ID="qtqty_snumLabel" runat="server" Text='<%# Bind("[qtqty-snum]") %>' />
            <br />
            qtqty-bnum:
            <asp:Label ID="qtqty_bnumLabel" runat="server" Text='<%# Bind("[qtqty-bnum]") %>' />
            <br />
            qtqty-bill:
            <asp:Label ID="qtqty_billLabel" runat="server" Text='<%# Bind("[qtqty-bill]") %>' />
            <br />
            qtqty-charge:
            <asp:Label ID="qtqty_chargeLabel" runat="server" Text='<%# Bind("[qtqty-charge]") %>' />
            <br />
            qtqty-prepqty:
            <asp:Label ID="qtqty_prepqtyLabel" runat="server" Text='<%# Bind("[qtqty-prepqty]") %>' />
            <br />
            qtqty-cost:
            <asp:Label ID="qtqty_costLabel" runat="server" Text='<%# Bind("[qtqty-cost]") %>' />
            <br />
            qtqty-mkup:
            <asp:Label ID="qtqty_mkupLabel" runat="server" Text='<%# Bind("[qtqty-mkup]") %>' />
            <br />
            qtqty-amtz:
            <asp:Label ID="qtqty_amtzLabel" runat="server" Text='<%# Bind("[qtqty-amtz]") %>' />
            <br />
            qtqty-amt:
            <asp:Label ID="qtqty_amtLabel" runat="server" Text='<%# Bind("[qtqty-amt]") %>' />
            <br />
            qtqty-matf:
            <asp:Label ID="qtqty_matfLabel" runat="server" Text='<%# Bind("[qtqty-matf]") %>' />
            <br />
            qtqty-matm:
            <asp:Label ID="qtqty_matmLabel" runat="server" Text='<%# Bind("[qtqty-matm]") %>' />
            <br />
            qtqty-labf:
            <asp:Label ID="qtqty_labfLabel" runat="server" Text='<%# Bind("[qtqty-labf]") %>' />
            <br />
            qtqty-labm:
            <asp:Label ID="qtqty_labmLabel" runat="server" Text='<%# Bind("[qtqty-labm]") %>' />
            <br />
            qtqty-simon:
            <asp:Label ID="qtqty_simonLabel" runat="server" Text='<%# Bind("[qtqty-simon]") %>' />
            <br />
            qtqty-reckey:
            <asp:Label ID="qtqty_reckeyLabel" runat="server" Text='<%# Bind("[qtqty-reckey]") %>' />
            <br />
            </td></tr><tr><td>
             <asp:Button ID="AddButton" runat="server"  CausesValidation="False" CssClass="buttonM" CommandName="new"
                 Text="Add">
                </asp:Button> 
            <asp:Button ID="UpdateButton" runat="server"  CssClass="button" CausesValidation="False" CommandName="edit"
                Text="Update">
            </asp:Button>
            <asp:Button ID="DeleteButton" runat="server"  CssClass="button" CausesValidation="False" OnClick="Formview2_deletebutton_Click"  OnClientClick="return confirm('Delete Currently Selected Record?')"     Text="Delete">
            </asp:Button>
            </td></tr></table>
        </ItemTemplate>
    </asp:FormView>
    
    <asp:ObjectDataSource ID="ObjectDataSource_item" runat="server" 
        OldValuesParameterFormatString="original_{0}" SelectMethod="SelectQuoteCharge" 
        TypeName="QuoteDetail">
        <SelectParameters>
            <asp:Parameter Name="prmUser" Type="String" />
            <asp:Parameter DefaultValue="View" Name="prmAction" Type="String" />
            <asp:SessionParameter DefaultValue="" Name="prmQuote" SessionField="quote_no" Type="Int32" />
            <asp:SessionParameter Name="prmQty" SessionField="MiscprpQuote_main_qty" Type="Int32" />
            <asp:SessionParameter Name="prmLine" DefaultValue="0" SessionField="viewquote_qty_lineno" Type="Int32" />
            <asp:Parameter Name="prmSnum" Type="Int32" />
            <asp:Parameter Name="prmBnum" Type="Int32" />
            <asp:Parameter Name="prmBill" Type="String" />
            <asp:Parameter Name="prmCharge" Type="String" />
            <asp:Parameter Name="prmPrepQty" Type="Decimal" />
            <asp:Parameter Name="prmCost" Type="Decimal" />
            <asp:Parameter Name="prmMkup" Type="Decimal" />
            <asp:Parameter Name="prmAmtz" Type="Decimal" />
            <asp:Parameter Name="prmAmt" Type="Decimal" />
            <asp:Parameter Name="prmMatF" Type="Decimal" />
            <asp:Parameter Name="prmMatM" Type="Decimal" />
            <asp:Parameter Name="prmLabF" Type="Decimal" />
            <asp:Parameter Name="prmLabM" Type="Decimal" />
            <asp:Parameter Name="prmSimon" Type="String" />
          <asp:ControlParameter ControlID="GridView1" Name="prmReckey" 
                PropertyName="SelectedValue" Type="String" />
        </SelectParameters>
    </asp:ObjectDataSource>
    
    <asp:ObjectDataSource ID="ObjectDataSource2" runat="server" 
        OldValuesParameterFormatString="original_{0}" SelectMethod="SelectQuoteCharge" 
        TypeName="QuoteDetail">
        <SelectParameters>
            <asp:Parameter Name="prmUser" Type="String" />
            <asp:Parameter Name="prmAction" DefaultValue="Select" Type="String" />
            <asp:SessionParameter DefaultValue="" Name="prmQuote" SessionField="quote_no" Type="Int32" />
            <asp:SessionParameter Name="prmQty" SessionField="MiscprpQuote_main_qty" Type="Int32" />
            <asp:SessionParameter Name="prmLine" DefaultValue="0" SessionField="viewquote_qty_lineno" Type="Int32" />
            <asp:Parameter Name="prmSnum" Type="Int32" />
            <asp:Parameter Name="prmBnum" Type="Int32" />
            <asp:Parameter Name="prmBill" Type="String" />
            <asp:Parameter Name="prmCharge" Type="String" />
            <asp:Parameter Name="prmPrepQty" Type="Decimal" />
            <asp:Parameter Name="prmCost" Type="Decimal" />
            <asp:Parameter Name="prmMkup" Type="Decimal" />
            <asp:Parameter Name="prmAmtz" Type="Decimal" />
              <asp:Parameter Name="prmAmt" Type="Decimal" />
            <asp:Parameter Name="prmMatF" Type="Decimal" />
            <asp:Parameter Name="prmMatM" Type="Decimal" />
            <asp:Parameter Name="prmLabF" Type="Decimal" />
            <asp:Parameter Name="prmLabM" Type="Decimal" />
            <asp:Parameter Name="prmSimon" Type="String" />
            <asp:Parameter Name="prmReckey" Type="String" />
        </SelectParameters>
    </asp:ObjectDataSource>
     
    
     
</div>


</asp:Content>

