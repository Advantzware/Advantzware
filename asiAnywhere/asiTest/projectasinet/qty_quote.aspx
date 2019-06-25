<%@ Page Language="C#" MasterPageFile="~/MasterPage6.master" Debug="true" AutoEventWireup="true" Inherits="qty_quote" Title="Quantities Quote" Codebehind="qty_quote.aspx.cs" %>
<asp:Content ID="Content1" ContentPlaceHolderID="ContentPlaceHolder1" Runat="Server">
<script type="text/javascript" src="include/CalendarControl.js"></script>
<script type="text/javascript" language="javascript" src="include/date.js"></script>
<script type="text/javascript" language="javascript" src="include/event.js"></script>
<script type="text/javascript" language="javascript" src="include/insert.js"></script>


<script language="javascript" type="text/javascript">
    function checkReprice() {
        if (confirm('Are you sure you wish to Reprice this quote?')) {
            document.getElementById("ctl00_ContentPlaceHolder1_FormView2_RadioButtonList1").style.display = "block";
            document.getElementById("ctl00_ContentPlaceHolder1_FormView2_AddButton").style.display = "none";
            document.getElementById("ctl00_ContentPlaceHolder1_FormView2_UpdateButton").style.display = "none";
            document.getElementById("ctl00_ContentPlaceHolder1_FormView2_DeleteButton").style.display = "none";
            document.getElementById("ctl00_ContentPlaceHolder1_FormView2_RepriceButton").style.display = "none";
            document.getElementById("ctl00_ContentPlaceHolder1_FormView2_CopyButton").style.display = "none"; 
            return false;
        }
        else {
            return false;
        } 

        return false;     
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
           <td><b>Style Code</b>&nbsp;&nbsp;
            <asp:Label ID="qtqty_styleLabel" Width="100px" BackColor="Turquoise"  runat="server" Text='<%# Bind("[qtqty-style]") %>' />
            </td>
          </tr></table> </fieldset>
        </ItemTemplate>
    </asp:FormView>
    <asp:ObjectDataSource ID="ObjectDataSource1" runat="server" 
        OldValuesParameterFormatString="original_{0}" SelectMethod="SelectQuoteQty" 
        TypeName="QuoteDetail">
        <SelectParameters>
            <asp:Parameter Name="prmUser" Type="String" />
            <asp:Parameter DefaultValue="TopInfo" Name="prmAction" Type="String" />
            <asp:SessionParameter DefaultValue="" Name="prmQuote" SessionField="quote_no" 
                Type="Int32" />
            <asp:Parameter Name="prmQty" Type="Int32" />
            <asp:Parameter Name="prmPrice" Type="Decimal" />
            <asp:Parameter Name="prmUom" Type="String" />
            <asp:Parameter Name="prmProfit" Type="Decimal" />
            <asp:Parameter Name="prmRels" Type="Int32" />
            <asp:Parameter Name="prmMatCost" Type="Decimal" />
            <asp:Parameter Name="prmLabCost" Type="Decimal" />
            <asp:Parameter Name="prmFoCost" Type="Decimal" />
            <asp:Parameter Name="prmVoCost" Type="Decimal" />
            <asp:Parameter Name="prmTotLab" Type="Decimal" />
            <asp:Parameter Name="prmDate" Type="String" />
            <asp:SessionParameter Name="prmLine" DefaultValue="0" SessionField="viewquote_qty_lineno" Type="Int32" />
            <asp:Parameter Name="prmReckey" Type="String" />
            <asp:Parameter Name="prmRePrice" Type="String" />
        </SelectParameters>
    </asp:ObjectDataSource>

      <asp:GridView ID="GridView1" runat="server" AllowPaging="True" DataKeyNames="qtqty-reckey"
        AllowSorting="True" OnSelectedIndexChanged="GridView1_SelectedIndex"
        AutoGenerateColumns="False" CssClass="Grid" DataSourceID="ObjectDataSource2"
        EmptyDataText="No Record Found"  Width="730px">
        <EmptyDataRowStyle BorderColor="Gray" BorderStyle="Dotted" BorderWidth="0px" Font-Bold="True"
            HorizontalAlign="Center" VerticalAlign="Middle" />
        <RowStyle CssClass="shade" />
        <Columns>
             <asp:CommandField ButtonType="Image" SelectImageUrl="~/Images/sel.gif" SelectText="" ShowSelectButton="True">
                <ItemStyle Width="10px" />
            </asp:CommandField>
            
            <asp:BoundField DataField="qtqty-qty" HeaderText="Qty" SortExpression="qtqty-qty" />
            <asp:BoundField DataField="qtqty-price" HeaderText="Price" SortExpression="qtqty-price" />
            <asp:BoundField DataField="qtqty-uom" HeaderText="UOM" SortExpression="qtqty-uom" />
            <asp:BoundField DataField="qtqty-profit" HeaderText="Profit%" SortExpression="qtqty-profit" />
            <asp:BoundField DataField="qtqty-rels" HeaderText="Rel" SortExpression="qtqty-rels" />
            <asp:BoundField DataField="qtqty-matcost" HeaderText="Mat'l Cost/M" SortExpression="qtqty-matcost" />
            <asp:BoundField DataField="qtqty-labcost" HeaderText="DL Cost/M" SortExpression="qtqty-labcost" />
            <asp:BoundField DataField="qtqty-focost" HeaderText="Fo Cost/M" SortExpression="qtqty-focost" />
            <asp:BoundField DataField="qtqty-vocost" HeaderText="Vo Cost/M" SortExpression="qtqty-vocost" />
            <asp:BoundField DataField="qtqty-msf" HeaderText="Total MSF" SortExpression="qtqty-msf" />
            <asp:BoundField DataField="qtqty-date" DataFormatString="{0:MM/dd/yyyy}" HeaderText="Quote Date" SortExpression="qtqty-date" />
            <asp:BoundField DataField="qtqty-user" HeaderText="Quoted By" SortExpression="qtqty-user" />
            <asp:BoundField Visible="false" DataField="qtqty-line" HeaderText="line" SortExpression="qtqty-line" />
            <asp:BoundField Visible="false" DataField="qtqty-reckey" HeaderText="line" SortExpression="qtqty-reckey" />
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
            <tr><td><b>Qty:</b></td><td><b>Price:</b></td><td><b>UOM:</b></td><td><b>Profit%:</b></td><td><b>Rel:</b></td>
            <td><b>Mat'l Cost/M:</b></td><td><b>DL Cost/M:</b></td><td><b>Fo Cost/M:</b></td><td><b>Vo Cost/M:</b></td>
            <td><b>Total MSF:</b></td><td><b>Quote Date:</b></td><td><b>Quoted By:</b></td></tr>
            
            <tr><td><asp:TextBox ID="qtqty_qtyTextBox" Width="70px" MaxLength="9" runat="server" Text='<%# Bind("[qtqty-qty]") %>' />
             <asp:CompareValidator ID="CompareValidator1" runat="server" ControlToValidate="qtqty_qtyTextBox" Operator="DataTypeCheck" Type="Integer" Display="Dynamic" SetFocusOnError="true" ErrorMessage="Invalid Entry"></asp:CompareValidator>
            </td>
            <td><asp:TextBox ID="qtqty_priceTextBox" Width="80px" runat="server" Text='<%# Bind("[qtqty-price]") %>' />
             <asp:CompareValidator ID="CompareValidator2" runat="server" ControlToValidate="qtqty_priceTextBox" Operator="DataTypeCheck" Type="Double" Display="Dynamic" SetFocusOnError="true" ErrorMessage="Invalid Entry"></asp:CompareValidator></td>
            <td><asp:TextBox ID="qtqty_uomTextBox" Width="30px" runat="server" Text='<%# Bind("[qtqty-uom]") %>' /></td>
            <td><asp:TextBox ID="qtqty_profitTextBox" Width="80px" runat="server"  Text='<%# Bind("[qtqty-profit]") %>' />
             <asp:CompareValidator ID="CompareValidator3" runat="server" ControlToValidate="qtqty_profitTextBox" Operator="DataTypeCheck" Type="Double" Display="Dynamic" SetFocusOnError="true" ErrorMessage="Invalid Entry"></asp:CompareValidator></td>
            <td><asp:TextBox ID="qtqty_relsTextBox" Width="30px" MaxLength="3" runat="server" Text='<%# Bind("[qtqty-rels]") %>' />
             <asp:CompareValidator ID="CompareValidator4" runat="server" ControlToValidate="qtqty_relsTextBox" Operator="DataTypeCheck" Type="Integer" Display="Dynamic" SetFocusOnError="true" ErrorMessage="Invalid Entry"></asp:CompareValidator></td>
            <td><asp:TextBox ID="qtqty_matcostTextBox" Width="80px" runat="server" Text='<%# Bind("[qtqty-matcost]") %>' />
             <asp:CompareValidator ID="CompareValidator5" runat="server" ControlToValidate="qtqty_matcostTextBox" Operator="DataTypeCheck" Type="Double" Display="Dynamic" SetFocusOnError="true" ErrorMessage="Invalid Entry"></asp:CompareValidator></td>
            <td><asp:TextBox ID="qtqty_labcostTextBox" Width="80px" runat="server" Text='<%# Bind("[qtqty-labcost]") %>' />
             <asp:CompareValidator ID="CompareValidator6" runat="server" ControlToValidate="qtqty_labcostTextBox" Operator="DataTypeCheck" Type="Double" Display="Dynamic" SetFocusOnError="true" ErrorMessage="Invalid Entry"></asp:CompareValidator></td>
            <td><asp:TextBox ID="qtqty_focostTextBox" Width="80px" runat="server" Text='<%# Bind("[qtqty-focost]") %>' />
            <asp:CompareValidator ID="CompareValidator7" runat="server" ControlToValidate="qtqty_focostTextBox" Operator="DataTypeCheck" Type="Double" Display="Dynamic" SetFocusOnError="true" ErrorMessage="Invalid Entry"></asp:CompareValidator></td>
            <td><asp:TextBox ID="qtqty_vocostTextBox" Width="80px" runat="server"  Text='<%# Bind("[qtqty-vocost]") %>' />
            <asp:CompareValidator ID="CompareValidator8" runat="server" ControlToValidate="qtqty_vocostTextBox" Operator="DataTypeCheck" Type="Double" Display="Dynamic" SetFocusOnError="true" ErrorMessage="Invalid Entry"></asp:CompareValidator></td>
            <td><asp:Label ID="qtqty_msfTextBox" BackColor="Turquoise" Width="80px" runat="server" Text='<%# Bind("[qtqty-msf]") %>' /></td>
            <td><asp:TextBox ID="qtqty_dateTextBox" Width="60px" runat="server" onfocus="javascript:preEnter( this, 'yes' );" onblur="javascript:preLeave( this, 'date', '99/99/9999' );" Text='<%# Bind("[qtqty-date]","{0:MM/dd/yyyy}") %>' />
            <a href="#" onblur="document.getElementById('ctl00_ContentPlaceHolder1_FormView2_qtqty_qtyTextBox').focus();"  onClick="showCalendarControl(ctl00_ContentPlaceHolder1_FormView2_qtqty_dateTextBox); return false"><asp:Image ID="Image6"  runat="server" ImageUrl="images/lookup_icon.gif" /></a></td>
            <td><asp:Label ID="qtqty_userTextBox" BackColor="Turquoise" Width="80px" runat="server" Text='<%# Bind("[qtqty-user]") %>' /></td>
            </tr><tr><td style="display:none" colspan="7">
            <asp:TextBox ID="qtqty_totlabTextBox" runat="server"  Text='<%# Bind("[qtqty-reckey]") %>' />           
            <asp:TextBox ID="qtqty_lineTextBox" runat="server" Text='<%# Bind("[qtqty-line]") %>' />
           
            </td></tr>
            
            <tr><td colspan="7">
            <asp:Button ID="UpdateButton" runat="server" CausesValidation="True" CssClass="button" OnClick="Formview2_update_button_click" Text="Save" />
            &nbsp;<asp:Button ID="UpdateCancelButton" runat="server" CssClass="button" OnClick="CancilButton_Click" CausesValidation="False" CommandName="Cancel" Text="Cancel" />
            </td></tr>
             </table> </asp:Panel>         
           
        </EditItemTemplate>
        <InsertItemTemplate>
            <asp:Panel ID="insertpanel" runat="server" DefaultButton="InsertButton">
            <table class="shade">
            <tr><td><b>Qty:</b></td><td><b>Price:</b></td><td><b>UOM:</b></td><td><b>Profit%:</b></td><td><b>Rel:</b></td>
            <td><b>Mat'l Cost/M:</b></td><td><b>DL Cost/M:</b></td><td><b>Fo Cost/M:</b></td><td><b>Vo Cost/M:</b></td>
            <td><b>Total MSF:</b></td><td><b>Quote Date:</b></td><td><b>Quoted By:</b></td></tr>
            
            <tr><td><asp:TextBox ID="qtqty_qtyTextBox" Width="70px" MaxLength="9" runat="server" Text='<%# Bind("[qtqty-qty]") %>' />
             <asp:CompareValidator ID="CompareValidator1" runat="server" ControlToValidate="qtqty_qtyTextBox" Operator="DataTypeCheck" Type="Integer" Display="Dynamic" SetFocusOnError="true" ErrorMessage="Invalid Entry"></asp:CompareValidator>
            </td>
            <td><asp:TextBox ID="qtqty_priceTextBox" Width="80px" runat="server" Text='<%# Bind("[qtqty-price]") %>' />
             <asp:CompareValidator ID="CompareValidator2" runat="server" ControlToValidate="qtqty_priceTextBox" Operator="DataTypeCheck" Type="Double" Display="Dynamic" SetFocusOnError="true" ErrorMessage="Invalid Entry"></asp:CompareValidator></td>
            <td><asp:TextBox ID="qtqty_uomTextBox" Width="30px" runat="server" Text='<%# Bind("[qtqty-uom]") %>' /></td>
            <td><asp:TextBox ID="qtqty_profitTextBox" Width="80px" runat="server"  Text='<%# Bind("[qtqty-profit]") %>' />
             <asp:CompareValidator ID="CompareValidator3" runat="server" ControlToValidate="qtqty_profitTextBox" Operator="DataTypeCheck" Type="Double" Display="Dynamic" SetFocusOnError="true" ErrorMessage="Invalid Entry"></asp:CompareValidator></td>
            <td><asp:TextBox ID="qtqty_relsTextBox" Width="30px" MaxLength="3" runat="server" Text='<%# Bind("[qtqty-rels]") %>' />
             <asp:CompareValidator ID="CompareValidator4" runat="server" ControlToValidate="qtqty_relsTextBox" Operator="DataTypeCheck" Type="Integer" Display="Dynamic" SetFocusOnError="true" ErrorMessage="Invalid Entry"></asp:CompareValidator></td>
            <td><asp:TextBox ID="qtqty_matcostTextBox" Width="80px" runat="server" Text='<%# Bind("[qtqty-matcost]") %>' />
             <asp:CompareValidator ID="CompareValidator5" runat="server" ControlToValidate="qtqty_matcostTextBox" Operator="DataTypeCheck" Type="Double" Display="Dynamic" SetFocusOnError="true" ErrorMessage="Invalid Entry"></asp:CompareValidator></td>
            <td><asp:TextBox ID="qtqty_labcostTextBox" Width="80px" runat="server" Text='<%# Bind("[qtqty-labcost]") %>' />
             <asp:CompareValidator ID="CompareValidator6" runat="server" ControlToValidate="qtqty_labcostTextBox" Operator="DataTypeCheck" Type="Double" Display="Dynamic" SetFocusOnError="true" ErrorMessage="Invalid Entry"></asp:CompareValidator></td>
            <td><asp:TextBox ID="qtqty_focostTextBox" Width="80px" runat="server" Text='<%# Bind("[qtqty-focost]") %>' />
            <asp:CompareValidator ID="CompareValidator7" runat="server" ControlToValidate="qtqty_focostTextBox" Operator="DataTypeCheck" Type="Double" Display="Dynamic" SetFocusOnError="true" ErrorMessage="Invalid Entry"></asp:CompareValidator></td>
            <td><asp:TextBox ID="qtqty_vocostTextBox" Width="80px" runat="server"  Text='<%# Bind("[qtqty-vocost]") %>' />
            <asp:CompareValidator ID="CompareValidator8" runat="server" ControlToValidate="qtqty_vocostTextBox" Operator="DataTypeCheck" Type="Double" Display="Dynamic" SetFocusOnError="true" ErrorMessage="Invalid Entry"></asp:CompareValidator></td>
            <td><asp:Label ID="qtqty_msfTextBox" BackColor="Turquoise" Width="80px" runat="server" Text='<%# Bind("[qtqty-msf]") %>' /></td>
            <td><asp:TextBox ID="qtqty_dateTextBox" Width="60px" runat="server" onfocus="javascript:preEnter( this, 'yes' );" onblur="javascript:preLeave( this, 'date', '99/99/9999' );" Text='<%# Bind("[qtqty-date]","{0:MM/dd/yyyy}") %>' />
            <a href="#" onblur="document.getElementById('ctl00_ContentPlaceHolder1_FormView2_qtqty_qtyTextBox').focus();" onClick="showCalendarControl(ctl00_ContentPlaceHolder1_FormView2_qtqty_dateTextBox); return false"><asp:Image ID="Image6" runat="server" ImageUrl="images/lookup_icon.gif" /></a></td>
            <td><asp:Label ID="qtqty_userTextBox" BackColor="Turquoise" Width="80px" runat="server" Text='<%# Bind("[qtqty-user]") %>' /></td>
            </tr><tr><td style="display:none" colspan="7">
            <asp:TextBox ID="qtqty_totlabTextBox" runat="server"  Text='<%# Bind("[qtqty-reckey]") %>' />           
            <asp:TextBox ID="qtqty_lineTextBox" runat="server" Text='<%# Bind("[qtqty-line]") %>' />
           
            </td></tr>
            
            <tr><td colspan="7">
            <asp:Button ID="InsertButton" runat="server" CausesValidation="True" OnClick="Formview2_insert_button_Click" CssClass="button" Text="Save" />
            &nbsp;<asp:Button ID="InsertCancelButton" runat="server" CssClass="button" CausesValidation="False" CommandName="Cancel" Text="Cancel" />
            </td></tr></table></asp:Panel>
        </InsertItemTemplate>
        <ItemTemplate>
            <table>
            <tr><td style="display:none">
            qtqty-qty:
            <asp:Label ID="qtqty_qtyLabel" runat="server" Text='<%# Bind("[qtqty-qty]") %>' />           
            qtqty-price:
            <asp:Label ID="qtqty_priceLabel" runat="server" Text='<%# Bind("[qtqty-price]") %>' />
          
            <asp:Label ID="qtqty_uomLabel" runat="server" Text='<%# Bind("[qtqty-uom]") %>' />            
            <asp:Label ID="qtqty_profitLabel" runat="server" Text='<%# Bind("[qtqty-profit]") %>' />           
            <asp:Label ID="qtqty_relsLabel" runat="server" Text='<%# Bind("[qtqty-rels]") %>' />            
            <asp:Label ID="qtqty_matcostLabel" runat="server" Text='<%# Bind("[qtqty-matcost]") %>' />            
            <asp:Label ID="qtqty_labcostLabel" runat="server" Text='<%# Bind("[qtqty-labcost]") %>' />            
            <asp:Label ID="qtqty_focostLabel" runat="server" Text='<%# Bind("[qtqty-focost]") %>' />           
            <asp:Label ID="qtqty_vocostLabel" runat="server" Text='<%# Bind("[qtqty-vocost]") %>' />            
            <asp:Label ID="qtqty_msfLabel" runat="server" Text='<%# Bind("[qtqty-msf]") %>' />          
            <asp:Label ID="qtqty_totlabLabel" runat="server" Text='<%# Bind("[qtqty-reckey]") %>' />         
            <asp:Label ID="qtqty_dateLabel" runat="server" Text='<%# Bind("[qtqty-date]") %>' />           
            <asp:Label ID="qtqty_userLabel" runat="server" Text='<%# Bind("[qtqty-user]") %>' />            
            <asp:Label ID="qtqty_lineLabel" runat="server" Text='<%# Bind("[qtqty-line]") %>' />
                </td></tr>
                
            <tr>
                <td>
                    <asp:RadioButtonList ID="RadioButtonList1" AutoPostBack="true" OnSelectedIndexChanged="RepriceButton_Click" runat="server" RepeatLayout="Flow" RepeatColumns="5" CellSpacing="1">
                        <asp:ListItem Value="EA">EA</asp:ListItem>
                        <asp:ListItem Value="M">M</asp:ListItem>
                        <asp:ListItem Value="MSF">MSF</asp:ListItem>
                        <asp:ListItem Value="CS">CASE</asp:ListItem>
                        <asp:ListItem Value="L">LOT</asp:ListItem>
                    </asp:RadioButtonList>
                </td>
            </tr>    
            
            <tr><td>
            <asp:Button ID="AddButton" runat="server"  CausesValidation="False" CssClass="buttonM" CommandName="new"
                 Text="Add">
                </asp:Button> 
            <asp:Button ID="UpdateButton" runat="server"  CssClass="button" CausesValidation="False" CommandName="edit"
                Text="Update">
            </asp:Button>
            <asp:Button ID="CopyButton" runat="server" OnClick="CopyButton_Click" CssClass="button" CausesValidation="False" CommandName="edit"
                Text="Copy">
            </asp:Button>
            <asp:Button ID="DeleteButton" runat="server"  CssClass="button" CausesValidation="False" OnClick="Formview2_deletebutton_Click"  OnClientClick="return confirm('Delete Currently Selected Record?')"   Text="Delete">
            </asp:Button>
            <asp:Button ID="RepriceButton" runat="server"  CssClass="button" CausesValidation="False" OnClientClick="return checkReprice();" Text="Reprice">
            </asp:Button>
            </td></tr>
            </table>
        </ItemTemplate>
    </asp:FormView>
    
    <asp:ObjectDataSource ID="ObjectDataSource_item" runat="server" 
        OldValuesParameterFormatString="original_{0}" SelectMethod="SelectQuoteQty" 
        TypeName="QuoteDetail">
        <SelectParameters>
            <asp:Parameter Name="prmUser" Type="String" />
            <asp:Parameter DefaultValue="View" Name="prmAction" Type="String" />
            <asp:SessionParameter DefaultValue="" Name="prmQuote" SessionField="quote_no" Type="Int32" />
            <asp:Parameter Name="prmQty" Type="Int32" />
            <asp:Parameter Name="prmPrice" Type="Decimal" />
            <asp:Parameter Name="prmUom" Type="String" />
            <asp:Parameter Name="prmProfit" Type="Decimal" />
            <asp:Parameter Name="prmRels" Type="Int32" />
            <asp:Parameter Name="prmMatCost" Type="Decimal" />
            <asp:Parameter Name="prmLabCost" Type="Decimal" />
            <asp:Parameter Name="prmFoCost" Type="Decimal" />
            <asp:Parameter Name="prmVoCost" Type="Decimal" />
            <asp:Parameter Name="prmTotLab" Type="Decimal" />
            <asp:Parameter Name="prmDate" Type="String" />
            <asp:SessionParameter Name="prmLine" DefaultValue="0" SessionField="viewquote_qty_lineno" Type="Int32" />
          <asp:ControlParameter ControlID="GridView1" Name="prmReckey" 
                PropertyName="SelectedValue" Type="String" />
            <asp:Parameter Name="prmRePrice" Type="String" />    
        </SelectParameters>
    </asp:ObjectDataSource>
    
    <asp:ObjectDataSource ID="ObjectDataSource2" runat="server" 
        OldValuesParameterFormatString="original_{0}" SelectMethod="SelectQuoteQty" 
        TypeName="QuoteDetail">
        <SelectParameters>
            <asp:Parameter Name="prmUser" Type="String" />
            <asp:Parameter Name="prmAction" DefaultValue="Select" Type="String" />
            <asp:SessionParameter DefaultValue="" Name="prmQuote" SessionField="quote_no" Type="Int32" />
            <asp:Parameter Name="prmQty" Type="Int32" />
            <asp:Parameter Name="prmPrice" Type="Decimal" />
            <asp:Parameter Name="prmUom" Type="String" />
            <asp:Parameter Name="prmProfit" Type="Decimal" />
            <asp:Parameter Name="prmRels" Type="Int32" />
            <asp:Parameter Name="prmMatCost" Type="Decimal" />
            <asp:Parameter Name="prmLabCost" Type="Decimal" />
              <asp:Parameter Name="prmFoCost" Type="Decimal" />
            <asp:Parameter Name="prmVoCost" Type="Decimal" />
            <asp:Parameter Name="prmTotLab" Type="Decimal" />
            <asp:Parameter Name="prmDate" Type="String" />
            <asp:SessionParameter Name="prmLine" DefaultValue="0" SessionField="viewquote_qty_lineno" Type="Int32" />
            <asp:Parameter Name="prmReckey" Type="String" />
            <asp:Parameter Name="prmRePrice" Type="String" />
        </SelectParameters>
    </asp:ObjectDataSource>
     
    
     
</div>

<script language="javascript" type="text/javascript">
    document.getElementById("ctl00_ContentPlaceHolder1_FormView2_RadioButtonList1").style.display = "none";    
</script>

</asp:Content>

