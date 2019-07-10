<%@ Page Language="C#" MasterPageFile="MasterPageMoveRcpt.master" Debug="true" AutoEventWireup="true" Inherits="movelist_rcpt" Title="Warehouse Transactions Receipts Update" Codebehind="movelist_rcpt.aspx.cs" %>

<asp:Content ID="Content1" ContentPlaceHolderID="ContentPlaceHolder1"  runat="server">
<LINK REL="stylesheet" TYPE="text/css" HREF="include/CalendarControl.css" >
    <script language = "JavaScript" src="include/CalendarControl.js"></script>
    <script language="javascript" src="include/date.js"></script>
    <script language="javascript" src="include/event.js"></script>
    <script language="javascript" src="include/insert.js"></script>
<script type="text/javascript" language="javascript">

    window.onload = setfocus;
    function setfocus() {
        document.forms[0].ctl00_ContentPlaceHolder1_txt_tag.focus();
    }
   


function fgtaglook() {
    //var loc1 = document.forms[0].ctl00_ContentPlaceHolder1_txt_tag.value;
    var NewWindow = window.open("fgtaglookup.aspx", "fgtagLookUpWindow", "width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}
function fgtaglookup(ReturnObj1) {
    document.forms[0].ctl00_ContentPlaceHolder1_txt_tag.value = ReturnObj1;
    document.forms[0].ctl00_ContentPlaceHolder1_txt_tag.focus();

}

function clickButton(e, ctl00$ContentPlaceHolder1$btnSearch){ 

      var evt = e ? e : window.event;

      var bt = document.getElementById(ctl00$ContentPlaceHolder1$btnSearch);

      if (bt){ 

          if (evt.keyCode == 13){ 

                bt.click(); 

                return false; 

          } 

      } 

}

</script>
<div >
<TABLE id="tblSearch" cellSpacing="1" cellPadding="5" border="0" width="900px" bgcolor=black>
              <TR>
                <TD class="shade" align="left"><br>
                  <table cellspacing="2" cellpadding="1"  border="0" class="shade" bgcolor="gray">    		   
		   <tr>
        		<td nowrap> <asp:Button ID="btnSearch" runat="server" Text="Go" OnClick="btnSearch_Click" CssClass="button" Width="40px" /><br />
                    <br />
                    <asp:Button ID="btn_reset" runat="server" CssClass="button" Text="All" OnClick="btn_reset_Click" Width="40px" />&nbsp;</td>
               
          	   <td nowrap id="customerid" runat="server">Tag#
                        <%--<asp:Label ID="customerlabel" runat="server" Text="Customer#"></asp:Label>--%>  <br>
            <asp:TextBox ID="txt_tag"  runat="server" MaxLength="20" Width="150px" ></asp:TextBox>
			<a href="#" tabindex="1" onClick="fgtaglook(); return false"><asp:Image ID="fgtagimage" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
                	</td>
                	               		
                  <td nowrap>Rows/Page<br>
                  
                      <asp:FormView ID="FormView1" runat="server" DataSourceID="ObjectDataSource2">
                          <EditItemTemplate>
                              aLine:
                              <asp:TextBox ID="aLineTextBox" runat="server" Text='<%# Bind("aLine") %>'>
                              </asp:TextBox><br />
                              <asp:LinkButton ID="UpdateButton" runat="server" CausesValidation="True" CommandName="Update"
                                  Text="Update">
                              </asp:LinkButton>
                              <asp:LinkButton ID="UpdateCancelButton" runat="server" CausesValidation="False" CommandName="Cancel"
                                  Text="Cancel">
                              </asp:LinkButton>
                          </EditItemTemplate>
                          <InsertItemTemplate>
                              aLine:
                              <asp:TextBox ID="aLineTextBox" runat="server" Text='<%# Bind("aLine") %>'>
                              </asp:TextBox><br />
                              <asp:LinkButton ID="InsertButton" runat="server" CausesValidation="True" CommandName="Insert"
                                  Text="Insert">
                              </asp:LinkButton>
                              <asp:LinkButton ID="InsertCancelButton" runat="server" CausesValidation="False" CommandName="Cancel"
                                  Text="Cancel">
                              </asp:LinkButton>
                          </InsertItemTemplate>
                          <ItemTemplate>
                             
                              <asp:TextBox ID="aLineLabel" runat="server" MaxLength="4" Width="40px" OnTextChanged="ddl_display_TextChanged" Text='<%# Bind("aLine") %>'></asp:TextBox>
                              <asp:CompareValidator ID="CompareValidator4" runat="server" ControlToValidate="aLineLabel" SetFocusOnError="true" Display="dynamic" Operator="dataTypeCheck" Type="Integer" ErrorMessage="Invalid Number"></asp:CompareValidator>
                              <%--<asp:Label ID="aLineLabel" runat="server" Text='<%# Bind("aLine") %>'></asp:Label>--%>
                          </ItemTemplate>
                      </asp:FormView>
                      <asp:ObjectDataSource ID="ObjectDataSource2" runat="server" OldValuesParameterFormatString="original_{0}"
                          SelectMethod="SelectRows" TypeName="Order">
                          <SelectParameters>
                              <asp:SessionParameter Name="prmUser" SessionField="Rowuser" Type="String" />
                              <asp:SessionParameter Name="vLine" Type="Int32" SessionField="gridsize" />
                          </SelectParameters>
                      </asp:ObjectDataSource>
                  
                </td>
			<td></td>
			<td></td>
            
                  </tr>
                  </table>
               </TD></TR>  
       </table>
</div>
<div>
    <br />
    <asp:GridView ID="GridView1" runat="server" OnSelectedIndexChanged="GridView1_SelectedIndexChanged" AutoGenerateColumns="False" DataSourceID="ObjectDataSource1" AllowPaging="True" AllowSorting="True" BorderStyle="Dotted" CssClass="Grid" EmptyDataText="No Record Found" OnPageIndexChanging="GridView1_PageIndexChanging">
        <Columns>
            
            <asp:CommandField ShowSelectButton="True" ButtonType="Image" SelectImageUrl="~/Images/sel.gif" SelectText="" > 
                    <ItemStyle Width="10px" />
            </asp:CommandField>
                      
            <asp:BoundField DataField="vTag" HeaderText="Tag#"  FooterStyle-Wrap="false" ItemStyle-Wrap="false" SortExpression="vTag" HeaderStyle-Wrap="false" />
            <asp:BoundField DataField="vLoc" HeaderText="Whse" SortExpression="vLoc" ItemStyle-Wrap="false" />
            <asp:BoundField DataField="vLocBin" HeaderText="Bin" ItemStyle-Wrap="false"
                SortExpression="vLocBin" />
            <asp:BoundField DataField="vJob_no" ItemStyle-Wrap="false" HeaderText="Job#" 
                SortExpression="vJob_no" />
            <asp:BoundField DataField="vJob_no2" HeaderText="" 
                SortExpression="vJob_no2" />    
            <asp:BoundField DataField="vDate" HeaderText="Receipt Date" SortExpression="vDate" />
            <asp:BoundField Visible="false" DataField="vTransTime" HeaderText="Receipt Time" 
                SortExpression="vTransTime" />
            
            <asp:BoundField DataField="vPo_no" ItemStyle-Wrap="false" HeaderText="Po#" SortExpression="vPono" />
            
            <asp:BoundField DataField="vItem" ItemStyle-Wrap="false" HeaderText="Item#" SortExpression="vItem" />
            <asp:BoundField DataField="vItemName" ItemStyle-Wrap="false" HeaderText="Name/Desc" 
                SortExpression="vItemName" />
            
            <asp:BoundField DataField="vCases" HeaderText="Units" 
                SortExpression="vCases" />
            <asp:BoundField DataField="vQtyCas" HeaderText="Unit Count" 
                SortExpression="vQtyCas" />
            <asp:BoundField DataField="vCasUnit" HeaderText="Unit per Pallet" 
                SortExpression="vCasUnit" />
            <asp:BoundField DataField="vPartial" HeaderText="Partial" 
                SortExpression="vPartial" />
            <asp:BoundField DataField="vStdCost" HeaderText="Cost/Uom" 
                SortExpression="vStdCost" />
            <asp:BoundField DataField="vCostUom" HeaderText="Uom" 
                SortExpression="vCostUom" />
            <asp:BoundField DataField="vT_Qty" HeaderText="Total!Qty" SortExpression="vT_Qty" />
            <%-- %><%--asp:BoundField DataField="vFrtCost" HeaderText="Costs" 
                SortExpression="vFrtCost" />--%>
            <asp:BoundField DataField="vExtCost" HeaderText="Extended Cost" 
                SortExpression="vExtCost" />            
            <%--<asp:BoundField DataField="vCreatedBy" HeaderText="Created By " 
                SortExpression="vCreatedBy" />
            <asp:BoundField DataField="vCreate2" HeaderText="Last Updated By" 
                SortExpression="vCreate2" />--%>
            <asp:BoundField DataField="vStackCode" HeaderText="FG Lot#" 
                SortExpression="vStackCode" />    
            <asp:BoundField Visible="false" DataField="vTot_Wt" HeaderText="Total Wheight" 
                SortExpression="vTot_Wt" />
            <asp:BoundField Visible="false" DataField="vRecKey" HeaderText="vRecKey" 
                SortExpression="vRecKey" />
              <asp:TemplateField HeaderText="vRno" Visible="False" >                    
                    <ItemTemplate>
                        <asp:Label ID="Labelseq" runat="server" Text='<%# Bind("[vRno]") %>'></asp:Label>
                    </ItemTemplate>
                    <ItemStyle Wrap="False" />
                </asp:TemplateField>
           </Columns>
        <EmptyDataRowStyle BorderColor="Gray" BorderStyle="Dotted" Width="100%" BorderWidth="0px" Font-Bold="True"
            HorizontalAlign="Center" VerticalAlign="Middle" Wrap="False" />
            <SelectedRowStyle CssClass="GridSelected" BackColor="Yellow" />
        <HeaderStyle ForeColor="White" CssClass="headcolor" HorizontalAlign="Center"
            VerticalAlign="Middle" Wrap="False" />
        <RowStyle CssClass="shade" />
        <AlternatingRowStyle CssClass="GridItemOdd" />
    </asp:GridView>
    
    
    <asp:ObjectDataSource ID="ObjectDataSource1" runat="server" OldValuesParameterFormatString="original_{0}"
        SelectMethod="MoveRece" TypeName="itemhistory">
        <SelectParameters>            
             <asp:SessionParameter Name="prmUser" SessionField="prmUser" Type="String" />            
            <asp:SessionParameter Name="prmAction" DefaultValue="GridSelect" SessionField="prmAction_move_list" Type="String"  />
            <asp:Parameter Name="prmFgItem"  Type="String" />
            <asp:Parameter Name="prmJobno"  Type="String" />
            <asp:Parameter Name="prmPono"  Type="String" />
            <asp:Parameter Name="prmSeqno"  Type="String" />
            <asp:Parameter Name="prmRcptDate"  Type="String" />
            <asp:SessionParameter SessionField="prmTag_move_list" Name="prmTagno"  Type="String" />
            <asp:Parameter Name="prmTransTime" Type="String"></asp:Parameter>
            <asp:Parameter Name="prmJob_no2" Type="String" />
            <asp:Parameter Name="prmName" Type="String" />
            <asp:Parameter Name="prmLoc" Type="String" />
            <asp:Parameter Name="prmLocBin" Type="String" />
            <asp:Parameter Name="prmCases" Type="String" />
            <asp:Parameter Name="prmQty_Cas" Type="String" />
            <asp:Parameter Name="prmCasUnit" Type="String" />
            <asp:Parameter Name="prmPartial" Type="String" />
            <asp:Parameter Name="prmStdCost" Type="String" />
            <asp:Parameter Name="prmCost_Uom" Type="String" />
            <asp:Parameter Name="prmTQty" Type="String" />
            <asp:Parameter Name="prmFrtCost" Type="String" />
            <asp:Parameter Name="prmExtCost" Type="String" />
            <asp:Parameter Name="prmStackCode" Type="String" />
            <asp:Parameter Name="prmCreatedBy" Type="String" />
            <asp:Parameter Name="prmCreate2" Type="String" />
            <asp:Parameter Name="prmTotWt" Type="String" />
            <asp:Parameter Name="prmRecKey" Type="String" />
        </SelectParameters>
    </asp:ObjectDataSource>
</div>

</asp:Content>
