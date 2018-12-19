<%@ Page Language="C#" MasterPageFile="MasterPageRcpt.master" Debug="true" AutoEventWireup="true" Inherits="list_rcpt" Title="Warehouse Transactions Receipts" Codebehind="list_rcpt.aspx.cs" %>

<asp:Content ID="Content1" ContentPlaceHolderID="ContentPlaceHolder1"  runat="server">
<LINK REL="stylesheet" TYPE="text/css" HREF="include/CalendarControl.css" >
    <script language = "JavaScript" src="include/CalendarControl.js"></script>
    <script language="javascript" src="include/date.js"></script>
    <script language="javascript" src="include/event.js"></script>
    <script language="javascript" src="include/insert.js"></script>
<script type="text/javascript" language="javascript">

    window.onload = setfocus;
    function setfocus() {
        document.forms[0].ctl00_ContentPlaceHolder1_txt_seq.focus();
    }
   
function rfqslook() {
    var custval = document.forms[0].ctl00$ContentPlaceHolder1$txt_customer.value;
    var NewWindow = window.open("rfqs_lookup.aspx?customer=" + custval, "RfqsLookupWindow", "width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}
function rfqLookup(ReturnObj1){
    document.forms[0].ctl00$ContentPlaceHolder1$txt_rfq.value = ReturnObj1;
    document.forms[0].ctl00$ContentPlaceHolder1$txt_rfq.focus();
}

function customerlook(){ 
  var NewWindow = window.open("customer_lookup.aspx","CustomerLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}

function CustomerLookup(ReturnObj1){
    document.forms[0].ctl00$ContentPlaceHolder1$txt_customer.value = ReturnObj1;
    document.forms[0].ctl00$ContentPlaceHolder1$txt_customer.focus();
}

function custpartlook() {
    var custval = document.forms[0].ctl00$ContentPlaceHolder1$txt_customer.value;
    var NewWindow = window.open("customerpart_lookup.aspx?customer=" + custval, "CustPartLookupWindow", "width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}

function CustPartLookup(ReturnObj1){
    document.forms[0].ctl00$ContentPlaceHolder1$txt_custpart.value = ReturnObj1;
    document.forms[0].ctl00$ContentPlaceHolder1$txt_custpart.focus();
}

function customerpolook() {

    var NewWindow = window.open("lpofglook.aspx", "CustomerpoWindow", "width=650,height=400,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}

function lpofglLookup(ReturnObj1) {
    document.forms[0].ctl00$ContentPlaceHolder1$txt_po.value = ReturnObj1;
    document.forms[0].ctl00_ContentPlaceHolder1_txt_po.focus();
}

function fglook() {
    var cust = "";
    //if(cust=="")
    //{
    //    var cust=document.getElementById("ctl00_ContentPlaceHolder1_HiddenField1").value;    
    //}

    var NewWindow = window.open("fgitem2_lookup.aspx?customer=" + cust + "", "FGLookupWindow", "width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}

function FGLookup(ReturnObj1) {
    document.forms[0].ctl00$ContentPlaceHolder1$txt_ino.value = ReturnObj1;
    document.forms[0].ctl00$ContentPlaceHolder1$txt_ino.focus();
}

function job1look() {
    var NewWindow = window.open("job1_lookup.aspx", "Job1LookupWindow", "width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}
function Job1Lookup(ReturnObj1) {
    document.forms[0].ctl00$ContentPlaceHolder1$txt_job.value = ReturnObj1;
    document.forms[0].ctl00$ContentPlaceHolder1$txt_job.focus();
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
<TABLE id="tblSearch" cellSpacing="1" cellPadding="5" border="0"  bgcolor=black>
              <TR>
                <TD class="shade" align="left"><br>
                  <table cellspacing="2" cellpadding="1"  border="0" class="shade" bgcolor="gray">    		   
		   <tr>
        		<td nowrap> <asp:Button ID="btnSearch" runat="server" Text="Go" OnClick="btnSearch_Click" CssClass="button" Width="40px" /><br />
                    <br />
                    <asp:Button ID="btn_reset" runat="server" CssClass="button" Text="All" OnClick="btn_reset_Click" Width="40px" />&nbsp;</td>
               <td nowrap="nowrap">
                   Seq# <br>           
			<asp:TextBox ID="txt_seq" runat="server" Width="65px" ></asp:TextBox>
			<asp:CompareValidator ID="CompareValidator4" runat="server" ControlToValidate="txt_seq" SetFocusOnError="true" Display="dynamic" Operator="dataTypeCheck" Type="Integer" ErrorMessage="Invalid Number"></asp:CompareValidator>
			
          	   </td>
          	   <td nowrap id="customerid" runat="server">Tag#
                        <%--<asp:Label ID="customerlabel" runat="server" Text="Customer#"></asp:Label>--%>  <br>
            <asp:TextBox ID="txt_tag"  runat="server" MaxLength="20" Width="118px" ></asp:TextBox>
			<%--<a href="#" tabindex="1" onClick="customerlook(); return false"><asp:Image ID="CustomerLook" runat="server" ImageUrl="images/lookup_icon.gif" /></a>--%>
                	</td>
                	
           		     
            		<td nowrap>Receipt Date# <br>
            <asp:TextBox ID="txt_date" runat="server" Width="65px" onfocus="javascript:preEnter( this, 'yes' );" onblur="javascript:preLeave( this, 'date', '99/99/9999' );"></asp:TextBox>
			<a href="#" tabindex="1" onblur="ctl00_ContentPlaceHolder1_txt_date.focus()" onClick="showCalendarControl(ctl00_ContentPlaceHolder1_txt_date); return false"><asp:Image ID="Image3" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
                	</td>         
            		<td nowrap> Po# <br>
            <asp:TextBox ID="txt_po" runat="server" MaxLength="9" Width="65px" ></asp:TextBox>
			<a href="#" tabindex="1" onClick="customerpolook(); return false"><asp:Image ID="pofglLookup" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
                	</td> 
                	
                	<td nowrap> Item No# <br>
            <asp:TextBox ID="txt_ino" runat="server" MaxLength="15" Width="95px" ></asp:TextBox>
			<a href="#" tabindex="1" onClick="fglook(); return false"><asp:Image ID="FGLookup" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
            
                	</td> 
                	<td nowrap> Job# <br>
            <asp:TextBox ID="txt_job" runat="server" MaxLength="6" Width="45px" ></asp:TextBox>            
			<a href="#" tabindex="1" onClick="job1look(); return false"><asp:Image ID="Job1Lookup" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
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
                
            <asp:BoundField DataField="vRno" HeaderText="Seq#" SortExpression="vRno" />
            <asp:BoundField DataField="vDate" HeaderText="Receipt Date" SortExpression="vDate" />
            <asp:BoundField DataField="vTransTime" HeaderText="Receipt Time" 
                SortExpression="vTransTime" />
            <asp:BoundField DataField="vTag" HeaderText="Tag#"  FooterStyle-Wrap="false" ItemStyle-Wrap="false" SortExpression="vTag" HeaderStyle-Wrap="false" />
            <asp:BoundField DataField="vPono" ItemStyle-Wrap="false" HeaderText="Po#" SortExpression="vPono" />
            <asp:BoundField DataField="vJobno" ItemStyle-Wrap="false" HeaderText="Job#" 
                SortExpression="vJobno" />
            <asp:BoundField DataField="vJobno2" HeaderText="" 
                SortExpression="vJobno2" />
            <asp:BoundField DataField="vItem" ItemStyle-Wrap="false" HeaderText="Item#" SortExpression="vItem" />
            <asp:BoundField DataField="vItemName" ItemStyle-Wrap="false" HeaderText="Name/Desc" 
                SortExpression="vItemName" />
            <asp:BoundField DataField="vLoc" HeaderText="Whse" SortExpression="vLoc" />
            <asp:BoundField DataField="vLocBin" ItemStyle-Wrap="false" HeaderText="Bin" 
                SortExpression="vLocBin" />
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
            <asp:BoundField DataField="vTQty" HeaderText="Total!Qty" SortExpression="vTQty" />
            <asp:BoundField DataField="vFrtCost" HeaderText="Freight Cost" 
                SortExpression="vFrtCost" />
            <asp:BoundField DataField="vExtCost" HeaderText="Extended Cost" 
                SortExpression="vExtCost" />
            <asp:BoundField DataField="vStackCode" HeaderText="FG Lot#" 
                SortExpression="vStackCode" />
            <asp:BoundField DataField="vCreatedBy" HeaderText="Created By " 
                SortExpression="vCreatedBy" />
            <asp:BoundField DataField="vCreate2" HeaderText="Last Updated By" 
                SortExpression="vCreate2" />
            <asp:BoundField DataField="vTotWt" HeaderText="Total Weight" 
                SortExpression="vTotWt" />
            <asp:BoundField Visible="false" DataField="vRecKey" HeaderText="vRecKey" 
                SortExpression="vRecKey" />
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
        SelectMethod="SelectRcpt" TypeName="itemhistory">
        <SelectParameters>            
            <asp:SessionParameter Name="prmUser" SessionField="prmUser" Type="String" />            
            <asp:SessionParameter Name="prmAction" DefaultValue="Select" SessionField="prmAction_rcpt" Type="String"  />
            <asp:SessionParameter Name="prmSeqno" SessionField="prmSeq_rcpt" Type="String" />
            <asp:SessionParameter Name="prmTagno" SessionField="prmTag_rcpt" Type="String" />
            <asp:SessionParameter Name="prmRcptDate" SessionField="prmDate_rcpt" Type="String" />
            <asp:SessionParameter Name="prmPono" SessionField="prmPo_rcpt" Type="String" />
            <asp:SessionParameter Name="prmFgItem" SessionField="prmIno_rcpt" Type="String" />
            <asp:SessionParameter Name="prmJobno" SessionField="prmJob_rcpt" Type="String" />
            <asp:Parameter Name="prmRecKey" Type="String" />
            <asp:Parameter Name="prmllSetParts" Type="String" DefaultValue="no" />
        </SelectParameters>
    </asp:ObjectDataSource>
</div>

</asp:Content>
