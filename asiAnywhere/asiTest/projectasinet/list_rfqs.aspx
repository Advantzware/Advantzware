<%@ Page Language="C#" MasterPageFile="MasterPage5.master" Debug="true" AutoEventWireup="true" Inherits="list_rfqs" Title="Request for Quote" Codebehind="list_rfqs.aspx.cs" %>

<asp:Content ID="Content1" ContentPlaceHolderID="ContentPlaceHolder1"  runat="server">
<script type="text/javascript" language="javascript">
   window.onload=setfocus;
function setfocus()
{
    document.forms[0].ctl00$ContentPlaceHolder1$txt_rfq.focus();
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

function rfgpartdescriptionlook(){ 
  var NewWindow = window.open("rfgpartdescription_lookup.aspx","RFGLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}

function RFGPartDescriptionLookup(ReturnObj1){
    document.forms[0].ctl00$ContentPlaceHolder1$txt_partdescription.value = ReturnObj1;
    document.forms[0].ctl00$ContentPlaceHolder1$txt_partdescription.focus();
}




function stylelook(){ 
  var NewWindow = window.open("rfgstyle_lookup.aspx","RfgStyleLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}

function RfgStyleLookup(ReturnObj1){
    document.forms[0].ctl00$ContentPlaceHolder1$txt_style.value = ReturnObj1;
    document.forms[0].ctl00$ContentPlaceHolder1$txt_style.focus();
}

function estimatelook() {
    var custval = document.forms[0].ctl00$ContentPlaceHolder1$txt_customer.value;
    var NewWindow = window.open("estimate_lookup.aspx?customer_val=" + custval, "EstimateLookupWindow", "width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}
function EstimateLookup(ReturnObj1){
    document.forms[0].ctl00$ContentPlaceHolder1$txt_estimate.value = ReturnObj1;
    document.forms[0].ctl00$ContentPlaceHolder1$txt_estimate.focus();
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
function blank()
{
  document.forms[0].ctl00$ContentPlaceHolder1$txt_l.value="";
}
function blank1()
{
  document.forms[0].ctl00$ContentPlaceHolder1$txt_w.value="";
}
function blank2()
{
  document.forms[0].ctl00$ContentPlaceHolder1$txt_d.value="";
}

function limit()
{
 var length=document.getElementById("ctl00_ContentPlaceHolder1_txt_l");
 var len=document.getElementById("ctl00_ContentPlaceHolder1_txt_l").value;
 length.value=len + ".0000";
 
}
function limit1()
{
 var width=document.getElementById("ctl00_ContentPlaceHolder1_txt_w");
 var wid=document.getElementById("ctl00_ContentPlaceHolder1_txt_w").value;
 width.value=wid + ".0000";
}
function limit2()
{
 var depth=document.getElementById("ctl00_ContentPlaceHolder1_txt_d");
 var dep=document.getElementById("ctl00_ContentPlaceHolder1_txt_d").value;
 depth.value=dep + ".0000";
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
               <td nowrap="nowrap" style="width: 97px">
                   RFQ# <br>           
			<asp:TextBox ID="txt_rfq" runat="server" MaxLength="9" Width="65px" ></asp:TextBox>
			<asp:CompareValidator ID="CompareValidator4" runat="server" ControlToValidate="txt_rfq" SetFocusOnError="true" Display="dynamic" Operator="dataTypeCheck" Type="Integer" ErrorMessage="Invalid Number"></asp:CompareValidator>
			<a href="#" tabindex="1" onClick="rfqslook(); return false"><asp:Image ID="RfqsLook" runat="server" ImageUrl="images/lookup_icon.gif" /></a></td>
          	   
          	   <td nowrap id="customerid" runat="server">Customer#
                        <%--<asp:Label ID="customerlabel" runat="server" Text="Customer#"></asp:Label>--%>  <br>
            <asp:TextBox ID="txt_customer" OnTextChanged="customerid_TextChanged" runat="server" Width="65px" ></asp:TextBox>
			<a href="#" tabindex="1" onClick="customerlook(); return false"><asp:Image ID="CustomerLook" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
                	</td>
                	
           		     
            		<td nowrap>Customer Part# <br>
            <asp:TextBox ID="txt_custpart" runat="server" Width="95px" MaxLength="15" ></asp:TextBox>
			<a href="#" tabindex="1" onClick="custpartlook(); return false"><asp:Image ID="CustPartLookup" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
                	</td>         
            		<td nowrap> Part Description <br>
            <asp:TextBox ID="txt_partdescription" runat="server" Width="95px" MaxLength="30" ></asp:TextBox>
			<%--<a href="#" onClick="rfgpartdescriptionlook(); return false"><asp:Image ID="RfgPartDescriptionLookup" runat="server" ImageUrl="images/lookup_icon.gif" /></a>--%>
                	</td> 
                	
                	<td nowrap> Style <br>
            <asp:TextBox ID="txt_style" runat="server" Width="65px" ></asp:TextBox>
			<%--<a href="#" onClick="stylelook(); return false"><asp:Image ID="StyleLook" runat="server" ImageUrl="images/lookup_icon.gif" /></a>--%>
                	</td> 
                	<td nowrap> Estimate <br>
            <asp:TextBox ID="txt_estimate" runat="server" Width="65px" ></asp:TextBox>
            <asp:CompareValidator ID="CompareValidator5" runat="server" ControlToValidate="txt_estimate" SetFocusOnError="true" Display="dynamic" Operator="dataTypeCheck" Type="Integer" ErrorMessage="Invalid Estimate"></asp:CompareValidator>
			<a href="#" tabindex="1" onClick="estimatelook(); return false"><asp:Image ID="EstimateLookup" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
                	</td>
                	 <td nowrap> LxWxD <br>
            <asp:TextBox ID="txt_l" runat="server" Width="65px" MaxLength="2" onblur="limit()" onfocus="blank()" ></asp:TextBox>
                         <asp:CompareValidator ID="CompareValidator1" runat="server" ControlToValidate="txt_l" SetFocusOnError="true" Display="dynamic" Operator="dataTypeCheck" Type="Double" ErrorMessage="Not a valid Number"></asp:CompareValidator>
            <br />
            <asp:TextBox ID="txt_w" runat="server" Width="65px" MaxLength="2" onblur="limit1()" onfocus="blank1()" ></asp:TextBox>
            <asp:CompareValidator ID="CompareValidator2" runat="server" ControlToValidate="txt_w" SetFocusOnError="true" Display="dynamic" Operator="dataTypeCheck" Type="Double" ErrorMessage="Not a valid Number"></asp:CompareValidator>
            <br />
            <asp:TextBox ID="txt_d" runat="server" Width="65px" MaxLength="2" onblur="limit2()" onfocus="blank2()" ></asp:TextBox>
			   	<asp:CompareValidator ID="CompareValidator3" runat="server" ControlToValidate="txt_d" SetFocusOnError="true" Display="dynamic" Operator="dataTypeCheck" Type="Double" ErrorMessage="Not a valid Number"></asp:CompareValidator>
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
        <asp:CommandField ButtonType="Image" SelectImageUrl="~/Images/sel.gif" SelectText=""
                    ShowSelectButton="True">
                    <ItemStyle Width="10px" />
                </asp:CommandField>
                
            <asp:BoundField DataField="vRfqNo" HeaderText="RFQ#" SortExpression="vRfqNo" >
                <ItemStyle HorizontalAlign="Right" Wrap="False" />
            </asp:BoundField>
            <asp:TemplateField HeaderText="Requested Date" SortExpression="ReqDt">
                <EditItemTemplate>
                    <asp:TextBox ID="TextBox1" runat="server" Text='<%# Bind("ReqDt") %>'></asp:TextBox>
                </EditItemTemplate>
                <ItemTemplate>
                    <asp:Label ID="Label1" runat="server" Text='<%# Bind("ReqDt","{0:MM/dd/yyyy}") %>'></asp:Label>
                </ItemTemplate>
                <ItemStyle Wrap="False" />
            </asp:TemplateField>
            <asp:TemplateField HeaderText="Qty" SortExpression="Qty">
                <EditItemTemplate>
                    <asp:TextBox ID="TextBox2" runat="server" Text='<%# Bind("Qty") %>'></asp:TextBox>
                </EditItemTemplate>
                <ItemTemplate>
                    <asp:Label ID="Label2" runat="server" Text='<%# Bind("Qty","{0:###,###,###}") %>'></asp:Label>
                </ItemTemplate>
                <ItemStyle HorizontalAlign="Right" Wrap="False" />
            </asp:TemplateField>
            <asp:BoundField DataField="prmCust" HeaderText="Cust#" SortExpression="prmCust" >
                <ItemStyle Wrap="False" />
            </asp:BoundField>
            <asp:BoundField DataField="prmPartno" HeaderText="Cust Part#" SortExpression="prmPartno" >
                <ItemStyle Wrap="False" />
            </asp:BoundField>
            <asp:BoundField DataField="vStyle" HeaderText="Style" SortExpression="vStyle" >
                <ItemStyle Wrap="False" />
            </asp:BoundField>
            <asp:TemplateField HeaderText="Length" SortExpression="vLength">
                <EditItemTemplate>
                    <asp:TextBox ID="TextBox3" runat="server" Text='<%# Bind("vLength") %>'></asp:TextBox>
                </EditItemTemplate>
                <ItemTemplate>
                    <asp:Label ID="Label3" runat="server" Text='<%# Bind("vLength","{0:###,###.00}") %>'></asp:Label>
                </ItemTemplate>
                <ItemStyle HorizontalAlign="Right" Wrap="False" />
            </asp:TemplateField>
            <asp:TemplateField HeaderText="Width" SortExpression="vWidth">
                <EditItemTemplate>
                    <asp:TextBox ID="TextBox4" runat="server" Text='<%# Bind("vWidth") %>'></asp:TextBox>
                </EditItemTemplate>
                <ItemTemplate>
                    <asp:Label ID="Label4" runat="server" Text='<%# Bind("vWidth","{0:###,###.00}") %>'></asp:Label>
                </ItemTemplate>
                <ItemStyle HorizontalAlign="Right" Wrap="False" />
            </asp:TemplateField>
            <asp:TemplateField HeaderText="Depth" SortExpression="vDepth">
                <EditItemTemplate>
                    <asp:TextBox ID="TextBox6" runat="server" Text='<%# Bind("vDepth") %>'></asp:TextBox>
                </EditItemTemplate>
                <ItemTemplate>
                    <asp:Label ID="Label6" runat="server" Text='<%# Bind("vDepth","{0:###,###.00}") %>'></asp:Label>
                </ItemTemplate>
                <ItemStyle HorizontalAlign="Right" Wrap="False" />
            </asp:TemplateField>
            <asp:BoundField DataField="vPartDscr" HeaderText="Item Description" SortExpression="vPartDscr" >
                <ItemStyle Wrap="False" />
            </asp:BoundField>
            <asp:BoundField DataField="Board" HeaderText="Board" SortExpression="Board" >
                <ItemStyle Wrap="False" />
            </asp:BoundField>
            <asp:TemplateField HeaderText="Caliper" SortExpression="vCal">
                <EditItemTemplate>
                    <asp:TextBox ID="TextBox5" runat="server" Text='<%# Bind("vCal") %>'></asp:TextBox>
                </EditItemTemplate>
                <ItemTemplate>
                    <asp:Label ID="Label5" runat="server" Text='<%# Bind("vCal","{0:###,##0.00000}") %>'></asp:Label>
                </ItemTemplate>
                <ItemStyle HorizontalAlign="Right" Wrap="False" />
            </asp:TemplateField>
            <asp:BoundField DataField="iCol" HeaderText="Colors" SortExpression="iCol" >
                <ItemStyle HorizontalAlign="Right" Wrap="False" />
            </asp:BoundField>
            <asp:BoundField DataField="iCoat" HeaderText="Coating" SortExpression="iCoat" >
                <ItemStyle HorizontalAlign="Right" Wrap="False" />
            </asp:BoundField>
            <asp:BoundField DataField="Procat" HeaderText="Category" SortExpression="Procat" >
                <ItemStyle Wrap="False" />
            </asp:BoundField>
            <asp:BoundField DataField="prmEst" HeaderText="Estimate#" SortExpression="prmEst" >
                <ItemStyle HorizontalAlign="Right" Wrap="False" />
            </asp:BoundField>
            <asp:TemplateField HeaderText="SeqNo" SortExpression="SeqNo" Visible="False">
                <EditItemTemplate>
                    <asp:TextBox ID="TextBox7" runat="server" Text='<%# Bind("SeqNo") %>'></asp:TextBox>
                </EditItemTemplate>
                <ItemTemplate>
                    <asp:Label ID="Label_seqno" runat="server" Text='<%# Bind("SeqNo") %>'></asp:Label>
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
        SelectMethod="ListRfqs" TypeName="rfqs">
        <SelectParameters>
            <asp:Parameter Name="prmComp" Type="String" />
            <asp:SessionParameter Name="prmCust" SessionField="rfqCust" Type="String" />
            <asp:SessionParameter Name="prmUser" SessionField="prmUser" Type="String" />
            <asp:SessionParameter Name="prmAction" DefaultValue="Select" SessionField="prmAction" Type="String" />
            <asp:SessionParameter Name="vRfqNo" SessionField="RfqNo" Type="Int32" />
            <asp:SessionParameter Name="prmPartno" SessionField="rfqPartno" Type="String" />
            <asp:SessionParameter Name="vPartDscr" SessionField="rfqPartDscr" Type="String" />
            <asp:SessionParameter Name="vStyle" SessionField="rfqStyle" Type="String" />
            <asp:SessionParameter Name="prmEst" SessionField="rfqEst" Type="String" />
            <asp:SessionParameter Name="vLength" SessionField="rfqLength" Type="String" />
            <asp:SessionParameter Name="vWidth" SessionField="rfqWidth" Type="String" />
            <asp:SessionParameter Name="vDepth" SessionField="rfqDepth" Type="String" />
        </SelectParameters>
    </asp:ObjectDataSource>
</div>
</asp:Content>
