<%@ Page Language="C#" MasterPageFile="~/MasterPage6.master" AutoEventWireup="true" Inherits="BrowseQuoteaspx" Title="Browse Quote" Codebehind="BrowseQuote.aspx.cs" %>
<asp:Content ID="Content1" ContentPlaceHolderID="ContentPlaceHolder1" Runat="Server">
<script src="include/CalendarControl.js"></script>
<script language="javascript" src="include/date.js"></script>
<script language="javascript" src="include/event.js"></script>
<script language="javascript" src="include/insert.js"></script>
<script type="text/javascript">
    window.onload = setfocus;
    function setfocus() {
        document.forms[0].ctl00_ContentPlaceHolder1_txt_quote.focus();
    }
function rfqslook(){ 
  var NewWindow = window.open("rfqs_lookup.aspx","RfqsLookupWindow","width=600,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}
function rfqLookup(ReturnObj1){
    document.forms[0].ctl00$ContentPlaceHolder1$txt_rfq.value = ReturnObj1;
    document.forms[0].ctl00$ContentPlaceHolder1$txt_rfq.focus();
  
}

function customerlook(){ 
  var NewWindow = window.open("customer_lookup.aspx","CustomerLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}

function CustomerLookup(ReturnObj1){
    document.forms[0].ctl00$ContentPlaceHolder1$txt_cust.value = ReturnObj1;
    document.forms[0].ctl00$ContentPlaceHolder1$txt_cust.focus();
}

function custpartlook(){ 
  var NewWindow = window.open("custpart_lookup.aspx","CustPartLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}

function CustPartLookup(ReturnObj1){
    document.forms[0].ctl00$ContentPlaceHolder1$txt_part.value = ReturnObj1;
    document.forms[0].ctl00$ContentPlaceHolder1$txt_part.focus();
}

function estimatelook(){ 
  var NewWindow = window.open("estimate_lookup.aspx","EstimateLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}
function EstimateLookup(ReturnObj1){
    document.forms[0].ctl00$ContentPlaceHolder1$txt_est.value = ReturnObj1;
    document.forms[0].ctl00$ContentPlaceHolder1$txt_est.focus();
}

function contactlook(){ 
  var NewWindow = window.open("contactlookup.aspx","ContactLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}

function contactlookup(ReturnObj1, ReturnObj2) {
    document.forms[0].ctl00$ContentPlaceHolder1$txt_contact.value = ReturnObj1 + " " + ReturnObj2;
    document.forms[0].ctl00$ContentPlaceHolder1$txt_contact.focus();
}

function Datelook(){ 
  var NewWindow = window.open("date_lookup.aspx","DateLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
  }
function Datelookup(obj)
{
  document.forms[0].ctl00_ContentPlaceHolder1_txt_fdate.value=obj;
}
function Datelook1()
{
  document.forms[0].ctl00_ContentPlaceHolder1_txt_fdate.value="";
  Datelook();
}
function datevalidate()
{
    var date=document.getElementById("ctl00_ContentPlaceHolder1_txt_fdate").value;
    
    if(date.length>1 && date.length<3 && date.indexOf('/')!=1)
    {
        document.getElementById("ctl00_ContentPlaceHolder1_txt_fdate").value = date + "/";
    }
    if(date.length>4 && date.length<6 && date.indexOf('/')!=3)
    {
        document.getElementById("ctl00_ContentPlaceHolder1_txt_fdate").value = date + "/";
    }   
}
function clickButton(e,ctl00$ContentPlaceHolder1$btnSearch)
{
    var evt = e ? e : window.event;    
    var bt = document.getElementById("ctl00$ContentPlaceHolder1$btnSearch");
    if(bt)
    {
        if(e.keyCode == 13)
        {
            bt.click();
            return false;
        }
    }
}
</script>
<div>

<asp:Panel ID="searchpanel" runat="server" DefaultButton="btnSearch">
<TABLE id="TABLE2" cellSpacing="1"  cellPadding="5" width="500px" border="0"  bgcolor=black>
              <TR>
                <TD class="shade" align="left"><br>
                  <table cellspacing="2" cellpadding="1" width="500px" border="0" class="shade" bgcolor="gray">    		   
		   <tr>
               <td nowrap="nowrap" >
                   
                <asp:Button ID="btnSearch" runat="server" Text="Go" OnClick="btnSearch_Click" CssClass="button" Width="58px" /><br />
            <br />
            <asp:Button ID="btn_reset" runat="server" CssClass="button" Text="All" OnClick="btn_reset_Click" Width="58px"  /></td>
            <td nowrap="nowrap" style="width: 10px;">
        
        		<td nowrap> Quote <br>           
			<asp:TextBox ID="txt_quote" runat="server" Width="65px"></asp:TextBox>
			<asp:CompareValidator ID="CompareValidator4" runat="server" ControlToValidate="txt_quote" SetFocusOnError="true" Display="dynamic" Operator="dataTypeCheck" Type="Integer" ErrorMessage="Invalid Number"></asp:CompareValidator>
				</td>
          	<td nowrap> Date <br>
            <asp:TextBox ID="txt_fdate" onfocus="javascript:preEnter( this, 'yes' );" onblur="javascript:preLeave( this, 'date', '99/99/9999' );"  MaxLength="10" ToolTip="MM/DD/YYYY" runat="server" Width="65px"></asp:TextBox>
            <a href="#" tabindex="1" onblur="ctl00_ContentPlaceHolder1_txt_fdate.focus()" onClick="showCalendarControl(ctl00_ContentPlaceHolder1_txt_fdate); return false"><asp:Image ID="Image4" runat="server" ImageUrl="images/lookup_icon.gif" /></a>            
            <asp:CompareValidator ID="CompareValidator1" runat="server" ControlToValidate="txt_fdate" SetFocusOnError="true" Display="dynamic" Operator="dataTypeCheck" Type="Date" ErrorMessage="Invalid Date"></asp:CompareValidator>
                	</td>    
           		    <td nowrap> Customer <br>
            <asp:TextBox ID="txt_cust"  runat="server" Width="65px"></asp:TextBox>
            <a href="#" tabindex="1" onClick="customerlook(); return false"><asp:Image ID="CustomerLook" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
                	</td>  
            		<td nowrap>Contact <br>
            <asp:TextBox ID="txt_contact" runat="server" Width="65px"></asp:TextBox>
            <a href="#" tabindex="1" onClick="contactlook(); return false"><asp:Image ID="contactlook" runat="server" ImageUrl="images/lookup_icon.gif" />
                	</td>
                	<td nowrap >Estimate <br>
            <asp:TextBox ID="txt_est" runat="server" Width="65px"></asp:TextBox>
			<a href="#" tabindex="1" onClick="estimatelook(); return false"><asp:Image ID="Image2" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
                	</td>         
            		<td nowrap> Rfq <br>
            <asp:TextBox ID="txt_rfq" runat="server" Width="65px"></asp:TextBox>
            <a href="#" tabindex="1" onClick="rfqslook(); return false"><asp:Image ID="Image3" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
                	</td>
                	<td nowrap> Cust Part No <br>
            <asp:TextBox ID="txt_part" runat="server" onblur="setfocus()" MaxLength="20" Width="100px"></asp:TextBox>
            <a href="#" tabindex="1" onClick="custpartlook(); return false"><asp:Image ID="Image1" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
                	</td>
                <td nowrap style="height: 23px"> </TD>
            
                  
                  </tr>
                  </table>
             
       </table>
      </asp:Panel>  
    
    <%--<a href="order_inquiry.aspx"><span style="color: #0000ff; text-decoration: underline">
        List Orders</span></a><br />--%>
    <asp:GridView ID="GridView1" runat="server" AllowPaging="True" AllowSorting="True" OnSelectedIndexChanged="GridView1_SelectedIndex"
        AutoGenerateColumns="False" CssClass="Grid" DataSourceID="ObjectDataSource1"
        EmptyDataText="No Record Found"  Width="730px">
        <EmptyDataRowStyle BorderColor="Gray" BorderStyle="Dotted" BorderWidth="0px" Font-Bold="True"
            HorizontalAlign="Center" VerticalAlign="Middle" />
        <Columns>
         <asp:CommandField ButtonType="Image" SelectImageUrl="~/Images/sel.gif" SelectText=""
                    ShowSelectButton="True">
                    <ItemStyle Width="10px" />
                </asp:CommandField>
            
            <asp:BoundField DataField="vQuote" HeaderText="Quote" SortExpression="vQuote" >
                    <ItemStyle Wrap="False" />
                </asp:BoundField>   
           <%-- <asp:BoundField DataField="" HeaderText="Date" SortExpression="vDate" />--%>
            <asp:TemplateField HeaderText="Date" SortExpression="vDate">
                    <EditItemTemplate>
                        <asp:TextBox ID="TextBox1" runat="server" Text='<%# Bind("vDate") %>'></asp:TextBox>
                    </EditItemTemplate>
                    <ItemStyle Wrap="False" />
                    <ItemTemplate>
                        <asp:Label ID="Label1" runat="server" Text='<%# Bind("vDate","{0:d}") %>'></asp:Label>
                    </ItemTemplate>
                </asp:TemplateField>
                <asp:BoundField DataField="vCust" HeaderText="Cust" SortExpression="vCust" >
                    <ItemStyle Wrap="False" />
                </asp:BoundField>
            <asp:BoundField DataField="vContact" HeaderText="Contact" SortExpression="vContact" >
                    <ItemStyle Wrap="False" />
                </asp:BoundField>
           <asp:BoundField DataField="vEstimate" HeaderText="Estimate" SortExpression="vEstimate" >
                    <ItemStyle Wrap="False" />
                </asp:BoundField>
            <asp:BoundField DataField="vRfq" HeaderText="Rfq" SortExpression="vRfq" >
                    <ItemStyle Wrap="False" />
                </asp:BoundField>
           <asp:BoundField DataField="VPart" HeaderText="CustPart" SortExpression="VPart" >
                    <ItemStyle Wrap="False" />
                </asp:BoundField> 
                 <asp:TemplateField HeaderText="Qty" SortExpression="vQty">
                  
                    <ItemStyle Wrap="False" />
                    <ItemTemplate>
                        <asp:Label ID="LabelQty" runat="server" Text='<%# Bind("vQty") %>'></asp:Label>
                    </ItemTemplate>
                </asp:TemplateField>   
                <asp:TemplateField Visible="false" HeaderText="line" SortExpression="vLine">
                  
                    <ItemStyle Wrap="False" />
                    <ItemTemplate>
                        <asp:Label ID="Labeline" runat="server" Text='<%# Bind("vLine") %>'></asp:Label>
                    </ItemTemplate>
                </asp:TemplateField>         
        </Columns>
        <RowStyle CssClass="shade" />
        <SelectedRowStyle CssClass="GridSelected" BackColor="Yellow" />
        <HeaderStyle BackColor="Teal" CssClass="gridrowhdr" ForeColor="White" HorizontalAlign="Center"
            VerticalAlign="Middle" Wrap="False" />
        <AlternatingRowStyle CssClass="GridItemOdd" />
    </asp:GridView>
    <asp:ObjectDataSource ID="ObjectDataSource1" runat="server" OldValuesParameterFormatString="original_{0}"
        SelectMethod="SelectBrowsQuote" TypeName="browsquote">
        <SelectParameters>
            <asp:Parameter Name="prmUser" Type="String" />
            <asp:SessionParameter SessionField="prmAction2_quote_list" DefaultValue="Select" Name="prmAction" Type="String" />
            <%--<asp:SessionParameter DefaultValue="" Name="prmQuote" SessionField="brwsquote" Type="Int32" />--%>
            <asp:SessionParameter SessionField="prmQuote_quote_list" Name="prmQuote" Type="Int32" />
            <asp:SessionParameter SessionField="prmDate_quote_list" Name="prmDate" Type="DateTime" />
            <asp:SessionParameter SessionField="prmCustomer_quote_list" Name="prmCustomer" Type="String" />
            <asp:SessionParameter SessionField="prmContact_quote_list" Name="prmContact" Type="String" />
            <asp:SessionParameter SessionField="prmEstimate_quote_list" Name="prmEstimate" Type="String" />
            <asp:SessionParameter SessionField="prmRfq_quote_list" Name="prmRfq" Type="String" />
            <asp:SessionParameter SessionField="prmPart_quote_list" Name="prmPart" Type="String" />
        </SelectParameters>
    </asp:ObjectDataSource>


</asp:Content>

