<%@ Page Language="C#" MasterPageFile="~/MasterPage2.master" AutoEventWireup="true" Inherits="brwsjobs" Title="Brows Jobs" Codebehind="brwsjobs.aspx.cs" %>
<asp:Content ID="Content1" ContentPlaceHolderID="ContentPlaceHolder1" Runat="Server">
<script>
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

function orderlook(){ 
  var NewWindow = window.open("order_lookup.aspx","OrderLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}
function OrderLookup(ReturnObj1){ 
  document.forms[0].ctl00$ContentPlaceHolder1$ddl_order.value = ReturnObj1;
}

function customerlook(){ 
  var NewWindow = window.open("customer_lookup.aspx","CustomerLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}

function CustomerLookup(ReturnObj1){ 
  document.forms[0].ctl00$ContentPlaceHolder1$txt_customer.value = ReturnObj1;
}

function fglook(){ 
  var NewWindow = window.open("fgitem_lookup.aspx","FGLookupWindow","width=450,height=300,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}

function FGLookup(ReturnObj1){ 
  document.forms[0].ctl00$ContentPlaceHolder1$txt_fgitem.value = ReturnObj1;
}


function estimatelook(){ 
  var NewWindow = window.open("estimate_lookup.aspx","EstimateLookupWindow","width=450,height=300,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}
function EstimateLookup(ReturnObj1){ 
  document.forms[0].ctl00$ContentPlaceHolder1$txt_estimate.value = ReturnObj1;
}


function job1look(){ 
  var NewWindow = window.open("job1_lookup.aspx","Job1LookupWindow","width=450,height=300,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}
function Job1Lookup(ReturnObj1){ 
  document.forms[0].ctl00$ContentPlaceHolder1$txt_job1.value = ReturnObj1;
}
</script>
<div>
<br />
    <asp:LinkButton OnClick="LinkButton1_Click" ID="LinkButton1" runat="server">List Order</asp:LinkButton>
     <asp:LinkButton OnClick="LinkButton2_Click" ID="LinkButton2" runat="server">List Order</asp:LinkButton>    
        
       <TABLE id="tblSearch" cellSpacing="1" cellPadding="5" width="100%" border="0"  bgcolor=black>
              <TR>
                <TD class="shade" align="left"><br>
                  <table cellspacing="2" cellpadding="1"  border="0" class="shade" bgcolor="gray">    		   
		   <tr>
		   <td><asp:Button ID="btnSearch" runat="server" Text="Go" CssClass="button" OnClick="btnSearch_Click" Width="40px" /><br /><br />
			<asp:Button ID="btn_reset" runat="server" CssClass="button" Text="All" OnClick="btn_reset_Click" Width="40px" /></td>
		   <td nowrap> Job# <br>
            <asp:TextBox ID="txt_job1" runat="server" Width="65px" ></asp:TextBox>
			<a href="#" onClick="job1look(); return false"><asp:Image ID="Job1Lookup" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
                	</td>           
                	<td nowrap style="width: 20px"># <br>
            <asp:TextBox ID="txt_job2"  runat="server" Width="15px" ></asp:TextBox>
			
                	</td>
                	<td nowrap> FG Item# <br>
            <asp:TextBox ID="txt_fgitem" runat="server" Width="65px" ></asp:TextBox>
			<a href="#" onClick="fglook(); return false"><asp:Image ID="FGLookup" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
                	</td> 
                	<td nowrap> Customer# <br>
            <asp:TextBox ID="txt_customer" runat="server" Width="65px" ></asp:TextBox>
			<a href="#" onClick="customerlook(); return false"><asp:Image ID="CustomerLook" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
                	</td> 
                	<td nowrap> Estimate# <br>
            <asp:TextBox ID="txt_estimate" runat="server" Width="65px" ></asp:TextBox>
			<a href="#" onClick="estimatelook(); return false"><asp:Image ID="EstimateLookup" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
                	</td>
        		<td nowrap> Order# <br>           
			<asp:TextBox ID="ddl_order" runat="server" Width="65px" ></asp:TextBox>
			<a href="#" onClick="orderlook(); return false"><asp:Image ID="OrderLook" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
        		</td>
          	   <td nowrap style="height: 23px"><asp:CheckBox id="CheckBox1" runat="server" ></asp:CheckBox>
        <asp:Label ID="Label1" runat="server" Text="Open Jobs" Font-Bold="true"></asp:Label>
            <asp:HiddenField ID="openvalue" runat="server" />        
         <asp:CheckBox  id="CheckBox2" runat="server" ></asp:CheckBox>
         <asp:Label ID="Label2" runat="server" Text="Closed Jobs" Font-Bold="true"></asp:Label> 
             <asp:HiddenField ID="closedvalue" runat="server" />
          </td>        		     
            					
                  </tr>
                  </table></TD></TR></TABLE>
        
        
   
    <br />
    
    <asp:GridView ID="GridView1" runat="server" AutoGenerateColumns="False" DataSourceID="ObjectDataSource1" AllowPaging="True" AllowSorting="True" BorderStyle="Dotted" CssClass="Grid" EmptyDataText="No Record Found" Width="100%" OnSelectedIndexChanged="GridView1_SelectedIndexChanged">
        <Columns>
        <asp:CommandField ButtonType="Image" SelectImageUrl="~/Images/sel.gif" SelectText=""
                    ShowSelectButton="True">
                    <ItemStyle Width="10px" />
                </asp:CommandField>
            <asp:BoundField DataField="job-no" HeaderText="Job#" SortExpression="job-no" />
            <asp:BoundField DataField="job-no2" SortExpression="job-no2" />
            <asp:BoundField DataField="i-no" HeaderText="FG Item#" SortExpression="i-no" />
            <asp:BoundField DataField="est-no" HeaderText="Estimate#" SortExpression="est-no" />
            <asp:BoundField DataField="ord-no" HeaderText="Order#" SortExpression="ord-no" />
            <asp:BoundField DataField="cust-no" HeaderText="Customer#" SortExpression="cust-no" />
            <asp:TemplateField HeaderText="Start Date" SortExpression="StartDt">
                <EditItemTemplate>
                    <asp:TextBox ID="TextBox1" runat="server" Text='<%# Bind("StartDt") %>'></asp:TextBox>
                </EditItemTemplate>
                <ItemTemplate>
                    <asp:Label ID="Label1" runat="server" Text='<%# Bind("StartDt","{0:MM/dd/yyyy}") %>'></asp:Label>
                </ItemTemplate>
            </asp:TemplateField>
            <asp:TemplateField HeaderText="Close Date" SortExpression="CloseDt">
                <EditItemTemplate>
                    <asp:TextBox ID="TextBox2" runat="server" Text='<%# Bind("CloseDt") %>'></asp:TextBox>
                </EditItemTemplate>
                <ItemTemplate>
                    <asp:Label ID="Label2" runat="server" Text='<%# Bind("CloseDt","{0:MM/dd/yyyy}") %>'></asp:Label>
                </ItemTemplate>
            </asp:TemplateField>
            <asp:BoundField DataField="Statu" HeaderText="Status" SortExpression="Statu" />
            <asp:BoundField DataField="Part" HeaderText="Customer Part" SortExpression="Part" />
            <asp:BoundField DataField="JobQty" HeaderText="Job Qty" SortExpression="JobQty" />
            <asp:BoundField DataField="OrdQty" HeaderText="Ordered Qty" SortExpression="OrdQty" />
            <asp:BoundField DataField="ProdQty" HeaderText="Prod Qty" SortExpression="ProdQty" />
            <asp:BoundField DataField="OnHand" HeaderText="On Hand Qty" SortExpression="OnHand" />
            <asp:BoundField DataField="ShipQty" HeaderText="Shipped Qty" SortExpression="ShipQty" />
            <asp:BoundField DataField="InvQty" HeaderText="Invoice Qty" SortExpression="InvQty" />
            <asp:BoundField DataField="WipQty" HeaderText="WIP Qty" SortExpression="WipQty" />
            <asp:BoundField DataField="OUPct" HeaderText="O/U%" SortExpression="OUPct" />
            <asp:BoundField DataField="FgItem" HeaderText="FG Item" SortExpression="FgItem" />
            </Columns>
        <EmptyDataRowStyle BorderColor="Gray" BorderStyle="Dotted" BorderWidth="0px" Font-Bold="True"
            HorizontalAlign="Center" VerticalAlign="Middle" />
        <RowStyle CssClass="shade" />
        <SelectedRowStyle CssClass="GridSelected" BackColor="Yellow" />
        <HeaderStyle  BackColor="Teal" ForeColor="White" Height="40px"  VerticalAlign="Middle"  HorizontalAlign="Center" Wrap="False"></HeaderStyle>
            <RowStyle CssClass="shade"  />
        <AlternatingRowStyle CssClass="GridItemOdd" />
    </asp:GridView>
    <asp:ObjectDataSource ID="ObjectDataSource1" runat="server" OldValuesParameterFormatString="original_{0}"
        SelectMethod="OnHandQty" TypeName="orderonhand">
        <SelectParameters>
            <asp:Parameter Name="prmUser" Type="String" /> 
            <asp:Parameter DefaultValue="Select" Name="prmAction" Type="String" />
            <asp:SessionParameter Name="prmOrderNum" SessionField="brwsjobs_order_no" Type="String" />
            <asp:SessionParameter DefaultValue="" Name="prmItemNum" SessionField="brwsjobs_item_num" Type="String" />
            <asp:SessionParameter SessionField="brwsjobs_job_no"  Name="prmJob" Type="String" />
            <asp:SessionParameter SessionField="brwsjobs_job_no2" Name="prmJob2" Type="String" />
            <asp:SessionParameter SessionField="brwsjobs_cust_no" Name="prmCustomer" Type="String" />
            <asp:SessionParameter SessionField="brwsjobs_est_no" Name="prmEst" Type="String" />
            <asp:SessionParameter SessionField="brwsjobs_open" Name="prmOpen" Type="String" />
            <asp:SessionParameter SessionField="brwsjobs_close"  Name="prmClosed" Type="String" />
        </SelectParameters>
    </asp:ObjectDataSource>
    
</div>
</asp:Content>

