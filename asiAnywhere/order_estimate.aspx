<%@ Page Language="C#" MasterPageFile="~/MasterPagestimate.master" Debug="false" EnableEventValidation="false" Inherits="Order_estimate" Title="Order Entry Estimate" Codebehind="~/order_estimate.aspx.cs" %>
<asp:Content ID="Content1" ContentPlaceHolderID="ContentPlaceHolder1" Runat="Server">
 

<script type="text/javascript">
    window.onload = setfocus;
    function setfocus() {
        document.forms[0].ctl00$ContentPlaceHolder1$ddl_order.focus();
    }
    
function orderlook(){ 
    var cust=document.getElementById("ctl00_ContentPlaceHolder1_txt_customer").value;
  var NewWindow = window.open("order_lookup.aspx?customer="+cust+"","OrderLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}
function OrderLookup(ReturnObj1){
    document.forms[0].ctl00$ContentPlaceHolder1$ddl_order.value = ReturnObj1;
    document.forms[0].ctl00$ContentPlaceHolder1$ddl_order.focus();
}

function customerlook(){ 
  var NewWindow = window.open("customer_lookup.aspx","CustomerLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}

function CustomerLookup(ReturnObj1){
    document.forms[0].ctl00$ContentPlaceHolder1$txt_customer.value = ReturnObj1;
    document.forms[0].ctl00$ContentPlaceHolder1$txt_customer.focus();
}

function fglook(){
var cust=document.getElementById("ctl00_ContentPlaceHolder1_txt_customer").value;
//if(cust=="")
//{
//    var cust=document.getElementById("ctl00_ContentPlaceHolder1_HiddenField1").value;    
//}

  var NewWindow = window.open("fgitem2_lookup.aspx?customer="+cust+"","FGLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}

function FGLookup(ReturnObj1){
    document.forms[0].ctl00$ContentPlaceHolder1$txt_fgitem.value = ReturnObj1;
    document.forms[0].ctl00$ContentPlaceHolder1$txt_fgitem.focus();
}

function custpartlook(){ 
    var cust=document.getElementById("ctl00_ContentPlaceHolder1_txt_customer").value;
////    if(cust=="")
////    {
////        var cust=document.getElementById("ctl00_ContentPlaceHolder1_HiddenField1").value;    
////    }
  var NewWindow = window.open("custpart2_lookup.aspx?customer="+cust+"","CustPartLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}

function CustPartLookup(ReturnObj1){
    document.forms[0].ctl00$ContentPlaceHolder1$txt_custpart.value = ReturnObj1;
    document.forms[0].ctl00$ContentPlaceHolder1$txt_custpart.focus();
}


function customerpolook(){ 
    var cust=document.getElementById("ctl00_ContentPlaceHolder1_txt_customer").value;
//    if(cust=="")
//    {
//        var cust=document.getElementById("ctl00_ContentPlaceHolder1_HiddenField1").value;    
//    }
  var NewWindow = window.open("customerpo_entry_lookup.aspx?customer="+cust+"","EstimateLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}

function CustomerPOLookup(ReturnObj1){
    document.forms[0].ctl00$ContentPlaceHolder1$txt_customerpo.value = ReturnObj1;
    document.forms[0].ctl00$ContentPlaceHolder1$txt_customerpo.focus();
}

function estimatelook(){ 
    var cust=document.getElementById("ctl00_ContentPlaceHolder1_txt_customer").value;
////    if(cust=="")
////    {
////        var cust=document.getElementById("ctl00_ContentPlaceHolder1_HiddenField1").value;    
////    }
  var NewWindow = window.open("estimate2_lookup.aspx?customer="+cust+"","EstimateLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}
function EstimateLookup(ReturnObj1){
    document.forms[0].ctl00$ContentPlaceHolder1$txt_estimate.value = ReturnObj1;
    document.forms[0].ctl00$ContentPlaceHolder1$txt_estimate.focus();
}


function job1look(){ 
  var NewWindow = window.open("job1_lookup.aspx","Job1LookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}
function Job1Lookup(ReturnObj1){
    document.forms[0].ctl00$ContentPlaceHolder1$txt_job1.value = ReturnObj1;
    document.forms[0].ctl00$ContentPlaceHolder1$txt_job1.focus();
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
function quotelook()
{ 
 var cust=document.getElementById("ctl00_ContentPlaceHolder1_txt_customer").value;
  var NewWindow = window.open("quote_order_lookup.aspx?customer="+ cust+"","ordestimateLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}
function quoteLookup(ReturnObj1)
{

    document.forms[0].ctl00$ContentPlaceHolder1$txt_quote.value = ReturnObj1;
    document.forms[0].ctl00$ContentPlaceHolder1$txt_quote.focus();
  }
  
 function select_col()
 {
    var NewWindow = window.open("show_avail_order_entry.aspx","SelectColumnWindow","width=500,height=600,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
 }

</script>

   <div>
       <asp:HiddenField ID="HiddenField1" runat="server" /> 
          
    
    <TABLE id="tblMain" cellSpacing="1" cellPadding="1" align='center' width='100%' border="0">
        <TR>
          <TD>
             <TABLE id="tblSearch" cellSpacing="1" cellPadding="5" width="100%" border="0"  bgcolor=black>
              <TR>
                <TD class="shade" align="left"><br>
                  <table cellspacing="2" cellpadding="1" border="0" class="shade" bgcolor="gray">    		   
		   <tr>
        		<td nowrap> <asp:Button ID="btnSearch" runat="server" Text="Go" OnClick="btnSearch_Click" CssClass="button" Width="40px" /><br />
                    <br />
                    <asp:Button ID="btn_reset" runat="server" CssClass="button" Text="All" OnClick="btn_reset_Click" Width="40px" />
                                        
                </td>
                <td>&nbsp;</td>
               <td nowrap="nowrap" style="width: 97px">
                   Order# <br>           
			<asp:TextBox ID="ddl_order" runat="server" Width="65px" ></asp:TextBox>
			<asp:CompareValidator ID="CompareValidator1" runat="server" ErrorMessage="Only Numbers" ControlToValidate="ddl_order" Display="dynamic" SetFocusOnError="true" Operator="datatypecheck" Type="integer"></asp:CompareValidator>
			<a href="#" tabindex="1" onClick="orderlook(); return false"><asp:Image ID="OrderLook" runat="server" ImageUrl="images/lookup_icon.gif" /></a></td>
          	   
           		    <td nowrap> FG Item# <br>
            <asp:TextBox ID="txt_fgitem" runat="server" MaxLength="15" Width="95px" ></asp:TextBox>
			<a href="#" tabindex="1" onClick="fglook(); return false"><asp:Image ID="FGLookup" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
                	</td>  
            		<td nowrap>Cust Part# <br>
            <asp:TextBox ID="txt_custpart" runat="server" MaxLength="15" Width="95px" ></asp:TextBox>
			<a href="#" tabindex="1" onClick="custpartlook(); return false"><asp:Image ID="CustPartLookup" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
                	</td>         
            		<td nowrap> Customer PO# <br>
            <asp:TextBox ID="txt_customerpo" runat="server" MaxLength="15" Width="95px"></asp:TextBox>
			<a href="#" tabindex="1" onClick="customerpolook(); return false"><asp:Image ID="CustomerPoLookup" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
                	</td>
                	<td nowrap> Estimate# <br>
            <asp:TextBox ID="txt_estimate" runat="server" Width="65px" ></asp:TextBox>
			<a href="#" tabindex="1" onClick="estimatelook(); return false"><asp:Image ID="EstimateLookup" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
                	</td>
                	<td nowrap> Quote# <br>
            <asp:TextBox ID="txt_quote" runat="server" Width="65px" ></asp:TextBox>
			<a href="#" tabindex="1" onClick="quotelook(); return false"><asp:Image ID="Image1" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
                	</td>
                	<td nowrap> Job# <br>
            <asp:TextBox ID="txt_job1" runat="server" Width="65px" ></asp:TextBox>
              <asp:TextBox ID="txt_job2" MaxLength="2"  runat="server" Width="15px" ></asp:TextBox>
              <asp:RegularExpressionValidator ID="RegularExpressionValidator1" runat="server" ErrorMessage="only numbers"
                         ControlToValidate="txt_job2" Display="Dynamic" SetFocusOnError="true" ValidationExpression="\d{2}"></asp:RegularExpressionValidator>
			<a href="#" tabindex="1" onClick="job1look(); return false"><asp:Image ID="Job1Lookup" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
                	</td>           
                	<td nowrap id="customerid" runat="server">Customer#
                        <%--<asp:Label ID="customerlabel" runat="server" Text="Customer#"></asp:Label>OnTextChanged="customerval" AutoPostBack="true" --%>  <br>
            <asp:TextBox ID="txt_customer" runat="server" Width="65px" ></asp:TextBox>
			<a href="#" tabindex="1" onClick="customerlook(); return false"><asp:Image ID="CustomerLook" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
                	</td> 
                	<td nowrap> Status: <br>
                    	<asp:DropDownList ID="ddl_po_status" AutoPostBack="true" Width="55px" runat="server" AppendDataBoundItems="True" OnSelectedIndexChanged="ddl_po_status_SelectedIndexChanged" >
                    	
                    	<asp:ListItem value="Open">Open</asp:ListItem>
                    	<asp:ListItem value="Any">Any</asp:ListItem>
                    	<asp:ListItem value="Pending">Pending</asp:ListItem>
                    	<asp:ListItem value="Closed">Closed</asp:ListItem>
               		</asp:DropDownList></td>               
               		
               	                 
              
                  
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
                             
                              <asp:TextBox ID="aLineLabel" runat="server" Width="40px" OnTextChanged="ddl_display_TextChanged" Text='<%# Bind("aLine") %>'></asp:TextBox>
                              <asp:CompareValidator ID="CompareValidator1" runat="server" ErrorMessage="Invalid Input" ControlToValidate="aLineLabel" Display="dynamic" SetFocusOnError="true" Operator="datatypecheck" Type="integer"></asp:CompareValidator>
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
                 <%-- <tr>
                    <td colspan="3">
                        <input type="button" ID="btn_move_col" runat="server" Class="button" value="Show/Hide Col." onclick="select_col()" />
                    </td>
                  </tr>--%>
                  </table>
                 
         
       </table>
            
       
       </td>
      </tr>    
        
      
        <tr width="100%">
        <td>
            <asp:GridView ID="GridView2" runat="server" OnRowCreated="GridView2_RowCreated" AllowPaging="true" OnPageIndexChanging="GridView2_PageIndexChanging" 
            AllowSorting="True" OnSorting="GridView2_Sorting" OnSelectedIndexChanged="GridView2_SelectedIndexChanged" OnRowDataBound="GridView2_RowDataBound"
            EmptyDataText="No Records Found" Width="100%" BorderStyle="Dotted" CssClass="Grid" >
            <SelectedRowStyle CssClass="GridSelected" BackColor="Yellow" Wrap="false" />
            <AlternatingRowStyle CssClass="GridItemOdd" Wrap="False" />            
            <EmptyDataRowStyle BorderStyle="Dotted" BorderColor="Gray" BorderWidth="0px" Font-Bold="True" HorizontalAlign="Center" VerticalAlign="Middle" />
            <HeaderStyle   ForeColor="White" CssClass="headcolor" Height="40px"  VerticalAlign="Middle"  HorizontalAlign="Center" Wrap="False"></HeaderStyle>
        <RowStyle CssClass="shade" Wrap="False"  />
        <Columns>
            <asp:CommandField ShowSelectButton="True" ButtonType="Image" SelectImageUrl="~/Images/sel.gif" SelectText="" > 
                    <ItemStyle Width="10px" />
            </asp:CommandField>
                 <asp:TemplateField HeaderText="Select" Visible="False" >
               <ItemStyle HorizontalAlign="Center" />
               <ItemTemplate>
                   
                   <asp:Label ID="value" runat="server" Text='<%# Bind("line") %>'></asp:Label>
                   <asp:Label ID="lbl_rec_key" runat="server" Text='<%# Bind("rec_key") %>'></asp:Label>
                   <input type="radio" checked="checked" name="radio1" value='<%# Server.UrlEncode(Convert.ToString(DataBinder.Eval(Container,"DataItem.Order"))) %>,<%# Server.UrlEncode(Convert.ToString(DataBinder.Eval(Container,"DataItem.line"))) %>,<%# Server.UrlEncode(Convert.ToString(DataBinder.Eval(Container,"DataItem.FG Item"))) %>' />
               </ItemTemplate>
                   <ControlStyle Width="28px" />
                   <HeaderStyle BackColor="Gray" ForeColor="White" />
           
               </asp:TemplateField>
                
                <asp:TemplateField HeaderText="Orderno" Visible="False" >                    
                    <ItemTemplate>
                        <asp:Label ID="order_label" runat="server" Text='<%# Bind("[Order]") %>'></asp:Label>
                    </ItemTemplate>
                    <ItemStyle Wrap="False" />
                </asp:TemplateField>
                <asp:TemplateField HeaderText="Customerno" Visible="False">                    
                    <ItemTemplate>
                        <asp:Label ID="customer_label" runat="server" Text='<%# Bind("[Customer]") %>'></asp:Label>
                    </ItemTemplate>
                    <ItemStyle Wrap="False" />
                </asp:TemplateField>
                
                <asp:TemplateField HeaderText="CustomerName" Visible="False">
                <ItemTemplate>
                <asp:Label ID="CustLabel" runat="server" Text='<%# Bind("[Customer Name]") %>'></asp:Label>
                </ItemTemplate>
                <ItemStyle Wrap="false" />
                </asp:TemplateField>
                <asp:TemplateField HeaderText="DueDate" Visible="False" >                    
                    <ItemTemplate>
                        <asp:Label ID="Label3" runat="server" Text='<%# Bind("[Due Date]","{0:MM/dd/yyyy}") %>'></asp:Label>
                    </ItemTemplate>
                    <ItemStyle Wrap="False" />
                </asp:TemplateField>
                
                <asp:TemplateField HeaderText="FGItemno" Visible="False" >                    
                    <ItemTemplate>
                        <asp:Label ID="Label11" runat="server" Text='<%# Bind("[FG Item]") %>'></asp:Label>
                    </ItemTemplate>
                    <ItemStyle Wrap="False" />
                </asp:TemplateField>
                               
                <asp:TemplateField HeaderText="CustomerPo" Visible="False" >                    
                    <ItemTemplate>
                        <asp:Label ID="cust_po_label" runat="server" Text='<%# Bind("[Customer Po#]") %>'></asp:Label>
                    </ItemTemplate>
                    <ItemStyle Wrap="False" />
                </asp:TemplateField>
                <asp:TemplateField HeaderText="Estno" Visible="False" >                    
                    <ItemTemplate>
                        <asp:Label ID="est_label" runat="server" Text='<%# Bind("[Est#]") %>'></asp:Label>
                    </ItemTemplate>
                    <ItemStyle Wrap="False" />
                </asp:TemplateField>
                               
                
                <asp:TemplateField HeaderText="Jobno#" Visible="False" >                    
                    <ItemTemplate>
                        <asp:Label ID="job_label" runat="server" Text='<%# Bind("[Job#]") %>'></asp:Label>
                    </ItemTemplate>
                    <ItemStyle Wrap="False" />
                </asp:TemplateField>
                
                <asp:TemplateField HeaderText="Jobno" Visible="False">                    
                    <ItemTemplate>
                        <asp:Label ID="job2_label" runat="server" Text='<%# Bind("[Job]") %>'></asp:Label>
                    </ItemTemplate>
                    <ItemStyle Wrap="False" />
                </asp:TemplateField>
                
                
                <asp:TemplateField HeaderText="OrderDate" Visible="False">                    
                    <ItemTemplate>
                        <asp:Label ID="vOrdDateLabel" runat="server" Text='<%# Bind("[Order Date]","{0:MM/dd/yyyy}") %>'></asp:Label>
                    </ItemTemplate>
                    <ItemStyle Wrap="False" />
                </asp:TemplateField>
                
                <asp:TemplateField HeaderText="Statusno" Visible="False">                    
                    <ItemTemplate>
                        <asp:Label ID="status_label" runat="server" Text='<%# Bind("[Status]") %>'></asp:Label>
                    </ItemTemplate>
                    <ItemStyle Wrap="False" />
                </asp:TemplateField>
                
                <asp:TemplateField  HeaderText="ReqDate" Visible="False" >
                    
                    <ItemTemplate>
                        <asp:Label ID="ReqDateLabel" runat="server" Text='<%# Bind("[Due Date]","{0:MM/dd/yyy}") %>'></asp:Label>
                    </ItemTemplate>
                    <ItemStyle Wrap="False" HorizontalAlign="right" />
                </asp:TemplateField>
                
               <asp:TemplateField  HeaderText="Req-code" Visible="False" >
                    
                    <ItemTemplate>
                        <asp:Label ID="ReqcodeLabel" runat="server" Text='<%# Bind("[req-code]") %>'></asp:Label>
                    </ItemTemplate>
                    <ItemStyle Wrap="False" HorizontalAlign="right" />
                </asp:TemplateField>
                
               <asp:TemplateField  HeaderText="PromCode" Visible="False">
                    
                    <ItemTemplate>
                        <asp:Label ID="PromCodeLabel" runat="server" Text='<%# Bind("[prom-code]") %>'></asp:Label>
                    </ItemTemplate>
                    <ItemStyle Wrap="False" HorizontalAlign="right" />
                </asp:TemplateField>
                
                 <asp:TemplateField  HeaderText="PromDate" Visible="False">                    
                    <ItemTemplate>
                        <asp:Label ID="PromDateLabel" runat="server" Text='<%# Bind("[prom-date]","{0:MM/dd/yyy}") %>'></asp:Label>
                    </ItemTemplate>
                    <ItemStyle Wrap="False" HorizontalAlign="right" />
                </asp:TemplateField>
                
                <asp:TemplateField  HeaderText="Over" Visible="False" >                    
                    <ItemTemplate>
                        <asp:Label ID="OverLabel" runat="server" Text='<%# Bind("[over-pct]") %>'></asp:Label>
                    </ItemTemplate>
                    <ItemStyle Wrap="False" HorizontalAlign="right" />
                </asp:TemplateField>
                
                <asp:TemplateField  HeaderText="Under" Visible="False">                    
                    <ItemTemplate>
                        <asp:Label ID="UnderLabel" runat="server" Text='<%# Bind("[under-pct]") %>'></asp:Label>
                    </ItemTemplate>
                    <ItemStyle Wrap="False" HorizontalAlign="right" />
                </asp:TemplateField>
                
                 <asp:TemplateField  HeaderText="Sman" Visible="False">                    
                    <ItemTemplate>
                        <asp:Label ID="sman1Label" runat="server" Text='<%# Bind("[s-man1]") %>'></asp:Label>
                    </ItemTemplate>
                    <ItemStyle Wrap="False" HorizontalAlign="right" />
                </asp:TemplateField>
                
                <asp:TemplateField  HeaderText="Sman2" Visible="False">                    
                    <ItemTemplate>
                        <asp:Label ID="sman2Label" runat="server" Text='<%# Bind("[s-man2]") %>'></asp:Label>
                    </ItemTemplate>
                    <ItemStyle Wrap="False" HorizontalAlign="right" />
                </asp:TemplateField>
                
                 <asp:TemplateField  HeaderText="Sman3" Visible="False">                    
                    <ItemTemplate>
                        <asp:Label ID="sman3Label" runat="server" Text='<%# Bind("[s-man3]") %>'></asp:Label>
                    </ItemTemplate>
                    <ItemStyle Wrap="False" HorizontalAlign="right" />
                </asp:TemplateField>
                
                  <asp:TemplateField  HeaderText="Spct" Visible="False">                    
                    <ItemTemplate>
                        <asp:Label ID="spct1Label" runat="server" Text='<%# Bind("[s-pct1]") %>'></asp:Label>
                    </ItemTemplate>
                    <ItemStyle Wrap="False" HorizontalAlign="right" />
                </asp:TemplateField>
                
                 <asp:TemplateField  HeaderText="Spct2" Visible="False">                    
                    <ItemTemplate>
                        <asp:Label ID="spct2Label" runat="server" Text='<%# Bind("[s-pct2]") %>'></asp:Label>
                    </ItemTemplate>
                    <ItemStyle Wrap="False" HorizontalAlign="right" />
                </asp:TemplateField>
                
                 <asp:TemplateField  HeaderText="Spct3" Visible="False">                    
                    <ItemTemplate>
                        <asp:Label ID="spct3Label" runat="server" Text='<%# Bind("[s-pct3]") %>'></asp:Label>
                    </ItemTemplate>
                    <ItemStyle Wrap="False" HorizontalAlign="right" />
                </asp:TemplateField>
                
                 <asp:TemplateField  HeaderText="Scomm" Visible="False" >                    
                    <ItemTemplate>
                        <asp:Label ID="scommLabel" runat="server" Text='<%# Bind("[s-comm1]") %>'></asp:Label>
                    </ItemTemplate>
                    <ItemStyle Wrap="False" HorizontalAlign="right" />
                </asp:TemplateField>
                
                <asp:TemplateField  HeaderText="Scomm2" Visible="False" >                    
                    <ItemTemplate>
                        <asp:Label ID="scomm2Label" runat="server" Text='<%# Bind("[s-comm2]") %>'></asp:Label>
                    </ItemTemplate>
                    <ItemStyle Wrap="False" HorizontalAlign="right" />
                </asp:TemplateField>
                <asp:TemplateField  HeaderText="Scomm3" Visible="False" >                    
                    <ItemTemplate>
                        <asp:Label ID="scomm3Label" runat="server" Text='<%# Bind("[s-comm3]") %>'></asp:Label>
                    </ItemTemplate>
                    <ItemStyle Wrap="False" HorizontalAlign="right" />
                </asp:TemplateField>
                
                <asp:TemplateField  HeaderText="Sname1" Visible="False" >                    
                    <ItemTemplate>
                        <asp:Label ID="sname1Label" runat="server" Text='<%# Bind("sname1") %>'></asp:Label>
                    </ItemTemplate>
                    <ItemStyle Wrap="False" HorizontalAlign="right" />
                </asp:TemplateField>
                
                <asp:TemplateField  HeaderText="Sname2" Visible="False">                    
                    <ItemTemplate>
                        <asp:Label ID="sname2Label" runat="server" Text='<%# Bind("sname2") %>'></asp:Label>
                    </ItemTemplate>
                    <ItemStyle Wrap="False" HorizontalAlign="right" />
                </asp:TemplateField>
                <asp:TemplateField  HeaderText="Sname3" Visible="False">                    
                    <ItemTemplate>
                        <asp:Label ID="sname3Label" runat="server" Text='<%# Bind("sname3") %>'></asp:Label>
                    </ItemTemplate>
                    <ItemStyle Wrap="False" HorizontalAlign="right" />
                </asp:TemplateField>
                <asp:TemplateField   HeaderText="Type" Visible="False" >                    
                    <ItemTemplate>
                        <asp:Label ID="typeLabel" runat="server" Text='<%# Bind("[type-code]") %>'></asp:Label>
                    </ItemTemplate>
                    <ItemStyle Wrap="False" HorizontalAlign="right" />
                </asp:TemplateField>
                
               <asp:TemplateField  HeaderText="Tax" Visible="False" >                    
                    <ItemTemplate>
                        <asp:Label ID="taxLabel" runat="server" Text='<%# Bind("tax") %>'></asp:Label>
                    </ItemTemplate>
                    <ItemStyle Wrap="False" HorizontalAlign="right" />
                </asp:TemplateField>
                
                <asp:TemplateField HeaderText="Quoteno" Visible="false" >                    
                    <ItemTemplate>
                        <asp:Label ID="Quote_Label" runat="server" Text='<%# Bind("[Quote#]") %>'></asp:Label>
                    </ItemTemplate>
                    <ItemStyle Wrap="False" />
                </asp:TemplateField>
                <asp:TemplateField HeaderText="OrderQuote" Visible="false" >                    
                    <ItemTemplate>
                        <asp:Label ID="order_quote_Label" runat="server" Text='<%# Bind("[Quote#]") %>'></asp:Label>
                    </ItemTemplate>
                    <ItemStyle Wrap="False" />
                </asp:TemplateField>        
                 
        </Columns>
            </asp:GridView> </td>
            </tr>
            
            
        </table>    
    </div>   
          

</asp:Content>