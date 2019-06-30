<%@ Page Language="C#" MasterPageFile="~/MasterPageCorrugated.master"  Debug="true" EnableEventValidation="false" Inherits="corrugated_brow" Title="Brws Estimate" Codebehind="~/corrugated_brow.aspx.cs" %>
<asp:Content ID="Content1" ContentPlaceHolderID="ContentPlaceHolder1" Runat="Server">
 

<script type="text/javascript">
    window.onload = setfocus;
    function setfocus() {
        document.forms[0].ctl00$ContentPlaceHolder1$txt_estimate.focus();
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
    document.forms[0].ctl00$ContentPlaceHolder1$txt_cust.value = ReturnObj1;
    document.forms[0].ctl00$ContentPlaceHolder1$txt_cust.focus();
}

function fglook(){ 
  var NewWindow = window.open("fgitem_lookup.aspx","FGLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}

function FGLookup(ReturnObj1){
    document.forms[0].ctl00$ContentPlaceHolder1$txt_fgitem.value = ReturnObj1;
    document.forms[0].ctl00$ContentPlaceHolder1$txt_fgitem.focus();
}

function custpartlook(){ 
  var NewWindow = window.open("custpart_lookup.aspx","CustPartLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}

function CustPartLookup(ReturnObj1){
    document.forms[0].ctl00$ContentPlaceHolder1$txt_custpart.value = ReturnObj1;
    document.forms[0].ctl00$ContentPlaceHolder1$txt_custpart.focus();
}


function stylelook(){ 
  var NewWindow = window.open("StyleLookup.aspx","StyleLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}

function styleLookUp(ReturnObj1){
    document.forms[0].ctl00$ContentPlaceHolder1$txt_style.value = ReturnObj1;
    document.forms[0].ctl00$ContentPlaceHolder1$txt_style.focus();
}

function estimatelook(){ 
  var NewWindow = window.open("estimate_lookup.aspx","EstimateLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}
function EstimateLookup(ReturnObj1){
    document.forms[0].ctl00$ContentPlaceHolder1$txt_estimate.value = ReturnObj1;
    document.getElementById("ctl00_ContentPlaceHolder1_txt_estimate").focus();
}


function job1look(){ 
  var NewWindow = window.open("job1_lookup.aspx","Job1LookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}
function Job1Lookup(ReturnObj1){ 
  document.forms[0].ctl00$ContentPlaceHolder1$txt_job1.value = ReturnObj1;
}
function ShipTOLook(){ 
var lookHidden = document.getElementById("ctl00_ContentPlaceHolder1_txt_cust").value;
 var NewWindow = window.open("ShipIdCustLook.aspx?look="+lookHidden +"","ShipToLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}
function ShipToLookup(ReturnObj1){ 
  var cust1 = document.getElementById("ctl00_ContentPlaceHolder1_txt_cust");
  if (cust1.value != "") {

      document.forms[0].ctl00_ContentPlaceHolder1_txt_shipto.value = ReturnObj1;
      document.forms[0].ctl00_ContentPlaceHolder1_txt_shipto.focus();
  }
  else {
      alert("Customer can not be blank");
      cust1.focus();
  } 
  
  } 
  function blankcust()
  {
  
     var cust1 = document.getElementById("ctl00_ContentPlaceHolder1_txt_cust");
     var ship1 = document.getElementById("ctl00_ContentPlaceHolder1_txt_shipto");
     if (ship1.value != "") {
         if (cust1.value == "") {
             alert("Customer can not be blank");
             ship1.value = "";
             cust1.focus();
         }
     }
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

function select_col()
 {
    var NewWindow = window.open("show_avail_order_entry.aspx","SelectColumnWindow","width=400,height=600,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
 }



</script>

   <div>
       <asp:HiddenField ID="HiddenField1" runat="server" />
       <asp:HiddenField ID="HiddenField2" runat="server" />
       <asp:HiddenField ID="HiddenField3" runat="server" />  
    
    <TABLE id="tblMain" cellSpacing="1" cellPadding="1"  width='95%' border="0">
        <TR>
          <TD style="width: 1330px; height: 60px;">
             <TABLE id="tblSearch" cellSpacing="1" cellPadding="5" width="100%" border="0"  bgcolor=black>
              <TR>
                <TD class="shade" align="left"><br>
                  <table cellspacing="2" cellpadding="1"  border="0" class="shade" bgcolor="gray">    		   
		   <tr>
        		<td nowrap> <asp:Button ID="btnSearch" runat="server" Text="Go" OnClick="btnSearch_Click" CssClass="button" Width="40px" /><br />
                    <br />
                    <asp:Button ID="btn_reset" runat="server" CssClass="button" Text="All" OnClick="btn_reset_Click" Width="40px" />&nbsp;</td>
               <td nowrap="nowrap" style="width: 97px">
                   Estimate# <br>           
			<asp:TextBox ID="txt_estimate" runat="server" MaxLength="8" Width="65px" ></asp:TextBox>
			<a href="#" tabindex="1" onclick="estimatelook(); return false"><asp:Image ID="estLook" runat="server" ImageUrl="~/images/lookup_icon.gif" /></a>
			<br /> <asp:CheckBox ID="CheckBox1" Text="Single" runat="server" /><asp:CheckBox ID="CheckBox2" Text="Set" runat="server" />    </td>
          	   
           		    <td nowrap> Customer <br>
            <asp:TextBox ID="txt_cust" runat="server" MaxLength="8" Width="65px" ></asp:TextBox>
			<a href="#" tabindex="1" onclick="customerlook(); return false"><asp:Image ID="custLookup" runat="server" ImageUrl="~/images/lookup_icon.gif" /></a>
                	     <br /><asp:CheckBox ID="CheckBox3" Text="Tandem/Combo" runat="server" />                  	</td>
                	<td nowrap align="center" style="padding-bottom:20px"> ShipTo <br>
            <asp:TextBox ID="txt_shipto" runat="server" onkeyup="blankcust()" MaxLength="8" Width="65px" ></asp:TextBox>
			<a href="#" tabindex="1" onclick="ShipTOLook(); return false"><asp:Image ID="Image1" runat="server" ImageUrl="~/images/lookup_icon.gif" /></a>
                	    	</td>  
            		<td nowrap align="center" style="padding-bottom:20px">Cust Part# <br>
            <asp:TextBox ID="txt_custpart" runat="server" MaxLength="15" Width="95px" ></asp:TextBox>
			<a href="#" tabindex="1" onClick="custpartlook(); return false"><asp:Image ID="CustPartLookup" runat="server" ImageUrl="~/images/lookup_icon.gif" /></a>
                	</td>         
            		<td nowrap align="center" style="padding-bottom:20px"> FG Item# <br>
            <asp:TextBox ID="txt_fgitem" runat="server" MaxLength="15" Width="95px"></asp:TextBox>
			<a href="#" tabindex="1" onClick="fglook(); return false"><asp:Image ID="FGLookup" runat="server" ImageUrl="~/images/lookup_icon.gif" /></a>
                	</td>
                	<td nowrap align="center" style="padding-bottom:20px"> Item Name <br>
            <asp:TextBox ID="txt_itemname" runat="server" MaxLength="30" Width="65px" ></asp:TextBox>			
                	</td>
                	<td nowrap style="padding-bottom:20px"> Style# <br>
            <asp:TextBox ID="txt_style" runat="server" MaxLength="6" Width="45px" ></asp:TextBox>
			<a href="#" tabindex="1" onClick="stylelook(); return false"><asp:Image ID="EstimateLookup" runat="server" ImageUrl="~/images/lookup_icon.gif" /></a>
                	</td>
                	
                	 <td nowrap> LxWxD <br>
                    <asp:TextBox ID="txt_l" runat="server" Width="65px" MaxLength="2"  ></asp:TextBox>
                         <asp:CompareValidator ID="CompareValidator1" runat="server" ControlToValidate="txt_l" SetFocusOnError="true" Display="dynamic" Operator="dataTypeCheck" Type="Double" ErrorMessage="Not a valid Number"></asp:CompareValidator>
                    <br />
                     <asp:TextBox ID="txt_w" runat="server" Width="65px" MaxLength="2"  ></asp:TextBox>
                     <asp:CompareValidator ID="CompareValidator2" runat="server" ControlToValidate="txt_w" SetFocusOnError="true" Display="dynamic" Operator="dataTypeCheck" Type="Double" ErrorMessage="Not a valid Number"></asp:CompareValidator>
                     <br />
                    <asp:TextBox ID="txt_d" runat="server" Width="65px" MaxLength="2"  ></asp:TextBox>
			   	    <asp:CompareValidator ID="CompareValidator3" runat="server" ControlToValidate="txt_d" SetFocusOnError="true" Display="dynamic" Operator="dataTypeCheck" Type="Double" ErrorMessage="Not a valid Number"></asp:CompareValidator>
			</td>        
			   	<td>
                       <br /> <b>To</b> <br />
			   	</td>
			   	<td nowrap> LxWxD <br>
                    <asp:TextBox ID="txt_l2" runat="server" Width="65px" MaxLength="2"  ></asp:TextBox>
                         <asp:CompareValidator ID="CompareValidator4" runat="server" ControlToValidate="txt_l2" SetFocusOnError="true" Display="dynamic" Operator="dataTypeCheck" Type="Double" ErrorMessage="Not a valid Number"></asp:CompareValidator>
                    <br />
                     <asp:TextBox ID="txt_w2" runat="server" Width="65px" MaxLength="2"  ></asp:TextBox>
                     <asp:CompareValidator ID="CompareValidator5" runat="server" ControlToValidate="txt_w2" SetFocusOnError="true" Display="dynamic" Operator="dataTypeCheck" Type="Double" ErrorMessage="Not a valid Number"></asp:CompareValidator>
                     <br />
                    <asp:TextBox ID="txt_d2" runat="server" Width="65px" MaxLength="2" ></asp:TextBox>
			   	    <asp:CompareValidator ID="CompareValidator6" runat="server" ControlToValidate="txt_d2" SetFocusOnError="true" Display="dynamic" Operator="dataTypeCheck" Type="Double" ErrorMessage="Not a valid Number"></asp:CompareValidator>
			</td>        
			   	
           
             <td nowrap >Die#/ Cad#/ Plate#  <br />                     
            <asp:TextBox ID="txt_die"  runat="server" Width="65px" ></asp:TextBox><br />
            <asp:TextBox ID="txt_cad"  runat="server" Width="65px" ></asp:TextBox><br />
            <asp:TextBox ID="txt_plate"  runat="server" Width="65px" ></asp:TextBox>
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
                             
                              <asp:TextBox ID="aLineLabel" runat="server" Width="40px" OnTextChanged="ddl_display_TextChanged" Text='<%# Bind("aLine") %>'></asp:TextBox>
                              <asp:CompareValidator ID="CompareValidator4" runat="server" ControlToValidate="aLineLabel" SetFocusOnError="true" Display="dynamic" Operator="dataTypeCheck" Type="Integer" ErrorMessage="Invalid Input"></asp:CompareValidator>
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
       </table>
       </td>
      </tr>    
         
      
        <tr width="100%">
        <td>
            <asp:GridView ID="GridView2" runat="server" OnRowCreated="GridView2_RowCreated" AllowPaging="true" OnPageIndexChanging="GridView2_PageIndexChanging" 
            AllowSorting="True" OnSorting="GridView2_Sorting" OnSelectedIndexChanged="GridView2_SelectedIndexChanged" OnRowDataBound="GridView2_RowDataBound"
            EmptyDataText="No Records Found" Width="100%" BorderStyle="Dotted" CssClass="Grid" >
            <SelectedRowStyle CssClass="GridSelected" BackColor="Yellow" />
            <AlternatingRowStyle CssClass="GridItemOdd" />            
            <EmptyDataRowStyle BorderStyle="Dotted" BorderColor="Gray" BorderWidth="0px" Font-Bold="True" HorizontalAlign="Center" VerticalAlign="Middle" />
            <HeaderStyle  CssClass="headcolor" ForeColor="White" Height="40px"  VerticalAlign="Middle"  HorizontalAlign="Center" Wrap="False"></HeaderStyle>
            <RowStyle CssClass="shade"  />
            <Columns>
                <asp:CommandField ShowSelectButton="True" ButtonType="Image" SelectImageUrl="~/Images/sel.gif" SelectText="" > 
                    <ItemStyle Width="10px" />
                </asp:CommandField>
                 <asp:TemplateField HeaderText="estno" Visible="false"  >                    
                    <ItemTemplate>
                        <asp:Label ID="label_est_no" runat="server" Text='<%# Bind("[Estimate]") %>'></asp:Label>
                    </ItemTemplate>
                    <ItemStyle Wrap="False" />
                </asp:TemplateField>
                
                <asp:TemplateField HeaderText="FGItem"  Visible="false" >                    
                    <ItemTemplate>
                        <asp:Label ID="Label_fgitem" runat="server" Text='<%# Bind("[Fg Item]") %>'></asp:Label>
                    </ItemTemplate>
                    <ItemStyle Wrap="False" />
                </asp:TemplateField>
                <asp:TemplateField HeaderText="reckey" Visible="false" >                    
                    <ItemTemplate>
                        <asp:Label ID="label_rec_key" runat="server" Text='<%# Bind("[Reckey]") %>'></asp:Label>
                    </ItemTemplate>
                    <ItemStyle Wrap="False" />
                </asp:TemplateField>
                
                <asp:TemplateField HeaderText="formno"  Visible="false" >                    
                    <ItemTemplate>
                        <asp:Label ID="typeLabel" runat="server" Text='<%# Bind("[vFormno]") %>'></asp:Label>
                    </ItemTemplate>
                    <ItemStyle Wrap="False" />
                </asp:TemplateField>
                <asp:TemplateField HeaderText="blankno" Visible="false" >                    
                    <ItemTemplate>
                        <asp:Label ID="blankLabel" runat="server" Text='<%# Bind("[vBlankno]") %>'></asp:Label>
                    </ItemTemplate>
                    <ItemStyle Wrap="False" />
                </asp:TemplateField>
                
            </Columns>
            </asp:GridView> </td>
            </tr>
        </table>    
    </div>   
          
</asp:Content>