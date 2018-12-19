<%@ Page Language="C#" MasterPageFile="~/MasterPage3.master"  EnableEventValidation="false" Inherits="fgitem_list" Title="FgItem" Codebehind="ItemInquiryList.aspx.cs" %>
<asp:Content ID="Content1" ContentPlaceHolderID="ContentPlaceHolder1" Runat="Server">
    <asp:ScriptManager ID="ScriptManager1" runat="server">
            </asp:ScriptManager>

<script language = "JavaScript" src="include/CalendarControl.js"></script>
<script language="javascript" src="include/date.js"></script>
<script language="javascript" src="include/event.js"></script>
<script type="text/javascript">
    window.onload = setfocus;
    function setfocus() {
        document.forms[0].ctl00$ContentPlaceHolder1$txt_fgitem.focus();
    }

    function preLeave(fieldObj, fieldType, fieldFormat) {
        fieldObj.style.backgroundColor = 'Window';
        fieldObj.style.color = 'WindowText';
        fieldType = fieldType.toLowerCase();

        if ((fieldType == "") || (fieldType == "text")) {
            leaveField(fieldObj);
        }

        if (fieldType == "date") {
            if (fieldFormat == "") {
                var dateFormat = "99/99/9999";
            } else { var dateFormat = fieldFormat; }
            checkDate(dateFormat, fieldObj, '01/01/1950', '12/31/3000', 0);
        }

        if (fieldType == "number") {
            if (fieldFormat == "") {
                var numFormat = "(>>>>9)";
            } else { var numFormat = fieldFormat; }
            checkNum(numFormat, fieldObj, '?', '?', 0);
        }
    }

    function preEnter(fieldObj, canEdit) {
        //fieldObj.style.backgroundColor = 'blue';
        //fieldObj.style.color = 'white';
        if (canEdit == "no") {
            fieldObj.blur();
            leaveField(fieldObj);
        }

        enterField(fieldObj);
        return;
    }


    function customerlook() {
        var NewWindow = window.open("customer_lookup.aspx", "CustomerLookupWindow", "width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
    }

    function CustomerLookup(ReturnObj1) {
        document.forms[0].ctl00$ContentPlaceHolder1$txt_Cad.value = ReturnObj1;
        document.forms[0].ctl00$ContentPlaceHolder1$txt_Cad.focus();
    }
function stylelook(){ 
  var NewWindow = window.open("StyleLookup.aspx","StyleWindow","width=600,height=600,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}

function styleLookUp(ReturnObj1){
    document.forms[0].ctl00$ContentPlaceHolder1$txt_style.value = ReturnObj1;
    document.forms[0].ctl00$ContentPlaceHolder1$txt_style.focus();  
}
function categorylookup(){ 
  var NewWindow =window.open("CategoryLookup.aspx","CategoryWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}

function categoryLookUp(ReturnObj1){ 
  document.forms[0].ctl00$ContentPlaceHolder1$txt_cat.value = ReturnObj1;
  document.forms[0].ctl00$ContentPlaceHolder1$txt_cat.focus();
}

function fglook(){ 
  var NewWindow = window.open("fgitemlook.aspx","FGLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}

function FGitemLookup(ReturnObj1) {
    document.forms[0].ctl00$ContentPlaceHolder1$txt_fgitem.value = ReturnObj1;
    document.forms[0].ctl00$ContentPlaceHolder1$txt_fgitem.focus();
}

function estimatelook(){ 
  var NewWindow = window.open("estimate_lookup.aspx","EstimateLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}
function EstimateLookup(ReturnObj1){
    document.forms[0].ctl00$ContentPlaceHolder1$txt_estimate.value = ReturnObj1;
    document.forms[0].ctl00$ContentPlaceHolder1$txt_estimate.focus();
}

function custpartlook() {
    //var cust = document.getElementById("ctl00_ContentPlaceHolder1_txt_customer").value;
    
    var NewWindow = window.open("custpart2_lookup.aspx?customer=" + "" + "", "CustPartLookupWindow", "width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}

function CustPartLookup(ReturnObj1) {
    document.forms[0].ctl00$ContentPlaceHolder1$txt_custpart.value = ReturnObj1;
    document.forms[0].ctl00$ContentPlaceHolder1$txt_custpart.focus();
}
function dielook() {
    var NewWindow = window.open("die_lookup.aspx", "DieLookupWindow", "width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}
function DieLookup(ReturnObj1) {
    document.forms[0].ctl00$ContentPlaceHolder1$txt_die.value = ReturnObj1;
    document.forms[0].ctl00$ContentPlaceHolder1$txt_die.focus();
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
<div>
      
    <TABLE id="tblMain" cellSpacing="1" cellPadding="1"  width='95%' border="0">
        <TR>
          <TD style="width: 1200px; height: 60px;">
             <TABLE id="tblSearch"  width="90%" border="0"  bgcolor=black>
              <TR>
                <TD class="shade" align="left"><br>
                  <table cellspacing="2" cellpadding="1"  border="0" class="shade" bgcolor="gray">    		   
		   <tr>
        		<td nowrap> <asp:Button ID="btnSearch" runat="server" Text="Go" OnClick="btnSearch_Click" CssClass="button" Width="40px" /><br />
                   
                   </td>
                         	   
           		    <td nowrap> FG Item# <br>
                    <asp:TextBox ID="txt_fgitem" runat="server" MaxLength="15" Width="95px" ></asp:TextBox>
			        <a href="#" tabindex="1" onClick="fglook(); return false"><asp:Image ID="FGLookup" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
                	</td>  
                    <td nowrap> Cust Part# <br>
                    <asp:TextBox ID="txt_custpart" runat="server" MaxLength="15" Width="95px" ></asp:TextBox>
			        <a href="#" tabindex="1" onClick="custpartlook(); return false"><asp:Image ID="Image3" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
                	</td>
            		<td nowrap>Item Name# <br>
            <asp:TextBox ID="txt_name" runat="server" Width="95px" ></asp:TextBox>	
                        <a href="#" tabindex="1" onClick="fglook(); return false"><asp:Image ID="Image4" runat="server" ImageUrl="images/lookup_icon.gif" /></a>		
                	</td>         
            		
                	<td nowrap> Estimate# <br>
            <asp:TextBox ID="txt_estimate" runat="server" MaxLength="8" Width="65px" ></asp:TextBox>
			<a href="#" tabindex="1" onClick="estimatelook(); return false"><asp:Image ID="EstimateLookup" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
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
		   </tr>
                <tr>
                    <td>
                         <asp:Button ID="btn_reset" runat="server" CssClass="button" Text="All" OnClick="btn_reset_Click" Width="40px" />&nbsp;
                    </td>
                	<td nowrap> Cat# <br>
            <asp:TextBox ID="txt_cat" runat="server" MaxLength="8" Width="65px" ></asp:TextBox>
            <a href="#" tabindex="1" onClick="categorylookup(); return false"><asp:Image ID="Image2" runat="server" ImageUrl="images/lookup_icon.gif" /></a>                         
                	</td>   
                	<td nowrap>Style#<br>
            <asp:TextBox ID="txt_style" runat="server" MaxLength="8" Width="65px" ></asp:TextBox> 
            <a href="#" tabindex="1" onClick="stylelook(); return false"><asp:Image ID="Image1" runat="server" ImageUrl="images/lookup_icon.gif" /></a>            
                	</td> 
               <td nowrap> Customer# <br>
            <asp:TextBox ID="txt_Cad" runat="server" Width="95px"></asp:TextBox>
                   <a href="#" tabindex="1" onClick="customerlook(); return false"><asp:Image ID="CustomerLook" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
			       	</td>    
               <td nowrap> Die# <br>
            <asp:TextBox ID="txt_die" runat="server" Width="65px"></asp:TextBox>
            <a href="#" tabindex="1" onclick="dielook(); return false"><asp:Image ID="Image9" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
			       	</td>
               <td nowrap> Start Date <br>
            <asp:TextBox ID="txt_startdate" onfocus="javascript:preEnter( this, 'yes' );" onblur="javascript:preLeave( this, 'date', '99/99/9999' );" ToolTip="MM/DD/YYYY" runat="server" Width="65px"></asp:TextBox>
                   <a href="#" onblur="ContentPlaceHolder1_txt_startdate.focus()" tabindex="1" onClick="showCalendarControl(ContentPlaceHolder1_txt_startdate); return false"><asp:Image ID="Image12" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
			       	</td>
               <td nowrap> End Date <br>
            <asp:TextBox ID="txt_enddate" onfocus="javascript:preEnter( this, 'yes' );" onblur="javascript:preLeave( this, 'date', '99/99/9999' );" ToolTip="MM/DD/YYYY" runat="server" Width="65px"></asp:TextBox>
                   <a href="#" onblur="ContentPlaceHolder1_txt_enddate.focus()" tabindex="1" onClick="showCalendarControl(ContentPlaceHolder1_txt_enddate); return false"><asp:Image ID="Image5" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
			       	</td>
                	 
                	                  
                  
			<td></td>
			<td></td>                    
			
                  </tr>
                  </table>              
         </TD></TR>
       </table>      
        
       </td>
      </tr>    
         
      <tr>      
        <td style="width: 1100px; height: 55px;">
            <br />
            <%--<asp:ObjectDataSource ID="ObjectDataSource1" runat="server" OldValuesParameterFormatString="original_{0}" SelectMethod="BrwsItemInq" TypeName="Order">
                <SelectParameters>
                    <asp:SessionParameter SessionField="item_inquery_list_user" Name="prmUser" Type="String" />
                    <asp:SessionParameter SessionField="item_inquery_list_action" Name="prmActItem" Type="String" DefaultValue="BrwsItem" />
                    <asp:SessionParameter SessionField="item_inquery_list_item" Name="prmItem" Type="String" />
                    <asp:SessionParameter SessionField="item_inquery_list_itemname" Name="prmItemName" Type="String" />
                    <asp:SessionParameter SessionField="item_inquery_list_user_cad" Name="prmCad" Type="String" />
                    <asp:SessionParameter SessionField="item_inquery_list_user_est" Name="prmEst" Type="String" />
                    <asp:SessionParameter SessionField="item_inquery_list_user_style" Name="prmStyle" Type="String" />
                    <asp:SessionParameter SessionField="item_inquery_list_user_procat" Name="prmProcat" Type="String" />

                    <asp:SessionParameter SessionField="item_inquery_list_user_castpart" Name="prmCustPart" Type="String" />
                    <asp:SessionParameter SessionField="item_inquery_list_user_die" Name="PrmDie" Type="String" />
                    <asp:SessionParameter SessionField="item_inquery_list_user_startdate" Name="prmStartDate" Type="String" />
                    <asp:SessionParameter SessionField="item_inquery_list_user_enddate" Name="prmEndDate" Type="String" />
                </SelectParameters>
            </asp:ObjectDataSource>--%>
        
        <asp:GridView ID="GridView1" runat="server" 
            OnRowCreated="GridView1_RowCreated" AllowPaging="true" OnPageIndexChanging="GridView1_PageIndexChanging" 
            AllowSorting="True" OnSorting="GridView1_Sorting" OnSelectedIndexChanged="GridView1_SelectedIndexChanged" OnRowDataBound="GridView1_RowDataBound"
            EmptyDataText="No Records Found" Width="800px" BorderStyle="Dotted" CssClass="Grid" PageSize="50"   >
            <SelectedRowStyle CssClass="GridSelected" BackColor="Yellow" />
            <AlternatingRowStyle CssClass="GridItemOdd" />
            <EmptyDataRowStyle BorderStyle="Dotted" BorderColor="Gray" BorderWidth="0px" Font-Bold="True" HorizontalAlign="Center" VerticalAlign="Middle" />
        <RowStyle CssClass="shade"  />
            
            <HeaderStyle  HorizontalAlign="Center" VerticalAlign="Middle" Wrap="False" ForeColor="White" CssClass="headcolor" Height="40px" />
            <Columns>
            <asp:CommandField ShowSelectButton="True" ButtonType="Image" SelectImageUrl="~/Images/sel.gif" SelectText="" > 
                    <ItemStyle Width="10px" />
                    </asp:CommandField>

                <asp:TemplateField HeaderText="Item No" Visible="False" >                    
                    <ItemTemplate>
                        <asp:Label ID="item_label" runat="server" Text='<%# Bind("[Item No]") %>'></asp:Label>
                    </ItemTemplate>
                    <ItemStyle Wrap="False" />
                </asp:TemplateField>
                <asp:TemplateField HeaderText="Name" Visible="False">                    
                    <ItemTemplate>
                        <asp:Label ID="name_label" runat="server" Text='<%# Bind("[Name]") %>'></asp:Label>
                    </ItemTemplate>
                    <ItemStyle Wrap="False" />
                </asp:TemplateField>

                <asp:TemplateField HeaderText="reckey" Visible="False" >                    
                    <ItemTemplate>
                        <asp:Label ID="lbl_rec_key" runat="server" Text='<%# Bind("[cReckey]") %>'></asp:Label>
                    </ItemTemplate>
                    <ItemStyle Wrap="False" />
                </asp:TemplateField>
                <asp:TemplateField HeaderText="Cust#" Visible="False">                    
                    <ItemTemplate>
                        <asp:Label ID="cust_label" runat="server" Text='<%# Bind("[Cust#]") %>'></asp:Label>
                    </ItemTemplate>
                    <ItemStyle Wrap="False" />
                </asp:TemplateField>

                <asp:TemplateField HeaderText="Style" Visible="False" >                    
                    <ItemTemplate>
                        <asp:Label ID="style_label" runat="server" Text='<%# Bind("[Style]") %>'></asp:Label>
                    </ItemTemplate>
                    <ItemStyle Wrap="False" />
                </asp:TemplateField>
                <asp:TemplateField HeaderText="Category" Visible="False">                    
                    <ItemTemplate>
                        <asp:Label ID="cat_label" runat="server" Text='<%# Bind("[Category]") %>'></asp:Label>
                    </ItemTemplate>
                    <ItemStyle Wrap="False" />
                </asp:TemplateField>
                <asp:TemplateField HeaderText="Stock/Custom" Visible="False" >                    
                    <ItemTemplate>
                        <asp:Label ID="stcust_label" runat="server" Text='<%# Bind("[Stock/Custom]") %>'></asp:Label>
                    </ItemTemplate>
                    <ItemStyle Wrap="False" />
                </asp:TemplateField>
                <asp:TemplateField HeaderText="Estimate" Visible="False">                    
                    <ItemTemplate>
                        <asp:Label ID="est_label" runat="server" Text='<%# Bind("[Estimate]") %>'></asp:Label>
                    </ItemTemplate>
                    <ItemStyle Wrap="False" />
                </asp:TemplateField>

                <asp:TemplateField HeaderText="Stocked" Visible="False" >                    
                    <ItemTemplate>
                        <asp:Label ID="stocked_label" runat="server" Text='<%# Bind("[Stocked]") %>'></asp:Label>
                    </ItemTemplate>
                    <ItemStyle Wrap="False" />
                </asp:TemplateField>
                <asp:TemplateField HeaderText="Qty On-Hand" Visible="False">                    
                    <ItemTemplate>
                        <asp:Label ID="qtyhand_label" runat="server" Text='<%# Bind("[Qty On-Hand]") %>'></asp:Label>
                    </ItemTemplate>
                    <ItemStyle Wrap="False" />
                </asp:TemplateField>

                <asp:TemplateField HeaderText="Cust Part#" Visible="False" >                    
                    <ItemTemplate>
                        <asp:Label ID="custpart_label" runat="server" Text='<%# Bind("[Cust Part#]") %>'></asp:Label>
                    </ItemTemplate>
                    <ItemStyle Wrap="False" />
                </asp:TemplateField>
                <asp:TemplateField HeaderText="Die#" Visible="False">                    
                    <ItemTemplate>
                        <asp:Label ID="die_label" runat="server" Text='<%# Bind("[Die#]") %>'></asp:Label>
                    </ItemTemplate>
                    <ItemStyle Wrap="False" />
                </asp:TemplateField>

                <asp:TemplateField HeaderText="Allocated Qty" Visible="False" >                    
                    <ItemTemplate>
                        <asp:Label ID="allqty_label" runat="server" Text='<%# Bind("[Allocated Qty]") %>'></asp:Label>
                    </ItemTemplate>
                    <ItemStyle Wrap="False" />
                </asp:TemplateField>
                <asp:TemplateField HeaderText="Available Qty" Visible="False">                    
                    <ItemTemplate>
                        <asp:Label ID="availqty_label" runat="server" Text='<%# Bind("[Available Qty]") %>'></asp:Label>
                    </ItemTemplate>
                    <ItemStyle Wrap="False" />
                </asp:TemplateField>

                <asp:TemplateField HeaderText="Ordered Qty" Visible="False" >                    
                    <ItemTemplate>
                        <asp:Label ID="ordqty_label" runat="server" Text='<%# Bind("[Ordered Qty]") %>'></asp:Label>
                    </ItemTemplate>
                    <ItemStyle Wrap="False" />
                </asp:TemplateField>
                <asp:TemplateField HeaderText="Shipped Qty" Visible="False">                    
                    <ItemTemplate>
                        <asp:Label ID="shipqty_label" runat="server" Text='<%# Bind("[Shipped Qty]") %>'></asp:Label>
                    </ItemTemplate>
                    <ItemStyle Wrap="False" />
                </asp:TemplateField>


                <%--<asp:BoundField DataField="vItemno" HeaderText="Item No" SortExpression="vItemno" />
                <asp:BoundField DataField="vName" HeaderText="Name" SortExpression="vName" />
                <asp:BoundField DataField="vPartdsc" HeaderText="Description" SortExpression="vPartdsc" />
                <asp:BoundField DataField="vCust" HeaderText="Cust#" SortExpression="vCust" />
                <asp:BoundField DataField="vStyle" HeaderText="Style" SortExpression="vStyle" />
                <asp:BoundField DataField="vProcat" HeaderText="Category" SortExpression="vProcat" />
                <asp:BoundField DataField="vCode" HeaderText="Stock/Custom" SortExpression="vCode" />
                <asp:BoundField DataField="vEstno" HeaderText="Estimate" SortExpression="vEstno" />                
                <asp:CheckBoxField DataField="vStocked" HeaderText="Stocked" SortExpression="vStocked" />
                <asp:BoundField DataField="vQonh" HeaderText="Qty On-Hand" SortExpression="vQonh" />--%>
            </Columns>
            
        </asp:GridView> 
     
        </td>
        </tr>
        <tr width="100%">
        <td style="width: 1330px">
            &nbsp;</td>
            </tr>
        </table>
</div>
</asp:Content>

