<%@ Page Language="c#" AutoEventWireup="true" Debug="true" Inherits="topbtnorderreport" Codebehind="topbtnorderreport.aspx.cs" %>

<html xmlns="http://www.w3.org/1999/xhtml" >
  <head id="Head1" runat="server">
    <title>Order Maintenance Report</title>
    
    <LINK href="include/style.css" type="text/css" rel="stylesheet"/>
     
    <link href="include/tree.css" rel="stylesheet" type="text/css" />
    <link href="include/dhtmlwindow.css" rel="stylesheet" type="text/css" />
    <script language = JavaScript>
    
    var bSelected=false;
    function ChSel()
    {
        var theForm = document.forms['frmList'];
        if (!theForm) theForm = document.frmList;
        bSelected = !bSelected; 
        var i;
        for (i=0;i<theForm.chDelete.length;++i) theForm.chDelete[i].checked=bSelected;
    } 
    
    function OnKeyDown()
    {
        e = window.event;
        if (e.keyCode == 13)
        {
            e.cancel = true;
            var theForm = document.forms['frmList'];
            if (!theForm) theForm = document.frmList;                
            theForm.SubmitButton.click();              
        }
    }
    function samevalue()
    {
        var beginc = document.getElementById("begincust_TextBox");
    var endc=document.getElementById("endcust_TextBox");
    endc.value=beginc.value;
    }
    
    function samevalue2()
    {
    var beginc=document.getElementById("begincust_TextBox");
    var endc=document.getElementById("endcust_TextBox");
    if(endc.value!=beginc.value)
    {
    alert("Begin and End Customer Value must be same");
    endc.value=beginc.value;
    endc.focus();
    }
    }
    
    function contactcustomerlook(){ 
  var NewWindow = window.open("contact_customer_lookup.aspx","ContactCustomerLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}

function ContactCustomerLookup(ReturnObj1,ReturnObj2,ReturnObj3,ReturnObj4,ReturnObj5,ReturnObj6,ReturnObj7,ReturnObj8,ReturnObj9, ReturnObj10,ReturnObj11){ 
  document.forms[0].begincust_TextBox.value = ReturnObj1;
  document.forms[0].endcust_TextBox.value = ReturnObj1;
  
    
}

    
    </script> 

      <link href="include/tree.css" rel="stylesheet" type="text/css" />
  </head>    
   <body>
    <form id="frmList" runat="server"   >   
       
      <div>
                      
      <TABLE id="tblTop" cellSpacing="3" border="0">
        <TR>
          
          <TD align=center nowrap><font size=+0><b>Order Maintenance Report&nbsp;</b></font></TD>
          
        </TR>
      </TABLE>
      
            <asp:HiddenField ID="hid_prmout" runat="server" />
            <asp:HiddenField ID="hid_open_close" runat="server" />
            <asp:HiddenField ID="hid_order" runat="server" />
            <asp:HiddenField ID="hid_cust" runat="server" />
            <asp:HiddenField ID="hid_ord_date" runat="server" />
            <asp:HiddenField ID="hid_fgitem" runat="server" />
            <asp:HiddenField ID="hid_cust_part" runat="server" />
            <asp:HiddenField ID="hid_item_name" runat="server" />
            <asp:HiddenField ID="hid_cust_po" runat="server" />
            <asp:HiddenField ID="hid_ord_qty" runat="server" />
            <asp:HiddenField ID="hid_prod_qty" runat="server" />
            <asp:HiddenField ID="hid_ship_qty" runat="server" />
            <asp:HiddenField ID="hid_on_hand_qty" runat="server" />
            <asp:HiddenField ID="hid_sell_price" runat="server" />
            <asp:HiddenField ID="hid_uom" runat="server" />
            <asp:HiddenField ID="hid_unit_count" runat="server" />
            <asp:HiddenField ID="hid_pallet_count" runat="server" />
            <asp:HiddenField ID="hid_skids" runat="server" />
            <asp:HiddenField ID="hid_status" runat="server" />
            <asp:HiddenField ID="hid_due_date" runat="server" />
            <asp:HiddenField ID="hid_cust_name" runat="server" />
            <asp:HiddenField ID="hid_est" runat="server" />
            <asp:HiddenField ID="hid_job" runat="server" />
            <asp:HiddenField ID="hid_cad" runat="server" />
            <asp:HiddenField ID="hid_inv_qty" runat="server" />
            <asp:HiddenField ID="hid_act_rel_qty" runat="server" />
            <asp:HiddenField ID="hid_prod_bal" runat="server" />
            <asp:HiddenField ID="hid_ou" runat="server" />
            
          <asp:Label ID="Label1" runat="server" ForeColor="red" Font-Bold="true"></asp:Label>
          
           <asp:FormView ID="FormView2" runat="server" DataSourceID="ObjectDataSource2">
                            
              <ItemTemplate>
                  
                  <asp:Label ID="CustLabel" Visible="false" runat="server" Text='<%# Bind("Cust") %>'></asp:Label><br />
              </ItemTemplate>
          </asp:FormView>
          <asp:ObjectDataSource ID="ObjectDataSource2" runat="server" OldValuesParameterFormatString="original_{0}"
              SelectMethod="FillAlphabeticList" TypeName="reports">
              <SelectParameters>
                  <asp:Parameter Name="prmUser" Type="String" />
                  <asp:Parameter Name="prmComp" Type="String" />
              </SelectParameters>
          </asp:ObjectDataSource>
          
          <fieldset class="shade">
            <table class="shade">
                <tr>
                    <td>Selection Parameters</td>
                    <td>&nbsp;</td>
                    <td nowrap>
                            <asp:RadioButtonList ID="rdl_openclose" runat="server" RepeatLayout="flow" RepeatColumns="2">
                                <asp:ListItem>Open</asp:ListItem>
                                <asp:ListItem>Close</asp:ListItem>
                            </asp:RadioButtonList>
                    </td>
                </tr>
                <tr>
                    <td align="right" style="padding-right:5px"><b>Beginning Customer#:</b></td>
                    <td nowrap>
                        <asp:TextBox ID="begincust_TextBox" onkeyup="samevalue()" runat="server"></asp:TextBox>
                        <a href="#" onClick="contactcustomerlook(); return false"><asp:Image ID="CustomerLook" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
                </td>
                    <td align="right" style="padding-right:5px"><b>Ending Customer:</b></td>
                    <td>
                        <asp:TextBox ID="endcust_TextBox"  runat="server"></asp:TextBox>
                    </td>
                </tr>  
            </table>
          </fieldset>
          <fieldset class="shade">
            Export Selection
            <br />
                <table >
                                      
                    <tr>
                        <td nowrap>
                            <b><asp:CheckBox ID="chk_order" runat="server" Text="Order#" /></b>
                        </td>
                        <td>
                            <b><asp:CheckBox ID="chk_cust" runat="server" Text="Customer#" /></b>
                        </td>
                        <td nowrap>
                            <b><asp:CheckBox ID="chk_ord_date" runat="server" Text="Order Date" /></b>
                        </td>
                        <td>
                            <b><asp:CheckBox ID="chk_fg_item" runat="server" Text="FG Item#" /></b>
                        </td>
                        <td nowrap>
                            <b><asp:CheckBox ID="chk_cust_part" runat="server" Text="Cust Part#" /></b>
                        </td>
                        <td>
                            <b><asp:CheckBox ID="chk_item_name" runat="server" Text="Item Name" /></b>
                        </td>
                    </tr>
                    <tr>
                        <td nowrap>
                            <b><asp:CheckBox ID="chk_cust_po" runat="server" Text="Cust Po#" /></b>
                        </td>
                        <td>
                            <b><asp:CheckBox ID="chk_ord_qty" runat="server" Text="Ordered Qty" /></b>
                        </td>
                        <td nowrap>
                            <b><asp:CheckBox ID="chk_prod_qty" runat="server" Text="Prod. Qty" /></b>
                        </td>
                        <td>
                            <b><asp:CheckBox ID="chk_ship_qty" runat="server" Text="Shipped Qty" /></b>
                        </td>
                        <td nowrap>
                            <b><asp:CheckBox ID="chk_on_hand_qty" runat="server" Text="On Hand Qty" /></b>
                        </td>
                        <td>
                            <b><asp:CheckBox ID="chk_sell_price" runat="server" Text="Sell Price" /></b>
                        </td>
                    </tr>
                    <tr>
                        <td nowrap>
                            <b><asp:CheckBox ID="chk_uom" runat="server" Text="UOM" /></b>
                        </td>
                        <td>
                            <b><asp:CheckBox ID="chk_unit_count" runat="server" Text="Unit Count" /></b>
                        </td>
                        <td nowrap>
                            <b><asp:CheckBox ID="chk_pallet_count" runat="server" Text="Pallet Count" /></b>
                        </td>
                        <td>
                            <b><asp:CheckBox ID="chk_skids" runat="server" Text="Skids" /></b>
                        </td>
                        <td nowrap>
                            <b><asp:CheckBox ID="chk_status" runat="server" Text="Status" /></b>
                        </td>
                        <td>
                            <b><asp:CheckBox ID="chk_due_date" runat="server" Text="Due Date" /></b>
                        </td>
                    </tr>
                    <tr>
                        <td nowrap>
                            <b><asp:CheckBox ID="chk_cust_name" runat="server" Text="Order#" /></b>
                        </td>
                        <td>
                            <b><asp:CheckBox ID="chk_est" runat="server" Text="Est#" /></b>
                        </td>
                        <td nowrap>
                            <b><asp:CheckBox ID="chk_job" runat="server" Text="Job#" /></b>
                        </td>
                        <td>
                            <b><asp:CheckBox ID="chk_cad" runat="server" Text="CAD#" /></b>
                        </td>
                        <td nowrap>
                            <b><asp:CheckBox ID="chk_inv_qty" runat="server" Text="Invoice Qty" /></b>
                        </td>
                        <td>
                            <b><asp:CheckBox ID="chk_act_rel_qty" runat="server" Text="Act Rel Qty" /></b>
                        </td>
                    </tr>
                    <tr>
                        <td nowrap>
                            <b><asp:CheckBox ID="chk_prod_bal" runat="server" Text="Prod Balance" /></b>
                        </td>
                        <td>
                            <b><asp:CheckBox ID="chk_ou" runat="server" Text="O/U%" /></b>
                        </td>
                        
                    </tr>
                    <tr>
                        <td>&nbsp;</td>
                    </tr>
                    <tr>
                        <%--<td align="right" style="padding-right: 5px">
                        <b>Output To:</b></td>--%>
                        <td><b><asp:RadioButtonList ID="RadioButtonList8" Visible="false" RepeatLayout="Flow"  CellSpacing="1" RepeatColumns="5" Font-Bold ="true" runat="server">
                            <asp:ListItem      Text="Pdf" />
                            <asp:ListItem      Text="Excel" />
                         </asp:RadioButtonList></b>
                    </td>
                </tr>
                <tr>    
                    <td colspan="4">
                        <asp:Button ID="SubmitButton" runat="server" CssClass="button" Text="Submit" OnClick="SubmitButton_Click"></asp:Button>
                        <input type="button" id="btn_close" value="Close" class="buttonM" onclick="javascript:window.close()" />
                            &nbsp;&nbsp;
                            <asp:Label ID="OutputLabel" runat="server" Text="Output File:" Font-Bold="True" Font-Size="Larger" ForeColor="Blue"></asp:Label>
                                &nbsp;
                            <asp:HyperLink ID="HyperLink1" runat="server"  Target="_blank" Font-Bold="True" Font-Size="Larger" ForeColor="Red"></asp:HyperLink> 
                  
                    </td>
                </tr>
            </table>
          </fieldset>                    
                              
         <asp:FormView ID="FormView1" Visible="False"   runat="server" DataSourceID="ObjectDataSource1" >
                        
            <ItemTemplate>
                vFile:
                <asp:Label ID="vFileLabel" runat="server" Text='<%# Bind("vFile") %>'></asp:Label><br />
                vvsdsads:
                <asp:Label ID="vvsdsadsLabel" runat="server" Text='<%# Bind("vvsdsads") %>'></asp:Label><br />
            </ItemTemplate>
             
             
        </asp:FormView>
        <asp:ObjectDataSource ID="ObjectDataSource1" runat="server" OldValuesParameterFormatString="original_{0}" SelectMethod="TopbtnOrderReport" TypeName="reports">
            <SelectParameters>
                <asp:Parameter Name="prmUser" Type="String"  />
                <asp:Parameter Name="prmAction" Type="String" DefaultValue="Order"  />
                <asp:Parameter Name="prmOut" Type="String" />
                <asp:Parameter Name="prmopenclose" Type="String" />
                <asp:Parameter Name="prmBeginCust" Type="String" />
                <asp:Parameter Name="prmEndCust" Type="String" />
                <asp:Parameter Name="prmOrder" Type="String" />
                <asp:Parameter Name="prmCust" Type="String" />
                <asp:Parameter Name="prmOrderDate" Type="String" />
                <asp:Parameter Name="prmFgItem" Type="String" />
                <asp:Parameter Name="prmCustPart" Type="String" />
                <asp:Parameter Name="prmItemName" Type="String" />
                <asp:Parameter Name="prmCustPo" Type="String" />
                <asp:Parameter Name="prmOrderQty" Type="String" />
                <asp:Parameter Name="prmProdQty" Type="String" />
                <asp:Parameter Name="prmShipQty" Type="String" />
                <asp:Parameter Name="prmOnHandQty" Type="String" />
                <asp:Parameter Name="prmSellPrice" Type="String" />
                <asp:Parameter Name="prmUom" Type="String" />
                <asp:Parameter Name="prmUnitCost" Type="String" />
                <asp:Parameter Name="prmPalletCount" Type="String" />
                <asp:Parameter Name="prmSkids" Type="String" />
                <asp:Parameter Name="prmStatus" Type="String" />
                <asp:Parameter Name="prmDueDate" Type="String" />
                <asp:Parameter Name="prmCustName" Type="String" />
                <asp:Parameter Name="prmEst" Type="String" />
                <asp:Parameter Name="prmJob" Type="String" />
                <asp:Parameter Name="prmCad" Type="String" />
                <asp:Parameter Name="prmInvoiceQty" Type="String" />
                <asp:Parameter Name="prmActRelQty" Type="String" />
                <asp:Parameter Name="prmProdBal" Type="String" />
                <asp:Parameter Name="prmOU" Type="String" />
            </SelectParameters>
        </asp:ObjectDataSource>
    </div>
       
    </form>
</body>
</html>
