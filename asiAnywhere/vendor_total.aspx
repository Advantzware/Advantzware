<%@ Page Language="C#" Debug="true" AutoEventWireup="true" Inherits="verdor_total" Title="Vendors" Codebehind="vendor_total.aspx.cs" %>
<%@ Register Src="footer.ascx" TagName="Footer" TagPrefix="ft" %>
<%@ Register Src="header.ascx" TagName="Header" TagPrefix="hd" %>

<html xmlns="http://www.w3.org/1999/xhtml" >
  <head id="Head1" runat="server">
    <title>Vendors</title>
    <LINK href="include/style2.css" type="text/css" rel="stylesheet"/>
    
<LINK REL="stylesheet" TYPE="text/css" HREF="include/CalendarControl.css" >
    <script language = "JavaScript" src="include/CalendarControl.js"></script>
    <script language="javascript" src="include/date.js"></script>
    <script language="javascript" src="include/event.js"></script>
    <script language="javascript" src="include/insert.js"></script>
<script type="text/javascript" language="javascript">

    function preEnter(fieldObj, canEdit) {
        fieldObj.style.backgroundColor = 'blue';
        fieldObj.style.color = 'white';
        if (canEdit == "no") {
            fieldObj.blur();
            leaveField(fieldObj);
        }

        enterField(fieldObj);
        return;
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

    function focusval(obj) {
        obj.style.backgroundColor = 'blue';
        obj.style.color = 'white';
    }
    function blurval(obj) {
        obj.style.backgroundColor = 'Window';
        obj.style.color = 'WindowText';
    }
   


</script>



</head>    
   <body>
    <form id="frmList" runat="server"  defaultfocus='cust_TextBox'>   
        <hd:header id="Header1" runat="server"></hd:header>
         <table width="100%"><tr><td><div>
        <table align="left" border="1" width="75%">
                <tr class="topheadcolor">                                      
                 
                        <td nowrap width="25px";>
                        <asp:ImageButton ID="img_btn_add" runat="server" Width="35px" ImageUrl="~/Images/add.bmp" ToolTip="Add" OnClick="img_btn_add_click" />
                        </td>                       
                        <td nowrap width="25px";>
                        <asp:ImageButton ID="img_btn_exit" runat="server" Width="35px" ImageUrl="~/Images/exit-au.bmp" ToolTip="LogOut" OnClick="hlnkLogOut_Click" />
                        </td>
                        <td nowrap> &nbsp;</td>
                </tr>
      </table></div>
        </td>
      </tr>
      <tr>
      <td>
    <div>
    <TABLE id="tblTop" cellSpacing="3" align="center" border="0" Width="100%">
        <TR>
          <TD width=30>&nbsp;</TD>
          <TD align=center nowrap><font size=+0><b>Vendors&nbsp;</b></font></TD>
          <TD >
            <asp:linkbutton id="hlkBackToMenu" OnClick="Back_tomenu_Click" runat="server" >Back to menu</asp:linkbutton>
          
            &nbsp;&nbsp;Logged as&nbsp;
            <asp:label id="lblUser" runat="server" Font-Bold="True">&nbsp;</asp:label>&nbsp;&nbsp;&nbsp;
            <asp:linkbutton id="hlnkLogOut" runat="server" OnClick="hlnkLogOut_Click">Log out</asp:linkbutton>
            &nbsp;&nbsp;<asp:hyperlink id="hlnkChangePwd" runat="server" NavigateUrl="changepwd.aspx"></asp:hyperlink>
            &nbsp;<b>Company:</b>&nbsp;  <asp:Label ID="labelcompany" runat="server" Text="Label"></asp:Label></TD>
          
         
          <TD vAlign="middle" width="20">&nbsp;</TD>
          
          <td width=30>&nbsp;</td>
        </TR>
      </TABLE>
      <table><tr bgcolor="gray"><td> <div  id="navigation" style="width:100%">
		<ul nowrap> <li >
      <asp:LinkButton ID="lnk_Listvend" runat="server" OnClick="Ink_vendorlist" >Brows Vendor</asp:LinkButton></li>
      <li><asp:LinkButton ID="lnk_viewvend" runat="server" OnClick="lnk_viewvend_Click" > View Vendor</asp:LinkButton></li>
      <li class="selected"><asp:LinkButton ID="lnk_listtot" runat="server" OnClick="lnk_listview_Click" >Totals</asp:LinkButton></li></ul></div>
      
      
      
      
      </td>
      </tr></table>
   
    <asp:FormView ID="FormView1" runat="server" DataSourceID="ObjectDataSource1">
        <EditItemTemplate>
        <asp:Panel ID="EditPanel" Width="600px" Height="120px" DefaultButton="UpdateButton" runat="server" >
                    
        <fieldset class="shade">
        <table>
        <tr><td nowrap align="right" style="padding-right:5px"><b>Vendor:</b></td>
        <td><asp:Label ID="vendLabel" Width="65" BackColor="Turquoise" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" runat="server" Text='<%# Bind("vend") %>' /></td>
            <td nowrap align="right" style="padding-right:5px"><b>Name:</b></td>
            <td><asp:Label ID="vendnameLabel" Width="250" BackColor="Turquoise" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" runat="server" Text='<%# Bind("vendname") %>' /></td></tr>
           </table> 
           </fieldset>
           <fieldset class="shade">
        <table>
        <tr><td></td><td nowrap align="right" style="padding-right:5px"><br /><b>Period of Date</b></td><td nowrap align="right" style="padding-right:5px"><br /><b>Year of Date</b></td><td nowrap align="right" style="padding-right:5px"><br /><b>Prior Date</b></td></tr>            
        <tr><td nowrap align="right" style="padding-right:5px"><b>Purchase:</b></td>
        <td><asp:TextBox ID="purchaseTextBox" Width="85" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" runat="server" Text='<%# Bind("purchase") %>' />
        <asp:CompareValidator ID="CompareValidator7" ControlToValidate="purchaseTextBox" SetFocusOnError="true" Display="Dynamic" Operator="DataTypeCheck" Type="Double" runat="server" ErrorMessage="Enter Valid Value"></asp:CompareValidator></td>
        <td nowrap align="right" style="padding-right:5px">
        <asp:TextBox ID="purchTextBox" Width="85" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" runat="server" Text='<%# Bind("purch") %>' />
        <asp:CompareValidator ID="CompareValidator1" ControlToValidate="purchTextBox" SetFocusOnError="true" Display="Dynamic" Operator="DataTypeCheck" Type="Double" runat="server" ErrorMessage="Enter Valid Value"></asp:CompareValidator></td>
        <td nowrap align="right" style="padding-right:5px">
        <asp:TextBox ID="lst_yrTextBox" Width="85" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" runat="server" Text='<%# Bind("lst_yr") %>' />
        <asp:CompareValidator ID="CompareValidator2" ControlToValidate="lst_yrTextBox" SetFocusOnError="true" Display="Dynamic" Operator="DataTypeCheck" Type="Double" runat="server" ErrorMessage="Enter Valid Value"></asp:CompareValidator></td></tr>
        <tr><td nowrap align="right" style="padding-right:5px"><b>Total MSF:</b></td>
        <td><asp:TextBox ID="tot_msfTextBox" Width="85" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" runat="server" Text='<%# Bind("tot_msf") %>' />
        <asp:CompareValidator ID="CompareValidator3" ControlToValidate="tot_msfTextBox" SetFocusOnError="true" Display="Dynamic" Operator="DataTypeCheck" Type="Double" runat="server" ErrorMessage="Enter Valid Value"></asp:CompareValidator></td>
        <td nowrap align="right" style="padding-right:5px">
        <asp:TextBox ID="ytd_msfTextBox" Width="85" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" runat="server" Text='<%# Bind("ytd_msf") %>' />
        <asp:CompareValidator ID="CompareValidator4" ControlToValidate="ytd_msfTextBox" SetFocusOnError="true" Display="Dynamic" Operator="DataTypeCheck" Type="Double" runat="server" ErrorMessage="Enter Valid Value"></asp:CompareValidator></td>
        <td nowrap align="right" style="padding-right:5px">
        <asp:TextBox ID="lyytd_msfTextBox" Width="85" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" runat="server" Text='<%# Bind("lyytd_msf") %>' />
        <asp:CompareValidator ID="CompareValidator5" ControlToValidate="lyytd_msfTextBox" SetFocusOnError="true" Display="Dynamic" Operator="DataTypeCheck" Type="Double" runat="server" ErrorMessage="Enter Valid Value"></asp:CompareValidator></td></tr>
        <tr><td nowrap align="right" style="padding-right:5px"><b><br />Hi Balance:</b></td>
        <td><br /><asp:TextBox ID="hibalTextBox" Width="85" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" runat="server" Text='<%# Bind("hibal") %>' />
        <asp:CompareValidator ID="CompareValidator6" ControlToValidate="hibalTextBox" SetFocusOnError="true" Display="Dynamic" Operator="DataTypeCheck" Type="Double" runat="server" ErrorMessage="Enter Valid Value"></asp:CompareValidator></td>
        <td nowrap align="right" style="padding-right:5px"><br /><b>On:</b></td>
        <td><br /><asp:TextBox ID="hibal_dateTextBox" Width="85" onfocus="this.select();javascript:preEnter( this, 'yes' );" onblur="javascript:preLeave( this, 'date', '99/99/9999' );" runat="server" Text='<%# Bind("hibal_date") %>' />
        <a href="#" onblur="document.getElementById('FormView1_hibal_dateTextBox').focus()"  tabindex="1" onClick="showCalendarControl(FormView1_hibal_dateTextBox); return false"><asp:Image ID="Image6" runat="server" ImageUrl="images/lookup_icon.gif" /></a></td>
        <td nowrap align="right" style="padding-right:5px"><br /><b>Total# of Inv. Paid</b></td>
        <td><br /><asp:TextBox ID="num_invTextBox" Width="40" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" runat="server" Text='<%# Bind("num_inv") %>' />
        <asp:CompareValidator ID="CompareValidator8" ControlToValidate="num_invTextBox" SetFocusOnError="true" Display="Dynamic" Operator="DataTypeCheck" Type="Integer" runat="server" ErrorMessage="Enter Valid Value"></asp:CompareValidator></td></tr>
        <tr><td nowrap align="right" style="padding-right:5px"><b>Last Payment:</b></td>
        <td><asp:TextBox ID="lpayTextBox" Width="85" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" runat="server" Text='<%# Bind("lpay") %>' />
        <asp:CompareValidator ID="CompareValidator9" ControlToValidate="lpayTextBox" SetFocusOnError="true" Display="Dynamic" Operator="DataTypeCheck" Type="Double" runat="server" ErrorMessage="Enter Valid Value"></asp:CompareValidator></td>
        <td nowrap align="right" style="padding-right:5px"><b>On:</b></td>
        <td><asp:TextBox ID="lpay_dateTextBox" Width="85" onfocus="this.select();javascript:preEnter( this, 'yes' );" onblur="javascript:preLeave( this, 'date', '99/99/9999' );" runat="server" Text='<%# Bind("lpay_date") %>' />
        <a href="#" onblur="document.getElementById('FormView1_lpay_dateTextBox').focus()"  tabindex="1" onClick="showCalendarControl(FormView1_lpay_dateTextBox); return false"><asp:Image ID="Image1" runat="server" ImageUrl="images/lookup_icon.gif" /></a></td>
        <td nowrap align="right" style="padding-right:5px"><b>Avg# Days to Paid:</b></td>
        <td><asp:TextBox ID="AVG_payTextBox" Width="40" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" runat="server" Text='<%# Bind("AVG_pay") %>' />
        <asp:CompareValidator ID="CompareValidator10" ControlToValidate="AVG_payTextBox" SetFocusOnError="true" Display="Dynamic" Operator="DataTypeCheck" Type="Integer" runat="server" ErrorMessage="Enter Valid Value"></asp:CompareValidator></td></tr>
        <tr><td nowrap align="right" style="padding-right:5px"><br /><b>Open Order Balance:</b></td>
        <td><br /><asp:TextBox ID="ordbalTextBox" Width="85" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" runat="server" Text='<%# Bind("ordbal") %>' />
        <asp:CompareValidator ID="CompareValidator11" ControlToValidate="ordbalTextBox" SetFocusOnError="true" Display="Dynamic" Operator="DataTypeCheck" Type="Double" runat="server" ErrorMessage="Enter Valid Value"></asp:CompareValidator></td>
        &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<td nowrap align="right" style="padding-right:5px"><br /><b>Account Balance:</b></td><td><br />
        <asp:TextBox   ID="acc_balTextBox" Width="85" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" runat="server" Text='<%# Bind("acc_bal") %>' />
        <asp:CompareValidator ID="CompareValidator12" ControlToValidate="acc_balTextBox" SetFocusOnError="true" Display="Dynamic" Operator="DataTypeCheck" Type="Double" runat="server" ErrorMessage="Enter Valid Value"></asp:CompareValidator></td></tr>
        <td nowrap align="right" style="padding-right:5px"><br /><b></b></td>
        <td><br /><asp:TextBox ID="reckeyTextBox" Visible="false" Width="85" runat="server" Text='<%# Bind("extra") %>' />
        </td>
        </table>   
            
           
            
            
            <br />
            <asp:Button ID="UpdateButton" runat="server" CausesValidation="True" OnClick="UpdateButton_Click" CssClass="button"  Text="Save" />
            &nbsp;<asp:Button ID="UpdateCancelButton" runat="server" 
                CausesValidation="False" CommandName="Cancel" CssClass="button" Text="Cancel" />
                </fieldset>
                </asp:Panel>
        </EditItemTemplate>
        
        <ItemTemplate>
            <fieldset class="shade">
        <table>
        <tr><td nowrap align="right" style="padding-right:5px"><b>Vendor:</b></td>
        <td><asp:Label ID="vendLabel" Width="65" BackColor="Turquoise" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" runat="server" Text='<%# Bind("vend") %>' /></td>
            <td nowrap align="right" style="padding-right:5px"><b>Name:</b></td>
            <td><asp:Label ID="vendnameLabel" Width="250" BackColor="Turquoise" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" runat="server" Text='<%# Bind("vendname") %>' /></td></tr>
           </table> 
           </fieldset>
           <fieldset class="shade">
        <table>
        <tr><td></td><td nowrap align="right" style="padding-right:5px"><br /><b>Period of Date</b></td><td nowrap align="right" style="padding-right:5px"><br /><b>Year of Date</b></td><td nowrap align="right" style="padding-right:5px"><br /><b>Prior Date</b></td></tr>            
        <tr><td nowrap align="right" style="padding-right:5px"><b>Purchase:</b></td>
        <td><asp:Label ID="purchaseLabel" Width="85" BackColor="Turquoise" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" runat="server" Text='<%# Bind("purchase") %>' /></td>
        <td nowrap align="right" style="padding-right:5px">
        <asp:Label ID="purchLabel" Width="85" BackColor="Turquoise" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" runat="server" Text='<%# Bind("purch") %>' /></td>
        <td nowrap align="right" style="padding-right:5px">
        <asp:Label ID="lst_yrLabel" Width="85" BackColor="Turquoise" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" runat="server" Text='<%# Bind("lst_yr") %>' /></td></tr>
        <tr><td nowrap align="right" style="padding-right:5px"><b>Total MSF:</b></td>
        <td><asp:Label ID="tot_msfLabel" Width="85" BackColor="Turquoise" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" runat="server" Text='<%# Bind("tot_msf") %>' /></td>
        <td nowrap align="right" style="padding-right:5px">
        <asp:Label ID="ytd_msfLabel" Width="85" BackColor="Turquoise" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" runat="server" Text='<%# Bind("ytd_msf") %>' /></td>
        <td nowrap align="right" style="padding-right:5px">
        <asp:Label ID="lyytd_msfLabel" Width="85" BackColor="Turquoise" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" runat="server" Text='<%# Bind("lyytd_msf") %>' /></td></tr>
        <tr><td nowrap align="right" style="padding-right:5px"><b><br />Hi Balance:</b></td>
        <td><br /><asp:Label ID="hibalLabel" Width="85" BackColor="Turquoise" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" runat="server" Text='<%# Bind("hibal") %>' /></td>
        <td nowrap align="right" style="padding-right:5px"><br /><b>On:</b></td>
        <td><br /><asp:Label ID="hibal_dateLabel" Width="85" BackColor="Turquoise" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" runat="server" Text='<%# Bind("hibal_date") %>' /></td>
        <td nowrap align="right" style="padding-right:5px"><br /><b>Total# of Inv. Paid</b></td>
        <td><br /><asp:Label ID="num_invLabel" Width="40" BackColor="Turquoise" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" runat="server" Text='<%# Bind("num_inv") %>' /></td></tr>
        <tr><td nowrap align="right" style="padding-right:5px"><b>Last Payment:</b></td>
        <td><asp:Label ID="lpayLabel" Width="85" BackColor="Turquoise" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" runat="server" Text='<%# Bind("lpay") %>' /></td>
        <td nowrap align="right" style="padding-right:5px"><b>On:</b></td>
        <td><asp:Label ID="lpay_dateLabel" Width="85" BackColor="Turquoise" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" runat="server" Text='<%# Bind("lpay_date") %>' /></td>
        <td nowrap align="right" style="padding-right:5px"><b>Avg# Days to Paid:</b></td>
        <td><asp:Label ID="AVG_payLabel" Width="40" BackColor="Turquoise" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" runat="server" Text='<%# Bind("AVG_pay") %>' /></td></tr>
        <tr><td nowrap align="right" style="padding-right:5px"><br /><b>Open Order Balance:</b></td>
        <td><br /><asp:Label ID="ordbalLabel" Width="85" BackColor="Turquoise" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" runat="server" Text='<%# Bind("ordbal") %>' /></td>
        <td nowrap align="right" style="padding-right:5px"><br /><b>Account Balance:</b></td>
        <td><br /><asp:Label ID="acc_balLabel" Width="85" BackColor="Turquoise" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" runat="server" Text='<%# Bind("acc_bal") %>' /></td></tr>
        <td><br /><asp:TextBox ID="reckeyTextBox" Width="85" Visible="false" runat="server" Text='<%# Bind("extra") %>' /></td>
        </table>
        <br />    
            <asp:Button ID="UpdateItemButton" runat="server" CssClass="button" CommandName="Edit" Text="Update" />
            
           
            
            
            
                </fieldset>
        </ItemTemplate>
    </asp:FormView>
    
    <asp:ObjectDataSource ID="ObjectDataSource1" runat="server" OldValuesParameterFormatString="original_{0}"
        SelectMethod="SelectVendTotal" TypeName="voucherpay">
        <SelectParameters>            
            <asp:Parameter Name="prmAction" DefaultValue="View"  Type="String"  />
            <asp:Parameter Name="prmUser"  Type="String" />            
            <asp:SessionParameter Name="prmRecKey" SessionField="vendor_list_vend_reckey" Type="String" />
            <asp:Parameter DefaultValue="" Name="prmvend" Type="String" />
            <asp:Parameter Name="prmVendName"  Type="String" />
            <asp:Parameter Name="prmpurch"  Type="Decimal" />
            <asp:Parameter Name="prmlst_yr" Type="Decimal"></asp:Parameter>
            <asp:Parameter Name="prmytd_msf" Type="Decimal" />
            <asp:Parameter Name="prmlyytd_msf" Type="Decimal" />
            <asp:Parameter Name="prmhibal" Type="Decimal" />
            <asp:Parameter Name="prmhibal_date"  Type="String" />
            <asp:Parameter Name="prmnum_inv" Type="Int32" />
            <asp:Parameter Name="prmlpay" Type="Decimal" />
            <asp:Parameter Name="prmlpay_date"  Type="String" />
            <asp:Parameter Name="prmAVG_pay" Type="Int32" />
            <asp:Parameter Name="prmacc_bal" Type="Decimal" />
            <asp:Parameter Name="prmpurchase" Type="Decimal" />
            <asp:Parameter Name="prmtot_msf" Type="Decimal" />
            <asp:Parameter Name="prmordbal" Type="Decimal" />
            <asp:Parameter Name="prmext"  Type="String" />
        </SelectParameters>
    </asp:ObjectDataSource>
</div>

</div></td></tr></table>
    <ft:footer id="Footer1" runat="server"></ft:footer>
    </form>
  </body>
</HTML>
