<%@ Page Language="c#" AutoEventWireup="true" Debug="true" Inherits="view_invoice" Codebehind="view_invoice.aspx.cs" %>
<%@ Register Src="footer.ascx" TagName="Footer" TagPrefix="ft" %>
<%@ Register Src="header.ascx" TagName="Header" TagPrefix="hd" %>
<html xmlns="http://www.w3.org/1999/xhtml" >
  <head id="Head1" runat="server">
    <title>Customer Invoices</title>
    <LINK href="include/style.css" type="text/css" rel="stylesheet"/>
    <LINK REL="stylesheet" TYPE="text/css" HREF="include/CalendarControl.css" >
    <script language = "JavaScript" src="include/CalendarControl.js"></script>
     <script language="javascript" src="include/date.js"></script>
    <script language="javascript" src="include/event.js"></script>
    <script language="javascript" src="include/insert.js"></script>
    <script language="javascript" src="include/validate2.js"></script>
    <script language =javascript>
    
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
            theForm.btnSearch.click();              
        }
    }
    function termslook() {
        var NewWindow = window.open("terms_lookup.aspx", "termsLookupWindow", "width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
    }

    function termsLookup(ReturnObj1, ReturnObj2) {
        document.forms[0].FormView1_termsTextBox.value = ReturnObj1;
        var terms = document.getElementById("FormView1_termsdescLabel");
        terms.innerHTML = ReturnObj2;
        //document.forms[0].FormView1_termsdescLabel.innerHtml = ReturnObj2;
        
    }
    function carrierlook() {
        var NewWindow = window.open("Carrier_lookup.aspx", "CarrierlookupWindow", "width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
    }
    function Carrierlookup(ReturnObj1, ReturnObj2) {
        document.forms[0].FormView1_carrierTextBox.value = ReturnObj1;
        //document.forms[0].FormView1_vdescarrierTextBox.value = ReturnObj2;
    }

    function currencylook() {
        var NewWindow = window.open("currency_lookup.aspx", "CurrencyLookupWindow", "width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
    }
    function CurrencyLookup(ReturnObj1, ReturnObj2) {
        document.forms[0].FormView1_vcurrcodeTextBox.value = ReturnObj1;
    }
    function taxcodelook() {
        var NewWindow = window.open("tax_lookup.aspx", "TaxLookupWindow", "width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
    }
    function TaxLookup(ReturnObj1, ReturnObj2) {
        document.forms[0].FormView1_taxcodeTextBox.value = ReturnObj1;
       
    }
    function ShipTOLook() {
        var lookHidden = document.getElementById("FormView1_custnoTextBox").value;
        var NewWindow = window.open("ShipIdCustLook.aspx?look=" + lookHidden + "", "ShipToLookupWindow", "width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
    }
    function ShipToLookup(ReturnObj1) {
        document.forms[0].FormView1_shipidTextBox.value = ReturnObj1;
    }
    function contactcustomerlook() {
        var NewWindow = window.open("contact_customer_lookup.aspx", "ContactCustomerLookupWindow", "width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
    }

    function ContactCustomerLookup(ReturnObj1, ReturnObj2, ReturnObj3, ReturnObj4, ReturnObj5, ReturnObj6, ReturnObj7, ReturnObj8, ReturnObj9, ReturnObj10, ReturnObj11) {
        document.forms[0].FormView1_custnoTextBox.value = ReturnObj1;
        document.forms[0].FormView1_custnoTextBox.onchange();
    }

    function getdecimal(obj, obj2) {
        if (obj.value.indexOf(".") != -1) {
            return;
        }
        else if (obj.value.length == obj2) {
            obj.value = obj.value + ".";
        }

    }
    var smanname = "";
    function salesreplook(sman) {
        smanname = sman;
        var NewWindow = window.open("salesrep_lookup.aspx", "SalesRepLookupWindow", "width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
    }
    function SalesRepLookup(ReturnObj1, ReturnObj2) {
        if(smanname == 1)
            document.forms[0].FormView2_sman1TextBox.value = ReturnObj1;
        if (smanname == 2)
            document.forms[0].FormView2_sman2TextBox.value = ReturnObj1;
        if (smanname == 3)
            document.forms[0].FormView2_sman3TextBox.value = ReturnObj1;

    }

    function AccountLook() {
       var NewWindow = window.open("accountlook.aspx", "AccountLookupWindow", "width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
   }

   function AccountLookup(ReturnObj1,ReturnObj2) {
       document.forms[0].FormView2_actnumTextBox.value = ReturnObj1;
       document.forms[0].FormView2_actdscrTextBox.value = ReturnObj2;
   }


   function orderhelp() {
       var NewWindow = window.open("ar_inv_help.aspx", "OrderHelpWindow", "width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
   }
   function printrep() {
       var NewWindow = window.open("topbtnorderreport.aspx", "OrderReport", "width=800,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
   }
   function printackrep() {
       var NewWindow = window.open("topprintorderack_report.aspx", "OrderAcknowledgementReport", "width=800,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
   }
   function ordernotes() {
       var NewWindow = window.open("toporder_list_notes.aspx", "OrderListNotes", "width=600,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
   }


    </script> 
  </head>    
   <body>
    <form id="frmList" runat="server"  defaultfocus='Inv_TextBox'>   
        <hd:header id="Header1" runat="server"></hd:header>
           <asp:ScriptManager ID="ScriptManager1" runat="server">
          </asp:ScriptManager>
         
         <table><tr><td><div> 
        <table align="left" border="1"  width="75%">
                <tr bgcolor="maroon">
                                        
                    <%-- <td nowrap width="1px";>
                        <a href="#" onClick="select_col(); return false"><asp:Image ID="Image1" Width="35px" runat="server" ImageUrl="~/Images/moveCol.ico" /></a>
                    </td>                  
                    <td nowrap width="25px";>
                        <a href="#" onClick="printspec(); return false"><asp:Image ID="Image6" Width="35px" runat="server" ImageUrl="~/Images/dict.ico" /></a>
                    </td>--%>
                        <td nowrap width="25px";>
                        <asp:ImageButton ID="img_btn_add" runat="server" Width="35px" ImageUrl="~/Images/add.bmp" ToolTip="Add" OnClick="img_btn_add_click" />
                        </td>
                        <td nowrap width="25px";>
                        <a href="#" onClick="orderhelp(); return false"><asp:Image ID="img_help" Width="35px" ToolTip="Help" runat="server" ImageUrl="~/Images/help.ico" /></a>
                        </td>
                        <td nowrap width="25px";>
                        <asp:ImageButton ID="img_btn_exit" runat="server" Width="35px" ImageUrl="~/Images/exit-au.bmp" ToolTip="LogOut" OnClick="img_btn_exit_click" />
                        </td>
                        <td nowrap> &nbsp;</td>
                </tr>
      </table>
        </div>   </td></tr>
        <tr><td>
        
      <div>
         
          <asp:HiddenField ID="HiddenField_oldinv" runat="server" />
      <TABLE id="tblTop" cellSpacing="3" align="center" border="0" Width="100%">
        <TR>         
          <TD align=left nowrap><font size=+0><b>Customer Invoices&nbsp;</b></font></TD>
          <TD >
            <asp:linkbutton id="hlkBackToMenu" OnClick="Back_tomenu_Click" runat="server" >Back to menu</asp:linkbutton>
          
            &nbsp;&nbsp;Logged as&nbsp;
            <asp:label id="lblUser" runat="server" Font-Bold="True">&nbsp;</asp:label>&nbsp;&nbsp;&nbsp;
            <asp:linkbutton id="hlnkLogOut" runat="server" OnClick="hlnkLogOut_Click">Log out</asp:linkbutton>
            &nbsp;&nbsp;<asp:hyperlink id="hlnkChangePwd" runat="server" NavigateUrl="changepwd.aspx"></asp:hyperlink>
            &nbsp;<b>Company:</b>&nbsp;  <asp:Label ID="labelcompany" runat="server" Text="Label"></asp:Label></TD>
          
         
          <TD vAlign="middle" width="20">&nbsp;</TD>
          
          <td width=30>&nbsp; </td>
        </TR>
      </TABLE>
      <table><tr bgcolor="gray"><td> <asp:LinkButton ID="lnk_Listcustomers" runat="server" OnClick="lnk_listinvoice" ><img src="Images/brws invoices 0.jpg" border="0" alt="List Invoices " /></asp:LinkButton>
      <asp:LinkButton ID="lnk_viewcustomers" runat="server" OnClick="lnk_viewcustomers_Click" > <img src="Images/view invoices 1.jpg" border="0" alt="View Invoices" /></asp:LinkButton>
            
      </td>
      </tr></table>
       <div>
           <asp:FormView ID="FormView1" runat="server" OnDataBound="FormView1_OnDataBound" DataSourceID="ObjectDataSource1">
               <EditItemTemplate><asp:Panel runat="server" ID="editpanel" DefaultButton="UpdateButton">
                    <fieldset class="shade">
                   <table>
                   <tr>
                   <td>
                   <table>
                   <tr><td align="right" style="padding-right:5px"><b>Customer #:</b></td>
                   <td>  <asp:TextBox ID="custnoTextBox" ReadOnly="true" onfocus="document.getElementById('FormView1_custnameTextBox').focus()" TabIndex="1" ForeColor="White" BackColor="#008284" Width="100px" runat="server" Text='<%# Bind("custno") %>' />                   </td>
                   <td colspan="2"> <asp:TextBox ID="custnameTextBox" Width="180px" runat="server" Text='<%# Bind("custname") %>' /></td></tr>
                   <tr><td align="right" style="padding-right:5px"><b>Ship To:</b></td>
                   <td> <asp:TextBox ID="shipidTextBox" Width="100px" runat="server" Text='<%# Bind("shipid") %>' />
                   <a href="#" tabindex="1" onClick="ShipTOLook(); return false"><asp:Image ID="Image5" runat="server" ImageUrl="images/lookup_icon.gif" /></a></td>
                   <td colspan="2"> <asp:Label ID="shipnameLabel" Width="180px" runat="server" Text='<%# Bind("shipname") %>' /></td></tr>
                   <tr><td align="right" style="padding-right:5px"><b>Invoice:</b></td>
                   <td><asp:TextBox ID="invnoTextBox" MaxLength="6" ForeColor="White" BackColor="#008284" Width="100px" runat="server" Text='<%# Bind("invno") %>' />
                   <asp:CompareValidator ID="CompareValidator11" ControlToValidate="invnoTextBox" Display="Dynamic" SetFocusOnError="true" Operator="DataTypeCheck" Type="Integer" runat="server" ErrorMessage="Invalid Entry"></asp:CompareValidator></td>
                   <td align="right" style="padding-right:5px"><b>Discount:</b></td>
                   <td> <asp:TextBox ID="discountTextBox" Width="80px" runat="server" Text='<%# Bind("discount") %>' />
                   <asp:CompareValidator ID="CompareValidator12" ControlToValidate="discountTextBox" Display="Dynamic" SetFocusOnError="true" Operator="DataTypeCheck" Type="Double" runat="server" ErrorMessage="Invalid Entry"></asp:CompareValidator>
                   </td></tr>
                   <tr><td align="right" style="padding-right:5px"><b>PO#:</b></td>
                   <td> <asp:TextBox ID="ponoTextBox" Width="100px" runat="server" Text='<%# Bind("pono") %>' /></td>
                   <td align="right" style="padding-right:5px"><b>Disc Days:</b></td>
                   <td><asp:TextBox ID="discdaysTextBox" Width="80px" runat="server" Text='<%# Bind("discdays") %>' />
                   <asp:CompareValidator ID="CompareValidator13" ControlToValidate="discdaysTextBox" Display="Dynamic" SetFocusOnError="true" Operator="DataTypeCheck" Type="Integer" runat="server" ErrorMessage="Invalid Entry"></asp:CompareValidator>
                   </td></tr>
                   <tr><td align="right" style="padding-right:5px"><b>Invoice Date:</b></td>
                   <td><asp:TextBox ID="invdateTextBox" Width="100px" onfocus="javascript:preEnter( this, 'yes' );" onblur="javascript:preLeave( this, 'date', '99/99/9999' );"  runat="server" Text='<%# Bind("invdate") %>' />
                   <a href="#" tabindex="1" onClick="showCalendarControl(document.getElementById('FormView1_invdateTextBox')); return false"><asp:Image ID="Image1" runat="server" ImageUrl="images/lookup_icon.gif" /></a></td>
                   <td align="right" style="padding-right:5px"><b>Carrier:</b></td>
                   <td><asp:TextBox ID="carrierTextBox" Width="80px" runat="server" Text='<%# Bind("carrier") %>' />
                   <a href="#" tabindex="1" onClick="carrierlook(); return false"><asp:Image ID="Image2" runat="server" ImageUrl="images/lookup_icon.gif" /></a></td></tr>
                   <tr><td align="right" style="padding-right:5px"><b>Due Date:</b></td>
                   <td><asp:TextBox ID="duedateTextBox" Width="100px" runat="server" onfocus="javascript:preEnter( this, 'yes' );" onblur="javascript:preLeave( this, 'date', '99/99/9999' );" Text='<%# Bind("duedate") %>' />
                   <a href="#" tabindex="1" onClick="showCalendarControl(document.getElementById('FormView1_duedateTextBox')); return false"><asp:Image ID="Image3" runat="server" ImageUrl="images/lookup_icon.gif" /></a></td></tr>
                   <tr><td align="right" style="padding-right:5px"><b>Tax Code:</b></td>
                   <td><asp:TextBox ID="taxcodeTextBox" Width="100px" runat="server" Text='<%# Bind("taxcode") %>' />
                   <a href="#" tabindex="1" onClick="taxcodelook(); return false"><asp:Image ID="Image4" runat="server" ImageUrl="images/lookup_icon.gif" /></a></td>   </tr>
                   <tr><td align="right" style="padding-right:5px"><b>Terms Code:</b></td>
                   <td><asp:TextBox ID="termsTextBox" Width="100px" runat="server" Text='<%# Bind("terms") %>' />
                   <a href="#" tabindex="1" onClick="termslook(); return false"><asp:Image ID="Image6" runat="server" ImageUrl="images/lookup_icon.gif" /></a></td>
                   <td colspan="2"> &nbsp;&nbsp;&nbsp;<asp:Label ID="termsdescLabel" Width="120px" runat="server" Text='<%# Bind("termsdesc") %>' /></td></tr>
                   <tr><td align="right" style="padding-right:5px"><b>Currency Code:</b></td>
                   <td> <asp:Label ID="currcodeTextBox" Width="100px" runat="server" Text='<%# Bind("currcode") %>' /></td>
                   <td align="right" style="padding-right:5px"><b>Exchange Rate:</b></td>
                   <td><asp:Label ID="exrateTextBox" Width="80px" runat="server" Text='<%# Bind("exrate") %>' /></td></tr>
                  
                   </table>
                   </td>
                   <td>
                   <table>
                   <tr><td align="right" style="padding-right:5px"><b>Invoice Amt:</b></td>
                   <td> <asp:Label ID="grossLabel" runat="server" Text='<%# Bind("gross") %>' /></td></tr>
                   <tr><td align="right" style="padding-right:5px"><b>Freight:</b></td>
                   <td> <asp:TextBox ID="freightTextBox" Width="80px" runat="server" Text='<%# Bind("freight") %>' />
                   <asp:CompareValidator ID="CompareValidator14" ControlToValidate="freightTextBox" Display="Dynamic" SetFocusOnError="true" Operator="DataTypeCheck" Type="Double" runat="server" ErrorMessage="Invalid Entry"></asp:CompareValidator>
                   </td></tr>
                   <tr><td align="right" style="padding-right:5px"><b>Tax:</b></td>
                   <td><asp:Label ID="taxamtLabel" runat="server" Text='<%# Bind("taxamt") %>' /></td></tr>
                   <tr><td colspan="2"><fieldset>
                   <table><tr><td align="right" style="padding-right:5px"><b>Discount:</b></td>
                   <td><asp:Label ID="disctakenLabel" runat="server" Text='<%# Bind("disctaken") %>' /></td></tr>
                   <tr><td align="right" style="padding-right:5px"><b>Amount Paid:</b></td>
                   <td> <asp:Label ID="paidLabel" runat="server" Text='<%# Bind("paid") %>' /></td></tr>
                   <tr><td align="right" style="padding-right:5px"><b>Balance Due:</b></td>
                   <td><asp:Label ID="baldueLabel" runat="server" Text='<%# Bind("baldue") %>' /></td></tr></table>
                   </fieldset></td>  </tr>
                   </table>
                   </td> </tr>
                   </table>
                  
                    <asp:Label ID="Label1" Visible="false" BackColor="Turquoise" Width="90" runat="server" Text='<%# Bind("reckey") %>' />
           <%--
                   gross:
                   <asp:TextBox ID="grossTextBox" runat="server" Text='<%# Bind("gross") %>' />
                   <br />
                   paid:
                   <asp:TextBox ID="paidTextBox" runat="server" Text='<%# Bind("paid") %>' />
                   <br />
                   baldue:
                   <asp:TextBox ID="baldueTextBox" runat="server" Text='<%# Bind("baldue") %>' />
                   <br />
                   taxamt:
                   <asp:TextBox ID="taxamtTextBox" runat="server" Text='<%# Bind("taxamt") %>' />
                   <br />
                   due:
                   <asp:TextBox ID="dueTextBox" runat="server" Text='<%# Bind("due") %>' />
                   <br />--%>
                   
                   <asp:Button ID="UpdateButton" runat="server" CausesValidation="True" CssClass="button" OnClick="UpdateButton_Click" Text="Save" />
                   &nbsp;<asp:Button ID="UpdateCancelButton" runat="server" CausesValidation="False" CssClass="button" CommandName="Cancel" Text="Cancel" />
                   </fieldset></asp:Panel>
               </EditItemTemplate>
               <InsertItemTemplate>
               <asp:Panel ID="Insertpanal" runat="server" DefaultButton="InsertButton">
               <fieldset class="shade">
               <table>
                   <tr>
                   <td>
                   <table>
                   <tr><td align="right" style="padding-right:5px"><b>Customer #:</b></td>
                   <td>  <asp:TextBox ID="custnoTextBox" Width="100px" OnTextChanged="custext_Change" AutoPostBack="true" ForeColor="White" BackColor="#008284" runat="server" Text='<%# Bind("custno") %>' />
                   <a href="#" onClick="contactcustomerlook(); return false"><asp:Image ID="Image5" runat="server" ImageUrl="images/lookup_icon.gif" /></a></td>
                   <td colspan="2"> <asp:TextBox ID="custnameTextBox" Width="180px" runat="server" Text='<%# Bind("custname") %>' /></td></tr>
                   <tr><td align="right" style="padding-right:5px"><b>Ship To:</b></td>
                   <td> <asp:TextBox ID="shipidTextBox" Width="100px" runat="server" Text='<%# Bind("shipid") %>' />
                   <a href="#" tabindex="1" onClick="ShipTOLook(); return false"><asp:Image ID="Image10" runat="server" ImageUrl="images/lookup_icon.gif" /></a></td>
                   <td colspan="2"> <asp:Label ID="shipnameLabel" Width="180px" runat="server" Text='<%# Bind("shipname") %>' /></td></tr>
                   <tr><td align="right" style="padding-right:5px"><b>Invoice:</b></td>
                   <td><asp:TextBox ID="invnoTextBox" Width="100px" ForeColor="White" BackColor="#008284" runat="server" Text='<%# Bind("invno") %>' />
                   <asp:CompareValidator ID="CompareValidator14" ControlToValidate="invnoTextBox" Display="Dynamic" SetFocusOnError="true" Operator="DataTypeCheck" Type="Integer" runat="server" ErrorMessage="Invalid Entry"></asp:CompareValidator>
                   </td>
                   <td align="right" style="padding-right:5px"><b>Discount:</b></td>
                   <td> <asp:TextBox ID="discountTextBox" Width="80px" runat="server" Text='<%# Bind("discount") %>' />
                   <asp:CompareValidator ID="CompareValidator15" ControlToValidate="discountTextBox" Display="Dynamic" SetFocusOnError="true" Operator="DataTypeCheck" Type="Double" runat="server" ErrorMessage="Invalid Entry"></asp:CompareValidator>
                   </td></tr>
                   <tr><td align="right" style="padding-right:5px"><b>PO#:</b></td>
                   <td> <asp:TextBox ID="ponoTextBox" Width="100px" runat="server" Text='<%# Bind("pono") %>' /></td>
                   <td align="right" style="padding-right:5px"><b>Disc Days:</b></td>
                   <td><asp:TextBox ID="discdaysTextBox" Width="80px" runat="server" Text='<%# Bind("discdays") %>' />
                   <asp:CompareValidator ID="CompareValidator16" ControlToValidate="discdaysTextBox" Display="Dynamic" SetFocusOnError="true" Operator="DataTypeCheck" Type="Integer" runat="server" ErrorMessage="Invalid Entry"></asp:CompareValidator>
                   </td></tr>
                   <tr><td align="right" style="padding-right:5px"><b>Invoice Date:</b></td>
                   <td><asp:TextBox ID="invdateTextBox" Width="100px" onfocus="javascript:preEnter( this, 'yes' );" onblur="javascript:preLeave( this, 'date', '99/99/9999' );" runat="server" Text='<%# Bind("invdate") %>' />
                   <a href="#" tabindex="1" onClick="showCalendarControl(document.getElementById('FormView1_invdateTextBox')); return false"><asp:Image ID="Image1" runat="server" ImageUrl="images/lookup_icon.gif" /></a></td>
                   <td align="right" style="padding-right:5px"><b>Carrier:</b></td>
                   <td><asp:TextBox ID="carrierTextBox" Width="80px" runat="server" Text='<%# Bind("carrier") %>' />
                   <a href="#" tabindex="1" onClick="carrierlook(); return false"><asp:Image ID="Image2" runat="server" ImageUrl="images/lookup_icon.gif" /></a></td></tr>
                   <tr><td align="right" style="padding-right:5px"><b>Due Date:</b></td>
                   <td><asp:TextBox ID="duedateTextBox" Width="100px" onfocus="javascript:preEnter( this, 'yes' );" onblur="javascript:preLeave( this, 'date', '99/99/9999' );" runat="server" Text='<%# Bind("duedate") %>' />
                   <a href="#" tabindex="1" onClick="showCalendarControl(document.getElementById('FormView1_duedateTextBox')); return false"><asp:Image ID="Image7" runat="server" ImageUrl="images/lookup_icon.gif" /></a></td></tr>
                   <tr><td align="right" style="padding-right:5px"><b>Tax Code:</b></td>
                   <td><asp:TextBox ID="taxcodeTextBox" Width="100px" runat="server" Text='<%# Bind("taxcode") %>' />
                   <a href="#" tabindex="1" onClick="taxcodelook(); return false"><asp:Image ID="Image8" runat="server" ImageUrl="images/lookup_icon.gif" /></a></td>   </tr>
                   <tr><td align="right" style="padding-right:5px"><b>Terms Code:</b></td>
                   <td><asp:TextBox ID="termsTextBox" Width="100px" runat="server" Text='<%# Bind("terms") %>' />
                   <a href="#" tabindex="1" onClick="termslook(); return false"><asp:Image ID="Image9" runat="server" ImageUrl="images/lookup_icon.gif" /></a></td>
                   <td colspan="2"> <asp:Label ID="termsdescLabel" Width="120px" runat="server" Text='<%# Bind("termsdesc") %>' /></td></tr>
                   <tr><td align="right" style="padding-right:5px"><b>Currency Code:</b></td>
                   <td> <asp:Label ID="currcodeTextBox" Width="100px" runat="server" Text='<%# Bind("currcode") %>' /></td>
                   <td align="right" style="padding-right:5px"><b>Exchange Rate:</b></td>
                   <td><asp:Label ID="exrateTextBox" Width="80px" runat="server" Text='<%# Bind("exrate") %>' /></td></tr>
                  
                   </table>
                   </td>
                   <td>
                   <table>
                   <tr><td align="right" style="padding-right:5px"><b>Invoice Amt:</b></td>
                   <td> <asp:Label ID="grossLabel" runat="server" Text='<%# Bind("gross") %>' /></td></tr>
                   <tr><td align="right" style="padding-right:5px"><b>Freight:</b></td>
                   <td> <asp:TextBox ID="freightTextBox" Width="80px" runat="server" Text='<%# Bind("freight") %>' />
                   <asp:CompareValidator ID="CompareValidator17" ControlToValidate="freightTextBox" Display="Dynamic" SetFocusOnError="true" Operator="DataTypeCheck" Type="Double" runat="server" ErrorMessage="Invalid Entry"></asp:CompareValidator>
                   </td></tr>
                   <tr><td align="right" style="padding-right:5px"><b>Tax:</b></td>
                   <td><asp:Label ID="taxamtLabel" runat="server" Text='<%# Bind("taxamt") %>' /></td></tr>
                   <tr><td colspan="2"><fieldset>
                   <table><tr><td align="right" style="padding-right:5px"><b>Discount:</b></td>
                   <td><asp:Label ID="disctakenLabel" runat="server" Text='<%# Bind("disctaken") %>' /></td></tr>
                   <tr><td align="right" style="padding-right:5px"><b>Amount Paid:</b></td>
                   <td> <asp:Label ID="paidLabel" runat="server" Text='<%# Bind("paid") %>' /></td></tr>
                   <tr><td align="right" style="padding-right:5px"><b>Balance Due:</b></td>
                   <td><asp:Label ID="baldueLabel" runat="server" Text='<%# Bind("baldue") %>' /></td></tr></table>
                   </fieldset></td>  </tr>
                   </table>
                   </td> </tr>
                   </table>
               
                   
                   
                  
                  <asp:Label ID="Label1" Visible="false" BackColor="Turquoise" Width="90" runat="server" Text='<%# Bind("reckey") %>' />
                  
                   <asp:Button ID="InsertButton" runat="server" CssClass="button" CausesValidation="True" OnClick="InsertButton_Click" Text="Save" />
                   &nbsp;<asp:Button ID="InsertCancelButton" runat="server" CssClass="button" CausesValidation="False" OnClick="CancelButton_Delete" CommandName="Cancel" Text="Cancel" />
                   </fieldset></asp:Panel>
               </InsertItemTemplate>
               <ItemTemplate>
               <asp:Panel ID="itempanelformview1" runat="server"  DefaultButton="UpdateButton">
                    <fieldset class="shade">
                   <table>
                   <tr>
                   <td>
                   <table>
                   <tr><td align="right" style="padding-right:5px"><b>Customer #:</b></td>
                   <td> <asp:Label ID="custnoLabel" BackColor="Turquoise" Width="100" runat="server" Text='<%# Bind("custno") %>' /></td>
                   <td colspan="2"> <asp:Label ID="custnameLabel" BackColor="Turquoise" Width="200" runat="server" Text='<%# Bind("custname") %>' /></td></tr>
                   <tr><td align="right" style="padding-right:5px"><b>Ship To:</b></td>
                   <td> <asp:Label ID="shipidLabel" BackColor="Turquoise" Width="100" runat="server" Text='<%# Bind("shipid") %>' /></td>
                   <td colspan="2"> <asp:Label ID="shipnameLabel" BackColor="Turquoise" Width="200" runat="server" Text='<%# Bind("shipname") %>' /></td></tr>
                   <tr><td align="right" style="padding-right:5px"><b>Invoice:</b></td>
                   <td><asp:Label ID="invnoLabel" runat="server" BackColor="Turquoise" Width="100" Text='<%# Bind("invno") %>' /></td>
                   <td align="right" style="padding-right:5px"><b>Discount:</b></td>
                   <td><asp:Label ID="discountLabel" runat="server" BackColor="Turquoise" Width="100" Text='<%# Bind("discount") %>' /></td></tr>
                   <tr><td align="right" style="padding-right:5px"><b>PO#:</b></td>
                   <td><asp:Label ID="ponoLabel" runat="server" BackColor="Turquoise" Width="100" Text='<%# Bind("pono") %>' /></td>
                   <td align="right" style="padding-right:5px"><b>Disc Days:</b></td>
                   <td><asp:Label ID="discdaysLabel" runat="server" BackColor="Turquoise" Width="100" Text='<%# Bind("discdays") %>' /></td></tr>
                   <tr><td align="right" style="padding-right:5px"><b>Invoice Date:</b></td>
                   <td><asp:Label ID="invdateLabel" runat="server" BackColor="Turquoise" Width="100" Text='<%# Bind("invdate") %>' /></td>
                   <td align="right" style="padding-right:5px"><b>Carrier:</b></td>
                   <td><asp:Label ID="carrierLabel" runat="server" BackColor="Turquoise" Width="100" Text='<%# Bind("carrier") %>' /></td></tr>
                   <tr><td align="right" style="padding-right:5px"><b>Due Date:</b></td>
                   <td><asp:Label ID="duedateLabel" runat="server" BackColor="Turquoise" Width="100" Text='<%# Bind("duedate") %>' /></td></tr>
                   <tr><td align="right" style="padding-right:5px"><b>Tax Code:</b></td>
                   <td><asp:Label ID="taxcodeLabel" runat="server" BackColor="Turquoise" Width="100" Text='<%# Bind("taxcode") %>' /></td>   </tr>
                   <tr><td align="right" style="padding-right:5px"><b>Terms Code:</b></td>
                   <td><asp:Label ID="termsLabel" runat="server" BackColor="Turquoise" Width="100" Text='<%# Bind("terms") %>' /></td>
                   <td colspan="2"> <asp:Label ID="termsdescLabel" BackColor="Turquoise" Width="100" runat="server" Text='<%# Bind("termsdesc") %>' /></td></tr>
                   <tr><td align="right" style="padding-right:5px"><b>Currency Code:</b></td>
                   <td><asp:Label ID="currcodeLabel" runat="server" BackColor="Turquoise" Width="100" Text='<%# Bind("currcode") %>' /></td>
                   <td align="right" style="padding-right:5px"><b>Exchange Rate:</b></td>
                   <td><asp:Label ID="exrateLabel" runat="server" BackColor="Turquoise" Width="100" Text='<%# Bind("exrate") %>' /></td></tr>
                  
                   </table>
                   </td>
                   <td>
                   <table>
                   <tr><td align="right" style="padding-right:5px"><b>Invoice Amt:</b></td>
                   <td> <asp:Label ID="grossLabel" BackColor="Turquoise" Width="100" runat="server" Text='<%# Bind("gross") %>' /></td></tr>
                   <tr><td align="right" style="padding-right:5px"><b>Freight:</b></td>
                   <td><asp:Label ID="freightLabel" BackColor="Turquoise" Width="100" runat="server" Text='<%# Bind("freight") %>' /></td></tr>
                   <tr><td align="right" style="padding-right:5px"><b>Tax:</b></td>
                   <td><asp:Label ID="taxamtLabel" BackColor="Turquoise" Width="100" runat="server" Text='<%# Bind("taxamt") %>' /></td></tr>
                   <tr><td colspan="2"><fieldset>
                   <table><tr><td align="right" style="padding-right:5px"><b>Discount:</b></td>
                   <td><asp:Label ID="disctakenLabel" BackColor="Turquoise" Width="90" runat="server" Text='<%# Bind("disctaken") %>' /></td></tr>
                   <tr><td align="right" style="padding-right:5px"><b>Amount Paid:</b></td>
                   <td> <asp:Label ID="paidLabel" BackColor="Turquoise" Width="90" runat="server" Text='<%# Bind("paid") %>' /></td></tr>
                   <tr><td align="right" style="padding-right:5px"><b>Balance Due:</b></td>
                   <td><asp:Label ID="baldueLabel" BackColor="Turquoise" Width="90" runat="server" Text='<%# Bind("baldue") %>' /></td></tr></table>
                   </fieldset></td>  </tr>
                   </table>
                   </td> </tr>
                   </table>
                 
                   <asp:Label ID="Label1" Visible="false" BackColor="Turquoise" Width="90" runat="server" Text='<%# Bind("reckey") %>' />
                   
                   <asp:Button ID="AddButton" runat="server" CssClass="button"  CommandName="new" Text="Add" />
                    <asp:Button ID="UpdateButton" runat="server" CssClass="button" CommandName="edit" Text="Update" />
                   &nbsp;<asp:Button ID="DeleteButton" runat="server" CssClass="button" OnClientClick="return confirm('Are you sure you want to delete this record')" OnClick="delete_Button_Click"  Text="Delete" />
                   </fieldset></asp:Panel>
               </ItemTemplate>
           </asp:FormView>
           
           <asp:ObjectDataSource ID="ObjectDataSource1" runat="server" 
               OldValuesParameterFormatString="original_{0}" SelectMethod="SelectCustInv" 
               TypeName="contact">
               <SelectParameters>
                   <asp:Parameter DefaultValue="View" Name="prmAction" Type="String" />
                   <asp:Parameter Name="prmComp" Type="String" />
                   <asp:Parameter Name="prmUser" Type="String" />
                   <asp:Parameter Name="prmCust" Type="String" />
                   <asp:Parameter DefaultValue="" Name="prmInv" Type="Int32" />
                    <asp:Parameter Name="prmOldInv" Type="Int32" />
                   <asp:Parameter Name="prmPosted" Type="String" />
                   <asp:Parameter Name="prmCustname" Type="String" />
                   <asp:Parameter Name="prmInvdate" Type="String" />
                   <asp:Parameter Name="prmGross" Type="Decimal" />
                   <asp:Parameter Name="prmPaid" Type="Decimal" />
                   <asp:Parameter Name="prmBaldue" Type="Decimal" />
                   <asp:Parameter Name="prmTaxamt" Type="Decimal" />
                   <asp:Parameter Name="prmDue" Type="Decimal" />
                   <asp:Parameter Name="prmShipid" Type="String" />
                   <asp:Parameter Name="prmShipname" Type="String" />
                   <asp:Parameter Name="prmPono" Type="String" />
                   <asp:Parameter Name="prmDuedate" Type="String" />
                   <asp:Parameter Name="prmTaxcode" Type="String" />
                   <asp:Parameter Name="prmTerms" Type="String" />
                   <asp:Parameter Name="prmTermsdesc" Type="String" />
                   <asp:Parameter Name="prmDiscount" Type="Decimal" />
                   <asp:Parameter Name="prmDisctaken" Type="Decimal" />
                   <asp:Parameter Name="prmDiscdays" Type="Int32" />
                   <asp:Parameter Name="prmCarrier" Type="String" />
                   <asp:Parameter Name="prmFreight" Type="Decimal" />
                   <asp:Parameter Name="prmCurrcode" Type="String" />
                   <asp:Parameter Name="prmExrate" Type="Decimal" />
                    <asp:SessionParameter SessionField="cust_invoice_reckey_rec" Name="prmReckey" Type="String" />
               </SelectParameters>
           </asp:ObjectDataSource>
       </div>    
       <div>
       <br />
        <asp:GridView ID="GridView1" runat="server" AllowPaging="True" AllowSorting="True" OnSelectedIndexChanged="GridView1_SelectedIndex"
        AutoGenerateColumns="False" CssClass="Grid" DataSourceID="ObjectDataSource2"  DataKeyNames="reckey"
        EmptyDataText="No Record Found"  Width="730px">
        <EmptyDataRowStyle BorderColor="Gray" BorderStyle="Dotted" BorderWidth="0px" Font-Bold="True"
            HorizontalAlign="Center" VerticalAlign="Middle" />
        <RowStyle CssClass="shade" />
        <Columns>
               <asp:CommandField ShowSelectButton="true" ButtonType="image" SelectImageUrl="~/Images/sel.gif" SelectText="" > 
                    <ItemStyle Width="10px" />
                    </asp:CommandField>
                    
            <asp:BoundField DataField="arline" HeaderText="Line"  SortExpression="arline" />
            <asp:BoundField DataField="actnum" ItemStyle-Wrap="false" HeaderText="Account Number"  SortExpression="actnum" />
            <asp:BoundField DataField="actdscr" ItemStyle-Wrap="false" HeaderText="Account Desc"  SortExpression="actdscr" />
            <asp:BoundField DataField="i-name" ItemStyle-Wrap="false" HeaderText="Item Name" SortExpression="i-name" />
            <asp:BoundField DataField="i-dscr" ItemStyle-Wrap="false" HeaderText="Item Desc" SortExpression="i-dscr" />
            <asp:BoundField DataField="lot-no" ItemStyle-Wrap="false" HeaderText="Customer Lot#" SortExpression="lot-no" />
            <asp:BoundField DataField="inv-qty" HeaderText="Invoice Qty" SortExpression="inv-qty" />
            <asp:BoundField DataField="cons-uom" HeaderText="Cons Uom"  SortExpression="cons-uom" />
            <asp:BoundField DataField="sf-sht" HeaderText="SqFt"  SortExpression="sf-sht" />
            <asp:BoundField DataField="unit-pr" HeaderText="Price"   SortExpression="unit-pr" />
            <asp:BoundField DataField="pr-qty-uom" HeaderText="UOM"   SortExpression="pr-qty-uom" />
                <asp:BoundField DataField="disc" HeaderText="Dsct%"  SortExpression="disc" />
            <asp:BoundField DataField="cal-amt" HeaderText="Amount"  SortExpression="cal-amt" />
            <asp:BoundField DataField="amt-msf" HeaderText="Amt MSF"  SortExpression="amt-msf" />
            <asp:BoundField DataField="cost" HeaderText="Cost" SortExpression="cost" />
            <asp:BoundField DataField="dscr1" HeaderText="Cost UOM" SortExpression="dscr1" />
            <asp:BoundField DataField="sman1" HeaderText="SlsRep" SortExpression="sman1" />
            <asp:BoundField DataField="s-pct1" HeaderText="% of Sales"  SortExpression="s-pct1" />
            <asp:BoundField DataField="s-comm1" HeaderText="Comm%" SortExpression="s-comm1" />
            <asp:BoundField DataField="sman2" HeaderText="SlsRep" SortExpression="sman2" />
            <asp:BoundField DataField="s-pct2" HeaderText="% of Sales"   SortExpression="s-pct2" />
            <asp:BoundField DataField="s-comm2" HeaderText="Comm%" SortExpression="s-comm2" />
            <asp:BoundField DataField="sman3" HeaderText="SlsRep" SortExpression="sman3" />
            <asp:BoundField DataField="s-pct3" HeaderText="% of Sales" SortExpression="s-pct3" />
            <asp:BoundField DataField="s-comm3" HeaderText="Comm%" SortExpression="s-comm3" />
           
            <asp:TemplateField HeaderText="Reckey" Visible="false" >
            <ItemTemplate>
            <asp:Label ID="reclabel" runat="server" Text='<%# Bind("[reckey]") %>'></asp:Label>
            </ItemTemplate>
            </asp:TemplateField>
        </Columns>
        <SelectedRowStyle CssClass="GridSelected" BackColor="Yellow" />
        <HeaderStyle BackColor="Teal" CssClass="gridrowhdr" ForeColor="White" HorizontalAlign="Center"
            VerticalAlign="Middle" Wrap="False" />
        <AlternatingRowStyle CssClass="GridItemOdd" />
    </asp:GridView><br />
    
    
    
           <asp:FormView ID="FormView2" runat="server" OnDataBound="FormView2_OnDataBound" DataSourceID="ObjectDataSource3">
               <EditItemTemplate>
                   <asp:Panel ID="EditPanel" Width="1000px" Height="120px" DefaultButton="UpdateButton" runat="server" >
                    
                   <fieldset class="shade">
                   <table>
                   <tr>
                   <td><b>Line:</b></td><td><b>Account Number:</b></td><td><b>Account Desc:</b></td><td><b>Item Name:</b></td>
                   <td><b>Item Desc:</b></td><td><b>Customer Lot#:</b></td><td><b>Invoice Qty:</b></td><td><b>Cons Uom:</b></td>
                   <td><b>SqFt:</b></td><td><b>Price:</b></td><td><b>UOM:</b></td><td><b>Dsct%:</b></td><td><b>Amount:</b></td>
                   <td><b>Amt MSF:</b></td><td><b>Cost:</b></td><td><b>Cost UOM:</b></td><td><b>SlsRep:</b></td><td><b>% of Sales:</b></td><td><b>Comm%:</b></td>
                   <td><b>SlsRep:</b></td><td><b>% of Sales:</b></td><td><b>Comm%:</b></td><td><b>SlsRep:</b></td><td><b>% of Sales:</b></td><td><b>Comm%:</b></td>
                   </tr>
                   <tr>
                   <td  nowrap><asp:TextBox ID="arlineTextBox" runat="server" Width="50" Text='<%# Bind("arline") %>' />
                   <asp:CompareValidator ID="CompareValidator11" ControlToValidate="arlineTextBox" Display="Dynamic" SetFocusOnError="true" Operator="DataTypeCheck" Type="Integer" runat="server" ErrorMessage="Invalid Entry"></asp:CompareValidator></td>                                      
                   <td nowrap><asp:TextBox ID="actnumTextBox" Width="80" runat="server" Text='<%# Bind("actnum") %>' />
                   <a href="#" tabindex="1" onClick="AccountLook(); return false"><asp:Image ID="Image13" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
                   </td><td>
                   <asp:TextBox ID="actdscrTextBox" runat="server" BackColor="Turquoise" onfocus="document.getElementById('FormView2_i_nameTextBox').focus()" Width="100" Text='<%# Bind("actdscr") %>' />
                   </td><td>
                   <asp:TextBox ID="i_nameTextBox" runat="server" Width="100" Text='<%# Bind("[i-name]") %>' />
                   </td><td>
                   <asp:TextBox ID="i_dscrTextBox" runat="server" Width="100" Text='<%# Bind("[i-dscr]") %>' />
                   </td><td>
                   <asp:TextBox ID="lot_noTextBox" runat="server" Width="100" Text='<%# Bind("[lot-no]") %>' />
                   </td><td nowrap>
                   <asp:TextBox ID="inv_qtyTextBox" runat="server" MaxLength="10" onkeyup="getdecimal(this,8);extractNumber(this,1,true);"   onkeypress="return blockNonNumbers(this, event, true, true);"  Width="80" Text='<%# Bind("[inv-qty]") %>' />
                   <asp:CompareValidator ID="CompareValidator2" ControlToValidate="inv_qtyTextBox" Display="Dynamic" SetFocusOnError="true" Operator="DataTypeCheck" Type="Double" runat="server" ErrorMessage="Invalid Entry"></asp:CompareValidator>
                   </td><td>
                   <asp:TextBox ID="cons_uomTextBox" runat="server" BackColor="Turquoise" onfocus="document.getElementById('FormView2_sf_shtTextBox').focus()" Width="40"  Text='<%# Bind("[cons-uom]") %>' />
                   </td><td nowrap>
                   <asp:TextBox ID="sf_shtTextBox" runat="server" MaxLength="8" onkeyup="getdecimal(this,5);extractNumber(this,2,true);"   onkeypress="return blockNonNumbers(this, event, true, true);" Width="60" Text='<%# Bind("[sf-sht]") %>' />
                   <asp:CompareValidator ID="CompareValidator9" ControlToValidate="sf_shtTextBox" Display="Dynamic" SetFocusOnError="true" Operator="DataTypeCheck" Type="Double" runat="server" ErrorMessage="Invalid Entry"></asp:CompareValidator>
                   </td><td nowrap>
                   <asp:TextBox ID="unit_prTextBox" runat="server" MaxLength="11" onkeyup="getdecimal(this,8);extractNumber(this,2,true);"   onkeypress="return blockNonNumbers(this, event, true, true);" Width="60" Text='<%# Bind("[unit-pr]") %>' />
                   <asp:CompareValidator ID="CompareValidator10" ControlToValidate="unit_prTextBox" Display="Dynamic" SetFocusOnError="true" Operator="DataTypeCheck" Type="Double" runat="server" ErrorMessage="Invalid Entry"></asp:CompareValidator>
                   </td><td>
                   <asp:TextBox ID="pr_qty_uomTextBox" runat="server" onblur="window.scroll(1000,1000)" Width="30" Text='<%# Bind("[pr-qty-uom]") %>' />
                   </td><td>
                   <asp:TextBox ID="discTextBox" runat="server" BackColor="Turquoise" onfocus="document.getElementById('FormView2_costTextBox').focus()" Width="80" Text='<%# Bind("disc") %>' />
                   </td><td>
                   <asp:TextBox ID="cal_amtTextBox" runat="server" BackColor="Turquoise" onfocus="document.getElementById('FormView2_costTextBox').focus()" Width="70" onblur="window.scroll(1000,1000)" Text='<%# Bind("[cal-amt]") %>' />
                   </td><td>
                   <asp:TextBox ID="amt_msfTextBox" runat="server" BackColor="Turquoise" onfocus="document.getElementById('FormView2_costTextBox').focus()" Width="70" Text='<%# Bind("[amt-msf]") %>' />
                   </td><td nowrap>
                   <asp:TextBox ID="costTextBox" runat="server" Width="50" onkeyup="getdecimal(this,9);extractNumber(this,2,true);"   onkeypress="return blockNonNumbers(this, event, true, true);" Text='<%# Bind("cost") %>' />
                   <asp:CompareValidator ID="CompareValidator1" ControlToValidate="costTextBox" Display="Dynamic" SetFocusOnError="true" Operator="DataTypeCheck" Type="Double" runat="server" ErrorMessage="Invalid Entry"></asp:CompareValidator>
                   </td><td>
                   <asp:TextBox ID="dscr1TextBox" runat="server" Width="50" Text='<%# Bind("dscr1") %>' />
                   </td><td nowrap>
                   <asp:TextBox ID="sman1TextBox" runat="server" Width="50" Text='<%# Bind("sman1") %>' />
                   <a href="#" tabindex="1" onClick="salesreplook(1); return false"><asp:Image ID="Image12" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
                   </td><td nowrap>
                   <asp:TextBox ID="s_pct1TextBox" runat="server" Width="70" MaxLength="6" onkeyup="getdecimal(this,3);extractNumber(this,2,true);"   onkeypress="return blockNonNumbers(this, event, true, true);" Text='<%# Bind("[s-pct1]") %>' />
                   <asp:CompareValidator ID="CompareValidator3" ControlToValidate="s_pct1TextBox" Display="Dynamic" SetFocusOnError="true" Operator="DataTypeCheck" Type="Double" runat="server" ErrorMessage="Invalid Entry"></asp:CompareValidator>
                   </td><td nowrap>
                   <asp:TextBox ID="s_comm1TextBox" runat="server" Width="50" MaxLength="6" onkeyup="getdecimal(this,3);extractNumber(this,2,true);"   onkeypress="return blockNonNumbers(this, event, true, true);" Text='<%# Bind("[s-comm1]") %>' />
                   <asp:CompareValidator ID="CompareValidator4" ControlToValidate="s_comm1TextBox" Display="Dynamic" SetFocusOnError="true" Operator="DataTypeCheck" Type="Double" runat="server" ErrorMessage="Invalid Entry"></asp:CompareValidator>
                   </td><td nowrap>
                   <asp:TextBox ID="sman2TextBox" runat="server" Width="50" Text='<%# Bind("sman2") %>' />
                   <a href="#" tabindex="1" onClick="salesreplook(2); return false"><asp:Image ID="Image11" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
                   </td><td nowrap>
                   <asp:TextBox ID="s_pct2TextBox" runat="server" Width="70" MaxLength="6" onkeyup="getdecimal(this,3);extractNumber(this,2,true);"   onkeypress="return blockNonNumbers(this, event, true, true);" Text='<%# Bind("[s-pct2]") %>' />
                   <asp:CompareValidator ID="CompareValidator5" ControlToValidate="s_pct2TextBox" Display="Dynamic" SetFocusOnError="true" Operator="DataTypeCheck" Type="Double" runat="server" ErrorMessage="Invalid Entry"></asp:CompareValidator>
                   </td><td nowrap>
                   <asp:TextBox ID="s_comm2TextBox" runat="server" Width="50" MaxLength="6" onkeyup="getdecimal(this,3);extractNumber(this,2,true);"   onkeypress="return blockNonNumbers(this, event, true, true);" Text='<%# Bind("[s-comm2]") %>' />
                   <asp:CompareValidator ID="CompareValidator6" ControlToValidate="s_comm2TextBox" Display="Dynamic" SetFocusOnError="true" Operator="DataTypeCheck" Type="Double" runat="server" ErrorMessage="Invalid Entry"></asp:CompareValidator>
                   </td><td nowrap>
                   <asp:TextBox ID="sman3TextBox" runat="server" Width="50" Text='<%# Bind("sman3") %>' />
                   <a href="#" tabindex="1" onClick="salesreplook(3); return false"><asp:Image ID="Image7" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
                   </td><td nowrap>
                   <asp:TextBox ID="s_pct3TextBox" runat="server" Width="70" MaxLength="6" onkeyup="getdecimal(this,3);extractNumber(this,2,true);"   onkeypress="return blockNonNumbers(this, event, true, true);" Text='<%# Bind("[s-pct3]") %>' />
                   <asp:CompareValidator ID="CompareValidator7" ControlToValidate="s_pct3TextBox" Display="Dynamic" SetFocusOnError="true" Operator="DataTypeCheck" Type="Double" runat="server" ErrorMessage="Invalid Entry"></asp:CompareValidator>
                   </td><td nowrap>
                   <asp:TextBox ID="s_comm3TextBox" runat="server" Width="50" onblur="window.scroll(0,1000)" MaxLength="6" onkeyup="getdecimal(this,3);extractNumber(this,2,true);"   onkeypress="return blockNonNumbers(this, event, true, true);" Text='<%# Bind("[s-comm3]") %>' />
                   <asp:CompareValidator ID="CompareValidator8" ControlToValidate="s_comm3TextBox" Display="Dynamic" SetFocusOnError="true" Operator="DataTypeCheck" Type="Double" runat="server" ErrorMessage="Invalid Entry"></asp:CompareValidator></td>
                                     
                   </tr>
                   </table>
                    <asp:Label ID="ReckeyLabel" runat="server" Visible="false" Text='<%# Bind("[reckey]") %>'></asp:Label>
                 
                   <asp:TextBox ID="x_noTextBox" runat="server" Visible="false" Text='<%# Bind("[x-no]") %>' />
                   <asp:Button ID="UpdateButton" runat="server" CausesValidation="True" CssClass="button" OnClick="UpdateButton_Formview2_Click" Text="Save" />
                   &nbsp;<asp:Button ID="UpdateCancelButton" runat="server" CausesValidation="False" CssClass="button" CommandName="Cancel" Text="Cancel" />
                   </fieldset>
                   </asp:Panel>
               </EditItemTemplate>
               <InsertItemTemplate>
                   <asp:Panel ID="EditPanel" Width="1000px" Height="120px" DefaultButton="InsertButton" runat="server" >
                    
                   <fieldset class="shade">
                   <table>
                   <tr>
                   <td><b>Line:</b></td><td><b>Account Number:</b></td><td><b>Account Desc:</b></td><td><b>Item Name:</b></td>
                   <td><b>Item Desc:</b></td><td><b>Customer Lot#:</b></td><td><b>Invoice Qty:</b></td><td><b>Cons Uom:</b></td>
                   <td><b>SqFt:</b></td><td><b>Price:</b></td><td><b>UOM:</b></td><td><b>Dsct%:</b></td><td><b>Amount:</b></td>
                   <td><b>Amt MSF:</b></td><td><b>Cost:</b></td><td><b>Cost UOM:</b></td><td><b>SlsRep:</b></td><td><b>% of Sales:</b></td><td><b>Comm%:</b></td>
                   <td><b>SlsRep:</b></td><td><b>% of Sales:</b></td><td><b>Comm%:</b></td><td><b>SlsRep:</b></td><td><b>% of Sales:</b></td><td><b>Comm%:</b></td>
                   </tr>
                  <tr>
                   <td nowrap><asp:TextBox ID="arlineTextBox" runat="server" Width="50" Text='<%# Bind("arline") %>' />
                   <asp:CompareValidator ID="CompareValidator11" ControlToValidate="arlineTextBox" Display="Dynamic" SetFocusOnError="true" Operator="DataTypeCheck" Type="Integer" runat="server" ErrorMessage="Invalid Entry"></asp:CompareValidator></td>                                      
                   <td nowrap><asp:TextBox ID="actnumTextBox" Width="80" runat="server" Text='<%# Bind("actnum") %>' />
                   <a href="#" tabindex="1" onClick="AccountLook(); return false"><asp:Image ID="Image13" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
                   </td><td>
                   <asp:TextBox ID="actdscrTextBox" runat="server" BackColor="Turquoise" onfocus="document.getElementById('FormView2_i_nameTextBox').focus()" Width="100" Text='<%# Bind("actdscr") %>' />
                   </td><td>
                   <asp:TextBox ID="i_nameTextBox" runat="server" Width="100" Text='<%# Bind("[i-name]") %>' />
                   </td><td>
                   <asp:TextBox ID="i_dscrTextBox" runat="server" Width="100" Text='<%# Bind("[i-dscr]") %>' />
                   </td><td>
                   <asp:TextBox ID="lot_noTextBox" runat="server" Width="100" Text='<%# Bind("[lot-no]") %>' />
                   </td><td nowrap>
                   <asp:TextBox ID="inv_qtyTextBox" runat="server" MaxLength="10" onkeyup="getdecimal(this,8);extractNumber(this,1,true);"   onkeypress="return blockNonNumbers(this, event, true, true);"  Width="80" Text='<%# Bind("[inv-qty]") %>' />
                   <asp:CompareValidator ID="CompareValidator2" ControlToValidate="inv_qtyTextBox" Display="Dynamic" SetFocusOnError="true" Operator="DataTypeCheck" Type="Double" runat="server" ErrorMessage="Invalid Entry"></asp:CompareValidator>
                   </td><td>
                   <asp:TextBox ID="cons_uomTextBox" runat="server" BackColor="Turquoise" onfocus="document.getElementById('FormView2_sf_shtTextBox').focus()" Width="40"  Text='<%# Bind("[cons-uom]") %>' />
                   </td><td nowrap>
                   <asp:TextBox ID="sf_shtTextBox" runat="server" MaxLength="8" onkeyup="getdecimal(this,5);extractNumber(this,2,true);"   onkeypress="return blockNonNumbers(this, event, true, true);" Width="60" Text='<%# Bind("[sf-sht]") %>' />
                   <asp:CompareValidator ID="CompareValidator9" ControlToValidate="sf_shtTextBox" Display="Dynamic" SetFocusOnError="true" Operator="DataTypeCheck" Type="Double" runat="server" ErrorMessage="Invalid Entry"></asp:CompareValidator>
                   </td><td nowrap>
                   <asp:TextBox ID="unit_prTextBox" runat="server" MaxLength="11" onkeyup="getdecimal(this,8);extractNumber(this,2,true);"   onkeypress="return blockNonNumbers(this, event, true, true);" Width="60" Text='<%# Bind("[unit-pr]") %>' />
                   <asp:CompareValidator ID="CompareValidator10" ControlToValidate="unit_prTextBox" Display="Dynamic" SetFocusOnError="true" Operator="DataTypeCheck" Type="Double" runat="server" ErrorMessage="Invalid Entry"></asp:CompareValidator>
                   </td><td>
                   <asp:TextBox ID="pr_qty_uomTextBox" runat="server" onblur="window.scroll(1000,1000)" Width="30" Text='<%# Bind("[pr-qty-uom]") %>' />
                   </td><td>
                   <asp:TextBox ID="discTextBox" runat="server" BackColor="Turquoise" onfocus="document.getElementById('FormView2_costTextBox').focus()" Width="80" Text='<%# Bind("disc") %>' />
                   </td><td>
                   <asp:TextBox ID="cal_amtTextBox" runat="server" BackColor="Turquoise" onfocus="document.getElementById('FormView2_costTextBox').focus()" Width="70" onblur="window.scroll(1000,1000)" Text='<%# Bind("[cal-amt]") %>' />
                   </td><td>
                   <asp:TextBox ID="amt_msfTextBox" runat="server" BackColor="Turquoise" onfocus="document.getElementById('FormView2_costTextBox').focus()" Width="70" Text='<%# Bind("[amt-msf]") %>' />
                   </td><td nowrap>
                   <asp:TextBox ID="costTextBox" runat="server" Width="50" onkeyup="getdecimal(this,9);extractNumber(this,2,true);"   onkeypress="return blockNonNumbers(this, event, true, true);" Text='<%# Bind("cost") %>' />
                   <asp:CompareValidator ID="CompareValidator1" ControlToValidate="costTextBox" Display="Dynamic" SetFocusOnError="true" Operator="DataTypeCheck" Type="Double" runat="server" ErrorMessage="Invalid Entry"></asp:CompareValidator>
                   </td><td>
                   <asp:TextBox ID="dscr1TextBox" runat="server" Width="50" Text='<%# Bind("dscr1") %>' />
                   </td><td nowrap>
                   <asp:TextBox ID="sman1TextBox" runat="server" Width="50" Text='<%# Bind("sman1") %>' />
                   <a href="#" tabindex="1" onClick="salesreplook(1); return false"><asp:Image ID="Image14" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
                   </td><td nowrap>
                   <asp:TextBox ID="s_pct1TextBox" runat="server" Width="70" MaxLength="6" onkeyup="getdecimal(this,3);extractNumber(this,2,true);"   onkeypress="return blockNonNumbers(this, event, true, true);" Text='<%# Bind("[s-pct1]") %>' />
                   <asp:CompareValidator ID="CompareValidator3" ControlToValidate="s_pct1TextBox" Display="Dynamic" SetFocusOnError="true" Operator="DataTypeCheck" Type="Double" runat="server" ErrorMessage="Invalid Entry"></asp:CompareValidator>
                   </td><td nowrap>
                   <asp:TextBox ID="s_comm1TextBox" runat="server" Width="50" MaxLength="6" onkeyup="getdecimal(this,3);extractNumber(this,2,true);"   onkeypress="return blockNonNumbers(this, event, true, true);" Text='<%# Bind("[s-comm1]") %>' />
                   <asp:CompareValidator ID="CompareValidator4" ControlToValidate="s_comm1TextBox" Display="Dynamic" SetFocusOnError="true" Operator="DataTypeCheck" Type="Double" runat="server" ErrorMessage="Invalid Entry"></asp:CompareValidator>
                   </td><td nowrap>
                   <asp:TextBox ID="sman2TextBox" runat="server" Width="50" Text='<%# Bind("sman2") %>' />
                   <a href="#" tabindex="1" onClick="salesreplook(2); return false"><asp:Image ID="Image15" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
                   </td><td nowrap>
                   <asp:TextBox ID="s_pct2TextBox" runat="server" Width="70" MaxLength="6" onkeyup="getdecimal(this,3);extractNumber(this,2,true);"   onkeypress="return blockNonNumbers(this, event, true, true);" Text='<%# Bind("[s-pct2]") %>' />
                   <asp:CompareValidator ID="CompareValidator5" ControlToValidate="s_pct2TextBox" Display="Dynamic" SetFocusOnError="true" Operator="DataTypeCheck" Type="Double" runat="server" ErrorMessage="Invalid Entry"></asp:CompareValidator>
                   </td><td nowrap>
                   <asp:TextBox ID="s_comm2TextBox" runat="server" Width="50" MaxLength="6" onkeyup="getdecimal(this,3);extractNumber(this,2,true);"   onkeypress="return blockNonNumbers(this, event, true, true);" Text='<%# Bind("[s-comm2]") %>' />
                   <asp:CompareValidator ID="CompareValidator6" ControlToValidate="s_comm2TextBox" Display="Dynamic" SetFocusOnError="true" Operator="DataTypeCheck" Type="Double" runat="server" ErrorMessage="Invalid Entry"></asp:CompareValidator>
                   </td><td nowrap>
                   <asp:TextBox ID="sman3TextBox" runat="server" Width="50" Text='<%# Bind("sman3") %>' />
                   <a href="#" tabindex="1" onClick="salesreplook(3); return false"><asp:Image ID="Image16" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
                   </td><td nowrap>
                   <asp:TextBox ID="s_pct3TextBox" runat="server" Width="70" MaxLength="6" onkeyup="getdecimal(this,3);extractNumber(this,2,true);"   onkeypress="return blockNonNumbers(this, event, true, true);" Text='<%# Bind("[s-pct3]") %>' />
                   <asp:CompareValidator ID="CompareValidator7" ControlToValidate="s_pct3TextBox" Display="Dynamic" SetFocusOnError="true" Operator="DataTypeCheck" Type="Double" runat="server" ErrorMessage="Invalid Entry"></asp:CompareValidator>
                   </td><td nowrap>
                   <asp:TextBox ID="s_comm3TextBox" runat="server" Width="50" onblur="window.scroll(0,1000)" MaxLength="6" onkeyup="getdecimal(this,3);extractNumber(this,2,true);"   onkeypress="return blockNonNumbers(this, event, true, true);" Text='<%# Bind("[s-comm3]") %>' />
                   <asp:CompareValidator ID="CompareValidator8" ControlToValidate="s_comm3TextBox" Display="Dynamic" SetFocusOnError="true" Operator="DataTypeCheck" Type="Double" runat="server" ErrorMessage="Invalid Entry"></asp:CompareValidator></td>
                                     
                   </tr>
                   </table>
                    <asp:Label ID="ReckeyLabel" Visible="false" runat="server" Text='<%# Bind("[reckey]") %>'></asp:Label>
                   <asp:Button ID="InsertButton" runat="server" CausesValidation="True" CssClass="button" OnClick="AddButton_Formview2_Click" Text="Save" />
                   &nbsp;<asp:Button ID="InsertCancelButton" runat="server" CssClass="button" CausesValidation="False" OnClick="CancelButton_FormView2_Delete" CommandName="Cancel" Text="Cancel" />
                   </fieldset></asp:Panel>
               </InsertItemTemplate>
               <ItemTemplate>
                  
                   <asp:Label ID="ReckeyLabel" Visible="false" runat="server" Text='<%# Bind("[reckey]") %>'></asp:Label>
                   <asp:Label ID="ArlineLabel" Visible="false" runat="server" Text='<%# Bind("[arline]") %>'></asp:Label>
                   
                    <asp:Button ID="AddButton" runat="server" CssClass="button" CommandName="New" Text="Add" />
                    <asp:Button ID="UpdateItemButton" runat="server" CssClass="button" CommandName="Edit" Text="Update" />
                   &nbsp;<asp:Button ID="DeleteButton" runat="server" CssClass="button" OnClientClick="return confirm('Are you sure you want to delete this record')" OnClick="deleteButton_FormView2_Click" Text="Delete" />
               </ItemTemplate>
           </asp:FormView>
            <asp:Button ID="AddNewFormView2Button" runat="server" CssClass="button"  OnClick="AddNewFormView2Button_Click" Text="Add" />
    
    
           <asp:ObjectDataSource ID="ObjectDataSource3" runat="server" 
               OldValuesParameterFormatString="original_{0}" SelectMethod="CustInvLine" 
               TypeName="contact">
               <SelectParameters>
                   <asp:Parameter DefaultValue="View" Name="prmAction" Type="String" />
                   <asp:Parameter Name="prmComp" Type="String" />
                   <asp:Parameter Name="prmUser" Type="String" />
                   <asp:SessionParameter Name="prmInv" SessionField="cust_invoice_invno" Type="Int32" />
                   <asp:Parameter  Name="prmLine"  Type="Int32" />
                   <asp:Parameter Name="prmActnum" Type="String" />
                   <asp:Parameter Name="prmActdscr" Type="String" />
                   <asp:Parameter Name="prmIname" Type="String" />
                   <asp:Parameter Name="prmIdscr" Type="String" />
                   <asp:Parameter Name="prmLotno" Type="String" />
                   <asp:Parameter Name="prmInvqty" Type="Decimal" />
                   <asp:Parameter Name="prmConsuom" Type="String" />
                   <asp:Parameter Name="prmSfsht" Type="Decimal" />
                   <asp:Parameter Name="prmUnitpr" Type="Decimal" />
                   <asp:Parameter Name="prmQtyuom" Type="String" />
                   <asp:Parameter Name="prmDisc" Type="Decimal" />
                   <asp:Parameter Name="prmCalamt" Type="Decimal" />
                   <asp:Parameter Name="prmAmtmsf" Type="Decimal" />
                   <asp:Parameter Name="prmCost" Type="Decimal" />
                   <asp:Parameter Name="prmDscr1" Type="String" />
                   <asp:Parameter Name="prmSman1" Type="String" />
                   <asp:Parameter Name="prmSpct1" Type="Decimal" />
                   <asp:Parameter Name="prmScomm1" Type="Decimal" />
                   <asp:Parameter Name="prmSman2" Type="String" />
                   <asp:Parameter Name="prmSpct2" Type="Decimal" />
                   <asp:Parameter Name="prmScomm2" Type="Decimal" />
                   <asp:Parameter Name="prmSman3" Type="String" />
                   <asp:Parameter Name="prmSpct3" Type="Decimal" />
                   <asp:Parameter Name="prmScomm3" Type="Decimal" />
                   <asp:SessionParameter SessionField="view_invoice_reckey_id" DefaultValue="" Name="prmReckey" Type="String" />
               </SelectParameters>
           </asp:ObjectDataSource>
    
    
    
           <asp:ObjectDataSource ID="ObjectDataSource2" runat="server" 
               OldValuesParameterFormatString="original_{0}" SelectMethod="CustInvLine" 
               TypeName="contact">
               <SelectParameters>
                   <asp:Parameter DefaultValue="Select" Name="prmAction" Type="String" />
                   <asp:Parameter Name="prmComp" Type="String" />
                   <asp:Parameter Name="prmUser" Type="String" />
                   <asp:SessionParameter DefaultValue="" Name="prmInv" SessionField="cust_invoice_invno" Type="Int32" />
                   <asp:Parameter Name="prmLine" Type="Int32" />
                   <asp:Parameter Name="prmActnum" Type="String" />
                   <asp:Parameter Name="prmActdscr" Type="String" />
                   <asp:Parameter Name="prmIname" Type="String" />
                   <asp:Parameter Name="prmIdscr" Type="String" />
                   <asp:Parameter Name="prmLotno" Type="String" />
                   <asp:Parameter Name="prmInvqty" Type="Decimal" />
                   <asp:Parameter Name="prmConsuom" Type="String" />
                   <asp:Parameter Name="prmSfsht" Type="Decimal" />
                   <asp:Parameter Name="prmUnitpr" Type="Decimal" />
                   <asp:Parameter Name="prmQtyuom" Type="String" />
                   <asp:Parameter Name="prmDisc" Type="Decimal" />
                   <asp:Parameter Name="prmCalamt" Type="Decimal" />
                   <asp:Parameter Name="prmAmtmsf" Type="Decimal" />
                   <asp:Parameter Name="prmCost" Type="Decimal" />
                   <asp:Parameter Name="prmDscr1" Type="String" />
                   <asp:Parameter Name="prmSman1" Type="String" />
                   <asp:Parameter Name="prmSpct1" Type="Decimal" />
                   <asp:Parameter Name="prmScomm1" Type="Decimal" />
                   <asp:Parameter Name="prmSman2" Type="String" />
                   <asp:Parameter Name="prmSpct2" Type="Decimal" />
                   <asp:Parameter Name="prmScomm2" Type="Decimal" />
                   <asp:Parameter Name="prmSman3" Type="String" />
                   <asp:Parameter Name="prmSpct3" Type="Decimal" />
                   <asp:Parameter Name="prmScomm3" Type="Decimal" />
                    <asp:Parameter Name="prmReckey" Type="String" />
               </SelectParameters>
           </asp:ObjectDataSource>
       </div>
       
    </div>
    </td></tr></table>    
    <ft:footer id="Footer1" runat="server"></ft:footer>
    </form>
  </body>
</HTML>

