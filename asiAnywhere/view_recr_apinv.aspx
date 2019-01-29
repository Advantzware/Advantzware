<%@ Page Language="c#" AutoEventWireup="true" Debug="true" Inherits="view_recr_apinv" Codebehind="view_recr_apinv.aspx.cs" %>
<%@ Register Src="footer.ascx" TagName="Footer" TagPrefix="ft" %>
<%@ Register Src="header.ascx" TagName="Header" TagPrefix="hd" %>
<html xmlns="http://www.w3.org/1999/xhtml" >
  <head id="Head1" runat="server">
    <title>Customer Invoices</title>
    <LINK href="include/style2.css" type="text/css" rel="stylesheet"/>
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

    function arlineText() {      
        var abc = document.getElementById("FormView2_actnumTextBox");
        abc.focus();
    }
    function actdescText() {
        var abc = document.getElementById("FormView2_inv_qtyTextBox");
        abc.focus();
    } 
    function itemdescText() {
        var abc = document.getElementById("FormView2_i_dscrTextBox");
        abc.focus();
    }
    function podescText() {
        var abc = document.getElementById("FormView2_ponoTextBox");
        abc.focus();
    }

    function focusval(obj) {
        obj.style.backgroundColor = 'blue';
        obj.style.color = 'white';
    }
    function blurval(obj) {
        obj.style.backgroundColor = 'Window';
        obj.style.color = 'WindowText';
    }

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
    }
     

    function exrateclick() {
        var curr = document.getElementById("FormView1_currcodeLabel").innerHTML;
        var rate = document.getElementById("FormView1_exrateLabel").innerHTML;
        
        var NewWindow = window.open("exrate_lookup.aspx?currc="+ curr +"&ratec="+ rate +"", "ExrateLookupWindow", "width=400,height=250,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
        
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

    function vendorlook() {

        var NewWindow = window.open("corvend_lookup.aspx", "UomLookup", "width=500,height=400,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
    }
    function VendLookup(ReturnObj1, ReturnObj2) {
        document.forms[0].FormView1_vendnoTextBox.value = ReturnObj1;
        var vname = document.getElementById("FormView1_vendnameLabel");
        vname.innerHTML = ReturnObj2;
        document.forms[0].FormView1_vendnoTextBox.focus();
    }

    function AccountLook() {
        var NewWindow = window.open("accountlook.aspx", "AccountLookupWindow", "width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
    }

    function AccountLookup(ReturnObj1, ReturnObj2) {
        document.forms[0].FormView2_actnumTextBox.value = ReturnObj1;
        var actdsc = document.getElementById("FormView2_actdscrlabel");
        if (document.forms[0].FormView2_actdscrLabel)
            document.forms[0].FormView2_actdscrLabel.value = ReturnObj2;
        else
            actdsc.innerHTML = ReturnObj2;
        
    }

    function popricelook() {
        var prmpo = document.getElementById("FormView2_ponoTextBox").value;
        var prmven = document.getElementById("FormView1_vendnoLabel").innerHTML;
        
        var NewWindow = window.open("po_price_history_lookup.aspx?pono="+ prmpo +"&invno="+ prmven +"","PopriceLookup","width=500,height=400,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
    }
    function PoPriceLookup(ReturnObj1, ReturnObj2, ReturnObj3, ReturnObj4, ReturnObj5, ReturnObj6, ReturnObj7, ReturnObj8, ReturnObj9, ReturnObj10, ReturnObj11) {
        document.forms[0].FormView2_ponoTextBox.value = ReturnObj1;       
        document.forms[0].FormView2_actnumTextBox.value = ReturnObj2;
        var actd = document.getElementById("FormView2_actdscrLabel");
        actd.value = ReturnObj3;
        
        document.forms[0].FormView2_inv_qtyTextBox.value = ReturnObj4;
        var cnsum = document.getElementById("FormView2_cons_uomLabel");
        cnsum.InnerHTML = ReturnObj5;
        document.forms[0].FormView2_unit_priceTextBox.value = ReturnObj6;
        document.forms[0].FormView2_qty_uom_priTextBox.value = ReturnObj7;
        document.forms[0].FormView2_sq_ftTextBox.value = ReturnObj8;
        var tmsf = document.getElementById("FormView2_totl_msfLabel");
        tmsf.value = ReturnObj9;
        document.forms[0].FormView2_i_dscrTextBox.value = ReturnObj10;
        var jb = document.getElementById("FormView2_jobLabel");
        jb.innerHTML = ReturnObj11;


        var prmpo2 = document.getElementById("FormView2_ponoTextBox").value;
        var NewWindow = window.open("item_price_po_lookup2.aspx?pono=" + prmpo2 + "", "itempricepoLookup", "width=500,height=400,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");

    }

    function ItemPoPriceLookup(ReturnObj1, ReturnObj2, ReturnObj3, ReturnObj4, ReturnObj5, ReturnObj6, ReturnObj7) {
        var line = document.getElementById("FormView2_arlineLabel")
        line.innerHTML = ReturnObj1;
        
        document.forms[0].FormView2_actnumTextBox.value = ReturnObj2;
        document.forms[0].FormView2_inv_qtyTextBox.value = ReturnObj3;
        var tmsf2 = document.getElementById("FormView2_totl_msfLabel");
        tmsf2.InnerHTML = ReturnObj4;
        var ino = document.getElementById("FormView2_i_noLabel")
        ino.innerHTML = ReturnObj5;
        var sn = document.getElementById("FormView2_snumLabel");
        sn.innerHTML = ReturnObj6;
        var accdesc = document.getElementById("FormView2_actdscrlabel");
        accdesc.innerHTML = ReturnObj7;
    }

    function uomlook() {
        
        var NewWindow = window.open("Uom_lookup.aspx", "UomLookupWindow", "width=500,height=400,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
    }
    function UomLookup(ReturnObj1) {
        document.forms[0].FormView2_qty_uom_priTextBox.value = ReturnObj1;

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
                <tr class="topheadcolor">
                                        
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
      <table><tr bgcolor="gray"><td> <div  id="navigation" style="width:100%">
		<ul nowrap> <li >
      <asp:LinkButton ID="lnk_Listcustomers" runat="server" OnClick="lnk_listinvoice" >Brws Invoices</asp:LinkButton></li>
      <li class="selected"><asp:LinkButton ID="lnk_viewcustomers" runat="server" OnClick="lnk_viewcustomers_Click" >View Invoices</asp:LinkButton></li>
      <li><asp:LinkButton ID="load_viewcustomers" runat="server" OnClick="load_viewcustomers_Click" > Load Invoices</asp:LinkButton>  </li></ul></div>
      </td>
      </tr></table>
       <div>
           <asp:FormView ID="FormView1" runat="server" OnDataBound="FormView1_OnDataBound" DataSourceID="ObjectDataSource1">
               <EditItemTemplate>
                    <fieldset><table class="shade">
               <tr><td>
               <table><tr><td align="right" style="padding-right:5px"><b>Vendor#:</b></td>
               <td><asp:TextBox ID="vendnoTextBox" Width="100px" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" runat="server" Text='<%# Bind("vendno") %>' />
               <a href="#" tabindex="1" onclick="vendorlook(); return false"><asp:Image ID="Image4" runat="server" ImageUrl="images/lookup_icon.gif" /></a></td>
               <td colspan="2"><asp:Label ID="vendnameLabel" BackColor="Turquoise" Width="200" runat="server" Text='<%# Bind("vendname") %>' /></td></tr>
               <tr><td align="right" style="padding-right:5px"><b></b></td>
               <td></td>
               <td><b>Tax Code:</b> &nbsp;&nbsp; <asp:TextBox ID="taxcodeTextBox" Width="50" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" runat="server" Text='<%# Bind("taxcode") %>' /></td>
               <td><b>Status:</b> &nbsp;&nbsp; <asp:TextBox ID="statsTextBox" Enabled="false" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" runat="server" Width="30" Text='<%# Bind("stats") %>' /></td></tr>
               <tr><td align="right" style="padding-right:5px"><b></b></td>
               <td>
               </td>
               <td colspan="2"><b>Discount%:</b> &nbsp; &nbsp; <asp:TextBox ID="discountTextBox" runat="server" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" Width="100px" Text='<%# Bind("discount") %>' /></td></tr>
               <tr><td align="right" style="padding-right:5px"><b></b></td>
               <td>
               </td>
               <td><b>Day:</b> &nbsp;&nbsp;  <asp:TextBox ID="discdaysTextBox" Width="50px" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" runat="server" Text='<%# Bind("discdays") %>' />      <td>
               <b>manual Check#</b></td></tr>
               <tr><td><input id="Button2" type="button" onclick="exrateclick()" value="Exrate" /></td>
               <td> 
                <asp:DropDownList ID="DropDownList1" Width="100px" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)"  SelectedValue='<%# Bind("extra") %>' DataTextField='<%# Bind("extra") %>' runat="server">
                                                    <asp:ListItem Value="Daily">Daily</asp:ListItem>
                                                    <asp:ListItem Value="Weekly">Weekly</asp:ListItem>
                                                    <asp:ListItem Value="Monthly">Monthly</asp:ListItem>
                                                    <asp:ListItem Value="Annually">Annually</asp:ListItem>
                                                    <asp:ListItem Value="Bi-weekly">Bi-weekly</asp:ListItem>
                                                    <asp:ListItem Value="Bi-monthly">Bi-monthly</asp:ListItem>
                                                    <asp:ListItem Value="Bi-annually">Bi-annually</asp:ListItem>
                                                    <asp:ListItem Value="Intermittently">Intermittently</asp:ListItem>
                                                    <asp:ListItem Value=""></asp:ListItem>
                                                </asp:DropDownList>
               </td><td></td>
               <td><asp:TextBox ID="mnulchecTextBox" runat="server" Width="100px" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" Text='<%# Bind("mnulchec") %>' /></td></tr>
               </table>
               </td>
               <td><fieldset>
               <table><tr><td align="right" style="padding-right:5px"><b>Tax:</b></td><td><asp:TextBox ID="taxamtTextBox" Width="100px" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" runat="server" Text='<%# Bind("taxamt") %>' /></td></tr>
               <tr><td align="right" style="padding-right:5px"><b>Net:</b></td><td><asp:TextBox ID="netTextBox" Enabled="false" Width="100px" runat="server" Text='<%# Bind("net") %>' /></td></tr>
               <tr><td align="right" style="padding-right:5px"><b>Paid:</b></td><td><asp:TextBox ID="paidTextBox" Enabled="false" Width="100px" runat="server" Text='<%# Bind("paid") %>' /></td></tr>
               <tr><td align="right" style="padding-right:5px"><b>Fright:</b></td><td><asp:TextBox ID="freightTextBox" Enabled="false" Width="100px" runat="server" Text='<%# Bind("freight") %>' /></td></tr>
               <tr><td align="right" style="padding-right:5px"><b>Balance Due:</b></td><td><asp:TextBox ID="dueTextBox" Enabled="false" Width="100px" runat="server" Text='<%# Bind("due") %>' /></td></tr></table>
               </fieldset></td>
               <td>
               <table><tr><td><b>User:</b></td><td><asp:Label ID="USRLabel" BackColor="Turquoise"  Width="50px" runat="server" Text='<%# Bind("USR") %>' /></td></tr>
               <tr><td colspan="2"><asp:Label Visible="false" ID="overwrtxLabel" BackColor="Turquoise"  runat="server" Text='<%# Bind("overwrtx") %>' />
                  <b> <asp:CheckBox ID="CheckBox1" Text="OverWrite Tax?" runat="server" /></b>
               
               </td></tr></table>
               </td></tr>
               </table></fieldset>
               <br /><div style="display:none">
                  <asp:Label ID="currcodeLabel"  runat="server" Text='<%# Bind("currcode") %>' />
                
                   <asp:Label ID="exrateLabel"  runat="server" Text='<%# Bind("exrate") %>' /></div>
               
                   <asp:Button ID="UpdateButton" runat="server" CausesValidation="True" CssClass="button" OnClick="UpdateButton_Click"  Text="Save" />
                   &nbsp;<asp:Button ID="UpdateCancelButton" runat="server" CssClass="button"
                       CausesValidation="False" CommandName="Cancel" Text="Cancel" />
               </EditItemTemplate>
               <InsertItemTemplate>
                   <fieldset><table class="shade">
               <tr><td>
               <table><tr><td align="right" style="padding-right:5px"><b>Vendor#:</b></td>
               <td><asp:TextBox ID="vendnoTextBox" Width="100px" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" runat="server" Text='<%# Bind("vendno") %>' />
               <a href="#" tabindex="1" onclick="vendorlook(); return false"><asp:Image ID="Image4" runat="server" ImageUrl="images/lookup_icon.gif" /></a></td>
               <td colspan="2"><asp:Label ID="vendnameLabel" BackColor="Turquoise" Width="200" runat="server" Text='<%# Bind("vendname") %>' /></td></tr>
               <tr><td align="right" style="padding-right:5px"><b></b></td>
               <td></td>
               <td><b>Tax Code:</b> &nbsp;&nbsp; <asp:TextBox ID="taxcodeTextBox" Width="50" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" runat="server" Text='<%# Bind("taxcode") %>' /></td>
               <td><b>Status:</b> &nbsp;&nbsp; <asp:TextBox ID="statsTextBox" Enabled="false" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" runat="server" Width="30" Text='<%# Bind("stats") %>' /></td></tr>
               <tr><td align="right" style="padding-right:5px"><b></b></td>
               <td>
               </td>
               <td colspan="2"><b>Discount%:</b> &nbsp; &nbsp; <asp:TextBox ID="discountTextBox" runat="server" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" Width="100px" Text='<%# Bind("discount") %>' /></td></tr>
               <tr><td align="right" style="padding-right:5px"><b></b></td>
               <td>
               </td>
               <td><b>Day:</b> &nbsp;&nbsp;  <asp:TextBox ID="discdaysTextBox" Width="50px" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" runat="server" Text='<%# Bind("discdays") %>' /></td>
               <td><b>manual Check#</b></td></tr>
               <tr><td><input id="Button2" type="button" onclick="exrateclick()" value="Exrate" /></td>
               <td><asp:DropDownList ID="DropDownList1" Width="100px" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)"  SelectedValue='<%# Bind("extra") %>' DataTextField='<%# Bind("extra") %>' runat="server">
                                                    <asp:ListItem Value="Daily">Daily</asp:ListItem>
                                                    <asp:ListItem Value="Weekly">Weekly</asp:ListItem>
                                                    <asp:ListItem Value="Monthly">Monthly</asp:ListItem>
                                                    <asp:ListItem Value="Annually">Annually</asp:ListItem>
                                                    <asp:ListItem Value="Bi-weekly">Bi-weekly</asp:ListItem>
                                                    <asp:ListItem Value="Bi-monthly">Bi-monthly</asp:ListItem>
                                                    <asp:ListItem Value="Bi-annually">Bi-annually</asp:ListItem>
                                                    <asp:ListItem Value="Intermittently">Intermittently</asp:ListItem>
                                                    <asp:ListItem Value=""></asp:ListItem>
                                                </asp:DropDownList></td><td></td>
               <td><asp:TextBox ID="mnulchecTextBox" runat="server" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" Width="100px" Text='<%# Bind("mnulchec") %>' /></td></tr>
               </table>
               </td>
               <td><fieldset>
               <table><tr><td align="right" style="padding-right:5px"><b>Tax:</b></td><td><asp:TextBox ID="taxamtTextBox" Width="100px" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" runat="server" Text='<%# Bind("taxamt") %>' /></td></tr>
               <tr><td align="right" style="padding-right:5px"><b>Net:</b></td><td><asp:TextBox ID="netTextBox" Enabled="false" Width="100px" runat="server" Text='<%# Bind("net") %>' /></td></tr>
               <tr><td align="right" style="padding-right:5px"><b>Paid:</b></td><td><asp:TextBox ID="paidTextBox" Enabled="false" Width="100px" runat="server" Text='<%# Bind("paid") %>' /></td></tr>
               <tr><td align="right" style="padding-right:5px"><b>Fright:</b></td><td><asp:TextBox ID="freightTextBox" Enabled="false" Width="100px" runat="server" Text='<%# Bind("freight") %>' /></td></tr>
               <tr><td align="right" style="padding-right:5px"><b>Balance Due:</b></td><td><asp:TextBox ID="dueTextBox" Enabled="false" Width="100px" runat="server" Text='<%# Bind("due") %>' /></td></tr></table>
               </fieldset></td>
               <td>
               <table><tr><td><b>User:</b></td><td><asp:Label ID="USRLabel" Width="100px" runat="server" Text='<%# Bind("USR") %>' /></td></tr>
               <tr><td colspan="2"><asp:Label ID="overwrtxLabel" Visible="false" runat="server" Text='<%# Bind("overwrtx") %>' />
               <b><asp:CheckBox ID="CheckBox1" Text="OverWrite Tax?" runat="server" /></b></td></tr></table>
               </td></tr>
               </table></fieldset>
               <br /><div style="display:none">
               <asp:Label ID="currcodeLabel"  runat="server" Text='<%# Bind("currcode") %>' />
                
                   <asp:Label ID="exrateLabel"  runat="server" Text='<%# Bind("exrate") %>' /></div>
                   
                   <asp:Button ID="InsertButton" runat="server" CausesValidation="True" CssClass="button" OnClick="InsertButton_Click" Text="Save" />
                   &nbsp;<asp:Button ID="InsertCancelButton" runat="server" CssClass="button"
                       CausesValidation="False" CommandName="Cancel" Text="Cancel" />
               </InsertItemTemplate>
               <ItemTemplate>
               <fieldset>
               <table class="shade">
               <tr><td>
               <table><tr><td align="right" style="padding-right:5px"><b>Vendor#:</b></td>
               <td><asp:Label ID="vendnoLabel" BackColor="Turquoise" Width="100" runat="server" Text='<%# Bind("vendno") %>' /></td>
               <td colspan="2"><asp:Label ID="vendnameLabel" BackColor="Turquoise" Width="200" runat="server" Text='<%# Bind("vendname") %>' /></td></tr>
               <tr><td align="right" style="padding-right:5px"><b></b></td>
               <td></td>
               <td><b>Tax Code:</b> &nbsp;&nbsp; <asp:Label ID="taxcodeLabel" BackColor="Turquoise" Width="35" runat="server" Text='<%# Bind("taxcode") %>' /></td>
               <td><b>Status:</b> &nbsp;&nbsp; <asp:Label ID="statsLabel" BackColor="Turquoise" Width="35" runat="server" Text='<%# Bind("stats") %>' /></td></tr>
               <tr><td align="right" style="padding-right:5px"><b></b></td>
               <td></td>
               <td colspan="2"><b>Discount%:</b> &nbsp; &nbsp; <asp:Label ID="discountLabel" BackColor="Turquoise" Width="100" runat="server" Text='<%# Bind("discount") %>' /></td></tr>
               <tr><td align="right" style="padding-right:5px"><b></b></td>
               <td></td>
               <td><b>Day:</b> &nbsp;&nbsp;  <asp:Label ID="discdaysLabel" BackColor="Turquoise" Width="50" runat="server" Text='<%# Bind("discdays") %>' /></td>
               <td><b>Manual Check#</b></td></tr>
               <tr><td> <input id="Button2" type="button" disabled="disabled" onclick="exrateclick()" value="Exrate" /></td>
               <td><asp:DropDownList ID="DropDownList1" Width="100px" Enabled="false"  SelectedValue='<%# Bind("extra") %>' DataTextField='<%# Bind("extra") %>' runat="server">
                                                    <asp:ListItem Value="Daily">Daily</asp:ListItem>
                                                    <asp:ListItem Value="Weekly">Weekly</asp:ListItem>
                                                    <asp:ListItem Value="Monthly">Monthly</asp:ListItem>
                                                    <asp:ListItem Value="Annually">Annually</asp:ListItem>
                                                    <asp:ListItem Value="Bi-weekly">Bi-weekly</asp:ListItem>
                                                    <asp:ListItem Value="Bi-monthly">Bi-monthly</asp:ListItem>
                                                    <asp:ListItem Value="Bi-annually">Bi-annually</asp:ListItem>
                                                    <asp:ListItem Value="Intermittently">Intermittently</asp:ListItem>
                                                    <asp:ListItem Value=""></asp:ListItem>
                                                </asp:DropDownList></td><td></td>
               <td><asp:Label ID="mnulchecLabel" runat="server" BackColor="Turquoise" Width="100" Text='<%# Bind("mnulchec") %>' /></td></tr>
               </table>
               </td>
               <td><fieldset>
               <table><tr><td align="right" style="padding-right:5px"><b>Tax:</b></td><td><asp:Label ID="taxamtLabel" BackColor="Turquoise" Width="100" runat="server" Text='<%# Bind("taxamt") %>' /></td></tr>
               <tr><td align="right" style="padding-right:5px"><b>Net:</b></td><td><asp:Label ID="netLabel" runat="server" BackColor="Turquoise" Width="100" Text='<%# Bind("net") %>' /></td></tr>
               <tr><td align="right" style="padding-right:5px"><b>Paid:</b></td><td><asp:Label ID="paidLabel" runat="server" BackColor="Turquoise" Width="100" Text='<%# Bind("paid") %>' /></td></tr>
               <tr><td align="right" style="padding-right:5px"><b>Fright:</b></td><td><asp:Label ID="freightLabel" runat="server" BackColor="Turquoise" Width="100" Text='<%# Bind("freight") %>' /></td></tr>
               <tr><td align="right" style="padding-right:5px"><b>Balance Due:</b></td><td><asp:Label ID="dueLabel" runat="server" BackColor="Turquoise" Width="100" Text='<%# Bind("due") %>' /></td></tr></table>
               </fieldset></td>
               <td>
               <table><tr><td align="right" style="padding-right:5px"><b>User:</b></td><td><asp:Label ID="USRLabel" BackColor="Turquoise" Width="40" runat="server" Text='<%# Bind("USR") %>' /></td></tr>
               <tr><td><b>Overwrite Tax:</b></td><td><asp:Label ID="overwrtxLabel" Visible="false" runat="server" Text='<%# Bind("overwrtx") %>' />
               <asp:CheckBox ID="CheckBox1"  Enabled="false" BackColor="Turquoise"  runat="server" /></td></tr></table>
               </td></tr>
               </table></fieldset>
                  
                   <div style="display:none"><asp:Label ID="currcodeLabel"  runat="server" Text='<%# Bind("currcode") %>' />
                
                   <asp:Label ID="exrateLabel"  runat="server" Text='<%# Bind("exrate") %>' /></div>
                   
                   
                   <asp:Label ID="reckeyLabel" Visible="false" runat="server" Text='<%# Bind("reckey") %>' />
                   
                   <br />
                   
                   <asp:Button ID="AddButton" runat="server" CssClass="button"  CommandName="new" Text="Add" />
                    <asp:Button ID="UpdateButton" runat="server" CssClass="button" CommandName="edit" Text="Update" />
                   &nbsp;<asp:Button ID="DeleteButton" runat="server" CssClass="button" OnClientClick="return confirm('Are you sure you want to delete this record')" OnClick="delete_Button_Click"  Text="Delete" />
                   <%--<asp:Button ID="Button1" runat="server" CssClass="button"  OnClick="holdButton_Click"  Text="Hold/Release" />--%>
               </ItemTemplate>
           </asp:FormView>
           
           <asp:ObjectDataSource ID="ObjectDataSource1" runat="server" 
                OldValuesParameterFormatString="original_{0}" SelectMethod="SelectVendor" 
                TypeName="voucherpay">
                <SelectParameters>
                   <asp:Parameter DefaultValue="View" Name="prmAction" Type="String" />                    
                   <asp:Parameter Name="prmComp"  Type="string" />
                   <asp:Parameter Name="prmUser" Type="string" />                     
                   <asp:Parameter  Name="prmvend" Type="String" />
                   <asp:Parameter Name="prmInv" Type="String" />
                     
                   <asp:Parameter  Name="prmPosted" Type="String" />
                   <asp:Parameter  Name="prmunPosted" Type="String" />
                   <asp:Parameter Name="prmvendname" Type="String" />
                   <asp:Parameter  Name="prmInvdate" Type="String" />
                   <asp:Parameter Name="prmnet" Type="Decimal" />
                   <asp:Parameter Name="prmPaid" Type="Decimal" />
                   <asp:Parameter Name="prmBaldue" Type="Decimal" />
                   <asp:Parameter Name="prmTaxamt" Type="Decimal" />
                   <asp:Parameter Name="prmDue" Type="Decimal" />
                   <asp:Parameter Name="prmDuedate" Type="String" />
                   <asp:Parameter Name="prmTaxcode" Type="String" />
                   <asp:Parameter Name="prmDiscount" Type="Decimal" />
                   <asp:Parameter Name="prmDiscdays" Type="Int32" />
                   <asp:Parameter Name="prmCurrcode" Type="String" />
                   <asp:Parameter Name="prmExrate" Type="Decimal" />
                   <asp:Parameter Name="prmMnlchac" Type="String" />
                   <asp:Parameter Name="prmTxovrwrt" Type="String" />
                   <asp:Parameter Name="prmFreight" Type="Decimal" />
                    <asp:SessionParameter SessionField="RecrApVend_invoice_reckey_rec" Name="prmReckey" Type="String" />
                </SelectParameters>
            </asp:ObjectDataSource>
       </div>    
       <div>
       <br />
        <asp:GridView ID="GridView1" runat="server" AllowPaging="True" AllowSorting="True" OnSelectedIndexChanged="GridView1_SelectedIndex"
        AutoGenerateColumns="False" CssClass="Grid" DataSourceID="ObjectDataSource2" DataKeyNames="reckey"
        EmptyDataText="No Record Found"  Width="730px">
        <EmptyDataRowStyle BorderColor="Gray" BorderStyle="Dotted" BorderWidth="0px" Font-Bold="True"
            HorizontalAlign="Center" VerticalAlign="Middle" />
        <RowStyle CssClass="shade" />
        <Columns>
             <asp:CommandField ShowSelectButton="true" ButtonType="image" SelectImageUrl="~/Images/sel.gif" SelectText="" > 
                    <ItemStyle Width="10px" />
                    </asp:CommandField>
            <asp:BoundField DataField="pono" HeaderText="Po Number"  SortExpression="pono" />
            <asp:BoundField DataField="arline" ItemStyle-Wrap="false" HeaderText="Line "  
                   SortExpression="arline" />
            <asp:BoundField DataField="actnum" ItemStyle-Wrap="false" HeaderText="Account Number"  
                   SortExpression="actnum" />
            <asp:BoundField DataField="actdscr" ItemStyle-Wrap="false" HeaderText="Account Description" 
                   SortExpression="actdscr" />
            <asp:BoundField DataField="inv-qty" HeaderText="Quantity" SortExpression="inv-qty" />   
            <asp:BoundField DataField="cons-uom" HeaderText="Uom"  SortExpression="cons-uom" />
            <asp:BoundField DataField="unit-price" HeaderText="Price" SortExpression="unit-price" />
            <asp:BoundField DataField="qty-uom-pri" HeaderText="Uom" SortExpression="qty-uom-pri" />    
            <asp:BoundField DataField="tax" HeaderText="Tax"   SortExpression="tax" />
                <asp:BoundField DataField="sq-ft" HeaderText="SqFt"  SortExpression="sq-ft" />
            <asp:BoundField DataField="amt" HeaderText="Amount"  SortExpression="amt" />
            <asp:BoundField DataField="totl-msf" HeaderText="Total MSF" SortExpression="totl-msf" />       
            <asp:BoundField DataField="i-no" ItemStyle-Wrap="false" HeaderText="Item#" 
                   SortExpression="i-no" />
            <asp:BoundField DataField="i-dscr" ItemStyle-Wrap="false" HeaderText="Description" 
                   SortExpression="i-dscr" />            
            <asp:BoundField DataField="job" HeaderText="Job" SortExpression="job" />
            <asp:BoundField DataField="snum" HeaderText="Sheet#" SortExpression="snum" />
            <asp:BoundField DataField="bnum" HeaderText="Blank#" SortExpression="bnum" />
            
           
            <asp:BoundField DataField="reckey" HeaderText="reckey" Visible="false" SortExpression="reckey" />
            <asp:TemplateField HeaderText="Reckey" Visible="false" >
            <ItemTemplate>
            <asp:Label ID="reclabel" runat="server" Text='<%# Bind("[reckey]") %>'></asp:Label>
            </ItemTemplate>
            </asp:TemplateField>
        </Columns>
        <SelectedRowStyle CssClass="GridSelected" BackColor="Yellow" />
        <HeaderStyle   ForeColor="White" CssClass="headcolor" HorizontalAlign="Center"
            VerticalAlign="Middle" Wrap="False" />
        <AlternatingRowStyle CssClass="GridItemOdd" />
    </asp:GridView><br />       
           
           
           <asp:FormView ID="FormView2" runat="server" OnDataBound="FormView2_OnDataBound" DataSourceID="ObjectDataSource3">
               <EditItemTemplate>
                   <asp:Panel ID="EditPanel" Width="1400px" Height="120px" DefaultButton="UpdateButton" runat="server" >
                    
                   <fieldset class="shade">
                   <table>
                   <tr>
                   <td><b>Po Number:</b></td><td><b>Line:</b></td><td><b>Account Number:</b></td><td><b>Account Description:</b></td>
                   <td><b>Quantity:</b></td><td><b>Uom:</b></td><td><b>Price:</b></td><td><b>Uom:</b></td><td><b>Tax:</b></td><td><b>SqFt:</b></td>
                   <td><b>Amount:</b></td><td><b>Total Msf:</b></td><td><b>Item #:</b></td><td><b>Description:</b></td><td><b>Job:</b></td>
                   <td><b>Sheet#:</b></td><td><b>Blank#:</b></td>
                   </tr>
                   <tr><td>
                   <asp:label ID="ponolabel" runat="server" BackColor="Turquoise" Width="100px" Text='<%# Bind("pono") %>' />
                   </td>
                   <td><asp:label ID="arlinelabel" runat="server" BackColor="Turquoise" Width="50px" Text='<%# Bind("arline") %>' /></td>
                   <td>
                   <asp:TextBox ID="actnumTextBox" runat="server" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" MaxLength="25" Width="100px" Text='<%# Bind("actnum") %>' />
                   <a href="#" tabindex="1" onClick="AccountLook(); return false"><asp:Image ID="Image13" runat="server" ImageUrl="images/lookup_icon.gif" /></a></td>
                   <td>
                   <asp:label ID="actdscrlabel" runat="server" BackColor="Turquoise" Width="135px" Text='<%# Bind("actdscr") %>' /></td>
                   <td>
                   <asp:TextBox ID="inv_qtyTextBox" runat="server" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" Width="70px" OnTextChanged="inv_qty_textbox_Change" AutoPostBack="true" MaxLength="9" Text='<%# Bind("[inv-qty]") %>' />
                   <asp:CompareValidator ID="CompareValidator1" ControlToValidate="inv_qtyTextBox" SetFocusOnError="true" Display="Dynamic" Operator="DataTypeCheck" Type="Double" runat="server" ErrorMessage="Invalid Entry"></asp:CompareValidator></td>
                   <td><asp:label ID="cons_uomlabel" runat="server" BackColor="Turquoise" Width="30px" MaxLength="3" Text='<%# Bind("[cons-uom]") %>' />
                   </td>
                   <td><asp:TextBox ID="unit_priceTextBox" runat="server" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" Width="70px" MaxLength="10" OnTextChanged="inv_qty_textbox_Change" AutoPostBack="true" Text='<%# Bind("[unit-price]") %>' />
                   <asp:CompareValidator ID="CompareValidator2" ControlToValidate="unit_priceTextBox" SetFocusOnError="true" Display="Dynamic" Operator="DataTypeCheck" Type="Double" runat="server" ErrorMessage="Invalid Entry"></asp:CompareValidator></td>
                   <td nowrap><asp:TextBox ID="qty_uom_priTextBox" runat="server" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" Width="30px" MaxLength="3" OnTextChanged="inv_qty_textbox_Change" AutoPostBack="true" Text='<%# Bind("[qty-uom-pri]") %>' />
                   <a href="#" onclick="uomlook(); return false"><asp:Image ID="Image2" runat="server" ImageUrl="images/lookup_icon.gif" /></a></td>
                   <td>
                   <asp:DropDownList ID="DropDownList1" Width="50px"  runat="server" SelectedValue='<%# Bind("tax") %>'   DataValueField='<%# Bind("tax") %>'>  
                        <asp:ListItem Text="Yes" Value="yes"></asp:ListItem>
                        <asp:ListItem Text="No" Value="no"></asp:ListItem>
                        </asp:DropDownList>
                   </td>
                   <td><asp:TextBox ID="sq_ftTextBox" runat="server" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" Width="70px" MaxLength="7" Text='<%# Bind("[sq-ft]") %>' />
                   <asp:CompareValidator ID="CompareValidator3" ControlToValidate="sq_ftTextBox" SetFocusOnError="true" Display="Dynamic" Operator="DataTypeCheck" Type="Double" runat="server" ErrorMessage="Invalid Entry"></asp:CompareValidator></td>
                   <td><asp:TextBox ID="amtTextBox" runat="server" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" Width="80px" MaxLength="10" OnTextChanged="inv_amount_textbox_Change" AutoPostBack="true" Text='<%# Bind("amt") %>' />
                   <asp:CompareValidator ID="CompareValidator4" ControlToValidate="amtTextBox" SetFocusOnError="true" Display="Dynamic" Operator="DataTypeCheck" Type="Double" runat="server" ErrorMessage="Invalid Entry"></asp:CompareValidator></td>
                   <td><asp:label ID="totl_msflabel" runat="server" BackColor="Turquoise" Width="80px" Text='<%# Bind("[totl-msf]") %>' /></td>
                   <td>
                   <asp:label ID="i_nolabel" runat="server" BackColor="Turquoise" Width="100px" Text='<%# Bind("[i-no]") %>' /></td>
                   <td>
                   <asp:TextBox ID="i_dscrTextBox" runat="server" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" Width="130px" MaxLength="30" Text='<%# Bind("[i-dscr]") %>' /></td>
                   <td><asp:label ID="joblabel" runat="server" BackColor="Turquoise" Width="70px" Text='<%# Bind("job") %>' /></td>
                   <td><asp:label ID="snumlabel" runat="server" BackColor="Turquoise" Width="40px" Text='<%# Bind("snum") %>' /></td>
                   <td><asp:label ID="bnumlabel" runat="server" BackColor="Turquoise" Width="40px" Text='<%# Bind("bnum") %>' /></td>
                   <td><asp:TextBox ID="reckeyTextBox" Visible="false" runat="server" Text='<%# Bind("reckey") %>' /></td>
                  
                    </tr></table>                   
                   
                   <asp:Button ID="UpdateButton" runat="server" CausesValidation="True" CssClass="button" OnClick="UpdateButton_Formview2_Click" Text="Save" />
                   &nbsp;<asp:Button ID="UpdateCancelButton" runat="server" CausesValidation="False" CssClass="button" CommandName="Cancel" Text="Cancel" />
                   </fieldset>
                   </asp:Panel>
               </EditItemTemplate>
               <InsertItemTemplate>
                   <asp:Panel ID="EditPanel" Width="1450px" Height="120px" DefaultButton="InsertButton" runat="server" >
                    
                   <fieldset class="shade">
                   <table>
                   <tr>
                   <td><b>Po Number:</b></td><td><b>Line:</b></td><td><b>Account Number:</b></td><td><b>Account Description:</b></td>
                   <td><b>Quantity:</b></td><td><b>Uom:</b></td><td><b>Price:</b></td><td><b>Uom:</b></td><td><b>Tax:</b></td><td><b>SqFt:</b></td>
                   <td><b>Amount:</b></td><td><b>Total Msf:</b></td><td><b>Item #:</b></td><td><b>Description:</b></td><td><b>Job:</b></td>
                   <td><b>Sheet#:</b></td><td><b>Blank#:</b></td>
                   </tr>
                  <tr><td>
                   <asp:TextBox ID="ponoTextBox" runat="server" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" Width="100px" Text='<%# Bind("pono") %>' />
                   <a href="#" tabindex="1" onclick="popricelook(); return false"><asp:Image ID="poprice" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
                   <asp:CompareValidator ID="CompareValidator8" ControlToValidate="ponoTextBox" SetFocusOnError="true" Display="Dynamic" Operator="DataTypeCheck" Type="Double" runat="server" ErrorMessage="Invalid Entry"></asp:CompareValidator></td>
                   <td><asp:TextBox ID="arlineLabel" runat="server" onfocus="arlineText()"  BackColor="Turquoise" Width="70px"  />
                   </td>
                   <td>
                   <asp:TextBox ID="actnumTextBox" runat="server" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" MaxLength="25" Width="100px" Text='<%# Bind("actnum") %>' />
                   <a href="#" tabindex="1" onClick="AccountLook(); return false"><asp:Image ID="Image13" runat="server" ImageUrl="images/lookup_icon.gif" /></a></td>
                   <td>
                   <asp:TextBox ID="actdscrLabel" runat="server" onfocus="actdescText()" Width="135px"  BackColor="Turquoise"  /></td>
                   <td>
                   <asp:TextBox ID="inv_qtyTextBox" runat="server" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" Width="70px" OnTextChanged="Insertinvqty_Change" AutoPostBack="true" MaxLength="9" Text='<%# Bind("[inv-qty]") %>' />
                   <asp:CompareValidator ID="CompareValidator1" ControlToValidate="inv_qtyTextBox" SetFocusOnError="true" Display="Dynamic" Operator="DataTypeCheck" Type="Double" runat="server" ErrorMessage="Invalid Entry"></asp:CompareValidator></td>
                   <td><asp:Label ID="cons_uomLabel" runat="server" BackColor="Turquoise" Width="30px" MaxLength="3" Text='<%# Bind("[cons-uom]") %>' />
                   </td>
                   <td><asp:TextBox ID="unit_priceTextBox" runat="server" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" Width="70px" OnTextChanged="Insertinvqty_Change" AutoPostBack="true" MaxLength="10" Text='<%# Bind("[unit-price]") %>' />
                   <asp:CompareValidator ID="CompareValidator5" ControlToValidate="unit_priceTextBox" SetFocusOnError="true" Display="Dynamic" Operator="DataTypeCheck" Type="Double" runat="server" ErrorMessage="Invalid Entry"></asp:CompareValidator></td>
                   <td nowrap><asp:TextBox ID="qty_uom_priTextBox" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" runat="server" Width="30px" OnTextChanged="Insertinvqty_Change" AutoPostBack="true" MaxLength="3" Text='<%# Bind("[qty-uom-pri]") %>' />
                   <a href="#" tabindex="1" onclick="uomlook(); return false"><asp:Image ID="Image2" runat="server" ImageUrl="images/lookup_icon.gif" /></a></td>
                   <td>
                   <asp:DropDownList ID="DropDownList1" Width="40px"  runat="server" SelectedValue='<%# Bind("tax") %>'   DataValueField='<%# Bind("tax") %>'>  
                        <asp:ListItem Text="Yes" Value="yes"></asp:ListItem>
                        <asp:ListItem Text="No" Value="no"></asp:ListItem>
                        </asp:DropDownList></td>
                   <td><asp:TextBox ID="sq_ftTextBox" runat="server" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" Width="70px" MaxLength="7" Text='<%# Bind("[sq-ft]") %>' />
                   <asp:CompareValidator ID="CompareValidator6" ControlToValidate="sq_ftTextBox" SetFocusOnError="true" Display="Dynamic" Operator="DataTypeCheck" Type="Double" runat="server" ErrorMessage="Invalid Entry"></asp:CompareValidator></td>
                   <td><asp:TextBox ID="amtTextBox" runat="server" Width="80px" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" MaxLength="10" OnTextChanged="InsertamtTextBox_Change" AutoPostBack="true" Text='<%# Bind("amt") %>' />
                   <asp:CompareValidator ID="CompareValidator7" ControlToValidate="amtTextBox" SetFocusOnError="true" Display="Dynamic" Operator="DataTypeCheck" Type="Double" runat="server" ErrorMessage="Invalid Entry"></asp:CompareValidator></td>
                   <td><asp:TextBox ID="totl_msfLabel" runat="server" Width="80px" onfocus="itemdescText()" BackColor="Turquoise" Text='<%# Bind("[totl-msf]") %>' /></td>
                   <td>
                   <asp:TextBox ID="i_noLabel" runat="server" BackColor="Turquoise" onfocus="itemdescText()" Width="110px" Text='<%# Bind("[i-no]") %>' /></td>
                   <td>
                   <asp:TextBox ID="i_dscrTextBox" runat="server" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" Width="130px" MaxLength="30" Text='<%# Bind("[i-dscr]") %>' /></td>
                   <td><asp:Label ID="jobLabel" runat="server" BackColor="Turquoise" Width="70px" Text='<%# Bind("job") %>' /></td>
                   <td><asp:TextBox ID="snumLabel" runat="server" BackColor="Turquoise" onfocus="podescText()" Width="40px" Text='<%# Bind("snum") %>' /></td>
                   <td><asp:Label ID="bnumLabel" runat="server" BackColor="Turquoise" Width="40px" Text='<%# Bind("bnum") %>' /></td>
                   <td><asp:TextBox ID="reckeyTextBox" Visible="false" runat="server" Text='<%# Bind("reckey") %>' /></td>
                  
                    </tr></table>   
                    
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
                   <%--<asp:Button ID="Button1" runat="server" CssClass="button" OnClick="BoardReclButton_Click"  Text="Board $ Recalc" />--%>
               </ItemTemplate>
           </asp:FormView>
            <asp:Button ID="AddNewFormView2Button" runat="server" CssClass="button"  OnClick="AddNewFormView2Button_Click" Text="Add" />
    
    
           <asp:ObjectDataSource ID="ObjectDataSource3" runat="server" 
               OldValuesParameterFormatString="original_{0}" SelectMethod="SelectViewVendor" 
               TypeName="voucherpay">
               <SelectParameters>
                   <asp:Parameter DefaultValue="View" Name="prmAction" Type="String" />
                   <asp:Parameter Name="prmComp" Type="String" />
                   <asp:Parameter Name="prmUser" Type="String" />
                   <asp:Parameter Name="prmInv" Type="String" />
                   <asp:Parameter Name="prmPono" Type="Int32" />
                   <asp:Parameter  Name="prmLine"  Type="Int32" />
                   <asp:Parameter Name="prmActnum" Type="String" />
                   <asp:Parameter Name="prmActdscr" Type="String" />
                   <asp:Parameter Name="prmIno" Type="String" />
                   <asp:Parameter Name="prmIdscr" Type="String" />
                   <asp:Parameter Name="prmInvqty" Type="Decimal" />
                   <asp:Parameter Name="prmConsuom" Type="String" />
                   <asp:Parameter Name="prmUnitPrice" Type="Decimal" />
                   <asp:Parameter Name="prmQtyUomPri" Type="String" />
                   <asp:Parameter Name="prmTax" Type="String" />
                   <asp:Parameter Name="prmSqft" Type="Decimal" />
                   <asp:Parameter Name="prmAmt" Type="Decimal" />
                   <asp:Parameter Name="prmTotlmsf" Type="Decimal" />
                   <asp:Parameter Name="prmJob" Type="String" />
                   <asp:Parameter Name="prmSnum" Type="Int32" />
                   <asp:Parameter Name="prmBnum" Type="Int32" DefaultValue="" />
                   <asp:SessionParameter SessionField="view_recr_apinv_reckey_rec" Name="prmReckey"  Type="String" />
               </SelectParameters>
           </asp:ObjectDataSource>
    
    
    
           <asp:ObjectDataSource ID="ObjectDataSource2" runat="server" 
               OldValuesParameterFormatString="original_{0}" SelectMethod="SelectViewVendor" 
               TypeName="voucherpay">
               <SelectParameters>
                   <asp:Parameter DefaultValue="SelectGrid" Name="prmAction" Type="String" />
                   <asp:Parameter Name="prmComp" Type="String" />
                   <asp:Parameter Name="prmUser" Type="String" />
                   <asp:Parameter Name="prmInv" Type="String" />
                   <asp:Parameter Name="prmPono" Type="Int32" />
                   <asp:Parameter Name="prmLine" Type="Int32" />
                   <asp:Parameter Name="prmActnum" Type="String" />
                   <asp:Parameter Name="prmActdscr" Type="String" />
                   <asp:Parameter Name="prmIno" Type="String" />
                   <asp:Parameter Name="prmIdscr" Type="String" />
                   <asp:Parameter Name="prmInvqty" Type="Decimal" />
                   <asp:Parameter Name="prmConsuom" Type="String" />
                   <asp:Parameter Name="prmUnitPrice" Type="Decimal" />
                   <asp:Parameter Name="prmQtyUomPri" Type="String" />
                   <asp:Parameter Name="prmTax" Type="String" />
                   <asp:Parameter Name="prmSqft" Type="Decimal" />
                   <asp:Parameter Name="prmAmt" Type="Decimal" />
                   <asp:Parameter Name="prmTotlmsf" Type="Decimal" />
                   <asp:Parameter Name="prmJob" Type="String" />
                   <asp:Parameter Name="prmSnum" Type="Int32" />
                   <asp:Parameter Name="prmBnum" Type="Int32" />
                   <asp:SessionParameter DefaultValue="" Name="prmReckey" SessionField="RecrApVend_invoice_reckey_rec" 
                       Type="String" />
               </SelectParameters>
           </asp:ObjectDataSource>
       </div>
       
    </div>
    </td></tr></table>    
    <ft:footer id="Footer1" runat="server"></ft:footer>
    </form>
  </body>
</HTML>

