<%@ Page Language="c#" AutoEventWireup="true" Debug="true" Inherits="view_release_order" Codebehind="view_release_order.aspx.cs" %>
<%@ Register Src="footer.ascx" TagName="Footer" TagPrefix="ft" %>
<%@ Register Src="header.ascx" TagName="Header" TagPrefix="hd" %>
<html xmlns="http://www.w3.org/1999/xhtml" >

  <head id="Head1" runat="server">
    <title>Release Order</title>
    <LINK href="include/style.css" type="text/css" rel="stylesheet"/>
    <LINK REL="stylesheet" TYPE="text/css" HREF="include/CalendarControl.css" >
    <link rel="stylesheet" type="text/css" href="include/help.css">
    <script language = "JavaScript" src="include/CalendarControl.js"></script>
     <script language="javascript" src="include/date.js"></script>
    <script language="javascript" src="include/event.js"></script>
    <script language="javascript" src="include/insert.js"></script>
    <script language="javascript" src="include/validate2.js"></script> 
        
     <%--<script language="javascript" src="include/help.js"></script> --%>
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

    function termsLookup(ReturnObj1) {
        document.forms[0].FormView1_termsTextBox.value = ReturnObj1;
        var terms = document.getElementById("FormView1_termsdescLabel");
        terms.innerHTML = ReturnObj2;
        //document.forms[0].FormView1_termsdescLabel.innerHtml = ReturnObj2;

    }

    function customerlook() {
        var NewWindow = window.open("customer_lookup.aspx", "CustomerLookupWindow", "width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
    }

    function CustomerLookup(ReturnObj1, ReturnObj2, ReturnObj3, ReturnObj4, ReturnObj5, ReturnObj6, ReturnObj7, ReturnObj8, ReturnObj9, ReturnObj10,ReturnObj11) {
        var cust = document.getElementById("FormView1_custLabel");
        cust.value = ReturnObj1;
        var add1 = document.getElementById("FormView1_custadd1Label");
        add1.innerHTML = ReturnObj10;
        var add2 = document.getElementById("FormView1_custadd2Label");
        add2.innerHTML = ReturnObj11;
        var cty = document.getElementById("FormView1_custctyLabel");
        cty.innerHTML = ReturnObj5;
        var stat = document.getElementById("FormView1_custstatLabel");
        stat.innerHTML = ReturnObj6;
        var zip = document.getElementById("FormView1_custzipLabel");
        zip.innerHTML = ReturnObj7;        
        document.forms[0].FormView1_custLabel.focus();
              
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

    function trailerlook() {
        var NewWindow = window.open("trailer_lookup.aspx", "TrailerlookupWindow", "width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
    }
    function TrailerLookUp(ReturnObj1) {
        document.forms[0].FormView1_trailerTextBox.value = ReturnObj1;
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
        var lookHidden = document.getElementById("FormView1_custLabel").value;
        var NewWindow = window.open("ShipIdCustLook.aspx?look=" + lookHidden + "", "ShipToLookupWindow", "width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
    }
    function ShipToLookup(ReturnObj1, ReturnObj2, ReturnObj3, ReturnObj4, ReturnObj5, ReturnObj6, ReturnObj7, ReturnObj8) {
        document.forms[0].FormView1_shiptoTextBox.value = ReturnObj1;
        var ad1 = document.getElementById("FormView1_shipadd1Label");
        ad1.innerHTML = ReturnObj4;
        var ad2 = document.getElementById("FormView1_shipadd2Label");
        ad2.innerHTML = ReturnObj5;
        var city = document.getElementById("FormView1_shipctyLabel");
        city.innerHTML = ReturnObj6;
        var sat = document.getElementById("FormView1_shipstatLabel");
        sat.innerHTML = ReturnObj7;
        var zp = document.getElementById("FormView1_shipzipLabel");
        zp.innerHTML = ReturnObj8;
       // document.forms[0].FormView1_carrierTextBox.value = ReturnObj8;
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

    

    function locationlook() {
        var NewWindow = window.open("location_lookup.aspx", "LocationLookUpWindow", "width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
    }
    function LocationLookUp(ReturnObj1) {
        document.forms[0].FormView2_whseTextBox.value = ReturnObj1;
        document.forms[0].FormView2_whseTextBox.focus();        
    }
    
    function binlook() {
        var loc1 = document.getElementById("FormView2_whseTextBox").value;
        var NewWindow = window.open("custbin_lookup.aspx?binloc=" + loc1 + "", "BinLookUpWindow", "width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
    }
    function CustBinLookup(ReturnObj1) {
        document.forms[0].FormView2_binlocTextBox.value = ReturnObj1;
        document.forms[0].FormView2_binlocTextBox.focus();
    }

    function custordlook() {
        var cst1 = document.getElementById("FormView1_custLabel").innerHTML;        
        var NewWindow = window.open("custord_rellookup.aspx?custord=" + cst1 + "", "CustOrdLookUpWindow", "width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
    }
    function CustOrderLookup(ReturnObj1,ReturnObj2,ReturnObj3,ReturnObj4,ReturnObj5,ReturnObj6,ReturnObj7,ReturnObj8,ReturnObj9,ReturnObj10,ReturnObj11,ReturnObj12,ReturnObj13,ReturnObj14) {
        document.forms[0].FormView2_ordnoTextBox.value = ReturnObj1;
        document.forms[0].FormView2_fgitemTextBox.value = ReturnObj2;
        document.forms[0].FormView2_qtyTextBox.value = ReturnObj3;
        document.forms[0].FormView2_jobnoTextBox.value = ReturnObj4;
        document.forms[0].FormView2_jobno2TextBox.value = ReturnObj5;
        document.forms[0].FormView2_custpartTextBox.value = ReturnObj6;

        document.forms[0].FormView2_whseTextBox.value = ReturnObj7;
        document.forms[0].FormView2_custpoTextBox.value = ReturnObj8;
        document.forms[0].FormView2_scodTextBox.value = ReturnObj9;
        document.forms[0].FormView2_qtyuntTextBox.value = ReturnObj10;
        var relno = document.getElementById("FormView2_relnoLabel");
        relno.innerHTML = ReturnObj11;        
        document.forms[0].FormView2_unitTextBox.value = ReturnObj12;
        document.forms[0].FormView2_partialTextBox.value = ReturnObj13;
       // document.forms[0].FormView2_relseqTextBox.value = ReturnObj14;        
        
        document.forms[0].FormView2_ordnoTextBox.focus();
    }

    function fgordlook() {
        var fgord = document.getElementById("FormView2_ordnoTextBox").innerHTML;        
        var NewWindow = window.open("FgItmOrd_Lookup.aspx?fgord=" + fgord + "", "FGOrdLookupWindow", "width=450,height=300,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
    }

    function FGOrdLookup(ReturnObj1,ReturnObj2) {
        document.forms[0].FormView2_fgitemTextBox.value = ReturnObj1;
        document.forms[0].FormView2_qtyTextBox.value = ReturnObj2;
        document.forms[0].FormView2_fgitemTextBox.focus();

    }
    function custpoordlook() {
        var pord = document.getElementById("FormView2_ordnoTextBox").innerHTML;
        var NewWindow = window.open("CustPoOrd_Lookup.aspx?poord=" + pord + "", "POOrderLookupWindow", "width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
    }

    function CustPOOrdLookup(ReturnObj1, ReturnObj2) {
        document.forms[0].FormView2_custpoTextBox.value = ReturnObj1;
        document.forms[0].FormView2_fgitemTextBox.value = ReturnObj2;
        document.forms[0].FormView2_custpoTextBox.focus();
    }
    function job1look() {
        var NewWindow = window.open("job1_lookup.aspx", "Job1LookupWindow", "width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
    }
    function Job1Lookup(ReturnObj1, ReturnObj2) {
        document.forms[0].FormView2_jobnoTextBox.value = ReturnObj1;
        document.forms[0].FormView2_jobno2TextBox.value = ReturnObj2;
        document.forms[0].FormView2_jobnoTextBox.focus();
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
          <TD align=left nowrap><font size=+0><b>Release Order&nbsp;</b></font></TD>
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
      <table><tr bgcolor="gray"><td> <asp:LinkButton ID="lnk_Listcustomers" runat="server" OnClick="lnk_listinvoice" ><img src="Images/brwsrelease0.jpg" border="0" alt="List Invoices " /></asp:LinkButton>
      <asp:LinkButton ID="lnk_viewcustomers" runat="server" OnClick="lnk_viewcustomers_Click" > <img src="Images/items1.jpg" border="0" alt="View Invoices" /></asp:LinkButton>
      <asp:LinkButton ID="load_viewcustomers" runat="server" OnClick="load_viewcustomers_Click" > <img src="Images/shipnotes0.jpg" border="0" alt="View Invoices" /></asp:LinkButton>  
      </td>
      </tr></table>
       <div>
           <asp:FormView ID="FormView1" runat="server" OnDataBound="FormView1_OnDataBound" DataSourceID="ObjectDataSource1">
               <EditItemTemplate>
                <fieldset><table class="shade">
               <tr><td>
                <table><tr><td align="right" style="padding-right:5px"><b>Customer:</b></td>
               <td><asp:Label ID="custLabel" Width="100px" BackColor="Turquoise" runat="server" Text='<%# Bind("custno") %>' /></td>
               <td align="right" style="padding-right:5px"><b>Ship To:</b></td>
               <td><asp:TextBox ID="shiptoTextBox" Width="100px" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" runat="server" Text='<%# Bind("shipid") %>' />
               <a href="#" tabindex="1" onClick="ShipTOLook(); return false"><asp:Image ID="Image1" runat="server" ImageUrl="images/lookup_icon.gif" /></a></td>
               <td align="right" style="padding-right:5px"><b>Status:</b></td>
               <td><asp:Label ID="statusLabel" Width="70px" BackColor="Turquoise" runat="server" Text='<%# Bind("hold") %>' /></td>
               <td align="right" style="padding-right:5px"><b>Printed?:</b></td>
               <td><asp:Label ID="printedLabel" Width="70px" BackColor="Turquoise" runat="server" Text='<%# Bind("printed") %>' /></td></tr>
               <tr><td></td><td><asp:Label ID="custadd1Label" Width="100px" BackColor="Turquoise" runat="server" Text='<%# Bind("custadd1") %>' /></td>
               <td></td><td><asp:Label ID="shipadd1Label" Width="100px" BackColor="Turquoise" runat="server" Text='<%# Bind("shipadd1") %>' /></td><td></td>
               <td align="right" style="padding-right:5px"><b>Release#:</b></td>
               <td><asp:Label ID="rellLabel" Width="70px" BackColor="Turquoise" runat="server" Text='<%# Bind("rellno") %>' /></td></tr>
               <tr><td></td><td><asp:Label ID="custadd2Label" BackColor="Turquoise" Width="100px" runat="server" Text='<%# Bind("custadd2") %>' /></td>
               <td></td><td><asp:Label ID="shipadd2Label" Width="100px" BackColor="Turquoise" runat="server" Text='<%# Bind("shipadd2") %>' /></td><td></td>
               <td align="right" style="padding-right:5px"><b>Carrier:</b></td>
               <td nowrap colspan="2"><asp:TextBox ID="carrierTextBox" Width="70px" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" runat="server" Text='<%# Bind("carrier") %>' />
               <a href="#" tabindex="1" onClick="carrierlook(); return false"><asp:Image ID="Image2" runat="server" ImageUrl="images/lookup_icon.gif" /></a></td></tr>
               <tr><td></td><td></td><td></td><td></td><td></td><td nowrap align="right" style="padding-right:5px"><b>Release Date:</b></td>
               <td nowrap colspan="2"><asp:TextBox ID="relldateTextBox" Width="70px" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" runat="server" Text='<%# Bind("reldate") %>' />
               <a href="#" tabindex="1" onClick="showCalendarControl(FormView1_relldateTextBox); return false"><asp:Image ID="Image15" runat="server" ImageUrl="images/lookup_icon.gif" /></a></td></tr>
               <tr><td></td><td norap><asp:Label ID="custctyLabel" Width="100px" BackColor="Turquoise" runat="server" Text='<%# Bind("custcty") %>' /></td>
               <td nowrap><asp:Label ID="custstatLabel" Width="40px" BackColor="Turquoise" runat="server" Text='<%# Bind("custstat") %>' />
               <asp:Label ID="custzipLabel" Width="80px" BackColor="Turquoise" runat="server" Text='<%# Bind("custzip") %>' /></td>
               <td nowrap><asp:Label ID="shipctyLabel" Width="100px" BackColor="Turquoise" runat="server" Text='<%# Bind("shipcty") %>' />
               <asp:Label ID="shipstatLabel" Width="40px" BackColor="Turquoise" runat="server" Text='<%# Bind("shipstat") %>' />
               <asp:Label ID="shipzipLabel" Width="80px" BackColor="Turquoise" runat="server" Text='<%# Bind("shipzip") %>' /></td>
               <td align="right" style="padding-right:5px"><b>Dt Chg Rsn:</b></td>
               <td><asp:Label ID="dtchngLabel" Width="50px" BackColor="Turquoise" runat="server" Text='<%# Bind("dtchng") %>' /></td>
               <td align="right" style="padding-right:5px"><b>Usr:</b></td>
               <td><asp:Label ID="usrLabel" Width="50px" BackColor="Turquoise" runat="server" Text='<%# Bind("usr") %>' /></td></tr>
               
               <tr><td colspan="2" align="right" style="padding-right:5px"><fieldset><b>FG Item#:</b>
               <asp:Label ID="fgitmLabel" Width="100px" BackColor="Turquoise" runat="server" Text='<%# Bind("ino") %>' /></fieldset></td>
               <td align="right" style="padding-right:5px">&nbsp;&nbsp;&nbsp;&nbsp;<b>Trailer:</b></td>
               <td><asp:TextBox ID="trailerTextBox" Width="100px" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" runat="server" Text='<%# Bind("trailer") %>' />
               <a href="#" tabindex="1" onClick="trailerlook(); return false"><asp:Image ID="Image3" runat="server" ImageUrl="images/lookup_icon.gif" /></a></td></tr>
               
               <tr><td nowrap colspan="5" align="right" style="padding-right:5px"><fieldset><b>Qty Ordered:</b>
               <asp:Label ID="qtyordLabel" Width="70px" BackColor="Turquoise" runat="server" Text='<%# Bind("qtyord") %>' />
               &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<b>Qty Released:</b>
               <asp:Label ID="qtyrellLabel" Width="70px" BackColor="Turquoise" runat="server" Text='<%# Bind("qtyrel") %>' />
               &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<b>Qty Shipped:</b>
               <asp:Label ID="qtyshipLabel" Width="70px" BackColor="Turquoise" runat="server" Text='<%# Bind("qtyship") %>' />
               &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<b>Qty On Hand:</b>
               <asp:Label ID="qtyhandLabel" Width="70px" BackColor="Turquoise" runat="server" Text='<%# Bind("qtyoh") %>' /></fieldset></td></tr>
               
                             
               </table>               
               </td></tr>
               </table></fieldset>
               <br /><div style="display:none">
                  <asp:Label ID="reckeyTextBox" Visible="false" runat="server" Text='<%# Bind("reckey") %>' />
                
                   <asp:Label ID="extraTextBox" Visible="false" runat="server" Text='<%# Bind("extra") %>' /></div>
               
                   <asp:Button ID="Button1" runat="server" CausesValidation="True" CssClass="button" OnClick="UpdateButton_Click"  Text="Save" />
                   &nbsp;<asp:Button ID="Button2" runat="server" CssClass="button"
                       CausesValidation="False" CommandName="Cancel" Text="Cancel" />               
               </EditItemTemplate>
               <InsertItemTemplate>
                   <fieldset><table class="shade">
               <tr><td>
                <table><tr><td align="right" style="padding-right:5px"><b>Customer:</b></td>
               <td nowrap><asp:TextBox ID="custLabel" Width="100px" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" runat="server" Text='<%# Bind("custno") %>' />
               <a href="#" tabindex="1" onClick="customerlook(); return false"><asp:Image ID="CustomerLook" runat="server" ImageUrl="images/lookup_icon.gif" /></a></td>
               <td align="right" style="padding-right:5px"><b>Ship To:</b></td>
               <td><asp:TextBox ID="shiptoTextBox" Width="100px" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" runat="server" Text='<%# Bind("shipid") %>' />
               <a href="#" tabindex="1" onClick="ShipTOLook(); return false"><asp:Image ID="Image1" runat="server" ImageUrl="images/lookup_icon.gif" /></a></td>
               <td align="right" style="padding-right:5px"><b>Status:</b></td>
               <td><asp:Label ID="statusLabel" Width="70px" BackColor="Turquoise" runat="server" Text='<%# Bind("hold") %>' /></td>
               <td align="right" style="padding-right:5px"><b>Printed?:</b></td>
               <td><asp:Label ID="printedLabel" Width="70px" BackColor="Turquoise" runat="server" Text='<%# Bind("printed") %>' /></td></tr>
               <tr><td></td><td><asp:Label ID="custadd1Label" Width="100px" BackColor="Turquoise" runat="server" Text='<%# Bind("custadd1") %>' /></td>
               <td></td><td><asp:Label ID="shipadd1Label" Width="100px" BackColor="Turquoise" runat="server" Text='<%# Bind("shipadd1") %>' /></td><td></td>
               <td align="right" style="padding-right:5px"><b>Release#:</b></td>
               <td><asp:Label ID="rellLabel" Width="70px" BackColor="Turquoise" runat="server" Text='<%# Bind("rellno") %>' /></td></tr>
               <tr><td></td><td><asp:Label ID="custadd2Label" BackColor="Turquoise" Width="100px" runat="server" Text='<%# Bind("custadd2") %>' /></td>
               <td></td><td><asp:Label ID="shipadd2Label" Width="100px" BackColor="Turquoise" runat="server" Text='<%# Bind("shipadd2") %>' /></td><td></td>
               <td align="right" style="padding-right:5px"><b>Carrier:</b></td>
               <td colspan="2"><asp:TextBox ID="carrierTextBox" Width="70px" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" runat="server" Text='<%# Bind("carrier") %>' />
               <a href="#" tabindex="1" onClick="carrierlook(); return false"><asp:Image ID="Image2" runat="server" ImageUrl="images/lookup_icon.gif" /></a></td></tr>
               <tr><td></td><td></td><td></td><td></td><td></td><td nowrap align="right" style="padding-right:5px"><b>Release Date:</b></td>
               <td colspan="2"><asp:TextBox ID="relldateTextBox" Width="70px" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" runat="server" Text='<%# Bind("reldate") %>' />
               <a href="#" tabindex="1" onClick="showCalendarControl(FormView1_relldateTextBox); return false"><asp:Image ID="Image15" runat="server" ImageUrl="images/lookup_icon.gif" /></a></td></tr>
               <tr><td></td><td norap><asp:Label ID="custctyLabel" Width="100px" BackColor="Turquoise" runat="server" Text='<%# Bind("custcty") %>' /></td>
               <td nowrap><asp:Label ID="custstatLabel" Width="40px" BackColor="Turquoise" runat="server" Text='<%# Bind("custstat") %>' />
               <asp:Label ID="custzipLabel" Width="80px" BackColor="Turquoise" runat="server" Text='<%# Bind("custzip") %>' /></td>
               <td nowrap><asp:Label ID="shipctyLabel" Width="100px" BackColor="Turquoise" runat="server" Text='<%# Bind("shipcty") %>' />
               <asp:Label ID="shipstatLabel" Width="40px" BackColor="Turquoise" runat="server" Text='<%# Bind("shipstat") %>' />
               <asp:Label ID="shipzipLabel" Width="80px" BackColor="Turquoise" runat="server" Text='<%# Bind("shipzip") %>' /></td>
               <td nowrap align="right" style="padding-right:5px"><b>Dt Chg Rsn:</b></td>
               <td><asp:Label ID="dtchngLabel" Width="50px" BackColor="Turquoise" runat="server" Text='<%# Bind("dtchng") %>' /></td>
               <td align="right" style="padding-right:5px"><b>Usr:</b></td>
               <td><asp:Label ID="usrLabel" Width="50px" BackColor="Turquoise" runat="server" Text='<%# Bind("usr") %>' /></td></tr>
               
               <tr><td colspan="2" align="right" style="padding-right:5px"><fieldset><b>FG Item#:</b>
               <asp:Label ID="fgitmLabel" Width="100px" BackColor="Turquoise" runat="server" Text='<%# Bind("ino") %>' /></fieldset></td>
               <td align="right" style="padding-right:5px">&nbsp;&nbsp;&nbsp;&nbsp;<b>Trailer:</b></td>
               <td><asp:TextBox ID="trailerTextBox" Width="100px" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" runat="server" Text='<%# Bind("trailer") %>' />
               <a href="#" tabindex="1" onClick="trailerlook(); return false"><asp:Image ID="Image3" runat="server" ImageUrl="images/lookup_icon.gif" /></a></td></tr>
               
               <tr><td norap colspan="5" align="right" style="padding-right:5px"><fieldset><b>Qty Ordered:</b>
               <asp:Label ID="qtyordLabel" Width="70px" BackColor="Turquoise" runat="server" Text='<%# Bind("qtyord") %>' />
               &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<b>Qty Released:</b>
               <asp:Label ID="qtyrellLabel" Width="70px" BackColor="Turquoise" runat="server" Text='<%# Bind("qtyrel") %>' />
               &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<b>Qty Shipped:</b>
               <asp:Label ID="qtyshipLabel" Width="70px" BackColor="Turquoise" runat="server" Text='<%# Bind("qtyship") %>' />
               &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<b>Qty On Hand:</b>
               <asp:Label ID="qtyhandLabel" Width="70px" BackColor="Turquoise" runat="server" Text='<%# Bind("qtyoh") %>' /></fieldset></td></tr>
               
                             
               </table>               
               </td></tr>
               </table></fieldset>               
                   <br /><div style="display:none">
                  <asp:Label ID="reckeyTextBox" runat="server" Text='<%# Bind("reckey") %>' />
                
                   <asp:TextBox ID="extraTextBox"  runat="server" Text='<%# Bind("extra") %>' /></div>
                   <asp:Button ID="InsertButton" runat="server" CausesValidation="True" CssClass="button" OnClick="InsertButton_Click" Text="Save" />
                   &nbsp;<asp:Button ID="InsertCancelButton" runat="server" CssClass="button" OnClick="InsertCancelButton_Click"
                       CausesValidation="False" CommandName="Cancel" Text="Cancel" />
               </InsertItemTemplate>
               <ItemTemplate>
                   <fieldset><table class="shade">
               <tr><td>
                <table><tr><td align="right" style="padding-right:5px"><b>Customer:</b></td>
               <td> <asp:Label ID="custLabel" Width="100px" BackColor="Turquoise" runat="server" Text='<%# Bind("custno") %>' />                                                                                        
               
               </td>
               <td align="right" style="padding-right:5px"><b>Ship To:</b></td>
               <td><asp:Label ID="shiptoTextBox" Width="100px" BackColor="Turquoise" runat="server" Text='<%# Bind("shipid") %>' /></td>
               <td align="right" style="padding-right:5px"><b>Status:</b></td>
               <td><asp:Label ID="statusLabel" Width="70px" BackColor="Turquoise" runat="server" Text='<%# Bind("hold") %>' /></td>
               <td align="right" style="padding-right:5px"><b>Printed?:</b></td>
               <td><asp:Label ID="printedLabel" Width="70px" BackColor="Turquoise" runat="server" Text='<%# Bind("printed") %>' /></td></tr>
               <tr><td></td><td><asp:Label ID="custadd1Label" Width="100px" BackColor="Turquoise" runat="server" Text='<%# Bind("custadd1") %>' /></td>
               <td></td><td><asp:Label ID="shipadd1Label" Width="100px" BackColor="Turquoise" runat="server" Text='<%# Bind("shipadd1") %>' /></td><td></td>
               <td align="right" style="padding-right:5px"><b>Release#:</b></td>
               <td><asp:Label ID="rellLabel" Width="70px" BackColor="Turquoise" runat="server" Text='<%# Bind("rellno") %>' /></td></tr>
               <tr><td></td><td><asp:Label ID="custadd2Label" BackColor="Turquoise" Width="100px" runat="server" Text='<%# Bind("custadd2") %>' /></td>
               <td></td><td><asp:Label ID="shipadd2Label" BackColor="Turquoise" Width="100px" runat="server" Text='<%# Bind("shipadd2") %>' /></td><td></td>
               <td align="right" style="padding-right:5px"><b>Carrier:</b></td>
               <td><asp:Label ID="carrierTextBox" Width="70px" BackColor="Turquoise" runat="server" Text='<%# Bind("carrier") %>' /></td></tr>
               <tr><td></td><td></td><td></td><td></td><td></td><td nowrap align="right" style="padding-right:5px"><b>Release Date:</b></td>
               <td><asp:Label ID="relldateTextBox" Width="70px" BackColor="Turquoise" runat="server" Text='<%# Bind("reldate") %>' /></td></tr>
               <tr><td></td><td norap><asp:Label ID="custctyLabel" Width="100px" BackColor="Turquoise" runat="server" Text='<%# Bind("custcty") %>' /></td>
               <td norap><asp:Label ID="custstatLabel" Width="40px" BackColor="Turquoise" runat="server" Text='<%# Bind("custstat") %>' />&nbsp;
               <asp:Label ID="custzipLabel" Width="80px" BackColor="Turquoise" runat="server" Text='<%# Bind("custzip") %>' /></td>
               <td norap><asp:Label ID="shipctyLabel" Width="100px" BackColor="Turquoise" runat="server" Text='<%# Bind("shipcty") %>' /></td>
               <td norap><asp:Label ID="shipstatLabel" Width="40px" BackColor="Turquoise" runat="server" Text='<%# Bind("shipstat") %>' />&nbsp;
               <asp:Label ID="shipzipLabel" Width="80px" BackColor="Turquoise" runat="server" Text='<%# Bind("shipzip") %>' /></td>
               <td align="right" nowrap style="padding-right:5px"><b>Dt Chg Rsn:</b></td>
               <td><asp:Label ID="dtchngLabel" Width="50px" BackColor="Turquoise" runat="server" Text='<%# Bind("dtchng") %>' /></td>
               <td align="right" style="padding-right:5px"><b>Usr:</b></td>
               <td><asp:Label ID="usrLabel" Width="50px" BackColor="Turquoise" runat="server" Text='<%# Bind("usr") %>' /></td></tr>
               
               <tr><td colspan="2" align="right" style="padding-right:5px"><fieldset><b>FG Item#:</b>
               <asp:Label ID="fgitmLabel" Width="100px" BackColor="Turquoise" runat="server" Text='<%# Bind("ino") %>' /></fieldset></td>
               <td align="right" style="padding-right:5px"><b>Trailer:</b></td>
               <td><asp:Label ID="trailerTextBox" Width="70px" BackColor="Turquoise" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" runat="server" Text='<%# Bind("trailer") %>' /></td></tr>
               
               <tr><td norap colspan="5" align="right" style="padding-right:5px"><fieldset><b>Qty Ordered:</b>
               <asp:Label ID="qtyordLabel" Width="70px" BackColor="Turquoise" runat="server" Text='<%# Bind("qtyord") %>' />
               &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<b>Qty Released:</b>
               <asp:Label ID="qtyrellLabel" Width="70px" BackColor="Turquoise" runat="server" Text='<%# Bind("qtyrel") %>' />
               &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<b>Qty Shipped:</b>
               <asp:Label ID="qtyshipLabel" Width="70px" BackColor="Turquoise" runat="server" Text='<%# Bind("qtyship") %>' />
               &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<b>Qty On Hand:</b>
               <asp:Label ID="qtyhandLabel" Width="70px" BackColor="Turquoise" runat="server" Text='<%# Bind("qtyoh") %>' /></fieldset></td></tr>
               
                             
               </table>               
               </td></tr>
               </table></fieldset> 
               
                <br />
                <div>
                  <asp:Label ID="reckeyTextBox" Visible="false" runat="server" Text='<%# Bind("reckey") %>' />
                
                   <asp:Label ID="extraTextBox" Visible="false" runat="server" Text='<%# Bind("extra") %>' />
                   <asp:Label ID="postedLabel" Visible="false" runat="server" Text='<%# Bind("posted") %>' /></div>
                   
                   <asp:Button ID="AddButton" runat="server" CssClass="button"  CommandName="new" Text="Add" />
                    <asp:Button ID="UpdateButton" runat="server" CssClass="button" CommandName="edit" Text="Update" />
                   &nbsp;<asp:Button ID="DeleteButton" runat="server" CssClass="button" OnClientClick="return confirm('Are you sure you want to delete this record')" OnClick="delete_Button_Click"  Text="Delete" />
               </ItemTemplate>
           </asp:FormView>
           
           <asp:ObjectDataSource ID="ObjectDataSource1" runat="server" 
                OldValuesParameterFormatString="original_{0}" SelectMethod="ReleaseOrderlist" 
                TypeName="release">
                <SelectParameters>
                   <asp:Parameter DefaultValue="View" Name="prmAction" Type="String" />                    
                   <asp:Parameter Name="prmComp"  Type="string" />
                   <asp:Parameter Name="prmUser" Type="string" />                     
                   <asp:Parameter Name="prmrellno" Type="Int32" />                                        
                   <asp:Parameter Name="prmordno" Type="Int32" />                                        
                   <asp:Parameter Name="prmpono" Type="String" />
                   <asp:Parameter Name="prmcustno" Type="String" />
                   <asp:Parameter Name="prmpartno" Type="String" />
                   <asp:Parameter Name="prmshipid" Type="String" />
                   <asp:Parameter Name="prmino" Type="String" />                   
                   <asp:Parameter Name="prmreldate"  Type="string" />
                   <asp:Parameter Name="prmjobno" Type="string" />                     
                   <asp:Parameter Name="prmjobno2" Type="Int32" />                                        
                   <asp:Parameter Name="prmcarrier" Type="String" />                                        
                   <asp:Parameter Name="prmtrailer" Type="String" />
                   <asp:Parameter Name="prmposted" Type="String" />
                   <asp:Parameter Name="prmship1" Type="String" />
                   <asp:Parameter Name="prmship2" Type="String" />
                   <asp:Parameter Name="prmship3" Type="String" />
                   <asp:Parameter Name="prmship4" Type="String" />
                   <asp:SessionParameter DefaultValue="" Name="prmReckey" SessionField="Release_ord_reckey_rec" 
                       Type="String" />
                   <asp:SessionParameter Name="prmextra" SessionField="Release_ord_r_no" Type="String" />
                </SelectParameters>
            </asp:ObjectDataSource>
       </div>    
       <div>
       <br />
        <asp:GridView ID="GridView1" runat="server" AllowPaging="True" AllowSorting="True" OnSelectedIndexChanged="GridView1_SelectedIndex"
        AutoGenerateColumns="False" CssClass="Grid" DataSourceID="ObjectDataSource2" DataKeyNames="reckey"
        EmptyDataText="No Record Found"  Width="100%">
        <EmptyDataRowStyle BorderColor="Gray" BorderStyle="Dotted" BorderWidth="0px" Font-Bold="True"
            HorizontalAlign="Center" VerticalAlign="Middle" />
        <RowStyle CssClass="shade" />
        <Columns>
             <asp:CommandField ShowSelectButton="true" ButtonType="image" SelectImageUrl="~/Images/sel.gif" SelectText="" > 
                    <ItemStyle Width="10px" />
                    </asp:CommandField>
            <asp:BoundField DataField="ordno" HeaderText="Order#"  SortExpression="ordno" />
            <asp:BoundField DataField="ino" HeaderText="FG Item#" SortExpression="ino" />
            <asp:BoundField DataField="pono" HeaderText="Customer PO"  SortExpression="pono" />
            <asp:BoundField DataField="qty" HeaderText="Qty" SortExpression="qty" />
            <asp:BoundField DataField="tag" HeaderText="Tag" SortExpression="tag" />   
            <asp:BoundField DataField="loc" HeaderText="Whse"  SortExpression="loc" />
            <asp:BoundField DataField="locbin" HeaderText="Bin Loc" SortExpression="locbin" />
            <asp:BoundField DataField="jobno" HeaderText="Job#" SortExpression="jobno" />    
            <asp:BoundField DataField="jobno2" HeaderText=" " SortExpression="jobno2" />
                <asp:BoundField DataField="custno" HeaderText="Customer#"  SortExpression="custno" />
            <asp:BoundField DataField="cases" HeaderText="Units"  SortExpression="cases" />
            <asp:BoundField DataField="qtycas" HeaderText="Qty/Unit" SortExpression="qtycas" />       
            <asp:BoundField DataField="partial" HeaderText="Partial" SortExpression="partial" />
            <asp:BoundField DataField="relno" HeaderText="Rel#" SortExpression="relno" />            
            <asp:BoundField DataField="bordno" HeaderText=" " SortExpression="bordno" />
            <asp:BoundField DataField="scod" HeaderText="S/I" SortExpression="scod" />
            <asp:BoundField DataField="partno" HeaderText="Cust Part#" SortExpression="partno" />
            <asp:BoundField DataField="linkno" HeaderText="Rel Seq#" SortExpression="linkno" />
            
           
            <asp:BoundField DataField="reckey" HeaderText="reckey" Visible="false" SortExpression="reckey" />
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
                   <asp:Panel ID="EditPanel" Width="1400px" Height="120px" DefaultButton="UpdateButton" runat="server" >
                    
                   <fieldset class="shade">
                   <table>
                   <tr>
                   <td><b>Order#:</b></td><td><b>FG Item#:</b></td><td><b>Customer PO:</b></td><td><b>Qty:</b></td>
                   <td><b>Tag:</b></td><td><b>Whse:</b></td><td><b>Bin Loc:</b></td><td><b>Job#:</b></td><td><b></b></td><td><b>Customer#:</b></td>
                   <td><b>Units:</b></td><td><b>Qty/Unit:</b></td><td><b>Partial:</b></td><td><b>Rel#:</b></td><td><b></b></td>
                   <td><b>S/I:</b></td><td><b>Cust Part#:</b></td><td><b>Rel Seq#:</b></td>
                   </tr>
                   <tr><td>
                   <asp:label ID="ordnoTextBox" runat="server" BackColor="Turquoise" Width="70px" Text='<%# Bind("ordno") %>' />
                   </td>
                   <td><asp:TextBox ID="fgitemTextBox" runat="server" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" MaxLength="15" Width="100px" Text='<%# Bind("ino") %>' />
                   <a href="#" tabindex="1" onClick="fgordlook(); return false"><asp:Image ID="Image1" runat="server" ImageUrl="images/lookup_icon.gif" /></a></td>
                   <td>
                   <asp:TextBox ID="custpoTextBox" runat="server" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" MaxLength="15" Width="100px" Text='<%# Bind("pono") %>' />
                   <a href="#" tabindex="1" onClick="custpoordlook(); return false"><asp:Image ID="CustomerPoLookup" runat="server" ImageUrl="images/lookup_icon.gif" /></a></td>
                   <td>
                   <asp:TextBox ID="qtyTextBox" runat="server" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" Width="70px" Text='<%# Bind("qty") %>' />
                   <asp:CompareValidator ID="CompareValidator1" ControlToValidate="qtyTextBox" SetFocusOnError="true" Display="Dynamic" Operator="DataTypeCheck" Type="Double" runat="server" ErrorMessage="Invalid Entry"></asp:CompareValidator></td>
                   <td>
                   <asp:TextBox ID="tagTextBox" runat="server" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" Width="100px" MaxLength="20" Text='<%# Bind("[tag]") %>' />
                   </td>
                   <td><asp:TextBox ID="whseTextBox" runat="server" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" Width="50px" MaxLength="5" Text='<%# Bind("[loc]") %>' />
                   <a href="#" tabindex="1" onClick="locationlook(); return false"><asp:Image ID="Image7" runat="server" ImageUrl="images/lookup_icon.gif" /></a></td>
                   <td><asp:TextBox ID="binlocTextBox" runat="server" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" Width="50px" MaxLength="8" Text='<%# Bind("[locbin]") %>' />
                   <a href="#" tabindex="1" tabindex="1" onClick="binlook(); return false"><asp:Image ID="Image5" runat="server" ImageUrl="images/lookup_icon.gif" /></a></td>
                   <td><asp:TextBox ID="jobnoTextBox" runat="server" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" Width="70px" MaxLength="6" Text='<%# Bind("[jobno]") %>' />
                   </td>
                   <td><asp:TextBox ID="jobno2TextBox" runat="server" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" Width="20px" MaxLength="2" Text='<%# Bind("[jobno2]") %>' />
                   <asp:CompareValidator ID="CompareValidator2" ControlToValidate="jobno2TextBox" SetFocusOnError="true" Display="Dynamic" Operator="DataTypeCheck" Type="Double" runat="server" ErrorMessage="Invalid Entry"></asp:CompareValidator>
                   <a href="#" tabindex="1" onClick="job1look(); return false"><asp:Image ID="Job1Lookup" runat="server" ImageUrl="images/lookup_icon.gif" /></a></td>                   
                   <td><asp:TextBox ID="custnoTextBox" runat="server" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" Width="70px" MaxLength="8" Text='<%# Bind("[custno]") %>' />
                   <a href="#" tabindex="1" onClick="customerlook(); return false"><asp:Image ID="CustomerLook" runat="server" ImageUrl="images/lookup_icon.gif" /></a></td>
                   <td><asp:TextBox ID="unitTextBox" runat="server" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" Width="50px" MaxLength="10" Text='<%# Bind("cases") %>' />
                   <asp:CompareValidator ID="CompareValidator3" ControlToValidate="unitTextBox" SetFocusOnError="true" Display="Dynamic" Operator="DataTypeCheck" Type="Double" runat="server" ErrorMessage="Invalid Entry"></asp:CompareValidator></td>
                   <td><asp:TextBox ID="qtyuntTextBox" runat="server" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" Width="50px" MaxLength="10" Text='<%# Bind("qtycas") %>' />
                   <asp:CompareValidator ID="CompareValidator4" ControlToValidate="qtyuntTextBox" SetFocusOnError="true" Display="Dynamic" Operator="DataTypeCheck" Type="Double" runat="server" ErrorMessage="Invalid Entry"></asp:CompareValidator></td>
                   <td><asp:TextBox ID="partialTextBox" runat="server" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" Width="70px" MaxLength="10" Text='<%# Bind("partial") %>' />
                   <asp:CompareValidator ID="CompareValidator9" ControlToValidate="partialTextBox" SetFocusOnError="true" Display="Dynamic" Operator="DataTypeCheck" Type="Double" runat="server" ErrorMessage="Invalid Entry"></asp:CompareValidator></td>
                   <td><asp:Label ID="relnoLabel" runat="server" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" Width="40px" MaxLength="10" Text='<%# Bind("relno") %>' />
                   </td>
                   <td><asp:Label ID="boardLabel" runat="server" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" Width="20px" MaxLength="10" Text='<%# Bind("bordno") %>' />
                   </td>
                   <td><asp:TextBox ID="scodTextBox" runat="server" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" Width="20px" MaxLength="10" Text='<%# Bind("scod") %>' />
                   </td>
                   <td><asp:TextBox ID="custpartTextBox" runat="server" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" Width="100px" MaxLength="10" Text='<%# Bind("partno") %>' />
                   </td>
                   <td><asp:TextBox ID="relseqTextBox" runat="server" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" Width="70px" MaxLength="10" Text='<%# Bind("linkno") %>' />
                   </td>
                   
                   <td><asp:TextBox ID="reckeyTextBox" Visible="false" runat="server" Text='<%# Bind("reckey") %>' /></td>
                   <td><asp:TextBox ID="extraTextBox" Visible="false" runat="server" Text='<%# Bind("extra") %>' /></td>
                  
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
                   <td><b>Order#:</b></td><td><b>FG Item#:</b></td><td><b>Customer PO:</b></td><td><b>Qty:</b></td>
                   <td><b>Tag:</b></td><td><b>Whse:</b></td><td><b>Bin Loc:</b></td><td><b>Job#:</b></td><td><b></b></td><td><b>Customer#:</b></td>
                   <td><b>Units:</b></td><td><b>Qty/Unit:</b></td><td><b>Partial:</b></td><td><b>Rel#:</b></td><td><b></b></td>
                   <td><b>S/I:</b></td><td><b>Cust Part#:</b></td><td><b>Rel Seq#:</b></td>
                   </tr>
                  <tr><td>
                   <asp:TextBox ID="ordnoTextBox" runat="server" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" MaxLength="8" Width="70px" Text='<%# Bind("ordno") %>' />
                   <a href="#" tabindex="1" tabindex="1" onClick="custordlook(); return false"><asp:Image ID="Image4" runat="server" ImageUrl="images/lookup_icon.gif" /></a></td>
                   <td><asp:TextBox ID="fgitemTextBox" runat="server" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" MaxLength="15" Width="100px" Text='<%# Bind("ino") %>' />
                   <a href="#" tabindex="1" onClick="fgordlook(); return false"><asp:Image ID="Image1" runat="server" ImageUrl="images/lookup_icon.gif" /></a></td>
                   <td>
                   <asp:TextBox ID="custpoTextBox" runat="server" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" MaxLength="15" Width="100px" Text='<%# Bind("pono") %>' />
                   <a href="#" tabindex="1" onClick="custpoordlook(); return false"><asp:Image ID="CustomerPoLookup" runat="server" ImageUrl="images/lookup_icon.gif" /></a></td>
                   <td>
                   <asp:TextBox ID="qtyTextBox" runat="server" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" Width="70px" Text='<%# Bind("qty") %>' />
                   <asp:CompareValidator ID="CompareValidator1" ControlToValidate="qtyTextBox" SetFocusOnError="true" Display="Dynamic" Operator="DataTypeCheck" Type="Double" runat="server" ErrorMessage="Invalid Entry"></asp:CompareValidator></td>
                   <td>
                   <asp:TextBox ID="tagTextBox" runat="server" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" Width="100px" MaxLength="20" Text='<%# Bind("[tag]") %>' />
                   </td>
                   <td><asp:TextBox ID="whseTextBox" runat="server" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" Width="50px" MaxLength="5" Text='<%# Bind("[loc]") %>' />
                   <a href="#" tabindex="1" onClick="locationlook(); return false"><asp:Image ID="Image7" runat="server" ImageUrl="images/lookup_icon.gif" /></a></td>
                   <td><asp:TextBox ID="binlocTextBox" runat="server" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" Width="50px" MaxLength="8" Text='<%# Bind("[locbin]") %>' />
                   <a href="#" tabindex="1" tabindex="1" onClick="binlook(); return false"><asp:Image ID="Image5" runat="server" ImageUrl="images/lookup_icon.gif" /></a></td>
                   <td><asp:TextBox ID="jobnoTextBox" runat="server" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" Width="70px" MaxLength="6" Text='<%# Bind("[jobno]") %>' />
                   </td>
                   <td><asp:TextBox ID="jobno2TextBox" runat="server" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" Width="20px" MaxLength="2" Text='<%# Bind("[jobno2]") %>' />
                   <asp:CompareValidator ID="CompareValidator2" ControlToValidate="jobno2TextBox" SetFocusOnError="true" Display="Dynamic" Operator="DataTypeCheck" Type="Double" runat="server" ErrorMessage="Invalid Entry"></asp:CompareValidator>
                   <a href="#" tabindex="1" onClick="job1look(); return false"><asp:Image ID="Job1Lookup" runat="server" ImageUrl="images/lookup_icon.gif" /></a></td>                   
                   <td><asp:TextBox ID="custnoTextBox" runat="server" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" Width="70px" MaxLength="8" Text='<%# Bind("[custno]") %>' />
                   <a href="#" tabindex="1" onClick="customerlook(); return false"><asp:Image ID="CustomerLook" runat="server" ImageUrl="images/lookup_icon.gif" /></a></td>
                   <td><asp:TextBox ID="unitTextBox" runat="server" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" Width="50px" MaxLength="10" Text='<%# Bind("cases") %>' />
                   <asp:CompareValidator ID="CompareValidator3" ControlToValidate="unitTextBox" SetFocusOnError="true" Display="Dynamic" Operator="DataTypeCheck" Type="Double" runat="server" ErrorMessage="Invalid Entry"></asp:CompareValidator></td>
                   <td><asp:TextBox ID="qtyuntTextBox" runat="server" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" Width="50px" MaxLength="10" Text='<%# Bind("qtycas") %>' />
                   <asp:CompareValidator ID="CompareValidator4" ControlToValidate="qtyuntTextBox" SetFocusOnError="true" Display="Dynamic" Operator="DataTypeCheck" Type="Double" runat="server" ErrorMessage="Invalid Entry"></asp:CompareValidator></td>
                   <td><asp:TextBox ID="partialTextBox" runat="server" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" Width="70px" MaxLength="10" Text='<%# Bind("partial") %>' />
                   <asp:CompareValidator ID="CompareValidator9" ControlToValidate="partialTextBox" SetFocusOnError="true" Display="Dynamic" Operator="DataTypeCheck" Type="Double" runat="server" ErrorMessage="Invalid Entry"></asp:CompareValidator></td>
                   <td><asp:Label ID="relnoLabel" runat="server" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" Width="40px" MaxLength="10" Text='<%# Bind("relno") %>' />
                   </td>
                   <td><asp:Label ID="boardLabel" runat="server" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" Width="20px" MaxLength="10" Text='<%# Bind("bordno") %>' />
                   </td>
                   <td><asp:TextBox ID="scodTextBox" runat="server" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" Width="20px" MaxLength="10" Text='<%# Bind("scod") %>' />
                   </td>
                   <td><asp:TextBox ID="custpartTextBox" runat="server" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" Width="100px" MaxLength="10" Text='<%# Bind("partno") %>' />
                   </td>
                   <td><asp:TextBox ID="relseqTextBox" runat="server" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" Width="70px" MaxLength="10" Text='<%# Bind("linkno") %>' />
                   </td>
                   <td><asp:TextBox ID="reckeyTextBox" Visible="false" runat="server" Text='<%# Bind("reckey") %>' /></td>
                   <td><asp:TextBox ID="extraTextBox" Visible="false" runat="server" Text='<%# Bind("extra") %>' /></td>
                  
                    </tr></table>   
                    
                   <asp:Button ID="InsertButton" runat="server" CausesValidation="True" CssClass="button" OnClick="AddButton_Formview2_Click" Text="Save" />
                   &nbsp;<asp:Button ID="InsertCancelButton" runat="server" CssClass="button" CausesValidation="False" OnClick="CancelButton_FormView2_Delete" CommandName="Cancel" Text="Cancel" />
                   </fieldset></asp:Panel>
               </InsertItemTemplate>
               <ItemTemplate>
                  
                   <asp:Label ID="ReckeyLabel" Visible="false" runat="server" Text='<%# Bind("[reckey]") %>'></asp:Label>
                   <asp:TextBox ID="extraTextBox" Visible="false" runat="server" Text='<%# Bind("extra") %>' />
                   
                    <asp:Button ID="AddButton" runat="server" CssClass="button" CommandName="New" Text="Add" />
                    <asp:Button ID="UpdateItemButton" runat="server" CssClass="button" CommandName="Edit" Text="Update" />
                   &nbsp;<asp:Button ID="DeleteButton" runat="server" CssClass="button" OnClientClick="return confirm('Are you sure you want to delete this record')" OnClick="deleteButton_FormView2_Click" Text="Delete" />
                   <%--<asp:Button ID="Button1" runat="server" CssClass="button" OnClick="BoardReclButton_Click"  Text="Board $ Recalc" />--%>
               </ItemTemplate>
           </asp:FormView>
            <asp:Button ID="AddNewFormView2Button" runat="server" CssClass="button"  OnClick="AddNewFormView2Button_Click" Text="Add" />
    
    
           <asp:ObjectDataSource ID="ObjectDataSource3" runat="server" 
               OldValuesParameterFormatString="original_{0}" SelectMethod="ViewReleaseOrder" 
               TypeName="release">
                <SelectParameters>
                   <asp:Parameter DefaultValue="View" Name="prmAction" Type="String" />                    
                   <asp:Parameter Name="prmComp"  Type="string" />
                   <asp:Parameter Name="prmUser" Type="string" />                     
                   <asp:Parameter Name="prmordno2" Type="Int32" />
                   <asp:Parameter  Name="prmino2" Type="String" />
                   <asp:Parameter Name="prmpono2" Type="String" />
                     
                   <asp:Parameter Name="prmqty" Type="Int32" />
                     
                   <asp:Parameter  Name="prmtag" Type="String" />
                   <asp:Parameter  Name="prmloc" Type="String" />
                   <asp:Parameter Name="prmlocbin" Type="String" />
                   <asp:Parameter  Name="prmjobno" Type="String" />
                   <asp:Parameter Name="prmjobno2" Type="Int32" />
                   <asp:Parameter Name="prmcustno" Type="String" />
                   <asp:Parameter Name="prmcases" Type="Int32" />
                   <asp:Parameter Name="prmqtycas" Type="Int32" />
                   <asp:Parameter Name="prmpartial" Type="Int32" />
                   <asp:Parameter Name="prmrelno2" Type="Int32" />
                   <asp:Parameter Name="prmbordno" Type="Int32" />
                   <asp:Parameter Name="prmscod" Type="String" />
                   <asp:Parameter Name="prmpartno" Type="String" />
                   <asp:Parameter Name="prmlinkno" Type="Int32" />
                    <asp:SessionParameter SessionField="view_rel_order_reckey_rec" Name="prmReckey" Type="String" />
                   <asp:SessionParameter Name="prmextra2" SessionField="Release_ord_r_no" Type="String" />
                </SelectParameters>
           </asp:ObjectDataSource>
    
    
    
           <asp:ObjectDataSource ID="ObjectDataSource2" runat="server" 
               OldValuesParameterFormatString="original_{0}" SelectMethod="ViewReleaseOrder" 
               TypeName="release">
               <SelectParameters>
                   <asp:Parameter DefaultValue="Gridview" Name="prmAction" Type="String" />
                   <asp:Parameter Name="prmComp" Type="String" />
                   <asp:Parameter Name="prmUser" Type="String" />
                   <asp:Parameter Name="prmordno2" Type="Int32" />
                   <asp:Parameter Name="prmino2" Type="String" />
                   <asp:Parameter Name="prmpono2" Type="String" />
                   <asp:Parameter Name="prmqty" Type="Int32" />
                   <asp:Parameter Name="prmtag" Type="String" />
                   <asp:Parameter Name="prmloc" Type="String" />
                   <asp:Parameter Name="prmlocbin" Type="String" />
                   <asp:Parameter Name="prmjobno" Type="String" />
                   <asp:Parameter Name="prmjobno2" Type="Int32" />
                   <asp:Parameter Name="prmcustno" Type="String" />
                   <asp:Parameter Name="prmcases" Type="Int32" />
                   <asp:Parameter Name="prmqtycas" Type="Int32" />
                   <asp:Parameter Name="prmpartial" Type="Int32" />
                   <asp:Parameter Name="prmrelno2" Type="Int32" />
                   <asp:Parameter Name="prmbordno" Type="Int32" />
                   <asp:Parameter Name="prmscod" Type="String" />
                   <asp:Parameter Name="prmpartno" Type="String" />
                   <asp:Parameter Name="prmlinkno" Type="Int32" />
                   <asp:SessionParameter DefaultValue="" Name="prmReckey" SessionField="Release_ord_reckey_rec" 
                       Type="String" />
                   <asp:SessionParameter Name="prmextra2" SessionField="Release_ord_r_no" Type="String" />
               </SelectParameters>
           </asp:ObjectDataSource>
       </div>
       
    </div>
    </td></tr></table>    
    <ft:footer id="Footer1" runat="server"></ft:footer>
    </form>
  </body>
</HTML>

