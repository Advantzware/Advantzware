<%@ Page Language="c#" AutoEventWireup="true" Debug="true" Inherits="view_gl_bank" Codebehind="view_gl_bank.aspx.cs" %>
<%@ Register Src="footer.ascx" TagName="Footer" TagPrefix="ft" %>
<%@ Register Src="header.ascx" TagName="Header" TagPrefix="hd" %>
<html xmlns="http://www.w3.org/1999/xhtml" >
  <head id="Head1" runat="server">
    <title>Banks</title>
    <LINK href="include/style2.css" type="text/css" rel="stylesheet"/>
    <LINK REL="stylesheet" TYPE="text/css" HREF="include/CalendarControl.css" >
    <script language = "JavaScript" src="include/CalendarControl.js"></script>
     <script language="javascript" src="include/date.js"></script>
    <script language="javascript" src="include/event.js"></script>
    <script language="javascript" src="include/insert.js"></script>
    <script language="javascript" src="include/validate2.js"></script>
    <script language =javascript>
   
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
     

    function vendorlook() {

        var NewWindow = window.open("corvend_lookup.aspx", "UomLookup", "width=500,height=400,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
    }
    function VendLookup(ReturnObj1, ReturnObj2) {
        document.forms[0].FormView1_vendnoTextBox.value = ReturnObj1;
        var vname = document.getElementById("FormView1_vendnameLabel");
        vname.innerHTML = ReturnObj2;
        document.forms[0].FormView1_vendnoTextBox.focus();
    }

//    function AccountLook() {
//        var NewWindow = window.open("accountlook.aspx", "AccountLookupWindow", "width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
//    }

//    function AccountLookup(ReturnObj1, ReturnObj2) {
//        document.forms[0].FormView1_actnumTextBox.value = ReturnObj1;
//        var actdsc = document.getElementById("FormView1_accdscrTextBox");
//        actdsc.innerHTML = ReturnObj2;
//    }

    function invinflook() {
        
         var vend = document.getElementById("FormView1_vendnoLabel");
       
        var NewWindow = window.open("invinfo_lookup.aspx?vend=" + vend.innerHTML + "", "invinfoWindow", "width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
        //var NewWindow = window.open("invinfo_lookup.aspx", "invinfoWindow", "width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
    }

    function InvInfoLookup(ReturnObj1, ReturnObj2, ReturnObj3, ReturnObj4, ReturnObj5, ReturnObj6) {
        document.forms[0].FormView2_invTextBox.value = ReturnObj1;
        document.forms[0].FormView2_duedateLabel.value = ReturnObj2;
        document.forms[0].FormView2_baldueLabel.value = ReturnObj3;
        document.forms[0].FormView2_cramtTextBox.value = ReturnObj4;
        document.forms[0].FormView2_dbamtTextBox.value = ReturnObj5;
        document.forms[0].FormView2_actnumTextBox.value = ReturnObj6;
       

        document.forms[0].FormView2_invTextBox.onchange();
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

   function invfocus() {
       var inv = document.getElementById("FormView2_invTextBox");
       inv.focus();
   }
   function cramtfocus() {
       var crt = document.getElementById("FormView2_cramtTextBox");
       crt.focus();
   }
   function datefocus() {
       var vdate = document.getElementById("FormView1_memodateTextBox");
       vdate.focus();
   }
   function AccountLook() {
       var NewWindow = window.open("accountlook.aspx?jrnl=" + "A" + " ", "AccountLookupWindow", "width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
   }

   function AccountLookup(ReturnObj1, ReturnObj2) {
        document.forms[0].FormView1_actnumTextBox.value = ReturnObj1;
        var actdsc = document.getElementById("FormView1_accdscrTextBox");
        actdsc.innerHTML = ReturnObj2;
    }
   function ordernotes() {
       var NewWindow = window.open("top_list_notes.aspx", "ListNotes", "width=600,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
   }
   function printgl() {
       var NewWindow = window.open("topprintglbank_report.aspx", "OrderReport", "width=600,height=600,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
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
                                        
                    <td nowrap width="25px";>
                        <asp:ImageButton ID="img_btn_add" runat="server" Width="35px" ImageUrl="~/Images/add.bmp" ToolTip="Add" OnClick="img_btn_add_click" />
                        </td>
                        <td nowrap width="25px";>
                        <a href="#" onClick="ordernotes(); return false"><asp:Image ID="img_btn_notes" Width="35px" ToolTip="Notes" runat="server" ImageUrl="~/Images/edit.ico" /></a>                        
                        </td>
                        <td nowrap width="25px";>
                        <a href="#" onClick="printgl(); return false"><asp:Image ID="Image1" Width="35px" runat="server" ImageUrl="~/Images/print-u.bmp" /></a>
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
          <TD align=left nowrap><font size=+0><b>Banks</b></font></TD>
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
      <table><tr bgcolor="gray"><td><div  id="navigation" style="width:100%">
		<ul nowrap> <li >
      <asp:LinkButton ID="lnk_Listcustomers" runat="server" OnClick="lnk_listglaccount" >Browse Bank</asp:LinkButton></li>
      <li class="selected"><asp:LinkButton ID="lnk_viewcustomers" runat="server" OnClick="lnk_viewglaccount_Click" > View Bank</asp:LinkButton></li></ul></div>
      
      </td>
      </tr></table>
       <div>
           <asp:FormView ID="FormView1" runat="server" DataSourceID="ObjectDataSource1">
               <EditItemTemplate>
               <asp:Panel ID="editpanel" Width="600" CssClass="shade" runat="server" DefaultButton="">
               <%--<table width="550"><tr><td align="center">--%>
               <fieldset>               
               <table class="shade">
               <tr><td nowrap><b>Bank:</b>
               <asp:TextBox ID="bank_codeTextBox" Width="70" runat="server" Text='<%# Bind("[bank-code]") %>' /></td>
               <td align="right" style="padding-right: 5px"><b>Bank Name:</b>
               <asp:TextBox ID="bank_nameTextBox" Width="150" runat="server" Text='<%# Bind("[bank-name]") %>' /></td></tr>
               <tr><td></td><td align="right" style="padding-right: 5px"><b>Address:</b>
               <asp:TextBox ID="addr1TextBox" Width="150" runat="server" Text='<%# Bind("addr1") %>' /></td></tr>
               <tr><td></td>
               <td align="right" style="padding-right: 5px"><asp:TextBox ID="addr2TextBox" Width="150" runat="server" Text='<%# Bind("addr2") %>' /></td></tr>
               <tr><td></td><td align="right" style="padding-right: 5px"><b>City:</b>
               <asp:TextBox ID="cityTextBox" Width="150" runat="server" Text='<%# Bind("city") %>' /></td>
               <td><asp:TextBox ID="stateTextBox" Width="40" runat="server" Text='<%# Bind("state") %>' />
               <asp:TextBox ID="zipTextBox" Width="70" runat="server" Text='<%# Bind("zip") %>' /></td></tr>
               <tr><td></td><td align="right" style="padding-right: 5px"><b>Phone Number:</b>
               <asp:TextBox ID="phoneTextBox" Width="150" runat="server" Text='<%# Bind("phone") %>' /></td></tr>
               <tr><td></td><td align="right" style="padding-right: 5px"><b>Contact Number:</b>
               <asp:TextBox ID="contactTextBox" Width="150" runat="server" Text='<%# Bind("contact") %>' /></td></tr>
               
               </table></fieldset>
               
               <fieldset>               
               <table class="shade" width="550">               
               <tr><td></td><td align="right" style="padding-right: 5px">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
               <b>Bank Account#:</b></td>
               <td><asp:TextBox ID="bk_actTextBox" Width="150" runat="server" Text='<%# Bind("[bk-act]") %>' /></td></tr>
               <tr><td></td><td align="right" style="padding-right: 5px">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
               <b>GL Account#:</b></td>
               <td><asp:TextBox ID="actnumTextBox" Width="150" runat="server" Text='<%# Bind("actnum") %>' />
               <a href="#" tabindex="1" onClick="AccountLook(); return false"><asp:Image ID="Image13" runat="server" ImageUrl="images/lookup_icon.gif" /></a></td>
               <td><asp:Label ID="accdscrTextBox" Width="150" runat="server" Text='<%# Bind("accdscr") %>' /></td></tr>                   
               
               </table></fieldset>
               
               <fieldset>               
               <table class="shade">
               <tr><td></td><td align="right" style="padding-right: 5px">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
               &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<b>Last Check # Used:</b>
               <asp:TextBox ID="last_chkTextBox" Width="100" runat="server" Text='<%# Bind("[last-chk]") %>' /></td></tr>
               <tr><td></td><td align="right" style="padding-right: 5px"><b>Account Banalce:</b>
               <asp:TextBox ID="balTextBox" Width="100" runat="server" Text='<%# Bind("bal") %>' /></td></tr>
               <tr><td></td><td align="right" style="padding-right: 5px"><b>Outstanding Balance:</b>
               <asp:TextBox ID="o_chkTextBox" Width="100" runat="server" Text='<%# Bind("[o-chk]") %>' /></td></tr>
               <tr><td></td><td align="right" style="padding-right: 5px"><b>Deposits in Transit:</b>
               <asp:TextBox ID="dep_trTextBox" Width="100" runat="server" Text='<%# Bind("[dep-tr]") %>' /></td></tr>
               <tr><td></td><td align="right" style="padding-right: 5px"><b>Service Charge:</b>
               <asp:TextBox ID="servTextBox" Width="100" runat="server" Text='<%# Bind("serv") %>' /></td></tr>
               <tr><td></td><td align="right" style="padding-right: 5px"><b>Currency Code:</b>
               <asp:TextBox ID="curr_codeTextBox" Width="100" runat="server" Text='<%# Bind("[curr-code]") %>' /></td></tr> 
                  
                </table></fieldset>    
                 <%--</td></tr></table>  --%>                  
                   
                  
                   <asp:TextBox ID="reckeyTextBox" Visible="false" runat="server" Text='<%# Bind("reckey") %>' />
                                      
                   <asp:Button ID="UpdateButton" runat="server" CausesValidation="True" OnClick="UpdateButton_Click" CssClass="button" Text="Save" />
                   &nbsp;<asp:Button ID="UpdateCancelButton" runat="server" CausesValidation="False" CssClass="button" CommandName="Cancel" Text="Cancel" />
                   </asp:Panel>
               </EditItemTemplate>
               <InsertItemTemplate>
                   <asp:Panel ID="editpanel" Width="600" CssClass="shade" runat="server" DefaultButton="">
               <%--<table width="550"><tr><td align="center"> --%>
               <fieldset>               
               <table class="shade">
               <tr><td nowrap><b>Bank:</b>
               <asp:TextBox ID="bank_codeTextBox" Width="70" runat="server" Text='<%# Bind("[bank-code]") %>' /></td>
               <td align="right" style="padding-right: 5px"><b>Bank Name:</b>
               <asp:TextBox ID="bank_nameTextBox" Width="150" runat="server" Text='<%# Bind("[bank-name]") %>' /></td></tr>
               <tr><td></td><td align="right" style="padding-right: 5px"><b>Address:</b>
               <asp:TextBox ID="addr1TextBox" Width="150" runat="server" Text='<%# Bind("addr1") %>' /></td></tr>
               <tr><td></td>
               <td align="right" style="padding-right: 5px"><asp:TextBox ID="addr2TextBox" Width="150" runat="server" Text='<%# Bind("addr2") %>' /></td></tr>
               <tr><td></td><td align="right" style="padding-right: 5px"><b>City:</b>
               <asp:TextBox ID="cityTextBox" Width="150" runat="server" Text='<%# Bind("city") %>' /></td>
               <td><asp:TextBox ID="stateTextBox" Width="40" runat="server" Text='<%# Bind("state") %>' />
               <asp:TextBox ID="zipTextBox" Width="70" runat="server" Text='<%# Bind("zip") %>' /></td></tr>
               <tr><td></td><td align="right" style="padding-right: 5px"><b>Phone Number:</b>
               <asp:TextBox ID="phoneTextBox" Width="150" runat="server" Text='<%# Bind("phone") %>' /></td></tr>
               <tr><td></td><td align="right" style="padding-right: 5px"><b>Contact Number:</b>
               <asp:TextBox ID="contactTextBox" Width="150" runat="server" Text='<%# Bind("contact") %>' /></td></tr>
               
               </table></fieldset>
               
               <fieldset>               
               <table class="shade" width="550">               
               <tr><td></td><td align="right" style="padding-right: 5px">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
               <b>Bank Account#:</b></td>
               <td><asp:TextBox ID="bk_actTextBox" Width="150" runat="server" Text='<%# Bind("[bk-act]") %>' /></td></tr>
               <tr><td></td><td align="right" style="padding-right: 5px">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
               <b>GL Account#:</b></td>
               <td><asp:TextBox ID="actnumTextBox" Width="150" runat="server" Text='<%# Bind("actnum") %>' />
               <a href="#" tabindex="1" onClick="AccountLook(); return false"><asp:Image ID="Image13" runat="server" ImageUrl="images/lookup_icon.gif" /></a></td>
               <td><asp:Label ID="accdscrTextBox" Width="150" runat="server" Text='<%# Bind("accdscr") %>' /></td></tr>                   
               
               </table></fieldset>
               
               <fieldset>               
               <table class="shade">
               <tr><td></td><td align="right" style="padding-right: 5px">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
               &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<b>Last Check # Used:</b>
               <asp:TextBox ID="last_chkTextBox" Width="100" runat="server" Text='<%# Bind("[last-chk]") %>' /></td></tr>
               <tr><td></td><td align="right" style="padding-right: 5px"><b>Account Banalce:</b>
               <asp:TextBox ID="balTextBox" Width="100" runat="server" Text='<%# Bind("bal") %>' /></td></tr>
               <tr><td></td><td align="right" style="padding-right: 5px"><b>Outstanding Balance:</b>
               <asp:TextBox ID="o_chkTextBox" Width="100" runat="server" Text='<%# Bind("[o-chk]") %>' /></td></tr>
               <tr><td></td><td align="right" style="padding-right: 5px"><b>Deposits in Transit:</b>
               <asp:TextBox ID="dep_trTextBox" Width="100" runat="server" Text='<%# Bind("[dep-tr]") %>' /></td></tr>
               <tr><td></td><td align="right" style="padding-right: 5px"><b>Service Charge:</b>
               <asp:TextBox ID="servTextBox" Width="100" runat="server" Text='<%# Bind("serv") %>' /></td></tr>
               <tr><td></td><td align="right" style="padding-right: 5px"><b>Currency Code:</b>
               <asp:TextBox ID="curr_codeTextBox" Width="100" runat="server" Text='<%# Bind("[curr-code]") %>' /></td></tr> 
                  
                </table></fieldset>    
                 <%--</td></tr></table>  --%>
                    
                   
                   
                   <asp:TextBox ID="reckeyTextBox" Visible="false" runat="server" Text='<%# Bind("reckey") %>' />
                  
                   <asp:Button ID="InsertButton" runat="server" CausesValidation="True" OnClick="addButton_Click" CssClass="button" Text="Save" />
                   &nbsp;<asp:Button ID="InsertCancelButton" runat="server" CausesValidation="False" CssClass="button" CommandName="Cancel" Text="Cancel" />
                   </asp:Panel>
               </InsertItemTemplate>
               <ItemTemplate>
               <asp:Panel ID="exitpanel" CssClass="shade" width="500px" runat="server" DefaultButton="">    
               <%--<table width="500"><tr><td align="center">--%>
               <fieldset>                 
               <table class="shade">
               <tr><td nowrap><b>Bank:</b>
               <asp:Label ID="bank_codeTextBox" Width="70" BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" runat="server" Text='<%# Bind("[bank-code]") %>' /></td>
               <td align="right" style="padding-right: 5px"><b>Bank Name:</b></td>
               <td><asp:Label ID="bank_nameTextBox" Width="150" BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" runat="server" Text='<%# Bind("[bank-name]") %>' /></td></tr>
               <tr><td></td><td align="right" style="padding-right: 5px"><b>Address:</b></td>
               <td><asp:Label ID="addr1TextBox" Width="150" BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" runat="server" Text='<%# Bind("addr1") %>' /></td></tr>
               <tr><td></td><td></td>
               <td><asp:Label ID="addr2TextBox" Width="150" BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" runat="server" Text='<%# Bind("addr2") %>' /></td></tr>
               <tr><td></td><td align="right" style="padding-right: 5px"><b>City:</b></td>
               <td><asp:Label ID="cityTextBox" Width="150" BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" runat="server" Text='<%# Bind("city") %>' /></td>
               </td><td><asp:Label ID="stateTextBox" Width="40" BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" runat="server" Text='<%# Bind("state") %>' />
               <asp:Label ID="zipTextBox" Width="70" BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" runat="server" Text='<%# Bind("zip") %>' /></td></tr>
               <tr><td></td><td align="right" style="padding-right: 5px"><b>Phone Number:</b></td>
               <td><asp:Label ID="phoneTextBox" Width="150" BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" runat="server" Text='<%# Bind("phone") %>' /></td></tr>
               <tr><td></td><td align="right" style="padding-right: 5px"><b>Contact Number:</b></td>
               <td><asp:Label ID="contactTextBox" Width="150" BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" runat="server" Text='<%# Bind("contact") %>' /></td></tr>
               
               </table></fieldset>
               
               <fieldset>               
               <table class="shade" width="500">           
               <tr>
               <td align="right" style="padding-right: 5px"> &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
               <b>Bank Account#:</b></td>
               <td><asp:Label ID="bk_actTextBox" Width="100" BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" runat="server" Text='<%# Bind("[bk-act]") %>' /></td></tr>
               <tr><td align="right" style="padding-right: 5px"><b>G/L Account#:</b></td>
               <td><asp:Label ID="actnumTextBox" Width="100" BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" runat="server" Text='<%# Bind("actnum") %>' />
               <asp:Label ID="accdscrTextBox" Width="150" runat="server" Text='<%# Bind("accdscr") %>' /></td></tr>                   
               
               </table></fieldset>
                
                          
               
               <fieldset>               
               <table class="shade">
               <tr><td></td><td align="right" style="padding-right: 5px">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
               &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<b>Last Check # Used:</b></td>
               <td><asp:Label ID="last_chkTextBox" Width="100" BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" runat="server" Text='<%# Bind("[last-chk]") %>' /></td></tr>
               <tr><td></td><td align="right" style="padding-right: 5px"><b>Account Banalce:</b></td>
               <td><asp:Label ID="balTextBox" Width="100" BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" runat="server" Text='<%# Bind("bal") %>' /></td></tr>
               <tr><td></td><td align="right" style="padding-right: 5px"><b>Outstanding Balance:</b></td>
               <td><asp:Label ID="o_chkTextBox" Width="100" BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" runat="server" Text='<%# Bind("[o-chk]") %>' /></td></tr>
               <tr><td></td><td align="right" style="padding-right: 5px"><b>Deposits in Transit:</b></td>
               <td><asp:Label ID="dep_trTextBox" Width="100" BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" runat="server" Text='<%# Bind("[dep-tr]") %>' /></td></tr>
               <tr><td></td><td align="right" style="padding-right: 5px"><b>Service Charge:</b></td>
               <td><asp:Label ID="servTextBox" Width="100" BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" runat="server" Text='<%# Bind("serv") %>' /></td></tr>
               <tr><td></td><td align="right" style="padding-right: 5px"><b>Currency Code:</b></td>
               <td><asp:Label ID="curr_codeTextBox" Width="100" BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" runat="server" Text='<%# Bind("[curr-code]") %>' /></td></tr> 
                  
                </table></fieldset>    
                   
             <%-- </td></tr></table>      --%>            
                   
                   
                   <asp:TextBox ID="reckeyTextBox" Visible="false" runat="server" Text='<%# Bind("reckey") %>' />
                   
                   <asp:Button ID="AddButton" runat="server" CssClass="button"  CommandName="new" Text="Add" />
                   &nbsp;<asp:Button ID="UpdateButton" runat="server" CssClass="button" CommandName="edit" Text="Update" />
                   &nbsp;<asp:Button ID="DeleteButton" runat="server" CssClass="button" OnClientClick="return confirm('Are you sure you want to delete this record')" OnClick="delete_Button_Click"  Text="Delete" />
                   
                   </asp:Panel>
                   
               </ItemTemplate>
           </asp:FormView>
           
           
           <asp:ObjectDataSource ID="ObjectDataSource1" runat="server" 
                OldValuesParameterFormatString="original_{0}" SelectMethod="banklistbank" 
                TypeName="ledger">
                <SelectParameters>
                   <asp:Parameter DefaultValue="View" Name="prmAction" Type="String" />                                                       
                   <asp:Parameter Name="prmUser" Type="string" />                     
                    <asp:Parameter Name="prmcomp" Type="String" />
                   <asp:Parameter Name="prmBankcode"  Type="string" />
                   <asp:Parameter Name="prmBankname" Type="String"></asp:Parameter>
                   <asp:Parameter Name="prmAddr1" Type="String" />                   
                   <asp:Parameter Name="prmAddr2" Type="String" />
                   <asp:Parameter Name="prmCity" Type="String" />
                   <asp:Parameter Name="prmState" Type="String" />
                    <asp:Parameter Name="prmZip" Type="String" />
                    <asp:Parameter Name="prmPhone" Type="String" />
                    <asp:Parameter Name="prmContact" Type="String" />
                    <asp:Parameter Name="prmBkact" Type="String" />
                    <asp:Parameter Name="prmActnum" Type="String" />
                    <asp:Parameter Name="prmLastchk" Type="String" />
                    <asp:Parameter Name="prmBal" Type="String" />
                    <asp:Parameter Name="prmOchk" Type="String" />
                    <asp:Parameter Name="prmDeptr" Type="String" />
                    <asp:Parameter Name="prmServ" Type="String" />
                    <asp:Parameter Name="prmCurrcode" Type="String" />
                    <asp:Parameter Name="prmBankno" Type="String" />
                   <asp:SessionParameter SessionField="gl_bank_list_reckey" Name="prmReckey" Type="String" />
                </SelectParameters>
            </asp:ObjectDataSource>
       </div>    
       
       
    </div>
    </td></tr></table>    
    <ft:footer id="Footer1" runat="server"></ft:footer>
    </form>
  </body>
</HTML>

