<%@ Page Language="c#" AutoEventWireup="true" Debug="true" Inherits="view_gl_account" Codebehind="view_gl_account.aspx.cs" %>
<%@ Register Src="footer.ascx" TagName="Footer" TagPrefix="ft" %>
<%@ Register Src="header.ascx" TagName="Header" TagPrefix="hd" %>
<html xmlns="http://www.w3.org/1999/xhtml" >
  <head id="Head1" runat="server">
    <title>G/L Account</title>
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

    function AccountLook() {
        var NewWindow = window.open("accountlook.aspx", "AccountLookupWindow", "width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
    }

    function AccountLookup(ReturnObj1, ReturnObj2) {
        document.forms[0].FormView2_actnumTextBox.value = ReturnObj1;
        var actdsc = document.getElementById("FormView2_actdscrlabel");
        
           
        if(document.forms[0].FormView2_actdscrLabel)
            document.forms[0].FormView2_actdscrLabel.value = ReturnObj2;
            else
                actdsc.innerHTML = ReturnObj2;
        
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
       var NewWindow = window.open("accountlook.aspx", "AccountLookupWindow", "width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
   }

   function AccountLookup(ReturnObj1, ReturnObj2) {
       document.forms[0].FormView1_actTextBox.value = ReturnObj1;
       document.forms[0].FormView1_actdscrTextBox.value = ReturnObj2;
   }
   function printgl() {
       var NewWindow = window.open("topprintcust_report.aspx", "OrderReport", "width=600,height=600,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
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
          <TD align=left nowrap><font size=+0><b>G/L Account&nbsp;</b></font></TD>
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
      <asp:LinkButton ID="lnk_Listcustomers" runat="server" OnClick="lnk_listglaccount" >Brws Account</asp:LinkButton></li>
      <li class="selected"><asp:LinkButton ID="lnk_viewcustomers" runat="server" OnClick="lnk_viewglaccount_Click" > View Account</asp:LinkButton></li>
      <li><asp:LinkButton ID="load_viewcustomers" runat="server" OnClick="load_viewglaccount_Click" >Distrib. Acct</asp:LinkButton>   </li></ul></div>   
      </td>
      </tr></table>
       <div>
           <asp:FormView ID="FormView1" runat="server" DataSourceID="ObjectDataSource1" OnDataBound="FormView1_DataBound">
               <EditItemTemplate>
               <asp:Panel ID="editpanel" Width="500px" CssClass="shade" runat="server" DefaultButton="">
               <fieldset>               
               <table><tr><td>
                   <tr><td align="right" style="padding-right: 5px"><b>Account No:</b></td>
                   <td><asp:TextBox ID="actTextBox" BackColor="Turquoise" ReadOnly="true" Width="100" runat="server" Text='<%# Bind("act") %>' /></td>
                   <td align="right" style="padding-right: 5px"><b>Desc:</b></td>
                   <td><asp:TextBox ID="actdscrTextBox" Width="180" runat="server" Text='<%# Bind("actdscr") %>' /></td></tr> 
                   <tr align="right" style="padding-right: 5px"><td><b>Cur Year Open Bal:</b></td>
                   <td><asp:TextBox ID="cry_opnTextBox" Width="100" runat="server" Text='<%# Bind("cry_opn") %>' /></td>
                   <td align="right" style="padding-right: 5px"><b>Last Year Open:</b></td>
                   <td align="Left" style="padding-right: 5px"><asp:TextBox ID="lyr_opnTextBox" Width="100" runat="server" Text='<%# Bind("lyr_opn") %>' /></td></tr>                   
                   </td></tr></table>
                   
                   <table class="shade"><tr><td>
                   <tr><td><b>Period:</b></td><td><b>Current Year:</b></td><td><b>Last Year:</b></td><td></td><td><b>Current Year Budget</b></td><td><b>Last Year Budget</b></td></tr>                 
                   
                   <tr><td><b>01:</b></td>
                   <td><asp:Label ID="cyr1TextBox" Width="100" BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" runat="server" Text='<%# Bind("cyr1") %>' /></td>
                   <td><asp:Label ID="lyr1TextBox" Width="100" BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" runat="server" Text='<%# Bind("lyr1") %>' /></td>
                   <td></td>
                   <td><asp:TextBox ID="bud1TextBox" Width="100" runat="server" Text='<%# Bind("bud1") %>' /></td>
                   <td><asp:TextBox ID="ly_bud1TextBox" Width="100" runat="server" Text='<%# Bind("ly_bud1") %>' /></td></tr>
                   <tr><td><b>02:</b></td>
                   <td><asp:Label ID="cyr2TextBox" Width="100" BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" runat="server" Text='<%# Bind("cyr2") %>' /></td>
                   <td><asp:Label ID="lyr2TextBox" Width="100" BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" runat="server" Text='<%# Bind("lyr2") %>' /></td>
                   <td></td>
                   <td><asp:TextBox ID="bud2TextBox" Width="100" runat="server" Text='<%# Bind("bud2") %>' /></td>
                   <td><asp:TextBox ID="ly_bud2TextBox" Width="100" runat="server" Text='<%# Bind("ly_bud2") %>' /></td></tr>                   
                   <tr><td><b>03:</b></td>
                   <td><asp:Label ID="cyr3TextBox" Width="100" BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" runat="server" Text='<%# Bind("cyr3") %>' /></td>
                   <td><asp:Label ID="lyr3TextBox" Width="100" BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" runat="server" Text='<%# Bind("lyr3") %>' /></td>
                   <td></td>
                   <td><asp:TextBox ID="bud3TextBox" Width="100" runat="server" Text='<%# Bind("bud3") %>' /></td>
                   <td><asp:TextBox ID="ly_bud3TextBox" Width="100" runat="server" Text='<%# Bind("ly_bud3") %>' /></td></tr>
                   <tr><td><b>04:</b></td>
                   <td><asp:Label ID="cyr4TextBox" Width="100" BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" runat="server" Text='<%# Bind("cyr4") %>' /> </td>
                   <td><asp:Label ID="lyr4TextBox" Width="100" BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" runat="server" Text='<%# Bind("lyr4") %>' /></td>
                   <td></td>
                   <td><asp:TextBox ID="bud4TextBox" Width="100" runat="server" Text='<%# Bind("bud4") %>' /></td>
                   <td><asp:TextBox ID="ly_bud4TextBox" Width="100" runat="server" Text='<%# Bind("ly_bud4") %>' /></td></tr>
                   <tr><td><b>05:</b></td>
                   <td><asp:Label ID="cyr5TextBox" Width="100" BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" runat="server" Text='<%# Bind("cyr5") %>' /></td>
                   <td><asp:Label ID="lyr5TextBox" Width="100" BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" runat="server" Text='<%# Bind("lyr5") %>' /></td>
                   <td></td>
                   <td><asp:TextBox ID="bud5TextBox" Width="100" runat="server" Text='<%# Bind("bud5") %>' /></td>
                   <td><asp:TextBox ID="ly_bud5TextBox" Width="100" runat="server" Text='<%# Bind("ly_bud5") %>' /></td></tr>
                   <tr><td><b>06:</b></td>
                   <td><asp:Label ID="cyr6TextBox" Width="100" BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" runat="server" Text='<%# Bind("cyr6") %>' /></td>
                   <td><asp:Label ID="lyr6TextBox" Width="100" BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" runat="server" Text='<%# Bind("lyr6") %>' /></td>
                   <td></td>
                   <td><asp:TextBox ID="bud6TextBox" Width="100" runat="server" Text='<%# Bind("bud6") %>' /></td>
                   <td><asp:TextBox ID="ly_bud6TextBox" Width="100" runat="server" Text='<%# Bind("ly_bud6") %>' /></td></tr>
                   <tr><td><b>07:</b></td>
                   <td><asp:Label ID="cyr7TextBox" Width="100" BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" runat="server" Text='<%# Bind("cyr7") %>' /></td>
                   <td><asp:Label ID="lyr7TextBox" Width="100" BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" runat="server" Text='<%# Bind("lyr7") %>' /></td>
                   <td></td>
                   <td><asp:TextBox ID="bud7TextBox" Width="100" runat="server" Text='<%# Bind("bud7") %>' /></td>
                   <td><asp:TextBox ID="ly_bud7TextBox" Width="100" runat="server" Text='<%# Bind("ly_bud7") %>' /></td></tr>
                   <tr><td><b>08:</b></td>
                   <td><asp:Label ID="cyr8TextBox" Width="100" BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" runat="server" Text='<%# Bind("cyr8") %>' /></td>
                   <td><asp:Label ID="lyr8TextBox" Width="100" BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" runat="server" Text='<%# Bind("lyr8") %>' /></td>
                   <td></td>
                   <td><asp:TextBox ID="bud8TextBox" Width="100" runat="server" Text='<%# Bind("bud8") %>' /></td>
                   <td><asp:TextBox ID="ly_bud8TextBox" Width="100" runat="server" Text='<%# Bind("ly_bud8") %>' /></td></tr>
                   <tr><td><b>09:</b></td>
                   <td><asp:Label ID="cyr9TextBox" Width="100" BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" runat="server" Text='<%# Bind("cyr9") %>' /></td>
                   <td><asp:Label ID="lyr9TextBox" Width="100" BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" runat="server" Text='<%# Bind("lyr9") %>' /></td>
                   <td></td>
                   <td><asp:TextBox ID="bud9TextBox" Width="100" runat="server" Text='<%# Bind("bud9") %>' /></td>
                   <td><asp:TextBox ID="ly_bud9TextBox" Width="100" runat="server" Text='<%# Bind("ly_bud9") %>' /></td></tr>
                   <tr><td><b>10:</b></td>
                   <td><asp:Label ID="cyr10TextBox" Width="100" BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" runat="server" Text='<%# Bind("cyr10") %>' /></td>
                   <td><asp:Label ID="lyr10TextBox" Width="100" BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" runat="server" Text='<%# Bind("lyr10") %>' /></td>
                   <td></td>
                   <td><asp:TextBox ID="bud10TextBox" Width="100" runat="server" Text='<%# Bind("bud10") %>' /></td>
                   <td><asp:TextBox ID="ly_bud10TextBox" Width="100" runat="server" Text='<%# Bind("ly_bud10") %>' /></td></tr>
                   <tr><td><b>11:</b></td>
                   <td><asp:Label ID="cyr11TextBox" Width="100" BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" runat="server" Text='<%# Bind("cyr11") %>' /></td>
                   <td><asp:Label ID="lyr11TextBox" Width="100" BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" runat="server" Text='<%# Bind("lyr11") %>' /> </td>
                   <td></td>
                   <td><asp:TextBox ID="bud11TextBox" Width="100" runat="server" Text='<%# Bind("bud11") %>' /></td>
                   <td><asp:TextBox ID="ly_bud11TextBox" Width="100" runat="server" Text='<%# Bind("ly_bud11") %>' /></td></tr>
                   <tr><td><b>12:</b></td>
                   <td><asp:Label ID="cyr12TextBox" Width="100" BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" runat="server" Text='<%# Bind("cyr12") %>' /></td>
                   <td><asp:Label ID="lyr12TextBox" Width="100" BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" runat="server" Text='<%# Bind("lyr12") %>' /></td>
                   <td></td>
                   <td><asp:TextBox ID="bud12TextBox" Width="100" runat="server" Text='<%# Bind("bud12") %>' /></td>
                   <td><asp:TextBox ID="ly_bud12TextBox" Width="100" runat="server" Text='<%# Bind("ly_bud12") %>' /></td></tr>
                   <tr><td><b>13:</b></td>
                   <td><asp:Label ID="cyr13TextBox" Width="100" BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" runat="server" Text='<%# Bind("cyr13") %>' /></td>
                   <td><asp:Label ID="lyr13TextBox" Width="100" BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" runat="server" Text='<%# Bind("lyr13") %>' /></td>
                   <td></td>
                   <td><asp:TextBox ID="bud13TextBox" Width="100" runat="server" Text='<%# Bind("bud13") %>' /></td>
                   <td><asp:TextBox ID="ly_bud13TextBox" Width="100" runat="server" Text='<%# Bind("ly_bud13") %>' /></td></tr>
                   
                   </td></tr></table> 
                     
                   
                   <table class="shade"><tr><td>                   
                   <tr><td align="right" style="padding-right: 5px"><b>Type:</b>
                   <asp:RadioButtonList ID="RadioButtonList1" RepeatLayout="Flow" CellSpacing="1" RepeatColumns="6" Font-Bold ="true" DataTextField='<%# Bind("actype") %>' runat="server" SelectedValue='<%# Bind("actype") %>'>
                                
                               <asp:ListItem  Value="A"    Text="Asset" />
                               <asp:ListItem Value="C"    Text="Capital" />
                               <asp:ListItem  Value="E"    Text="Expense" />
                               <asp:ListItem Value="L"    Text="Liability" />
                               <asp:ListItem  Value="R"    Text="Revenue" />
                               <asp:ListItem Value="T"    Text="Title" />
                               
                   </asp:RadioButtonList></td></tr>
                   
                   <tr><td align="center"><b><asp:CheckBox ID="CheckBox2" Text="No Terms Discount" runat="server"></asp:CheckBox></b>
                   <asp:Label ID="checkLabel" Visible="false" runat="server" Text='<%# Bind("tb_not_disc") %>'></asp:Label>
                   </td></tr>                                 
                   
                   </td></tr><tr></tr><tr></tr><tr></tr><tr></tr></table>  
                   
                                  
                   
                  
                   <asp:TextBox ID="RecKeyTextBox" Visible="false" runat="server" Text='<%# Bind("RecKey") %>' />
                   
                   
                   
                   
                   
                   <asp:Button ID="UpdateButton" runat="server" CausesValidation="True" OnClick="UpdateButton_Click" CssClass="button" Text="Save" />
                   &nbsp;<asp:Button ID="UpdateCancelButton" runat="server" CausesValidation="False" CssClass="button" CommandName="Cancel" Text="Cancel" />
                       </fieldset></asp:Panel>
               </EditItemTemplate>
               <InsertItemTemplate>
               <asp:Panel ID="editpanel" Width="510px" CssClass="shade" runat="server" DefaultButton="">
                   <fieldset>               
               <table class="shade"><tr><td>
                   <tr><td align="right" style="padding-right: 5px"><b>Account No:</b></td>
                   <td><asp:TextBox ID="actTextBox" Width="100" runat="server" Text='<%# Bind("act") %>' />
                   </td>
                   <td align="right" style="padding-right: 5px"><b>Desc:</b></td>
                   <td><asp:TextBox ID="actdscrTextBox" Width="180" runat="server" Text='<%# Bind("actdscr") %>' /></td></tr> 
                   <tr nowrap align="right" style="padding-right: 5px"><td><b>Cur Year Open Bal:</b></td>
                   <td><asp:TextBox ID="cry_opnTextBox" Width="100" runat="server" Text='<%# Bind("cry_opn") %>' /></td>
                   <td nowrap align="right" style="padding-right: 5px"><b>Last Year Open:</b></td>
                   <td align="Left" style="padding-right: 5px"><asp:TextBox ID="lyr_opnTextBox" Width="100" runat="server" Text='<%# Bind("lyr_opn") %>' /></td></tr>                   
                   </td></tr></table>
                   
                   <table class="shade"><tr><td>
                   <tr><td><b>Period:</b></td><td><b>Current Year:</b></td><td><b>Last Year:</b></td><td></td><td><b>Current Year Budget</b></td><td><b>Last Year Budget</b></td></tr>                 
                   
                   <tr><td><b>01:</b></td>
                   <td><asp:Label ID="cyr1TextBox" Width="100" BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" runat="server" Text='<%# Bind("cyr1") %>' /></td>
                   <td><asp:Label ID="lyr1TextBox" Width="100" BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" runat="server" Text='<%# Bind("lyr1") %>' /></td>
                   <td></td>
                   <td><asp:TextBox ID="bud1TextBox" Width="100" runat="server" Text='<%# Bind("bud1") %>' /></td>
                   <td><asp:TextBox ID="ly_bud1TextBox" Width="100" runat="server" Text='<%# Bind("ly_bud1") %>' /></td></tr>
                   <tr><td><b>02:</b></td>
                   <td><asp:Label ID="cyr2TextBox" Width="100" BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" runat="server" Text='<%# Bind("cyr2") %>' /></td>
                   <td><asp:Label ID="lyr2TextBox" Width="100" BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" runat="server" Text='<%# Bind("lyr2") %>' /></td>
                   <td></td>
                   <td><asp:TextBox ID="bud2TextBox" Width="100" runat="server" Text='<%# Bind("bud2") %>' /></td>
                   <td><asp:TextBox ID="ly_bud2TextBox" Width="100" runat="server" Text='<%# Bind("ly_bud2") %>' /></td></tr>                   
                   <tr><td><b>03:</b></td>
                   <td><asp:Label ID="cyr3TextBox" Width="100" BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" runat="server" Text='<%# Bind("cyr3") %>' /></td>
                   <td><asp:Label ID="lyr3TextBox" Width="100" BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" runat="server" Text='<%# Bind("lyr3") %>' /></td>
                   <td></td>
                   <td><asp:TextBox ID="bud3TextBox" Width="100" runat="server" Text='<%# Bind("bud3") %>' /></td>
                   <td><asp:TextBox ID="ly_bud3TextBox" Width="100" runat="server" Text='<%# Bind("ly_bud3") %>' /></td></tr>
                   <tr><td><b>04:</b></td>
                   <td><asp:Label ID="cyr4TextBox" Width="100" BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" runat="server" Text='<%# Bind("cyr4") %>' /> </td>
                   <td><asp:Label ID="lyr4TextBox" Width="100" BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" runat="server" Text='<%# Bind("lyr4") %>' /></td>
                   <td></td>
                   <td><asp:TextBox ID="bud4TextBox" Width="100" runat="server" Text='<%# Bind("bud4") %>' /></td>
                   <td><asp:TextBox ID="ly_bud4TextBox" Width="100" runat="server" Text='<%# Bind("ly_bud4") %>' /></td></tr>
                   <tr><td><b>05:</b></td>
                   <td><asp:Label ID="cyr5TextBox" Width="100" BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" runat="server" Text='<%# Bind("cyr5") %>' /></td>
                   <td><asp:Label ID="lyr5TextBox" Width="100" BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" runat="server" Text='<%# Bind("lyr5") %>' /></td>
                   <td></td>
                   <td><asp:TextBox ID="bud5TextBox" Width="100" runat="server" Text='<%# Bind("bud5") %>' /></td>
                   <td><asp:TextBox ID="ly_bud5TextBox" Width="100" runat="server" Text='<%# Bind("ly_bud5") %>' /></td></tr>
                   <tr><td><b>06:</b></td>
                   <td><asp:Label ID="cyr6TextBox" Width="100" BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" runat="server" Text='<%# Bind("cyr6") %>' /></td>
                   <td><asp:Label ID="lyr6TextBox" Width="100" BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" runat="server" Text='<%# Bind("lyr6") %>' /></td>
                   <td></td>
                   <td><asp:TextBox ID="bud6TextBox" Width="100" runat="server" Text='<%# Bind("bud6") %>' /></td>
                   <td><asp:TextBox ID="ly_bud6TextBox" Width="100" runat="server" Text='<%# Bind("ly_bud6") %>' /></td></tr>
                   <tr><td><b>07:</b></td>
                   <td><asp:Label ID="cyr7TextBox" Width="100" BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" runat="server" Text='<%# Bind("cyr7") %>' /></td>
                   <td><asp:Label ID="lyr7TextBox" Width="100" BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" runat="server" Text='<%# Bind("lyr7") %>' /></td>
                   <td></td>
                   <td><asp:TextBox ID="bud7TextBox" Width="100" runat="server" Text='<%# Bind("bud7") %>' /></td>
                   <td><asp:TextBox ID="ly_bud7TextBox" Width="100" runat="server" Text='<%# Bind("ly_bud7") %>' /></td></tr>
                   <tr><td><b>08:</b></td>
                   <td><asp:Label ID="cyr8TextBox" Width="100" BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" runat="server" Text='<%# Bind("cyr8") %>' /></td>
                   <td><asp:Label ID="lyr8TextBox" Width="100" BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" runat="server" Text='<%# Bind("lyr8") %>' /></td>
                   <td></td>
                   <td><asp:TextBox ID="bud8TextBox" Width="100" runat="server" Text='<%# Bind("bud8") %>' /></td>
                   <td><asp:TextBox ID="ly_bud8TextBox" Width="100" runat="server" Text='<%# Bind("ly_bud8") %>' /></td></tr>
                   <tr><td><b>09:</b></td>
                   <td><asp:Label ID="cyr9TextBox" Width="100" BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" runat="server" Text='<%# Bind("cyr9") %>' /></td>
                   <td><asp:Label ID="lyr9TextBox" Width="100" BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" runat="server" Text='<%# Bind("lyr9") %>' /></td>
                   <td></td>
                   <td><asp:TextBox ID="bud9TextBox" Width="100" runat="server" Text='<%# Bind("bud9") %>' /></td>
                   <td><asp:TextBox ID="ly_bud9TextBox" Width="100" runat="server" Text='<%# Bind("ly_bud9") %>' /></td></tr>
                   <tr><td><b>10:</b></td>
                   <td><asp:Label ID="cyr10TextBox" Width="100" BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" runat="server" Text='<%# Bind("cyr10") %>' /></td>
                   <td><asp:Label ID="lyr10TextBox" Width="100" BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" runat="server" Text='<%# Bind("lyr10") %>' /></td>
                   <td></td>
                   <td><asp:TextBox ID="bud10TextBox" Width="100" runat="server" Text='<%# Bind("bud10") %>' /></td>
                   <td><asp:TextBox ID="ly_bud10TextBox" Width="100" runat="server" Text='<%# Bind("ly_bud10") %>' /></td></tr>
                   <tr><td><b>11:</b></td>
                   <td><asp:Label ID="cyr11TextBox" Width="100" BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" runat="server" Text='<%# Bind("cyr11") %>' /></td>
                   <td><asp:Label ID="lyr11TextBox" Width="100" BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" runat="server" Text='<%# Bind("lyr11") %>' /> </td>
                   <td></td>
                   <td><asp:TextBox ID="bud11TextBox" Width="100" runat="server" Text='<%# Bind("bud11") %>' /></td>
                   <td><asp:TextBox ID="ly_bud11TextBox" Width="100" runat="server" Text='<%# Bind("ly_bud11") %>' /></td></tr>
                   <tr><td><b>12:</b></td>
                   <td><asp:Label ID="cyr12TextBox" Width="100" BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" runat="server" Text='<%# Bind("cyr12") %>' /></td>
                   <td><asp:Label ID="lyr12TextBox" Width="100" BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" runat="server" Text='<%# Bind("lyr12") %>' /></td>
                   <td></td>
                   <td><asp:TextBox ID="bud12TextBox" Width="100" runat="server" Text='<%# Bind("bud12") %>' /></td>
                   <td><asp:TextBox ID="ly_bud12TextBox" Width="100" runat="server" Text='<%# Bind("ly_bud12") %>' /></td></tr>
                   <tr><td><b>13:</b></td>
                   <td><asp:Label ID="cyr13TextBox" Width="100" BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" runat="server" Text='<%# Bind("cyr13") %>' /></td>
                   <td><asp:Label ID="lyr13TextBox" Width="100" BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" runat="server" Text='<%# Bind("lyr13") %>' /></td>
                   <td></td>
                   <td><asp:TextBox ID="bud13TextBox" Width="100" runat="server" Text='<%# Bind("bud13") %>' /></td>
                   <td><asp:TextBox ID="ly_bud13TextBox" Width="100" runat="server" Text='<%# Bind("ly_bud13") %>' /></td></tr>
                   
                   </td></tr></table> 
                   
                   
                   <table class="shade"><tr><td>                   
                   <tr><td align="right" style="padding-right: 5px"><b>Type:</b>
                   <asp:RadioButtonList ID="RadioButtonList1" RepeatLayout="Flow" CellSpacing="1" RepeatColumns="6" Font-Bold ="true" runat="server">
                                
                               <asp:ListItem  Value="A"    Text="Asset" />
                               <asp:ListItem Value="C"    Text="Capital" />
                               <asp:ListItem  Value="E"    Text="Expense" />
                               <asp:ListItem Value="L"    Text="Liability" />
                               <asp:ListItem  Value="R"    Text="Revenue" />
                               <asp:ListItem Value="T"    Text="Title" />
                               
                   </asp:RadioButtonList></td></tr>
                   
                   <tr><td align="center"><b><asp:CheckBox ID="CheckBox2" Text="No Terms Discount" runat="server"></asp:CheckBox></b>
                   </td></tr>                                 
                   
                   </td></tr><tr></tr><tr></tr><tr></tr><tr></tr></table>  
                                     
                   
                   
                   <asp:TextBox ID="RecKeyTextBox" Visible="false" runat="server" Text='<%# Bind("RecKey") %>' />
                   
                   <asp:Button ID="InsertButton" runat="server" CausesValidation="True" OnClick="addButton_Click" CssClass="button" Text="Save" />
                   &nbsp;<asp:Button ID="InsertCancelButton" runat="server" CausesValidation="False" CssClass="button" CommandName="Cancel" Text="Cancel" />
                   </fieldset></asp:Panel>
               </InsertItemTemplate>
               <ItemTemplate>
               <asp:Panel ID="exitpanel" CssClass="shade" width="500px" runat="server" DefaultButton="">
                   <fieldset class="shade">               
               <table class="shade"><tr><td>
                   <tr><td align="right" style="padding-right: 5px"><b>Account No:</b></td>
                   <td><asp:Label ID="actTextBox" Width="100" runat="server" BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" Text='<%# Bind("act") %>' /></td>
                   <td align="right" style="padding-right: 5px"><b>Desc:</b></td>
                   <td><asp:Label ID="actdscrTextBox" Width="180" runat="server" BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" Text='<%# Bind("actdscr") %>' /></td></tr> 
                   <tr><td align="right" style="padding-right: 5px"><b>Cur Year Open Bal:</b></td>
                   <td><asp:Label ID="cry_opnTextBox" Width="100" runat="server" BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" Text='<%# Bind("cry_opn") %>' /></td>
                   <td align="left" style="padding-left: 5px"><b>Last Year Open:</b></td>
                   <td td align="Left" style="padding-right: 5px"><asp:Label ID="lyr_opnTextBox" Width="100" runat="server" BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" Text='<%# Bind("lyr_opn") %>' /></td></tr>                   
                                    
                   </td></tr></table>
                 
                   
                   <table class="shade"><tr><td>
                   <tr><td><b>Period</b></td><td><b>Current Year</b></td><td><b>Last Year</b></td><td></td><td><b>Current Year Budget</b></td><td><b>Last Year Budget</b></td></tr>                 
                   
                   <tr><td><b>01:</b></td>
                   <td><asp:Label ID="cyr1TextBox" runat="server" Width="100" BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" Text='<%# Bind("cyr1") %>' /></td>
                   <td><asp:Label ID="lyr1TextBox" runat="server" Width="100" BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" Text='<%# Bind("lyr1") %>' /></td>
                   <td></td>
                   <td><asp:Label ID="bud1TextBox" runat="server" Width="115" BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" Text='<%# Bind("bud1") %>' /></td>
                   <td><asp:Label ID="ly_bud1TextBox" runat="server" Width="100" BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" Text='<%# Bind("ly_bud1") %>' /></td></tr>
                   <tr><td><b>02:</b></td>
                   <td><asp:Label ID="cyr2TextBox" runat="server" Width="100" BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" Text='<%# Bind("cyr2") %>' /></td>
                   <td><asp:Label ID="lyr2TextBox" runat="server" Width="100" BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" Text='<%# Bind("lyr2") %>' /></td>
                   <td></td>
                   <td><asp:Label ID="bud2TextBox" runat="server" Width="115" BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" Text='<%# Bind("bud2") %>' /></td>
                   <td><asp:Label ID="ly_bud2TextBox" runat="server" Width="100" BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" Text='<%# Bind("ly_bud2") %>' /></td></tr>                   
                   <tr><td><b>03:</b></td>
                   <td><asp:Label ID="cyr3TextBox" runat="server" Width="100" BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" Text='<%# Bind("cyr3") %>' /></td>
                   <td><asp:Label ID="lyr3TextBox" runat="server" Width="100" BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" Text='<%# Bind("lyr3") %>' /></td>
                   <td></td>
                   <td><asp:Label ID="bud3TextBox" runat="server" Width="115" BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" Text='<%# Bind("bud3") %>' /></td>
                   <td><asp:Label ID="ly_bud3TextBox" runat="server" Width="100" BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" Text='<%# Bind("ly_bud3") %>' /></td></tr>
                   <tr><td><b>04:</b></td>
                   <td><asp:Label ID="cyr4TextBox" runat="server" Width="100" BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" Text='<%# Bind("cyr4") %>' /> </td>
                   <td><asp:Label ID="lyr4TextBox" runat="server" Width="100" BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" Text='<%# Bind("lyr4") %>' /></td>
                   <td></td>
                   <td><asp:Label ID="bud4TextBox" runat="server" Width="115" BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" Text='<%# Bind("bud4") %>' /></td>
                   <td><asp:Label ID="ly_bud4TextBox" runat="server" Width="100" BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" Text='<%# Bind("ly_bud4") %>' /></td></tr>
                   <tr><td><b>05:</b></td>
                   <td><asp:Label ID="cyr5TextBox" runat="server" Width="100" BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" Text='<%# Bind("cyr5") %>' /></td>
                   <td><asp:Label ID="lyr5TextBox" runat="server" Width="100" BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" Text='<%# Bind("lyr5") %>' /></td>
                   <td></td>
                   <td><asp:Label ID="bud5TextBox" runat="server" Width="115" BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" Text='<%# Bind("bud5") %>' /></td>
                   <td><asp:Label ID="ly_bud5TextBox" runat="server" Width="100" BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" Text='<%# Bind("ly_bud5") %>' /></td></tr>
                   <tr><td><b>06:</b></td>
                   <td><asp:Label ID="cyr6TextBox" runat="server" Width="100" BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" Text='<%# Bind("cyr6") %>' /></td>
                   <td><asp:Label ID="lyr6TextBox" runat="server" Width="100" BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" Text='<%# Bind("lyr6") %>' /></td>
                   <td></td>
                   <td><asp:Label ID="bud6TextBox" runat="server" Width="115" BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" Text='<%# Bind("bud6") %>' /></td>
                   <td><asp:Label ID="ly_bud6TextBox" runat="server" Width="100" BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" Text='<%# Bind("ly_bud6") %>' /></td></tr>
                   <tr><td><b>07:</b></td>
                   <td><asp:Label ID="cyr7TextBox" runat="server" Width="100" BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" Text='<%# Bind("cyr7") %>' /></td>
                   <td><asp:Label ID="lyr7TextBox" runat="server" Width="100" BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" Text='<%# Bind("lyr7") %>' /></td>
                   <td></td>
                   <td><asp:Label ID="bud7TextBox" runat="server" Width="115" BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" Text='<%# Bind("bud7") %>' /></td>
                   <td><asp:Label ID="ly_bud7TextBox" runat="server" Width="100" BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" Text='<%# Bind("ly_bud7") %>' /></td></tr>
                   <tr><td><b>08:</b></td>
                   <td><asp:Label ID="cyr8TextBox" runat="server" Width="100" BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" Text='<%# Bind("cyr8") %>' /></td>
                   <td><asp:Label ID="lyr8TextBox" runat="server" Width="100" BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" Text='<%# Bind("lyr8") %>' /></td>
                   <td></td>
                   <td><asp:Label ID="bud8TextBox" runat="server" Width="115" BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" Text='<%# Bind("bud8") %>' /></td>
                   <td><asp:Label ID="ly_bud8TextBox" runat="server" Width="100" BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" Text='<%# Bind("ly_bud8") %>' /></td></tr>
                   <tr><td><b>09:</b></td>
                   <td><asp:Label ID="cyr9TextBox" runat="server" Width="100" BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" Text='<%# Bind("cyr9") %>' /></td>
                   <td><asp:Label ID="lyr9TextBox" runat="server" Width="100" BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" Text='<%# Bind("lyr9") %>' /></td>
                   <td></td>
                   <td><asp:Label ID="bud9TextBox" runat="server" Width="115" BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" Text='<%# Bind("bud9") %>' /></td>
                   <td><asp:Label ID="ly_bud9TextBox" runat="server" Width="100" BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" Text='<%# Bind("ly_bud9") %>' /></td></tr>
                   <tr><td><b>10:</b></td>
                   <td><asp:Label ID="cyr10TextBox" runat="server" Width="100" BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" Text='<%# Bind("cyr10") %>' /></td>
                   <td><asp:Label ID="lyr10TextBox" runat="server" Width="100" BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" Text='<%# Bind("lyr10") %>' /></td>
                   <td></td>
                   <td><asp:Label ID="bud10TextBox" runat="server" Width="115" BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" Text='<%# Bind("bud10") %>' /></td>
                   <td><asp:Label ID="ly_bud10TextBox" runat="server" Width="100" BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" Text='<%# Bind("ly_bud10") %>' /></td></tr>
                   <tr><td><b>11:</b></td>
                   <td><asp:Label ID="cyr11TextBox" runat="server" Width="100"  BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" Text='<%# Bind("cyr11") %>' /></td>
                   <td><asp:Label ID="lyr11TextBox" runat="server" Width="100" BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" Text='<%# Bind("lyr11") %>' /> </td>
                   <td></td>
                   <td><asp:Label ID="bud11TextBox" runat="server" Width="115" BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" Text='<%# Bind("bud11") %>' /></td>
                   <td><asp:Label ID="ly_bud11TextBox" runat="server" Width="100" BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" Text='<%# Bind("ly_bud11") %>' /></td></tr>
                   <tr><td><b>12:</b></td>
                   <td><asp:Label ID="cyr12TextBox" runat="server" Width="100" BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" Text='<%# Bind("cyr12") %>' /></td>
                   <td><asp:Label ID="lyr12TextBox" runat="server" Width="100" BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" Text='<%# Bind("lyr12") %>' /></td>
                   <td></td>
                   <td><asp:Label ID="bud12TextBox" runat="server" Width="115" BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" Text='<%# Bind("bud12") %>' /></td>
                   <td><asp:Label ID="ly_bud12TextBox" runat="server" Width="100" BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" Text='<%# Bind("ly_bud12") %>' /></td></tr>
                   <tr><td><b>13:</b></td>
                   <td><asp:Label ID="cyr13TextBox" runat="server" Width="100" BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" Text='<%# Bind("cyr13") %>' /></td>
                   <td><asp:Label ID="lyr13TextBox" runat="server" Width="100" BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" Text='<%# Bind("lyr13") %>' /></td>
                   <td></td>
                   <td><asp:Label ID="bud13TextBox" runat="server" Width="115" BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" Text='<%# Bind("bud13") %>' /></td>
                   <td><asp:Label ID="ly_bud13TextBox" runat="server" Width="100" BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" Text='<%# Bind("ly_bud13") %>' /></td></tr>
                   
                   </td></tr></table> 
                                      
                   <table class="shade"><tr><td>                   
                   <tr><td align="right" style="padding-right: 5px"><b>Type:</b>
                   <asp:RadioButtonList ID="RadioButtonList1" RepeatLayout="Flow" Enabled="false" CellSpacing="1" RepeatColumns="6" Font-Bold ="true" runat="server" >
                                
                               <asp:ListItem  Value="A"    Text="Asset" />
                               <asp:ListItem Value="C"    Text="Capital" />
                               <asp:ListItem  Value="E"    Text="Expense" />
                               <asp:ListItem Value="L"    Text="Liability" />
                               <asp:ListItem  Value="R"    Text="Revenue" />
                               <asp:ListItem Value="T"    Text="Title" />
                               
                               
                   </asp:RadioButtonList>
                   <asp:Label ID="actypeLabel" Visible="false" runat="server" Text='<%# Bind("actype") %>' />
                   </td></tr>
                   
                   <tr><td align="center"><b><asp:CheckBox ID="CheckBox2" Enabled="false" Text="No Terms Discount" runat="server"></asp:CheckBox></b>
                   <asp:Label ID="checkLabel" Visible="false" runat="server" Text='<%# Bind("tb_not_disc") %>'></asp:Label>
                   </td></tr>                                 
                   
                   </td></tr><tr></tr><tr></tr><tr></tr><tr></tr></table>                 
                                     
                   
                   
                   <asp:Label ID="RecKeyLabel" Visible="false" runat="server" Text='<%# Bind("RecKey") %>' />
                   
                   <asp:Button ID="AddButton" runat="server" CssClass="button"  CommandName="new" Text="Add" />
                   &nbsp;<asp:Button ID="UpdateButton" runat="server" CssClass="button" CommandName="edit" Text="Update" />
                   &nbsp;<asp:Button ID="DeleteButton" runat="server" CssClass="button" OnClientClick="return confirm('Are you sure you want to delete this record')" OnClick="delete_Button_Click"  Text="Delete" />
                   &nbsp;<asp:Button ID="Button1" runat="server" CssClass="button" OnClientClick="return confirm('Are you sure you wish to copy Current Year Balances to Current Year Budgets?')" OnClick="copy_balance_bdgt" Width="170"  Text="Copy Balances To Budgets" />
                   </fieldset></asp:Panel>
               </ItemTemplate>
           </asp:FormView>
           
           
           <asp:ObjectDataSource ID="ObjectDataSource1" runat="server" 
                OldValuesParameterFormatString="original_{0}" SelectMethod="GLAccountList" 
                TypeName="ledger">
                <SelectParameters>
                   <asp:Parameter Name="prmUser" Type="string" />                     
                   <asp:Parameter DefaultValue="View" Name="prmAction" Type="String" />                                                       
                   <asp:Parameter Name="prmact"  Type="string" />
                   <asp:Parameter Name="prmactdscr" Type="String"></asp:Parameter>
                   <asp:Parameter Name="prmactype" Type="String" />                   
                   <asp:SessionParameter SessionField="gl_account_list_reckey" Name="prmReckey" Type="String" />
                   <asp:Parameter Name="prmcry_opn" Type="Decimal" />
                   <asp:Parameter Name="prmlyr_opn" Type="Decimal" />
                   <asp:Parameter Name="prmbud1" Type="Decimal" />
                    <asp:Parameter Name="prmbud2" Type="Decimal" />
                    <asp:Parameter Name="prmbud3" Type="Decimal" />
                    <asp:Parameter Name="prmbud4" Type="Decimal" />
                    <asp:Parameter Name="prmbud5" Type="Decimal" />
                    <asp:Parameter Name="prmbud6" Type="Decimal" />
                    <asp:Parameter Name="prmbud7" Type="Decimal" />
                    <asp:Parameter Name="prmbud8" Type="Decimal" />
                    <asp:Parameter Name="prmbud9" Type="Decimal" />
                    <asp:Parameter Name="prmbud10" Type="Decimal" />
                    <asp:Parameter Name="prmbud11" Type="Decimal" />
                    <asp:Parameter Name="prmbud12" Type="Decimal" />
                    <asp:Parameter Name="prmbud13" Type="Decimal" />
                    <asp:Parameter Name="prmly_bud1" Type="Decimal" />
                    <asp:Parameter Name="prmly_bud2" Type="Decimal" />
                    <asp:Parameter Name="prmly_bud3" Type="Decimal" />
                    <asp:Parameter Name="prmly_bud4" Type="Decimal" />
                    <asp:Parameter Name="prmly_bud5" Type="Decimal" />
                    <asp:Parameter Name="prmly_bud6" Type="Decimal" />
                    <asp:Parameter Name="prmly_bud7" Type="Decimal" />
                    <asp:Parameter Name="prmly_bud8" Type="Decimal" />
                    <asp:Parameter Name="prmly_bud9" Type="Decimal" />
                    <asp:Parameter Name="prmly_bud10" Type="Decimal" />
                    <asp:Parameter Name="prmly_bud11" Type="Decimal" />
                    <asp:Parameter Name="prmly_bud12" Type="Decimal" />
                    <asp:Parameter Name="prmly_bud13" Type="Decimal" />
                    <asp:Parameter Name="prmlyr1" Type="Decimal" />
                    <asp:Parameter Name="prmlyr2" Type="Decimal" />
                    <asp:Parameter Name="prmlyr3" Type="Decimal" />
                    <asp:Parameter Name="prmlyr4" Type="Decimal" />
                    <asp:Parameter Name="prmlyr5" Type="Decimal" />
                    <asp:Parameter Name="prmlyr6" Type="Decimal" />
                    <asp:Parameter Name="prmlyr7" Type="Decimal" />
                    <asp:Parameter Name="prmlyr8" Type="Decimal" />
                    <asp:Parameter Name="prmlyr9" Type="Decimal" />
                    <asp:Parameter Name="prmlyr10" Type="Decimal" />
                    <asp:Parameter Name="prmlyr11" Type="Decimal" />
                    <asp:Parameter Name="prmlyr12" Type="Decimal" />
                    <asp:Parameter Name="prmlyr13" Type="Decimal" />
                    <asp:Parameter Name="prmcyr1" Type="Decimal" />
                    <asp:Parameter Name="prmcyr2" Type="Decimal" />
                    <asp:Parameter Name="prmcyr3" Type="Decimal" />
                    <asp:Parameter Name="prmcyr4" Type="Decimal" />
                    <asp:Parameter Name="prmcyr5" Type="Decimal" />
                    <asp:Parameter Name="prmcyr6" Type="Decimal" />
                    <asp:Parameter Name="prmcyr7" Type="Decimal" />
                    <asp:Parameter Name="prmcyr8" Type="Decimal" />
                    <asp:Parameter Name="prmcyr9" Type="Decimal" />
                    <asp:Parameter Name="prmcyr10" Type="Decimal" />
                    <asp:Parameter Name="prmcyr11" Type="Decimal" />
                    <asp:Parameter Name="prmcyr12" Type="Decimal" />
                    <asp:Parameter Name="prmcyr13" Type="Decimal" />
                   <asp:Parameter Name="prmtb_not_disc" Type="String" />                   
                   <asp:Parameter Name="prmbtn" Type="String" />
                </SelectParameters>
            </asp:ObjectDataSource>
       </div>    
       
       
    </div>
    </td></tr></table>    
    <ft:footer id="Footer1" runat="server"></ft:footer>
    </form>
  </body>
</HTML>

