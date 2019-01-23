<%@ Page Language="c#" AutoEventWireup="true" Debug="true" Inherits="view_misc_cshrcpt" Codebehind="view_misc_cshrcpt.aspx.cs" %>
<%@ Register Src="footer.ascx" TagName="Footer" TagPrefix="ft" %>
<%@ Register Src="header.ascx" TagName="Header" TagPrefix="hd" %>
<html xmlns="http://www.w3.org/1999/xhtml" >
  <head id="Head1" runat="server">
    <title>Miscellaneous Cash Receipts</title>
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
    
    function Arinvlook() {
        var cust = document.getElementById("FormView1_custnoLabel");
                
        var NewWindow = window.open("crdb_inv_lookup.aspx?cust=" + cust.innerHTML + "", "ArinvWindow", "width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
        //var NewWindow = window.open("invinfo_lookup.aspx", "invinfoWindow", "width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
    }

    function arinvlook(ReturnObj1, ReturnObj2, ReturnObj3) {
        document.forms[0].FormView2_invTextBox.value = ReturnObj1;
        var invdt = document.getElementById("FormView2_invdateLabel");
        invdt.value = ReturnObj2;
        document.forms[0].FormView2_baldueLabel.value = ReturnObj3;


        //document.forms[0].FormView2_invTextBox.onchange();
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
   function mmofocus() {
       var mmo = document.getElementById("FormView2_memdscrTextBox");
       mmo.focus();
   }
   function discfocus() {
       var disc = document.getElementById("FormView2_disTextBox");
       disc.focus();
   }
   function dscrfocus() {
       var dscrd = document.getElementById("FormView1_dscrTextBox");
       dscrd.focus();
   }
   
   function currfocus() {
       var curr = document.getElementById("FormView1_currTextBox");
       curr.focus();
   }
   function chkamt() {
       var chk = document.getElementById("FormView1_amtTextBox");
       chk.focus();
   }
   function ttlfocus() {
       var ttl = document.getElementById("FormView2_ttlappTextBox");
       ttl.focus();
   }
   function actfocus() {
       var amt = document.getElementById("FormView2_amtTextBox");
       amt.focus();
   }
   function contactcustomerlook() {    
    var NewWindow = window.open("contact_customer_lookup.aspx", "ContactCustomerLookupWindow", "width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}

function ContactCustomerLookup(ReturnObj1, ReturnObj2) {
    document.forms[0].FormView1_custnoTextBox.value = ReturnObj1;
    var custnam = document.getElementById("FormView1_custnameLabel");
    custnam.innerHTML = ReturnObj2;
}
function banklookup() {

    var NewWindow = window.open("bank_lookup.aspx", "banklookup", "width=500,height=400,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}
function banklook(ReturnObj1, ReturnObj2) {
    document.forms[0].FormView1_bnkcdTextBox.value = ReturnObj1;
    var bnk = document.getElementById("FormView1_bnknameLabel")
    bnk.innerHTML = ReturnObj2;

}

    </script> 
  </head>    
   <body>
    <form id="frmList" runat="server"  defaultfocus='Inv_TextBox'>   
        <hd:header id="Header1" runat="server"></hd:header>
           <asp:ScriptManager ID="ScriptManager1" runat="server">
          </asp:ScriptManager>
         
         <table width="100%"><tr><td><div> 
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
          <TD align=left nowrap><font size=+0><b>Miscellaneous Cash Receipts&nbsp;</b></font></TD>
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
      <asp:LinkButton ID="lnk_Listcustomers" runat="server" OnClick="lnk_listinvoice" >Browse Memos</asp:LinkButton></li>
      <li class="selected"><asp:LinkButton ID="lnk_viewcustomers" runat="server" OnClick="lnk_viewcustomers_Click" > View Memos</asp:LinkButton></li></ul></div>
            
      </td>
      </tr></table>
       <div>
           <asp:FormView ID="FormView1" runat="server" OnDataBound="FormView1_OnDataBound" DataSourceID="ObjectDataSource1">
               <EditItemTemplate>
               <fieldset><table class="shade">
               <tr><td>
               <table><tr><td align="right" style="padding-right:5px"><b>Payer:</b></td>
               <td><asp:TextBox ID="pyerTextBox" Width="100px" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" runat="server" Text='<%# Bind("payr") %>' />
               <td align="right" style="padding-right:5px"><b>Check No:</b></td>
               <td><asp:TextBox ID="chknoTextBox" Width="100px" onfocus="dscrfocus()" ForeColor="#ACA899" runat="server" Text='<%# Bind("chkno") %>' /></tr>
               <tr><td align="right" style="padding-right:5px"><b>Description:</b></td>
               <td><asp:TextBox ID="dscrTextBox" Width="100px" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" runat="server" Text='<%# Bind("dscr") %>' /></td></tr>
               <tr><td align="right" style="padding-right:5px"><b>Date:</b></td>
               <td><asp:TextBox ID="dateTextBox" runat="server" Width="100px" onfocus="javascript:preEnter( this, 'yes' );" onblur="javascript:preLeave( this, 'date', '99/99/9999' );" Text='<%# Bind("chkdt") %>' />
               <a href="#" tabindex="1" onClick="showCalendarControl(FormView1_dateTextBox); return false"><asp:Image ID="Image1" runat="server" ImageUrl="images/lookup_icon.gif" /></a></td>
               <td colspan="2"><b>Bank Code:</b> &nbsp; &nbsp; <asp:TextBox ID="bnkcdTextBox" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" runat="server" Width="100px" Text='<%# Bind("bnkcd") %>' /></td></tr>
               <tr><td align="right" style="padding-right:5px"><b>Currency Code:</b></td>
               <td><asp:TextBox ID="currTextBox" runat="server" Width="100px" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" Text='<%# Bind("cur_cod") %>' /></td>               
               <td><b>Exchange Rate:</b></td>
               <td><asp:TextBox ID="exgTextBox" Width="50px" onfocus="currfocus()" ForeColor="#ACA899" runat="server" Text='<%# Bind("ex_rate") %>' /></td></tr>
               </table>
               </td>
               <td><fieldset>
               <table><tr><td align="right" style="padding-right:5px"><b>Record#:</b></td>
               <td><asp:TextBox ID="recordTextBox" Width="100px" onfocus="currfocus()" ForeColor="#ACA899" runat="server" Text='<%# Bind("rcrd") %>' /></td></tr>
               <tr><td align="right" style="padding-right:5px"><b>Posted:</b></td>
               <td><asp:TextBox ID="pstdTextBox" Width="100px" onfocus="currfocus()" ForeColor="#ACA899" runat="server" Text='<%# Bind("pstd") %>' />                              
               </td></tr>
               </table>
               </fieldset></td>
               </tr>
               </table></fieldset>
               
                   <asp:Button ID="UpdateButton" runat="server" CausesValidation="True" CssClass="button" OnClick="UpdateButton_Click"  Text="Save" />
                   &nbsp;<asp:Button ID="UpdateCancelButton" runat="server" CssClass="button"
                       CausesValidation="False" CommandName="Cancel" Text="Cancel" />                       
               </EditItemTemplate>
               <InsertItemTemplate>
                   <fieldset><table class="shade">
               <tr><td>
               <table><tr><td align="right" style="padding-right:5px"><b>Payer:</b></td>
               <td><asp:TextBox ID="pyerTextBox" Width="100px" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" runat="server" Text='<%# Bind("payr") %>' />
               <td align="right" style="padding-right:5px"><b>Check No:</b></td>
               <td><asp:TextBox ID="chknoTextBox" Width="100px" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" runat="server" Text='<%# Bind("chkno") %>' /></tr>
               <tr><td align="right" style="padding-right:5px"><b>Description:</b></td>
               <td><asp:TextBox ID="dscrTextBox" Width="100px" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" runat="server" Text='<%# Bind("dscr") %>' /></td></tr>
               <tr><td align="right" style="padding-right:5px"><b>Date:</b></td>
               <td><asp:TextBox ID="dateTextBox" runat="server" Width="100px" onfocus="javascript:preEnter( this, 'yes' );" onblur="javascript:preLeave( this, 'date', '99/99/9999' );" Text='<%# Bind("chkdt") %>' />
               <a href="#" tabindex="1" onClick="showCalendarControl(FormView1_dateTextBox); return false"><asp:Image ID="Image1" runat="server" ImageUrl="images/lookup_icon.gif" /></a></td>
               <td colspan="2"><b>Bank Code:</b> &nbsp; &nbsp; <asp:TextBox ID="bnkcdTextBox" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" runat="server" Width="100px" Text='<%# Bind("bnkcd") %>' /></td></tr>
               <tr><td align="right" style="padding-right:5px"><b>Currency Code:</b></td>
               <td><asp:TextBox ID="currTextBox" runat="server" Width="100px" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" Text='<%# Bind("cur_cod") %>' /></td>               
               <td><b>Exchange Rate:</b></td>
               <td><asp:TextBox ID="exgTextBox" Width="50px" onfocus="currfocus()" ForeColor="#ACA899" runat="server" Text='<%# Bind("ex_rate") %>' /></td></tr>
               </table>
               </td>
               <td><fieldset>
               <table><tr><td align="right" style="padding-right:5px"><b>Record#:</b></td>
               <td><asp:TextBox ID="recordTextBox" Width="100px" onfocus="currfocus()" ForeColor="#ACA899" runat="server" Text='<%# Bind("rcrd") %>' /></td></tr>
               <tr><td align="right" style="padding-right:5px"><b>Posted:</b></td>
               <td><asp:TextBox ID="pstdTextBox" Width="100px" onfocus="currfocus()" ForeColor="#ACA899" runat="server" Text='<%# Bind("pstd") %>' />                              
               </td></tr>
               </table>
               </fieldset></td>
               </tr>
               </table></fieldset>
               
                   <asp:TextBox ID="Reckey_TextBox" Visible="false" runat="server" Text='<%# Bind("reckey") %>' ></asp:TextBox>
                   <asp:Button ID="InsertButton" runat="server" CausesValidation="True" CssClass="button" OnClick="InsertButton_Click" Text="Save" />
                   &nbsp;<asp:Button ID="InsertCancelButton" runat="server" CssClass="button" OnClick="Formview1_InsertCancelButtonClick"
                       CausesValidation="False" CommandName="Cancel" Text="Cancel" />                       
               </InsertItemTemplate>
               <ItemTemplate>
               <fieldset><table class="shade">
               <tr><td>
               <table><tr><td align="right" style="padding-right:5px"><b>Payer:</b></td>
               <td><asp:Label ID="pyerTextBox" Width="100px" BackColor="Turquoise" BorderColor="White" BorderStyle="Solid" BorderWidth="1px"  runat="server" Text='<%# Bind("payr") %>' />
               <td align="right" style="padding-right:5px"><b>Check No:</b></td>
               <td><asp:Label ID="chknoTextBox" Width="100px" BackColor="Turquoise" BorderColor="White" BorderStyle="Solid" BorderWidth="1px"  runat="server" Text='<%# Bind("chkno") %>' /></tr>
               <tr><td align="right" style="padding-right:5px"><b>Description:</b></td>
               <td><asp:Label ID="dscrTextBox" Width="100px" BackColor="Turquoise" BorderColor="White" BorderStyle="Solid" BorderWidth="1px"  runat="server" Text='<%# Bind("dscr") %>' /></td></tr>
               <tr><td align="right" style="padding-right:5px"><b>Date:</b></td>
               <td><asp:Label ID="dateTextBox" runat="server" BackColor="Turquoise" BorderColor="White" BorderStyle="Solid" BorderWidth="1px" Width="100px" Text='<%# Bind("chkdt") %>' /></td>
               <td colspan="2"><b>Bank Code:</b>
               <asp:Label ID="bnkcdTextBox" runat="server" BackColor="Turquoise" BorderColor="White" BorderStyle="Solid" BorderWidth="1px"  Width="100px" Text='<%# Bind("bnkcd") %>' /></td></tr>
               <tr><td align="right" style="padding-right:5px"><b>Currency Code:</b></td>
               <td><asp:Label ID="currTextBox" runat="server" BackColor="Turquoise" BorderColor="White" BorderStyle="Solid" BorderWidth="1px"   Width="100px" Text='<%# Bind("cur_cod") %>' /></td>               
               <td><b>Exchange Rate:</b></td>
               <td><asp:Label ID="exgTextBox" Width="50px" BackColor="Turquoise" BorderColor="White" BorderStyle="Solid" BorderWidth="1px"  runat="server" Text='<%# Bind("ex_rate") %>' /><td></tr>
               </table>
               </td>
               <td><fieldset>
               <table><tr><td align="right" style="padding-right:5px"><b>Record#:</b></td>
               <td><asp:Label ID="recordTextBox" Width="100px" BackColor="Turquoise" BorderColor="White" BorderStyle="Solid" BorderWidth="1px"  runat="server" Text='<%# Bind("rcrd") %>' /></td></tr>
               <tr><td align="right" style="padding-right:5px"><b>Posted:</b></td>
               <td><asp:Label ID="pstdTextBox" Width="100px" BackColor="Turquoise" BorderColor="White" BorderStyle="Solid" BorderWidth="1px"  runat="server" Text='<%# Bind("pstd") %>' />                              
               </td></tr>
               </table>
               </fieldset></td>
               </tr>
               </table></fieldset>              
                   
                   
                   
                   <asp:Label ID="reckeyLabel" Visible="false" runat="server" Text='<%# Bind("reckey") %>' />
                   
                   <asp:Button ID="AddButton" runat="server" CssClass="button"  CommandName="new" Text="Add" />
                    <asp:Button ID="UpdateButton" runat="server" CssClass="button" CommandName="edit" Text="Update" />
                   &nbsp;<asp:Button ID="DeleteButton" runat="server" CssClass="button" OnClientClick="return confirm('Are you sure you want to delete this record')" OnClick="delete_Button_Click"  Text="Delete" />                                      
               </ItemTemplate>
           </asp:FormView>
           
           <asp:ObjectDataSource ID="ObjectDataSource1" runat="server" 
                OldValuesParameterFormatString="original_{0}" SelectMethod="MiscellaneousCashReceiptList" 
                TypeName="account">
                <SelectParameters>
                   <asp:Parameter DefaultValue="View" Name="prmAction" Type="String" />                    
                   <asp:Parameter Name="prmComp"  Type="string" />
                   <asp:Parameter Name="prmUser" Type="string" />                     
                   <asp:Parameter Name="prmchkno" Type="String" />                   
                   <asp:Parameter Name="prmpayr" Type="String" />                   
                   <asp:Parameter Name="prmchkamt" Type="Decimal" />
                   <asp:Parameter Name="prmchkdt" Type="String" />
                   <asp:Parameter Name="prmbnkcd" Type="String" />
                   <asp:Parameter Name="prmdscr" Type="String" />                   
                   <asp:Parameter Name="prmcur_cod" Type="String" />
                   <asp:Parameter Name="prmex_rate" Type="Decimal" />
                   <asp:Parameter Name="prmrcrd" Type="Int32" />
                   <asp:Parameter Name="prmpstd" Type="String" />
                   <asp:Parameter Name="prmpost" Type="String" />
                   <asp:Parameter Name="prmout" Type="String" />
                   <asp:SessionParameter SessionField="misc_cash_receipt_cust_reckey_rec" Name="prmReckey" Type="String" />                
                </SelectParameters>
            </asp:ObjectDataSource>
       </div>    
       <div>
       <br />
        <asp:GridView ID="GridView1" runat="server" AllowPaging="True" AllowSorting="True" OnSelectedIndexChanged="GridView1_SelectedIndex"
        AutoGenerateColumns="False" CssClass="Grid" DataSourceID="ObjectDataSource2" DataKeyNames="reckey"
        EmptyDataText="No Record Found"  Width="580px">
        <EmptyDataRowStyle BorderColor="Gray" BorderStyle="Dotted" BorderWidth="0px" Font-Bold="True"
            HorizontalAlign="Center" VerticalAlign="Middle" />
        <RowStyle CssClass="shade" />
        <Columns>
             <asp:CommandField ShowSelectButton="true" ButtonType="image" SelectImageUrl="~/Images/sel.gif" SelectText="" > 
                    <ItemStyle Width="10px" />
                    </asp:CommandField>            
            <asp:BoundField DataField="actno" HeaderText="Account#"  SortExpression="actno" />
            <asp:BoundField DataField="actdscr" HeaderText="Account Description" SortExpression="actdscr" />  
            <asp:BoundField DataField="chkamt" HeaderText="Account# Amt" SortExpression="chkamt" />                  
            
           
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
                   <asp:Panel ID="EditPanel" Width="580px" Height="120px" DefaultButton="UpdateButton" runat="server" >
                    
                   <fieldset class="shade">
                   <table>
                   <tr>
                   <td><b>Account#:</b></td><td><b>Account Description:</b></td><td><b>Account# Amt:</b></td>
                   </tr>
                   <tr><td><asp:TextBox ID="actnumTextBox" runat="server" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" Width="160px" MaxLength="25" Text='<%# Bind("actno") %>' />
                   <a href="#" tabindex="1" onClick="AccountLook(); return false"><asp:Image ID="Image13" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
                   </td>
                   <td><asp:TextBox ID="actdscrLabel" runat="server" ForeColor="#ACA899" onfocus="actfocus()" Width="200px" MaxLength="10" AutoPostBack="true" Text='<%# Bind("actdscr") %>' /></td>
                   <td><asp:TextBox ID="amtTextBox" runat="server" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" Width="160px" MaxLength="25" Text='<%# Bind("chkamt") %>' />
                   <asp:CompareValidator ID="CompareValidator4" runat="server" ControlToValidate="amtTextBox" SetFocusOnError="true" Display="dynamic" Operator="dataTypeCheck" Type="Double" ErrorMessage="Invalid Number"></asp:CompareValidator></td>
                   
                   <td><asp:TextBox ID="reckeyTextBox" Visible="false" runat="server" Text='<%# Bind("reckey") %>' /></td>
                  
                    </tr></table>                   
                   
                   <asp:Button ID="UpdateButton" runat="server" CausesValidation="True" CssClass="button" OnClick="UpdateButton_Formview2_Click" Text="Save" />
                   &nbsp;<asp:Button ID="UpdateCancelButton" runat="server" CausesValidation="False" CssClass="button" CommandName="Cancel" Text="Cancel" />
                   </fieldset>
                   </asp:Panel>
               </EditItemTemplate>
               <InsertItemTemplate>
                   <asp:Panel ID="InsertPanel" Width="580px" Height="120px" DefaultButton="InsertButton" runat="server" >
                    
                   <fieldset class="shade">
                   <table>
                   <tr>
                   <td><b>Account#:</b></td><td><b>Account Description:</b></td><td><b>Account# Amt:</b></td>
                   </tr>
                   <tr><td><asp:TextBox ID="actnumTextBox" runat="server" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" Width="150px" MaxLength="25" Text='<%# Bind("actno") %>' />
                   <a href="#" tabindex="1" onClick="AccountLook(); return false"><asp:Image ID="Image13" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
                   </td>
                   <td><asp:TextBox ID="actdscrLabel" runat="server" ForeColor="#ACA899" onfocus="actfocus()" Width="200px" MaxLength="10" AutoPostBack="true" Text='<%# Bind("actdscr") %>' /></td>
                   <td><asp:TextBox ID="amtTextBox" runat="server" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" Width="150px" MaxLength="25" Text='<%# Bind("chkamt") %>' />
                   <asp:CompareValidator ID="CompareValidator4" runat="server" ControlToValidate="amtTextBox" SetFocusOnError="true" Display="dynamic" Operator="dataTypeCheck" Type="Double" ErrorMessage="Invalid Number"></asp:CompareValidator></td>
                   
                   <td><asp:TextBox ID="reckeyTextBox" Visible="false" runat="server" Text='<%# Bind("reckey") %>' /></td>
                  
                    </tr></table>  
                    
                   <asp:Button ID="InsertButton" runat="server" CausesValidation="True" CssClass="button" OnClick="AddButton_Formview2_Click" Text="Save" />
                   &nbsp;<asp:Button ID="InsertCancelButton" runat="server" CssClass="button" CausesValidation="False" OnClick="CancelButton_FormView2_Delete" CommandName="Cancel" Text="Cancel" />
                   </fieldset></asp:Panel>
               </InsertItemTemplate>
               <ItemTemplate>
                  
                   <asp:Label ID="ReckeyTextBox" Visible="false" runat="server" Text='<%# Bind("[reckey]") %>'></asp:Label>
                   
                   
                    <asp:Button ID="AddButton" runat="server" CssClass="button" CommandName="New" Text="Add" />
                    <asp:Button ID="UpdateItemButton" runat="server" CssClass="button" CommandName="Edit" Text="Update" />
                   &nbsp;<asp:Button ID="DeleteButton" runat="server" CssClass="button" OnClientClick="return confirm('Are you sure you want to delete this record')" OnClick="deleteButton_FormView2_Click" Text="Delete" />
                   
               </ItemTemplate>
           </asp:FormView>
            <asp:Button ID="AddNewFormView2Button" runat="server" CssClass="button"  OnClick="AddNewFormView2Button_Click" Text="Add" />
    
    
           <asp:ObjectDataSource ID="ObjectDataSource3" runat="server" 
               OldValuesParameterFormatString="original_{0}" SelectMethod="MiscellaneousCashReceiptView" 
               TypeName="account">
               <SelectParameters>
                   <asp:Parameter DefaultValue="View" Name="prmAction" Type="String" />
                   <asp:Parameter Name="prmComp" Type="String" />
                   <asp:Parameter Name="prmUser" Type="String" />
                   <asp:Parameter Name="prmcust" Type="String" />
                   <asp:Parameter Name="prmchkno" Type="String" />
                   <asp:Parameter Name="prmchkamt" Type="Decimal" />
                   <asp:Parameter  Name="prmactno"  Type="String" />
                   <asp:Parameter Name="prmactdscr" Type="String" />                   
                   <asp:SessionParameter SessionField="misc_cash_receipt_cust_reckey_rec" Name="prmOut" Type="String" />
                   <asp:SessionParameter SessionField="view_misc_cash_receipt_cust_reckey" Name="prmReckey"  Type="String" />
               </SelectParameters>
           </asp:ObjectDataSource>
    
    
    
           <asp:ObjectDataSource ID="ObjectDataSource2" runat="server" 
               OldValuesParameterFormatString="original_{0}" SelectMethod="MiscellaneousCashReceiptView" 
               TypeName="account">
               <SelectParameters>
                   <asp:Parameter DefaultValue="SelectGrid" Name="prmAction" Type="String" />
                   <asp:Parameter Name="prmComp" Type="String" />
                   <asp:Parameter Name="prmUser" Type="String" />
                   <asp:Parameter Name="prmcust" Type="String" />
                   <asp:Parameter Name="prmchkno" Type="String" />
                   <asp:Parameter Name="prmchkamt" Type="Decimal" />
                   <asp:Parameter  Name="prmactno"  Type="String" />
                   <asp:Parameter Name="prmactdscr" Type="String" /> 
                   <asp:SessionParameter SessionField="misc_cash_receipt_cust_reckey_rec" Name="prmOut" Type="String" />
                   <asp:Parameter Name="prmReckey"  Type="String" />
               </SelectParameters>
           </asp:ObjectDataSource>
       </div>
       
    </div>
    </td></tr></table>    
    <ft:footer id="Footer1" runat="server"></ft:footer>
    </form>
  </body>
</HTML>

