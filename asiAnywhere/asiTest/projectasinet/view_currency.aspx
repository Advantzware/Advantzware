<%@ Page Language="c#" AutoEventWireup="true" Debug="true" Inherits="view_currency" Codebehind="view_currency.aspx.cs" %>
<%@ Register Src="footer.ascx" TagName="Footer" TagPrefix="ft" %>
<%@ Register Src="header.ascx" TagName="Header" TagPrefix="hd" %>
<html xmlns="http://www.w3.org/1999/xhtml" >
  <head id="Head1" runat="server">
    <title>Currency</title>
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
       var NewWindow = window.open("accountlook.aspx?jrnl=" + "A" + " ", "AccountLookupWindow", "width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
   }

   function AccountLookup(ReturnObj1, ReturnObj2) {
       document.forms[0].actnumTextBox.value = ReturnObj1;
       document.forms[0].accdscrTextBox.value = ReturnObj1;

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
          <TD align=left nowrap><font size=+0><b>Currency</b></font></TD>
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
      <asp:LinkButton ID="lnk_Listcurrency" runat="server" OnClick="lnk_listcurrency" >Browse Curr</asp:LinkButton></li>
      <li class="selected"><asp:LinkButton ID="lnk_viewcustomers" runat="server" OnClick="lnk_viewcurr_Click" > View Currency </asp:LinkButton></li></ul></div>
      
      </td>
      </tr></table>
       <div>
           <asp:FormView ID="FormView1" runat="server" DataSourceID="ObjectDataSource1" OnDataBound="FormView1_ondataBound">
               <EditItemTemplate>
                   <asp:Panel ID="editpanel" runat="server" DefaultButton=""><fieldset class="shade">
                   <table><tr><td align="right" style="padding-right:5px;"><b>Currency Code:</b></td>
                   <td><asp:TextBox ID="c_codeTextBox" MaxLength="3" Width="60" runat="server" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)"  Text='<%# Bind("[c-code]") %>' />
                   <asp:TextBox ID="c_descTextBox" runat="server" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)"  Text='<%# Bind("[c-desc]") %>' />
                   </td></tr>
                   <tr><td align="right" style="padding-right:5px;"><b>Country:</b></td>
                   <td><asp:TextBox ID="countryTextBox" Width="120px" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)"  runat="server" Text='<%# Bind("country") %>' /></td></tr>
                   <tr><td align="right" style="padding-right:5px;"><b>Numeric Code:</b></td>
                   <td><asp:TextBox ID="c_numTextBox" Width="60px" MaxLength="3" runat="server" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)"  Text='<%# Bind("[c-num]") %>' />
                   <asp:CompareValidator ID="CompareValidator7" runat="server" ControlToValidate="c_numTextBox" Operator="DataTypeCheck" Type="Integer" Display="Dynamic" SetFocusOnError="true" ErrorMessage="Enter Integer Value"></asp:CompareValidator>
                   </td></tr>
                   <tr><td align="right" style="padding-right:5px;"><b>Minor Unit:</b></td>
                   <td><asp:TextBox ID="minor_unitTextBox" MaxLength="2" Width="60px" runat="server" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)"  Text='<%# Bind("[minor-unit]") %>' /></td></tr>
                   <tr><td align="right" style="padding-right:5px;"><b>Base?:</b></td>
                   <td><asp:TextBox ID="is_baseTextBox" Visible="false" Width="60px" runat="server" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)"  Text='<%# Bind("[is-base]") %>' />
                       <asp:CheckBox ID="CheckBox1" runat="server" />
                   </td></tr>
                   <tr><td align="right" style="padding-right:5px;"><b>Exchange Date:</b></td>
                   <td><asp:TextBox ID="ex_rateTextBox" MaxLength="8" Width="100px" runat="server" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)"  Text='<%# Bind("[ex-rate]") %>' />
                   <asp:CompareValidator ID="CompareValidator1" runat="server" ControlToValidate="ex_rateTextBox" Operator="DataTypeCheck" Type="Double" Display="Dynamic" SetFocusOnError="true" ErrorMessage="Enter Decimal Value"></asp:CompareValidator>
                   </td></tr>
                   <tr><td align="right" style="padding-right:5px;"><b>Foreign Exchange Gain/Loss Account:</b></td>
                   <td><asp:TextBox ID="ar_ast_acctTextBox" Width="100px" runat="server" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)"  Text='<%# Bind("[ar-ast-acct]") %>' /></td></tr>
                   
                   </table>                   
                                      
                   <asp:TextBox ID="reckeyTextBox" Visible="false" runat="server" Text='<%# Bind("reckey") %>' />
                   <br />
                   <asp:Button ID="UpdateButton" runat="server" CausesValidation="True" CssClass="button"
                       OnClick="UpdateButton_Click" Text="Save" />
                   &nbsp;<asp:Button ID="UpdateCancelButton" runat="server" CssClass="button"
                       CausesValidation="False" CommandName="Cancel" Text="Cancel" />
                   </fieldset></asp:Panel>
               </EditItemTemplate>
               <InsertItemTemplate>
                   <asp:Panel ID="insertpanel" runat="server" DefaultButton=""><fieldset class="shade">
                   <table><tr><td align="right" style="padding-right:5px;"><b>Currency Code:</b></td>
                   <td><asp:TextBox ID="c_codeTextBox" MaxLength="3" Width="60" runat="server" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)"  Text='<%# Bind("[c-code]") %>' />
                   <asp:TextBox ID="c_descTextBox" runat="server" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)"  Text='<%# Bind("[c-desc]") %>' />
                   </td></tr>
                   <tr><td align="right" style="padding-right:5px;"><b>Country:</b></td>
                   <td><asp:TextBox ID="countryTextBox" Width="120px" runat="server" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)"  Text='<%# Bind("country") %>' /></td></tr>
                   <tr><td align="right" style="padding-right:5px;"><b>Numeric Code:</b></td>
                   <td><asp:TextBox ID="c_numTextBox" Width="60px" MaxLength="3" runat="server" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)"  Text='<%# Bind("[c-num]") %>' />
                   <asp:CompareValidator ID="CompareValidator7" runat="server" ControlToValidate="c_numTextBox" Operator="DataTypeCheck" Type="Integer" Display="Dynamic" SetFocusOnError="true" ErrorMessage="Enter Integer Value"></asp:CompareValidator>
                   </td></tr>
                   <tr><td align="right" style="padding-right:5px;"><b>Minor Unit:</b></td>
                   <td><asp:TextBox ID="minor_unitTextBox" MaxLength="2" Width="60px" runat="server" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)"  Text='<%# Bind("[minor-unit]") %>' /></td></tr>
                   <tr><td align="right" style="padding-right:5px;"><b>Base?:</b></td>
                   <td><asp:TextBox ID="is_baseTextBox" Visible="false" Width="60px" runat="server" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)"  Text='<%# Bind("[is-base]") %>' />
                       <asp:CheckBox ID="CheckBox1" runat="server" />
                   </td></tr>
                   <tr><td align="right" style="padding-right:5px;"><b>Exchange Date:</b></td>
                   <td><asp:TextBox ID="ex_rateTextBox" MaxLength="8" Width="100px" runat="server" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)"  Text='<%# Bind("[ex-rate]") %>' />
                   <asp:CompareValidator ID="CompareValidator1" runat="server" ControlToValidate="ex_rateTextBox" Operator="DataTypeCheck" Type="Double" Display="Dynamic" SetFocusOnError="true" ErrorMessage="Enter Decimal Value"></asp:CompareValidator>
                   </td></tr>
                   <tr><td align="right" style="padding-right:5px;"><b>Foreign Exchange Gain/Loss Account:</b></td>
                   <td><asp:TextBox ID="ar_ast_acctTextBox" Width="100px" runat="server" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" Text='<%# Bind("[ar-ast-acct]") %>' /></td></tr>
                   
                   </table> 
                   <asp:TextBox ID="reckeyTextBox" Visible="false" runat="server" Text='<%# Bind("reckey") %>' />
                   <br />
                   <asp:Button ID="InsertButton" runat="server" CausesValidation="True" CssClass="button" 
                     OnClick="addButton_Click" Text="Save" />
                   &nbsp;<asp:Button ID="InsertCancelButton" runat="server" CssClass="button"
                       CausesValidation="False" CommandName="Cancel" Text="Cancel" /></fieldset></asp:Panel>
               </InsertItemTemplate>
               <ItemTemplate>
               <fieldset  class="shade">
                   <table><tr><td align="right" style="padding-right:5px;"><b>Currency Code:</b></td>
                   <td><asp:Label ID="c_codeLabel" runat="server" Width="50px" BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" Text='<%# Bind("[c-code]") %>' />
                   <asp:Label ID="c_descLabel" runat="server" Width="140px" BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" Text='<%# Bind("[c-desc]") %>' />
                   </td></tr>
                   <tr><td align="right" style="padding-right:5px;"><b>Country:</b></td>
                   <td><asp:Label ID="countryLabel" runat="server" Width="100px" BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" Text='<%# Bind("country") %>' /></td></tr>
                   <tr><td align="right" style="padding-right:5px;"><b>Numeric Code:</b></td>
                   <td><asp:Label ID="c_numLabel" runat="server" Width="100px" BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" Text='<%# Bind("[c-num]") %>' /></td></tr>
                   <tr><td align="right" style="padding-right:5px;"><b>Minor Unit:</b></td>
                   <td><asp:Label ID="minor_unitLabel" runat="server" Width="100px" BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" Text='<%# Bind("[minor-unit]") %>' /></td></tr>
                   <tr><td align="right" style="padding-right:5px;"><b>Base?:</b></td>
                   <td><asp:Label ID="is_baseLabel" runat="server" Width="100px" BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" Text='<%# Bind("[is-base]") %>' /></td></tr>
                   <tr><td align="right" style="padding-right:5px;"><b>Exchange Date:</b></td>
                   <td><asp:Label ID="ex_rateLabel" runat="server" Width="100px" BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" Text='<%# Bind("[ex-rate]") %>' /></td></tr>
                   <tr><td align="right" style="padding-right:5px;"><b>Foreign Exchange Gain/Loss Account:</b></td>
                   <td><asp:Label ID="ar_ast_acctLabel" runat="server" Width="100px" BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" Text='<%# Bind("[ar-ast-acct]") %>' /></td></tr>
                   
                   </table>
                   
                   
                   
                   <asp:Label ID="reckeyLabel" Visible="false" runat="server" Text='<%# Bind("reckey") %>' />
                   
                   <br />
                   <asp:Button ID="AddButton" runat="server" CssClass="buttonM" CausesValidation="False" CommandName="new"
                      Text="Add" >
                    </asp:Button>
                    <asp:Button ID="EditButton" runat="server" CssClass="buttonM" CausesValidation="False" CommandName="Edit"
                      Text="Update">
                    </asp:Button>
                   <asp:Button ID="DeleteButton" runat="server" OnClick="delete_Button_Click" CssClass="buttonM" CausesValidation="False" OnClientClick="return confirm('Are you sure you want to delete')"
                      Text="Delete"></asp:Button>
                   </fieldset>
               </ItemTemplate>
           </asp:FormView>
           
           
           <asp:ObjectDataSource ID="ObjectDataSource1" runat="server" 
                OldValuesParameterFormatString="original_{0}" SelectMethod="CurrencyList" 
                TypeName="ledger">
                <SelectParameters>
                   <asp:Parameter DefaultValue="View" Name="prmAction" Type="String" />                                                       
                    <asp:Parameter Name="prmcomp" Type="String" />
                   <asp:Parameter Name="prmUser" Type="string" />                     
                    <asp:Parameter Name="prmCcode" Type="String" />
                   <asp:Parameter Name="prmCdesc"  Type="string" />
                   <asp:Parameter Name="prmCountry" Type="String"></asp:Parameter>
                   <asp:Parameter Name="prmCnum" Type="String" />                   
                   <asp:Parameter Name="prMinorunit" Type="String" />
                   <asp:Parameter Name="prmIsbase" Type="String" />
                   <asp:Parameter Name="prmExrate" Type="String" />
                    <asp:Parameter Name="prmArastacct" Type="String" />
                   <asp:SessionParameter SessionField="currency_list_reckey" Name="prmReckey" Type="String" />
                </SelectParameters>
            </asp:ObjectDataSource>
       </div>    
       
       
    </div>
    </td></tr></table>    
    <ft:footer id="Footer1" runat="server"></ft:footer>
    </form>
  </body>
</HTML>

