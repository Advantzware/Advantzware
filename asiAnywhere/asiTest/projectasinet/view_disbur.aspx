<%@ Page Language="c#" AutoEventWireup="true" Debug="true" Inherits="view_disbur" Codebehind="view_disbur.aspx.cs" %>
<%@ Register Src="footer.ascx" TagName="Footer" TagPrefix="ft" %>
<%@ Register Src="header.ascx" TagName="Header" TagPrefix="hd" %>
<html xmlns="http://www.w3.org/1999/xhtml" >
  <head id="Head1" runat="server">
    <title>Cash Disbursements</title>
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
        vname.value = ReturnObj2;
        document.forms[0].FormView1_vendnoTextBox.focus();
    }

    function banklookup() {

        var NewWindow = window.open("bank_lookup.aspx", "banklookup", "width=500,height=400,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
    }
    function banklook(ReturnObj1, ReturnObj2) {
        document.forms[0].FormView1_bankcodeTextBox.value = ReturnObj1;
        document.forms[0].FormView1_banknameTextBox.value = ReturnObj2;        
        
    }

    function AccountLook() {
        var NewWindow = window.open("accountlook.aspx", "AccountLookupWindow", "width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
    }

    function AccountLookup(ReturnObj1) {
        document.forms[0].FormView2_actTextBox.value = ReturnObj1;
        

    }

    function invinflook() {

        var vend = document.getElementById("FormView1_vendnoLabel");
        var fielddate = document.getElementById("FormView1_memodateLabel");
       
        var NewWindow = window.open("invinfo_lookup.aspx?vend=" + vend.innerHTML + "&datefield="+ fielddate.innerHTML +"", "invinfoWindow", "width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
        //var NewWindow = window.open("invinfo_lookup.aspx", "invinfoWindow", "width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
    }

    function InvInfoLookup(ReturnObj1, ReturnObj2, ReturnObj3, ReturnObj4, ReturnObj5, ReturnObj6) {
        document.forms[0].FormView2_invTextBox.value = ReturnObj1;
        document.forms[0].FormView2_duedateLabel.value = ReturnObj2;
        document.forms[0].FormView2_baldueLabel.value = ReturnObj3;
        document.forms[0].FormView2_cramtTextBox.value = ReturnObj3;
        document.forms[0].FormView2_dbamtTextBox.value = ReturnObj5;
        
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
   function printrep() {
       var NewWindow = window.open("topbtnorderreport.aspx", "OrderReport", "width=800,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
   }
   function printackrep() {
       var NewWindow = window.open("topprintorderack_report.aspx", "OrderAcknowledgementReport", "width=800,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
   }
   function ordernotes() {
       var NewWindow = window.open("top_list_notes.aspx", "OrderListNotes", "width=600,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
   }

   function invfocus() {
       var inv = document.getElementById("FormView2_invTextBox");
       inv.focus();
   }
   function crdscrfocus() {
       var crt = document.getElementById("FormView2_dscrTextBox");
       crt.focus();
   }
   function cramtfocus() {
       var crt = document.getElementById("FormView2_dscrTextBox");
       crt.focus();
   }
   function datefocus() {
       var vendor = document.getElementById("FormView1_vendnoTextBox");
       vendor.focus();
   }
   function checkno() {
       var checkno = document.getElementById("FormView1_ChecknoTextBox");
       checkno.focus();
   } 
   function payeefocus() {
       var checkno = document.getElementById("FormView1_payeeTextBox");
       checkno.focus();
   }
   function exgrt() {
       var exg = document.getElementById("FormView1_exchTextBox");
       exg.focus();
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
          <TD align=left nowrap><font size=+0><b>Cash Disbursements&nbsp;</b></font></TD>
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
      <asp:LinkButton ID="lnk_Listcustomers" runat="server" OnClick="lnk_listcash" >Brws Disb</asp:LinkButton></li>
      <li class="selected"><asp:LinkButton ID="lnk_viewcustomers" runat="server" OnClick="lnk_viewcustomers_Click" > View Disb</asp:LinkButton></li></ul></div>
            
      </td>
      </tr></table>
       <div>
           <asp:FormView ID="FormView1" runat="server" OnDataBound="FormView1_OnDataBound" DataSourceID="ObjectDataSource1">
               <EditItemTemplate>
               <asp:Panel ID="editpanel" runat="server" DefaultButton="UpdateButton">
               <fieldset class="shade"><table class="shade">               
               <tr><td align="right" style="padding-right:5px"><b>Vendor#:</b></td>
               <td><asp:TextBox ID="vendnoTextBox" Width="100px" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)"  runat="server" Text='<%# Bind("vend") %>' />
               <a href="#" tabindex="1" onclick="vendorlook(); return false"><asp:Image ID="Image4" runat="server" ImageUrl="images/lookup_icon.gif" /></a></td>
               <td colspan="2"><asp:TextBox ID="vendnameLabel" ForeColor="#ACA899" onfocus="payeefocus()" Width="200" runat="server" Text='<%# Bind("vname") %>' /></td></tr>
               <tr><td align="right" style="padding-right:5px"><b>Payee:</b></td>
               <td><asp:TextBox ID="payeeTextBox" runat="server" Width="100px" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" Text='<%# Bind("payee") %>' /></td></tr>
               <tr><td align="right" style="padding-right:5px"><b>Date:</b></td>
               <td><asp:TextBox ID="chkdateTextBox" runat="server" Width="100px" onfocus="javascript:preEnter( this, 'yes' );" onblur="javascript:preLeave( this, 'date', '99/99/9999' );" Text='<%# Bind("checkdate") %>' />
               <a href="#" tabindex="1" onClick="showCalendarControl(FormView1_chkdateTextBox); return false"><asp:Image ID="Image1" runat="server" ImageUrl="images/lookup_icon.gif" /></a></td></tr>
               <tr><td align="right" style="padding-right:5px"><b>Bank:</b></td>
               <td><asp:TextBox ID="bankcodeTextBox" Width="100px" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" runat="server" Text='<%# Bind("bnkcod") %>' />
               <a href="#" tabindex="1" onclick="banklookup(); return false"><asp:Image ID="banklookimg" runat="server" ImageUrl="images/lookup_icon.gif" /></a></td>
               <td colspan="2"><asp:TextBox ID="banknameTextBox" Width="200px" onfocus="checkno()" ForeColor="#ACA899" runat="server" Text='<%# Bind("bnkname") %>' /></td></tr>
               <tr><td align="right" style="padding-right:5px"><b>Check No:</b></td>
               <td><asp:TextBox ID="ChecknoTextBox" Width="100px" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" runat="server" Text='<%# Bind("checkno") %>' /></td>               
               <td align="right" style="padding-right:5px"><b>Amount:</b></td>
               <td><asp:TextBox ID="amtTextBox" onfocus="exgrt()" ForeColor="#ACA899" runat="server" Width="100px" Text='<%# Bind("checkamt") %>' /></td></tr>
               <tr><td align="right" style="padding-right:5px"><b>Currency Code:</b></td>
               <td><asp:TextBox ID="currcodTextBox" Width="100px" ForeColor="#ACA899" onfocus="exgrt()" runat="server" Text='<%# Bind("currcod") %>' /></td>               
               <td align="right" style="padding-right:5px"><b>Exchange Rate:</b></td>
               <td><asp:TextBox ID="exchTextBox" runat="server" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" Width="100px" Text='<%# Bind("exrat") %>' /></td></tr> 
                </table>
               
               
                   <asp:Button ID="UpdateButton" runat="server" CausesValidation="True" CssClass="button" OnClick="UpdateButton_Click"  Text="Save" />
                   &nbsp;<asp:Button ID="UpdateCancelButton" runat="server" CssClass="button"
                       CausesValidation="False" CommandName="Cancel" Text="Cancel" />
                       </fieldset></asp:Panel>
               </EditItemTemplate>
               <InsertItemTemplate>
               <asp:Panel ID="insertpanel" runat="server" DefaultButton="InsertButton">
                   <fieldset><table class="shade">               
               <tr><td align="right" style="padding-right:5px"><b>Vendor#:</b></td>
               <td><asp:TextBox ID="vendnoTextBox" Width="100px" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)"  runat="server" Text='<%# Bind("vend") %>' />
               <a href="#" tabindex="1" onclick="vendorlook(); return false"><asp:Image ID="Image4" runat="server" ImageUrl="images/lookup_icon.gif" /></a></td>
               <td colspan="2"><asp:TextBox ID="vendnameLabel" ForeColor="#ACA899" onfocus="payeefocus()" Width="200" runat="server" Text='<%# Bind("vname") %>' /></td></tr>
               <tr><td align="right" style="padding-right:5px"><b>Payee:</b></td>
               <td><asp:TextBox ID="payeeTextBox" runat="server" Width="100px" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" Text='<%# Bind("payee") %>' /></td></tr>
               <tr><td align="right" style="padding-right:5px"><b>Date:</b></td>
               <td><asp:TextBox ID="chkdateTextBox" runat="server" Width="100px" onfocus="javascript:preEnter( this, 'yes' );" onblur="javascript:preLeave( this, 'date', '99/99/9999' );" Text='<%# Bind("checkdate") %>' />
               <a href="#" tabindex="1" onClick="showCalendarControl(FormView1_chkdateTextBox); return false"><asp:Image ID="Image1" runat="server" ImageUrl="images/lookup_icon.gif" /></a></td></tr>
               <tr><td align="right" style="padding-right:5px"><b>Bank:</b></td>
               <td><asp:TextBox ID="bankcodeTextBox" Width="100px" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" runat="server" Text='<%# Bind("bnkcod") %>' />
               <a href="#" tabindex="1" onclick="banklookup(); return false"><asp:Image ID="banklookimg" runat="server" ImageUrl="images/lookup_icon.gif" /></a></td>
               <td colspan="2"><asp:TextBox ID="banknameTextBox" Width="200px" onfocus="checkno()" ForeColor="#ACA899" runat="server" Text='<%# Bind("bnkname") %>' /></td></tr>
               <tr><td align="right" style="padding-right:5px"><b>Check No:</b></td>
               <td><asp:TextBox ID="ChecknoTextBox" Width="100px" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" runat="server" Text='<%# Bind("checkno") %>' /></td>               
               <td align="right" style="padding-right:5px"><b>Amount:</b></td>
               <td><asp:TextBox ID="amtTextBox" onfocus="exgrt()" ForeColor="#ACA899" runat="server" Width="100px" Text='<%# Bind("checkamt") %>' /></td></tr>
               <tr><td align="right" style="padding-right:5px"><b>Currency Code:</b></td>
               <td><asp:TextBox ID="currcodTextBox" Width="100px" ForeColor="#ACA899" onfocus="exgrt()" runat="server" Text='<%# Bind("currcod") %>' /></td>               
               <td align="right" style="padding-right:5px"><b>Exchange Rate:</b></td>
               <td><asp:TextBox ID="exchTextBox" runat="server" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" Width="100px" Text='<%# Bind("exrat") %>' /></td></tr> 
                </table>
               <br />
                   <asp:TextBox ID="Reckey_TextBox" Visible="false" runat="server" Text='<%# Bind("reckey") %>' ></asp:TextBox>
                   <asp:Button ID="InsertButton" runat="server" CausesValidation="True" CssClass="button" OnClick="InsertButton_Click" Text="Save" />
                   &nbsp;<asp:Button ID="InsertCancelButton" runat="server" CssClass="button" OnClick="Formview1_InsertCancelButtonClick"
                       CausesValidation="False" CommandName="Cancel" Text="Cancel" />
                       </fieldset></asp:Panel>
               </InsertItemTemplate>
               <ItemTemplate>
               <fieldset>
               <table class="shade">
               
               <fieldset><table class="shade">               
               <tr><td align="right" style="padding-right:5px"><b>Vendor#:</b></td>
               <td><asp:Label ID="vendnoLabel" Width="100px" BackColor="Turquoise" BorderColor="White" BorderStyle="Solid" BorderWidth="1px"  runat="server" Text='<%# Bind("vend") %>' /></td>
               <td colspan="2"><asp:Label ID="vendnameLabel" BackColor="Turquoise" BorderColor="White" BorderStyle="Solid" BorderWidth="1px"  Width="200" runat="server" Text='<%# Bind("vname") %>' /></td></tr>
               <tr><td align="right" style="padding-right:5px"><b>Payee:</b></td>
               <td><asp:Label ID="payeeLabel" runat="server" BackColor="Turquoise" BorderColor="White" BorderStyle="Solid" BorderWidth="1px"  Width="100px" Text='<%# Bind("payee") %>' /></td></tr>
               <tr><td align="right" style="padding-right:5px"><b>Date:</b></td>
               <td><asp:Label ID="memodateLabel" runat="server" BackColor="Turquoise" BorderColor="White" BorderStyle="Solid" BorderWidth="1px"  Width="100px" Text='<%# Bind("checkdate") %>' /></td></tr>
               <tr><td align="right" style="padding-right:5px"><b>Bank:</b></td>
               <td><asp:Label ID="bankcodeLabel" Width="100px" BackColor="Turquoise" BorderColor="White" BorderStyle="Solid" BorderWidth="1px"  runat="server" Text='<%# Bind("bnkcod") %>' /></td>
               <td colspan="2"><asp:Label ID="banknameLabel" BackColor="Turquoise" BorderColor="White" BorderStyle="Solid" BorderWidth="1px"  Width="200px" runat="server" Text='<%# Bind("bnkname") %>' /></td></tr>
               <tr><td align="right" style="padding-right:5px"><b>Check No:</b></td>
               <td><asp:Label ID="ChecknoLabel" Width="100px" BackColor="Turquoise" BorderColor="White" BorderStyle="Solid" BorderWidth="1px"  runat="server" Text='<%# Bind("checkno") %>' /></td>               
               <td align="right" style="padding-right:5px"><b>Amount:</b></td>
               <td><asp:Label ID="amtLabel" runat="server" BackColor="Turquoise" BorderColor="White" BorderStyle="Solid" BorderWidth="1px"  Width="100px" Text='<%# Bind("checkamt") %>' /></td></tr>
               <tr><td align="right" style="padding-right:5px"><b>Currency Code:</b></td>
               <td><asp:Label ID="currcodLabel" Width="100px" BackColor="Turquoise" BorderColor="White" BorderStyle="Solid" BorderWidth="1px"  runat="server" Text='<%# Bind("currcod") %>' /></td>               
               <td align="right" style="padding-right:5px"><b>Exchange Rate:</b></td>
               <td><asp:Label ID="exchLabel" runat="server" BackColor="Turquoise" BorderColor="White" BorderStyle="Solid" BorderWidth="1px"  Width="100px" Text='<%# Bind("exrat") %>' /></td></tr> 
                </table>                   
                   <asp:Label ID="reckeyLabel" Visible="false" runat="server" Text='<%# Bind("reckey") %>' />                   
                   <br />                   
                   <asp:Button ID="AddButton" runat="server" CssClass="button"  CommandName="new" Text="Add" />
                    <asp:Button ID="UpdateButton" runat="server" CssClass="button" CommandName="edit" Text="Update" />
                   &nbsp;<asp:Button ID="DeleteButton" runat="server" CssClass="button" OnClientClick="return confirm('Are you sure you want to delete this record')" OnClick="delete_Button_Click"  Text="Delete" />
                   </fieldset>
               </ItemTemplate>
           </asp:FormView>
           
           <asp:ObjectDataSource ID="ObjectDataSource1" runat="server" 
                OldValuesParameterFormatString="original_{0}" SelectMethod="SelectDisbursements" 
                TypeName="voucherpay">
                <SelectParameters>
                   <asp:Parameter DefaultValue="View" Name="prmAction" Type="String" />                    
                   <asp:Parameter Name="prmComp"  Type="string" />
                   <asp:Parameter Name="prmUser" Type="string" />                     
                   <asp:Parameter Name="prmvend" Type="String" />                   
                   <asp:Parameter Name="prmvname" Type="String" />                   
                   <asp:Parameter Name="prmcheckno" Type="Int32" />
                   <asp:Parameter Name="prmcheckdate" Type="String" />
                   <asp:Parameter Name="prmpayee" Type="String" />                    
                   <asp:Parameter Name="prmcheckamt" Type="Decimal" />
                   <asp:Parameter Name="prmbnkcod" Type="String" /> 
                   <asp:Parameter Name="prmbnkname" Type="String" /> 
                   <asp:Parameter Name="prmcurrcod" Type="String" /> 
                   <asp:Parameter Name="prmexrat" Type="Decimal" /> 
                   <asp:SessionParameter Name="prmReckey" SessionField="disbur_list_reckey_rec" Type="String" />
                </SelectParameters>
            </asp:ObjectDataSource>
       </div>    
       <div>
       <br />
        <asp:GridView ID="GridView1" runat="server" AllowPaging="True" AllowSorting="True" OnSelectedIndexChanged="GridView1_SelectedIndex"
        AutoGenerateColumns="False" CssClass="Grid" DataSourceID="ObjectDataSource2" DataKeyNames="reckey"
        EmptyDataText="No Record Found"  Width="550px">
        <EmptyDataRowStyle BorderColor="Gray" BorderStyle="Dotted" BorderWidth="0px" Font-Bold="True"
            HorizontalAlign="Center" VerticalAlign="Middle" />
        <RowStyle CssClass="shade" />
        <Columns>
             <asp:CommandField ShowSelectButton="true" ButtonType="image" SelectImageUrl="~/Images/sel.gif" SelectText="" > 
                    <ItemStyle Width="10px" />
                    </asp:CommandField>
            <asp:BoundField DataField="vline" HeaderText="Line"  SortExpression="vline" />
            <asp:BoundField DataField="dscr" HeaderText="Description "    SortExpression="dscr" />
            <asp:BoundField DataField="actnum" HeaderText="Account Number" SortExpression="actnum" />
            <asp:BoundField DataField="qty" HeaderText="Quantity" SortExpression="qty" />
            <asp:BoundField DataField="untprice" HeaderText="Unit Price" SortExpression="untprice" />
            <asp:BoundField DataField="amt" HeaderText="Amount" SortExpression="amt" />
               
                       
           
            <asp:BoundField DataField="reckey" HeaderText="reckey" Visible="false" SortExpression="reckey" />
            <asp:TemplateField HeaderText="Reckey" Visible="false" >
            <ItemTemplate>
            <asp:Label ID="reclabel" runat="server" Text='<%# Bind("[reckey]") %>'></asp:Label>
            </ItemTemplate>
            </asp:TemplateField>
        </Columns>
        <SelectedRowStyle CssClass="GridSelected" BackColor="Yellow" />
        <HeaderStyle  ForeColor="White" CssClass="headcolor" HorizontalAlign="Center"
            VerticalAlign="Middle" Wrap="False" />
        <AlternatingRowStyle CssClass="GridItemOdd" />
    </asp:GridView><br />       
           
           
           <asp:FormView ID="FormView2" runat="server" OnDataBound="FormView2_OnDataBound" DataSourceID="ObjectDataSource3">
               <EditItemTemplate>
                   <asp:Panel ID="EditPanel" Width="590px" Height="120px" DefaultButton="UpdateButton" runat="server" >
                    
                   <fieldset class="shade">
                   <table>
                   <tr>
                   <td><b>Line:</b></td><td><b>Description:</b></td><td><b>Account Number:</b></td><td><b>Quantity:</b></td>
                   <td><b>Unit Price:</b></td><td><b>Amount:</b></td>
                   </tr>
                   <tr><td>
                   <asp:TextBox ID="lineTextBox" runat="server" ForeColor="#ACA899" onfocus="crdscrfocus()" Width="100px" Text='<%# Bind("vline") %>' />
                   </td>
                   <td><asp:TextBox ID="dscrTextBox" runat="server" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" Width="100px" Text='<%# Bind("dscr") %>' /></td>
                   <td>
                   <asp:TextBox ID="actTextBox" runat="server" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" Width="100px" Text='<%# Bind("actnum") %>' />                   
                   <a href="#" tabindex="1" onClick="AccountLook(); return false"><asp:Image ID="Image13" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
                   </td>
                   <td>
                   <asp:TextBox ID="qtyTextBox" runat="server" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" Width="70px"  MaxLength="11" Text='<%# Bind("qty") %>' />                   
                   </td>
                   <td>
                   <asp:TextBox ID="untprTextBox" runat="server" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" Width="70px" MaxLength="11" Text='<%# Bind("untprice") %>' />
                   <asp:CompareValidator ID="CompareValidator3" runat="server" ControlToValidate="untprTextBox" SetFocusOnError="true" Display="dynamic" Operator="dataTypeCheck" Type="Double" ErrorMessage="Invalid Number"></asp:CompareValidator></td>
                   <td><asp:TextBox ID="amtTextBox" runat="server" Width="70px" ForeColor="#ACA899" onfocus="cramtfocus()" Text='<%# Bind("amt") %>' />
                   </td>
                                    
                   <td><asp:TextBox ID="reckeyTextBox" Visible="false" runat="server" Text='<%# Bind("reckey") %>' /></td>
                  
                    </tr></table>                   
                   
                   <asp:Button ID="UpdateButton" runat="server" CausesValidation="True" CssClass="button" OnClick="UpdateButton_Formview2_Click" Text="Save" />
                   &nbsp;<asp:Button ID="UpdateCancelButton" runat="server" CausesValidation="False" CssClass="button" CommandName="Cancel" Text="Cancel" />
                   </fieldset>
                   </asp:Panel>
               </EditItemTemplate>
               <InsertItemTemplate>
                   <asp:Panel ID="EditPanel" Width="590px" Height="120px" DefaultButton="InsertButton" runat="server" >
                    
                   <fieldset class="shade">
                   <table>
                   <tr>
                   <td><b>Line:</b></td><td><b>Description:</b></td><td><b>Account Number:</b></td><td><b>Quantity:</b></td>
                   <td><b>Unit Price:</b></td><td><b>Amount:</b></td>
                   </tr>
                   <tr><td>
                   <asp:TextBox ID="lineTextBox" runat="server" ForeColor="#ACA899" onfocus="crdscrfocus()" Width="100px" Text='<%# Bind("vline") %>' />
                   </td>
                   <td><asp:TextBox ID="dscrTextBox" runat="server" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" Width="100px" Text='<%# Bind("dscr") %>' /></td>
                   <td>
                   <asp:TextBox ID="actTextBox" runat="server" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" Width="100px" Text='<%# Bind("actnum") %>' />                   
                   <a href="#" tabindex="1" onClick="AccountLook(); return false"><asp:Image ID="Image13" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
                   </td>
                   <td>
                   <asp:TextBox ID="qtyTextBox" runat="server" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" Width="70px"  MaxLength="11" Text='<%# Bind("qty") %>' />                   
                   </td>
                   <td>
                   <asp:TextBox ID="untprTextBox" runat="server" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" Width="70px" MaxLength="11" Text='<%# Bind("untprice") %>' />
                   <asp:CompareValidator ID="CompareValidator3" runat="server" ControlToValidate="untprTextBox" SetFocusOnError="true" Display="dynamic" Operator="dataTypeCheck" Type="Double" ErrorMessage="Invalid Number"></asp:CompareValidator></td>
                   <td><asp:TextBox ID="amtTextBox" runat="server" Width="70px" ForeColor="#ACA899" onfocus="cramtfocus()" Text='<%# Bind("amt") %>' />
                   </td>
                                    
                   <td><asp:TextBox ID="reckeyTextBox" Visible="false" runat="server" Text='<%# Bind("reckey") %>' /></td>
                  
                    </tr></table>   
                    
                   <asp:Button ID="InsertButton" runat="server" CausesValidation="True" CssClass="button" OnClick="AddButton_Formview2_Click" Text="Save" />
                   &nbsp;<asp:Button ID="InsertCancelButton" runat="server" CssClass="button" CausesValidation="False" OnClick="CancelButton_FormView2_Delete" CommandName="Cancel" Text="Cancel" />
                   </fieldset></asp:Panel>
               </InsertItemTemplate>
               <ItemTemplate>
              
                   <asp:Label ID="ReckeyLabel" Visible="false"  runat="server" Text='<%# Bind("[reckey]") %>'></asp:Label>
                   
                   
                    <asp:Button ID="AddButton" runat="server" CssClass="button" CommandName="New" Text="Add" />
                    <asp:Button ID="UpdateItemButton" runat="server" CssClass="button" CommandName="Edit" Text="Update" />
                   &nbsp;<asp:Button ID="DeleteButton" runat="server" CssClass="button" OnClientClick="return confirm('Are you sure you want to delete this record')" OnClick="deleteButton_FormView2_Click" Text="Delete" />
                   
               </ItemTemplate>
           </asp:FormView>
            <asp:Button ID="AddNewFormView2Button" runat="server" CssClass="button"  OnClick="AddNewFormView2Button_Click" Text="Add" />
    
    
           <asp:ObjectDataSource ID="ObjectDataSource3" runat="server" 
               OldValuesParameterFormatString="original_{0}" SelectMethod="SelectViewDisbursements" 
               TypeName="voucherpay">
               <SelectParameters>                                      
                   <asp:Parameter DefaultValue="View" Name="prmAction" Type="String" />
                   <asp:Parameter Name="prmComp" Type="String" />
                   <asp:Parameter Name="prmUser" Type="String" />
                   <asp:Parameter Name="prmvline" Type="Int32" />
                   <asp:Parameter Name="prmdscr" Type="String" />
                   <asp:Parameter  Name="prmactnum"  Type="String" />
                   <asp:Parameter Name="prmqty" Type="Decimal" />
                   <asp:Parameter Name="prmuntprice" Type="Decimal" />
                   <asp:Parameter Name="prmamt" Type="Decimal" />
                    <asp:SessionParameter SessionField="view_disbur_reckey" Name="prmreckey" Type="String" />
                   <asp:SessionParameter SessionField="disbur_list_reckey_rec"  Name="prmchkno" Type="String" />
                   
               </SelectParameters>
           </asp:ObjectDataSource>
                                  
    
           <asp:ObjectDataSource ID="ObjectDataSource2" runat="server" 
               OldValuesParameterFormatString="original_{0}" SelectMethod="SelectViewDisbursements" 
               TypeName="voucherpay">
               <SelectParameters>
                   <asp:Parameter DefaultValue="GridSelect" Name="prmAction" Type="String" />
                   <asp:Parameter Name="prmComp" Type="String" />
                   <asp:Parameter Name="prmUser" Type="String" />
                   <asp:Parameter Name="prmvline" Type="Int32" />
                   <asp:Parameter Name="prmdscr" Type="String" />
                   <asp:Parameter  Name="prmactnum"  Type="String" />
                   <asp:Parameter Name="prmqty" Type="Decimal" />
                   <asp:Parameter Name="prmuntprice" Type="Decimal" />
                   <asp:Parameter Name="prmamt" Type="Decimal" />
                   <asp:Parameter Name="prmreckey" Type="String" />
                   <asp:SessionParameter SessionField="disbur_list_reckey_rec"  Name="prmchkno" Type="String" />                  
                  
               </SelectParameters>
           </asp:ObjectDataSource>
       </div>
       
    </div>
    </td></tr></table>    
    <ft:footer id="Footer1" runat="server"></ft:footer>
    </form>
  </body>
</HTML>

