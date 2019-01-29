<%@ Page Language="c#" AutoEventWireup="true" Debug="false" Inherits="void_cashrcpt" Codebehind="void_cashrcpt.aspx.cs" %>
<%@ Register Src="footer.ascx" TagName="Footer" TagPrefix="ft" %>
<%@ Register Src="header.ascx" TagName="Header" TagPrefix="hd" %>
<html xmlns="http://www.w3.org/1999/xhtml" >
  <head id="Head1" runat="server">
    <title>Void Cash Receipts</title>
    <LINK href="include/style.css" type="text/css" rel="stylesheet"/>
    <LINK REL="stylesheet" TYPE="text/css" HREF="include/CalendarControl.css" >
    <link href="include/tree.css" rel="stylesheet" type="text/css" />
    <link href="include/dhtmlwindow.css" rel="stylesheet" type="text/css" />
    <script language="javascript" src="include/date.js"></script>
    <script language="javascript" src="include/event.js"></script>
    <script language="javascript" src="include/insert.js"></script>
    <script language = "JavaScript" src="include/CalendarControl.js"></script>
    <script language ="javascript">
    
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
    
     function contactcustomerlook(){ 
  var NewWindow = window.open("contact_customer_lookup.aspx","ContactCustomerLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}

function ContactCustomerLookup(ReturnObj1,ReturnObj2,ReturnObj3,ReturnObj4,ReturnObj5,ReturnObj6,ReturnObj7,ReturnObj8,ReturnObj9, ReturnObj10,ReturnObj11){ 
  document.forms[0].cust_TextBox.value = ReturnObj1;
  }
  
function vendorlook() {
     
    var NewWindow = window.open("corvend_lookup.aspx", "VendLookup", "width=500,height=400,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}
function VendLookup(ReturnObj1, ReturnObj2) {
    document.forms[0].vendor_TextBox.value = ReturnObj1;
    document.forms[0].vendname_TextBox.value = ReturnObj2;
}

function voidfocus() {
    var da = document.getElementById("DropDownList1");
    da.focus();
}
function custfocus() {
    var cst = document.getElementById("cust_TextBox");
    cst.focus();
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
    var NewWindow = window.open("top_list_notes.aspx", "OrderListNotes", "width=600,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}
function banklookup() {

    var NewWindow = window.open("bank_lookup.aspx", "banklookup", "width=500,height=400,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}
function banklook(ReturnObj1, ReturnObj2) {
    document.forms[0].bankcode_TextBox.value = ReturnObj1;
}
function topattach() {
    var NewWindow = window.open("top_attach_invlist.aspx", "Attachment", "width=650,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}
function contactcustomerlook() {
    var NewWindow = window.open("contact_customer_lookup.aspx", "ContactCustomerLookupWindow", "width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}
function printrep() {
    var NewWindow = window.open("topprintcust_report.aspx", "OrderReport", "width=600,height=600,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}
function ContactCustomerLookup(ReturnObj1, ReturnObj2) {
    document.forms[0].cust_TextBox.value = ReturnObj1;
    document.forms[0].custname_TextBox.value = ReturnObj2;   
}


    </script> 
  </head>    
   <body>
    <form id="frmList" runat="server"  defaultfocus='vendor_TextBox'>   
        <hd:header id="Header1" runat="server"></hd:header>
      
       <table width="100%"><tr><td><div>
        <table align="left" border="1" width="75%">
                <tr class="topheadcolor">
                        <td nowrap width="25px";>
                        <a href="#" onClick="topattach(); return false"><asp:Image ID="Image3" Width="35px" ToolTip="Attachment" runat="server" ImageUrl="~/Images/clip.ico" /></a>                        
                        </td>
                        <td nowrap width="25px";>
                        <a href="#" onClick="printrep(); return false"><asp:Image ID="Image1" Width="35px" runat="server" ImageUrl="~/Images/print-u.bmp" /></a>
                        </td> 
                        <td nowrap width="25px";>
                        <a href="#" onClick="ordernotes(); return false"><asp:Image ID="img_btn_notes" Width="35px" ToolTip="Notes" runat="server" ImageUrl="~/Images/edit.ico" /></a>                        
                        </td>                        
                        <td nowrap width="25px";>
                        <asp:ImageButton ID="img_btn_exit" runat="server" Width="35px" ImageUrl="~/Images/exit-au.bmp" ToolTip="LogOut" OnClick="img_btn_exit_click" />
                        </td>
                        <td nowrap> &nbsp;</td>
                </tr>
      </table></div>
          </td>
      </tr>
      <tr>
      <td>
      <div>
          <asp:ScriptManager ID="ScriptManager1" runat="server">
          </asp:ScriptManager>
          <asp:HiddenField ID="HiddenField1" runat="server" />
      <TABLE id="tblTop" cellSpacing="3" align="center" border="0" Width="100%">
        <TR>         
          <TD align=left nowrap><font size=+0><b>Void Cash Receipts  &nbsp;</b></font></TD>
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
     
      
       <asp:Panel ID="searchpanel" runat="server" DefaultButton="btnSearch">    
      <TABLE id="tblMain" cellSpacing="1" cellPadding="1" width='650px' border="0">
        <TR>
          <TD class="shade">
            <TABLE id="tblSearch"  width="100%">
              <TR >
                <%--<TD class="shade"  width="50" class="shade" bgcolor="gray"><nobr>                  
                <asp:button id="btnSearch" runat="server" OnClick=" btnSearch_Click" Width="40px" CssClass="button" Text="Update" ></asp:button>                
                </TD>--%> 
                <td><table>
                <tr>
                <td nowrap><b>Check#: </b>
                <asp:TextBox ID="chk_TextBox" Width="80px" MaxLength="12" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" runat="server"></asp:TextBox></td>
                <td nowrap>
                <b>Customer#:</b>
                    <asp:TextBox ID="cust_TextBox" Width="80px" MaxLength="8" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" runat="server"></asp:TextBox>
                    <a href="#" tabindex="1" onClick="contactcustomerlook(); return false"><asp:Image ID="Image2" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
                    <asp:TextBox ID="custname_TextBox" Width="170px" ForeColor="#ACA899" onfocus="custfocus()" runat="server"></asp:TextBox>
                    
                </td>                
                </tr>
                <tr><td><br /> <asp:button id="btnSearch" runat="server" OnClick=" btnSearch_Click" Width="60px" CssClass="button" Text="Update" ></asp:button></td></tr>
                </table></td>              
                 
                <TD id="tdPageCount" align="left" runat="server" class="shade" >
          <%--<table><tr><td style="width:250px;" align="left">
           <b> Records/Page</b>
            <asp:FormView ID="FormView1" runat="server" DataSourceID="ObjectDataSource2">
                          <ItemTemplate>                             
                              <asp:TextBox ID="aLineLabel" runat="server" Width="40px" OnTextChanged="ddl_display_TextChanged" Text='<%# Bind("aLine") %>'></asp:TextBox>
                               <asp:CompareValidator ID="CompareValidator4" runat="server" ControlToValidate="aLineLabel" SetFocusOnError="true" Display="dynamic" Operator="dataTypeCheck" Type="Integer" ErrorMessage="Invalid Number"></asp:CompareValidator>
                          </ItemTemplate>
                      </asp:FormView>
                      <asp:ObjectDataSource ID="ObjectDataSource2" runat="server" OldValuesParameterFormatString="original_{0}"
                          SelectMethod="SelectRows" TypeName="Order">
                          <SelectParameters>
                              <asp:SessionParameter Name="prmUser" SessionField="Rowuser" Type="String" />
                              <asp:SessionParameter Name="vLine" Type="Int32" SessionField="gridsize" />
                          </SelectParameters>
                      </asp:ObjectDataSource>
          </td></tr></table> --%> 
                </TD>
              </TR>
            </TABLE>
            <asp:label id="lblMessage" runat="server" ForeColor="Red"></asp:label>
          </td>
        </tr>
       
        </TABLE></asp:Panel>
                      
          
          </div></td></tr></table>
          <fieldset style="width:650px" class="shade"><table class="shade">
          <tr><td><table>
            <tr><td align="right" style="padding-right:5px">Check No:</td>            
            <td><asp:TextBox ID="chknoTextBox" runat="server" ReadOnly="true" onfocus="voidfocus()" /></td>
            <td align="right" style="padding-right:5px">Check Amount:</td>
            <td><asp:TextBox ID="amtTextBox" runat="server" ReadOnly="true" onfocus="voidfocus()" /></td>
            <td align="right" style="padding-right:5px">Check Date:</td>
            <td><asp:TextBox ID="custdtTextBox" runat="server" ReadOnly="true" onfocus="voidfocus()"  /></td></tr>
            <tr align="right" style="padding-right:5px"><td>Customer:</td>
            <td><asp:TextBox ID="custnoTextBox" runat="server" ReadOnly="true" onfocus="voidfocus()"  /></td>
            <td align="left" colspan="3"><asp:Label ID="custnameTextBox" runat="server" Width="200px"  onfocus="voidfocus()" /></td></tr>
            <tr align="right" style="padding-right:5px"><td>Bank Code:</td>
            <td><asp:TextBox ID="bnkTextBox" ReadOnly="true" runat="server"  /></td>
            <td align="left"><asp:Label ID="bnknamTextBox"  runat="server" onfocus="voidfocus()"   /></td>
            <td align="right" style="padding-right:5px">Voided?:</td>
            <%--<asp:TextBox ID="voidedTextBox" Width="30" Text="Voided?:" runat="server"  /></td>--%>
            <td><asp:DropDownList ID="DropDownList1" Width="60px" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)"  runat="server">  
                        <asp:ListItem Text="Yes" Value="Yes"></asp:ListItem>
                        <asp:ListItem Text="No" Value="No"></asp:ListItem>
                        </asp:DropDownList></td></tr>
            
            </table> </td></tr> 
            <tr><td align="right"><table>          
            <asp:TextBox ID="reckeyTextBox" Visible="false" runat="server"  />
            
            <asp:Button ID="UpdateButton" runat="server" CausesValidation="True" CssClass="button" OnClick="btnSave_Click"  Text="Save" />
                   &nbsp;<asp:Button ID="UpdateCancelButton" OnClick="btnCancel_Click" runat="server" CssClass="button"
                       CausesValidation="False" Text="Cancel" />
               </table></td></tr>
               </table></fieldset>
          
    
    
      
      <asp:ObjectDataSource ID="ObjectDataSource3" runat="server" 
                OldValuesParameterFormatString="original_{0}" SelectMethod="VoidCashReceiptList" 
                TypeName="account">
                <SelectParameters>
                   <asp:Parameter Name="prmAction" DefaultValue="View" Type="String" />                    
                   <asp:Parameter Name="prmComp"  Type="string" />
                   <asp:Parameter Name="prmUser" Type="string" />                     
                   <asp:Parameter Name="prmchkno" Type="Int32" />                    
                   <asp:Parameter Name="prmcustno" Type="String" />
                   <asp:Parameter Name="prmcustname" Type="String" /> 
                   <asp:Parameter Name="prmcustdt" Type="String" /> 
                   <asp:Parameter Name="prmbnk" Type="String" /> 
                   <asp:Parameter Name="prmbnknam" Type="String" /> 
                    <asp:Parameter Name="prmvoided" Type="String" />
                   <asp:Parameter Name="prmamt" Type="Decimal" />                   
                    <asp:Parameter Name="prmout" Type="String" />
                   <asp:Parameter Name="prmReckey" Type="String" />
                   
                </SelectParameters>
            </asp:ObjectDataSource>
     
    <ft:footer id="Footer1" runat="server"></ft:footer>
    </form>
  </body>
</HTML>

