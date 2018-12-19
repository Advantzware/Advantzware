<%@ Page Language="c#" AutoEventWireup="true" Debug="false" Inherits="gl_trans_report" Codebehind="gl_trans_report.aspx.cs" %>
<%@ Register Src="footer.ascx" TagName="Footer" TagPrefix="ft" %>
<%@ Register Src="header.ascx" TagName="Header" TagPrefix="hd" %>
<html xmlns="http://www.w3.org/1999/xhtml" >
  <head id="Head1" runat="server">
    <title>GL Transaction Report</title>
    <LINK href="include/style.css" type="text/css" rel="stylesheet"/>    
    <script language="javascript" src="include/insert.js"></script>
    <LINK REL="stylesheet" TYPE="text/css" HREF="include/CalendarControl.css" >
    <script language = "JavaScript" src="include/CalendarControl.js"></script>
    <script language="javascript" src="include/date.js"></script>
    <script language="javascript" src="include/event.js"></script>
    <script language="javascript" src="include/insert.js"></script>
     <script language="VBScript">
    Function makeMsgBox(title,message,icon,buttons,defButton,mode)
        butVal = icon + buttons + defButton + mode
        makeMsgBox = MsgBox(message,butVal,title)
    End Function

</script>
<script language="javascript">
//    function confirmPost() {
//        var retVal = makeMsgBox("Confirmation", "Are you ready to  Post Invoices?", 48, 4, 256, 4096);
//        if (retVal == 6) {
//            document.forms[0].HiddenFieldPost.value = "Yes";
//        }
//        else {
//            document.forms[0].HiddenFieldPost.value = "No";
//        }
//    }
</script>
        
    <script language = JavaScript>    
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

//    function preEnter(fieldObj, canEdit) {
//        fieldObj.style.backgroundColor = 'blue';
//        fieldObj.style.color = 'white';
//        if (canEdit == "no") {
//            fieldObj.blur();
//            leaveField(fieldObj);
//        }

//        enterField(fieldObj);
//        return;
//    }
//    
//    function preLeave( fieldObj, fieldType, fieldFormat ){
//    fieldObj.style.backgroundColor='Window';
//    fieldObj.style.color='WindowText';
//    fieldType = fieldType.toLowerCase();
//    if ((fieldType == "") || (fieldType == "text")) {
//        leaveField(fieldObj);
//    }
}

function focusval(obj) {
    obj.style.backgroundColor = 'blue';
    obj.style.color = 'white';
}
function blurval(obj) {
    obj.style.backgroundColor = 'Window';
    obj.style.color = 'WindowText';
}
var act;
function AccountLook(obj1) {
    act = obj1;
    var NewWindow = window.open("accountlook.aspx?jrnl=" + "journal" + " ", "AccountLookupWindow", "width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}

function AccountLookup(ReturnObj1) {
    if (act == "1") {
        document.forms[0].beactTextBox.value = ReturnObj1;
    }
    else {
        document.forms[0].endactTextBox.value = ReturnObj1;
    }   

}

    
    
      
   </script> 
  </head>    
   <body>
    <form id="frmList" runat="server"  defaultfocus='TextBox1'>   
      <hd:header id="Header1" runat="server"></hd:header>  
      <div>
        <asp:HiddenField ID="HiddenField1" runat="server" />  
        <asp:HiddenField ID="HiddenField2" runat="server" />
        <asp:HiddenField ID="HiddenField3" runat="server" />
        <asp:HiddenField ID="HiddenField4" runat="server" />
        <asp:HiddenField ID="HiddenField5" runat="server" /> 
        <asp:HiddenField ID="HiddenField6" runat="server" />
        <asp:HiddenField ID="HiddenField7" runat="server" />  
        <asp:HiddenField ID="HiddenField8" runat="server" />
        <asp:HiddenField ID="HiddenField9" runat="server" />
        <asp:HiddenField ID="HiddenField10" runat="server" />
        <asp:HiddenField ID="HiddenField11" runat="server" /> 
        <asp:HiddenField ID="HiddenField12" runat="server" />
        <asp:HiddenField ID="HiddenField13" runat="server" />  
        <asp:HiddenField ID="HiddenField14" runat="server" />
        <asp:HiddenField ID="HiddenField15" runat="server" />
        <asp:HiddenField ID="HiddenField16" runat="server" />
        <asp:HiddenField ID="HiddenField17" runat="server" /> 
        <asp:HiddenField ID="HiddenField18" runat="server" />
        <asp:HiddenField ID="HiddenField19" runat="server" />
         <asp:HiddenField ID="HiddenFieldPost" runat="server" /> 
            
      <asp:Label ID="Label1" runat="server" ForeColor="red" Font-Bold="true"></asp:Label>
          
      <TABLE id="tblTop" cellSpacing="3" align="center" border="0" Width="100%">
        <TR>
            
          <TD align=center nowrap><font size=+0><b>GL Transaction Report &nbsp;</b></font></TD>
          <TD vAlign="middle" nowrap >
            <asp:linkbutton id="hlkBackToMenu" OnClick="Back_tomenu_Click" runat="server" >Back to menu</asp:linkbutton>
          </TD>
          <TD vAlign="middle" align="center" nowrap >Logged as&nbsp;
            <asp:label id="lblUser" runat="server" Font-Bold="True">&nbsp;</asp:label>&nbsp;&nbsp;&nbsp;
            <asp:linkbutton id="hlnkLogOut" runat="server" OnClick="hlnkLogOut_Click">Log out</asp:linkbutton>
            &nbsp;&nbsp;<asp:hyperlink id="hlnkChangePwd" runat="server" NavigateUrl="changepwd.aspx"></asp:hyperlink>
          &nbsp;<b>Company:</b>&nbsp;<asp:Label ID="labelcompany" runat="server" Text="Label"></asp:Label></TD>
          
         
          <TD vAlign="middle" width="20">&nbsp;</TD>
          
          <td width=30>&nbsp;</td>
        </TR>
      </TABLE>
       
      <fieldset class="shade" style="width:500px">
      <table class="shade">
      <tr><td><table>
      <tr>
         <td align="right" style="padding-right: 5px"><b>Begining Date:</b></td>
          <td nowrap><asp:TextBox ID="bedtTextBox" onfocus="javascript:preEnter( this, 'yes' );" onblur="javascript:preLeave( this, 'date', '99/99/9999' );" Width="100px" runat="server"></asp:TextBox>          
          <a href="#" onblur="document.getElementById('bedtTextBox').focus()" tabindex="1" onClick="showCalendarControl(bedtTextBox); return false"><asp:Image ID="Image1" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
          </td>
         <td align="right" style="padding-right: 5px"><b>Ending Date:</b></td>
          <td nowrap><asp:TextBox ID="enddtTextBox" onfocus="javascript:preEnter( this, 'yes' );" onblur="javascript:preLeave( this, 'date', '99/99/9999' );" Width="100px" runat="server"></asp:TextBox>          
          <a href="#" onblur="document.getElementById('enddtTextBox').focus()" tabindex="1" onClick="showCalendarControl(enddtTextBox); return false"><asp:Image ID="Image2" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
          </td>
        </tr>       
      <tr><td align="right" style="padding-right: 5px"><b>Begining Acct#:</b></td><td>
          <asp:TextBox ID="beactTextBox" width="100px" runat="server"></asp:TextBox>
          <a href="#" tabindex="1" onClick="AccountLook(1); return false"><asp:Image ID="Image13" runat="server" ImageUrl="images/lookup_icon.gif" /></a>          
          </td>
        <td align="right" style="padding-right: 5px"><b>Ending Acct#:</b></td>
        <td><asp:TextBox ID="endactTextBox" width="100px" runat="server"></asp:TextBox>
        <a href="#" tabindex="1" onClick="AccountLook(2); return false"><asp:Image ID="Image3" runat="server" ImageUrl="images/lookup_icon.gif" /></a>            
        </td>
      </tr>         
        </table></td></tr>        
        
        <tr></tr><tr></tr>
        <tr><td><table>
            <tr><td><b><asp:CheckBox ID="CheckBox1" Text="Cash Receipts " runat="server" /></b>
            </td>
            <td><b><asp:CheckBox ID="CheckBox2" Text="Voided Cash Receipts " runat="server" /></b>
            </td></tr>                         
            <tr><td><b><asp:CheckBox ID="CheckBox3" Text="General Journal Entries" runat="server" /></b>
            </td>
            <td><b><asp:CheckBox ID="CheckBox4" Text="Accounts Payable Check Register " runat="server" /></b>
            </td></tr>            
            <tr><td><b><asp:CheckBox ID="CheckBox5" Text="Misc Cash Receipts " runat="server" /></b>
            </td>
            <td><b><asp:CheckBox ID="CheckBox13" Text="Accounts Receivable Invoice " runat="server" /></b><br />
            </td></tr>
            <tr><td><b><asp:CheckBox ID="CheckBox6" Text="Accounts Payable Memo " runat="server" /></b>
            </td>
            <td><b><asp:CheckBox ID="CheckBox14" Text="Cash Disbursements " runat="server" /></b><br />
            </td></tr>
            <tr><td><b><asp:CheckBox ID="CheckBox7" Text="Accounts Payable " runat="server" /></b>
            </td>
            <td><b><asp:CheckBox ID="CheckBox15" Text="Accounts Receivable Memo " runat="server" /></b><br />
            </td></tr>
            <tr><td><b><asp:CheckBox ID="CheckBox8" Text="Accounts Payable Purchases " runat="server" /></b>
            </td>
            <td><b><asp:CheckBox ID="CheckBox17" Text="Accounts Payable Void Check " runat="server" /></b><br />
            </td></tr>
            <tr><td><b><asp:CheckBox ID="CheckBox9" Text="FG Adjustments " runat="server" /></b>
            </td>
            <td><b><asp:CheckBox ID="CheckBox16" Text="Order Entry Invoice " runat="server" /></b><br />
            </td></tr>
            <tr><td><b><asp:CheckBox ID="CheckBox10" Text="Finished Goods Posting " runat="server" /></b>
            </td>
            <td><b><asp:CheckBox ID="CheckBox18" Text="Job Cost Posting Register " runat="server" /></b><br />
            </td></tr>
            <tr><td><b><asp:CheckBox ID="CheckBox11" Text="Automatic Distributions " runat="server" /></b>
            </td>
            <td><b><asp:CheckBox ID="CheckBox19" Text="Raw Materials Posting " runat="server" /></b><br />
            </td></tr>
            <tr><td><b><asp:CheckBox ID="CheckBox12" Text="Voided Checks " runat="server" /></b><br />
            </td></tr>
            
         </table></td></tr>
         
         <tr><td colspan="3"><b> &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; 
        &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; <b>OutPut To?:</b> <asp:RadioButtonList ID="RadioButtonList3" RepeatLayout="Flow"  CellSpacing="1" RepeatColumns="5" Font-Bold ="true" runat="server">
             
                 
                 <asp:ListItem   Value="no"   Text="Text File" />
                  <asp:ListItem  Value="yes"   Text="Excel" />
                 
         </asp:RadioButtonList></b></td></tr>
                 
          <tr><td colspan="3">
            &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
            <asp:Button ID="submitbutton" OnClick="submitbutton_click" CausesValidation="true" OnClientClick="confirmPost()" runat="server" class="buttonM" Text="Submit" />
          &nbsp;&nbsp;&nbsp;&nbsp;
          <asp:Label ID="OutPutFile" runat="server" Text="Output File:" Font-Bold="True" Font-Size="Larger" ForeColor="Blue"></asp:Label>
              <asp:HyperLink ID="HyperLink1" runat="server" Target="_blank" Font-Bold="True" Font-Size="Larger" ForeColor="Red"></asp:HyperLink>
          </td></tr>
          </table>
       </fieldset>
          <asp:FormView ID="FormView1" Visible="False" runat="server" 
              DataSourceID="ObjectDataSource1">               
             
              <ItemTemplate>
                  trnas:
                  <asp:Label ID="trnasLabel" runat="server" 
                      Text='<%# Bind("trnas") %>'></asp:Label>
                      
                      post:
                  <asp:Label ID="postLabel" runat="server" 
                      Text='<%# Bind("post") %>'></asp:Label><br />                 
                  
                  
                  
              </ItemTemplate>                            
          </asp:FormView>
          
          <asp:ObjectDataSource ID="ObjectDataSource1" runat="server" OldValuesParameterFormatString="original_{0}"
              SelectMethod="TransactionReport" TypeName="ledger">
              <SelectParameters>
                  <asp:Parameter Name="prmUser" Type="String" />
                  <asp:Parameter Name="prmAction" Type="String" />
                  <asp:Parameter Name="prmbegdate" Type="String" />
                  <asp:Parameter Name="prmenddate" Type="String" />
                  <asp:Parameter Name="prmbegact" Type="String" />
                  <asp:Parameter Name="prmendact" Type="String" />
                  <asp:Parameter Name="prmtb_acpay" Type="String" />
                  <asp:Parameter Name="prmtb_adjust" Type="String" />
                  <asp:Parameter Name="prmtb_appurch" Type="String" />
                  <asp:Parameter Name="prmtb_apckr" Type="String" />
                  <asp:Parameter Name="prmtb_apmem" Type="String" />
                  <asp:Parameter Name="prmtb_apvoidck" Type="String" />
                  <asp:Parameter Name="prmtb_arinv" Type="String" />
                  <asp:Parameter Name="prmtb_autodist" Type="String" />
                  <asp:Parameter Name="prmtb_cashr" Type="String" />
                  <asp:Parameter Name="prmtb_cashrvd" Type="String" />
                  <asp:Parameter Name="prmtb_cdisb" Type="String" />
                  <asp:Parameter Name="prmtb_crmem" Type="String" /> 
                  <asp:Parameter Name="prmtb_fgpost" Type="String" />
                  <asp:Parameter Name="prmtb_general" Type="String" />
                  <asp:Parameter Name="prmtb_jcost" Type="String" /> 
                  <asp:Parameter Name="prmtb_mcshrec" Type="String" />
                  <asp:Parameter Name="prmtb_oeinv" Type="String" />
                  <asp:Parameter Name="prmtb_rmpost" Type="String" /> 
                  <asp:Parameter Name="prmtb_void_checks" Type="String" />                
                  <asp:Parameter Name="prmOut" Type="String" />                
                  
                  
              </SelectParameters>
          </asp:ObjectDataSource>
     
    </div>
    <ft:footer id="Footer1" runat="server"></ft:footer>
    </form>
  </body>
</HTML>


