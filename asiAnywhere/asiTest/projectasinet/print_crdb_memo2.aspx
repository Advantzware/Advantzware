<%@ Page Language="c#" AutoEventWireup="true" Debug="false" Inherits="print_crdb_memo2" Codebehind="print_crdb_memo2.aspx.cs" %>

<html xmlns="http://www.w3.org/1999/xhtml" >
  <head id="Head1" runat="server">
    <title>Print Credit/Debit Memo</title>
    <LINK href="include/style.css" type="text/css" rel="stylesheet"/>    
    <script language="javascript" src="include/insert.js"></script>
    <LINK REL="stylesheet" TYPE="text/css" HREF="include/CalendarControl.css" >
    <script language = "JavaScript" src="include/CalendarControl.js"></script>
    <script language="javascript" src="include/date.js"></script>
    <script language="javascript" src="include/event.js"></script>
    
     <script language="VBScript">
    Function makeMsgBox(title,message,icon,buttons,defButton,mode)
        butVal = icon + buttons + defButton + mode
        makeMsgBox = MsgBox(message,butVal,title)
    End Function

</script>
<script language="javascript">
    
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
    
    function preLeave( fieldObj, fieldType, fieldFormat ){
    fieldObj.style.backgroundColor='Window';
    fieldObj.style.color='WindowText';
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

function focusval(obj) {
    obj.style.backgroundColor = 'blue';
    obj.style.color = 'white';
}
function blurval(obj) {
    obj.style.backgroundColor = 'Window';
    obj.style.color = 'WindowText';
}


    function vendtext() {
        var vend = document.getElementById("bevendTextBox");
        vend.focus();
    }
    function periodtext() {
        var vend = document.getElementById("postdateTextBox");
        var pertext = document.getElementById("perTextBox");
        pertext.value = (vend.value).substring(0, 2);

    }
    var vendlook = "";
    function vendorlook(var1) {
        vendlook = var1;
        var NewWindow = window.open("corvend_lookup.aspx", "VendLookup", "width=500,height=400,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
    }
    function VendLookup(ReturnObj1) {
        if(vendlook == 1)
            document.forms[0].bevendTextBox.value = ReturnObj1;
            else
                document.forms[0].endvendTextBox.value = ReturnObj1;
        }
        var cust = "";
        function contactcustomerlook(obj) {
            cust = obj;
            var NewWindow = window.open("contact_customer_lookup.aspx", "ContactCustomerLookupWindow", "width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
        }

        function ContactCustomerLookup(ReturnObj1) {
            if (cust == 1) {
                document.forms[0].becustTextBox.value = ReturnObj1;
            }
            else {
                document.forms[0].endcustTextBox.value = ReturnObj1;
            }
        }
      
   </script> 
  </head>    
   <body>
    <form id="frmList" runat="server"  defaultfocus='TextBox1'>   
        <%--<hd:header id="Header1" runat="server"></hd:header>--%>
      <div>
        <asp:HiddenField ID="HiddenField1" runat="server" />  
        <asp:HiddenField ID="HiddenField2" runat="server" />  
        <asp:HiddenField ID="HiddenField3" runat="server" /> 
         <asp:HiddenField ID="HiddenFieldPost" runat="server" />    
      <TABLE id="tblTop" cellSpacing="3" align="center" border="0" Width="100%">
        <TR>
            
          <TD align=left nowrap><font size=+0><b>Print Credit/Debit Memo&nbsp;</b></font></TD>
          
          <TD vAlign="middle" align="center" nowrap >Logged as&nbsp;
            <asp:label id="lblUser" runat="server" Font-Bold="True">&nbsp;</asp:label>&nbsp;&nbsp;&nbsp;                       
          &nbsp;<b>Company:</b>&nbsp;<asp:Label ID="labelcompany" runat="server" Text="Label"></asp:Label></TD>
          
         
          <TD vAlign="middle" width="20">&nbsp;</TD>
          
          <td width=30>&nbsp;</td>
        </TR>
      </TABLE>
       <asp:Label ID="Label1" runat="server" ForeColor="red" Font-Bold="true"></asp:Label>
       
       
      <table class="shade" >
      <tr><td><table>    
      <tr><td align="right" style="padding-right: 5px"><b>Begining Customer#:</b></td><td>
          <asp:TextBox ID="becustTextBox" MaxLength="8" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)"  width="100px" runat="server"></asp:TextBox>          
          <a href="#" tabindex="1" onClick="contactcustomerlook(1); return false"><asp:Image ID="Image1" runat="server" ImageUrl="images/lookup_icon.gif" /></a></td>
        <td align="right" style="padding-right: 5px"><b>Ending Customer#:</b></td>
        <td><asp:TextBox ID="endcustTextBox" MaxLength="8" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" width="100px" runat="server"></asp:TextBox>
         <a href="#" tabindex="1" onClick="contactcustomerlook(2); return false"><asp:Image ID="Image2" runat="server" ImageUrl="images/lookup_icon.gif" /></a>   
        </td>
      </tr>
      <tr>
         <td align="right" style="padding-right: 5px"><b>Begining Memo#:</b></td>
          <td nowrap><asp:TextBox MaxLength="15" ID="bememoTextBox" Width="100px" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)"  runat="server"></asp:TextBox>
          </td>
         <td align="right" style="padding-right: 5px"><b>Ending Memo#:</b></td>
          <td nowrap><asp:TextBox MaxLength="15" ID="endmemoTextBox" Width="100px" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)"  runat="server"></asp:TextBox>
          
          </td>
        </tr>            
      <tr>
        <td align="right" style="padding-right: 5px"><b>Begining Date:</b></td>
          <td><asp:TextBox ID="bedateTextBox" Width="100px" onfocus="this.select();javascript:preEnter( this, 'yes' );" onblur="javascript:preLeave( this, 'date', '99/99/9999' );" runat="server"></asp:TextBox>
          <a href="#" onblur="document.getElementById('bedateTextBox').focus()"  tabindex="1" onClick="showCalendarControl(bedateTextBox); return false"><asp:Image ID="Image6" runat="server" ImageUrl="images/lookup_icon.gif" /></a>          
        </td>
        <td align="right" style="padding-right: 5px"><b>Ending Date:</b></td>
          <td><asp:TextBox ID="enddateTextBox" Width="100px" onfocus="this.select();javascript:preEnter( this, 'yes' );" onblur="javascript:preLeave( this, 'date', '99/99/9999' );" runat="server"></asp:TextBox>
          <a href="#" onblur="document.getElementById('enddateTextBox').focus()"  tabindex="1" onClick="showCalendarControl(enddateTextBox); return false"><asp:Image ID="Image3" runat="server" ImageUrl="images/lookup_icon.gif" /></a>          
        </td>
       </tr>                       
        
        <tr><td nowrap align="center"></td>
        <td colspan="3"><b><asp:CheckBox ID="CheckBox1" Text="Reprint Memos? " runat="server" /></b></td></tr>
        <tr><td nowrap align="right"></td>
        <td colspan="3"><b><asp:CheckBox ID="CheckBox2" Text="Reprint Post Memos? " runat="server" /></b></td></tr>
        <tr><td nowrap align="right"></td>
        <td colspan="3"><b><asp:CheckBox ID="CheckBox3" Text="Export/FTP  Memos? " runat="server" /></b></td></tr>
        
         </table></td></tr>   
                 
          <tr><td>            
            <asp:Button ID="submitbutton" OnClick="submitbutton_click" CausesValidation="true" OnClientClick="confirmPost()" runat="server" class="buttonM" Text="Submit" />          
          <asp:Label ID="OutPutFile" runat="server" Text="Output File:" Font-Bold="True" Font-Size="Larger" ForeColor="Blue"></asp:Label>
              <asp:HyperLink ID="HyperLink1" runat="server" Target="_blank" Font-Bold="True" Font-Size="Larger" ForeColor="Red"></asp:HyperLink>
          </td></tr>
          </table>
       
          <asp:FormView ID="FormView1" Visible="False" runat="server" 
              DataSourceID="ObjectDataSource1">                                                                                                                                                             
              
             
              <ItemTemplate>
                  prcd:
                  <asp:Label ID="prcdLabel" runat="server" 
                      Text='<%# Bind("prcd") %>'></asp:Label><br />                 
                  
                  <br />
              </ItemTemplate>                            
          </asp:FormView>
          
          <asp:ObjectDataSource ID="ObjectDataSource1" runat="server" OldValuesParameterFormatString="original_{0}"
              SelectMethod="PrintCreditDebitMemo" TypeName="account">
              <SelectParameters>
                  <asp:Parameter Name="prmUser" Type="String" />
                  <asp:Parameter Name="prmAction" Type="String" />
                  <asp:Parameter Name="prmbegcust" Type="String" />
                  <asp:Parameter Name="prmbegmemo" Type="Int32" />
                  <asp:Parameter Name="prmbegdt" Type="String" />
                  <asp:Parameter Name="prmendcust" Type="String" />
                  <asp:Parameter Name="prmendmemo" Type="Int32" />
                  <asp:Parameter Name="prmenddt" Type="String" />
                  <asp:Parameter Name="prmprtmo" Type="String" />
                  <asp:Parameter Name="prmpstmo" Type="String" />
                  <asp:Parameter Name="prmexprt" Type="String" />
                  <asp:Parameter Name="prmOut" Type="String" />
                  
              </SelectParameters>
          </asp:ObjectDataSource>
     
    </div>
    
    </form>
  </body>
</HTML>


