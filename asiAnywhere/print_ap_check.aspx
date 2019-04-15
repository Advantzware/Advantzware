<%@ Page Language="c#" AutoEventWireup="true" Debug="false" Inherits="print_ap_check" Codebehind="print_ap_check.aspx.cs" %>
<%@ Register Src="footer.ascx" TagName="Footer" TagPrefix="ft" %>
<%@ Register Src="header.ascx" TagName="Header" TagPrefix="hd" %>
<html xmlns="http://www.w3.org/1999/xhtml" >
  <head id="Head1" runat="server">
    <title>Print A/P Checks</title>
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
    function confirmPost() {
        var retVal = makeMsgBox("Confirmation", "Post Debit/Credit Memos?", 48, 4, 256, 4096);
        if (retVal == 6) {
            document.forms[0].HiddenFieldPost.value = "Yes";
        }
        else {
            document.forms[0].HiddenFieldPost.value = "No";
        }
    }
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

   
    function periodtext() {
        var vend = document.getElementById("postdateTextBox");
        var pertext = document.getElementById("perTextBox");
        pertext.value = (vend.value).substring(0, 2);

    }

    function banklookup() {

        var NewWindow = window.open("bank_lookup.aspx", "banklookup", "width=500,height=400,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
    }
    function banklook(ReturnObj1, ReturnObj2) {
        document.forms[0].bnkcodeTextBox.value = ReturnObj1;


    }
    var vend = "" ;
    function vendorlook(var1) { 
    vend = var1;       
        var NewWindow = window.open("corvend_lookup.aspx", "vendorLookup", "width=500,height=400,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
    }
    function VendLookup(ReturnObj1) {
    if (vend == 1) {
        document.forms[0].begvendTextBox.value = ReturnObj1;
        }
        else
        document.forms[0].endvendTextBox.value = ReturnObj1;
        
    }
    
      
   </script> 
  </head>    
   <body>
    <form id="frmList" runat="server"  defaultfocus='chkdateTextBox'>   
        <hd:header id="Header1" runat="server"></hd:header>
      <div>
        <asp:HiddenField ID="HiddenField1" runat="server" />  
        <asp:HiddenField ID="HiddenField2" runat="server" />  
         <asp:HiddenField ID="HiddenFieldPost" runat="server" />    
      <TABLE id="tblTop" cellSpacing="3" align="center" border="0" Width="100%">
        <TR>
            
          <TD align=left nowrap><font size=+0><b>Print A/P Checks&nbsp;</b></font></TD>
          <TD vAlign="middle" nowrap >
            <asp:linkbutton id="hlkBackToMenu" OnClick="Back_tomenu_Click" runat="server" >Back to menu</asp:linkbutton>
          </TD>
          <TD vAlign="middle" align="center" nowrap >Logged as&nbsp;
            <asp:label id="lblUser" runat="server" Font-Bold="True">&nbsp;</asp:label>&nbsp;&nbsp;&nbsp;
            <asp:linkbutton id="hlnkLogOut" runat="server" OnClick="hlnkLogOut_Click">Log out</asp:linkbutton>            
          &nbsp;<b>Company:</b>&nbsp;<asp:Label ID="labelcompany" runat="server" Text="Label"></asp:Label></TD>
          
         
          <%--<TD width="20">&nbsp;</TD>
          
          <td width=30>&nbsp;</td>--%>
        </TR>
      </TABLE>
       <asp:Label ID="Label1" runat="server" ForeColor="red" Font-Bold="true"></asp:Label>
       
       
      <table class="shade" width="400px" >
      
      <tr><td><table>
      <tr>      
        <td nowrap align="right" style="padding-right:5px;"><b>Check Date:</b></td>           
            <td><asp:TextBox ID="chkdateTextBox" onfocus="javascript:preEnter( this, 'yes' );" onblur="javascript:preLeave( this, 'date', '99/99/9999' );"  runat="server" Width="100px"></asp:TextBox>
            <a href="#" onblur="document.getElementById('chkdateTextBox').focus()"  tabindex="1" onClick="showCalendarControl(chkdateTextBox); return false"><asp:Image ID="Image5" runat="server" ImageUrl="images/lookup_icon.gif" /></a>          
         </td>      
      </tr>   
      <tr>        
        <td nowrap align="right" style="padding-right:5px;"><b>Bank code:</b></td>            
            <td><asp:TextBox ID="bnkcodeTextBox"  onfocus="focusval(this)" onblur="blurval(this)" runat="server" Width="100px"></asp:TextBox>            
            <a href="#" tabindex="1" onclick="banklookup(); return false"><asp:Image ID="banklookimg" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
         </td>      
      </tr>   
      <tr>        
        <td nowrap align="right" style="padding-right:5px;"><b>Starting Check#:</b></td>           
            <td><asp:TextBox ID="strtchkTextBox" onfocus="focusval(this)" onblur="blurval(this)" runat="server" Width="100px"></asp:TextBox>  
                <asp:CompareValidator ID="CompareValidator1" ControlToValidate="strtchkTextBox" Operator="DataTypeCheck" Display="Dynamic" SetFocusOnError="true" Type="Integer" runat="server" ErrorMessage="Enter Invalid Value"></asp:CompareValidator>         
         </td>      
      </tr>  
      </table></td></tr>
      <tr></tr><tr></tr><tr></tr><tr></tr>
      
      
       <tr><td style="width:300px;" align="right" >      
      <fieldset style="width:260px;">
      <legend> REPRINT OPTIONS:</legend>
      <table><tr><td></td>
      <td colspan="2" align="right" style="padding-right: 5px"><b>Begining Vendor#:</b></td>
          <td><asp:TextBox MaxLength="8" ID="begvendTextBox" onfocus="focusval(this)" onblur="blurval(this)" Width="100px" runat="server"></asp:TextBox>        
          <a href="#" tabindex="1" onclick="vendorlook(1); return false"><asp:Image ID="Image4" runat="server" ImageUrl="images/lookup_icon.gif" /></a>   
        </td></tr>
        <tr><td></td><td colspan="2" align="right" style="padding-right: 5px"><b>Ending Vendor#:</b></td>
          <td><asp:TextBox MaxLength="8" ID="endvendTextBox" onfocus="focusval(this)" onblur="blurval(this)" Width="100px" runat="server"></asp:TextBox>          
          <a href="#" tabindex="1" onclick="vendorlook(2); return false"><asp:Image ID="Image1" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
        </td></tr></table>
        </fieldset>
        </td></tr>
      
       
               
        
        <tr></tr><tr></tr>
                         
          <tr><td colspan="3">
            &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
            <asp:Button ID="submitbutton" OnClick="submitbutton_click" CausesValidation="true" runat="server" class="buttonM" Text="Submit" />
          &nbsp;&nbsp;&nbsp;&nbsp;
          <asp:Label ID="OutPutFile" runat="server" Text="Output File:" Font-Bold="True" Font-Size="Larger" ForeColor="Blue"></asp:Label>
              <asp:HyperLink ID="HyperLink1" runat="server" Target="_blank" Font-Bold="True" Font-Size="Larger" ForeColor="Red"></asp:HyperLink>
          </td></tr>
          </table>
       
          <asp:FormView ID="FormView1" Visible="false"  runat="server" 
              DataSourceID="ObjectDataSource1">                                                                                                                                                             
              
             
              
             
              <ItemTemplate>
                  apchek:
                  <asp:Label ID="apchekLabel" runat="server" 
                      Text='<%# Bind("apchek") %>'></asp:Label><br />                 
                  
              </ItemTemplate>                            
          </asp:FormView>
          
          <asp:ObjectDataSource ID="ObjectDataSource1" runat="server" OldValuesParameterFormatString="original_{0}"
              SelectMethod="SelectPrintAPChecks" TypeName="voucherpay">
              <SelectParameters>
                  <asp:Parameter Name="prmUser"   Type="String" />
                  <asp:Parameter Name="prmapchek"  Type="String" />
                  <asp:Parameter Name="prmChekdate" Type="String" />
                  <asp:Parameter Name="prmbnkcode" Type="String" />
                  <asp:Parameter Name="prmbnkname" Type="String" />
                  <asp:Parameter Name="prmstrtchek" Type="Int32" />
                  <asp:Parameter Name="prmBegVend" Type="String" />
                  <asp:Parameter Name="prmEndVend" Type="String" />
                  <asp:Parameter Name="prmOut" Type="String" />
                  
              </SelectParameters>
          </asp:ObjectDataSource>
     
    </div>
    <ft:footer id="Footer1" runat="server"></ft:footer>
    </form>
  </body>
</HTML>


