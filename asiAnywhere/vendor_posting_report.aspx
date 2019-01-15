<%@ Page Language="c#" AutoEventWireup="true" Debug="false" Inherits="vendor_posting_report" Codebehind="vendor_posting_report.aspx.cs" %>
<%@ Register Src="footer.ascx" TagName="Footer" TagPrefix="ft" %>
<%@ Register Src="header.ascx" TagName="Header" TagPrefix="hd" %>
<html xmlns="http://www.w3.org/1999/xhtml" >
  <head id="Head1" runat="server">
    <title>Vendor Invoice Edit/Post Register</title>
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
        var retVal = makeMsgBox("Confirmation", "Are you ready to  Post Invoices?", 48, 4, 256, 4096);
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
      
   </script> 
  </head>    
   <body>
    <form id="frmList" runat="server"  defaultfocus='TextBox1'>   
        <hd:header id="Header1" runat="server"></hd:header>
      <div>
        <asp:HiddenField ID="HiddenField1" runat="server" />  
        <asp:HiddenField ID="HiddenField2" runat="server" />  
         <asp:HiddenField ID="HiddenFieldPost" runat="server" />    
      <TABLE id="tblTop" cellSpacing="3" align="center" border="0" Width="100%">
        <TR>
            
          <TD align=left nowrap><font size=+0><b>Vendor Invoice Edit/Post Register&nbsp;</b></font></TD>
          <TD vAlign="middle" nowrap >
            <asp:linkbutton id="hlkBackToMenu" OnClick="Back_tomenu_Click" runat="server" >Back to menu</asp:linkbutton>
          </TD>
          <TD vAlign="middle" align="center" nowrap >Logged as&nbsp;
            <asp:label id="lblUser" runat="server" Font-Bold="True">&nbsp;</asp:label>&nbsp;&nbsp;&nbsp;
            <asp:linkbutton id="hlnkLogOut" runat="server" OnClick="hlnkLogOut_Click">Log out</asp:linkbutton>            
          &nbsp;<b>Company:</b>&nbsp;<asp:Label ID="labelcompany" runat="server" Text="Label"></asp:Label></TD>
          
         
          <TD vAlign="middle" width="20">&nbsp;</TD>
          
          <td width=30>&nbsp;</td>
        </TR>
      </TABLE>
       <asp:Label ID="Label1" runat="server" ForeColor="red" Font-Bold="true"></asp:Label>
       
       
      <table class="shade" >
      <tr>
        <td >&nbsp;</td>
        <td colspan="3" align="left" style="padding-right: 5px; " nowrap>&nbsp;&nbsp;&nbsp;&nbsp;<b>Post Date:</b>
           &nbsp;
            <asp:TextBox ID="postdateTextBox" onfocus="this.select();javascript:preEnter( this, 'yes' );" onblur="javascript:preLeave( this, 'date', '99/99/9999' );periodtext()"  runat="server" Width="100px"></asp:TextBox>
            <a href="#" onblur="document.getElementById('postdateTextBox').focus()"  tabindex="1" onClick="showCalendarControl(postdateTextBox); return false"><asp:Image ID="Image5" runat="server" ImageUrl="images/lookup_icon.gif" /></a>          
         </td>      
      </tr>   
      <tr>
        <td >&nbsp;</td>
        <td colspan="3" align="left" style="padding-right: 5px; " nowrap>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<b>Period:</b>
            &nbsp;
            <asp:TextBox ID="perTextBox" onfocus="vendtext()" BackColor="Turquoise"  runat="server" Width="100px"></asp:TextBox>
            
         </td>      
      </tr>       
      <tr><td align="right" style="padding-right: 5px"><b>Begining Vendor#:</b></td><td>
          <asp:TextBox ID="bevendTextBox" MaxLength="8" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)"  width="100px" runat="server"></asp:TextBox>
          <a href="#" tabindex="1" onClick="vendorlook(1); return false"><asp:Image ID="CustomerLook" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
          </td>
        <td align="right" style="padding-right: 5px"><b>Ending Vendor#:</b></td><td><asp:TextBox ID="endvendTextBox" MaxLength="8" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" width="100px" runat="server"></asp:TextBox>
            <a href="#" tabindex="1" onClick="vendorlook(2); return false"><asp:Image ID="Image2" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
        </td>
      </tr>           
      <tr>
        <td align="right" style="padding-right: 5px"><b>Begining Invoice Date:</b></td>
          <td><asp:TextBox MaxLength="5" ID="beinvTextBox" Width="100px" onfocus="this.select();javascript:preEnter( this, 'yes' );" onblur="javascript:preLeave( this, 'date', '99/99/9999' );" runat="server"></asp:TextBox>
          <a href="#" onblur="document.getElementById('beinvTextBox').focus()"  tabindex="1" onClick="showCalendarControl(beinvTextBox); return false"><asp:Image ID="Image6" runat="server" ImageUrl="images/lookup_icon.gif" /></a>          
        </td>
        <td align="right" style="padding-right: 5px"><b>Ending Invoice Date:</b></td>
          <td><asp:TextBox MaxLength="5" ID="endinvTextBox" Width="100px" onfocus="this.select();javascript:preEnter( this, 'yes' );" onblur="javascript:preLeave( this, 'date', '99/99/9999' );" runat="server"></asp:TextBox>
          <a href="#" onblur="document.getElementById('endinvTextBox').focus()"  tabindex="1" onClick="showCalendarControl(endinvTextBox); return false"><asp:Image ID="Image3" runat="server" ImageUrl="images/lookup_icon.gif" /></a>          
        </td>
       </tr>
       <tr>
         <td align="right" style="padding-right: 5px"><b>Begining User#:</b></td>
          <td nowrap><asp:TextBox MaxLength="15" ID="beuserTextBox" Width="100px" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)"  runat="server"></asp:TextBox>
          </td>
         <td align="right" style="padding-right: 5px"><b>Ending User#:</b></td>
          <td nowrap><asp:TextBox MaxLength="15" ID="enduserTextBox" Width="100px" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)"  runat="server"></asp:TextBox>
          
          </td>
        </tr>          
        
        <tr></tr><tr></tr>
        <tr>
            <td nowrap align="right"></td>
            <td colspan="3"> 
                <b><asp:CheckBox ID="CheckBox1" Text="Print G/L Account Description " runat="server" /></b><br />
            </td>
         </tr>
                 
          <tr><td colspan="3">
            &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
            <asp:Button ID="submitbutton" OnClick="submitbutton_click" CausesValidation="true" OnClientClick="confirmPost()" runat="server" class="buttonM" Text="Submit" />
          &nbsp;&nbsp;&nbsp;&nbsp;
          <asp:Label ID="OutPutFile" runat="server" Text="Output File:" Font-Bold="True" Font-Size="Larger" ForeColor="Blue"></asp:Label>
              <asp:HyperLink ID="HyperLink1" runat="server" Target="_blank" Font-Bold="True" Font-Size="Larger" ForeColor="Red"></asp:HyperLink>
          </td></tr>
          </table>
       
          <asp:FormView ID="FormView1" Visible="False" runat="server" 
              DataSourceID="ObjectDataSource1">                                                                                                                                                             
              
             
              <ItemTemplate>
                  vendpost:
                  <asp:Label ID="vendpostLabel" runat="server" 
                      Text='<%# Bind("vendpost") %>'></asp:Label><br />                 
                  vpstinv:
                  <asp:Label ID="vpstinvLabel" runat="server" Text='<%# Bind("vpstinv") %>' />
                  <br />
              </ItemTemplate>                            
          </asp:FormView>
          
          <asp:ObjectDataSource ID="ObjectDataSource1" runat="server" OldValuesParameterFormatString="original_{0}"
              SelectMethod="SelectVendInvPost" TypeName="voucherpay">
              <SelectParameters>
                  <asp:Parameter Name="prmUser" Type="String" />
                  <asp:Parameter Name="prmvendpost" Type="String" />
                  <asp:Parameter Name="prmBeginvend" Type="String" />
                  <asp:Parameter Name="prmEndvend" Type="String" />
                  <asp:Parameter Name="prmBeginUsr" Type="String" />
                  <asp:Parameter Name="prmEndUsr" Type="String" />
                  <asp:Parameter Name="prmBegindate" Type="String" />
                  <asp:Parameter Name="prmEnddate" Type="String" />
                  <asp:Parameter Name="prmPstDate" Type="String" />
                  <asp:Parameter Name="prmGlActNm" Type="String" />
                  <asp:Parameter Name="prmperiod" Type="Int32" />
                  <asp:Parameter Name="prmOut" Type="String" />
                  <asp:Parameter Name="prmGl2" Type="String" />
              </SelectParameters>
          </asp:ObjectDataSource>
     
    </div>
    <ft:footer id="Footer1" runat="server"></ft:footer>
    </form>
  </body>
</HTML>


