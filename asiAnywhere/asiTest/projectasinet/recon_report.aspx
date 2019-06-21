<%@ Page Language="c#" AutoEventWireup="true" Debug="false" Inherits="recon_report" Codebehind="recon_report.aspx.cs" %>
<%@ Register Src="footer.ascx" TagName="Footer" TagPrefix="ft" %>
<%@ Register Src="header.ascx" TagName="Header" TagPrefix="hd" %>
<html xmlns="http://www.w3.org/1999/xhtml" >
  <head id="Head1" runat="server">
    <title>Reconciliation Report</title>
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

        function banklookup() {

            var NewWindow = window.open("bank_lookup.aspx", "banklookup", "width=500,height=400,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
        }
        function banklook(ReturnObj1, ReturnObj2) {
            document.forms[0].bnkcodTextBox.value = ReturnObj1;


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
         <asp:HiddenField ID="HiddenFieldPost" runat="server" />    
      <TABLE id="tblTop" cellSpacing="3" align="center" border="0" Width="100%">
        <TR>
            
          <TD align=left nowrap><font size=+0><b>Reconciliation Report&nbsp;</b></font></TD>
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
      <tr><td><table>
      <tr>
        
        <td colspan="3" align="left" style="padding-right: 5px; " nowrap><b>Enter Bank Code or Leave Blank For All:</b>
            &nbsp;
            <asp:TextBox ID="bnkcodTextBox" runat="server" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" Width="100px"></asp:TextBox>
            <a href="#" tabindex="1" onclick="banklookup(); return false"><asp:Image ID="banklookimg" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
        <br /></td>      
      </tr><tr></tr><tr></tr>
      <tr><td align="right" style="padding-right: 5px"><b>Begining Vendor#:</b></td><td>
          <asp:TextBox ID="bevendTextBox" MaxLength="8" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)"  width="100px" runat="server"></asp:TextBox>
          <a href="#" tabindex="1" onClick="vendorlook(1); return false"><asp:Image ID="CustomerLook" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
          </td>
        <td align="right" style="padding-right: 5px"><b>Ending Vendor#:</b></td><td><asp:TextBox ID="endvendTextBox" MaxLength="8" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" width="100px" runat="server"></asp:TextBox>
            <a href="#" tabindex="1" onClick="vendorlook(2); return false"><asp:Image ID="Image2" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
        </td>
      </tr>           
      <tr>
        <td align="right" style="padding-right: 5px"><b>Begining Check Date:</b></td>
          <td><asp:TextBox  ID="bechkdtTextBox" Width="100px" onfocus="this.select();javascript:preEnter( this, 'yes' );" onblur="javascript:preLeave( this, 'date', '99/99/9999' );" runat="server"></asp:TextBox>
          <a href="#" onblur="document.getElementById('bechkdtTextBox').focus()"  tabindex="1" onClick="showCalendarControl(bechkdtTextBox); return false"><asp:Image ID="Image6" runat="server" ImageUrl="images/lookup_icon.gif" /></a>          
        </td>
        <td align="right" style="padding-right: 5px"><b>Ending Check Date:</b></td>
          <td><asp:TextBox ID="endchkdtTextBox" Width="100px" onfocus="this.select();javascript:preEnter( this, 'yes' );" onblur="javascript:preLeave( this, 'date', '99/99/9999' );" runat="server"></asp:TextBox>
          <a href="#" onblur="document.getElementById('endchkdtTextBox').focus()"  tabindex="1" onClick="showCalendarControl(endchkdtTextBox); return false"><asp:Image ID="Image3" runat="server" ImageUrl="images/lookup_icon.gif" /></a>          
        </td>
       </tr>
       <tr>
         <td align="right" style="padding-right: 5px"><b>Begining Check#:</b></td>
          <td nowrap><asp:TextBox MaxLength="8" ID="bechkTextBox" Width="100px" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)"  runat="server"></asp:TextBox>
          </td>
         <td align="right" style="padding-right: 5px"><b>Ending Check#:</b></td>
          <td nowrap><asp:TextBox MaxLength="8" ID="endchkTextBox" Width="100px" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)"  runat="server"></asp:TextBox>
          
          </td>
        </tr>  
        <tr>
         <td align="right" style="padding-right: 5px"><b>Begining Journal#:</b></td>
          <td nowrap><asp:TextBox MaxLength="8" ID="bejurTextBox" Width="100px" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)"  runat="server"></asp:TextBox>
          </td>
         <td align="right" style="padding-right: 5px"><b>Ending Journal#:</b></td>
          <td nowrap><asp:TextBox MaxLength="8" ID="endjurTextBox" Width="100px" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)"  runat="server"></asp:TextBox>
          
          </td>
        </tr> 
        </table></td></tr>        
        
        <tr></tr><tr></tr>
        <tr><td align="middle"><table>
        <tr><td><b><asp:CheckBox ID="CheckBox1" Text="Include Deposits? " runat="server" /></b>
            </td></tr>
         <tr><td><b><asp:CheckBox ID="CheckBox2" Text="Include Journals? " runat="server" /></b>
            </td>
         </tr> </table></td></tr>        
            <tr><td align="middle"><table>
            <tr><td><b>Print?   <asp:RadioButtonList ID="RadioButtonList1" RepeatLayout="Flow" CellSpacing="1" RepeatColumns="3"  Font-Bold ="true" runat="server" >                                                      
                            <asp:ListItem  Value="unrec"    Text="Unreconcilied" /> 
                            <asp:ListItem   Value="rec" Text="Reconcilied" />   
                            <asp:ListItem   Value="all" Text="All" />                                                              
                            </asp:RadioButtonList></b></td></tr></table></td></tr>
            <tr><td align="middle"><table>
            <tr><td><b><asp:CheckBox ID="CheckBox3" Text="Clear Unreconcilied? " runat="server" /></b>
            </td><td> 
                <b><asp:CheckBox ID="CheckBox4" Text="Unclear Reconcilied? " runat="server" /></b>
            </td>         
            </tr></table></td></tr>
            <tr><td align="middle"><table>
            <tr><td><b><asp:CheckBox ID="CheckBox5" Text="Sort by Vendor#? " runat="server" /></b><br />
            </td>
         </tr></table></td></tr>
                 
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
                  recrpt:
                  <asp:Label ID="recrptLabel" runat="server" 
                      Text='<%# Bind("recrpt") %>'></asp:Label><br />                 
                  
              </ItemTemplate>                            
          </asp:FormView>
          
          <asp:ObjectDataSource ID="ObjectDataSource1" runat="server" OldValuesParameterFormatString="original_{0}"
              SelectMethod="SelectReconciliationReport" TypeName="voucherpay">
              <SelectParameters>
                  <asp:Parameter Name="prmUser" Type="String" />
                  <asp:Parameter Name="prmrecrpt" Type="String" />
                  <asp:Parameter Name="prmbnkcod" Type="String" />
                  <asp:Parameter Name="prmbegvend" Type="String" />
                  <asp:Parameter Name="prmendvend" Type="String" />
                  <asp:Parameter Name="prmbegchkdt" Type="String" />
                  <asp:Parameter Name="prmendchkdt" Type="String" />
                  <asp:Parameter Name="prmbegchk" Type="String" />
                  <asp:Parameter Name="prmendchk" Type="String" />
                  <asp:Parameter Name="prmbegjno" Type="String" />
                  <asp:Parameter Name="prmendjno" Type="Int32" />
                  <asp:Parameter Name="prmdep" Type="String" />
                  <asp:Parameter Name="prmjournl" Type="String" />
                  <asp:Parameter Name="prmprint" Type="String" />
                  <asp:Parameter Name="prmclrunrec" Type="String" />
                  <asp:Parameter Name="prmunclrunrec" Type="String" />
                  <asp:Parameter Name="prmsrtvend" Type="String" />
                  <asp:Parameter Name="prmextra" Type="String" />                  
                  <asp:Parameter Name="prmOut" Type="String" />
                  
              </SelectParameters>
          </asp:ObjectDataSource>
     
    </div>
    <ft:footer id="Footer1" runat="server"></ft:footer>
    </form>
  </body>
</HTML>


