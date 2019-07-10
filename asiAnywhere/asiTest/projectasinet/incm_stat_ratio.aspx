<%@ Page Language="c#" AutoEventWireup="true" Debug="false" Inherits="incm_stat_ratio" Codebehind="incm_stat_ratio.aspx.cs" %>
<%@ Register Src="footer.ascx" TagName="Footer" TagPrefix="ft" %>
<%@ Register Src="header.ascx" TagName="Header" TagPrefix="hd" %>
<html xmlns="http://www.w3.org/1999/xhtml" >
  <head id="Head1" runat="server">
    <title>GL Financial Statement</title>
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

   
   
    

    
function glvendtext() {
    var vend = document.getElementById("CheckBox1");
    vend.focus();
}
var glvand;
function AccountLook(obj) {
    glvand = obj;
    var NewWindow = window.open("accountlook.aspx?jrnl=" + "E" + " ", "AccountLookupWindow", "width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}

function AccountLookup(ReturnObj1) {
    if (glvand == "1") {
        document.forms[0].begact1TextBox.value = ReturnObj1;
    }
    if (glvand == "2") {
        document.forms[0].endact1TextBox.value = ReturnObj1;
    }
    if (glvand == "3") {
        document.forms[0].begact2TextBox.value = ReturnObj1;
    }
    if (glvand == "4") {
        document.forms[0].endact2TextBox.value = ReturnObj1;
    }
    if (glvand == "5") {
        document.forms[0].begact3TextBox.value = ReturnObj1;
    }
    if (glvand == "6") {
        document.forms[0].endact3TextBox.value = ReturnObj1;
    }
    if (glvand == "7") {
        document.forms[0].begact4TextBox.value = ReturnObj1;
    }
    if (glvand == "8") {
        document.forms[0].endact4TextBox.value = ReturnObj1;
    }
    if (glvand == "9") {
        document.forms[0].begact5TextBox.value = ReturnObj1;
    }
    if (glvand == "10") {
        document.forms[0].endact5TextBox.value = ReturnObj1;
    }
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
            
          <TD align=left nowrap><font size=+0><b>GL Financial Statement&nbsp;</b></font></TD>
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
      
      <tr><td align="center"><table>
      <tr>      
        <td nowrap align="right" style="padding-right:5px;"><b>Transaction Date:</b></td>           
            <td><asp:TextBox ID="trndateTextBox" onfocus="javascript:preEnter( this, 'yes' );" onblur="javascript:preLeave( this, 'date', '99/99/9999' );"  runat="server" Width="100px"></asp:TextBox>
            <a href="#" onblur="document.getElementById('trndateTextBox').focus()"  tabindex="1" onClick="showCalendarControl(trndateTextBox); return false"><asp:Image ID="Image5" runat="server" ImageUrl="images/lookup_icon.gif" /></a></td>      
      </tr><tr>        
        <td nowrap align="right" style="padding-right:5px;"><b>Period:</b></td>            
           <td><asp:TextBox ID="predTextBox" onfocus="glvendtext()" runat="server" Width="100px"></asp:TextBox></td>      
      </tr><tr> <td nowrap align="right" style="padding-right:5px;"><b>Pre Close Period:</b></td>       
        <td><asp:CheckBox ID="CheckBox1" runat="server" /><br /></td>      
      </tr>  
      </table></td></tr>
      <tr></tr><tr></tr><tr></tr><tr></tr>
      
      
       <tr><td style="width:300px;" align="right" >      
      <fieldset style="width:440px;">
      <legend style="color:Blue"><b>Account Ranges:</b></legend>         
      
      <table>
      <tr><td colspan="2" align="right"><b>From Account#:</b>&nbsp;&nbsp;&nbsp&nbsp;&nbsp;&nbsp&nbsp;&nbsp;&nbsp&nbsp;&nbsp;&nbsp&nbsp;&nbsp;&nbsp&nbsp</td>
      <td colspan="2" align="right"><b>To Account#:</b>&nbsp;&nbsp;&nbsp&nbsp;&nbsp;&nbsp&nbsp;&nbsp;&nbsp&nbsp;&nbsp;&nbsp&nbsp;&nbsp;&nbsp&nbsp;&nbsp;&nbsp</td></tr>
          <tr><td align="right" style="padding-right: 5px"><b>Cost of Sales:</b></td>
          <td nowrap><asp:TextBox  ID="begact1TextBox" Width="130px" runat="server"></asp:TextBox>
          <a href="#" tabindex="1" onClick="AccountLook(1); return false"><asp:Image ID="Image13" runat="server" ImageUrl="images/lookup_icon.gif" /></a></td>
          <td nowrap><asp:TextBox  ID="endact1TextBox" Width="130px" runat="server"></asp:TextBox>
          <a href="#" tabindex="1" onClick="AccountLook(2); return false"><asp:Image ID="Image1" runat="server" ImageUrl="images/lookup_icon.gif" /></a></td></tr>
          <tr nowrap><td align="right" style="padding-right: 5px"><b>Operation Expense:</b></td>
          <td nowrap><asp:TextBox  ID="begact2TextBox" Width="130px" runat="server"></asp:TextBox>
          <a href="#" tabindex="1" onClick="AccountLook(3); return false"><asp:Image ID="Image2" runat="server" ImageUrl="images/lookup_icon.gif" /></a></td>
          <td nowrap><asp:TextBox  ID="endact2TextBox" Width="130px" runat="server"></asp:TextBox>
          <a href="#" tabindex="1" onClick="AccountLook(4); return false"><asp:Image ID="Image3" runat="server" ImageUrl="images/lookup_icon.gif" /></a></td></tr>
          <tr><td align="right" style="padding-right: 5px"><b>General Admin:</b></td>
          <td nowrap><asp:TextBox  ID="begact3TextBox" Width="130px" runat="server"></asp:TextBox>
          <a href="#" tabindex="1" onClick="AccountLook(5); return false"><asp:Image ID="Image4" runat="server" ImageUrl="images/lookup_icon.gif" /></a></td>
          <td nowrap><asp:TextBox  ID="endact3TextBox" Width="130px" runat="server"></asp:TextBox>
          <a href="#" tabindex="1" onClick="AccountLook(6); return false"><asp:Image ID="Image6" runat="server" ImageUrl="images/lookup_icon.gif" /></a></td></tr>
          <tr><td align="right" style="padding-right: 5px"><b>Income Expense:</b></td>
          <td nowrap><asp:TextBox  ID="begact4TextBox" Width="130px" runat="server"></asp:TextBox>
          <a href="#" tabindex="1" onClick="AccountLook(7); return false"><asp:Image ID="Image7" runat="server" ImageUrl="images/lookup_icon.gif" /></a></td>
          <td nowrap><asp:TextBox  ID="endact4TextBox" Width="130px" runat="server"></asp:TextBox>
          <a href="#" tabindex="1" onClick="AccountLook(8); return false"><asp:Image ID="Image8" runat="server" ImageUrl="images/lookup_icon.gif" /></a></td></tr>
          <tr><td align="right" style="padding-right: 5px"><b>Other Expense:</b></td>
          <td nowrap><asp:TextBox  ID="begact5TextBox" Width="130px" runat="server"></asp:TextBox>
          <a href="#" tabindex="1" onClick="AccountLook(9); return false"><asp:Image ID="Image9" runat="server" ImageUrl="images/lookup_icon.gif" /></a></td>
          <td nowrap><asp:TextBox  ID="endact5TextBox" Width="130px" runat="server"></asp:TextBox>
          <a href="#" tabindex="1" onClick="AccountLook(10); return false"><asp:Image ID="Image10" runat="server" ImageUrl="images/lookup_icon.gif" /></a></td></tr>
        </table>
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
       
          <asp:FormView ID="FormView1" Visible="False" runat="server" 
              DataSourceID="ObjectDataSource1">               
             
              <ItemTemplate>
                  inc:
                  <asp:Label ID="incLabel" runat="server" 
                      Text='<%# Bind("inc") %>'></asp:Label>
                      
                      ext:
                  <asp:Label ID="extLabel" runat="server" 
                      Text='<%# Bind("ext") %>'></asp:Label><br />                 
                  
                  
                  
              </ItemTemplate>                            
          </asp:FormView>
          
          <asp:ObjectDataSource ID="ObjectDataSource1" runat="server" OldValuesParameterFormatString="original_{0}"
              SelectMethod="IncomeSmtRatios" TypeName="ledger">
              <SelectParameters>
                  <asp:Parameter Name="prmUser"   Type="String" />
                  <asp:Parameter Name="prmAction"  Type="String" />
                  <asp:Parameter Name="prmtrnsdt" Type="String" />
                  <asp:Parameter Name="prmclsprd" Type="String" />
                  <asp:Parameter Name="prmbeact1" Type="String" />
                  <asp:Parameter Name="prmendact1" Type="String" />
                  <asp:Parameter Name="prmbeact2" Type="String" />
                  <asp:Parameter Name="prmendact2" Type="String" />                  
                  <asp:Parameter Name="prmbegact3" Type="String" />
                  <asp:Parameter Name="prmendact3" Type="String" />
                  <asp:Parameter Name="prmbeact4" Type="String" />
                  <asp:Parameter Name="prmendact4" Type="String" />
                  <asp:Parameter Name="prmbegact5" Type="String" />
                  <asp:Parameter Name="prmendact5" Type="String" />
                  <asp:Parameter Name="prmPeriod" Type="Int32" />
                  <asp:Parameter Name="prmOut" Type="String" />
                  
              </SelectParameters>
          </asp:ObjectDataSource>
     
    </div>
    <ft:footer id="Footer1" runat="server"></ft:footer>
    </form>
  </body>
</HTML>


