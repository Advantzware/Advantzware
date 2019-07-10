<%@ Page Language="c#" AutoEventWireup="true" Debug="true" Inherits="view_void_apchk" Codebehind="view_void_apchk.aspx.cs" %>
<%@ Register Src="footer.ascx" TagName="Footer" TagPrefix="ft" %>
<%@ Register Src="header.ascx" TagName="Header" TagPrefix="hd" %>
<html xmlns="http://www.w3.org/1999/xhtml" >
  <head id="Head1" runat="server">
    <title>Void Accounts Payable Checks</title>
    <LINK href="include/style2.css" type="text/css" rel="stylesheet"/>
    <LINK REL="stylesheet" TYPE="text/css" HREF="include/CalendarControl.css" >
    <script language = "JavaScript" src="include/CalendarControl.js"></script>
     <script language="javascript" src="include/date.js"></script>
    <script language="javascript" src="include/event.js"></script>
    <script language="javascript" src="include/insert.js"></script>
    <script language="javascript" src="include/validate2.js"></script>
    <script language =javascript>
        window.onload = detfocus;
        function detfocus() {
            if (document.getElementById("FormView1_voidedTextBox"))
                document.getElementById("FormView1_voidedTextBox").focus();
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

       
    

      
    function getdecimal(obj, obj2) {
        if (obj.value.indexOf(".") != -1) {
            return;
        }
        else if (obj.value.length == obj2) {
            obj.value = obj.value + ".";
        }

    }
    

    
 
   function voidfocus() {
       var voided = document.getElementById("FormView1_voidedTextBox");
       voided.focus();
   }
   function voidchnage(obj) {
       if ((obj.value) == "Yes" || (obj.value) == "yes" || (obj.value) == "YES" || (obj.value) == "NO" || (obj.value) == "no" || (obj.value) == "No")
       { }
       else {
           alert("Enter only Yes or No");
           obj.focus();
       }
       
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
          <TD align=left nowrap><font size=+0><b>Void Accounts Payable Checks&nbsp;</b></font></TD>
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
      <asp:LinkButton ID="lnk_Listcustomers" runat="server" OnClick="lnk_listinvoice" >Brws Checks</asp:LinkButton></li>
      <li class="selected"><asp:LinkButton ID="lnk_viewcustomers" runat="server" OnClick="lnk_viewcustomers_Click" > View Check</asp:LinkButton></li></ul></div>
            
      </td>
      </tr></table>
       <div>
           <asp:FormView ID="FormView1" runat="server" Width="650px" DataSourceID="ObjectDataSource1">
               <EditItemTemplate>
                   <fieldset class="shade" style="width:560px;"><table class="shade">
                   <tr><td align="right" style="padding-right:5px"><b>Check#:</b></td>
                   <td><asp:TextBox ID="chknoTextBox" Width="100px" ForeColor="#ACA899" onfocus="voidfocus()" ReadOnly="true" runat="server" Text='<%# Bind("chkno") %>' /></td>
                   <td align="right" style="padding-right:5px"><b>Check Amount:</b></td>
                   <td><asp:TextBox ID="chkamtTextBox" ForeColor="#ACA899" Width="100px" onfocus="voidfocus()" ReadOnly="true" runat="server" Text='<%# Bind("chkamt") %>' /></td>
                   <td align="right" style="padding-right:5px"><b>Date:</b></td>
                   <td><asp:TextBox ID="chkdateTextBox" ForeColor="#ACA899" Width="80px" onfocus="voidfocus()" ReadOnly="true" runat="server" Text='<%# Bind("chkdate") %>' /></td></tr><tr></tr><tr></tr>
                   <tr><td align="right" style="padding-right:5px"><b>Vendor#:</b></td>
                   <td><asp:TextBox ID="vendTextBox" Width="100px" ForeColor="#ACA899" onfocus="voidfocus()" ReadOnly="true" runat="server" Text='<%# Bind("vend") %>' /></td>
                   <td colspan="2"><asp:TextBox ID="vendnameTextBox" ForeColor="#ACA899" Width="200px" onfocus="voidfocus()" ReadOnly="true" runat="server" Text='<%# Bind("vendname") %>' /></td></tr><tr></tr><tr></tr>
                   <tr><td align="right" style="padding-right:5px"><b>BankCode:</b></td>
                   <td><asp:TextBox ID="bnkcodTextBox" Width="100px" ForeColor="#ACA899" onfocus="voidfocus()" ReadOnly="true" runat="server" Text='<%# Bind("bnkcod") %>' /></td>
                   <td colspan="2"><asp:TextBox ID="bnknameTextBox" ForeColor="#ACA899" onfocus="voidfocus()" ReadOnly="true" Width="200" runat="server" Text='<%# Bind("bnkname") %>' /></td>
                   <td align="right" style="padding-right:5px"><b>Voided:</b></td>
                   <td><asp:TextBox ID="voidedTextBox" Width="80px"  onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this);voidchnage(this);" runat="server" Text='<%# Bind("voided") %>' />
                   
                   </td></tr>
                   
                   
                   
                   <asp:TextBox ID="vRecKeyTextBox" Visible="false" runat="server" Text='<%# Bind("vRecKey") %>' />
                   
                   </table>
                   <asp:Button ID="UpdateButton" runat="server" CssClass="button" OnClick="UpdateButton_Click" CausesValidation="True" Text="Save" />
                   &nbsp;<asp:Button ID="UpdateCancelButton" runat="server" CausesValidation="False" CssClass="button" CommandName="Cancel" Text="Cancel" />
                       </fieldset>
               </EditItemTemplate>
               
               <ItemTemplate>
                   <fieldset class="shade" style="width:560px;"><table id="tblSearch" class="shade" >
                   <tr><td align="right" style="padding-right:5px"><b>Check#:</b></td>
                   <td><asp:Label ID="chknoLabel" Width="70" BackColor="Turquoise" BorderColor="White" BorderStyle="Solid" BorderWidth="1px" runat="server" Text='<%# Bind("chkno") %>' /></td>
                   <td align="right" style="padding-right:5px"><b>Check Amount:</b></td>
                   <td><asp:Label ID="chkamtLabel" Width="70" BackColor="Turquoise" BorderColor="White" BorderStyle="Solid" BorderWidth="1px" runat="server" Text='<%# Bind("chkamt") %>' /></td>
                   <td align="right" style="padding-right:5px"><b>Date:</b></td>
                   <td><asp:Label ID="chkdateLabel" Width="70" BackColor="Turquoise" BorderColor="White" BorderStyle="Solid" BorderWidth="1px" runat="server" Text='<%# Bind("chkdate") %>' /></td></tr><tr></tr><tr></tr>
                   <tr><td align="right" style="padding-right:5px"><b>Vendor#:</b></td>
                   <td><asp:Label ID="vendLabel" Width="70" BackColor="Turquoise" BorderColor="White" BorderStyle="Solid" BorderWidth="1px" runat="server" Text='<%# Bind("vend") %>' /></td>
                   <td colspan="2"><asp:Label ID="vendnameLabel" Width="200" BackColor="Turquoise" BorderColor="White" BorderStyle="Solid" BorderWidth="1px" runat="server" Text='<%# Bind("vendname") %>' /></td></tr><tr></tr><tr></tr>
                   <tr><td align="right" style="padding-right:5px"><b>Bank Code:</b></td>
                   <td><asp:Label ID="bnkcodLabel" Width="70" BackColor="Turquoise" BorderColor="White" BorderStyle="Solid" BorderWidth="1px" runat="server" Text='<%# Bind("bnkcod") %>' /></td>
                   <td><asp:Label ID="bnknameLabel" Width="130" BackColor="Turquoise" BorderColor="White" BorderStyle="Solid" BorderWidth="1px" runat="server" Text='<%# Bind("bnkname") %>' /></td>
                   <td align="right" style="padding-right:5px"><b>Voided:</b></td>
                   <td><asp:Label ID="voidedLabel" Width="70" BackColor="Turquoise" BorderColor="White" BorderStyle="Solid" BorderWidth="1px" runat="server" Text='<%# Bind("voided") %>' /></td></tr>
                   
                   <asp:Label ID="vRecKeyLabel" Visible="false" runat="server" Text='<%# Bind("vRecKey") %>' />
                   </table>
                   
                   <asp:Button ID="UpdateButton" runat="server" CssClass="button" CommandName="edit" Text="Update" />
                   </fieldset>
               </ItemTemplate>
           
           
           
           
           </asp:FormView>
           
           <asp:ObjectDataSource ID="ObjectDataSource1" runat="server" 
                OldValuesParameterFormatString="original_{0}" SelectMethod="SelectVoidAPCheck" 
                TypeName="voucherpay">
                <SelectParameters>
                   <asp:Parameter Name="prmUser" Type="string" />  
                   <asp:Parameter DefaultValue="Select" Name="prmAction" Type="String" />                    
                   <asp:Parameter Name="prmvend"  Type="string" />
                   <asp:Parameter Name="prmchkno" Type="Int32" />
                   <asp:Parameter Name="prmbnkcod" Type="String" />                                       
                   <asp:Parameter Name="prmchkdate" Type="String" />                   
                   <asp:Parameter Name="prmchkamt" Type="Decimal" />                   
                   <asp:Parameter Name="prmmanchk" Type="String" />
                   <asp:Parameter Name="prmvendname" Type="String" />
                   <asp:Parameter Name="prmbnkname" Type="String" />
                   <asp:Parameter Name="prmvoided" Type="String" />
                   <asp:SessionParameter SessionField="void_chklist_reckey_rec" Name="prmRecKey" Type="String" />
                   
                </SelectParameters>
            </asp:ObjectDataSource>
       </div>   
       
       
    </div>
    </td></tr></table>    
    <ft:footer id="Footer1" runat="server"></ft:footer>
    </form>
  </body>
</HTML>

