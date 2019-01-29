<%@ Page Language="c#" AutoEventWireup="true" Debug="true" Inherits="salestax_viewlist" Codebehind="salestax_viewlist.aspx.cs" %>

<%@ Register Src="footer.ascx" TagName="Footer" TagPrefix="ft" %>
<%@ Register Src="header.ascx" TagName="Header" TagPrefix="hd" %>
<html xmlns="http://www.w3.org/1999/xhtml" >
  <head id="Head1" runat="server">
    <title>Sales Tax Codes</title>
    <LINK href="include/style2.css" type="text/css" rel="stylesheet"/>
   <LINK REL="stylesheet" TYPE="text/css" HREF="include/CalendarControl.css" >
    <script language="javascript" src="include/date.js"></script>
    <script language="javascript" src="include/event.js"></script>
    <script language="javascript" src="include/insert.js"></script>
    <script language = "JavaScript" src="include/CalendarControl.js">
    </script>
  
    
<script language="javascript" >
    var taxcode = "";
    function taxcodelook(var1) {
        taxcode = var1;
        var NewWindow = window.open("taxgr_lookup.aspx", "TaxLookupWindow", "width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
    }
    function TaxGroupLookup(ReturnObj1, ReturnObj2, ReturnObj3, ReturnObj4, ReturnObj5, ReturnObj6) {
        if (taxcode == "1") {
            document.forms[0].FormView1_code1TextBox.value = ReturnObj2;
            document.forms[0].FormView1_dscr1TextBox.value = ReturnObj3;
            document.forms[0].FormView1_rate1TextBox.value = ReturnObj4;
            document.forms[0].FormView1_acclTextBox.value = ReturnObj6;
            if (ReturnObj5 == "yes") {               
                document.forms[0].FormView1_CheckBox1.checked = true;               
            }
            else
                document.forms[0].FormView1_CheckBox1.Checked = false;
        }
        else if (taxcode == "2") {
            document.forms[0].FormView1_code2TextBox.value = ReturnObj2;
            document.forms[0].FormView1_dscr2TextBox.value = ReturnObj3;
            document.forms[0].FormView1_rate2TextBox.value = ReturnObj4;
            document.forms[0].FormView1_acc2TextBox.value = ReturnObj6;
            if (ReturnObj5 == "yes") {
                document.forms[0].FormView1_CheckBox2.checked = true;
            }
            else
                document.forms[0].FormView1_CheckBox2.Checked = false;
        }
        else if (taxcode == "3") {
            document.forms[0].FormView1_code3TextBox.value = ReturnObj2;
            document.forms[0].FormView1_dscr3TextBox.value = ReturnObj3;
            document.forms[0].FormView1_rate3TextBox.value = ReturnObj4;
            document.forms[0].FormView1_acc3TextBox.value = ReturnObj6;
            if (ReturnObj5 == "yes") {
                document.forms[0].FormView1_CheckBox3.checked = true;
            }
            else
                document.forms[0].FormView1_CheckBox3.Checked = false;
        }
        else if (taxcode == "4") {
            document.forms[0].FormView1_code4TextBox.value = ReturnObj2;
            document.forms[0].FormView1_dscr4TextBox.value = ReturnObj3;
            document.forms[0].FormView1_rate4TextBox.value = ReturnObj4;
            document.forms[0].FormView1_acc4TextBox.value = ReturnObj6;
            if (ReturnObj5 == "yes") {
                document.forms[0].FormView1_CheckBox4.checked = true;
            }
            else
                document.forms[0].FormView1_CheckBox4.Checked = false;
        }
        else {
            document.forms[0].FormView1_code5TextBox.value = ReturnObj2;
            document.forms[0].FormView1_dscr5TextBox.value = ReturnObj3;
            document.forms[0].FormView1_rate5TextBox.value = ReturnObj4;
            document.forms[0].FormView1_acc5TextBox.value = ReturnObj6;
            if (ReturnObj5 == "yes") {
                document.forms[0].FormView1_CheckBox5.checked = true;
            }
            else
                document.forms[0].FormView1_CheckBox5.Checked = false;
        }

    }
    var account = "";
    function AccountLook(var1) {
        account = var1;
        var NewWindow = window.open("accountlook.aspx", "AccountLookupWindow", "width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
    }

    function AccountLookup(ReturnObj1, ReturnObj2) {
        if (account == "1")
            document.forms[0].FormView1_acclTextBox.value = ReturnObj1;
        else if (account == "2")
            document.forms[0].FormView1_acc2TextBox.value = ReturnObj1;
        else if (account == "3")
            document.forms[0].FormView1_acc3TextBox.value = ReturnObj1;
        else if (account == "4")
            document.forms[0].FormView1_acc4TextBox.value = ReturnObj1;
        else
            document.forms[0].FormView1_acc5TextBox.value = ReturnObj1;
    }

    function ratetext() {
        var comm = document.getElementById("FormView1_rate1TextBox").value;
        if (comm.indexOf(".") != -1) {
            return;
        }
        else if (comm.length > 3 && comm.length < 5)
            comm = comm + ".";
        document.getElementById("FormView1_rate1TextBox").value = comm;
    }
    function ratetext2() {
        var comm = document.getElementById("FormView1_rate2TextBox").value;
        if (comm.indexOf(".") != -1) {
            return;
        }
        else if (comm.length > 3 && comm.length < 5)
            comm = comm + ".";
        document.getElementById("FormView1_rate2TextBox").value = comm;
    }
    function ratetext3() {
        var comm = document.getElementById("FormView1_rate3TextBox").value;
        if (comm.indexOf(".") != -1) {
            return;
        }
        else if (comm.length > 3 && comm.length < 5)
            comm = comm + ".";
        document.getElementById("FormView1_rate3TextBox").value = comm;
    }
    function ratetext4() {
        var comm = document.getElementById("FormView1_rate4TextBox").value;
        if (comm.indexOf(".") != -1) {
            return;
        }
        else if (comm.length > 3 && comm.length < 5)
            comm = comm + ".";
        document.getElementById("FormView1_rate4TextBox").value = comm;
    }
    function ratetext5() {
        var comm = document.getElementById("FormView1_rate5TextBox").value;
        if (comm.indexOf(".") != -1) {
            return;
        }
        else if (comm.length > 3 && comm.length < 5)
            comm = comm + ".";
        document.getElementById("FormView1_rate5TextBox").value = comm;
    }

    function focusval(obj) {
        obj.style.backgroundColor = 'blue';
        obj.style.color = 'white';
    }
    function blurval(obj) {
        obj.style.backgroundColor = 'Window';
        obj.style.color = 'WindowText';
    }
    
 </script>

 </head>    

   <body>
        <form id="frmList" runat="server"  defaultfocus='cust_TextBox'>   
            <hd:header id="Header1" runat="server"></hd:header>
                <table width="100%"><tr><td><div>
        <table align="left" border="1" width="75%">
                <tr class="topheadcolor">                                      
                 
                        <td nowrap width="25px";>
                        <asp:ImageButton ID="img_btn_add" runat="server" Width="35px" ImageUrl="~/Images/add.bmp" ToolTip="Add" OnClick="img_btn_add_click" />
                        </td>                       
                        <td nowrap width="25px";>
                        <asp:ImageButton ID="img_btn_exit" runat="server" Width="35px" ImageUrl="~/Images/exit-au.bmp" ToolTip="LogOut" OnClick="hlnkLogOut_Click" />
                        </td>
                        <td nowrap> &nbsp;</td>
                </tr>
      </table></div>
        </td>
      </tr>
      <tr>
      <td>
                <div>            
                    <TABLE id="tblTop" cellSpacing="3" align="center" border="0" Width="100%">
                        <TR>
                             <TD width=30>&nbsp;</TD>
                            <TD align=left nowrap><font size=+0><b>Sales Tax Codes&nbsp;</b></font></TD>
                            <td nowrap>
                                <asp:LinkButton ID="backtomenuLinkButton" OnClick ="Back_tomenu_Click" runat="server">Back to menu</asp:LinkButton>
                            </td>          
                            <TD  align="left" nowrap>Logged as&nbsp;
                                <asp:label id="lblUser" runat="server" Font-Bold="True">&nbsp;</asp:label>&nbsp;&nbsp;&nbsp;            
                                <asp:linkbutton id="hlnkLogOut" runat="server" OnClick="hlnkLogOut_Click">Log out</asp:linkbutton>
                                &nbsp;&nbsp;<asp:hyperlink id="hlnkChangePwd" runat="server" NavigateUrl="changepwd.aspx"></asp:hyperlink>
                                &nbsp;<b>Company: &nbsp;</b><asp:label id="labelcompany"   runat="server" Font-Bold="True">&nbsp;</asp:label>
                            </TD>
          
                            <TD vAlign="middle" width="20">&nbsp;</TD>          
                            <td width=30>&nbsp;</td>
                        </TR>
                    </TABLE>
                    <table>
                        <tr bgcolor="gray">
                            <td nowrap><div  id="navigation" style="width:100%">
		                        <ul nowrap> <li >                            
                                <asp:LinkButton ID="lnk_Listvend" runat="server" OnClick="lnk_Listvend_Click" >Browse Code</asp:LinkButton></li>
                                <li class="selected"><asp:LinkButton ID="lnk_viewvend" runat="server"  OnClick="lnk_viewvend_Click"  > View Code</asp:LinkButton></li></ul></div>
                                
                                
                            </td>      
                        </tr>
                   </table>
            
            
                    <asp:FormView ID="FormView1" runat="server" OnDataBound="FormView1_DataBound" OnUnload="Formview_Unload" DataSourceID="ObjectDataSource1">
                      <EditItemTemplate>
                      <asp:Panel ID="EditPanel"  DefaultButton="UpdateButton" runat="server" >
                    
                   <fieldset class="shade">
                   <table>
                   <tr><td><b>Tax Group</b></td><td><b>Code</b></td><td><b>Descriprion</b></td><td><b>Tax Rate</b></td><td><b>Tax frt</b></td>
                   <td><b>Sales Tax Account</b></td></tr>                               
                   
                            
                    <tr><td><asp:TextBox ID="taxgrpTextBox" ReadOnly="true" BackColor="turquoise" Width="100px" MaxLength="3" runat="server" Text='<%# Bind("taxgrp") %>' /></td>        
                    <td><asp:TextBox ID="code1TextBox" runat="server" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" Width="50px" Text='<%# Bind("code1") %>' />
                    <a href="#" tabindex="1" onClick="taxcodelook(1); return false"><asp:Image ID="Image14" runat="server" ImageUrl="images/lookup_icon.gif" /></a></td>        
                    <td><asp:TextBox ID="dscr1TextBox" runat="server" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" Width="140px" Text='<%# Bind("dscr1") %>' /></td>        
                    <td><asp:TextBox ID="rate1TextBox" runat="server" onkeyup="ratetext()" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" MaxLength="7" Width="60px" Text='<%# Bind("rate1") %>' />
                    <asp:CompareValidator ID="CompareValidator4" ControlToValidate="rate1TextBox" Display="Dynamic" SetFocusOnError="true" Operator="DataTypeCheck" Type="Double" runat="server" ErrorMessage="Enter Valid Value"></asp:CompareValidator></td>                            
                    <td><asp:TextBox ID="frt1TextBox" runat="server" Visible="false" Width="120px" Text='<%# Bind("frt1") %>' />
                        <asp:CheckBox ID="CheckBox1" runat="server" /></td>
                    <td><asp:TextBox ID="acclTextBox" runat="server" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" Text='<%# Bind("accl") %>' />
                    <a href="#" tabindex="1" onClick="AccountLook(1); return false"><asp:Image ID="Image13" runat="server" ImageUrl="images/lookup_icon.gif" /></a></td></tr>
                    
                    <tr><td></td><td><asp:TextBox ID="code2TextBox" Width="50px" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" runat="server" Text='<%# Bind("code2") %>' />
                    <a href="#" tabindex="1" onClick="taxcodelook(2); return false"><asp:Image ID="Image1" runat="server" ImageUrl="images/lookup_icon.gif" /></a></td>
                    <td><asp:TextBox ID="dscr2TextBox" runat="server" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" Width="140px" Text='<%# Bind("dscr2") %>' /></td>
                    <td><asp:TextBox ID="rate2TextBox" runat="server" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" onkeyup="ratetext2()" MaxLength="7"  Width="60px" Text='<%# Bind("rate2") %>' />
                    <asp:CompareValidator ID="CompareValidator1" ControlToValidate="rate2TextBox" Display="Dynamic" SetFocusOnError="true" Operator="DataTypeCheck" Type="Double" runat="server" ErrorMessage="Enter Valid Value"></asp:CompareValidator></td>
                    <td><asp:TextBox ID="frt2TextBox" runat="server" Visible="false" Width="120px" Text='<%# Bind("frt2") %>' />
                    <asp:CheckBox ID="CheckBox2" runat="server" /></td>
                    <td><asp:TextBox ID="acc2TextBox" runat="server" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" Text='<%# Bind("acc2") %>' />
                    <a href="#" tabindex="1" onClick="AccountLook(2); return false"><asp:Image ID="Image5" runat="server" ImageUrl="images/lookup_icon.gif" /></a></td></tr> 
                    
                    <tr><td></td><td><asp:TextBox ID="code3TextBox" Width="50px" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" runat="server" Text='<%# Bind("code3") %>' />
                    <a href="#" tabindex="1" onClick="taxcodelook(3); return false"><asp:Image ID="Image2" runat="server" ImageUrl="images/lookup_icon.gif" /></a></td>
                    <td><asp:TextBox ID="dscr3TextBox" runat="server" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" Width="140px" Text='<%# Bind("dscr3") %>' /></td>
                    <td><asp:TextBox ID="rate3TextBox" runat="server" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" onkeyup="ratetext3()" MaxLength="7"  Width="60px" Text='<%# Bind("rate3") %>' />
                    <asp:CompareValidator ID="CompareValidator2" ControlToValidate="rate3TextBox" Display="Dynamic" SetFocusOnError="true" Operator="DataTypeCheck" Type="Double" runat="server" ErrorMessage="Enter Valid Value"></asp:CompareValidator></td>
                    <td><asp:TextBox ID="frt3TextBox" runat="server" Visible="false" Width="120px" Text='<%# Bind("frt3") %>' />
                    <asp:CheckBox ID="CheckBox3" runat="server" /></td>
                    <td><asp:TextBox ID="acc3TextBox" runat="server" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" Text='<%# Bind("acc3") %>' />
                    <a href="#" tabindex="1" onClick="AccountLook(3); return false"><asp:Image ID="Image6" runat="server" ImageUrl="images/lookup_icon.gif" /></a></td></tr> 
                    
                    <tr><td></td><td><asp:TextBox ID="code4TextBox" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" Width="50px"  runat="server" Text='<%# Bind("code4") %>' />
                    <a href="#" tabindex="1" onClick="taxcodelook(4); return false"><asp:Image ID="Image3" runat="server" ImageUrl="images/lookup_icon.gif" /></a></td>
                    <td><asp:TextBox ID="dscr4TextBox" runat="server" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" Width="140px" Text='<%# Bind("dscr4") %>' /></td>
                    <td><asp:TextBox ID="rate4TextBox" runat="server" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" Width="60px" onkeyup="ratetext4()" MaxLength="7"  Text='<%# Bind("rate4") %>' />
                    <asp:CompareValidator ID="CompareValidator3" ControlToValidate="rate4TextBox" Display="Dynamic" SetFocusOnError="true" Operator="DataTypeCheck" Type="Double" runat="server" ErrorMessage="Enter Valid Value"></asp:CompareValidator></td>
                    <td><asp:TextBox ID="frt4TextBox" runat="server" Visible="false" Width="120px" Text='<%# Bind("frt4") %>' />
                    <asp:CheckBox ID="CheckBox4" runat="server" /></td>
                    <td><asp:TextBox ID="acc4TextBox" runat="server" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" Text='<%# Bind("acc4") %>' />
                    <a href="#" tabindex="1" onClick="AccountLook(4); return false"><asp:Image ID="Image7" runat="server" ImageUrl="images/lookup_icon.gif" /></a></td></tr> 
                    
                    <tr><td></td><td><asp:TextBox ID="code5TextBox" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" Width="50px"  runat="server" Text='<%# Bind("code5") %>' />
                    <a href="#" tabindex="1" onClick="taxcodelook(5); return false"><asp:Image ID="Image4" runat="server" ImageUrl="images/lookup_icon.gif" /></a></td>
                    <td><asp:TextBox ID="dscr5TextBox" runat="server" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" Width="140px" Text='<%# Bind("dscr5") %>' /></td>
                    <td><asp:TextBox ID="rate5TextBox" runat="server" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" Width="60px" onkeyup="ratetext5()" MaxLength="7"  Text='<%# Bind("rate5") %>' />
                    <asp:CompareValidator ID="CompareValidator5" ControlToValidate="rate5TextBox" Display="Dynamic" SetFocusOnError="true" Operator="DataTypeCheck" Type="Double" runat="server" ErrorMessage="Enter Valid Value"></asp:CompareValidator></td>
                    <td><asp:TextBox ID="frt5TextBox" runat="server" Visible="false" Width="120px" Text='<%# Bind("frt5") %>' />
                    <asp:CheckBox ID="CheckBox5" runat="server" /></td>
                    <td><asp:TextBox ID="acc5TextBox" runat="server" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" Text='<%# Bind("acc5") %>' />
                    <a href="#" tabindex="1" onClick="AccountLook(5); return false"><asp:Image ID="Image8" runat="server" ImageUrl="images/lookup_icon.gif" /></a></td></tr>
                    
                    <tr><td></td><td><asp:CheckBox ID="taxTextBox" Text="Tax on Tax" runat="server" /></td><td></td></tr>
                    
                            <asp:Label ID="textlabel" runat="server" Visible="false" Text='<%# Bind("tax") %>'></asp:Label>
                            <asp:TextBox ID="reckeyTextBox" Visible="false" runat="server" Text='<%# Bind("reckey") %>' />
                            
                            <br />
                            </table>
                            <asp:Button ID="UpdateButton" runat="server" CausesValidation="True"  OnClick="UpdateButton_Click"
                                CssClass="button" Text="Save" />
                            &nbsp;<asp:Button ID="UpdateCancelButton" runat="server" CssClass="button"
                                CausesValidation="False" CommandName="Cancel" Text="Cancel" />
                                </fieldset>
                                </asp:Panel>
                 </EditItemTemplate>
                 <InsertItemTemplate>
                 <asp:Panel ID="insertPanel"  DefaultButton="InsertButton" runat="server" >
                                   
                  <fieldset class="shade">
                   <table>
                   <tr><td><b>Tax Group</b></td><td><b>Code</b></td><td><b>Descriprion</b></td><td><b>Tax Rate</b></td><td><b>Tax frt</b></td>
                   <td><b>Sales Tax Account</b></td></tr>                               
                   
                            
                    <tr><td><asp:TextBox ID="taxgrpTextBox" Width="100px" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" MaxLength="3" runat="server" Text='<%# Bind("taxgrp") %>' /></td>        
                    <td><asp:TextBox ID="code1TextBox" runat="server" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" Width="50px" Text='<%# Bind("code1") %>' />
                    <a href="#" tabindex="1" onClick="taxcodelook(1); return false"><asp:Image ID="Image14" runat="server" ImageUrl="images/lookup_icon.gif" /></a></td>        
                    <td><asp:TextBox ID="dscr1TextBox" runat="server" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" Width="140px" Text='<%# Bind("dscr1") %>' /></td>        
                    <td><asp:TextBox ID="rate1TextBox" runat="server" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" onkeyup="ratetext()" MaxLength="7" Width="60px" Text='<%# Bind("rate1") %>' />
                    <asp:CompareValidator ID="CompareValidator4" ControlToValidate="rate1TextBox" Display="Dynamic" SetFocusOnError="true" Operator="DataTypeCheck" Type="Double" runat="server" ErrorMessage="Enter Valid Value"></asp:CompareValidator></td>                            
                    <td><asp:TextBox ID="frt1TextBox" runat="server" Visible="false" Width="120px" Text='<%# Bind("frt1") %>' />
                        <asp:CheckBox ID="CheckBox1" runat="server" /></td>
                    <td><asp:TextBox ID="acclTextBox" runat="server" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" Text='<%# Bind("accl") %>' />
                    <a href="#" tabindex="1" onClick="AccountLook(1); return false"><asp:Image ID="Image8" runat="server" ImageUrl="images/lookup_icon.gif" /></a></td></tr>
                    
                    <tr><td></td><td><asp:TextBox ID="code2TextBox" Width="50px" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" runat="server" Text='<%# Bind("code2") %>' />
                    <a href="#" tabindex="1" onClick="taxcodelook(2); return false"><asp:Image ID="Image9" runat="server" ImageUrl="images/lookup_icon.gif" /></a></td>
                    <td><asp:TextBox ID="dscr2TextBox" runat="server" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" Width="140px" Text='<%# Bind("dscr2") %>' /></td>
                    <td><asp:TextBox ID="rate2TextBox" runat="server" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" Width="60px" onkeyup="ratetext2()" MaxLength="7" Text='<%# Bind("rate2") %>' />
                    <asp:CompareValidator ID="CompareValidator1" ControlToValidate="rate2TextBox" Display="Dynamic" SetFocusOnError="true" Operator="DataTypeCheck" Type="Double" runat="server" ErrorMessage="Enter Valid Value"></asp:CompareValidator></td>
                    <td><asp:TextBox ID="frt2TextBox" runat="server" Visible="false" Width="120px" Text='<%# Bind("frt2") %>' />
                    <asp:CheckBox ID="CheckBox2" runat="server" /></td>
                    <td><asp:TextBox ID="acc2TextBox" runat="server" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" Text='<%# Bind("acc2") %>' />
                    <a href="#" tabindex="1" onClick="AccountLook(2); return false"><asp:Image ID="Image15" runat="server" ImageUrl="images/lookup_icon.gif" /></a></td></tr> 
                    
                    <tr><td></td><td><asp:TextBox ID="code3TextBox" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" Width="50px"  runat="server" Text='<%# Bind("code3") %>' />
                    <a href="#" tabindex="1" onClick="taxcodelook(3); return false"><asp:Image ID="Image10" runat="server" ImageUrl="images/lookup_icon.gif" /></a></td>
                    <td><asp:TextBox ID="dscr3TextBox" runat="server" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" Width="140px" Text='<%# Bind("dscr3") %>' /></td>
                    <td><asp:TextBox ID="rate3TextBox" runat="server" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" Width="60px" onkeyup="ratetext3()" MaxLength="7" Text='<%# Bind("rate3") %>' />
                    <asp:CompareValidator ID="CompareValidator2" ControlToValidate="rate3TextBox" Display="Dynamic" SetFocusOnError="true" Operator="DataTypeCheck" Type="Double" runat="server" ErrorMessage="Enter Valid Value"></asp:CompareValidator></td>
                    <td><asp:TextBox ID="frt3TextBox" runat="server" Visible="false" Width="120px" Text='<%# Bind("frt3") %>' />
                    <asp:CheckBox ID="CheckBox3" runat="server" /></td>
                    <td><asp:TextBox ID="acc3TextBox" runat="server" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" Text='<%# Bind("acc3") %>' />
                    <a href="#" tabindex="1" onClick="AccountLook(3); return false"><asp:Image ID="Image16" runat="server" ImageUrl="images/lookup_icon.gif" /></a></td></tr> 
                    
                    <tr><td></td><td><asp:TextBox ID="code4TextBox" Width="50px" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" runat="server" Text='<%# Bind("code4") %>' />
                    <a href="#" tabindex="1" onClick="taxcodelook(4); return false"><asp:Image ID="Image11" runat="server" ImageUrl="images/lookup_icon.gif" /></a></td>
                    <td><asp:TextBox ID="dscr4TextBox" runat="server" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" Width="140px" Text='<%# Bind("dscr4") %>' /></td>
                    <td><asp:TextBox ID="rate4TextBox" runat="server" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" Width="60px" onkeyup="ratetext4()" MaxLength="7" Text='<%# Bind("rate4") %>' />
                    <asp:CompareValidator ID="CompareValidator3" ControlToValidate="rate4TextBox" Display="Dynamic" SetFocusOnError="true" Operator="DataTypeCheck" Type="Double" runat="server" ErrorMessage="Enter Valid Value"></asp:CompareValidator></td>
                    <td><asp:TextBox ID="frt4TextBox" runat="server" Visible="false" Width="120px" Text='<%# Bind("frt4") %>' />
                    <asp:CheckBox ID="CheckBox4" runat="server" /></td>
                    <td><asp:TextBox ID="acc4TextBox" runat="server" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" Text='<%# Bind("acc4") %>' />
                    <a href="#" tabindex="1" onClick="AccountLook(4); return false"><asp:Image ID="Image17" runat="server" ImageUrl="images/lookup_icon.gif" /></a></td></tr> 
                    
                    <tr><td></td><td><asp:TextBox ID="code5TextBox" Width="50px" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" runat="server" Text='<%# Bind("code5") %>' />
                    <a href="#" tabindex="1" onClick="taxcodelook(5); return false"><asp:Image ID="Image12" runat="server" ImageUrl="images/lookup_icon.gif" /></a></td>
                    <td><asp:TextBox ID="dscr5TextBox" runat="server" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" Width="140px" Text='<%# Bind("dscr5") %>' /></td>
                    <td><asp:TextBox ID="rate5TextBox" runat="server" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" Width="60px" onkeyup="ratetext5()" MaxLength="7" Text='<%# Bind("rate5") %>' />
                    <asp:CompareValidator ID="CompareValidator5" ControlToValidate="rate5TextBox" Display="Dynamic" SetFocusOnError="true" Operator="DataTypeCheck" Type="Double" runat="server" ErrorMessage="Enter Valid Value"></asp:CompareValidator></td>
                    <td><asp:TextBox ID="frt5TextBox" runat="server" Visible="false" Width="120px" Text='<%# Bind("frt5") %>' />
                    <asp:CheckBox ID="CheckBox5" runat="server" /></td>
                    <td><asp:TextBox ID="acc5TextBox" runat="server" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" Text='<%# Bind("acc5") %>' />
                    <a href="#" tabindex="1" onClick="AccountLook(5); return false"><asp:Image ID="Image18" runat="server" ImageUrl="images/lookup_icon.gif" /></a></td></tr>
                    
                    <tr><td></td><td><asp:CheckBox ID="taxTextBox" Text="Tax on Tax" runat="server"  /></td></tr>
                    
                      <asp:Label ID="textlabel" runat="server" Visible="false" Text='<%# Bind("tax") %>'></asp:Label>
                            <asp:TextBox ID="reckeyTextBox" runat="server" Visible="false" Text='<%# Bind("reckey") %>' />
                            
                            <br />
                            </table>
                            <br />
                            <asp:Button ID="InsertButton" runat="server" CausesValidation="True"  OnClick="addButton_Click"
                              CssClass="button" Text="Save" />
                            &nbsp;<asp:Button ID="InsertCancelButton" runat="server" CssClass="button"
                                CausesValidation="False" CommandName="Cancel" Text="Cancel" />
                                </fieldset></asp:Panel>
                 </InsertItemTemplate>
                 <ItemTemplate>
                 <fieldset class="shade">
                   <table>
                   <tr><td><b>Tax Group</b></td><td><b>Code</b></td><td><b>Descriprion</b></td><td><b>Tax Rate</b></td><td><b>Tax frt</b></td>
                   <td><b>Sales Tax Account</b></td></tr>                               
                   
                            
                    <tr><td><asp:Label ID="taxgrpLabel" Width="100px" BackColor="turquoise" runat="server" Text='<%# Bind("taxgrp") %>' /></td>        
                    <td><asp:Label ID="code1Label" BackColor="turquoise" runat="server" Width="50px" Text='<%# Bind("code1") %>' /></td>        
                    <td><asp:Label ID="dscr1Label" BackColor="turquoise" runat="server" Width="140px" Text='<%# Bind("dscr1") %>' /></td>        
                    <td><asp:Label ID="rate1Label" BackColor="turquoise" runat="server" Width="60px" Text='<%# Bind("rate1") %>' /></td>        
                    <td><asp:Label ID="frt1Label" BackColor="turquoise" runat="server" Visible="false" Width="120px" Text='<%# Bind("frt1") %>' />
                        <asp:CheckBox ID="CheckBox1" BackColor="turquoise" Enabled="false" runat="server" /></td>
                    <td><asp:Label ID="acclLabel" BackColor="turquoise" runat="server" Width="120px" Text='<%# Bind("accl") %>' /></td></tr>
                    
                    <tr><td></td><td><asp:Label ID="code2Label" BackColor="turquoise" Width="50px" runat="server" Text='<%# Bind("code2") %>' /></td>
                    <td><asp:Label ID="dscr2Label" BackColor="turquoise" runat="server" Width="140px" Text='<%# Bind("dscr2") %>' /></td>
                    <td><asp:Label ID="rate2Label" BackColor="turquoise" runat="server" Width="60px" Text='<%# Bind("rate2") %>' /></td>
                    <td><asp:Label ID="frt2Label" BackColor="turquoise" runat="server" Visible="false" Width="120px" Text='<%# Bind("frt2") %>' />
                    <asp:CheckBox ID="CheckBox2" BackColor="turquoise" Enabled="false" runat="server" /></td>
                    <td><asp:Label ID="acc2Label" BackColor="turquoise" Width="120px" runat="server" Text='<%# Bind("acc2") %>' /></td></tr> 
                    
                    <tr><td></td><td><asp:Label ID="code3Label" BackColor="turquoise" Width="50px"  runat="server" Text='<%# Bind("code3") %>' /></td>
                    <td><asp:Label ID="dscr3Label" runat="server" BackColor="turquoise" Width="140px" Text='<%# Bind("dscr3") %>' /></td>
                    <td><asp:Label ID="rate3Label" runat="server" BackColor="turquoise" Width="60px" Text='<%# Bind("rate3") %>' /></td>
                    <td><asp:Label ID="frt3Label" runat="server" Visible="false" Width="120px" Text='<%# Bind("frt3") %>' />
                    <asp:CheckBox ID="CheckBox3" BackColor="turquoise" Enabled="false" runat="server" /></td>
                    <td><asp:Label ID="acc3Label" runat="server" BackColor="turquoise" Width="120px" Text='<%# Bind("acc3") %>' /></td></tr> 
                    
                    <tr><td></td><td><asp:Label ID="code4Label" BackColor="turquoise" Width="50px"  runat="server" Text='<%# Bind("code4") %>' /></td>
                    <td><asp:Label ID="dscr4Label" runat="server" BackColor="turquoise" Width="140px" Text='<%# Bind("dscr4") %>' /></td>
                    <td><asp:Label ID="rate4Label" runat="server" BackColor="turquoise" Width="60px" Text='<%# Bind("rate4") %>' /></td>
                    <td><asp:Label ID="frt4Label" runat="server" BackColor="turquoise" Visible="false" Width="120px" Text='<%# Bind("frt4") %>' />
                    <asp:CheckBox ID="CheckBox4" BackColor="turquoise" Enabled="false" runat="server" /></td>
                    <td><asp:Label ID="acc4Label" runat="server" Width="120px" BackColor="turquoise" Text='<%# Bind("acc4") %>' /></td></tr> 
                    
                    <tr><td></td><td><asp:Label ID="code5Label" Width="50px" BackColor="turquoise"  runat="server" Text='<%# Bind("code5") %>' /></td>
                    <td><asp:Label ID="dscr5Label" runat="server" BackColor="turquoise" Width="140px" Text='<%# Bind("dscr5") %>' /></td>
                    <td><asp:Label ID="rate5Label" runat="server" BackColor="turquoise" Width="60px" Text='<%# Bind("rate5") %>' /></td>
                    <td><asp:Label ID="frt5Label" runat="server" Visible="false" Width="120px" Text='<%# Bind("frt5") %>' />
                    <asp:CheckBox ID="CheckBox5" BackColor="turquoise" Enabled="false" runat="server" /></td>
                    <td><asp:Label ID="acc5Label" runat="server" Width="120px" BackColor="turquoise" Text='<%# Bind("acc5") %>' /></td></tr>
                    
                    <tr><td></td><td><asp:CheckBox ID="taxTextBox" Enabled="false" BackColor="turquoise" Text="Tax on Tax" runat="server"  /></td><td></td></tr>
                    
                             <asp:Label ID="textlabel" runat="server" Text='<%# Bind("tax") %>'></asp:Label>
                            <asp:Label ID="reckeyLabel" runat="server" Visible="false" Text='<%# Bind("reckey") %>' />
                            
                            <br />
                            </table>
                            <br />
                            <asp:Button ID="AddButton" runat="server" CssClass="button" CommandName="New" Text="Add" />
                    <asp:Button ID="UpdateItemButton" runat="server" CssClass="button" CommandName="Edit" Text="Update" />
                   &nbsp;<asp:Button ID="DeleteButton" runat="server" CssClass="button" OnClientClick="return confirm('Are you sure you want to delete this record')" OnClick="Deletebutton_Click" Text="Delete" />
                </fieldset>
                </ItemTemplate>
                </asp:FormView>
  
          <asp:ObjectDataSource ID="ObjectDataSource1" runat="server" OldValuesParameterFormatString="original_{0}"
              SelectMethod="SelectSalesTaxCode" TypeName="voucherpay">
              <SelectParameters>
                  <asp:Parameter DefaultValue="View" Name="prmAction" Type="String" />
                  <asp:Parameter Name="prmComp" Type="String" />
                  <asp:Parameter Name="prmUser" Type="String"  DefaultValue="" />
                  <asp:Parameter Name="prmtaxgrp" Type="String" />                  
                  <asp:Parameter Name="prmcode1" Type="String" />
                  <asp:Parameter Name="prmdscr1" Type="String" />
                  <asp:Parameter Name="prmrate1" Type="Decimal" />
                  <asp:Parameter Name="prmaccl" Type="String" />
                  <asp:Parameter Name="prmfrt1" Type="String" />
                  <asp:Parameter Name="prmcode2" Type="String" />
                  <asp:Parameter Name="prmdscr2" Type="String" />
                  <asp:Parameter Name="prmrate2" Type="Decimal" />
                  <asp:Parameter Name="prmacc2" Type="String" />
                  <asp:Parameter Name="prmfrt2" Type="String" />
                  <asp:Parameter Name="prmcode3" Type="String" />
                  <asp:Parameter Name="prmdscr3" Type="String" />
                  <asp:Parameter Name="prmrate3" Type="Decimal" />
                  <asp:Parameter Name="prmacc3" Type="String" />
                  <asp:Parameter Name="prmfrt3" Type="String" />                 
                  <asp:Parameter Name="prmcode4" Type="String" />
                  <asp:Parameter Name="prmdscr4" Type="String" />
                  <asp:Parameter Name="prmrate4" Type="Decimal" />
                  <asp:Parameter Name="prmacc4" Type="String" />
                  <asp:Parameter Name="prmfrt4" Type="String" />
                  <asp:Parameter Name="prmcode5" Type="String" />
                  <asp:Parameter Name="prmdscr5" Type="String" />
                  <asp:Parameter Name="prmrate5" Type="Decimal" />
                  <asp:Parameter Name="prmacc5" Type="String" />
                  <asp:Parameter Name="prmfrt5" Type="String" />
                  <asp:Parameter Name="prmtax" Type="String" />
                  <asp:SessionParameter SessionField="salestax_code_reckey" Name="prmReckey" Type="String" />
              </SelectParameters>
          </asp:ObjectDataSource>
                                      
        
    </div></td></tr></table>
    <ft:footer id="Footer1" runat="server"></ft:footer>
    </form>
  </body>
</HTML>

