<%@ Page Language="c#" AutoEventWireup="true" Debug="true" Inherits="view_recr_gljrn" Codebehind="view_recr_gljrn.aspx.cs" %>
<%@ Register Src="footer.ascx" TagName="Footer" TagPrefix="ft" %>
<%@ Register Src="header.ascx" TagName="Header" TagPrefix="hd" %>
<html xmlns="http://www.w3.org/1999/xhtml" >
  <head id="Head1" runat="server">
    <title>General Ledger Recurring Journal Entries</title>
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
        vname.innerHTML = ReturnObj2;
        document.forms[0].FormView1_vendnoTextBox.focus();
    }

    function AccountLook() {
        var NewWindow = window.open("accountlook.aspx?jrnl="+ "journal" +" ", "AccountLookupWindow", "width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
    }

    function AccountLookup(ReturnObj1, ReturnObj2) {
        document.forms[0].FormView2_actnumTextBox.value = ReturnObj1;
        var actdsc = document.getElementById("FormView2_actdscrlabel");


        if (document.forms[0].FormView2_actdscrLabel)
            document.forms[0].FormView2_actdscrLabel.value = ReturnObj2;
        else
            actdsc.innerHTML = ReturnObj2;

    }

    function invinflook() {
        
         var vend = document.getElementById("FormView1_vendnoLabel");
       
        var NewWindow = window.open("invinfo_lookup.aspx?vend=" + vend.innerHTML + "", "invinfoWindow", "width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
        //var NewWindow = window.open("invinfo_lookup.aspx", "invinfoWindow", "width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
    }

    function InvInfoLookup(ReturnObj1, ReturnObj2, ReturnObj3, ReturnObj4, ReturnObj5, ReturnObj6) {
        document.forms[0].FormView2_invTextBox.value = ReturnObj1;
        document.forms[0].FormView2_duedateLabel.value = ReturnObj2;
        document.forms[0].FormView2_baldueLabel.value = ReturnObj3;
        document.forms[0].FormView2_cramtTextBox.value = ReturnObj4;
        document.forms[0].FormView2_dbamtTextBox.value = ReturnObj5;
        document.forms[0].FormView2_actnumTextBox.value = ReturnObj6;
       

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
       var NewWindow = window.open("top_list_notes.aspx", "ListNotes", "width=600,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
   }

   function invfocus() {
       var inv = document.getElementById("FormView2_dscrTextBox");
       inv.focus();
   }
   function cramtfocus() {
       var crt = document.getElementById("FormView2_cramtTextBox");
       crt.focus();
   }
   function datefocus() {
       var vdate = document.getElementById("FormView1_CheckBox1");
       vdate.focus();
   }
   function dateper() {
       var vdate = document.getElementById("FormView1_dateTextBox");
       var per = document.getElementById("FormView1_periodTextBox")
       per.value = (vdate.value).substring(0, 2);
   }

    </script> 
  </head>    
   <body>
    <form id="frmList" runat="server"  defaultfocus='Inv_TextBox'>   
        <hd:header id="Header1" runat="server"></hd:header>
           <asp:ScriptManager ID="ScriptManager1" runat="server">
          </asp:ScriptManager>
         
         <table width="100%"><tr><td><div> 
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
          <TD align=left nowrap><font size=+0><b>General Ledger Recurring Journal Entries&nbsp;</b></font></TD>
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
      <asp:LinkButton ID="lnk_Listcustomers" runat="server" OnClick="lnk_listinvoice" >Brws Journals</asp:LinkButton></li>
      <li class="selected"><asp:LinkButton ID="lnk_viewcustomers" runat="server" OnClick="lnk_viewcustomers_Click" > View Journals </asp:LinkButton></li>
      <li><asp:LinkButton ID="load_viewcustomers" runat="server" OnClick="load_viewjournals_Click" >Load Journals</asp:LinkButton></li></ul></div>
            
      </td>
      </tr></table>
       <div>
           <asp:FormView ID="FormView1" runat="server" OnDataBound="FormView1_OnDataBound" DataSourceID="ObjectDataSource1">
               <EditItemTemplate>
               <fieldset class="shade" style="width:500px;"><table class="shade">               
               <tr><td align="right" style="padding-right:5px"><b>Journal#:</b></td>
               <td><asp:TextBox ID="jrnlTextBox" Width="100px" onfocus="datefocus()" ForeColor="#ACA899" runat="server" Text='<%# Bind("jrn_no") %>' />
               </td>
               <td align="right" style="padding-right:5px"><b>Total Credits:</b></td>
               <td colspan="2"><asp:TextBox ID="tcrdtTextBox" Width="100" onfocus="datefocus()" ForeColor="#ACA899" runat="server" Text='<%# Bind("t_crd") %>' /></td></tr>
               <tr><td></td><td></td><%--<td align="right" style="padding-right:5px"><b>Date:</b></td>
               <td><asp:TextBox ID="dateTextBox" runat="server" Width="100px" onfocus="javascript:preEnter( this, 'yes' );" onblur="javascript:preLeave( this, 'date', '99/99/9999' );dateper();" Text='<%# Bind("tr_date") %>' />
               <a href="#" tabindex="1" onClick="showCalendarControl(FormView1_dateTextBox); return false"><asp:Image ID="Image1" runat="server" ImageUrl="images/lookup_icon.gif" /></a></td>--%>
               <td align="right" style="padding-right:5px"><b>Total Debits:</b></td>
               <td><asp:TextBox ID="tdebitTextBox" Width="100px"  ForeColor="#ACA899" onfocus="datefocus()" runat="server" Text='<%# Bind("t_deb") %>' /></td></tr>
               <tr><td></td><td></td><%--<td align="right" style="padding-right:5px"><b>Period:</b></td>
               <td><asp:TextBox ID="periodTextBox" runat="server" onfocus="datefocus()" ForeColor="#ACA899" Width="100px" Text='<%# Bind("period") %>' /></td>--%>
               <td align="right" style="padding-right:5px"><b>Balance:</b></td>
               <td><asp:TextBox ID="balanTextBox" runat="server" onfocus="datefocus()" ForeColor="#ACA899" Width="100px" Text='<%# Bind("t_amt") %>' /></td></tr>
               
               </td></tr></table>
               <table class="shade"><tr><td colspan="2"><b>
               <asp:CheckBox ID="CheckBox1" Text="Reverse This Entry?" runat="server" /></b>
                   <asp:Label ID="reverLabel" Visible="false" BackColor="Turquoise"  runat="server" Text='<%# Bind("reverse") %>' />
                 <td align="right" style="padding-right:5px"><b>Frequency:</b></td>  
                   <td> 
                <asp:DropDownList ID="DropDownList1" Width="100px" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)"  SelectedValue='<%# Bind("cb_freq") %>' DataTextField='<%# Bind("cb_freq") %>' runat="server">
                                                    <asp:ListItem Value="Daily">Daily</asp:ListItem>
                                                    <asp:ListItem Value="Weekly">Weekly</asp:ListItem>
                                                    <asp:ListItem Value="Monthly">Monthly</asp:ListItem>
                                                    <asp:ListItem Value="Annually">Annually</asp:ListItem>
                                                    <asp:ListItem Value="Bi-weekly">Bi-weekly</asp:ListItem>
                                                    <asp:ListItem Value="Bi-monthly">Bi-monthly</asp:ListItem>
                                                    <asp:ListItem Value="Bi-annually">Bi-annually</asp:ListItem>
                                                    <asp:ListItem Value="Intermittently">Intermittently</asp:ListItem>
                                                    <asp:ListItem Value=""></asp:ListItem>
                                                </asp:DropDownList>
               </td>
               <tr><td colspan="2"><b>
               <asp:CheckBox ID="CheckBox2" Text="Entry From Previous Reversing Entry?" runat="server" /></b>
                   <asp:Label ID="prereverLabel" Visible="false" BackColor="Turquoise"  runat="server" Text='<%# Bind("from_revr") %>' />                               
               </td></tr>               
               </td></tr></table>  
               
               
                   <asp:Button ID="UpdateButton" runat="server" CausesValidation="True" CssClass="button" OnClick="UpdateButton_Click"  Text="Save" />
                   &nbsp;<asp:Button ID="UpdateCancelButton" runat="server" CssClass="button"
                       CausesValidation="False" CommandName="Cancel" Text="Cancel" />
                       </fieldset>
               </EditItemTemplate>
               <InsertItemTemplate>
                   <fieldset class="shade" style="width:500px;"><table class="shade">               
              <tr><td align="right" style="padding-right:5px"><b>Journal#:</b></td>
               <td><asp:TextBox ID="jrnlTextBox" Width="100px" onfocus="datefocus()" ForeColor="#ACA899" runat="server" Text='<%# Bind("jrn_no") %>' />
               </td>
               <td align="right" style="padding-right:5px"><b>Total Credits:</b></td>
               <td colspan="2"><asp:TextBox ID="tcrdtTextBox" Width="100" onfocus="datefocus()" ForeColor="#ACA899" runat="server" Text='<%# Bind("t_crd") %>' /></td></tr>
               <tr><td></td><td></td><%--<td align="right" style="padding-right:5px"><b>Date:</b></td>
               <td><asp:TextBox ID="dateTextBox" runat="server" Width="100px" onfocus="javascript:preEnter( this, 'yes' );" onblur="javascript:preLeave( this, 'date', '99/99/9999' );dateper();" Text='<%# Bind("tr_date") %>' />
               <a href="#" tabindex="1" onClick="showCalendarControl(FormView1_dateTextBox); return false"><asp:Image ID="Image1" runat="server" ImageUrl="images/lookup_icon.gif" /></a></td>--%>
               <td align="right" style="padding-right:5px"><b>Total Debits:</b></td>
               <td><asp:TextBox ID="tdebitTextBox" Width="100px" onfocus="datefocus()" ForeColor="#ACA899" runat="server" Text='<%# Bind("t_deb") %>' /></td></tr>
               <tr><td></td><td></td><%--<td align="right" style="padding-right:5px"><b>Period:</b></td>
               <td><asp:TextBox ID="periodTextBox" runat="server" onfocus="datefocus()" ForeColor="#ACA899" Width="100px" Text='<%# Bind("period") %>' /></td>--%>
               <td align="right" style="padding-right:5px"><b>Balance:</b></td>
               <td><asp:TextBox ID="balanTextBox" runat="server" onfocus="datefocus()" ForeColor="#ACA899" Width="100px" Text='<%# Bind("t_amt") %>' /></td></tr>
               
               </td></tr></table>
               <table class="shade"><tr><td colspan="2"><b>
               <asp:CheckBox ID="CheckBox1" Text="Reverse This Entry?" runat="server" /></b>
                   <asp:Label ID="reverLabel" Visible="false" BackColor="Turquoise"  runat="server" Text='<%# Bind("reverse") %>' />
                <td align="right" style="padding-right:5px"><b>Frequency:</b></td>   
                   <td> 
                <asp:DropDownList ID="DropDownList1" Width="100px" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)"  SelectedValue='<%# Bind("cb_freq") %>' DataTextField='<%# Bind("cb_freq") %>' runat="server">
                                                    <asp:ListItem Value="Daily">Daily</asp:ListItem>
                                                    <asp:ListItem Value="Weekly">Weekly</asp:ListItem>
                                                    <asp:ListItem Value="Monthly">Monthly</asp:ListItem>
                                                    <asp:ListItem Value="Annually">Annually</asp:ListItem>
                                                    <asp:ListItem Value="Bi-weekly">Bi-weekly</asp:ListItem>
                                                    <asp:ListItem Value="Bi-monthly">Bi-monthly</asp:ListItem>
                                                    <asp:ListItem Value="Bi-annually">Bi-annually</asp:ListItem>
                                                    <asp:ListItem Value="Intermittently">Intermittently</asp:ListItem>
                                                    <asp:ListItem Value=""></asp:ListItem>
                                                </asp:DropDownList>
               </td>
               <tr><td colspan="2"><b>
                <asp:CheckBox ID="CheckBox2" Text="Entry From Previous Reversing Entry?" runat="server" /></b>
                   <asp:Label ID="prereverLabel" Visible="false" BackColor="Turquoise"  runat="server" Text='<%# Bind("from_revr") %>' />                               
               </td></tr>               
               </td></tr></table>
                
               <br />
                   <asp:TextBox ID="Reckey_TextBox" Visible="false"  runat="server" Text='<%# Bind("reckey") %>' ></asp:TextBox>
                   <asp:Button ID="InsertButton" runat="server" CausesValidation="True" CssClass="button" OnClick="InsertButton_Click" Text="Save" />
                   &nbsp;<asp:Button ID="InsertCancelButton" runat="server" CssClass="button" OnClick="Formview1_InsertCancelButtonClick"
                       CausesValidation="False" CommandName="Cancel" Text="Cancel" />
                       </fieldset>
               </InsertItemTemplate>
               <ItemTemplate>
               <fieldset class="shade" style="width:500px;">
               <table class="shade">               
               <tr><td align="right" style="padding-right:5px"><b>Journal#:</b></td>
               <td><asp:Label ID="jrnlLabel" Width="100px" BackColor="Turquoise" BorderColor="White" BorderStyle="Solid" BorderWidth="1px" runat="server" Text='<%# Bind("jrn_no") %>' />
               </td>
               <td align="right" style="padding-right:5px"><b>Total Credits:</b></td>
               <td colspan="2"><asp:Label ID="tcrdtLabel" Width="100px" BackColor="Turquoise" BorderColor="White" BorderStyle="Solid" BorderWidth="1px" runat="server" Text='<%# Bind("t_crd") %>' /></td></tr>
               <tr><td></td><td></td><%--<td align="right" style="padding-right:5px"><b>Date:</b></td>
               <td><asp:Label ID="dateLabel" runat="server" Width="100px" BackColor="Turquoise" BorderColor="White" BorderStyle="Solid" BorderWidth="1px" Text='<%# Bind("tr_date") %>' />
               </td>--%>
               <td align="right" style="padding-right:5px"><b>Total Debits:</b></td>
               <td><asp:Label ID="tdebitLabel" Width="100px" BackColor="Turquoise" BorderColor="White" BorderStyle="Solid" BorderWidth="1px" runat="server" Text='<%# Bind("t_deb") %>' /></td></tr>
               <tr><td></td><td></td><%--<td align="right" style="padding-right:5px"><b>Period:</b></td>
               <td><asp:Label ID="periodLabel" Width="100px" runat="server" BackColor="Turquoise" BorderColor="White" BorderStyle="Solid" BorderWidth="1px" Text='<%# Bind("period") %>' /></td>--%>
               <td align="right" style="padding-right:5px"><b>Balance:</b></td>
               <td><asp:Label ID="balanLabel" Width="100px" runat="server" BackColor="Turquoise" BorderColor="White" BorderStyle="Solid" BorderWidth="1px" Text='<%# Bind("t_amt") %>' /></td></tr>
               
               </td></tr> </table>              
                   <table class="shade"><tr><td colspan="2"><b> 
                   <asp:CheckBox ID="CheckBox1" Enabled="false" Text="Reverse This Entry?"  runat="server" /></b>
                   <asp:Label ID="reverLabel" Visible="false" BackColor="Turquoise"  runat="server" Text='<%# Bind("reverse") %>' />
                   <td align="right" style="padding-right:5px"><b>Frequency:</b><asp:DropDownList ID="DropDownList1" Width="100px" Enabled="false"  SelectedValue='<%# Bind("cb_freq") %>' DataTextField='<%# Bind("cb_freq") %>' runat="server">
                                                    <asp:ListItem Value="Daily">Daily</asp:ListItem>
                                                    <asp:ListItem Value="Weekly">Weekly</asp:ListItem>
                                                    <asp:ListItem Value="Monthly">Monthly</asp:ListItem>
                                                    <asp:ListItem Value="Annually">Annually</asp:ListItem>
                                                    <asp:ListItem Value="Bi-weekly">Bi-weekly</asp:ListItem>
                                                    <asp:ListItem Value="Bi-monthly">Bi-monthly</asp:ListItem>
                                                    <asp:ListItem Value="Bi-annually">Bi-annually</asp:ListItem>
                                                    <asp:ListItem Value="Intermittently">Intermittently</asp:ListItem>
                                                    <asp:ListItem Value=""></asp:ListItem>
                                                </asp:DropDownList></td>
               <tr><td colspan="2"><b> 
               <asp:CheckBox ID="CheckBox2" Enabled="false" Text="Entry From Previous Reversing Entry?" runat="server" /></b>
                   <asp:Label ID="prereverLabel" Visible="false" BackColor="Turquoise"  runat="server" Text='<%# Bind("from_revr") %>' />                               
               </td></tr>               
               </td></tr></table>  
                  
                   
                   <asp:Label ID="reckeyLabel" Visible="false" runat="server" Text='<%# Bind("reckey") %>' />
                   
                   <br />
                   
                   <asp:Button ID="AddButton" runat="server" CssClass="button"  CommandName="new" Text="Add" />
                    <asp:Button ID="UpdateButton" runat="server" CssClass="button" CommandName="edit" Text="Update" />
                   &nbsp;<asp:Button ID="DeleteButton" runat="server" CssClass="button" OnClientClick="return confirm('Are you sure you want to delete this record')" OnClick="delete_Button_Click"  Text="Delete" />
                   </fieldset>
               </ItemTemplate>
           </asp:FormView>
           
           <asp:ObjectDataSource ID="ObjectDataSource1" runat="server" 
                OldValuesParameterFormatString="original_{0}" SelectMethod="RecrGeneralLedgerlist" 
                TypeName="ledger">
                <SelectParameters>
                                      
                   <asp:Parameter DefaultValue="View" Name="prmAction" Type="String" />                    
                   <asp:Parameter Name="prmComp"  Type="string" />
                   <asp:Parameter Name="prmUser" Type="string" />                     
                   <asp:Parameter Name="prmjrn_no" Type="Int32" />                   
                   <asp:Parameter Name="prmtr_date" Type="String" />                   
                   <asp:Parameter Name="prmperiod" Type="Int32" />
                   <asp:Parameter Name="prmt_deb" Type="Decimal" />
                   <asp:Parameter Name="prmt_crd" Type="Decimal" />
                   <asp:Parameter Name="prmt_amt" Type="Decimal" />
                   <asp:Parameter Name="prmreverse" Type="String" />
                   <asp:Parameter Name="prmfrom_revr" Type="String" />
                   <asp:Parameter Name="prmcb_freq" Type="String" />
                   <asp:Parameter Name="prmdscr" Type="String" />
                   <asp:SessionParameter SessionField="recr_general_ledger_reckey_journal" Name="prmReckey" Type="String" />
                </SelectParameters>
            </asp:ObjectDataSource>
       </div>    
       <div>
       <br />
        <asp:GridView ID="GridView1" runat="server" AllowPaging="True" AllowSorting="True" OnSelectedIndexChanged="GridView1_SelectedIndex"
        AutoGenerateColumns="False" CssClass="Grid" DataSourceID="ObjectDataSource2" DataKeyNames="reckey"
        EmptyDataText="No Record Found"  Width="630px">
        <EmptyDataRowStyle BorderColor="Gray" BorderStyle="Dotted" BorderWidth="0px" Font-Bold="True"
            HorizontalAlign="Center" VerticalAlign="Middle" />
        <RowStyle CssClass="shade" />
        <Columns>
             <asp:CommandField ShowSelectButton="true" ButtonType="image" SelectImageUrl="~/Images/sel.gif" SelectText="" > 
                    <ItemStyle Width="10px" />
                    </asp:CommandField>
            <asp:BoundField DataField="line_no" HeaderText="Line"  SortExpression="line_no" />              
            <asp:BoundField DataField="actnum" HeaderText="Account Number"  SortExpression="actnum" />
            <asp:BoundField DataField="actdscr" HeaderText="Account Name" SortExpression="actdscr" />           
            <asp:BoundField DataField="dscr" HeaderText="Description "    SortExpression="dscr" />
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
                   <asp:Panel ID="EditPanel" Width="560px" Height="120px" DefaultButton="UpdateButton" runat="server" >
                    
                   <fieldset class="shade">
                   <table>
                   <tr>
                   <td><b>Line:</b></td><td><b>Account Number:</b></td><td><b>Account Name:</b></td>
                   <td><b>Description:</b></td><td><b>Amount:</b></td>
                   </tr>
                   <tr><td>
                   <asp:TextBox ID="lineTextBox" runat="server" MaxLength="12" Width="40px" Text='<%# Bind("line_no") %>' />                   
                   </td>
                   <td nowrap><asp:TextBox ID="actnumTextBox" runat="server" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" Width="100px" MaxLength="25" Text='<%# Bind("actnum") %>' />
                   <a href="#" tabindex="1" onClick="AccountLook(); return false"><asp:Image ID="Image13" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
                   </td>
                   <td><asp:TextBox ID="actdscrLabel" runat="server" ReadOnly="true" ForeColor="#ACA899" onfocus="invfocus()" Width="150px" MaxLength="10" AutoPostBack="true" Text='<%# Bind("actdscr") %>' />
                   <td><asp:TextBox ID="dscrTextBox" runat="server" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" Width="120px" MaxLength="25" Text='<%# Bind("dscr") %>' /></td>
                   <td nowrap><asp:TextBox ID="amtTextBox" runat="server" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" Width="80px" MaxLength="25" Text='<%# Bind("amt") %>' />
                    <asp:CompareValidator ID="CompareValidator1" ControlToValidate="amtTextBox" SetFocusOnError="true" Operator="DataTypeCheck" Type="Double" Display="Dynamic" runat="server" ErrorMessage="Enter Decimal Values"></asp:CompareValidator>
                   </td>
                   <td><asp:TextBox ID="reckeyTextBox" Visible="false" runat="server" Text='<%# Bind("reckey") %>' /></td>
                  
                    </tr></table>                   
                   
                   <asp:Button ID="UpdateButton" runat="server" CausesValidation="True" CssClass="button" OnClick="UpdateButton_Formview2_Click" Text="Save" />
                   &nbsp;<asp:Button ID="UpdateCancelButton" runat="server"  CausesValidation="False" CssClass="button" CommandName="Cancel" Text="Cancel" />
                   </fieldset>
                   </asp:Panel>
               </EditItemTemplate>
               <InsertItemTemplate>
                   <asp:Panel ID="EditPanel" Width="560px" Height="120px" DefaultButton="InsertButton" runat="server" >
                    
                   <fieldset class="shade">
                   <table>
                   <tr>
                   <td><b>Line:</b></td><td><b>Account Number:</b></td><td><b>Account Name:</b></td>
                   <td><b>Description:</b></td><td><b>Amount:</b></td>
                   </tr>
                   <tr><td>
                   <asp:TextBox ID="lineTextBox" runat="server" MaxLength="12" Width="40px" Text='<%# Bind("line_no") %>' />                   
                   </td>
                   <td nowrap><asp:TextBox ID="actnumTextBox" runat="server" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" Width="100px" MaxLength="25" Text='<%# Bind("actnum") %>' />
                   <a href="#" tabindex="1" onClick="AccountLook(); return false"><asp:Image ID="Image13" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
                   </td>
                   <td><asp:TextBox ID="actdscrLabel" runat="server" ReadOnly="true" ForeColor="#ACA899" onfocus="invfocus()" Width="150px" MaxLength="10" AutoPostBack="true" Text='<%# Bind("actdscr") %>' />
                   <td><asp:TextBox ID="dscrTextBox" runat="server" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" Width="120px" MaxLength="25" Text='<%# Bind("dscr") %>' /></td>
                   <td nowrap><asp:TextBox ID="amtTextBox" runat="server" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this);" Width="80px" MaxLength="25" Text='<%# Bind("amt") %>' />
                       <asp:CompareValidator ID="CompareValidator1" ControlToValidate="amtTextBox" SetFocusOnError="true" Operator="DataTypeCheck" Type="Double" Display="Dynamic" runat="server" ErrorMessage="Enter Decimal Values"></asp:CompareValidator>
                   </td>
                   <td><asp:TextBox ID="reckeyTextBox" Visible="false" runat="server" Text='<%# Bind("reckey") %>' /></td>
                  
                    </tr></table> 
                    
                   <asp:Button ID="InsertButton" runat="server" CausesValidation="True" CssClass="button" OnClick="AddButton_Formview2_Click" Text="Save" />
                   &nbsp;<asp:Button ID="InsertCancelButton" runat="server" CssClass="button" OnClick="InsertCancelButton_Formview2_Click" CausesValidation="False" CommandName="Cancel" Text="Cancel" />
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
               OldValuesParameterFormatString="original_{0}" SelectMethod="GeneralLedgerView" 
               TypeName="ledger">
               <SelectParameters>
                   <asp:Parameter DefaultValue="View" Name="prmAction" Type="String" />
                   <asp:Parameter Name="prmComp" Type="String" />
                   <asp:Parameter Name="prmUser" Type="String" />
                   <asp:Parameter Name="prmline_no" Type="Int32" />                   
                   <asp:Parameter Name="prmactnum" Type="String" />
                   <asp:SessionParameter SessionField="recr_general_ledger_reckey_journal" Name="prmactdscr" Type="String" />
                   <asp:Parameter Name="prmdscr" Type="String" />
                   <asp:Parameter  Name="prmamt"  Type="Decimal" />                   
                   <asp:Parameter  Name="prmjrl_no" Type="Int32" />
                   <asp:SessionParameter SessionField="view_recrgeneral_ledger_reckey" Name="prmReckey"  Type="String" />
                                                     
                   
               </SelectParameters>
           </asp:ObjectDataSource>
    
    
    
           <asp:ObjectDataSource ID="ObjectDataSource2" runat="server" 
               OldValuesParameterFormatString="original_{0}" SelectMethod="GeneralLedgerView" 
               TypeName="ledger">
               <SelectParameters>
                   <asp:Parameter DefaultValue="SelectGrid" Name="prmAction" Type="String" />
                   <asp:Parameter Name="prmComp" Type="String" />
                   <asp:Parameter Name="prmUser" Type="String" />
                   <asp:Parameter Name="prmline_no" Type="Int32" />                   
                   <asp:Parameter Name="prmactnum" Type="String" />
                   <asp:Parameter Name="prmactdscr" Type="String" />
                   <asp:Parameter Name="prmdscr" Type="String" />
                   <asp:Parameter  Name="prmamt"  Type="Decimal" />                   
                   <asp:Parameter  Name="prmjrl_no" Type="Int32" />
                   <asp:SessionParameter SessionField="recr_general_ledger_reckey_journal" Name="prmReckey" Type="String" />
               </SelectParameters>
           </asp:ObjectDataSource>
       </div>
       
    </div>
    </td></tr></table>    
    <ft:footer id="Footer1" runat="server"></ft:footer>
    </form>
  </body>
</HTML>

