<%@ Page Language="c#" AutoEventWireup="true" Debug="false" Inherits="gl_inquiry" Codebehind="gl_inquiry.aspx.cs" %>
<%@ Register Src="footer.ascx" TagName="Footer" TagPrefix="ft" %>
<%@ Register Src="header.ascx" TagName="Header" TagPrefix="hd" %>
<html xmlns="http://www.w3.org/1999/xhtml" >
  <head id="Head1" runat="server">
    <title>GL Inquiry</title>
    <LINK href="include/style2.css" type="text/css" rel="stylesheet"/>
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

function setdate() {

    var da = document.getElementById("date_TextBox");
    da.focus();

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

function AccountLook() {    
    var NewWindow = window.open("accountlook.aspx", "AccountLookupWindow", "width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}

function AccountLookup(ReturnObj1, ReturnObj2) {
    document.forms[0].actno_TextBox.value = ReturnObj1;
    document.forms[0].actdscr_TextBox.value = ReturnObj2;

}


    </script> 
  </head>    
   <body>
    <form id="frmList" runat="server"  defaultfocus='vendor_TextBox'>   
        <hd:header id="Header1" runat="server"></hd:header>
      
       <table width="100%"><tr><td><div>
        <table align="left" border="1" width="62%">
                <tr class="topheadcolor">                        
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
          <TD align=left nowrap><font size=+0><b>GL Inquiry  &nbsp;</b></font></TD>
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
      <table><tr bgcolor="gray"><td> <div  id="navigation" style="width:100%">
		<ul nowrap> <li class="selected">
      <asp:LinkButton ID="lnk_Listcustomers" runat="server" >GL Inquiry</asp:LinkButton></li>
      <li><asp:LinkButton ID="lnk_viewcustomers" runat="server" OnClick="lnk_viewcustomers_Click" > GL Run Detail</asp:LinkButton></li></ul></div>
      <%--<asp:LinkButton ID="load_viewcustomers" runat="server" OnClick="load_viewjournals_Click" > <img src="Images/load journals 0.jpg" border="0" alt="View Invoices" /></asp:LinkButton>--%>
            
      </td>
      </tr></table>
     
      
       <asp:Panel ID="searchpanel" runat="server" DefaultButton="btnSearch">    
      <TABLE id="tblMain" cellSpacing="1" cellPadding="1" width='500px' border="0">
        <TR>
          <TD class="shade"><fieldset>
            <TABLE id="tblSearch"  width="100%" border="0" class="shade" bgcolor="gray" >
              <TR >
                <TD class="shade"  width="50" class="shade" bgcolor="gray"><nobr>
                  <%--<asp:linkbutton id="btnAdd" runat="server" OnClick="btnAdd_Click">Add new</asp:linkbutton></nobr>--%>
                <asp:button id="btnSearch" runat="server" OnClick=" btnSearch_Click" Width="40px" CssClass="button" Text="Go" ></asp:button>
                <br />
                <br />
                <asp:Button runat="server" OnClick=" btnShowAll_Click" Width="40px" CssClass="button" Text="All"></asp:Button>
                <br />
                <br />
                  <asp:Button runat="server" OnClick="print_Click" CssClass="button" Text="Print"></asp:Button>
                </TD> 
                <td><table>
                <tr>
                <td align="right" style="padding-right:5px" nowrap ><b>
                Account Number:</b>
                    <asp:TextBox ID="actno_TextBox" Width="80px" MaxLength="8" runat="server"></asp:TextBox>
                    <a href="#" tabindex="1" onClick="AccountLook(); return false"><asp:Image ID="Image1" runat="server" ImageUrl="images/lookup_icon.gif" /></a>                    
                    <asp:TextBox ID="actdscr_TextBox" Width="170px" runat="server"></asp:TextBox>                                                           
                </td>
                </tr>
                <tr>
                <td align="right" style="padding-right:5px" nowrap><b> 
                 Year: </b>
                    <asp:TextBox ID="year_TextBox" Width="100px" MaxLength="4" runat="server"></asp:TextBox>                                   
                    <asp:CompareValidator ID="CompareValidator2" runat="server" ControlToValidate="year_TextBox" SetFocusOnError="true" Display="dynamic" Operator="dataTypeCheck" Type="Integer" ErrorMessage="Number Only"></asp:CompareValidator>         
                &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
                <b> Opening Balance: </b>
                <asp:TextBox ID="opbal_TextBox" Width="70px" runat="server"></asp:TextBox>
                <asp:CompareValidator ID="CompareValidator1" runat="server" ControlToValidate="opbal_TextBox" SetFocusOnError="true" Display="dynamic" Operator="dataTypeCheck" Type="Double" ErrorMessage="Number Only"></asp:CompareValidator>        </td>                
                </tr>
                <tr>
                <td align="right" style="padding-right:5px" nowrap>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
                <b> Closing Balance: </b>
                <asp:TextBox ID="clbal_TextBox" Width="70px" runat="server"></asp:TextBox>
                <asp:CompareValidator ID="CompareValidator3" runat="server" ControlToValidate="clbal_TextBox" SetFocusOnError="true" Display="dynamic" Operator="dataTypeCheck" Type="Double" ErrorMessage="Number Only"></asp:CompareValidator>        </td>        
                </tr>
                <tr>
                <td nowrap><b> Period Range: </b>
                    <asp:TextBox ID="bepdrn_TextBox" Width="50px" runat="server"></asp:TextBox>
                    <asp:CompareValidator ID="CompareValidator5" runat="server" ControlToValidate="bepdrn_TextBox" SetFocusOnError="true" Display="dynamic" Operator="dataTypeCheck" Type="Integer" ErrorMessage="Number Only"></asp:CompareValidator>                                           
                &nbsp;&nbsp;&nbsp;&nbsp;
                <asp:TextBox ID="endpdrn_TextBox" Width="50px" runat="server"></asp:TextBox>
                <asp:CompareValidator ID="CompareValidator6" runat="server" ControlToValidate="endpdrn_TextBox" SetFocusOnError="true" Display="dynamic" Operator="dataTypeCheck" Type="Integer" ErrorMessage="Number Only"></asp:CompareValidator>        </td>        
                </tr>
                </table></td>              
                 
                <TD id="tdPageCount" align="left" runat="server" class="shade" >
          <table><tr><td style="width:250px;" align="left">
           <b> Records/Page</b><BR>
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
          </td></tr></table>  
                </TD>
              </TR>
            </TABLE>
            <asp:label id="lblMessage" runat="server" ForeColor="Red"></asp:label></fieldset>
          </td>
        </tr>
       
        <tr><td> 
            <asp:GridView ID="GridView1" runat="server" DataSourceID="ObjectDataSource1" 
                EmptyDataText=" Records not  Found" AllowPaging="True" AllowSorting="True" 
                Width="650px" BorderStyle="Dotted" CssClass="Grid" 
                OnSelectedIndexChanged="GridView1_SelectedIndex" AutoGenerateColumns="False">
                <EmptyDataRowStyle BorderColor="Gray" BorderStyle="Dotted" BorderWidth="0px" Font-Bold="True"
                        HorizontalAlign="Center" VerticalAlign="Middle" />
                        
                <Columns>
                    <asp:CommandField ShowSelectButton="True" ButtonType="Image" SelectImageUrl="images\sel.gif" SelectText="" >
                    <ItemStyle Width="10px" />                    
                    </asp:CommandField>
                    
                    <asp:BoundField DataField="actnum" HeaderText="Account#" SortExpression="actnum" />                    
                    <asp:BoundField DataField="trdate" HeaderText="Date" SortExpression="trdate" />
                    <asp:BoundField DataField="jrnl" HeaderText="Ref#" SortExpression="jrnl" />        
                    <asp:BoundField DataField="trdscr" HeaderText="Description" SortExpression="trdscr" />            
                    <asp:BoundField DataField="tramt" HeaderText="Amount" SortExpression="tramt" />
                    <asp:BoundField DataField="trfrm" HeaderText="Inquiry From" SortExpression="trfrm" />                                                        
                    
                    
                    <asp:TemplateField Visible="false"  HeaderText="Reckey"  >
                    <ItemTemplate>      
                    <asp:Label id="reckeyLabel" runat="server" Text='<%# Bind("reckey") %>'></asp:Label>
                    </ItemTemplate>
                    </asp:TemplateField>
                    
                    <asp:TemplateField >
                    <ItemTemplate>                    
                    <asp:Label id="extraLabel" runat="server" Text='<%# Bind("extra") %>'></asp:Label>
                    </ItemTemplate>
                    </asp:TemplateField>
                                                          
                </Columns>
                    <SelectedRowStyle CssClass="GridSelected" BackColor="Yellow" />
                    <HeaderStyle ForeColor="White" CssClass="selected" HorizontalAlign="Center"
                        VerticalAlign="Middle" Wrap="False" />
                    <AlternatingRowStyle CssClass="GridItemOdd" />
                    <RowStyle  CssClass="shade" />
            </asp:GridView>
            <asp:ObjectDataSource ID="ObjectDataSource1" runat="server" 
                OldValuesParameterFormatString="original_{0}" SelectMethod="ListQueryGeneralLedgerHi" 
                TypeName="ledger">
                <SelectParameters>
                   <asp:Parameter Name="prmUser" Type="string" />
                   <asp:Parameter DefaultValue="Search" Name="prmAction" Type="String" />                    
                   <asp:SessionParameter Name="prmbegact" SessionField="gl_inquiry_account"  Type="string" />                                        
                   <asp:SessionParameter Name="prmglyear" SessionField="gl_inquiry_year" Type="Int32" />                   
                   <asp:SessionParameter Name="prmperiodfr" SessionField="gl_inquiry_frmprd" Type="Int32" />
                   <asp:SessionParameter Name="prmperiodto" SessionField="gl_inquiry_toprd" Type="Int32" />                    
                   <asp:SessionParameter Name="prmopnbal" SessionField="gl_inquiry_opbal" Type="Decimal" />
                   <asp:SessionParameter Name="prmclsbal" SessionField="gl_inquiry_clsbal" Type="Decimal" /> 
                   <asp:Parameter Name="prmextra" Type="String" />                    
                   <asp:Parameter Name="prmReckey" Type="String" />
                   
                </SelectParameters>
            </asp:ObjectDataSource></td></tr></TABLE></asp:Panel>
                        
          
          </div></td></tr></table>
          
    <asp:FormView ID="FormView2" runat="server" Visible="false" DataSourceID="ObjectDataSource3">
        
        <ItemTemplate>
            actnum:
            <asp:Label ID="actnumLabel" runat="server" Text='<%# Bind("actnum") %>' />
            <br />
            actdscr:
            <asp:Label ID="actdscrLabel" runat="server" Text='<%# Bind("actdscr") %>' />
            <br />
            trdate:
            <asp:Label ID="trdateLabel" runat="server" Text='<%# Bind("trdate") %>' />
            <br />
            jrnl:
            <asp:Label ID="jrnlLabel" runat="server" Text='<%# Bind("jrnl") %>' />
            <br />
            trdscr:
            <asp:Label ID="trdscrLabel" runat="server" Text='<%# Bind("trdscr") %>' />
            <br />
            tramt:
            <asp:Label ID="tramtLabel" runat="server" Text='<%# Bind("tramt") %>' />
            <br />
            trfrm:
            <asp:Label ID="trfrmLabel" runat="server" Text='<%# Bind("trfrm") %>' />
            <br />
            reckey:
            <asp:Label ID="reckeyLabel" runat="server" Text='<%# Bind("reckey") %>' />
            <br />
            extra:
            <asp:Label ID="extraLabel" runat="server" Text='<%# Bind("extra") %>' />
            <br />
        </ItemTemplate>
    </asp:FormView>
    
      
      <asp:ObjectDataSource ID="ObjectDataSource3" runat="server" 
                OldValuesParameterFormatString="original_{0}" SelectMethod="ListQueryGeneralLedgerHi" 
                TypeName="ledger">
                <SelectParameters>
                   <asp:Parameter Name="prmUser" Type="string" />
                   <asp:Parameter Name="prmAction" Type="String" />                    
                   <asp:Parameter Name="prmbegact"  Type="string" />                                        
                   <asp:Parameter Name="prmglyear" Type="Int32" />                   
                   <asp:Parameter Name="prmperiodfr" Type="Int32" />
                   <asp:Parameter Name="prmperiodto" Type="Int32" />                    
                   <asp:Parameter Name="prmopnbal" Type="Decimal" />
                   <asp:Parameter Name="prmclsbal" Type="Decimal" /> 
                   <asp:Parameter Name="prmextra" Type="String" />                    
                   <asp:Parameter Name="prmReckey" Type="String" />
                   
                </SelectParameters>
            </asp:ObjectDataSource>
            
          
     
    <ft:footer id="Footer1" runat="server"></ft:footer>
    </form>
  </body>
</HTML>

