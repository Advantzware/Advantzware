<%@ Page Language="c#" AutoEventWireup="true" Debug="false" Inherits="view_gl_inquiry" Codebehind="view_gl_inquiry.aspx.cs" %>
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
		<ul nowrap> <li >
      <asp:LinkButton ID="lnk_Listcustomers" runat="server" OnClick="inl_list_inquiry" >GL Inquiry</asp:LinkButton></li>
      <li class="selected"><asp:LinkButton ID="lnk_viewcustomers" runat="server" OnClick="lnk_viewcustomers_Click" > GL Run Detail</asp:LinkButton></li></ul></div>
      <%--<asp:LinkButton ID="load_viewcustomers" runat="server" OnClick="load_viewjournals_Click" > <img src="Images/load journals 0.jpg" border="0" alt="View Invoices" /></asp:LinkButton>--%>
            
      </td>
      </tr></table>
     <asp:UpdatePanel id="gridviewupdatepanel" runat="server">
      <ContentTemplate>
       <div >
                <asp:UpdateProgress ID="UpdateProgress1" runat="server" 
                    AssociatedUpdatePanelID="gridviewupdatepanel"
                    DisplayAfter="100" DynamicLayout="true">                    
                    <ProgressTemplate>                       
                        <asp:Label ID="lblProgress" runat="server" ></asp:Label>               
                    Please wait ...             
                    </ProgressTemplate>                    
                </asp:UpdateProgress>
                </div>   
     
      
       <asp:Panel ID="searchpanel" runat="server" >    
      <TABLE id="tblMain" cellSpacing="1" cellPadding="1" width='500px' border="0">
        <TR>
          <TD class="shade"><fieldset>
            
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
                    <asp:CommandField ShowSelectButton="True" ButtonType="Image" SelectText="" SelectImageUrl="images\sel.gif" >
                    <ItemStyle Width="10px" />                    
                    </asp:CommandField>
                                                          
                    <asp:BoundField DataField="trnum" HeaderText="Run#" SortExpression="trnum" />
                    <asp:BoundField DataField="actnum" HeaderText="Account#" SortExpression="actnum" />
                    <asp:BoundField DataField="jrnl" HeaderText="Journal" SortExpression="jrnl" />
                    <asp:BoundField DataField="trdscr" HeaderText="Reference" SortExpression="trdscr" />
                    <asp:BoundField DataField="tramt" HeaderText="Amount" SortExpression="tramt" />
                    <asp:BoundField DataField="trdate" HeaderText="Date" SortExpression="trdate" />
                     <asp:TemplateField Visible="false"   HeaderText="invoice"  >
                    <ItemTemplate>      
                    <asp:Label id="invoiceLabel" runat="server" Text='<%# Bind("invoice") %>'></asp:Label>
                    </ItemTemplate>
                    </asp:TemplateField>
                    
                                                          
                </Columns>
                    <SelectedRowStyle CssClass="GridSelected" BackColor="Yellow" />
                    <HeaderStyle BackColor="Teal" CssClass="gridrowhdr" ForeColor="White" HorizontalAlign="Center"
                        VerticalAlign="Middle" Wrap="False" />
                    <AlternatingRowStyle CssClass="GridItemOdd" />
                    <RowStyle  CssClass="shade" />
            </asp:GridView>
            <asp:ObjectDataSource ID="ObjectDataSource1" runat="server" 
                OldValuesParameterFormatString="original_{0}" SelectMethod="ViewQueryGeneralLedgerhist" 
                TypeName="ledger">
                <SelectParameters>
                   <asp:Parameter Name="prmUser" Type="string" />
                   <asp:Parameter DefaultValue="Search" Name="prmAction" Type="String" />                    
                   <asp:SessionParameter SessionField="top_gl_inquiry_reckey" Name="prmtrnum"  Type="Int32" />                                        
                   <asp:Parameter Name="prmactnum" Type="String" />                   
                   <asp:Parameter Name="prmjrnl" Type="String" />
                   <asp:Parameter Name="prmtrdscr" Type="String" />                    
                   <asp:Parameter Name="prmtramt" Type="Int32" />
                   <asp:Parameter Name="prmtrdate" Type="String" /> 
                   <asp:Parameter Name="prmReckey" Type="String" />
                   
                </SelectParameters>
            </asp:ObjectDataSource></td></tr></TABLE></asp:Panel>
                        
          
          </div></td></tr></table>
          
    <asp:FormView ID="FormView1" runat="server" Visible="false" DataSourceID="ObjectDataSource3">
        
        <ItemTemplate>
            trnum:
            <asp:Label ID="trnumLabel" runat="server" Text='<%# Bind("trnum") %>' />
            <br />
            actnum:
            <asp:Label ID="actnumLabel" runat="server" Text='<%# Bind("actnum") %>' />
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
            trdate:
            <asp:Label ID="trdateLabel" runat="server" Text='<%# Bind("trdate") %>' />
            <br />
        </ItemTemplate>
    </asp:FormView>
    
      
      <asp:ObjectDataSource ID="ObjectDataSource3" runat="server" 
                OldValuesParameterFormatString="original_{0}" SelectMethod="ViewQueryGeneralLedgerhist" 
                TypeName="ledger">
                <SelectParameters>
                   <asp:Parameter Name="prmUser" Type="string" />
                   <asp:Parameter DefaultValue="Search" Name="prmAction" Type="String" />                    
                   <asp:Parameter Name="prmtrnum"  Type="Int32" />                                        
                   <asp:Parameter Name="prmactnum" Type="String" />                   
                   <asp:Parameter Name="prmjrnl" Type="String" />
                   <asp:Parameter Name="prmtrdscr" Type="String" />                    
                   <asp:Parameter Name="prmtramt" Type="Int32" />
                   <asp:Parameter Name="prmtrdate" Type="String" /> 
                   <asp:Parameter Name="prmReckey" Type="String" />
                   
                </SelectParameters>
            </asp:ObjectDataSource>
           </ContentTemplate>
                        </asp:UpdatePanel>        
          
     
    <ft:footer id="Footer1" runat="server"></ft:footer>
    </form>
  </body>
</HTML>

