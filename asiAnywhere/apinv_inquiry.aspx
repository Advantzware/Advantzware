<%@ Page Language="c#" AutoEventWireup="true" Debug="false" Inherits="apinv_inquiry" Codebehind="apinv_inquiry.aspx.cs" %>
<%@ Register Src="footer.ascx" TagName="Footer" TagPrefix="ft" %>
<%@ Register Src="header.ascx" TagName="Header" TagPrefix="hd" %>
<html xmlns="http://www.w3.org/1999/xhtml" >
  <head id="Head1" runat="server">
    <title>Vendor Invoice Inquiry</title>
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
function VendLookup(ReturnObj1) {
    document.forms[0].vendor_TextBox.value = ReturnObj1;
    
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


    </script> 
  </head>    
   <body>
    <form id="frmList" runat="server"  defaultfocus='Inv_TextBox'>   
        <hd:header id="Header1" runat="server"></hd:header>
      
       <table width="100%"><tr><td><div>
        <table align="left" border="1" width="75%">
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
          <TD align=left nowrap><font size=+0><b>Vendor Invoice Inquiry&nbsp;</b></font></TD>
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
      <asp:LinkButton ID="lnk_Listcustomers" runat="server" >Brws Invoices </asp:LinkButton></li>
      <li><asp:LinkButton ID="lnk_viewcustomers" runat="server" OnClick="lnk_viewcustomers_Click" >View Invoices</asp:LinkButton></li></ul></div>
            
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
       <asp:Panel ID="searchpanel" runat="server" DefaultButton="btnSearch">    
      <TABLE id="tblMain" cellSpacing="1" cellPadding="1" width='650px' border="0">
        <TR>
          <TD><fieldset>
            <TABLE id="tblSearch" cellSpacing="1" cellPadding="5" width="100%" border="0" class="shade" bgcolor="gray" >
              <TR >
                <TD class="shade" align="center" width="50" class="shade" bgcolor="gray"><nobr>
                  <%--<asp:linkbutton id="btnAdd" runat="server" OnClick="btnAdd_Click">Add new</asp:linkbutton></nobr>--%>
                <asp:button id="btnSearch" runat="server" OnClick=" btnSearch_Click" Width="40px" CssClass="button" Text="Go" ></asp:button>
                <br />
                <br />
                  <asp:button id="btnShowAll" runat="server" OnClick=" btnShowAll_Click" Width="40px" CssClass="button" Text="All" ></asp:button>
                </TD>               
                          
                <TD class="shade" bgcolor="gray" >&nbsp;                
                <b>Invoice</b> <br />
                <asp:TextBox ID="Inv_TextBox" Width="70px" runat="server"></asp:TextBox></td>
                <td class="shade" bgcolor="gray" ><b> Vendor #</b><br />
                    <asp:TextBox ID="vendor_TextBox" Width="70px" runat="server"></asp:TextBox>
                    <a href="#" tabindex="1" onclick="vendorlook(); return false"><asp:Image ID="Image4" runat="server" ImageUrl="images/lookup_icon.gif" /></a></td>
                    <td><b> Inv Date#</b><br />
                    <asp:TextBox ID="date_TextBox" Width="70px" runat="server" onfocus="javascript:preEnter( this, 'yes' );" onblur="javascript:preLeave( this, 'date', '99/99/9999' );"></asp:TextBox>
                    <a href="#" tabindex="1" onClick="showCalendarControl(date_TextBox); return false"><asp:Image ID="Image1" runat="server" ImageUrl="images/lookup_icon.gif" /></a></td>
                    
                    <td>
                   &nbsp;&nbsp; <b><asp:CheckBox ID="CheckBox1" Text="Posted" runat="server" /></b>
                   &nbsp;&nbsp; <b><asp:CheckBox ID="CheckBox2" Text="UnPosted" runat="server" /></b>
                </TD>               
                
                
                <TD id="tdPageCount" runat="server" class="shade" >
          <table><tr><td align="center">
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
            <asp:GridView ID="GridView1" runat="server" DataSourceID="ObjectDataSource1" EmptyDataText=" Records not  Found" AllowPaging="True" AllowSorting="True" Width="650px" AutoGenerateColumns="False" BorderStyle="Dotted" CssClass="Grid" OnSelectedIndexChanged="GridView1_SelectedIndex">
                <EmptyDataRowStyle BorderColor="Gray" BorderStyle="Dotted" BorderWidth="0px" Font-Bold="True"
                        HorizontalAlign="Center" VerticalAlign="Middle" />
                        
                <Columns>
                    <asp:CommandField ShowSelectButton="true" ButtonType="image" SelectImageUrl="~/Images/sel.gif" SelectText="" > 
                    <ItemStyle Width="10px" />
                    </asp:CommandField>
                    
                    <asp:BoundField DataField="invno" HeaderText="Invoice#" SortExpression="invno" />
                    <asp:BoundField DataField="vendno" HeaderText="Vendor#" SortExpression="vendno" />
                    <asp:BoundField DataField="invdate" HeaderText="Invoice Date" SortExpression="invdate" />
                    <asp:BoundField DataField="duedate" HeaderText="Due Date" SortExpression="duedate" />
                    <asp:BoundField DataField="net" HeaderText="Net" SortExpression="net" />                    
                    <asp:BoundField DataField="paid" HeaderText="Paid" SortExpression="paid" />
                    <asp:BoundField DataField="discount" HeaderText="Discount" SortExpression="discount" />
                    <asp:BoundField DataField="baldue" HeaderText="Balance Due" SortExpression="baldue" />
                    <asp:TemplateField HeaderText="Reckey" Visible="false" >
                    <ItemTemplate>
                    <asp:Label ID="reclabel" runat="server" Text='<%# Bind("[reckey]") %>'></asp:Label>
                    </ItemTemplate>
                    </asp:TemplateField>
                                                          
                </Columns>
                    <SelectedRowStyle CssClass="GridSelected" BackColor="Yellow" />
                    <HeaderStyle ForeColor="White" CssClass="headcolor" HorizontalAlign="Center"
                        VerticalAlign="Middle" Wrap="False" />
                    <AlternatingRowStyle CssClass="GridItemOdd" />
                    <RowStyle  CssClass="shade" />
            </asp:GridView>
            <asp:ObjectDataSource ID="ObjectDataSource1" runat="server" 
                OldValuesParameterFormatString="original_{0}" SelectMethod="SelectVendor" 
                TypeName="voucherpay">
                <SelectParameters>
                   <asp:Parameter DefaultValue="InqSearch" Name="prmAction" Type="String" />                    
                   <asp:Parameter Name="prmComp"  Type="string" />
                   <asp:Parameter Name="prmUser" Type="string" />                     
                   <asp:SessionParameter SessionField="vendor_invoice_vendor" Name="prmvend" Type="String" />
                   <asp:SessionParameter SessionField="vendor_invoice_inv" Name="prmInv" Type="String" />
                     
                   <asp:SessionParameter SessionField="vendor_invoice_posted" Name="prmPosted" Type="String" />
                   <asp:SessionParameter SessionField="vendor_invoice_unposted" Name="prmunPosted" Type="String" />
                   <asp:Parameter Name="prmvendname" Type="String" />
                   <asp:SessionParameter SessionField="vendor_invoice_invdate" Name="prmInvdate" Type="String" />
                   <asp:Parameter Name="prmnet" Type="Decimal" />
                   <asp:Parameter Name="prmPaid" Type="Decimal" />
                   <asp:Parameter Name="prmBaldue" Type="Decimal" />
                   <asp:Parameter Name="prmTaxamt" Type="Decimal" />
                   <asp:Parameter Name="prmDue" Type="Decimal" />
                   <asp:Parameter Name="prmDuedate" Type="String" />
                   <asp:Parameter Name="prmTaxcode" Type="String" />
                   <asp:Parameter Name="prmDiscount" Type="Decimal" />
                   <asp:Parameter Name="prmDiscdays" Type="Int32" />
                   <asp:Parameter Name="prmCurrcode" Type="String" />
                   <asp:Parameter Name="prmExrate" Type="Decimal" />
                   <asp:Parameter Name="prmMnlchac" Type="String" />
                   <asp:Parameter Name="prmTxovrwrt" Type="String" />
                   <asp:Parameter Name="prmFreight" Type="Decimal" />
                    <asp:Parameter Name="prmReckey" Type="String" />
                </SelectParameters>
            </asp:ObjectDataSource></td></tr></TABLE></asp:Panel>
                        </ContentTemplate>
                        </asp:UpdatePanel>
          
          </div></td></tr></table>
      
     
    <ft:footer id="Footer1" runat="server"></ft:footer>
    </form>
  </body>
</HTML>

