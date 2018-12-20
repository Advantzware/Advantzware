<%@ Page Language="c#" AutoEventWireup="true" Debug="false" Inherits="apinv_bal" Codebehind="apinv_bal.aspx.cs" %>
<%@ Register Src="footer.ascx" TagName="Footer" TagPrefix="ft" %>
<%@ Register Src="header.ascx" TagName="Header" TagPrefix="hd" %>
<html xmlns="http://www.w3.org/1999/xhtml" >
  <head id="Head1" runat="server">
    <title>AP Invoice Balances</title>
    <LINK href="include/style.css" type="text/css" rel="stylesheet"/>
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


    </script> 
  </head>    
   <body>
    <form id="frmList" runat="server"  defaultfocus='vendor_TextBox'>   
        <hd:header id="Header1" runat="server"></hd:header>
      
       <table width="100%"><tr><td><div>
        <table align="left" border="1" width="75%">
                <tr class="topheadcolor">
                        <td nowrap width="25px";>
                        <a href="#" onClick="topattach(); return false"><asp:Image ID="Image3" Width="35px" ToolTip="Attachment" runat="server" ImageUrl="~/Images/clip.ico" /></a>                        
                        </td>
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
          <TD align=left nowrap><font size=+0><b>AP Invoice Balances  &nbsp;</b></font></TD>
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
     
      
       <asp:Panel ID="searchpanel" runat="server" DefaultButton="btnSearch">    
      <TABLE id="tblMain" cellSpacing="1" cellPadding="1" width='600px' border="0">
        <TR>
          <TD class="shade"><fieldset>
            <TABLE id="tblSearch"  width="100%" border="0" class="shade" bgcolor="gray" >
              <TR >
                <TD class="shade"  width="50" class="shade" bgcolor="gray"><nobr>
                  <%--<asp:linkbutton id="btnAdd" runat="server" OnClick="btnAdd_Click">Add new</asp:linkbutton></nobr>--%>
                <asp:button id="btnSearch" runat="server" OnClick=" btnSearch_Click" Width="40px" CssClass="button" Text="Go" ></asp:button>
                <br />
                <br />
                  <asp:Button runat="server" OnClick="print_Click" CssClass="button" Text="Print"></asp:Button>
                </TD> 
                <td><table>
                <tr>
                <td nowrap ><b> &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
                Vendor:</b>
                    <asp:TextBox ID="vendor_TextBox" Width="80px" MaxLength="8" runat="server"></asp:TextBox>
                    <a href="#" tabindex="1" onclick="vendorlook(); return false"><asp:Image ID="Image4" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
                    <asp:TextBox ID="vendname_TextBox" Width="170px" runat="server"></asp:TextBox>                         
                      <b>&nbsp; Up To: </b>
                <asp:TextBox ID="TextBox1" Width="40px" runat="server"></asp:TextBox>      &nbsp;  <b>day only</b>            
                </td>
                </tr>
                <tr>
                <td nowrap><b> Beginning Invoice: </b>
                    <asp:TextBox ID="beginv_TextBox" Width="100px" MaxLength="12" runat="server"></asp:TextBox>                    
                
                &nbsp;<b> Ending Invoice: </b>
                <asp:TextBox ID="endinv_TextBox" Width="100px" MaxLength="12" runat="server"></asp:TextBox>                   
                 
                     <b><asp:CheckBox ID="CheckBox1" Text="Open Invoices Only?" runat="server" /></b></td>        
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
            <asp:GridView ID="GridView1" runat="server" DataSourceID="ObjectDataSource1" EmptyDataText=" Records not  Found" AllowPaging="True" AllowSorting="True" Width="700px" AutoGenerateColumns="False" BorderStyle="Dotted" CssClass="Grid" OnSelectedIndexChanged="GridView1_SelectedIndex">
                <EmptyDataRowStyle BorderColor="Gray" BorderStyle="Dotted" BorderWidth="0px" Font-Bold="True"
                        HorizontalAlign="Center" VerticalAlign="Middle" />
                        
                <Columns>
                    <asp:CommandField ShowSelectButton="True" ButtonType="Image" SelectText="" SelectImageUrl="images\sel.gif" >
                    <ItemStyle Width="10px" />                    
                    </asp:CommandField>
                    
                    <asp:BoundField DataField="invno" HeaderText="Invoice#" SortExpression="invno" />
                    <asp:BoundField DataField="chkno" HeaderText="Cheek#" SortExpression="chkno" />
                    <asp:BoundField DataField="trnsdate" HeaderText="Date" 
                        SortExpression="trnsdate" />
                    <asp:BoundField DataField="dscr" HeaderText="Description" SortExpression="dscr" />                    
                    <asp:BoundField DataField="credit" HeaderText="Credits" 
                        SortExpression="credit" />
                    <asp:BoundField DataField="debit" HeaderText="Debits" SortExpression="debit" />
                    
                    <asp:BoundField DataField="balance" HeaderText="Balance" 
                        SortExpression="balance" />
                    <asp:BoundField DataField="po" HeaderText="PO#s" SortExpression="po" />
                    
                    
                          <asp:TemplateField HeaderText="reckey" Visible="false">
                    <ItemTemplate>
                    <asp:Label ID="reclabel" runat="server" Text='<%# Bind("reckey") %>'></asp:Label>
                    </ItemTemplate>
                    </asp:TemplateField>
                    
                    <asp:TemplateField HeaderText="reckey" Visible="false" >
                        <ItemTemplate>
                    <asp:Label ID="templabel" runat="server" Text='<%# Bind("temp") %>'></asp:Label>
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
                OldValuesParameterFormatString="original_{0}" SelectMethod="BalanceInvoice" 
                TypeName="voucherpay">
                <SelectParameters>
                   <asp:Parameter DefaultValue="Search" Name="prmAction" Type="String" />                    
                   <asp:Parameter Name="prmComp"  Type="string" />
                   <asp:Parameter Name="prmUser" Type="string" />                     
                   <asp:Parameter Name="prmvend" Type="String" DefaultValue="" />                   
                   <asp:Parameter Name="prmInv" Type="String" />
                   <asp:Parameter Name="prmchkno" Type="String" />                    
                   <asp:Parameter Name="prmtrnsdate" Type="String" />
                   <asp:Parameter Name="prmdscr" Type="String" /> 
                   <asp:Parameter Name="prmcredit" Type="Decimal" /> 
                   <asp:Parameter Name="prmdebit" Type="Decimal" /> 
                   <asp:Parameter Name="prmbalance" Type="Decimal" /> 
                    <asp:Parameter Name="prmpo" Type="String" />
                    <asp:Parameter Name="prminvbal" Type="String" />
                    <asp:Parameter Name="prmvendname" Type="String" />
                    <asp:Parameter Name="prmbeginv" Type="String" />
                    <asp:Parameter Name="prmendinv" Type="String" />
                    <asp:Parameter Name="prmupto" Type="Int32" />
                    <asp:Parameter Name="prmvopen" Type="String" />
                    <asp:Parameter Name="prmshrtby" Type="String" />
                    <asp:Parameter Name="prmout" Type="String" />
                    <asp:Parameter Name="prmcon" Type="String" />
                   <asp:Parameter Name="prmReckey" Type="String" />
                   
                </SelectParameters>
            </asp:ObjectDataSource></td></tr></TABLE></asp:Panel>
                        
          
          </div></td></tr></table>
          
          
    <asp:FormView ID="FormView2" DataSourceID="ObjectDataSource3" Visible="false"
            runat="server"  OnPreRender="FormView2_PreRender" >
        
    <ItemTemplate>
        invno:
    <asp:Label ID="invnoLabel" runat="server" Text='<%# Bind("invno") %>' ></asp:Label>
        <br />
        chkno:
        <asp:Label ID="chknoLabel" runat="server" Text='<%# Bind("chkno") %>' />
        <br />
        trnsdate:
        <asp:Label ID="trnsdateLabel" runat="server" Text='<%# Bind("trnsdate") %>' />
        <br />
        dscr:
        <asp:Label ID="dscrLabel" runat="server" Text='<%# Bind("dscr") %>' />
        <br />
        credit:
        <asp:Label ID="creditLabel" runat="server" Text='<%# Bind("credit") %>' />
        <br />
        debit:
        <asp:Label ID="debitLabel" runat="server" Text='<%# Bind("debit") %>' />
        <br />
        balance:
        <asp:Label ID="balanceLabel" runat="server" Text='<%# Bind("balance") %>' />
        <br />
        po:
        <asp:Label ID="poLabel" runat="server" Text='<%# Bind("po") %>' />
        <br />
        invbal:
        <asp:Label ID="invbalLabel" runat="server" Text='<%# Bind("invbal") %>' />
        <br />
        vname:
        <asp:Label ID="vnameLabel" runat="server" Text='<%# Bind("vname") %>' />
        <br />
        temp:
        <asp:Label ID="tempLabel" runat="server" Text='<%# Bind("temp") %>' />
        <br />
        reckey:
        <asp:Label ID="reckeyLabel" runat="server" Text='<%# Bind("reckey") %>' />
        <br />
    </ItemTemplate>
    </asp:FormView>
      
      <asp:ObjectDataSource ID="ObjectDataSource3" runat="server" 
                OldValuesParameterFormatString="original_{0}" SelectMethod="BalanceInvoice" 
                TypeName="voucherpay">
                <SelectParameters>
                   <asp:Parameter Name="prmAction" Type="String" />                    
                   <asp:Parameter Name="prmComp"  Type="string" />
                   <asp:Parameter Name="prmUser" Type="string" />                     
                   <asp:Parameter Name="prmvend" Type="String" DefaultValue="" />                   
                   <asp:Parameter Name="prmInv" Type="String" />
                   <asp:Parameter Name="prmchkno" Type="String" />                    
                   <asp:Parameter Name="prmtrnsdate" Type="String" />
                   <asp:Parameter Name="prmdscr" Type="String" /> 
                   <asp:Parameter Name="prmcredit" Type="Decimal" /> 
                   <asp:Parameter Name="prmdebit" Type="Decimal" /> 
                   <asp:Parameter Name="prmbalance" Type="Decimal" /> 
                    <asp:Parameter Name="prmpo" Type="String" />
                    <asp:Parameter Name="prminvbal" Type="String" />
                    <asp:Parameter Name="prmvendname" Type="String" />
                    <asp:Parameter Name="prmbeginv" Type="String" />
                    <asp:Parameter Name="prmendinv" Type="String" />
                    <asp:Parameter Name="prmupto" Type="Int32" />
                    <asp:Parameter Name="prmvopen" Type="String" />
                    <asp:Parameter Name="prmshrtby" Type="String" />
                    <asp:Parameter Name="prmout" Type="String" />
                    <asp:Parameter Name="prmcon" Type="String" />
                   <asp:Parameter Name="prmReckey" Type="String" />
                   
                </SelectParameters>
            </asp:ObjectDataSource>
     
    <ft:footer id="Footer1" runat="server"></ft:footer>
    </form>
  </body>
</HTML>

