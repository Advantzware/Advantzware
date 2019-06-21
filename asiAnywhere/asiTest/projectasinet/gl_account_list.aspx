<%@ Page Language="c#" AutoEventWireup="true" Debug="false" Inherits="gl_account_list" Codebehind="gl_account_list.aspx.cs" %>
<%@ Register Src="footer.ascx" TagName="Footer" TagPrefix="ft" %>
<%@ Register Src="header.ascx" TagName="Header" TagPrefix="hd" %>
<html xmlns="http://www.w3.org/1999/xhtml" >
  <head id="Head1" runat="server">
    <title>G/L Accounts</title>
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
    var NewWindow = window.open("top_list_notes.aspx", "ListNotes", "width=600,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}
function printgl() {
    var NewWindow = window.open("topprintglact_report.aspx", "OrderReport", "width=600,height=600,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}


    </script> 
  </head>    
   <body>
    <form id="frmList" runat="server"  defaultfocus='jurl_TextBox'>   
        <hd:header id="Header1" runat="server"></hd:header>
      
       <table width="100%"><tr><td><div>
        <table align="left" border="1" width="75%">
                <tr class="topheadcolor">                                        
                   
                        <td nowrap width="25px";>
                        <asp:ImageButton ID="img_btn_add" runat="server" Width="35px" ImageUrl="~/Images/add.bmp" ToolTip="Add" OnClick="img_btn_add_click" />
                        </td>
                        <td nowrap width="25px";>
                        <a href="#" onClick="ordernotes(); return false"><asp:Image ID="img_btn_notes" Width="35px" ToolTip="Notes" runat="server" ImageUrl="~/Images/edit.ico" /></a>                        
                        </td>
                        <td nowrap width="25px";>
                        <a href="#" onClick="printgl(); return false"><asp:Image ID="Image1" Width="35px" runat="server" ImageUrl="~/Images/print-u.bmp" /></a>
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
          <TD align=left nowrap><font size=+0><b>G/L Accounts&nbsp;</b></font></TD>
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
      <table><tr bgcolor="gray"><td><div  id="navigation" style="width:100%">
		<ul nowrap> <li class="selected">
      <asp:LinkButton ID="lnk_Listcustomers" runat="server" >Brws Account</asp:LinkButton></li>
      <li><asp:LinkButton ID="lnk_viewcustomers" runat="server" OnClick="lnk_viewcustomers_Click" > View Account</asp:LinkButton></li>
      <li><asp:LinkButton ID="load_viewcustomers" runat="server" OnClick="load_viewjournals_Click" > Distrib. Acct</asp:LinkButton></li></ul></div>
            
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
      <TABLE id="tblMain" cellSpacing="1" cellPadding="1" width='600px' border="0">
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
                <b>Account#</b> <br />
                <asp:TextBox ID="act_TextBox" Width="100px" runat="server"></asp:TextBox>                    
                </td>
                <td><b> Description</b><br />
                    <asp:TextBox ID="dscr_TextBox" Width="100px" runat="server"></asp:TextBox>                    </td>
                    <td><b> Type</b><br />
                    <asp:TextBox ID="type_TextBox" Width="100px" runat="server"></asp:TextBox>                    </td>
                
                    
                    
                            
                
                
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
            <asp:GridView ID="GridView1" runat="server" DataSourceID="ObjectDataSource1" EmptyDataText=" Records not  Found" AllowPaging="True" AllowSorting="True" Width="600px" AutoGenerateColumns="False" BorderStyle="Dotted" CssClass="Grid" OnSelectedIndexChanged="GridView1_SelectedIndex">
                <EmptyDataRowStyle BorderColor="Gray" BorderStyle="Dotted" BorderWidth="0px" Font-Bold="True"
                        HorizontalAlign="Center" VerticalAlign="Middle" />
                        
                <Columns>
                    <asp:CommandField ShowSelectButton="true" ButtonType="image" SelectImageUrl="~/Images/sel.gif" SelectText="" > 
                    <ItemStyle Width="10px" />
                    </asp:CommandField>
                    
                    <asp:BoundField DataField="act" HeaderText="Account No" SortExpression="act" />
                    <asp:BoundField DataField="actdscr" HeaderText="Description" SortExpression="actdscr" />
                    <asp:BoundField DataField="actype" HeaderText="Type" SortExpression="actype" />
                    
                    
                    
                    <asp:TemplateField  Visible="false" HeaderText="Reckey"  >
                    <ItemTemplate>
                    <asp:Label ID="reclabel" runat="server"  Text='<%# Bind("[RecKey]") %>'></asp:Label>
                    </ItemTemplate>
                    </asp:TemplateField>
                                                          
                </Columns>
                    <SelectedRowStyle CssClass="GridSelected" BackColor="Yellow" />
                    <HeaderStyle  ForeColor="White" CssClass="headcolor" HorizontalAlign="Center"
                        VerticalAlign="Middle" Wrap="False" />
                    <AlternatingRowStyle CssClass="GridItemOdd" />
                    <RowStyle  CssClass="shade" />
            </asp:GridView>
            <asp:ObjectDataSource ID="ObjectDataSource1" runat="server" 
                OldValuesParameterFormatString="original_{0}" SelectMethod="GLAccountList" 
                TypeName="ledger">
                <SelectParameters>
                   <asp:Parameter Name="prmUser" Type="string" />  
                   <asp:Parameter DefaultValue="Search" Name="prmAction" Type="String" />                                                                             
                   <asp:SessionParameter SessionField="gl_account_list_prmacc" Name="prmact" Type="String" />
                   <asp:SessionParameter SessionField="gl_account_list_prmactdscr" Name="prmactdscr" Type="String" />
                   <asp:SessionParameter SessionField="gl_account_list_prmactype" Name="prmactype" Type="String" />
                   <asp:Parameter Name="prmReckey" Type="String" />
                                      
                   <asp:Parameter Name="prmcry_opn" Type="Decimal" />                   
                   <asp:Parameter Name="prmlyr_opn" Type="Decimal" />
                   <asp:Parameter Name="prmbud1" Type="Decimal" />
                   <asp:Parameter Name="prmbud2" Type="Decimal" />
                   <asp:Parameter Name="prmbud3" Type="Decimal" />
                   <asp:Parameter Name="prmbud4" Type="Decimal" />
                   <asp:Parameter Name="prmbud5" Type="Decimal" />
                   <asp:Parameter Name="prmbud6" Type="Decimal" />
                   <asp:Parameter Name="prmbud7" Type="Decimal" />
                   <asp:Parameter Name="prmbud8" Type="Decimal" />                   
                   <asp:Parameter Name="prmbud9" Type="Decimal" />
                   <asp:Parameter Name="prmbud10" Type="Decimal" />
                   <asp:Parameter Name="prmbud11" Type="Decimal" />
                   <asp:Parameter Name="prmbud12" Type="Decimal" />
                   <asp:Parameter Name="prmbud13" Type="Decimal" />
                   <asp:Parameter Name="prmly_bud1" Type="Decimal" />
                   <asp:Parameter Name="prmly_bud2" Type="Decimal" />
                   <asp:Parameter Name="prmly_bud3" Type="Decimal" />
                   <asp:Parameter Name="prmly_bud4" Type="Decimal" />                   
                   <asp:Parameter Name="prmly_bud5" Type="Decimal" />
                   <asp:Parameter Name="prmly_bud6" Type="Decimal" />
                   <asp:Parameter Name="prmly_bud7" Type="Decimal" />
                   <asp:Parameter Name="prmly_bud8" Type="Decimal" />
                   <asp:Parameter Name="prmly_bud9" Type="Decimal" />
                   <asp:Parameter Name="prmly_bud10" Type="Decimal" />
                   <asp:Parameter Name="prmly_bud11" Type="Decimal" />
                   <asp:Parameter Name="prmly_bud12" Type="Decimal" />
                   <asp:Parameter Name="prmly_bud13" Type="Decimal" />                   
                   <asp:Parameter Name="prmlyr1" Type="Decimal" />
                   <asp:Parameter Name="prmlyr2" Type="Decimal" />
                   <asp:Parameter Name="prmlyr3" Type="Decimal" />
                   <asp:Parameter Name="prmlyr4" Type="Decimal" />
                   <asp:Parameter Name="prmlyr5" Type="Decimal" />
                   <asp:Parameter Name="prmlyr6" Type="Decimal" />
                   <asp:Parameter Name="prmlyr7" Type="Decimal" />
                   <asp:Parameter Name="prmlyr8" Type="Decimal" />
                   <asp:Parameter Name="prmlyr9" Type="Decimal" />                   
                   <asp:Parameter Name="prmlyr10" Type="Decimal" />
                   <asp:Parameter Name="prmlyr11" Type="Decimal" />
                   <asp:Parameter Name="prmlyr12" Type="Decimal" />
                   <asp:Parameter Name="prmlyr13" Type="Decimal" />
                   <asp:Parameter Name="prmcyr1" Type="Decimal" />
                   <asp:Parameter Name="prmcyr2" Type="Decimal" />
                   <asp:Parameter Name="prmcyr3" Type="Decimal" />
                   <asp:Parameter Name="prmcyr4" Type="Decimal" />
                   <asp:Parameter Name="prmcyr5" Type="Decimal" />                   
                   <asp:Parameter Name="prmcyr6" Type="Decimal" />
                   <asp:Parameter Name="prmcyr7" Type="Decimal" />
                   <asp:Parameter Name="prmcyr8" Type="Decimal" />
                   <asp:Parameter Name="prmcyr9" Type="Decimal" />
                   <asp:Parameter Name="prmcyr10" Type="Decimal" />
                   <asp:Parameter Name="prmcyr11" Type="Decimal" />
                   <asp:Parameter Name="prmcyr12" Type="Decimal" />
                   <asp:Parameter Name="prmcyr13" Type="Decimal" />
                   <asp:Parameter Name="prmtb_not_disc" Type="String" />
                   <asp:Parameter Name="prmbtn" Type="String" />
                   
                   
                </SelectParameters>
            </asp:ObjectDataSource></td></tr></TABLE></asp:Panel>
                        </ContentTemplate>
                        </asp:UpdatePanel>
          
          </div></td></tr></table>
      
     
    <ft:footer id="Footer1" runat="server"></ft:footer>
    </form>
  </body>
</HTML>

