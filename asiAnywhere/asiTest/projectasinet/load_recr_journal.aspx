<%@ Page Language="c#" AutoEventWireup="true" Debug="false" Inherits="load_recr_journal" Codebehind="load_recr_journal.aspx.cs" %>
<%@ Register Src="footer.ascx" TagName="Footer" TagPrefix="ft" %>
<%@ Register Src="header.ascx" TagName="Header" TagPrefix="hd" %>
<html xmlns="http://www.w3.org/1999/xhtml" >
  <head id="Head1" runat="server">
    <title>General Ledger Recurring Journal Entries</title>
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
  
   function zipcodelook(){ 
  var NewWindow = window.open("zipcode_lookup.aspx","ZipCodeLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}

function ZipCodeLookup(ReturnObj1,ReturnObj2,ReturnObj3){ 
  document.forms[0].Zip_TextBox.value = ReturnObj1;
    document.forms[0].State_TextBox.value = ReturnObj3;
   document.forms[0].City_TextBox.value = ReturnObj2;
}
function citylook(){ 
  var NewWindow = window.open("city_lookup.aspx","CityCodeLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}

function CityCodeLookup(ReturnObj1){ 
  
   document.forms[0].City_TextBox.value = ReturnObj1;
  
    
}
function statecodelook(){ 
  var NewWindow = window.open("statecode_lookup.aspx","StateCodeLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}

function StateCodeLookup(ReturnObj1){ 
  document.forms[0].State_TextBox.value = ReturnObj1;
  
    
}
function terrlook(){ 
  var NewWindow = window.open("terr_lookup.aspx","TerrCodeLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}

function TerrCodeLookup(ReturnObj1){ 
  document.forms[0].Territory_TextBox.value = ReturnObj1;
    
}

function salesreplook(){ 
  var NewWindow = window.open("salesrep_lookup.aspx","SalesRepLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}

function SalesRepLookup(ReturnObj1){ 
  document.forms[0].Salesman_TextBox.value = ReturnObj1;


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
    var NewWindow = window.open("toporder_list_notes.aspx", "OrderListNotes", "width=600,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}
function ordernotes() {
    var NewWindow = window.open("toporder_list_notes.aspx", "OrderListNotes", "width=600,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}

function myFunction() {
    var x;
    var currentTime = new Date();
    var day = currentTime.getDate();
    var month = currentTime.getMonth() + 1;
    var year = currentTime.getFullYear();
   
    var date1 = document.getElementById("HiddenField2");
    if (confirm("This will select all highlighted recurring AP invoice entries, do you want to continue?")) {
        var name = prompt("Enter Posting Date", month + "/" + day + "/" + year);
        if (name != null) {
            var da = document.getElementById("HiddenField1");
            da.value = name;
        }
        date1.value = "Yes";
    }
    else date1.value = "No";
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
          <asp:HiddenField ID="HiddenField2" runat="server" />
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
          
          <td width=30>&nbsp;</td>
        </TR>
      </TABLE>
      <table><tr bgcolor="gray"><td><div  id="navigation" style="width:100%">
		<ul nowrap> <li >
       <asp:LinkButton ID="lnk_Listcustomers" runat="server" OnClick="lnk_list_click" >Brws Journals</asp:LinkButton></li>
       <li><asp:LinkButton ID="lnk_viewcustomers" runat="server" OnClick="lnk_viewcustomers_Click" > View journals</asp:LinkButton></li>
       <li class="selected"><asp:LinkButton ID="load_viewcustomers" runat="server" OnClick="load_viewjournals_Click" > Load Journals</asp:LinkButton></li></ul></div>
            
      </td>
      </tr></table>
     
              
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
                <b>Journal#</b> <br />
                <asp:TextBox ID="jurl_TextBox" Width="100px" runat="server"></asp:TextBox>
                <asp:CompareValidator ID="CompareValidator1" ControlToValidate="jurl_TextBox" Operator="DataTypeCheck" Type="Integer" Display="Dynamic" SetFocusOnError="true" runat="server" ErrorMessage="Integer Value Only"></asp:CompareValidator>
                </td>
                <td><b> Frequency</b><br />
                    <asp:TextBox ID="frenq_TextBox" Width="100px" runat="server"></asp:TextBox>                    </td>
                    
                                  
                
                
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
                    
                    <asp:BoundField DataField="jrn_no" HeaderText="Jrnl#" SortExpression="jrn_no" />
                    <asp:BoundField DataField="cb_freq" HeaderText="Frequency" SortExpression="cb_freq" />
                    <asp:BoundField DataField="dscr" HeaderText="Description" SortExpression="dscr" />
                    <asp:BoundField DataField="t_deb" HeaderText="Debit" SortExpression="t_deb" />
                    <asp:BoundField DataField="t_crd" HeaderText="Credit" SortExpression="t_crd" />
                    <asp:BoundField DataField="t_amt" HeaderText="Amount" SortExpression="t_amt" />
                    
                    
                    <asp:TemplateField Visible="false" HeaderText="Reckey"  >
                    <ItemTemplate>
                    <asp:Label ID="reclabel" runat="server"  Text='<%# Bind("[reckey]") %>'></asp:Label>
                    </ItemTemplate>
                    </asp:TemplateField>
                    <asp:TemplateField Visible="false"  HeaderText="date"  >
                    <ItemTemplate>
                    <asp:Label ID="tr_datelabel" runat="server"  Text='<%# Bind("[tr_date]") %>'></asp:Label>
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
                OldValuesParameterFormatString="original_{0}" SelectMethod="RecrGeneralLedgerload" 
                TypeName="ledger">
                <SelectParameters>
                   <asp:Parameter DefaultValue="Search" Name="prmAction" Type="String" />                    
                   <asp:Parameter Name="prmComp"  Type="string" />
                   <asp:Parameter Name="prmUser" Type="string" />                     
                   <asp:SessionParameter SessionField="load_general_ledger_jrnl" Name="prmjrn_no" Type="Int32" />                   
                   <asp:Parameter Name="prmtr_date" Type="String" />                   
                   <asp:Parameter Name="prmperiod" Type="Int32" />
                   <asp:Parameter Name="prmt_deb" Type="Decimal" />
                   <asp:Parameter Name="prmt_crd" Type="Decimal" />
                   <asp:Parameter Name="prmt_amt" Type="Decimal" />
                   <asp:Parameter Name="prmreverse" Type="String" />
                   <asp:Parameter Name="prmfrom_revr" Type="String" />
                   <asp:SessionParameter SessionField="load_general_ledger_freq" Name="prmcb_freq" Type="String" />
                   <asp:Parameter Name="prmdscr" Type="String" />
                   <asp:Parameter Name="prmReckey" Type="String" />
                </SelectParameters>
            </asp:ObjectDataSource></td></tr></TABLE></asp:Panel>
                       
                        <asp:Button ID="LoadButton" runat="server" CssClass="button" OnClientClick="myFunction()" OnClick="loadButton_Click"  Text="Load Highlighted Invoices" />
          
          </div></td></tr></table>
      
     
    <ft:footer id="Footer1" runat="server"></ft:footer>
    </form>
  </body>
</HTML>

