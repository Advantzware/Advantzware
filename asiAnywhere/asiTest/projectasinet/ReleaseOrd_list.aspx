<%@ Page Language="c#" AutoEventWireup="true" Debug="false" Inherits="ReleaseOrd_list" Codebehind="ReleaseOrd_list.aspx.cs" %>
<%@ Register Src="footer.ascx" TagName="Footer" TagPrefix="ft" %>
<%@ Register Src="header.ascx" TagName="Header" TagPrefix="hd" %>
<html xmlns="http://www.w3.org/1999/xhtml" >
  <head id="Head1" runat="server">
    <title>Order Release</title>
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
    
    


function releaselook() {

    var NewWindow = window.open("release_lookup.aspx", "ReleaseLookup", "width=500,height=400,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}
function ReleaseLookUp(ReturnObj1) {
    document.forms[0].release_TextBox.value = ReturnObj1;

}

function customerlook() {
    var NewWindow = window.open("customer_lookup.aspx", "CustomerLookupWindow", "width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}
function CustomerLookup(ReturnObj1) {
    document.forms[0].cust_TextBox.value = ReturnObj1;
    document.forms[0].cust_TextBox.focus();
}

function fglook() {
    var NewWindow = window.open("fgitem_lookup.aspx", "FGLookupWindow", "width=450,height=300,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}

function FGLookup(ReturnObj1) {
    document.forms[0].fgitm_TextBox.value = ReturnObj1;
    document.forms[0].fgitm_TextBox.focus();

}
function customerpolook() {
    var cust = document.getElementById("cust_TextBox").value;
    var NewWindow = window.open("customerpo_entry_lookup.aspx?customer=" + cust + "", "EstimateLookupWindow", "width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}

function CustomerPOLookup(ReturnObj1) {
    document.forms[0].cstpo_TextBox.value = ReturnObj1;
    document.forms[0].cstpo_TextBox.focus();
}
function job1look() {
    var NewWindow = window.open("job1_lookup.aspx", "Job1LookupWindow", "width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}
function Job1Lookup(ReturnObj1, ReturnObj2) {
    document.forms[0].job_TextBox.value = ReturnObj1;
    document.forms[0].job2_TextBox.value = ReturnObj2;
    document.forms[0].job_TextBox.focus();
}

function orderlook() {
    var cust = document.getElementById("cust_TextBox").value;
    var NewWindow = window.open("order_lookup.aspx?customer=" + cust + "", "OrderLookupWindow", "width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}
function OrderLookup(ReturnObj1) {
    document.forms[0].ordno_TextBox.value = ReturnObj1;
    document.forms[0].ordno_TextBox.focus();    
}

function setdate() {

    var da = document.getElementById("date_TextBox");
    da.focus();

}
function printrel() {
    var NewWindow = window.open("print_relord_report.aspx", "ReleaseOrderReport", "width=580,height=540,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}
function ordernotes() {
    var NewWindow = window.open("toporder_list_notes.aspx", "OrderListNotes", "width=600,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}     

    </script> 
  </head>    
   <body>
    <form id="frmList" runat="server"  defaultfocus='vendor_TextBox'>   
        <hd:header id="Header1" runat="server"></hd:header>
      
       <table width="100%"><tr><td><div>
        <table align="left" border="1" width="75%">
                <tr bgcolor="maroon">
                                        
                    <%-- <td nowrap width="1px";>
                        <a href="#" onClick="select_col(); return false"><asp:Image ID="Image1" Width="35px" runat="server" ImageUrl="~/Images/moveCol.ico" /></a>
                    </td>                  
                    <td nowrap width="25px";>
                        <a href="#" onClick="printspec(); return false"><asp:Image ID="Image6" Width="35px" runat="server" ImageUrl="~/Images/dict.ico" /></a>
                    </td>--%>
                        <td nowrap width="25px";>
                        <asp:ImageButton ID="img_btn_add" runat="server" Width="35px" ImageUrl="~/Images/add.bmp" ToolTip="Add" OnClick="img_btn_add_click" />
                        </td>
                        <td nowrap width="25px";>
                        <a href="#" onClick="printrel(); return false"><asp:Image ID="Image1" Width="35px" runat="server" ImageUrl="~/Images/print-u.bmp" /></a>
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
          <TD align=left nowrap><font size=+0><b>Order Release&nbsp;</b></font></TD>
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
      <table><tr bgcolor="gray"><td> <asp:LinkButton ID="lnk_Listcustomers" runat="server" ><img src="Images/brwsrelease1.jpg" border="0" alt="List Invoices " /></asp:LinkButton>
      <asp:LinkButton ID="lnk_viewcustomers" runat="server" OnClick="lnk_viewcustomers_Click" > <img src="Images/items0.jpg" border="0" alt="View Invoices" /></asp:LinkButton>
      <asp:LinkButton ID="load_viewcustomers" runat="server" OnClick="load_viewcustomers_Click" > <img src="Images/shipnotes0.jpg" border="0" alt="View Invoices" /></asp:LinkButton>
            
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
      <TABLE id="tblMain" cellSpacing="1" cellPadding="1" width='950px' border="0">
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
                          
                
                <td class="shade" bgcolor="gray" ><b> Release #</b><br />
                    <asp:TextBox ID="release_TextBox" Width="70px" runat="server"></asp:TextBox>
                    <a href="#" tabindex="1" onClick="releaselook(); return false"><asp:Image ID="Image2" runat="server" ImageUrl="images/lookup_icon.gif" /></a></td>
                    <td><b>Order# </b><br />
                    <asp:TextBox ID="ordno_TextBox" Width="70px" runat="server" ></asp:TextBox>
                    <a href="#" tabindex="1" onClick="orderlook(); return false"><asp:Image ID="OrderLook" runat="server" ImageUrl="images/lookup_icon.gif" /></a></td>
                    <td><b>Customer# </b><br />
                    <asp:TextBox ID="cust_TextBox" Width="70px" runat="server" ></asp:TextBox>
                    <a href="#" tabindex="1" onClick="customerlook(); return false"><asp:Image ID="CustomerLook" runat="server" ImageUrl="images/lookup_icon.gif" /></a></td>
                    <td><b>FG Item# </b><br />
                    <asp:TextBox ID="fgitm_TextBox" Width="100px" runat="server" ></asp:TextBox>
                    <a href="#" tabindex="1" onClick="fglook(); return false"><asp:Image ID="Image3" runat="server" ImageUrl="images/lookup_icon.gif" /></a></td>
                    <td><b>Customer PO# </b><br />
                    <asp:TextBox ID="cstpo_TextBox" Width="100px" runat="server" ></asp:TextBox>
                    <a href="#" tabindex="1" onClick="customerpolook(); return false"><asp:Image ID="CustomerPoLookup" runat="server" ImageUrl="images/lookup_icon.gif" /></a></td>
                    <td><b>Job# </b><br />
                    <asp:TextBox ID="job_TextBox" Width="50px" runat="server" ></asp:TextBox>
                    <asp:TextBox ID="job2_TextBox" Width="20px" runat="server" ></asp:TextBox>
                    <a href="#" tabindex="1" onClick="job1look(); return false"><asp:Image ID="Job1Lookup" runat="server" ImageUrl="images/lookup_icon.gif" /></a></td>
                    
                    <td>
                   &nbsp;&nbsp; <b><asp:CheckBox ID="CheckBox1" Text="Posted" runat="server" /></b>                   
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
            <asp:GridView ID="GridView1" runat="server" DataSourceID="ObjectDataSource1" EmptyDataText=" Records not  Found" AllowPaging="True" AllowSorting="True" Width="950px" AutoGenerateColumns="False" BorderStyle="Dotted" CssClass="Grid" OnSelectedIndexChanged="GridView1_SelectedIndex">
                <EmptyDataRowStyle BorderColor="Gray" BorderStyle="Dotted" BorderWidth="0px" Font-Bold="True"
                        HorizontalAlign="Center" VerticalAlign="Middle" />
                        
                <Columns>
                    <asp:CommandField ShowSelectButton="true" ButtonType="image" SelectImageUrl="~/Images/sel.gif" SelectText="" > 
                    <ItemStyle Width="10px" />
                    </asp:CommandField>
                    <asp:BoundField DataField="rellno" HeaderText="Release#" SortExpression="rellno" />
                    <asp:BoundField DataField="ordno" HeaderText="Order#" SortExpression="ordno" />                    
                    <asp:BoundField DataField="pono" HeaderText="Customer PO" SortExpression="pono" />
                    <asp:BoundField DataField="custno" HeaderText="Cust #" SortExpression="custno" />  
                    <asp:BoundField DataField="partno" HeaderText="Cust Part#" SortExpression="partno" />
                    <asp:BoundField DataField="shipid" HeaderText="Ship TO" SortExpression="shipid" />                    
                    <asp:BoundField DataField="ino" HeaderText="FG Item Number" SortExpression="ino" />
                    <asp:BoundField DataField="reldate" HeaderText="Release Date" SortExpression="reldate" /> 
                    <asp:BoundField DataField="jobno" HeaderText="Job#" SortExpression="jobno" />
                    <asp:BoundField DataField="jobno2" HeaderText=" " SortExpression="jobno2" />                    
                    <asp:BoundField DataField="printed" HeaderText="Printed?" SortExpression="printed" />
                    <asp:BoundField DataField="qty" HeaderText="Release Qty" SortExpression="qty" />  
                   
                    <asp:TemplateField Visible="false" HeaderText="Reckey" >
                    <ItemTemplate>
                    <asp:Label ID="reclabel" Visible="false" runat="server" Text='<%# Bind("[reckey]") %>'></asp:Label>
                    <asp:Label ID="Label1" Visible="false" runat="server" Text='<%# Bind("[extra]") %>'></asp:Label>
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
                OldValuesParameterFormatString="original_{0}" SelectMethod="ReleaseOrderlist" 
                TypeName="release">
                <SelectParameters>
                   <asp:Parameter DefaultValue="Search" Name="prmAction" Type="String" />                    
                   <asp:Parameter Name="prmComp"  Type="string" />
                   <asp:Parameter Name="prmUser" Type="string" />                     
                   <asp:SessionParameter SessionField="Release_ord_rellno" Name="prmrellno" Type="Int32" />                                        
                   <asp:SessionParameter SessionField="Release_ord_ordno" Name="prmordno" Type="Int32" />                                        
                   <asp:SessionParameter SessionField="Release_ord_pono" Name="prmpono" Type="String" />
                   <asp:SessionParameter SessionField="Release_ord_custno" Name="prmcustno" Type="String" />
                   <asp:Parameter Name="prmpartno" Type="String" />
                   <asp:Parameter Name="prmshipid" Type="String" />
                   <asp:SessionParameter SessionField="Release_ord_ino" Name="prmino" Type="String" />                   
                   <asp:Parameter Name="prmreldate"  Type="string" />
                   <asp:SessionParameter SessionField="Release_ord_job" Name="prmjobno" Type="string" />                     
                   <asp:SessionParameter SessionField="Release_ord_job2" Name="prmjobno2" Type="Int32" />                                        
                   <asp:Parameter Name="prmcarrier" Type="String" />                                        
                   <asp:Parameter Name="prmtrailer" Type="String" />
                   <asp:SessionParameter SessionField="Release_ord_posted" Name="prmposted" Type="String" />
                   <asp:Parameter Name="prmship1" Type="String" />
                   <asp:Parameter Name="prmship2" Type="String" />
                   <asp:Parameter Name="prmship3" Type="String" />
                   <asp:Parameter Name="prmship4" Type="String" />
                   <asp:Parameter Name="prmReckey" Type="String" />
                   <asp:Parameter Name="prmextra" Type="String" />
                </SelectParameters>
            </asp:ObjectDataSource></td></tr></TABLE></asp:Panel>
                        </ContentTemplate>
                        </asp:UpdatePanel>
          
          </div></td></tr></table>
      
     
    <ft:footer id="Footer1" runat="server"></ft:footer>
    </form>
  </body>
</HTML>

