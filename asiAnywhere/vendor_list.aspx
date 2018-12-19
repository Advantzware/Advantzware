<%@ Page Language="c#" AutoEventWireup="true" Debug="false" Inherits="vendor_list" Codebehind="vendor_list.aspx.cs" %>
<%@ Register Src="footer.ascx" TagName="Footer" TagPrefix="ft" %>
<%@ Register Src="header.ascx" TagName="Header" TagPrefix="hd" %>
<html xmlns="http://www.w3.org/1999/xhtml" >
  <head id="Head1" runat="server">
    <title>Vendors</title>
    <LINK href="include/style2.css" type="text/css" rel="stylesheet"/>
    <script language =javascript>
    
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
    document.forms[0].cust_TextBox.focus();
  }

  function buyerlookup() {

      var NewWindow = window.open("buyerlook.aspx", "UomLookup", "width=500,height=400,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
  }
  function buyerlook(ReturnObj1) {
      document.forms[0].buyer_TextBox.value = ReturnObj1;

  }

  function vendortypelook() {
      var NewWindow = window.open("vendtype_lookup.aspx", "vendtypeLookupWindow", "width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
  }
  function VendTypeLookup(ReturnObj1) {
      document.forms[0].type_TextBox.value = ReturnObj1;
      
  }

function terrlook(){ 
  var NewWindow = window.open("terr_lookup.aspx","TerrCodeLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}

function TerrCodeLookup(ReturnObj1){ 
  document.forms[0].Territory_TextBox.value = ReturnObj1;
  document.forms[0].Territory_TextBox.focus();   
}

function salesreplook(){ 
  var NewWindow = window.open("salesrep_lookup.aspx","SalesRepLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}

function SalesRepLookup(ReturnObj1){ 
  document.forms[0].Salesman_TextBox.value = ReturnObj1;
  document.forms[0].Salesman_TextBox.focus();
  
}

    </script> 
  </head>    
   <body>
    <form id="frmList" runat="server"  defaultfocus='tdSearch'>   
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
          <asp:ScriptManager ID="ScriptManager1" runat="server">
          </asp:ScriptManager>
      <TABLE id="tblTop" cellSpacing="3" align="center" border="0" Width="100%">
        <TR>
          <TD width=30>&nbsp;</TD>
          <TD align=center nowrap><font size=+0><b>Vendors&nbsp;</b></font></TD>
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
      <asp:LinkButton ID="lnk_Listvend" runat="server" >Brows Vendor</asp:LinkButton></li>
      <li><asp:LinkButton ID="lnk_viewvend" runat="server" OnClick="lnk_viewvend_Click" > View Vendor</asp:LinkButton></li>
      <li><asp:LinkButton ID="lnk_listtot" runat="server" OnClick="lnk_listview_Click" >Totals</asp:LinkButton></li></ul></div>
      
      
      
      
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
      <TABLE id="tblMain" cellSpacing="1" cellPadding="1" width='800px' border="0">
        <TR>
          <TD>
            <TABLE id="tblSearch" cellSpacing="1" cellPadding="5" width="100%" border="0"  bgcolor=black>
              <TR>
                <TD class="shade" align="center" width="50"><nobr>
                  <%--<asp:linkbutton id="btnAdd" runat="server" OnClick="btnAdd_Click">Add new</asp:linkbutton></nobr>--%>
                <asp:button id="btnSearch" runat="server" OnClick=" btnSearch_Click" Width="40px" CssClass="button" Text="Go" ></asp:button>
                <br />
                <br />
                  <asp:button id="btnShowAll" runat="server" OnClick=" btnShowAll_Click" Width="40px" CssClass="button" Text="All" ></asp:button>
                </TD>               
                          
                <TD id="tdSearch" runat="server" class="shade" vAlign="middle" align="center" width="800">&nbsp;                
                <table>
                <tr><td ><b>Vendor#</b></td><td align="center"><b> Name</b></td><td align="center"><b>Type</b></td><td align="center" ><b>Buyer</b></td>
                </tr>
                <tr><td>
                    <asp:TextBox ID="vend_TextBox" Width="100px" runat="server"></asp:TextBox></td>
                    <td><asp:TextBox ID="name_TextBox" Width="120px" runat="server"></asp:TextBox></td>
                    <td><asp:TextBox ID="type_TextBox" Width="100px" runat="server"></asp:TextBox>
                    <a href="#" tabindex="1" onClick="vendortypelook(); return false"><asp:Image ID="vendtype" runat="server" ImageUrl="images/lookup_icon.gif" /></a></td>
                    <td><asp:TextBox ID="buyer_TextBox" Width="100px" runat="server"></asp:TextBox>
                    <a href="#" tabindex="1" onClick="buyerlookup(); return false"><asp:Image ID="Image5" runat="server" ImageUrl="images/lookup_icon.gif" /></a></td>
                    
                    </td></tr></table>
                  
                </TD>               
                
                
                 
                
                <TD id="tdPageCount" runat="server" class="shade" >
          <table><tr><td align="center">
           <b> Records/Page</b><BR>
            <asp:FormView ID="FormView1" runat="server" DataSourceID="ObjectDataSource2">
                          <EditItemTemplate>
                              aLine:
                              <asp:TextBox ID="aLineTextBox" runat="server" Text='<%# Bind("aLine") %>'>
                              </asp:TextBox><br />
                              <asp:LinkButton ID="UpdateButton" runat="server" CausesValidation="True" CommandName="Update"
                                  Text="Update">
                              </asp:LinkButton>
                              <asp:LinkButton ID="UpdateCancelButton" runat="server" CausesValidation="False" CommandName="Cancel"
                                  Text="Cancel">
                              </asp:LinkButton>
                          </EditItemTemplate>
                          <InsertItemTemplate>
                              aLine:
                              <asp:TextBox ID="aLineTextBox" runat="server" Text='<%# Bind("aLine") %>'>
                              </asp:TextBox><br />
                              <asp:LinkButton ID="InsertButton" runat="server" CausesValidation="True" CommandName="Insert"
                                  Text="Insert">
                              </asp:LinkButton>
                              <asp:LinkButton ID="InsertCancelButton" runat="server" CausesValidation="False" CommandName="Cancel"
                                  Text="Cancel">
                              </asp:LinkButton>
                          </InsertItemTemplate>
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
            <asp:label id="lblMessage" runat="server" ForeColor="Red"></asp:label>
          <%--</td>
        </tr>
        <tr>
          <td>

    
           
            
          </TD>
        </TR>
        <tr><td> --%>
            <asp:GridView ID="GridView1" runat="server" DataSourceID="ObjectDataSource1" EmptyDataText=" Records not  Found" AllowPaging="True" AllowSorting="True" Width="800px" AutoGenerateColumns="False" BorderStyle="Dotted" CssClass="Grid" OnSelectedIndexChanged="GridView1_SelectedIndex">
                <EmptyDataRowStyle BorderColor="Gray" BorderStyle="Dotted" BorderWidth="0px" Font-Bold="True"
                        HorizontalAlign="Center" VerticalAlign="Middle" />
                        
                <Columns>
                    <asp:CommandField ShowSelectButton="true" ButtonType="image" SelectImageUrl="~/Images/sel.gif" SelectText="" > 
                    <ItemStyle Width="10px" />
                    </asp:CommandField>
                    
                    
                    <asp:BoundField DataField="vendor" HeaderText="vendor#" SortExpression="vendor" />
                    <asp:BoundField DataField="vname" HeaderText="Name" SortExpression="vname" />
                    <asp:BoundField DataField="vtype" HeaderText="Type" SortExpression="vtype" />
                    <asp:BoundField DataField="vactive" HeaderText="Status" SortExpression="vactive" />
                    <asp:BoundField DataField="vareacode" HeaderText="Area-code" SortExpression="vareacode" />
                    <asp:BoundField DataField="vphone" HeaderText="Phone#" SortExpression="vphone" />
                    <asp:BoundField DataField="vfaxarea" HeaderText="Fax Area Code" SortExpression="vfaxarea" />
                    <asp:BoundField DataField="vfax" HeaderText="Fax" SortExpression="vfax" />
                    <asp:BoundField DataField="vbuyer" HeaderText="Buyer" SortExpression="vbuyer" />
                    
                    <asp:TemplateField HeaderText="reckey" Visible="false">
                    <ItemTemplate>
                    <asp:Label id="reckeyLabel" runat="server" Text='<%# Bind("vreckey") %>'></asp:Label>
                    </ItemTemplate>
                    </asp:TemplateField>
                                       
                </Columns>
                    <SelectedRowStyle CssClass="GridSelected" BackColor="Yellow" />
                    <HeaderStyle ForeColor="White" CssClass="headcolor" HorizontalAlign="Center"
                        VerticalAlign="Middle" Wrap="False" />
                    <AlternatingRowStyle CssClass="GridItemOdd" />
                    <RowStyle  CssClass="shade" />
            </asp:GridView></asp:Panel></ContentTemplate>
                        </asp:UpdatePanel>
            <asp:ObjectDataSource ID="ObjectDataSource1" runat="server" 
              OldValuesParameterFormatString="original_{0}" SelectMethod="SelectVendorList" 
              TypeName="voucherpay">
                <SelectParameters>
                    <%--<asp:Parameter Name="prmComp" DefaultValue="" Type="String"  />                    
                    <asp:Parameter Name="prmUser" DefaultValue="" Type="String" />--%>
                    <asp:Parameter DefaultValue="Search" Name="prmAction" Type="String" />
                     
                    <asp:Parameter Name="prmUser" Type="String" DefaultValue="" />
                    <asp:Parameter Name="prmReckey" Type="String" />
                    <asp:Parameter Name="prmActive" Type="String" />
                    <asp:SessionParameter SessionField="vendor_list_prmvendor" Name="prmVendor" Type="String" />
                    <asp:SessionParameter SessionField="vendor_list_prmname" Name="prmName" Type="String" />
                    <asp:Parameter Name="prmAdd1" Type="String" />
                    <asp:Parameter Name="prmAdd2" Type="String" />
                    <asp:Parameter Name="prmCity" Type="String" />
                    <asp:Parameter Name="prmState" Type="String" />
                    <asp:SessionParameter SessionField="vendor_list_prmzip" Name="prmZip" Type="String" />
                    <asp:Parameter Name="prmCountry" Type="String" />
                    <asp:Parameter Name="prmPostal" Type="String" />
                    <asp:Parameter Name="prmTaxid" Type="String" />
                    <asp:Parameter Name="prmRemit" Type="String" />
                    <asp:Parameter Name="prmRadd1" Type="String" />
                    <asp:Parameter Name="prmRadd2" Type="String" />
                    <asp:Parameter Name="prmRcity" Type="String" />
                    <asp:Parameter Name="prmRstate" Type="String" />
                    <asp:Parameter Name="prmRzip" Type="String" />
                    <asp:Parameter Name="prmRcountry" Type="String" />
                    <asp:Parameter Name="prmRpostal" Type="String" />
                    <asp:Parameter Name="prmCheckmemo" Type="String" />
                    <asp:SessionParameter SessionField="vendor_list_prmtype" Name="prmType" Type="String" />
                    <asp:Parameter Name="prmContact" Type="String" />
                    <asp:SessionParameter SessionField="vendor_list_prmbuyer" Name="prmBuyer" Type="String" />
                    <asp:Parameter Name="prmAreacode" Type="String" />
                    <asp:Parameter Name="prmPhone" Type="String" />
                    <asp:Parameter Name="prmFaxarea" Type="String" />
                    <asp:Parameter Name="prmFax" Type="String" />
                    <asp:Parameter Name="prmFaxprefix" Type="String" />
                    <asp:Parameter Name="prmFaxcountry" Type="String" />
                    <asp:Parameter Name="prmOverpct" Type="Decimal" />
                    <asp:Parameter Name="prmUnderpct" Type="Decimal" />
                    <asp:Parameter Name="prmActnum" Type="String" />
                    <asp:Parameter Name="prmCurrcode" Type="String" />
                    <asp:Parameter Name="prmTaxgr" Type="String" />
                    <asp:Parameter Name="prmCode1099" Type="String" />
                    <asp:Parameter Name="prmAnedivend" Type="String" />
                    <asp:Parameter Name="prmTerms" Type="String" />
                    <asp:Parameter Name="prmDisc" Type="Decimal" />
                    <asp:Parameter Name="prmRebate" Type="Decimal" />
                    <asp:Parameter Name="prmFrtpay" Type="String" />
                    <asp:Parameter Name="prmDiscdays" Type="Int32" />
                    <asp:Parameter Name="prmCarrier" Type="String" />
                    <asp:Parameter Name="prmFobcode" Type="String" />
                    <asp:Parameter Name="prmTtypedscr" Type="String" />
                    <asp:Parameter Name="prmBuyerdscr" Type="String" />
                    <asp:Parameter Name="prmTermsdscr" Type="String" />
                    <asp:Parameter Name="prmCarrierdscr" Type="String" />
                    <asp:Parameter Name="prmCurrdscr" Type="String" />
                    <asp:Parameter Name="prmActdscr" Type="String" />
                    <asp:Parameter Name="prmPoexport" Type="String" />
                </SelectParameters>
            </asp:ObjectDataSource>
           </div>
        </td></tr>
      </TABLE>    
   
    <ft:footer id="Footer1" runat="server"></ft:footer>
    </form>
  </body>
</HTML>

