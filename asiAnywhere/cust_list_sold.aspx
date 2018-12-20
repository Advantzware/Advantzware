<%@ Page Language="c#" AutoEventWireup="true" Debug="true" Inherits="cust_list_sold" Codebehind="cust_list_sold.aspx.cs" %>
<%@ Register Src="footer.ascx" TagName="Footer" TagPrefix="ft" %>
<%@ Register Src="header.ascx" TagName="Header" TagPrefix="hd" %>
<html xmlns="http://www.w3.org/1999/xhtml" >
  <head id="Head1" runat="server">
    <title>Sold To</title>
    <LINK href="include/style2.css" type="text/css" rel="stylesheet"/>
    <script language =javascript >
        window.onload = setfocus;
        function setfocus() {
            document.forms[0].Sold_TextBox.focus();
        }
    
    var bSelected=false;
    {
        var theForm = document.forms['frmList'];
        if (!theForm) theForm = document.frmList;
        bSelected = !bSelected; 
        var i;
        for (i=0;i<theForm.chDelete.length;++i) theForm.chDelete[i].checked=bSelected;
    }

    function contactcustomerlook() {
        var NewWindow = window.open("contact_customer_lookup.aspx", "ContactCustomerLookupWindow", "width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
    }

    function ContactCustomerLookup(ReturnObj1, ReturnObj2, ReturnObj3, ReturnObj4, ReturnObj5, ReturnObj6, ReturnObj7, ReturnObj8, ReturnObj9, ReturnObj10, ReturnObj11) {
        document.forms[0].Sold_TextBox.value = ReturnObj1;
        document.forms[0].Sold_TextBox.focus();
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
    
  
    </script> 
  </head>    
   <body>
    <form id="frmList" runat="server"  defaultfocus='cust_TextBox'>   
        <hd:header id="Header1" runat="server"></hd:header>
      <div>
            
      <TABLE id="tblTop" cellSpacing="3" align="center" border="0" Width="100%">
        <TR>
          <TD width=30>&nbsp;</TD>
          <TD align=center nowrap><font size=+0><b>Sold To&nbsp;</b></font></TD>
          <TD nowrap >
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
      <table><tr bgcolor="gray"  ><td nowrap><div  id="navigation" style="width:100%">
		<ul nowrap> <li  > 
      <asp:LinkButton ID="lnk_Listcustomers" runat="server" OnClick="lnk_Listcustomers_Click" >List Customers</asp:LinkButton></li>
      <li><asp:LinkButton ID="lnk_viewcustomers" runat="server" OnClick="lnk_viewcustomers_Click" >View Customers</asp:LinkButton></li>
      <li><asp:LinkButton ID="lnk_listship" runat="server" OnClick="lnk_listship_Click" >List Ship To</asp:LinkButton></li>
      <li><asp:LinkButton ID="lnk_viewship" runat="server" OnClick="lnk_viewship_Click"  >View Ship To</asp:LinkButton></li>
      <li class="selected"><asp:LinkButton ID="lnk_listsold" runat="server" OnClick="lnk_listsold_Click" >List Sold To</asp:LinkButton></li>
      <li><asp:LinkButton ID="lnk_viewsold" runat="server" OnClick="lnk_viewsold_Click" >View Sold To</asp:LinkButton></li></ul></div>
      
      
      </td>
      </tr></table>
      <TABLE id="tblMain" cellSpacing="1" cellPadding="1" width='500px' border="0">
        <TR>
          <TD>
            <TABLE id="tblSearch" cellSpacing="1" cellPadding="5" width="500px" border="0"  bgcolor=black>
              <TR>
                <TD class="shade" align="center" width="50"><nobr>
                  <%--<asp:linkbutton id="btnAdd" runat="server" OnClick="btnAdd_Click">Add new</asp:linkbutton></nobr>--%>
                <asp:button id="btnSearch" runat="server" OnClick=" btnSearch_Click" Width="40px" CssClass="button" Text="Go" ></asp:button>
                <br />
                <br />
                  <asp:button id="btnShowAll" runat="server" OnClick=" btnShowAll_Click" Width="40px" CssClass="button" Text="All" ></asp:button>
                </TD>               
                          
                <TD id="tdSearch" runat="server" class="shade" vAlign="middle"  width="800">&nbsp;                
                <table>
                <tr><td ><b>Sold To</b></td><td ><b> Name</b></td></tr>
                <tr><td>
                    <asp:TextBox ID="Sold_TextBox" Width="70px" runat="server"></asp:TextBox>
                     <a href="#" tabindex="1" onClick="contactcustomerlook(); return false"><asp:Image ID="Image5" runat="server" ImageUrl="images/lookup_icon.gif" /></a></td>
                    <td><asp:TextBox ID="Name_TextBox" Width="70px" runat="server"></asp:TextBox></td>
                    </tr></table>
                  
                </TD>               
                
                <TD id="tdPageCount" runat="server" class="shade" >
          <table><tr><td align="center">
           <b> Records/Page</b><BR><asp:FormView ID="FormView1" runat="server" DataSourceID="ObjectDataSource2">
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
          </td>
        </tr>
        <tr>
          <td>

    
           
            
          </TD>
        </TR>
        <tr><td>
            <asp:GridView ID="GridView1" runat="server" DataSourceID="ObjectDataSource1" EmptyDataText=" Records not  Found" AllowPaging="True" AllowSorting="True" Width="500px" AutoGenerateColumns="False" BorderStyle="Dotted" CssClass="Grid" OnSelectedIndexChanged="GridView1_SelectedIndex">
                <EmptyDataRowStyle BorderColor="Gray" BorderStyle="Dotted" BorderWidth="0px" Font-Bold="True"
                        HorizontalAlign="Center" VerticalAlign="Middle" />
                        
                <Columns>
                    <asp:CommandField ShowSelectButton="true" SelectImageUrl="~/Images/sel.gif" SelectText="" ButtonType="image" >
                    <ItemStyle Width="10px" />
                    </asp:CommandField >
                    <asp:BoundField DataField="vsoldid" HeaderText="Sold Id" SortExpression="vsoldid" />
                    <%--<asp:BoundField DataField="vsoldno" HeaderText="vsoldno" SortExpression="vsoldno" />--%>
                    <asp:BoundField DataField="vsoldname" HeaderText="Name" SortExpression="vsoldname" />
                    <asp:BoundField DataField="vsoldcity" HeaderText="City" SortExpression="vsoldcity" />
                    <asp:BoundField DataField="vsoldstate" HeaderText="State" SortExpression="vsoldstate" />
                    <asp:BoundField DataField="vsoldzip" HeaderText="Zip" SortExpression="vsoldzip" />
                    <asp:BoundField DataField="vsoldreckey" Visible="false" HeaderText="Reckey" SortExpression="vsoldreckey" />
                   <%-- <asp:BoundField DataField="prmCustomer" HeaderText="prmCustomer" SortExpression="prmCustomer" />--%>
                   <%-- <asp:BoundField DataField="vsoldaddr1" HeaderText="Address" SortExpression="vsoldaddr1" />
                    <asp:BoundField DataField="vsoldaddr2" HeaderText="Address" SortExpression="vsoldaddr2" />--%>
                </Columns>
                    <SelectedRowStyle CssClass="GridSelected" BackColor="Yellow" />
                    <HeaderStyle ForeColor="White" CssClass="headcolor" HorizontalAlign="Center"
                        VerticalAlign="Middle" Wrap="False" />
                    <AlternatingRowStyle CssClass="GridItemOdd" />
                    <RowStyle  CssClass="shade" />
            </asp:GridView>
            <asp:ObjectDataSource ID="ObjectDataSource1" runat="server" OldValuesParameterFormatString="original_{0}" SelectMethod="Selectsoldto" TypeName="contact">
                <SelectParameters>
                    <asp:Parameter DefaultValue="Select" Name="prmAction" Type="String" />
                    <asp:Parameter Name="prmComp" Type="String" DefaultValue="" />
                    <asp:Parameter Name="prmUser" Type="String" />
                    <asp:SessionParameter Name="prmCustomer" SessionField="customer1_list_cust" Type="String" />
                    <asp:Parameter Name="prmsoldid" Type="String" />
                    <asp:Parameter Name="prmsoldno" Type="Int32" />
                    <asp:Parameter Name="prmsoldname" Type="String" />
                    <asp:Parameter Name="prmsoldcity" Type="String" />
                    <asp:Parameter Name="prmsoldstate" Type="String" />
                    <asp:Parameter Name="prmsoldzip" Type="String" />
                    <asp:Parameter Name="prmsoldaddr1" Type="String" />
                    <asp:Parameter Name="prmsoldaddr2" Type="String" />
                    <asp:Parameter Name="prmsoldreckey" Type="String" />
                </SelectParameters>
            </asp:ObjectDataSource>
          
        </td></tr>
      </TABLE>      
    </div>
    <ft:footer id="Footer1" runat="server"></ft:footer>
    </form>
  </body>
</HTML>

