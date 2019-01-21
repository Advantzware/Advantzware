<%@ Page Language="c#" AutoEventWireup="true" Debug="false" Inherits="customers_list" Codebehind="customer_list.aspx.cs" %>
<%@ Register Src="footer.ascx" TagName="Footer" TagPrefix="ft" %>
<%@ Register Src="header.ascx" TagName="Header" TagPrefix="hd" %>
<html xmlns="http://www.w3.org/1999/xhtml" >
  <head id="Head1" runat="server">
    <title>Customers</title>
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
  
   function zipcodelook(){ 
  var NewWindow = window.open("zipcode_lookup.aspx","ZipCodeLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}

function ZipCodeLookup(ReturnObj1,ReturnObj2,ReturnObj3){
    document.forms[0].Zip_TextBox.value = ReturnObj1;    
    document.forms[0].State_TextBox.value = ReturnObj3;    
    document.forms[0].City_TextBox.value = ReturnObj2;
    document.forms[0].Zip_TextBox.focus();
}
function citylook(){ 
  var NewWindow = window.open("city_lookup.aspx","CityCodeLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}

function CityCodeLookup(ReturnObj1){ 
  
   document.forms[0].City_TextBox.value = ReturnObj1;
   document.forms[0].City_TextBox.focus();
    
}
function statecodelook(){ 
  var NewWindow = window.open("statecode_lookup.aspx","StateCodeLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}

function StateCodeLookup(ReturnObj1){ 
  document.forms[0].State_TextBox.value = ReturnObj1;
  document.forms[0].State_TextBox.focus();
    
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
    <form id="frmList" runat="server"  defaultfocus='cust_TextBox'>   
        <hd:header id="Header1" runat="server"></hd:header>
      <div>
          <asp:ScriptManager ID="ScriptManager1" runat="server">
          </asp:ScriptManager>
      <TABLE id="tblTop" cellSpacing="3" align="center" border="0" Width="100%">
        <TR>
          <TD width=30>&nbsp;</TD>
          <TD align=center nowrap><font size=+0><b>Customers&nbsp;</b></font></TD>
          <TD >
            <asp:linkbutton id="hlkBackToMenu" OnClick="Back_tomenu_Click" runat="server" >Back to menu</asp:linkbutton>
          
            &nbsp;&nbsp;Logged as&nbsp;
            <asp:label id="lblUser" runat="server" Font-Bold="True">&nbsp;</asp:label>&nbsp;&nbsp;&nbsp;
            <asp:linkbutton id="hlnkLogOut" runat="server" OnClick="hlnkLogOut_Click">Log out</asp:linkbutton>
            &nbsp;&nbsp;<asp:hyperlink id="hlnkChangePwd" runat="server" NavigateUrl="changepwd.aspx">Change password</asp:hyperlink>
            &nbsp;<b>Company:</b>&nbsp;  <asp:Label ID="labelcompany" runat="server" Text="Label"></asp:Label></TD>
          
         
          <TD vAlign="middle" width="20">&nbsp;</TD>
          
          <td width=30>&nbsp;</td>
        </TR>
      </TABLE>
      <table><tr bgcolor="gray"><td> <div  id="navigation" style="width:100%">
		<ul nowrap> <li class="selected" >
      <asp:LinkButton ID="lnk_Listcustomers" runat="server" >List customers</asp:LinkButton></li>
      <li><asp:LinkButton ID="lnk_viewcustomers" runat="server" OnClick="lnk_viewcustomers_Click" >View Customers</asp:LinkButton></li>
      <li><asp:LinkButton ID="lnk_listship" runat="server" OnClick="lnk_listship_Click" >List Ship To</asp:LinkButton></li>
      <li><asp:LinkButton ID="lnk_viewship" runat="server" OnClick="lnk_viewship_Click" >View Ship TO</asp:LinkButton></li>
      <li><asp:LinkButton ID="lnk_listsold" runat="server" OnClick="lnk_listsold_Click" >List Sold To</asp:LinkButton></li>
      <li><asp:LinkButton ID="lnk_viewsold" runat="server" OnClick="lnk_viewsold_Click" >View Sold TO</asp:LinkButton></li></ul></div>
      
      
      
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
                <tr><td ><b>Customer #</b></td><td align="center"><b> Name</b></td><td align="center"><b>City</b></td><td align="center" ><b>State</b></td><td align="center"><b>Zip</b></td>
                <td align="center" ><b>Type</b></td><td><b>Salesman</b></td><td><b>Territory</b></td></tr>
                <tr><td>
                    <asp:TextBox ID="cust_TextBox" Width="70px" runat="server"></asp:TextBox>
                     <a href="#" tabindex="1" onClick="contactcustomerlook(); return false"><asp:Image ID="Image5" runat="server" ImageUrl="images/lookup_icon.gif" /></a></td>
                    <td><asp:TextBox ID="Name_TextBox" Width="70px" runat="server"></asp:TextBox></td>
                    <td><asp:TextBox ID="City_TextBox" Width="70px" runat="server"></asp:TextBox>
                    <a href="#" tabindex="1" onClick="citylook(); return false"><asp:Image ID="Image9" runat="server" ImageUrl="images/lookup_icon.gif" /></a></td>
                    <td><asp:TextBox ID="State_TextBox" Width="50px" runat="server"></asp:TextBox>
                    <a href="#" tabindex="1" onClick="statecodelook(); return false"><asp:Image ID="Image1" runat="server" ImageUrl="images/lookup_icon.gif" /></a></td>
                    <td><asp:TextBox ID="Zip_TextBox" Width="50px" runat="server"></asp:TextBox>
                    <a href="#" tabindex="1" onClick="zipcodelook(); return false"><asp:Image ID="Image2" runat="server" ImageUrl="images/lookup_icon.gif" /></a></td>
                    <td><asp:TextBox ID="Type_TextBox" Width="50px" runat="server"></asp:TextBox></td>
                    <td><asp:TextBox ID="Salesman_TextBox" Width="50px" runat="server"></asp:TextBox>
                    <a href="#" tabindex="1" onClick="salesreplook(); return false"><asp:Image ID="Image3" runat="server" ImageUrl="images/lookup_icon.gif" /></a></td>
                    <td><asp:TextBox ID="Territory_TextBox" Width="50px" runat="server"></asp:TextBox>
                    <a href="#" tabindex="1" onClick="terrlook(); return false"><asp:Image ID="Image4" runat="server" ImageUrl="images/lookup_icon.gif" /></a></td></tr></table>
                  
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
          </td>
        </tr>
        <tr>
          <td>

    
           
            
          </TD>
        </TR>
        <tr><td> 
            <asp:GridView ID="GridView1" runat="server" DataSourceID="ObjectDataSource1" EmptyDataText=" Records not  Found" AllowPaging="True" AllowSorting="True" Width="800px" AutoGenerateColumns="False" BorderStyle="Dotted" CssClass="Grid" OnSelectedIndexChanged="GridView1_SelectedIndex">
                <EmptyDataRowStyle BorderColor="Gray" BorderStyle="Dotted" BorderWidth="0px" Font-Bold="True"
                        HorizontalAlign="Center" VerticalAlign="Middle" />
                        
                <Columns>
                    <asp:CommandField ShowSelectButton="true" ButtonType="image" SelectImageUrl="~/Images/sel.gif" SelectText="" > 
                    <ItemStyle Width="10px" />
                    </asp:CommandField>
                    
                    <asp:BoundField DataField="vcustno" HeaderText="customer #" SortExpression="vcustno" />
                    <asp:BoundField DataField="vcustname" HeaderText="Name" SortExpression="vcustname" />
                    <asp:BoundField DataField="vcity" HeaderText="City" SortExpression="vcity" />
                    <asp:BoundField DataField="vstate" HeaderText="State" SortExpression="vstate" />
                    <asp:BoundField DataField="vzip" HeaderText="Zip" SortExpression="vzip" />
                    <asp:BoundField DataField="vtype" HeaderText="Type" SortExpression="vtype" />
                    <asp:BoundField DataField="vsman" HeaderText="Salesman" SortExpression="vsman" />
                    <asp:BoundField DataField="vterr" HeaderText="Territory" SortExpression="vterr" />
                    <asp:BoundField DataField="vreckey" HeaderText="Reckey" Visible="false" SortExpression="vreckey" />
                                       
                </Columns>
                    <SelectedRowStyle CssClass="GridSelected" BackColor="Yellow" />
                    <HeaderStyle ForeColor="White" CssClass="headcolor" HorizontalAlign="Center"
                        VerticalAlign="Middle" Wrap="False" />
                    <AlternatingRowStyle CssClass="GridItemOdd" />
                    <RowStyle  CssClass="shade" />
            </asp:GridView></ContentTemplate>
                        </asp:UpdatePanel>
            <asp:ObjectDataSource ID="ObjectDataSource1" runat="server" OldValuesParameterFormatString="original_{0}" SelectMethod="SelectCustlist" TypeName="contact">
                <SelectParameters>
                    <asp:Parameter DefaultValue="Select" Name="prmAction" Type="String" />
                    <%--<asp:Parameter Name="prmComp" DefaultValue="" Type="String"  />                    
                    <asp:Parameter Name="prmUser" DefaultValue="" Type="String" />--%>
                    <asp:SessionParameter Name="prmComp" SessionField="Customers_Company" Type="string" />
                    <asp:SessionParameter Name="prmUser" SessionField="customer_user_id" Type="string" />
                     
                    <asp:Parameter Name="prmCustno" Type="String" />
                    <asp:Parameter Name="prmCustname" Type="String" />
                    <asp:Parameter Name="prmCity" Type="String" />
                    <asp:Parameter Name="prmState" Type="String" />
                    <asp:Parameter Name="prmZip" Type="String" />
                    <asp:Parameter Name="prmType" Type="String" />
                    <asp:Parameter Name="prmSman" Type="String" />
                    <asp:Parameter Name="prmTerr" Type="String" />
                    <asp:Parameter Name="prmReckey" Type="String" />
                </SelectParameters>
            </asp:ObjectDataSource>
          
        </td></tr>
      </TABLE>      
    </div>
    <ft:footer id="Footer1" runat="server"></ft:footer>
    </form>
  </body>
</HTML>

