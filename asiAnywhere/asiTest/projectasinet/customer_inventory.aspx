<%@ Page Language="c#" AutoEventWireup="true" Debug="false" Inherits="customerinentory" Codebehind="customer_inventory.aspx.cs" %>
<%@ Register Src="footer.ascx" TagName="Footer" TagPrefix="ft" %>
<%@ Register Src="header.ascx" TagName="Header" TagPrefix="hd" %>
<html xmlns="http://www.w3.org/1999/xhtml" >
  <head id="Head1" runat="server">
    <title>Customer Inventory</title>
    <LINK href="include/style2.css" type="text/css" rel="stylesheet"/>
    <LINK REL="stylesheet" TYPE="text/css" HREF="include/CalendarControl.css" >
    <script language = "JavaScript" src="include/CalendarControl.js"></script>
    <script language = JavaScript>
    
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

    function setfocus() {

        var da = document.getElementById("txt_cust");
        da.focus();

    }
    
    function contactcustomerlook(){ 
  var NewWindow = window.open("contact_customer_lookup.aspx","ContactCustomerLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}

function ContactCustomerLookup(ReturnObj1,ReturnObj2,ReturnObj3,ReturnObj4,ReturnObj5,ReturnObj6,ReturnObj7,ReturnObj8,ReturnObj9, ReturnObj10,ReturnObj11){ 
  document.forms[0].txt_cust.value = ReturnObj1;
  document.forms[0].txt_cust.focus();
  
    
}

function fglook(){ 
  var NewWindow = window.open("fgitem_lookup.aspx","FGLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}

function FGLookup(ReturnObj1){ 
  document.forms[0].txt_item.value = ReturnObj1;
  document.forms[0].txt_item.focus();
}

function ShipTOLook2(){ 
var lookHidden = document.getElementById("txt_cust").value;
  var NewWindow = window.open("ShipToCustLook.aspx?look="+ lookHidden +"","LocationLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}
function ShipTo2Lookup(ReturnObj1){
    document.forms[0].txt_ship.value = ReturnObj1;
    document.forms[0].txt_ship.focus();
}   
function custchk()
{
var cust = document.getElementById("txt_cust");
if (cust.value == "")
{
alert("First Customers A/R Code be Enter");
cust.focus();
}
}
    
    </script> 
  </head>    
   <body>
    <form id="frmList" runat="server"  defaultfocus='txt_cust'>   
        <hd:header id="Header1" runat="server"></hd:header>
      <div>       
            
                       
      <TABLE id="tblTop" cellSpacing="3"  border="0" Width="100%">
        <TR>
            
          <TD align=center nowrap><font size=+0><b>Customer Inventory&nbsp;</b></font></TD>
          <TD vAlign="middle" nowrap >
            <asp:linkbutton id="hlkBackToMenu" OnClick="Back_tomenu_Click" runat="server" >Back to menu</asp:linkbutton>
          </TD>
          <TD align="right"><font size=+0><b>&nbsp;&nbsp;</b></font></TD>
          <TD vAlign="middle" align="left">Logged as&nbsp;
            <asp:label id="lblUser" runat="server" Font-Bold="True">&nbsp;</asp:label>&nbsp;&nbsp;&nbsp;
            <asp:linkbutton id="hlnkLogOut" runat="server" OnClick="hlnkLogOut_Click">Log out</asp:linkbutton>
            &nbsp;&nbsp;<asp:hyperlink id="hlnkChangePwd" runat="server" NavigateUrl="changepwd.aspx">Change password</asp:hyperlink>
          &nbsp;<b>Company:</b>&nbsp;<asp:Label ID="labelcompany" runat="server" Text="Label"></asp:Label></TD>
          
         
          <TD vAlign="middle" width="20">&nbsp;</TD>
          
          <td width=30>&nbsp;</td>
        </TR>
      </TABLE>      
          <asp:Label ID="Label1" Font-Bold="true" ForeColor="red" runat="server" ></asp:Label>  
          <table   >
          <tr bgcolor="gray"><td><div  id="navigation" style="width:100%">
		<ul nowrap><li class="selected" >
          <asp:LinkButton ID="listImageButton"  OnClick="list_cust_click"  runat="server" >List Inventory</asp:LinkButton></li>
         <li><asp:LinkButton ID="viewImageButton" OnClick="view_cust_click"  runat="server" >View Inventory</asp:LinkButton> </li></ul></div> </td>
          </tr>
          </table>
          
          
          <TABLE id="tblMain" align="left" cellSpacing="1" cellPadding="5"  width='100%' border="0">
          
        <TR>
          <TD style="width: 600px; height: 60px;">
             <TABLE id="tblSearch" cellSpacing="1" cellPadding="1" width="100%" border="0"  bgcolor=black>
                            
          </tr>
              <TR>
                <TD class="shade" ><br>
                  <table cellspacing="2" cellpadding="1"  border="0" class="shade" bgcolor="gray">    		   
		   <tr>
        		<td nowrap> <asp:Button ID="btnSearch" runat="server" Text="Go" OnClick="btnSearch_Click" CssClass="button" Width="40px" /><br />
                    <br />
                    <asp:Button ID="btn_reset" runat="server" CssClass="button" Text="All" OnClick="btn_reset_Click" Width="40px" />&nbsp;</td>
                        <td nowrap>Customer# <br>
            <asp:TextBox ID="txt_cust" runat="server" Width="65px" ></asp:TextBox>
            <a href="#" tabindex="1" onClick="contactcustomerlook(); return false"><asp:Image ID="Image1" runat="server" ImageUrl="images/lookup_icon.gif" /></a>			
                	</td>   	   
                	<td nowrap>Ship To<br>
            <asp:TextBox ID="txt_ship" runat="server" MaxLength="15" Width="100px" ></asp:TextBox>
            	  <a href="#" tabindex="1" onfocus="custchk()" onClick="ShipTOLook2(); return false"><asp:Image ID="Image2" runat="server" ImageUrl="images/lookup_icon.gif" /></a>					
                	</td> 
           		    <td nowrap>FG Item# <br>
                    <asp:TextBox ID="txt_item" runat="server" MaxLength="15" Width="100px" ></asp:TextBox>
			        <a href="#" tabindex="1" onClick="fglook(); return false"><asp:Image ID="FGLookup" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
                	</td>  
            		       
                	                  
                  <td nowrap>Rows/Page<br>
                  
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
                             
                              <asp:TextBox ID="aLineLabel" runat="server" onblur="setfocus()" Width="40px" OnTextChanged="ddl_display_TextChanged" Text='<%# Bind("aLine") %>'></asp:TextBox>
                              <asp:CompareValidator ID="CompareValidator4" runat="server" ControlToValidate="aLineLabel" SetFocusOnError="true" Display="dynamic" Operator="dataTypeCheck" Type="Integer" ErrorMessage="Invalid Input"></asp:CompareValidator>
                              <%--<asp:Label ID="aLineLabel" runat="server" Text='<%# Bind("aLine") %>'></asp:Label>--%>
                          </ItemTemplate>
                      </asp:FormView>
                      <asp:ObjectDataSource ID="ObjectDataSource2" runat="server" OldValuesParameterFormatString="original_{0}"
                          SelectMethod="SelectRows" TypeName="Order">
                          <SelectParameters>
                              <asp:SessionParameter Name="prmUser" SessionField="Rowuser" Type="String" />
                              <asp:SessionParameter Name="vLine" Type="Int32" SessionField="gridsize" />
                          </SelectParameters>
                      </asp:ObjectDataSource>
                  
                 </td>
			<td></td>
			<td></td>                    
			
                  </tr>
                  </table>              
         
     </TD></TR></TABLE>
   
    
        <asp:GridView ID="GridView1" runat="server" AutoGenerateColumns="False" 
        AllowPaging="True" AllowSorting="True" EmptyDataText="No Records Found" Width="600px" BorderStyle="Dotted" CssClass="Grid" PageSize="50" 
         DataSourceID="ObjectDataSource1" OnSelectedIndexChanged="GridView1_SelectedIndexChanged">
        <SelectedRowStyle CssClass="GridSelected" BackColor="Yellow" />
            <AlternatingRowStyle CssClass="GridItemOdd" />
            <EmptyDataRowStyle BorderStyle="Dotted" BorderColor="Gray" BorderWidth="0px" Font-Bold="True" HorizontalAlign="Center" VerticalAlign="Middle" />
        <RowStyle CssClass="shade"  />
            
            <HeaderStyle HorizontalAlign="Center" VerticalAlign="Middle" Wrap="False" ForeColor="White" CssClass="headcolor" Height="40px" />
            <Columns >
            <asp:CommandField ShowSelectButton="True" ButtonType="Image" SelectImageUrl="~/Images/sel.gif" SelectText="" > 
                    <ItemStyle Width="10px" />
                    </asp:CommandField>
            
                <asp:BoundField DataField="vCustno" HeaderText="Customer#"  SortExpression="vCustno" />
                <asp:BoundField DataField="vLoc" HeaderText="Ship-To" SortExpression="vLoc" />
                <asp:BoundField DataField="vItem" HeaderText="FG Item#" SortExpression="vItem" />
                <asp:BoundField DataField="vQty" HeaderText="On-Hand Qty" SortExpression="vQty" />
                <asp:BoundField DataField="vConsum" HeaderText="Annual Consumption" SortExpression="vConsum" />
                                
                 
            </Columns>
        </asp:GridView>
        <asp:ObjectDataSource ID="ObjectDataSource1" runat="server" OldValuesParameterFormatString="original_{0}"
            SelectMethod="SelectCustitem" TypeName="custitem">
            <SelectParameters>
                <asp:Parameter Name="prmUser" Type="String" />
                <asp:Parameter Name="prmActCitem" DefaultValue="SelCustItem" Type="String" />
                <asp:Parameter Name="prmCust" Type="String" />
                <asp:Parameter Name="prmLoc" Type="String" />
                <asp:Parameter Name="prmItem" Type="String" />
                <asp:Parameter Name="prmQty" Type="Decimal" />
                <asp:Parameter Name="prmConsum" Type="Decimal" />
                <asp:Parameter Name="prmRecKey" Type="string" />
                <asp:Parameter Name="prmUpdateCust" Type="string" />
                <asp:Parameter Name="prmUpdateloc" Type="string" />
                <asp:Parameter Name="prmUpdateItem" Type="string" />
            </SelectParameters>
        </asp:ObjectDataSource>
        </DIV>
    </form>
  </body>
</HTML>

