<%@ Page Language="c#" AutoEventWireup="true" Debug="true" Inherits="list_cust_inv" Codebehind="item_cust_inv.aspx.cs" %>
<%@ Register Src="footer.ascx" TagName="Footer" TagPrefix="ft" %>
<%@ Register Src="header.ascx" TagName="Header" TagPrefix="hd" %>
<html xmlns="http://www.w3.org/1999/xhtml" >
  <head id="Head1" runat="server">
    <title>File Maintenance Item</title>
    
    <link href="include/style.css" type="text/css" rel="stylesheet"/>
      <LINK REL="stylesheet" TYPE="text/css" HREF="include/CalendarControl.css" >
    <link href="include/tree.css" rel="stylesheet" type="text/css" />
    <link href="include/dhtmlwindow.css" rel="stylesheet" type="text/css" />
    <script language = "JavaScript" src="include/CalendarControl.js">
    
    </script>
    <script language = "JavaScript" type="text/javascript">
    
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

    function setestdate() {

        var da = document.getElementById("txt_vencode");
        da.focus();

    }
   

function fglook(){ 
  var NewWindow = window.open("fgitem_lookup.aspx","FGLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}

function FGLookup(ReturnObj1){
    document.forms[0].txt_venfgitem.value = ReturnObj1;
    document.forms[0].txt_venfgitem.focus();
}

function contactcustomerlook(){ 
  var NewWindow = window.open("contact_customer_lookup.aspx","ContactCustomerLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}

function ContactCustomerLookup(ReturnObj1,ReturnObj2,ReturnObj3,ReturnObj4,ReturnObj5,ReturnObj6,ReturnObj7,ReturnObj8,ReturnObj9, ReturnObj10,ReturnObj11){
    document.forms[0].txt_vencode.value = ReturnObj1;
    document.forms[0].txt_vencode.focus();
   
}



 function Datelook(){ 
  var NewWindow = window.open("date_lookup.aspx","DateLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
  }
function Datelookup(obj)
{
  document.forms[0].txt_recdate.value=obj;
}

function Datelook1()
{
  document.forms[0].txt_recdate.value="";
  Datelook();
}
    </script> 
  </head>    
   <body>
    <form id="frmList" runat="server"   defaultfocus ='txt_vencode' >   
        <hd:header id="Header1" runat="server"></hd:header>
      <div>
            
      <table id="tblTop" cellSpacing="3" border="0">
        <tr>
          
          <td align=center nowrap><font size=+0><b>File Maintenance Item&nbsp;</b></font></td>
          <td vAlign="middle">
            <asp:linkbutton id="hlkBackToMenu" runat="server" OnClick="hlkBackToMenu_Click"></asp:linkbutton>
          </td>
          <td vAlign="middle" align="center"><b></b>&nbsp;&nbsp;&nbsp;Logged as&nbsp;
            <asp:label id="lblUser" runat="server" Font-Bold="True">&nbsp;</asp:label>&nbsp;&nbsp;&nbsp;
            <asp:linkbutton id="hlnkLogOut" runat="server" OnClick="hlnkLogOut_Click">Log out</asp:linkbutton>
            &nbsp;&nbsp;&nbsp;<asp:hyperlink id="hlnkChangePwd" runat="server" NavigateUrl="changepwd.aspx">Change password</asp:hyperlink>
          </td>  <td>&nbsp;&nbsp;&nbsp;
            <b>Company:</b>&nbsp;
            <asp:label id="lblComp" runat="server" Font-Bold="True">&nbsp;</asp:label>&nbsp;&nbsp;&nbsp;
          </TD>        
         
          
          <td vAlign="middle" width="20">&nbsp;</td>
          
          <td width=30>&nbsp;</td>
        </tr>
      </table>
      <table>
    <tr style="background-color:Gray">
    <td><div  id="navigation" style="width:100%">
		<ul nowrap><li class="selected" >
    <asp:LinkButton ID="lnk_listitem" runat="server" OnClick="lnk_list_click">List Item</asp:LinkButton></li>
    <li><asp:LinkButton ID="lnk_viewitem" runat="server" OnClick="lnk_view_click">View Item</asp:LinkButton></li></ul></div>
    </td>
    </tr></table>
     
      <TABLE id="tblMain" cellSpacing="1" Width="700px" cellPadding="1" border="0">
        <TR>
          <TD style="width: 761px">
            <TABLE id="tblSearch" cellSpacing="1" Width="700px"  cellPadding="5" border="0"  bgcolor=black>
              <TR>
                <TD class="shade" align="center" width="50">
                  <table cellspacing="2" cellpadding="1"  border="0" class="shade" bgcolor="gray">    		   
		            <tr><td>
                  <asp:button id="btnSearch" runat="server" Width="40px" CssClass="button" Text="Go" OnClick="btnSearch_Click"></asp:button>
                  <br />
                 
                  <asp:button id="btnShowAll" runat="server" Width="40px" CssClass="button" Text="All" OnClick="btnShowAll_Click"></asp:button>
                </TD>  
                         
                     <td class="shade" align="center" nowrap >
                  <b>Suppliers A/R Code</b> <br />
                    <asp:TextBox ID="txt_vencode" Width="80px" runat="server"></asp:TextBox>
                    <a href="#" tabindex="1" onClick="contactcustomerlook(); return false"><asp:Image ID="Image1" runat="server" ImageUrl="images/lookup_icon.gif" /></a>                    
                    </td>
                    <td class="shade" align="center" nowrap >
                   <b>Suppliers FG Item</b><br />
                    <asp:textbox id="txt_venfgitem" runat="server" MaxLength="15"  Width="120px"></asp:textbox> 
                    <a href="#" tabindex="1" onClick="fglook(); return false"><asp:Image ID="FGLookup" runat="server" ImageUrl="images/lookup_icon.gif" /></a>                    
                    </td>  
                    <%--<td class="shade" align="center" nowrap >
                   <b>Cust A/P Vendor Code</b><br />
                    <asp:textbox id="txt_apvencode" runat="server" Width="90px"></asp:textbox>                    
                    </td> --%>           
                      <td class="shade" align="center" nowrap >
                      <b>Customers Part#</b> <br />
                    <asp:TextBox ID="txt_custpart" Width="120px" MaxLength="15" runat="server" ></asp:TextBox>
                    
                    </td>                     
                    
                <td align="center" class="shade" nowrap>
           <b> Records/Page</b><BR>
                        
            <asp:FormView ID="FormView2" runat="server" DataSourceID="ObjectDataSource2">
                          
                          <ItemTemplate>                             
                              <asp:TextBox ID="aLineLabel" runat="server" Width="70px" onblur="setestdate()" OnTextChanged="ddl_display_TextChanged" Text='<%# Bind("aLine") %>'></asp:TextBox>
                              <asp:CompareValidator ID="CompareValidator4" runat="server" ControlToValidate="aLineLabel" SetFocusOnError="true" Display="dynamic" Operator="dataTypeCheck" Type="Integer" ErrorMessage="Invalid Input"></asp:CompareValidator>
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
                </TR></table>
               </TD>
              </TR>
            </TABLE>
           
          </td>
        </tr>
        <tr>
          <td style="width: 761px">

     <asp:GridView ID="GridView1" AutoGenerateColumns="False" runat="server" DataSourceID="ObjectDataSource1"
     AllowPaging="True" AllowSorting="True" 
            EmptyDataText="No Records Found" BorderStyle="Dotted" CssClass="Grid" OnSelectedIndexChanged="GridView1_SelectedIndexChanged">
            <SelectedRowStyle CssClass="GridSelected" BackColor="Yellow" />
            <AlternatingRowStyle CssClass="GridItemOdd" />
            <EmptyDataRowStyle BorderStyle="Dotted" BorderColor="Gray" BorderWidth="0px" Font-Bold="True" HorizontalAlign="Center" VerticalAlign="Middle" />
            <HeaderStyle   ForeColor="White" CssClass="headcolor" Height="40px"  VerticalAlign="Middle"  HorizontalAlign="Center" ></HeaderStyle>
        <RowStyle CssClass="shade"  />
         <Columns>
             <asp:CommandField ShowSelectButton="True" ButtonType="Image" SelectImageUrl="~/Images/sel.gif" SelectText="" > 
                    <ItemStyle Width="10px" />
                    </asp:CommandField>   
		<asp:TemplateField HeaderText="Reckey"  Visible="False" >                    
                    <ItemTemplate>
                        <asp:Label ID="Label3" runat="server" Text='<%# Bind("[vRecKey]") %>'></asp:Label>
                    </ItemTemplate>
                    <ItemStyle Wrap="False" />
                </asp:TemplateField>               
             
             <asp:BoundField DataField="vCustNum" HeaderText="Suppliers A/R Code" SortExpression="vCustNum" />
             <asp:BoundField DataField="vRevision" HeaderText="REV" SortExpression="vRevision" />
             <asp:BoundField DataField="vFgItmNum" HeaderText="Suppliers FG Item" SortExpression="vFgItmNum" />
             <asp:BoundField DataField="vCustPartNum" HeaderText="Customers Part#" SortExpression="vCustPartNum" />             
             <asp:BoundField DataField="vCustVenCode" HeaderText="Customers A/P Code" SortExpression="vCustVenCode" />
             <asp:BoundField DataField="vVendPlantId" HeaderText="Customers Plant Id" SortExpression="vVendPlantId" />             
             <asp:BoundField DataField="vVendorDeptCode" HeaderText="Customers Dept Code" SortExpression="vVendorDeptCode" />
             <asp:BoundField DataField="vOsoleteDate" HeaderText="Obsolete Date" SortExpression="vOsoleteDate" />   
             <asp:BoundField DataField="vAnnUsage" HeaderText="Customers Est. Annual usage" SortExpression="vAnnUsage" />          
             <asp:BoundField DataField="vCustHandQty" HeaderText="Customers On Hand Qty" SortExpression="vCustHandQty" />             
             
         </Columns>
     </asp:GridView>
              <asp:ObjectDataSource ID="ObjectDataSource1" runat="server" OldValuesParameterFormatString="original_{0}"
                  SelectMethod="Selectcustinv" TypeName="custitem">
                  <SelectParameters>
                      <asp:Parameter Name="prmUser" Type="String" />
                      <asp:Parameter Name="prmAction" Type="String" DefaultValue="Select" />
                      <asp:Parameter Name="prmComp" Type="String" />
                      <asp:Parameter Name="prmCustNum" Type="String" />
                      <asp:Parameter Name="prmCustPartNum" Type="String" />
                      <asp:Parameter Name="prmFgItmNum" Type="String" />
                      <asp:Parameter Name="prmVendCode" Type="String" />
                      <asp:Parameter Name="prmVendPlantId" Type="String" />
                      <asp:Parameter Name="prmAnnUsageQty" Type="Decimal" />
                      <asp:Parameter Name="prmOnHandQty" Type="Decimal" />
                  </SelectParameters>
              </asp:ObjectDataSource>           
           
          </TD>
        </TR>
      </TABLE>      
      
      
    </div>
    <ft:footer id="Footer1" runat="server"></ft:footer>
    </form>
  </body>
</html>

