<%@ Page Language="c#" AutoEventWireup="true" Debug="true" Inherits="list_receipt" Codebehind="item_daily_receipt.aspx.cs" %>
<%@ Register Src="footer.ascx" TagName="Footer" TagPrefix="ft" %>
<%@ Register Src="header.ascx" TagName="Header" TagPrefix="hd" %>
<html xmlns="http://www.w3.org/1999/xhtml" >
  <head id="Head1" runat="server">
    <title>Customer Plant Receipts</title>
    
    <link href="include/style.css" type="text/css" rel="stylesheet"/>
      <LINK REL="stylesheet" TYPE="text/css" HREF="include/CalendarControl.css" >
    <link href="include/tree.css" rel="stylesheet" type="text/css" />
    <link href="include/dhtmlwindow.css" rel="stylesheet" type="text/css" />
    <script language="javascript" src="include/date.js"></script>
    <script language="javascript" src="include/event.js"></script>
    <script language="javascript" src="include/insert.js"></script>
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
   

function fglook(){ 
  var NewWindow = window.open("fgitem_lookup.aspx","FGLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}

function FGLookup(ReturnObj1){ 
  document.forms[0].txt_item.value = ReturnObj1;
}

function contactcustomerlook(){ 
  var NewWindow = window.open("contact_customer_lookup.aspx","ContactCustomerLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}

function ContactCustomerLookup(ReturnObj1,ReturnObj2,ReturnObj3,ReturnObj4,ReturnObj5,ReturnObj6,ReturnObj7,ReturnObj8,ReturnObj9, ReturnObj10,ReturnObj11){ 
  document.forms[0].txt_po.value = ReturnObj1;
   
}

function setestdate() {

    var da = document.getElementById("txt_seqno");
    da.focus();

}
function setdate() {

    var da = document.getElementById("txt_recdate");
    da.focus();

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
function datevalidate()
{
    var date=document.getElementById("txt_recdate").value;
    
    if(date.length>1 && date.length<3 && date.indexOf('/')!=1)
    {
        document.getElementById("txt_recdate").value = date + "/";
    }
    if(date.length>4 && date.length<6 && date.indexOf('/')!=3)
    {
        document.getElementById("txt_recdate").value = date + "/";
    }   
}
    </script> 
  </head>    
   <body>
    <form id="frmList" runat="server"   defaultfocus ='txt_seqno' >   
        <hd:header id="Header1" runat="server"></hd:header>
      <div>
            
      <table id="tblTop" cellSpacing="3" border="0">
        <tr>
          
          <td align=center nowrap><font size=+0><b>Customer Plant Receipts &nbsp;</b></font></td>
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
		<ul nowrap><li class="selected">
    <asp:LinkButton ID="lnk_listitem" runat="server" OnClick="lnk_list_click">List Receipt </asp:LinkButton></li>
    <li> <asp:LinkButton ID="lnk_viewitem" runat="server" OnClick="lnk_view_click">View Receipt</asp:LinkButton></li></ul></div>
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
                  <b>Seq#</b> <br />
                    <asp:TextBox ID="txt_seqno" Width="70px" runat="server"></asp:TextBox>
                    <asp:CompareValidator ID="CompareValidator1" runat="server" ErrorMessage="Only Numbers" ControlToValidate="txt_seqno" Display="dynamic" SetFocusOnError="true" Operator="datatypecheck" Type="integer"></asp:CompareValidator>
                    </td>
                    <td class="shade" align="center" nowrap >
                   <b>Usage/Reciept Date</b><br />
                    <asp:textbox id="txt_recdate" runat="server" onfocus="javascript:preEnter( this, 'yes' );" onblur="javascript:preLeave( this, 'date', '99/99/9999' );" MaxLength="10" ToolTip="MM/DD/YYYY" Width="90px"></asp:textbox>
                     <a href="#" tabindex="1" onblur="setdate()" onClick="showCalendarControl(txt_recdate); return false"><asp:Image ID="Image1" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
                     
                    </td>  
                    <td class="shade" align="center" nowrap >
                   <b>CustPo#</b><br />
                    <asp:textbox id="txt_po" runat="server" MaxLength="15" Width="100px"></asp:textbox>                    
                    </td>            
                      <td class="shade" align="center" nowrap >
                      <b>Cust Part#</b> <br />
                    <asp:TextBox ID="txt_custpart" MaxLength="15" Width="100px" runat="server" ></asp:TextBox>
                    
                    </td>
                     
                     <td class="shade" align="center" nowrap > 
                    <b>FG Item#</b> <br />
                    <asp:TextBox ID="txt_item" MaxLength="15" Width="100px" runat="server"></asp:TextBox>
                    <a href="#" tabindex="1" onClick="fglook(); return false"><asp:Image ID="FGLookup" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
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
            <HeaderStyle  BackColor="Teal" ForeColor="White" Height="40px"  VerticalAlign="Middle"  HorizontalAlign="Center" ></HeaderStyle>
            <RowStyle CssClass="shade"  />
            <Columns>
                <asp:CommandField ShowSelectButton="True" ButtonType="Image" SelectImageUrl="~/Images/sel.gif" SelectText="" > 
                    <ItemStyle Width="10px" />
                </asp:CommandField>
                <asp:BoundField DataField="vSeqNum" HeaderText="Seq#" SortExpression="vSeqNum" >
                    <ItemStyle  Wrap="false" />
                </asp:BoundField>   
                
                <asp:BoundField DataField="vBolno" HeaderText="Suppliers BOL No" SortExpression="vBolno" />  
                <asp:BoundField DataField="vCustPartNum" HeaderText="Customers Part#" SortExpression="vCustPartNum" />   
                <asp:BoundField DataField="vQtyUsed" HeaderText="Receipt Quantity " SortExpression="vQtyUsed" />
                <asp:BoundField DataField="vOnHandQty" HeaderText="Cust. Plant on Hand Qty" SortExpression="vOnHandQty" />                     
                <asp:TemplateField HeaderText="Customers Receipt Date" SortExpression="vUsgDate">                    
                    <ItemTemplate>
                        <asp:Label ID="Label3" runat="server" Text='<%# Bind("[vUsgDate]","{0:MM/dd/yyyy}") %>'></asp:Label>
                    </ItemTemplate>
                    <ItemStyle Wrap="False" />
                </asp:TemplateField>
                <asp:BoundField DataField="vCustVenCode" HeaderText="Customers A/P Code" SortExpression="vCustVenCode" /> 
                <asp:BoundField DataField="vCustPlantId" HeaderText="Customers Plant ID" SortExpression="vCustPlantId" />
                <asp:BoundField DataField="vDeptCode" HeaderText="Customers Dept Code" SortExpression="vDeptCode" />
                <asp:BoundField DataField="vItemFgNum" HeaderText="Suppliers FG Item" SortExpression="vItemFgNum" /> 
                <asp:BoundField DataField="vVenOrdNum" HeaderText="Suppliers Order#" SortExpression="vVenOrdNum" />
                <asp:BoundField DataField="vCustPoLineNum" HeaderText="Line#" SortExpression="vCustPoLineNum" />             
                <asp:BoundField DataField="vCustPoNum" HeaderText="Customers Po#" SortExpression="vCustPoNum" />
                <asp:BoundField DataField="vSellPrice" HeaderText="Suppliers Item Sell Price" SortExpression="vSellPrice" /> 
                <asp:BoundField DataField="vVenJobNum" HeaderText="Suppliers Job#" SortExpression="vVenJobNum" />                                                                               
                <asp:BoundField DataField="vVenJob2Num" HeaderText="" SortExpression="vVenJob2Num" />            
                             
                <%--<asp:BoundField DataField="vTransType" HeaderText="Trans-type" SortExpression="vTransType" />--%>
             
             
         </Columns>
     </asp:GridView>
              <asp:ObjectDataSource ID="ObjectDataSource1" runat="server" OldValuesParameterFormatString="original_{0}"
                  SelectMethod="Selectreceipt" TypeName="custitem">
                  <SelectParameters>
                      <asp:Parameter Name="prmUser" Type="String" />
                      <asp:Parameter Name="prmAction" Type="String" DefaultValue="Select" />
                      <asp:Parameter Name="prmSeqNum" Type="Int32" />
                      <asp:Parameter Name="prmComp" Type="String" />
                      <asp:Parameter Name="prmRcptDate" Type="DateTime" />
                      <asp:Parameter Name="prmCustPoNum" Type="String" />
                      <asp:Parameter Name="prmCustPartNum" Type="String" />
                      <asp:Parameter Name="prmFgItemNum" Type="String" />
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

