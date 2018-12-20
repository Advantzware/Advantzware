<%@ Page Language="c#" AutoEventWireup="True" Debug="true" Inherits="returns_rcpt_list" Codebehind="returns_rcpt_list.aspx.cs" %>
<%@ Register Src="footer.ascx" TagName="Footer" TagPrefix="ft" %>
<%@ Register Src="header.ascx" TagName="Header" TagPrefix="hd" %>
<html xmlns="http://www.w3.org/1999/xhtml" >
  <head id="Head1" runat="server">
    <title>FG Wharehouse Returns </title>
    
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

    var da = document.getElementById("txt_date");
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


function retrntaglook() {
    //var loc1 = document.forms[0].ctl00_ContentPlaceHolder1_txt_tag.value;
    var NewWindow = window.open("return_tag_lookup.aspx", "retrntagLookUpWindow", "width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}
function retrntaglookup(ReturnObj1) {
    document.forms[0].txt_tag.value = ReturnObj1;
    document.forms[0].txt_tag.focus();

}
function job1look() {
    var NewWindow = window.open("job1_lookup.aspx", "Job1LookupWindow", "width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}
function Job1Lookup(ReturnObj1) {
    document.forms[0].txt_job.value = ReturnObj1;
    document.forms[0].txt_job.focus();
}

function customerpolook() {

    var NewWindow = window.open("lpofglook.aspx", "CustomerpoWindow", "width=650,height=400,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}

function lpofglLookup(ReturnObj1) {
    document.forms[0].txt_po.value = ReturnObj1;
    document.forms[0].txt_po.focus();
}
    </script> 
  </head>    
   <body>
    <form id="frmList" runat="server"   defaultfocus ='txt_tag' >   
        <hd:header id="Header1" runat="server"></hd:header>
       <table width="100%"><tr><td><div>
    <table align="left" border="1" width="75%">
                <tr align="left" class="topheadcolor">
                   
                    
                    <td nowrap align="left" width="25px";>
                        <asp:ImageButton ID="img_btn_add" runat="server" Width="35px" ImageUrl="~/Images/add.bmp" OnClick="img_btn_add_click" ToolTip="Add"  />
                      </td>
                     <%--<td nowrap width="25px";>
                        <a href="#" onClick="ordernotes(); return false"><asp:Image ID="img_btn_notes" Width="35px" ToolTip="Notes" runat="server" ImageUrl="~/Images/edit.ico" /></a>                        
                        </td>
                        <td nowrap width="25px";>
                        <a href="#" onClick="printspec(); return false"><asp:Image ID="Image3" Width="35px" runat="server" ToolTip="Spec Notes" ImageUrl="~/Images/dict.ico" /></a>
                        </td>
                        <td nowrap width="25px";>
                        <a href="#" onClick="orderhelp(); return false"><asp:Image ID="img_help" Width="35px" ToolTip="Help" runat="server" ImageUrl="~/Images/help.ico" /></a>
                        </td>--%>
                      <td nowrap align="left" width="25px";>
                        <asp:ImageButton ID="img_btn_exit" runat="server" ImageUrl="~/Images/exit-au.bmp" Width="35px" OnClick="hlnkLogOut_Click" ToolTip="LogOut"  />
                    </td>
                     <td align="left" nowrap> &nbsp;</td>
                </tr>
      </table></td></tr>
    <tr><td>
    <div>
     <TABLE id="TABLE1" cellSpacing="3" align="center" border="0" Width="100%">
        <TR>
          
          <TD align=left nowrap><font size=+0><b>FG Wharehouse Returns &nbsp; </b></font></TD>
          <TD vAlign="middle">
            <asp:linkbutton id="LinkButton2" runat="server" OnClick="LinkButton1_Click">Back to menu</asp:linkbutton>
          </TD>
          <TD align="right"><font size=+0><b>Users&nbsp;&nbsp;</b></font></TD>
          <TD vAlign="middle" align="left">Logged as&nbsp;
            <asp:label id="lblUser" runat="server" Font-Bold="True">&nbsp;</asp:label>&nbsp;&nbsp;&nbsp;
            <asp:linkbutton id="Linkbutton3" runat="server" OnClick="hlnkLogOut_Click">Log out</asp:linkbutton>
                        
            Company:&nbsp;
            <asp:label id="lblComp" runat="server" Font-Bold="True">&nbsp;</asp:label>&nbsp;&nbsp;&nbsp;
          </TD>
          
                    
          <TD vAlign="middle" width="20">&nbsp;</TD>
          <td width=30>&nbsp;</td>
        </TR>
      </TABLE>  
  </div>
     </td></tr></table>
      <table>
    <tr style="background-color:Gray">
    <td><div  id="navigation" style="width:100%">
		<ul nowrap><li class="selected">
    <asp:LinkButton ID="lnk_listitem" runat="server" OnClick="lnk_list_click">List Receipt</asp:LinkButton></li>
    <li><asp:LinkButton ID="lnk_viewitem" runat="server" OnClick="lnk_view_click">View Receipt</asp:LinkButton></li></ul></div>
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
                   <b>Tag#</b><br />
                    <asp:textbox id="txt_tag" runat="server" MaxLength="20" Width="150px"></asp:textbox>                    
                     <a href="#" tabindex="1" onClick="retrntaglook(); return false"><asp:Image ID="retrntagimage" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
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
                <asp:BoundField DataField="vTag" HeaderText="Tag#"  SortExpression="vTag" HeaderStyle-Wrap="false" ItemStyle-Wrap="false" />
                <asp:BoundField DataField="vLoc" HeaderText="Whse"   SortExpression="vLoc" HeaderStyle-Wrap="false" ItemStyle-Wrap="false" />                              
                <asp:BoundField DataField="vLocBin" HeaderText="Bin" SortExpression="vLocBin" HeaderStyle-Wrap="false" ItemStyle-Wrap="false" />  
                <asp:BoundField DataField="vDate" HeaderText="Count Date" SortExpression="vDate" HeaderStyle-Wrap="false" ItemStyle-Wrap="false" />     
                <asp:BoundField DataField="vTransTime" HeaderText="Count Time" SortExpression="vTransTime" HeaderStyle-Wrap="false" ItemStyle-Wrap="false" />
                <asp:BoundField DataField="vItem" HeaderText="Item No" SortExpression="vItem" HeaderStyle-Wrap="false" ItemStyle-Wrap="false" />                 
                <asp:BoundField DataField="vItemName" HeaderText="Item Name" SortExpression="vItemName" HeaderStyle-Wrap="false" ItemStyle-Wrap="false" /> 
                <asp:BoundField DataField="vJob_no" HeaderText="Job#" SortExpression="vJob_no" HeaderStyle-Wrap="false" ItemStyle-Wrap="false" />                     
                <asp:BoundField DataField="vJob_no2" HeaderText="" SortExpression="vJob_no2" HeaderStyle-Wrap="false" ItemStyle-Wrap="false" />  
                <asp:BoundField DataField="vT_Qty" HeaderText="Qty" SortExpression="vT_Qty" /> 
                <asp:BoundField DataField="vCases" HeaderText="Cases" SortExpression="vCases" HeaderStyle-Wrap="false" ItemStyle-Wrap="false" />
                <asp:BoundField DataField="vCasUnit" HeaderText="Cases/Bundles Per Unit"  SortExpression="vCasUnit" HeaderStyle-Wrap="false" ItemStyle-Wrap="false" ></asp:BoundField>    
                <asp:BoundField DataField="vQtyCas" HeaderText="Qty/Case"  SortExpression="vQtyCas" HeaderStyle-Wrap="false" ItemStyle-Wrap="false" />  
                <asp:BoundField DataField="vPartial" HeaderText="Partial" SortExpression="vPartial" HeaderStyle-Wrap="false" ItemStyle-Wrap="false" />  
                <asp:BoundField DataField="vStdCost" HeaderText="Cost" SortExpression="vStdCost" />
                <asp:BoundField DataField="vCostUom" HeaderText="Cost Uom"  SortExpression="vCostUom" />
                <asp:BoundField DataField="vExtCost" HeaderText="Ext Cost" SortExpression="vExtCost" />
                <asp:BoundField DataField="vinvno" HeaderText="Counted Qty" SortExpression="vinvno" />
                 
                
                
                <asp:BoundField Visible="false" DataField="vRecKey" HeaderText="vRecKey" 
                    SortExpression="vRecKey" />
                <asp:TemplateField HeaderText="">
                <ItemTemplate>
                <asp:Label ID="seqlabel" Visible="false" runat="server" Text='<%# Bind("vRno") %>' ></asp:Label>
                </ItemTemplate>
                </asp:TemplateField>
                            
             
         </Columns>
     </asp:GridView>
              <asp:ObjectDataSource ID="ObjectDataSource1" runat="server" OldValuesParameterFormatString="original_{0}"
                  SelectMethod="SelectReturnsGoods" TypeName="itemhistory">
                  <SelectParameters>
                      <asp:Parameter Name="prmUser" Type="String" />
                      <asp:Parameter Name="prmAction" Type="String" DefaultValue="GridSelect" />
                      <asp:Parameter Name="prmFgItem" Type="String" />
                      <asp:Parameter  Name="prmJobno" Type="String" />
                      <asp:Parameter  Name="prmPono" Type="String" />
                      <asp:Parameter  Name="prmSeqno" Type="String" />
                      <asp:Parameter  Name="prmRcptDate" Type="String" />
                      <asp:SessionParameter SessionField="return_recept_list_tag" Name="prmTagno" Type="String" />
                      <asp:Parameter Name="prmTransTime" Type="String" />
                      <asp:Parameter Name="prmJob_no2" Type="Int32" />
                      <asp:Parameter Name="prmName" Type="String" />
                      <asp:Parameter Name="prmloc" Type="String" />
                      <asp:Parameter Name="prmlocbin" Type="String" />
                      <asp:Parameter Name="prmCases" Type="Int32" />
                      <asp:Parameter Name="prmQty_Cas" Type="Int32" />
                      <asp:Parameter Name="prmCasUnit" Type="Int32" />
                      <asp:Parameter Name="prmPartial" Type="Int32" />
                      <asp:Parameter Name="prmStdCost" Type="Decimal" />
                      <asp:Parameter Name="prmCost_Uom" Type="String" />
                      <asp:Parameter Name="prmTqty" Type="Decimal" />
<asp:Parameter Name="prmExtCost" Type="Decimal"></asp:Parameter>
<asp:Parameter Name="prmInvNo" Type="Int32"></asp:Parameter>
                      <asp:Parameter Name="prmRecKey" Type="String" />
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

