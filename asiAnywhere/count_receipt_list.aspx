<%@ Page Language="c#" AutoEventWireup="True" Debug="true" Inherits="count_receipt_list" Codebehind="count_receipt_list.aspx.cs" %>
<%@ Register Src="footer.ascx" TagName="Footer" TagPrefix="ft" %>
<%@ Register Src="header.ascx" TagName="Header" TagPrefix="hd" %>
<html xmlns="http://www.w3.org/1999/xhtml" >
  <head id="Head1" runat="server">
    <title>Finished Goods Physical Count Processing </title>
    
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


function fgtaglook() {
    //var loc1 = document.forms[0].ctl00_ContentPlaceHolder1_txt_tag.value;
    var NewWindow = window.open("fgtaglookup.aspx", "fgtagLookUpWindow", "width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}
function fgtaglookup(ReturnObj1) {
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
    <form id="frmList" runat="server"   defaultfocus ='txt_seqno' >   
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
          
          <TD align=left nowrap><font size=+0><b>Finished Goods Physical Count Processing &nbsp;</asp:label> </b></font></TD>
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
    <td>
    <div  id="navigation" style="width:100%">
		<ul nowrap><li class="selected" >
    <asp:LinkButton ID="lnk_listitem" runat="server" OnClick="lnk_list_click"> List Count </asp:LinkButton></li>
    <li><asp:LinkButton ID="lnk_viewitem" runat="server" OnClick="lnk_view_click"> View Count </asp:LinkButton></li></ul></div>
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
                   <b>Tag#</b><br />
                    <asp:textbox id="txt_tag" runat="server" MaxLength="20" Width="90px"></asp:textbox>                    
                     <a href="#" tabindex="1" onClick="fgtaglook(); return false"><asp:Image ID="fgtagimage" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
                    </td>  
                    <td class="shade" align="center" nowrap >
                   <b>Reciept Date</b><br />
                    <asp:textbox id="txt_date" runat="server" onfocus="javascript:preEnter( this, 'yes' );" onblur="javascript:preLeave( this, 'date', '99/99/9999' );" MaxLength="10" ToolTip="MM/DD/YYYY" Width="100px"></asp:textbox>                    
                    <a href="#" tabindex="1" onblur="setdate()" onClick="showCalendarControl(txt_date); return false"><asp:Image ID="Image1" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
                    </td>            
                      <td class="shade" align="center" nowrap >
                      <b>Po#</b> <br />
                    <asp:TextBox ID="txt_po" MaxLength="15" Width="100px" runat="server" ></asp:TextBox>
                    <a href="#" tabindex="1" onClick="customerpolook(); return false"><asp:Image ID="pofglLookup" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
                    </td>
                     
                     <td class="shade" align="center" nowrap > 
                    <b>Item No#</b> <br />
                    <asp:TextBox ID="txt_item" MaxLength="15" Width="100px" runat="server"></asp:TextBox>
                    <a href="#" tabindex="1" onClick="fglook(); return false"><asp:Image ID="FGLookup" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
                    </td>
                    <td class="shade" align="center" nowrap > 
                    <b>Job#</b> <br />
                    <asp:TextBox ID="txt_job" MaxLength="15" Width="100px" runat="server"></asp:TextBox>
                    <a href="#" tabindex="1" onClick="job1look(); return false"><asp:Image ID="Job1Lookup" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
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
                <asp:BoundField DataField="vRno" HeaderText="Seq#" HeaderStyle-Wrap="false" 
                    ItemStyle-Wrap="false" SortExpression="vRno" />
                <asp:BoundField DataField="vTag" HeaderText="Tag#" SortExpression="vTag" 
                    HeaderStyle-Wrap="false" ItemStyle-Wrap="false" />     
                <asp:BoundField DataField="vcntDate" HeaderText="Count Date" 
                    SortExpression="vcntDate" HeaderStyle-Wrap="false" ItemStyle-Wrap="false" />
                <asp:BoundField DataField="vcntTime" HeaderText="Count Time"  
                    SortExpression="vcntTime" HeaderStyle-Wrap="false" ItemStyle-Wrap="false" /> 
                <asp:BoundField DataField="vItem" HeaderText="FG Item#"  SortExpression="vItem" 
                    HeaderStyle-Wrap="false" ItemStyle-Wrap="false" />
                <asp:BoundField DataField="vItemName" HeaderText="Name" 
                    SortExpression="vItemName" HeaderStyle-Wrap="false" ItemStyle-Wrap="false" />                     
                <asp:BoundField DataField="vJobno" HeaderText="Jobno" SortExpression="vJobno" 
                    HeaderStyle-Wrap="false" ItemStyle-Wrap="false" /> 
                <asp:BoundField DataField="vJobno2" HeaderText="" 
                    SortExpression="vJobno2" HeaderStyle-Wrap="false" ItemStyle-Wrap="false" />  
                 
                <asp:BoundField DataField="vLoc" HeaderText="Warehouse" SortExpression="vLoc" 
                    HeaderStyle-Wrap="false" ItemStyle-Wrap="false" /> 
                <asp:BoundField DataField="vLocBin" HeaderText="Bin" 
                    SortExpression="vLocBin" HeaderStyle-Wrap="false" ItemStyle-Wrap="false" />                              
                <asp:BoundField DataField="vCases" HeaderText="Units" SortExpression="vCases" 
                    HeaderStyle-Wrap="false" ItemStyle-Wrap="false" /> 
                <asp:BoundField DataField="vQtyCas" HeaderText="Unit Count" 
                    SortExpression="vQtyCas" HeaderStyle-Wrap="false" ItemStyle-Wrap="false" />
                <asp:BoundField DataField="vCasUnit" HeaderText="Units/Pallet" 
                    SortExpression="vCasUnit" HeaderStyle-Wrap="false" ItemStyle-Wrap="false" /> 
                <asp:BoundField DataField="vPartial" HeaderText="Partial" 
                    SortExpression="vPartial" HeaderStyle-Wrap="false" ItemStyle-Wrap="false" ></asp:BoundField>   
                <asp:BoundField DataField="vTQty" HeaderText="Quantity" SortExpression="vTQty" 
                    HeaderStyle-Wrap="false" ItemStyle-Wrap="false" />  
                  
                                   
                <asp:BoundField DataField="vCreatedBy" HeaderText="Created by" 
                    SortExpression="vCreatedBy" />
                <asp:BoundField DataField="vCreate2" HeaderText="Last Update" 
                    SortExpression="vCreate2" />
                <asp:BoundField DataField="vRecKey" Visible="false" HeaderText="" 
                    SortExpression="vRecKey" />
                            
             
         </Columns>
     </asp:GridView>
              <asp:ObjectDataSource ID="ObjectDataSource1" runat="server" OldValuesParameterFormatString="original_{0}"
                  SelectMethod="selectcountrcpt" TypeName="itemhistory">
                  <SelectParameters>
                      <asp:Parameter Name="prmUser" Type="String" />
                      <asp:Parameter Name="prmAction" Type="String" DefaultValue="GridSelect" />
                      <asp:SessionParameter SessionField="count_recept_list_inum" Name="prmFgItem" Type="String" />
                      <asp:Parameter Name="prmName" Type="String" />
                      <asp:SessionParameter SessionField="count_recept_list_job" Name="prmJobno" Type="String" />
                      <asp:Parameter Name="prmJobno2" Type="Int32" />
                      <asp:SessionParameter SessionField="count_recept_list_po" Name="prmPono" Type="String" />
                      <asp:SessionParameter SessionField="count_recept_list_seq_no" Name="prmSeqno" Type="String" />
                      <asp:SessionParameter SessionField="count_recept_list_recdate" Name="prmRcptDate" Type="String" />
                      <asp:SessionParameter SessionField="count_recept_list_tag" Name="prmTagno" Type="String" />
                      <asp:Parameter Name="prmloc" Type="String" />
                      <asp:Parameter Name="prmlocbin" Type="String" />
                      <asp:Parameter Name="prmTqty" Type="Decimal" />
                      <asp:Parameter Name="prmCases" Type="Int32" />
                      <asp:Parameter Name="prmQty_Cas" Type="Int32" />
                      <asp:Parameter Name="prmCasUnit" Type="Int32" />
                      <asp:Parameter Name="prmPartial" Type="Int32" />
                      <asp:Parameter Name="prmRecKey" Type="String" />
                      <asp:Parameter Name="prmllSetParts" Type="String" />
                      <asp:Parameter Name="prmTransTime" Type="String" />
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

