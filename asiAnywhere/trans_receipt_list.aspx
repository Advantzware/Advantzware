<%@ Page Language="c#" AutoEventWireup="True" Debug="true" Inherits="trans_receipt_list" Codebehind="trans_receipt_list.aspx.cs" %>
<%@ Register Src="footer.ascx" TagName="Footer" TagPrefix="ft" %>
<%@ Register Src="header.ascx" TagName="Header" TagPrefix="hd" %>
<html xmlns="http://www.w3.org/1999/xhtml" >
  <head id="Head1" runat="server">
    <title>Finished Goods Warehouse Transaction Transfer </title>
    
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
function trnsfgbinlook() {
    var NewWindow = window.open("trns_fgbin_lookup.aspx", "DateLookupWindow", "width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}
function trnsfgbinlookup(obj1, obj2, obj3, obj4, obj5, obj6, obj7) {
    document.forms[0].FormView1_vItemTextBox.value = obj1;
    document.forms[0].FormView1_vTagTextBox.value = obj2;
    document.forms[0].FormView1_vJobnoTextBox.value = obj3;
    document.forms[0].FormView1_vJobno2TextBox.value = obj4;
    document.forms[0].FormView1_vLocTextBox.value = obj5;
    document.forms[0].FormView1_vLocBinTextBox.value = obj6;
    document.forms[0].FormView1_vcustTextBox.value = obj7;

    document.forms[0].FormView1_vTagTextBox.focus();

}

function trnsfgbintglook() {
    var locl2 = document.forms[0].FormView1_vItemTextBox.value;
    var NewWindow = window.open("trns_fgbintg_lookup2.aspx?fgbin=" + locl2 + "", "DateLookupWindow", "width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}
function trnsfgbintglookup(obj1, obj2, obj3) {
    document.forms[0].FormView1_vLoc2TextBox.value = obj1;
    document.forms[0].FormView1_vLocBin2TextBox.value = obj2;
    document.forms[0].FormView1_vTag2TextBox.value = obj3;

}

function trnstaglook() {
    var NewWindow = window.open("trnstaglook.aspx", "DateLookupWindow", "width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}
function trnstaglookup(obj1, obj2, obj3, obj4, obj5, obj6, obj7, obj8, obj9, obj10, obj11) {
    document.forms[0].FormView1_vTagTextBox.value = obj1;
    document.forms[0].FormView1_vItemNameTextBox.value = obj2;
    document.forms[0].FormView1_vJobnoTextBox.value = obj3;
    document.forms[0].FormView1_vJobno2TextBox.value = obj4;
    document.forms[0].FormView1_vLocTextBox.value = obj5;
    document.forms[0].FormView1_vLocBinTextBox.value = obj6;
    //document.forms[0].FormView1_vcustTextBox.value = obj7;
    document.forms[0].FormView1_vCasesTextBox.value = obj7;
    document.forms[0].FormView1_vQtyCasTextBox.value = obj8;
    document.forms[0].FormView1_vTag2TextBox.value = obj9;
    document.forms[0].FormView1_vItemTextBox.value = obj10;

    document.forms[0].FormView1_vTagTextBox.onchange();


}

function locationlook() {
    var NewWindow = window.open("location_lookup.aspx", "LocationLookUpWindow", "width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}
function LocationLookUp(ReturnObj1, ReturnObj2) {
    document.forms[0].FormView1_vLoc2TextBox.value = ReturnObj1;
    document.forms[0].FormView1_vLoc2TextBox.focus();
}

function unit() {
    var unt = document.getElementById("FormView1_vCasesTextBox");
    unt.focus();
}

function binlook() {
    var loc1 = document.forms[0].FormView1_vLoc2TextBox.value;
    var NewWindow = window.open("custbin_lookup.aspx?binloc=" + loc1 + "", "BinLookUpWindow", "width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}
function CustBinLookup(ReturnObj1, ReturnObj2) {
    document.forms[0].FormView1_vLocBin2TextBox.value = ReturnObj1;
    document.forms[0].FormView1_vLocBin2TextBox.focus();
}
function ordernotes() {
    var NewWindow = window.open("toporder_list_notes.aspx", "OrderListNotes", "width=600,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}
    </script> 
  </head>    
   <body>
    <form id="frmList" runat="server"   defaultfocus ='txt_seqno' >   
        <hd:header id="Header1" runat="server"></hd:header>
      <div>
      <table width="100%"><tr><td><div>
    <table align="left" border="1" width="75%">
                <tr align="left" class="topheadcolor">
                   
                    
                    <td nowrap align="left" width="25px";>
                        <asp:ImageButton ID="img_btn_add" runat="server" Width="35px" ImageUrl="~/Images/add.bmp" OnClick="img_btn_add_click" ToolTip="Add"  />
                      </td>
                     <td nowrap width="25px";>
                        <a href="#" onClick="ordernotes(); return false"><asp:Image ID="img_btn_notes" Width="35px" ToolTip="Notes" runat="server" ImageUrl="~/Images/edit.ico" /></a>                        
                        </td>
                        <%--<td nowrap width="25px";>
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
          
          <TD align=left nowrap><font size=+0><b>Finished Goods Warehouse Transaction Transfer &nbsp;</asp:label> </b></font></TD>
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
		<ul nowrap><li class="selected" >
    <asp:LinkButton ID="lnk_listitem" runat="server" OnClick="lnk_list_click">List Transfer</asp:LinkButton> </li></ul></div>
    <%--<asp:LinkButton ID="lnk_viewitem" runat="server" OnClick="lnk_view_click"><img src="Images/view transfer1.jpg" border="0" alt="View Transfer" /></asp:LinkButton>--%>
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
     <fieldset style="width:100%;">
     <asp:Panel ID="Grid_panel" runat="server" Width="100%"  ScrollBars="Vertical" Height="180px">
     <asp:GridView ID="GridView1" AutoGenerateColumns="False" runat="server" DataSourceID="ObjectDataSource3"
     AllowPaging="True" AllowSorting="True" 
            EmptyDataText="No Records Found" BorderStyle="Dotted" CssClass="Grid" OnSelectedIndexChanged="GridView1_SelectedIndexChanged">
            <SelectedRowStyle CssClass="GridSelected" BackColor="Yellow" />
            <AlternatingRowStyle CssClass="GridItemOdd" />
            <EmptyDataRowStyle BorderStyle="Dotted" BorderColor="Gray" BorderWidth="0px" Font-Bold="True" HorizontalAlign="Center" VerticalAlign="Middle" />
            <HeaderStyle  ForeColor="White" CssClass="headcolor" Height="40px"  VerticalAlign="Middle"  HorizontalAlign="Center" ></HeaderStyle>
            <RowStyle CssClass="shade"  />
            <Columns>
                <asp:CommandField ShowSelectButton="True" ButtonType="Image" SelectImageUrl="~/Images/sel.gif" SelectText="" > 
                    <ItemStyle Width="10px" />
                </asp:CommandField>
                <asp:BoundField DataField="vTag" HeaderText="From Tag" HeaderStyle-Wrap="false" ItemStyle-Wrap="false" SortExpression="vTag" />
                <asp:BoundField DataField="vLoc" HeaderText="From Whse" SortExpression="vLoc" HeaderStyle-Wrap="false" ItemStyle-Wrap="false" />
                <asp:BoundField DataField="vLocBin" HeaderText="From Bin" SortExpression="vLocBin" HeaderStyle-Wrap="false" ItemStyle-Wrap="false" /> 
                <asp:BoundField DataField="vCases" HeaderText="Units"  SortExpression="vCases" HeaderStyle-Wrap="false" ItemStyle-Wrap="false" />
                <asp:BoundField DataField="vQtyCas" HeaderText="Unit Count" SortExpression="vQtyCas" HeaderStyle-Wrap="false" ItemStyle-Wrap="false" />  
                <asp:BoundField DataField="vPartial" HeaderText="Partial" SortExpression="vPartial" HeaderStyle-Wrap="false" ItemStyle-Wrap="false" />
                <asp:BoundField DataField="vTag2" HeaderText="To Tag" SortExpression="vTag2" HeaderStyle-Wrap="false" ItemStyle-Wrap="false" /> 
                <asp:BoundField DataField="vLoc2" HeaderText="To Whse" SortExpression="vLoc2" HeaderStyle-Wrap="false" ItemStyle-Wrap="false" />                              
                <asp:BoundField DataField="vLocBin2" HeaderText="To Bin" SortExpression="vLocBin2" HeaderStyle-Wrap="false" ItemStyle-Wrap="false" /> 
                <asp:BoundField DataField="vcust" HeaderText="Customer" SortExpression="vcust" HeaderStyle-Wrap="false" ItemStyle-Wrap="false" />  
                <asp:BoundField DataField="vJobno" HeaderText="Job#"  SortExpression="vJobno" HeaderStyle-Wrap="false" ItemStyle-Wrap="false" />
                <asp:BoundField DataField="vJobno2" HeaderText="" SortExpression="vJobno2" HeaderStyle-Wrap="false" ItemStyle-Wrap="false" /> 
                <asp:BoundField DataField="vItem" HeaderText="Item No" SortExpression="vItem" HeaderStyle-Wrap="false" ItemStyle-Wrap="false" /> 
                <asp:BoundField DataField="vItemName" HeaderText="Item Name" SortExpression="vItemName" HeaderStyle-Wrap="false" ItemStyle-Wrap="false" />                
                <asp:BoundField DataField="blan" HeaderText="Blank#" SortExpression="blan" HeaderStyle-Wrap="false" ItemStyle-Wrap="false" /> 
                <asp:BoundField DataField="usrcrt" HeaderText="User Created" SortExpression="usrcrt" HeaderStyle-Wrap="false" ItemStyle-Wrap="false" /> 
                <asp:BoundField DataField="usrupdt" HeaderText="User Updated" SortExpression="usrupdt" HeaderStyle-Wrap="false" ItemStyle-Wrap="false" />                
                <asp:BoundField DataField="vDate" HeaderText="Transfer Date" SortExpression="vDate" HeaderStyle-Wrap="false" ItemStyle-Wrap="false" ></asp:BoundField>   
                <asp:BoundField DataField="vTransTime" HeaderText="Transfer Time" SortExpression="vTransTime" HeaderStyle-Wrap="false" ItemStyle-Wrap="false" />  
                  
                                   
                <asp:TemplateField HeaderText="rec_key" Visible="false">
                <ItemTemplate>
                <asp:Label ID="reckeylabel" runat="server"  Text='<%# Bind("vRecKey") %>' ></asp:Label>
                </ItemTemplate>
                </asp:TemplateField>
                            
             
         </Columns>
     </asp:GridView>
     </asp:Panel>
     </fieldset>
              <asp:ObjectDataSource ID="ObjectDataSource3" runat="server" OldValuesParameterFormatString="original_{0}"
                  SelectMethod="SelecttrnsRcpt" TypeName="itemhistory">
                  <SelectParameters>
                      <asp:Parameter Name="prmUser" Type="String" />
                      <asp:Parameter Name="prmAction" Type="String" DefaultValue="Select" />
                      <asp:SessionParameter SessionField="trans_recept_list_inum" Name="prmFgItem" Type="String" />
                      <asp:SessionParameter SessionField="trans_recept_list_job" Name="prmJobno" Type="String" />
                      <asp:SessionParameter SessionField="trans_recept_list_po" Name="prmPono" Type="String" />
                      <asp:SessionParameter SessionField="trans_recept_list_seq_no" Name="prmSeqno" Type="String" />
                      <asp:SessionParameter SessionField="trans_recept_list_recdate" Name="prmRcptDate" Type="String" />
                      <asp:SessionParameter SessionField="trans_recept_list_tag" Name="prmTagno" Type="String" />
                      <asp:Parameter Name="prmRecKey" Type="String" />
                      <asp:Parameter Name="prmOut" Type="String" />
                      <asp:Parameter Name="prmllSetParts" Type="String" />
                  </SelectParameters>
              </asp:ObjectDataSource>           
           
          </TD>
        </TR>
      </TABLE>         
        
      <asp:FormView ID="FormView1" runat="server" DataSourceID="ObjectDataSource1" OnDataBound="FormView1_DataBound" OnPreRender="FormView1_PreRender">
            <EditItemTemplate>
                <asp:Panel ID="Edit_panel" runat="server" DefaultButton="UpdateButton">
                <fieldset style="background-color:#EFF3FB">
                <table class="shade">
                <tr>
                <td nowrap align="left" style="padding-right:5px;"><b>From Tag:</b> <br />
                <asp:TextBox ID="vTagTextBox" runat="server" MaxLength="20" ReadOnly="true" ForeColor="#848284" Text='<%# Bind("vTag") %>' />                
                </td>
                <td nowrap align="left" style="padding-right:5px;"><b>From Whse:</b> <br />
                <asp:TextBox ID="vLocTextBox" runat="server" MaxLength="5" ReadOnly="true" ForeColor="#848284" Width="70px" Text='<%# Bind("vLoc") %>' />                
                </td>
                <td nowrap align="left" style="padding-right:5px;"><b>From Bin:</b> <br />
                <asp:TextBox ID="vLocBinTextBox" runat="server" ReadOnly="true" MaxLength="8" ForeColor="#848284" Width="70px" Text='<%# Bind("vLocBin") %>' />                
                </td>
                <td nowrap align="left" style="padding-right:5px;"><b>Units:</b> <br />
                <asp:TextBox ID="vCasesTextBox" runat="server" Width="45px" MaxLength="6" Text='<%# Bind("vCases") %>' />
                <asp:CompareValidator ID="CompareValidator1" runat="server" ControlToValidate="vCasesTextBox" SetFocusOnError="true" Display="dynamic" Operator="dataTypeCheck" Type="Integer" ErrorMessage="Number Only"></asp:CompareValidator>    
                </td>
                <td nowrap align="left" style="padding-right:5px;"><b>Unit Count:</b> <br />
                <asp:TextBox ID="vQtyCasTextBox" runat="server" Width="45px" TabIndex="1" ReadOnly="true" ForeColor="#848284" Text='<%# Bind("vQtyCas") %>' />                
                </td>
                <td nowrap align="left" style="padding-right:5px;"><b>Partial:</b> <br />
                <asp:TextBox ID="vPartialTextBox" runat="server" Width="45px" MaxLength="6" Text='<%# Bind("vPartial") %>' />
                <asp:CompareValidator ID="CompareValidator3" runat="server" ControlToValidate="vPartialTextBox" SetFocusOnError="true" Display="dynamic" Operator="dataTypeCheck" Type="Integer" ErrorMessage="Number Only"></asp:CompareValidator>    
                </td>
                <td nowrap align="left" style="padding-right:5px;"><b>To Tag:</b> <br />
                <asp:TextBox ID="vTag2TextBox" runat="server" TabIndex="1" ReadOnly="true" ForeColor="#848284" Text='<%# Bind("vTag2") %>' />                
                </td>
                <td nowrap align="left" style="padding-right:5px;"><b>To Whse:</b> <br />
                <asp:TextBox ID="vLoc2TextBox" runat="server" MaxLength="5" Width="50px" Text='<%# Bind("vLoc2") %>' />
                <a href="#" tabindex="1" tabindex="1" onClick="locationlook(); return false"><asp:Image ID="Image4" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
                </td>
                <td nowrap align="left" style="padding-right:5px;"><b>To Bin:</b> <br />
                <asp:TextBox ID="vLocBin2TextBox" runat="server" MaxLength="8" Width="70px" onblur="focuset()" Text='<%# Bind("vLocBin2") %>' />
                <a href="#" tabindex="1" tabindex="1" onClick="binlook(); return false"><asp:Image ID="Image5" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
                </td>
                <td nowrap align="left" style="padding-right:5px;"><b>Customer:</b> <br />
                <asp:TextBox ID="vcustTextBox" runat="server" Width="100px" onfocus="unit()" ReadOnly="true" ForeColor="#848284" Text='<%# Bind("vcust") %>' />                
                </td>
                <td nowrap align="left" style="padding-right:5px;"><b>Job#:</b> <br />
                <asp:TextBox ID="vJobnoTextBox" Width="70px" ReadOnly="true" onfocus="unit()" runat="server" ForeColor="#848284" Text='<%# Bind("vJobno") %>' />
                </td>
                <td nowrap align="left" style="padding-right:5px;"><b></b> <br />
                <asp:TextBox ID="vJobno2TextBox" runat="server" Width="20px" onfocus="unit()" ReadOnly="true" ForeColor="#848284" Text='<%# Bind("vJobno2") %>' />                
                </td>
                <td nowrap align="left" style="padding-right:5px;"><b>Item No:</b> <br />
                <asp:TextBox ID="vItemTextBox" runat="server" Width="100px" onfocus="unit()" ForeColor="#848284" Text='<%# Bind("vItem") %>' />
                </td>
                <td nowrap align="left" style="padding-right:5px;"><b>Item Name:</b> <br />
                <asp:TextBox ID="vItemNameTextBox" ReadOnly="true" onfocus="unit()" runat="server" ForeColor="#848284" Text='<%# Bind("vItemName") %>' />
                </td>
                <td nowrap align="left" style="padding-right:5px;"><b>Blank #:</b> <br />
                <asp:TextBox ID="vblankTextBox" ReadOnly="true" onfocus="unit()" Width="50px" ForeColor="#848284" runat="server" Text='<%# Bind("blan") %>' />
                </td> 
                <td nowrap align="left" style="padding-right:5px;"><b>User Created:</b> <br />
                <asp:Label ID="vusrcrtTextBox" onkeyup="tagblank()" Width="80px" runat="server" Text='<%# Bind("usrcrt") %>' />
                </td> 
                <td nowrap align="left" style="padding-right:5px;"><b>User Updated:</b> <br />
                <asp:Label ID="vusrupdtTextBox" onkeyup="tagblank()" Width="80px" runat="server" Text='<%# Bind("usrupdt") %>' />
                </td> 
                <td nowrap align="left" style="padding-right:5px;"><b>Transfer Date:</b> <br />
                <asp:Label ID="vDateTextBox" Width="80px" runat="server" Text='<%# Bind("vDate") %>' />
                </td>
                <td nowrap align="left" style="padding-right:5px;"><b>Transfer Time:</b> <br />
                <asp:label ID="vTransTimeTextBox" Width="80px" runat="server" Text='<%# Bind("vTransTime") %>' />
                </td>  
                <td nowrap align="left" style="padding-right:5px;"> <br />
                <asp:Label ID="vRecKeyLabel" Width="100px" Visible="false" runat="server" Text='<%# Bind("vRecKey") %>' />
                </td>  </tr>
                
                <tr><td colspan="4"><br />
                <asp:Button ID="UpdateButton" runat="server" CausesValidation="True" CssClass="button" OnClick="Update_Button_Click" Text="Save" />
                &nbsp;<asp:Button ID="UpdateCancelButton" runat="server" CssClass="button"
                    CausesValidation="False" CommandName="Cancel" Text="Cancel" />
                </td></tr>
                
                </table> </fieldset>             
                </asp:Panel>             
            </EditItemTemplate>
            
            <InsertItemTemplate>
                                
                <asp:Panel ID="Insert_panel" runat="server" DefaultButton="InsertButton">
                <fieldset style="background-color:#EFF3FB"><table class="shade">
                <tr>
                <td nowrap align="left" style="padding-right:5px;"><b>From Tag:</b> <br />
                <asp:TextBox ID="vTagTextBox" runat="server" MaxLength="20" OnTextChanged="TagTextBox_Change" AutoPostBack="true" Text='<%# Bind("vTag") %>' />
                <a href="#" tabindex="1" onClick="trnstaglook(); return false"><asp:Image ID="Image10" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
                </td>
                <td nowrap align="left" style="padding-right:5px;"><b>From Whse:</b> <br />
                <asp:TextBox ID="vLocTextBox" runat="server" MaxLength="5" onfocus="unit()" ForeColor="#848284" Width="70px" Text='<%# Bind("vLoc") %>' />                
                </td>
                <td nowrap align="left" style="padding-right:5px;"><b>From Bin:</b> <br />
                <asp:TextBox ID="vLocBinTextBox" runat="server" onfocus="unit()" ForeColor="#848284" Width="70px" Text='<%# Bind("vLocBin") %>' />                
                </td>
                <td nowrap align="left" style="padding-right:5px;"><b>Units:</b> <br />
                <asp:TextBox ID="vCasesTextBox" runat="server" Width="45px" MaxLength="6" Text='<%# Bind("vCases") %>' />
                <asp:CompareValidator ID="CompareValidator1" runat="server" ControlToValidate="vCasesTextBox" SetFocusOnError="true" Display="dynamic" Operator="dataTypeCheck" Type="Integer" ErrorMessage="Number Only"></asp:CompareValidator>    
                </td>
                <td nowrap align="left" style="padding-right:5px;"><b>Unit Count:</b> <br />
                <asp:TextBox ID="vQtyCasTextBox" runat="server" Width="45px" TabIndex="1" ReadOnly="true" ForeColor="#848284" Text='<%# Bind("vQtyCas") %>' />                
                </td>
                <td nowrap align="left" style="padding-right:5px;"><b>Partial:</b> <br />
                <asp:TextBox ID="vPartialTextBox" runat="server" Width="45px" MaxLength="6" Text='<%# Bind("vPartial") %>' />
                <asp:CompareValidator ID="CompareValidator3" runat="server" ControlToValidate="vPartialTextBox" SetFocusOnError="true" Display="dynamic" Operator="dataTypeCheck" Type="Integer" ErrorMessage="Number Only"></asp:CompareValidator>    
                </td>
                <td nowrap align="left" style="padding-right:5px;"><b>To Tag:</b> <br />
                <asp:TextBox ID="vTag2TextBox" runat="server" TabIndex="1" ReadOnly="true" ForeColor="#848284" Text='<%# Bind("vTag2") %>' />                
                </td>
                <td nowrap align="left" style="padding-right:5px;"><b>To Whse:</b> <br />
                <asp:TextBox ID="vLoc2TextBox" runat="server" MaxLength="5" Width="50px" Text='<%# Bind("vLoc2") %>' />
                <a href="#" tabindex="1" tabindex="1" onClick="locationlook(); return false"><asp:Image ID="Image4" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
                </td>
                <td nowrap align="left" style="padding-right:5px;"><b>To Bin:</b> <br />
                <asp:TextBox ID="vLocBin2TextBox" runat="server" MaxLength="8" Width="70px" onblur="focuset()" Text='<%# Bind("vLocBin2") %>' />
                <a href="#" tabindex="1" tabindex="1" onClick="binlook(); return false"><asp:Image ID="Image5" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
                </td>
                <td nowrap align="left" style="padding-right:5px;"><b>Customer:</b> <br />
                <asp:TextBox ID="vcustTextBox" runat="server" Width="100px" onfocus="unit()" ReadOnly="true" ForeColor="#848284" Text='<%# Bind("vcust") %>' />                
                </td>
                <td nowrap align="left" style="padding-right:5px;"><b>Job#:</b> <br />
                <asp:TextBox ID="vJobnoTextBox" Width="70px" ReadOnly="true" onfocus="unit()" runat="server" ForeColor="#848284" Text='<%# Bind("vJobno") %>' />
                </td>
                <td nowrap align="left" style="padding-right:5px;"><b></b> <br />
                <asp:TextBox ID="vJobno2TextBox" runat="server" Width="20px" onfocus="unit()" ReadOnly="true" ForeColor="#848284" Text='<%# Bind("vJobno2") %>' />                
                </td>
                <td nowrap align="left" style="padding-right:5px;"><b>Item No:</b> <br />
                <asp:TextBox ID="vItemTextBox" runat="server" Width="100px" onfocus="unit()" ForeColor="#848284" Text='<%# Bind("vItem") %>' />
                </td>
                <td nowrap align="left" style="padding-right:5px;"><b>Item Name:</b> <br />
                <asp:TextBox ID="vItemNameTextBox" ReadOnly="true" onfocus="unit()" runat="server" ForeColor="#848284" Text='<%# Bind("vItemName") %>' />
                </td>
                <td nowrap align="left" style="padding-right:5px;"><b>Blank #:</b> <br />
                <asp:TextBox ID="vblankTextBox" ReadOnly="true" onfocus="unit()" Width="50px" ForeColor="#848284" runat="server" Text='<%# Bind("blan") %>' />
                </td> 
                <td nowrap align="left" style="padding-right:5px;"><b>User Created:</b> <br />
                <asp:Label ID="vusrcrtTextBox" onkeyup="tagblank()" Width="80px" runat="server" Text='<%# Bind("usrcrt") %>' />
                </td> 
                <td nowrap align="left" style="padding-right:5px;"><b>User Updated:</b> <br />
                <asp:Label ID="vusrupdtTextBox" onkeyup="tagblank()" Width="80px" runat="server" Text='<%# Bind("usrupdt") %>' />
                </td> 
                <td nowrap align="left" style="padding-right:5px;"><b>Transfer Date:</b> <br />
                <asp:Label ID="vDateTextBox" Width="80px" runat="server" Text='<%# Bind("vDate") %>' />
                </td>
                <td nowrap align="left" style="padding-right:5px;"><b>Transfer Time:</b> <br />
                <asp:label ID="vTransTimeTextBox" Width="80px" runat="server" Text='<%# Bind("vTransTime") %>' />
                </td> 
                <td nowrap align="left" style="padding-right:5px;"> <br />
                <asp:Label ID="vRecKeyLabel" runat="server" Visible="false" Text='<%# Bind("vRecKey") %>' />
                </td>  </tr>
                <tr><td colspan="4"><br />
                <asp:Button ID="InsertButton" runat="server" CausesValidation="True" CssClass="button"
                     OnClick="Insert_Button_Click" Text="Save" />
                 &nbsp;<asp:Button ID="InsertCancelButton" runat="server" CssClass="button"
                     CausesValidation="False" CommandName="Cancel" Text="Cancel" />
                </td></tr>
                </table></fieldset></asp:Panel>
                 
            </InsertItemTemplate>
            
            <ItemTemplate>                
                <asp:Label ID="vRecLabel" Visible="false" runat="server" Text='<%# Bind("vRecKey") %>' BackColor="PaleTurquoise" BorderColor="White" BorderStyle="Solid" BorderWidth="1px" Width="100px" />
                
                <tr><td colspan="4">
                <asp:Button ID="addButton" runat="server" CommandName="new"  CssClass="button" Text="Add"></asp:Button>
                <asp:Button ID="UpdatButton"  runat="server" CommandName="Edit" CssClass="buttonM"  Text="Update" />
                <asp:Button ID="deleteButton" runat="server" CssClass="button" CausesValidation="False" Text="Delete" OnClick="DeleteButton_Click"  OnClientClick="return confirm('Are you sure you want to delete this record')"></asp:Button>
                 
                </ItemTemplate>
        </asp:FormView>
        <asp:ObjectDataSource ID="ObjectDataSource1" runat="server" OldValuesParameterFormatString="original_{0}"
            SelectMethod="trnsViewRece" TypeName="itemhistory">
            <SelectParameters>
                <asp:Parameter Name="prmUser" Type="String" />
                <asp:Parameter Name="prmAction" Type="String" DefaultValue="Select" />
                <asp:Parameter Name="prmFgItem" Type="String" />
                <asp:Parameter Name="prmJobno" Type="String" />
                <asp:Parameter Name="prmRcptDate" Type="String" />
                <asp:Parameter Name="prmTagno" Type="String" />
                <asp:Parameter Name="prmTransTime" Type="String" />
                <asp:Parameter Name="prmJob_no2" Type="String" />
                <asp:Parameter Name="prmName" Type="String" />
                <asp:Parameter Name="prmLoc" Type="String" />
                <asp:Parameter Name="prmLocBin" Type="String" />
                <asp:Parameter Name="prmCases" Type="String" />
                <asp:Parameter Name="prmQty_Cas" Type="String" />
                <asp:Parameter Name="prmCasUnit" Type="String" />
                <asp:Parameter Name="prmPartial" Type="String" />
                <asp:Parameter Name="prmLoc2" Type="String" />
                <asp:Parameter Name="prmLocBin2" Type="String" />
                <asp:Parameter Name="prmTagno2" Type="String" />
                <asp:SessionParameter Name="prmRecKey" SessionField="trans_recept_list_seq" 
                    Type="String" />
                <asp:Parameter Name="prmcontrans" Type="String" />
                <asp:Parameter Name="prmSeq" Type="String" />
                <asp:Parameter Name="prmOut" Type="String" />
            </SelectParameters>
            
        </asp:ObjectDataSource>
      
    </div>
    <ft:footer id="Footer1" runat="server"></ft:footer>
    </form>
  </body>
</html>

