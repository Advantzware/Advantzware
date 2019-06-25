<%@ Page Language="C#" AutoEventWireup="true" Inherits="ArInvoice" Codebehind="Arinvoice.aspx.cs" %>
<%@ Register Src="footer.ascx" TagName="Footer" TagPrefix="ft" %>
<%@ Register Src="header.ascx" TagName="Header" TagPrefix="hd" %>

<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">
<html xmlns="http://www.w3.org/1999/xhtml" >
<head id="Head1" runat="server">
    <title>Brows Invoice</title>
    <LINK href="include/style.css" type="text/css" rel="stylesheet"/>
     <link href="include/tree.css" rel="stylesheet" type="text/css" />
    <link href="include/tree.css" rel="stylesheet" type="text/css" />
    <link href="include/tree.css" rel="stylesheet" type="text/css" />
    <link href="include/tree.css" rel="stylesheet" type="text/css" />
    <link href="include/tree.css" rel="stylesheet" type="text/css" />
    <link href="include/dhtmlwindow.css" rel="stylesheet" type="text/css" />
    <link href="include/tree.css" rel="stylesheet" type="text/css" />
    <link href="include/dhtmlwindow.css" rel="stylesheet" type="text/css" />
    <link href="include/tree.css" rel="stylesheet" type="text/css" />
    <link href="include/tree.css" rel="stylesheet" type="text/css" />
    <link href="include/dhtmlwindow.css" rel="stylesheet" type="text/css" />
    <link href="include/tree.css" rel="stylesheet" type="text/css" />
    <link href="include/dhtmlwindow.css" rel="stylesheet" type="text/css" />
    <link href="include/tree.css" rel="stylesheet" type="text/css" />
    <link href="include/dhtmlwindow.css" rel="stylesheet" type="text/css" />
    <link href="include/tree.css" rel="stylesheet" type="text/css" />
    <link href="include/dhtmlwindow.css" rel="stylesheet" type="text/css" />
    <link href="include/tree.css" rel="stylesheet" type="text/css" />
    <link href="include/tree.css" rel="stylesheet" type="text/css" />
    <link href="include/dhtmlwindow.css" rel="stylesheet" type="text/css" />
    <link href="include/tree.css" rel="stylesheet" type="text/css" />
    <link href="include/dhtmlwindow.css" rel="stylesheet" type="text/css" />
    <LINK REL="stylesheet" TYPE="text/css" HREF="include/CalendarControl.css" >
    <script language = "JavaScript" src="include/CalendarControl.js"></script>
    <script language="javascript" src="include/date.js"></script>
    <script language="javascript" src="include/event.js"></script>
    <script language="javascript" src="include/insert.js"></script>
    <script type="text/javascript">

        window.onload = setfocus;
        function setfocus() {
            document.forms[0].txt_inv.focus();
        }
    
function invdateval()
{
    var duedate=document.getElementById("txt_date").value;
    
    if(duedate.length>1 && duedate.length<3 && duedate.indexOf('/')!=1)
    {
        document.getElementById("txt_date").value = duedate + "/";
    }
    if(duedate.length>4 && duedate.length<6 && duedate.indexOf('/')!=3)
    {
        document.getElementById("txt_date").value = duedate + "/";
    }
}
function InvLook(){ 
  var NewWindow = window.open("InvNum_lookup.aspx","InvNumLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}

function InvNumLookup(ReturnObj1){
    document.forms[0].txt_inv.value = ReturnObj1;
    document.forms[0].txt_inv.focus();
}
function BolLook(){ 
  var NewWindow = window.open("BolNum_lookup.aspx","BolNumLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}

function BolNumLookup(ReturnObj1){
    document.forms[0].txt_bol.value = ReturnObj1;
    document.forms[0].txt_bol.focus();
}
function customerlook(){ 
  var NewWindow = window.open("customer_lookup.aspx","CustomerLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}

function CustomerLookup(ReturnObj1){
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
function custpartlook(){ 
  var NewWindow = window.open("Invcustpart_lookup.aspx","CustPartLookupWindow","width=500,height=300,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}

function CustPartLookup(ReturnObj1){ 
  document.forms[0].txt_part.value = ReturnObj1;
  document.forms[0].txt_part.focus();
}
function customerpolook(){ 
  var NewWindow = window.open("customerpo_lookup.aspx","EstimateLookupWindow","width=500,height=300,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}
function CustomerPOLookup(ReturnObj1){
    document.forms[0].txt_po.value = ReturnObj1;
    document.forms[0].txt_po.focus();
}
function estimatelook(){ 
 var cust = document.getElementById("txt_cust").value; 
  var NewWindow = window.open("estimate_lookup.aspx?customer_val="+cust +"","EstimateLookupWindow","width=500,height=300,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}
function EstimateLookup(ReturnObj1){
    document.forms[0].txt_est.value = ReturnObj1;
    document.forms[0].txt_est.focus();
}

function orderlook(){ 
  var NewWindow = window.open("order_lookup.aspx","OrderLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}
function OrderLookup(ReturnObj1){
    document.forms[0].ddl_order.value = ReturnObj1;
    document.forms[0].ddl_order.focus();
}


function Datelook(){ 
  var NewWindow = window.open("date_lookup.aspx","DateLookupWindow","width=320,height=260,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
  }
function Datelookup(obj)
{
    document.forms[0].txt_date.value = obj;
    document.forms[0].txt_date.focus();
}


function Datelook1()
{
  document.forms[0].txt_date.value="";
  Datelook();  
}

    function orderhelp()
    {
        var NewWindow = window.open("ar_inv_help.aspx","OrderHelpWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
    }
    function printrep()
    {
        var NewWindow = window.open("topbtnorderreport.aspx","OrderReport","width=800,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");    
    }
    function printackrep()
    {
        var NewWindow = window.open("topprintorderack_report.aspx","OrderAcknowledgementReport","width=800,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");    
    }
    function ordernotes()
    {
        var NewWindow = window.open("toporder_list_notes.aspx","OrderListNotes","width=600,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");    
    }
    function printspec()
    {
        var NewWindow = window.open("cust_list_notes.aspx","OrderListNotes","width=600,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");    
    }
    function topattach()
    {
        var NewWindow = window.open("top_attach_list.aspx","Attachment","width=650,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");    
    }
    function select_col()
    {
        var NewWindow = window.open("show_avail_order_entry.aspx","SelectColumnWindow","width=300,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
    }
</script>
</head>
<body>
    <form id="form1" runat="server" defaultbutton="btnSearch">
    <hd:header id="Header1" runat="server"></hd:header>
    <table width="100%"><tr><td>
    <table align="left" border="1" width="75%">
                <tr class="topheadcolor">
                                        
                     <td nowrap width="1px";>
                        <a href="#" tabindex="1" onClick="select_col(); return false"><asp:Image ID="Image5" Width="35px" runat="server" ImageUrl="~/Images/moveCol.ico" /></a>
                    </td>                  
                   <%-- <td nowrap width="25px";>
                        <a href="#" onClick="printackrep(); return false"><asp:Image ID="Image2" Width="35px" runat="server" ImageUrl="~/Images/printAck.bmp" /></a>
                    </td>
                    <td nowrap width="25px";>
                        <a href="#" onClick="printrep(); return false"><asp:Image ID="Image1" Width="35px" runat="server" ImageUrl="~/Images/print-u.bmp" /></a>
                        </td>                    
                   
                      <td nowrap width="25px";>
                        <a href="#" onClick="topattach(); return false"><asp:Image ID="Image4" Width="35px" ToolTip="Attachment" runat="server" ImageUrl="~/Images/clip.ico" /></a>                        
                        </td>
                      <td nowrap width="25px";>
                        <a href="#" onClick="ordernotes(); return false"><asp:Image ID="img_btn_notes" Width="35px" ToolTip="Notes" runat="server" ImageUrl="~/Images/edit.ico" /></a>                        
                        </td>--%>
                         <td nowrap width="25px";>
                        <a href="#" tabindex="1" onClick="printspec(); return false"><asp:Image ID="Image6" Width="35px" runat="server" ImageUrl="~/Images/dict.ico" /></a>
                    </td>
                        <td nowrap width="25px";>
                        <a href="#" tabindex="1" onClick="orderhelp(); return false"><asp:Image ID="img_help" Width="35px" ToolTip="Help" runat="server" ImageUrl="~/Images/help.ico" /></a>
                        </td>
                      <td nowrap width="25px";>
                        <asp:ImageButton ID="img_btn_exit" runat="server" Width="35px" ImageUrl="~/Images/exit-au.bmp" ToolTip="LogOut" OnClick="img_btn_exit_click" />
                    </td>
                     <td nowrap> &nbsp;</td>
                </tr>
      </table>
     </td>
      </tr>
      <tr>
      <td>
    <div>
     <TABLE id="tblTop" cellSpacing="3" align="center" border="0" Width="100%">
        <TR>
          <TD width=30>&nbsp;</TD>
          <TD align=center nowrap><font size=+0><b>Invoice&nbsp;:<asp:Label ID="lbl_page" runat="server"></asp:Label></b></font></TD>
          <TD vAlign="middle">
            <asp:linkbutton id="LinkButton1" runat="server" OnClick="LinkButton1_Click">Back to menu</asp:linkbutton>
          </TD>
          <TD align="right"><font size=+0><b>Users&nbsp;&nbsp;</b></font></TD>
          <TD vAlign="middle" align="left">Logged as&nbsp;
            <asp:label id="lblUser" runat="server" Font-Bold="True">&nbsp;</asp:label>&nbsp;&nbsp;&nbsp;
            <asp:linkbutton id="hlnkLogOut" runat="server" OnClick="hlnkLogOut_Click">Log out</asp:linkbutton>
            
            &nbsp;&nbsp;&nbsp;<asp:hyperlink id="hlnkChangePwd" runat="server" NavigateUrl="changepwd.aspx">Change password</asp:hyperlink>
            &nbsp;&nbsp;&nbsp;
            Company:&nbsp;
            <asp:label id="lblComp" runat="server" Font-Bold="True">&nbsp;</asp:label>&nbsp;&nbsp;&nbsp;
          </TD>
          
                    
          <TD vAlign="middle" width="20">&nbsp;</TD>
          <td width=30>&nbsp;</td>
        </TR>
      </TABLE>  
  </div>
    
    <div>
    
    <table>
    <tr style="background-color:Gray;">
    <td>
    <div  id="navigation" style="width:100%">
		<ul nowrap><li class="selected">
    <asp:LinkButton ID="lnkbrowsinvoice" runat="server" > List Invoices</asp:LinkButton></li>
    <li ><asp:LinkButton ID="lnkviewinvoice" runat="server" OnClick="lnkviewinvoice_Click" >View Invoice</asp:LinkButton></li>
     <li id="liCreditStatus" runat="server" ><asp:LinkButton ID="lnkcreditstatus" runat="server" OnClick="lnkcreditstatus_Click">Credit Status </asp:LinkButton></li>
    <li id="liBol" runat="server" ><asp:LinkButton ID="lnkbol" runat="server" OnClick="lnkbol_Click">Bol</asp:LinkButton></li></ul></div>    </td>
    </tr>
    </table>
    
    <%--<a href="order_inquiry.aspx"><span style="font-size: 12pt; color: #0000ff; font-family: Times New Roman;
            text-decoration: underline">Back To Order Inquiry</span></a>&nbsp;<br />--%>
     
     

        <%--<asp:FormView ID="FormView1" runat="server" DataSourceID="ObjectDataSource1" Width="938px" Height="108px" >
            <ItemTemplate>
                ord-no:
                <asp:Label ID="ord_noLabel" runat="server" Text='<%# Bind("[ord-no]") %>'></asp:Label>
                <br />
                
                
            
            </ItemTemplate>
        </asp:FormView>--%>
       
        
       
        <table cellspacing="2" cellpadding="1" border="0" class="shade" bgcolor="gray" style="width: 80%">
          
        
        <tr>
        <td style="width: 54px; height: 21px">
                <asp:Button ID="btnSearch" runat="server" Text="Go" OnClick="btnSearch_Click" CssClass="button" Width="58px" /><br />
            <br />
            <asp:Button ID="btn_reset" runat="server" CssClass="button" Text="All" OnClick="btn_reset_Click" Width="58px"  /></td>
            <td nowrap="nowrap" style="width: 54px; height: 21px">
        
                <asp:CheckBox id="CheckBox1" runat="server" Text="Open" ></asp:CheckBox>
                <br />
         <asp:CheckBox  id="CheckBox2" runat="server" Text="Paid" ></asp:CheckBox>
            </td>
        <td nowrap >           
            Invoice#:<br>
        <asp:TextBox ID="txt_inv" runat="server" Width="70px"></asp:TextBox>
        <a href="#" tabindex="1" onClick="InvLook(); return false"><asp:Image ID="Inv" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
            <br />
                    Inv Date:<br />
                <asp:TextBox ID="txt_date" runat="server" MaxLength="10" Width="70px" onfocus="javascript:preEnter( this, 'yes' );" onblur="javascript:preLeave( this, 'date', '99/99/9999' );"></asp:TextBox>
                <a href="#" tabindex="1" onClick="showCalendarControl(txt_date); return false"><asp:Image ID="Image3" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
                
                </td>
            <td nowrap>         
                  Cust PO#:
                <br />
                <asp:TextBox ID="txt_po" runat="server" Width="70px"></asp:TextBox>
                <a href="#" tabindex="1" onClick="customerpolook(); return false"><asp:Image ID="PoLook" runat="server" ImageUrl="images/lookup_icon.gif"  /> </a>
                <br />
                Bol #:<br />
                <asp:TextBox ID="txt_bol" runat="server" Width="70px"></asp:TextBox>
                <a href="#" tabindex="1" onClick="BolLook(); return false"><asp:Image ID="Bol" runat="server" ImageUrl="images/lookup_icon.gif" /> </a></td>
                
            <%--<td nowrap>
                <br />
                
                                             
            </td>--%>
            <td nowrap>
                
                Cust Part #:<br />
                <asp:TextBox ID="txt_part" runat="server" Width="70px"></asp:TextBox>
                <a href="#" tabindex="1" onClick="custpartlook(); return false"><asp:Image ID="PartLook" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
                <br />
                FG Item #:<br /> 
                <asp:TextBox ID="txt_item" runat="server" Width="70px"></asp:TextBox>
                <a href="#" tabindex="1" onClick="fglook(); return false"><asp:Image ID="ItemLook" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
            </td>
            <%--<td nowrap>
                
                <br />
                <br />
                </td>--%>
        <td nowrap >
        Est #:<br />
                    <asp:TextBox ID="txt_est" runat="server" Width="70px"></asp:TextBox>
        <a href="#" tabindex="1" onClick="estimatelook(); return false"><asp:Image ID="EstLook" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
        <br />
            
                    
                   Order# <br>           
			<asp:TextBox ID="ddl_order" runat="server" Width="70px" ></asp:TextBox>
			<a href="#" tabindex="1" onClick="orderlook(); return false"><asp:Image ID="OrderLook" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
                    
                    </td>
            
                <%--<td valign="top" nowrap style="height: 21px; width: 22px;">
                <a href="#" onClick="fglook(); return false"></a>
                    <br />
                    </td>--%>
                <td nowrap>
                <asp:Label ID="customerlabel" runat="server" Text="Customer #:"></asp:Label><br />
                 <asp:TextBox ID="txt_cust" runat="server" Width="70px"></asp:TextBox>
                 <a href="#" tabindex="1" onClick="customerlook(); return false"><asp:Image ID="CustomerLook" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
                 <br />    
                    
        Rows/Page<br>
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
                             
                              <asp:TextBox ID="aLineLabel" runat="server" Width="70px" OnTextChanged="ddl_display_TextChanged" Text='<%# Bind("aLine") %>'></asp:TextBox>
                              <asp:CompareValidator ID="CompareValidator1" runat="server" ErrorMessage="Invalid Input" ControlToValidate="aLineLabel" Display="dynamic" SetFocusOnError="true" Operator="datatypecheck" Type="integer"></asp:CompareValidator>
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
                <%--<td valign="top" nowrap style="height: 21px; width: 21px;">
                    
                    </td> --%>
        
         
        </tr>
        </TABLE>
                
        <table>
            <tr>
                <td >
                    <%--<asp:GridView ID="GridView1" runat="server" AllowPaging="True" AllowSorting="True"
                        AutoGenerateColumns="False" BorderStyle="Dotted" CssClass="Grid" DataSourceID="ObjectDataSource1" 
                        EmptyDataText="No Record Found" Width="80%" OnSelectedIndexChanged="GridView1_SelectedIndexChanged">
                        <EmptyDataRowStyle BorderColor="#404040" BorderStyle="Dotted" BorderWidth="0px" Font-Bold="True"
                            HorizontalAlign="Center" VerticalAlign="Middle" />
                        <Columns>
                            <asp:CommandField ButtonType="Image" SelectImageUrl="~/Images/sel.gif" SelectText=""
                                ShowSelectButton="True">
                                <ItemStyle Width="10px" />
                            </asp:CommandField>
                            <asp:BoundField DataField="Arinv-no" HeaderText="Inv#" SortExpression="Arinv-no" >
                                <ItemStyle Wrap="False" Width="40px" />
                            </asp:BoundField>
                            <asp:TemplateField HeaderText="Inv Date" SortExpression="inv-date">
                                <EditItemTemplate>
                                    <asp:TextBox ID="TextBox1" runat="server" Text='<%# Bind("[Arinv-date]") %>'></asp:TextBox>
                                </EditItemTemplate>
                                <ItemStyle Wrap="False" Width="60px" />
                                <ItemTemplate>
                                    <asp:Label ID="Label1" runat="server" Text='<%# Bind("[Arinv-date]","{0:d}") %>'></asp:Label>
                                </ItemTemplate>
                            </asp:TemplateField>
                            <asp:BoundField DataField="Arord-no" HeaderText="Order#" SortExpression="Arord-no" >
                                <ItemStyle Wrap="False" Width="60px" />
                            </asp:BoundField>
                            <asp:BoundField DataField="Arbol-no" HeaderText="Bol#" SortExpression="Arbol-no" >
                                <ItemStyle Wrap="False" Width="60px" />
                            </asp:BoundField>
                            <asp:BoundField DataField="Arcust-no" HeaderText="Cust#" SortExpression="Arcust-no" >
                                <ItemStyle Wrap="False" Width="60px" />
                            </asp:BoundField>
                            <asp:BoundField DataField="Ari-no" HeaderText="FG Item#" SortExpression="Ari-no" >
                                <ItemStyle Wrap="False" Width="70px" />
                            </asp:BoundField>
                            <asp:BoundField DataField="Arpart-no" HeaderText="Cust Part#" SortExpression="Arpart-no" >
                                <ItemStyle Wrap="False" Width="80px" />
                            </asp:BoundField>
                            <asp:BoundField DataField="Arpo-no" HeaderText="Cust Po#" SortExpression="Arpo-no" >
                                <ItemStyle Wrap="False" Width="60px" />
                            </asp:BoundField>
                            <asp:BoundField DataField="Arest-no" HeaderText="Est#" SortExpression="Arest-no" >
                                <ItemStyle Wrap="False" Width="60px" />
                            </asp:BoundField>
                            <asp:BoundField DataField="Arname" HeaderText="Name" SortExpression="Arname" >
                                <ItemStyle Wrap="False" Width="60px" />
                            </asp:BoundField>
                             <asp:BoundField DataField="rec_key_cust" HeaderText="RecKey" SortExpression="rec_key_cust" >
                                <ItemStyle Wrap="False" Width="60px" />
                            </asp:BoundField>
                        <asp:TemplateField HeaderText="Price" SortExpression="price">
                                <EditItemTemplate>
                                    <asp:TextBox ID="TextBox2" runat="server" Text='<%# Bind("price") %>'></asp:TextBox>
                                </EditItemTemplate>
                                <ItemStyle Wrap="False" HorizontalAlign="Right" Width="60px" />
                               <ItemTemplate>
                                    <asp:Label ID="Label2" runat="server" Text='<%# Bind("price","{0:###,###,##0.00}") %>'></asp:Label>
                                </ItemTemplate>
                          </asp:TemplateField>
                        </Columns>
                        <RowStyle CssClass="shade" />
                        <SelectedRowStyle CssClass="GridSelected" BackColor="Yellow" />
                        <HeaderStyle BackColor="Teal" CssClass="gridrowhdr" ForeColor="White" HorizontalAlign="Center"
                            VerticalAlign="Middle" Wrap="False" />
                        <AlternatingRowStyle CssClass="GridItemOdd" />
                    </asp:GridView>
                    <asp:ObjectDataSource ID="ObjectDataSource1" runat="server" OldValuesParameterFormatString="original_{0}"
                        SelectMethod="ArInvoice" TypeName="browsinvoice">
                        <SelectParameters>
                            <asp:SessionParameter DefaultValue="select" Name="prmAction" SessionField="prmAction2" Type="String" />
                            <asp:SessionParameter Name="prmUser" SessionField="prmUser" Type="String" />
                            <asp:Parameter Name="prmOrderNum" Type="String" />
                            <asp:SessionParameter Name="prmInvoice" SessionField="prmInvoice" Type="String" />
                            <asp:SessionParameter Name="prmCustomer" SessionField="prmCustomer" Type="String" />
                            <asp:SessionParameter Name="prmItem" SessionField="prmItem" Type="String" />

                            <asp:SessionParameter Name="prmPart" SessionField="prmPart" Type="String" />
                            <asp:SessionParameter Name="prmCustPo" SessionField="prmCustPo" Type="String" />
                            <asp:SessionParameter Name="prmBOL" SessionField="prmBOL" Type="String" />
                            <asp:SessionParameter Name="prmEstimate" SessionField="prmEstimate" Type="String" />
                            <asp:SessionParameter Name="prmDate" SessionField="prmDate" Type="String" />
                            <asp:SessionParameter Name="prmOpen" SessionField="prmOpen" Type="String" />
                            <asp:SessionParameter Name="prmPaid" SessionField="prmPaid" Type="String" />
                                           
                        </SelectParameters>
                    </asp:ObjectDataSource>--%>
                        <asp:HiddenField ID="openinvoice" runat="server" />        
                          <asp:HiddenField ID="paidinvoice" runat="server"/>
                </td>
            </tr>
            <tr>
                <td>
                    <asp:GridView ID="GridView2" runat="server" OnRowCreated="GridView2_RowCreated" AllowPaging="true" OnPageIndexChanging="GridView2_PageIndexChanging"  
                       AllowSorting="True" OnSorting="GridView2_Sorting" OnSelectedIndexChanged="GridView2_SelectedIndexChanged" OnRowDataBound="GridView2_RowDataBound"
                        EmptyDataText="No Records Found" Width="100%" BorderStyle="Dotted" CssClass="Grid" >
                        <SelectedRowStyle CssClass="GridSelected" BackColor="Yellow" Wrap="false" />
                        <AlternatingRowStyle CssClass="GridItemOdd" Wrap="False" />            
                        <EmptyDataRowStyle BorderStyle="Dotted" BorderColor="Gray" BorderWidth="0px" Font-Bold="True" HorizontalAlign="Center" VerticalAlign="Middle" />
                        <HeaderStyle  ForeColor="White" CssClass="headcolor" Height="40px"  VerticalAlign="Middle"  HorizontalAlign="Center" Wrap="False"></HeaderStyle>
                        <RowStyle CssClass="shade" Wrap="False"  />
                            <Columns>
                                <asp:CommandField ShowSelectButton="True" ButtonType="Image" SelectImageUrl="~/Images/sel.gif" SelectText="" > 
                                    <ItemStyle Width="10px" />
                                </asp:CommandField>
                                
                                <asp:TemplateField HeaderText="Bolno" Visible="false"  >                                
                                <ItemStyle Wrap="False"  />
                                <ItemTemplate>
                                    <asp:Label ID="bol_Label" runat="server" Text='<%# Bind("[Bol#]") %>'></asp:Label>
                                </ItemTemplate>
                                </asp:TemplateField> 
                                <asp:TemplateField HeaderText="invno" Visible="false" >                                
                                <ItemStyle Wrap="False" Width="60px" />
                                <ItemTemplate>
                                    <asp:Label ID="inv_Label" runat="server" Text='<%# Bind("[Inv#]") %>'></asp:Label>
                                </ItemTemplate>
                                </asp:TemplateField> 
                                 <asp:TemplateField HeaderText="Orderno" Visible="false" >                                
                                <ItemStyle Wrap="False" Width="60px" />
                                <ItemTemplate>
                                    <asp:Label ID="Order_Label" runat="server" Text='<%# Bind("[Order#]") %>'></asp:Label>
                                </ItemTemplate>
                                </asp:TemplateField>
                                <asp:TemplateField HeaderText="Custno"  Visible="false" >                                
                                <ItemStyle Wrap="False" Width="60px" />
                                <ItemTemplate>
                                    <asp:Label ID="cust_Label" runat="server" Text='<%# Bind("[Cust#]") %>'></asp:Label>
                                </ItemTemplate>
                                </asp:TemplateField> 
                                <asp:TemplateField HeaderText="Reckey" Visible="false" >                                
                                <ItemStyle Wrap="False" Width="60px" />
                                <ItemTemplate>
                                    <asp:Label ID="reckey_Label" runat="server" Text='<%# Bind("[rec_key_cust]") %>'></asp:Label>
                                </ItemTemplate>
                                </asp:TemplateField>
                                           
                            </Columns>
                     </asp:GridView> 
                </td>
            </tr>
        </table>
        
       
            
    </div>
    </td></tr></table>
    
    <ft:footer id="Footer1" runat="server"></ft:footer>
        &nbsp; &nbsp;
    </form>
</body>
</html>
