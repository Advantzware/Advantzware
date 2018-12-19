<%@ Page Language="C#" AutoEventWireup="true" Inherits="browsinvoiceaspx" Codebehind="browsinvoice.aspx.cs" %>
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
</head>
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

</script>
<body>
    <form id="form1" runat="server">
    <hd:header id="Header1" runat="server"></hd:header>
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
    <td><div  id="navigation" style="width:100%">
		<ul nowrap><li class="selected">
    <asp:LinkButton ID="lnkbrowsinvoice" runat="server" >Brws Invoice<%--<img src="img/browsinvoice1.jpg" border="0" alt="Brws Invoice" />--%></asp:LinkButton></li>
    <li><asp:LinkButton ID="lnkviewinvoice" runat="server" OnClick="lnkviewinvoice_Click" >View Invoice<%--<img src="img/viewinvoice0.jpg" border="0" alt="View Invoice" />--%></asp:LinkButton></li>
    <li> <asp:LinkButton ID="lnkcreditstatus" runat="server" OnClick="lnkcreditstatus_Click">Credit Status<%--<img src="img/creditstatus0.jpg" border="0" alt="Credit Status" />--%></asp:LinkButton></li>
    <li><asp:LinkButton ID="lnkbol" runat="server" OnClick="lnkbol_Click">Bol<%--<img src="img/bol0.jpg" border="0" alt="Bol" />--%></asp:LinkButton> </li></ul></div>   </td>
    </tr>
    </table>
    
    <asp:LinkButton OnClick="LinkButton_Click" ID="LinkButton2" runat="server">List Order</asp:LinkButton>
    <asp:LinkButton OnClick="LinkButton2_Click" ID="LinkButton3" runat="server">List Order</asp:LinkButton>
    <asp:LinkButton OnClick="LinkButton3_Click" ID="LinkButton4" runat="server">List Order</asp:LinkButton>
    <br />
        &nbsp;<br />
     
     <script type="text/javascript">

function customerlook(){ 
  var NewWindow = window.open("Invcustomer_lookup.aspx","CustomerLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}

function CustomerLookup(ReturnObj1){
    document.forms[0].txt_cust.value = ReturnObj1;
    document.forms[0].txt_cust.focus();
}
function fglook(){ 
  var NewWindow = window.open("Invfgitem_lookup.aspx","FGLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}

function FGLookup(ReturnObj1){
    document.forms[0].txt_item.value = ReturnObj1;
    document.forms[0].txt_item.focus();
}
function custpartlook(){ 
  var NewWindow = window.open("Invcustpart_lookup.aspx","CustPartLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}

function CustPartLookup(ReturnObj1){
    document.forms[0].txt_part.value = ReturnObj1;
    document.forms[0].txt_part.focus();
  
}
function customerpolook(){ 
  var NewWindow = window.open("Invcustpo_lookup.aspx","EstimateLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}
function CustomerPOLookup(ReturnObj1){
    document.forms[0].txt_po.value = ReturnObj1;
    document.forms[0].txt_po.focus();
}
function estimatelook(){ 
  var NewWindow = window.open("InvEst_lookup.aspx","EstimateLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}
function EstimateLookup(ReturnObj1){
    document.forms[0].txt_est.value = ReturnObj1;
    document.forms[0].txt_est.focus();
}

function Datelook(){ 
  var NewWindow = window.open("date_lookup.aspx","DateLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
  }
function Datelookup(obj)
{
  document.forms[0].txt_date.value=obj;
}


function Datelook1()
{
  document.forms[0].txt_date.value="";
  Datelook();
}

function InvLook(){ 
  var NewWindow = window.open("Invo_lookup.aspx","InvNumLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}

function InvNumLookup(ReturnObj1){
    document.forms[0].txt_inv.value = ReturnObj1;
    document.forms[0].txt_inv.focus();
}
function BolLook(){ 
  var NewWindow = window.open("Bol_lookup.aspx","BolNumLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}

function BolNumLookup(ReturnObj1){
    document.forms[0].txt_bol.value = ReturnObj1;
    document.forms[0].txt_bol.focus();
}
</script>

        <%--<asp:FormView ID="FormView1" runat="server" DataSourceID="ObjectDataSource1" Width="938px" Height="108px" >
            <ItemTemplate>
                ord-no:
                <asp:Label ID="ord_noLabel" runat="server" Text='<%# Bind("[ord-no]") %>'></asp:Label>
                <br />
                
                
            
            </ItemTemplate>
        </asp:FormView>--%>
       
        
       
        <table cellspacing="2" cellpadding="1" border="0" class="shade" bgcolor="gray" style="width: 80%">
          
        
        <tr>
        <td >
                <asp:Button ID="btnSearch" runat="server" Text="Go" OnClick="btnSearch_Click" CssClass="button" Width="58px" /><br />
            <br />
            <asp:Button ID="btn_reset" runat="server" CssClass="button" Text="All" OnClick="btn_reset_Click" Width="58px"  /></td>
            <td nowrap="nowrap" >
        
                <asp:CheckBox id="CheckBox1" runat="server" Text="Open" ></asp:CheckBox>
                <br />
         <asp:CheckBox  id="CheckBox2" runat="server" Text="Paid" ></asp:CheckBox>
            </td>
        <td nowrap valign="top"  >
            <br />
            Invoice #:<br>
        <asp:TextBox ID="txt_inv" runat="server" MaxLength="8" Width="70px"></asp:TextBox>
         <a href="#" tabindex="1" onClick="InvLook(); return false"><asp:Image ID="Inv" runat="server" ImageUrl="images/lookup_icon.gif"  /></a><br />
                    Inv Date:<br />
                <asp:TextBox ID="txt_date" MaxLength="10" runat="server" Width="70px" onKeyUp="invdateval()"></asp:TextBox>
                <a href="#" tabindex="1" onClick="Datelook(); return false"><asp:Image ID="Image3" runat="server" ImageUrl="images/lookup_icon.gif"  /></a>
                <asp:CompareValidator ID="CompareValidator6" runat="server" ControlToValidate="txt_date" Display="dynamic" Operator="DataTypeCheck" Type="Date" SetFocusOnError="true" ErrorMessage="Invalid Date. Format is MM/DD/YYYY"></asp:CompareValidator> 
                </td>
               
            <td nowrap="nowrap"  valign="top">                
                <br />
                Cust PO #:<br />
                <asp:TextBox ID="txt_po" runat="server" MaxLength="15" Width="70px"></asp:TextBox>
                <a href="#" tabindex="1" onClick="customerpolook(); return false"><asp:Image ID="PoLook" runat="server" ImageUrl="images/lookup_icon.gif"  /> </a><br />
                Bol #:<br /><asp:TextBox ID="txt_bol" runat="server" MaxLength="8" Width="70px"></asp:TextBox>
                <a href="#" tabindex="1" onClick="BolLook(); return false"><asp:Image ID="Bol" runat="server" ImageUrl="images/lookup_icon.gif" /> </a></td>
                
            
            <td nowrap="nowrap"  valign="top">
                <br />
                Cust Part #:<br />
                <asp:TextBox ID="txt_part" runat="server" MaxLength="15" Width="70px"></asp:TextBox>
                <a href="#" tabindex="1" onClick="custpartlook(); return false"><asp:Image ID="PartLook" runat="server" ImageUrl="images/lookup_icon.gif"  /></a><br />
                FG Item #:<br /> <asp:TextBox ID="txt_item" MaxLength="15" runat="server" Width="70px"></asp:TextBox>
                <a href="#" tabindex="1" onClick="fglook(); return false"><asp:Image ID="ItemLook" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
            </td>
           
        <td nowrap valign="top"><br />
            Est #:<br /><asp:TextBox ID="txt_est" runat="server" MaxLength="8" Width="70px"></asp:TextBox>
            <a href="#" tabindex="1" onClick="estimatelook(); return false"><asp:Image ID="EstLook" runat="server" ImageUrl="images/lookup_icon.gif"  /></a><br />
                   </td>
            
               
                <td nowrap valign="top" >
                
                    <br />
                    <asp:Label ID="customerlabel" runat="server" Text=" Customer #:"></asp:Label><br />
                 <asp:TextBox ID="txt_cust" runat="server" Width="70px"></asp:TextBox>
                 <a href="#" tabindex="1" onClick="customerlook(); return false"><asp:Image ID="CustomerLook" runat="server" ImageUrl="images/lookup_icon.gif"  /></a><br />
                    
                </td>
                              
        </tr>
        </TABLE>
                
        <br />
        <asp:GridView ID="GridView1" runat="server" AllowPaging="True" AllowSorting="True"
            AutoGenerateColumns="False" BorderStyle="Dotted" CssClass="Grid" DataSourceID="ObjectDataSource1"
            EmptyDataText="No Record Found" Width="85%" OnSelectedIndexChanged="GridView1_SelectedIndexChanged">
            <EmptyDataRowStyle BorderColor="#404040" BorderStyle="Dotted" BorderWidth="0px" Font-Bold="True"
                HorizontalAlign="Center" VerticalAlign="Middle" />
            <Columns>
                <asp:CommandField ButtonType="Image" SelectImageUrl="~/Images/sel.gif" SelectText=""
                    ShowSelectButton="True">
                    <ItemStyle Width="10px" />
                </asp:CommandField>
                <asp:BoundField DataField="inv-no" HeaderText="Inv#" SortExpression="inv-no" >
                    <ItemStyle Wrap="False" Width="40px" />
                </asp:BoundField>
                <asp:TemplateField HeaderText="Inv Date" SortExpression="inv-date">
                    <EditItemTemplate>
                        <asp:TextBox ID="TextBox1" runat="server" Text='<%# Bind("[inv-date]") %>'></asp:TextBox>
                    </EditItemTemplate>
                    <ItemStyle Wrap="False" Width="60px" />
                    <ItemTemplate>
                        <asp:Label ID="Label1" runat="server" Text='<%# Bind("[inv-date]","{0:d}") %>'></asp:Label>
                    </ItemTemplate>
                </asp:TemplateField>
                <asp:BoundField DataField="ord-no" HeaderText="Order#" SortExpression="ord-no" >
                    <ItemStyle Wrap="False" Width="60px" />
                </asp:BoundField>
                <asp:BoundField DataField="bol-no" HeaderText="Bol#" SortExpression="bol-no" >
                    <ItemStyle Wrap="False" Width="60px" />
                </asp:BoundField>
                <asp:BoundField DataField="cust-no" HeaderText="Cust#" SortExpression="cust-no" >
                    <ItemStyle Wrap="False" Width="60px" />
                </asp:BoundField>
                <asp:BoundField DataField="i-no" HeaderText="FG Item#" SortExpression="i-no" >
                    <ItemStyle Wrap="False" Width="70px" />
                </asp:BoundField>
                <asp:BoundField DataField="part-no" HeaderText="Cust Part#" SortExpression="part-no" >
                    <ItemStyle Wrap="False" Width="80px" />
                </asp:BoundField>
                <asp:BoundField DataField="po-no" HeaderText="Cust Po#" SortExpression="po-no" >
                    <ItemStyle Wrap="False" Width="60px" />
                </asp:BoundField>
                <asp:BoundField DataField="est-no" HeaderText="Est#" SortExpression="est-no" >
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
            <HeaderStyle   ForeColor="White" CssClass="headcolor" HorizontalAlign="Center"
                VerticalAlign="Middle" Wrap="False" />
            <AlternatingRowStyle CssClass="GridItemOdd" />
        </asp:GridView>
        <asp:ObjectDataSource ID="ObjectDataSource1" runat="server" OldValuesParameterFormatString="original_{0}"
            SelectMethod="SelectBrowsInvoice" TypeName="browsinvoice">
            <SelectParameters>
                <asp:Parameter Name="prmUser" Type="String" />
                <asp:Parameter DefaultValue="select" Name="prmAction" Type="String" />
                <asp:SessionParameter Name="prmOrderNum" SessionField="brwsinvoice" Type="String" />
                <asp:Parameter Name="prmInvoice" Type="String" />
                <asp:Parameter Name="prmCustomer" Type="String" />
                <asp:Parameter Name="prmItem" Type="String" />
                <asp:Parameter Name="prmAccount" Type="String" />
                <asp:Parameter Name="prmPart" Type="String" />
                <asp:Parameter Name="prmCustPo" Type="String" />
                <asp:Parameter Name="prmBOL" Type="String" />
                <asp:Parameter Name="prmEstimate" Type="String" />
                <asp:Parameter Name="prmDate" Type="String" />
                <asp:Parameter Name="prmOpen" Type="String" />
                <asp:Parameter Name="prmPaid" Type="String" />
            </SelectParameters>
        </asp:ObjectDataSource>
            <asp:HiddenField ID="openinvoice" runat="server" />        
              <asp:HiddenField ID="paidinvoice" runat="server"/>
        <br />
   
    </div>
    <ft:footer id="Footer1" runat="server"></ft:footer>
        &nbsp; &nbsp;
    </form>
</body>
</html>
