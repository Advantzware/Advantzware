<%@ Page Language="C#" AutoEventWireup="true" Inherits="fg_lookup" Codebehind="fg_lookup.aspx.cs" %>

<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">

<html xmlns="http://www.w3.org/1999/xhtml" >
<head id="Head1" runat="server">
    <title>FG Item Lookup</title>
    <LINK href="include/style.css" type="text/css" rel="stylesheet"/>
<link rel="stylesheet" href="include/dhtmlwindow.css" type="text/css" />
</head>
<body>
    &nbsp;&nbsp;
    <form id="form1" runat="server" defaultfocus="txtSearchValue">
    <div>
    <asp:Panel ID="Panel1" runat="server" DefaultButton="Button1">
  <table id="tblSearch" runat="server" cellSpacing="1" cellPadding="5" width="100%" border="0"  bgcolor=black>
  <tr>
  <td class="shade"><asp:button id="Button1" runat="server" Width="40px" CssClass="button" Text="Go" OnClick="btnSearch_Click"></asp:button>&nbsp;<br /><br />
                  <asp:button id="Button2" runat="server" Width="40px" CssClass="button" Text=" All" OnClick="btnShowAll_Click" ></asp:button>&nbsp;
 </td>
  <td id="tdSearch" runat="server" class="shade" vAlign="middle" align="center" width="800">&nbsp;                
                <B>Search for:&nbsp; </B>&nbsp;&nbsp;&nbsp;
                  <asp:dropdownlist id="ddlSearchField" runat="server">
                    <%--<asp:ListItem Value="Any">Any</asp:ListItem>--%>
                      <asp:ListItem Value="Item">FG Item</asp:ListItem>
                       <asp:ListItem Value="name">Name</asp:ListItem>  
                       <asp:ListItem Value="CustPart">Cust Part</asp:ListItem>
                      
                  </asp:dropdownlist>&nbsp;&nbsp;
                  <asp:dropdownlist id="ddlSearchOperation" runat="server">
                    <%--<asp:ListItem Value="Contains">Contains</asp:ListItem>--%>
                    
                    <asp:ListItem Value="BEGIN">BEGIN</asp:ListItem>
                     <asp:ListItem Value="EQUAL">EQUAL</asp:ListItem>                 
                  </asp:dropdownlist>
                  <asp:textbox id="txtSearchValue" runat="server" Width="136px"></asp:textbox>
                  
 </td>
  </tr>
  </table></asp:Panel>
  </div>
    <div>
        <asp:GridView ID="GridView1" AllowPaging="True" runat="server" AllowSorting="true" Width="700px" AutoGenerateColumns="False" DataSourceID="ObjectDataSource1"
            Style="position: static" OnSelectedIndexChanged="GridView1_SelectedIndexChanged" EmptyDataText="No Records Found"  BorderStyle="Dotted" CssClass="Grid" >
            <SelectedRowStyle CssClass="GridSelected" />
            <AlternatingRowStyle CssClass="GridItemOdd" />
            <EmptyDataRowStyle BorderStyle="Dotted" BorderColor="Gray" BorderWidth="0px" Font-Bold="True" HorizontalAlign="Center" VerticalAlign="Middle" />
            <RowStyle CssClass="shade"  />   
            <HeaderStyle  HorizontalAlign="Center" VerticalAlign="Middle" Wrap="False" BackColor="Teal" ForeColor="White" />
            
            <Columns> 
            <asp:TemplateField>
                  <ItemStyle HorizontalAlign=Center Wrap="False" />
                   <ItemTemplate>       
		    <a href="#" onClick="javascript:top.opener.window.FGItemLookup('<%#DataBinder.Eval(Container,"DataItem.vino") %>', '<%#DataBinder.Eval(Container,"DataItem.viname")%>', '<%#DataBinder.Eval(Container,"DataItem.vcust")%>', '<%#DataBinder.Eval(Container,"DataItem.vcustpart")%>', '<%#DataBinder.Eval(Container,"DataItem.vdscr")%>', '<%#DataBinder.Eval(Container,"DataItem.vpartdscr1")%>', '<%#DataBinder.Eval(Container,"DataItem.vpartdscr2")%>', '<%#DataBinder.Eval(Container,"DataItem.vprice")%>', '<%#DataBinder.Eval(Container,"DataItem.vuom")%>', '<%#DataBinder.Eval(Container,"DataItem.vcasecount")%>', '<%#DataBinder.Eval(Container,"DataItem.vcasepall")%>', '<%#DataBinder.Eval(Container,"DataItem.vtotprice")%>', '<%#DataBinder.Eval(Container,"DataItem.vcost")%>', '<%#DataBinder.Eval(Container,"DataItem.vtype")%>','<%#DataBinder.Eval(Container,"DataItem.vdiscount")%>');window.close();">Select</a>             	    
                   </ItemTemplate>
                </asp:TemplateField> 
                <asp:BoundField DataField="vino2" HeaderText="FG Item" SortExpression="vino2" />
                <asp:BoundField DataField="viname2" HeaderText="Item Name" SortExpression="viname2" />
                <asp:BoundField DataField="vhand" HeaderText="Qty-on-Hand" SortExpression="vhand" />
                <asp:BoundField DataField="vcust" HeaderText="Cust#" SortExpression="vcust" />
                <asp:BoundField DataField="vcustpart2" HeaderText="Cust Part#" SortExpression="vcustpart2" />
                <asp:BoundField DataField="vdscr2" HeaderText="Item Description" SortExpression="vdscr2" />
                <asp:CheckBoxField DataField="vmat" HeaderText="Stock Mat" SortExpression="vmat" />
                <asp:BoundField DataField="vest" HeaderText="Estimate" SortExpression="vest" />
               <asp:BoundField DataField="vprice" HeaderText="Price" SortExpression="vprice" />
               <asp:BoundField DataField="vcasecount" HeaderText="Casecount" SortExpression="vcasecount" />
              
                
            </Columns>
        </asp:GridView>
        
        <asp:ObjectDataSource ID="ObjectDataSource1" runat="server" OldValuesParameterFormatString="original_{0}"
            SelectMethod="SelectfgLookup" TypeName="LookUp">
            <SelectParameters>               
                                <asp:Parameter Name="prmAction" Type="String" />
                                <asp:Parameter Name="prmUser"  Type="String" />
                <asp:Parameter Name="prmField" Type="String" />
                <asp:Parameter Name="prmCondition" Type="String" />
                <asp:Parameter Name="prmText" Type="String" />
                <asp:SessionParameter SessionField="order_entry_cust_no" Name="prmCustomer" Type="String" />
                <asp:QueryStringParameter Name="prmQty" QueryStringField="qty" Type="int32" />
                <asp:QueryStringParameter QueryStringField="quote"  Name="prmQuote" Type="int32" />
            </SelectParameters>
        </asp:ObjectDataSource>
    
    </div>
    
    </form>
</body>
</html>

