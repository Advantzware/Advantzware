
<%@ Page Language="C#" AutoEventWireup="true" Inherits="crdb_inv_lookup" Codebehind="crdb_inv_lookup.aspx.cs" %>

<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">

<html xmlns="http://www.w3.org/1999/xhtml" >
<head id="Head1" runat="server">
    <title>Invoice Information</title>
    <LINK href="include/style.css" type="text/css" rel="stylesheet"/>
<link rel="stylesheet" href="include/dhtmlwindow.css" type="text/css" />
</head>
<script type="text/javascript" src="include/dhtmlwindow.js"></script>
<body>
    <form id="form1" runat="server" defaultfocus="txtSearchValue">
    <asp:Panel ID="Panel1" runat="server" DefaultButton="btnSearch">
    <div>
  <table id="tblSearch" cellSpacing="1" cellPadding="5" width="100%" border="0"  bgcolor=black>
  <tr>
  <td class="shade">
  <asp:button id="btnSearch" runat="server" width="50px" CssClass="button" Text="Go" OnClick="btnSearch_Click"></asp:button>
                  <asp:button id="btnShowAll" runat="server" width="50px" CssClass="button" Text="All" OnClick="btnShowAll_Click" ></asp:button>&nbsp;</td>
  <td nowrap  id="tdSearch" runat="server" class="shade" vAlign="middle" align="center" width="400">&nbsp;                
                <B>Search for:&nbsp; </B>&nbsp;&nbsp;&nbsp;
                  <asp:dropdownlist id="ddlSearchField" runat="server">
                      <asp:ListItem Value="inv">Invoice#</asp:ListItem>  
                      
                      
                  </asp:dropdownlist>&nbsp;&nbsp;
                  <asp:dropdownlist id="ddlSearchOperation" runat="server">
                    
                    <asp:ListItem Value="EQUAL">EQUAL</asp:ListItem>
                    
                                    
                  </asp:dropdownlist>
                  <asp:textbox id="txtSearchValue" runat="server" Width="136px"></asp:textbox>
                  
 </td>
  </tr>
  </table>
  </div>
  </asp:Panel>
    
    <div>
        <asp:GridView ID="GridView1" runat="server" AutoGenerateColumns="False" OnSelectedIndexChanged="GridView1_SelectedIndexChanged"
            DataSourceID="ObjectDataSource1" AllowPaging="True" AllowSorting="True"  
            EmptyDataText="No Records Found" Width="100%" BorderStyle="Dotted" CssClass="Grid">
            <SelectedRowStyle CssClass="GridSelected" />
            <AlternatingRowStyle CssClass="GridItemOdd" />
            <EmptyDataRowStyle BorderStyle="Dotted" BorderColor="Gray" BorderWidth="0px" Font-Bold="True" HorizontalAlign="Center" VerticalAlign="Middle" />
            <RowStyle CssClass="shade"  />   
            <HeaderStyle CssClass="gridrowhdr" HorizontalAlign="Center" VerticalAlign="Middle" Wrap="False" BackColor="Gray" ForeColor="White" />
            <Columns > 
              <asp:TemplateField>
                  <ItemStyle HorizontalAlign=Center />
                   <ItemTemplate>       
		    <a href="#" onClick="javascript:top.opener.window.arinvlook('<%#DataBinder.Eval(Container,"DataItem.invno")%>','<%#DataBinder.Eval(Container,"DataItem.invdt")%>','<%#DataBinder.Eval(Container,"DataItem.due")%>');window.close();">Select</a>
                   </ItemTemplate>
                </asp:TemplateField>
                <asp:BoundField DataField="custno" HeaderText="Customer#" SortExpression="custno" >
                    <ItemStyle Wrap="False" />
                </asp:BoundField>
                <asp:BoundField DataField="invno" HeaderText="Invoice#" SortExpression="invno" >
                    <ItemStyle Wrap="False" />
                </asp:BoundField>
                <asp:BoundField DataField="invdt" HeaderText="Invoice Date" SortExpression="invdt" >
                    <ItemStyle Wrap="False" />
                </asp:BoundField>
                <asp:BoundField DataField="net" HeaderText="Net" SortExpression="net" >
                    <ItemStyle Wrap="False" />
                </asp:BoundField>
                <asp:BoundField DataField="paid" HeaderText="Amount Paid" SortExpression="paid" >
                    <ItemStyle Wrap="False" />
                </asp:BoundField>
              <asp:BoundField DataField="due" HeaderText="Balance Due" SortExpression="due" >
                    <ItemStyle Wrap="False" />
                </asp:BoundField>
                                
                </Columns>
            
        </asp:GridView>
        <input type="button" name="close" class="buttonM" id="close" value="Close" onclick="javascript:window.close()" />
        <asp:ObjectDataSource ID="ObjectDataSource1" runat="server" OldValuesParameterFormatString="original_{0}"
            SelectMethod="CrDbInvoiceLookup" TypeName="account">
            <SelectParameters>
                <asp:Parameter Name="prmAction" Type="String" DefaultValue="Select" />
                <asp:Parameter Name="prmUser" Type="String" />                
                <asp:Parameter Name="prmField" Type="String" />
                <asp:Parameter Name="prmCondition" Type="String" />
                <asp:Parameter Name="prmText" Type="String" />
                <asp:QueryStringParameter QueryStringField="cust" Name="prmCust" Type="String" DefaultValue="" />
            </SelectParameters>
        </asp:ObjectDataSource>
    
    </div>
    </form>
</body>
</html>
