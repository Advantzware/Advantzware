<%@ Page Language="C#" AutoEventWireup="true" Inherits="taxgr_lookup" Codebehind="taxgr_lookup.aspx.cs" %>

<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">

<html xmlns="http://www.w3.org/1999/xhtml" >
<head id="Head1" runat="server">
    <title>Tax Code Lookup</title>
    <LINK href="include/style.css" type="text/css" rel="stylesheet"/>
<link rel="stylesheet" href="include/dhtmlwindow.css" type="text/css" />
</head>
<body>
    &nbsp;&nbsp;
    <form id="form1" runat="server" defaultfocus="txtSearchValue">
    <div>
     <asp:Panel ID="searchpanel" runat="server" DefaultButton="Button1">
  <table id="tblSearch" cellSpacing="1" cellPadding="5" width="100%" border="0"  bgcolor=black>
  <tr>
  <td class="shade"><asp:button id="Button1" runat="server" Width="40px" CssClass="button" Text="Go" OnClick="btnSearch_Click"></asp:button>&nbsp;<br /><br />
                  <asp:button id="Button2" runat="server" Width="40px" CssClass="button" Text=" All" OnClick="btnShowAll_Click" ></asp:button>&nbsp;
 </td>
  <td id="tdSearch" runat="server" class="shade" vAlign="middle" align="center" width="800">&nbsp;                
                <B>Search for:&nbsp; </B>&nbsp;&nbsp;&nbsp;
                  <asp:dropdownlist id="ddlSearchField" runat="server">
                    <%--<asp:ListItem Value="Any">Any</asp:ListItem>--%>
                      <asp:ListItem Value="group">Tax Group</asp:ListItem>
                      <asp:ListItem Value="code">Tax Code</asp:ListItem>  
                     
                      
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
        <asp:GridView ID="GridView1" AllowPaging="True" runat="server" 
            AllowSorting="True" AutoGenerateColumns="False" DataSourceID="ObjectDataSource1"
            Style="position: static" 
            OnSelectedIndexChanged="GridView1_SelectedIndexChanged" 
            EmptyDataText="No Records Found" Width="100%" BorderStyle="Dotted" 
            CssClass="Grid">
            <SelectedRowStyle CssClass="GridSelected" />
            <AlternatingRowStyle CssClass="GridItemOdd" />
            <EmptyDataRowStyle BorderStyle="Dotted" BorderColor="Gray" BorderWidth="0px" Font-Bold="True" HorizontalAlign="Center" VerticalAlign="Middle" />
            <RowStyle CssClass="shade"  />   
            <HeaderStyle  HorizontalAlign="Center" VerticalAlign="Middle" Wrap="False" BackColor="teal" ForeColor="White" />
            
            <Columns>
                          
                <asp:TemplateField>
                  <ItemStyle HorizontalAlign=Center Wrap="False" />
                   <ItemTemplate>       
		    <a href="#" onClick="javascript:top.opener.window.TaxGroupLookup('<%#DataBinder.Eval(Container,"DataItem.vgroup")%>','<%#DataBinder.Eval(Container,"DataItem.vtaxcode1")%>','<%#DataBinder.Eval(Container,"DataItem.vdscr")%>','<%#DataBinder.Eval(Container,"DataItem.rate")%>','<%#DataBinder.Eval(Container,"DataItem.taxfrt")%>','<%#DataBinder.Eval(Container,"DataItem.taxacc")%>');window.close();">Select</a>             	    
                   </ItemTemplate>
                </asp:TemplateField>  
                
                 <asp:BoundField DataField="vgroup" HeaderText="Tax Group"  SortExpression="vgroup" />
                 <asp:BoundField DataField="vtaxcode1" HeaderText="Tax Code" SortExpression="vtaxcode1" /> 
                 <asp:BoundField DataField="vdscr" HeaderText="Description[1]" SortExpression="vdscr" />
                 <asp:BoundField DataField="rate" HeaderText="Tax Rate[1]" SortExpression="rate" />
                 <asp:BoundField DataField="taxfrt" HeaderText="Tax frt" SortExpression="taxfrt" />
                 <asp:BoundField DataField="taxacc" HeaderText="Sales Tax Account" SortExpression="taxacc" />
                 <asp:BoundField DataField="vtaxcode2" HeaderText="Tax Code" SortExpression="vtaxcode2" />
                 <asp:BoundField DataField="vdscr2" HeaderText="Description[2]" SortExpression="vdscr2" />
                 <asp:BoundField DataField="vtaxcode3" HeaderText="Tax Code" SortExpression="vtaxcode3" />
                 <asp:BoundField DataField="vdscr3" HeaderText="Description[3]" SortExpression="vdscr3" />
                 <asp:BoundField DataField="vtaxcode4" HeaderText="Tax Code" SortExpression="vtaxcode4" />
                 <asp:BoundField DataField="vdscr4" HeaderText="Description[4]" SortExpression="vdscr4" />
                 <asp:BoundField DataField="vtaxcode5" HeaderText="Tax Code" SortExpression="vtaxcode5" />
                 <asp:BoundField DataField="vdscr5" HeaderText="Description[5]" SortExpression="vdscr5" />               
                
            </Columns>
        </asp:GridView>
        <input type="button" name="close" class="buttonM" id="close" value="Close" onclick="javascript:window.close()" />
        <asp:ObjectDataSource ID="ObjectDataSource1" runat="server" OldValuesParameterFormatString="original_{0}"
            SelectMethod="SelectTaxGroupLook" TypeName="voucherpay">
            <SelectParameters>               
                                <asp:Parameter Name="prmAction" Type="String" />
                                <asp:Parameter Name="prmUser"  Type="String" />
                <asp:Parameter Name="prmField" Type="String" />
                <asp:Parameter Name="prmCondition" Type="String" />
                <asp:Parameter Name="prmText" Type="String" />
            </SelectParameters>
        </asp:ObjectDataSource>
    
    </div>
    </form>
</body>
</html>

