<%@ Page Language="C#" AutoEventWireup="true" Inherits="company_lookup" Codebehind="company_lookup.aspx.cs" %>

<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">

<html xmlns="http://www.w3.org/1999/xhtml" >
<head runat="server">
    <title>Company Lookup</title>
    <LINK href="include/style.css" type="text/css" rel="stylesheet"/>
<link rel="stylesheet" href="include/dhtmlwindow.css" type="text/css" />
</head>
<body>
    &nbsp;&nbsp;
    <form id="form1" runat="server" defaultfocus="txtSearchValue">
    
    <asp:Panel ID="Panel1" runat="server" DefaultButton="btnSearch">    
    <div>
  <table id="tblSearch" cellSpacing="1" cellPadding="5" width="100%" border="0"  bgcolor=black>
  <tr><td class="shade"><asp:button id="btnSearch" runat="server" Width="40px" CssClass="button" Text="Go" OnClick="btnSearch_Click"></asp:button>&nbsp;<br /><br />
                  <asp:button id="btnShowAll" runat="server" Width="40px" CssClass="button" Text=" All "  OnClick="btnShowAll_Click"  ></asp:button>&nbsp;
 </td>
  <td id="tdSearch" runat="server" class="shade" vAlign="middle" align="center" width="800">&nbsp;                
                <B>Search for:&nbsp; </B>&nbsp;&nbsp;&nbsp;
                  <asp:dropdownlist id="ddlSearchField" runat="server">
                    <%--<asp:ListItem Value="Any">Any</asp:ListItem>--%>
                      <asp:ListItem Value="company">Comapny</asp:ListItem>  
                      <asp:ListItem Value="name">Name</asp:ListItem>
                      
                  </asp:dropdownlist>&nbsp;&nbsp;
                  <asp:dropdownlist id="ddlSearchOperation" runat="server">
                    <%--<asp:ListItem Value="Contains">Contains</asp:ListItem>--%>
                    <asp:ListItem Value="EQUAL">EQUAL</asp:ListItem>
                    <asp:ListItem Value="BEGIN">BEGIN</asp:ListItem>
                                      
                  </asp:dropdownlist>
                  <asp:textbox id="txtSearchValue" runat="server" Width="136px"></asp:textbox>
                  </td>
  </tr>
  </table>
  </div>
  </asp:Panel>
    
    <div>
        <asp:GridView ID="GridView1" runat="server" AllowSorting="true" AutoGenerateColumns="False" DataSourceID="ObjectDataSource1"
            Style="position: static" OnSelectedIndexChanged="GridView1_SelectedIndexChanged" EmptyDataText="No Records Found" Width="100%" BorderStyle="Dotted" CssClass="Grid">
            <SelectedRowStyle CssClass="GridSelected" />
            <AlternatingRowStyle CssClass="GridItemOdd" />
            <EmptyDataRowStyle BorderStyle="Dotted" BorderColor="Gray" BorderWidth="0px" Font-Bold="True" HorizontalAlign="Center" VerticalAlign="Middle" />
            <RowStyle CssClass="shade"  />   
            <HeaderStyle  BackColor="teal" VerticalAlign="Middle"  ForeColor ="white"   HorizontalAlign="Center" Wrap="False"></HeaderStyle>
            
            <Columns>
            <asp:TemplateField>
                  <ItemStyle HorizontalAlign=Center Wrap="False" />
                   <ItemTemplate>       
		    <a href="#" onClick="javascript:top.opener.window.CompanyLookup('<%#DataBinder.Eval(Container,"DataItem.vCompany")%>','<%#DataBinder.Eval(Container,"DataItem.vName")%>');window.close();">Select</a>             	    
                   </ItemTemplate>
                </asp:TemplateField> 
                <asp:BoundField DataField="vCompany" HeaderText="Company" SortExpression="vCompany" >
                    <ItemStyle Wrap="False" />
                </asp:BoundField>
                <asp:BoundField DataField="vName" HeaderText="Name" SortExpression="vName" >
                    <ItemStyle Wrap="False" />
                </asp:BoundField>
                
                <asp:BoundField DataField="vCurrCode" HeaderText="Currency Code" SortExpression="vCurrCode" >
                    <ItemStyle Wrap="False" />
                </asp:BoundField>
            </Columns>
        </asp:GridView>
        <input type="button" name="close" class="buttonM" id="close" value="Close" onclick="javascript:window.close()" />
        
        <asp:ObjectDataSource ID="ObjectDataSource1" runat="server" OldValuesParameterFormatString="original_{0}"
            SelectMethod="SelCompLook" TypeName="LookUp">
            <SelectParameters>
                <asp:Parameter Name="prmAction" Type="String" />
               <%-- <asp:Parameter Name="prmUser" Type="String" />--%>
                <asp:SessionParameter Name="prmUser" SessionField="comp" Type="String" />
                <asp:Parameter Name="prmField" Type="String" />
                <asp:Parameter Name="prmCondition" Type="String" />
                <asp:Parameter Name="prmText" Type="String" />
            </SelectParameters>
        </asp:ObjectDataSource>
    
    </div>
    </form>
</body>
</html>
