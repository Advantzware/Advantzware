<%@ Page Language="C#" AutoEventWireup="true" Inherits="eb_stock_look2" Codebehind="eb_stock_look.aspx.cs" %>

<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">

<html xmlns="http://www.w3.org/1999/xhtml" >
<head id="Head1" runat="server">
    <title>Select Estimate</title>
    <LINK href="include/style.css" type="text/css" rel="stylesheet"/>
<link rel="stylesheet" href="include/dhtmlwindow.css" type="text/css" />
</head>
<body>
    &nbsp;&nbsp;
    <form id="form1" runat="server" defaultfocus="txtSearchValue" defaultbutton="Button1">
    <div>
  <table id="tblSearch" cellSpacing="1" cellPadding="5" width="100%" border="0"  bgcolor=black>
  <tr>
  <td class="shade"><asp:button id="Button1" runat="server" Width="40px" CssClass="button" Text="Go" OnClick="btnSearch_Click"></asp:button>&nbsp;<br /><br />
                  <asp:button id="Button2" runat="server" Width="40px" CssClass="button" Text=" All" OnClick="btnShowAll_Click" ></asp:button>&nbsp;
 </td>
  <td id="tdSearch" runat="server" class="shade" vAlign="middle" align="center" width="800">&nbsp;                
                <B>Search for:&nbsp; </B>&nbsp;&nbsp;&nbsp;
                  <asp:dropdownlist id="ddlSearchField" runat="server">
                     <asp:ListItem Value="Est">Estimate</asp:ListItem>  
                      <asp:ListItem Value="Cust">Customer</asp:ListItem>
                      <asp:ListItem Value="FGItem">FG Item</asp:ListItem>  
                     
                      
                  </asp:dropdownlist>&nbsp;&nbsp;
                  <asp:dropdownlist id="ddlSearchOperation" runat="server">
                    <%--<asp:ListItem Value="Contains">Contains</asp:ListItem>--%>
                    
                    <asp:ListItem Value="BEGIN">BEGIN</asp:ListItem>
                    <asp:ListItem Value="EQUAL">EQUAL</asp:ListItem>
                                      
                  </asp:dropdownlist>
                  <asp:textbox id="txtSearchValue" runat="server" Width="136px"></asp:textbox>
                  
 </td>
  </tr>
  </table>
  </div>
    <div>
        <asp:GridView ID="GridView1"  AllowPaging="True" runat="server" AllowSorting="True" AutoGenerateColumns="False" DataSourceID="ObjectDataSource1"
            Style="position: static" OnSelectedIndexChanged="GridView1_SelectedIndexChanged" EmptyDataText="No Records Found" Width="100%" BorderStyle="Dotted" CssClass="Grid">
            <SelectedRowStyle CssClass="GridSelected" />
            <AlternatingRowStyle CssClass="GridItemOdd" />
            <EmptyDataRowStyle BorderStyle="Dotted" BorderColor="Gray" BorderWidth="0px" Font-Bold="True" HorizontalAlign="Center" VerticalAlign="Middle" />
            <RowStyle CssClass="shade"  />   
            <HeaderStyle  HorizontalAlign="Center" VerticalAlign="Middle" Wrap="False" BackColor="Teal" ForeColor="White" />
            
            <Columns>
             <asp:TemplateField>
                  <ItemStyle HorizontalAlign=Center Wrap="False" />
                   <ItemTemplate>       
		    <a href="#" onClick="javascript:top.opener.window.EbStockLook('<%#DataBinder.Eval(Container,"DataItem.vStock")%>','<%#DataBinder.Eval(Container,"DataItem.vPart")%>','<%#DataBinder.Eval(Container,"DataItem.vPartDscr")%>','<%#DataBinder.Eval(Container,"DataItem.vDscr2")%>','<%#DataBinder.Eval(Container,"DataItem.vProcat")%>','<%#DataBinder.Eval(Container,"DataItem.vLen")%>','<%#DataBinder.Eval(Container,"DataItem.vWid")%>','<%#DataBinder.Eval(Container,"DataItem.vDep")%>');window.close();">Select</a>             	    
                   </ItemTemplate>
                </asp:TemplateField>
            
                <asp:BoundField DataField="vEst" HeaderText="Estimate" SortExpression="vEst" />
                <asp:BoundField DataField="vCust" HeaderText="Customer" SortExpression="vCust" />
                <asp:BoundField DataField="vPart" HeaderText="Part-no" SortExpression="vPart" />
                <asp:BoundField DataField="vPartDscr" HeaderText="vPartDscr" SortExpression="vPartDscr" />
                <asp:BoundField DataField="vStock" HeaderText="FG Item" SortExpression="vStock" />
                  <%--<asp:BoundField DataField="vDscr2" HeaderText="vDscr2" SortExpression="vDscr2" />
                <asp:BoundField DataField="vProcat" HeaderText="vProcat" SortExpression="vProcat" />
                <asp:BoundField DataField="vLen" HeaderText="vLen" SortExpression="vLen" />
                <asp:BoundField DataField="vWid" HeaderText="vWid" SortExpression="vWid" />
                <asp:BoundField DataField="vDep" HeaderText="vDep" SortExpression="vDep" />--%>
                
                
            </Columns>
        </asp:GridView>
        <input type="button" name="close" class="buttonM" id="close" value="Close" onclick="javascript:window.close()" />
        <asp:ObjectDataSource ID="ObjectDataSource1" runat="server" OldValuesParameterFormatString="original_{0}"
            SelectMethod="SelectEbStockLook" TypeName="LookUp">
            <SelectParameters>               
                 <asp:Parameter Name="prmAction" Type="String" />
                 <asp:Parameter Name="prmUser"  Type="String" />
                <asp:Parameter Name="prmField" Type="String" />
                <asp:Parameter Name="prmCondition" Type="String" />
                <asp:Parameter Name="prmText" Type="String" />
                <asp:SessionParameter SessionField="corr_est_type_eb"  Name="prmType" Type="Int32" />
                 <asp:Parameter Name="prmEstimate" Type="String" />
            </SelectParameters>
        </asp:ObjectDataSource>
        
    
    </div>
    </form>
</body>
</html>


