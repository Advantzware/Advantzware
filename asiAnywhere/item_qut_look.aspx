<%@ Page Language="C#" AutoEventWireup="true" Debug="true" Inherits="item_qut_look" Codebehind="item_qut_look.aspx.cs" %>

<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">

<html xmlns="http://www.w3.org/1999/xhtml" >
<head id="Head1" runat="server">
    <title>Item Quotes Lookup</title>
    <LINK href="include/style.css" type="text/css" rel="stylesheet"/>
<link rel="stylesheet" href="include/dhtmlwindow.css" type="text/css" />
</head>
<body>

    &nbsp;&nbsp;
    <form id="form1" runat="server" defaultfocus="txtSearchValue" >
    <div>
    <asp:Panel ID="Panel1" runat="server" DefaultButton="Button1">
    <table id="tblSearch" runat="server" cellSpacing="1" cellPadding="5" width="700px" border="0"  bgcolor=black>
  <tr>
  <td class="shade"><asp:button id="Button1" runat="server" Width="40px" CssClass="button" Text="Go" OnClick="btnSearch_Click"></asp:button>&nbsp;<br /><br />
                  <asp:button id="Button2" runat="server" Width="40px" CssClass="button" Text=" All" OnClick="btnShowAll_Click" ></asp:button>&nbsp;
 </td>
  <td id="tdSearch" runat="server" class="shade" vAlign="middle" align="center" width="800">&nbsp;                
                <B>Search for:&nbsp; </B>&nbsp;&nbsp;&nbsp;
                  <asp:dropdownlist id="ddlSearchField"  runat="server">
                    <asp:ListItem Value="Quote">Quote</asp:ListItem>  
                    <asp:ListItem Value="Est">Estimate</asp:ListItem>                     
                  </asp:dropdownlist>&nbsp;&nbsp;
                  <asp:dropdownlist id="ddlSearchOperation" Enabled="false" runat="server">
                    
                     <asp:ListItem Value="EQUAL">EQUAL</asp:ListItem>                 
                  </asp:dropdownlist>
                  <asp:textbox id="txtSearchValue" runat="server" Width="120px"></asp:textbox>
                  
 </td>
  </tr>
  </table></asp:Panel>
  
  </div>
    <div>
        <asp:GridView ID="GridView1" AllowPaging="True" runat="server" AllowSorting="True" AutoGenerateColumns="False" DataSourceID="ObjectDataSource1"
            Style="position: static" OnSelectedIndexChanged="GridView1_SelectedIndexChanged" EmptyDataText="Sorry! Quote does not match Customer on Order" Width="600px" BorderStyle="Dotted" CssClass="Grid">
            <SelectedRowStyle CssClass="GridSelected" />
            <AlternatingRowStyle CssClass="GridItemOdd" />
            <EmptyDataRowStyle BorderStyle="Dotted" BorderColor="Gray" BorderWidth="0px" Font-Bold="True" HorizontalAlign="Center" VerticalAlign="Middle" />
            <RowStyle CssClass="shade"  />   
            <HeaderStyle  HorizontalAlign="Center" VerticalAlign="Middle" Wrap="False" BackColor="Teal" ForeColor="White" />
            
            
            <Columns>
            <asp:TemplateField>
                  <ItemStyle HorizontalAlign=Center Wrap="False" />
                   <ItemTemplate>       
		    <a href="#" onClick="javascript:top.opener.window.ItemQuoteLookup('<%#DataBinder.Eval(Container,"DataItem.vqno")%>', '<%#DataBinder.Eval(Container,"DataItem.vino")%>', '<%#DataBinder.Eval(Container,"DataItem.viname")%>','<%#DataBinder.Eval(Container,"DataItem.vhand")%>','<%#DataBinder.Eval(Container,"DataItem.vcust")%>','<%#DataBinder.Eval(Container,"DataItem.vcustpart")%>','<%#DataBinder.Eval(Container,"DataItem.vdscr")%>','<%#DataBinder.Eval(Container,"DataItem.vest")%>','<%#DataBinder.Eval(Container,"DataItem.vpartdscr1")%>','<%#DataBinder.Eval(Container,"DataItem.vpartdscr2")%>','<%#DataBinder.Eval(Container,"DataItem.vprice")%>','<%#DataBinder.Eval(Container,"DataItem.vuom")%>','<%#DataBinder.Eval(Container,"DataItem.vtype ")%>','<%#DataBinder.Eval(Container,"DataItem.vdiscount")%>','<%#DataBinder.Eval(Container,"DataItem.vqty")%>','<%#DataBinder.Eval(Container,"DataItem.vqtyunit")%>','<%#DataBinder.Eval(Container,"DataItem.vunitpallet")%>','<%#DataBinder.Eval(Container,"DataItem.vpartial")%>');window.close();">Select</a>             	    
                   </ItemTemplate>
                </asp:TemplateField>
                
                <asp:BoundField ItemStyle-Wrap="false" DataField="vqno" HeaderText="Quote" SortExpression="vqno" />
                <asp:BoundField DataField="vino2" ItemStyle-Wrap="false" HeaderText="Item" SortExpression="vino2" />                
                <asp:BoundField DataField="vcustpart2" ItemStyle-Wrap="false" HeaderText="CustPart" SortExpression="vcustpart2" />
                <asp:BoundField DataField="vdscr2" ItemStyle-Wrap="false" HeaderText="Item Name" SortExpression="vdscr2" />
                <asp:BoundField DataField="vpartdscr3" ItemStyle-Wrap="false" HeaderText="Dscription" SortExpression="vpartdscr3" />
                <asp:BoundField DataField="vpartdscr4" ItemStyle-Wrap="false" HeaderText="dscription" SortExpression="vpartdscr4" />
                <asp:BoundField DataField="vprice" ItemStyle-Wrap="false" HeaderText="Price" SortExpression="vprice" />
                <asp:BoundField DataField="vuom" ItemStyle-Wrap="false" HeaderText="Uom" SortExpression="vuom" />               
                <asp:BoundField DataField="vdiscount" ItemStyle-Wrap="false" HeaderText="Discount" SortExpression="vdiscount" />
                <asp:BoundField DataField="vqty" ItemStyle-Wrap="false" HeaderText="Qty" SortExpression="vqty" />
                <asp:BoundField DataField="vest" HeaderText="Estimate" SortExpression="vest" />
                <%--<asp:BoundField DataField="viname2" HeaderText="Name" SortExpression="viname2" />
                <asp:BoundField DataField="vino" HeaderText="vino" SortExpression="vino" />
                <asp:BoundField DataField="viname" HeaderText="viname" SortExpression="viname" />
                <asp:BoundField DataField="vhand" HeaderText="vhand" SortExpression="vhand" />
                <asp:BoundField DataField="vcust" HeaderText="vcust" SortExpression="vcust" />
                <asp:BoundField DataField="vcustpart" HeaderText="vcustpart" SortExpression="vcustpart" />
                <asp:BoundField DataField="vdscr" HeaderText="vdscr" SortExpression="vdscr" />
                <asp:BoundField DataField="vest" HeaderText="vest" SortExpression="vest" />
                <asp:BoundField DataField="vpartdscr1" HeaderText="vpartdscr1" SortExpression="vpartdscr1" />
                <asp:BoundField DataField="vpartdscr2" HeaderText="vpartdscr2" SortExpression="vpartdscr2" />
                 <asp:BoundField DataField="vtype" HeaderText="vtype" SortExpression="vtype" />--%>
                
                
                
                
            </Columns>
        </asp:GridView>
        
        <asp:ObjectDataSource ID="ObjectDataSource1" EnableCaching="true" CacheDuration="3600" runat="server" OldValuesParameterFormatString="original_{0}"
            SelectMethod="ItemQuoteLookup" TypeName="LookUp">
            <SelectParameters>
                                <asp:Parameter Name="prmAction" DefaultValue = "" Type="String" />
                                <asp:Parameter Name="prmUser" Type="String" />
                                <asp:Parameter Name="prmField" Type="String" />
                                <asp:Parameter Name="prmCondition" Type="String" />
                                <asp:Parameter Name="prmText" Type="String" />
                                <asp:QueryStringParameter QueryStringField="quote"  Name="prmQuote" Type="String" />
                                <asp:QueryStringParameter QueryStringField="cust"  Name="prmCust" Type="String" />
            </SelectParameters>
        </asp:ObjectDataSource>
    
    
    
    <div>
        
        <input type="button" name="close" class="buttonM" id="close" value="Close" onclick="javascript:window.close()" />
        
       
    </div>
    </div>
    </form>
</body>
</html>

