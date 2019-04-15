<%@ Page Language="C#" AutoEventWireup="true" CodeFile="corr_bom_item_lookup.aspx.cs" Inherits="corr_bom_item_lookup" %>

<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">

<html xmlns="http://www.w3.org/1999/xhtml" >
<head runat="server">
    <title>Paper Information</title>
    <LINK href="include/style.css" type="text/css" rel="stylesheet"/>
</head>
<body>
    <form id="form1" runat="server" defaultfocus="txtSearchValue" DefaultButton="Button1">
    <div>
        <table id="tblSearch" cellSpacing="1" cellPadding="5" width="100%" border="0"  bgcolor=black>
            <tr>
                <td class="shade">
                    <asp:button id="Button1" runat="server" Width="40px" CssClass="button" Text="Go" OnClick="btnSearch_Click"></asp:button>&nbsp;<br /><br />
                    <asp:button id="Button2" runat="server" Width="40px" CssClass="button" Text=" All" OnClick="btnShowAll_Click" ></asp:button>&nbsp;
                </td>
                <td id="tdSearch" runat="server" class="shade" vAlign="middle" align="center" width="800">&nbsp;                
                    <B>Search for:&nbsp; </B>&nbsp;&nbsp;&nbsp;
                    <asp:dropdownlist id="ddlSearchField" runat="server">                    
                        <asp:ListItem Value="item">Item</asp:ListItem>  
                        <asp:ListItem Value="name">Name</asp:ListItem>                        
                    </asp:dropdownlist>&nbsp;&nbsp;
                    <asp:dropdownlist id="ddlSearchOperation" runat="server">                    
                        <asp:ListItem Value="BEGIN">BEGIN</asp:ListItem>
                        <asp:ListItem Value="EQUAL">EQUAL</asp:ListItem>                                      
                    </asp:dropdownlist>
                    <asp:textbox id="txtSearchValue" runat="server" Width="136px"></asp:textbox>                  
                </td>
            </tr>
        </table>
    </div>
    <div>
        <asp:GridView ID="GridView1"  AllowPaging="true" PageSize ="10" runat="server" AllowSorting="true" AutoGenerateColumns="False" DataSourceID="ObjectDataSource1"
            Style="position: static" EmptyDataText="No Records Found" Width="100%" BorderStyle="Dotted" CssClass="Grid">
            <SelectedRowStyle CssClass="GridSelected" />
            <AlternatingRowStyle CssClass="GridItemOdd" />
            <EmptyDataRowStyle BorderStyle="Dotted" BorderColor="Gray" BorderWidth="0px" Font-Bold="True" HorizontalAlign="Center" VerticalAlign="Middle" />
            <RowStyle CssClass="shade"  />   
            <HeaderStyle  HorizontalAlign="Center" VerticalAlign="Middle" Wrap="False" BackColor="teal" ForeColor="White" />
            <Columns>
                <asp:TemplateField>
                  <ItemStyle HorizontalAlign=Center Wrap="False" />
                   <ItemTemplate>       
		                <a href="#" onClick="javascript:top.opener.window.ItemLookup7('<%#DataBinder.Eval(Container,"DataItem.vItemNo2")%>','<%#DataBinder.Eval(Container,"DataItem.vItemName2")%>','<%#DataBinder.Eval(Container,"DataItem.vShrink")%>','<%#DataBinder.Eval(Container,"DataItem.vSqInch")%>');window.close();">Select</a>             	    
                   </ItemTemplate>
                </asp:TemplateField> 
                
                <asp:BoundField DataField="vItemNo" HeaderText="Item No" SortExpression="vItemNo" />
                <asp:BoundField DataField="vItemName" HeaderText="Name" SortExpression="vItemName" />
                <asp:BoundField DataField="vShrink" HeaderText="Production Shrink%" SortExpression="vShrink" />
                <asp:BoundField DataField="vSqInch" Visible="false" HeaderText="Sqinch" SortExpression="vSqInch" />
            </Columns>
        </asp:GridView>
        
        <input type="button" name="close" class="buttonM" id="close" value="Close" onclick="javascript:window.close()" />
        
        <asp:ObjectDataSource ID="ObjectDataSource1" runat="server" OldValuesParameterFormatString="original_{0}"
            SelectMethod="BOMItemLookup" TypeName="Corrugated">
            <SelectParameters>
                <asp:Parameter Name="prmAction" Type="String" />
                <asp:Parameter Name="prmUser" Type="String" />
                <asp:Parameter Name="prmField" Type="String" />
                <asp:Parameter Name="prmCondition" Type="String" />
                <asp:Parameter Name="prmText" Type="String" />
                <asp:Parameter Name="prmComp" Type="String" />
                <asp:QueryStringParameter QueryStringField="type" Name="prmMatType" Type="String" />
                <asp:QueryStringParameter QueryStringField="item" Name="prmItem" Type="String" />
                <asp:SessionParameter SessionField="order_corrugated_est" Name="prmEstimate" Type="String" />
                <asp:SessionParameter SessionField="order_corrugated_formno" Name="prmForm" Type="String" />
            </SelectParameters>
        </asp:ObjectDataSource>
    </div>
    </form>
</body>
</html>
