<%@ Page Language="C#" AutoEventWireup="true" Inherits="corr_bom_adhesive_lookup" Codebehind="corr_bom_adhesive_lookup.aspx.cs" %>

<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">

<html xmlns="http://www.w3.org/1999/xhtml" >
<head runat="server">
    <title>Paper Information</title>
    <LINK href="include/style.css" type="text/css" rel="stylesheet"/>
</head>
<body>
    <form id="form1" runat="server" DefaultButton="Button1">
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
        <asp:GridView ID="GridView1"  AllowPaging="True" runat="server" AllowSorting="True" AutoGenerateColumns="False" DataSourceID="ObjectDataSource1"
            Style="position: static" EmptyDataText="No Records Found" Width="100%" BorderStyle="Dotted" CssClass="Grid">
            <SelectedRowStyle CssClass="GridSelected" />
            <AlternatingRowStyle CssClass="GridItemOdd" />
            <EmptyDataRowStyle BorderStyle="Dotted" BorderColor="Gray" BorderWidth="0px" Font-Bold="True" HorizontalAlign="Center" VerticalAlign="Middle" />
            <RowStyle CssClass="shade"  />   
            <HeaderStyle  HorizontalAlign="Center" VerticalAlign="Middle" Wrap="False" BackColor="Teal" ForeColor="White" />
            <Columns>
                <asp:TemplateField>
                  <ItemStyle HorizontalAlign=Center Wrap="False" />
                   <ItemTemplate>       
		                <a href="#" onClick="javascript:top.opener.window.AdhesiveLookup1('<%#DataBinder.Eval(Container,"DataItem.vItemNo")%>','<%#DataBinder.Eval(Container,"DataItem.vItemName")%>');window.close();">Select</a>             	    
                   </ItemTemplate>
                </asp:TemplateField>
                <asp:BoundField DataField="vItemNo" HeaderText="Item No" SortExpression="vItemNo" />
                <asp:BoundField DataField="vItemName" HeaderText="Name" SortExpression="vItemName" />
                <asp:BoundField DataField="vER" HeaderText="E/R" SortExpression="vER" />
                <asp:BoundField DataField="vCal" HeaderText="Caliper" HtmlEncode="false" DataFormatString="{0:0.####0}" SortExpression="vCal" />
                <asp:BoundField DataField="vWid" HeaderText="Width" HtmlEncode="false" DataFormatString="{0:###,##0.0}" SortExpression="vWid" />
                <asp:BoundField DataField="vLen" HeaderText="Length" HtmlEncode="false" DataFormatString="{0:###,##0.0}" SortExpression="vLen" />
                <asp:BoundField DataField="vQOH" HeaderText="Qty on Hand" HtmlEncode="false" DataFormatString="{0:###,###,##0.##0}" SortExpression="vQOH" />
                <asp:BoundField DataField="vComit" HeaderText="Committed" HtmlEncode="false" DataFormatString="{0:###,##0.0}" SortExpression="vComit" />
                <asp:BoundField DataField="vAvail" HeaderText="Available" HtmlEncode="false" DataFormatString="{0:###,###,##0.##0}" SortExpression="vAvail" />
                
            </Columns>
        </asp:GridView>
        
        <input type="button" name="close" class="buttonM" id="close" value="Close" onclick="javascript:window.close()" />
        
        <asp:ObjectDataSource ID="ObjectDataSource1" runat="server" OldValuesParameterFormatString="original_{0}"
            SelectMethod="BOMAdhesiveLookup" TypeName="Corrugated">
            <SelectParameters>
                <asp:Parameter Name="prmAction" Type="String" />
                <asp:Parameter Name="prmUser" Type="String" />
                <asp:Parameter Name="prmField" Type="String" />
                <asp:Parameter Name="prmCondition" Type="String" />
                <asp:Parameter Name="prmText" Type="String" />
                <asp:Parameter Name="prmComp" Type="String" />
                <asp:QueryStringParameter QueryStringField="indus" Name="prmIndustry" Type="String" />
                <asp:QueryStringParameter QueryStringField="type" Name="prmMatType" Type="String" />
                <asp:QueryStringParameter QueryStringField="item" Name="prmItem" Type="String" />
                
                <asp:SessionParameter SessionField="order_corrugated_est" Name="prmEstimate" Type="String" />
                <asp:SessionParameter Name="prmForm" SessionField="order_corrugated_formno" Type="Int32" />
            </SelectParameters>
        </asp:ObjectDataSource>
    </div>
    </form>
</body>
</html>
