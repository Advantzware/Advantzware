<%@ Page Language="C#" AutoEventWireup="true" Inherits="CodeLook" Title="Raw Material Information(Window)" Codebehind="CodeLook.aspx.cs" %>

<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">

<html xmlns="http://www.w3.org/1999/xhtml" >
<head id="Head1" runat="server">
    <title>Raw Material Information</title>
    <LINK href="include/style.css" type="text/css" rel="stylesheet"/>
<link rel="stylesheet" href="include/dhtmlwindow.css" type="text/css" />
</head>
<script type="text/javascript" src="include/dhtmlwindow.js"></script>
<body>
    <form id="form1" runat="server" defaultfocus="txtSearchValue" DefaultButton="Button1">
    
    <div>
    <table style="display:none;"><tr><td>
        <asp:TextBox runat="server" id="Text1"  />
        <asp:TextBox runat="server" id="Text2" />
        <asp:TextBox runat="server" id="Text3"  />
        <asp:TextBox runat="server" id="Text4" />
        <asp:TextBox runat="server" id="Text5"  />
        <asp:TextBox runat="server" id="Text6" />
        <asp:TextBox runat="server" id="Text7"  />
        <asp:TextBox runat="server" id="Text8" />
        <asp:TextBox runat="server" id="Text9"  />
        <asp:TextBox runat="server" id="Text10" />
        
        <asp:TextBox runat="server" id="Text11"  />
        <asp:TextBox runat="server" id="Text12" />
        <asp:TextBox runat="server" id="Text13"  />
        <asp:TextBox runat="server" id="Text14" />
        <asp:TextBox runat="server" id="Text15"  />
        <asp:TextBox runat="server" id="Text16" />
        <asp:TextBox runat="server" id="Text17"  />
        <asp:TextBox runat="server" id="Text18" />
        <asp:TextBox runat="server" id="Text19"  />
        <asp:TextBox runat="server" id="Text20" />
        
        <asp:TextBox runat="server" id="Text21"  />
        <asp:TextBox runat="server" id="Text22" />
        <asp:TextBox runat="server" id="Text23"  />
        <asp:TextBox runat="server" id="Text24" />
        <asp:TextBox runat="server" id="Text25"  />
        <asp:TextBox runat="server" id="Text26" />
        <asp:TextBox runat="server" id="Text27"  />
        <asp:TextBox runat="server" id="Text28" />
        <asp:TextBox runat="server" id="Text29"  />
        <asp:TextBox runat="server" id="Text30" />
        </td></tr></table>
        
        
    </div>
        
    
    <div>
  <table id="tblSearch" cellSpacing="1" cellPadding="5" width="100%" border="0"  bgcolor=black>
  <tr>
  <td class="shade" >
  <asp:button id="Button1" runat="server" Width="50px" CssClass="button" Text="Go" OnClick="btnSearch_Click"></asp:button>
                  <asp:button id="Button2" runat="server" Width="50px" CssClass="button" Text="All" OnClick="btnShowAll_Click" ></asp:button>&nbsp;</td>
  <td id="tdSearch" runat="server" class="shade" vAlign="middle" align="center" width="800">&nbsp;                
                <B>Search for:&nbsp; </B>&nbsp;&nbsp;&nbsp;
                  <asp:dropdownlist id="ddlSearchField" runat="server">                  
                      <asp:ListItem Value="i-no">Item#</asp:ListItem>
                      <asp:ListItem Value="i-name">Item Name</asp:ListItem>                      
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
    
    <div>
        &nbsp;&nbsp;
        <asp:GridView ID="GridView1" runat="server" AutoGenerateColumns="False" OnSelectedIndexChanged="GridView1_SelectedIndexChanged"
            DataSourceID="ObjectDataSource1" AllowPaging="True" AllowSorting="True"  
            EmptyDataText="No Records Found" Width="100%" BorderStyle="Dotted" CssClass="Grid">
            <SelectedRowStyle CssClass="GridSelected" />
            <AlternatingRowStyle CssClass="GridItemOdd" />
            <EmptyDataRowStyle BorderStyle="Dotted" BorderColor="Gray" BorderWidth="0px" Font-Bold="True" HorizontalAlign="Center" VerticalAlign="Middle" />
            <RowStyle CssClass="shade"  />   
            <HeaderStyle CssClass="gridrowhdr" HorizontalAlign="Center" VerticalAlign="Middle" Wrap="False" BackColor="Gray" ForeColor="White" />
            <Columns>
            <asp:TemplateField>
                  <ItemStyle HorizontalAlign=Center />
                   <ItemTemplate>       
		    <%--<a href="#" onClick="javascript:top.opener.window.CodeLookUp('<%#DataBinder.Eval(Container,"DataItem.Prinno")%>','<%#DataBinder.Eval(Container,"DataItem.PrintName")%>');window.close();">Select</a>  --%> 
		    <asp:CheckBox ID="chkSelect" runat="server" />          	    
                   </ItemTemplate>
                </asp:TemplateField>
                <asp:BoundField DataField="Prinno" HeaderText="Item#" SortExpression="Prinno" />
                <asp:BoundField DataField="PrintName" HeaderText="Name" SortExpression="PrintName" />
                <asp:BoundField DataField="InkType" HeaderText="Type" SortExpression="InkType" />
               
                
                
                
            </Columns>
        </asp:GridView>
        <asp:ObjectDataSource ID="ObjectDataSource1" runat="server" OldValuesParameterFormatString="original_{0}"
         SelectMethod="PrintLook" TypeName="LookUp">
            <SelectParameters>
            <asp:Parameter Name="prmUser" Type="String" />
                    
                <asp:Parameter DefaultValue="" Name="prmAction" Type="String" />
                <asp:Parameter Name="prmField" Type="String" />
                <asp:Parameter Name="prmCondition" Type="String" />
                <asp:Parameter Name="prmText" Type="String" />
                
                
                 
            </SelectParameters>
        </asp:ObjectDataSource>
        
        <asp:Button ID="select" runat="server" CssClass="buttonM" OnClick="select_click" Text="Select" />
            <asp:Button ID="Button3" runat="server" CssClass="buttonM" Text="Select"  />
    </div>
    </form>
</body>
</html>
