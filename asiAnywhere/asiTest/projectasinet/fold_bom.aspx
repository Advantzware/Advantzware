<%@ Page Language="C#" AutoEventWireup="true" Inherits="fold_bom" Codebehind="fold_bom.aspx.cs" %>

<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">

<html xmlns="http://www.w3.org/1999/xhtml" >
<head runat="server">
    <title>Item Bill of Materials</title>
     <LINK href="include/style.css" type="text/css" rel="stylesheet"/>
     <script>
         window.onload = setfocus;
         function setfocus() {
             document.forms[0].FormView1_vMediumTextBox.focus();
         }
        
        function mediumlook()
        {
            var code = "1";
            var item = "P";
            var NewWindow = window.open("corr_bom_laminate_lookup1.aspx?code="+code+"&item="+item+"","PaperLookupWindow","width=520,height=550,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
        }
        function ItemLaminateLookup1(ReturnObj1, ReturnObj2, ReturnObj3)
        { 
            if(ReturnObj1.indexOf(":"))
            {
                var val=ReturnObj1;    
                ReturnObj1=val.replace(":", "\"");    
            }

            document.forms[0].FormView1_vMediumTextBox.value = ReturnObj1;
            document.forms[0].FormView1_vMediumTextBox.focus();            
        }
        function linerlook()
        {
            var code = "1";
            var item = "P";
            var NewWindow = window.open("corr_bom_laminate_lookup2.aspx?code="+code+"&item="+item+"","PaperLookupWindow","width=520,height=550,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
        }
        function ItemLaminateLookup2(ReturnObj1, ReturnObj2, ReturnObj3)
        { 
            if(ReturnObj1.indexOf(":"))
            {
                var val=ReturnObj1;    
                ReturnObj1=val.replace(":", "\"");    
            }
            document.forms[0].FormView1_vLinerTextBox.value = ReturnObj1;
            document.forms[0].FormView1_vLinerTextBox.focus();            
        }
        
        function laminatelook()
        {
            var code = "1";
            var item = "L";
            var NewWindow = window.open("corr_bom_laminate_lookup.aspx?code="+code+"&item="+item+"","PaperLookupWindow","width=520,height=550,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
        }
        function ItemLaminateLookup(ReturnObj1, ReturnObj2, ReturnObj3)
        { 
            if(ReturnObj1.indexOf(":"))
            {
                var val=ReturnObj1;    
                ReturnObj1=val.replace(":", "\"");    
            }
            document.forms[0].FormView1_vLamCodeTextBox.value = ReturnObj1;
            document.forms[0].FormView1_vLamCodeTextBox.focus();           
        }
        function adhesivelook()
        {
            var indus = "1";
            var type = "G,T";
            var item = document.getElementById("FormView1_vAdhesiveTextBox").value;
            var NewWindow = window.open("corr_bom_adhesive_lookup.aspx?indus="+indus+"&type="+type+"&item="+item+"","PaperLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
        }
        function AdhesiveLookup1(ReturnObj1, ReturnObj2)
        {
            document.forms[0].FormView1_vAdhesiveTextBox.value = ReturnObj1;
            document.forms[0].FormView1_vAdhesiveTextBox.focus();            
        }
     </script>
</head>
<body>
    <form id="form1" runat="server">
    <div>
        <asp:FormView ID="FormView1" runat="server" DataSourceID="ObjectDataSource1" OnDataBound="FromView1_DataBound">
            <EditItemTemplate>
                <fieldset class="shade">
                    <table>
                        <tr>
                            <td nowrap align="right" style="padding-right:5px;">Medium:</td>
                            <td nowrap>
                                <asp:TextBox ID="vMediumTextBox" runat="server" Text='<%# Bind("vMedium") %>'></asp:TextBox>
                                <a href="#" tabindex="1" onclick="mediumlook(); return false"><asp:Image ID="img_board" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
                            </td>
                            <td width="30px">&nbsp;</td>
                            <td nowrap align="right" style="padding-right:5px;" rowspan="4">Flute Size:</td>
                            <td nowrap rowspan="4">
                                <asp:TextBox ID="vFluteTextBox" Visible="false" runat="server" Text='<%# Bind("vFlute") %>'></asp:TextBox>
                                <asp:RadioButtonList ID="RadioButtonList1" runat="server">
                                    <asp:ListItem Value="125">B</asp:ListItem>
                                    <asp:ListItem Value="78">E</asp:ListItem>
                                    <asp:ListItem Value="52">F</asp:ListItem>
                                    <asp:ListItem Value="0">None</asp:ListItem>
                                </asp:RadioButtonList>
                            </td>
                        </tr>
                        <tr>
                            <td nowrap align="right" style="padding-right:5px;">Liner:</td>
                            <td nowrap>
                                <asp:TextBox ID="vLinerTextBox" runat="server" Text='<%# Bind("vLiner") %>'></asp:TextBox>
                                <a href="#" tabindex="1" onclick="linerlook(); return false"><asp:Image ID="Image1" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
                            </td>
                        </tr>
                        <tr>
                            <td nowrap align="right" style="padding-right:5px;">Laminate Code:</td>
                            <td nowrap>
                                <asp:TextBox ID="vLamCodeTextBox" runat="server" Text='<%# Bind("vLamCode") %>'></asp:TextBox>
                                <a href="#" tabindex="1" onclick="laminatelook(); return false"><asp:Image ID="Image2" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
                            </td>
                        </tr>
                        <tr>
                            <td nowrap align="right" style="padding-right:5px;">Adhesive Code:</td>
                            <td nowrap>
                                <asp:TextBox ID="vAdhesiveTextBox" runat="server" Text='<%# Bind("vAdhesive") %>'></asp:TextBox>
                                <a href="#" tabindex="1" onclick="adhesivelook(); return false"><asp:Image ID="Image3" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
                            </td>
                        </tr>
                        <tr>
                            <td nowrap align="right" style="padding-right:5px;">Sq. Inches:</td>
                            <td nowrap>
                                <asp:Label ID="vSqInchTextBox" runat="server" Text='<%# Bind("vSqInch","{0:##0.0}") %>'> </asp:Label>
                            </td>
                        </tr>
                        
                    </table>
                </fieldset>
                <br />
                <asp:Button ID="UpdateButton" CssClass="buttonM" runat="server" CausesValidation="True" 
                    Text="Save" OnClick="UpdateButton_Click">
                </asp:Button>
                <input type="button" id="btn_cancel" value="Cancel" class="buttonM" onclick="self.close();" />
                
            </EditItemTemplate>
            <%--<InsertItemTemplate>
                vMedium:
                <asp:TextBox ID="vMediumTextBox" runat="server" Text='<%# Bind("vMedium") %>'>
                </asp:TextBox><br />
                vLiner:
                <asp:TextBox ID="vLinerTextBox" runat="server" Text='<%# Bind("vLiner") %>'>
                </asp:TextBox><br />
                vLamCode:
                <asp:TextBox ID="vLamCodeTextBox" runat="server" Text='<%# Bind("vLamCode") %>'>
                </asp:TextBox><br />
                vAdhesive:
                <asp:TextBox ID="vAdhesiveTextBox" runat="server" Text='<%# Bind("vAdhesive") %>'>
                </asp:TextBox><br />
                vFlute:
                <asp:TextBox ID="vFluteTextBox" runat="server" Text='<%# Bind("vFlute") %>'>
                </asp:TextBox><br />
                vSqInch:
                <asp:TextBox ID="vSqInchTextBox" runat="server" Text='<%# Bind("vSqInch") %>'>
                </asp:TextBox><br />
                <asp:LinkButton ID="InsertButton" runat="server" CausesValidation="True" CommandName="Insert"
                    Text="Insert">
                </asp:LinkButton>
                <asp:LinkButton ID="InsertCancelButton" runat="server" CausesValidation="False" CommandName="Cancel"
                    Text="Cancel">
                </asp:LinkButton>
            </InsertItemTemplate>
            <ItemTemplate>
                vMedium:
                <asp:Label ID="vMediumLabel" runat="server" Text='<%# Bind("vMedium") %>'></asp:Label><br />
                vLiner:
                <asp:Label ID="vLinerLabel" runat="server" Text='<%# Bind("vLiner") %>'></asp:Label><br />
                vLamCode:
                <asp:Label ID="vLamCodeLabel" runat="server" Text='<%# Bind("vLamCode") %>'></asp:Label><br />
                vAdhesive:
                <asp:Label ID="vAdhesiveLabel" runat="server" Text='<%# Bind("vAdhesive") %>'></asp:Label><br />
                vFlute:
                <asp:Label ID="vFluteLabel" runat="server" Text='<%# Bind("vFlute") %>'></asp:Label><br />
                vSqInch:
                <asp:Label ID="vSqInchLabel" runat="server" Text='<%# Bind("vSqInch") %>'></asp:Label><br />
            </ItemTemplate>--%>
        </asp:FormView>
        <asp:ObjectDataSource ID="ObjectDataSource1" runat="server" OldValuesParameterFormatString="original_{0}"
            SelectMethod="SelectFoldBom" TypeName="Corrugated">
            <SelectParameters>
                <asp:Parameter Name="prmUser" Type="String" />
                <asp:Parameter DefaultValue="Select" Name="prmAction" Type="String" />
                <asp:Parameter Name="prmComp" Type="String" />
                <asp:SessionParameter Name="prmEstimate" SessionField="order_folding_est" Type="String" />
                <asp:SessionParameter Name="prmForm" SessionField="order_folding_formno" Type="Int32" />
                <asp:Parameter Name="prmMedium" Type="String" />
                <asp:Parameter Name="prmLiner" Type="String" />
                <asp:Parameter Name="prmLamCode" Type="String" />
                <asp:Parameter Name="prmAdhesive" Type="String" />
                <asp:Parameter Name="prmFlute" Type="String" />
                <asp:Parameter Name="prmSqInch" Type="Decimal" />
            </SelectParameters>
        </asp:ObjectDataSource>
    </div>
    </form>
</body>
</html>
