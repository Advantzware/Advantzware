<%@ Page Language="C#" AutoEventWireup="true" Inherits="corr_bom" Codebehind="corr_bom.aspx.cs" %>

<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">

<html xmlns="http://www.w3.org/1999/xhtml" >
<head runat="server">
    <title>Item Bill of Materials</title>
     <LINK href="include/style.css" type="text/css" rel="stylesheet"/>
     <script>
     window.onload = setfocus;
     function setfocus() {
         document.forms[0].FormView1_vBomItem1TextBox.focus();
     }
         
        function validshrink(obj)
        {
            if(obj.value.indexOf(".") != -1)
               return;
             
            if(obj.value.length > 2 && obj.value.length < 4)                
                obj.value = obj.value + ".";           
        }
        
        function itemlook1()
        {
            var mattype = "P";
            var item = document.getElementById("FormView1_vBomItem1TextBox").value;            
            var NewWindow = window.open("corr_bom_item_lookup.aspx?type="+mattype+"&item="+item+"","PaperLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
        }
        function ItemLookup1(ReturnObj1, ReturnObj2, ReturnObj3, ReturnObj4)
        { 
            if(ReturnObj1.indexOf(":"))
            {
                var val=ReturnObj1;    
                ReturnObj1=val.replace(":", "\"");    
            }
            if(ReturnObj2.indexOf(":"))
            {
                var val2=ReturnObj2;    
                ReturnObj2=val2.replace(":", "\"");    
            }

            document.forms[0].FormView1_vBomItem1TextBox.value = ReturnObj1;
            var desc = document.getElementById("FormView1_vBomItemDscr1TextBox");
            desc.innerText = ReturnObj2;
            document.forms[0].FormView1_vShrink1TextBox.value = ReturnObj3;
            var sqinch = document.getElementById("FormView1_vSqInch1TextBox");
            sqinch.innerText = ReturnObj4;
            document.forms[0].FormView1_vShrink1TextBox.focus();
        }
        function itemlook2()
        {
            var mattype = "P";
            var item = document.getElementById("FormView1_vBomItem2TextBox").value;            
            var NewWindow = window.open("corr_bom_item_lookup2.aspx?type="+mattype+"&item="+item+"","PaperLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
        }
        function ItemLookup2(ReturnObj1, ReturnObj2, ReturnObj3, ReturnObj4)
        { 
            if(ReturnObj1.indexOf(":"))
            {
                var val=ReturnObj1;    
                ReturnObj1=val.replace(":", "\"");    
            }
            if(ReturnObj2.indexOf(":"))
            {
                var val2=ReturnObj2;    
                ReturnObj2=val2.replace(":", "\"");    
            }
            document.forms[0].FormView1_vBomItem2TextBox.value = ReturnObj1;
            var desc = document.getElementById("FormView1_vBomItemDscr2TextBox");
            desc.innerText = ReturnObj2;
            document.forms[0].FormView1_vShrink2TextBox.value = ReturnObj3;
            var sqinch = document.getElementById("FormView1_vSqInch2TextBox");
            sqinch.innerText = ReturnObj4;
            document.forms[0].FormView1_vShrink2TextBox.focus();
        }
        function itemlook3()
        {
            var mattype = "P";
            var item = document.getElementById("FormView1_vBomItem3TextBox").value;            
            var NewWindow = window.open("corr_bom_item_lookup3.aspx?type="+mattype+"&item="+item+"","PaperLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
        }
        function ItemLookup3(ReturnObj1, ReturnObj2, ReturnObj3, ReturnObj4)
        { 
            if(ReturnObj1.indexOf(":"))
            {
                var val=ReturnObj1;    
                ReturnObj1=val.replace(":", "\"");    
            }
            if(ReturnObj2.indexOf(":"))
            {
                var val2=ReturnObj2;    
                ReturnObj2=val2.replace(":", "\"");    
            }
            document.forms[0].FormView1_vBomItem3TextBox.value = ReturnObj1;
            var desc = document.getElementById("FormView1_vBomItemDscr3TextBox");
            desc.innerText = ReturnObj2;
            document.forms[0].FormView1_vShrink3TextBox.value = ReturnObj3;
            var sqinch = document.getElementById("FormView1_vSqInch3TextBox");
            sqinch.innerText = ReturnObj4;
            document.forms[0].FormView1_vShrink3TextBox.focus();
        }
        function itemlook4()
        {
            var mattype = "P";
            var item = document.getElementById("FormView1_vBomItem4TextBox").value;            
            var NewWindow = window.open("corr_bom_item_lookup4.aspx?type="+mattype+"&item="+item+"","PaperLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
        }
        function ItemLookup4(ReturnObj1, ReturnObj2, ReturnObj3, ReturnObj4)
        { 
            if(ReturnObj1.indexOf(":"))
            {
                var val=ReturnObj1;    
                ReturnObj1=val.replace(":", "\"");    
            }
            if(ReturnObj2.indexOf(":"))
            {
                var val2=ReturnObj2;    
                ReturnObj2=val2.replace(":", "\"");    
            }
            document.forms[0].FormView1_vBomItem4TextBox.value = ReturnObj1;
            var desc = document.getElementById("FormView1_vBomItemDscr4TextBox");
            desc.innerText = ReturnObj2;
            document.forms[0].FormView1_vShrink4TextBox.value = ReturnObj3;
            var sqinch = document.getElementById("FormView1_vSqInch4TextBox");
            sqinch.innerText = ReturnObj4;
            document.forms[0].FormView1_vShrink4TextBox.focus();
        }
        function itemlook5()
        {
            var mattype = "P";
            var item = document.getElementById("FormView1_vBomItem5TextBox").value;            
            var NewWindow = window.open("corr_bom_item_lookup5.aspx?type="+mattype+"&item="+item+"","PaperLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
        }
        function ItemLookup5(ReturnObj1, ReturnObj2, ReturnObj3, ReturnObj4)
        { 
            if(ReturnObj1.indexOf(":"))
            {
                var val=ReturnObj1;    
                ReturnObj1=val.replace(":", "\"");    
            }
            if(ReturnObj2.indexOf(":"))
            {
                var val2=ReturnObj2;    
                ReturnObj2=val2.replace(":", "\"");    
            }
            document.forms[0].FormView1_vBomItem5TextBox.value = ReturnObj1;
            var desc = document.getElementById("FormView1_vBomItemDscr5TextBox");
            desc.innerText = ReturnObj2;
            document.forms[0].FormView1_vShrink5TextBox.value = ReturnObj3;
            var sqinch = document.getElementById("FormView1_vSqInch5TextBox");
            sqinch.innerText = ReturnObj4;
            document.forms[0].FormView1_vShrink5TextBox.focus();
        }
        function itemlook6()
        {
            var mattype = "P";
            var item = document.getElementById("FormView1_vBomItem6TextBox").value;            
            var NewWindow = window.open("corr_bom_item_lookup6.aspx?type="+mattype+"&item="+item+"","PaperLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
        }
        function ItemLookup6(ReturnObj1, ReturnObj2, ReturnObj3, ReturnObj4)
        { 
            if(ReturnObj1.indexOf(":"))
            {
                var val=ReturnObj1;    
                ReturnObj1=val.replace(":", "\"");    
            }
            if(ReturnObj2.indexOf(":"))
            {
                var val2=ReturnObj2;    
                ReturnObj2=val2.replace(":", "\"");    
            }
            document.forms[0].FormView1_vBomItem6TextBox.value = ReturnObj1;
            var desc = document.getElementById("FormView1_vBomItemDscr6TextBox");
            desc.innerText = ReturnObj2;
            document.forms[0].FormView1_vShrink6TextBox.value = ReturnObj3;
            var sqinch = document.getElementById("FormView1_vSqInch6TextBox");
            sqinch.innerText = ReturnObj4;
            document.forms[0].FormView1_vShrink6TextBox.focus();
        }
        function itemlook7()
        {
            var mattype = "P";
            var item = document.getElementById("FormView1_vBomItem7TextBox").value;            
            var NewWindow = window.open("corr_bom_item_lookup7.aspx?type="+mattype+"&item="+item+"","PaperLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
        }
        function ItemLookup7(ReturnObj1, ReturnObj2, ReturnObj3, ReturnObj4)
        { 
            if(ReturnObj1.indexOf(":"))
            {
                var val=ReturnObj1;    
                ReturnObj1=val.replace(":", "\"");    
            }
            if(ReturnObj2.indexOf(":"))
            {
                var val2=ReturnObj2;    
                ReturnObj2=val2.replace(":", "\"");    
            }
            document.forms[0].FormView1_vBomItem7TextBox.value = ReturnObj1;
            var desc = document.getElementById("FormView1_vBomItemDscr7TextBox");
            desc.innerText = ReturnObj2;
            document.forms[0].FormView1_vShrink7TextBox.value = ReturnObj3;
            var sqinch = document.getElementById("FormView1_vSqInch7TextBox");
            sqinch.innerText = ReturnObj4;
            document.forms[0].FormView1_vShrink7TextBox.focus();
        }
        function itemlook8()
        {
            var mattype = "P";
            var item = document.getElementById("FormView1_vBomItem8TextBox").value;            
            var NewWindow = window.open("corr_bom_item_lookup8.aspx?type="+mattype+"&item="+item+"","PaperLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
        }
        function ItemLookup8(ReturnObj1, ReturnObj2, ReturnObj3, ReturnObj4)
        { 
            if(ReturnObj1.indexOf(":"))
            {
                var val=ReturnObj1;    
                ReturnObj1=val.replace(":", "\"");    
            }
            if(ReturnObj2.indexOf(":"))
            {
                var val2=ReturnObj2;    
                ReturnObj2=val2.replace(":", "\"");    
            }
            document.forms[0].FormView1_vBomItem8TextBox.value = ReturnObj1;
            var desc = document.getElementById("FormView1_vBomItemDscr8TextBox");
            desc.innerText = ReturnObj2;
            document.forms[0].FormView1_vShrink8TextBox.value = ReturnObj3;
            var sqinch = document.getElementById("FormView1_vSqInch8TextBox");
            sqinch.innerText = ReturnObj4;
            document.forms[0].FormView1_vShrink8TextBox.focus();
        }
        function laminatelook()
        {
            var code = "2";
            var item = "L";
            var NewWindow = window.open("corr_bom_laminate_lookup.aspx?code="+code+"&item="+item+"","PaperLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
        }
        function ItemLaminateLookup(ReturnObj1, ReturnObj2, ReturnObj3)
        { 
            if(ReturnObj1.indexOf(":"))
            {
                var val=ReturnObj1;    
                ReturnObj1=val.replace(":", "\"");    
            }
            if(ReturnObj2.indexOf(":"))
            {
                var val2=ReturnObj2;    
                ReturnObj2=val2.replace(":", "\"");    
            }
            document.forms[0].FormView1_vBomItem9TextBox.value = ReturnObj1;
            var desc = document.getElementById("FormView1_vBomItemDscr9TextBox");
            desc.innerText = ReturnObj2;
            var sqinch = document.getElementById("FormView1_vSqInch9TextBox");
            sqinch.innerText = ReturnObj3;
            document.forms[0].FormView1_vBomItem9TextBox.focus();
        }
        function adhesivelook()
        {
            var indus = "2";
            var type = "G,T";
            var item = document.getElementById("FormView1_vBomItem10TextBox").value;
            var NewWindow = window.open("corr_bom_adhesive_lookup.aspx?indus="+indus+"&type="+type+"&item="+item+"","PaperLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
        }
        function AdhesiveLookup1(ReturnObj1, ReturnObj2)
        { 
            document.forms[0].FormView1_vBomItem10TextBox.value = ReturnObj1;
            var desc = document.getElementById("FormView1_vBomItemDscr10TextBox");
            desc.innerText = ReturnObj2;
            document.forms[0].FormView1_vBomItem10TextBox.focus();
            
        }
     </script>
</head>
<body>
    <form id="form1" runat="server">
    <div>
        <asp:FormView ID="FormView1" runat="server" DataSourceID="ObjectDataSource1">
            <EditItemTemplate>
                <fieldset class="shade">
                    <table>
                        <tr>
                            <td nowrap></td>
                            <td nowrap></td>
                            <td nowrap>Item Description</td>
                            <td nowrap width="80px"></td>
                            <td nowrap>Shrink %</td>
                            <td nowrap>Sq. Inches</td>
                        </tr>
                        <tr>
                            <td nowrap align="right" style="padding-right:5px;">Paper 1:</td>
                            <td nowrap>
                                <asp:TextBox ID="vBomItem1TextBox" runat="server" Text='<%# Bind("vBomItem1") %>'></asp:TextBox>
                                <a href="#" tabindex="1" onclick="itemlook1(); return false"><asp:Image ID="Image11" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
                            </td>
                            <td nowrap>
                                <asp:Label ID="vBomItemDscr1TextBox" Width="120px"  BackColor="Turquoise" runat="server" Text='<%# Eval("vBomItemDscr1") %>'></asp:Label>
                            </td>
                            <td nowrap></td>
                            <td nowrap>
                                <asp:TextBox ID="vShrink1TextBox" Width="80px" runat="server" onkeyup="validshrink(this)" MaxLength="8" Text='<%# Bind("vShrink1","{0:##0.###0}") %>'></asp:TextBox>
                                <asp:CompareValidator ID="CompareValidator1" runat="server" ControlToValidate="vShrink1TextBox" Display="dynamic" SetFocusOnError="true" Operator="dataTypeCheck" Type="Double" ErrorMessage="Only decimal value."></asp:CompareValidator>
                            </td>
                            <td nowrap>
                                <asp:Label ID="vSqInch1TextBox" BackColor="Turquoise" Width="60px" runat="server" Text='<%# Eval("vSqInch1","{0:0.#0}") %>'></asp:Label>
                            </td>
                        </tr>
                         <tr>
                            <td nowrap align="right" style="padding-right:5px;">Paper 2:</td>
                            <td nowrap>
                                <asp:TextBox ID="vBomItem2TextBox" runat="server" Text='<%# Bind("vBomItem2") %>'></asp:TextBox>
                                <a href="#" tabindex="1" onclick="itemlook2(); return false"><asp:Image ID="Image1" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
                            </td>
                            <td nowrap>
                                <asp:Label ID="vBomItemDscr2TextBox" Width="120px" BackColor="Turquoise" runat="server" Text='<%# Eval("vBomItemDscr2") %>'></asp:Label>
                            </td>
                            <td nowrap></td>
                            <td nowrap>
                                <asp:TextBox ID="vShrink2TextBox" Width="80px" MaxLength="8" onkeyup="validshrink(this)" runat="server" Text='<%# Bind("vShrink2","{0:##0.###0}") %>'></asp:TextBox>
                                <asp:CompareValidator ID="CompareValidator2" runat="server" ControlToValidate="vShrink2TextBox" Display="dynamic" SetFocusOnError="true" Operator="dataTypeCheck" Type="Double" ErrorMessage="Only decimal value."></asp:CompareValidator>
                            </td>
                            <td nowrap>
                                <asp:Label ID="vSqInch2TextBox" BackColor="Turquoise" Width="60px" runat="server" Text='<%# Eval("vSqInch2","{0:0.#0}") %>'></asp:Label>
                            </td>
                        </tr>
                         <tr>
                            <td nowrap align="right" style="padding-right:5px;">Paper 3:</td>
                            <td nowrap>
                                <asp:TextBox ID="vBomItem3TextBox" runat="server" Text='<%# Bind("vBomItem3") %>'></asp:TextBox>
                                <a href="#" tabindex="1" onclick="itemlook3(); return false"><asp:Image ID="Image2" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
                            </td>
                            <td nowrap>
                                <asp:Label ID="vBomItemDscr3TextBox" Width="120px" BackColor="Turquoise" runat="server" Text='<%# Eval("vBomItemDscr3") %>'></asp:Label>
                            </td>
                            <td nowrap></td>
                            <td nowrap>
                                <asp:TextBox ID="vShrink3TextBox" Width="80px" MaxLength="8" onkeyup="validshrink(this)" runat="server" Text='<%# Bind("vShrink3","{0:##0.###0}") %>'></asp:TextBox>
                                <asp:CompareValidator ID="CompareValidator3" runat="server" ControlToValidate="vShrink3TextBox" Display="dynamic" SetFocusOnError="true" Operator="dataTypeCheck" Type="Double" ErrorMessage="Only decimal value."></asp:CompareValidator>
                            </td>
                            <td nowrap>
                                <asp:Label ID="vSqInch3TextBox" BackColor="Turquoise" Width="60px" runat="server" Text='<%# Eval("vSqInch3","{0:0.#0}") %>'></asp:Label>
                            </td>
                        </tr>
                         <tr>
                            <td nowrap align="right" style="padding-right:5px;">Paper 4:</td>
                            <td nowrap>
                                <asp:TextBox ID="vBomItem4TextBox" runat="server" Text='<%# Bind("vBomItem4") %>'></asp:TextBox>
                                <a href="#" tabindex="1" onclick="itemlook4(); return false"><asp:Image ID="Image3" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
                            </td>
                            <td nowrap>
                                <asp:Label ID="vBomItemDscr4TextBox" Width="120px" BackColor="Turquoise" runat="server" Text='<%# Eval("vBomItemDscr4") %>'></asp:Label>
                            </td>
                            <td nowrap></td>
                            <td nowrap>
                                <asp:TextBox ID="vShrink4TextBox" Width="80px" MaxLength="8" onkeyup="validshrink(this)" runat="server" Text='<%# Bind("vShrink4","{0:##0.###0}") %>'></asp:TextBox>
                                <asp:CompareValidator ID="CompareValidator4" runat="server" ControlToValidate="vShrink4TextBox" Display="dynamic" SetFocusOnError="true" Operator="dataTypeCheck" Type="Double" ErrorMessage="Only decimal value."></asp:CompareValidator>
                            </td>
                            <td nowrap>
                                <asp:Label ID="vSqInch4TextBox" BackColor="Turquoise" Width="60px" runat="server" Text='<%# Eval("vSqInch4","{0:0.#0}") %>'></asp:Label>
                            </td>
                        </tr>
                         <tr>
                            <td nowrap align="right" style="padding-right:5px;">Paper 5:</td>
                            <td nowrap>
                                <asp:TextBox ID="vBomItem5TextBox" runat="server" Text='<%# Bind("vBomItem5") %>'></asp:TextBox>
                                <a href="#" tabindex="1" onclick="itemlook5(); return false"><asp:Image ID="Image4" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
                            </td>
                            <td nowrap>
                                <asp:Label ID="vBomItemDscr5TextBox" Width="120px" BackColor="Turquoise" runat="server" Text='<%# Eval("vBomItemDscr5") %>'></asp:Label>
                            </td>
                            <td nowrap></td>
                            <td nowrap>
                                <asp:TextBox ID="vShrink5TextBox" Width="80px" MaxLength="8" onkeyup="validshrink(this)" runat="server" Text='<%# Bind("vShrink5","{0:##0.###0}") %>'></asp:TextBox>
                                <asp:CompareValidator ID="CompareValidator5" runat="server" ControlToValidate="vShrink5TextBox" Display="dynamic" SetFocusOnError="true" Operator="dataTypeCheck" Type="Double" ErrorMessage="Only decimal value."></asp:CompareValidator>
                            </td>
                            <td nowrap>
                                <asp:Label ID="vSqInch5TextBox" BackColor="Turquoise" Width="60px" runat="server" Text='<%# Eval("vSqInch5","{0:0.#0}") %>'></asp:Label>
                            </td>
                        </tr>
                         <tr>
                            <td nowrap align="right" style="padding-right:5px;">Paper 6:</td>
                            <td nowrap>
                                <asp:TextBox ID="vBomItem6TextBox" runat="server" Text='<%# Bind("vBomItem6") %>'></asp:TextBox>
                                <a href="#" tabindex="1" onclick="itemlook6(); return false"><asp:Image ID="Image5" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
                            </td>
                            <td nowrap>
                                <asp:Label ID="vBomItemDscr6TextBox" Width="120px" BackColor="Turquoise" runat="server" Text='<%# Eval("vBomItemDscr6") %>'></asp:Label>
                            </td>
                            <td nowrap></td>
                            <td nowrap>
                                <asp:TextBox ID="vShrink6TextBox" Width="80px" MaxLength="8" onkeyup="validshrink(this)" runat="server" Text='<%# Bind("vShrink6","{0:##0.###0}") %>'></asp:TextBox>
                                <asp:CompareValidator ID="CompareValidator6" runat="server" ControlToValidate="vShrink6TextBox" Display="dynamic" SetFocusOnError="true" Operator="dataTypeCheck" Type="Double" ErrorMessage="Only decimal value."></asp:CompareValidator>
                            </td>
                            <td nowrap>
                                <asp:Label ID="vSqInch6TextBox" BackColor="Turquoise" Width="60px" runat="server" Text='<%# Eval("vSqInch6","{0:0.#0}") %>'></asp:Label>
                            </td>
                        </tr>
                         <tr>
                            <td nowrap align="right" style="padding-right:5px;">Paper 7:</td>
                            <td nowrap>
                                <asp:TextBox ID="vBomItem7TextBox"  runat="server" Text='<%# Bind("vBomItem7") %>'></asp:TextBox>
                                <a href="#" tabindex="1" onclick="itemlook7(); return false"><asp:Image ID="Image6" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
                            </td>
                            <td nowrap>
                                <asp:Label ID="vBomItemDscr7TextBox" Width="120px" BackColor="Turquoise" runat="server" Text='<%# Eval("vBomItemDscr7") %>'></asp:Label>
                            </td>
                            <td nowrap></td>
                            <td nowrap>
                                <asp:TextBox ID="vShrink7TextBox" Width="80px" MaxLength="8" onkeyup="validshrink(this)" runat="server" Text='<%# Bind("vShrink7","{0:##0.###0}") %>'></asp:TextBox>
                                <asp:CompareValidator ID="CompareValidator7" runat="server" ControlToValidate="vShrink7TextBox" Display="dynamic" SetFocusOnError="true" Operator="dataTypeCheck" Type="Double" ErrorMessage="Only decimal value."></asp:CompareValidator>
                            </td>
                            <td nowrap>
                                <asp:Label ID="vSqInch7TextBox" BackColor="Turquoise" Width="60px" runat="server" Text='<%# Eval("vSqInch7","{0:0.#0}") %>'></asp:Label>
                            </td>
                        </tr>
                         <tr>
                            <td nowrap align="right" style="padding-right:5px;">Paper 8:</td>
                            <td nowrap>
                                <asp:TextBox ID="vBomItem8TextBox" runat="server" Text='<%# Bind("vBomItem8") %>'></asp:TextBox>
                                <a href="#" tabindex="1" onclick="itemlook8(); return false"><asp:Image ID="Image7" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
                            </td>
                            <td nowrap>
                                <asp:Label ID="vBomItemDscr8TextBox" Width="120px" BackColor="Turquoise" runat="server" Text='<%# Eval("vBomItemDscr8") %>'></asp:Label>
                            </td>
                            <td nowrap></td>
                            <td nowrap>
                                <asp:TextBox ID="vShrink8TextBox" Width="80px" MaxLength="8" onkeyup="validshrink(this)" runat="server" Text='<%# Bind("vShrink8","{0:##0.###0}") %>'></asp:TextBox>
                                <asp:CompareValidator ID="CompareValidator8" runat="server" ControlToValidate="vShrink8TextBox" Display="dynamic" SetFocusOnError="true" Operator="dataTypeCheck" Type="Double" ErrorMessage="Only decimal value."></asp:CompareValidator>
                            </td>
                            <td nowrap>
                                <asp:Label ID="vSqInch8TextBox" BackColor="Turquoise" Width="60px" runat="server" Text='<%# Eval("vSqInch8","{0:0.#0}") %>'></asp:Label>
                            </td>
                        </tr>
                         <tr>
                            <td nowrap align="right" style="padding-right:5px;">Laminate:</td>
                            <td nowrap>
                                <asp:TextBox ID="vBomItem9TextBox" runat="server" Text='<%# Bind("vLamCode") %>'></asp:TextBox>
                                <a href="#" tabindex="1" onclick="laminatelook(); return false"><asp:Image ID="Image8" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
                            </td>
                            <td nowrap>
                                <asp:Label ID="vBomItemDscr9TextBox" Width="120px" BackColor="Turquoise" runat="server" Text='<%# Eval("vBomItemDscr9") %>'></asp:Label>
                            </td>
                            <td nowrap></td>
                            <td nowrap>
                                
                            </td>
                            <td nowrap>
                                <asp:Label ID="vSqInch9TextBox" BackColor="Turquoise" Width="60px" runat="server" Text='<%# Eval("vSqInch9","{0:0.#0}") %>'></asp:Label>
                            </td>
                        </tr>
                         <tr>
                            <td nowrap align="right" style="padding-right:5px;">Adhesive:</td>
                            <td nowrap>
                                <asp:TextBox ID="vBomItem10TextBox" runat="server" Text='<%# Bind("vAdhesive") %>'></asp:TextBox>
                                <a href="#" tabindex="1" onclick="adhesivelook(); return false"><asp:Image ID="Image9" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
                            </td>
                            <td nowrap>
                                <asp:Label ID="vBomItemDscr10TextBox" Width="120px" BackColor="Turquoise" runat="server" Text='<%# Eval("vBomItemDscr10") %>'></asp:Label>
                            </td>
                            <td nowrap></td>
                            <td nowrap>
                                
                            </td>
                            <td nowrap>
                                <asp:Label ID="vSqInch10TextBox" BackColor="Turquoise" Width="60px" runat="server" Text='<%# Eval("vSqInch10","{0:0.#0}") %>'></asp:Label>
                            </td>
                        </tr>
                    </table>
                </fieldset>
                
                <br />
                <asp:Button ID="UpdateButton" runat="server" CssClass="buttonM" CausesValidation="True" OnClick="UpdateButton_Click"
                    Text="Save">
                </asp:Button>
                <input type="button" id="btn_cancel" class="buttonM" value="Cancel" onclick="window.close();" />
                
            </EditItemTemplate>
            <%--<InsertItemTemplate>
                vBomItem1:
                <asp:TextBox ID="vBomItem1TextBox" runat="server" Text='<%# Bind("vBomItem1") %>'>
                </asp:TextBox><br />
                vBomItem2:
                <asp:TextBox ID="vBomItem2TextBox" runat="server" Text='<%# Bind("vBomItem2") %>'>
                </asp:TextBox><br />
                vBomItem3:
                <asp:TextBox ID="vBomItem3TextBox" runat="server" Text='<%# Bind("vBomItem3") %>'>
                </asp:TextBox><br />
                vBomItem4:
                <asp:TextBox ID="vBomItem4TextBox" runat="server" Text='<%# Bind("vBomItem4") %>'>
                </asp:TextBox><br />
                vBomItem5:
                <asp:TextBox ID="vBomItem5TextBox" runat="server" Text='<%# Bind("vBomItem5") %>'>
                </asp:TextBox><br />
                vBomItem6:
                <asp:TextBox ID="vBomItem6TextBox" runat="server" Text='<%# Bind("vBomItem6") %>'>
                </asp:TextBox><br />
                vBomItem7:
                <asp:TextBox ID="vBomItem7TextBox" runat="server" Text='<%# Bind("vBomItem7") %>'>
                </asp:TextBox><br />
                vBomItem8:
                <asp:TextBox ID="vBomItem8TextBox" runat="server" Text='<%# Bind("vBomItem8") %>'>
                </asp:TextBox><br />
                vBomItemDscr1:
                <asp:TextBox ID="vBomItemDscr1TextBox" runat="server" Text='<%# Bind("vBomItemDscr1") %>'>
                </asp:TextBox><br />
                vBomItemDscr2:
                <asp:TextBox ID="vBomItemDscr2TextBox" runat="server" Text='<%# Bind("vBomItemDscr2") %>'>
                </asp:TextBox><br />
                vBomItemDscr3:
                <asp:TextBox ID="vBomItemDscr3TextBox" runat="server" Text='<%# Bind("vBomItemDscr3") %>'>
                </asp:TextBox><br />
                vBomItemDscr4:
                <asp:TextBox ID="vBomItemDscr4TextBox" runat="server" Text='<%# Bind("vBomItemDscr4") %>'>
                </asp:TextBox><br />
                vBomItemDscr5:
                <asp:TextBox ID="vBomItemDscr5TextBox" runat="server" Text='<%# Bind("vBomItemDscr5") %>'>
                </asp:TextBox><br />
                vBomItemDscr6:
                <asp:TextBox ID="vBomItemDscr6TextBox" runat="server" Text='<%# Bind("vBomItemDscr6") %>'>
                </asp:TextBox><br />
                vBomItemDscr7:
                <asp:TextBox ID="vBomItemDscr7TextBox" runat="server" Text='<%# Bind("vBomItemDscr7") %>'>
                </asp:TextBox><br />
                vBomItemDscr8:
                <asp:TextBox ID="vBomItemDscr8TextBox" runat="server" Text='<%# Bind("vBomItemDscr8") %>'>
                </asp:TextBox><br />
                vBomItemDscr9:
                <asp:TextBox ID="vBomItemDscr9TextBox" runat="server" Text='<%# Bind("vBomItemDscr9") %>'>
                </asp:TextBox><br />
                vBomItemDscr10:
                <asp:TextBox ID="vBomItemDscr10TextBox" runat="server" Text='<%# Bind("vBomItemDscr10") %>'>
                </asp:TextBox><br />
                vShrink1:
                <asp:TextBox ID="vShrink1TextBox" runat="server" Text='<%# Bind("vShrink1") %>'>
                </asp:TextBox><br />
                vShrink2:
                <asp:TextBox ID="vShrink2TextBox" runat="server" Text='<%# Bind("vShrink2") %>'>
                </asp:TextBox><br />
                vShrink3:
                <asp:TextBox ID="vShrink3TextBox" runat="server" Text='<%# Bind("vShrink3") %>'>
                </asp:TextBox><br />
                vShrink4:
                <asp:TextBox ID="vShrink4TextBox" runat="server" Text='<%# Bind("vShrink4") %>'>
                </asp:TextBox><br />
                vShrink5:
                <asp:TextBox ID="vShrink5TextBox" runat="server" Text='<%# Bind("vShrink5") %>'>
                </asp:TextBox><br />
                vShrink6:
                <asp:TextBox ID="vShrink6TextBox" runat="server" Text='<%# Bind("vShrink6") %>'>
                </asp:TextBox><br />
                vShrink7:
                <asp:TextBox ID="vShrink7TextBox" runat="server" Text='<%# Bind("vShrink7") %>'>
                </asp:TextBox><br />
                vShrink8:
                <asp:TextBox ID="vShrink8TextBox" runat="server" Text='<%# Bind("vShrink8") %>'>
                </asp:TextBox><br />
                vSqInch1:
                <asp:TextBox ID="vSqInch1TextBox" runat="server" Text='<%# Bind("vSqInch1") %>'>
                </asp:TextBox><br />
                vSqInch2:
                <asp:TextBox ID="vSqInch2TextBox" runat="server" Text='<%# Bind("vSqInch2") %>'>
                </asp:TextBox><br />
                vSqInch3:
                <asp:TextBox ID="vSqInch3TextBox" runat="server" Text='<%# Bind("vSqInch3") %>'>
                </asp:TextBox><br />
                vSqInch4:
                <asp:TextBox ID="vSqInch4TextBox" runat="server" Text='<%# Bind("vSqInch4") %>'>
                </asp:TextBox><br />
                vSqInch5:
                <asp:TextBox ID="vSqInch5TextBox" runat="server" Text='<%# Bind("vSqInch5") %>'>
                </asp:TextBox><br />
                vSqInch6:
                <asp:TextBox ID="vSqInch6TextBox" runat="server" Text='<%# Bind("vSqInch6") %>'>
                </asp:TextBox><br />
                vSqInch7:
                <asp:TextBox ID="vSqInch7TextBox" runat="server" Text='<%# Bind("vSqInch7") %>'>
                </asp:TextBox><br />
                vSqInch8:
                <asp:TextBox ID="vSqInch8TextBox" runat="server" Text='<%# Bind("vSqInch8") %>'>
                </asp:TextBox><br />
                vSqInch9:
                <asp:TextBox ID="vSqInch9TextBox" runat="server" Text='<%# Bind("vSqInch9") %>'>
                </asp:TextBox><br />
                vSqInch10:
                <asp:TextBox ID="vSqInch10TextBox" runat="server" Text='<%# Bind("vSqInch10") %>'>
                </asp:TextBox><br />
                vLamCode:
                <asp:TextBox ID="vLamCodeTextBox" runat="server" Text='<%# Bind("vLamCode") %>'>
                </asp:TextBox><br />
                vAdhesive:
                <asp:TextBox ID="vAdhesiveTextBox" runat="server" Text='<%# Bind("vAdhesive") %>'>
                </asp:TextBox><br />
                <asp:LinkButton ID="InsertButton" runat="server" CausesValidation="True" CommandName="Insert"
                    Text="Insert">
                </asp:LinkButton>
                <asp:LinkButton ID="InsertCancelButton" runat="server" CausesValidation="False" CommandName="Cancel"
                    Text="Cancel">
                </asp:LinkButton>
            </InsertItemTemplate>
            <ItemTemplate>
                vBomItem1:
                <asp:Label ID="vBomItem1Label" runat="server" Text='<%# Bind("vBomItem1") %>'></asp:Label><br />
                vBomItem2:
                <asp:Label ID="vBomItem2Label" runat="server" Text='<%# Bind("vBomItem2") %>'></asp:Label><br />
                vBomItem3:
                <asp:Label ID="vBomItem3Label" runat="server" Text='<%# Bind("vBomItem3") %>'></asp:Label><br />
                vBomItem4:
                <asp:Label ID="vBomItem4Label" runat="server" Text='<%# Bind("vBomItem4") %>'></asp:Label><br />
                vBomItem5:
                <asp:Label ID="vBomItem5Label" runat="server" Text='<%# Bind("vBomItem5") %>'></asp:Label><br />
                vBomItem6:
                <asp:Label ID="vBomItem6Label" runat="server" Text='<%# Bind("vBomItem6") %>'></asp:Label><br />
                vBomItem7:
                <asp:Label ID="vBomItem7Label" runat="server" Text='<%# Bind("vBomItem7") %>'></asp:Label><br />
                vBomItem8:
                <asp:Label ID="vBomItem8Label" runat="server" Text='<%# Bind("vBomItem8") %>'></asp:Label><br />
                vBomItemDscr1:
                <asp:Label ID="vBomItemDscr1Label" runat="server" Text='<%# Bind("vBomItemDscr1") %>'>
                </asp:Label><br />
                vBomItemDscr2:
                <asp:Label ID="vBomItemDscr2Label" runat="server" Text='<%# Bind("vBomItemDscr2") %>'>
                </asp:Label><br />
                vBomItemDscr3:
                <asp:Label ID="vBomItemDscr3Label" runat="server" Text='<%# Bind("vBomItemDscr3") %>'>
                </asp:Label><br />
                vBomItemDscr4:
                <asp:Label ID="vBomItemDscr4Label" runat="server" Text='<%# Bind("vBomItemDscr4") %>'>
                </asp:Label><br />
                vBomItemDscr5:
                <asp:Label ID="vBomItemDscr5Label" runat="server" Text='<%# Bind("vBomItemDscr5") %>'>
                </asp:Label><br />
                vBomItemDscr6:
                <asp:Label ID="vBomItemDscr6Label" runat="server" Text='<%# Bind("vBomItemDscr6") %>'>
                </asp:Label><br />
                vBomItemDscr7:
                <asp:Label ID="vBomItemDscr7Label" runat="server" Text='<%# Bind("vBomItemDscr7") %>'>
                </asp:Label><br />
                vBomItemDscr8:
                <asp:Label ID="vBomItemDscr8Label" runat="server" Text='<%# Bind("vBomItemDscr8") %>'>
                </asp:Label><br />
                vBomItemDscr9:
                <asp:Label ID="vBomItemDscr9Label" runat="server" Text='<%# Bind("vBomItemDscr9") %>'>
                </asp:Label><br />
                vBomItemDscr10:
                <asp:Label ID="vBomItemDscr10Label" runat="server" Text='<%# Bind("vBomItemDscr10") %>'>
                </asp:Label><br />
                vShrink1:
                <asp:Label ID="vShrink1Label" runat="server" Text='<%# Bind("vShrink1") %>'></asp:Label><br />
                vShrink2:
                <asp:Label ID="vShrink2Label" runat="server" Text='<%# Bind("vShrink2") %>'></asp:Label><br />
                vShrink3:
                <asp:Label ID="vShrink3Label" runat="server" Text='<%# Bind("vShrink3") %>'></asp:Label><br />
                vShrink4:
                <asp:Label ID="vShrink4Label" runat="server" Text='<%# Bind("vShrink4") %>'></asp:Label><br />
                vShrink5:
                <asp:Label ID="vShrink5Label" runat="server" Text='<%# Bind("vShrink5") %>'></asp:Label><br />
                vShrink6:
                <asp:Label ID="vShrink6Label" runat="server" Text='<%# Bind("vShrink6") %>'></asp:Label><br />
                vShrink7:
                <asp:Label ID="vShrink7Label" runat="server" Text='<%# Bind("vShrink7") %>'></asp:Label><br />
                vShrink8:
                <asp:Label ID="vShrink8Label" runat="server" Text='<%# Bind("vShrink8") %>'></asp:Label><br />
                vSqInch1:
                <asp:Label ID="vSqInch1Label" runat="server" Text='<%# Bind("vSqInch1") %>'></asp:Label><br />
                vSqInch2:
                <asp:Label ID="vSqInch2Label" runat="server" Text='<%# Bind("vSqInch2") %>'></asp:Label><br />
                vSqInch3:
                <asp:Label ID="vSqInch3Label" runat="server" Text='<%# Bind("vSqInch3") %>'></asp:Label><br />
                vSqInch4:
                <asp:Label ID="vSqInch4Label" runat="server" Text='<%# Bind("vSqInch4") %>'></asp:Label><br />
                vSqInch5:
                <asp:Label ID="vSqInch5Label" runat="server" Text='<%# Bind("vSqInch5") %>'></asp:Label><br />
                vSqInch6:
                <asp:Label ID="vSqInch6Label" runat="server" Text='<%# Bind("vSqInch6") %>'></asp:Label><br />
                vSqInch7:
                <asp:Label ID="vSqInch7Label" runat="server" Text='<%# Bind("vSqInch7") %>'></asp:Label><br />
                vSqInch8:
                <asp:Label ID="vSqInch8Label" runat="server" Text='<%# Bind("vSqInch8") %>'></asp:Label><br />
                vSqInch9:
                <asp:Label ID="vSqInch9Label" runat="server" Text='<%# Bind("vSqInch9") %>'></asp:Label><br />
                vSqInch10:
                <asp:Label ID="vSqInch10Label" runat="server" Text='<%# Bind("vSqInch10") %>'></asp:Label><br />
                vLamCode:
                <asp:Label ID="vLamCodeLabel" runat="server" Text='<%# Bind("vLamCode") %>'></asp:Label><br />
                vAdhesive:
                <asp:Label ID="vAdhesiveLabel" runat="server" Text='<%# Bind("vAdhesive") %>'></asp:Label><br />
            </ItemTemplate>--%>
        </asp:FormView>
        <asp:ObjectDataSource ID="ObjectDataSource1" runat="server" OldValuesParameterFormatString="original_{0}"
            SelectMethod="SelectBOM" TypeName="Corrugated">
            <SelectParameters>
                <asp:Parameter Name="prmUser" Type="String" />
                <asp:Parameter DefaultValue="Select" Name="prmAction" Type="String" />
                <asp:Parameter Name="prmComp" Type="String" />
                <asp:SessionParameter Name="prmEstimate" SessionField="order_corrugated_est" Type="String" />
                <asp:SessionParameter Name="prmForm" SessionField="order_corrugated_formno" Type="Int32" />
                <asp:Parameter Name="prmBomItem1" Type="String" />
                <asp:Parameter Name="prmBomItem2" Type="String" />
                <asp:Parameter Name="prmBomItem3" Type="String" />
                <asp:Parameter Name="prmBomItem4" Type="String" />
                <asp:Parameter Name="prmBomItem5" Type="String" />
                <asp:Parameter Name="prmBomItem6" Type="String" />
                <asp:Parameter Name="prmBomItem7" Type="String" />
                <asp:Parameter Name="prmBomItem8" Type="String" />
                <asp:Parameter Name="prmLamCode" Type="String" />
                <asp:Parameter Name="prmAdhesive" Type="String" />
                <asp:Parameter Name="prmBomItemDscr1" Type="String" />
                <asp:Parameter Name="prmBomItemDscr2" Type="String" />
                <asp:Parameter Name="prmBomItemDscr3" Type="String" />
                <asp:Parameter Name="prmBomItemDscr4" Type="String" />
                <asp:Parameter Name="prmBomItemDscr5" Type="String" />
                <asp:Parameter Name="prmBomItemDscr6" Type="String" />
                <asp:Parameter Name="prmBomItemDscr7" Type="String" />
                <asp:Parameter Name="prmBomItemDscr8" Type="String" />
                <asp:Parameter Name="prmBomItemDscr9" Type="String" />
                <asp:Parameter Name="prmBomItemDscr10" Type="String" />
                <asp:Parameter Name="prmShrink1" Type="Decimal" />
                <asp:Parameter Name="prmShrink2" Type="Decimal" />
                <asp:Parameter Name="prmShrink3" Type="Decimal" />
                <asp:Parameter Name="prmShrink4" Type="Decimal" />
                <asp:Parameter Name="prmShrink5" Type="Decimal" />
                <asp:Parameter Name="prmShrink6" Type="Decimal" />
                <asp:Parameter Name="prmShrink7" Type="Decimal" />
                <asp:Parameter Name="prmShrink8" Type="Decimal" />
                <asp:Parameter Name="prmSqInch1" Type="Decimal" />
                <asp:Parameter Name="prmSqInch2" Type="Decimal" />
                <asp:Parameter Name="prmSqInch3" Type="Decimal" />
                <asp:Parameter Name="prmSqInch4" Type="Decimal" />
                <asp:Parameter Name="prmSqInch5" Type="Decimal" />
                <asp:Parameter Name="prmSqInch6" Type="Decimal" />
                <asp:Parameter Name="prmSqInch7" Type="Decimal" />
                <asp:Parameter Name="prmSqInch8" Type="Decimal" />
                <asp:Parameter Name="prmSqInch9" Type="Decimal" />
                <asp:Parameter Name="prmSqInch10" Type="Decimal" />
            </SelectParameters>
        </asp:ObjectDataSource>
    </div>
    </form>
</body>
</html>
