using System;
using System.Data;
using System.Configuration;
using System.Collections;
using System.Web;
using System.Web.Security;
using System.Web.UI;
using System.Web.UI.WebControls;
using System.Web.UI.WebControls.WebParts;
using System.Web.UI.HtmlControls;
using Progress.Open4GL.Proxy;
using ASINET1;
using ASIDataNS;
using System.Text;

public partial class fold_Inks : System.Web.UI.Page
{
    protected void Page_Load(object sender, EventArgs e)
    {
        
        //FormView_Fold.ChangeMode(FormViewMode.ReadOnly);
        CorrugatedFoldDataSource.SelectParameters["prmAction"].DefaultValue = "Select";
        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];
        if (!Page.IsPostBack)
        {
            
        }

        if (Session["User"] != null)
        {
            string vUserId = UserLogin.UserName;
            string vPage = "fold_inks.aspx";
            string aUsers = null;
            string PrmComp = null;
            bool vCanCreate = false;
            bool vCanRun = false;
            bool vCanUpdate = false;
            bool vCanDelete = false;


            func1 f1 = new func1();
            //Response.Write(Page);
            f1.CheckProgramPermissions(vPage, vUserId, ref  vCanCreate, ref  vCanRun, ref  vCanUpdate, ref  vCanDelete, ref  PrmComp, ref  aUsers);

            Label compname = (Label)Master.FindControl("lblComp");
            Label username = (Label)Master.FindControl("lblUser");
            Label labelname = (Label)Master.FindControl("lbl_page");
            compname.Text = PrmComp;
            username.Text = UserLogin.UserName;
            labelname.Text = "Folding";
            
            if (aUsers == "external")
            {

            }
            if (vCanRun == false)
            {
                Response.Write("<script>alert('Sorry! You don't have permission to access this page');</script>");
                Response.Write("<script>window.location.href = 'login.aspx';</script>");

            }
        }
        try
        {            
            Image img_mov_col = (Image)Master.FindControl("Image5");
            img_mov_col.Visible = false;
        }
        catch { }


        StringBuilder str = new StringBuilder();
        str.Append("<script language=javascript>");
        str.Append("function update(e){");

        str.Append("document.forms[0].ctl00$ContentPlaceHolder1$FormView_Fold$vCode1TextBox.value=e[0];");
        str.Append("document.forms[0].ctl00$ContentPlaceHolder1$FormView_Fold$vCode2TextBox.value=e[1];");
        str.Append("document.forms[0].ctl00$ContentPlaceHolder1$FormView_Fold$vCode3TextBox.value=e[2];");
        str.Append("document.forms[0].ctl00$ContentPlaceHolder1$FormView_Fold$vCode4TextBox.value=e[3];");
        str.Append("document.forms[0].ctl00$ContentPlaceHolder1$FormView_Fold$vCode5TextBox.value=e[4];");
        str.Append("document.forms[0].ctl00$ContentPlaceHolder1$FormView_Fold$vCode6TextBox.value=e[5];");
        str.Append("document.forms[0].ctl00$ContentPlaceHolder1$FormView_Fold$vCode7TextBox.value=e[6];");
        str.Append("document.forms[0].ctl00$ContentPlaceHolder1$FormView_Fold$vCode8TextBox.value=e[7];");
        str.Append("document.forms[0].ctl00$ContentPlaceHolder1$FormView_Fold$vCode9TextBox.value=e[8];");
        str.Append("document.forms[0].ctl00$ContentPlaceHolder1$FormView_Fold$vCode10TextBox.value=e[9];");

        str.Append("document.forms[0].ctl00$ContentPlaceHolder1$FormView_Fold$vDscr1TextBox.value=e[10];");
        str.Append("document.forms[0].ctl00$ContentPlaceHolder1$FormView_Fold$vDscr2TextBox.value=e[11];");
        str.Append("document.forms[0].ctl00$ContentPlaceHolder1$FormView_Fold$vDscr3TextBox.value=e[12];");
        str.Append("document.forms[0].ctl00$ContentPlaceHolder1$FormView_Fold$vDscr4TextBox.value=e[13];");
        str.Append("document.forms[0].ctl00$ContentPlaceHolder1$FormView_Fold$vDscr5TextBox.value=e[14];");
        str.Append("document.forms[0].ctl00$ContentPlaceHolder1$FormView_Fold$vDscr6TextBox.value=e[15];");
        str.Append("document.forms[0].ctl00$ContentPlaceHolder1$FormView_Fold$vDscr7TextBox.value=e[16];");
        str.Append("document.forms[0].ctl00$ContentPlaceHolder1$FormView_Fold$vDscr8TextBox.value=e[17];");
        str.Append("document.forms[0].ctl00$ContentPlaceHolder1$FormView_Fold$vDscr9TextBox.value=e[18];");
        str.Append("document.forms[0].ctl00$ContentPlaceHolder1$FormView_Fold$vDscr10TextBox.value=e[19];");

        str.Append("document.forms[0].ctl00$ContentPlaceHolder1$FormView_Fold$vPer1TextBox.value=e[20];");
        str.Append("document.forms[0].ctl00$ContentPlaceHolder1$FormView_Fold$vPer2TextBox.value=e[21];");
        str.Append("document.forms[0].ctl00$ContentPlaceHolder1$FormView_Fold$vPer3TextBox.value=e[22];");
        str.Append("document.forms[0].ctl00$ContentPlaceHolder1$FormView_Fold$vPer4TextBox.value=e[23];");
        str.Append("document.forms[0].ctl00$ContentPlaceHolder1$FormView_Fold$vPer5TextBox.value=e[24];");
        str.Append("document.forms[0].ctl00$ContentPlaceHolder1$FormView_Fold$vPer6TextBox.value=e[25];");
        str.Append("document.forms[0].ctl00$ContentPlaceHolder1$FormView_Fold$vPer7TextBox.value=e[26];");
        str.Append("document.forms[0].ctl00$ContentPlaceHolder1$FormView_Fold$vPer8TextBox.value=e[27];");
        str.Append("document.forms[0].ctl00$ContentPlaceHolder1$FormView_Fold$vPer9TextBox.value=e[28];");
        str.Append("document.forms[0].ctl00$ContentPlaceHolder1$FormView_Fold$vPer10TextBox.value=e[29];}");

        str.Append("</script>");

        // register the javascript into the Page
        if (!ClientScript.IsClientScriptBlockRegistered(this.GetType(), "update"))
        {
            Page.RegisterClientScriptBlock("update", str.ToString());
        }


    }

    protected void Save_Click(object sender, EventArgs e)
    {

        TextBox est = (TextBox)FormView_Fold.FindControl("vEstNumTextBox");
        Label formno = (Label)FormView_Fold.FindControl("vFormNoTextBox");
        TextBox color = (TextBox)FormView_Fold.FindControl("vColorTextBox");
        TextBox pass = (TextBox)FormView_Fold.FindControl("vPassesTextBox");
        TextBox coat = (TextBox)FormView_Fold.FindControl("vCoatTextBox");
        TextBox coatpass = (TextBox)FormView_Fold.FindControl("vCoatPassTextBox");
        TextBox dscr = (TextBox)FormView_Fold.FindControl("vDscrTextBox");
        TextBox ps1 = (TextBox)FormView_Fold.FindControl("vPs1TextBox");
        TextBox ps2 = (TextBox)FormView_Fold.FindControl("vPs2TextBox");
        TextBox ps3 = (TextBox)FormView_Fold.FindControl("vPs3TextBox");
        TextBox ps4 = (TextBox)FormView_Fold.FindControl("vPs4TextBox");
        TextBox ps5 = (TextBox)FormView_Fold.FindControl("vPs5TextBox");
        TextBox ps6 = (TextBox)FormView_Fold.FindControl("vPs6TextBox");
        TextBox ps7 = (TextBox)FormView_Fold.FindControl("vPs7TextBox");
        TextBox ps8 = (TextBox)FormView_Fold.FindControl("vPs8TextBox");
        TextBox ps9 = (TextBox)FormView_Fold.FindControl("vPs9TextBox");
        TextBox ps10 = (TextBox)FormView_Fold.FindControl("vPs10TextBox");
        TextBox ps11 = (TextBox)FormView_Fold.FindControl("vPs11TextBox");
        TextBox ps12 = (TextBox)FormView_Fold.FindControl("vPs12TextBox");
        TextBox ps13 = (TextBox)FormView_Fold.FindControl("vPs13TextBox");
        TextBox ps14 = (TextBox)FormView_Fold.FindControl("vPs14TextBox");
        TextBox ps15 = (TextBox)FormView_Fold.FindControl("vPs15TextBox");

        TextBox code1 = (TextBox)FormView_Fold.FindControl("vCode1TextBox");
        TextBox code2 = (TextBox)FormView_Fold.FindControl("vCode2TextBox");
        TextBox code3 = (TextBox)FormView_Fold.FindControl("vCode3TextBox");
        TextBox code4 = (TextBox)FormView_Fold.FindControl("vCode4TextBox");
        TextBox code5 = (TextBox)FormView_Fold.FindControl("vCode5TextBox");
        TextBox code6 = (TextBox)FormView_Fold.FindControl("vCode6TextBox");
        TextBox code7 = (TextBox)FormView_Fold.FindControl("vCode7TextBox");
        TextBox code8 = (TextBox)FormView_Fold.FindControl("vCode8TextBox");
        TextBox code9 = (TextBox)FormView_Fold.FindControl("vCode9TextBox");
        TextBox code10 = (TextBox)FormView_Fold.FindControl("vCode10TextBox");
        TextBox code11 = (TextBox)FormView_Fold.FindControl("vCode11TextBox");
        TextBox code12 = (TextBox)FormView_Fold.FindControl("vCode12TextBox");
        TextBox code13 = (TextBox)FormView_Fold.FindControl("vCode13TextBox");
        TextBox code14 = (TextBox)FormView_Fold.FindControl("vCode14TextBox");
        TextBox code15 = (TextBox)FormView_Fold.FindControl("vCode15TextBox");

        TextBox dscr1 = (TextBox)FormView_Fold.FindControl("vDscr1TextBox");
        TextBox dscr2 = (TextBox)FormView_Fold.FindControl("vDscr2TextBox");
        TextBox dscr3 = (TextBox)FormView_Fold.FindControl("vDscr3TextBox");
        TextBox dscr4 = (TextBox)FormView_Fold.FindControl("vDscr4TextBox");
        TextBox dscr5 = (TextBox)FormView_Fold.FindControl("vDscr5TextBox");
        TextBox dscr6 = (TextBox)FormView_Fold.FindControl("vDscr6TextBox");
        TextBox dscr7 = (TextBox)FormView_Fold.FindControl("vDscr7TextBox");
        TextBox dscr8 = (TextBox)FormView_Fold.FindControl("vDscr8TextBox");
        TextBox dscr9 = (TextBox)FormView_Fold.FindControl("vDscr9TextBox");
        TextBox dscr10 = (TextBox)FormView_Fold.FindControl("vDscr10TextBox");
        TextBox dscr11 = (TextBox)FormView_Fold.FindControl("vDscr11TextBox");
        TextBox dscr12 = (TextBox)FormView_Fold.FindControl("vDscr12TextBox");
        TextBox dscr13 = (TextBox)FormView_Fold.FindControl("vDscr13TextBox");
        TextBox dscr14 = (TextBox)FormView_Fold.FindControl("vDscr14TextBox");
        TextBox dscr15 = (TextBox)FormView_Fold.FindControl("vDscr15TextBox");

        TextBox per1 = (TextBox)FormView_Fold.FindControl("vPer1TextBox");
        TextBox per2 = (TextBox)FormView_Fold.FindControl("vPer2TextBox");
        TextBox per3 = (TextBox)FormView_Fold.FindControl("vPer3TextBox");
        TextBox per4 = (TextBox)FormView_Fold.FindControl("vPer4TextBox");
        TextBox per5 = (TextBox)FormView_Fold.FindControl("vPer5TextBox");
        TextBox per6 = (TextBox)FormView_Fold.FindControl("vPer6TextBox");
        TextBox per7 = (TextBox)FormView_Fold.FindControl("vPer7TextBox");
        TextBox per8 = (TextBox)FormView_Fold.FindControl("vPer8TextBox");
        TextBox per9 = (TextBox)FormView_Fold.FindControl("vPer9TextBox");
        TextBox per10 = (TextBox)FormView_Fold.FindControl("vPer10TextBox");
        TextBox per11 = (TextBox)FormView_Fold.FindControl("vPer11TextBox");
        TextBox per12 = (TextBox)FormView_Fold.FindControl("vPer12TextBox");
        TextBox per13 = (TextBox)FormView_Fold.FindControl("vPer13TextBox");
        TextBox per14 = (TextBox)FormView_Fold.FindControl("vPer14TextBox");
        TextBox per15 = (TextBox)FormView_Fold.FindControl("vPer15TextBox");
        TextBox unit1 = (TextBox)FormView_Fold.FindControl("vUnit1TextBox");
        TextBox unit2 = (TextBox)FormView_Fold.FindControl("vUnit2TextBox");
        TextBox unit3 = (TextBox)FormView_Fold.FindControl("vUnit3TextBox");
        TextBox unit4 = (TextBox)FormView_Fold.FindControl("vUnit4TextBox");
        TextBox unit5 = (TextBox)FormView_Fold.FindControl("vUnit5TextBox");
        TextBox unit6 = (TextBox)FormView_Fold.FindControl("vUnit6TextBox");
        TextBox unit7 = (TextBox)FormView_Fold.FindControl("vUnit7TextBox");
        TextBox unit8 = (TextBox)FormView_Fold.FindControl("vUnit8TextBox");
        TextBox unit9 = (TextBox)FormView_Fold.FindControl("vUnit9TextBox");
        TextBox unit10 = (TextBox)FormView_Fold.FindControl("vUnit10TextBox");
        TextBox unit11 = (TextBox)FormView_Fold.FindControl("vUnit11TextBox");
        TextBox unit12 = (TextBox)FormView_Fold.FindControl("vUnit12TextBox");
        TextBox unit13 = (TextBox)FormView_Fold.FindControl("vUnit13TextBox");
        TextBox unit14 = (TextBox)FormView_Fold.FindControl("vUnit14TextBox");
        TextBox unit15 = (TextBox)FormView_Fold.FindControl("vUnit15TextBox");

        TextBox side1 = (TextBox)FormView_Fold.FindControl("vSide1TextBox");
        TextBox side2 = (TextBox)FormView_Fold.FindControl("vSide2TextBox");
        TextBox side3 = (TextBox)FormView_Fold.FindControl("vSide3TextBox");
        TextBox side4 = (TextBox)FormView_Fold.FindControl("vSide4TextBox");
        TextBox side5 = (TextBox)FormView_Fold.FindControl("vSide5TextBox");
        TextBox side6 = (TextBox)FormView_Fold.FindControl("vSide6TextBox");
        TextBox side7 = (TextBox)FormView_Fold.FindControl("vSide7TextBox");
        TextBox side8 = (TextBox)FormView_Fold.FindControl("vSide8TextBox");
        TextBox side9 = (TextBox)FormView_Fold.FindControl("vSide9TextBox");
        TextBox side10 = (TextBox)FormView_Fold.FindControl("vSide10TextBox");
        TextBox side11 = (TextBox)FormView_Fold.FindControl("vSide11TextBox");
        TextBox side12 = (TextBox)FormView_Fold.FindControl("vSide12TextBox");
        TextBox side13 = (TextBox)FormView_Fold.FindControl("vSide13TextBox");
        TextBox side14 = (TextBox)FormView_Fold.FindControl("vSide14TextBox");
        TextBox side15 = (TextBox)FormView_Fold.FindControl("vSide15TextBox");


        //TextBox packqty = (TextBox)FormView_Fold.FindControl("vPackQtyTextBox");
        TextBox layerpad = (TextBox)FormView_Fold.FindControl("vLayerPadTextBox");
        TextBox layerlen = (TextBox)FormView_Fold.FindControl("vLayerLenTextBox");
        TextBox layerwid = (TextBox)FormView_Fold.FindControl("vLayerWidTextBox");
        //TextBox layerdep = (TextBox)FormView_Fold.FindControl("vLayerDepTextBox");
        TextBox layerqty = (TextBox)FormView_Fold.FindControl("vLayerQtyTextBox");
        TextBox div = (TextBox)FormView_Fold.FindControl("vDelTextBox");
        TextBox divlen = (TextBox)FormView_Fold.FindControl("vDelLenTextBox");
        TextBox divwid = (TextBox)FormView_Fold.FindControl("vDelWidTextBox");
        //TextBox divdep = (TextBox)FormView_Fold.FindControl("vDelDepTextBox");
        TextBox divqty = (TextBox)FormView_Fold.FindControl("vDelQtyTextBox");
        TextBox note = (TextBox)FormView_Fold.FindControl("vNoteTextBox");


        TextBox packcode = (TextBox)FormView_Fold.FindControl("vPackCodeTextBox");
        TextBox unitlen = (TextBox)FormView_Fold.FindControl("vUnitLenTextBox");
        TextBox cost1 = (TextBox)FormView_Fold.FindControl("vCostTextBox");
        TextBox unitwid = (TextBox)FormView_Fold.FindControl("vUnitWidTextBox");
        TextBox boxcode = (TextBox)FormView_Fold.FindControl("vBoxCodeTextBox");
        TextBox unitdep = (TextBox)FormView_Fold.FindControl("vUnitDepTextBox");
        TextBox bundl = (TextBox)FormView_Fold.FindControl("vBundlTextBox");
        TextBox wtpack = (TextBox)FormView_Fold.FindControl("vWtPackTextBox");
        TextBox unit = (TextBox)FormView_Fold.FindControl("vUnitTextBox");
        TextBox len = (TextBox)FormView_Fold.FindControl("vLengthTextBox");
        TextBox cost2 = (TextBox)FormView_Fold.FindControl("vCost2TextBox");
        TextBox wid = (TextBox)FormView_Fold.FindControl("vWidthTextBox");
        TextBox count = (TextBox)FormView_Fold.FindControl("vCountTextBox");
        TextBox hight = (TextBox)FormView_Fold.FindControl("vHeightTextBox");
        TextBox layer = (TextBox)FormView_Fold.FindControl("vLayerTextBox");        
        TextBox weiper = (TextBox)FormView_Fold.FindControl("vWeiPerTextBox");
        TextBox carrier = (TextBox)FormView_Fold.FindControl("vCarrierTextBox");
        TextBox carrdscr = (TextBox)FormView_Fold.FindControl("vCarrDscrTextBox");
        TextBox delzon = (TextBox)FormView_Fold.FindControl("vDelZonTextBox");
        TextBox frcwt = (TextBox)FormView_Fold.FindControl("vFreifgtTextBox");
        TextBox frm = (TextBox)FormView_Fold.FindControl("vFreOutTextBox");
        RadioButtonList fright = (RadioButtonList)FormView_Fold.FindControl("RadioButtonList1");

        if (fright.SelectedIndex == 0)
        {
            HiddenField1.Value = "P";
        }
        if (fright.SelectedIndex == 1)
        {
            HiddenField1.Value = "C";
        }
        if (fright.SelectedIndex == 2)
        {
            HiddenField1.Value = "B";
        }
        if (fright.SelectedIndex == 3)
        {
            HiddenField1.Value = "T";
        }
        
        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];
        CorrugatedFoldDataSource.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
        CorrugatedFoldDataSource.SelectParameters["prmAction"].DefaultValue = "FoldsUpdate";
        //CorrugatedFoldDataSource.SelectParameters["prmComp"].DefaultValue = cust.Text.Trim();
        CorrugatedFoldDataSource.SelectParameters["prmEstNum"].DefaultValue = est.Text.Trim();
        CorrugatedFoldDataSource.SelectParameters["prmFormNo"].DefaultValue = formno.Text.Trim();
        CorrugatedFoldDataSource.SelectParameters["prmColor"].DefaultValue = color.Text.Trim();
        CorrugatedFoldDataSource.SelectParameters["prmPass"].DefaultValue = pass.Text.Trim();
        CorrugatedFoldDataSource.SelectParameters["prmCoat"].DefaultValue = coat.Text.Trim();
        CorrugatedFoldDataSource.SelectParameters["prmCoatPass"].DefaultValue = coatpass.Text.Trim();
        CorrugatedFoldDataSource.SelectParameters["prmDscr"].DefaultValue = dscr.Text.Trim();
        CorrugatedFoldDataSource.SelectParameters["prmPs1"].DefaultValue = ps1.Text.Trim();
        CorrugatedFoldDataSource.SelectParameters["prmPs2"].DefaultValue = ps2.Text.Trim();
        CorrugatedFoldDataSource.SelectParameters["prmPs3"].DefaultValue = ps3.Text.Trim();
        CorrugatedFoldDataSource.SelectParameters["prmPs4"].DefaultValue = ps4.Text.Trim();
        CorrugatedFoldDataSource.SelectParameters["prmPs5"].DefaultValue = ps5.Text.Trim();
        CorrugatedFoldDataSource.SelectParameters["prmPs6"].DefaultValue = ps6.Text.Trim();
        CorrugatedFoldDataSource.SelectParameters["prmPs7"].DefaultValue = ps7.Text.Trim();
        CorrugatedFoldDataSource.SelectParameters["prmPs8"].DefaultValue = ps8.Text.Trim();
        CorrugatedFoldDataSource.SelectParameters["prmPs9"].DefaultValue = ps9.Text.Trim();
        CorrugatedFoldDataSource.SelectParameters["prmPs10"].DefaultValue = ps10.Text.Trim();
        CorrugatedFoldDataSource.SelectParameters["prmPs11"].DefaultValue = ps11.Text.Trim();
        CorrugatedFoldDataSource.SelectParameters["prmPs12"].DefaultValue = ps12.Text.Trim();
        CorrugatedFoldDataSource.SelectParameters["prmPs13"].DefaultValue = ps13.Text.Trim();
        CorrugatedFoldDataSource.SelectParameters["prmPs14"].DefaultValue = ps14.Text.Trim();
        CorrugatedFoldDataSource.SelectParameters["prmPs15"].DefaultValue = ps15.Text.Trim();
        CorrugatedFoldDataSource.SelectParameters["prmCode1"].DefaultValue = code1.Text.Trim();
        CorrugatedFoldDataSource.SelectParameters["prmCode2"].DefaultValue = code2.Text.Trim();
        CorrugatedFoldDataSource.SelectParameters["prmCode3"].DefaultValue = code3.Text.Trim();
        CorrugatedFoldDataSource.SelectParameters["prmCode4"].DefaultValue = code4.Text.Trim();
        CorrugatedFoldDataSource.SelectParameters["prmCode5"].DefaultValue = code5.Text.Trim();
        CorrugatedFoldDataSource.SelectParameters["prmCode6"].DefaultValue = code6.Text.Trim();
        CorrugatedFoldDataSource.SelectParameters["prmCode7"].DefaultValue = code7.Text.Trim();
        CorrugatedFoldDataSource.SelectParameters["prmCode8"].DefaultValue = code8.Text.Trim();
        CorrugatedFoldDataSource.SelectParameters["prmCode9"].DefaultValue = code9.Text.Trim();
        CorrugatedFoldDataSource.SelectParameters["prmCode10"].DefaultValue = code10.Text.Trim();
        CorrugatedFoldDataSource.SelectParameters["prmCode11"].DefaultValue = code11.Text.Trim();
        CorrugatedFoldDataSource.SelectParameters["prmCode12"].DefaultValue = code12.Text.Trim();
        CorrugatedFoldDataSource.SelectParameters["prmCode13"].DefaultValue = code13.Text.Trim();
        CorrugatedFoldDataSource.SelectParameters["prmCode14"].DefaultValue = code14.Text.Trim();
        CorrugatedFoldDataSource.SelectParameters["prmCode15"].DefaultValue = code15.Text.Trim();
        CorrugatedFoldDataSource.SelectParameters["prmDscr1"].DefaultValue = dscr1.Text.Trim();
        CorrugatedFoldDataSource.SelectParameters["prmDscr2"].DefaultValue = dscr2.Text.Trim();
        CorrugatedFoldDataSource.SelectParameters["prmDscr3"].DefaultValue = dscr3.Text.Trim();
        CorrugatedFoldDataSource.SelectParameters["prmDscr4"].DefaultValue = dscr4.Text.Trim();
        CorrugatedFoldDataSource.SelectParameters["prmDscr5"].DefaultValue = dscr5.Text.Trim();
        CorrugatedFoldDataSource.SelectParameters["prmDscr6"].DefaultValue = dscr6.Text.Trim();
        CorrugatedFoldDataSource.SelectParameters["prmDscr7"].DefaultValue = dscr7.Text.Trim();
        CorrugatedFoldDataSource.SelectParameters["prmDscr8"].DefaultValue = dscr8.Text.Trim();
        CorrugatedFoldDataSource.SelectParameters["prmDscr9"].DefaultValue = dscr9.Text.Trim();
        CorrugatedFoldDataSource.SelectParameters["prmDscr10"].DefaultValue = dscr10.Text.Trim();
        CorrugatedFoldDataSource.SelectParameters["prmDscr11"].DefaultValue = dscr11.Text.Trim();
        CorrugatedFoldDataSource.SelectParameters["prmDscr12"].DefaultValue = dscr12.Text.Trim();
        CorrugatedFoldDataSource.SelectParameters["prmDscr13"].DefaultValue = dscr13.Text.Trim();
        CorrugatedFoldDataSource.SelectParameters["prmDscr14"].DefaultValue = dscr14.Text.Trim();
        CorrugatedFoldDataSource.SelectParameters["prmDscr15"].DefaultValue = dscr15.Text.Trim();
        CorrugatedFoldDataSource.SelectParameters["prmPer1"].DefaultValue =  per1.Text.Trim();
        CorrugatedFoldDataSource.SelectParameters["prmPer2"].DefaultValue = per2.Text.Trim();
        CorrugatedFoldDataSource.SelectParameters["prmPer3"].DefaultValue = per3.Text.Trim();
        CorrugatedFoldDataSource.SelectParameters["prmPer4"].DefaultValue = per4.Text.Trim();
        CorrugatedFoldDataSource.SelectParameters["prmPer5"].DefaultValue = per5.Text.Trim();
        CorrugatedFoldDataSource.SelectParameters["prmPer6"].DefaultValue = per6.Text.Trim();
        CorrugatedFoldDataSource.SelectParameters["prmPer7"].DefaultValue = per7.Text.Trim();
        CorrugatedFoldDataSource.SelectParameters["prmPer8"].DefaultValue = per8.Text.Trim();
        CorrugatedFoldDataSource.SelectParameters["prmPer9"].DefaultValue = per9.Text.Trim();
        CorrugatedFoldDataSource.SelectParameters["prmPer10"].DefaultValue = per10.Text.Trim();
        CorrugatedFoldDataSource.SelectParameters["prmPer11"].DefaultValue = per11.Text.Trim();
        CorrugatedFoldDataSource.SelectParameters["prmPer12"].DefaultValue = per12.Text.Trim();
        CorrugatedFoldDataSource.SelectParameters["prmPer13"].DefaultValue = per13.Text.Trim();
        CorrugatedFoldDataSource.SelectParameters["prmPer14"].DefaultValue = per14.Text.Trim();
        CorrugatedFoldDataSource.SelectParameters["prmPer15"].DefaultValue = per15.Text.Trim();

        CorrugatedFoldDataSource.SelectParameters["prmUnit1"].DefaultValue = unit1.Text.Trim();
        CorrugatedFoldDataSource.SelectParameters["prmUnit2"].DefaultValue = unit2.Text.Trim();
        CorrugatedFoldDataSource.SelectParameters["prmUnit3"].DefaultValue = unit3.Text.Trim();
        CorrugatedFoldDataSource.SelectParameters["prmUnit4"].DefaultValue = unit4.Text.Trim();
        CorrugatedFoldDataSource.SelectParameters["prmUnit5"].DefaultValue = unit5.Text.Trim();
        CorrugatedFoldDataSource.SelectParameters["prmUnit6"].DefaultValue = unit6.Text.Trim();
        CorrugatedFoldDataSource.SelectParameters["prmUnit7"].DefaultValue = unit7.Text.Trim();
        CorrugatedFoldDataSource.SelectParameters["prmUnit8"].DefaultValue = unit8.Text.Trim();
        CorrugatedFoldDataSource.SelectParameters["prmUnit9"].DefaultValue = unit9.Text.Trim();
        CorrugatedFoldDataSource.SelectParameters["prmUnit10"].DefaultValue = unit10.Text.Trim();
        CorrugatedFoldDataSource.SelectParameters["prmUnit11"].DefaultValue = unit11.Text.Trim();
        CorrugatedFoldDataSource.SelectParameters["prmUnit12"].DefaultValue = unit12.Text.Trim();
        CorrugatedFoldDataSource.SelectParameters["prmUnit13"].DefaultValue = unit13.Text.Trim();
        CorrugatedFoldDataSource.SelectParameters["prmUnit14"].DefaultValue = unit14.Text.Trim();
        CorrugatedFoldDataSource.SelectParameters["prmUnit15"].DefaultValue = unit15.Text.Trim();

        //CorrugatedFoldDataSource.SelectParameters["prmPackQty"].DefaultValue = packqty.Text.Trim();
        CorrugatedFoldDataSource.SelectParameters["prmLayerPad"].DefaultValue = layerpad.Text.Trim();
        CorrugatedFoldDataSource.SelectParameters["prmLayerLen"].DefaultValue = layerlen.Text.Trim();
        CorrugatedFoldDataSource.SelectParameters["prmLayerWid"].DefaultValue = layerwid.Text.Trim();
        //CorrugatedFoldDataSource.SelectParameters["prmLayerDep"].DefaultValue = layerdep.Text.Trim();
        CorrugatedFoldDataSource.SelectParameters["prmLayerQty"].DefaultValue = layerqty.Text.Trim();
        CorrugatedFoldDataSource.SelectParameters["prmDivider"].DefaultValue = div.Text.Trim();
        CorrugatedFoldDataSource.SelectParameters["prmDividerLen"].DefaultValue = divlen.Text.Trim();
        CorrugatedFoldDataSource.SelectParameters["prmDividerWid"].DefaultValue = divwid.Text.Trim();
        //CorrugatedFoldDataSource.SelectParameters["prmDividerDep"].DefaultValue = divdep.Text.Trim();
        CorrugatedFoldDataSource.SelectParameters["prmDivQty"].DefaultValue = divqty.Text.Trim();
        CorrugatedFoldDataSource.SelectParameters["prmNote"].DefaultValue = note.Text.Trim();
        

        CorrugatedFoldDataSource.SelectParameters["prmPackCode"].DefaultValue = packcode.Text.Trim();
        CorrugatedFoldDataSource.SelectParameters["prmUnitLen"].DefaultValue = unitlen.Text.Trim();
        CorrugatedFoldDataSource.SelectParameters["prmCostEa"].DefaultValue = cost1.Text.Trim();
        CorrugatedFoldDataSource.SelectParameters["prmUnitWid"].DefaultValue = unitwid.Text.Trim();
        CorrugatedFoldDataSource.SelectParameters["prmBoxCode"].DefaultValue = boxcode.Text.Trim();
        CorrugatedFoldDataSource.SelectParameters["prmUnitDep"].DefaultValue = unitdep.Text.Trim();
        CorrugatedFoldDataSource.SelectParameters["prmPallet"].DefaultValue = bundl.Text.Trim();
        CorrugatedFoldDataSource.SelectParameters["prmWTUnit"].DefaultValue = wtpack.Text.Trim();
        CorrugatedFoldDataSource.SelectParameters["prmUnit"].DefaultValue = unit.Text.Trim();
        CorrugatedFoldDataSource.SelectParameters["prmCost2"].DefaultValue = cost2.Text.Trim();
        CorrugatedFoldDataSource.SelectParameters["prmCount"].DefaultValue = count.Text.Trim();
        CorrugatedFoldDataSource.SelectParameters["prmLength"].DefaultValue = len.Text.Trim();
        CorrugatedFoldDataSource.SelectParameters["prmWidth"].DefaultValue = wid.Text.Trim();
        CorrugatedFoldDataSource.SelectParameters["prmHeight"].DefaultValue = hight.Text.Trim();
        CorrugatedFoldDataSource.SelectParameters["prmLayer"].DefaultValue = layer.Text.Trim();
        
        CorrugatedFoldDataSource.SelectParameters["prmFrCharge"].DefaultValue = HiddenField1.Value;
        CorrugatedFoldDataSource.SelectParameters["prmWeightPer"].DefaultValue = weiper.Text.Trim();
        CorrugatedFoldDataSource.SelectParameters["prmCarrier"].DefaultValue = carrier.Text.Trim();
        CorrugatedFoldDataSource.SelectParameters["prmCarrDscr"].DefaultValue = carrdscr.Text.Trim();
        CorrugatedFoldDataSource.SelectParameters["prmDelZon"].DefaultValue = delzon.Text.Trim();
        CorrugatedFoldDataSource.SelectParameters["prmFreight"].DefaultValue = frcwt.Text.Trim();
        CorrugatedFoldDataSource.SelectParameters["prmFreight2"].DefaultValue = frm.Text.Trim();

        CorrugatedFoldDataSource.SelectParameters["prmSide1"].DefaultValue = side1.Text.Trim();
        CorrugatedFoldDataSource.SelectParameters["prmSide2"].DefaultValue = side2.Text.Trim();
        CorrugatedFoldDataSource.SelectParameters["prmSide3"].DefaultValue = side3.Text.Trim();
        CorrugatedFoldDataSource.SelectParameters["prmSide4"].DefaultValue = side4.Text.Trim();
        CorrugatedFoldDataSource.SelectParameters["prmSide5"].DefaultValue = side5.Text.Trim();
        CorrugatedFoldDataSource.SelectParameters["prmSide6"].DefaultValue = side6.Text.Trim();
        CorrugatedFoldDataSource.SelectParameters["prmSide7"].DefaultValue = side7.Text.Trim();
        CorrugatedFoldDataSource.SelectParameters["prmSide8"].DefaultValue = side8.Text.Trim();
        CorrugatedFoldDataSource.SelectParameters["prmSide9"].DefaultValue = side9.Text.Trim();
        CorrugatedFoldDataSource.SelectParameters["prmSide10"].DefaultValue = side10.Text.Trim();
        CorrugatedFoldDataSource.SelectParameters["prmSide11"].DefaultValue = side11.Text.Trim();
        CorrugatedFoldDataSource.SelectParameters["prmSide12"].DefaultValue = side12.Text.Trim();
        CorrugatedFoldDataSource.SelectParameters["prmSide13"].DefaultValue = side13.Text.Trim();
        CorrugatedFoldDataSource.SelectParameters["prmSide14"].DefaultValue = side14.Text.Trim();
        CorrugatedFoldDataSource.SelectParameters["prmSide15"].DefaultValue = side15.Text.Trim();

        FormView_Fold.ChangeMode(FormViewMode.ReadOnly);
    }

    protected void colortextchanged(object sender, EventArgs e)
    {
        TextBox est = (TextBox)FormView_Fold.FindControl("vEstNumTextBox");
        Label formno = (Label)FormView_Fold.FindControl("vFormNoTextBox");
        TextBox color = (TextBox)FormView_Fold.FindControl("vColorTextBox");
        TextBox pass = (TextBox)FormView_Fold.FindControl("vPassesTextBox");
        TextBox coat = (TextBox)FormView_Fold.FindControl("vCoatTextBox");
        TextBox coatpass = (TextBox)FormView_Fold.FindControl("vCoatPassTextBox");
        TextBox dscr = (TextBox)FormView_Fold.FindControl("vDscrTextBox");
        TextBox ps1 = (TextBox)FormView_Fold.FindControl("vPs1TextBox");
        TextBox ps2 = (TextBox)FormView_Fold.FindControl("vPs2TextBox");
        TextBox ps3 = (TextBox)FormView_Fold.FindControl("vPs3TextBox");
        TextBox ps4 = (TextBox)FormView_Fold.FindControl("vPs4TextBox");
        TextBox ps5 = (TextBox)FormView_Fold.FindControl("vPs5TextBox");
        TextBox ps6 = (TextBox)FormView_Fold.FindControl("vPs6TextBox");
        TextBox ps7 = (TextBox)FormView_Fold.FindControl("vPs7TextBox");
        TextBox ps8 = (TextBox)FormView_Fold.FindControl("vPs8TextBox");
        TextBox ps9 = (TextBox)FormView_Fold.FindControl("vPs9TextBox");
        TextBox ps10 = (TextBox)FormView_Fold.FindControl("vPs10TextBox");
        TextBox ps11 = (TextBox)FormView_Fold.FindControl("vPs11TextBox");
        TextBox ps12 = (TextBox)FormView_Fold.FindControl("vPs12TextBox");
        TextBox ps13 = (TextBox)FormView_Fold.FindControl("vPs13TextBox");
        TextBox ps14 = (TextBox)FormView_Fold.FindControl("vPs14TextBox");
        TextBox ps15 = (TextBox)FormView_Fold.FindControl("vPs15TextBox");

        TextBox code1 = (TextBox)FormView_Fold.FindControl("vCode1TextBox");
        TextBox code2 = (TextBox)FormView_Fold.FindControl("vCode2TextBox");
        TextBox code3 = (TextBox)FormView_Fold.FindControl("vCode3TextBox");
        TextBox code4 = (TextBox)FormView_Fold.FindControl("vCode4TextBox");
        TextBox code5 = (TextBox)FormView_Fold.FindControl("vCode5TextBox");
        TextBox code6 = (TextBox)FormView_Fold.FindControl("vCode6TextBox");
        TextBox code7 = (TextBox)FormView_Fold.FindControl("vCode7TextBox");
        TextBox code8 = (TextBox)FormView_Fold.FindControl("vCode8TextBox");
        TextBox code9 = (TextBox)FormView_Fold.FindControl("vCode9TextBox");
        TextBox code10 = (TextBox)FormView_Fold.FindControl("vCode10TextBox");
        TextBox code11 = (TextBox)FormView_Fold.FindControl("vCode11TextBox");
        TextBox code12 = (TextBox)FormView_Fold.FindControl("vCode12TextBox");
        TextBox code13 = (TextBox)FormView_Fold.FindControl("vCode13TextBox");
        TextBox code14 = (TextBox)FormView_Fold.FindControl("vCode14TextBox");
        TextBox code15 = (TextBox)FormView_Fold.FindControl("vCode15TextBox");

        TextBox dscr1 = (TextBox)FormView_Fold.FindControl("vDscr1TextBox");
        TextBox dscr2 = (TextBox)FormView_Fold.FindControl("vDscr2TextBox");
        TextBox dscr3 = (TextBox)FormView_Fold.FindControl("vDscr3TextBox");
        TextBox dscr4 = (TextBox)FormView_Fold.FindControl("vDscr4TextBox");
        TextBox dscr5 = (TextBox)FormView_Fold.FindControl("vDscr5TextBox");
        TextBox dscr6 = (TextBox)FormView_Fold.FindControl("vDscr6TextBox");
        TextBox dscr7 = (TextBox)FormView_Fold.FindControl("vDscr7TextBox");
        TextBox dscr8 = (TextBox)FormView_Fold.FindControl("vDscr8TextBox");
        TextBox dscr9 = (TextBox)FormView_Fold.FindControl("vDscr9TextBox");
        TextBox dscr10 = (TextBox)FormView_Fold.FindControl("vDscr10TextBox");
        TextBox dscr11 = (TextBox)FormView_Fold.FindControl("vDscr11TextBox");
        TextBox dscr12 = (TextBox)FormView_Fold.FindControl("vDscr12TextBox");
        TextBox dscr13 = (TextBox)FormView_Fold.FindControl("vDscr13TextBox");
        TextBox dscr14 = (TextBox)FormView_Fold.FindControl("vDscr14TextBox");
        TextBox dscr15 = (TextBox)FormView_Fold.FindControl("vDscr15TextBox");

        TextBox per1 = (TextBox)FormView_Fold.FindControl("vPer1TextBox");
        TextBox per2 = (TextBox)FormView_Fold.FindControl("vPer2TextBox");
        TextBox per3 = (TextBox)FormView_Fold.FindControl("vPer3TextBox");
        TextBox per4 = (TextBox)FormView_Fold.FindControl("vPer4TextBox");
        TextBox per5 = (TextBox)FormView_Fold.FindControl("vPer5TextBox");
        TextBox per6 = (TextBox)FormView_Fold.FindControl("vPer6TextBox");
        TextBox per7 = (TextBox)FormView_Fold.FindControl("vPer7TextBox");
        TextBox per8 = (TextBox)FormView_Fold.FindControl("vPer8TextBox");
        TextBox per9 = (TextBox)FormView_Fold.FindControl("vPer9TextBox");
        TextBox per10 = (TextBox)FormView_Fold.FindControl("vPer10TextBox");
        TextBox per11 = (TextBox)FormView_Fold.FindControl("vPer11TextBox");
        TextBox per12 = (TextBox)FormView_Fold.FindControl("vPer12TextBox");
        TextBox per13 = (TextBox)FormView_Fold.FindControl("vPer13TextBox");
        TextBox per14 = (TextBox)FormView_Fold.FindControl("vPer14TextBox");
        TextBox per15 = (TextBox)FormView_Fold.FindControl("vPer15TextBox");
        TextBox unit1 = (TextBox)FormView_Fold.FindControl("vUnit1TextBox");
        TextBox unit2 = (TextBox)FormView_Fold.FindControl("vUnit2TextBox");
        TextBox unit3 = (TextBox)FormView_Fold.FindControl("vUnit3TextBox");
        TextBox unit4 = (TextBox)FormView_Fold.FindControl("vUnit4TextBox");
        TextBox unit5 = (TextBox)FormView_Fold.FindControl("vUnit5TextBox");
        TextBox unit6 = (TextBox)FormView_Fold.FindControl("vUnit6TextBox");
        TextBox unit7 = (TextBox)FormView_Fold.FindControl("vUnit7TextBox");
        TextBox unit8 = (TextBox)FormView_Fold.FindControl("vUnit8TextBox");
        TextBox unit9 = (TextBox)FormView_Fold.FindControl("vUnit9TextBox");
        TextBox unit10 = (TextBox)FormView_Fold.FindControl("vUnit10TextBox");
        TextBox unit11 = (TextBox)FormView_Fold.FindControl("vUnit11TextBox");
        TextBox unit12 = (TextBox)FormView_Fold.FindControl("vUnit12TextBox");
        TextBox unit13 = (TextBox)FormView_Fold.FindControl("vUnit13TextBox");
        TextBox unit14 = (TextBox)FormView_Fold.FindControl("vUnit14TextBox");
        TextBox unit15 = (TextBox)FormView_Fold.FindControl("vUnit15TextBox");

        TextBox side1  = (TextBox)FormView_Fold.FindControl("vSide1TextBox");
        TextBox side2  = (TextBox)FormView_Fold.FindControl("vSide2TextBox");
        TextBox side3  = (TextBox)FormView_Fold.FindControl("vSide3TextBox");
        TextBox side4  = (TextBox)FormView_Fold.FindControl("vSide4TextBox");
        TextBox side5  = (TextBox)FormView_Fold.FindControl("vSide5TextBox");
        TextBox side6  = (TextBox)FormView_Fold.FindControl("vSide6TextBox");
        TextBox side7  = (TextBox)FormView_Fold.FindControl("vSide7TextBox");
        TextBox side8  = (TextBox)FormView_Fold.FindControl("vSide8TextBox");
        TextBox side9  = (TextBox)FormView_Fold.FindControl("vSide9TextBox");
        TextBox side10 = (TextBox)FormView_Fold.FindControl("vSide10TextBox");
        TextBox side11 = (TextBox)FormView_Fold.FindControl("vSide11TextBox");
        TextBox side12 = (TextBox)FormView_Fold.FindControl("vSide12TextBox");
        TextBox side13 = (TextBox)FormView_Fold.FindControl("vSide13TextBox");
        TextBox side14 = (TextBox)FormView_Fold.FindControl("vSide14TextBox");
        TextBox side15 = (TextBox)FormView_Fold.FindControl("vSide15TextBox");

        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];

        Corrugated corr = new Corrugated();
        DataSet ds = new DataSet();
        ds = corr.FoldInks(UserLogin.UserName, "ColorChange", "", est.Text.Trim(), Convert.ToInt32(formno.Text.Trim()), Convert.ToInt32(color.Text.Trim()), Convert.ToInt32(pass.Text.Trim()), Convert.ToInt32(coat.Text.Trim()), Convert.ToInt32(coatpass.Text.Trim()), dscr.Text.Trim(), Convert.ToInt32(ps1.Text.Trim()), Convert.ToInt32(ps2.Text.Trim()), Convert.ToInt32(ps3.Text.Trim()), Convert.ToInt32(ps4.Text.Trim()), Convert.ToInt32(ps5.Text.Trim()), Convert.ToInt32(ps6.Text.Trim()), Convert.ToInt32(ps7.Text.Trim()), Convert.ToInt32(ps8.Text.Trim()), Convert.ToInt32(ps9.Text.Trim()), Convert.ToInt32(ps10.Text.Trim()), Convert.ToInt32(ps11.Text.Trim()), Convert.ToInt32(ps12.Text.Trim()), Convert.ToInt32(ps13.Text.Trim()), Convert.ToInt32(ps14.Text.Trim()), Convert.ToInt32(ps15.Text.Trim()), code1.Text.Trim(), code2.Text.Trim(), code3.Text.Trim(), code4.Text.Trim(), code5.Text.Trim(), code6.Text.Trim(), code7.Text.Trim(), code8.Text.Trim(), code9.Text.Trim(), code10.Text.Trim(), code11.Text.Trim(), code12.Text.Trim(), code13.Text.Trim(), code14.Text.Trim(), code15.Text.Trim(), dscr1.Text.Trim(), dscr2.Text.Trim(), dscr3.Text.Trim(), dscr4.Text.Trim(), dscr5.Text.Trim(), dscr6.Text.Trim(), dscr7.Text.Trim(), dscr8.Text.Trim(), dscr9.Text.Trim(), dscr10.Text.Trim(), dscr11.Text.Trim(), dscr12.Text.Trim(), dscr13.Text.Trim(), dscr14.Text.Trim(), dscr15.Text.Trim(), Convert.ToInt32(per1.Text.Trim()), Convert.ToInt32(per2.Text.Trim()), Convert.ToInt32(per3.Text.Trim()), Convert.ToInt32(per4.Text.Trim()), Convert.ToInt32(per5.Text.Trim()), Convert.ToInt32(per6.Text.Trim()), Convert.ToInt32(per7.Text.Trim()), Convert.ToInt32(per8.Text.Trim()), Convert.ToInt32(per9.Text.Trim()), Convert.ToInt32(per10.Text.Trim()), Convert.ToInt32(per11.Text.Trim()), Convert.ToInt32(per12.Text.Trim()), Convert.ToInt32(per13.Text.Trim()), Convert.ToInt32(per14.Text.Trim()), Convert.ToInt32(per15.Text.Trim()), Convert.ToDecimal(unit1.Text.Trim()), Convert.ToDecimal(unit2.Text.Trim()), Convert.ToDecimal(unit3.Text.Trim()), Convert.ToDecimal(unit4.Text.Trim()), Convert.ToDecimal(unit5.Text.Trim()), Convert.ToDecimal(unit6.Text.Trim()), Convert.ToDecimal(unit7.Text.Trim()), Convert.ToDecimal(unit8.Text.Trim()), Convert.ToDecimal(unit9.Text.Trim()), Convert.ToDecimal(unit10.Text.Trim()), Convert.ToDecimal(unit11.Text.Trim()), Convert.ToDecimal(unit12.Text.Trim()), Convert.ToDecimal(unit13.Text.Trim()), Convert.ToDecimal(unit14.Text.Trim()), Convert.ToDecimal(unit15.Text.Trim()), "", 0, 0, 0, "", 0, 0, 0, "", 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, "", 0, 0, 0, 0, 0, 0, "", "", 0, "", "", "", 0, 0, Convert.ToInt32(Session["order_folding_blankno"]), side1.Text.Trim(), side2.Text.Trim(), side3.Text.Trim(), side4.Text.Trim(), side5.Text.Trim(), side6.Text.Trim(), side7.Text.Trim(), side8.Text.Trim(), side9.Text.Trim(), side10.Text.Trim(), side11.Text.Trim(), side12.Text.Trim(), side13.Text.Trim(), side14.Text.Trim(), side15.Text.Trim());

        pass.Text = ds.Tables[0].Rows[0][8].ToString();       

        ps1.Text = ds.Tables[0].Rows[0][12].ToString();
        ps2.Text = ds.Tables[0].Rows[0][13].ToString();
        ps3.Text = ds.Tables[0].Rows[0][14].ToString();
        ps4.Text = ds.Tables[0].Rows[0][15].ToString();
        ps5.Text = ds.Tables[0].Rows[0][16].ToString();
        ps6.Text = ds.Tables[0].Rows[0][17].ToString();
        ps7.Text = ds.Tables[0].Rows[0][18].ToString();
        ps8.Text = ds.Tables[0].Rows[0][19].ToString();
        ps9.Text = ds.Tables[0].Rows[0][20].ToString();
        ps10.Text = ds.Tables[0].Rows[0][21].ToString();
        ps11.Text = ds.Tables[0].Rows[0][22].ToString();
        ps12.Text = ds.Tables[0].Rows[0][23].ToString();
        ps13.Text = ds.Tables[0].Rows[0][24].ToString();
        ps14.Text = ds.Tables[0].Rows[0][25].ToString();
        ps15.Text = ds.Tables[0].Rows[0][26].ToString();

        code1.Text = ds.Tables[0].Rows[0][27].ToString();
        code2.Text = ds.Tables[0].Rows[0][28].ToString();
        code3.Text = ds.Tables[0].Rows[0][29].ToString();
        code4.Text = ds.Tables[0].Rows[0][30].ToString();
        code5.Text = ds.Tables[0].Rows[0][31].ToString();
        code6.Text = ds.Tables[0].Rows[0][32].ToString();
        code7.Text = ds.Tables[0].Rows[0][33].ToString();
        code8.Text = ds.Tables[0].Rows[0][34].ToString();
        code9.Text = ds.Tables[0].Rows[0][35].ToString();
        code10.Text = ds.Tables[0].Rows[0][36].ToString();
        code11.Text = ds.Tables[0].Rows[0][37].ToString();
        code12.Text = ds.Tables[0].Rows[0][38].ToString();
        code13.Text = ds.Tables[0].Rows[0][39].ToString();
        code14.Text = ds.Tables[0].Rows[0][40].ToString();
        code15.Text = ds.Tables[0].Rows[0][41].ToString();

        dscr1.Text = ds.Tables[0].Rows[0][42].ToString();
        dscr2.Text = ds.Tables[0].Rows[0][43].ToString();
        dscr3.Text = ds.Tables[0].Rows[0][44].ToString();
        dscr4.Text = ds.Tables[0].Rows[0][45].ToString();
        dscr5.Text = ds.Tables[0].Rows[0][46].ToString();
        dscr6.Text = ds.Tables[0].Rows[0][47].ToString();
        dscr7.Text = ds.Tables[0].Rows[0][48].ToString();
        dscr8.Text = ds.Tables[0].Rows[0][49].ToString();
        dscr9.Text = ds.Tables[0].Rows[0][50].ToString();
        dscr10.Text = ds.Tables[0].Rows[0][51].ToString();
        dscr11.Text = ds.Tables[0].Rows[0][52].ToString();
        dscr12.Text = ds.Tables[0].Rows[0][53].ToString();
        dscr13.Text = ds.Tables[0].Rows[0][54].ToString();
        dscr14.Text = ds.Tables[0].Rows[0][55].ToString();
        dscr15.Text = ds.Tables[0].Rows[0][56].ToString();

        per1.Text = ds.Tables[0].Rows[0][57].ToString();
        per2.Text = ds.Tables[0].Rows[0][58].ToString();
        per3.Text = ds.Tables[0].Rows[0][59].ToString();
        per4.Text = ds.Tables[0].Rows[0][60].ToString();
        per5.Text = ds.Tables[0].Rows[0][61].ToString();
        per6.Text = ds.Tables[0].Rows[0][62].ToString();
        per7.Text = ds.Tables[0].Rows[0][63].ToString();
        per8.Text = ds.Tables[0].Rows[0][64].ToString();
        per9.Text = ds.Tables[0].Rows[0][65].ToString();
        per10.Text = ds.Tables[0].Rows[0][66].ToString();
        per11.Text = ds.Tables[0].Rows[0][67].ToString();
        per12.Text = ds.Tables[0].Rows[0][68].ToString();
        per13.Text = ds.Tables[0].Rows[0][69].ToString();
        per14.Text = ds.Tables[0].Rows[0][70].ToString();
        per15.Text = ds.Tables[0].Rows[0][71].ToString();

        side1.Text = ds.Tables[0].Rows[0][122].ToString();
        side2.Text = ds.Tables[0].Rows[0][123].ToString();
        side3.Text = ds.Tables[0].Rows[0][124].ToString();
        side4.Text = ds.Tables[0].Rows[0][125].ToString();
        side5.Text = ds.Tables[0].Rows[0][126].ToString();
        side6.Text = ds.Tables[0].Rows[0][127].ToString();
        side7.Text = ds.Tables[0].Rows[0][128].ToString();
        side8.Text = ds.Tables[0].Rows[0][129].ToString();
        side9.Text = ds.Tables[0].Rows[0][130].ToString();
        side10.Text = ds.Tables[0].Rows[0][131].ToString();
        side11.Text = ds.Tables[0].Rows[0][132].ToString();
        side12.Text = ds.Tables[0].Rows[0][133].ToString();
        side13.Text = ds.Tables[0].Rows[0][134].ToString();
        side14.Text = ds.Tables[0].Rows[0][135].ToString();
        side15.Text = ds.Tables[0].Rows[0][136].ToString();

        pass.Focus();

        Page.ClientScript.RegisterStartupScript(this.GetType(), "alert", "getSelect('vPassesTextBox');", true);
    }

    protected void coattextchanged(object sender, EventArgs e)
    {
        TextBox est = (TextBox)FormView_Fold.FindControl("vEstNumTextBox");
        Label formno = (Label)FormView_Fold.FindControl("vFormNoTextBox");
        TextBox color = (TextBox)FormView_Fold.FindControl("vColorTextBox");
        TextBox pass = (TextBox)FormView_Fold.FindControl("vPassesTextBox");
        TextBox coat = (TextBox)FormView_Fold.FindControl("vCoatTextBox");
        TextBox coatpass = (TextBox)FormView_Fold.FindControl("vCoatPassTextBox");
        TextBox dscr = (TextBox)FormView_Fold.FindControl("vDscrTextBox");
        TextBox ps1 = (TextBox)FormView_Fold.FindControl("vPs1TextBox");
        TextBox ps2 = (TextBox)FormView_Fold.FindControl("vPs2TextBox");
        TextBox ps3 = (TextBox)FormView_Fold.FindControl("vPs3TextBox");
        TextBox ps4 = (TextBox)FormView_Fold.FindControl("vPs4TextBox");
        TextBox ps5 = (TextBox)FormView_Fold.FindControl("vPs5TextBox");
        TextBox ps6 = (TextBox)FormView_Fold.FindControl("vPs6TextBox");
        TextBox ps7 = (TextBox)FormView_Fold.FindControl("vPs7TextBox");
        TextBox ps8 = (TextBox)FormView_Fold.FindControl("vPs8TextBox");
        TextBox ps9 = (TextBox)FormView_Fold.FindControl("vPs9TextBox");
        TextBox ps10 = (TextBox)FormView_Fold.FindControl("vPs10TextBox");
        TextBox ps11 = (TextBox)FormView_Fold.FindControl("vPs11TextBox");
        TextBox ps12 = (TextBox)FormView_Fold.FindControl("vPs12TextBox");
        TextBox ps13 = (TextBox)FormView_Fold.FindControl("vPs13TextBox");
        TextBox ps14 = (TextBox)FormView_Fold.FindControl("vPs14TextBox");
        TextBox ps15 = (TextBox)FormView_Fold.FindControl("vPs15TextBox");

        TextBox code1 = (TextBox)FormView_Fold.FindControl("vCode1TextBox");
        TextBox code2 = (TextBox)FormView_Fold.FindControl("vCode2TextBox");
        TextBox code3 = (TextBox)FormView_Fold.FindControl("vCode3TextBox");
        TextBox code4 = (TextBox)FormView_Fold.FindControl("vCode4TextBox");
        TextBox code5 = (TextBox)FormView_Fold.FindControl("vCode5TextBox");
        TextBox code6 = (TextBox)FormView_Fold.FindControl("vCode6TextBox");
        TextBox code7 = (TextBox)FormView_Fold.FindControl("vCode7TextBox");
        TextBox code8 = (TextBox)FormView_Fold.FindControl("vCode8TextBox");
        TextBox code9 = (TextBox)FormView_Fold.FindControl("vCode9TextBox");
        TextBox code10 = (TextBox)FormView_Fold.FindControl("vCode10TextBox");
        TextBox code11 = (TextBox)FormView_Fold.FindControl("vCode11TextBox");
        TextBox code12 = (TextBox)FormView_Fold.FindControl("vCode12TextBox");
        TextBox code13 = (TextBox)FormView_Fold.FindControl("vCode13TextBox");
        TextBox code14 = (TextBox)FormView_Fold.FindControl("vCode14TextBox");
        TextBox code15 = (TextBox)FormView_Fold.FindControl("vCode15TextBox");

        TextBox dscr1 = (TextBox)FormView_Fold.FindControl("vDscr1TextBox");
        TextBox dscr2 = (TextBox)FormView_Fold.FindControl("vDscr2TextBox");
        TextBox dscr3 = (TextBox)FormView_Fold.FindControl("vDscr3TextBox");
        TextBox dscr4 = (TextBox)FormView_Fold.FindControl("vDscr4TextBox");
        TextBox dscr5 = (TextBox)FormView_Fold.FindControl("vDscr5TextBox");
        TextBox dscr6 = (TextBox)FormView_Fold.FindControl("vDscr6TextBox");
        TextBox dscr7 = (TextBox)FormView_Fold.FindControl("vDscr7TextBox");
        TextBox dscr8 = (TextBox)FormView_Fold.FindControl("vDscr8TextBox");
        TextBox dscr9 = (TextBox)FormView_Fold.FindControl("vDscr9TextBox");
        TextBox dscr10 = (TextBox)FormView_Fold.FindControl("vDscr10TextBox");
        TextBox dscr11 = (TextBox)FormView_Fold.FindControl("vDscr11TextBox");
        TextBox dscr12 = (TextBox)FormView_Fold.FindControl("vDscr12TextBox");
        TextBox dscr13 = (TextBox)FormView_Fold.FindControl("vDscr13TextBox");
        TextBox dscr14 = (TextBox)FormView_Fold.FindControl("vDscr14TextBox");
        TextBox dscr15 = (TextBox)FormView_Fold.FindControl("vDscr15TextBox");

        TextBox per1 = (TextBox)FormView_Fold.FindControl("vPer1TextBox");
        TextBox per2 = (TextBox)FormView_Fold.FindControl("vPer2TextBox");
        TextBox per3 = (TextBox)FormView_Fold.FindControl("vPer3TextBox");
        TextBox per4 = (TextBox)FormView_Fold.FindControl("vPer4TextBox");
        TextBox per5 = (TextBox)FormView_Fold.FindControl("vPer5TextBox");
        TextBox per6 = (TextBox)FormView_Fold.FindControl("vPer6TextBox");
        TextBox per7 = (TextBox)FormView_Fold.FindControl("vPer7TextBox");
        TextBox per8 = (TextBox)FormView_Fold.FindControl("vPer8TextBox");
        TextBox per9 = (TextBox)FormView_Fold.FindControl("vPer9TextBox");
        TextBox per10 = (TextBox)FormView_Fold.FindControl("vPer10TextBox");
        TextBox per11 = (TextBox)FormView_Fold.FindControl("vPer11TextBox");
        TextBox per12 = (TextBox)FormView_Fold.FindControl("vPer12TextBox");
        TextBox per13 = (TextBox)FormView_Fold.FindControl("vPer13TextBox");
        TextBox per14 = (TextBox)FormView_Fold.FindControl("vPer14TextBox");
        TextBox per15 = (TextBox)FormView_Fold.FindControl("vPer15TextBox");
        TextBox unit1 = (TextBox)FormView_Fold.FindControl("vUnit1TextBox");
        TextBox unit2 = (TextBox)FormView_Fold.FindControl("vUnit2TextBox");
        TextBox unit3 = (TextBox)FormView_Fold.FindControl("vUnit3TextBox");
        TextBox unit4 = (TextBox)FormView_Fold.FindControl("vUnit4TextBox");
        TextBox unit5 = (TextBox)FormView_Fold.FindControl("vUnit5TextBox");
        TextBox unit6 = (TextBox)FormView_Fold.FindControl("vUnit6TextBox");
        TextBox unit7 = (TextBox)FormView_Fold.FindControl("vUnit7TextBox");
        TextBox unit8 = (TextBox)FormView_Fold.FindControl("vUnit8TextBox");
        TextBox unit9 = (TextBox)FormView_Fold.FindControl("vUnit9TextBox");
        TextBox unit10 = (TextBox)FormView_Fold.FindControl("vUnit10TextBox");
        TextBox unit11 = (TextBox)FormView_Fold.FindControl("vUnit11TextBox");
        TextBox unit12 = (TextBox)FormView_Fold.FindControl("vUnit12TextBox");
        TextBox unit13 = (TextBox)FormView_Fold.FindControl("vUnit13TextBox");
        TextBox unit14 = (TextBox)FormView_Fold.FindControl("vUnit14TextBox");
        TextBox unit15 = (TextBox)FormView_Fold.FindControl("vUnit15TextBox");

        TextBox side1 = (TextBox)FormView_Fold.FindControl("vSide1TextBox");
        TextBox side2 = (TextBox)FormView_Fold.FindControl("vSide2TextBox");
        TextBox side3 = (TextBox)FormView_Fold.FindControl("vSide3TextBox");
        TextBox side4 = (TextBox)FormView_Fold.FindControl("vSide4TextBox");
        TextBox side5 = (TextBox)FormView_Fold.FindControl("vSide5TextBox");
        TextBox side6 = (TextBox)FormView_Fold.FindControl("vSide6TextBox");
        TextBox side7 = (TextBox)FormView_Fold.FindControl("vSide7TextBox");
        TextBox side8 = (TextBox)FormView_Fold.FindControl("vSide8TextBox");
        TextBox side9 = (TextBox)FormView_Fold.FindControl("vSide9TextBox");
        TextBox side10 = (TextBox)FormView_Fold.FindControl("vSide10TextBox");
        TextBox side11 = (TextBox)FormView_Fold.FindControl("vSide11TextBox");
        TextBox side12 = (TextBox)FormView_Fold.FindControl("vSide12TextBox");
        TextBox side13 = (TextBox)FormView_Fold.FindControl("vSide13TextBox");
        TextBox side14 = (TextBox)FormView_Fold.FindControl("vSide14TextBox");
        TextBox side15 = (TextBox)FormView_Fold.FindControl("vSide15TextBox");

        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];

        Corrugated corr = new Corrugated();
        DataSet ds = new DataSet();
        ds = corr.FoldInks(UserLogin.UserName, "CoatChange", "", est.Text.Trim(), Convert.ToInt32(formno.Text.Trim()), Convert.ToInt32(color.Text.Trim()), Convert.ToInt32(pass.Text.Trim()), Convert.ToInt32(coat.Text.Trim()), Convert.ToInt32(coatpass.Text.Trim()), dscr.Text.Trim(), Convert.ToInt32(ps1.Text.Trim()), Convert.ToInt32(ps2.Text.Trim()), Convert.ToInt32(ps3.Text.Trim()), Convert.ToInt32(ps4.Text.Trim()), Convert.ToInt32(ps5.Text.Trim()), Convert.ToInt32(ps6.Text.Trim()), Convert.ToInt32(ps7.Text.Trim()), Convert.ToInt32(ps8.Text.Trim()), Convert.ToInt32(ps9.Text.Trim()), Convert.ToInt32(ps10.Text.Trim()), Convert.ToInt32(ps11.Text.Trim()), Convert.ToInt32(ps12.Text.Trim()), Convert.ToInt32(ps13.Text.Trim()), Convert.ToInt32(ps14.Text.Trim()), Convert.ToInt32(ps15.Text.Trim()), code1.Text.Trim(), code2.Text.Trim(), code3.Text.Trim(), code4.Text.Trim(), code5.Text.Trim(), code6.Text.Trim(), code7.Text.Trim(), code8.Text.Trim(), code9.Text.Trim(), code10.Text.Trim(), code11.Text.Trim(), code12.Text.Trim(), code13.Text.Trim(), code14.Text.Trim(), code15.Text.Trim(), dscr1.Text.Trim(), dscr2.Text.Trim(), dscr3.Text.Trim(), dscr4.Text.Trim(), dscr5.Text.Trim(), dscr6.Text.Trim(), dscr7.Text.Trim(), dscr8.Text.Trim(), dscr9.Text.Trim(), dscr10.Text.Trim(), dscr11.Text.Trim(), dscr12.Text.Trim(), dscr13.Text.Trim(), dscr14.Text.Trim(), dscr15.Text.Trim(), Convert.ToInt32(per1.Text.Trim()), Convert.ToInt32(per2.Text.Trim()), Convert.ToInt32(per3.Text.Trim()), Convert.ToInt32(per4.Text.Trim()), Convert.ToInt32(per5.Text.Trim()), Convert.ToInt32(per6.Text.Trim()), Convert.ToInt32(per7.Text.Trim()), Convert.ToInt32(per8.Text.Trim()), Convert.ToInt32(per9.Text.Trim()), Convert.ToInt32(per10.Text.Trim()), Convert.ToInt32(per11.Text.Trim()), Convert.ToInt32(per12.Text.Trim()), Convert.ToInt32(per13.Text.Trim()), Convert.ToInt32(per14.Text.Trim()), Convert.ToInt32(per15.Text.Trim()), Convert.ToDecimal(unit1.Text.Trim()), Convert.ToDecimal(unit2.Text.Trim()), Convert.ToDecimal(unit3.Text.Trim()), Convert.ToDecimal(unit4.Text.Trim()), Convert.ToDecimal(unit5.Text.Trim()), Convert.ToDecimal(unit6.Text.Trim()), Convert.ToDecimal(unit7.Text.Trim()), Convert.ToDecimal(unit8.Text.Trim()), Convert.ToDecimal(unit9.Text.Trim()), Convert.ToDecimal(unit10.Text.Trim()), Convert.ToDecimal(unit11.Text.Trim()), Convert.ToDecimal(unit12.Text.Trim()), Convert.ToDecimal(unit13.Text.Trim()), Convert.ToDecimal(unit14.Text.Trim()), Convert.ToDecimal(unit15.Text.Trim()), "", 0, 0, 0, "", 0, 0, 0, "", 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, "", 0, 0, 0, 0, 0, 0, "", "", 0, "", "", "", 0, 0, Convert.ToInt32(Session["order_folding_blankno"]), side1.Text.Trim(), side2.Text.Trim(), side3.Text.Trim(), side4.Text.Trim(), side5.Text.Trim(), side6.Text.Trim(), side7.Text.Trim(), side8.Text.Trim(), side9.Text.Trim(), side10.Text.Trim(), side11.Text.Trim(), side12.Text.Trim(), side13.Text.Trim(), side14.Text.Trim(), side15.Text.Trim());
        
        coatpass.Text = ds.Tables[0].Rows[0][10].ToString();

        ps1.Text = ds.Tables[0].Rows[0][12].ToString();
        ps2.Text = ds.Tables[0].Rows[0][13].ToString();
        ps3.Text = ds.Tables[0].Rows[0][14].ToString();
        ps4.Text = ds.Tables[0].Rows[0][15].ToString();
        ps5.Text = ds.Tables[0].Rows[0][16].ToString();
        ps6.Text = ds.Tables[0].Rows[0][17].ToString();
        ps7.Text = ds.Tables[0].Rows[0][18].ToString();
        ps8.Text = ds.Tables[0].Rows[0][19].ToString();
        ps9.Text = ds.Tables[0].Rows[0][20].ToString();
        ps10.Text = ds.Tables[0].Rows[0][21].ToString();
        ps11.Text = ds.Tables[0].Rows[0][22].ToString();
        ps12.Text = ds.Tables[0].Rows[0][23].ToString();
        ps13.Text = ds.Tables[0].Rows[0][24].ToString();
        ps14.Text = ds.Tables[0].Rows[0][25].ToString();
        ps15.Text = ds.Tables[0].Rows[0][26].ToString();

        code1.Text = ds.Tables[0].Rows[0][27].ToString();
        code2.Text = ds.Tables[0].Rows[0][28].ToString();
        code3.Text = ds.Tables[0].Rows[0][29].ToString();
        code4.Text = ds.Tables[0].Rows[0][30].ToString();
        code5.Text = ds.Tables[0].Rows[0][31].ToString();
        code6.Text = ds.Tables[0].Rows[0][32].ToString();
        code7.Text = ds.Tables[0].Rows[0][33].ToString();
        code8.Text = ds.Tables[0].Rows[0][34].ToString();
        code9.Text = ds.Tables[0].Rows[0][35].ToString();
        code10.Text = ds.Tables[0].Rows[0][36].ToString();
        code11.Text = ds.Tables[0].Rows[0][37].ToString();
        code12.Text = ds.Tables[0].Rows[0][38].ToString();
        code13.Text = ds.Tables[0].Rows[0][39].ToString();
        code14.Text = ds.Tables[0].Rows[0][40].ToString();
        code15.Text = ds.Tables[0].Rows[0][41].ToString();

        dscr1.Text = ds.Tables[0].Rows[0][42].ToString();
        dscr2.Text = ds.Tables[0].Rows[0][43].ToString();
        dscr3.Text = ds.Tables[0].Rows[0][44].ToString();
        dscr4.Text = ds.Tables[0].Rows[0][45].ToString();
        dscr5.Text = ds.Tables[0].Rows[0][46].ToString();
        dscr6.Text = ds.Tables[0].Rows[0][47].ToString();
        dscr7.Text = ds.Tables[0].Rows[0][48].ToString();
        dscr8.Text = ds.Tables[0].Rows[0][49].ToString();
        dscr9.Text = ds.Tables[0].Rows[0][50].ToString();
        dscr10.Text = ds.Tables[0].Rows[0][51].ToString();
        dscr11.Text = ds.Tables[0].Rows[0][52].ToString();
        dscr12.Text = ds.Tables[0].Rows[0][53].ToString();
        dscr13.Text = ds.Tables[0].Rows[0][54].ToString();
        dscr14.Text = ds.Tables[0].Rows[0][55].ToString();
        dscr15.Text = ds.Tables[0].Rows[0][56].ToString();

        per1.Text = ds.Tables[0].Rows[0][57].ToString();
        per2.Text = ds.Tables[0].Rows[0][58].ToString();
        per3.Text = ds.Tables[0].Rows[0][59].ToString();
        per4.Text = ds.Tables[0].Rows[0][60].ToString();
        per5.Text = ds.Tables[0].Rows[0][61].ToString();
        per6.Text = ds.Tables[0].Rows[0][62].ToString();
        per7.Text = ds.Tables[0].Rows[0][63].ToString();
        per8.Text = ds.Tables[0].Rows[0][64].ToString();
        per9.Text = ds.Tables[0].Rows[0][65].ToString();
        per10.Text = ds.Tables[0].Rows[0][66].ToString();
        per11.Text = ds.Tables[0].Rows[0][67].ToString();
        per12.Text = ds.Tables[0].Rows[0][68].ToString();
        per13.Text = ds.Tables[0].Rows[0][69].ToString();
        per14.Text = ds.Tables[0].Rows[0][70].ToString();
        per15.Text = ds.Tables[0].Rows[0][71].ToString();

        side1.Text = ds.Tables[0].Rows[0][122].ToString();
        side2.Text = ds.Tables[0].Rows[0][123].ToString();
        side3.Text = ds.Tables[0].Rows[0][124].ToString();
        side4.Text = ds.Tables[0].Rows[0][125].ToString();
        side5.Text = ds.Tables[0].Rows[0][126].ToString();
        side6.Text = ds.Tables[0].Rows[0][127].ToString();
        side7.Text = ds.Tables[0].Rows[0][128].ToString();
        side8.Text = ds.Tables[0].Rows[0][129].ToString();
        side9.Text = ds.Tables[0].Rows[0][130].ToString();
        side10.Text = ds.Tables[0].Rows[0][131].ToString();
        side11.Text = ds.Tables[0].Rows[0][132].ToString();
        side12.Text = ds.Tables[0].Rows[0][133].ToString();
        side13.Text = ds.Tables[0].Rows[0][134].ToString();
        side14.Text = ds.Tables[0].Rows[0][135].ToString();
        side15.Text = ds.Tables[0].Rows[0][136].ToString();

        coatpass.Focus();

        Page.ClientScript.RegisterStartupScript(this.GetType(), "alert", "getSelect('vCoatPassTextBox');", true);
    }

    protected void coatpasstextchanged(object sender, EventArgs e)
    {
        TextBox est = (TextBox)FormView_Fold.FindControl("vEstNumTextBox");
        Label formno = (Label)FormView_Fold.FindControl("vFormNoTextBox");
        TextBox color = (TextBox)FormView_Fold.FindControl("vColorTextBox");
        TextBox pass = (TextBox)FormView_Fold.FindControl("vPassesTextBox");
        TextBox coat = (TextBox)FormView_Fold.FindControl("vCoatTextBox");
        TextBox coatpass = (TextBox)FormView_Fold.FindControl("vCoatPassTextBox");
        TextBox dscr = (TextBox)FormView_Fold.FindControl("vDscrTextBox");
        TextBox ps1 = (TextBox)FormView_Fold.FindControl("vPs1TextBox");
        TextBox ps2 = (TextBox)FormView_Fold.FindControl("vPs2TextBox");
        TextBox ps3 = (TextBox)FormView_Fold.FindControl("vPs3TextBox");
        TextBox ps4 = (TextBox)FormView_Fold.FindControl("vPs4TextBox");
        TextBox ps5 = (TextBox)FormView_Fold.FindControl("vPs5TextBox");
        TextBox ps6 = (TextBox)FormView_Fold.FindControl("vPs6TextBox");
        TextBox ps7 = (TextBox)FormView_Fold.FindControl("vPs7TextBox");
        TextBox ps8 = (TextBox)FormView_Fold.FindControl("vPs8TextBox");
        TextBox ps9 = (TextBox)FormView_Fold.FindControl("vPs9TextBox");
        TextBox ps10 = (TextBox)FormView_Fold.FindControl("vPs10TextBox");
        TextBox ps11 = (TextBox)FormView_Fold.FindControl("vPs11TextBox");
        TextBox ps12 = (TextBox)FormView_Fold.FindControl("vPs12TextBox");
        TextBox ps13 = (TextBox)FormView_Fold.FindControl("vPs13TextBox");
        TextBox ps14 = (TextBox)FormView_Fold.FindControl("vPs14TextBox");
        TextBox ps15 = (TextBox)FormView_Fold.FindControl("vPs15TextBox");

        TextBox code1 = (TextBox)FormView_Fold.FindControl("vCode1TextBox");
        TextBox code2 = (TextBox)FormView_Fold.FindControl("vCode2TextBox");
        TextBox code3 = (TextBox)FormView_Fold.FindControl("vCode3TextBox");
        TextBox code4 = (TextBox)FormView_Fold.FindControl("vCode4TextBox");
        TextBox code5 = (TextBox)FormView_Fold.FindControl("vCode5TextBox");
        TextBox code6 = (TextBox)FormView_Fold.FindControl("vCode6TextBox");
        TextBox code7 = (TextBox)FormView_Fold.FindControl("vCode7TextBox");
        TextBox code8 = (TextBox)FormView_Fold.FindControl("vCode8TextBox");
        TextBox code9 = (TextBox)FormView_Fold.FindControl("vCode9TextBox");
        TextBox code10 = (TextBox)FormView_Fold.FindControl("vCode10TextBox");
        TextBox code11 = (TextBox)FormView_Fold.FindControl("vCode11TextBox");
        TextBox code12 = (TextBox)FormView_Fold.FindControl("vCode12TextBox");
        TextBox code13 = (TextBox)FormView_Fold.FindControl("vCode13TextBox");
        TextBox code14 = (TextBox)FormView_Fold.FindControl("vCode14TextBox");
        TextBox code15 = (TextBox)FormView_Fold.FindControl("vCode15TextBox");

        TextBox dscr1 = (TextBox)FormView_Fold.FindControl("vDscr1TextBox");
        TextBox dscr2 = (TextBox)FormView_Fold.FindControl("vDscr2TextBox");
        TextBox dscr3 = (TextBox)FormView_Fold.FindControl("vDscr3TextBox");
        TextBox dscr4 = (TextBox)FormView_Fold.FindControl("vDscr4TextBox");
        TextBox dscr5 = (TextBox)FormView_Fold.FindControl("vDscr5TextBox");
        TextBox dscr6 = (TextBox)FormView_Fold.FindControl("vDscr6TextBox");
        TextBox dscr7 = (TextBox)FormView_Fold.FindControl("vDscr7TextBox");
        TextBox dscr8 = (TextBox)FormView_Fold.FindControl("vDscr8TextBox");
        TextBox dscr9 = (TextBox)FormView_Fold.FindControl("vDscr9TextBox");
        TextBox dscr10 = (TextBox)FormView_Fold.FindControl("vDscr10TextBox");
        TextBox dscr11 = (TextBox)FormView_Fold.FindControl("vDscr11TextBox");
        TextBox dscr12 = (TextBox)FormView_Fold.FindControl("vDscr12TextBox");
        TextBox dscr13 = (TextBox)FormView_Fold.FindControl("vDscr13TextBox");
        TextBox dscr14 = (TextBox)FormView_Fold.FindControl("vDscr14TextBox");
        TextBox dscr15 = (TextBox)FormView_Fold.FindControl("vDscr15TextBox");

        TextBox per1 = (TextBox)FormView_Fold.FindControl("vPer1TextBox");
        TextBox per2 = (TextBox)FormView_Fold.FindControl("vPer2TextBox");
        TextBox per3 = (TextBox)FormView_Fold.FindControl("vPer3TextBox");
        TextBox per4 = (TextBox)FormView_Fold.FindControl("vPer4TextBox");
        TextBox per5 = (TextBox)FormView_Fold.FindControl("vPer5TextBox");
        TextBox per6 = (TextBox)FormView_Fold.FindControl("vPer6TextBox");
        TextBox per7 = (TextBox)FormView_Fold.FindControl("vPer7TextBox");
        TextBox per8 = (TextBox)FormView_Fold.FindControl("vPer8TextBox");
        TextBox per9 = (TextBox)FormView_Fold.FindControl("vPer9TextBox");
        TextBox per10 = (TextBox)FormView_Fold.FindControl("vPer10TextBox");
        TextBox per11 = (TextBox)FormView_Fold.FindControl("vPer11TextBox");
        TextBox per12 = (TextBox)FormView_Fold.FindControl("vPer12TextBox");
        TextBox per13 = (TextBox)FormView_Fold.FindControl("vPer13TextBox");
        TextBox per14 = (TextBox)FormView_Fold.FindControl("vPer14TextBox");
        TextBox per15 = (TextBox)FormView_Fold.FindControl("vPer15TextBox");
        TextBox unit1 = (TextBox)FormView_Fold.FindControl("vUnit1TextBox");
        TextBox unit2 = (TextBox)FormView_Fold.FindControl("vUnit2TextBox");
        TextBox unit3 = (TextBox)FormView_Fold.FindControl("vUnit3TextBox");
        TextBox unit4 = (TextBox)FormView_Fold.FindControl("vUnit4TextBox");
        TextBox unit5 = (TextBox)FormView_Fold.FindControl("vUnit5TextBox");
        TextBox unit6 = (TextBox)FormView_Fold.FindControl("vUnit6TextBox");
        TextBox unit7 = (TextBox)FormView_Fold.FindControl("vUnit7TextBox");
        TextBox unit8 = (TextBox)FormView_Fold.FindControl("vUnit8TextBox");
        TextBox unit9 = (TextBox)FormView_Fold.FindControl("vUnit9TextBox");
        TextBox unit10 = (TextBox)FormView_Fold.FindControl("vUnit10TextBox");
        TextBox unit11 = (TextBox)FormView_Fold.FindControl("vUnit11TextBox");
        TextBox unit12 = (TextBox)FormView_Fold.FindControl("vUnit12TextBox");
        TextBox unit13 = (TextBox)FormView_Fold.FindControl("vUnit13TextBox");
        TextBox unit14 = (TextBox)FormView_Fold.FindControl("vUnit14TextBox");
        TextBox unit15 = (TextBox)FormView_Fold.FindControl("vUnit15TextBox");

        TextBox side1  = (TextBox)FormView_Fold.FindControl("vSide1TextBox");
        TextBox side2  = (TextBox)FormView_Fold.FindControl("vSide2TextBox");
        TextBox side3  = (TextBox)FormView_Fold.FindControl("vSide3TextBox");
        TextBox side4  = (TextBox)FormView_Fold.FindControl("vSide4TextBox");
        TextBox side5  = (TextBox)FormView_Fold.FindControl("vSide5TextBox");
        TextBox side6  = (TextBox)FormView_Fold.FindControl("vSide6TextBox");
        TextBox side7  = (TextBox)FormView_Fold.FindControl("vSide7TextBox");
        TextBox side8  = (TextBox)FormView_Fold.FindControl("vSide8TextBox");
        TextBox side9  = (TextBox)FormView_Fold.FindControl("vSide9TextBox");
        TextBox side10 = (TextBox)FormView_Fold.FindControl("vSide10TextBox");
        TextBox side11 = (TextBox)FormView_Fold.FindControl("vSide11TextBox");
        TextBox side12 = (TextBox)FormView_Fold.FindControl("vSide12TextBox");
        TextBox side13 = (TextBox)FormView_Fold.FindControl("vSide13TextBox");
        TextBox side14 = (TextBox)FormView_Fold.FindControl("vSide14TextBox");
        TextBox side15 = (TextBox)FormView_Fold.FindControl("vSide15TextBox");

        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];

        Corrugated corr = new Corrugated();
        DataSet ds = new DataSet();
        ds = corr.FoldInks(UserLogin.UserName, "CoatPassChange", "", est.Text.Trim(), Convert.ToInt32(formno.Text.Trim()), Convert.ToInt32(color.Text.Trim()), Convert.ToInt32(pass.Text.Trim()), Convert.ToInt32(coat.Text.Trim()), Convert.ToInt32(coatpass.Text.Trim()), dscr.Text.Trim(), Convert.ToInt32(ps1.Text.Trim()), Convert.ToInt32(ps2.Text.Trim()), Convert.ToInt32(ps3.Text.Trim()), Convert.ToInt32(ps4.Text.Trim()), Convert.ToInt32(ps5.Text.Trim()), Convert.ToInt32(ps6.Text.Trim()), Convert.ToInt32(ps7.Text.Trim()), Convert.ToInt32(ps8.Text.Trim()), Convert.ToInt32(ps9.Text.Trim()), Convert.ToInt32(ps10.Text.Trim()), Convert.ToInt32(ps11.Text.Trim()), Convert.ToInt32(ps12.Text.Trim()), Convert.ToInt32(ps13.Text.Trim()), Convert.ToInt32(ps14.Text.Trim()), Convert.ToInt32(ps15.Text.Trim()), code1.Text.Trim(), code2.Text.Trim(), code3.Text.Trim(), code4.Text.Trim(), code5.Text.Trim(), code6.Text.Trim(), code7.Text.Trim(), code8.Text.Trim(), code9.Text.Trim(), code10.Text.Trim(), code11.Text.Trim(), code12.Text.Trim(), code13.Text.Trim(), code14.Text.Trim(), code15.Text.Trim(), dscr1.Text.Trim(), dscr2.Text.Trim(), dscr3.Text.Trim(), dscr4.Text.Trim(), dscr5.Text.Trim(), dscr6.Text.Trim(), dscr7.Text.Trim(), dscr8.Text.Trim(), dscr9.Text.Trim(), dscr10.Text.Trim(), dscr11.Text.Trim(), dscr12.Text.Trim(), dscr13.Text.Trim(), dscr14.Text.Trim(), dscr15.Text.Trim(), Convert.ToInt32(per1.Text.Trim()), Convert.ToInt32(per2.Text.Trim()), Convert.ToInt32(per3.Text.Trim()), Convert.ToInt32(per4.Text.Trim()), Convert.ToInt32(per5.Text.Trim()), Convert.ToInt32(per6.Text.Trim()), Convert.ToInt32(per7.Text.Trim()), Convert.ToInt32(per8.Text.Trim()), Convert.ToInt32(per9.Text.Trim()), Convert.ToInt32(per10.Text.Trim()), Convert.ToInt32(per11.Text.Trim()), Convert.ToInt32(per12.Text.Trim()), Convert.ToInt32(per13.Text.Trim()), Convert.ToInt32(per14.Text.Trim()), Convert.ToInt32(per15.Text.Trim()), Convert.ToDecimal(unit1.Text.Trim()), Convert.ToDecimal(unit2.Text.Trim()), Convert.ToDecimal(unit3.Text.Trim()), Convert.ToDecimal(unit4.Text.Trim()), Convert.ToDecimal(unit5.Text.Trim()), Convert.ToDecimal(unit6.Text.Trim()), Convert.ToDecimal(unit7.Text.Trim()), Convert.ToDecimal(unit8.Text.Trim()), Convert.ToDecimal(unit9.Text.Trim()), Convert.ToDecimal(unit10.Text.Trim()), Convert.ToDecimal(unit11.Text.Trim()), Convert.ToDecimal(unit12.Text.Trim()), Convert.ToDecimal(unit13.Text.Trim()), Convert.ToDecimal(unit14.Text.Trim()), Convert.ToDecimal(unit15.Text.Trim()), "", 0, 0, 0, "", 0, 0, 0, "", 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, "", 0, 0, 0, 0, 0, 0, "", "", 0, "", "", "", 0, 0, Convert.ToInt32(Session["order_folding_blankno"]), side1.Text.Trim(), side2.Text.Trim(), side3.Text.Trim(), side4.Text.Trim(), side5.Text.Trim(), side6.Text.Trim(), side7.Text.Trim(), side8.Text.Trim(), side9.Text.Trim(), side10.Text.Trim(), side11.Text.Trim(), side12.Text.Trim(), side13.Text.Trim(), side14.Text.Trim(), side15.Text.Trim());
        
        ps1.Text = ds.Tables[0].Rows[0][12].ToString();
        ps2.Text = ds.Tables[0].Rows[0][13].ToString();
        ps3.Text = ds.Tables[0].Rows[0][14].ToString();
        ps4.Text = ds.Tables[0].Rows[0][15].ToString();
        ps5.Text = ds.Tables[0].Rows[0][16].ToString();
        ps6.Text = ds.Tables[0].Rows[0][17].ToString();
        ps7.Text = ds.Tables[0].Rows[0][18].ToString();
        ps8.Text = ds.Tables[0].Rows[0][19].ToString();
        ps9.Text = ds.Tables[0].Rows[0][20].ToString();
        ps10.Text = ds.Tables[0].Rows[0][21].ToString();
        ps11.Text = ds.Tables[0].Rows[0][22].ToString();
        ps12.Text = ds.Tables[0].Rows[0][23].ToString();
        ps13.Text = ds.Tables[0].Rows[0][24].ToString();
        ps14.Text = ds.Tables[0].Rows[0][25].ToString();
        ps15.Text = ds.Tables[0].Rows[0][26].ToString();

        code1.Text = ds.Tables[0].Rows[0][27].ToString();
        code2.Text = ds.Tables[0].Rows[0][28].ToString();
        code3.Text = ds.Tables[0].Rows[0][29].ToString();
        code4.Text = ds.Tables[0].Rows[0][30].ToString();
        code5.Text = ds.Tables[0].Rows[0][31].ToString();
        code6.Text = ds.Tables[0].Rows[0][32].ToString();
        code7.Text = ds.Tables[0].Rows[0][33].ToString();
        code8.Text = ds.Tables[0].Rows[0][34].ToString();
        code9.Text = ds.Tables[0].Rows[0][35].ToString();
        code10.Text = ds.Tables[0].Rows[0][36].ToString();
        code11.Text = ds.Tables[0].Rows[0][37].ToString();
        code12.Text = ds.Tables[0].Rows[0][38].ToString();
        code13.Text = ds.Tables[0].Rows[0][39].ToString();
        code14.Text = ds.Tables[0].Rows[0][40].ToString();
        code15.Text = ds.Tables[0].Rows[0][41].ToString();

        dscr1.Text = ds.Tables[0].Rows[0][42].ToString();
        dscr2.Text = ds.Tables[0].Rows[0][43].ToString();
        dscr3.Text = ds.Tables[0].Rows[0][44].ToString();
        dscr4.Text = ds.Tables[0].Rows[0][45].ToString();
        dscr5.Text = ds.Tables[0].Rows[0][46].ToString();
        dscr6.Text = ds.Tables[0].Rows[0][47].ToString();
        dscr7.Text = ds.Tables[0].Rows[0][48].ToString();
        dscr8.Text = ds.Tables[0].Rows[0][49].ToString();
        dscr9.Text = ds.Tables[0].Rows[0][50].ToString();
        dscr10.Text = ds.Tables[0].Rows[0][51].ToString();
        dscr11.Text = ds.Tables[0].Rows[0][52].ToString();
        dscr12.Text = ds.Tables[0].Rows[0][53].ToString();
        dscr13.Text = ds.Tables[0].Rows[0][54].ToString();
        dscr14.Text = ds.Tables[0].Rows[0][55].ToString();
        dscr15.Text = ds.Tables[0].Rows[0][56].ToString();

        per1.Text = ds.Tables[0].Rows[0][57].ToString();
        per2.Text = ds.Tables[0].Rows[0][58].ToString();
        per3.Text = ds.Tables[0].Rows[0][59].ToString();
        per4.Text = ds.Tables[0].Rows[0][60].ToString();
        per5.Text = ds.Tables[0].Rows[0][61].ToString();
        per6.Text = ds.Tables[0].Rows[0][62].ToString();
        per7.Text = ds.Tables[0].Rows[0][63].ToString();
        per8.Text = ds.Tables[0].Rows[0][64].ToString();
        per9.Text = ds.Tables[0].Rows[0][65].ToString();
        per10.Text = ds.Tables[0].Rows[0][66].ToString();
        per11.Text = ds.Tables[0].Rows[0][67].ToString();
        per12.Text = ds.Tables[0].Rows[0][68].ToString();
        per13.Text = ds.Tables[0].Rows[0][69].ToString();
        per14.Text = ds.Tables[0].Rows[0][70].ToString();
        per15.Text = ds.Tables[0].Rows[0][71].ToString();

        side1.Text = ds.Tables[0].Rows[0][122].ToString();
        side2.Text = ds.Tables[0].Rows[0][123].ToString();
        side3.Text = ds.Tables[0].Rows[0][124].ToString();
        side4.Text = ds.Tables[0].Rows[0][125].ToString();
        side5.Text = ds.Tables[0].Rows[0][126].ToString();
        side6.Text = ds.Tables[0].Rows[0][127].ToString();
        side7.Text = ds.Tables[0].Rows[0][128].ToString();
        side8.Text = ds.Tables[0].Rows[0][129].ToString();
        side9.Text = ds.Tables[0].Rows[0][130].ToString();
        side10.Text = ds.Tables[0].Rows[0][131].ToString();
        side11.Text = ds.Tables[0].Rows[0][132].ToString();
        side12.Text = ds.Tables[0].Rows[0][133].ToString();
        side13.Text = ds.Tables[0].Rows[0][134].ToString();
        side14.Text = ds.Tables[0].Rows[0][135].ToString();
        side15.Text = ds.Tables[0].Rows[0][136].ToString();

        dscr.Focus();

        Page.ClientScript.RegisterStartupScript(this.GetType(), "alert", "getSelect('vDscrTextBox');", true);
    }

    protected void passestextchanged(object sender, EventArgs e)
    {
        TextBox est = (TextBox)FormView_Fold.FindControl("vEstNumTextBox");
        Label formno = (Label)FormView_Fold.FindControl("vFormNoTextBox");
        TextBox color = (TextBox)FormView_Fold.FindControl("vColorTextBox");
        TextBox pass = (TextBox)FormView_Fold.FindControl("vPassesTextBox");
        TextBox coat = (TextBox)FormView_Fold.FindControl("vCoatTextBox");
        TextBox coatpass = (TextBox)FormView_Fold.FindControl("vCoatPassTextBox");
        TextBox dscr = (TextBox)FormView_Fold.FindControl("vDscrTextBox");
        TextBox ps1 = (TextBox)FormView_Fold.FindControl("vPs1TextBox");
        TextBox ps2 = (TextBox)FormView_Fold.FindControl("vPs2TextBox");
        TextBox ps3 = (TextBox)FormView_Fold.FindControl("vPs3TextBox");
        TextBox ps4 = (TextBox)FormView_Fold.FindControl("vPs4TextBox");
        TextBox ps5 = (TextBox)FormView_Fold.FindControl("vPs5TextBox");
        TextBox ps6 = (TextBox)FormView_Fold.FindControl("vPs6TextBox");
        TextBox ps7 = (TextBox)FormView_Fold.FindControl("vPs7TextBox");
        TextBox ps8 = (TextBox)FormView_Fold.FindControl("vPs8TextBox");
        TextBox ps9 = (TextBox)FormView_Fold.FindControl("vPs9TextBox");
        TextBox ps10 = (TextBox)FormView_Fold.FindControl("vPs10TextBox");
        TextBox ps11 = (TextBox)FormView_Fold.FindControl("vPs11TextBox");
        TextBox ps12 = (TextBox)FormView_Fold.FindControl("vPs12TextBox");
        TextBox ps13 = (TextBox)FormView_Fold.FindControl("vPs13TextBox");
        TextBox ps14 = (TextBox)FormView_Fold.FindControl("vPs14TextBox");
        TextBox ps15 = (TextBox)FormView_Fold.FindControl("vPs15TextBox");

        TextBox code1 = (TextBox)FormView_Fold.FindControl("vCode1TextBox");
        TextBox code2 = (TextBox)FormView_Fold.FindControl("vCode2TextBox");
        TextBox code3 = (TextBox)FormView_Fold.FindControl("vCode3TextBox");
        TextBox code4 = (TextBox)FormView_Fold.FindControl("vCode4TextBox");
        TextBox code5 = (TextBox)FormView_Fold.FindControl("vCode5TextBox");
        TextBox code6 = (TextBox)FormView_Fold.FindControl("vCode6TextBox");
        TextBox code7 = (TextBox)FormView_Fold.FindControl("vCode7TextBox");
        TextBox code8 = (TextBox)FormView_Fold.FindControl("vCode8TextBox");
        TextBox code9 = (TextBox)FormView_Fold.FindControl("vCode9TextBox");
        TextBox code10 = (TextBox)FormView_Fold.FindControl("vCode10TextBox");
        TextBox code11 = (TextBox)FormView_Fold.FindControl("vCode11TextBox");
        TextBox code12 = (TextBox)FormView_Fold.FindControl("vCode12TextBox");
        TextBox code13 = (TextBox)FormView_Fold.FindControl("vCode13TextBox");
        TextBox code14 = (TextBox)FormView_Fold.FindControl("vCode14TextBox");
        TextBox code15 = (TextBox)FormView_Fold.FindControl("vCode15TextBox");

        TextBox dscr1 = (TextBox)FormView_Fold.FindControl("vDscr1TextBox");
        TextBox dscr2 = (TextBox)FormView_Fold.FindControl("vDscr2TextBox");
        TextBox dscr3 = (TextBox)FormView_Fold.FindControl("vDscr3TextBox");
        TextBox dscr4 = (TextBox)FormView_Fold.FindControl("vDscr4TextBox");
        TextBox dscr5 = (TextBox)FormView_Fold.FindControl("vDscr5TextBox");
        TextBox dscr6 = (TextBox)FormView_Fold.FindControl("vDscr6TextBox");
        TextBox dscr7 = (TextBox)FormView_Fold.FindControl("vDscr7TextBox");
        TextBox dscr8 = (TextBox)FormView_Fold.FindControl("vDscr8TextBox");
        TextBox dscr9 = (TextBox)FormView_Fold.FindControl("vDscr9TextBox");
        TextBox dscr10 = (TextBox)FormView_Fold.FindControl("vDscr10TextBox");
        TextBox dscr11 = (TextBox)FormView_Fold.FindControl("vDscr11TextBox");
        TextBox dscr12 = (TextBox)FormView_Fold.FindControl("vDscr12TextBox");
        TextBox dscr13 = (TextBox)FormView_Fold.FindControl("vDscr13TextBox");
        TextBox dscr14 = (TextBox)FormView_Fold.FindControl("vDscr14TextBox");
        TextBox dscr15 = (TextBox)FormView_Fold.FindControl("vDscr15TextBox");

        TextBox per1 = (TextBox)FormView_Fold.FindControl("vPer1TextBox");
        TextBox per2 = (TextBox)FormView_Fold.FindControl("vPer2TextBox");
        TextBox per3 = (TextBox)FormView_Fold.FindControl("vPer3TextBox");
        TextBox per4 = (TextBox)FormView_Fold.FindControl("vPer4TextBox");
        TextBox per5 = (TextBox)FormView_Fold.FindControl("vPer5TextBox");
        TextBox per6 = (TextBox)FormView_Fold.FindControl("vPer6TextBox");
        TextBox per7 = (TextBox)FormView_Fold.FindControl("vPer7TextBox");
        TextBox per8 = (TextBox)FormView_Fold.FindControl("vPer8TextBox");
        TextBox per9 = (TextBox)FormView_Fold.FindControl("vPer9TextBox");
        TextBox per10 = (TextBox)FormView_Fold.FindControl("vPer10TextBox");
        TextBox per11 = (TextBox)FormView_Fold.FindControl("vPer11TextBox");
        TextBox per12 = (TextBox)FormView_Fold.FindControl("vPer12TextBox");
        TextBox per13 = (TextBox)FormView_Fold.FindControl("vPer13TextBox");
        TextBox per14 = (TextBox)FormView_Fold.FindControl("vPer14TextBox");
        TextBox per15 = (TextBox)FormView_Fold.FindControl("vPer15TextBox");
        TextBox unit1 = (TextBox)FormView_Fold.FindControl("vUnit1TextBox");
        TextBox unit2 = (TextBox)FormView_Fold.FindControl("vUnit2TextBox");
        TextBox unit3 = (TextBox)FormView_Fold.FindControl("vUnit3TextBox");
        TextBox unit4 = (TextBox)FormView_Fold.FindControl("vUnit4TextBox");
        TextBox unit5 = (TextBox)FormView_Fold.FindControl("vUnit5TextBox");
        TextBox unit6 = (TextBox)FormView_Fold.FindControl("vUnit6TextBox");
        TextBox unit7 = (TextBox)FormView_Fold.FindControl("vUnit7TextBox");
        TextBox unit8 = (TextBox)FormView_Fold.FindControl("vUnit8TextBox");
        TextBox unit9 = (TextBox)FormView_Fold.FindControl("vUnit9TextBox");
        TextBox unit10 = (TextBox)FormView_Fold.FindControl("vUnit10TextBox");
        TextBox unit11 = (TextBox)FormView_Fold.FindControl("vUnit11TextBox");
        TextBox unit12 = (TextBox)FormView_Fold.FindControl("vUnit12TextBox");
        TextBox unit13 = (TextBox)FormView_Fold.FindControl("vUnit13TextBox");
        TextBox unit14 = (TextBox)FormView_Fold.FindControl("vUnit14TextBox");
        TextBox unit15 = (TextBox)FormView_Fold.FindControl("vUnit15TextBox");

        TextBox side1 = (TextBox)FormView_Fold.FindControl("vSide1TextBox");
        TextBox side2 = (TextBox)FormView_Fold.FindControl("vSide2TextBox");
        TextBox side3 = (TextBox)FormView_Fold.FindControl("vSide3TextBox");
        TextBox side4 = (TextBox)FormView_Fold.FindControl("vSide4TextBox");
        TextBox side5 = (TextBox)FormView_Fold.FindControl("vSide5TextBox");
        TextBox side6 = (TextBox)FormView_Fold.FindControl("vSide6TextBox");
        TextBox side7 = (TextBox)FormView_Fold.FindControl("vSide7TextBox");
        TextBox side8 = (TextBox)FormView_Fold.FindControl("vSide8TextBox");
        TextBox side9 = (TextBox)FormView_Fold.FindControl("vSide9TextBox");
        TextBox side10 = (TextBox)FormView_Fold.FindControl("vSide10TextBox");
        TextBox side11 = (TextBox)FormView_Fold.FindControl("vSide11TextBox");
        TextBox side12 = (TextBox)FormView_Fold.FindControl("vSide12TextBox");
        TextBox side13 = (TextBox)FormView_Fold.FindControl("vSide13TextBox");
        TextBox side14 = (TextBox)FormView_Fold.FindControl("vSide14TextBox");
        TextBox side15 = (TextBox)FormView_Fold.FindControl("vSide15TextBox");

        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];

        Corrugated corr = new Corrugated();
        DataSet ds = new DataSet();
        ds = corr.FoldInks(UserLogin.UserName, "PassesChange", "", est.Text.Trim(), Convert.ToInt32(formno.Text.Trim()), Convert.ToInt32(color.Text.Trim()), Convert.ToInt32(pass.Text.Trim()), Convert.ToInt32(coat.Text.Trim()), Convert.ToInt32(coatpass.Text.Trim()), dscr.Text.Trim(), Convert.ToInt32(ps1.Text.Trim()), Convert.ToInt32(ps2.Text.Trim()), Convert.ToInt32(ps3.Text.Trim()), Convert.ToInt32(ps4.Text.Trim()), Convert.ToInt32(ps5.Text.Trim()), Convert.ToInt32(ps6.Text.Trim()), Convert.ToInt32(ps7.Text.Trim()), Convert.ToInt32(ps8.Text.Trim()), Convert.ToInt32(ps9.Text.Trim()), Convert.ToInt32(ps10.Text.Trim()), Convert.ToInt32(ps11.Text.Trim()), Convert.ToInt32(ps12.Text.Trim()), Convert.ToInt32(ps13.Text.Trim()), Convert.ToInt32(ps14.Text.Trim()), Convert.ToInt32(ps15.Text.Trim()), code1.Text.Trim(), code2.Text.Trim(), code3.Text.Trim(), code4.Text.Trim(), code5.Text.Trim(), code6.Text.Trim(), code7.Text.Trim(), code8.Text.Trim(), code9.Text.Trim(), code10.Text.Trim(), code11.Text.Trim(), code12.Text.Trim(), code13.Text.Trim(), code14.Text.Trim(), code15.Text.Trim(), dscr1.Text.Trim(), dscr2.Text.Trim(), dscr3.Text.Trim(), dscr4.Text.Trim(), dscr5.Text.Trim(), dscr6.Text.Trim(), dscr7.Text.Trim(), dscr8.Text.Trim(), dscr9.Text.Trim(), dscr10.Text.Trim(), dscr11.Text.Trim(), dscr12.Text.Trim(), dscr13.Text.Trim(), dscr14.Text.Trim(), dscr15.Text.Trim(), Convert.ToInt32(per1.Text.Trim()), Convert.ToInt32(per2.Text.Trim()), Convert.ToInt32(per3.Text.Trim()), Convert.ToInt32(per4.Text.Trim()), Convert.ToInt32(per5.Text.Trim()), Convert.ToInt32(per6.Text.Trim()), Convert.ToInt32(per7.Text.Trim()), Convert.ToInt32(per8.Text.Trim()), Convert.ToInt32(per9.Text.Trim()), Convert.ToInt32(per10.Text.Trim()), Convert.ToInt32(per11.Text.Trim()), Convert.ToInt32(per12.Text.Trim()), Convert.ToInt32(per13.Text.Trim()), Convert.ToInt32(per14.Text.Trim()), Convert.ToInt32(per15.Text.Trim()), Convert.ToDecimal(unit1.Text.Trim()), Convert.ToDecimal(unit2.Text.Trim()), Convert.ToDecimal(unit3.Text.Trim()), Convert.ToDecimal(unit4.Text.Trim()), Convert.ToDecimal(unit5.Text.Trim()), Convert.ToDecimal(unit6.Text.Trim()), Convert.ToDecimal(unit7.Text.Trim()), Convert.ToDecimal(unit8.Text.Trim()), Convert.ToDecimal(unit9.Text.Trim()), Convert.ToDecimal(unit10.Text.Trim()), Convert.ToDecimal(unit11.Text.Trim()), Convert.ToDecimal(unit12.Text.Trim()), Convert.ToDecimal(unit13.Text.Trim()), Convert.ToDecimal(unit14.Text.Trim()), Convert.ToDecimal(unit15.Text.Trim()), "", 0, 0, 0, "", 0, 0, 0, "", 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, "", 0, 0, 0, 0, 0, 0, "", "", 0, "", "", "", 0, 0, Convert.ToInt32(Session["order_folding_blankno"]), side1.Text.Trim(), side2.Text.Trim(), side3.Text.Trim(), side4.Text.Trim(), side5.Text.Trim(), side6.Text.Trim(), side7.Text.Trim(), side8.Text.Trim(), side9.Text.Trim(), side10.Text.Trim(), side11.Text.Trim(), side12.Text.Trim(), side13.Text.Trim(), side14.Text.Trim(), side15.Text.Trim());
      
        ps1.Text = ds.Tables[0].Rows[0][12].ToString();
        ps2.Text = ds.Tables[0].Rows[0][13].ToString();
        ps3.Text = ds.Tables[0].Rows[0][14].ToString();
        ps4.Text = ds.Tables[0].Rows[0][15].ToString();
        ps5.Text = ds.Tables[0].Rows[0][16].ToString();
        ps6.Text = ds.Tables[0].Rows[0][17].ToString();
        ps7.Text = ds.Tables[0].Rows[0][18].ToString();
        ps8.Text = ds.Tables[0].Rows[0][19].ToString();
        ps9.Text = ds.Tables[0].Rows[0][20].ToString();
        ps10.Text = ds.Tables[0].Rows[0][21].ToString();
        ps11.Text = ds.Tables[0].Rows[0][22].ToString();
        ps12.Text = ds.Tables[0].Rows[0][23].ToString();
        ps13.Text = ds.Tables[0].Rows[0][24].ToString();
        ps14.Text = ds.Tables[0].Rows[0][25].ToString();
        ps15.Text = ds.Tables[0].Rows[0][26].ToString();

        code1.Text = ds.Tables[0].Rows[0][27].ToString();
        code2.Text = ds.Tables[0].Rows[0][28].ToString();
        code3.Text = ds.Tables[0].Rows[0][29].ToString();
        code4.Text = ds.Tables[0].Rows[0][30].ToString();
        code5.Text = ds.Tables[0].Rows[0][31].ToString();
        code6.Text = ds.Tables[0].Rows[0][32].ToString();
        code7.Text = ds.Tables[0].Rows[0][33].ToString();
        code8.Text = ds.Tables[0].Rows[0][34].ToString();
        code9.Text = ds.Tables[0].Rows[0][35].ToString();
        code10.Text = ds.Tables[0].Rows[0][36].ToString();
        code11.Text = ds.Tables[0].Rows[0][37].ToString();
        code12.Text = ds.Tables[0].Rows[0][38].ToString();
        code13.Text = ds.Tables[0].Rows[0][39].ToString();
        code14.Text = ds.Tables[0].Rows[0][40].ToString();
        code15.Text = ds.Tables[0].Rows[0][41].ToString();

        dscr1.Text = ds.Tables[0].Rows[0][42].ToString();
        dscr2.Text = ds.Tables[0].Rows[0][43].ToString();
        dscr3.Text = ds.Tables[0].Rows[0][44].ToString();
        dscr4.Text = ds.Tables[0].Rows[0][45].ToString();
        dscr5.Text = ds.Tables[0].Rows[0][46].ToString();
        dscr6.Text = ds.Tables[0].Rows[0][47].ToString();
        dscr7.Text = ds.Tables[0].Rows[0][48].ToString();
        dscr8.Text = ds.Tables[0].Rows[0][49].ToString();
        dscr9.Text = ds.Tables[0].Rows[0][50].ToString();
        dscr10.Text = ds.Tables[0].Rows[0][51].ToString();
        dscr11.Text = ds.Tables[0].Rows[0][52].ToString();
        dscr12.Text = ds.Tables[0].Rows[0][53].ToString();
        dscr13.Text = ds.Tables[0].Rows[0][54].ToString();
        dscr14.Text = ds.Tables[0].Rows[0][55].ToString();
        dscr15.Text = ds.Tables[0].Rows[0][56].ToString();

        per1.Text = ds.Tables[0].Rows[0][57].ToString();
        per2.Text = ds.Tables[0].Rows[0][58].ToString();
        per3.Text = ds.Tables[0].Rows[0][59].ToString();
        per4.Text = ds.Tables[0].Rows[0][60].ToString();
        per5.Text = ds.Tables[0].Rows[0][61].ToString();
        per6.Text = ds.Tables[0].Rows[0][62].ToString();
        per7.Text = ds.Tables[0].Rows[0][63].ToString();
        per8.Text = ds.Tables[0].Rows[0][64].ToString();
        per9.Text = ds.Tables[0].Rows[0][65].ToString();
        per10.Text = ds.Tables[0].Rows[0][66].ToString();
        per11.Text = ds.Tables[0].Rows[0][67].ToString();
        per12.Text = ds.Tables[0].Rows[0][68].ToString();
        per13.Text = ds.Tables[0].Rows[0][69].ToString();
        per14.Text = ds.Tables[0].Rows[0][70].ToString();
        per15.Text = ds.Tables[0].Rows[0][71].ToString();

        side1.Text = ds.Tables[0].Rows[0][122].ToString();
        side2.Text = ds.Tables[0].Rows[0][123].ToString();
        side3.Text = ds.Tables[0].Rows[0][124].ToString();
        side4.Text = ds.Tables[0].Rows[0][125].ToString();
        side5.Text = ds.Tables[0].Rows[0][126].ToString();
        side6.Text = ds.Tables[0].Rows[0][127].ToString();
        side7.Text = ds.Tables[0].Rows[0][128].ToString();
        side8.Text = ds.Tables[0].Rows[0][129].ToString();
        side9.Text = ds.Tables[0].Rows[0][130].ToString();
        side10.Text = ds.Tables[0].Rows[0][131].ToString();
        side11.Text = ds.Tables[0].Rows[0][132].ToString();
        side12.Text = ds.Tables[0].Rows[0][133].ToString();
        side13.Text = ds.Tables[0].Rows[0][134].ToString();
        side14.Text = ds.Tables[0].Rows[0][135].ToString();
        side15.Text = ds.Tables[0].Rows[0][136].ToString();

        coat.Focus();
        Page.ClientScript.RegisterStartupScript(this.GetType(), "alert", "getSelect('vCoatTextBox');", true);
    }

    protected void updateFoldbutton_Click(object sender, EventArgs e)
    {
        Session["Fold_update_button"] = 1;
    }
    protected void overwrite_Click(object sender, EventArgs e)
    {
        Session["Fold_update_button"] = null;
    }

    protected void FormView_Fold_DataBound(object sender, EventArgs e)
    {
        if (FormView_Fold.CurrentMode == FormViewMode.ReadOnly)
        {
            UserClass UserLogin = (UserClass)Session["User"];
            Button job = (Button)FormView_Fold.FindControl("jobButton");
            if (Session["User"] != null)
            {
                string vUserId = UserLogin.UserName;
                string vPage = "fold_inks.aspx";
                string aUsers = null;
                string PrmComp = null;
                bool vCanCreate = false;
                bool vCanRun = false;
                bool vCanUpdate = false;
                bool vCanDelete = false;


                func1 f1 = new func1();
                //Response.Write(Page);
                f1.CheckProgramPermissions(vPage, vUserId, ref  vCanCreate, ref  vCanRun, ref  vCanUpdate, ref  vCanDelete, ref  PrmComp, ref  aUsers);

                if (aUsers == "external")
                {
                    job.Visible = false;
                }                
            }
        }
        if (FormView_Fold.CurrentMode == FormViewMode.Edit)
        {
            TextBox color = (TextBox)FormView_Fold.FindControl("vColorTextBox");
            TextBox pass = (TextBox)FormView_Fold.FindControl("vPassesTextBox");
            TextBox coat = (TextBox)FormView_Fold.FindControl("vCoatTextBox");
            TextBox coatpass = (TextBox)FormView_Fold.FindControl("vCoatPassTextBox");
            TextBox dscr=(TextBox)FormView_Fold.FindControl("vDscrTextBox");
            TextBox ps1 = (TextBox)FormView_Fold.FindControl("vPs1TextBox");
            TextBox ps2 = (TextBox)FormView_Fold.FindControl("vPs2TextBox");
            TextBox ps3 = (TextBox)FormView_Fold.FindControl("vPs3TextBox");
            TextBox ps4 = (TextBox)FormView_Fold.FindControl("vPs4TextBox");
            TextBox ps5 = (TextBox)FormView_Fold.FindControl("vPs5TextBox");
            TextBox ps6 = (TextBox)FormView_Fold.FindControl("vPs6TextBox");
            TextBox ps7 = (TextBox)FormView_Fold.FindControl("vPs7TextBox");
            TextBox ps8 = (TextBox)FormView_Fold.FindControl("vPs8TextBox");
            TextBox ps9 = (TextBox)FormView_Fold.FindControl("vPs9TextBox");
            TextBox ps10 = (TextBox)FormView_Fold.FindControl("vPs10TextBox");
            TextBox ps11 = (TextBox)FormView_Fold.FindControl("vPs11TextBox");
            TextBox ps12 = (TextBox)FormView_Fold.FindControl("vPs12TextBox");
            TextBox ps13 = (TextBox)FormView_Fold.FindControl("vPs13TextBox");
            TextBox ps14 = (TextBox)FormView_Fold.FindControl("vPs14TextBox");
            TextBox ps15 = (TextBox)FormView_Fold.FindControl("vPs15TextBox");
            TextBox code1 = (TextBox)FormView_Fold.FindControl("vCode1TextBox");
            TextBox code2 = (TextBox)FormView_Fold.FindControl("vCode2TextBox");
            TextBox code3 = (TextBox)FormView_Fold.FindControl("vCode3TextBox");
            TextBox code4 = (TextBox)FormView_Fold.FindControl("vCode4TextBox");
            TextBox code5 = (TextBox)FormView_Fold.FindControl("vCode5TextBox");
            TextBox code6 = (TextBox)FormView_Fold.FindControl("vCode6TextBox");
            TextBox code7 = (TextBox)FormView_Fold.FindControl("vCode7TextBox");
            TextBox code8 = (TextBox)FormView_Fold.FindControl("vCode8TextBox");
            TextBox code9 = (TextBox)FormView_Fold.FindControl("vCode9TextBox");
            TextBox code10 = (TextBox)FormView_Fold.FindControl("vCode10TextBox");
            TextBox code11 = (TextBox)FormView_Fold.FindControl("vCode11TextBox");
            TextBox code12 = (TextBox)FormView_Fold.FindControl("vCode12TextBox");
            TextBox code13 = (TextBox)FormView_Fold.FindControl("vCode13TextBox");
            TextBox code14 = (TextBox)FormView_Fold.FindControl("vCode14TextBox");
            TextBox code15 = (TextBox)FormView_Fold.FindControl("vCode15TextBox");
            TextBox dscr1 = (TextBox)FormView_Fold.FindControl("vDscr1TextBox");
            TextBox dscr2 = (TextBox)FormView_Fold.FindControl("vDscr2TextBox");
            TextBox dscr3 = (TextBox)FormView_Fold.FindControl("vDscr3TextBox");
            TextBox dscr4 = (TextBox)FormView_Fold.FindControl("vDscr4TextBox");
            TextBox dscr5 = (TextBox)FormView_Fold.FindControl("vDscr5TextBox");
            TextBox dscr6 = (TextBox)FormView_Fold.FindControl("vDscr6TextBox");
            TextBox dscr7 = (TextBox)FormView_Fold.FindControl("vDscr7TextBox");
            TextBox dscr8 = (TextBox)FormView_Fold.FindControl("vDscr8TextBox");
            TextBox dscr9 = (TextBox)FormView_Fold.FindControl("vDscr9TextBox");
            TextBox dscr10 = (TextBox)FormView_Fold.FindControl("vDscr10TextBox");
            TextBox dscr11 = (TextBox)FormView_Fold.FindControl("vDscr11TextBox");
            TextBox dscr12 = (TextBox)FormView_Fold.FindControl("vDscr12TextBox");
            TextBox dscr13 = (TextBox)FormView_Fold.FindControl("vDscr13TextBox");
            TextBox dscr14 = (TextBox)FormView_Fold.FindControl("vDscr14TextBox");
            TextBox dscr15 = (TextBox)FormView_Fold.FindControl("vDscr15TextBox");
            TextBox per1 = (TextBox)FormView_Fold.FindControl("vPer1TextBox");
            TextBox per2 = (TextBox)FormView_Fold.FindControl("vPer2TextBox");
            TextBox per3 = (TextBox)FormView_Fold.FindControl("vPer3TextBox");
            TextBox per4 = (TextBox)FormView_Fold.FindControl("vPer4TextBox");
            TextBox per5 = (TextBox)FormView_Fold.FindControl("vPer5TextBox");
            TextBox per6 = (TextBox)FormView_Fold.FindControl("vPer6TextBox");
            TextBox per7 = (TextBox)FormView_Fold.FindControl("vPer7TextBox");
            TextBox per8 = (TextBox)FormView_Fold.FindControl("vPer8TextBox");
            TextBox per9 = (TextBox)FormView_Fold.FindControl("vPer9TextBox");
            TextBox per10 = (TextBox)FormView_Fold.FindControl("vPer10TextBox");
            TextBox per11 = (TextBox)FormView_Fold.FindControl("vPer11TextBox");
            TextBox per12 = (TextBox)FormView_Fold.FindControl("vPer12TextBox");
            TextBox per13 = (TextBox)FormView_Fold.FindControl("vPer13TextBox");
            TextBox per14 = (TextBox)FormView_Fold.FindControl("vPer14TextBox");
            TextBox per15 = (TextBox)FormView_Fold.FindControl("vPer15TextBox");

            TextBox unit1 = (TextBox)FormView_Fold.FindControl("vUnit1TextBox");
            TextBox unit2 = (TextBox)FormView_Fold.FindControl("vUnit2TextBox");
            TextBox unit3 = (TextBox)FormView_Fold.FindControl("vUnit3TextBox");
            TextBox unit4 = (TextBox)FormView_Fold.FindControl("vUnit4TextBox");
            TextBox unit5 = (TextBox)FormView_Fold.FindControl("vUnit5TextBox");
            TextBox unit6 = (TextBox)FormView_Fold.FindControl("vUnit6TextBox");
            TextBox unit7 = (TextBox)FormView_Fold.FindControl("vUnit7TextBox");
            TextBox unit8 = (TextBox)FormView_Fold.FindControl("vUnit8TextBox");
            TextBox unit9 = (TextBox)FormView_Fold.FindControl("vUnit9TextBox");
            TextBox unit10 = (TextBox)FormView_Fold.FindControl("vUnit10TextBox");
            TextBox unit11 = (TextBox)FormView_Fold.FindControl("vUnit11TextBox");
            TextBox unit12 = (TextBox)FormView_Fold.FindControl("vUnit12TextBox");
            TextBox unit13 = (TextBox)FormView_Fold.FindControl("vUnit13TextBox");
            TextBox unit14 = (TextBox)FormView_Fold.FindControl("vUnit14TextBox");
            TextBox unit15 = (TextBox)FormView_Fold.FindControl("vUnit15TextBox");


            TextBox packcode = (TextBox)FormView_Fold.FindControl("vPackCodeTextBox");
            TextBox unitlen = (TextBox)FormView_Fold.FindControl("vUnitLenTextBox");
            TextBox cost1 = (TextBox)FormView_Fold.FindControl("vCostTextBox");
            TextBox unitwid = (TextBox)FormView_Fold.FindControl("vUnitWidTextBox");
            TextBox boxcode = (TextBox)FormView_Fold.FindControl("vBoxCodeTextBox");
            TextBox unitdep = (TextBox)FormView_Fold.FindControl("vUnitDepTextBox");
            TextBox bundl = (TextBox)FormView_Fold.FindControl("vBundlTextBox");
            TextBox wtpack = (TextBox)FormView_Fold.FindControl("vWtPackTextBox");
            TextBox unit = (TextBox)FormView_Fold.FindControl("vUnitTextBox");
            TextBox len = (TextBox)FormView_Fold.FindControl("vLengthTextBox");
            TextBox cost2 = (TextBox)FormView_Fold.FindControl("vCost2TextBox");
            TextBox wid = (TextBox)FormView_Fold.FindControl("vWidthTextBox");
            TextBox count = (TextBox)FormView_Fold.FindControl("vCountTextBox");
            TextBox hight = (TextBox)FormView_Fold.FindControl("vHeightTextBox");
            TextBox layer = (TextBox)FormView_Fold.FindControl("vLayerTextBox");
            
            TextBox weiper = (TextBox)FormView_Fold.FindControl("vWeiPerTextBox");
            TextBox carrier = (TextBox)FormView_Fold.FindControl("vCarrierTextBox");
            TextBox carrdscr = (TextBox)FormView_Fold.FindControl("vCarrDscrTextBox");
            TextBox delzon = (TextBox)FormView_Fold.FindControl("vDelZonTextBox");
            TextBox frcwt = (TextBox)FormView_Fold.FindControl("vFreifgtTextBox");
            TextBox frm = (TextBox)FormView_Fold.FindControl("vFreOutTextBox");
            Label packqty = (Label)FormView_Fold.FindControl("vPackQtyTextBox");
            TextBox note = (TextBox)FormView_Fold.FindControl("vNoteTextBox");
            RadioButtonList fright = (RadioButtonList)FormView_Fold.FindControl("RadioButtonList1");
            Image image1 = (Image)FormView_Fold.FindControl("Image1");
            Image image2 = (Image)FormView_Fold.FindControl("Image2");
            Image image3 = (Image)FormView_Fold.FindControl("Image3");
            Image image4 = (Image)FormView_Fold.FindControl("Image4");
            Image image5 = (Image)FormView_Fold.FindControl("Image5");
            Image image6 = (Image)FormView_Fold.FindControl("Image6");
            Image image7 = (Image)FormView_Fold.FindControl("Image7");
            Image image8 = (Image)FormView_Fold.FindControl("Image8");
            Image image9 = (Image)FormView_Fold.FindControl("Image9");
            Image image10 = (Image)FormView_Fold.FindControl("Image11");

            Image image11 = (Image)FormView_Fold.FindControl("Image10");
            Image image12 = (Image)FormView_Fold.FindControl("Image12");
            Image image13 = (Image)FormView_Fold.FindControl("Image13");
            Image image14 = (Image)FormView_Fold.FindControl("Image14");
            Image image15 = (Image)FormView_Fold.FindControl("Image15");
            Image image16 = (Image)FormView_Fold.FindControl("Image16");
            Image image17 = (Image)FormView_Fold.FindControl("Image17");
            Image image18 = (Image)FormView_Fold.FindControl("Image18");
            Image image19 = (Image)FormView_Fold.FindControl("Image19");
            Label stock = (Label)FormView_Fold.FindControl("vstockLabel");
            
            if (stock.Text == "")
            {
               
                note.Enabled = false;
            }

            if (Convert.ToInt32(Session["Fold_update_button"]) == 1)
            {
                packcode.Enabled = false;
                packcode.BackColor = System.Drawing.Color.Turquoise;
                unitlen.ReadOnly = true;
                unitlen.BackColor = System.Drawing.Color.Turquoise;
                cost1.ReadOnly = true;
                cost1.BackColor = System.Drawing.Color.Turquoise;
                unitwid.ReadOnly = true;
                unitwid.BackColor = System.Drawing.Color.Turquoise;
                boxcode.ReadOnly = true;
                boxcode.BackColor = System.Drawing.Color.Turquoise;
                unitdep.ReadOnly = true;
                unitdep.BackColor = System.Drawing.Color.Turquoise;
                bundl.ReadOnly = true;
                bundl.BackColor = System.Drawing.Color.Turquoise;
                wtpack.ReadOnly = true;
                wtpack.BackColor = System.Drawing.Color.Turquoise;
                unit.ReadOnly = true;
                unit.BackColor = System.Drawing.Color.Turquoise;
                len.ReadOnly = true;
                len.BackColor = System.Drawing.Color.Turquoise;
                cost2.ReadOnly = true;
                cost2.BackColor = System.Drawing.Color.Turquoise;
                wid.ReadOnly = true;
                wid.BackColor = System.Drawing.Color.Turquoise;
                count.ReadOnly = true;
                count.BackColor = System.Drawing.Color.Turquoise;
                hight.ReadOnly = true;
                hight.BackColor = System.Drawing.Color.Turquoise;
                layer.ReadOnly = true;
                layer.BackColor = System.Drawing.Color.Turquoise;
               
                weiper.ReadOnly = true;
                weiper.BackColor = System.Drawing.Color.Turquoise;
                carrier.ReadOnly = true;
                carrier.BackColor = System.Drawing.Color.Turquoise;
                carrdscr.ReadOnly = true;
                carrdscr.BackColor = System.Drawing.Color.Turquoise;
                delzon.ReadOnly = true;
                delzon.BackColor = System.Drawing.Color.Turquoise;
                frcwt.ReadOnly = true;
                frcwt.BackColor = System.Drawing.Color.Turquoise;
                frm.ReadOnly = true;
                frm.BackColor = System.Drawing.Color.Turquoise;
                //packqty.ReadOnly = true;
                packqty.BackColor = System.Drawing.Color.Turquoise;
                note.ReadOnly = true;
                note.BackColor = System.Drawing.Color.Turquoise;
                fright.Enabled = false;
                image11.Visible = false;
                image12.Visible = false;
                image13.Visible = false;                
                image15.Visible = false;

                color.Focus();

                Page.ClientScript.RegisterStartupScript(this.GetType(), "alert", "getSelect('vColorTextBox');", true);
            }
            else
            {
                color.Enabled = false;
                color.BackColor = System.Drawing.Color.Turquoise;
                pass.ReadOnly = true;
                pass.BackColor = System.Drawing.Color.Turquoise;
                coat.ReadOnly = true;
                coat.BackColor = System.Drawing.Color.Turquoise;
                coatpass.ReadOnly = true;
                coatpass.BackColor = System.Drawing.Color.Turquoise;
                dscr.ReadOnly = true;
                dscr.BackColor = System.Drawing.Color.Turquoise;
                ps1.ReadOnly = true;
                ps1.BackColor = System.Drawing.Color.Turquoise;
                ps2.ReadOnly = true;
                ps2.BackColor = System.Drawing.Color.Turquoise;
                ps3.ReadOnly = true;
                ps3.BackColor = System.Drawing.Color.Turquoise;
                ps4.ReadOnly = true;
                ps4.BackColor = System.Drawing.Color.Turquoise;
                ps5.ReadOnly = true;
                ps5.BackColor = System.Drawing.Color.Turquoise;
                ps6.ReadOnly = true;
                ps6.BackColor = System.Drawing.Color.Turquoise;
                ps7.ReadOnly = true;
                ps7.BackColor = System.Drawing.Color.Turquoise;
                ps8.ReadOnly = true;
                ps8.BackColor = System.Drawing.Color.Turquoise;
                ps9.ReadOnly = true;
                ps9.BackColor = System.Drawing.Color.Turquoise;
                ps10.ReadOnly = true;
                ps10.BackColor = System.Drawing.Color.Turquoise;
                code1.ReadOnly = true;
                code1.BackColor = System.Drawing.Color.Turquoise;
                code2.ReadOnly = true;
                code2.BackColor = System.Drawing.Color.Turquoise;
                code3.ReadOnly = true;
                code3.BackColor = System.Drawing.Color.Turquoise;
                code4.ReadOnly = true;
                code4.BackColor = System.Drawing.Color.Turquoise;
                code5.ReadOnly = true;
                code5.BackColor = System.Drawing.Color.Turquoise;
                code6.ReadOnly = true;
                code6.BackColor = System.Drawing.Color.Turquoise;
                code7.ReadOnly = true;
                code7.BackColor = System.Drawing.Color.Turquoise;
                code8.ReadOnly = true;
                code8.BackColor = System.Drawing.Color.Turquoise;
                code9.ReadOnly = true;
                code9.BackColor = System.Drawing.Color.Turquoise;
                code10.ReadOnly = true;
                code10.BackColor = System.Drawing.Color.Turquoise;
                dscr1.ReadOnly = true;
                dscr1.BackColor = System.Drawing.Color.Turquoise;
                dscr2.ReadOnly = true;
                dscr2.BackColor = System.Drawing.Color.Turquoise;
                dscr3.ReadOnly = true;
                dscr3.BackColor = System.Drawing.Color.Turquoise;
                dscr4.ReadOnly = true;
                dscr4.BackColor = System.Drawing.Color.Turquoise;
                dscr5.ReadOnly = true;
                dscr5.BackColor = System.Drawing.Color.Turquoise;
                dscr6.ReadOnly = true;
                dscr6.BackColor = System.Drawing.Color.Turquoise;
                dscr7.ReadOnly = true;
                dscr7.BackColor = System.Drawing.Color.Turquoise;
                dscr8.ReadOnly = true;
                dscr8.BackColor = System.Drawing.Color.Turquoise;
                dscr9.ReadOnly = true;
                dscr9.BackColor = System.Drawing.Color.Turquoise;
                dscr10.ReadOnly = true;
                dscr10.BackColor = System.Drawing.Color.Turquoise;
                per1.ReadOnly = true;
                per1.BackColor = System.Drawing.Color.Turquoise;
                per2.ReadOnly = true;
                per2.BackColor = System.Drawing.Color.Turquoise;
                per3.ReadOnly = true;
                per3.BackColor = System.Drawing.Color.Turquoise;
                per4.ReadOnly = true;
                per4.BackColor = System.Drawing.Color.Turquoise;
                per5.ReadOnly = true;
                per5.BackColor = System.Drawing.Color.Turquoise;
                per6.ReadOnly = true;
                per6.BackColor = System.Drawing.Color.Turquoise;
                per7.ReadOnly = true;
                per7.BackColor = System.Drawing.Color.Turquoise;
                per8.ReadOnly = true;
                per8.BackColor = System.Drawing.Color.Turquoise;
                per9.ReadOnly = true;
                per9.BackColor = System.Drawing.Color.Turquoise;
                per10.ReadOnly = true;
                per10.BackColor = System.Drawing.Color.Turquoise;
                ps11.ReadOnly = true;
                ps11.BackColor = System.Drawing.Color.Turquoise;
                ps12.ReadOnly = true;
                ps12.BackColor = System.Drawing.Color.Turquoise;
                ps13.ReadOnly = true;
                ps13.BackColor = System.Drawing.Color.Turquoise;
                ps14.ReadOnly = true;
                ps14.BackColor = System.Drawing.Color.Turquoise;
                ps15.ReadOnly = true;
                ps15.BackColor = System.Drawing.Color.Turquoise;
                code11.ReadOnly = true;
                code11.BackColor = System.Drawing.Color.Turquoise;
                code12.ReadOnly = true;
                code12.BackColor = System.Drawing.Color.Turquoise;
                code13.ReadOnly = true;
                code13.BackColor = System.Drawing.Color.Turquoise;
                code14.ReadOnly = true;
                code14.BackColor = System.Drawing.Color.Turquoise;
                code15.ReadOnly = true;
                code15.BackColor = System.Drawing.Color.Turquoise;
                dscr11.ReadOnly = true;
                dscr11.BackColor = System.Drawing.Color.Turquoise;
                dscr12.ReadOnly = true;
                dscr12.BackColor = System.Drawing.Color.Turquoise;
                dscr13.ReadOnly = true;
                dscr13.BackColor = System.Drawing.Color.Turquoise;
                dscr14.ReadOnly = true;
                dscr14.BackColor = System.Drawing.Color.Turquoise;
                dscr15.ReadOnly = true;
                dscr15.BackColor = System.Drawing.Color.Turquoise;
                per11.ReadOnly = true;
                per11.BackColor = System.Drawing.Color.Turquoise;
                per12.ReadOnly = true;
                per12.BackColor = System.Drawing.Color.Turquoise;
                per13.ReadOnly = true;
                per13.BackColor = System.Drawing.Color.Turquoise;
                per14.ReadOnly = true;
                per14.BackColor = System.Drawing.Color.Turquoise;
                per15.ReadOnly = true;
                per15.BackColor = System.Drawing.Color.Turquoise;
                unit1.ReadOnly = true;
                unit15.ReadOnly = true;
                unit2.ReadOnly = true;
                unit3.ReadOnly = true;
                unit4.ReadOnly = true;
                unit5.ReadOnly = true;
                unit6.ReadOnly = true;
                unit7.ReadOnly = true;
                unit8.ReadOnly = true;
                unit9.ReadOnly = true;
                unit10.ReadOnly = true;
                unit11.ReadOnly = true;
                unit12.ReadOnly = true;
                unit13.ReadOnly = true;
                unit14.ReadOnly = true;
                unit1.BackColor = System.Drawing.Color.Turquoise;
                unit2.BackColor = System.Drawing.Color.Turquoise;
                unit3.BackColor = System.Drawing.Color.Turquoise;
                unit4.BackColor = System.Drawing.Color.Turquoise;
                unit5.BackColor = System.Drawing.Color.Turquoise;
                unit6.BackColor = System.Drawing.Color.Turquoise;
                unit7.BackColor = System.Drawing.Color.Turquoise;
                unit8.BackColor = System.Drawing.Color.Turquoise;
                unit9.BackColor = System.Drawing.Color.Turquoise;
                unit10.BackColor = System.Drawing.Color.Turquoise;
                unit11.BackColor = System.Drawing.Color.Turquoise;
                unit12.BackColor = System.Drawing.Color.Turquoise;
                unit13.BackColor = System.Drawing.Color.Turquoise;
                unit14.BackColor = System.Drawing.Color.Turquoise;
                unit15.BackColor = System.Drawing.Color.Turquoise;
                image1.Visible = false;
                image2.Visible = false;
                image3.Visible = false;
                image4.Visible = false;
                image5.Visible = false;
                image6.Visible = false;
                image7.Visible = false;
                image8.Visible = false;
                image9.Visible = false;
                image10.Visible = false;
                image14.Visible = false;
                image16.Visible = false;
                image17.Visible = false;
                image18.Visible = false;
                image19.Visible = false;

                packcode.Focus();

                Page.ClientScript.RegisterStartupScript(this.GetType(), "alert", "getSelect('vPackCodeTextBox');", true);
            }                           
        }
    }
    protected void Job_Button_Click(object sender, EventArgs e)
    {
        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];

        Session["corr_vendor_cost_est"] = Session["order_folding_est"];
        Session["corr_vendor_cost_form"] = Session["order_folding_formno"];
        Session["corr_vendor_cost_blank"] = Session["order_folding_blankno"];

        /*Label line = (Label)FormView_FoldLayout.FindControl("vLineLabel");*/

        string vmessage = "";
        Corrugated corr = new Corrugated();
        corr.SelectPrep(UserLogin.UserName, "jobstd", "", "", Convert.ToString(Session["order_folding_est"]), Convert.ToInt32(Session["order_folding_formno"]), 0, 0, "", 0, "", 0, "", "", 0, 0, 0, 0, ref vmessage);

        if (vmessage != "")
        {

            Page.ClientScript.RegisterStartupScript(this.GetType(), "alert", "confirmAdd('" + vmessage + "');", true);

        }

        //ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "jobstd";
        //ObjectDataSource1.SelectParameters["prmLine"].DefaultValue = line.Text.Trim();

    }
}

