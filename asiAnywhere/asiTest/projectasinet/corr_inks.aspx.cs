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
using System.IO;

public partial class corr_Inks : System.Web.UI.Page
{
    protected void Page_Load(object sender, EventArgs e)
    {
        
        //FormView_Ink.ChangeMode(FormViewMode.ReadOnly);
        CorrugatedInkDataSource.SelectParameters["prmAction"].DefaultValue = "Select";
        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];
        if (!Page.IsPostBack)
        {
            
        }
        if (Session["User"] != null)
        {
            string vUserId = UserLogin.UserName;
            string vPage = "corr_inks.aspx";
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
            labelname.Text = "Corrugated";
            
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
            /*ImageButton est = (ImageButton)Master.FindControl("img_inks");
            est.ImageUrl = "~/images/inks_pack_1.jpg";*/
            Image img_mov_col = (Image)Master.FindControl("Image5");
            img_mov_col.Visible = false;
        }
        catch { }


        StringBuilder str = new StringBuilder();
        str.Append("<script language=javascript>");
        str.Append("function update(e){");

        str.Append("document.forms[0].ctl00$ContentPlaceHolder1$FormView_Ink$vCode1TextBox.value=e[0];");
        str.Append("document.forms[0].ctl00$ContentPlaceHolder1$FormView_Ink$vCode2TextBox.value=e[1];");
        str.Append("document.forms[0].ctl00$ContentPlaceHolder1$FormView_Ink$vCode3TextBox.value=e[2];");
        str.Append("document.forms[0].ctl00$ContentPlaceHolder1$FormView_Ink$vCode4TextBox.value=e[3];");
        str.Append("document.forms[0].ctl00$ContentPlaceHolder1$FormView_Ink$vCode5TextBox.value=e[4];");
        str.Append("document.forms[0].ctl00$ContentPlaceHolder1$FormView_Ink$vCode6TextBox.value=e[5];");
        str.Append("document.forms[0].ctl00$ContentPlaceHolder1$FormView_Ink$vCode7TextBox.value=e[6];");
        str.Append("document.forms[0].ctl00$ContentPlaceHolder1$FormView_Ink$vCode8TextBox.value=e[7];");
        str.Append("document.forms[0].ctl00$ContentPlaceHolder1$FormView_Ink$vCode9TextBox.value=e[8];");
        str.Append("document.forms[0].ctl00$ContentPlaceHolder1$FormView_Ink$vCode10TextBox.value=e[9];");

        str.Append("document.forms[0].ctl00$ContentPlaceHolder1$FormView_Ink$vDscr1TextBox.value=e[10];");
        str.Append("document.forms[0].ctl00$ContentPlaceHolder1$FormView_Ink$vDscr2TextBox.value=e[11];");
        str.Append("document.forms[0].ctl00$ContentPlaceHolder1$FormView_Ink$vDscr3TextBox.value=e[12];");
        str.Append("document.forms[0].ctl00$ContentPlaceHolder1$FormView_Ink$vDscr4TextBox.value=e[13];");
        str.Append("document.forms[0].ctl00$ContentPlaceHolder1$FormView_Ink$vDscr5TextBox.value=e[14];");
        str.Append("document.forms[0].ctl00$ContentPlaceHolder1$FormView_Ink$vDscr6TextBox.value=e[15];");
        str.Append("document.forms[0].ctl00$ContentPlaceHolder1$FormView_Ink$vDscr7TextBox.value=e[16];");
        str.Append("document.forms[0].ctl00$ContentPlaceHolder1$FormView_Ink$vDscr8TextBox.value=e[17];");
        str.Append("document.forms[0].ctl00$ContentPlaceHolder1$FormView_Ink$vDscr9TextBox.value=e[18];");
        str.Append("document.forms[0].ctl00$ContentPlaceHolder1$FormView_Ink$vDscr10TextBox.value=e[19];");

        str.Append("document.forms[0].ctl00$ContentPlaceHolder1$FormView_Ink$vPer1TextBox.value=e[20];");
        str.Append("document.forms[0].ctl00$ContentPlaceHolder1$FormView_Ink$vPer2TextBox.value=e[21];");
        str.Append("document.forms[0].ctl00$ContentPlaceHolder1$FormView_Ink$vPer3TextBox.value=e[22];");
        str.Append("document.forms[0].ctl00$ContentPlaceHolder1$FormView_Ink$vPer4TextBox.value=e[23];");
        str.Append("document.forms[0].ctl00$ContentPlaceHolder1$FormView_Ink$vPer5TextBox.value=e[24];");
        str.Append("document.forms[0].ctl00$ContentPlaceHolder1$FormView_Ink$vPer6TextBox.value=e[25];");
        str.Append("document.forms[0].ctl00$ContentPlaceHolder1$FormView_Ink$vPer7TextBox.value=e[26];");
        str.Append("document.forms[0].ctl00$ContentPlaceHolder1$FormView_Ink$vPer8TextBox.value=e[27];");
        str.Append("document.forms[0].ctl00$ContentPlaceHolder1$FormView_Ink$vPer9TextBox.value=e[28];");
        str.Append("document.forms[0].ctl00$ContentPlaceHolder1$FormView_Ink$vPer10TextBox.value=e[29];}");

        str.Append("</script>");

        // register the javascript into the Page
        if (!ClientScript.IsClientScriptBlockRegistered(this.GetType(), "update"))
        {
            Page.RegisterClientScriptBlock("update", str.ToString());
        }


    }

    protected void Save_Click(object sender, EventArgs e)
    {
        TextBox est = (TextBox)FormView_Ink.FindControl("vEstNumTextBox");
        Label formno=(Label)FormView_Ink.FindControl("vFormNoTextBox");
        TextBox color = (TextBox)FormView_Ink.FindControl("vColorTextBox");
        TextBox pass = (TextBox)FormView_Ink.FindControl("vPassesTextBox");
        TextBox coat = (TextBox)FormView_Ink.FindControl("vCoatTextBox");
        TextBox coatpass = (TextBox)FormView_Ink.FindControl("vCoatPassTextBox");
        TextBox dscr = (TextBox)FormView_Ink.FindControl("vDscrTextBox");
        TextBox ps1 = (TextBox)FormView_Ink.FindControl("vPs1TextBox");
        TextBox ps2 = (TextBox)FormView_Ink.FindControl("vPs2TextBox");
        TextBox ps3 = (TextBox)FormView_Ink.FindControl("vPs3TextBox");
        TextBox ps4 = (TextBox)FormView_Ink.FindControl("vPs4TextBox");
        TextBox ps5 = (TextBox)FormView_Ink.FindControl("vPs5TextBox");
        TextBox ps6 = (TextBox)FormView_Ink.FindControl("vPs6TextBox");
        TextBox ps7 = (TextBox)FormView_Ink.FindControl("vPs7TextBox");
        TextBox ps8 = (TextBox)FormView_Ink.FindControl("vPs8TextBox");
        TextBox ps9 = (TextBox)FormView_Ink.FindControl("vPs9TextBox");
        TextBox ps10 = (TextBox)FormView_Ink.FindControl("vPs10TextBox");
        TextBox code1 = (TextBox)FormView_Ink.FindControl("vCode1TextBox");
        TextBox code2 = (TextBox)FormView_Ink.FindControl("vCode2TextBox");
        TextBox code3 = (TextBox)FormView_Ink.FindControl("vCode3TextBox");
        TextBox code4 = (TextBox)FormView_Ink.FindControl("vCode4TextBox");
        TextBox code5 = (TextBox)FormView_Ink.FindControl("vCode5TextBox");
        TextBox code6 = (TextBox)FormView_Ink.FindControl("vCode6TextBox");
        TextBox code7 = (TextBox)FormView_Ink.FindControl("vCode7TextBox");
        TextBox code8 = (TextBox)FormView_Ink.FindControl("vCode8TextBox");
        TextBox code9 = (TextBox)FormView_Ink.FindControl("vCode9TextBox");
        TextBox code10 = (TextBox)FormView_Ink.FindControl("vCode10TextBox");
        TextBox dscr1 = (TextBox)FormView_Ink.FindControl("vDscr1TextBox");
        TextBox dscr2 = (TextBox)FormView_Ink.FindControl("vDscr2TextBox");
        TextBox dscr3 = (TextBox)FormView_Ink.FindControl("vDscr3TextBox");
        TextBox dscr4 = (TextBox)FormView_Ink.FindControl("vDscr4TextBox");
        TextBox dscr5 = (TextBox)FormView_Ink.FindControl("vDscr5TextBox");
        TextBox dscr6 = (TextBox)FormView_Ink.FindControl("vDscr6TextBox");
        TextBox dscr7 = (TextBox)FormView_Ink.FindControl("vDscr7TextBox");
        TextBox dscr8 = (TextBox)FormView_Ink.FindControl("vDscr8TextBox");
        TextBox dscr9 = (TextBox)FormView_Ink.FindControl("vDscr9TextBox");
        TextBox dscr10 = (TextBox)FormView_Ink.FindControl("vDscr10TextBox");
        TextBox per1 = (TextBox)FormView_Ink.FindControl("vPer1TextBox");
        TextBox per2 = (TextBox)FormView_Ink.FindControl("vPer2TextBox");
        TextBox per3 = (TextBox)FormView_Ink.FindControl("vPer3TextBox");
        TextBox per4 = (TextBox)FormView_Ink.FindControl("vPer4TextBox");
        TextBox per5 = (TextBox)FormView_Ink.FindControl("vPer5TextBox");
        TextBox per6 = (TextBox)FormView_Ink.FindControl("vPer6TextBox");
        TextBox per7 = (TextBox)FormView_Ink.FindControl("vPer7TextBox");
        TextBox per8 = (TextBox)FormView_Ink.FindControl("vPer8TextBox");
        TextBox per9 = (TextBox)FormView_Ink.FindControl("vPer9TextBox");
        TextBox per10 = (TextBox)FormView_Ink.FindControl("vPer10TextBox");

        TextBox packcode = (TextBox)FormView_Ink.FindControl("vPackCodeTextBox");
        TextBox unitlen = (TextBox)FormView_Ink.FindControl("vUnitLenTextBox");
        TextBox cost1 = (TextBox)FormView_Ink.FindControl("vCostTextBox");
        TextBox unitwid = (TextBox)FormView_Ink.FindControl("vUnitWidTextBox");
        TextBox boxcode = (TextBox)FormView_Ink.FindControl("vBoxCodeTextBox");
        TextBox unitdep = (TextBox)FormView_Ink.FindControl("vUnitDepTextBox");
        TextBox bundl = (TextBox)FormView_Ink.FindControl("vBundlTextBox");
        

        TextBox wtpack = (TextBox)FormView_Ink.FindControl("vWtPackTextBox");
        TextBox unit = (TextBox)FormView_Ink.FindControl("vUnitTextBox");
        TextBox len = (TextBox)FormView_Ink.FindControl("vLengthTextBox");
        TextBox cost2 = (TextBox)FormView_Ink.FindControl("vCost2TextBox");
        TextBox wid = (TextBox)FormView_Ink.FindControl("vWidthTextBox");
        TextBox count = (TextBox)FormView_Ink.FindControl("vCountTextBox");
        TextBox hight = (TextBox)FormView_Ink.FindControl("vHeightTextBox");
        TextBox layer = (TextBox)FormView_Ink.FindControl("vLayerTextBox");
        TextBox stack = (TextBox)FormView_Ink.FindControl("vStackTextBox");
        TextBox stcode = (TextBox)FormView_Ink.FindControl("vStCodeTextBox");
        TextBox weiper = (TextBox)FormView_Ink.FindControl("vWeiPerTextBox");
        TextBox carrier = (TextBox)FormView_Ink.FindControl("vCarrierTextBox");
        TextBox carrdscr = (TextBox)FormView_Ink.FindControl("vCarrDscrTextBox");
        TextBox delzon = (TextBox)FormView_Ink.FindControl("vDelZonTextBox");
        TextBox frcwt = (TextBox)FormView_Ink.FindControl("vFreifgtTextBox");
        TextBox frm = (TextBox)FormView_Ink.FindControl("vFreOutTextBox");
        RadioButtonList fright = (RadioButtonList)FormView_Ink.FindControl("RadioButtonList1");

                
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
        CorrugatedInkDataSource.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
        CorrugatedInkDataSource.SelectParameters["prmAction"].DefaultValue = "InksUpdate";
        //CorrugatedInkDataSource.SelectParameters["prmComp"].DefaultValue = cust.Text.Trim();
        CorrugatedInkDataSource.SelectParameters["prmEstNum"].DefaultValue = est.Text.Trim();
        CorrugatedInkDataSource.SelectParameters["prmFormNo"].DefaultValue = formno.Text.Trim();
        CorrugatedInkDataSource.SelectParameters["prmColor"].DefaultValue = color.Text.Trim();
        CorrugatedInkDataSource.SelectParameters["prmPass"].DefaultValue = pass.Text.Trim();
        CorrugatedInkDataSource.SelectParameters["prmCoat"].DefaultValue = coat.Text.Trim();
        CorrugatedInkDataSource.SelectParameters["prmCoatPass"].DefaultValue = coatpass.Text.Trim();
        CorrugatedInkDataSource.SelectParameters["prmDscr"].DefaultValue = dscr.Text.Trim();
        CorrugatedInkDataSource.SelectParameters["prmPs1"].DefaultValue = ps1.Text.Trim();
        CorrugatedInkDataSource.SelectParameters["prmPs2"].DefaultValue = ps2.Text.Trim();
        CorrugatedInkDataSource.SelectParameters["prmPs3"].DefaultValue = ps3.Text.Trim();
        CorrugatedInkDataSource.SelectParameters["prmPs4"].DefaultValue = ps4.Text.Trim();
        CorrugatedInkDataSource.SelectParameters["prmPs5"].DefaultValue = ps5.Text.Trim();
        CorrugatedInkDataSource.SelectParameters["prmPs6"].DefaultValue = ps6.Text.Trim();
        CorrugatedInkDataSource.SelectParameters["prmPs7"].DefaultValue = ps7.Text.Trim();
        CorrugatedInkDataSource.SelectParameters["prmPs8"].DefaultValue = ps8.Text.Trim();
        CorrugatedInkDataSource.SelectParameters["prmPs9"].DefaultValue = ps9.Text.Trim();
        CorrugatedInkDataSource.SelectParameters["prmPs10"].DefaultValue = ps10.Text.Trim();
        CorrugatedInkDataSource.SelectParameters["prmCode1"].DefaultValue = code1.Text.Trim();
        CorrugatedInkDataSource.SelectParameters["prmCode2"].DefaultValue = code2.Text.Trim();
        CorrugatedInkDataSource.SelectParameters["prmCode3"].DefaultValue = code3.Text.Trim();
        CorrugatedInkDataSource.SelectParameters["prmCode4"].DefaultValue = code4.Text.Trim();
        CorrugatedInkDataSource.SelectParameters["prmCode5"].DefaultValue = code5.Text.Trim();
        CorrugatedInkDataSource.SelectParameters["prmCode6"].DefaultValue = code6.Text.Trim();
        CorrugatedInkDataSource.SelectParameters["prmCode7"].DefaultValue = code7.Text.Trim();
        CorrugatedInkDataSource.SelectParameters["prmCode8"].DefaultValue = code8.Text.Trim();
        CorrugatedInkDataSource.SelectParameters["prmCode9"].DefaultValue = code9.Text.Trim();
        CorrugatedInkDataSource.SelectParameters["prmCode10"].DefaultValue = code10.Text.Trim();
        CorrugatedInkDataSource.SelectParameters["prmDscr1"].DefaultValue = dscr1.Text.Trim();
        CorrugatedInkDataSource.SelectParameters["prmDscr2"].DefaultValue = dscr2.Text.Trim();
        CorrugatedInkDataSource.SelectParameters["prmDscr3"].DefaultValue = dscr3.Text.Trim();
        CorrugatedInkDataSource.SelectParameters["prmDscr4"].DefaultValue = dscr4.Text.Trim();
        CorrugatedInkDataSource.SelectParameters["prmDscr5"].DefaultValue = dscr5.Text.Trim();
        CorrugatedInkDataSource.SelectParameters["prmDscr6"].DefaultValue = dscr6.Text.Trim();
        CorrugatedInkDataSource.SelectParameters["prmDscr7"].DefaultValue = dscr7.Text.Trim();
        CorrugatedInkDataSource.SelectParameters["prmDscr8"].DefaultValue = dscr8.Text.Trim();
        CorrugatedInkDataSource.SelectParameters["prmDscr9"].DefaultValue = dscr9.Text.Trim();
        CorrugatedInkDataSource.SelectParameters["prmDscr10"].DefaultValue = dscr10.Text.Trim();
        CorrugatedInkDataSource.SelectParameters["prmPer1"].DefaultValue =  per1.Text.Trim();
        CorrugatedInkDataSource.SelectParameters["prmPer2"].DefaultValue = per2.Text.Trim();
        CorrugatedInkDataSource.SelectParameters["prmPer3"].DefaultValue = per3.Text.Trim();
        CorrugatedInkDataSource.SelectParameters["prmPer4"].DefaultValue = per4.Text.Trim();
        CorrugatedInkDataSource.SelectParameters["prmPer5"].DefaultValue = per5.Text.Trim();
        CorrugatedInkDataSource.SelectParameters["prmPer6"].DefaultValue = per6.Text.Trim();
        CorrugatedInkDataSource.SelectParameters["prmPer7"].DefaultValue = per7.Text.Trim();
        CorrugatedInkDataSource.SelectParameters["prmPer8"].DefaultValue = per8.Text.Trim();
        CorrugatedInkDataSource.SelectParameters["prmPer9"].DefaultValue = per9.Text.Trim();
        CorrugatedInkDataSource.SelectParameters["prmPer10"].DefaultValue = per10.Text.Trim();

        CorrugatedInkDataSource.SelectParameters["prmPackCode"].DefaultValue = packcode.Text.Trim();
        CorrugatedInkDataSource.SelectParameters["prmUnitLen"].DefaultValue = unitlen.Text.Trim();
        CorrugatedInkDataSource.SelectParameters["prmCostEa"].DefaultValue = cost1.Text.Trim();
        CorrugatedInkDataSource.SelectParameters["prmUnitWid"].DefaultValue = unitwid.Text.Trim();
        CorrugatedInkDataSource.SelectParameters["prmBoxCode"].DefaultValue = boxcode.Text.Trim();
        CorrugatedInkDataSource.SelectParameters["prmUnitDep"].DefaultValue = unitdep.Text.Trim();

        CorrugatedInkDataSource.SelectParameters["prmBundl"].DefaultValue = bundl.Text.Trim();
        CorrugatedInkDataSource.SelectParameters["prmCost2"].DefaultValue = cost2.Text.Trim();
        CorrugatedInkDataSource.SelectParameters["prmCount"].DefaultValue = count.Text.Trim();
        CorrugatedInkDataSource.SelectParameters["prmLength"].DefaultValue = len.Text.Trim();
        CorrugatedInkDataSource.SelectParameters["prmWidth"].DefaultValue = wid.Text.Trim();
        CorrugatedInkDataSource.SelectParameters["prmHeight"].DefaultValue = hight.Text.Trim();
        CorrugatedInkDataSource.SelectParameters["prmLayer"].DefaultValue = layer.Text.Trim();
        CorrugatedInkDataSource.SelectParameters["prmStack"].DefaultValue = stack.Text.Trim();
        CorrugatedInkDataSource.SelectParameters["prmStCode"].DefaultValue = stcode.Text.Trim();

        //CorrugatedInkDataSource.SelectParameters["prmBundl"].DefaultValue = vBundlHiddenField.Value;
        //CorrugatedInkDataSource.SelectParameters["prmCost2"].DefaultValue = vCost2HiddenField.Value;
        //CorrugatedInkDataSource.SelectParameters["prmCount"].DefaultValue = vCountHiddenField.Value;
        //CorrugatedInkDataSource.SelectParameters["prmLength"].DefaultValue = vLengthHiddenField.Value;
        //CorrugatedInkDataSource.SelectParameters["prmWidth"].DefaultValue = vWidthHiddenField.Value;
        //CorrugatedInkDataSource.SelectParameters["prmHeight"].DefaultValue = vHeightHiddenField.Value;
        //CorrugatedInkDataSource.SelectParameters["prmLayer"].DefaultValue = vLayerHiddenField.Value;
        //CorrugatedInkDataSource.SelectParameters["prmStack"].DefaultValue = vStackHiddenField.Value;
        //CorrugatedInkDataSource.SelectParameters["prmStCode"].DefaultValue = vStcodeHiddenField.Value;
        CorrugatedInkDataSource.SelectParameters["prmWTPack"].DefaultValue = wtpack.Text.Trim();
        CorrugatedInkDataSource.SelectParameters["prmUnit"].DefaultValue = unit.Text.Trim();                                
        CorrugatedInkDataSource.SelectParameters["prmFrCharge"].DefaultValue = HiddenField1.Value;
        CorrugatedInkDataSource.SelectParameters["prmWeightPer"].DefaultValue = weiper.Text.Trim();
        CorrugatedInkDataSource.SelectParameters["prmCarrier"].DefaultValue = carrier.Text.Trim();
        CorrugatedInkDataSource.SelectParameters["prmCarrDscr"].DefaultValue = carrdscr.Text.Trim();
        CorrugatedInkDataSource.SelectParameters["prmDelZon"].DefaultValue = delzon.Text.Trim();
        CorrugatedInkDataSource.SelectParameters["prmFreight"].DefaultValue = frcwt.Text.Trim();
        CorrugatedInkDataSource.SelectParameters["prmFreight2"].DefaultValue = frm.Text.Trim();

        FormView_Ink.ChangeMode(FormViewMode.ReadOnly);
    }
    protected void updateinkbutton_Click(object sender, EventArgs e)
    {
        Session["ink_update_button"] = 1;
    }

    protected void resetinkbutton_Click(object sender, EventArgs e)
    {       
        Label est = (Label)FormView_Ink.FindControl("vEstNumLabel");
        Label formno = (Label)FormView_Ink.FindControl("vFormNoLabel");
        Label blankno = (Label)FormView_Ink.FindControl("vBlankNoLabel");        
        Label color = (Label)FormView_Ink.FindControl("vColorLabel");
        Label pass = (Label)FormView_Ink.FindControl("vPassesLabel");
        Label coat = (Label)FormView_Ink.FindControl("vCoatLabel");
        Label coatpass = (Label)FormView_Ink.FindControl("vCoatPassLabel");
        Label dscr = (Label)FormView_Ink.FindControl("vDscrLabel");
        Label ps1 = (Label)FormView_Ink.FindControl("vPs1Label");
        Label ps2 = (Label)FormView_Ink.FindControl("vPs2Label");
        Label ps3 = (Label)FormView_Ink.FindControl("vPs3Label");
        Label ps4 = (Label)FormView_Ink.FindControl("vPs4Label");
        Label ps5 = (Label)FormView_Ink.FindControl("vPs5Label");
        Label ps6 = (Label)FormView_Ink.FindControl("vPs6Label");
        Label ps7 = (Label)FormView_Ink.FindControl("vPs7Label");
        Label ps8 = (Label)FormView_Ink.FindControl("vPs8Label");
        Label ps9 = (Label)FormView_Ink.FindControl("vPs9Label");
        Label ps10 = (Label)FormView_Ink.FindControl("vPs10Label");
        Label code1 = (Label)FormView_Ink.FindControl("vCode1Label");
        Label code2 = (Label)FormView_Ink.FindControl("vCode2Label");
        Label code3 = (Label)FormView_Ink.FindControl("vCode3Label");
        Label code4 = (Label)FormView_Ink.FindControl("vCode4Label");
        Label code5 = (Label)FormView_Ink.FindControl("vCode5Label");
        Label code6 = (Label)FormView_Ink.FindControl("vCode6Label");
        Label code7 = (Label)FormView_Ink.FindControl("vCode7Label");
        Label code8 = (Label)FormView_Ink.FindControl("vCode8Label");
        Label code9 = (Label)FormView_Ink.FindControl("vCode9Label");
        Label code10 = (Label)FormView_Ink.FindControl("vCode10Label");
        Label dscr1 = (Label)FormView_Ink.FindControl("vDscr1Label");
        Label dscr2 = (Label)FormView_Ink.FindControl("vDscr2Label");
        Label dscr3 = (Label)FormView_Ink.FindControl("vDscr3Label");
        Label dscr4 = (Label)FormView_Ink.FindControl("vDscr4Label");
        Label dscr5 = (Label)FormView_Ink.FindControl("vDscr5Label");
        Label dscr6 = (Label)FormView_Ink.FindControl("vDscr6Label");
        Label dscr7 = (Label)FormView_Ink.FindControl("vDscr7Label");
        Label dscr8 = (Label)FormView_Ink.FindControl("vDscr8Label");
        Label dscr9 = (Label)FormView_Ink.FindControl("vDscr9Label");
        Label dscr10 = (Label)FormView_Ink.FindControl("vDscr10Label");
        Label per1 = (Label)FormView_Ink.FindControl("vPer1Label");
        Label per2 = (Label)FormView_Ink.FindControl("vPer2Label");
        Label per3 = (Label)FormView_Ink.FindControl("vPer3Label");
        Label per4 = (Label)FormView_Ink.FindControl("vPer4Label");
        Label per5 = (Label)FormView_Ink.FindControl("vPer5Label");
        Label per6 = (Label)FormView_Ink.FindControl("vPer6Label");
        Label per7 = (Label)FormView_Ink.FindControl("vPer7Label");
        Label per8 = (Label)FormView_Ink.FindControl("vPer8Label");
        Label per9 = (Label)FormView_Ink.FindControl("vPer9Label");
        Label per10 = (Label)FormView_Ink.FindControl("vPer10Label");


        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];
        CorrugatedInkDataSource.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
        CorrugatedInkDataSource.SelectParameters["prmAction"].DefaultValue = "ResetInk";        
        CorrugatedInkDataSource.SelectParameters["prmEstNum"].DefaultValue = est.Text.Trim();
        CorrugatedInkDataSource.SelectParameters["prmFormNo"].DefaultValue = formno.Text.Trim();
        CorrugatedInkDataSource.SelectParameters["prmBlankno"].DefaultValue = blankno.Text.Trim();
        CorrugatedInkDataSource.SelectParameters["prmColor"].DefaultValue = color.Text.Trim();
        CorrugatedInkDataSource.SelectParameters["prmPass"].DefaultValue = pass.Text.Trim();
        CorrugatedInkDataSource.SelectParameters["prmCoat"].DefaultValue = coat.Text.Trim();
        CorrugatedInkDataSource.SelectParameters["prmCoatPass"].DefaultValue = coatpass.Text.Trim();
        CorrugatedInkDataSource.SelectParameters["prmDscr"].DefaultValue = dscr.Text.Trim();
        CorrugatedInkDataSource.SelectParameters["prmPs1"].DefaultValue = ps1.Text.Trim();
        CorrugatedInkDataSource.SelectParameters["prmPs2"].DefaultValue = ps2.Text.Trim();
        CorrugatedInkDataSource.SelectParameters["prmPs3"].DefaultValue = ps3.Text.Trim();
        CorrugatedInkDataSource.SelectParameters["prmPs4"].DefaultValue = ps4.Text.Trim();
        CorrugatedInkDataSource.SelectParameters["prmPs5"].DefaultValue = ps5.Text.Trim();
        CorrugatedInkDataSource.SelectParameters["prmPs6"].DefaultValue = ps6.Text.Trim();
        CorrugatedInkDataSource.SelectParameters["prmPs7"].DefaultValue = ps7.Text.Trim();
        CorrugatedInkDataSource.SelectParameters["prmPs8"].DefaultValue = ps8.Text.Trim();
        CorrugatedInkDataSource.SelectParameters["prmPs9"].DefaultValue = ps9.Text.Trim();
        CorrugatedInkDataSource.SelectParameters["prmPs10"].DefaultValue = ps10.Text.Trim();
        CorrugatedInkDataSource.SelectParameters["prmCode1"].DefaultValue = code1.Text.Trim();
        CorrugatedInkDataSource.SelectParameters["prmCode2"].DefaultValue = code2.Text.Trim();
        CorrugatedInkDataSource.SelectParameters["prmCode3"].DefaultValue = code3.Text.Trim();
        CorrugatedInkDataSource.SelectParameters["prmCode4"].DefaultValue = code4.Text.Trim();
        CorrugatedInkDataSource.SelectParameters["prmCode5"].DefaultValue = code5.Text.Trim();
        CorrugatedInkDataSource.SelectParameters["prmCode6"].DefaultValue = code6.Text.Trim();
        CorrugatedInkDataSource.SelectParameters["prmCode7"].DefaultValue = code7.Text.Trim();
        CorrugatedInkDataSource.SelectParameters["prmCode8"].DefaultValue = code8.Text.Trim();
        CorrugatedInkDataSource.SelectParameters["prmCode9"].DefaultValue = code9.Text.Trim();
        CorrugatedInkDataSource.SelectParameters["prmCode10"].DefaultValue = code10.Text.Trim();
        CorrugatedInkDataSource.SelectParameters["prmDscr1"].DefaultValue = dscr1.Text.Trim();
        CorrugatedInkDataSource.SelectParameters["prmDscr2"].DefaultValue = dscr2.Text.Trim();
        CorrugatedInkDataSource.SelectParameters["prmDscr3"].DefaultValue = dscr3.Text.Trim();
        CorrugatedInkDataSource.SelectParameters["prmDscr4"].DefaultValue = dscr4.Text.Trim();
        CorrugatedInkDataSource.SelectParameters["prmDscr5"].DefaultValue = dscr5.Text.Trim();
        CorrugatedInkDataSource.SelectParameters["prmDscr6"].DefaultValue = dscr6.Text.Trim();
        CorrugatedInkDataSource.SelectParameters["prmDscr7"].DefaultValue = dscr7.Text.Trim();
        CorrugatedInkDataSource.SelectParameters["prmDscr8"].DefaultValue = dscr8.Text.Trim();
        CorrugatedInkDataSource.SelectParameters["prmDscr9"].DefaultValue = dscr9.Text.Trim();
        CorrugatedInkDataSource.SelectParameters["prmDscr10"].DefaultValue = dscr10.Text.Trim();
        CorrugatedInkDataSource.SelectParameters["prmPer1"].DefaultValue = per1.Text.Trim();
        CorrugatedInkDataSource.SelectParameters["prmPer2"].DefaultValue = per2.Text.Trim();
        CorrugatedInkDataSource.SelectParameters["prmPer3"].DefaultValue = per3.Text.Trim();
        CorrugatedInkDataSource.SelectParameters["prmPer4"].DefaultValue = per4.Text.Trim();
        CorrugatedInkDataSource.SelectParameters["prmPer5"].DefaultValue = per5.Text.Trim();
        CorrugatedInkDataSource.SelectParameters["prmPer6"].DefaultValue = per6.Text.Trim();
        CorrugatedInkDataSource.SelectParameters["prmPer7"].DefaultValue = per7.Text.Trim();
        CorrugatedInkDataSource.SelectParameters["prmPer8"].DefaultValue = per8.Text.Trim();
        CorrugatedInkDataSource.SelectParameters["prmPer9"].DefaultValue = per9.Text.Trim();
        CorrugatedInkDataSource.SelectParameters["prmPer10"].DefaultValue = per10.Text.Trim();       
    }

    protected void colortextchanged(object sender, EventArgs e)
    {
        TextBox est = (TextBox)FormView_Ink.FindControl("vEstNumTextBox");
        Label formno = (Label)FormView_Ink.FindControl("vFormNoTextBox");
        TextBox color = (TextBox)FormView_Ink.FindControl("vColorTextBox");
        TextBox pass = (TextBox)FormView_Ink.FindControl("vPassesTextBox");
        TextBox coat = (TextBox)FormView_Ink.FindControl("vCoatTextBox");
        TextBox coatpass = (TextBox)FormView_Ink.FindControl("vCoatPassTextBox");
        TextBox dscr = (TextBox)FormView_Ink.FindControl("vDscrTextBox");
        TextBox ps1 = (TextBox)FormView_Ink.FindControl("vPs1TextBox");
        TextBox ps2 = (TextBox)FormView_Ink.FindControl("vPs2TextBox");
        TextBox ps3 = (TextBox)FormView_Ink.FindControl("vPs3TextBox");
        TextBox ps4 = (TextBox)FormView_Ink.FindControl("vPs4TextBox");
        TextBox ps5 = (TextBox)FormView_Ink.FindControl("vPs5TextBox");
        TextBox ps6 = (TextBox)FormView_Ink.FindControl("vPs6TextBox");
        TextBox ps7 = (TextBox)FormView_Ink.FindControl("vPs7TextBox");
        TextBox ps8 = (TextBox)FormView_Ink.FindControl("vPs8TextBox");
        TextBox ps9 = (TextBox)FormView_Ink.FindControl("vPs9TextBox");
        TextBox ps10 = (TextBox)FormView_Ink.FindControl("vPs10TextBox");
        TextBox code1 = (TextBox)FormView_Ink.FindControl("vCode1TextBox");
        TextBox code2 = (TextBox)FormView_Ink.FindControl("vCode2TextBox");
        TextBox code3 = (TextBox)FormView_Ink.FindControl("vCode3TextBox");
        TextBox code4 = (TextBox)FormView_Ink.FindControl("vCode4TextBox");
        TextBox code5 = (TextBox)FormView_Ink.FindControl("vCode5TextBox");
        TextBox code6 = (TextBox)FormView_Ink.FindControl("vCode6TextBox");
        TextBox code7 = (TextBox)FormView_Ink.FindControl("vCode7TextBox");
        TextBox code8 = (TextBox)FormView_Ink.FindControl("vCode8TextBox");
        TextBox code9 = (TextBox)FormView_Ink.FindControl("vCode9TextBox");
        TextBox code10 = (TextBox)FormView_Ink.FindControl("vCode10TextBox");
        TextBox dscr1 = (TextBox)FormView_Ink.FindControl("vDscr1TextBox");
        TextBox dscr2 = (TextBox)FormView_Ink.FindControl("vDscr2TextBox");
        TextBox dscr3 = (TextBox)FormView_Ink.FindControl("vDscr3TextBox");
        TextBox dscr4 = (TextBox)FormView_Ink.FindControl("vDscr4TextBox");
        TextBox dscr5 = (TextBox)FormView_Ink.FindControl("vDscr5TextBox");
        TextBox dscr6 = (TextBox)FormView_Ink.FindControl("vDscr6TextBox");
        TextBox dscr7 = (TextBox)FormView_Ink.FindControl("vDscr7TextBox");
        TextBox dscr8 = (TextBox)FormView_Ink.FindControl("vDscr8TextBox");
        TextBox dscr9 = (TextBox)FormView_Ink.FindControl("vDscr9TextBox");
        TextBox dscr10 = (TextBox)FormView_Ink.FindControl("vDscr10TextBox");
        TextBox per1 = (TextBox)FormView_Ink.FindControl("vPer1TextBox");
        TextBox per2 = (TextBox)FormView_Ink.FindControl("vPer2TextBox");
        TextBox per3 = (TextBox)FormView_Ink.FindControl("vPer3TextBox");
        TextBox per4 = (TextBox)FormView_Ink.FindControl("vPer4TextBox");
        TextBox per5 = (TextBox)FormView_Ink.FindControl("vPer5TextBox");
        TextBox per6 = (TextBox)FormView_Ink.FindControl("vPer6TextBox");
        TextBox per7 = (TextBox)FormView_Ink.FindControl("vPer7TextBox");
        TextBox per8 = (TextBox)FormView_Ink.FindControl("vPer8TextBox");
        TextBox per9 = (TextBox)FormView_Ink.FindControl("vPer9TextBox");
        TextBox per10 = (TextBox)FormView_Ink.FindControl("vPer10TextBox");
        

        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];

        Corrugated corr = new Corrugated();
        DataSet ds = new DataSet();
        ds = corr.CorrInks(UserLogin.UserName, "ColorChange", "", est.Text.Trim(), Convert.ToInt32(formno.Text.Trim()), Convert.ToInt32(color.Text.Trim()), Convert.ToInt32(pass.Text.Trim()), Convert.ToInt32(coat.Text.Trim()), Convert.ToInt32(coatpass.Text.Trim()), dscr.Text.Trim(), Convert.ToInt32(ps1.Text.Trim()), Convert.ToInt32(ps2.Text.Trim()), Convert.ToInt32(ps3.Text.Trim()), Convert.ToInt32(ps4.Text.Trim()), Convert.ToInt32(ps5.Text.Trim()), Convert.ToInt32(ps6.Text.Trim()), Convert.ToInt32(ps7.Text.Trim()), Convert.ToInt32(ps8.Text.Trim()), Convert.ToInt32(ps9.Text.Trim()), Convert.ToInt32(ps10.Text.Trim()), code1.Text.Trim(), code2.Text.Trim(), code3.Text.Trim(), code4.Text.Trim(), code5.Text.Trim(), code6.Text.Trim(), code7.Text.Trim(), code8.Text.Trim(), code9.Text.Trim(), code10.Text.Trim(), dscr1.Text.Trim(), dscr2.Text.Trim(), dscr3.Text.Trim(), dscr4.Text.Trim(), dscr5.Text.Trim(), dscr6.Text.Trim(), dscr7.Text.Trim(), dscr8.Text.Trim(), dscr9.Text.Trim(), dscr10.Text.Trim(), Convert.ToInt32(per1.Text.Trim()), Convert.ToInt32(per2.Text.Trim()), Convert.ToInt32(per3.Text.Trim()), Convert.ToInt32(per4.Text.Trim()), Convert.ToInt32(per5.Text.Trim()), Convert.ToInt32(per6.Text.Trim()), Convert.ToInt32(per7.Text.Trim()), Convert.ToInt32(per8.Text.Trim()), Convert.ToInt32(per9.Text.Trim()), Convert.ToInt32(per10.Text.Trim()), "", 0, 0, 0, 0, 0, 0, 0, "", 0, 0, 0, 0, 0, 0, 0, "", "", 0, "", "", "", 0, 0, Convert.ToInt32(Session["order_corrugated_blankno"]));

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
        code1.Text = ds.Tables[0].Rows[0][22].ToString();
        code2.Text = ds.Tables[0].Rows[0][23].ToString();
        code3.Text = ds.Tables[0].Rows[0][24].ToString();
        code4.Text = ds.Tables[0].Rows[0][25].ToString();
        code5.Text = ds.Tables[0].Rows[0][26].ToString();
        code6.Text = ds.Tables[0].Rows[0][27].ToString();
        code7.Text = ds.Tables[0].Rows[0][28].ToString();
        code8.Text = ds.Tables[0].Rows[0][29].ToString();
        code9.Text = ds.Tables[0].Rows[0][30].ToString();
        code10.Text = ds.Tables[0].Rows[0][31].ToString();
        dscr1.Text = ds.Tables[0].Rows[0][32].ToString();
        dscr2.Text = ds.Tables[0].Rows[0][33].ToString();
        dscr3.Text = ds.Tables[0].Rows[0][34].ToString();
        dscr4.Text = ds.Tables[0].Rows[0][35].ToString();
        dscr5.Text = ds.Tables[0].Rows[0][36].ToString();
        dscr6.Text = ds.Tables[0].Rows[0][37].ToString();
        dscr7.Text = ds.Tables[0].Rows[0][38].ToString();
        dscr8.Text = ds.Tables[0].Rows[0][39].ToString();
        dscr9.Text = ds.Tables[0].Rows[0][40].ToString();
        dscr10.Text = ds.Tables[0].Rows[0][41].ToString();
        per1.Text = ds.Tables[0].Rows[0][42].ToString();
        per2.Text = ds.Tables[0].Rows[0][43].ToString();
        per3.Text = ds.Tables[0].Rows[0][44].ToString();
        per4.Text = ds.Tables[0].Rows[0][45].ToString();
        per5.Text = ds.Tables[0].Rows[0][46].ToString();
        per6.Text = ds.Tables[0].Rows[0][47].ToString();
        per7.Text = ds.Tables[0].Rows[0][48].ToString();
        per8.Text = ds.Tables[0].Rows[0][49].ToString();
        per9.Text = ds.Tables[0].Rows[0][50].ToString();
        per10.Text = ds.Tables[0].Rows[0][51].ToString();

        pass.Focus();

        Page.ClientScript.RegisterStartupScript(this.GetType(), "alert", "getSelect('vPassesTextBox');", true);
    }

    protected void coattextchanged(object sender, EventArgs e)
    {
        TextBox est = (TextBox)FormView_Ink.FindControl("vEstNumTextBox");
        Label formno = (Label)FormView_Ink.FindControl("vFormNoTextBox");
        TextBox color = (TextBox)FormView_Ink.FindControl("vColorTextBox");
        TextBox pass = (TextBox)FormView_Ink.FindControl("vPassesTextBox");
        TextBox coat = (TextBox)FormView_Ink.FindControl("vCoatTextBox");
        TextBox coatpass = (TextBox)FormView_Ink.FindControl("vCoatPassTextBox");
        TextBox dscr = (TextBox)FormView_Ink.FindControl("vDscrTextBox");
        TextBox ps1 = (TextBox)FormView_Ink.FindControl("vPs1TextBox");
        TextBox ps2 = (TextBox)FormView_Ink.FindControl("vPs2TextBox");
        TextBox ps3 = (TextBox)FormView_Ink.FindControl("vPs3TextBox");
        TextBox ps4 = (TextBox)FormView_Ink.FindControl("vPs4TextBox");
        TextBox ps5 = (TextBox)FormView_Ink.FindControl("vPs5TextBox");
        TextBox ps6 = (TextBox)FormView_Ink.FindControl("vPs6TextBox");
        TextBox ps7 = (TextBox)FormView_Ink.FindControl("vPs7TextBox");
        TextBox ps8 = (TextBox)FormView_Ink.FindControl("vPs8TextBox");
        TextBox ps9 = (TextBox)FormView_Ink.FindControl("vPs9TextBox");
        TextBox ps10 = (TextBox)FormView_Ink.FindControl("vPs10TextBox");
        TextBox code1 = (TextBox)FormView_Ink.FindControl("vCode1TextBox");
        TextBox code2 = (TextBox)FormView_Ink.FindControl("vCode2TextBox");
        TextBox code3 = (TextBox)FormView_Ink.FindControl("vCode3TextBox");
        TextBox code4 = (TextBox)FormView_Ink.FindControl("vCode4TextBox");
        TextBox code5 = (TextBox)FormView_Ink.FindControl("vCode5TextBox");
        TextBox code6 = (TextBox)FormView_Ink.FindControl("vCode6TextBox");
        TextBox code7 = (TextBox)FormView_Ink.FindControl("vCode7TextBox");
        TextBox code8 = (TextBox)FormView_Ink.FindControl("vCode8TextBox");
        TextBox code9 = (TextBox)FormView_Ink.FindControl("vCode9TextBox");
        TextBox code10 = (TextBox)FormView_Ink.FindControl("vCode10TextBox");
        TextBox dscr1 = (TextBox)FormView_Ink.FindControl("vDscr1TextBox");
        TextBox dscr2 = (TextBox)FormView_Ink.FindControl("vDscr2TextBox");
        TextBox dscr3 = (TextBox)FormView_Ink.FindControl("vDscr3TextBox");
        TextBox dscr4 = (TextBox)FormView_Ink.FindControl("vDscr4TextBox");
        TextBox dscr5 = (TextBox)FormView_Ink.FindControl("vDscr5TextBox");
        TextBox dscr6 = (TextBox)FormView_Ink.FindControl("vDscr6TextBox");
        TextBox dscr7 = (TextBox)FormView_Ink.FindControl("vDscr7TextBox");
        TextBox dscr8 = (TextBox)FormView_Ink.FindControl("vDscr8TextBox");
        TextBox dscr9 = (TextBox)FormView_Ink.FindControl("vDscr9TextBox");
        TextBox dscr10 = (TextBox)FormView_Ink.FindControl("vDscr10TextBox");
        TextBox per1 = (TextBox)FormView_Ink.FindControl("vPer1TextBox");
        TextBox per2 = (TextBox)FormView_Ink.FindControl("vPer2TextBox");
        TextBox per3 = (TextBox)FormView_Ink.FindControl("vPer3TextBox");
        TextBox per4 = (TextBox)FormView_Ink.FindControl("vPer4TextBox");
        TextBox per5 = (TextBox)FormView_Ink.FindControl("vPer5TextBox");
        TextBox per6 = (TextBox)FormView_Ink.FindControl("vPer6TextBox");
        TextBox per7 = (TextBox)FormView_Ink.FindControl("vPer7TextBox");
        TextBox per8 = (TextBox)FormView_Ink.FindControl("vPer8TextBox");
        TextBox per9 = (TextBox)FormView_Ink.FindControl("vPer9TextBox");
        TextBox per10 = (TextBox)FormView_Ink.FindControl("vPer10TextBox");


        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];

        Corrugated corr = new Corrugated();
        DataSet ds = new DataSet();
        ds = corr.CorrInks(UserLogin.UserName, "CoatChange", "", est.Text.Trim(), Convert.ToInt32(formno.Text.Trim()), Convert.ToInt32(color.Text.Trim()), Convert.ToInt32(pass.Text.Trim()), Convert.ToInt32(coat.Text.Trim()), Convert.ToInt32(coatpass.Text.Trim()), dscr.Text.Trim(), Convert.ToInt32(ps1.Text.Trim()), Convert.ToInt32(ps2.Text.Trim()), Convert.ToInt32(ps3.Text.Trim()), Convert.ToInt32(ps4.Text.Trim()), Convert.ToInt32(ps5.Text.Trim()), Convert.ToInt32(ps6.Text.Trim()), Convert.ToInt32(ps7.Text.Trim()), Convert.ToInt32(ps8.Text.Trim()), Convert.ToInt32(ps9.Text.Trim()), Convert.ToInt32(ps10.Text.Trim()), code1.Text.Trim(), code2.Text.Trim(), code3.Text.Trim(), code4.Text.Trim(), code5.Text.Trim(), code6.Text.Trim(), code7.Text.Trim(), code8.Text.Trim(), code9.Text.Trim(), code10.Text.Trim(), dscr1.Text.Trim(), dscr2.Text.Trim(), dscr3.Text.Trim(), dscr4.Text.Trim(), dscr5.Text.Trim(), dscr6.Text.Trim(), dscr7.Text.Trim(), dscr8.Text.Trim(), dscr9.Text.Trim(), dscr10.Text.Trim(), Convert.ToInt32(per1.Text.Trim()), Convert.ToInt32(per2.Text.Trim()), Convert.ToInt32(per3.Text.Trim()), Convert.ToInt32(per4.Text.Trim()), Convert.ToInt32(per5.Text.Trim()), Convert.ToInt32(per6.Text.Trim()), Convert.ToInt32(per7.Text.Trim()), Convert.ToInt32(per8.Text.Trim()), Convert.ToInt32(per9.Text.Trim()), Convert.ToInt32(per10.Text.Trim()), "", 0, 0, 0, 0, 0, 0, 0, "", 0, 0, 0, 0, 0, 0, 0, "", "", 0, "", "", "", 0, 0, Convert.ToInt32(Session["order_corrugated_blankno"]));

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
        code1.Text = ds.Tables[0].Rows[0][22].ToString();
        code2.Text = ds.Tables[0].Rows[0][23].ToString();
        code3.Text = ds.Tables[0].Rows[0][24].ToString();
        code4.Text = ds.Tables[0].Rows[0][25].ToString();
        code5.Text = ds.Tables[0].Rows[0][26].ToString();
        code6.Text = ds.Tables[0].Rows[0][27].ToString();
        code7.Text = ds.Tables[0].Rows[0][28].ToString();
        code8.Text = ds.Tables[0].Rows[0][29].ToString();
        code9.Text = ds.Tables[0].Rows[0][30].ToString();
        code10.Text = ds.Tables[0].Rows[0][31].ToString();
        dscr1.Text = ds.Tables[0].Rows[0][32].ToString();
        dscr2.Text = ds.Tables[0].Rows[0][33].ToString();
        dscr3.Text = ds.Tables[0].Rows[0][34].ToString();
        dscr4.Text = ds.Tables[0].Rows[0][35].ToString();
        dscr5.Text = ds.Tables[0].Rows[0][36].ToString();
        dscr6.Text = ds.Tables[0].Rows[0][37].ToString();
        dscr7.Text = ds.Tables[0].Rows[0][38].ToString();
        dscr8.Text = ds.Tables[0].Rows[0][39].ToString();
        dscr9.Text = ds.Tables[0].Rows[0][40].ToString();
        dscr10.Text = ds.Tables[0].Rows[0][41].ToString();
        per1.Text = ds.Tables[0].Rows[0][42].ToString();
        per2.Text = ds.Tables[0].Rows[0][43].ToString();
        per3.Text = ds.Tables[0].Rows[0][44].ToString();
        per4.Text = ds.Tables[0].Rows[0][45].ToString();
        per5.Text = ds.Tables[0].Rows[0][46].ToString();
        per6.Text = ds.Tables[0].Rows[0][47].ToString();
        per7.Text = ds.Tables[0].Rows[0][48].ToString();
        per8.Text = ds.Tables[0].Rows[0][49].ToString();
        per9.Text = ds.Tables[0].Rows[0][50].ToString();
        per10.Text = ds.Tables[0].Rows[0][51].ToString();

        coatpass.Focus();

        Page.ClientScript.RegisterStartupScript(this.GetType(), "alert", "getSelect('vCoatPassTextBox');", true);
    }

    protected void coatpasstextchanged(object sender, EventArgs e)
    {
        TextBox est = (TextBox)FormView_Ink.FindControl("vEstNumTextBox");
        Label formno = (Label)FormView_Ink.FindControl("vFormNoTextBox");
        TextBox color = (TextBox)FormView_Ink.FindControl("vColorTextBox");
        TextBox pass = (TextBox)FormView_Ink.FindControl("vPassesTextBox");
        TextBox coat = (TextBox)FormView_Ink.FindControl("vCoatTextBox");
        TextBox coatpass = (TextBox)FormView_Ink.FindControl("vCoatPassTextBox");
        TextBox dscr = (TextBox)FormView_Ink.FindControl("vDscrTextBox");
        TextBox ps1 = (TextBox)FormView_Ink.FindControl("vPs1TextBox");
        TextBox ps2 = (TextBox)FormView_Ink.FindControl("vPs2TextBox");
        TextBox ps3 = (TextBox)FormView_Ink.FindControl("vPs3TextBox");
        TextBox ps4 = (TextBox)FormView_Ink.FindControl("vPs4TextBox");
        TextBox ps5 = (TextBox)FormView_Ink.FindControl("vPs5TextBox");
        TextBox ps6 = (TextBox)FormView_Ink.FindControl("vPs6TextBox");
        TextBox ps7 = (TextBox)FormView_Ink.FindControl("vPs7TextBox");
        TextBox ps8 = (TextBox)FormView_Ink.FindControl("vPs8TextBox");
        TextBox ps9 = (TextBox)FormView_Ink.FindControl("vPs9TextBox");
        TextBox ps10 = (TextBox)FormView_Ink.FindControl("vPs10TextBox");
        TextBox code1 = (TextBox)FormView_Ink.FindControl("vCode1TextBox");
        TextBox code2 = (TextBox)FormView_Ink.FindControl("vCode2TextBox");
        TextBox code3 = (TextBox)FormView_Ink.FindControl("vCode3TextBox");
        TextBox code4 = (TextBox)FormView_Ink.FindControl("vCode4TextBox");
        TextBox code5 = (TextBox)FormView_Ink.FindControl("vCode5TextBox");
        TextBox code6 = (TextBox)FormView_Ink.FindControl("vCode6TextBox");
        TextBox code7 = (TextBox)FormView_Ink.FindControl("vCode7TextBox");
        TextBox code8 = (TextBox)FormView_Ink.FindControl("vCode8TextBox");
        TextBox code9 = (TextBox)FormView_Ink.FindControl("vCode9TextBox");
        TextBox code10 = (TextBox)FormView_Ink.FindControl("vCode10TextBox");
        TextBox dscr1 = (TextBox)FormView_Ink.FindControl("vDscr1TextBox");
        TextBox dscr2 = (TextBox)FormView_Ink.FindControl("vDscr2TextBox");
        TextBox dscr3 = (TextBox)FormView_Ink.FindControl("vDscr3TextBox");
        TextBox dscr4 = (TextBox)FormView_Ink.FindControl("vDscr4TextBox");
        TextBox dscr5 = (TextBox)FormView_Ink.FindControl("vDscr5TextBox");
        TextBox dscr6 = (TextBox)FormView_Ink.FindControl("vDscr6TextBox");
        TextBox dscr7 = (TextBox)FormView_Ink.FindControl("vDscr7TextBox");
        TextBox dscr8 = (TextBox)FormView_Ink.FindControl("vDscr8TextBox");
        TextBox dscr9 = (TextBox)FormView_Ink.FindControl("vDscr9TextBox");
        TextBox dscr10 = (TextBox)FormView_Ink.FindControl("vDscr10TextBox");
        TextBox per1 = (TextBox)FormView_Ink.FindControl("vPer1TextBox");
        TextBox per2 = (TextBox)FormView_Ink.FindControl("vPer2TextBox");
        TextBox per3 = (TextBox)FormView_Ink.FindControl("vPer3TextBox");
        TextBox per4 = (TextBox)FormView_Ink.FindControl("vPer4TextBox");
        TextBox per5 = (TextBox)FormView_Ink.FindControl("vPer5TextBox");
        TextBox per6 = (TextBox)FormView_Ink.FindControl("vPer6TextBox");
        TextBox per7 = (TextBox)FormView_Ink.FindControl("vPer7TextBox");
        TextBox per8 = (TextBox)FormView_Ink.FindControl("vPer8TextBox");
        TextBox per9 = (TextBox)FormView_Ink.FindControl("vPer9TextBox");
        TextBox per10 = (TextBox)FormView_Ink.FindControl("vPer10TextBox");


        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];

        Corrugated corr = new Corrugated();
        DataSet ds = new DataSet();
        ds = corr.CorrInks(UserLogin.UserName, "CoatPassChange", "", est.Text.Trim(), Convert.ToInt32(formno.Text.Trim()), Convert.ToInt32(color.Text.Trim()), Convert.ToInt32(pass.Text.Trim()), Convert.ToInt32(coat.Text.Trim()), Convert.ToInt32(coatpass.Text.Trim()), dscr.Text.Trim(), Convert.ToInt32(ps1.Text.Trim()), Convert.ToInt32(ps2.Text.Trim()), Convert.ToInt32(ps3.Text.Trim()), Convert.ToInt32(ps4.Text.Trim()), Convert.ToInt32(ps5.Text.Trim()), Convert.ToInt32(ps6.Text.Trim()), Convert.ToInt32(ps7.Text.Trim()), Convert.ToInt32(ps8.Text.Trim()), Convert.ToInt32(ps9.Text.Trim()), Convert.ToInt32(ps10.Text.Trim()), code1.Text.Trim(), code2.Text.Trim(), code3.Text.Trim(), code4.Text.Trim(), code5.Text.Trim(), code6.Text.Trim(), code7.Text.Trim(), code8.Text.Trim(), code9.Text.Trim(), code10.Text.Trim(), dscr1.Text.Trim(), dscr2.Text.Trim(), dscr3.Text.Trim(), dscr4.Text.Trim(), dscr5.Text.Trim(), dscr6.Text.Trim(), dscr7.Text.Trim(), dscr8.Text.Trim(), dscr9.Text.Trim(), dscr10.Text.Trim(), Convert.ToInt32(per1.Text.Trim()), Convert.ToInt32(per2.Text.Trim()), Convert.ToInt32(per3.Text.Trim()), Convert.ToInt32(per4.Text.Trim()), Convert.ToInt32(per5.Text.Trim()), Convert.ToInt32(per6.Text.Trim()), Convert.ToInt32(per7.Text.Trim()), Convert.ToInt32(per8.Text.Trim()), Convert.ToInt32(per9.Text.Trim()), Convert.ToInt32(per10.Text.Trim()), "", 0, 0, 0, 0, 0, 0, 0, "", 0, 0, 0, 0, 0, 0, 0, "", "", 0, "", "", "", 0, 0, Convert.ToInt32(Session["order_corrugated_blankno"]));

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
        code1.Text = ds.Tables[0].Rows[0][22].ToString();
        code2.Text = ds.Tables[0].Rows[0][23].ToString();
        code3.Text = ds.Tables[0].Rows[0][24].ToString();
        code4.Text = ds.Tables[0].Rows[0][25].ToString();
        code5.Text = ds.Tables[0].Rows[0][26].ToString();
        code6.Text = ds.Tables[0].Rows[0][27].ToString();
        code7.Text = ds.Tables[0].Rows[0][28].ToString();
        code8.Text = ds.Tables[0].Rows[0][29].ToString();
        code9.Text = ds.Tables[0].Rows[0][30].ToString();
        code10.Text = ds.Tables[0].Rows[0][31].ToString();
        dscr1.Text = ds.Tables[0].Rows[0][32].ToString();
        dscr2.Text = ds.Tables[0].Rows[0][33].ToString();
        dscr3.Text = ds.Tables[0].Rows[0][34].ToString();
        dscr4.Text = ds.Tables[0].Rows[0][35].ToString();
        dscr5.Text = ds.Tables[0].Rows[0][36].ToString();
        dscr6.Text = ds.Tables[0].Rows[0][37].ToString();
        dscr7.Text = ds.Tables[0].Rows[0][38].ToString();
        dscr8.Text = ds.Tables[0].Rows[0][39].ToString();
        dscr9.Text = ds.Tables[0].Rows[0][40].ToString();
        dscr10.Text = ds.Tables[0].Rows[0][41].ToString();
        per1.Text = ds.Tables[0].Rows[0][42].ToString();
        per2.Text = ds.Tables[0].Rows[0][43].ToString();
        per3.Text = ds.Tables[0].Rows[0][44].ToString();
        per4.Text = ds.Tables[0].Rows[0][45].ToString();
        per5.Text = ds.Tables[0].Rows[0][46].ToString();
        per6.Text = ds.Tables[0].Rows[0][47].ToString();
        per7.Text = ds.Tables[0].Rows[0][48].ToString();
        per8.Text = ds.Tables[0].Rows[0][49].ToString();
        per9.Text = ds.Tables[0].Rows[0][50].ToString();
        per10.Text = ds.Tables[0].Rows[0][51].ToString();

        dscr.Focus();

        Page.ClientScript.RegisterStartupScript(this.GetType(), "alert", "getSelect('vDscrTextBox');", true);
    }
    protected void passestextchanged(object sender, EventArgs e)
    {
        TextBox est = (TextBox)FormView_Ink.FindControl("vEstNumTextBox");
        Label formno = (Label)FormView_Ink.FindControl("vFormNoTextBox");
        TextBox color = (TextBox)FormView_Ink.FindControl("vColorTextBox");
        TextBox pass = (TextBox)FormView_Ink.FindControl("vPassesTextBox");
        TextBox coat = (TextBox)FormView_Ink.FindControl("vCoatTextBox");
        TextBox coatpass = (TextBox)FormView_Ink.FindControl("vCoatPassTextBox");
        TextBox dscr = (TextBox)FormView_Ink.FindControl("vDscrTextBox");
        TextBox ps1 = (TextBox)FormView_Ink.FindControl("vPs1TextBox");
        TextBox ps2 = (TextBox)FormView_Ink.FindControl("vPs2TextBox");
        TextBox ps3 = (TextBox)FormView_Ink.FindControl("vPs3TextBox");
        TextBox ps4 = (TextBox)FormView_Ink.FindControl("vPs4TextBox");
        TextBox ps5 = (TextBox)FormView_Ink.FindControl("vPs5TextBox");
        TextBox ps6 = (TextBox)FormView_Ink.FindControl("vPs6TextBox");
        TextBox ps7 = (TextBox)FormView_Ink.FindControl("vPs7TextBox");
        TextBox ps8 = (TextBox)FormView_Ink.FindControl("vPs8TextBox");
        TextBox ps9 = (TextBox)FormView_Ink.FindControl("vPs9TextBox");
        TextBox ps10 = (TextBox)FormView_Ink.FindControl("vPs10TextBox");
        TextBox code1 = (TextBox)FormView_Ink.FindControl("vCode1TextBox");
        TextBox code2 = (TextBox)FormView_Ink.FindControl("vCode2TextBox");
        TextBox code3 = (TextBox)FormView_Ink.FindControl("vCode3TextBox");
        TextBox code4 = (TextBox)FormView_Ink.FindControl("vCode4TextBox");
        TextBox code5 = (TextBox)FormView_Ink.FindControl("vCode5TextBox");
        TextBox code6 = (TextBox)FormView_Ink.FindControl("vCode6TextBox");
        TextBox code7 = (TextBox)FormView_Ink.FindControl("vCode7TextBox");
        TextBox code8 = (TextBox)FormView_Ink.FindControl("vCode8TextBox");
        TextBox code9 = (TextBox)FormView_Ink.FindControl("vCode9TextBox");
        TextBox code10 = (TextBox)FormView_Ink.FindControl("vCode10TextBox");
        TextBox dscr1 = (TextBox)FormView_Ink.FindControl("vDscr1TextBox");
        TextBox dscr2 = (TextBox)FormView_Ink.FindControl("vDscr2TextBox");
        TextBox dscr3 = (TextBox)FormView_Ink.FindControl("vDscr3TextBox");
        TextBox dscr4 = (TextBox)FormView_Ink.FindControl("vDscr4TextBox");
        TextBox dscr5 = (TextBox)FormView_Ink.FindControl("vDscr5TextBox");
        TextBox dscr6 = (TextBox)FormView_Ink.FindControl("vDscr6TextBox");
        TextBox dscr7 = (TextBox)FormView_Ink.FindControl("vDscr7TextBox");
        TextBox dscr8 = (TextBox)FormView_Ink.FindControl("vDscr8TextBox");
        TextBox dscr9 = (TextBox)FormView_Ink.FindControl("vDscr9TextBox");
        TextBox dscr10 = (TextBox)FormView_Ink.FindControl("vDscr10TextBox");
        TextBox per1 = (TextBox)FormView_Ink.FindControl("vPer1TextBox");
        TextBox per2 = (TextBox)FormView_Ink.FindControl("vPer2TextBox");
        TextBox per3 = (TextBox)FormView_Ink.FindControl("vPer3TextBox");
        TextBox per4 = (TextBox)FormView_Ink.FindControl("vPer4TextBox");
        TextBox per5 = (TextBox)FormView_Ink.FindControl("vPer5TextBox");
        TextBox per6 = (TextBox)FormView_Ink.FindControl("vPer6TextBox");
        TextBox per7 = (TextBox)FormView_Ink.FindControl("vPer7TextBox");
        TextBox per8 = (TextBox)FormView_Ink.FindControl("vPer8TextBox");
        TextBox per9 = (TextBox)FormView_Ink.FindControl("vPer9TextBox");
        TextBox per10 = (TextBox)FormView_Ink.FindControl("vPer10TextBox");


        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];

        Corrugated corr = new Corrugated();
        DataSet ds = new DataSet();
        ds = corr.CorrInks(UserLogin.UserName, "PassesChange", "", est.Text.Trim(), Convert.ToInt32(formno.Text.Trim()), Convert.ToInt32(color.Text.Trim()), Convert.ToInt32(pass.Text.Trim()), Convert.ToInt32(coat.Text.Trim()), Convert.ToInt32(coatpass.Text.Trim()), dscr.Text.Trim(), Convert.ToInt32(ps1.Text.Trim()), Convert.ToInt32(ps2.Text.Trim()), Convert.ToInt32(ps3.Text.Trim()), Convert.ToInt32(ps4.Text.Trim()), Convert.ToInt32(ps5.Text.Trim()), Convert.ToInt32(ps6.Text.Trim()), Convert.ToInt32(ps7.Text.Trim()), Convert.ToInt32(ps8.Text.Trim()), Convert.ToInt32(ps9.Text.Trim()), Convert.ToInt32(ps10.Text.Trim()), code1.Text.Trim(), code2.Text.Trim(), code3.Text.Trim(), code4.Text.Trim(), code5.Text.Trim(), code6.Text.Trim(), code7.Text.Trim(), code8.Text.Trim(), code9.Text.Trim(), code10.Text.Trim(), dscr1.Text.Trim(), dscr2.Text.Trim(), dscr3.Text.Trim(), dscr4.Text.Trim(), dscr5.Text.Trim(), dscr6.Text.Trim(), dscr7.Text.Trim(), dscr8.Text.Trim(), dscr9.Text.Trim(), dscr10.Text.Trim(), Convert.ToInt32(per1.Text.Trim()), Convert.ToInt32(per2.Text.Trim()), Convert.ToInt32(per3.Text.Trim()), Convert.ToInt32(per4.Text.Trim()), Convert.ToInt32(per5.Text.Trim()), Convert.ToInt32(per6.Text.Trim()), Convert.ToInt32(per7.Text.Trim()), Convert.ToInt32(per8.Text.Trim()), Convert.ToInt32(per9.Text.Trim()), Convert.ToInt32(per10.Text.Trim()), "", 0, 0, 0, 0, 0, 0, 0, "", 0, 0, 0, 0, 0, 0, 0, "", "", 0, "", "", "", 0, 0, Convert.ToInt32(Session["order_corrugated_blankno"]));

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
        code1.Text = ds.Tables[0].Rows[0][22].ToString();
        code2.Text = ds.Tables[0].Rows[0][23].ToString();
        code3.Text = ds.Tables[0].Rows[0][24].ToString();
        code4.Text = ds.Tables[0].Rows[0][25].ToString();
        code5.Text = ds.Tables[0].Rows[0][26].ToString();
        code6.Text = ds.Tables[0].Rows[0][27].ToString();
        code7.Text = ds.Tables[0].Rows[0][28].ToString();
        code8.Text = ds.Tables[0].Rows[0][29].ToString();
        code9.Text = ds.Tables[0].Rows[0][30].ToString();
        code10.Text = ds.Tables[0].Rows[0][31].ToString();
        dscr1.Text = ds.Tables[0].Rows[0][32].ToString();
        dscr2.Text = ds.Tables[0].Rows[0][33].ToString();
        dscr3.Text = ds.Tables[0].Rows[0][34].ToString();
        dscr4.Text = ds.Tables[0].Rows[0][35].ToString();
        dscr5.Text = ds.Tables[0].Rows[0][36].ToString();
        dscr6.Text = ds.Tables[0].Rows[0][37].ToString();
        dscr7.Text = ds.Tables[0].Rows[0][38].ToString();
        dscr8.Text = ds.Tables[0].Rows[0][39].ToString();
        dscr9.Text = ds.Tables[0].Rows[0][40].ToString();
        dscr10.Text = ds.Tables[0].Rows[0][41].ToString();
        per1.Text = ds.Tables[0].Rows[0][42].ToString();
        per2.Text = ds.Tables[0].Rows[0][43].ToString();
        per3.Text = ds.Tables[0].Rows[0][44].ToString();
        per4.Text = ds.Tables[0].Rows[0][45].ToString();
        per5.Text = ds.Tables[0].Rows[0][46].ToString();
        per6.Text = ds.Tables[0].Rows[0][47].ToString();
        per7.Text = ds.Tables[0].Rows[0][48].ToString();
        per8.Text = ds.Tables[0].Rows[0][49].ToString();
        per9.Text = ds.Tables[0].Rows[0][50].ToString();
        per10.Text = ds.Tables[0].Rows[0][51].ToString();

        coat.Focus();

        Page.ClientScript.RegisterStartupScript(this.GetType(), "alert", "getSelect('vCoatTextBox');", true);
    }

    protected void unitcalcbutton_Click(object sender, EventArgs e)
    {
        Session["ink_update_button"] = 2;
        HiddenField_unit_cal.Value = "1";
    }

    protected void overwrite_Click(object sender, EventArgs e)
    {
        Session["ink_update_button"] = null;
        HiddenField_unit_cal.Value = "5";
    }
   
    protected void FormView_Inks_DataBound(object sender, EventArgs e)
    {
        if (FormView_Ink.CurrentMode == FormViewMode.ReadOnly)
        {
            try
            {
                Label box_img_label = (Label)FormView_Ink.FindControl("labelImagpath");
                Image img = (Image)FormView_Ink.FindControl("ImageMap");
                string imgpath = box_img_label.Text;
                string firstchar = imgpath.Substring(0, 1);
                string laststr = imgpath.Substring(1, imgpath.Length - 1);

                if (firstchar == "p" || firstchar == "P" || firstchar == "q" || firstchar == "Q")
                {
                    imgpath = "D" + laststr;
                }
                
                

                if (File.Exists(imgpath))
                {
                    string imgfilename = Path.GetFileName(imgpath);
                    string dirPath = Server.MapPath(@"Images\Rcode\die");
                    string destimgpath = Server.MapPath(@"Images\Rcode\die" + "\\" + imgfilename);

                    string[] files = Directory.GetFiles(dirPath);
                    foreach (string file in files)
                        File.Delete(file);

                    File.Copy(imgpath, destimgpath);
                    img.ImageUrl = "~/Images/Rcode/die/" + imgfilename;
                }
            }
            catch { }
            if (FormView_Ink.CurrentMode == FormViewMode.ReadOnly)
            {
                UserClass UserLogin = (UserClass)Session["User"];
                Button job = (Button)FormView_Ink.FindControl("jobButton");
                if (Session["User"] != null)
                {
                    string vUserId = UserLogin.UserName;
                    string vPage = "corr_inks.aspx";
                    string aUsers = null;
                    string PrmComp = null;
                    bool vCanCreate = false;
                    bool vCanRun = false;
                    bool vCanUpdate = false;
                    bool vCanDelete = false;


                    func1 f1 = new func1();
                    //Response.Write(Page);
                    f1.CheckProgramPermissions(vPage, vUserId, ref  vCanCreate, ref  vCanRun, ref  vCanUpdate, ref  vCanDelete, ref  PrmComp, ref  aUsers);
                    try
                    {
                        if (aUsers == "external")
                        {
                            job.Visible = false;
                        }
                    }
                    catch { }
                }
            }
        }

        if (FormView_Ink.CurrentMode == FormViewMode.Edit)
        {
            TextBox color = (TextBox)FormView_Ink.FindControl("vColorTextBox");
            TextBox pass = (TextBox)FormView_Ink.FindControl("vPassesTextBox");
            TextBox coat = (TextBox)FormView_Ink.FindControl("vCoatTextBox");
            TextBox coatpass = (TextBox)FormView_Ink.FindControl("vCoatPassTextBox");
            TextBox dscr=(TextBox)FormView_Ink.FindControl("vDscrTextBox");
            TextBox ps1 = (TextBox)FormView_Ink.FindControl("vPs1TextBox");
            TextBox ps2 = (TextBox)FormView_Ink.FindControl("vPs2TextBox");
            TextBox ps3 = (TextBox)FormView_Ink.FindControl("vPs3TextBox");
            TextBox ps4 = (TextBox)FormView_Ink.FindControl("vPs4TextBox");
            TextBox ps5 = (TextBox)FormView_Ink.FindControl("vPs5TextBox");
            TextBox ps6 = (TextBox)FormView_Ink.FindControl("vPs6TextBox");
            TextBox ps7 = (TextBox)FormView_Ink.FindControl("vPs7TextBox");
            TextBox ps8 = (TextBox)FormView_Ink.FindControl("vPs8TextBox");
            TextBox ps9 = (TextBox)FormView_Ink.FindControl("vPs9TextBox");
            TextBox ps10 = (TextBox)FormView_Ink.FindControl("vPs10TextBox");
            TextBox code1 = (TextBox)FormView_Ink.FindControl("vCode1TextBox");
            TextBox code2 = (TextBox)FormView_Ink.FindControl("vCode2TextBox");
            TextBox code3 = (TextBox)FormView_Ink.FindControl("vCode3TextBox");
            TextBox code4 = (TextBox)FormView_Ink.FindControl("vCode4TextBox");
            TextBox code5 = (TextBox)FormView_Ink.FindControl("vCode5TextBox");
            TextBox code6 = (TextBox)FormView_Ink.FindControl("vCode6TextBox");
            TextBox code7 = (TextBox)FormView_Ink.FindControl("vCode7TextBox");
            TextBox code8 = (TextBox)FormView_Ink.FindControl("vCode8TextBox");
            TextBox code9 = (TextBox)FormView_Ink.FindControl("vCode9TextBox");
            TextBox code10 = (TextBox)FormView_Ink.FindControl("vCode10TextBox");
            TextBox dscr1 = (TextBox)FormView_Ink.FindControl("vDscr1TextBox");
            TextBox dscr2 = (TextBox)FormView_Ink.FindControl("vDscr2TextBox");
            TextBox dscr3 = (TextBox)FormView_Ink.FindControl("vDscr3TextBox");
            TextBox dscr4 = (TextBox)FormView_Ink.FindControl("vDscr4TextBox");
            TextBox dscr5 = (TextBox)FormView_Ink.FindControl("vDscr5TextBox");
            TextBox dscr6 = (TextBox)FormView_Ink.FindControl("vDscr6TextBox");
            TextBox dscr7 = (TextBox)FormView_Ink.FindControl("vDscr7TextBox");
            TextBox dscr8 = (TextBox)FormView_Ink.FindControl("vDscr8TextBox");
            TextBox dscr9 = (TextBox)FormView_Ink.FindControl("vDscr9TextBox");
            TextBox dscr10 = (TextBox)FormView_Ink.FindControl("vDscr10TextBox");
            TextBox per1 = (TextBox)FormView_Ink.FindControl("vPer1TextBox");
            TextBox per2 = (TextBox)FormView_Ink.FindControl("vPer2TextBox");
            TextBox per3 = (TextBox)FormView_Ink.FindControl("vPer3TextBox");
            TextBox per4 = (TextBox)FormView_Ink.FindControl("vPer4TextBox");
            TextBox per5 = (TextBox)FormView_Ink.FindControl("vPer5TextBox");
            TextBox per6 = (TextBox)FormView_Ink.FindControl("vPer6TextBox");
            TextBox per7 = (TextBox)FormView_Ink.FindControl("vPer7TextBox");
            TextBox per8 = (TextBox)FormView_Ink.FindControl("vPer8TextBox");
            TextBox per9 = (TextBox)FormView_Ink.FindControl("vPer9TextBox");
            TextBox per10 = (TextBox)FormView_Ink.FindControl("vPer10TextBox");

            TextBox packcode = (TextBox)FormView_Ink.FindControl("vPackCodeTextBox");
            TextBox unitlen = (TextBox)FormView_Ink.FindControl("vUnitLenTextBox");
            TextBox cost1 = (TextBox)FormView_Ink.FindControl("vCostTextBox");
            TextBox unitwid = (TextBox)FormView_Ink.FindControl("vUnitWidTextBox");
            TextBox boxcode = (TextBox)FormView_Ink.FindControl("vBoxCodeTextBox");
            TextBox unitdep = (TextBox)FormView_Ink.FindControl("vUnitDepTextBox");
            TextBox bundl = (TextBox)FormView_Ink.FindControl("vBundlTextBox");
            TextBox wtpack = (TextBox)FormView_Ink.FindControl("vWtPackTextBox");
            TextBox unit = (TextBox)FormView_Ink.FindControl("vUnitTextBox");
            TextBox len = (TextBox)FormView_Ink.FindControl("vLengthTextBox");
            TextBox cost2 = (TextBox)FormView_Ink.FindControl("vCost2TextBox");
            TextBox wid = (TextBox)FormView_Ink.FindControl("vWidthTextBox");
            TextBox count = (TextBox)FormView_Ink.FindControl("vCountTextBox");
            TextBox hight = (TextBox)FormView_Ink.FindControl("vHeightTextBox");
            TextBox layer = (TextBox)FormView_Ink.FindControl("vLayerTextBox");
            TextBox stack = (TextBox)FormView_Ink.FindControl("vStackTextBox");
            TextBox stcode = (TextBox)FormView_Ink.FindControl("vStCodeTextBox");
            TextBox weiper = (TextBox)FormView_Ink.FindControl("vWeiPerTextBox");
            TextBox carrier = (TextBox)FormView_Ink.FindControl("vCarrierTextBox");
            TextBox carrdscr = (TextBox)FormView_Ink.FindControl("vCarrDscrTextBox");
            TextBox delzon = (TextBox)FormView_Ink.FindControl("vDelZonTextBox");
            TextBox frcwt = (TextBox)FormView_Ink.FindControl("vFreifgtTextBox");
            TextBox frm = (TextBox)FormView_Ink.FindControl("vFreOutTextBox");
            RadioButtonList fright = (RadioButtonList)FormView_Ink.FindControl("RadioButtonList1");
            Image image1 = (Image)FormView_Ink.FindControl("Image1");
            Image image2 = (Image)FormView_Ink.FindControl("Image2");
            Image image3 = (Image)FormView_Ink.FindControl("Image3");
            Image image4 = (Image)FormView_Ink.FindControl("Image4");
            Image image5 = (Image)FormView_Ink.FindControl("Image5");
            Image image6 = (Image)FormView_Ink.FindControl("Image6");
            Image image7 = (Image)FormView_Ink.FindControl("Image7");
            Image image8 = (Image)FormView_Ink.FindControl("Image8");
            Image image9 = (Image)FormView_Ink.FindControl("Image9");
            Image image10 = (Image)FormView_Ink.FindControl("Image11");

            Image image11 = (Image)FormView_Ink.FindControl("Image10");
            Image image12 = (Image)FormView_Ink.FindControl("Image12");
            Image image13 = (Image)FormView_Ink.FindControl("Image13");
            //Image image14 = (Image)FormView_Ink.FindControl("Image14");
            Image image15 = (Image)FormView_Ink.FindControl("Image15");

            vBundlHiddenField.Value     = bundl.Text.Trim();
            vLengthHiddenField.Value    = len.Text.Trim();
            vCost2HiddenField.Value     = cost2.Text.Trim();
            vWidthHiddenField.Value     = wid.Text.Trim();
            vCountHiddenField.Value     = count.Text.Trim();
            vHeightHiddenField.Value    = hight.Text.Trim();
            vLayerHiddenField.Value     = layer.Text.Trim();
            vStackHiddenField.Value     = stack.Text.Trim();
            vStcodeHiddenField.Value    = stcode.Text.Trim();            

            if (Convert.ToInt32(stack.Text) <= 0)
            {
                stack.Text = "1";
            }

            if (Convert.ToInt32(Session["ink_update_button"]) == 1)
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
                stack.ReadOnly = true;
                stack.BackColor = System.Drawing.Color.Turquoise;
                stcode.ReadOnly = true;
                stcode.BackColor = System.Drawing.Color.Turquoise;
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



                fright.Enabled = false;
                image11.Visible = false;
                image12.Visible = false;
                image13.Visible = false;
                //image14.Visible = false;
                image15.Visible = false;

                color.Focus();

                Page.ClientScript.RegisterStartupScript(this.GetType(), "alert", "getSelect('vColorTextBox');", true);
            }
            else if (Convert.ToInt32(Session["ink_update_button"]) == 2)
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
                bundl.ReadOnly = true;
                bundl.BackColor = System.Drawing.Color.Turquoise;
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
                stack.ReadOnly = true;
                stack.BackColor = System.Drawing.Color.Turquoise;
                stcode.ReadOnly = true;
                stcode.BackColor = System.Drawing.Color.Turquoise;
                carrdscr.ReadOnly = true;
                carrdscr.BackColor = System.Drawing.Color.Turquoise;
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

                packcode.Focus();

                Page.ClientScript.RegisterStartupScript(this.GetType(), "alert", "getSelect('vPackCodeTextBox');", true);
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

                packcode.Focus();

                Page.ClientScript.RegisterStartupScript(this.GetType(), "alert", "getSelect('vPackCodeTextBox');", true);
            }
            try
            {
                Label box_img_label = (Label)FormView_Ink.FindControl("labelImagpath");
                Image img = (Image)FormView_Ink.FindControl("ImageMap");
                string imgpath = box_img_label.Text;
                string firstchar = imgpath.Substring(0, 1);
                string laststr = imgpath.Substring(1, imgpath.Length - 1);

                if (firstchar == "p" || firstchar == "P" || firstchar == "q" || firstchar == "Q")
                {
                    imgpath = "D" + laststr;
                }

                if (File.Exists(imgpath))
                {
                    string imgfilename = Path.GetFileName(imgpath);
                    string dirPath = Server.MapPath(@"Images\Rcode\die");
                    string destimgpath = Server.MapPath(@"Images\Rcode\die" + "\\" + imgfilename);

                    string[] files = Directory.GetFiles(dirPath);
                    foreach (string file in files)
                        File.Delete(file);

                    File.Copy(imgpath, destimgpath);
                    img.ImageUrl = "~/Images/Rcode/die/" + imgfilename;
                }
            }
            catch { }
           
        }
    }

    protected void Job_Button_Click(object sender, EventArgs e)
    {
        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];

        Session["corr_vendor_cost_est"] = Session["order_corrugated_est"];
        Session["corr_vendor_cost_form"] = Session["order_corrugated_formno"];
        Session["corr_vendor_cost_blank"] = Session["order_corrugated_blankno"];
        
        string vmessage = "";
        Corrugated corr = new Corrugated();
        corr.SelectPrep(UserLogin.UserName, "jobstd", "", "", Convert.ToString(Session["order_corrugated_est"]), Convert.ToInt32(Session["order_corrugated_formno"]), 0, 0, "", 0, "", 0, "", "", 0, 0, 0, 0, ref vmessage);

        if (vmessage != "")
        {

            Page.ClientScript.RegisterStartupScript(this.GetType(), "alert", "confirmAdd('" + vmessage + "');", true);

        }
        //ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "jobstd";
        //ObjectDataSource1.SelectParameters["prmLine"].DefaultValue = line.Text.Trim();

    }    
   
}

