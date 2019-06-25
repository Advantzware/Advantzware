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
using System.IO;

public partial class corr_specs : System.Web.UI.Page
{
    protected void Page_Load(object sender, EventArgs e)
    {
        
       
        CorrugatedSpecsDataSource.SelectParameters["prmAction"].DefaultValue = "Select";
        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];
        ObjectDataSource_list.SelectParameters["prmAction"].DefaultValue = "ListEst";
        /*GridView1.DataBind();*/

        if (!Page.IsPostBack)
        {
        }
        if (Session["User"] != null)
        {
            string vUserId = UserLogin.UserName;
            string vPage = "corr_specs.aspx";
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
            /*ImageButton est = (ImageButton)Master.FindControl("Ink_specs");
            est.ImageUrl = "~/images/specs_1.jpg";*/

            Image img_mov_col = (Image)Master.FindControl("Image5");
            img_mov_col.Visible = false;
        }
        catch { }

        if (Session["index_list1_corrugated"] != null)
        {
            GridView1.SelectedIndex = Convert.ToInt32(Session["index_list1_corrugated"]);
        }
        
    }

    protected void Save_Click(object sender, EventArgs e)
    {
        Label est = (Label)FormView_Specs.FindControl("vEstNumTextBox");
        TextBox form = (TextBox)FormView_Specs.FindControl("vFromDtTextBox");
        TextBox cust = (TextBox)FormView_Specs.FindControl("vCustNumTextBox");
        TextBox shipto = (TextBox)FormView_Specs.FindControl("vShipToTextBox");
        TextBox shipname = (TextBox)FormView_Specs.FindControl("vShipNameTextBox");
        TextBox addr = (TextBox)FormView_Specs.FindControl("vAddrTextBox");
        TextBox addr2 = (TextBox)FormView_Specs.FindControl("vAddr2TextBox");
        TextBox city = (TextBox)FormView_Specs.FindControl("vCityTextBox");
        TextBox state = (TextBox)FormView_Specs.FindControl("vStateTextBox");
        TextBox zip = (TextBox)FormView_Specs.FindControl("vZipTextBox");
        TextBox sales = (TextBox)FormView_Specs.FindControl("vSalesmanTextBox");
        TextBox saledscr = (TextBox)FormView_Specs.FindControl("vSmanDscrTextBox");
        TextBox comm = (TextBox)FormView_Specs.FindControl("vCommTextBox");
        TextBox categ = (TextBox)FormView_Specs.FindControl("vFgCategoryTextBox");
        TextBox catdscr = (TextBox)FormView_Specs.FindControl("vFgCatDscrTextBox");
        TextBox custpart = (TextBox)FormView_Specs.FindControl("vCustPartTextBox");
        TextBox fgitem = (TextBox)FormView_Specs.FindControl("vFgItemTextBox");
        TextBox itemname = (TextBox)FormView_Specs.FindControl("vItemNameTextBox");
        TextBox desc = (TextBox)FormView_Specs.FindControl("vDescrTextBox");
        TextBox die = (TextBox)FormView_Specs.FindControl("vDieNumTextBox");        
        TextBox cad = (TextBox)FormView_Specs.FindControl("vCadNumTextBox");
        TextBox plate = (TextBox)FormView_Specs.FindControl("vPlateNumTextBox");
        TextBox spcnum = (TextBox)FormView_Specs.FindControl("vSpcNumTextBox");
        TextBox upcnum = (TextBox)FormView_Specs.FindControl("vUpcNumTextBox");
        TextBox style = (TextBox)FormView_Specs.FindControl("vStyleTextBox");
        TextBox styledesc = (TextBox)FormView_Specs.FindControl("vStyleDscrTextBox");
        TextBox flute = (TextBox)FormView_Specs.FindControl("vFluteTextBox");
        TextBox test = (TextBox)FormView_Specs.FindControl("vTestTextBox");
        DropDownList tab = (DropDownList)FormView_Specs.FindControl("DropDownList1");
        DropDownList metric = (DropDownList)FormView_Specs.FindControl("DropDownList2");
        TextBox board = (TextBox)FormView_Specs.FindControl("vBoardTextBox");
        TextBox boardesc = (TextBox)FormView_Specs.FindControl("vBrdDscrTextBox");
        TextBox length = (TextBox)FormView_Specs.FindControl("vLengthTextBox");
        TextBox width = (TextBox)FormView_Specs.FindControl("vWidthTextBox");
        TextBox depth = (TextBox)FormView_Specs.FindControl("vDepthTextBox");
        TextBox jointmat = (TextBox)FormView_Specs.FindControl("vJointMatTextBox");
        TextBox dustflap = (TextBox)FormView_Specs.FindControl("vDustFlapTextBox");
        TextBox botflap = (TextBox)FormView_Specs.FindControl("vBotFlapTextBox");
        TextBox locktab = (TextBox)FormView_Specs.FindControl("vLockTabTextBox");
        TextBox tabwidth = (TextBox)FormView_Specs.FindControl("vTabWidTextBox");
        TextBox scorewid = (TextBox)FormView_Specs.FindControl("vScoreWidTextBox");
        TextBox scorelen = (TextBox)FormView_Specs.FindControl("vScoreLenTextBox");
        TextBox tuck = (TextBox)FormView_Specs.FindControl("vTuckTextBox");
        TextBox jointlength = (TextBox)FormView_Specs.FindControl("vJointLenTextBox");
        TextBox blackwid = (TextBox)FormView_Specs.FindControl("vBlankWidTextBox");
        TextBox blacklen = (TextBox)FormView_Specs.FindControl("vBlankLenTextBox");
        TextBox blacksqft = (TextBox)FormView_Specs.FindControl("vBlankSqFtTextBox");
        Label estdate = (Label)FormView_Specs.FindControl("vEstDateTextBox");
        TextBox image = (TextBox)FormView_Specs.FindControl("vImageTextBox");
        FileUpload FileUpload1 = (FileUpload)FormView_Specs.FindControl("FileUpload1");

        if (tab.SelectedIndex == 0)
        {
            HiddenField1.Value = "Out";
        }
        if (tab.SelectedIndex == 1)
        {
            HiddenField1.Value = "In";
        }
        if (metric.SelectedIndex == 0)
        {
            HiddenField2.Value = "Yes";
        }
        if (metric.SelectedIndex == 1)
        {
            HiddenField2.Value = "No";
        }

         UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];
        Corrugated corr = new Corrugated();

        bool check = corr.ValidateCorrSpec(UserLogin.UserName, "Override", cust.Text.Trim(), shipto.Text.Trim(), fgitem.Text.Trim(), custpart.Text.Trim(), itemname.Text.Trim(), "", "", "", "", "", "", "", sales.Text.Trim(), "", 0, categ.Text.Trim(), "", style.Text.Trim(), "", board.Text.Trim(), "", 0, 0, 0, flute.Text.Trim(), test.Text.Trim(), "", "", jointmat.Text.Trim(), 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, est.Text.Trim(), Convert.ToDateTime(estdate.Text.Trim()), Convert.ToDateTime(estdate.Text.Trim()), Convert.ToDateTime(estdate.Text.Trim()), 0, Convert.ToDateTime(estdate.Text.Trim()), 0, 0, 0, "", "", "", "", "", "", Convert.ToInt32(Session["order_corrugated_formno"]), "", Convert.ToInt32(Session["order_corrugated_blankno"]));

        string value = Convert.ToString(check);
        if (value == "True")
        {
            if (FileUpload1.FileName != "")
            {
                Label imagepath = (Label)FormView_Specs.FindControl("ImagePathLabel");
                string str1 = imagepath.Text.Trim();
                string str2 = Path.GetFileName(str1);
                string path = str2;

                string firstchar = str1.Substring(0, 1);
                string laststr = str1.Substring(1, str1.Length - 1);

                if (firstchar == "p" || firstchar == "P")
                {
                    str1 = "D" + laststr;
                }
                string str11 = FileUpload1.PostedFile.FileName;
                string str12 = Path.GetFileName(str11);
               // string str3 = @"D:\webapps\uploadedimages\";  //give the path where you want to upload the file.
                string str3 = str1;  //give the path where you want to upload the file.
                FileUpload1.PostedFile.SaveAs(Path.Combine(str3, str12));
                str12 = Path.GetFileNameWithoutExtension(str11);
                CorrugatedSpecsDataSource.SelectParameters["prmImage"].DefaultValue =  str12;
            }
            if (FileUpload1.FileName == "")
            {
                CorrugatedSpecsDataSource.SelectParameters["prmImage"].DefaultValue = image.Text.Trim();
            }

            CorrugatedSpecsDataSource.SelectParameters["prmAction"].DefaultValue = "Override";
            CorrugatedSpecsDataSource.SelectParameters["prmEstNum"].DefaultValue = est.Text.Trim();
            CorrugatedSpecsDataSource.SelectParameters["prmCustNum"].DefaultValue = cust.Text.Trim();
            CorrugatedSpecsDataSource.SelectParameters["prmShipTo"].DefaultValue = shipto.Text.Trim();
            CorrugatedSpecsDataSource.SelectParameters["prmFgItem"].DefaultValue = fgitem.Text.Trim();
            CorrugatedSpecsDataSource.SelectParameters["prmCustPart"].DefaultValue = custpart.Text.Trim();
            CorrugatedSpecsDataSource.SelectParameters["prmItemName"].DefaultValue = itemname.Text.Trim();
            CorrugatedSpecsDataSource.SelectParameters["prmPartDscr"].DefaultValue = desc.Text.Trim();
            CorrugatedSpecsDataSource.SelectParameters["prmDieNum"].DefaultValue = die.Text.Trim();
            CorrugatedSpecsDataSource.SelectParameters["prmCadNum"].DefaultValue = cad.Text.Trim();
            CorrugatedSpecsDataSource.SelectParameters["prmSpcNum"].DefaultValue = spcnum.Text.Trim();
            CorrugatedSpecsDataSource.SelectParameters["prmPlateNum"].DefaultValue = plate.Text.Trim();            
            CorrugatedSpecsDataSource.SelectParameters["prmUpcNum"].DefaultValue = upcnum.Text.Trim();
            CorrugatedSpecsDataSource.SelectParameters["prmSman"].DefaultValue = sales.Text.Trim();
            //CorrugatedSpecsDataSource.SelectParameters["prmSmanDscr"].DefaultValue = saledscr.Text.Trim();
            CorrugatedSpecsDataSource.SelectParameters["prmComm"].DefaultValue = comm.Text.Trim();
            CorrugatedSpecsDataSource.SelectParameters["prmFgCat"].DefaultValue = categ.Text.Trim();
            //CorrugatedSpecsDataSource.SelectParameters["prmFgCatDscr"].DefaultValue = catdscr.Text.Trim();
            CorrugatedSpecsDataSource.SelectParameters["prmStyle"].DefaultValue = style.Text.Trim();
            //CorrugatedSpecsDataSource.SelectParameters["prmStyDscr"].DefaultValue = styledesc.Text.Trim();
            CorrugatedSpecsDataSource.SelectParameters["prmBoard"].DefaultValue = board.Text.Trim();
            CorrugatedSpecsDataSource.SelectParameters["prmBrdDscr"].DefaultValue = boardesc.Text.Trim();
            CorrugatedSpecsDataSource.SelectParameters["prmLength"].DefaultValue = length.Text.Trim();
            CorrugatedSpecsDataSource.SelectParameters["prmWidth"].DefaultValue = width.Text.Trim();
            CorrugatedSpecsDataSource.SelectParameters["prmDepth"].DefaultValue = depth.Text.Trim();
            CorrugatedSpecsDataSource.SelectParameters["prmFlute"].DefaultValue = flute.Text.Trim();
            CorrugatedSpecsDataSource.SelectParameters["prmTest"].DefaultValue = test.Text.Trim();
            CorrugatedSpecsDataSource.SelectParameters["prmTab"].DefaultValue = HiddenField1.Value;
            CorrugatedSpecsDataSource.SelectParameters["prmMetric"].DefaultValue = HiddenField2.Value;
            CorrugatedSpecsDataSource.SelectParameters["prmJointMat"].DefaultValue = jointmat.Text.Trim();
            CorrugatedSpecsDataSource.SelectParameters["prmDustFlap"].DefaultValue = dustflap.Text.Trim();
            CorrugatedSpecsDataSource.SelectParameters["prmBotFlap"].DefaultValue = botflap.Text.Trim();
            CorrugatedSpecsDataSource.SelectParameters["prmLockTab"].DefaultValue = locktab.Text.Trim();
            CorrugatedSpecsDataSource.SelectParameters["prmTabWid"].DefaultValue = tabwidth.Text.Trim();
            CorrugatedSpecsDataSource.SelectParameters["prmScWid"].DefaultValue = scorewid.Text.Trim();
            CorrugatedSpecsDataSource.SelectParameters["prmScLen"].DefaultValue = scorelen.Text.Trim();
            CorrugatedSpecsDataSource.SelectParameters["prmTuck"].DefaultValue = tuck.Text.Trim();
            CorrugatedSpecsDataSource.SelectParameters["prmJointLen"].DefaultValue = jointlength.Text.Trim();
            CorrugatedSpecsDataSource.SelectParameters["prmBlankWid"].DefaultValue = blackwid.Text.Trim();
            CorrugatedSpecsDataSource.SelectParameters["prmBlankLen"].DefaultValue = blacklen.Text.Trim();
            CorrugatedSpecsDataSource.SelectParameters["prmBlankSqFt"].DefaultValue = blacksqft.Text.Trim();
            CorrugatedSpecsDataSource.SelectParameters["prmShipName"].DefaultValue = shipname.Text.Trim();
            //CorrugatedSpecsDataSource.SelectParameters["prmAddr"].DefaultValue = addr.Text.Trim();
            //CorrugatedSpecsDataSource.SelectParameters["prmAddr2"].DefaultValue = addr2.Text.Trim();

            //CorrugatedSpecsDataSource.SelectParameters["prmCity"].DefaultValue = city.Text.Trim();
            //CorrugatedSpecsDataSource.SelectParameters["prmState"].DefaultValue = state.Text.Trim();
            //CorrugatedSpecsDataSource.SelectParameters["prmZip"].DefaultValue = zip.Text.Trim();
            FormView_Specs.ChangeMode(FormViewMode.ReadOnly);
            Response.Write("<script>window.location.href='corr_specs.aspx'</script>");
        }

    }
    protected void updatebutton_click(object sender, EventArgs e)
    {
        Session["autocalc"] = null;
    }

    protected void AutoCalculateButton_Click(object sender, EventArgs e)
    {
        Session["autocalc"] = 1;
    }
    protected void FormView_Specs_DataBound(object sender, EventArgs e)
    {
        if (FormView_Specs.CurrentMode == FormViewMode.Edit)
        {
            GridView1.Visible = false;
            TextBox form = (TextBox)FormView_Specs.FindControl("vFromDtTextBox");
            TextBox cust = (TextBox)FormView_Specs.FindControl("vCustNumTextBox");
            TextBox shipto = (TextBox)FormView_Specs.FindControl("vShipToTextBox");
            TextBox shipname = (TextBox)FormView_Specs.FindControl("vShipNameTextBox");
            TextBox addr = (TextBox)FormView_Specs.FindControl("vAddrTextBox");
            TextBox addr2 = (TextBox)FormView_Specs.FindControl("vAddr2TextBox");
            TextBox city = (TextBox)FormView_Specs.FindControl("vCityTextBox");
            TextBox state = (TextBox)FormView_Specs.FindControl("vStateTextBox");
            TextBox zip = (TextBox)FormView_Specs.FindControl("vZipTextBox");
            TextBox sales = (TextBox)FormView_Specs.FindControl("vSalesmanTextBox");
            TextBox saledscr = (TextBox)FormView_Specs.FindControl("vSmanDscrTextBox");
            TextBox comm = (TextBox)FormView_Specs.FindControl("vCommTextBox");
            TextBox categ = (TextBox)FormView_Specs.FindControl("vFgCategoryTextBox");
            TextBox catdscr = (TextBox)FormView_Specs.FindControl("vFgCatDscrTextBox");
            TextBox custpart = (TextBox)FormView_Specs.FindControl("vCustPartTextBox");
            TextBox fgitem = (TextBox)FormView_Specs.FindControl("vFgItemTextBox");
            TextBox fgitemdscr = (TextBox)FormView_Specs.FindControl("vFgCatDscrTextBox");
            TextBox itemname = (TextBox)FormView_Specs.FindControl("vItemNameTextBox");
            TextBox desc = (TextBox)FormView_Specs.FindControl("vDescrTextBox");
            TextBox die = (TextBox)FormView_Specs.FindControl("vDieNumTextBox");
            TextBox image = (TextBox)FormView_Specs.FindControl("vImageTextBox");
            TextBox cad = (TextBox)FormView_Specs.FindControl("vCadNumTextBox");
            TextBox plate = (TextBox)FormView_Specs.FindControl("vPlateNumTextBox");
            TextBox spcnum = (TextBox)FormView_Specs.FindControl("vSpcNumTextBox");
            TextBox upcnum = (TextBox)FormView_Specs.FindControl("vUpcNumTextBox");
            TextBox style = (TextBox)FormView_Specs.FindControl("vStyleTextBox");                        
            
            TextBox styledesc = (TextBox)FormView_Specs.FindControl("vStyleDscrTextBox");
            TextBox flute = (TextBox)FormView_Specs.FindControl("vFluteTextBox");
            TextBox test = (TextBox)FormView_Specs.FindControl("vTestTextBox");
            DropDownList tab = (DropDownList)FormView_Specs.FindControl("DropDownList1");
            DropDownList metric = (DropDownList)FormView_Specs.FindControl("DropDownList2");
            TextBox board = (TextBox)FormView_Specs.FindControl("vBoardTextBox");
            TextBox boardesc = (TextBox)FormView_Specs.FindControl("vBrdDscrTextBox");
            TextBox length = (TextBox)FormView_Specs.FindControl("vLengthTextBox");
            TextBox width = (TextBox)FormView_Specs.FindControl("vWidthTextBox");
            TextBox depth = (TextBox)FormView_Specs.FindControl("vDepthTextBox");
            TextBox jointmat = (TextBox)FormView_Specs.FindControl("vJointMatTextBox");
            TextBox dustflap = (TextBox)FormView_Specs.FindControl("vDustFlapTextBox");
            TextBox botflap = (TextBox)FormView_Specs.FindControl("vBotFlapTextBox");
            TextBox locktab = (TextBox)FormView_Specs.FindControl("vLockTabTextBox");
            TextBox tabwidth = (TextBox)FormView_Specs.FindControl("vTabWidTextBox");
            TextBox scorewid = (TextBox)FormView_Specs.FindControl("vScoreWidTextBox");
            TextBox scorelen = (TextBox)FormView_Specs.FindControl("vScoreLenTextBox");
            TextBox tuck = (TextBox)FormView_Specs.FindControl("vTuckTextBox");
            TextBox jointlength = (TextBox)FormView_Specs.FindControl("vJointLenTextBox");
            TextBox blackwid = (TextBox)FormView_Specs.FindControl("vBlankWidTextBox");
            TextBox blacklen = (TextBox)FormView_Specs.FindControl("vBlankLenTextBox");
            TextBox blacksqft = (TextBox)FormView_Specs.FindControl("vBlankSqFtTextBox");
            FileUpload FileUploder1 = (FileUpload)FormView_Specs.FindControl("FileUpload1");

            TextBox blankwid  = (TextBox)FormView_Specs.FindControl("vBlankWidTextBox");
            TextBox blanklen  = (TextBox)FormView_Specs.FindControl("vBlankLenTextBox");
            TextBox blanksqft = (TextBox)FormView_Specs.FindControl("vBlankSqFtTextBox");                        

            Image image1 = (Image)FormView_Specs.FindControl("Image1");
            Image image2 = (Image)FormView_Specs.FindControl("Image2");
            Image image3 = (Image)FormView_Specs.FindControl("Image3");
            Image image4 = (Image)FormView_Specs.FindControl("Image4");
            Image image5 = (Image)FormView_Specs.FindControl("Image9");
            Image image6 = (Image)FormView_Specs.FindControl("Image10");
            Image image7 = (Image)FormView_Specs.FindControl("Image11");
            Image image8 = (Image)FormView_Specs.FindControl("Image12");
            Image image9 = (Image)FormView_Specs.FindControl("Image13");
            Image image10 = (Image)FormView_Specs.FindControl("Image14");
            Image image11 = (Image)FormView_Specs.FindControl("Image15");

            Button updatesave = (Button)FormView_Specs.FindControl("UpdateButton");
            Button autosave = (Button)FormView_Specs.FindControl("auto_cal_save_Button");
            form.ReadOnly = true;
            if (Session["autocalc"] != null)
            {
                FileUploder1.Enabled = false;
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
                image11.Visible = false;

                form.Enabled = false;
                cust.Enabled = false;
                shipto.Enabled = false;
                shipname.Enabled = false;
                addr.Enabled = false;
                addr2.Enabled = false;
                city.Enabled = false;
                state.Enabled = false;
                zip.Enabled = false;
                sales.Enabled = false;
                saledscr.Enabled = false;
                comm.Enabled = false;
                categ.Enabled = false;
                custpart.Enabled = false;
                fgitem.Enabled = false;
                fgitemdscr.Enabled = false;
                itemname.Enabled = false;
                desc.Enabled = false;
                die.Enabled = false;
                image.Enabled = false;
                cad.Enabled = false;
                plate.Enabled = false;
                spcnum.Enabled = false;
                upcnum.Enabled = false;
                updatesave.Visible = false;
                blankwid.Enabled = false;
                blanklen.Enabled = false;
                blanksqft.Enabled = false;

                style.Attributes.Add("onkeypress", "return clickButton(event,'" + autosave.ClientID + "')");
                flute.Attributes.Add("onkeypress", "return clickButton(event,'" + autosave.ClientID + "')");
                test.Attributes.Add("onkeypress", "return clickButton(event,'" + autosave.ClientID + "')");
                board.Attributes.Add("onkeypress", "return clickButton(event,'" + autosave.ClientID + "')");
                boardesc.Attributes.Add("onkeypress", "return clickButton(event,'" + autosave.ClientID + "')");
                length.Attributes.Add("onkeypress", "return clickButton(event,'" + autosave.ClientID + "')");
                width.Attributes.Add("onkeypress", "return clickButton(event,'" + autosave.ClientID + "')");
                depth.Attributes.Add("onkeypress", "return clickButton(event,'" + autosave.ClientID + "')");
                jointmat.Attributes.Add("onkeypress", "return clickButton(event,'" + autosave.ClientID + "')");
                dustflap.Attributes.Add("onkeypress", "return clickButton(event,'" + autosave.ClientID + "')");
                botflap.Attributes.Add("onkeypress", "return clickButton(event,'" + autosave.ClientID + "')");

                locktab.Attributes.Add("onkeypress", "return clickButton(event,'" + autosave.ClientID + "')");
                tabwidth.Attributes.Add("onkeypress", "return clickButton(event,'" + autosave.ClientID + "')");
                scorewid.Attributes.Add("onkeypress", "return clickButton(event,'" + autosave.ClientID + "')");
                scorelen.Attributes.Add("onkeypress", "return clickButton(event,'" + autosave.ClientID + "')");
                tuck.Attributes.Add("onkeypress", "return clickButton(event,'" + autosave.ClientID + "')");
                jointlength.Attributes.Add("onkeypress", "return clickButton(event,'" + autosave.ClientID + "')");
                blacklen.Attributes.Add("onkeypress", "return clickButton(event,'" + autosave.ClientID + "')");
                blackwid.Attributes.Add("onkeypress", "return clickButton(event,'" + autosave.ClientID + "')");
                blacksqft.Attributes.Add("onkeypress", "return clickButton(event,'" + autosave.ClientID + "')");
                                
            }
            if (Session["autocalc"] == null)
            {                
                autosave.Visible = false;
                style.Attributes.Add("onkeypress", "return clickButton1(event,'" + updatesave.ClientID + "')");
                flute.Attributes.Add("onkeypress", "return clickButton1(event,'" + updatesave.ClientID + "')");
                test.Attributes.Add("onkeypress", "return clickButton1(event,'" + updatesave.ClientID + "')");
                board.Attributes.Add("onkeypress", "return clickButton1(event,'" + updatesave.ClientID + "')");
                boardesc.Attributes.Add("onkeypress", "return clickButton1(event,'" + updatesave.ClientID + "')");
                length.Attributes.Add("onkeypress", "return clickButton1(event,'" + updatesave.ClientID + "')");
                width.Attributes.Add("onkeypress", "return clickButton1(event,'" + updatesave.ClientID + "')");
                depth.Attributes.Add("onkeypress", "return clickButton1(event,'" + updatesave.ClientID + "')");
                jointmat.Attributes.Add("onkeypress", "return clickButton1(event,'" + updatesave.ClientID + "')");
                dustflap.Attributes.Add("onkeypress", "return clickButton1(event,'" + updatesave.ClientID + "')");
                botflap.Attributes.Add("onkeypress", "return clickButton1(event,'" + updatesave.ClientID + "')");
                locktab.Attributes.Add("onkeypress", "return clickButton1(event,'" + updatesave.ClientID + "')");
                tabwidth.Attributes.Add("onkeypress", "return clickButton1(event,'" + updatesave.ClientID + "')");
                scorewid.Attributes.Add("onkeypress", "return clickButton1(event,'" + updatesave.ClientID + "')");
                scorelen.Attributes.Add("onkeypress", "return clickButton1(event,'" + updatesave.ClientID + "')");
                tuck.Attributes.Add("onkeypress", "return clickButton1(event,'" + updatesave.ClientID + "')");
                jointlength.Attributes.Add("onkeypress", "return clickButton1(event,'" + updatesave.ClientID + "')");
                blacklen.Attributes.Add("onkeypress", "return clickButton1(event,'" + updatesave.ClientID + "')");
                blackwid.Attributes.Add("onkeypress", "return clickButton1(event,'" + updatesave.ClientID + "')");
                blacksqft.Attributes.Add("onkeypress", "return clickButton1(event,'" + updatesave.ClientID + "')");


                cust.Attributes.Add("onkeypress", "return clickButton1(event,'" + updatesave.ClientID + "')");
                shipto.Attributes.Add("onkeypress", "return clickButton1(event,'" + updatesave.ClientID + "')");
                sales.Attributes.Add("onkeypress", "return clickButton1(event,'" + updatesave.ClientID + "')");
                comm.Attributes.Add("onkeypress", "return clickButton1(event,'" + updatesave.ClientID + "')");
                categ.Attributes.Add("onkeypress", "return clickButton1(event,'" + updatesave.ClientID + "')");
                custpart.Attributes.Add("onkeypress", "return clickButton1(event,'" + updatesave.ClientID + "')");
                fgitem.Attributes.Add("onkeypress", "return clickButton1(event,'" + updatesave.ClientID + "')");
                itemname.Attributes.Add("onkeypress", "return clickButton1(event,'" + updatesave.ClientID + "')");
                desc.Attributes.Add("onkeypress", "return clickButton1(event,'" + updatesave.ClientID + "')");
                die.Attributes.Add("onkeypress", "return clickButton1(event,'" + updatesave.ClientID + "')");
                image.Attributes.Add("onkeypress", "return clickButton1(event,'" + updatesave.ClientID + "')");
                cad.Attributes.Add("onkeypress", "return clickButton1(event,'" + updatesave.ClientID + "')");
                plate.Attributes.Add("onkeypress", "return clickButton1(event,'" + updatesave.ClientID + "')");
                spcnum.Attributes.Add("onkeypress", "return clickButton1(event,'" + updatesave.ClientID + "')");
                upcnum.Attributes.Add("onkeypress", "return clickButton1(event,'" + updatesave.ClientID + "')");
                
            }
        }
        if (FormView_Specs.CurrentMode == FormViewMode.ReadOnly)
        {
            try
            {
                GridView1.Visible = true;
                Label image = (Label)FormView_Specs.FindControl("vImageLabel");
                string str1 = Path.GetFileName(image.Text.Trim());
                image.Text = str1;
            }
            catch { }
        }
    }

    protected void GridView1_SelectedIndexChanged(object sender, EventArgs e)
    {
        try
        {
            Session["index_list1_corrugated"] = GridView1.SelectedIndex;
            //Session["order_corrugated_est"] = GridView1.SelectedRow.Cells[1].Text;
            //Session["order_corrugated_fgitem"] = GridView1.SelectedRow.Cells[6].Text; 

            Session["order_corrugated_formno"] = GridView1.SelectedRow.Cells[17].Text;
            Session["order_corrugated_blankno"] = GridView1.SelectedRow.Cells[18].Text;

        }
        catch { }

    }

    protected void Auto_Save_Click(object sender, EventArgs e)
    {
        Label est = (Label)FormView_Specs.FindControl("vEstNumTextBox");
        TextBox form = (TextBox)FormView_Specs.FindControl("vFromDtTextBox");
        TextBox cust = (TextBox)FormView_Specs.FindControl("vCustNumTextBox");
        TextBox shipto = (TextBox)FormView_Specs.FindControl("vShipToTextBox");
        TextBox shipname = (TextBox)FormView_Specs.FindControl("vShipNameTextBox");
        TextBox addr = (TextBox)FormView_Specs.FindControl("vAddrTextBox");
        TextBox addr2 = (TextBox)FormView_Specs.FindControl("vAddr2TextBox");
        TextBox city = (TextBox)FormView_Specs.FindControl("vCityTextBox");
        TextBox state = (TextBox)FormView_Specs.FindControl("vStateTextBox");
        TextBox zip = (TextBox)FormView_Specs.FindControl("vZipTextBox");
        TextBox sales = (TextBox)FormView_Specs.FindControl("vSalesmanTextBox");
        TextBox saledscr = (TextBox)FormView_Specs.FindControl("vSmanDscrTextBox");
        TextBox comm = (TextBox)FormView_Specs.FindControl("vCommTextBox");
        TextBox categ = (TextBox)FormView_Specs.FindControl("vFgCategoryTextBox");
        TextBox catdscr = (TextBox)FormView_Specs.FindControl("vFgCatDscrTextBox");
        TextBox custpart = (TextBox)FormView_Specs.FindControl("vCustPartTextBox");
        TextBox fgitem = (TextBox)FormView_Specs.FindControl("vFgItemTextBox");
        TextBox itemname = (TextBox)FormView_Specs.FindControl("vItemNameTextBox");
        TextBox desc = (TextBox)FormView_Specs.FindControl("vDescrTextBox");
        TextBox die = (TextBox)FormView_Specs.FindControl("vDieNumTextBox");
        TextBox image = (TextBox)FormView_Specs.FindControl("vImageTextBox");
        TextBox cad = (TextBox)FormView_Specs.FindControl("vCadNumTextBox");
        TextBox plate = (TextBox)FormView_Specs.FindControl("vPlateNumTextBox");
        TextBox spcnum = (TextBox)FormView_Specs.FindControl("vSpcNumTextBox");
        TextBox upcnum = (TextBox)FormView_Specs.FindControl("vUpcNumTextBox");
        TextBox style = (TextBox)FormView_Specs.FindControl("vStyleTextBox");
        TextBox styledesc = (TextBox)FormView_Specs.FindControl("vStyleDscrTextBox");
        TextBox flute = (TextBox)FormView_Specs.FindControl("vFluteTextBox");
        TextBox test = (TextBox)FormView_Specs.FindControl("vTestTextBox");
        DropDownList tab = (DropDownList)FormView_Specs.FindControl("DropDownList1");
        DropDownList metric = (DropDownList)FormView_Specs.FindControl("DropDownList2");
        TextBox board = (TextBox)FormView_Specs.FindControl("vBoardTextBox");
        TextBox boardesc = (TextBox)FormView_Specs.FindControl("vBrdDscrTextBox");
        TextBox length = (TextBox)FormView_Specs.FindControl("vLengthTextBox");
        TextBox width = (TextBox)FormView_Specs.FindControl("vWidthTextBox");
        TextBox depth = (TextBox)FormView_Specs.FindControl("vDepthTextBox");
        TextBox jointmat = (TextBox)FormView_Specs.FindControl("vJointMatTextBox");
        TextBox dustflap = (TextBox)FormView_Specs.FindControl("vDustFlapTextBox");
        TextBox botflap = (TextBox)FormView_Specs.FindControl("vBotFlapTextBox");
        TextBox locktab = (TextBox)FormView_Specs.FindControl("vLockTabTextBox");
        TextBox tabwidth = (TextBox)FormView_Specs.FindControl("vTabWidTextBox");
        TextBox scorewid = (TextBox)FormView_Specs.FindControl("vScoreWidTextBox");
        TextBox scorelen = (TextBox)FormView_Specs.FindControl("vScoreLenTextBox");
        TextBox tuck = (TextBox)FormView_Specs.FindControl("vTuckTextBox");
        TextBox jointlength = (TextBox)FormView_Specs.FindControl("vJointLenTextBox");
        TextBox blackwid = (TextBox)FormView_Specs.FindControl("vBlankWidTextBox");
        TextBox blacklen = (TextBox)FormView_Specs.FindControl("vBlankLenTextBox");
        TextBox blacksqft = (TextBox)FormView_Specs.FindControl("vBlankSqFtTextBox");
        Label estdate = (Label)FormView_Specs.FindControl("vEstDateTextBox");     

        if (tab.SelectedIndex == 0)
        {
            HiddenField1.Value = "Out";
        }
        if (tab.SelectedIndex == 1)
        {
            HiddenField1.Value = "In";
        }
        if (metric.SelectedIndex == 0)
        {
            HiddenField2.Value = "Yes";
        }
        if (metric.SelectedIndex == 1)
        {
            HiddenField2.Value = "No";
        }

        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];
        Corrugated corr = new Corrugated();

        bool check = corr.ValidateCorrSpec(UserLogin.UserName, "Override", cust.Text.Trim(), shipto.Text.Trim(), fgitem.Text.Trim(), custpart.Text.Trim(), itemname.Text.Trim(), "", "", "", "", "", "", "", sales.Text.Trim(), "", 0, categ.Text.Trim(), "", style.Text.Trim(), "", board.Text.Trim(), "", 0, 0, 0, flute.Text.Trim(), test.Text.Trim(), "", "", jointmat.Text.Trim(), 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, est.Text.Trim(), Convert.ToDateTime(estdate.Text.Trim()), Convert.ToDateTime(estdate.Text.Trim()), Convert.ToDateTime(estdate.Text.Trim()), 0, Convert.ToDateTime(estdate.Text.Trim()), 0, 0, 0, "", "", "", "", "", "", Convert.ToInt32(Session["order_corrugated_formno"]), "", Convert.ToInt32(Session["order_corrugated_blankno"]));

        string value = Convert.ToString(check);
        if (value == "True")
        {

            CorrugatedSpecsDataSource.SelectParameters["prmAction"].DefaultValue = "Override";
            CorrugatedSpecsDataSource.SelectParameters["prmEstNum"].DefaultValue = est.Text.Trim();
            CorrugatedSpecsDataSource.SelectParameters["prmCustNum"].DefaultValue = cust.Text.Trim();
            CorrugatedSpecsDataSource.SelectParameters["prmShipTo"].DefaultValue = shipto.Text.Trim();
            CorrugatedSpecsDataSource.SelectParameters["prmFgItem"].DefaultValue = fgitem.Text.Trim();
            CorrugatedSpecsDataSource.SelectParameters["prmCustPart"].DefaultValue = custpart.Text.Trim();
            CorrugatedSpecsDataSource.SelectParameters["prmItemName"].DefaultValue = itemname.Text.Trim();
            CorrugatedSpecsDataSource.SelectParameters["prmPartDscr"].DefaultValue = desc.Text.Trim();
            CorrugatedSpecsDataSource.SelectParameters["prmDieNum"].DefaultValue = die.Text.Trim();
            CorrugatedSpecsDataSource.SelectParameters["prmCadNum"].DefaultValue = cad.Text.Trim();
            CorrugatedSpecsDataSource.SelectParameters["prmSpcNum"].DefaultValue = spcnum.Text.Trim();
            CorrugatedSpecsDataSource.SelectParameters["prmPlateNum"].DefaultValue = plate.Text.Trim();
            CorrugatedSpecsDataSource.SelectParameters["prmImage"].DefaultValue = image.Text.Trim();
            CorrugatedSpecsDataSource.SelectParameters["prmUpcNum"].DefaultValue = upcnum.Text.Trim();
            CorrugatedSpecsDataSource.SelectParameters["prmSman"].DefaultValue = sales.Text.Trim();
            //CorrugatedSpecsDataSource.SelectParameters["prmSmanDscr"].DefaultValue = saledscr.Text.Trim();
            CorrugatedSpecsDataSource.SelectParameters["prmComm"].DefaultValue = comm.Text.Trim();
            CorrugatedSpecsDataSource.SelectParameters["prmFgCat"].DefaultValue = categ.Text.Trim();
            //CorrugatedSpecsDataSource.SelectParameters["prmFgCatDscr"].DefaultValue = catdscr.Text.Trim();
            CorrugatedSpecsDataSource.SelectParameters["prmStyle"].DefaultValue = style.Text.Trim();
            //CorrugatedSpecsDataSource.SelectParameters["prmStyDscr"].DefaultValue = styledesc.Text.Trim();
            CorrugatedSpecsDataSource.SelectParameters["prmBoard"].DefaultValue = board.Text.Trim();
            CorrugatedSpecsDataSource.SelectParameters["prmBrdDscr"].DefaultValue = boardesc.Text.Trim();
            CorrugatedSpecsDataSource.SelectParameters["prmLength"].DefaultValue = length.Text.Trim();
            CorrugatedSpecsDataSource.SelectParameters["prmWidth"].DefaultValue = width.Text.Trim();
            CorrugatedSpecsDataSource.SelectParameters["prmDepth"].DefaultValue = depth.Text.Trim();
            CorrugatedSpecsDataSource.SelectParameters["prmFlute"].DefaultValue = flute.Text.Trim();
            CorrugatedSpecsDataSource.SelectParameters["prmTest"].DefaultValue = test.Text.Trim();
            CorrugatedSpecsDataSource.SelectParameters["prmTab"].DefaultValue = HiddenField1.Value;
            CorrugatedSpecsDataSource.SelectParameters["prmMetric"].DefaultValue = HiddenField2.Value;
            CorrugatedSpecsDataSource.SelectParameters["prmJointMat"].DefaultValue = jointmat.Text.Trim();
            CorrugatedSpecsDataSource.SelectParameters["prmDustFlap"].DefaultValue = dustflap.Text.Trim();
            CorrugatedSpecsDataSource.SelectParameters["prmBotFlap"].DefaultValue = botflap.Text.Trim();
            CorrugatedSpecsDataSource.SelectParameters["prmLockTab"].DefaultValue = locktab.Text.Trim();
            CorrugatedSpecsDataSource.SelectParameters["prmTabWid"].DefaultValue = tabwidth.Text.Trim();
            CorrugatedSpecsDataSource.SelectParameters["prmScWid"].DefaultValue = scorewid.Text.Trim();
            CorrugatedSpecsDataSource.SelectParameters["prmScLen"].DefaultValue = scorelen.Text.Trim();
            CorrugatedSpecsDataSource.SelectParameters["prmTuck"].DefaultValue = tuck.Text.Trim();
            CorrugatedSpecsDataSource.SelectParameters["prmJointLen"].DefaultValue = jointlength.Text.Trim();
            CorrugatedSpecsDataSource.SelectParameters["prmBlankWid"].DefaultValue = blackwid.Text.Trim();
            CorrugatedSpecsDataSource.SelectParameters["prmBlankLen"].DefaultValue = blacklen.Text.Trim();
            CorrugatedSpecsDataSource.SelectParameters["prmBlankSqFt"].DefaultValue = blacksqft.Text.Trim();
            CorrugatedSpecsDataSource.SelectParameters["prmShipName"].DefaultValue = shipname.Text.Trim();
            //CorrugatedSpecsDataSource.SelectParameters["prmAddr"].DefaultValue = addr.Text.Trim();
            //CorrugatedSpecsDataSource.SelectParameters["prmAddr2"].DefaultValue = addr2.Text.Trim();

            //CorrugatedSpecsDataSource.SelectParameters["prmCity"].DefaultValue = city.Text.Trim();
            //CorrugatedSpecsDataSource.SelectParameters["prmState"].DefaultValue = state.Text.Trim();
            //CorrugatedSpecsDataSource.SelectParameters["prmZip"].DefaultValue = zip.Text.Trim();

            CorrugatedSpecsDataSource.SelectParameters["prmAutocalcSelected"].DefaultValue = "yes";
            FormView_Specs.ChangeMode(FormViewMode.ReadOnly);
          //  Response.Write("<script>window.location.href='corr_specs.aspx'</script>");
        }

    }

    protected void test_change(object sender, EventArgs e)
    {
        try
        {
        TextBox style = (TextBox)FormView_Specs.FindControl("vStyleTextBox");
        TextBox flute = (TextBox)FormView_Specs.FindControl("vFluteTextBox");
        TextBox test = (TextBox)FormView_Specs.FindControl("vTestTextBox");
        TextBox board = (TextBox)FormView_Specs.FindControl("vBoardTextBox");
        TextBox boardesc = (TextBox)FormView_Specs.FindControl("vBrdDscrTextBox");

        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];
        Corrugated look = new Corrugated();
        DataSet ds = new DataSet();
        ds = look.TestCode("Search", "Test", test.Text.Trim(), UserLogin.UserName, "001", "", flute.Text.Trim(), style.Text.Trim());
        
        board.Text = ds.Tables[0].Rows[0][1].ToString();
        boardesc.Text = ds.Tables[0].Rows[0][3].ToString();            

        }
        catch { }
    }
    protected void board_textbox_change(object sender, EventArgs e)
    {
        try
        {
            TextBox board = (TextBox)FormView_Specs.FindControl("vBoardTextBox");
            TextBox boardesc = (TextBox)FormView_Specs.FindControl("vBrdDscrTextBox");
            TextBox style = (TextBox)FormView_Specs.FindControl("vStyleTextBox");

            UserClass.CheckLogin(Page);
            UserClass UserLogin = (UserClass)Session["User"];
            Corrugated look = new Corrugated();
            DataSet ds = new DataSet();
            ds = look.SelectBoard("Search", UserLogin.UserName, "i-no", "EQUAL", board.Text.Trim(), "", "", style.Text.Trim());


            boardesc.Text = ds.Tables[0].Rows[0][1].ToString();
        }
        catch { }
    }
   
}

