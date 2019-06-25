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

public partial class fold_specs : System.Web.UI.Page
{
    protected void Page_Load(object sender, EventArgs e)
    {
        
        FormView_Specs.ChangeMode(FormViewMode.ReadOnly);
        CorrugatedSpecsDataSource.SelectParameters["prmAction"].DefaultValue = "Select";
        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];
        ObjectDataSource_list.SelectParameters["prmAction"].DefaultValue = "ListSelect";
        GridView1.DataBind();
        try
        {
            if (Session["fold_grid_index"] != null)
            {
                GridView1.SelectedIndex = Convert.ToInt32(Session["fold_grid_index"]);
            }
        }
        catch { }

        if (!Page.IsPostBack)
        {
        }
        if (Session["User"] != null)
        {
            string vUserId = UserLogin.UserName;
            string vPage = "fold_specs.aspx";
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
            labelname.Text = "Foldings";
            
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
    }

    protected void Save_Click(object sender, EventArgs e)
    {
        TextBox est = (TextBox)FormView_Specs.FindControl("vEstNumTextBox");
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
        TextBox qty = (TextBox)FormView_Specs.FindControl("vQtyTextBox");
        Label estdate = (Label)FormView_Specs.FindControl("vEstDateTextBox");
        TextBox image = (TextBox)FormView_Specs.FindControl("vImageTextBox");
        FileUpload FileUpload1 = (FileUpload)FormView_Specs.FindControl("FileUpload1");

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

        bool check = corr.FoldingSpecValidate(UserLogin.UserName, "Override", cust.Text.Trim(), shipto.Text.Trim(), fgitem.Text.Trim(), custpart.Text.Trim(), itemname.Text.Trim(), "", "", "", "", "", "", "", sales.Text.Trim(), "", 0, categ.Text.Trim(), "", style.Text.Trim(), "", board.Text.Trim(), "", 0, 0, 0, "", jointmat.Text.Trim(), 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, est.Text.Trim(), Convert.ToDateTime(estdate.Text.Trim()), Convert.ToDateTime(estdate.Text.Trim()), Convert.ToDateTime(estdate.Text.Trim()), 0, Convert.ToDateTime(estdate.Text.Trim()), 0, "", "", "", "", "", "", Convert.ToInt32(Session["order_folding_formno"]), "", Convert.ToInt32(Session["order_folding_blankno"]));

        string value = Convert.ToString(check);
        if (value == "True")
        {
            if (FileUpload1.FileName != "")
            {
                string str1 = FileUpload1.PostedFile.FileName;
                string str2 = Path.GetFileName(str1);
                string str3 = @"D:\webapps\uploadedimages\";  //give the path where you want to upload the file.
                FileUpload1.PostedFile.SaveAs(Path.Combine(str3, str2));
                CorrugatedSpecsDataSource.SelectParameters["prmImage"].DefaultValue = Path.Combine(str3, str2);
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
            CorrugatedSpecsDataSource.SelectParameters["prmImage"].DefaultValue = image.Text.Trim();
            CorrugatedSpecsDataSource.SelectParameters["prmUpcNum"].DefaultValue = upcnum.Text.Trim();
            CorrugatedSpecsDataSource.SelectParameters["prmSman"].DefaultValue = sales.Text.Trim();
            CorrugatedSpecsDataSource.SelectParameters["prmSmanDscr"].DefaultValue = saledscr.Text.Trim();
            CorrugatedSpecsDataSource.SelectParameters["prmComm"].DefaultValue = comm.Text.Trim();
            CorrugatedSpecsDataSource.SelectParameters["prmFgCat"].DefaultValue = categ.Text.Trim();
            CorrugatedSpecsDataSource.SelectParameters["prmFgCatDscr"].DefaultValue = catdscr.Text.Trim();
            CorrugatedSpecsDataSource.SelectParameters["prmStyle"].DefaultValue = style.Text.Trim();
            CorrugatedSpecsDataSource.SelectParameters["prmStyDscr"].DefaultValue = styledesc.Text.Trim();
            CorrugatedSpecsDataSource.SelectParameters["prmBoard"].DefaultValue = board.Text.Trim();
            CorrugatedSpecsDataSource.SelectParameters["prmBrdDscr"].DefaultValue = boardesc.Text.Trim();
            CorrugatedSpecsDataSource.SelectParameters["prmLength"].DefaultValue = length.Text.Trim();
            CorrugatedSpecsDataSource.SelectParameters["prmWidth"].DefaultValue = width.Text.Trim();
            CorrugatedSpecsDataSource.SelectParameters["prmDepth"].DefaultValue = depth.Text.Trim();

            CorrugatedSpecsDataSource.SelectParameters["prmMetric"].DefaultValue = HiddenField2.Value;
            CorrugatedSpecsDataSource.SelectParameters["prmAdhe"].DefaultValue = jointmat.Text.Trim();
            CorrugatedSpecsDataSource.SelectParameters["prmDustFlap"].DefaultValue = dustflap.Text.Trim();
            CorrugatedSpecsDataSource.SelectParameters["prmPanel"].DefaultValue = botflap.Text.Trim();
            CorrugatedSpecsDataSource.SelectParameters["prmLockTab"].DefaultValue = locktab.Text.Trim();
            CorrugatedSpecsDataSource.SelectParameters["prmGlue"].DefaultValue = tabwidth.Text.Trim();
            CorrugatedSpecsDataSource.SelectParameters["prmScWid"].DefaultValue = scorewid.Text.Trim();
            CorrugatedSpecsDataSource.SelectParameters["prmScLen"].DefaultValue = scorelen.Text.Trim();
            CorrugatedSpecsDataSource.SelectParameters["prmTuck"].DefaultValue = tuck.Text.Trim();
            CorrugatedSpecsDataSource.SelectParameters["prmLinInc"].DefaultValue = jointlength.Text.Trim();
            CorrugatedSpecsDataSource.SelectParameters["prmBlankWid"].DefaultValue = blackwid.Text.Trim();
            CorrugatedSpecsDataSource.SelectParameters["prmBlankLen"].DefaultValue = blacklen.Text.Trim();
            CorrugatedSpecsDataSource.SelectParameters["prmBlankSqFt"].DefaultValue = blacksqft.Text.Trim();
            CorrugatedSpecsDataSource.SelectParameters["prmShipName"].DefaultValue = shipname.Text.Trim();
            CorrugatedSpecsDataSource.SelectParameters["prmAddr"].DefaultValue = addr.Text.Trim();
            CorrugatedSpecsDataSource.SelectParameters["prmAddr2"].DefaultValue = addr2.Text.Trim();

            CorrugatedSpecsDataSource.SelectParameters["prmCity"].DefaultValue = city.Text.Trim();
            CorrugatedSpecsDataSource.SelectParameters["prmState"].DefaultValue = state.Text.Trim();
            CorrugatedSpecsDataSource.SelectParameters["prmZip"].DefaultValue = zip.Text.Trim();
            CorrugatedSpecsDataSource.SelectParameters["prmQty"].DefaultValue = qty.Text.Trim();

            Response.Write("<script>window.location.href='fold_specs.aspx'</script>");
        }
    }
    protected void updatebutton_click(object sender, EventArgs e)
    {
        Session["autocalc2"] = null;
    }

    protected void AutoCalculateButton_Click(object sender, EventArgs e)
    {
        Session["autocalc2"] = 1;
    }
    protected void FormView_Specs_DataBound(object sender, EventArgs e)
    {
        if (FormView_Specs.CurrentMode == FormViewMode.Edit)
        {
            try
            {
                GridView1.Visible = false;
                TextBox form = (TextBox)FormView_Specs.FindControl("vFromDtTextBox");
                form.ReadOnly = true;
                if (Session["autocalc2"] != null)
                {

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
                    TextBox qty = (TextBox)FormView_Specs.FindControl("vQtyTextBox");

                    TextBox blankwid = (TextBox)FormView_Specs.FindControl("vBlankWidTextBox");
                    TextBox blanklen = (TextBox)FormView_Specs.FindControl("vBlankLenTextBox");
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
                    Button update = (Button)FormView_Specs.FindControl("UpdateButton");
                    Button auto = (Button)FormView_Specs.FindControl("auotButton");
                    FileUpload FileUpload1 = (FileUpload)FormView_Specs.FindControl("FileUpload1");

                    FileUpload1.Enabled = false;
                    update.Visible = false;
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

                    blankwid.Enabled = false;
                    blanklen.Enabled = false;
                    blanksqft.Enabled = false;
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
                    qty.Enabled = false;
                }
                if (Session["autocalc2"] == null)
                {
                    Button auto = (Button)FormView_Specs.FindControl("auotButton");
                    auto.Visible = false;
                }
            }
            catch { }
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
        Session["order_folding_formno"] = GridView1.SelectedRow.Cells[17].Text;
        Session["order_folding_blankno"] = GridView1.SelectedRow.Cells[18].Text;
        Session["fold_grid_index"] = GridView1.SelectedIndex;
    }
    protected void Auto_Save_Click(object sender, EventArgs e)
    {
        TextBox est = (TextBox)FormView_Specs.FindControl("vEstNumTextBox");
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
        TextBox qty = (TextBox)FormView_Specs.FindControl("vQtyTextBox");
        Label estdate = (Label)FormView_Specs.FindControl("vEstDateTextBox");
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

        bool check = corr.FoldingSpecValidate(UserLogin.UserName, "Override", cust.Text.Trim(), shipto.Text.Trim(), fgitem.Text.Trim(), custpart.Text.Trim(), itemname.Text.Trim(), "", "", "", "", "", "", "", sales.Text.Trim(), "", 0, categ.Text.Trim(), "", style.Text.Trim(), "", board.Text.Trim(), "", 0, 0, 0, "", jointmat.Text.Trim(), 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, est.Text.Trim(), Convert.ToDateTime(estdate.Text.Trim()), Convert.ToDateTime(estdate.Text.Trim()), Convert.ToDateTime(estdate.Text.Trim()), 0, Convert.ToDateTime(estdate.Text.Trim()), 0, "", "", "", "", "", "", Convert.ToInt32(Session["order_folding_formno"]), "", Convert.ToInt32(Session["order_folding_blankno"]));

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
            CorrugatedSpecsDataSource.SelectParameters["prmSmanDscr"].DefaultValue = saledscr.Text.Trim();
            CorrugatedSpecsDataSource.SelectParameters["prmComm"].DefaultValue = comm.Text.Trim();
            CorrugatedSpecsDataSource.SelectParameters["prmFgCat"].DefaultValue = categ.Text.Trim();
            CorrugatedSpecsDataSource.SelectParameters["prmFgCatDscr"].DefaultValue = catdscr.Text.Trim();
            CorrugatedSpecsDataSource.SelectParameters["prmStyle"].DefaultValue = style.Text.Trim();
            CorrugatedSpecsDataSource.SelectParameters["prmStyDscr"].DefaultValue = styledesc.Text.Trim();
            CorrugatedSpecsDataSource.SelectParameters["prmBoard"].DefaultValue = board.Text.Trim();
            CorrugatedSpecsDataSource.SelectParameters["prmBrdDscr"].DefaultValue = boardesc.Text.Trim();
            CorrugatedSpecsDataSource.SelectParameters["prmLength"].DefaultValue = length.Text.Trim();
            CorrugatedSpecsDataSource.SelectParameters["prmWidth"].DefaultValue = width.Text.Trim();
            CorrugatedSpecsDataSource.SelectParameters["prmDepth"].DefaultValue = depth.Text.Trim();

            CorrugatedSpecsDataSource.SelectParameters["prmMetric"].DefaultValue = HiddenField2.Value;
            CorrugatedSpecsDataSource.SelectParameters["prmAdhe"].DefaultValue = jointmat.Text.Trim();
            CorrugatedSpecsDataSource.SelectParameters["prmDustFlap"].DefaultValue = dustflap.Text.Trim();
            CorrugatedSpecsDataSource.SelectParameters["prmPanel"].DefaultValue = botflap.Text.Trim();
            CorrugatedSpecsDataSource.SelectParameters["prmLockTab"].DefaultValue = locktab.Text.Trim();
            CorrugatedSpecsDataSource.SelectParameters["prmGlue"].DefaultValue = tabwidth.Text.Trim();
            CorrugatedSpecsDataSource.SelectParameters["prmScWid"].DefaultValue = scorewid.Text.Trim();
            CorrugatedSpecsDataSource.SelectParameters["prmScLen"].DefaultValue = scorelen.Text.Trim();
            CorrugatedSpecsDataSource.SelectParameters["prmTuck"].DefaultValue = tuck.Text.Trim();
            CorrugatedSpecsDataSource.SelectParameters["prmLinInc"].DefaultValue = jointlength.Text.Trim();
            CorrugatedSpecsDataSource.SelectParameters["prmBlankWid"].DefaultValue = blackwid.Text.Trim();
            CorrugatedSpecsDataSource.SelectParameters["prmBlankLen"].DefaultValue = blacklen.Text.Trim();
            CorrugatedSpecsDataSource.SelectParameters["prmBlankSqFt"].DefaultValue = blacksqft.Text.Trim();
            CorrugatedSpecsDataSource.SelectParameters["prmShipName"].DefaultValue = shipname.Text.Trim();
            CorrugatedSpecsDataSource.SelectParameters["prmAddr"].DefaultValue = addr.Text.Trim();
            CorrugatedSpecsDataSource.SelectParameters["prmAddr2"].DefaultValue = addr2.Text.Trim();

            CorrugatedSpecsDataSource.SelectParameters["prmCity"].DefaultValue = city.Text.Trim();
            CorrugatedSpecsDataSource.SelectParameters["prmState"].DefaultValue = state.Text.Trim();
            CorrugatedSpecsDataSource.SelectParameters["prmZip"].DefaultValue = zip.Text.Trim();
            CorrugatedSpecsDataSource.SelectParameters["prmQty"].DefaultValue = qty.Text.Trim();
            CorrugatedSpecsDataSource.SelectParameters["prmAutocalcSelected"].DefaultValue = "yes";

            Response.Write("<script>window.location.href='fold_specs.aspx'</script>");
        }
    }
   
}

